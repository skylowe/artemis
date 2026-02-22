#' U.S. Employment Projection (USEMP) — Core Functions
#'
#' @description
#' Projects unemployment rates, labor force participation rates, labor force,
#' and employment by age group and sex. Implements Equations 2.1.1–2.1.6
#' from the 2025 Long-Range OASDI Model Documentation.
#'
#' @references
#' - 2025_LR_Model_Documentation_Economics_1_USEmployment.md (Eqs 2.1.1-2.1.6)
#' - economics_equations_1_USEmployment.md (Sections 1-10)
#'
#' @name us_employment
NULL

# =============================================================================
# Constants
# =============================================================================

#' USEMP unemployment rate age groups (14 groups x 2 sexes = 28 equations)
USEMP_RU_AGE_GROUPS <- c(
  "16-17", "18-19", "20-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"
)

#' USEMP LFPR age groups for 5-year groups (ages 16-54)
USEMP_LFPR_5YR_GROUPS <- c(
  "16-17", "18-19", "20-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54"
)

#' USEMP LFPR single-year ages (55-100)
USEMP_LFPR_SINGLE_AGES <- 55:100

#' USEMP age group to single-year age mapping
#' @keywords internal
USEMP_AGE_GROUP_RANGES <- list(
  "16-17" = 16:17, "18-19" = 18:19, "20-24" = 20:24,
  "25-29" = 25:29, "30-34" = 30:34, "35-39" = 35:39,
  "40-44" = 40:44, "45-49" = 45:49, "50-54" = 50:54,
  "55-59" = 55:59, "60-64" = 60:64, "65-69" = 65:69,
  "70-74" = 70:74, "75+"   = 75:120
)

# =============================================================================
# Unemployment Rate Projection (Eq 2.1.3)
# =============================================================================

#' Project unemployment rates by age group and sex
#'
#' @description
#' Implements the unemployment rate projection using a first-difference model
#' with distributed lags on the change in RTP (ratio of real to potential GDP).
#' Rates are constrained to match the Trustees' aggregate unemployment rate target.
#'
#' Steps:
#' 1. Compute preliminary rates using D(RTP) distributed lags (Section 1)
#' 2. Compute age-sex adjusted aggregate (Section 2)
#' 3. Constrain to Trustees' target via proportional adjustment (Section 3)
#' 4. Compute full-employment differentials (Sections 5-6)
#'
#' @param rtp_quarterly Quarterly RTP series (data.table with year, quarter, rtp)
#' @param base_year_labor_force Labor force by age group and sex in base year
#'   (data.table with age_group, sex, labor_force)
#' @param historical_ru Historical unemployment rates by age group and sex
#'   (data.table with year, quarter, age_group, sex, rate)
#' @param target_ru Target aggregate unemployment rate path
#'   (data.table with year, quarter, rate)
#' @param ru_coefficients Unemployment regression coefficients
#'   (list loaded from unemployment_coefficients.yaml)
#' @param config_employment Employment config section
#'
#' @return List with components:
#'   - `actual`: data.table of quarterly unemployment rates (year, quarter, age_group, sex, rate)
#'   - `full_employment`: data.table of full-employment unemployment rates
#'   - `differentials`: data.table of full-employment differentials
#'
#' @references Eq 2.1.3, economics_equations_1_USEmployment.md Sections 1-6
#' @export
project_unemployment_rates <- function(rtp_quarterly,
                                        base_year_labor_force,
                                        historical_ru,
                                        target_ru,
                                        ru_coefficients,
                                        config_employment) {
  checkmate::assert_data_table(rtp_quarterly)
  checkmate::assert_data_table(base_year_labor_force)
  checkmate::assert_data_table(historical_ru)
  checkmate::assert_data_table(target_ru)
  checkmate::assert_list(ru_coefficients)
  checkmate::assert_list(config_employment)

  base_year <- config_employment$base_year
  cli::cli_alert_info("Projecting unemployment rates from base year {base_year}")

  # Step 1: Compute quarterly D(RTP) = RTP(q) - RTP(q-1)
  # ── Vectorized RTP preparation ──────────────────────────────────
  rtp <- data.table::copy(rtp_quarterly)
  data.table::setorder(rtp, year, quarter)
  rtp[, d_rtp := rtp - shift(rtp, 1)]
  rtp[, d_rtp_1 := shift(d_rtp, 1)]
  rtp[, d_rtp_2 := shift(d_rtp, 2)]
  rtp[, d_rtp_3 := shift(d_rtp, 3)]

  # RTP lags for full-employment differentials (Section 5)
  rtp[, rtp_1 := shift(rtp, 1)]
  rtp[, rtp_2 := shift(rtp, 2)]
  rtp[, rtp_3 := shift(rtp, 3)]

  # Projection quarters as plain vectors (avoid repeated data.table indexing)
  proj_mask <- (rtp$year > base_year | (rtp$year == base_year & rtp$quarter > 0)) &
               !is.na(rtp$d_rtp)
  proj_idx <- which(proj_mask)
  n_proj <- length(proj_idx)

  # Extract D(RTP) lag matrix: n_proj × 4, each row = [d_rtp, d_rtp_1, d_rtp_2, d_rtp_3]
  d_rtp_mat <- cbind(
    rtp$d_rtp[proj_idx],
    rtp$d_rtp_1[proj_idx],
    rtp$d_rtp_2[proj_idx],
    rtp$d_rtp_3[proj_idx]
  )
  d_rtp_mat[is.na(d_rtp_mat)] <- 0

  # RTP lag matrix for FE differentials: (1 - RTP) for current and 3 lags
  fe_rtp_mat <- cbind(
    1 - rtp$rtp[proj_idx],
    1 - fifelse(is.na(rtp$rtp_1[proj_idx]), 1, rtp$rtp_1[proj_idx]),
    1 - fifelse(is.na(rtp$rtp_2[proj_idx]), 1, rtp$rtp_2[proj_idx]),
    1 - fifelse(is.na(rtp$rtp_3[proj_idx]), 1, rtp$rtp_3[proj_idx])
  )

  proj_years <- rtp$year[proj_idx]
  proj_qtrs <- rtp$quarter[proj_idx]

  # ── Build coefficient matrix (28 groups × 4 lags) ─────────────
  group_specs <- list()
  for (sex_name in c("male", "female")) {
    sex_coeffs <- ru_coefficients[[sex_name]]
    for (ag in USEMP_RU_AGE_GROUPS) {
      coeffs <- sex_coeffs[[ag]]
      if (is.null(coeffs)) {
        cli::cli_abort("Missing unemployment coefficients for {sex_name} {ag}")
      }
      group_specs[[length(group_specs) + 1]] <- list(
        sex = sex_name, age_group = ag,
        d = c(coeffs$d0, coeffs$d1, coeffs$d2, coeffs$d3)
      )
    }
  }
  n_groups <- length(group_specs)

  # Coefficient matrix: n_groups × 4
  coeff_mat <- do.call(rbind, lapply(group_specs, function(g) g$d))

  # ── Vectorized preliminary rate computation ────────────────────
  # rate_change[q] = coeff %*% d_rtp_lags[q]  (matrix multiply)
  # rate_change is n_proj × n_groups
  rate_change_mat <- d_rtp_mat %*% t(coeff_mat)  # n_proj × n_groups

  # Get seed rates from historical data for each group
  seed_rates <- vapply(group_specs, function(g) {
    hist_vals <- historical_ru[age_group == g$age_group & sex == g$sex]
    if (nrow(hist_vals) > 0) hist_vals[.N, rate] else 0
  }, numeric(1))

  # Cumulative sum of rate changes + seed rate = preliminary rates
  # R_P(q) = R_P(q-1) + rate_change(q) = seed + cumsum(rate_changes)
  prelim_rates <- apply(rate_change_mat, 2, cumsum)  # n_proj × n_groups
  prelim_rates <- sweep(prelim_rates, 2, seed_rates, `+`)

  # ── Vectorized FE differentials ────────────────────────────────
  # DR_FE = coeff %*% (1 - RTP_lags)
  fe_diff_mat <- fe_rtp_mat %*% t(coeff_mat)  # n_proj × n_groups

  # ── Assemble results ───────────────────────────────────────────
  # Build long-format data.table directly
  group_names <- vapply(group_specs, function(g) g$age_group, character(1))
  group_sexes <- vapply(group_specs, function(g) g$sex, character(1))

  preliminary <- data.table::data.table(
    year = rep(proj_years, n_groups),
    quarter = rep(proj_qtrs, n_groups),
    age_group = rep(group_names, each = n_proj),
    sex = rep(group_sexes, each = n_proj),
    rate = as.vector(prelim_rates)
  )

  fe_rates_dt <- data.table::data.table(
    year = rep(proj_years, n_groups),
    quarter = rep(proj_qtrs, n_groups),
    age_group = rep(group_names, each = n_proj),
    sex = rep(group_sexes, each = n_proj),
    fe_rate = as.vector(prelim_rates + fe_diff_mat)
  )

  differentials <- data.table::data.table(
    year = rep(proj_years, n_groups),
    quarter = rep(proj_qtrs, n_groups),
    age_group = rep(group_names, each = n_proj),
    sex = rep(group_sexes, each = n_proj),
    fe_differential = as.vector(fe_diff_mat)
  )

  # Step 2: Age-sex adjusted aggregate (Section 2)
  # Weight preliminary rates by base year labor force
  by_lf <- merge(preliminary, base_year_labor_force,
                  by = c("age_group", "sex"), all.x = TRUE)
  total_lf <- sum(base_year_labor_force$labor_force, na.rm = TRUE)

  ru_asa_p <- by_lf[, .(ru_asa_p = sum(rate * labor_force, na.rm = TRUE) / total_lf),
                     by = .(year, quarter)]

  # Step 3: Constrain to Trustees' target (Section 3)
  constrained <- merge(ru_asa_p, target_ru, by = c("year", "quarter"),
                       suffixes = c("_prelim", "_target"))
  constrained[, ru_asa_adj := rate - ru_asa_p]

  # Apply proportional adjustment to each age-sex group
  # Many-to-one join: each (year, quarter) in constrained maps to 28 age-sex groups
  actual <- merge(preliminary, constrained[, .(year, quarter, ru_asa_adj, ru_asa_p)],
                  by = c("year", "quarter"), allow.cartesian = TRUE)
  actual[, rate := rate * (1 + ru_asa_adj / ru_asa_p)]
  actual[, c("ru_asa_adj", "ru_asa_p") := NULL]

  # Ensure rates are non-negative

  actual[rate < 0, rate := 0]
  actual[rate > 100, rate := 100]

  cli::cli_alert_success("Projected unemployment rates: {nrow(actual)} rows, {length(unique(actual$year))} years")

  list(
    actual = actual,
    full_employment = fe_rates_dt,
    differentials = differentials
  )
}

# =============================================================================
# LFPR Time Trend Calibration
# =============================================================================

#' Calibrate LFPR time trend base values from historical CPS data
#'
#' @description
#' For each (age_group, sex, marital_status, child_status) combination that has
#' a non-zero trend coefficient, solve for the base-year trend value that makes
#' the LFPR equation reproduce the observed CPS LFPR.
#'
#' The LFPR equations include a time trend term `trend_coeff * TR`. The trend
#' variable is a cumulative counter reflecting decades of structural participation
#' change. At the base year, it should be calibrated so the equation reproduces
#' observed LFPR. For projection years, trend_val = TR_base + (year - base_year).
#'
#' @param lfpr_coefficients LFPR regression coefficients
#' @param historical_lfpr Historical LFPR from CPS (data.table)
#' @param historical_ru Historical unemployment rates from CPS (data.table)
#' @param rd Disability prevalence ratio (data.table or NULL)
#' @param children_props Children under 6 proportions (data.table or NULL)
#' @param base_year Base year for calibration
#'
#' @return data.table with columns: age_group, sex, marital_status, child_status,
#'   category, type, calibrated_tr_base
#'
#' @keywords internal
calibrate_lfpr_trends <- function(lfpr_coefficients, historical_lfpr,
                                   historical_ru, rd, children_props,
                                   base_year) {
  cli::cli_alert_info("Calibrating LFPR time trends from CPS data at base year {base_year}")

  results <- list()

  # Helper: get RU lag terms for (age_group, sex) at base_year from historical data
  get_ru_lags_hist <- function(ag, sx, ru_lag_coeffs) {
    ru_sum <- 0
    for (lag_i in seq_along(ru_lag_coeffs)) {
      lag_year <- base_year - (lag_i - 1L)
      ru_val <- historical_ru[age_group == ag & sex == sx & year == lag_year, rate]
      if (length(ru_val) == 0) ru_val <- 0
      ru_sum <- ru_sum + ru_lag_coeffs[lag_i] * ru_val[1]
    }
    ru_sum
  }

  # Helper: get RD at base year for (age_group, sex)
  get_rd_base <- function(ag, sx) {
    if (is.null(rd)) return(0)
    rd_val <- rd[age_group == ag & sex == sx & year == base_year, rd]
    if (length(rd_val) == 0) {
      # Try first available year
      rd_val <- rd[age_group == ag & sex == sx, rd]
      if (length(rd_val) == 0) return(0)
      return(rd_val[1])
    }
    rd_val[1]
  }

  # Helper: get children proportion at base year for (age_group)
  get_c6u_base <- function(ag) {
    if (is.null(children_props)) return(0)
    cp_val <- children_props[age_group == ag & year == base_year, proportion]
    if (length(cp_val) == 0) {
      cp_val <- children_props[age_group == ag, proportion]
      if (length(cp_val) == 0) return(0)
      return(cp_val[1])
    }
    cp_val[1]
  }

  # ── Section 1: Young ages (16-17, 18-19) ──────────────────────────
  for (sex_name in c("male", "female")) {
    for (ag in c("16-17", "18-19")) {
      coeffs <- lfpr_coefficients[[sex_name]][[ag]]
      if (is.null(coeffs$trend_coeff) || coeffs$trend_coeff == 0) next

      # Observed CPS LFPR at base year (marital_status = "all", child_status = "all")
      obs_lfpr <- historical_lfpr[
        age_group == ag & sex == sex_name & year == base_year &
          marital_status == "all" & child_status == "all", value]
      if (length(obs_lfpr) == 0) {
        cli::cli_abort("No CPS LFPR for {sex_name} {ag} at base year {base_year}")
      }
      obs_lfpr <- obs_lfpr[1]

      # RU lag terms
      ru_sum <- get_ru_lags_hist(ag, sex_name, coeffs$ru_lags)

      # RD at base year
      rd_val <- get_rd_base(ag, sex_name)

      # Children proportion terms (female only)
      c6u_effect <- 0
      if (!is.null(coeffs$child_under6_coeff)) {
        c6u_prop <- get_c6u_base(ag)
        c6u_effect <- coeffs$child_under6_coeff * c6u_prop +
          (coeffs$child_under6_offset %||% 0)
      }

      # Solve: obs_lfpr * (1 + rd) = ru_sum + trend_coeff * TR + trend_offset + c6u + intercept
      numerator_target <- obs_lfpr * (1 + rd_val)
      tr_base <- (numerator_target - ru_sum - (coeffs$trend_offset %||% 0) -
                    c6u_effect - coeffs$intercept) / coeffs$trend_coeff

      # Validate: plug back in to verify
      check_num <- ru_sum + coeffs$trend_coeff * tr_base +
        (coeffs$trend_offset %||% 0) + c6u_effect + coeffs$intercept
      check_lfpr <- check_num / (1 + rd_val)
      if (abs(check_lfpr - obs_lfpr) > 0.005) {
        cli::cli_abort(c(
          "Trend calibration failed for {sex_name} {ag}",
          "i" = "Observed LFPR: {round(obs_lfpr, 4)}, reconstructed: {round(check_lfpr, 4)}"
        ))
      }

      results[[length(results) + 1L]] <- data.table::data.table(
        age_group = ag, sex = sex_name,
        marital_status = "all", child_status = "all",
        category = NA_character_, type = "young",
        calibrated_tr_base = tr_base
      )
      cli::cli_alert_success(
        "  {sex_name} {ag}: TR_base = {round(tr_base, 1)}, obs LFPR = {round(obs_lfpr * 100, 1)}%"
      )
    }
  }

  # ── Section 2: Male marital ages (20-24, 25-29) ───────────────────
  marital_statuses <- c("never_married", "married_present", "married_absent")
  for (ag in c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")) {
    coeffs <- lfpr_coefficients$male[[ag]]
    if (is.null(coeffs) || coeffs$type != "marital") next
    for (ms in marital_statuses) {
      ms_coeffs <- coeffs[[ms]]
      tc <- ms_coeffs$trend_coeff %||% 0
      if (tc == 0) next

      # Observed CPS LFPR for this (age_group, male, marital_status)
      obs_lfpr <- historical_lfpr[
        age_group == ag & sex == "male" & year == base_year &
          marital_status == ms & child_status == "all", value]
      if (length(obs_lfpr) == 0) {
        cli::cli_abort("No CPS LFPR for male {ag} {ms} at base year {base_year}")
      }
      obs_lfpr <- obs_lfpr[1]

      ru_sum <- get_ru_lags_hist(ag, "male", ms_coeffs$ru_lags)
      rd_val <- get_rd_base(ag, "male")

      # Solve: obs_lfpr * (1 + rd) = ru_sum + tc * TR + intercept
      numerator_target <- obs_lfpr * (1 + rd_val)
      tr_base <- (numerator_target - ru_sum - ms_coeffs$intercept) / tc

      results[[length(results) + 1L]] <- data.table::data.table(
        age_group = ag, sex = "male",
        marital_status = ms, child_status = "all",
        category = NA_character_, type = "marital",
        calibrated_tr_base = tr_base
      )
    }
  }

  # ── Section 3: Female marital×children ages (20-44) ────────────────
  mc_categories <- c(
    "never_married_child_under6", "never_married_no_child",
    "married_present_child_under6", "married_present_no_child",
    "married_absent_child_under6", "married_absent_no_child"
  )
  for (ag in c("20-24", "25-29", "30-34", "35-39", "40-44")) {
    coeffs <- lfpr_coefficients$female[[ag]]
    if (is.null(coeffs) || coeffs$type != "marital_children") next
    for (cat in mc_categories) {
      cat_coeffs <- coeffs[[cat]]
      if (is.null(cat_coeffs)) next
      tc <- cat_coeffs$trend_coeff %||% 0
      if (tc == 0) next

      ms <- sub("_(child_under6|no_child)$", "", cat)
      cs <- sub("^.*_(child_under6|no_child)$", "\\1", cat)

      # Observed CPS LFPR for this (age_group, female, marital_status, child_status)
      obs_lfpr <- historical_lfpr[
        age_group == ag & sex == "female" & year == base_year &
          marital_status == ms & child_status == cs, value]
      if (length(obs_lfpr) == 0) {
        cli::cli_abort("No CPS LFPR for female {ag} {ms}/{cs} at base year {base_year}")
      }
      obs_lfpr <- obs_lfpr[1]

      ru_sum <- get_ru_lags_hist(ag, "female", cat_coeffs$ru_lags)
      rd_val <- get_rd_base(ag, "female")

      # Solve: obs_lfpr * (1 + rd) = ru_sum + tc * TR + intercept
      numerator_target <- obs_lfpr * (1 + rd_val)
      tr_base <- (numerator_target - ru_sum - cat_coeffs$intercept) / tc

      results[[length(results) + 1L]] <- data.table::data.table(
        age_group = ag, sex = "female",
        marital_status = ms, child_status = cs,
        category = cat, type = "marital_children",
        calibrated_tr_base = tr_base
      )
    }
  }

  result_dt <- data.table::rbindlist(results)
  cli::cli_alert_success(
    "Calibrated {nrow(result_dt)} trend base values (TR_base range: {round(min(result_dt$calibrated_tr_base), 1)} to {round(max(result_dt$calibrated_tr_base), 1)})"
  )
  result_dt
}

# =============================================================================
# Labor Force Participation Rate Projection (Eq 2.1.4)
# =============================================================================

#' Project labor force participation rates
#'
#' @description
#' Implements the 153-equation LFPR model with disaggregation by age, sex,
#' marital status, and (for females 20-44) presence of children under 6.
#'
#' Equation types vary by age group:
#' - Ages 16-19: RU lags + time trend
#' - Ages 20-54 male: RU lags by marital status (NM, MS, MA)
#' - Ages 20-44 female: RU lags by marital status x children under 6
#' - Ages 45-54 female: RU lags by marital status
#' - Ages 55-74: EDSCORE + MSSHARE (+ RRADJ/POT_ET for 62-69)
#' - Ages 75-79: Cohort decay from prior year's age-1
#' - Ages 80-100: Decay from age 79 value
#'
#' @param unemployment_rates Projected unemployment rates from project_unemployment_rates()
#' @param cni_population Civilian noninstitutional population by age, sex, marital status
#' @param employment_inputs List of constructed input variables (EDSCORE, MSSHARE, RD, RRADJ, etc.)
#' @param lfpr_coefficients LFPR regression coefficients (from lfpr_coefficients.yaml)
#' @param historical_lfpr Historical LFPR by detailed disaggregation
#' @param historical_ru Historical unemployment rates by age group and sex
#'   (data.table with year, age_group, sex, rate)
#' @param config_employment Employment config section
#'
#' @return List with:
#'   - `aggregate`: data.table of LFPR by age group and sex (annual)
#'   - `detailed`: data.table of LFPR by detailed disaggregation
#'   - `quarterly`: data.table of quarterly-interpolated LFPR
#'
#' @references Eq 2.1.4, economics_equations_1_USEmployment.md Section 7
#' @export
project_lfpr <- function(unemployment_rates,
                          cni_population,
                          employment_inputs,
                          lfpr_coefficients,
                          historical_lfpr,
                          historical_ru,
                          config_employment) {
  checkmate::assert_list(unemployment_rates)
  checkmate::assert_data_table(cni_population)
  checkmate::assert_list(employment_inputs)
  checkmate::assert_list(lfpr_coefficients)
  checkmate::assert_data_table(historical_ru)
  checkmate::assert_list(config_employment)

  base_year <- config_employment$base_year
  end_year <- config_employment$end_year
  if (is.null(end_year)) {
    cli::cli_abort("economics.employment.end_year not set in config")
  }
  projection_years <- (base_year + 1):end_year
  n_proj_years <- length(projection_years)

  # Extract input components
  edscore <- employment_inputs$edscore
  msshare <- employment_inputs$msshare
  marital_pop <- employment_inputs$marital_cni_pop
  rd <- employment_inputs$rd
  rradj <- employment_inputs$rradj
  pot_et_txrt <- employment_inputs$pot_et_txrt
  children_props <- employment_inputs$children_proportions

  # Get annual average unemployment rates
  ru_annual <- unemployment_rates$actual[,
    .(rate = mean(rate)), by = .(year, age_group, sex)]

  # Get decay factors from config
  decay_75_79 <- config_employment$lfpr_decay_75_79
  decay_80_plus <- config_employment$lfpr_decay_80_plus

  # Per-group trend adjustments: rate and max_years per age-sex group
  trend_adjustments <- config_employment$lfpr_trend_adjustments

  # MSSHARE cap: maximum deviation from base-year values
  msshare_max_dev <- config_employment$msshare_max_deviation

  # Addfactors (default to empty)
  addfactors <- config_employment$addfactors

  cli::cli_alert_info("Projecting LFPR for {n_proj_years} years")

  # ============================================================================
  # Calibrate time trend base values from historical CPS data
  # ============================================================================
  trend_calibration <- calibrate_lfpr_trends(
    lfpr_coefficients = lfpr_coefficients,
    historical_lfpr = historical_lfpr,
    historical_ru = historical_ru,
    rd = rd,
    children_props = children_props,
    base_year = base_year
  )

  # ============================================================================
  # Pre-index input tables for fast keyed lookup
  # ============================================================================
  # Set keys on input data.tables for O(log n) binary search instead of O(n) scan
  if (!is.null(rd)) data.table::setkey(rd, age_group, sex, year)

  # Pre-compute 5-year grouped RD for sections 1-4 (which use 5-year age groups).
  # RD data has single-year age_groups ("16", "17", ...) but the young/marital
  # equations use "16-17", "18-19", "20-24", etc.
  rd_5yr <- NULL
  if (!is.null(rd)) {
    rd_tmp <- data.table::copy(rd)
    rd_tmp[, age_int := as.integer(age_group)]
    rd_tmp <- rd_tmp[!is.na(age_int) & age_int >= 16L & age_int <= 54L]
    rd_tmp[, age_group_5yr := data.table::fcase(
      age_int <= 17L, "16-17", age_int <= 19L, "18-19",
      age_int <= 24L, "20-24", age_int <= 29L, "25-29",
      age_int <= 34L, "30-34", age_int <= 39L, "35-39",
      age_int <= 44L, "40-44", age_int <= 49L, "45-49",
      age_int <= 54L, "50-54"
    )]
    rd_5yr <- rd_tmp[, .(rd = mean(rd)), by = .(age_group = age_group_5yr, sex, year)]
    data.table::setkey(rd_5yr, age_group, sex, year)
  }

  if (!is.null(edscore)) data.table::setkey(edscore, age_group, sex, year)
  if (!is.null(msshare)) data.table::setkey(msshare, age, sex, year)
  if (!is.null(rradj)) data.table::setkey(rradj, age, sex, year)
  if (!is.null(pot_et_txrt)) data.table::setkey(pot_et_txrt, age, year)
  if (!is.null(children_props)) data.table::setkey(children_props, age_group, year)
  data.table::setkey(ru_annual, age_group, sex, year)
  if (!is.null(marital_pop)) data.table::setkey(marital_pop, age, sex, lfpr_marital_status, year)

  # ============================================================================
  # Build RU lag matrix for all (age_group, sex, year) triples needed by 16-54
  # ============================================================================
  # For each age_group x sex x year, we need RU rates at year, year-1, ..., year-5
  # Build this as a wide matrix via a single merge operation

  ru_groups_16_54 <- USEMP_LFPR_5YR_GROUPS  # "16-17" through "50-54"
  ru_grid <- data.table::CJ(
    age_group = ru_groups_16_54,
    sex = c("male", "female"),
    year = projection_years,
    sorted = FALSE
  )

  # Build lag columns via repeated merge
  for (lag_i in 0:5) {
    lag_col <- paste0("ru_lag", lag_i)
    ru_grid[, lag_year := year - lag_i]
    ru_grid <- merge(ru_grid, ru_annual[, .(age_group, sex, year, rate)],
                     by.x = c("age_group", "sex", "lag_year"),
                     by.y = c("age_group", "sex", "year"),
                     all.x = TRUE, sort = FALSE)
    data.table::setnames(ru_grid, "rate", lag_col)
    ru_grid[is.na(get(lag_col)), (lag_col) := 0]
  }
  ru_grid[, lag_year := NULL]
  ru_lag_cols <- paste0("ru_lag", 0:5)

  # ============================================================================
  # SECTION 1: Ages 16-19 (young) — fully vectorized
  # ============================================================================

  young_specs <- list()
  for (sex_name in c("male", "female")) {
    for (ag in c("16-17", "18-19")) {
      coeffs <- lfpr_coefficients[[sex_name]][[ag]]
      young_specs[[length(young_specs) + 1L]] <- list(
        age_group = ag, sex = sex_name,
        ru_lags = coeffs$ru_lags,
        trend_coeff = coeffs$trend_coeff,
        trend_offset = coeffs$trend_offset,
        intercept = coeffs$intercept,
        c6u_coeff = coeffs$child_under6_coeff %||% 0,
        c6u_offset = coeffs$child_under6_offset %||% 0,
        has_c6u = !is.null(coeffs$child_under6_coeff)
      )
    }
  }

  young_dt <- data.table::rbindlist(lapply(young_specs, function(s) {
    data.table::data.table(
      age_group = s$age_group, sex = s$sex,
      rl0 = s$ru_lags[1], rl1 = s$ru_lags[2], rl2 = s$ru_lags[3],
      rl3 = s$ru_lags[4], rl4 = s$ru_lags[5], rl5 = s$ru_lags[6],
      trend_coeff = s$trend_coeff, trend_offset = s$trend_offset,
      intercept = s$intercept,
      c6u_coeff = s$c6u_coeff, c6u_offset = s$c6u_offset,
      has_c6u = s$has_c6u
    )
  }))

  # Cross with projection years
  young_grid <- young_dt[, .(year = projection_years), by = names(young_dt)]

  # Merge RU lags
  young_grid <- merge(young_grid, ru_grid,
                       by = c("age_group", "sex", "year"), all.x = TRUE, sort = FALSE)

  # Merge RD (use 5-year grouped RD for 5-year age groups)
  if (!is.null(rd_5yr)) {
    young_grid <- merge(young_grid, rd_5yr[, .(age_group, sex, year, rd)],
                         by = c("age_group", "sex", "year"), all.x = TRUE, sort = FALSE)
    young_grid[is.na(rd), rd := 0]
  } else {
    young_grid[, rd := 0]
  }

  # Merge children proportions (female only, for child_under6 effect)
  if (!is.null(children_props)) {
    cp_unique <- unique(children_props[, .(age_group, year, proportion)])
    young_grid <- merge(young_grid, cp_unique,
                         by = c("age_group", "year"), all.x = TRUE, sort = FALSE)
    young_grid[is.na(proportion), proportion := 0]
  } else {
    young_grid[, proportion := 0]
  }

  # Merge calibrated trend base values
  young_cal <- trend_calibration[type == "young", .(age_group, sex, calibrated_tr_base)]
  young_grid <- merge(young_grid, young_cal,
                       by = c("age_group", "sex"), all.x = TRUE, sort = FALSE)
  young_grid[is.na(calibrated_tr_base), calibrated_tr_base := 0]

  # Compute LFPR — apply per-group trend adjustments
  young_grid[, trend_increment := year - base_year]
  if (!is.null(trend_adjustments)) {
    for (ag_name in intersect(names(trend_adjustments), unique(young_grid$age_group))) {
      ag_adj <- trend_adjustments[[ag_name]]
      for (sx in c("male", "female")) {
        sx_adj <- ag_adj[[sx]]
        if (!is.null(sx_adj)) {
          ta_rate <- sx_adj$rate %||% 1.0
          ta_max <- sx_adj$max_years %||% .Machine$integer.max
          young_grid[age_group == ag_name & sex == sx,
                     trend_increment := pmin(trend_increment, ta_max) * ta_rate]
        }
      }
    }
  }
  young_grid[, trend_val := calibrated_tr_base + trend_increment]
  young_grid[, trend_increment := NULL]
  young_grid[, ru_effect := rl0 * ru_lag0 + rl1 * ru_lag1 + rl2 * ru_lag2 +
                            rl3 * ru_lag3 + rl4 * ru_lag4 + rl5 * ru_lag5]
  young_grid[, trend_effect := trend_coeff * trend_val + trend_offset]
  young_grid[, c6u_effect := fifelse(has_c6u, c6u_coeff * proportion + c6u_offset, 0)]
  young_grid[, lfpr := (ru_effect + trend_effect + c6u_effect + intercept) / (1 + rd)]

  # Apply addfactors to young ages
  if (!is.null(addfactors)) {
    for (ag_name in names(addfactors)) {
      for (sx in c("male", "female")) {
        af_val <- addfactors[[ag_name]][[sx]]
        if (!is.null(af_val)) {
          young_grid[age_group == ag_name & sex == sx, lfpr := lfpr + af_val]
        }
      }
    }
  }

  young_detailed <- young_grid[, .(year, age_group, sex,
                                    marital_status = "all", child_status = "all",
                                    lfpr, type = "young")]

  # ============================================================================
  # SECTION 2: Ages 20-54 male (marital status disaggregation) — vectorized
  # ============================================================================

  male_marital_ags <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")
  marital_statuses <- c("never_married", "married_present", "married_absent")

  # Build coefficient table: one row per (age_group, marital_status)
  male_marital_specs <- list()
  for (ag in male_marital_ags) {
    coeffs <- lfpr_coefficients$male[[ag]]
    if (is.null(coeffs) || coeffs$type != "marital") next
    for (ms in marital_statuses) {
      ms_coeffs <- coeffs[[ms]]
      male_marital_specs[[length(male_marital_specs) + 1L]] <- data.table::data.table(
        age_group = ag, sex = "male", marital_status = ms,
        rl0 = ms_coeffs$ru_lags[1], rl1 = ms_coeffs$ru_lags[2], rl2 = ms_coeffs$ru_lags[3],
        rl3 = ms_coeffs$ru_lags[4], rl4 = ms_coeffs$ru_lags[5], rl5 = ms_coeffs$ru_lags[6],
        trend_coeff = ms_coeffs$trend_coeff %||% 0,
        intercept = ms_coeffs$intercept
      )
    }
  }
  male_coeff_dt <- data.table::rbindlist(male_marital_specs)

  # Cross with projection years
  male_grid <- male_coeff_dt[, .(year = projection_years), by = names(male_coeff_dt)]

  # Merge RU lags
  male_grid <- merge(male_grid, ru_grid[sex == "male"],
                      by = c("age_group", "sex", "year"), all.x = TRUE, sort = FALSE)

  # Merge RD (use 5-year grouped RD for 5-year age groups)
  if (!is.null(rd_5yr)) {
    male_grid <- merge(male_grid, rd_5yr[sex == "male", .(age_group, year, rd)],
                        by = c("age_group", "year"), all.x = TRUE, sort = FALSE)
    male_grid[is.na(rd), rd := 0]
  } else {
    male_grid[, rd := 0]
  }

  # Merge calibrated trend base values (only 20-24 and 25-29 have trend_coeff)
  male_cal <- trend_calibration[type == "marital" & sex == "male",
    .(age_group, marital_status, calibrated_tr_base)]
  male_grid <- merge(male_grid, male_cal,
                      by = c("age_group", "marital_status"), all.x = TRUE, sort = FALSE)
  male_grid[is.na(calibrated_tr_base), calibrated_tr_base := 0]

  # Compute per-status LFPR — apply per-group trend adjustments
  male_grid[, trend_increment := year - base_year]
  if (!is.null(trend_adjustments)) {
    for (ag_name in intersect(names(trend_adjustments), unique(male_grid$age_group))) {
      male_adj <- trend_adjustments[[ag_name]]$male
      if (!is.null(male_adj)) {
        ta_rate <- male_adj$rate %||% 1.0
        ta_max <- male_adj$max_years %||% .Machine$integer.max
        male_grid[age_group == ag_name,
                  trend_increment := pmin(trend_increment, ta_max) * ta_rate]
      }
    }
  }
  male_grid[, trend_val := calibrated_tr_base + trend_increment]
  male_grid[, trend_increment := NULL]
  male_grid[, ru_effect := rl0 * ru_lag0 + rl1 * ru_lag1 + rl2 * ru_lag2 +
                           rl3 * ru_lag3 + rl4 * ru_lag4 + rl5 * ru_lag5]
  male_grid[, lfpr_raw := (ru_effect + trend_coeff * trend_val + intercept) / (1 + rd)]

  # Merge marital population for weighting
  # marital_pop has single-year ages; aggregate to age_group
  if (!is.null(marital_pop)) {
    marital_pop_grouped <- marital_pop[sex == "male" & year %in% projection_years,
      .(population = sum(population, na.rm = TRUE)),
      by = .(year, lfpr_marital_status,
             age_group = fcase(
               age <= 24L, "20-24", age <= 29L, "25-29", age <= 34L, "30-34",
               age <= 39L, "35-39", age <= 44L, "40-44", age <= 49L, "45-49",
               age <= 54L, "50-54", default = NA_character_
             ))][!is.na(age_group)]
    male_grid <- merge(male_grid,
                        marital_pop_grouped[, .(age_group, year, marital_status = lfpr_marital_status, population)],
                        by = c("age_group", "year", "marital_status"), all.x = TRUE, sort = FALSE)
    male_grid[is.na(population) | population <= 0, population := 1]
  } else {
    male_grid[, population := 1]
  }

  # Aggregate: pop-weighted LFPR per (age_group, year)
  male_grid[, pop_total := sum(population), by = .(age_group, year)]
  male_grid[, lfpr_agg := sum(lfpr_raw * population) / pop_total, by = .(age_group, year)]

  # Apply addfactors
  if (!is.null(addfactors)) {
    for (ag_name in names(addfactors)) {
      af_male <- addfactors[[ag_name]]$male
      if (!is.null(af_male)) {
        male_grid[age_group == ag_name, lfpr_agg := lfpr_agg + af_male]
      }
    }
  }

  # Rescale: scale_factor = lfpr_agg_after_addfactor / lfpr_agg_before_addfactor
  male_grid[, lfpr_agg_raw := sum(lfpr_raw * population) / pop_total, by = .(age_group, year)]
  male_grid[, scale_factor := fifelse(lfpr_agg_raw > 0, lfpr_agg / lfpr_agg_raw, 1)]
  male_grid[, lfpr := lfpr_raw * scale_factor]

  male_detailed <- male_grid[, .(year, age_group, sex = "male",
                                  marital_status, child_status = "all",
                                  lfpr, type = "marital")]
  male_aggregate <- male_grid[, .(lfpr = lfpr_agg[1L]),
                               by = .(year, age_group, sex)]

  # ============================================================================
  # SECTION 3: Ages 20-44 female (marital x children) — vectorized
  # ============================================================================

  female_mc_ags <- c("20-24", "25-29", "30-34", "35-39", "40-44")
  mc_categories <- c(
    "never_married_child_under6", "never_married_no_child",
    "married_present_child_under6", "married_present_no_child",
    "married_absent_child_under6", "married_absent_no_child"
  )

  female_mc_specs <- list()
  for (ag in female_mc_ags) {
    coeffs <- lfpr_coefficients$female[[ag]]
    if (is.null(coeffs) || coeffs$type != "marital_children") next
    for (cat in mc_categories) {
      cat_coeffs <- coeffs[[cat]]
      female_mc_specs[[length(female_mc_specs) + 1L]] <- data.table::data.table(
        age_group = ag, sex = "female", category = cat,
        marital_status = sub("_(child_under6|no_child)$", "", cat),
        child_status = sub("^.*_(child_under6|no_child)$", "\\1", cat),
        rl0 = cat_coeffs$ru_lags[1], rl1 = cat_coeffs$ru_lags[2], rl2 = cat_coeffs$ru_lags[3],
        rl3 = cat_coeffs$ru_lags[4], rl4 = cat_coeffs$ru_lags[5], rl5 = cat_coeffs$ru_lags[6],
        trend_coeff = cat_coeffs$trend_coeff %||% 0,
        intercept = cat_coeffs$intercept,
        is_child_under6 = grepl("child_under6$", cat)
      )
    }
  }
  fmc_coeff_dt <- data.table::rbindlist(female_mc_specs)

  # Cross with years
  fmc_grid <- fmc_coeff_dt[, .(year = projection_years), by = names(fmc_coeff_dt)]

  # Merge RU lags
  fmc_grid <- merge(fmc_grid, ru_grid[sex == "female"],
                     by = c("age_group", "sex", "year"), all.x = TRUE, sort = FALSE)

  # Merge RD (use 5-year grouped RD for 5-year age groups)
  if (!is.null(rd_5yr)) {
    fmc_grid <- merge(fmc_grid, rd_5yr[sex == "female", .(age_group, year, rd)],
                       by = c("age_group", "year"), all.x = TRUE, sort = FALSE)
    fmc_grid[is.na(rd), rd := 0]
  } else {
    fmc_grid[, rd := 0]
  }

  # Merge calibrated trend base values (only no_child categories in 20-24, 25-29 have trend)
  fmc_cal <- trend_calibration[type == "marital_children" & sex == "female",
    .(age_group, category, calibrated_tr_base)]
  fmc_grid <- merge(fmc_grid, fmc_cal,
                     by = c("age_group", "category"), all.x = TRUE, sort = FALSE)
  fmc_grid[is.na(calibrated_tr_base), calibrated_tr_base := 0]

  # Compute per-category LFPR — apply per-group trend adjustments
  fmc_grid[, trend_increment := year - base_year]
  if (!is.null(trend_adjustments)) {
    for (ag_name in intersect(names(trend_adjustments), unique(fmc_grid$age_group))) {
      female_adj <- trend_adjustments[[ag_name]]$female
      if (!is.null(female_adj)) {
        ta_rate <- female_adj$rate %||% 1.0
        ta_max <- female_adj$max_years %||% .Machine$integer.max
        fmc_grid[age_group == ag_name,
                 trend_increment := pmin(trend_increment, ta_max) * ta_rate]
      }
    }
  }
  fmc_grid[, trend_val := calibrated_tr_base + trend_increment]
  fmc_grid[, trend_increment := NULL]
  fmc_grid[, ru_effect := rl0 * ru_lag0 + rl1 * ru_lag1 + rl2 * ru_lag2 +
                          rl3 * ru_lag3 + rl4 * ru_lag4 + rl5 * ru_lag5]
  fmc_grid[, lfpr_raw := (ru_effect + trend_coeff * trend_val + intercept) / (1 + rd)]

  # Compute population by category: marital_pop × children_proportion
  if (!is.null(marital_pop)) {
    fmc_marital_pop <- marital_pop[sex == "female" & year %in% projection_years &
                                     age <= 44L & age >= 20L,
      .(population = sum(population, na.rm = TRUE)),
      by = .(year, lfpr_marital_status,
             age_group = fcase(
               age <= 24L, "20-24", age <= 29L, "25-29", age <= 34L, "30-34",
               age <= 39L, "35-39", age <= 44L, "40-44", default = NA_character_
             ))][!is.na(age_group)]

    fmc_grid <- merge(fmc_grid,
                       fmc_marital_pop[, .(age_group, year,
                                           marital_status = lfpr_marital_status,
                                           ms_population = population)],
                       by = c("age_group", "year", "marital_status"),
                       all.x = TRUE, sort = FALSE)
    fmc_grid[is.na(ms_population) | ms_population <= 0, ms_population := 1]
  } else {
    fmc_grid[, ms_population := 1]
  }

  # Children proportion split
  if (!is.null(children_props)) {
    cp_unique_fmc <- unique(children_props[, .(age_group, year, proportion)])
    fmc_grid <- merge(fmc_grid, cp_unique_fmc,
                       by = c("age_group", "year"), all.x = TRUE, sort = FALSE)
    fmc_grid[is.na(proportion), proportion := 0]
  } else {
    fmc_grid[, proportion := 0]
  }

  fmc_grid[, cat_population := fifelse(is_child_under6, ms_population * proportion,
                                        ms_population * (1 - proportion))]
  fmc_grid[cat_population <= 0, cat_population := 1]

  # Aggregate
  fmc_grid[, pop_total := sum(cat_population), by = .(age_group, year)]
  fmc_grid[, lfpr_agg := sum(lfpr_raw * cat_population) / pop_total, by = .(age_group, year)]

  # Addfactors
  if (!is.null(addfactors)) {
    for (ag_name in names(addfactors)) {
      af_female <- addfactors[[ag_name]]$female
      if (!is.null(af_female)) {
        fmc_grid[age_group == ag_name, lfpr_agg := lfpr_agg + af_female]
      }
    }
  }

  fmc_grid[, lfpr_agg_raw := sum(lfpr_raw * cat_population) / pop_total, by = .(age_group, year)]
  fmc_grid[, scale_factor := fifelse(lfpr_agg_raw > 0, lfpr_agg / lfpr_agg_raw, 1)]
  fmc_grid[, lfpr := lfpr_raw * scale_factor]

  fmc_detailed <- fmc_grid[, .(year, age_group, sex = "female",
                                marital_status, child_status,
                                lfpr, type = "marital_children")]
  fmc_aggregate <- fmc_grid[, .(lfpr = lfpr_agg[1L]),
                              by = .(year, age_group, sex)]

  # ============================================================================
  # SECTION 4: Ages 45-54 female (marital, no children) — vectorized
  # ============================================================================

  f4554_specs <- list()
  for (ag in c("45-49", "50-54")) {
    coeffs <- lfpr_coefficients$female[[ag]]
    if (is.null(coeffs) || coeffs$type != "marital") next
    for (ms in marital_statuses) {
      ms_coeffs <- coeffs[[ms]]
      f4554_specs[[length(f4554_specs) + 1L]] <- data.table::data.table(
        age_group = ag, sex = "female", marital_status = ms,
        rl0 = ms_coeffs$ru_lags[1], rl1 = ms_coeffs$ru_lags[2], rl2 = ms_coeffs$ru_lags[3],
        rl3 = ms_coeffs$ru_lags[4], rl4 = ms_coeffs$ru_lags[5], rl5 = ms_coeffs$ru_lags[6],
        edscore_coeff = ms_coeffs$edscore_coeff %||% 0,
        has_edscore = !is.null(ms_coeffs$edscore_coeff),
        intercept = ms_coeffs$intercept
      )
    }
  }
  f4554_coeff_dt <- data.table::rbindlist(f4554_specs)

  # Cross with years
  f4554_grid <- f4554_coeff_dt[, .(year = projection_years), by = names(f4554_coeff_dt)]

  # Merge RU lags
  f4554_grid <- merge(f4554_grid, ru_grid[sex == "female"],
                       by = c("age_group", "sex", "year"), all.x = TRUE, sort = FALSE)

  # Merge RD (use 5-year grouped RD for 5-year age groups)
  if (!is.null(rd_5yr)) {
    f4554_grid <- merge(f4554_grid, rd_5yr[sex == "female", .(age_group, year, rd)],
                         by = c("age_group", "year"), all.x = TRUE, sort = FALSE)
    f4554_grid[is.na(rd), rd := 0]
  } else {
    f4554_grid[, rd := 0]
  }

  # Merge EDSCORE (for married_present 50-54 which has edscore_coeff)
  if (!is.null(edscore)) {
    f4554_grid <- merge(f4554_grid,
                         edscore[sex == "female" & age_group %in% c("45-49", "50-54"),
                                 .(age_group, year, edscore_val = edscore)],
                         by = c("age_group", "year"), all.x = TRUE, sort = FALSE)
    f4554_grid[is.na(edscore_val), edscore_val := 1]
  } else {
    f4554_grid[, edscore_val := 1]
  }

  # Compute
  f4554_grid[, ru_effect := rl0 * ru_lag0 + rl1 * ru_lag1 + rl2 * ru_lag2 +
                            rl3 * ru_lag3 + rl4 * ru_lag4 + rl5 * ru_lag5]
  f4554_grid[, edscore_effect := fifelse(has_edscore, edscore_coeff * edscore_val, 0)]
  f4554_grid[, lfpr_raw := (ru_effect + edscore_effect + intercept) / (1 + rd)]

  # Population weights
  if (!is.null(marital_pop)) {
    f4554_marital_pop <- marital_pop[sex == "female" & year %in% projection_years &
                                       age >= 45L & age <= 54L,
      .(population = sum(population, na.rm = TRUE)),
      by = .(year, lfpr_marital_status,
             age_group = fcase(
               age <= 49L, "45-49", age <= 54L, "50-54", default = NA_character_
             ))][!is.na(age_group)]
    f4554_grid <- merge(f4554_grid,
                         f4554_marital_pop[, .(age_group, year,
                                               marital_status = lfpr_marital_status,
                                               population)],
                         by = c("age_group", "year", "marital_status"),
                         all.x = TRUE, sort = FALSE)
    f4554_grid[is.na(population) | population <= 0, population := 1]
  } else {
    f4554_grid[, population := 1]
  }

  f4554_grid[, pop_total := sum(population), by = .(age_group, year)]
  f4554_grid[, lfpr_agg := sum(lfpr_raw * population) / pop_total, by = .(age_group, year)]

  if (!is.null(addfactors)) {
    for (ag_name in names(addfactors)) {
      af_female <- addfactors[[ag_name]]$female
      if (!is.null(af_female)) {
        f4554_grid[age_group == ag_name, lfpr_agg := lfpr_agg + af_female]
      }
    }
  }

  f4554_grid[, lfpr_agg_raw := sum(lfpr_raw * population) / pop_total, by = .(age_group, year)]
  f4554_grid[, scale_factor := fifelse(lfpr_agg_raw > 0, lfpr_agg / lfpr_agg_raw, 1)]
  f4554_grid[, lfpr := lfpr_raw * scale_factor]

  f4554_detailed <- f4554_grid[, .(year, age_group, sex = "female",
                                    marital_status, child_status = "all",
                                    lfpr, type = "marital")]
  f4554_aggregate <- f4554_grid[, .(lfpr = lfpr_agg[1L]),
                                  by = .(year, age_group, sex)]

  # ============================================================================
  # SECTION 5: Ages 55-74 (older / retirement) — fully vectorized
  # ============================================================================

  older_specs <- list()
  for (sex_name in c("male", "female")) {
    sex_coeffs <- lfpr_coefficients[[sex_name]]
    for (age_val in 55:74) {
      ag <- as.character(age_val)
      coeffs <- sex_coeffs[[ag]]
      if (is.null(coeffs)) next
      older_specs[[length(older_specs) + 1L]] <- data.table::data.table(
        age = age_val, age_group = ag, sex = sex_name,
        edscore_coeff = coeffs$edscore_coeff,
        msshare_coeff = coeffs$msshare_coeff,
        intercept_val = coeffs$intercept,
        eq_type = coeffs$type,
        rradj_coeff = coeffs$rradj_coeff %||% 0,
        pot_et_coeff = coeffs$pot_et_coeff %||% 0,
        is_retirement = (coeffs$type == "retirement")
      )
    }
  }
  older_coeff_dt <- data.table::rbindlist(older_specs)

  # Cross with projection years
  older_grid <- older_coeff_dt[, .(year = projection_years), by = names(older_coeff_dt)]

  # Merge EDSCORE by single-year age_group (e.g., "55", "56", ..., "74")
  if (!is.null(edscore)) {
    older_grid <- merge(older_grid,
                         edscore[, .(age_group, sex, year, edscore_val = edscore)],
                         by = c("age_group", "sex", "year"), all.x = TRUE, sort = FALSE)
    older_grid[is.na(edscore_val), edscore_val := 1]
  } else {
    older_grid[, edscore_val := 1]
  }

  # Merge MSSHARE (uses integer age)
  if (!is.null(msshare)) {
    older_grid <- merge(older_grid,
                         msshare[, .(age, sex, year, msshare_val = msshare)],
                         by = c("age", "sex", "year"), all.x = TRUE, sort = FALSE)
    older_grid[is.na(msshare_val), msshare_val := 0.5]
  } else {
    older_grid[, msshare_val := 0.5]
  }

  # Cap MSSHARE deviation from base year to prevent coefficient extrapolation
  if (!is.null(msshare_max_dev)) {
    base_msshare <- older_grid[year == (base_year + 1L),
                               .(age, sex, msshare_base = msshare_val)]
    base_msshare <- unique(base_msshare, by = c("age", "sex"))
    older_grid <- merge(older_grid, base_msshare,
                        by = c("age", "sex"), all.x = TRUE, sort = FALSE)
    older_grid[!is.na(msshare_base),
               msshare_val := pmin(pmax(msshare_val,
                                         msshare_base - msshare_max_dev),
                                    msshare_base + msshare_max_dev)]
    older_grid[, msshare_base := NULL]
  }

  # Merge RD: for ages 62+, use cohort RD at age 61 from year - (age - 61)
  # For ages 55-61, use RD at current age and year
  older_grid[, rd_age_group := fifelse(age >= 62L, "61", age_group)]
  older_grid[, rd_year := fifelse(age >= 62L, year - (age - 61L), year)]
  if (!is.null(rd)) {
    older_grid <- merge(older_grid,
                         rd[, .(rd_age_group = age_group, sex, rd_year = year, rd_val = rd)],
                         by = c("rd_age_group", "sex", "rd_year"), all.x = TRUE, sort = FALSE)
    older_grid[is.na(rd_val), rd_val := 0]
  } else {
    older_grid[, rd_val := 0]
  }

  # Merge RRADJ (ages 62-69, uses integer age)
  if (!is.null(rradj)) {
    older_grid <- merge(older_grid,
                         rradj[, .(age, sex, year, rradj_val = rradj)],
                         by = c("age", "sex", "year"), all.x = TRUE, sort = FALSE)
    older_grid[is.na(rradj_val), rradj_val := 0]
  } else {
    older_grid[, rradj_val := 0]
  }

  # Merge POT_ET_TXRT (ages 62-69, uses integer age)
  if (!is.null(pot_et_txrt)) {
    older_grid <- merge(older_grid,
                         pot_et_txrt[, .(age, year, pot_et_val = pot_et_txrt)],
                         by = c("age", "year"), all.x = TRUE, sort = FALSE)
    older_grid[is.na(pot_et_val), pot_et_val := 0]
  } else {
    older_grid[, pot_et_val := 0]
  }

  # Compute LFPR
  older_grid[, lfpr := edscore_coeff * edscore_val +
                        msshare_coeff * msshare_val +
                        intercept_val]
  older_grid[is_retirement == TRUE,
              lfpr := lfpr + rradj_coeff * rradj_val + pot_et_coeff * pot_et_val]
  older_grid[, lfpr := lfpr / (1 + rd_val)]

  # Apply addfactors
  if (!is.null(addfactors)) {
    for (ag_name in names(addfactors)) {
      for (sx in c("male", "female")) {
        af_val <- addfactors[[ag_name]][[sx]]
        if (!is.null(af_val)) {
          older_grid[age_group == ag_name & sex == sx, lfpr := lfpr + af_val]
        }
      }
    }
  }

  older_detailed <- older_grid[, .(year, age_group, sex,
                                    marital_status = "all", child_status = "all",
                                    lfpr, type = eq_type)]
  older_aggregate <- older_grid[, .(year, age_group, sex, lfpr)]

  # ============================================================================
  # SECTION 6: Ages 75-100 (cohort decay) — year loop with vectorized ages
  # ============================================================================
  # This section must loop over years because each year depends on the prior year.
  # However, within each year we use plain vector indexing, not data.table lookups.

  # Build a lookup matrix for ages 55-100 x 2 sexes x all years
  # Index: [year_idx, age - 54, sex_idx]  where sex_idx: male=1, female=2
  # year range: base_year (for seed values) through end_year
  all_years <- base_year:end_year
  n_all_years <- length(all_years)
  n_ages_55_100 <- 46  # ages 55 through 100

  # Initialize LFPR matrices: [year_offset, age_offset] where year_offset = year - base_year + 1
  # and age_offset = age - 54
  lfpr_mat_male <- matrix(0, nrow = n_all_years, ncol = n_ages_55_100)
  lfpr_mat_female <- matrix(0, nrow = n_all_years, ncol = n_ages_55_100)

  # Fill in ages 55-74 from the vectorized older_grid results using matrix indexing
  for (sex_name in c("male", "female")) {
    mat <- if (sex_name == "male") lfpr_mat_male else lfpr_mat_female
    sub <- older_grid[sex == sex_name, .(year, age, lfpr)]
    idx_mat <- cbind(sub$year - base_year + 1L, sub$age - 54L)
    mat[idx_mat] <- sub$lfpr
    if (sex_name == "male") lfpr_mat_male <- mat else lfpr_mat_female <- mat
  }

  # Also need seed values for base_year from historical data (for cohort decay of year base_year+1)
  # historical_lfpr may contain age 74 at base_year for the first 75 computation
  # The older_grid already contains projection years, but we need base_year values for
  # the decay chain. Try to get from historical_lfpr if available.
  if (is.data.table(historical_lfpr)) {
    for (sex_name in c("male", "female")) {
      mat <- if (sex_name == "male") lfpr_mat_male else lfpr_mat_female
      # historical_lfpr uses 'value' column (from CPS data) and may have
      # both single-year and group entries — use only single-year (has 'age' col)
      hist_sub <- historical_lfpr[sex == sex_name & year == base_year &
                                    !is.na(age) & age >= 55 & age <= 100,
                                  .(age, lfpr_val = value)]
      hist_sub <- unique(hist_sub, by = "age")
      for (i in seq_len(nrow(hist_sub))) {
        age_idx <- hist_sub$age[i] - 54L
        mat[1L, age_idx] <- hist_sub$lfpr_val[i]
      }
      if (sex_name == "male") lfpr_mat_male <- mat else lfpr_mat_female <- mat
    }
  }

  # Pre-build population matrix for 80+ aggregation: pop_mat[year_offset, age-79]
  # where age-79 goes from 1 (age 80) to 21 (age 100)
  # This avoids repeated data.table filtering inside the year loop
  pop_80_mat_male <- matrix(0, nrow = n_all_years, ncol = 21L)
  pop_80_mat_female <- matrix(0, nrow = n_all_years, ncol = 21L)
  cni_80_plus <- cni_population[age >= 80L & age <= 100L & year %in% all_years,
    .(population = sum(population, na.rm = TRUE)), by = .(year, age, sex)]
  if (nrow(cni_80_plus) > 0) {
    for (sx in c("male", "female")) {
      pop_mat <- if (sx == "male") pop_80_mat_male else pop_80_mat_female
      sub <- cni_80_plus[sex == sx]
      if (nrow(sub) > 0) {
        idx_mat <- cbind(sub$year - base_year + 1L, sub$age - 79L)
        valid <- idx_mat[, 1] >= 1L & idx_mat[, 1] <= n_all_years &
                 idx_mat[, 2] >= 1L & idx_mat[, 2] <= 21L
        if (any(valid)) {
          pop_mat[idx_mat[valid, , drop = FALSE]] <- sub$population[valid]
        }
      }
      if (sx == "male") pop_80_mat_male <- pop_mat else pop_80_mat_female <- pop_mat
    }
  }

  # Pre-compute decay power factors
  decay_powers_80 <- decay_80_plus^(1:21)  # for ages 80-100 (years_from_79 = 1..21)

  # Pre-allocate character age labels
  ages_75_100_char <- as.character(75:100)
  type_labels_75_100 <- fifelse(75:100 <= 79, "cohort_decay_75", "cohort_decay_80")

  # Now loop over projection years for ages 75-100
  decay_results <- vector("list", n_proj_years * 2L)
  decay_agg_results <- vector("list", n_proj_years * 2L)
  result_idx <- 0L

  for (yr in projection_years) {
    yr_idx <- yr - base_year + 1L
    prev_yr_idx <- yr_idx - 1L

    for (sex_name in c("male", "female")) {
      mat <- if (sex_name == "male") lfpr_mat_male else lfpr_mat_female
      decay_factor_75 <- decay_75_79[[sex_name]]

      # Ages 75-79: prior year's (age-1) value * decay_factor
      # age 75 -> prior age 74 (idx 20), ..., age 79 -> prior age 78 (idx 24)
      for (age_idx in 21:25) {  # 75-54=21 through 79-54=25
        mat[yr_idx, age_idx] <- mat[prev_yr_idx, age_idx - 1L] * decay_factor_75
      }

      # Ages 80-84: PM79 from (years_from_79) years ago * decay^years_from_79
      age79_idx <- 25L  # 79 - 54
      for (yf79 in 1:5) {  # ages 80-84
        src_yr_idx <- yr_idx - yf79
        base_val <- if (src_yr_idx >= 1L) mat[src_yr_idx, age79_idx] else 0
        mat[yr_idx, 25L + yf79] <- base_val * decay_powers_80[yf79]
      }

      # Ages 85-94: 2-year moving average of PM79 * decay^years_from_79
      for (yf79 in 6:15) {  # ages 85-94
        lag1 <- yr_idx - yf79
        lag2 <- lag1 - 1L
        val1 <- if (lag1 >= 1L) mat[lag1, age79_idx] else 0
        val2 <- if (lag2 >= 1L) mat[lag2, age79_idx] else 0
        mat[yr_idx, 25L + yf79] <- ((val1 + val2) / 2) * decay_powers_80[yf79]
      }

      # Ages 95-100: chain from prior age at same year
      for (age_idx in 41:46) {  # 95-54=41 through 100-54=46
        mat[yr_idx, age_idx] <- mat[yr_idx, age_idx - 1L] * decay_80_plus
      }

      # Write back
      if (sex_name == "male") lfpr_mat_male <- mat else lfpr_mat_female <- mat

      # Collect results for ages 75-100
      lfpr_vals <- mat[yr_idx, 21:46]  # ages 75-100

      result_idx <- result_idx + 1L
      decay_results[[result_idx]] <- data.table::data.table(
        year = yr,
        age_group = ages_75_100_char,
        sex = sex_name,
        marital_status = "all",
        child_status = "all",
        lfpr = lfpr_vals,
        type = type_labels_75_100
      )

      # 80+ aggregate using pre-built population matrix
      pop_mat <- if (sex_name == "male") pop_80_mat_male else pop_80_mat_female
      pop_80_vals <- pop_mat[yr_idx, ]  # 21 values for ages 80-100
      lfpr_80_vals <- mat[yr_idx, 26:46]  # ages 80-100 = indices 26..46
      total_pop_80 <- sum(pop_80_vals)
      lfpr_80o <- if (total_pop_80 > 0) sum(lfpr_80_vals * pop_80_vals) / total_pop_80 else 0

      # Per-age aggregate entries plus 80+ aggregate
      decay_agg_results[[result_idx]] <- data.table::data.table(
        year = c(rep(yr, 26L), yr),
        age_group = c(ages_75_100_char, "80+"),
        sex = sex_name,
        lfpr = c(lfpr_vals, lfpr_80o)
      )
    }
  }

  decay_detailed <- data.table::rbindlist(decay_results[seq_len(result_idx)])
  decay_aggregate <- data.table::rbindlist(decay_agg_results[seq_len(result_idx)])

  # ============================================================================
  # Combine all sections
  # ============================================================================

  # Young ages also go into aggregate (one row per age_group x sex x year)
  young_aggregate <- young_detailed[, .(year, age_group, sex, lfpr)]

  detailed <- data.table::rbindlist(list(
    young_detailed, male_detailed, fmc_detailed, f4554_detailed,
    older_detailed, decay_detailed
  ), fill = TRUE)

  aggregate <- data.table::rbindlist(list(
    young_aggregate, male_aggregate, fmc_aggregate, f4554_aggregate,
    older_aggregate, decay_aggregate
  ), fill = TRUE)

  # Clamp LFPR to [0, 1]
  detailed[lfpr < 0, lfpr := 0]
  detailed[lfpr > 1, lfpr := 1]
  aggregate[lfpr < 0, lfpr := 0]
  aggregate[lfpr > 1, lfpr := 1]

  # Quarterly interpolation (uniform distribution across quarters)
  quarterly <- aggregate[, .(
    quarter = 1:4,
    lfpr = lfpr
  ), by = .(year, age_group, sex)]

  cli::cli_alert_success("Projected LFPR: {nrow(detailed)} detailed rows, {nrow(aggregate)} aggregate rows")

  list(
    aggregate = aggregate,
    detailed = detailed,
    quarterly = quarterly
  )
}

# =============================================================================
# Labor Force and Employment (Eqs 2.1.5, 2.1.6)
# =============================================================================

#' Project labor force and employment
#'
#' @description
#' Computes labor force (LC = LFPR × N) and employment (E = LC × (1 - RU/100))
#' by age group and sex, quarterly.
#'
#' @param lfpr_projection LFPR projection from project_lfpr()
#' @param unemployment_projection Unemployment projection from project_unemployment_rates()
#' @param quarterly_cni_pop Quarterly civilian noninstitutional population
#'
#' @return List with:
#'   - `labor_force`: data.table (year, quarter, age_group, sex, labor_force)
#'   - `employment`: data.table (year, quarter, age_group, sex, employment)
#'   - `full_employment`: full-employment variant of above
#'
#' @references Eqs 2.1.5-2.1.6
#' @export
project_labor_force_employment <- function(lfpr_projection,
                                            unemployment_projection,
                                            quarterly_cni_pop) {
  checkmate::assert_list(lfpr_projection)
  checkmate::assert_list(unemployment_projection)
  checkmate::assert_data_table(quarterly_cni_pop)

  # Use aggregate LFPR only (one value per age_group × sex × year × quarter)
  # The detailed LFPR has marital status disaggregation — not needed for total LC/E
  lfpr_q <- data.table::copy(lfpr_projection$quarterly)

  # Deduplicate: keep only the aggregate (first) value per age-sex-quarter
  lfpr_q <- unique(lfpr_q, by = c("year", "quarter", "age_group", "sex"))

  ru_q <- unemployment_projection$actual

  # Aggregate quarterly CNI population from single-year ages to LFPR age groups
  # Filter to working-age population (16+) — ages 0-15 are not in the labor force
  cni_pop <- data.table::copy(quarterly_cni_pop)
  cni_pop <- cni_pop[age >= 16]
  cni_pop[, age_group := fcase(
    age <= 17, "16-17", age <= 19, "18-19", age <= 24, "20-24",
    age <= 29, "25-29", age <= 34, "30-34", age <= 39, "35-39",
    age <= 44, "40-44", age <= 49, "45-49", age <= 54, "50-54",
    age <= 59, "55-59", age <= 64, "60-64", age <= 69, "65-69",
    age <= 74, "70-74", default = "75+"
  )]
  # Single-year age groups for ages 55+ (LFPR uses single-year resolution)
  cni_single <- cni_pop[age >= 55, .(population = sum(population)),
                        by = .(year, quarter, age_group = as.character(age), sex)]
  # 5-year groups for ages 16-54 (55+ uses single-year from above)
  cni_grouped <- cni_pop[age <= 54, .(population = sum(population)),
                         by = .(year, quarter, age_group, sex)]
  # Combine (no 80+ aggregate — single-year ages already cover each age individually)
  cni_by_group <- data.table::rbindlist(list(cni_grouped, cni_single),
                                         use.names = TRUE)

  # Map single-year LFPR ages (55-100, 80+) to UR 5-year groups for the merge
  # LFPR uses single-year ages for 55+, but UR uses 5-year groups (55-59, 60-64, etc.)
  ur_age_lookup <- data.table::data.table(
    age_group = c(USEMP_LFPR_5YR_GROUPS, as.character(USEMP_LFPR_SINGLE_AGES), "80+"),
    ur_age_group = c(
      USEMP_LFPR_5YR_GROUPS,  # 16-17 through 50-54 map to themselves
      rep("55-59", 5), rep("60-64", 5), rep("65-69", 5), rep("70-74", 5),
      rep("75+", 26),  # ages 75-100
      "75+"  # 80+ aggregate
    )
  )
  lfpr_q <- merge(lfpr_q, ur_age_lookup, by = "age_group", all.x = TRUE)
  lfpr_q[is.na(ur_age_group), ur_age_group := "75+"]

  # Merge LFPR, RU, and population
  lf_data <- merge(lfpr_q, cni_by_group,
                   by = c("year", "quarter", "age_group", "sex"), all.x = TRUE)
  lf_data <- merge(lf_data, ru_q,
                   by.x = c("year", "quarter", "ur_age_group", "sex"),
                   by.y = c("year", "quarter", "age_group", "sex"),
                   all.x = TRUE, suffixes = c("", "_ru"))

  # Drop LFPR rows with no matching population (e.g., "80+" aggregate when
  # single-year ages 80-99 already cover those ages individually)
  lf_data <- lf_data[!is.na(population)]

  # LC = LFPR × N (Eq 2.1.5)
  lf_data[, labor_force := lfpr * population]

  # E = LC × (1 - RU/100) (Eq 2.1.6)
  lf_data[, employment := labor_force * (1 - rate / 100)]

  labor_force <- lf_data[, .(year, quarter, age_group, sex, labor_force)]
  employment <- lf_data[, .(year, quarter, age_group, sex, employment)]

  cli::cli_alert_success("Projected labor force and employment: {nrow(labor_force)} quarterly rows")

  list(
    labor_force = labor_force,
    employment = employment
  )
}

# =============================================================================
# Internal Helper Functions
# =============================================================================

#' @keywords internal
.get_ru_lags <- function(ru_annual, tgt_age_group, tgt_sex, tgt_year, n_lags) {
  vals <- numeric(n_lags)
  for (i in seq_len(n_lags)) {
    lag_year <- tgt_year - (i - 1)
    row <- ru_annual[age_group == tgt_age_group & sex == tgt_sex & year == lag_year]
    vals[i] <- if (nrow(row) > 0) row$rate[1] else NA_real_
  }
  vals[is.na(vals)] <- 0
  vals
}

#' @keywords internal
.compute_time_trend <- function(tgt_year, tgt_age_group, tgt_sex, base_year) {
  tgt_year - base_year
}

#' @keywords internal
.get_child_under6_prop <- function(children_props, tgt_age_group, tgt_year) {
  if (is.null(children_props)) {
    cli::cli_abort("Required input 'children_proportions' is NULL \u2014 ensure build_employment_inputs() provides it.")
  }
  row <- children_props[age_group == tgt_age_group & year == tgt_year]
  if (nrow(row) == 0) {
    cli::cli_abort("No children_proportions data found for age_group={tgt_age_group}, year={tgt_year}.")
  }
  row$proportion[1]
}

#' @keywords internal
.get_rd <- function(rd, tgt_age_group, tgt_sex, tgt_year) {
  if (is.null(rd)) {
    cli::cli_abort("Required input 'rd' is NULL \u2014 ensure build_employment_inputs() provides it.")
  }
  row <- rd[age_group == tgt_age_group & sex == tgt_sex & year == tgt_year]
  if (nrow(row) == 0) {
    cli::cli_abort("No rd data found for age_group={tgt_age_group}, sex={tgt_sex}, year={tgt_year}.")
  }
  row$rd[1]
}

#' @keywords internal
.get_edscore <- function(edscore, tgt_age_group, tgt_sex, tgt_year) {
  if (is.null(edscore)) {
    cli::cli_abort("Required input 'edscore' is NULL \u2014 ensure build_employment_inputs() provides it.")
  }
  row <- edscore[age_group == tgt_age_group & sex == tgt_sex & year == tgt_year]
  if (nrow(row) == 0) {
    cli::cli_abort("No edscore data found for age_group={tgt_age_group}, sex={tgt_sex}, year={tgt_year}.")
  }
  row$edscore[1]
}

#' @keywords internal
.get_msshare <- function(msshare, tgt_age, tgt_sex, tgt_year) {
  if (is.null(msshare)) {
    cli::cli_abort("Required input 'msshare' is NULL \u2014 ensure build_employment_inputs() provides it.")
  }
  row <- msshare[age == tgt_age & sex == tgt_sex & year == tgt_year]
  if (nrow(row) == 0) {
    cli::cli_abort("No msshare data found for age={tgt_age}, sex={tgt_sex}, year={tgt_year}.")
  }
  row$msshare[1]
}

#' @keywords internal
.get_rradj <- function(rradj, tgt_age, tgt_sex, tgt_year) {
  if (is.null(rradj)) {
    cli::cli_abort("Required input 'rradj' is NULL \u2014 ensure build_employment_inputs() provides it.")
  }
  row <- rradj[age == tgt_age & sex == tgt_sex & year == tgt_year]
  if (nrow(row) == 0) {
    cli::cli_abort("No rradj data found for age={tgt_age}, sex={tgt_sex}, year={tgt_year}.")
  }
  row$rradj[1]
}

#' @keywords internal
.get_pot_et_txrt <- function(pot_et_txrt, tgt_age, tgt_year) {
  if (is.null(pot_et_txrt)) {
    cli::cli_abort("Required input 'pot_et_txrt' is NULL \u2014 ensure build_employment_inputs() provides it.")
  }
  row <- pot_et_txrt[age == tgt_age & year == tgt_year]
  if (nrow(row) == 0) {
    cli::cli_abort("No pot_et_txrt data found for age={tgt_age}, year={tgt_year}.")
  }
  row$pot_et_txrt[1]
}

#' Get CNI population for a given age group, sex, and marital status
#'
#' @description
#' Looks up population from either:
#' - Total CNI population (quarterly, single-year age) for marital_status = "all"
#' - Marital CNI population (annual, single-year age, 3 LFPR categories) for disaggregated
#'
#' Aggregates single-year ages into the requested age group.
#'
#' @keywords internal
.get_cni_pop <- function(cni_pop, tgt_age_group, tgt_sex, tgt_marital_status, tgt_year,
                          marital_pop = NULL) {
  age_range <- USEMP_AGE_GROUP_RANGES[[tgt_age_group]]
  if (is.null(age_range)) {
    age_range <- as.integer(tgt_age_group)
  }

  if (tgt_marital_status == "all") {
    # Use total CNI population (quarterly -- pick Q2 for midyear)
    if ("quarter" %in% names(cni_pop)) {
      pop_rows <- cni_pop[age %in% age_range & sex == tgt_sex &
                           year == tgt_year & quarter == 2L]
      if (nrow(pop_rows) == 0) {
        pop_rows <- cni_pop[age %in% age_range & sex == tgt_sex & year == tgt_year]
      }
    } else {
      pop_rows <- cni_pop[age %in% age_range & sex == tgt_sex & year == tgt_year]
    }
    total <- sum(pop_rows$population, na.rm = TRUE)
    if (total <= 0) {
      cli::cli_abort("CNI population is zero/missing for age_group={tgt_age_group}, sex={tgt_sex}, year={tgt_year}.")
    }
    return(total)
  }

  # Marital status disaggregation -- use marital_cni_pop
  if (is.null(marital_pop)) {
    cli::cli_abort(c(
      "Required input 'marital_pop' is NULL for marital status '{tgt_marital_status}'.",
      "i" = "age_group={tgt_age_group}, sex={tgt_sex}, year={tgt_year}",
      "i" = "Ensure build_employment_inputs() provides marital_cni_pop."
    ))
  }
  pop_rows <- marital_pop[age %in% age_range & sex == tgt_sex &
                           lfpr_marital_status == tgt_marital_status &
                           year == tgt_year]
  total <- sum(pop_rows$population, na.rm = TRUE)
  if (total <= 0) {
    cli::cli_abort("Marital CNI population is zero/missing for age_group={tgt_age_group}, sex={tgt_sex}, marital_status={tgt_marital_status}, year={tgt_year}.")
  }
  total
}

#' @keywords internal
.get_cni_pop_children <- function(cni_pop, children_props, tgt_age_group, category, tgt_year) {
  ms <- sub("_(child_under6|no_child)$", "", category)
  total_pop <- .get_cni_pop(cni_pop, tgt_age_group, "female", ms, tgt_year)
  c6u_prop <- .get_child_under6_prop(children_props, tgt_age_group, tgt_year)
  if (grepl("child_under6$", category)) {
    return(total_pop * c6u_prop)
  } else {
    return(total_pop * (1 - c6u_prop))
  }
}

#' @keywords internal
.apply_addfactor <- function(lfpr, addfactors, age_group, sex) {
  if (is.null(addfactors)) return(lfpr)
  af <- addfactors[[age_group]]
  if (is.null(af)) return(lfpr)
  val <- af[[sex]]
  if (is.null(val)) return(lfpr)
  lfpr + val
}

#' @keywords internal
.get_prior_lfpr <- function(results, year, age_group, sex) {
  key <- paste(year, sex, age_group)
  if (!is.null(results[[key]])) {
    return(results[[key]]$lfpr[1])
  }
  0
}

#' @keywords internal
.get_movavg_lfpr <- function(results, year, lag_years, age_group, sex, window = 2) {
  # Moving average over `window` years of the age 79 LFPR
  vals <- vapply(seq_len(window), function(w) {
    yr <- year - lag_years - (w - 1)
    .get_prior_lfpr(results, yr, age_group, sex)
  }, numeric(1))
  mean(vals, na.rm = TRUE)
}

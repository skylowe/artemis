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
  rtp <- data.table::copy(rtp_quarterly)
  data.table::setorder(rtp, year, quarter)
  rtp[, d_rtp := rtp - shift(rtp, 1)]

  # Prepare lagged D(RTP) columns
  rtp[, d_rtp_1 := shift(d_rtp, 1)]
  rtp[, d_rtp_2 := shift(d_rtp, 2)]
  rtp[, d_rtp_3 := shift(d_rtp, 3)]

  # Initialize results from historical data
  all_results <- list()
  all_fe_results <- list()
  all_diff_results <- list()

  for (sex in c("male", "female")) {
    sex_coeffs <- ru_coefficients[[sex]]
    sex_prefix <- if (sex == "male") "M" else "F"

    for (ag in USEMP_RU_AGE_GROUPS) {
      coeffs <- sex_coeffs[[ag]]
      if (is.null(coeffs)) {
        cli::cli_abort("Missing unemployment coefficients for {sex} {ag}")
      }

      d <- c(coeffs$d0, coeffs$d1, coeffs$d2, coeffs$d3)

      # Get historical values for this age-sex group
      hist_vals <- historical_ru[age_group == ag & sex == (!!sex)]
      data.table::setorder(hist_vals, year, quarter)

      # Seed the projection with the last historical value
      last_hist <- hist_vals[.N, rate]

      # Project forward quarterly using first-difference model
      proj_quarters <- rtp[year > base_year | (year == base_year & quarter > 0)]
      proj_quarters <- proj_quarters[!is.na(d_rtp)]

      prev_rate <- last_hist
      rates <- numeric(nrow(proj_quarters))

      for (i in seq_len(nrow(proj_quarters))) {
        d_rtp_vec <- c(
          proj_quarters[i, d_rtp],
          proj_quarters[i, d_rtp_1],
          proj_quarters[i, d_rtp_2],
          proj_quarters[i, d_rtp_3]
        )
        # Replace NA lags with 0 (before enough history exists)
        d_rtp_vec[is.na(d_rtp_vec)] <- 0

        # Preliminary rate: R_P(q) = R_P(q-1) + sum(d_k * D(RTP(q-k)))
        rate_change <- sum(d * d_rtp_vec)
        prev_rate <- prev_rate + rate_change
        rates[i] <- prev_rate
      }

      proj_quarters[, rate := rates]
      proj_quarters[, age_group := ag]
      proj_quarters[, sex := sex]

      all_results[[paste(sex, ag)]] <- proj_quarters[, .(year, quarter, age_group, sex, rate)]

      # Full employment differentials (Section 5)
      # DR_FE = sum(d_k * (1 - RTP(q-k)))
      fe_diff <- numeric(nrow(proj_quarters))
      for (i in seq_len(nrow(proj_quarters))) {
        rtp_vec <- c(
          proj_quarters[i, rtp],
          rtp[year == proj_quarters[i, year] & quarter == proj_quarters[i, quarter] - 1, rtp],
          rtp[year == proj_quarters[i, year] & quarter == proj_quarters[i, quarter] - 2, rtp],
          rtp[year == proj_quarters[i, year] & quarter == proj_quarters[i, quarter] - 3, rtp]
        )
        # Simplified — proper lagging will use full quarterly index
        rtp_vals <- rep(1, 4)  # Placeholder for proper RTP lag lookup
        fe_diff[i] <- sum(d * (1 - rtp_vals))
      }

      proj_quarters[, fe_differential := fe_diff]
      proj_quarters[, fe_rate := rate + fe_differential]

      all_fe_results[[paste(sex, ag)]] <- proj_quarters[, .(year, quarter, age_group, sex, fe_rate)]
      all_diff_results[[paste(sex, ag)]] <- proj_quarters[, .(year, quarter, age_group, sex, fe_differential)]
    }
  }

  # Combine all age-sex groups
  preliminary <- data.table::rbindlist(all_results)
  fe_rates <- data.table::rbindlist(all_fe_results)
  differentials <- data.table::rbindlist(all_diff_results)

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
  actual <- merge(preliminary, constrained[, .(year, quarter, ru_asa_adj, ru_asa_p)],
                  by = c("year", "quarter"))
  actual[, rate := rate * (1 + ru_asa_adj / ru_asa_p)]
  actual[, c("ru_asa_adj", "ru_asa_p") := NULL]

  # Ensure rates are non-negative

  actual[rate < 0, rate := 0]
  actual[rate > 100, rate := 100]

  cli::cli_alert_success("Projected unemployment rates: {nrow(actual)} rows, {length(unique(actual$year))} years")

  list(
    actual = actual,
    full_employment = fe_rates,
    differentials = differentials
  )
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
                          config_employment) {
  checkmate::assert_list(unemployment_rates)
  checkmate::assert_data_table(cni_population)
  checkmate::assert_list(employment_inputs)
  checkmate::assert_list(lfpr_coefficients)
  checkmate::assert_list(config_employment)

  base_year <- config_employment$base_year
  projection_years <- (base_year + 1):2100

  # Extract input components
  edscore <- employment_inputs$edscore
  msshare <- employment_inputs$msshare
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

  # Addfactors (default to empty)
  addfactors <- config_employment$addfactors

  cli::cli_alert_info("Projecting LFPR for {length(projection_years)} years")

  # ── Compute LFPR for each equation type ──────────────────────────

  detailed_results <- list()
  aggregate_results <- list()

  for (yr in projection_years) {

    # --- Ages 16-19 (young) ---
    for (sex in c("male", "female")) {
      sex_coeffs <- lfpr_coefficients[[sex]]

      for (ag in c("16-17", "18-19")) {
        coeffs <- sex_coeffs[[ag]]
        ru_ag_rates <- .get_ru_lags(ru_annual, ag, sex, yr, n_lags = 6)
        ru_effect <- sum(coeffs$ru_lags * ru_ag_rates)
        # Time trend
        trend_val <- .compute_time_trend(yr, ag, sex, base_year)
        trend_effect <- coeffs$trend_coeff * trend_val + coeffs$trend_offset

        # Child-under-6 effect (female only)
        c6u_effect <- 0
        if (sex == "female" && !is.null(coeffs$child_under6_coeff)) {
          c6u_prop <- .get_child_under6_prop(children_props, ag, yr)
          c6u_effect <- coeffs$child_under6_coeff * c6u_prop + coeffs$child_under6_offset
        }

        # Disability adjustment
        rd_val <- .get_rd(rd, ag, sex, yr)

        lfpr_p <- (ru_effect + trend_effect + c6u_effect + coeffs$intercept) / (1 + rd_val)

        detailed_results[[paste(yr, sex, ag)]] <- data.table::data.table(
          year = yr, age_group = ag, sex = sex,
          marital_status = "all", child_status = "all",
          lfpr = lfpr_p, type = "young"
        )
      }
    }

    # --- Ages 20-54 male (marital status disaggregation) ---
    for (ag in c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")) {
      coeffs <- lfpr_coefficients$male[[ag]]
      if (is.null(coeffs) || coeffs$type != "marital") next

      ru_ag_rates <- .get_ru_lags(ru_annual, ag, "male", yr, n_lags = 6)
      rd_val <- .get_rd(rd, ag, "male", yr)

      # Population weights for aggregation
      pop_nm <- .get_cni_pop(cni_population, ag, "male", "never_married", yr)
      pop_ms <- .get_cni_pop(cni_population, ag, "male", "married_present", yr)
      pop_ma <- .get_cni_pop(cni_population, ag, "male", "married_absent", yr)
      pop_total <- pop_nm + pop_ms + pop_ma

      lfpr_by_status <- list()
      for (ms in c("never_married", "married_present", "married_absent")) {
        ms_coeffs <- coeffs[[ms]]
        ru_effect <- sum(ms_coeffs$ru_lags * ru_ag_rates)
        trend_effect <- 0
        if (!is.null(ms_coeffs$trend_coeff)) {
          trend_val <- .compute_time_trend(yr, ag, "male", base_year)
          trend_effect <- ms_coeffs$trend_coeff * trend_val
        }
        lfpr_by_status[[ms]] <- (ru_effect + trend_effect + ms_coeffs$intercept) / (1 + rd_val)
      }

      # Aggregate
      lfpr_agg <- (lfpr_by_status$never_married * pop_nm +
                   lfpr_by_status$married_present * pop_ms +
                   lfpr_by_status$married_absent * pop_ma) / pop_total

      # Apply addfactor if any
      lfpr_agg <- .apply_addfactor(lfpr_agg, addfactors, ag, "male")

      # Rescale disaggregated to match aggregate
      lfpr_agg_p <- (lfpr_by_status$never_married * pop_nm +
                     lfpr_by_status$married_present * pop_ms +
                     lfpr_by_status$married_absent * pop_ma) / pop_total
      scale_factor <- if (lfpr_agg_p > 0) lfpr_agg / lfpr_agg_p else 1

      for (ms in c("never_married", "married_present", "married_absent")) {
        detailed_results[[paste(yr, "male", ag, ms)]] <- data.table::data.table(
          year = yr, age_group = ag, sex = "male",
          marital_status = ms, child_status = "all",
          lfpr = lfpr_by_status[[ms]] * scale_factor, type = "marital"
        )
      }

      aggregate_results[[paste(yr, "male", ag)]] <- data.table::data.table(
        year = yr, age_group = ag, sex = "male", lfpr = lfpr_agg
      )
    }

    # --- Ages 20-44 female (marital x children disaggregation) ---
    for (ag in c("20-24", "25-29", "30-34", "35-39", "40-44")) {
      coeffs <- lfpr_coefficients$female[[ag]]
      if (is.null(coeffs) || coeffs$type != "marital_children") next

      ru_ag_rates <- .get_ru_lags(ru_annual, ag, "female", yr, n_lags = 6)
      rd_val <- .get_rd(rd, ag, "female", yr)

      categories <- c(
        "never_married_child_under6", "never_married_no_child",
        "married_present_child_under6", "married_present_no_child",
        "married_absent_child_under6", "married_absent_no_child"
      )

      lfpr_by_cat <- list()
      pop_by_cat <- list()
      for (cat in categories) {
        cat_coeffs <- coeffs[[cat]]
        ru_effect <- sum(cat_coeffs$ru_lags * ru_ag_rates)
        trend_effect <- 0
        if (!is.null(cat_coeffs$trend_coeff)) {
          trend_val <- .compute_time_trend(yr, ag, "female", base_year)
          trend_effect <- cat_coeffs$trend_coeff * trend_val
        }
        lfpr_by_cat[[cat]] <- (ru_effect + trend_effect + cat_coeffs$intercept) / (1 + rd_val)
        pop_by_cat[[cat]] <- .get_cni_pop_children(cni_population, children_props, ag, cat, yr)
      }

      pop_total <- sum(unlist(pop_by_cat))
      lfpr_agg <- sum(mapply(function(l, p) l * p, lfpr_by_cat, pop_by_cat)) / pop_total

      lfpr_agg <- .apply_addfactor(lfpr_agg, addfactors, ag, "female")
      lfpr_agg_p <- sum(mapply(function(l, p) l * p, lfpr_by_cat, pop_by_cat)) / pop_total
      scale_factor <- if (lfpr_agg_p > 0) lfpr_agg / lfpr_agg_p else 1

      for (cat in categories) {
        ms <- sub("_(child_under6|no_child)$", "", cat)
        cs <- sub("^[^_]+_[^_]+_", "", cat)
        detailed_results[[paste(yr, "female", ag, cat)]] <- data.table::data.table(
          year = yr, age_group = ag, sex = "female",
          marital_status = ms, child_status = cs,
          lfpr = lfpr_by_cat[[cat]] * scale_factor, type = "marital_children"
        )
      }

      aggregate_results[[paste(yr, "female", ag)]] <- data.table::data.table(
        year = yr, age_group = ag, sex = "female", lfpr = lfpr_agg
      )
    }

    # --- Ages 45-54 female (marital, no children disagg) ---
    for (ag in c("45-49", "50-54")) {
      coeffs <- lfpr_coefficients$female[[ag]]
      if (is.null(coeffs) || coeffs$type != "marital") next

      ru_ag_rates <- .get_ru_lags(ru_annual, ag, "female", yr, n_lags = 6)
      rd_val <- .get_rd(rd, ag, "female", yr)

      pop_nm <- .get_cni_pop(cni_population, ag, "female", "never_married", yr)
      pop_ms <- .get_cni_pop(cni_population, ag, "female", "married_present", yr)
      pop_ma <- .get_cni_pop(cni_population, ag, "female", "married_absent", yr)
      pop_total <- pop_nm + pop_ms + pop_ma

      lfpr_by_status <- list()
      for (ms in c("never_married", "married_present", "married_absent")) {
        ms_coeffs <- coeffs[[ms]]
        ru_effect <- sum(ms_coeffs$ru_lags * ru_ag_rates)
        edscore_effect <- 0
        if (!is.null(ms_coeffs$edscore_coeff)) {
          edscore_val <- .get_edscore(edscore, ag, "female", yr)
          edscore_effect <- ms_coeffs$edscore_coeff * edscore_val
        }
        lfpr_by_status[[ms]] <- (ru_effect + edscore_effect + ms_coeffs$intercept) / (1 + rd_val)
      }

      lfpr_agg <- (lfpr_by_status$never_married * pop_nm +
                   lfpr_by_status$married_present * pop_ms +
                   lfpr_by_status$married_absent * pop_ma) / pop_total

      lfpr_agg <- .apply_addfactor(lfpr_agg, addfactors, ag, "female")
      lfpr_agg_p <- (lfpr_by_status$never_married * pop_nm +
                     lfpr_by_status$married_present * pop_ms +
                     lfpr_by_status$married_absent * pop_ma) / pop_total
      scale_factor <- if (lfpr_agg_p > 0) lfpr_agg / lfpr_agg_p else 1

      for (ms in c("never_married", "married_present", "married_absent")) {
        detailed_results[[paste(yr, "female", ag, ms)]] <- data.table::data.table(
          year = yr, age_group = ag, sex = "female",
          marital_status = ms, child_status = "all",
          lfpr = lfpr_by_status[[ms]] * scale_factor, type = "marital"
        )
      }
      aggregate_results[[paste(yr, "female", ag)]] <- data.table::data.table(
        year = yr, age_group = ag, sex = "female", lfpr = lfpr_agg
      )
    }

    # --- Ages 55-74 (older / retirement) ---
    for (sex in c("male", "female")) {
      sex_coeffs <- lfpr_coefficients[[sex]]
      for (age in 55:74) {
        ag <- as.character(age)
        coeffs <- sex_coeffs[[ag]]
        if (is.null(coeffs)) next

        edscore_val <- .get_edscore(edscore, ag, sex, yr)
        msshare_val <- .get_msshare(msshare, age, sex, yr)

        # Use cohort RD at age 61 for ages 62-74
        if (age >= 62) {
          cohort_year <- yr - (age - 61)
          rd_val <- .get_rd(rd, "61", sex, cohort_year)
        } else {
          rd_val <- .get_rd(rd, ag, sex, yr)
        }

        lfpr_p <- coeffs$edscore_coeff * edscore_val +
                  coeffs$msshare_coeff * msshare_val +
                  coeffs$intercept

        # Retirement variables for ages 62-69
        if (coeffs$type == "retirement") {
          rradj_val <- .get_rradj(rradj, age, sex, yr)
          pot_et_val <- .get_pot_et_txrt(pot_et_txrt, age, yr)
          lfpr_p <- lfpr_p + coeffs$rradj_coeff * rradj_val +
                    coeffs$pot_et_coeff * pot_et_val
        }

        lfpr_p <- lfpr_p / (1 + rd_val)
        lfpr_p <- .apply_addfactor(lfpr_p, addfactors, ag, sex)

        detailed_results[[paste(yr, sex, ag)]] <- data.table::data.table(
          year = yr, age_group = ag, sex = sex,
          marital_status = "all", child_status = "all",
          lfpr = lfpr_p, type = coeffs$type
        )
        aggregate_results[[paste(yr, sex, ag)]] <- data.table::data.table(
          year = yr, age_group = ag, sex = sex, lfpr = lfpr_p
        )
      }
    }

    # --- Ages 75-79 (cohort decay) ---
    for (sex in c("male", "female")) {
      decay_factor <- decay_75_79[[sex]]
      for (age in 75:79) {
        ag <- as.character(age)
        prior_age <- as.character(age - 1)
        # 4-quarter (1-year) lag: use prior year's LFPR for age-1
        prior_lfpr <- .get_prior_lfpr(aggregate_results, yr - 1, prior_age, sex)
        lfpr_p <- prior_lfpr * decay_factor

        detailed_results[[paste(yr, sex, ag)]] <- data.table::data.table(
          year = yr, age_group = ag, sex = sex,
          marital_status = "all", child_status = "all",
          lfpr = lfpr_p, type = "cohort_decay_75"
        )
        aggregate_results[[paste(yr, sex, ag)]] <- data.table::data.table(
          year = yr, age_group = ag, sex = sex, lfpr = lfpr_p
        )
      }
    }

    # --- Ages 80-100 (decay from age 79) ---
    for (sex in c("male", "female")) {
      for (age in 80:100) {
        ag <- as.character(age)
        years_from_79 <- age - 79

        if (age <= 84) {
          # PM80_P = PM79(-4) * 0.965^1  (i.e., age 79 value from years_from_79 years ago)
          prior_yr <- yr - years_from_79
          base_lfpr <- .get_prior_lfpr(aggregate_results, prior_yr, "79", sex)
        } else if (age <= 94) {
          # PM85_P = MOVAVG(8, PM79(-24)) * 0.965^6
          # 8-quarter moving average of PM79 starting from (years_from_79) years ago
          lag_years <- years_from_79
          base_lfpr <- .get_movavg_lfpr(aggregate_results, yr, lag_years, "79", sex, window = 2)
        } else {
          # PM95+ = chain from PM94 * 0.965 each additional year
          prior_age <- as.character(age - 1)
          base_lfpr <- .get_prior_lfpr(aggregate_results, yr, prior_age, sex)
          lfpr_p <- base_lfpr * decay_80_plus
          detailed_results[[paste(yr, sex, ag)]] <- data.table::data.table(
            year = yr, age_group = ag, sex = sex,
            marital_status = "all", child_status = "all",
            lfpr = lfpr_p, type = "cohort_decay_80"
          )
          aggregate_results[[paste(yr, sex, ag)]] <- data.table::data.table(
            year = yr, age_group = ag, sex = sex, lfpr = lfpr_p
          )
          next
        }

        lfpr_p <- base_lfpr * decay_80_plus^years_from_79

        detailed_results[[paste(yr, sex, ag)]] <- data.table::data.table(
          year = yr, age_group = ag, sex = sex,
          marital_status = "all", child_status = "all",
          lfpr = lfpr_p, type = "cohort_decay_80"
        )
        aggregate_results[[paste(yr, sex, ag)]] <- data.table::data.table(
          year = yr, age_group = ag, sex = sex, lfpr = lfpr_p
        )
      }

      # Aggregate 80+ into PM80O / PF80O using population weights
      pop_80_plus <- cni_population[sex == (!!sex) & age >= 80 & year == yr]
      if (nrow(pop_80_plus) > 0) {
        lfpr_80_vals <- vapply(80:100, function(a) {
          key <- paste(yr, sex, as.character(a))
          if (!is.null(aggregate_results[[key]])) aggregate_results[[key]]$lfpr else 0
        }, numeric(1))
        pop_80_vals <- vapply(80:100, function(a) {
          pop_80_plus[age == a, sum(population, na.rm = TRUE)]
        }, numeric(1))
        total_pop_80 <- sum(pop_80_vals)
        lfpr_80o <- if (total_pop_80 > 0) sum(lfpr_80_vals * pop_80_vals) / total_pop_80 else 0

        aggregate_results[[paste(yr, sex, "80+")]] <- data.table::data.table(
          year = yr, age_group = "80+", sex = sex, lfpr = lfpr_80o
        )
      }
    }
  }

  # Combine results
  detailed <- data.table::rbindlist(detailed_results, fill = TRUE)
  aggregate <- data.table::rbindlist(aggregate_results, fill = TRUE)

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

  lfpr_q <- lfpr_projection$quarterly
  ru_q <- unemployment_projection$actual

  # Merge LFPR, RU, and population
  lf_data <- merge(lfpr_q, quarterly_cni_pop,
                   by = c("year", "quarter", "age_group", "sex"), all.x = TRUE)
  lf_data <- merge(lf_data, ru_q,
                   by = c("year", "quarter", "age_group", "sex"),
                   all.x = TRUE, suffixes = c("", "_ru"))

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
.get_ru_lags <- function(ru_annual, age_group, sex, year, n_lags) {
  vals <- numeric(n_lags)
  for (i in seq_len(n_lags)) {
    lag_year <- year - (i - 1)
    row <- ru_annual[age_group == (!!age_group) & sex == (!!sex) & year == lag_year]
    vals[i] <- if (nrow(row) > 0) row$rate[1] else NA_real_
  }
  # Replace NA with 0 for initialization
  vals[is.na(vals)] <- 0
  vals
}

#' @keywords internal
.compute_time_trend <- function(year, age_group, sex, base_year) {
  # Linear time trend counting from base year
  year - base_year
}

#' @keywords internal
.get_child_under6_prop <- function(children_props, age_group, year) {
  if (is.null(children_props)) return(0)
  row <- children_props[age_group == (!!age_group) & year == (!!year)]
  if (nrow(row) > 0) row$proportion[1] else 0
}

#' @keywords internal
.get_rd <- function(rd, age_group, sex, year) {
  if (is.null(rd)) return(0)
  row <- rd[age_group == (!!age_group) & sex == (!!sex) & year == (!!year)]
  if (nrow(row) > 0) row$rd[1] else 0
}

#' @keywords internal
.get_edscore <- function(edscore, age_group, sex, year) {
  if (is.null(edscore)) return(1)
  row <- edscore[age_group == (!!age_group) & sex == (!!sex) & year == (!!year)]
  if (nrow(row) > 0) row$edscore[1] else 1
}

#' @keywords internal
.get_msshare <- function(msshare, age, sex, year) {
  if (is.null(msshare)) return(0.5)
  row <- msshare[age == (!!age) & sex == (!!sex) & year == (!!year)]
  if (nrow(row) > 0) row$msshare[1] else 0.5
}

#' @keywords internal
.get_rradj <- function(rradj, age, sex, year) {
  if (is.null(rradj)) return(0)
  row <- rradj[age == (!!age) & sex == (!!sex) & year == (!!year)]
  if (nrow(row) > 0) row$rradj[1] else 0
}

#' @keywords internal
.get_pot_et_txrt <- function(pot_et_txrt, age, year) {
  if (is.null(pot_et_txrt)) return(0)
  row <- pot_et_txrt[age == (!!age) & year == (!!year)]
  if (nrow(row) > 0) row$pot_et_txrt[1] else 0
}

#' @keywords internal
.get_cni_pop <- function(cni_pop, age_group, sex, marital_status, year) {
  # Map internal marital status names to data column values
  ms_map <- c(
    never_married = "single",
    married_present = "married",
    married_absent = "married"
  )
  ms_val <- ms_map[[marital_status]]
  if (is.null(ms_val)) ms_val <- marital_status

  row <- cni_pop[age_group == (!!age_group) & sex == (!!sex) &
                 marital_status == ms_val & year == (!!year)]
  if (nrow(row) > 0) sum(row$population) else 1
}

#' @keywords internal
.get_cni_pop_children <- function(cni_pop, children_props, age_group, category, year) {
  # Get total population for age-group and split by children proportion
  total_pop <- .get_cni_pop(cni_pop, age_group, "female",
                            sub("_(child_under6|no_child)$", "", category), year)
  if (grepl("child_under6$", category)) {
    prop <- .get_child_under6_prop(children_props, age_group, year)
    return(total_pop * prop)
  } else {
    prop <- .get_child_under6_prop(children_props, age_group, year)
    return(total_pop * (1 - prop))
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

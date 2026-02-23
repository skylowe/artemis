#' Employment Input Variable Construction
#'
#' @description
#' Constructs the intermediate variables needed by the unemployment and LFPR
#' equations from demography pipeline outputs and external data sources.
#'
#' @references
#' - 2025_LR_Model_Documentation_Economics_1_USEmployment.md (Input Data section)
#' - economics_equations_1_USEmployment.md (Variable Notation Guide)
#'
#' @name employment_inputs
NULL

# =============================================================================
# Quarterly Population Interpolation
# =============================================================================

#' Interpolate annual population to quarterly
#'
#' @description
#' Converts year-end population to quarterly values using linear interpolation.
#' Q1-Q3 interpolate between adjacent year-end values; Q4 equals the year-end value.
#'
#' @param annual_pop data.table with year-end population (year, age, sex, population, ...)
#' @param id_cols Character vector of columns that identify groups (default: c("age", "sex"))
#' @param value_col Character: name of the population column (default: "population")
#'
#' @return data.table with columns: year, quarter, <id_cols>, <value_col>
#'
#' @export
interpolate_quarterly_population <- function(annual_pop,
                                              id_cols = c("age", "sex"),
                                              value_col = "population") {
  checkmate::assert_data_table(annual_pop)
  checkmate::assert_names(names(annual_pop), must.include = c("year", id_cols, value_col))

  dt <- data.table::copy(annual_pop)
  data.table::setorderv(dt, c(id_cols, "year"))

  # Create lagged population within each group
  dt[, pop_lag := shift(get(value_col), 1), by = id_cols]

  # Only interpolate where we have both current and prior year
  dt <- dt[!is.na(pop_lag)]

  # Create quarterly rows
  quarters <- dt[, {
    pop_prev <- pop_lag
    pop_curr <- get(value_col)
    list(
      quarter = 1:4,
      population_q = pop_prev + c(0.25, 0.50, 0.75, 1.00) * (pop_curr - pop_prev)
    )
  }, by = c("year", id_cols)]

  data.table::setnames(quarters, "population_q", value_col)

  cli::cli_alert_success(
    "Interpolated {nrow(annual_pop)} annual rows to {nrow(quarters)} quarterly rows"
  )

  quarters
}

# =============================================================================
# Civilian Noninstitutional Population (Eq 2.1.2)
# =============================================================================

#' Compute projected civilian noninstitutional population
#'
#' @description
#' Implements Eq 2.1.2: N^t = [(N^(t-1) + M^(t-1)) × (P^t / P^(t-1))] - M^t
#' where P = SS area population, M = military, N = civilian noninstitutional.
#'
#' @param projected_population SS area population from demography pipeline
#' @param military_population Military population (held constant from config year)
#' @param historical_cni Historical CNI population for initialization
#' @param config_employment Employment config section
#'
#' @return data.table with quarterly CNI population by age group and sex
#'
#' @references Eq 2.1.2
#' @export
compute_cni_population <- function(projected_population,
                                    military_population,
                                    historical_cni,
                                    config_employment) {
  checkmate::assert_data_table(projected_population)
  checkmate::assert_data_table(military_population)
  checkmate::assert_list(config_employment)

  base_year <- config_employment$base_year

  cli::cli_alert_info("Computing CNI population from Eq 2.1.2 (base year: {base_year})")

  # Sum SS area population across pop_status to get total P by age and sex
  total_pop <- projected_population[, .(P = sum(population)),
                                     by = .(year, age, sex)]
  data.table::setorder(total_pop, age, sex, year)

  # Military population held constant (use total_af column)
  mil_col <- if ("total_af" %in% names(military_population)) "total_af" else "population"
  mil_const <- military_population[, .(age, sex, M = get(mil_col))]
  # Aggregate in case of duplicates
  mil_const <- mil_const[, .(M = sum(M)), by = .(age, sex)]

  # Merge military onto total_pop
  total_pop <- merge(total_pop, mil_const, by = c("age", "sex"), all.x = TRUE)
  total_pop[is.na(M), M := 0]

  # Compute year-over-year growth ratio P^t / P^(t-1)
  total_pop[, P_lag := shift(P, 1), by = .(age, sex)]

  # Initialize N from historical CNI in base year
  total_pop[, N := NA_real_]
  if (!is.null(historical_cni) && nrow(historical_cni) > 0) {
    # historical_cni may have various column names for the population value
    pop_col <- intersect(names(historical_cni), c("population", "cni_population", "N"))
    if (length(pop_col) > 0) {
      base_cni <- historical_cni[year == base_year, .(age, sex, N_hist = get(pop_col[1]))]
      base_cni <- base_cni[, .(N_hist = sum(N_hist)), by = .(age, sex)]
      total_pop <- merge(total_pop, base_cni, by = c("age", "sex"), all.x = TRUE)
      total_pop[year == base_year & !is.na(N_hist), N := N_hist]
      total_pop[, N_hist := NULL]
    }
  }

  # If no historical CNI, initialize base year N ≈ P - M - institutionalized
  # Rough estimate: N ≈ 0.97 * P - M (3% institutionalization rate)
  total_pop[year == base_year & is.na(N), N := pmax(0, 0.97 * P - M)]

  # Forward fill N using Eq 2.1.2: N^t = [(N^(t-1) + M^(t-1)) × (P^t / P^(t-1))] - M^t
  # Data is from CJ (complete cross-join), so after sorting by (age, sex, year),
  # which(year == yr) and which(year == yr-1) return same-length vectors with
  # corresponding (age, sex) pairs — no merge needed.
  data.table::setorder(total_pop, age, sex, year)
  projection_years <- sort(unique(total_pop[year > base_year, year]))
  for (yr in projection_years) {
    curr <- which(total_pop$year == yr)
    prev <- which(total_pop$year == yr - 1L)
    N_prev <- total_pop$N[prev]
    P_curr <- total_pop$P[curr]
    P_lag  <- total_pop$P_lag[curr]
    M_curr <- total_pop$M[curr]
    valid <- !is.na(N_prev) & !is.na(P_lag) & P_lag > 0
    data.table::set(total_pop, i = curr[valid], j = "N",
                    value = ((N_prev[valid] + M_curr[valid]) *
                               (P_curr[valid] / P_lag[valid])) - M_curr[valid])
  }

  result <- total_pop[!is.na(N), .(year, age, sex, population = N)]

  # Interpolate to quarterly
  quarterly <- interpolate_quarterly_population(result, id_cols = c("age", "sex"))

  cli::cli_alert_success("Computed CNI population: {nrow(quarterly)} quarterly rows")
  quarterly
}

# =============================================================================
# Marital Status CNI Population (for LFPR disaggregation)
# =============================================================================

#' Remap CNI population marital statuses to LFPR categories
#'
#' @description
#' The LFPR equations for ages 20-54 disaggregate by 3 marital statuses:
#' - NM (never married)
#' - MS (married, spouse present)
#' - MA (married, spouse absent)
#'
#' The demography pipeline's `projected_cni_population` already provides:
#' - single → NM (never married)
#' - married_spouse_present → MS
#' - separated, divorced, widowed → MA (married, spouse absent)
#'
#' This function simply remaps these categories.
#'
#' @param projected_cni_population CNI population from demography pipeline
#'   (data.table with year, age, sex, marital_status, population)
#'
#' @return data.table with columns: year, age, sex, lfpr_marital_status, population
#'   lfpr_marital_status values: never_married, married_present, married_absent
#'
#' @export
compute_marital_cni_population <- function(projected_cni_population) {
  checkmate::assert_data_table(projected_cni_population)
  checkmate::assert_names(names(projected_cni_population),
                          must.include = c("year", "age", "sex", "marital_status", "population"))

  cli::cli_alert_info("Remapping CNI marital statuses to LFPR categories")

  dt <- data.table::copy(projected_cni_population)

  # Map demography marital statuses to LFPR 3-category scheme
  dt[, lfpr_marital_status := fcase(
    marital_status == "single", "never_married",
    marital_status == "married_spouse_present", "married_present",
    marital_status %in% c("separated", "divorced", "widowed"), "married_absent",
    default = NA_character_
  )]

  # Drop any unmapped rows and aggregate (separated + divorced + widowed → married_absent)
  dt <- dt[!is.na(lfpr_marital_status)]
  result <- dt[, .(population = sum(population)),
               by = .(year, age, sex, lfpr_marital_status)]

  data.table::setorder(result, year, sex, age, lfpr_marital_status)

  cli::cli_alert_success(
    "Remapped marital CNI population: {nrow(result)} rows, {length(unique(result$year))} years"
  )

  result
}

# =============================================================================
# Married Share (MSSHARE)
# =============================================================================

#' Compute married share by age and sex
#'
#' @description
#' MSSHARE = married / total for each age-sex group.
#' Used in LFPR equations for ages 55-74 (both sexes).
#'
#' @param marital_population Projected marital population from demography pipeline
#'   (data.table with year, age, sex, marital_status, population)
#'
#' @return data.table with columns: year, age, sex, msshare
#'
#' @export
compute_married_share <- function(marital_population) {
  checkmate::assert_data_table(marital_population)
  checkmate::assert_names(names(marital_population),
                          must.include = c("year", "age", "sex", "marital_status", "population"))

  dt <- data.table::copy(marital_population)

  # Total population by age, sex, year

  total <- dt[, .(total_pop = sum(population)), by = .(year, age, sex)]

  # Married population
  married <- dt[marital_status == "married",
                .(married_pop = sum(population)), by = .(year, age, sex)]

  result <- merge(total, married, by = c("year", "age", "sex"), all.x = TRUE)
  result[is.na(married_pop), married_pop := 0]
  result[, msshare := fifelse(total_pop > 0, married_pop / total_pop, 0)]

  result <- result[, .(year, age, sex, msshare)]
  data.table::setorder(result, year, sex, age)

  cli::cli_alert_success("Computed married share for {length(unique(result$year))} years, ages {min(result$age)}-{max(result$age)}")

  result
}

# =============================================================================
# Educational Attainment Score (EDSCORE)
# =============================================================================

#' Compute educational attainment score
#'
#' @description
#' EDSCORE is a weighted average of educational attainment proportions
#' where higher education maps to higher labor force participation.
#' Used in LFPR equations for men 55+ and women 50+.
#'
#' @param education_data CPS educational attainment proportions
#'   (data.table with year, age_group, sex, education_level, proportion)
#' @param projection_years Years to project forward
#'
#' @return data.table with columns: year, age_group, sex, edscore
#'
#' @export
compute_edscore <- function(education_data, projection_years) {
  checkmate::assert_data_table(education_data)

  # Ordinal education level weights matching SSA regression coefficient scale.
  # The LFPR coefficients were estimated with EDSCORE on a 0-3 integer scale.
  edu_weights <- c(
    less_than_hs = 0,
    high_school = 1,
    some_college = 2,
    bachelors_plus = 3
  )

  dt <- data.table::copy(education_data)

  # Standardize column names — handle both formats
  if (!"education_level" %in% names(dt) && "child_status" %in% names(dt)) {
    data.table::setnames(dt, "child_status", "education_level")
  }
  if (!"proportion" %in% names(dt) && "value" %in% names(dt)) {
    data.table::setnames(dt, "value", "proportion")
  }
  if (!"age_group" %in% names(dt) && "age" %in% names(dt)) {
    dt[, age_group := as.character(age)]
  }

  # Ensure education_level is character (not factor)
  dt[, education_level := as.character(education_level)]

  # Only keep valid education levels
  valid_levels <- names(edu_weights)
  dt <- dt[education_level %in% valid_levels]

  # Historical EDSCORE: weighted average by age_group, sex, year
  historical <- dt[, .(
    edscore = sum(proportion * edu_weights[education_level], na.rm = TRUE)
  ), by = .(year, age_group, sex)]

  # Cohort-based projection: education is largely fixed by age 25, so for a
  # target age group in a future year, use the EDSCORE from the age group that
  # cohort occupied at the last historical year. This captures the rising
  # education trend as more-educated young cohorts age into 55+.
  #
  # Historical data has two resolutions:
  # - Single-year ages 50+ (from CPS microdata tabulated by individual AGE)
  # - 5-year groups 25-54 (from CPS tabulated by age group)
  # For cohort tracking, use single-year when source age >= 50, else 5-year.
  last_hist_year <- max(historical$year)
  last_values <- historical[year == last_hist_year]

  # Build projection grid for all target age_groups
  proj_grid <- data.table::CJ(
    age_group = unique(last_values$age_group),
    sex = unique(last_values$sex),
    year = projection_years
  )

  # Determine the target age (midpoint for 5-year groups, exact for single-year)
  proj_grid[, is_single_year := grepl("^\\d+$", age_group)]
  proj_grid[is_single_year == TRUE, target_age := as.integer(age_group)]
  proj_grid[is_single_year == FALSE, target_age := data.table::fcase(
    age_group == "25-29", 27L, age_group == "30-34", 32L,
    age_group == "35-39", 37L, age_group == "40-44", 42L,
    age_group == "45-49", 47L, age_group == "50-54", 52L,
    default = 77L  # 75+
  )]

  # Where was this cohort at the last historical year?
  proj_grid[, source_age := target_age - (year - last_hist_year)]
  proj_grid[source_age < 25L, source_age := 25L]  # Clamp to youngest available

  # Source age_group: use single-year if >= 50, otherwise map to 5-year group
  proj_grid[, source_age_group := data.table::fifelse(
    source_age >= 50L, as.character(source_age),
    data.table::fcase(
      source_age < 30L, "25-29",
      source_age < 35L, "30-34",
      source_age < 40L, "35-39",
      source_age < 45L, "40-44",
      source_age < 50L, "45-49",
      default = "50-54"
    )
  )]

  # Look up source cohort's EDSCORE from the last historical year
  proj_grid <- merge(proj_grid,
                     last_values[, .(source_age_group = age_group, sex, edscore)],
                     by = c("source_age_group", "sex"), all.x = TRUE)

  projection <- proj_grid[, .(year, age_group, sex, edscore)]

  result <- data.table::rbindlist(list(historical, projection), fill = TRUE)
  data.table::setorder(result, year, sex, age_group)

  cli::cli_alert_success("Computed EDSCORE for {nrow(result)} age-sex-year cells (cohort-based projection)")

  result
}

# =============================================================================
# Children Under 6 Proportions
# =============================================================================

#' Compute proportion of females with child under 6 by age group
#'
#' @description
#' Computes the fraction of females in each age group who have at least one
#' own child under 6. Used in 30 LFPR equations (females 20-44 × 3 marital
#' statuses × 2 child statuses) to weight the "with child" and "no child"
#' LFPR estimates. For ages 16-19, the proportion enters the equation directly
#' as RF1617CU6 / RF1819CU6.
#'
#' Historical proportions come from CPS ASEC microdata. Projection years use
#' `projected_mothers_child_under6` from the demography pipeline (Phase 8D),
#' which computes Poisson-converted proportions from the children_fate arrays
#' at single-year mother age resolution. A 3-year linear blend smooths the
#' CPS→demography handoff at the transition.
#'
#' @param cps_labor_data CPS ASEC labor force data from IPUMS
#' @param projected_mothers_child_under6 Demography pipeline output: proportion
#'   of females with at least one child under 6, by year and LFPR age group
#'   (data.table with year, age_group, proportion)
#' @param config_employment Employment config section
#'
#' @return data.table with columns: age_group, year, proportion
#'
#' @export
construct_children_proportions <- function(cps_labor_data,
                                            projected_mothers_child_under6,
                                            config_employment) {
  checkmate::assert_data_table(cps_labor_data)
  checkmate::assert_data_table(projected_mothers_child_under6)
  checkmate::assert_names(names(projected_mothers_child_under6),
                          must.include = c("year", "age_group", "proportion"))

  cli::cli_alert_info("Computing children-under-6 proportions from CPS + demography")

  end_year <- config_employment$end_year
  if (is.null(end_year)) cli::cli_abort("economics.employment.end_year not set in config")

  target_age_groups <- c("16-17", "18-19", "20-24", "25-29", "30-34", "35-39", "40-44")

  # ── 1. Historical proportions from CPS ─────────────────────────────
  child_pop <- cps_labor_data[
    concept == "population" &
      sex == "female" &
      child_status %in% c("child_under6", "no_child") &
      age_group %in% target_age_groups
  ]

  if (nrow(child_pop) == 0) {
    cli::cli_abort(c(
      "No child-under-6 population data found in CPS extract",
      "i" = "CPS data must include concept='population' with child_status='child_under6'/'no_child' for female ages 16-44"
    ))
  }

  # Sum across marital statuses to get total by age_group × year × child_status
  totals <- child_pop[, .(pop = sum(value)), by = .(year, age_group, child_status)]

  c6u <- totals[child_status == "child_under6", .(year, age_group, pop_c6u = pop)]
  nc6 <- totals[child_status == "no_child", .(year, age_group, pop_nc6 = pop)]

  props <- merge(c6u, nc6, by = c("year", "age_group"))
  props[, total := pop_c6u + pop_nc6]

  if (any(props$total <= 0)) {
    cli::cli_abort("Zero total female population in child-under-6 computation — check CPS data")
  }

  props[, proportion := pop_c6u / total]
  historical <- props[, .(year, age_group, proportion)]
  last_cps_year <- max(historical$year)

  cli::cli_alert_info("CPS historical proportions: {min(historical$year)}-{last_cps_year}")

  # ── 2. Projection from demography pipeline ─────────────────────────
  # projected_mothers_child_under6 provides Poisson-converted proportions
  # from Phase 8D children_fate arrays at single-year mother age resolution.
  # Use these directly for projection years after the CPS→demography blend.

  demog <- projected_mothers_child_under6[age_group %in% target_age_groups]
  demog_years <- sort(unique(demog$year))
  first_demog_year <- min(demog_years)

  if (length(demog_years) == 0) {
    cli::cli_abort("projected_mothers_child_under6 has no data for target age groups")
  }

  cli::cli_alert_info(
    "Demography projections: {first_demog_year}-{max(demog_years)}, {length(target_age_groups)} age groups"
  )

  # ── 3. Blend CPS→demography at transition ─────────────────────────
  # Use a 3-year linear blend over the overlap period to avoid a jump.
  # Years <= last_cps_year: 100% CPS
  # Years > last_cps_year + 3: 100% demography
  # Transition years: linear interpolation between CPS last value and demography

  blend_years <- 3L
  transition_start <- last_cps_year + 1L
  transition_end <- last_cps_year + blend_years

  # CPS endpoint proportions (average of last 2 years for stability)
  cps_endpoint <- historical[year >= last_cps_year - 1,
                              .(cps_prop = mean(proportion)),
                              by = age_group]

  missing_cps_groups <- setdiff(target_age_groups, cps_endpoint$age_group)
  if (length(missing_cps_groups) > 0) {
    cli::cli_abort(c(
      "CPS data missing child-under-6 proportions for age groups: {paste(missing_cps_groups, collapse = ', ')}",
      "i" = "All 7 LFPR age groups (16-17 through 40-44) must have CPS data"
    ))
  }

  # Verify demography covers all projection years through end_year
  max_demog_year <- max(demog_years)
  if (max_demog_year < end_year) {
    cli::cli_abort(c(
      "Demography projections end at {max_demog_year} but economics needs through {end_year}",
      "i" = "projected_mothers_child_under6 must cover the full projection period"
    ))
  }

  # Verify demography covers all target age groups
  missing_demog_groups <- setdiff(target_age_groups, unique(demog$age_group))
  if (length(missing_demog_groups) > 0) {
    cli::cli_abort(c(
      "Demography data missing age groups: {paste(missing_demog_groups, collapse = ', ')}",
      "i" = "projected_mothers_child_under6 must provide all 7 LFPR age groups"
    ))
  }

  # Build projection: blend then pure demography
  proj_years <- seq(transition_start, end_year)
  proj_years <- proj_years[proj_years %in% demog_years]

  if (length(proj_years) == 0) {
    cli::cli_abort(c(
      "No overlap between projection years and demography data",
      "i" = "Projection starts at {transition_start} but demography covers {first_demog_year}-{max_demog_year}"
    ))
  }

  projected <- demog[year %in% proj_years, .(year, age_group, demog_prop = proportion)]
  projected <- merge(projected, cps_endpoint, by = "age_group")

  # Verify merge produced no NAs
  if (any(is.na(projected$cps_prop)) || any(is.na(projected$demog_prop))) {
    cli::cli_abort("NA values after merging CPS and demography proportions — data alignment issue")
  }

  # Blending weight: 0 at transition_start, 1 at transition_end+1
  projected[, blend_weight := pmin(1, pmax(0, (year - last_cps_year) / (blend_years + 1)))]
  projected[, proportion := cps_prop * (1 - blend_weight) + demog_prop * blend_weight]
  projected <- projected[, .(year, age_group, proportion)]

  result <- data.table::rbindlist(list(historical, projected))
  data.table::setorder(result, year, age_group)

  n_hist <- nrow(historical)
  n_proj <- nrow(result) - n_hist
  cli::cli_alert_success(
    "Computed children-under-6 proportions: {n_hist} historical + {n_proj} projected, {length(target_age_groups)} age groups"
  )

  result[, .(age_group, year, proportion)]
}

# =============================================================================
# Unemployment Path Override
# =============================================================================

#' Override V.B2 unemployment path with user's ultimate rate
#'
#' @description
#' When the user's `ultimate_unemployment_rate` differs from V.B2's terminal
#' rate, smoothly transitions the V.B2 path to converge to the user's target.
#' V.B2's near-term dynamics (cyclical adjustments) are preserved; only the
#' long-run convergence target is modified.
#'
#' The transition uses a linear phase-in: at the base year, the path equals
#' V.B2 exactly; by the year V.B2 first reaches its own terminal rate, the
#' path reaches the user's ultimate rate and stays there.
#'
#' @param ru_annual data.table with `year` and `rate` columns (V.B2 annual path)
#' @param config_employment Employment config section (needs `base_year` and
#'   `ultimate_unemployment_rate`)
#'
#' @return data.table with `year` and `rate` columns (modified path, or
#'   unmodified if no override is needed)
#'
#' @export
override_unemployment_path <- function(ru_annual, config_employment) {
  user_ultimate <- config_employment$ultimate_unemployment_rate
  if (is.null(user_ultimate)) return(ru_annual)

  base_year <- config_employment$base_year
  vb2_terminal <- ru_annual[year == max(year), rate]

  # No override needed if user's ultimate matches V.B2 terminal
  if (abs(user_ultimate - vb2_terminal) < 0.01) return(ru_annual)

  dt <- data.table::copy(ru_annual)
  offset <- user_ultimate - vb2_terminal

  # Find convergence year: first year where V.B2 reaches its terminal value
  # and stays there for all subsequent years
  data.table::setorder(dt, year)
  at_terminal <- abs(dt$rate - vb2_terminal) < 0.01
  n <- nrow(dt)
  convergence_idx <- n
  for (i in seq_len(n)) {
    if (all(at_terminal[i:n])) {
      convergence_idx <- i
      break
    }
  }
  convergence_year <- max(dt$year[convergence_idx], base_year + 10L)

  # Phase in offset: 0 at base_year, 1 at convergence_year (min 10 years)
  span <- convergence_year - base_year
  if (span <= 0) span <- 1L
  dt[, weight := pmin(1, pmax(0, (year - base_year) / span))]
  dt[, rate := rate + offset * weight]
  dt[, weight := NULL]

  cli::cli_alert_info(
    "Overriding UR path: V.B2 terminal {vb2_terminal}% \u2192 user ultimate {user_ultimate}% (converges by {convergence_year})"
  )

  dt
}

# =============================================================================
# RTP (Ratio of Real to Potential GDP)
# =============================================================================

#' Compute RTP from historical GDP data and projected unemployment rate path
#'
#' @description
#' Builds a continuous quarterly RTP (Real GDP / Potential GDP) series by
#' splicing historical RTP from FRED (GDPC1/GDPPOT) with projected RTP
#' derived from Okun's Law. This ensures D(RTP) lags are available for
#' Q1 of the first projection year, matching the SSA methodology.
#'
#' Projected RTP uses Okun's Law:
#'   RTP = 1 + (u* - u) * beta / 100
#'
#' where u* = natural rate (ultimate unemployment rate), u = actual,
#' and beta = Okun coefficient (~2).
#'
#' @param unemployment_path Annual unemployment rate path (data.table with year, rate)
#' @param config_employment Employment config section
#' @param historical_rtp Quarterly historical RTP from FRED (data.table with year, quarter, rtp).
#'   If NULL, only projected RTP is returned (Q1 of first year will have NA D(RTP)).
#'
#' @return data.table with columns: year, quarter, rtp
#'
#' @references Economics overview documentation, Section 2.1.c
#' @export
compute_rtp <- function(unemployment_path, config_employment,
                        historical_rtp = NULL) {
  checkmate::assert_data_table(unemployment_path)
  checkmate::assert_list(config_employment)

  u_star <- config_employment$ultimate_unemployment_rate
  beta <- config_employment$okun_coefficient

  if (is.null(u_star)) cli::cli_abort("ultimate_unemployment_rate not set in config")
  if (is.null(beta)) cli::cli_abort("okun_coefficient not set in config")

  # --- Projected RTP from Okun's Law ---
  dt <- data.table::copy(unemployment_path)
  data.table::setorder(dt, year)
  dt[, rtp := 1 + (u_star - rate) * beta / 100]

  # Interpolate annual RTP to quarterly using cubic spline.
  # Annual values are treated as midyear (Q2/Q3 boundary), and the spline
  # produces smooth quarterly values so that D(RTP) = RTP(q) - RTP(q-1)
  # has realistic quarter-to-quarter variation rather than step changes
  # at year boundaries.
  annual_times <- dt$year + 0.5  # midyear
  annual_rtp <- dt$rtp

  # Quarterly midpoints: Q1=0.125, Q2=0.375, Q3=0.625, Q4=0.875
  q_offsets <- c(0.125, 0.375, 0.625, 0.875)
  all_years <- dt$year
  q_times <- sort(as.vector(outer(all_years, q_offsets, `+`)))

  # Cubic spline interpolation with natural boundary conditions
  spline_fit <- stats::splinefun(annual_times, annual_rtp, method = "natural")
  q_rtp <- spline_fit(q_times)

  projected <- data.table::data.table(
    year = rep(all_years, each = 4),
    quarter = rep(1:4, times = length(all_years)),
    rtp = q_rtp
  )

  # --- Splice with historical RTP ---
  if (!is.null(historical_rtp) && nrow(historical_rtp) > 0) {
    checkmate::assert_data_table(historical_rtp)
    hist <- data.table::copy(historical_rtp[, .(year, quarter, rtp)])

    # Keep only historical quarters that precede the projected period
    first_proj_year <- min(projected$year)
    hist <- hist[year < first_proj_year]

    if (nrow(hist) > 0) {
      quarterly <- data.table::rbindlist(list(hist, projected), use.names = TRUE)
      data.table::setorder(quarterly, year, quarter)

      cli::cli_alert_success(
        "Spliced quarterly RTP: {nrow(hist)} historical + {nrow(projected)} projected = {nrow(quarterly)} total ({min(quarterly$year)}Q{min(quarterly[year == min(year)]$quarter)}-{max(quarterly$year)}Q{max(quarterly[year == max(year)]$quarter)})"
      )
      return(quarterly)
    }
  }

  cli::cli_alert_success("Computed quarterly RTP: {nrow(projected)} rows ({min(projected$year)}-{max(projected$year)})")

  projected
}

# =============================================================================
# Disability Prevalence Ratio (RD)
# =============================================================================

#' Construct disability prevalence ratio by age and sex
#'
#' @description
#' Constructs age-sex-specific RD by combining:
#' - Published DI age profile (from DI ASR + SSA Supplement) as the shape
#' - V.C5 aggregate prevalence rate for year-to-year scaling
#'
#' For ages 16-61: current-year age-specific RD.
#' For ages 62-74: cohort RD at age 61 (lagged).
#'
#' This will be replaced when the Beneficiaries subprocess computes
#' age-sex-specific disability prevalence directly.
#'
#' @param di_prevalence V.C5 disability prevalence (data.table with year,
#'   age_sex_adj_prevalence_rate)
#' @param di_age_profile Age-sex-specific prevalence rates from load_di_age_profile()
#'   (data.table with age_group, sex, prevalence_rate in per-1,000 insured)
#' @param config_employment Employment config section
#'
#' @return data.table with columns: year, age_group, sex, rd
#'
#' @export
construct_disability_ratio <- function(di_prevalence, di_age_profile, config_employment) {
  checkmate::assert_data_table(di_prevalence)
  checkmate::assert_data_table(di_age_profile)
  checkmate::assert_names(names(di_prevalence),
                          must.include = c("year", "age_sex_adj_prevalence_rate"))
  checkmate::assert_names(names(di_age_profile),
                          must.include = c("age_group", "sex", "prevalence_rate"))

  cli::cli_alert_info("Constructing disability prevalence ratio (RD)")

  # V.C5 aggregate rate by year (per 1,000 insured)
  agg_rates <- di_prevalence[, .(year, agg_rate = age_sex_adj_prevalence_rate)]
  years <- sort(unique(agg_rates$year))

  # Normalize profile to a shape with mean = 1.0
  # so that: RD(age, sex, year) = shape(age, sex) × V.C5(year) / 1000
  profile_mean <- mean(di_age_profile$prevalence_rate, na.rm = TRUE)
  profile <- data.table::copy(di_age_profile)
  profile[, shape := prevalence_rate / profile_mean]

  # ── Ages 16-61: expand profile to all years, scaled by V.C5 ──────
  base_rd <- data.table::CJ(
    year = years,
    age_group = unique(profile$age_group),
    sex = c("male", "female")
  )
  base_rd <- merge(base_rd, profile[, .(age_group, sex, shape)],
                    by = c("age_group", "sex"), all.x = TRUE)
  base_rd <- merge(base_rd, agg_rates, by = "year", all.x = TRUE)
  base_rd[, rd := shape * agg_rate / 1000]

  na_count <- sum(is.na(base_rd$rd))
  if (na_count > 0) {
    cli::cli_abort(c(
      "NA values in base RD computation",
      "x" = "{na_count} cells have NA — check DI age profile and V.C5 data for missing values"
    ))
  }

  # Expand age groups to single-year ages for compatibility with LFPR code
  age_group_map <- data.table::data.table(
    age = c(16:17, 18:19, 20:24, 25:29, 30:34, 35:39,
            40:44, 45:49, 50:54, 55:59, 60:61),
    age_group = c(rep("16-17", 2), rep("18-19", 2), rep("20-24", 5),
                   rep("25-29", 5), rep("30-34", 5), rep("35-39", 5),
                   rep("40-44", 5), rep("45-49", 5), rep("50-54", 5),
                   rep("55-59", 5), rep("60-61", 2))
  )

  base_expanded <- merge(
    data.table::CJ(year = years, age = 16:61, sex = c("male", "female")),
    age_group_map, by = "age"
  )
  base_expanded <- merge(base_expanded, base_rd[, .(year, age_group, sex, rd)],
                          by = c("year", "age_group", "sex"), all.x = TRUE)
  base_expanded[, age_group := as.character(age)]
  base_expanded[, age := NULL]

  # ── Ages 62-74: cohort RD at age 61 (lagged) ────────────────────
  # Person at age a in year y was age 61 in year (y - a + 61)
  rd_at_61 <- base_rd[age_group == "60-61", .(year, sex, rd_61 = rd)]

  cohort_rd <- data.table::CJ(year = years, age = 62:74, sex = c("male", "female"))
  cohort_rd[, cohort_year := year - age + 61L]
  cohort_rd <- merge(cohort_rd, rd_at_61,
                      by.x = c("cohort_year", "sex"), by.y = c("year", "sex"),
                      all.x = TRUE)
  # Cohorts whose age-61 year predates V.C5 data range cannot be computed
  min_vc5_year <- min(rd_at_61$year)
  n_missing <- sum(is.na(cohort_rd$rd_61))
  if (n_missing > 0) {
    cli::cli_alert_warning(
      "{n_missing} cohort RD cells have age-61 year before V.C5 start ({min_vc5_year}) — using earliest available V.C5 value"
    )
    earliest_rd61 <- rd_at_61[, .SD[year == min(year)], by = sex]
    cohort_rd <- merge(cohort_rd, earliest_rd61[, .(sex, rd_61_earliest = rd_61)],
                        by = "sex", all.x = TRUE)
    cohort_rd[is.na(rd_61), rd_61 := rd_61_earliest]
    cohort_rd[, rd_61_earliest := NULL]
  }

  cohort_rd[, `:=`(age_group = as.character(age), rd = rd_61)]
  cohort_rd <- cohort_rd[, .(year, age_group, sex, rd)]

  # ── Ages 75-100: RD = 0 (DI converts to retirement) ─────────────
  old_age_rd <- data.table::CJ(year = years, age_group = as.character(75:100),
                                 sex = c("male", "female"))
  old_age_rd[, rd := 0]

  # ── Combine all ages ─────────────────────────────────────────────
  result <- data.table::rbindlist(list(base_expanded, cohort_rd, old_age_rd))
  na_final <- sum(is.na(result$rd))
  if (na_final > 0) {
    cli::cli_abort(c(
      "NA values in final RD table",
      "x" = "{na_final} cells have NA after combining all age ranges"
    ))
  }
  data.table::setorder(result, year, sex, age_group)

  cli::cli_alert_success(
    "Constructed RD: {nrow(result)} cells ({min(years)}-{max(years)}), ages 16-100"
  )

  result
}

# =============================================================================
# RRADJ and POT_ET_TXRT (Retirement Variables)
# =============================================================================

#' Construct replacement rate adjustment
#'
#' @description
#' RRADJ measures the change in the PIA replacement rate at claiming ages 62-69
#' due to NRA increasing from 66 to 67. For each cohort, it compares the
#' benefit adjustment factor (ERR/DRC) at the actual NRA against a fixed NRA
#' of 66, scaled by the medium worker's base replacement rate from V.C7.
#'
#' Formula: RRADJ(a, y) = base_RR × [adj(a, actual_NRA) - adj(a, NRA=66)]
#'
#' Where:
#' - base_RR = V.C7 medium worker pct_earnings_nra / 100 (PIA / career-avg)
#' - adj(a, NRA) = benefit adjustment factor at claiming age a given NRA
#'   (ERR if a < NRA, DRC if a > NRA, 1.0 if a = NRA)
#'
#' RRADJ is ≤0 for cohorts with NRA > 66 (more ERR / less DRC at every claiming
#' age), zero for NRA = 66 cohorts, and transitions smoothly for 1955-1959 cohorts.
#'
#' @param benefit_amounts TR2025 V.C7 benefit amounts (data.table with year,
#'   earnings_level, pct_earnings_nra)
#' @param awi_levels AWI level path from VI.G6 (unused, retained for interface)
#' @param config_employment Employment config section
#'
#' @return data.table with columns: year, age, sex, rradj
#'
#' @references
#' - Actuarial Note 2025.3 (scaled worker methodology)
#' - SSA NRA schedule: born ≤1937 NRA=65, 1938-42 65+2mo/yr, 1943-54 NRA=66,
#'   1955-59 66+2mo/yr, ≥1960 NRA=67
#' - ERR: 5/9% per month for first 36 months before NRA, 5/12% per month beyond
#' - DRC (birth 1943+): 8% per year after NRA
#'
#' @export
construct_rradj <- function(benefit_amounts, awi_levels, config_employment) {
  checkmate::assert_data_table(benefit_amounts)
  checkmate::assert_names(names(benefit_amounts),
                          must.include = c("year", "earnings_level", "pct_earnings_nra"))

  cli::cli_alert_info("Constructing RRADJ (replacement rate adjustment)")

  base_year <- config_employment$base_year
  end_year <- config_employment$end_year
  if (is.null(base_year)) cli::cli_abort("economics.employment.base_year not set in config")
  if (is.null(end_year)) cli::cli_abort("economics.employment.end_year not set in config")

  ages <- 62:69
  sexes <- c("male", "female")
  projection_years <- base_year:end_year

  # ── 1. Extract medium worker base replacement rates from V.C7 ──────
  # V.C7 year = year attaining age 65. For claiming age a in year y:
  # birth_year = y - a, v_c7_year = birth_year + 65
  medium_rr <- benefit_amounts[
    earnings_level == "medium",
    .(v_c7_year = year, base_rr = pct_earnings_nra / 100)
  ]

  if (nrow(medium_rr) == 0) {
    cli::cli_abort("No medium earnings level data in V.C7 — cannot compute RRADJ")
  }

  # ── 2. Build year × age grid and look up base RR ──────────────────
  result <- data.table::CJ(year = projection_years, age = ages, sex = sexes)
  result[, birth_year := year - age]
  result[, v_c7_year := birth_year + 65L]

  result <- merge(result, medium_rr, by = "v_c7_year", all.x = TRUE)

  # V.C7 may not extend far enough for youngest cohorts at the end of
  # the projection window (e.g., age 62 in 2100 → V.C7 year 2103).
  # In steady state the replacement rate is constant (AWI and bend points
  # grow at the same rate), so forward-fill the last available V.C7 value.
  missing_rr <- sum(is.na(result$base_rr))
  if (missing_rr > 0) {
    missing_range <- range(result[is.na(base_rr), v_c7_year])
    vc7_max <- max(medium_rr$v_c7_year)
    gap_years <- missing_range[2] - vc7_max

    last_rr <- medium_rr[v_c7_year == vc7_max, base_rr]
    result[is.na(base_rr), base_rr := last_rr]
    cli::cli_alert_warning(
      "Extended V.C7 medium replacement rate ({round(last_rr * 100, 1)}%) to {missing_rr} cells beyond V.C7 year {vc7_max} (steady-state extrapolation, gap = {gap_years} years)"
    )
  }

  # ── 3. Compute NRA and benefit adjustment factors ──────────────────
  result[, nra := compute_nra(birth_year)]

  # Benefit adjustment at actual NRA vs fixed NRA=66
  result[, adj_actual := compute_benefit_adjustment(age, nra)]
  result[, adj_nra66 := compute_benefit_adjustment(age, 66)]

  # ── 4. RRADJ = base_RR × (adj_actual - adj_nra66) ─────────────────
  result[, rradj := base_rr * (adj_actual - adj_nra66)]

  # Clean up intermediate columns
  result[, c("birth_year", "v_c7_year", "base_rr", "nra", "adj_actual", "adj_nra66") := NULL]
  data.table::setorder(result, year, sex, age)

  # Log summary
  nonzero <- result[rradj != 0]
  if (nrow(nonzero) > 0) {
    cli::cli_alert_info(
      "RRADJ range: {round(min(nonzero$rradj), 4)} to {round(max(nonzero$rradj), 4)} (nonzero for NRA > 66 cohorts)"
    )
  }

  cli::cli_alert_success("Constructed RRADJ for {nrow(result)} cells ({min(projection_years)}-{max(projection_years)})")
  result
}

#' Compute benefit adjustment factor for early/delayed retirement
#'
#' @description
#' Returns the multiplicative factor applied to PIA based on claiming age
#' relative to NRA:
#' - Before NRA: Early Retirement Reduction (ERR)
#'   - First 36 months: 5/9 of 1% per month
#'   - Additional months: 5/12 of 1% per month
#' - After NRA: Delayed Retirement Credit (DRC, birth 1943+)
#'   - 8% per year (2/3% per month)
#' - At NRA: factor = 1.0 (no adjustment)
#'
#' @param claiming_age Numeric vector: age at which benefits are claimed
#' @param nra Numeric vector: Normal Retirement Age (fractional years)
#'
#' @return Numeric vector of adjustment factors (< 1 for ERR, > 1 for DRC)
#' @keywords internal
compute_benefit_adjustment <- function(claiming_age, nra) {
  # Months between claiming and NRA (negative = before NRA)
  months_from_nra <- round((claiming_age - nra) * 12)

  months_early <- pmax(0L, -months_from_nra)
  months_late <- pmax(0L, months_from_nra)

  # ERR: 5/9% per month for first 36 months, 5/12% per month beyond
  err <- fifelse(months_early <= 36,
    months_early * 5 / 900,
    36 * 5 / 900 + (months_early - 36) * 5 / 1200
  )

  # DRC: 8% per year (2/3% per month) for birth year 1943+
  drc <- months_late * 2 / 300

  1 - err + drc
}

#' Compute Normal Retirement Age for a given birth year
#'
#' @description
#' Returns NRA in years (fractional) based on the Social Security NRA schedule:
#' - Born ≤1937: 65
#' - Born 1938-1942: 65 + 2 months per year after 1937
#' - Born 1943-1954: 66
#' - Born 1955-1959: 66 + 2 months per year after 1954
#' - Born ≥1960: 67
#'
#' @param birth_year Integer vector of birth years
#' @return Numeric vector of NRA in years (e.g., 66.5 = 66 years 6 months)
#' @keywords internal
compute_nra <- function(birth_year) {
  fifelse(birth_year <= 1937, 65,
  fifelse(birth_year <= 1942, 65 + (birth_year - 1937) * 2 / 12,
  fifelse(birth_year <= 1954, 66,
  fifelse(birth_year <= 1959, 66 + (birth_year - 1954) * 2 / 12,
          67))))
}

#' Construct potential earnings test tax rate
#'
#' @description
#' POT_ET_TXRT is the implicit marginal tax rate on earnings above the
#' earnings test threshold for Social Security beneficiaries below NRA.
#'
#' Uses the full NRA schedule to determine rates:
#' - Ages below NRA: $1 withheld per $2 earned above exempt amount → 0.50
#' - Year of reaching NRA (months before NRA month): $1 per $3 → 0.33
#' - At/after NRA: no earnings test → 0
#'
#' @param benefit_params TR2025 benefit/earnings test parameters
#' @param config_employment Employment config section
#'
#' @return data.table with columns: year, age, pot_et_txrt
#'
#' @export
construct_pot_et_txrt <- function(benefit_params, config_employment) {
  cli::cli_alert_info("Constructing POT_ET_TXRT (earnings test tax rate)")

  base_year <- config_employment$base_year
  end_year <- config_employment$end_year
  if (is.null(end_year)) cli::cli_abort("economics.employment.end_year not set in config")
  years <- (base_year + 1):end_year
  ages <- 62:69
  result <- data.table::CJ(year = years, age = ages)

  # Compute NRA for each cohort (birth_year = year - age)
  result[, birth_year := year - age]
  result[, nra := compute_nra(birth_year)]

  # The integer age at which NRA is reached (floor of NRA)
  result[, nra_age := floor(nra)]

  # Earnings test rates based on relationship to NRA:
  # Below NRA age: 50% ($1 per $2)
  # At NRA age (the year of attaining NRA): 33% ($1 per $3 for months before NRA)
  # Above NRA age: 0% (no earnings test)
  result[, pot_et_txrt := fifelse(
    age < nra_age, 0.50,
    fifelse(age == nra_age, 1 / 3,
            0)
  )]

  result[, c("birth_year", "nra", "nra_age") := NULL]

  cli::cli_alert_success("Constructed POT_ET_TXRT for {nrow(result)} cells")
  result
}

# =============================================================================
# Aggregate Input Construction
# =============================================================================

#' Build all employment input variables
#'
#' @description
#' Master function that constructs all intermediate variables needed by the
#' unemployment and LFPR projection equations.
#'
#' @param projected_population From demography pipeline
#' @param projected_cni_population From demography pipeline (Phase 8E)
#' @param projected_marital_population From demography pipeline (Phase 8C)
#' @param projected_mothers_child_under6 From demography pipeline (Phase 8D):
#'   proportion of females with at least one child under 6, by year and age group
#' @param o_population_stock OP components from demography pipeline
#' @param military_population Armed forces for projection
#' @param tr_economic_assumptions TR V.B1/V.B2 data
#' @param tr_economic_levels TR VI.G6 data (AWI, CPI, GDP levels)
#' @param tr_di_prevalence TR V.C5 data
#' @param tr_benefit_params TR V.C7 data
#' @param di_age_profile DI age-sex profile from load_di_age_profile()
#' @param cps_labor_data CPS ASEC labor force data from IPUMS (unified extract)
#' @param config_employment Employment config section
#'
#' @return Named list of input data.tables:
#'   - quarterly_cni_pop
#'   - quarterly_op
#'   - edscore
#'   - msshare
#'   - rd
#'   - rradj
#'   - pot_et_txrt
#'   - rtp_quarterly
#'   - children_proportions
#'
#' @export
build_employment_inputs <- function(projected_population,
                                     projected_cni_population,
                                     projected_marital_population,
                                     projected_mothers_child_under6,
                                     o_population_stock,
                                     military_population,
                                     tr_economic_assumptions,
                                     tr_economic_levels,
                                     tr_di_prevalence,
                                     tr_benefit_params,
                                     di_age_profile,
                                     cps_labor_data,
                                     historical_rtp = NULL,
                                     config_employment) {
  cli::cli_h1("Building Employment Input Variables")

  # Extend TR economic assumptions past their data endpoint (2100 for TR2025).

  # V.B1/V.B2 only contain data through the standard 75-year projection window.
  # When end_year > max(data year), forward-fill each variable's last value
  # (ultimate steady-state assumption) to cover the extended projection period.
  end_year <- config_employment$end_year
  max_tr_year <- max(tr_economic_assumptions$year)
  if (!is.null(end_year) && end_year > max_tr_year) {
    gap_years <- (max_tr_year + 1L):end_year
    cli::cli_alert_warning(
      "TR economic assumptions end at {max_tr_year}; extending {length(gap_years)} years to {end_year} via steady-state forward-fill"
    )
    # For each variable, replicate its last-year value
    last_vals <- tr_economic_assumptions[year == max_tr_year]
    extension <- data.table::rbindlist(lapply(gap_years, function(yr) {
      row <- data.table::copy(last_vals)
      row[, year := yr]
      row
    }))
    tr_economic_assumptions <- data.table::rbindlist(
      list(tr_economic_assumptions, extension), use.names = TRUE
    )
  }

  # Extend V.C5 disability prevalence similarly
  max_vc5_year <- max(tr_di_prevalence$year)
  if (!is.null(end_year) && end_year > max_vc5_year) {
    gap_years_vc5 <- (max_vc5_year + 1L):end_year
    cli::cli_alert_warning(
      "V.C5 disability prevalence ends at {max_vc5_year}; extending {length(gap_years_vc5)} years to {end_year}"
    )
    last_vc5 <- tr_di_prevalence[year == max_vc5_year]
    vc5_extension <- data.table::rbindlist(lapply(gap_years_vc5, function(yr) {
      row <- data.table::copy(last_vc5)
      row[, year := yr]
      row
    }))
    tr_di_prevalence <- data.table::rbindlist(
      list(tr_di_prevalence, vc5_extension), use.names = TRUE
    )
  }

  # 1. Quarterly CNI population
  cli::cli_h2("Quarterly Population Interpolation")
  quarterly_cni_pop <- compute_cni_population(
    projected_population, military_population,
    projected_cni_population, config_employment
  )

  # 2. Quarterly OP population
  # o_population_stock uses type (I=immigrants, N=nonimmigrants, V=visitors/unlawful)
  # Map to USEMP visa statuses: OP_A (authorized=I), OP_NA (overstayed=N), OP_NO (never auth=V)
  op_stock <- data.table::copy(o_population_stock)
  op_stock[, visa_status := fcase(
    type == "I", "OP_A",
    type == "N", "OP_NA",
    type == "V", "OP_NO",
    default = type
  )]
  quarterly_op <- interpolate_quarterly_population(
    op_stock[, .(year, age, sex, visa_status, population)],
    id_cols = c("age", "sex", "visa_status"),
    value_col = "population"
  )

  # 3. Marital status population (remap demography categories to LFPR categories)
  cli::cli_h2("Marital Status Population")
  marital_cni_pop <- compute_marital_cni_population(projected_cni_population)

  cli::cli_h2("Married Share")
  msshare <- compute_married_share(projected_marital_population)

  # 4. Educational attainment score
  cli::cli_h2("Educational Attainment Score")
  # Extract education proportions from unified CPS data
  cps_education <- cps_labor_data[concept == "education_proportion"]
  if (nrow(cps_education) > 0) {
    # Reshape: child_status column holds education_level in education rows
    cps_edu_reshaped <- data.table::data.table(
      year = cps_education$year,
      age_group = cps_education$age_group,
      sex = cps_education$sex,
      education_level = cps_education$child_status,
      proportion = cps_education$value
    )
    proj_years <- (config_employment$base_year + 1):config_employment$end_year
    edscore <- compute_edscore(cps_edu_reshaped, projection_years = proj_years)
  } else {
    cli::cli_abort(c(
      "No education data found in CPS extract (concept == 'education_proportion' has 0 rows).",
      "i" = "EDSCORE is required for LFPR projection — ensure CPS labor data includes education proportions."
    ))
  }

  # 5. Disability prevalence ratio
  cli::cli_h2("Disability Prevalence Ratio")
  rd <- construct_disability_ratio(tr_di_prevalence, di_age_profile, config_employment)

  # 6. RRADJ and POT_ET_TXRT
  cli::cli_h2("Retirement Variables")
  rradj <- construct_rradj(
    tr_benefit_params,
    tr_economic_levels,
    config_employment
  )
  pot_et_txrt <- construct_pot_et_txrt(tr_benefit_params, config_employment)

  # 7. Children under 6 proportions
  cli::cli_h2("Children Under 6 Proportions")
  children_proportions <- construct_children_proportions(
    cps_labor_data, projected_mothers_child_under6, config_employment
  )

  # 8. RTP
  cli::cli_h2("RTP (Real/Potential GDP Ratio)")
  # Extract unemployment rate path from TR assumptions (deduplicate)
  unemployment_path <- unique(tr_economic_assumptions[
    variable == "unemployment_rate",
    .(year, rate = value)
  ])
  # Apply user's ultimate UR override so RTP converges to 1.0 at the user's
  # natural rate, not V.B2's terminal rate
  unemployment_path <- override_unemployment_path(unemployment_path, config_employment)
  rtp_quarterly <- compute_rtp(unemployment_path, config_employment,
                               historical_rtp = historical_rtp)

  cli::cli_alert_success("All employment inputs constructed")

  list(
    quarterly_cni_pop = quarterly_cni_pop,
    quarterly_op = quarterly_op,
    marital_cni_pop = marital_cni_pop,
    edscore = edscore,
    msshare = msshare,
    rd = rd,
    rradj = rradj,
    pot_et_txrt = pot_et_txrt,
    rtp_quarterly = rtp_quarterly,
    children_proportions = children_proportions
  )
}

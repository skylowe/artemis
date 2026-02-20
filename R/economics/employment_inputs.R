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

  # Merge with military (constant from military_constant_year)
  mil_const <- military_population[, .(age, sex, M = population)]

  # Compute year-over-year growth ratios
  data.table::setorder(total_pop, age, sex, year)
  total_pop[, P_lag := shift(P, 1), by = .(age, sex)]

  # Initialize N from historical CNI in base year
  total_pop <- merge(total_pop, mil_const, by = c("age", "sex"), all.x = TRUE)
  total_pop[is.na(M), M := 0]

  # Apply Eq 2.1.2
  total_pop[!is.na(P_lag) & P_lag > 0,
            N := ((shift(N, 1, fill = NA) + M) * (P / P_lag)) - M,
            by = .(age, sex)]

  # For the first projection year, initialize from historical CNI
  if (!is.null(historical_cni) && nrow(historical_cni) > 0) {
    base_cni <- historical_cni[year == base_year]
    if (nrow(base_cni) > 0) {
      total_pop <- merge(total_pop, base_cni[, .(age, sex, N_hist = population)],
                         by = c("age", "sex"), all.x = TRUE)
      total_pop[year == base_year & !is.na(N_hist), N := N_hist]
      total_pop[, N_hist := NULL]
    }
  }

  # Forward fill N using Eq 2.1.2
  projection_years <- sort(unique(total_pop[year > base_year, year]))
  for (yr in projection_years) {
    prev_data <- total_pop[year == yr - 1, .(age, sex, N_prev = N, M_prev = M)]
    curr_data <- total_pop[year == yr]
    curr_data <- merge(curr_data, prev_data, by = c("age", "sex"), all.x = TRUE)

    curr_data[!is.na(N_prev) & !is.na(P_lag) & P_lag > 0,
              N := ((N_prev + M_prev) * (P / P_lag)) - M]

    total_pop[year == yr, N := curr_data$N]
  }

  result <- total_pop[!is.na(N), .(year, age, sex, population = N)]

  # Interpolate to quarterly
  quarterly <- interpolate_quarterly_population(result, id_cols = c("age", "sex"))

  cli::cli_alert_success("Computed CNI population: {nrow(quarterly)} quarterly rows")
  quarterly
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
#' @param education_data CPS educational attainment proportions by age, sex, year
#'   (data.table with year, age, sex, education_level, proportion)
#' @param projection_years Years to project forward
#'
#' @return data.table with columns: year, age_group, sex, edscore
#'
#' @export
compute_edscore <- function(education_data, projection_years = 2025:2100) {
  checkmate::assert_data_table(education_data)

  # Education level weights (higher weight = higher participation differential)
  edu_weights <- c(
    less_than_hs = 0.0,
    high_school = 0.5,
    some_college = 0.75,
    bachelors_plus = 1.0
  )

  # Historical EDSCORE
  historical <- education_data[, .(
    edscore = sum(proportion * edu_weights[education_level], na.rm = TRUE)
  ), by = .(year, age, sex)]

  data.table::setnames(historical, "age", "age_group")

  # Cohort-based projection
  # The educational composition at age a in year t is determined by the
  # cohort born in year (t-a), whose education was observed when younger
  last_hist_year <- max(historical$year)
  projection <- list()

  for (yr in projection_years) {
    for (sex in c("male", "female")) {
      min_age <- if (sex == "male") 55 else 50
      for (age in min_age:100) {
        # Look up this cohort's EDSCORE from when they were younger
        birth_year <- yr - age
        # Find the most recent observation for this cohort
        obs_age <- last_hist_year - birth_year
        if (obs_age >= min_age && obs_age <= 100) {
          row <- historical[age_group == as.character(obs_age) & sex == (!!sex) &
                           year == last_hist_year]
          edscore_val <- if (nrow(row) > 0) row$edscore[1] else NA_real_
        } else if (obs_age > 0 && obs_age < min_age) {
          # Cohort hasn't aged into the target range yet; use last available
          row <- historical[age_group == as.character(min_age) & sex == (!!sex) &
                           year == last_hist_year]
          edscore_val <- if (nrow(row) > 0) row$edscore[1] else NA_real_
        } else {
          # Extrapolate from trend
          edscore_val <- NA_real_
        }

        if (!is.na(edscore_val)) {
          projection[[length(projection) + 1]] <- data.table::data.table(
            year = yr, age_group = as.character(age), sex = sex,
            edscore = edscore_val
          )
        }
      }
    }
  }

  projected <- data.table::rbindlist(projection)
  result <- data.table::rbindlist(list(historical, projected), fill = TRUE)
  data.table::setorder(result, year, sex, age_group)

  cli::cli_alert_success("Computed EDSCORE for {nrow(result)} age-sex-year cells")

  result
}

# =============================================================================
# RTP (Ratio of Real to Potential GDP)
# =============================================================================

#' Compute RTP from unemployment rate path
#'
#' @description
#' Derives the ratio of real GDP to potential GDP from the published
#' unemployment rate using Okun's Law:
#'   RTP = 1 + (u* - u) × beta / 100
#'
#' where u* = natural rate (ultimate unemployment rate), u = actual,
#' and beta = Okun coefficient (~2).
#'
#' @param unemployment_path Annual unemployment rate path (data.table with year, rate)
#' @param config_employment Employment config section
#'
#' @return data.table with columns: year, quarter, rtp
#'
#' @references Economics overview documentation
#' @export
compute_rtp <- function(unemployment_path, config_employment) {
  checkmate::assert_data_table(unemployment_path)
  checkmate::assert_list(config_employment)

  u_star <- config_employment$ultimate_unemployment_rate
  beta <- config_employment$okun_coefficient

  if (is.null(u_star)) cli::cli_abort("ultimate_unemployment_rate not set in config")
  if (is.null(beta)) cli::cli_abort("okun_coefficient not set in config")

  dt <- data.table::copy(unemployment_path)
  dt[, rtp := 1 + (u_star - rate) * beta / 100]

  # Interpolate annual RTP to quarterly
  # Use smooth cubic spline for quarterly disaggregation
  years <- dt$year
  rtp_vals <- dt$rtp

  # Create quarterly time points
  quarterly_times <- sort(c(
    outer(years, c(0.125, 0.375, 0.625, 0.875), `+`)
  ))
  quarterly_rtp <- stats::approx(years + 0.5, rtp_vals, quarterly_times,
                                  rule = 2)$y

  quarterly <- data.table::data.table(
    year = as.integer(floor(quarterly_times)),
    quarter = rep(1:4, length(years)),
    rtp = quarterly_rtp
  )

  # Ensure year assignment is correct
  quarterly[, year := as.integer(floor(quarterly_times))]
  quarterly[, quarter := ((seq_len(.N) - 1) %% 4) + 1]

  # Simpler approach: repeat annual RTP for each quarter
  quarterly <- dt[, .(quarter = 1:4, rtp = rtp), by = .(year)]

  cli::cli_alert_success("Computed quarterly RTP: {nrow(quarterly)} rows ({min(quarterly$year)}-{max(quarterly$year)})")

  quarterly
}

# =============================================================================
# Disability Prevalence Ratio (RD)
# =============================================================================

#' Construct disability prevalence ratio by age and sex
#'
#' @description
#' Constructs RD from TR2025 V.C5 disability prevalence data.
#' For ages 62-74, uses the cohort's RD at age 61.
#'
#' @param di_prevalence TR2025 disability prevalence data
#'   (data.table with year, rate or similar)
#' @param config_employment Employment config section
#'
#' @return data.table with columns: year, age_group, sex, rd
#'
#' @export
construct_disability_ratio <- function(di_prevalence, config_employment) {
  checkmate::assert_data_table(di_prevalence)

  cli::cli_alert_info("Constructing disability prevalence ratio (RD)")

  # V.C5 provides aggregate DI prevalence rates
  # For USEMP, we need age-sex specific RD
  # Initial implementation: use aggregate rate scaled by age profile

  # Placeholder: return uniform RD by age and sex

  # This will be refined when Beneficiaries subprocess provides age-sex detail
  years <- sort(unique(di_prevalence$year))
  ages <- 16:100
  sexes <- c("male", "female")

  result <- data.table::CJ(year = years, age_group = as.character(ages), sex = sexes)

  # Aggregate prevalence rate from V.C5
  agg_rate <- merge(result[, .(year)], di_prevalence, by = "year", all.x = TRUE)

  # Scale by age: disability prevalence rises with age
  result[, age_num := as.integer(age_group)]
  result[, rd := fifelse(
    age_num < 20, 0.001,
    fifelse(age_num < 30, 0.005,
    fifelse(age_num < 40, 0.015,
    fifelse(age_num < 50, 0.035,
    fifelse(age_num < 60, 0.065,
    fifelse(age_num <= 61, 0.090,
    0  # Ages 62+ use cohort RD at 61, handled by caller
  ))))))]

  result[, age_num := NULL]

  cli::cli_alert_success("Constructed RD for {nrow(result)} age-sex-year cells")
  result
}

# =============================================================================
# RRADJ and POT_ET_TXRT (Retirement Variables)
# =============================================================================

#' Construct replacement rate adjustment
#'
#' @description
#' RRADJ represents the change in PIA replacement rate at ages 62-69
#' due to NRA increases. Constructed from V.C7 benefit amounts and AWI.
#'
#' @param benefit_amounts TR2025 V.C7 benefit amounts
#' @param awi_levels AWI level path from VI.G6
#' @param config_employment Employment config section
#'
#' @return data.table with columns: year, age, sex, rradj
#'
#' @export
construct_rradj <- function(benefit_amounts, awi_levels, config_employment) {
  checkmate::assert_data_table(benefit_amounts)

  cli::cli_alert_info("Constructing RRADJ (replacement rate adjustment)")

  # RRADJ = change in replacement rate due to NRA shifting from 66 to 67
  # For now, construct from V.C7 medium-scaled worker PIA / AWI
  years <- sort(unique(benefit_amounts$year))
  ages <- 62:69
  sexes <- c("male", "female")

  result <- data.table::CJ(year = years, age = ages, sex = sexes)
  # Initial implementation: RRADJ is small and declining as NRA transition completes
  # NRA reached 67 for cohorts born 1960+ (turning 67 in 2027)
  result[, rradj := fifelse(year <= 2027, 0.01, 0)]

  cli::cli_alert_success("Constructed RRADJ for {nrow(result)} cells")
  result
}

#' Construct potential earnings test tax rate
#'
#' @description
#' POT_ET_TXRT is the implicit marginal tax rate on earnings above the
#' earnings test threshold for Social Security beneficiaries below NRA.
#'
#' @param benefit_params TR2025 benefit/earnings test parameters
#' @param config_employment Employment config section
#'
#' @return data.table with columns: year, age, pot_et_txrt
#'
#' @export
construct_pot_et_txrt <- function(benefit_params, config_employment) {
  cli::cli_alert_info("Constructing POT_ET_TXRT (earnings test tax rate)")

  # The earnings test withholds $1 for every $2 earned above threshold (ages 62 to NRA-1)
  # and $1 for every $3 in the year of reaching NRA
  # Effective marginal tax rate depends on benefit amount relative to earnings

  years <- 2025:2100
  ages <- 62:69
  result <- data.table::CJ(year = years, age = ages)

  # NRA is 67 for cohorts born 1960+ (age 67 in 2027+)
  # Before NRA: $1 withheld per $2 over exempt amount → 50% marginal rate
  # At NRA year: $1 per $3 → 33% rate
  # At/after NRA: no earnings test
  result[, nra := 67]
  result[, pot_et_txrt := fifelse(age < nra, 0.50, fifelse(age == nra, 0.33, 0))]
  result[, nra := NULL]

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
#' @param o_population_stock OP components from demography pipeline
#' @param military_population Armed forces for projection
#' @param tr2025_economic_assumptions TR2025 V.B1/V.B2 data
#' @param tr2025_di_prevalence TR2025 V.C5 data
#' @param tr2025_benefit_params TR2025 V.C7/V.C1 data
#' @param cps_education CPS educational attainment data
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
                                     o_population_stock,
                                     military_population,
                                     tr2025_economic_assumptions,
                                     tr2025_di_prevalence,
                                     tr2025_benefit_params,
                                     cps_education,
                                     config_employment) {
  cli::cli_h1("Building Employment Input Variables")

  # 1. Quarterly CNI population
  cli::cli_h2("Quarterly Population Interpolation")
  quarterly_cni_pop <- compute_cni_population(
    projected_population, military_population,
    projected_cni_population, config_employment
  )

  # 2. Quarterly OP population
  quarterly_op <- interpolate_quarterly_population(
    o_population_stock,
    id_cols = c("age", "sex", "visa_status"),
    value_col = "population"
  )

  # 3. Married share
  cli::cli_h2("Married Share")
  msshare <- compute_married_share(projected_marital_population)

  # 4. Educational attainment score
  cli::cli_h2("Educational Attainment Score")
  edscore <- compute_edscore(cps_education)

  # 5. Disability prevalence ratio
  cli::cli_h2("Disability Prevalence Ratio")
  rd <- construct_disability_ratio(tr2025_di_prevalence, config_employment)

  # 6. RRADJ and POT_ET_TXRT
  cli::cli_h2("Retirement Variables")
  rradj <- construct_rradj(
    tr2025_benefit_params,
    tr2025_economic_assumptions,
    config_employment
  )
  pot_et_txrt <- construct_pot_et_txrt(tr2025_benefit_params, config_employment)

  # 7. RTP
  cli::cli_h2("RTP (Real/Potential GDP Ratio)")
  # Extract unemployment rate path from TR2025 assumptions
  unemployment_path <- tr2025_economic_assumptions[
    variable == "unemployment_rate",
    .(year, rate = value)
  ]
  rtp_quarterly <- compute_rtp(unemployment_path, config_employment)

  cli::cli_alert_success("All employment inputs constructed")

  list(
    quarterly_cni_pop = quarterly_cni_pop,
    quarterly_op = quarterly_op,
    edscore = edscore,
    msshare = msshare,
    rd = rd,
    rradj = rradj,
    pot_et_txrt = pot_et_txrt,
    rtp_quarterly = rtp_quarterly,
    children_proportions = NULL  # Placeholder — will construct from CPS/demography
  )
}

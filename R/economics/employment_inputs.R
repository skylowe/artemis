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
  data.table::setorder(total_pop, age, sex, year)
  projection_years <- sort(unique(total_pop[year > base_year, year]))
  for (yr in projection_years) {
    prev <- total_pop[year == yr - 1, .(age, sex, N_prev = N)]
    total_pop <- merge(total_pop, prev, by = c("age", "sex"), all.x = TRUE,
                       suffixes = c("", ".prev"))
    total_pop[year == yr & !is.na(N_prev) & !is.na(P_lag) & P_lag > 0,
              N := ((N_prev + M) * (P / P_lag)) - M]
    total_pop[, N_prev := NULL]
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
compute_edscore <- function(education_data, projection_years = 2025:2100) {
  checkmate::assert_data_table(education_data)

  # Education level weights (higher weight = higher participation differential)
  edu_weights <- c(
    less_than_hs = 0.0,
    high_school = 0.5,
    some_college = 0.75,
    bachelors_plus = 1.0
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

  # For projection: hold each age group's EDSCORE at last historical value
  # (simplified cohort projection — use last observed value for each age-sex group)
  last_hist_year <- max(historical$year)
  last_values <- historical[year == last_hist_year, .(age_group, sex, edscore)]

  projection <- last_values[, .(year = projection_years, edscore = edscore),
                             by = .(age_group, sex)]

  result <- data.table::rbindlist(list(historical, projection), fill = TRUE)
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

  quarterly <- data.table::data.table(
    year = rep(all_years, each = 4),
    quarter = rep(1:4, times = length(all_years)),
    rtp = q_rtp
  )

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

  # V.C5 provides the age-sex adjusted prevalence rate per 1,000 insured.
  # This is an aggregate rate. For USEMP, we need age-specific RD.
  #
  # Approach: Scale the aggregate rate by an age profile derived from
  # published SSA disability statistics. The age profile shape comes from
  # the DI beneficiary age distribution (SSA Annual Statistical Supplement),
  # normalized so the population-weighted average matches V.C5's aggregate.
  #
  # This will be replaced when the Beneficiaries subprocess computes
  # age-sex-specific disability prevalence directly.

  checkmate::assert_names(names(di_prevalence),
                          must.include = c("year", "age_sex_adj_prevalence_rate"))

  base_year <- config_employment$base_year %||% 2024

  # Get aggregate prevalence rate by year (per 1,000 → convert to proportion)
  agg_rd <- di_prevalence[, .(year, agg_rd = age_sex_adj_prevalence_rate / 1000)]

  # Age profile: relative disability prevalence by age

  # From published SSA data, disability prevalence rises steeply with age:
  # ~0.5% at age 20, ~2% at age 40, ~8% at age 55, peaking ~9% at age 60-61.
  # After age 62, DI beneficiaries convert to retirement benefits, so RD
  # for LFPR purposes is 0 (handled by setting default = 0 below).
  # The relative profile is normalized so the weighted average = 1.0,
  # then multiplied by the aggregate rate to produce age-specific RD.
  age_profile <- data.table::data.table(
    age_num = 16:100,
    # Relative profile (will be multiplied by aggregate rate)
    rel_profile = c(
      rep(0.05, 4),    # 16-19: very low
      rep(0.15, 5),    # 20-24
      rep(0.25, 5),    # 25-29
      rep(0.45, 5),    # 30-34
      rep(0.65, 5),    # 35-39
      rep(0.90, 5),    # 40-44
      rep(1.20, 5),    # 45-49
      rep(1.60, 5),    # 50-54
      rep(2.10, 5),    # 55-59
      rep(2.40, 2),    # 60-61
      rep(0, 39)       # 62-100: DI converts to retirement
    )
  )

  # Normalize so weighted average ≈ 1.0 (equal population weight approximation)
  working_ages <- age_profile[age_num <= 61]
  mean_profile <- mean(working_ages$rel_profile)
  age_profile[, rel_profile := rel_profile / mean_profile]

  # Expand to year × age × sex
  sexes <- c("male", "female")
  years <- sort(unique(agg_rd$year))

  result <- data.table::CJ(year = years, age_group = as.character(16:100), sex = sexes)
  result[, age_num := as.integer(age_group)]

  # Merge age profile
  result <- merge(result, age_profile, by = "age_num", all.x = TRUE)
  result[is.na(rel_profile), rel_profile := 0]

  # Merge aggregate rate by year
  result <- merge(result, agg_rd, by = "year", all.x = TRUE)

  # RD = aggregate_rate × relative_profile
  result[, rd := agg_rd * rel_profile]
  result[is.na(rd), rd := 0]

  # Clean up
  result[, c("age_num", "rel_profile", "agg_rd") := NULL]

  cli::cli_alert_success(
    "Constructed RD for {nrow(result)} age-sex-year cells from V.C5 ({min(years)}-{max(years)})"
  )

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
                                     o_population_stock,
                                     military_population,
                                     tr2025_economic_assumptions,
                                     tr2025_di_prevalence,
                                     tr2025_benefit_params,
                                     cps_labor_data,
                                     config_employment) {
  cli::cli_h1("Building Employment Input Variables")

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
      age = NA_integer_,  # age_group will be mapped
      sex = cps_education$sex,
      education_level = cps_education$child_status,
      proportion = cps_education$value
    )
    data.table::setnames(cps_edu_reshaped, "age", "age", skip_absent = TRUE)
    edscore <- compute_edscore(cps_edu_reshaped)
  } else {
    cli::cli_alert_warning("No education data in CPS extract — using placeholder EDSCORE")
    edscore <- NULL
  }

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
  # Extract unemployment rate path from TR2025 assumptions (deduplicate)
  unemployment_path <- unique(tr2025_economic_assumptions[
    variable == "unemployment_rate",
    .(year, rate = value)
  ])
  rtp_quarterly <- compute_rtp(unemployment_path, config_employment)

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
    children_proportions = NULL  # Placeholder — will construct from CPS/demography
  )
}

#' DACA Population Projection
#'
#' Functions for projecting the Deferred Action for Childhood Arrivals (DACA)
#' population as a subset of the O (temporary/unlawfully present) population.
#'
#' Per TR2025 Section 1.5.c:
#' "This subprocess also projects the DACA population, a subset of the temporary
#' or unlawfully present immigrant population, by age (x) and sex (s). The DACA
#' population consists of temporary or unlawfully present immigrants who meet
#' specific criteria and are granted authorization to work."
#'
#' @section DACA Program Background:
#' - Established June 15, 2012 by DHS executive memorandum
#' - Provides temporary deportation relief and work authorization
#' - No new initial applications accepted since September 2017
#' - Renewals continue for existing recipients
#' - DAPA and 2014 DACA expansion are no longer being applied
#'
#' @section Key Assumptions (TR2025):
#' - No significant new 2012 DACA grants for years 2019-20 and 2022-23
#' - DACA recipients have lower departure rates than non-DACA
#' - Population ages in place (no new entrants)
#'
#' @name daca_projection
NULL

# =============================================================================
# DACA ELIGIBILITY ESTIMATION
# =============================================================================

#' Estimate DACA-eligible population
#'
#' @description
#' Estimates the population eligible for DACA based on age, residency, and
#' educational requirements. Uses 2012 ACS data as the primary source.
#'
#' Per TR2025: "The eligible DACA population is estimated separately by those
#' that meet the age, residency, and educational requirements."
#'
#' @param acs_2012_daca_data data.table from fetch_acs_2012_daca_eligible()
#'   or estimate_daca_eligible_2012() with eligible population by age/sex
#' @param mpi_estimates Optional data.table from Migration Policy Institute
#'   with eligibility estimates for calibration
#' @param config Optional list with eligibility criteria overrides
#'
#' @return data.table with columns:
#'   - age: Single year of age
#'   - sex: "male" or "female"
#'   - eligible_population: Population meeting DACA criteria
#'   - base_year: Reference year for eligibility (2012)
#'
#' @details
#' DACA 2012 eligibility criteria:
#' 1. Born after June 15, 1981 (under 31 on June 15, 2012)
#' 2. Came to US before June 15, 2007 (continuously resided since)
#' 3. Came to US before 16th birthday
#' 4. In school, high school graduate, GED holder, or military veteran
#' 5. No felony, significant misdemeanor, or 3+ misdemeanors
#'
#' Criminal history cannot be determined from ACS, so we estimate total
#' eligible and apply an adjustment factor.
#'
#' @export
estimate_daca_eligible_population <- function(acs_2012_daca_data = NULL,
                                               mpi_estimates = NULL,
                                               config = NULL) {
  cli::cli_alert_info("Estimating DACA-eligible population...")

  # Get eligibility criteria
  criteria <- get_daca_criteria(config)

  # If pre-computed eligible data provided, use it

  if (!is.null(acs_2012_daca_data) && nrow(acs_2012_daca_data) > 0) {
    # Check if already processed
    if ("eligible_population" %in% names(acs_2012_daca_data)) {
      cli::cli_alert_success("Using pre-computed DACA eligible population")
      result <- data.table::copy(acs_2012_daca_data)
      result[, base_year := 2012]
      return(result)
    }
  }

  # Otherwise compute from scratch or use MPI estimates
  if (!is.null(mpi_estimates)) {
    cli::cli_alert_info("Using MPI eligibility estimates")
    result <- process_mpi_daca_estimates(mpi_estimates)
    return(result)
  }

  # Fallback: Use internally developed estimates based on published sources
  cli::cli_alert_info("Using internally developed DACA eligibility estimates")
  result <- get_default_daca_eligible(config)

  cli::cli_alert_success(
    "DACA eligible population: {format(sum(result$eligible_population), big.mark = ',')}"
  )

  result
}

#' Get DACA eligibility criteria
#'
#' @description
#' Returns the eligibility criteria for the 2012 DACA program.
#'
#' @param config Optional list with overrides
#'
#' @return list with eligibility parameters
#'
#' @keywords internal
get_daca_criteria <- function(config = NULL) {
  # DACA 2012 eligibility criteria
  # Source: DHS Executive Memorandum, June 15, 2012
  # criminal_exclusion_factor from config: immigration.o_immigration.daca

  default <- list(
    max_age_at_announcement = 30L,
    min_birth_year = 1981L,
    max_age_at_entry = 15L,
    continuous_residence_since = 2007L,
    physical_presence_date = 2012L,
    min_age_at_application = 15L,
    program_start_year = 2012L,
    new_grants_cutoff = 2017L,
    criminal_exclusion_factor = 0.90
  )

  # Apply config overrides from YAML path
  daca_cfg <- NULL
  if (!is.null(config)) {
    daca_cfg <- config$immigration$o_immigration$daca
  }
  if (!is.null(daca_cfg)) {
    if (!is.null(daca_cfg$criminal_exclusion_factor)) {
      default$criminal_exclusion_factor <- daca_cfg$criminal_exclusion_factor
    }
  }

  # Apply direct config overrides (legacy)
  if (!is.null(config) && is.list(config)) {
    for (name in intersect(names(config), names(default))) {
      default[[name]] <- config[[name]]
    }
  }

  default
}

#' Get default DACA eligible population estimates
#'
#' @description
#' Returns internally developed DACA eligibility estimates based on published
#' sources including MPI, Pew Research, and USCIS data.
#'
#' @return data.table with eligible population by age and sex
#'
#' @keywords internal
get_default_daca_eligible <- function(config = NULL) {
  # DACA eligibility estimates from config or defaults
  # Sources: MPI (~1.1M eligible 2012), Pew Research, USCIS grant data
  daca_cfg <- NULL
  if (!is.null(config)) daca_cfg <- config$immigration$o_immigration$daca

  total_eligible <- daca_cfg$total_eligible_2012 %||% 1100000L

  # Age distribution of eligibles (2012)
  # Peak ages 18-25 due to:
  # - Must be 15+ to apply
  # - Must be under 31 at announcement
  # - Must have entered before age 16
  age_dist <- data.table::data.table(
    age = 15:30,
    age_weight = c(
      0.04,  # 15 - youngest eligible
      0.06,  # 16
      0.07,  # 17
      0.09,  # 18 - peak starts
      0.10,  # 19
      0.10,  # 20
      0.09,  # 21
      0.08,  # 22
      0.07,  # 23
      0.06,  # 24
      0.06,  # 25
      0.05,  # 26
      0.04,  # 27
      0.03,  # 28
      0.03,  # 29
      0.03   # 30 - max eligible age
    )
  )

  # Normalize weights
  age_dist[, age_weight := age_weight / sum(age_weight)]

  # Sex split from config or default
  male_pct <- daca_cfg$male_pct %||% 0.54
  female_pct <- 1 - male_pct

  # Build result
  results <- list()

  for (i in seq_len(nrow(age_dist))) {
    a <- age_dist$age[i]
    w <- age_dist$age_weight[i]

    # Male
    results[[length(results) + 1]] <- data.table::data.table(
      age = a,
      sex = "male",
      eligible_population = as.integer(total_eligible * w * male_pct),
      base_year = 2012L
    )

    # Female
    results[[length(results) + 1]] <- data.table::data.table(
      age = a,
      sex = "female",
      eligible_population = as.integer(total_eligible * w * female_pct),
      base_year = 2012L
    )
  }

  data.table::rbindlist(results)
}

#' Process MPI DACA eligibility estimates
#'
#' @description
#' Processes Migration Policy Institute DACA eligibility estimates into
#' single-year-of-age format.
#'
#' @param mpi_data data.table with MPI estimates
#'
#' @return data.table with eligible population by age and sex
#'
#' @keywords internal
process_mpi_daca_estimates <- function(mpi_data) {
  # MPI provides estimates by age group - convert to single years
  # This is a placeholder - actual MPI data format varies by publication

  if (is.null(mpi_data) || nrow(mpi_data) == 0) {
    return(get_default_daca_eligible())
  }

  # If MPI data has age groups, distribute to single years
  dt <- data.table::copy(mpi_data)

  # Assume MPI provides: age_group, total_eligible, sex_pct_male
  if ("age_group" %in% names(dt)) {
    # Expand age groups to single years
    # (Implementation depends on actual MPI data format)
    cli::cli_alert_warning("MPI data processing not fully implemented, using defaults")
    return(get_default_daca_eligible())
  }

  dt[, base_year := 2012L]
  dt
}

# =============================================================================
# DACA ATTAINMENT RATES
# =============================================================================

#' Calculate DACA attainment rates
#'
#' @description
#' Calculates attainment rates - the proportion of eligible population that
#' actually obtains DACA status. Rates vary by year (first, second, ultimate)
#' and by age/sex.
#'
#' Per TR2025 Input #23: "Internally developed factors of potential DACA stock
#' attaining DACA status by sex and ages 5-100 for the first, second, and
#' ultimate DACA years."
#'
#' @param eligible_population data.table from estimate_daca_eligible_population()
#' @param dhs_grants data.table from fetch_dhs_daca_grants() with initial grants
#' @param config Optional list with rate parameters
#'
#' @return data.table with columns:
#'   - age: Single year of age
#'   - sex: "male" or "female"
#'   - first_year_rate: Attainment rate in first DACA year (FY 2013)
#'   - second_year_rate: Attainment rate in second year (FY 2014)
#'   - ultimate_rate: Attainment rate for ultimate years
#'
#' @export
calculate_daca_attainment_rates <- function(eligible_population = NULL,
                                             dhs_grants = NULL,
                                             config = NULL) {
  cli::cli_alert_info("Calculating DACA attainment rates...")

  # Get rate parameters
  params <- get_daca_rate_params(config)

  # If we have DHS grants data, calibrate rates to actual approvals
  if (!is.null(dhs_grants) && nrow(dhs_grants) > 0) {
    rates <- calibrate_attainment_to_grants(eligible_population, dhs_grants, params)
  } else {
    # Use internally developed rates
    rates <- get_default_attainment_rates(params)
  }

  cli::cli_alert_success("Calculated DACA attainment rates")

  rates
}

#' Get DACA attainment rate parameters
#'
#' @param config Optional list with overrides
#'
#' @return list with rate parameters
#'
#' @keywords internal
get_daca_rate_params <- function(config = NULL) {
  # DACA attainment parameters from config YAML or defaults
  # FY 2013: 472K grants / ~1.1M eligible = ~43% first-year attainment
  # FY 2014: 123K additional / remaining eligible = ~15% second-year
  # Ultimate: Total ~725K grants / 1.1M = ~66% cumulative attainment

  # Try YAML config path
  att_cfg <- NULL
  if (!is.null(config)) att_cfg <- config$immigration$o_immigration$daca$attainment

  default <- list(
    first_year_base_rate = att_cfg$first_year_base_rate %||% 0.43,
    second_year_base_rate = att_cfg$second_year_base_rate %||% 0.15,
    ultimate_base_rate = att_cfg$ultimate_base_rate %||% 0.66,
    age_adjustment_young = att_cfg$age_adjustment_young %||% 0.85,
    age_adjustment_peak = att_cfg$age_adjustment_peak %||% 1.05,
    age_adjustment_older = att_cfg$age_adjustment_older %||% 0.95,
    male_adjustment = att_cfg$male_adjustment %||% 1.02,
    female_adjustment = att_cfg$female_adjustment %||% 0.98,
    annual_decline_factor = att_cfg$annual_decline_factor %||% 0.98
  )

  # Apply direct config overrides (legacy)
  if (!is.null(config) && is.list(config)) {
    for (name in intersect(names(config), names(default))) {
      default[[name]] <- config[[name]]
    }
  }

  default
}

#' Get default DACA attainment rates
#'
#' @param params list with rate parameters
#'
#' @return data.table with rates by age and sex
#'
#' @keywords internal
get_default_attainment_rates <- function(params) {
  # Build rates for all eligible ages
  ages <- 15:50  # Extended range for aging cohort

  results <- list()

  for (a in ages) {
    # Determine age adjustment
    age_adj <- ifelse(a < 18, params$age_adjustment_young,
                      ifelse(a <= 25, params$age_adjustment_peak,
                             params$age_adjustment_older))

    for (s in c("male", "female")) {
      sex_adj <- ifelse(s == "male", params$male_adjustment, params$female_adjustment)

      results[[length(results) + 1]] <- data.table::data.table(
        age = a,
        sex = s,
        first_year_rate = params$first_year_base_rate * age_adj * sex_adj,
        second_year_rate = params$second_year_base_rate * age_adj * sex_adj,
        ultimate_rate = params$ultimate_base_rate * age_adj * sex_adj
      )
    }
  }

  data.table::rbindlist(results)
}

#' Calibrate attainment rates to DHS grant data
#'
#' @param eligible_population data.table with eligible by age/sex
#' @param dhs_grants data.table with grants by fiscal year
#' @param params list with rate parameters
#'
#' @return data.table with calibrated rates
#'
#' @keywords internal
calibrate_attainment_to_grants <- function(eligible_population, dhs_grants, params) {
  # Calculate actual overall attainment from grants vs eligible
  if (is.null(eligible_population)) {
    return(get_default_attainment_rates(params))
  }

  total_eligible <- sum(eligible_population$eligible_population)

  # Get grants for calibration years
  fy13_grants <- dhs_grants[fiscal_year == 2013, initial_grants]
  fy14_grants <- dhs_grants[fiscal_year == 2014, initial_grants]

  if (length(fy13_grants) == 0) fy13_grants <- 472417
  if (length(fy14_grants) == 0) fy14_grants <- 122658

  # Calculate observed rates
  observed_first_year <- fy13_grants / total_eligible
  remaining_after_fy13 <- total_eligible - fy13_grants
  observed_second_year <- fy14_grants / remaining_after_fy13

  # Update params with observed rates
  params$first_year_base_rate <- observed_first_year
  params$second_year_base_rate <- observed_second_year

  # Use updated params to generate rates
  get_default_attainment_rates(params)
}

# =============================================================================
# DACA POPULATION PROJECTION
# =============================================================================

#' Project DACA population
#'
#' @description
#' Projects DACA population forward using eligibility estimates and attainment
#' rates. Accounts for:
#' - Initial grant surge (FY 2013-2014)
#' - Program stabilization (2015-2017)
#' - No new grants period (2017+)
#' - Aging cohort effect
#' - Attrition (non-renewal, departure, death)
#'
#' Per TR2025: "Rates are applied to the eligible population to estimate the
#' net number of individuals who actually apply and obtain DACA status."
#'
#' @param eligible_population data.table from estimate_daca_eligible_population()
#' @param attainment_rates data.table from calculate_daca_attainment_rates()
#' @param dhs_stock data.table from fetch_dhs_daca_stock() for calibration
#' @param projection_years Integer vector of years to project (default: 2023:2099)
#' @param config Optional list with projection parameters
#'
#' @return data.table with columns:
#'   - year: Calendar year
#'   - age: Single year of age
#'   - sex: "male" or "female"
#'   - daca_population: DACA recipient population
#'   - status: "recipient" (active), "aged_out" (no longer eligible)
#'
#' @export
project_daca_population <- function(eligible_population,
                                     attainment_rates,
                                     dhs_stock = NULL,
                                     projection_years = 2023:2099,
                                     config = NULL) {
  cli::cli_alert_info("Projecting DACA population...")

  # Get projection parameters
  params <- get_daca_projection_params(config)

  # Step 1: Build historical DACA population (2012-2022)
  historical <- build_historical_daca(eligible_population, attainment_rates,
                                       dhs_stock, params)

  # Step 2: Get starting population (end of last historical year)
  last_historical_year <- max(historical$year)
  starting_pop <- historical[year == last_historical_year]

  # Step 3: Project forward
  projected <- project_daca_forward(starting_pop, projection_years, params)

  # Combine historical and projected
  result <- data.table::rbindlist(list(historical, projected), fill = TRUE)

  cli::cli_alert_success(
    "Projected DACA population for {length(unique(result$year))} years"
  )

  result
}

#' Get DACA projection parameters
#'
#' @param config Optional list with overrides
#'
#' @return list with projection parameters
#'
#' @keywords internal
get_daca_projection_params <- function(config = NULL) {
  # DACA projection parameters from config YAML or defaults
  # Try YAML config path
  proj_cfg <- NULL
  if (!is.null(config)) proj_cfg <- config$immigration$o_immigration$daca$projection

  default <- list(
    program_start_year = 2012L,
    peak_year = 2017L,
    new_grants_cutoff = 2017L,
    base_attrition_rate = proj_cfg$base_attrition_rate %||% 0.03,
    departure_rate = proj_cfg$departure_rate %||% 0.01,
    death_rate = proj_cfg$death_rate %||% 0.001,
    annual_decline_rate = proj_cfg$annual_decline_rate %||% 0.053,
    minimum_population_pct = proj_cfg$minimum_population_pct %||% 0.30,
    min_active_age = 15L,
    max_active_age = proj_cfg$max_active_age %||% 50L,
    assume_new_grants = FALSE
  )

  # Apply direct config overrides (legacy)
  if (!is.null(config) && is.list(config)) {
    for (name in intersect(names(config), names(default))) {
      default[[name]] <- config[[name]]
    }
  }

  default
}

#' Build historical DACA population
#'
#' @param eligible_population data.table with eligible by age/sex
#' @param attainment_rates data.table with rates
#' @param dhs_stock data.table with DHS stock for calibration
#' @param params list with projection parameters
#'
#' @return data.table with historical DACA by year/age/sex
#'
#' @keywords internal
build_historical_daca <- function(eligible_population, attainment_rates,
                                   dhs_stock, params) {
  # =========================================================================
  # BUILD HISTORICAL DACA POPULATION (2012-2022)
  #
  # Key events:
  # - 2012: Program announced (Aug), first grants
  # - 2013: Peak initial grants (472K)
  # - 2014: Continued grants (123K)
  # - 2015-2017: Stabilization, modest growth to 800K
  # - 2017: Program suspended for new applicants
  # - 2018-2022: Decline due to non-renewal, no new grants
  # =========================================================================

  # DHS total stock by year (for calibration)
  dhs_totals <- data.table::data.table(
    year = 2013:2022,
    dhs_total = c(
      472000L,   # 2013
      610000L,   # 2014
      680000L,   # 2015
      740000L,   # 2016
      800000L,   # 2017 - peak
      700000L,   # 2018
      660000L,   # 2019
      640000L,   # 2020
      616000L,   # 2021
      594000L    # 2022
    )
  )

  # If DHS stock provided, use it for totals
  if (!is.null(dhs_stock) && nrow(dhs_stock) > 0) {
    dhs_totals_from_data <- dhs_stock[, .(dhs_total = sum(daca_population)), by = year]
    # Merge/update with provided data
    dhs_totals <- merge(dhs_totals, dhs_totals_from_data,
                         by = "year", all.x = TRUE, suffixes = c("", "_new"))
    dhs_totals[!is.na(dhs_total_new), dhs_total := dhs_total_new]
    dhs_totals[, dhs_total_new := NULL]
  }

  # Build population for each year
  results <- list()

  for (yr in 2013:2022) {
    target_total <- dhs_totals[year == yr, dhs_total]
    if (length(target_total) == 0) next

    # Calculate age distribution for this year
    # DACA recipients age by one year each year
    year_offset <- yr - 2012  # Years since program start

    yr_pop <- build_daca_year(eligible_population, attainment_rates,
                               yr, year_offset, target_total)

    results[[length(results) + 1]] <- yr_pop
  }

  data.table::rbindlist(results)
}

#' Build DACA population for a single historical year
#'
#' @param eligible_population data.table with base eligible population
#' @param attainment_rates data.table with rates
#' @param year Integer calendar year
#' @param year_offset Integer years since program start
#' @param target_total Integer target total population for calibration
#'
#' @return data.table with DACA population for the year
#'
#' @keywords internal
build_daca_year <- function(eligible_population, attainment_rates,
                             year, year_offset, target_total) {
  # Start with eligible population (base year 2012)
  dt <- data.table::copy(eligible_population)

  # Age the population by year_offset
  dt[, current_age := age + year_offset]

  # Filter to reasonable age range
  dt <- dt[current_age >= 15 & current_age <= 50]

  # Merge attainment rates (using current age)
  dt <- merge(dt, attainment_rates,
              by.x = c("current_age", "sex"),
              by.y = c("age", "sex"),
              all.x = TRUE)

  # Fill missing rates with defaults
  dt[is.na(ultimate_rate), ultimate_rate := 0.66]

  # Calculate raw DACA population
  if (year <= 2013) {
    # First year - use first year rate
    dt[, raw_daca := eligible_population * first_year_rate]
  } else if (year == 2014) {
    # Second year - cumulative
    dt[, raw_daca := eligible_population * (first_year_rate + second_year_rate * (1 - first_year_rate))]
  } else {
    # Ultimate - use ultimate rate
    dt[, raw_daca := eligible_population * ultimate_rate]
  }

  # Calibrate to target total
  raw_total <- sum(dt$raw_daca, na.rm = TRUE)
  if (raw_total > 0) {
    calibration_factor <- target_total / raw_total
    dt[, daca_population := as.integer(raw_daca * calibration_factor)]
  } else {
    dt[, daca_population := 0L]
  }

  # Clean up and format
  result <- dt[, .(
    year = year,
    age = current_age,
    sex = sex,
    daca_population = daca_population,
    status = "recipient"
  )]

  data.table::setorder(result, sex, age)

  result
}

#' Project DACA population forward
#'
#' @param starting_pop data.table with starting population
#' @param projection_years Integer vector of years to project
#' @param params list with projection parameters
#'
#' @return data.table with projected DACA by year/age/sex
#'
#' @keywords internal
project_daca_forward <- function(starting_pop, projection_years, params) {
  # =========================================================================
  # PROJECT DACA FORWARD
  #
  # Key assumptions:
  # 1. No new grants (program closed to new applicants)
  # 2. Existing recipients age in place
  # 3. Annual attrition from non-renewal, departure, death
  # 4. Population declines until reaching a floor
  # =========================================================================

  results <- list()

  # Get last year's population
  current_pop <- data.table::copy(starting_pop)
  last_year <- max(current_pop$year)

  # Calculate floor (minimum population)
  initial_total <- sum(current_pop$daca_population)
  floor_total <- initial_total * params$minimum_population_pct

  for (yr in projection_years) {
    if (yr <= last_year) next

    # Age the population
    current_pop[, age := age + 1L]
    current_pop[, year := yr]

    # Remove those who age out (practical upper limit ~50)
    current_pop <- current_pop[age <= params$max_active_age]

    # Apply attrition
    current_total <- sum(current_pop$daca_population)

    if (current_total > floor_total) {
      # Apply annual decline
      decline_factor <- 1 - params$annual_decline_rate

      # Decline rate decreases as we approach floor
      distance_to_floor <- (current_total - floor_total) / (initial_total - floor_total)
      adjusted_decline <- 1 - (params$annual_decline_rate * distance_to_floor)

      current_pop[, daca_population := as.integer(daca_population * adjusted_decline)]
    }

    # Ensure non-negative
    current_pop[daca_population < 0, daca_population := 0L]

    # Store result
    results[[length(results) + 1]] <- data.table::copy(current_pop)
  }

  if (length(results) == 0) {
    return(data.table::data.table(
      year = integer(),
      age = integer(),
      sex = character(),
      daca_population = integer(),
      status = character()
    ))
  }

  data.table::rbindlist(results)
}

# =============================================================================
# CALIBRATION TO DHS
# =============================================================================

#' Calibrate DACA projection to DHS stock
#'
#' @description
#' Adjusts DACA population projections to match DHS published stock estimates.
#'
#' Per TR2025: "A final adjustment ensures that the DACA population by age and
#' sex is appropriate, based on DHS stock estimates."
#'
#' @param daca_projection data.table from project_daca_population()
#' @param dhs_stock data.table from fetch_dhs_daca_stock()
#' @param calibration_years Integer vector of years to calibrate
#'   (default: 2013:2019, the years with DHS data)
#'
#' @return data.table with calibrated DACA population
#'
#' @export
calibrate_daca_to_dhs <- function(daca_projection, dhs_stock,
                                   calibration_years = 2013:2019) {
  cli::cli_alert_info("Calibrating DACA projection to DHS stock...")

  if (is.null(dhs_stock) || nrow(dhs_stock) == 0) {
    cli::cli_alert_warning("No DHS stock data provided, skipping calibration")
    return(daca_projection)
  }

  dt <- data.table::copy(daca_projection)

  # Calculate DHS totals by year
  dhs_totals <- dhs_stock[, .(dhs_total = sum(daca_population)), by = year]

  # Calculate projection totals by year
  proj_totals <- dt[, .(proj_total = sum(daca_population)), by = year]

  # Merge and calculate adjustment factors
  adj_factors <- merge(proj_totals, dhs_totals, by = "year", all.x = TRUE)
  adj_factors[, adj_factor := ifelse(is.na(dhs_total) | proj_total == 0, 1,
                                      dhs_total / proj_total)]

  # Only apply calibration to calibration years
  adj_factors[!(year %in% calibration_years), adj_factor := 1]

  # Apply adjustments
  dt <- merge(dt, adj_factors[, .(year, adj_factor)], by = "year", all.x = TRUE)
  dt[is.na(adj_factor), adj_factor := 1]
  dt[, daca_population := as.integer(daca_population * adj_factor)]
  dt[, adj_factor := NULL]

  # Report calibration results
  for (yr in calibration_years) {
    if (yr %in% dt$year && yr %in% dhs_totals$year) {
      calibrated <- dt[year == yr, sum(daca_population)]
      target <- dhs_totals[year == yr, dhs_total]
      cli::cli_alert_success(
        "Year {yr}: calibrated to {format(calibrated, big.mark = ',')} (target: {format(target, big.mark = ',')})"
      )
    }
  }

  dt
}

# =============================================================================
# INTEGRATION WITH O POPULATION
# =============================================================================

#' Get DACA population for O emigration rate adjustment
#'
#' @description
#' Extracts DACA population for use in O emigration calculations.
#' DACA recipients have lower departure rates than non-DACA unauthorized.
#'
#' @param daca_projection data.table from project_daca_population() or
#'   calibrate_daca_to_dhs()
#' @param years Integer vector of years to extract
#'
#' @return data.table with DACA population by year/age/sex for rate adjustment
#'
#' @export
get_daca_for_rate_adjustment <- function(daca_projection, years = NULL) {
  dt <- data.table::copy(daca_projection)

  if (!is.null(years)) {
    dt <- dt[year %in% years]
  }

  # Return in format suitable for O emigration calculation
  result <- dt[, .(year, age, sex, daca_population)]
  data.table::setorder(result, year, sex, age)

  result
}

#' Get DACA as share of never-authorized population
#'
#' @description
#' Calculates DACA as a share of never-authorized O population.
#' Used to determine rate adjustment for O emigration.
#'
#' @param daca_projection data.table with DACA population
#' @param o_population data.table with O population by type
#' @param years Integer vector of years
#'
#' @return data.table with DACA share by year/age/sex
#'
#' @export
calculate_daca_share_of_unauthorized <- function(daca_projection, o_population,
                                                   years = NULL) {
  if (is.null(years)) {
    years <- intersect(unique(daca_projection$year), unique(o_population$year))
  }

  # Get never-authorized population
  n_pop <- o_population[type == "N" & year %in% years,
                        .(year, age, sex, n_population = population)]

  # Get DACA population
  daca <- daca_projection[year %in% years,
                          .(year, age, sex, daca_population)]

  # Merge
  result <- merge(n_pop, daca, by = c("year", "age", "sex"), all = TRUE)

  # Fill NAs
  result[is.na(n_population), n_population := 0]
  result[is.na(daca_population), daca_population := 0]

  # Calculate share
  result[, daca_share := ifelse(n_population > 0,
                                 daca_population / n_population,
                                 0)]

  # Cap at reasonable maximum
  result[daca_share > 1, daca_share := 1]

  result[, .(year, age, sex, daca_share)]
}

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Run complete DACA projection
#'
#' @description
#' Main entry point for DACA projection. Orchestrates eligibility estimation,
#' attainment rate calculation, projection, and calibration.
#'
#' @param acs_2012_data data.table from fetch_acs_2012_daca_eligible() (optional)
#' @param dhs_stock data.table from fetch_dhs_daca_stock() (optional)
#' @param dhs_grants data.table from fetch_dhs_daca_grants() (optional)
#' @param projection_years Integer vector of years to project
#' @param config Optional list with parameters
#'
#' @return list with:
#'   - eligible_population: DACA-eligible population (2012)
#'   - attainment_rates: Attainment rates by age/sex
#'   - daca_population: Projected DACA population by year/age/sex
#'   - summary: Summary statistics
#'
#' @export
run_daca_projection <- function(acs_2012_data = NULL,
                                 dhs_stock = NULL,
                                 dhs_grants = NULL,
                                 projection_years = 2023:2099,
                                 config = NULL) {
  cli::cli_h1("DACA Population Projection")

  # Step 1: Estimate eligible population
  cli::cli_h2("Step 1: Eligibility Estimation")
  eligible <- estimate_daca_eligible_population(acs_2012_data, config = config)

  # Step 2: Calculate attainment rates
  cli::cli_h2("Step 2: Attainment Rates")
  rates <- calculate_daca_attainment_rates(eligible, dhs_grants, config = config)

  # Step 3: Project DACA population
  cli::cli_h2("Step 3: Population Projection")
  projection <- project_daca_population(eligible, rates, dhs_stock,
                                         projection_years, config)

  # Step 4: Calibrate to DHS (if stock data available)
  cli::cli_h2("Step 4: DHS Calibration")
  if (!is.null(dhs_stock) && nrow(dhs_stock) > 0) {
    projection <- calibrate_daca_to_dhs(projection, dhs_stock)
  }

  # Generate summary
  summary <- generate_daca_summary(projection)

  cli::cli_alert_success("DACA projection complete")

  list(
    eligible_population = eligible,
    attainment_rates = rates,
    daca_population = projection,
    summary = summary
  )
}

#' Generate DACA projection summary
#'
#' @param daca_projection data.table with projected DACA population
#'
#' @return data.table with summary statistics by year
#'
#' @keywords internal
generate_daca_summary <- function(daca_projection) {
  daca_projection[, .(
    total = sum(daca_population),
    male = sum(daca_population[sex == "male"]),
    female = sum(daca_population[sex == "female"]),
    mean_age = as.integer(weighted.mean(age, daca_population)),  # Using mean as proxy for median
    min_age = min(age[daca_population > 0]),
    max_age = max(age[daca_population > 0])
  ), by = year]
}

# =============================================================================
# VALIDATION
# =============================================================================

#' Validate DACA projection
#'
#' @description
#' Validates DACA projection against DHS stock data and expected patterns.
#'
#' @param daca_projection data.table from run_daca_projection()
#' @param dhs_stock data.table from fetch_dhs_daca_stock()
#' @param tolerance Numeric tolerance for validation (default: 0.10 = 10%)
#'
#' @return list with validation results
#'
#' @export
validate_daca_projection <- function(daca_projection, dhs_stock = NULL,
                                      tolerance = 0.10) {
  cli::cli_alert_info("Validating DACA projection...")

  results <- list(
    checks = list(),
    passed = 0,
    failed = 0
  )

  # Check 1: Non-negative populations
  check1_passed <- all(daca_projection$daca_population >= 0)
  results$checks$non_negative <- check1_passed
  if (check1_passed) {
    results$passed <- results$passed + 1
    cli::cli_alert_success("Check 1 PASSED: All populations non-negative")
  } else {
    results$failed <- results$failed + 1
    cli::cli_alert_danger("Check 1 FAILED: Negative populations found")
  }

  # Check 2: Reasonable age range
  age_range <- range(daca_projection[daca_population > 0, age])
  check2_passed <- age_range[1] >= 15 && age_range[2] <= 60
  results$checks$age_range <- check2_passed
  if (check2_passed) {
    results$passed <- results$passed + 1
    cli::cli_alert_success("Check 2 PASSED: Age range {age_range[1]}-{age_range[2]} is reasonable")
  } else {
    results$failed <- results$failed + 1
    cli::cli_alert_danger("Check 2 FAILED: Unexpected age range")
  }

  # Check 3: Declining trend (no new grants assumption)
  totals <- daca_projection[, .(total = sum(daca_population)), by = year]
  data.table::setorder(totals, year)
  future_totals <- totals[year >= 2020]
  if (nrow(future_totals) > 1) {
    is_declining <- all(diff(future_totals$total) <= 0)
    check3_passed <- is_declining
    results$checks$declining_trend <- check3_passed
    if (check3_passed) {
      results$passed <- results$passed + 1
      cli::cli_alert_success("Check 3 PASSED: Population shows expected decline")
    } else {
      results$failed <- results$failed + 1
      cli::cli_alert_warning("Check 3 FAILED: Population not declining as expected")
    }
  }

  # Check 4: Match DHS stock within tolerance
  if (!is.null(dhs_stock) && nrow(dhs_stock) > 0) {
    dhs_totals <- dhs_stock[, .(dhs_total = sum(daca_population)), by = year]
    proj_totals <- daca_projection[, .(proj_total = sum(daca_population)), by = year]

    comparison <- merge(dhs_totals, proj_totals, by = "year")
    comparison[, pct_diff := abs(proj_total - dhs_total) / dhs_total]

    check4_passed <- all(comparison$pct_diff <= tolerance)
    results$checks$dhs_match <- check4_passed
    results$dhs_comparison <- comparison

    if (check4_passed) {
      results$passed <- results$passed + 1
      cli::cli_alert_success("Check 4 PASSED: Matches DHS within {tolerance*100}% tolerance")
    } else {
      results$failed <- results$failed + 1
      max_diff <- max(comparison$pct_diff) * 100
      cli::cli_alert_danger("Check 4 FAILED: Max difference {round(max_diff, 1)}% exceeds tolerance")
    }
  }

  # Overall result
  results$overall_passed <- results$failed == 0

  cli::cli_alert_info(
    "Validation complete: {results$passed} passed, {results$failed} failed"
  )

  results
}

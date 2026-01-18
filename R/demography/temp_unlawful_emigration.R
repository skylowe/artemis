#' Temporary or Unlawfully Present Emigration
#'
#' Functions for calculating departure rates and projecting O emigration
#' following TR2025 Section 1.5 methodology (Equation 1.5.2).
#'
#' Per TR2025: "These estimates are based on 2014 TR build-up of stocks from
#' 2008 through 2010 including temporary or unlawfully present immigration,
#' deaths, adjustments of status, and assumptions about the number of
#' departures from each type."
#'
#' Key methodology:
#' - Deaths: OD = qx × OP (same mortality as total population)
#' - Rates: OE / OP for each age, sex, type (from 2008-2010 period)
#' - Never-authorized recent arrivals: 2× departure rate
#' - DACA eligible: lower departure rates
#' - Nonimmigrants: rates transition from initial (2015) to ultimate (2025)
#'
#' @name temp_unlawful_emigration
NULL

# =============================================================================
# STOCK BUILD-UP FOR RATE DERIVATION (2008-2010)
# =============================================================================

#' Build up O population stocks for departure rate derivation
#'
#' @description
#' Builds up O population stocks from 2008 through 2010 using the cohort-component
#' method. This stock build-up is used to derive departure rates.
#'
#' Per TR2025: "rates are calculated by dividing OE by OP for each age, sex, and type"
#'
#' @param historical_o_pop Historical O population from HISTORICAL subprocess (Dec 31)
#' @param historical_o_imm Historical O immigration
#' @param historical_aos Historical adjustments of status from LPR subprocess
#' @param mortality_qx Death probabilities by age, sex, year
#' @param type_splits Type proportions (N/I/V) by age, sex for the period
#' @param years Years for stock build-up (default: 2008:2010)
#'
#' @return data.table with O population stock by year, age, sex, type
#'
#' @details
#' The stock equation is:
#' OP^z_{x,s,t} = OP^{z-1}_{x-1,s,t} + OI^z_{x,s,t} - OE^z_{x,s,t} - AOS^z_{x,s,t} - OD^z_{x,s,t}
#'
#' For the initial build-up period, we solve for OE given all other components.
#'
#' @export
build_o_stock_for_rates <- function(historical_o_pop,
                                     historical_o_imm,
                                     historical_aos,
                                     mortality_qx,
                                     type_splits,
                                     years = 2008:2010) {
  checkmate::assert_data_table(historical_o_pop)
  checkmate::assert_data_table(historical_o_imm)
  checkmate::assert_data_table(mortality_qx)

  cli::cli_h3("Building O stock for rate derivation ({min(years)}-{max(years)})")

  # Get starting population (Dec 31 of year before first year)
  start_year <- min(years) - 1
  start_pop <- historical_o_pop[year == start_year]

  if (nrow(start_pop) == 0) {
    cli::cli_abort("No historical O population for starting year {start_year}")
  }

  # Apply type splits to starting population
  start_pop_typed <- apply_type_splits_to_population(start_pop, type_splits)

  results <- list()
  current_pop <- start_pop_typed

  for (yr in years) {
    cli::cli_alert("Processing year {yr}...")

    # Get immigration for this year
    yr_imm <- historical_o_imm[year == yr]
    if (nrow(yr_imm) == 0) {
      cli::cli_alert_warning("No O immigration data for {yr}, using previous year")
      yr_imm <- historical_o_imm[year == yr - 1]
    }

    # Apply type splits to immigration
    yr_imm_typed <- apply_type_splits_to_flow(yr_imm, type_splits)

    # Get AOS for this year (only affects type I and V who become LPR)
    yr_aos <- get_aos_for_year(historical_aos, yr, type_splits)

    # Get mortality for this year
    yr_qx <- mortality_qx[year == yr]
    if (nrow(yr_qx) == 0) {
      yr_qx <- mortality_qx[year == max(mortality_qx$year)]
    }

    # Age the population (x-1 at z-1 becomes x at z)
    aged_pop <- age_population(current_pop)

    # Calculate deaths: OD = qx × OP
    deaths <- calculate_o_deaths(aged_pop, yr_qx)

    # Get ending population for this year from historical data
    end_pop <- historical_o_pop[year == yr]
    end_pop_typed <- apply_type_splits_to_population(end_pop, type_splits)

    # Solve for emigration: OE = aged_pop + OI - AOS - OD - end_pop
    emigration <- calculate_implied_emigration(
      aged_pop = aged_pop,
      immigration = yr_imm_typed,
      aos = yr_aos,
      deaths = deaths,
      end_pop = end_pop_typed
    )

    # Store results
    results[[as.character(yr)]] <- list(
      year = yr,
      population = end_pop_typed,
      immigration = yr_imm_typed,
      emigration = emigration,
      deaths = deaths,
      aos = yr_aos
    )

    # Update current population for next iteration
    current_pop <- end_pop_typed
  }

  # Combine results
  combine_stock_results(results)
}

#' Apply type splits to population stock
#'
#' @keywords internal
apply_type_splits_to_population <- function(population, type_splits) {
  # Ensure we have required columns
  if (!"population" %in% names(population)) {
    if ("o_population" %in% names(population)) {
      data.table::setnames(population, "o_population", "population")
    }
  }

  # Merge with type splits
  pop_typed <- merge(
    population[, .(year, age, sex, population)],
    type_splits[, .(age, sex, type_n, type_i, type_v)],
    by = c("age", "sex"),
    all.x = TRUE
  )

  # Fill missing type splits with defaults
  pop_typed[is.na(type_n), `:=`(type_n = 0.50, type_i = 0.15, type_v = 0.35)]

  # Create long format with type dimension
  result <- data.table::rbindlist(list(
    pop_typed[, .(year, age, sex, type = "N", population = population * type_n)],
    pop_typed[, .(year, age, sex, type = "I", population = population * type_i)],
    pop_typed[, .(year, age, sex, type = "V", population = population * type_v)]
  ))

  data.table::setorder(result, year, type, sex, age)
  result
}

#' Apply type splits to immigration flow
#'
#' @keywords internal
apply_type_splits_to_flow <- function(flow_data, type_splits) {
  # Get the flow column name
  flow_col <- intersect(names(flow_data), c("o_immigration", "immigration", "count"))
  if (length(flow_col) == 0) {
    cli::cli_abort("No flow column found in data")
  }
  flow_col <- flow_col[1]

  # Standardize column name
  dt <- data.table::copy(flow_data)
  data.table::setnames(dt, flow_col, "flow", skip_absent = TRUE)

  # Merge with type splits
  flow_typed <- merge(
    dt[, .(year, age, sex, flow)],
    type_splits[, .(age, sex, type_n, type_i, type_v)],
    by = c("age", "sex"),
    all.x = TRUE
  )

  # Fill missing
  flow_typed[is.na(type_n), `:=`(type_n = 0.50, type_i = 0.15, type_v = 0.35)]

  # Create long format
  result <- data.table::rbindlist(list(
    flow_typed[, .(year, age, sex, type = "N", flow = flow * type_n)],
    flow_typed[, .(year, age, sex, type = "I", flow = flow * type_i)],
    flow_typed[, .(year, age, sex, type = "V", flow = flow * type_v)]
  ))

  data.table::setorder(result, year, type, sex, age)
  result
}

#' Get AOS for a specific year by type
#'
#' @description
#' AOS (Adjustments of Status) only comes from types I (nonimmigrant) and V (overstayer).
#' Never-authorized (N) cannot directly adjust to LPR status.
#'
#' @keywords internal
get_aos_for_year <- function(aos_data, year, type_splits) {
  if (is.null(aos_data) || nrow(aos_data) == 0) {
    # Return empty AOS
    ages <- 0:99
    sexes <- c("male", "female")
    types <- c("N", "I", "V")
    result <- data.table::CJ(year = year, age = ages, sex = sexes, type = types)
    result[, aos := 0]
    return(result)
  }

  yr_aos <- aos_data[year == year]
  if (nrow(yr_aos) == 0) {
    yr_aos <- aos_data[year == max(aos_data$year)]
  }

  # AOS column name
  aos_col <- intersect(names(yr_aos), c("aos", "adjustments", "count"))
  if (length(aos_col) > 0) {
    data.table::setnames(yr_aos, aos_col[1], "aos", skip_absent = TRUE)
  } else {
    yr_aos[, aos := 0]
  }

  # Distribute AOS by type (only I and V contribute)
  # Assume 60% from I (nonimmigrants), 40% from V (overstayers)
  # N (never-authorized) cannot directly adjust
  ages <- 0:99
  sexes <- c("male", "female")
  types <- c("N", "I", "V")

  result <- data.table::CJ(year = year, age = ages, sex = sexes, type = types)

  # Merge total AOS
  result <- merge(result, yr_aos[, .(age, sex, aos)],
                  by = c("age", "sex"), all.x = TRUE)
  result[is.na(aos), aos := 0]

  # ===========================================================================
  # HARDCODED: AOS type distribution
  # ===========================================================================
  # Source: TR2025 LPR Immigration subprocess - AOS comes from nonimmigrants
  # and visa overstayers who regularize their status
  # Approximation: 60% from nonimmigrants (I), 40% from overstayers (V)
  # ===========================================================================
  result[type == "N", aos := 0]           # Never-authorized cannot directly adjust
  result[type == "I", aos := aos * 0.60]  # HARDCODED: 60% from nonimmigrants
  result[type == "V", aos := aos * 0.40]  # HARDCODED: 40% from overstayers

  result[, .(year, age, sex, type, aos)]
}

#' Age population by one year
#'
#' @keywords internal
age_population <- function(population) {
  dt <- data.table::copy(population)

  # Age everyone by 1 year
  dt[, age := age + 1]

  # Cap at age 99 (100+ grouped)
  dt[age > 99, age := 99]

  # Aggregate any duplicates from age capping
  # Check if year column exists
  if ("year" %in% names(dt)) {
    dt <- dt[, .(population = sum(population)), by = .(year, age, sex, type)]
  } else {
    dt <- dt[, .(population = sum(population)), by = .(age, sex, type)]
  }

  dt
}

#' Calculate O deaths
#'
#' @description
#' Per TR2025: "Deaths for the temporary or unlawfully present immigrant population
#' use the same death probabilities as the total population: OD = qx × OP"
#'
#' @keywords internal
calculate_o_deaths <- function(population, mortality_qx) {
  # Merge population with mortality rates
  merged <- merge(
    population[, .(year, age, sex, type, population)],
    mortality_qx[, .(age, sex, qx)],
    by = c("age", "sex"),
    all.x = TRUE
  )

  # Fill missing qx with small value
  merged[is.na(qx), qx := 0.001]

  # Calculate deaths
  merged[, deaths := population * qx]

  merged[, .(year, age, sex, type, deaths)]
}

#' Calculate implied emigration from stock equation
#'
#' @description
#' Solves for OE from the stock equation:
#' OE = aged_pop + OI - AOS - OD - end_pop
#'
#' @keywords internal
calculate_implied_emigration <- function(aged_pop, immigration, aos, deaths, end_pop) {
  # Merge all components
  merged <- merge(
    aged_pop[, .(age, sex, type, aged_pop = population)],
    immigration[, .(age, sex, type, immigration = flow)],
    by = c("age", "sex", "type"),
    all = TRUE
  )

  merged <- merge(merged, aos[, .(age, sex, type, aos)],
                  by = c("age", "sex", "type"), all.x = TRUE)
  merged <- merge(merged, deaths[, .(age, sex, type, deaths)],
                  by = c("age", "sex", "type"), all.x = TRUE)
  merged <- merge(merged, end_pop[, .(age, sex, type, end_pop = population)],
                  by = c("age", "sex", "type"), all.x = TRUE)

  # Fill NAs with 0
  merged[is.na(aged_pop), aged_pop := 0]
  merged[is.na(immigration), immigration := 0]
  merged[is.na(aos), aos := 0]
  merged[is.na(deaths), deaths := 0]
  merged[is.na(end_pop), end_pop := 0]

  # Solve for emigration (ensure non-negative)
  merged[, emigration := pmax(0, aged_pop + immigration - aos - deaths - end_pop)]

  merged[, .(age, sex, type, emigration)]
}

#' Combine stock build-up results
#'
#' @keywords internal
combine_stock_results <- function(results) {
  # Extract each component
  populations <- list()
  immigrations <- list()
  emigrations <- list()
  deaths_list <- list()

  for (yr_name in names(results)) {
    yr_data <- results[[yr_name]]
    yr <- yr_data$year

    pop <- yr_data$population
    pop[, year := yr]
    populations[[yr_name]] <- pop

    imm <- yr_data$immigration
    imm[, year := yr]
    immigrations[[yr_name]] <- imm

    emig <- yr_data$emigration
    emig[, year := yr]
    emigrations[[yr_name]] <- emig

    dth <- yr_data$deaths
    dth[, year := yr]
    deaths_list[[yr_name]] <- dth
  }

  list(
    population = data.table::rbindlist(populations),
    immigration = data.table::rbindlist(immigrations),
    emigration = data.table::rbindlist(emigrations),
    deaths = data.table::rbindlist(deaths_list)
  )
}

# =============================================================================
# DEPARTURE RATE CALCULATION
# =============================================================================

#' Calculate base departure rates from 2008-2010 stock build-up
#'
#' @description
#' Calculates departure rates by dividing OE by OP for each age, sex, and type.
#' Per TR2025: "rates are calculated by dividing OE by OP for each age, sex, and type"
#'
#' @param stock_buildup Output from build_o_stock_for_rates()
#' @param smooth Logical: whether to smooth rates (default: TRUE)
#'
#' @return data.table with departure rates by age, sex, type
#'
#' @export
calculate_base_departure_rates <- function(stock_buildup, smooth = TRUE) {
  checkmate::assert_list(stock_buildup)

  cli::cli_h3("Calculating base departure rates")

  # Get emigration and population
  emigration <- stock_buildup$emigration
  population <- stock_buildup$population

  # Calculate average over the period
  avg_emigration <- emigration[, .(avg_emigration = mean(emigration)),
                               by = .(age, sex, type)]
  avg_population <- population[, .(avg_population = mean(population)),
                               by = .(age, sex, type)]

  # Merge and calculate rates
  rates <- merge(avg_emigration, avg_population, by = c("age", "sex", "type"))

  # Calculate rate (avoid division by zero)
  rates[, base_rate := data.table::fifelse(
    avg_population > 0,
    avg_emigration / avg_population,
    0
  )]

  # Cap rates at reasonable maximum (annual departure rate shouldn't exceed 50%)
  rates[base_rate > 0.50, base_rate := 0.50]

  if (smooth) {
    rates <- smooth_departure_rates(rates)
  }

  cli::cli_alert_success("Calculated departure rates for {nrow(rates)} age-sex-type combinations")

  # Report summary by type
  type_summary <- rates[, .(
    mean_rate = mean(base_rate),
    median_rate = median(base_rate),
    max_rate = max(base_rate)
  ), by = type]

  cli::cli_alert_info("Rate summary by type:")
  for (i in seq_len(nrow(type_summary))) {
    cli::cli_bullets(c(
      " " = "{type_summary$type[i]}: mean={round(type_summary$mean_rate[i]*100, 1)}%, median={round(type_summary$median_rate[i]*100, 1)}%, max={round(type_summary$max_rate[i]*100, 1)}%"
    ))
  }

  rates[, .(age, sex, type, base_rate)]
}

#' Smooth departure rates
#'
#' @description
#' Applies smoothing to departure rates to remove noise from small cell sizes.
#' Uses moving average within age groups.
#'
#' @keywords internal
smooth_departure_rates <- function(rates) {
  dt <- data.table::copy(rates)

  # Smooth within each sex-type combination
  for (sx in c("male", "female")) {
    for (tp in c("N", "I", "V")) {
      idx <- dt$sex == sx & dt$type == tp
      if (sum(idx) > 0) {
        # Apply 5-year moving average
        dt[idx, smoothed_rate := stats::filter(base_rate, rep(1/5, 5), sides = 2)]
        # Fill NAs at edges with original values
        dt[idx & is.na(smoothed_rate), smoothed_rate := base_rate]
      }
    }
  }

  # Replace base_rate with smoothed
  dt[!is.na(smoothed_rate), base_rate := smoothed_rate]
  dt[, smoothed_rate := NULL]

  dt
}

#' Adjust rates for recession effects
#'
#' @description
#' The 2008-2010 period was during the Great Recession, which affected
#' emigration patterns. This function adjusts rates to account for
#' atypical conditions.
#'
#' Per TR2025: "After smoothing and adjusting for the effects of the recent recession"
#'
#' @param rates Base departure rates
#' @param recession_factor Adjustment factor (default: 0.85 = 15% reduction during recession)
#'
#' @return Adjusted departure rates
#'
#' @export
adjust_rates_for_recession <- function(rates, recession_factor = 0.85) {
  # ===========================================================================
  # HARDCODED: Recession adjustment factor
  # ===========================================================================
  # The 2008-2010 period had elevated return migration due to economic conditions.
  # This factor adjusts rates to estimate "normal" departure rates.
  # Source: Professional judgment based on migration literature
  # A factor of 0.85 means recession increased departures by ~18% (1/0.85 = 1.18)
  # ===========================================================================

  cli::cli_alert_info("Adjusting rates for recession effects (factor: {recession_factor})")

  dt <- data.table::copy(rates)

  # Adjust all rates
  dt[, adjusted_rate := base_rate * recession_factor]

  # Working-age adults (18-64) were more affected by recession
  # Apply additional adjustment for these ages
  dt[age >= 18 & age <= 64, adjusted_rate := adjusted_rate * 0.95]

  dt[, base_rate := adjusted_rate]
  dt[, adjusted_rate := NULL]

  dt
}

# =============================================================================
# TYPE-SPECIFIC RATE ADJUSTMENTS
# =============================================================================

#' Apply type-specific rate adjustments
#'
#' @description
#' Applies adjustments per TR2025:
#' - Never-authorized recent arrivals: 2× rate
#' - Nonimmigrants: initial rates (2015) → ultimate (2025)
#' - DACA eligible: lower rates
#'
#' @param base_rates Base departure rates by age, sex, type
#' @param year Projection year
#' @param config Configuration with adjustment parameters
#'
#' @return data.table with adjusted rates for the year
#'
#' @export
apply_type_adjustments <- function(base_rates, year, config = NULL) {
  dt <- data.table::copy(base_rates)

  # Get configuration parameters
  if (is.null(config)) {
    config <- get_default_rate_config()
  }

  # -------------------------------------------------------------------------
  # Policy period determination
  # -------------------------------------------------------------------------
  # Pre-2015: Before Executive Actions (higher enforcement)
  # 2015+: After Executive Actions (decreased deportation of non-felons)
  policy_period <- ifelse(year < 2015, "pre_2015", "post_2015")

  # -------------------------------------------------------------------------
  # Type-specific adjustments
  # -------------------------------------------------------------------------

  # NEVER-AUTHORIZED (N)
  # - Recent arrivals (proxy: ages 18-35) get 2× rate
  # - Pre-2015 rates differ from post-2015
  if (policy_period == "pre_2015") {
    # Higher rates before Executive Actions
    dt[type == "N", rate := base_rate * config$n_pre_2015_multiplier]
  } else {
    # Lower rates after Executive Actions
    dt[type == "N", rate := base_rate * config$n_post_2015_multiplier]
  }

  # NONIMMIGRANT (I)
  # - Rates transition from initial (2015) to ultimate (2025)
  if (year <= 2015) {
    ni_multiplier <- config$ni_initial_multiplier
  } else if (year >= 2025) {
    ni_multiplier <- config$ni_ultimate_multiplier
  } else {
    # Linear interpolation between 2015 and 2025
    progress <- (year - 2015) / (2025 - 2015)
    ni_multiplier <- config$ni_initial_multiplier +
      progress * (config$ni_ultimate_multiplier - config$ni_initial_multiplier)
  }
  dt[type == "I", rate := base_rate * ni_multiplier]

  # VISA-OVERSTAYER (V)
  if (policy_period == "pre_2015") {
    dt[type == "V", rate := base_rate * config$v_pre_2015_multiplier]
  } else {
    dt[type == "V", rate := base_rate * config$v_post_2015_multiplier]
  }

  # Ensure rates are in valid range [0, 1]
  dt[rate < 0, rate := 0]
  dt[rate > 1, rate := 1]

  dt[, year := year]

  dt[, .(year, age, sex, type, rate)]
}

#' Get default rate configuration
#'
#' @description
#' Returns default configuration for departure rate adjustments.
#'
#' **HARDCODED VALUES**: These are approximations based on TR2025 description
#' of methodology. Actual SSA internal values are not public.
#'
#' @section Sources:
#' - TR2025 Section 1.5.c describes the methodology
#' - Pre/post 2015 distinction relates to Executive Actions
#' - Recent arrival 2× rate is explicitly stated in TR2025
#'
#' @param config Optional list to override defaults
#'
#' @return List with rate adjustment parameters
#'
#' @export
get_default_rate_config <- function(config = NULL) {
  # Check for user overrides

  if (!is.null(config) && !is.null(config$departure_rates)) {
    cli::cli_alert_info("Using user-provided departure rate configuration")
    return(config$departure_rates)
  }

  # ===========================================================================
  # HARDCODED DEFAULT VALUES
  # ===========================================================================
  # Source: TR2025 Section 1.5.c methodology description
  # These are approximations - actual SSA internal values not published
  # ===========================================================================

  cli::cli_alert_info("Using default departure rate configuration (HARDCODED)")

  list(
    # Never-authorized multipliers
    n_pre_2015_multiplier = 1.20,     # HARDCODED: Higher enforcement pre-2015
    n_post_2015_multiplier = 0.80,    # HARDCODED: Lower after Executive Actions
    n_recent_arrival_multiplier = 2.0, # TR2025: "recent arrivals exposed to 2× rates"

    # Nonimmigrant multipliers (transition from initial to ultimate)
    ni_initial_multiplier = 0.70,     # HARDCODED: Lower in 2015
    ni_ultimate_multiplier = 1.00,    # HARDCODED: Ultimate by 2025

    # Visa-overstayer multipliers
    v_pre_2015_multiplier = 1.10,     # HARDCODED: Higher enforcement pre-2015
    v_post_2015_multiplier = 0.85,    # HARDCODED: Lower after Executive Actions

    # DACA adjustments
    daca_rate_reduction = 0.50,       # HARDCODED: 50% lower rates for DACA eligible

    # Recent arrival definition (years since arrival)
    recent_arrival_years = 5          # HARDCODED: Recent = arrived within 5 years
  )
}

#' Apply DACA adjustment to departure rates
#'
#' @description
#' Per TR2025: "For the potential DACA population, and for those that are already
#' in the DACA population, the exit rates are lower than for those not eligible
#' for DACA."
#'
#' @param rates Adjusted departure rates
#' @param daca_eligible DACA-eligible population by age, sex
#' @param config Configuration with DACA parameters
#'
#' @return Rates with DACA adjustment applied
#'
#' @export
apply_daca_rate_adjustment <- function(rates, daca_eligible = NULL, config = NULL) {
  if (is.null(config)) {
    config <- get_default_rate_config()
  }

  dt <- data.table::copy(rates)

  if (is.null(daca_eligible)) {
    # Apply DACA adjustment to typical DACA-eligible ages (15-40)
    # Only applies to never-authorized (N) and visa-overstayers (V)
    daca_reduction <- config$daca_rate_reduction

    # DACA eligibility: ages roughly 15-40 (born after June 15, 1981)
    # As of 2024, this means ages up to about 43
    dt[type %in% c("N", "V") & age >= 15 & age <= 43,
       rate := rate * daca_reduction]
  } else {
    # Use provided DACA-eligible population
    # Merge and apply reduction where eligible
    dt <- merge(dt, daca_eligible[, .(age, sex, daca_pct)],
                by = c("age", "sex"), all.x = TRUE)
    dt[is.na(daca_pct), daca_pct := 0]

    # Weighted average rate: (1 - daca_pct) * rate + daca_pct * rate * reduction
    dt[type %in% c("N", "V"),
       rate := rate * (1 - daca_pct + daca_pct * config$daca_rate_reduction)]

    dt[, daca_pct := NULL]
  }

  dt
}

# =============================================================================
# COHORT-BASED RECENT ARRIVAL TRACKING
# =============================================================================
# Per TR2025: "recent arrivals are exposed to twice the rates as the residual
# never authorized stock." This section implements proper cohort tracking
# rather than age-based approximation.
# =============================================================================

#' Initialize O population with arrival cohort tracking
#'
#' @description
#' Converts O population stock to include arrival cohort tracking.
#' Each person is assigned to either "recent" or "non_recent" based on
#' years since arrival.
#'
#' @param o_population O population without cohort tracking
#' @param current_year Current year for cohort calculation
#' @param recent_threshold Years to be considered "recent" (default: 5)
#' @param historical_arrival_dist Distribution of arrival years for existing stock
#'
#' @return data.table with arrival_status column ("recent" or "non_recent")
#'
#' @details
#' For initial population (historical stock), we estimate the proportion that
#' are recent arrivals based on historical immigration patterns. New arrivals
#' each year enter as "recent" and transition to "non_recent" after the threshold.
#'
#' @export
initialize_cohort_tracking <- function(o_population,
                                        current_year,
                                        recent_threshold = 5,
                                        historical_arrival_dist = NULL) {
  dt <- data.table::copy(o_population)

  # Check if already has cohort tracking

  if ("arrival_status" %in% names(dt)) {
    return(dt)
  }

  cli::cli_alert_info("Initializing cohort tracking for O population")

  # For initial stock, estimate recent vs non-recent based on historical patterns
  # Per TR2025, we need to split the never-authorized (N) population
  if (is.null(historical_arrival_dist)) {
    # Estimate proportion recent based on recent immigration relative to stock
    # This is derived from comparing recent O immigration to total O stock
    historical_arrival_dist <- estimate_historical_recent_proportion(
      current_year, recent_threshold
    )
  }

  # Expand population to include arrival_status dimension
  # Only type N needs the recent/non-recent split per TR2025
  result_list <- list()

  # Types I and V don't get the 2× rate adjustment, so no split needed
  # But we still add arrival_status for consistency
  for (tp in unique(dt$type)) {
    tp_data <- dt[type == tp]

    if (tp == "N") {
      # Split never-authorized into recent and non-recent
      recent_pct <- historical_arrival_dist$recent_pct

      # Recent arrivals
      recent_pop <- data.table::copy(tp_data)
      recent_pop[, arrival_status := "recent"]
      recent_pop[, population := population * recent_pct]

      # Non-recent arrivals
      non_recent_pop <- data.table::copy(tp_data)
      non_recent_pop[, arrival_status := "non_recent"]
      non_recent_pop[, population := population * (1 - recent_pct)]

      result_list[[paste0(tp, "_recent")]] <- recent_pop
      result_list[[paste0(tp, "_non_recent")]] <- non_recent_pop
    } else {
      # Types I and V - mark all as non_recent (they don't get 2× rate)
      tp_data[, arrival_status := "non_recent"]
      result_list[[tp]] <- tp_data
    }
  }

  result <- data.table::rbindlist(result_list)
  data.table::setorder(result, type, arrival_status, sex, age)

  cli::cli_alert_success(
    "Initialized cohort tracking: {nrow(result)} rows (recent N: {sum(result$arrival_status == 'recent')} rows)"
  )

  result
}

#' Estimate historical recent proportion
#'
#' @description
#' Estimates what proportion of the current O stock are "recent" arrivals
#' based on comparing recent immigration to total stock.
#'
#' @param current_year Current year
#' @param recent_threshold Years to be considered recent
#'
#' @return List with recent_pct
#'
#' @keywords internal
estimate_historical_recent_proportion <- function(current_year, recent_threshold = 5) {
  # ===========================================================================
  # Estimation based on TR2025 methodology
  # ===========================================================================
  # Recent arrivals = sum of O immigration over last `recent_threshold` years
  # Total stock = current O population
  #
  # Using TR2025 assumptions:
  # - O immigration ~1.35M - 2.7M per year recently
  # - O stock ~13M total
  # - Recent (5 years) ~ 5 × 2M = 10M of flows, but with emigration/deaths
  #   actual recent stock is lower
  #
  # Rough estimate: ~25-35% of never-authorized are recent arrivals
  # ===========================================================================

  # This will be refined when we have actual historical data
  # For now, use a reasonable estimate based on flow/stock ratios
  recent_pct <- 0.30  # ~30% of N population are recent arrivals

  cli::cli_alert_info(
    "Estimated recent arrival proportion: {round(recent_pct * 100)}% (based on flow/stock ratio)"
  )

  list(recent_pct = recent_pct)
}

#' Add new arrivals to population with cohort tracking
#'
#' @description
#' Adds new O immigration to the population stock, marking them as "recent".
#' Per TR2025, new arrivals enter the "recent" category and will transition
#' to "non_recent" after the threshold period.
#'
#' @param o_population Current O population with arrival_status
#' @param o_immigration New O immigration for the year
#' @param year Immigration year
#'
#' @return Updated population with new arrivals added as "recent"
#'
#' @export
add_new_arrivals_with_cohort <- function(o_population, o_immigration, year) {
  # Ensure population has cohort tracking
  if (!"arrival_status" %in% names(o_population)) {
    cli::cli_abort("Population must have arrival_status column. Use initialize_cohort_tracking() first.")
  }

  # Get immigration data
  imm <- data.table::copy(o_immigration)

  # Standardize column names
  if ("o_immigration" %in% names(imm)) {
    data.table::setnames(imm, "o_immigration", "population")
  } else if ("flow" %in% names(imm)) {
    data.table::setnames(imm, "flow", "population")
  } else if ("immigration" %in% names(imm)) {
    data.table::setnames(imm, "immigration", "population")
  }

  # Mark new arrivals
  # Type N arrivals enter as "recent" (subject to 2× rate)
  # Types I and V enter as "non_recent" (don't get 2× rate per TR2025)
  imm[type == "N", arrival_status := "recent"]
  imm[type != "N", arrival_status := "non_recent"]

  # Add year if not present
  if (!"year" %in% names(imm)) {
    imm[, year := year]
  }

  # Combine with existing population
  # Need to aggregate where same age/sex/type/arrival_status
  combined <- data.table::rbindlist(
    list(o_population, imm[, names(o_population), with = FALSE]),
    fill = TRUE
  )

  # Aggregate
  if ("year" %in% names(combined)) {
    result <- combined[, .(population = sum(population, na.rm = TRUE)),
                       by = .(year, age, sex, type, arrival_status)]
  } else {
    result <- combined[, .(population = sum(population, na.rm = TRUE)),
                       by = .(age, sex, type, arrival_status)]
  }

  result
}

#' Transition recent arrivals to non-recent based on threshold
#'
#' @description
#' Each year, a portion of "recent" arrivals transition to "non_recent"
#' as they exceed the threshold years since arrival.
#'
#' Per TR2025, this transition happens as cohorts age past the threshold.
#' With a 5-year threshold, 1/5 of recent arrivals transition each year
#' (assuming uniform distribution of arrival years within recent period).
#'
#' @param o_population O population with arrival_status
#' @param recent_threshold Years to be considered recent (default: 5)
#'
#' @return Updated population with transitions applied
#'
#' @export
transition_recent_to_non_recent <- function(o_population, recent_threshold = 5) {
  dt <- data.table::copy(o_population)

  # Calculate transition rate: 1/threshold of recent become non-recent each year
  # This assumes uniform distribution of arrival years within the recent period
  transition_rate <- 1 / recent_threshold

  # Get recent N population
  recent_n <- dt[type == "N" & arrival_status == "recent"]

  if (nrow(recent_n) == 0) {
    return(dt)
  }

  # Calculate how many transition
  transitioning <- data.table::copy(recent_n)
  transitioning[, population := population * transition_rate]
  transitioning[, arrival_status := "non_recent"]

  # Reduce recent population
  dt[type == "N" & arrival_status == "recent",
     population := population * (1 - transition_rate)]

  # Add transitioning to non-recent
  result <- data.table::rbindlist(list(dt, transitioning), fill = TRUE)

  # Aggregate (combine non_recent that was already there with newly transitioned)
  if ("year" %in% names(result)) {
    result <- result[, .(population = sum(population, na.rm = TRUE)),
                     by = .(year, age, sex, type, arrival_status)]
  } else {
    result <- result[, .(population = sum(population, na.rm = TRUE)),
                     by = .(age, sex, type, arrival_status)]
  }

  result
}

#' Apply departure rates with cohort-based recent arrival tracking
#'
#' @description
#' Applies departure rates to O population, with 2× rate for recent
#' never-authorized arrivals per TR2025 methodology.
#'
#' This replaces the age-based approximation with actual cohort tracking.
#'
#' @param o_population O population with arrival_status column
#' @param base_rates Base departure rates by age, sex, type
#' @param year Projection year
#' @param config Configuration with rate multipliers
#'
#' @return data.table with emigration by age, sex, type, arrival_status
#'
#' @export
calculate_emigration_with_cohorts <- function(o_population,
                                               base_rates,
                                               year,
                                               config = NULL) {
  if (is.null(config)) {
    config <- get_default_rate_config()
  }

  # First apply standard type adjustments (pre/post 2015, NI transition, etc.)
  rates <- apply_type_adjustments(base_rates, year, config)

  # Apply DACA adjustment
  rates <- apply_daca_rate_adjustment(rates, config = config)

  # Now merge with population (which has arrival_status)
  pop <- data.table::copy(o_population)

  # Expand rates to include arrival_status dimension
  rates_recent <- data.table::copy(rates)
  rates_recent[, arrival_status := "recent"]

  rates_non_recent <- data.table::copy(rates)
  rates_non_recent[, arrival_status := "non_recent"]

  rates_expanded <- data.table::rbindlist(list(rates_recent, rates_non_recent))

  # Apply 2× rate multiplier for recent never-authorized
  # Per TR2025: "recent arrivals are exposed to twice the rates"
  rates_expanded[type == "N" & arrival_status == "recent",
                 rate := rate * config$n_recent_arrival_multiplier]

  # Merge population with rates
  merged <- merge(
    pop[, .(age, sex, type, arrival_status, population)],
    rates_expanded[, .(age, sex, type, arrival_status, rate)],
    by = c("age", "sex", "type", "arrival_status"),
    all.x = TRUE
  )

  # Fill missing rates
  merged[is.na(rate), rate := 0.05]

  # Calculate emigration
  merged[, emigration := population * rate]
  merged[, year := year]

  merged[, .(year, age, sex, type, arrival_status, emigration, rate)]
}

#' Run O emigration projection with cohort tracking
#'
#' @description
#' Projects O emigration using proper cohort-based recent arrival tracking
#' per TR2025 methodology. This tracks actual years since arrival rather
#' than using age-based approximations.
#'
#' @param starting_population O population at start of projection
#' @param base_rates Base departure rates
#' @param projection_years Years to project
#' @param o_immigration Projected O immigration by year
#' @param aos Adjustments of status (optional)
#' @param mortality_qx Death probabilities
#' @param config Configuration
#' @param recent_threshold Years to be considered "recent" (default: 5)
#'
#' @return List with emigration, population, and cohort statistics
#'
#' @export
run_o_emigration_with_cohorts <- function(starting_population,
                                           base_rates,
                                           projection_years,
                                           o_immigration,
                                           aos = NULL,
                                           mortality_qx,
                                           config = NULL,
                                           recent_threshold = 5) {
  cli::cli_h2("Running O Emigration Projection (Cohort Tracking)")

  if (is.null(config)) {
    config <- get_default_rate_config()
  }

  # Initialize cohort tracking on starting population
  current_pop <- initialize_cohort_tracking(
    starting_population,
    current_year = min(projection_years) - 1,
    recent_threshold = recent_threshold
  )

  emigration_results <- list()
  population_results <- list()
  cohort_stats <- list()

  for (yr in projection_years) {
    cli::cli_alert("Projecting year {yr}...")

    # Step 1: Calculate emigration with cohort-based rates
    yr_emigration <- calculate_emigration_with_cohorts(
      current_pop, base_rates, yr, config
    )

    # Step 2: Get immigration for this year
    yr_imm <- o_immigration[year == yr]

    # Step 3: Get AOS for this year
    yr_aos <- if (!is.null(aos) && nrow(aos[year == yr]) > 0) {
      aos[year == yr]
    } else {
      NULL
    }

    # Step 4: Get mortality
    yr_qx <- mortality_qx[year == yr]
    if (nrow(yr_qx) == 0) {
      yr_qx <- mortality_qx[year == max(mortality_qx$year)]
    }

    # Step 5: Update population
    current_pop <- update_o_population_with_cohorts(
      current_pop, yr_imm, yr_emigration, yr_aos, yr_qx, yr, recent_threshold
    )

    # Step 6: Transition recent → non_recent
    current_pop <- transition_recent_to_non_recent(current_pop, recent_threshold)

    # Store results
    emigration_results[[as.character(yr)]] <- yr_emigration
    pop_with_year <- data.table::copy(current_pop)
    pop_with_year[, year := yr]
    population_results[[as.character(yr)]] <- pop_with_year

    # Track cohort statistics
    cohort_stats[[as.character(yr)]] <- current_pop[
      type == "N",
      .(
        recent_pop = sum(population[arrival_status == "recent"]),
        non_recent_pop = sum(population[arrival_status == "non_recent"]),
        recent_pct = sum(population[arrival_status == "recent"]) /
          sum(population)
      )
    ][, year := yr]
  }

  cli::cli_alert_success("Projected O emigration for {length(projection_years)} years with cohort tracking")

  # Report cohort statistics
  cohort_summary <- data.table::rbindlist(cohort_stats)
  cli::cli_alert_info("Recent arrival proportion (type N) over projection:")
  for (yr in projection_years[seq(1, length(projection_years), by = max(1, length(projection_years) %/% 5))]) {
    pct <- cohort_summary[year == yr, recent_pct]
    if (length(pct) > 0) {
      cli::cli_bullets(c(" " = "{yr}: {round(pct * 100, 1)}% recent"))
    }
  }

  list(
    emigration = data.table::rbindlist(emigration_results),
    population = data.table::rbindlist(population_results),
    cohort_stats = cohort_summary
  )
}

#' Update O population stock with cohort tracking
#'
#' @keywords internal
update_o_population_with_cohorts <- function(current_pop, immigration, emigration,
                                              aos, mortality_qx, year, recent_threshold) {
  # Age the population (but keep arrival_status)
  aged_pop <- age_population_with_cohorts(current_pop)

  # Add new arrivals (marked as recent for type N)
  if (!is.null(immigration) && nrow(immigration) > 0) {
    aged_pop <- add_new_arrivals_with_cohort(aged_pop, immigration, year)
  }

  # Subtract emigration
  emig_agg <- emigration[, .(emigration = sum(emigration)),
                         by = .(age, sex, type, arrival_status)]

  aged_pop <- merge(aged_pop, emig_agg,
                    by = c("age", "sex", "type", "arrival_status"),
                    all.x = TRUE)
  aged_pop[is.na(emigration), emigration := 0]
  aged_pop[, population := pmax(0, population - emigration)]
  aged_pop[, emigration := NULL]

  # Subtract AOS (only from I and V)
  if (!is.null(aos) && nrow(aos) > 0) {
    aos_agg <- aos[, .(aos = sum(aos)), by = .(age, sex, type)]
    aged_pop <- merge(aged_pop, aos_agg,
                      by = c("age", "sex", "type"),
                      all.x = TRUE)
    aged_pop[is.na(aos), aos := 0]
    aged_pop[, population := pmax(0, population - aos)]
    aged_pop[, aos := NULL]
  }

  # Apply mortality
  aged_pop <- merge(aged_pop, mortality_qx[, .(age, sex, qx)],
                    by = c("age", "sex"), all.x = TRUE)
  aged_pop[is.na(qx), qx := 0.01]
  aged_pop[, deaths := population * qx]
  aged_pop[, population := pmax(0, population - deaths)]
  aged_pop[, c("qx", "deaths") := NULL]

  aged_pop
}

#' Age population preserving cohort tracking
#'
#' @keywords internal
age_population_with_cohorts <- function(population) {
  dt <- data.table::copy(population)

  # Age everyone by 1 year
  dt[, age := age + 1]

  # Cap at age 99
  dt[age > 99, age := 99]

  # Aggregate preserving arrival_status
  if ("year" %in% names(dt)) {
    dt <- dt[, .(population = sum(population)),
             by = .(year, age, sex, type, arrival_status)]
  } else {
    dt <- dt[, .(population = sum(population)),
             by = .(age, sex, type, arrival_status)]
  }

  dt
}

# =============================================================================
# NOTE: Legacy age-based approximation removed
# =============================================================================
# The age-based approximation method (split_never_authorized_rates) has been
# removed in favor of the cohort-tracking approach (run_o_emigration_with_cohorts)
# which is more faithful to TR2025 methodology.
#
# Per TR2025: "recent arrivals are exposed to twice the rates as the residual
# never authorized stock." The cohort-tracking approach tracks actual years
# since arrival rather than using age as a proxy.
# =============================================================================

# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

#' Get departure rate sources documentation
#'
#' @description
#' Returns documentation of sources for departure rate estimates.
#'
#' @return List with source citations and methodology notes
#'
#' @export
get_departure_rate_sources <- function() {
  list(
    tr2025_reference = paste0(
      "TR2025 Section 1.5.c: 'These estimates are based on 2014 TR build-up of stocks ",
      "from 2008 through 2010... After smoothing and adjusting for the effects of the ",
      "recent recession, these rates are used to calculate OE in projected years.'"
    ),
    methodology_notes = list(
      stock_buildup = "Rates derived from 2008-2010 stock build-up using cohort-component method",
      recession_adjustment = paste0(
        "2008-2010 was during the Great Recession, requiring adjustment for atypical ",
        "emigration patterns (elevated return migration)"
      ),
      policy_period = paste0(
        "Pre-2015 vs post-2015 distinction relates to Executive Actions that ",
        "decreased deportation of non-felons"
      ),
      recent_arrivals = paste0(
        "TR2025 explicitly states: 'recent arrivals are exposed to twice the rates ",
        "as the residual never authorized stock'"
      ),
      daca_adjustment = paste0(
        "TR2025: 'For the potential DACA population, and for those that are already ",
        "in the DACA population, the exit rates are lower'"
      )
    ),
    hardcoded_values = list(
      recession_factor = "0.85 - Professional judgment based on migration literature",
      n_pre_2015_multiplier = "1.20 - Higher enforcement before Executive Actions",
      n_post_2015_multiplier = "0.80 - Lower after Executive Actions",
      ni_transition = "0.70 (2015) to 1.00 (2025) - Gradual increase per TR2025",
      daca_reduction = "0.50 - Lower rates for DACA-eligible (TR2025 methodology)"
    ),
    note = paste0(
      "The actual SSA internal departure rates are not publicly available. ",
      "Values here are approximations based on the methodology described in TR2025. ",
      "Users can override these values via configuration."
    )
  )
}

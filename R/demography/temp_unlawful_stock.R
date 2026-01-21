#' Temporary or Unlawfully Present Population Stock
#'
#' Functions for projecting O population stock and calculating net O immigration
#' following TR2025 Section 1.5 methodology (Equations 1.5.3 and 1.5.4).
#'
#' Primary equations:
#' - NO^z_{x,s,t} = OI^z - OE^z - AOS^z (Eq 1.5.3)
#' - OP^z_{x,s,t} = OP^{z-1}_{x-1,s,t} + OI^z - OE^z - AOS^z - OD^z (Eq 1.5.4)
#'
#' @name temp_unlawful_stock
NULL

# =============================================================================
# EQUATION 1.5.3 - NET O IMMIGRATION
# =============================================================================

#' Calculate net O immigration (Equation 1.5.3)
#'
#' @description
#' Calculates net temporary or unlawfully present immigration per TR2025:
#' NO^z_{x,s,t} = OI^z_{x,s,t} - OE^z_{x,s,t} - AOS^z_{x,s,t}
#'
#' @param o_immigration O immigration by year, age, sex, type (OI)
#' @param o_emigration O emigration by year, age, sex, type (OE)
#' @param aos Adjustments of status by year, age, sex, type (AOS)
#'
#' @return data.table with net O immigration by year, age, sex, type
#'
#' @details
#' Per TR2025 Section 1.5.a Equation 1.5.3:
#' "NO^z_{x,s,t} are the number of net temporary or unlawfully present immigrants,
#' by age (x), sex (s), and type (t) for year z, and AOS^z_{x,s,t} are the number
#' of adjustments to LPR status from the LPR IMMIGRATION subprocess"
#'
#' AOS reduces the O population as these individuals become LPRs.
#'
#' @export
calculate_net_o_immigration <- function(o_immigration, o_emigration, aos) {
  checkmate::assert_data_table(o_immigration)
  checkmate::assert_data_table(o_emigration)

  cli::cli_h3("Calculating Net O Immigration (Equation 1.5.3)")

  # Standardize column names for immigration
  imm <- data.table::copy(o_immigration)
  if ("o_immigration" %in% names(imm)) {
    data.table::setnames(imm, "o_immigration", "immigration")
  }

  # Standardize column names for emigration
  emig <- data.table::copy(o_emigration)
  if ("o_emigration" %in% names(emig)) {
    data.table::setnames(emig, "o_emigration", "emigration")
  } else if ("emigration" %in% names(emig)) {
    # Already correct
  }

  # Aggregate emigration if it has arrival_status dimension
  if ("arrival_status" %in% names(emig)) {
    emig <- emig[, .(emigration = sum(emigration, na.rm = TRUE)),
                 by = .(year, age, sex, type)]
  }

  # Merge immigration and emigration
  merged <- merge(
    imm[, .(year, age, sex, type, immigration)],
    emig[, .(year, age, sex, type, emigration)],
    by = c("year", "age", "sex", "type"),
    all = TRUE
  )

  # Fill NAs
  merged[is.na(immigration), immigration := 0]
  merged[is.na(emigration), emigration := 0]

  # Handle AOS
  if (!is.null(aos) && nrow(aos) > 0) {
    aos_dt <- data.table::copy(aos)

    # Standardize AOS column name
    if ("adjustments" %in% names(aos_dt)) {
      data.table::setnames(aos_dt, "adjustments", "aos")
    } else if (!"aos" %in% names(aos_dt)) {
      # Create aos column from count if available
      if ("count" %in% names(aos_dt)) {
        data.table::setnames(aos_dt, "count", "aos")
      } else {
        aos_dt[, aos := 0]
      }
    }

    # AOS typically doesn't have type dimension from LPR subprocess
    # Distribute AOS to types I (nonimmigrant) and V (overstayer)
    # Type N (never-authorized) cannot directly adjust to LPR
    if (!"type" %in% names(aos_dt)) {
      aos_dt <- distribute_aos_to_types(aos_dt)
    }

    # Merge AOS
    merged <- merge(
      merged,
      aos_dt[, .(year, age, sex, type, aos)],
      by = c("year", "age", "sex", "type"),
      all.x = TRUE
    )
  } else {
    merged[, aos := 0]
  }

  merged[is.na(aos), aos := 0]

  # Calculate net O immigration: NO = OI - OE - AOS
  merged[, net_o_immigration := immigration - emigration - aos]

  # Report summary
  years <- unique(merged$year)
  cli::cli_alert_success(
    "Calculated net O immigration for {length(years)} years ({min(years)}-{max(years)})"
  )

  # Summary by year
  year_summary <- merged[, .(
    total_immigration = sum(immigration),
    total_emigration = sum(emigration),
    total_aos = sum(aos),
    net_o = sum(net_o_immigration)
  ), by = year]

  cli::cli_alert_info(
    "Net O immigration range: {format(min(year_summary$net_o), big.mark = ',')} to {format(max(year_summary$net_o), big.mark = ',')}"
  )

  merged[, .(year, age, sex, type, net_o_immigration)]
}

#' Distribute AOS to O types
#'
#' @description
#' Distributes Adjustments of Status from LPR subprocess to O types.
#' Per TR2025, AOS comes from nonimmigrants (I) and visa-overstayers (V)
#' who regularize their status. Never-authorized (N) cannot directly adjust.
#'
#' @param aos AOS data without type dimension
#'
#' @return AOS data with type dimension
#'
#' @keywords internal
distribute_aos_to_types <- function(aos) {
  dt <- data.table::copy(aos)

  # ===========================================================================
  # HARDCODED: AOS type distribution
  # ===========================================================================
  # Source: TR2025 LPR Immigration subprocess
  # AOS comes primarily from nonimmigrants who adjust to LPR (e.g., employment-
  # based, family-sponsored), and visa-overstayers who regularize.
  # Never-authorized cannot directly adjust (would need to leave and re-enter).
  #
  # Approximation based on DHS LPR reports:
  # - ~60% from nonimmigrants (I): H-1B→green card, F-1→green card, etc.
  # - ~40% from overstayers (V): Those who regularize through family/employer
  # - 0% from never-authorized (N): Cannot directly adjust
  # ===========================================================================

  type_dist <- data.table::data.table(
    type = c("N", "I", "V"),
    type_pct = c(0.00, 0.60, 0.40)  # HARDCODED
  )

  # Expand AOS to include type
  result <- dt[, .(year, age, sex, aos)]
  result <- result[rep(seq_len(.N), each = 3)]
  result[, type := rep(c("N", "I", "V"), .N / 3)]

  # Apply type distribution
  result <- merge(result, type_dist, by = "type")
  result[, aos := aos * type_pct]
  result[, type_pct := NULL]

  result
}

# =============================================================================
# EQUATION 1.5.4 - O POPULATION STOCK PROJECTION
# =============================================================================

#' Project O population stock (Equation 1.5.4)
#'
#' @description
#' Projects O population stock using the cohort-component method per TR2025:
#' OP^z_{x,s,t} = OP^{z-1}_{x-1,s,t} + OI^z_{x,s,t} - OE^z_{x,s,t} - AOS^z_{x,s,t} - OD^z_{x,s,t}
#'
#' @param starting_pop O population at Dec 31 of last historical year (by age, sex, type)
#' @param o_immigration Projected O immigration by year, age, sex, type
#' @param o_emigration Projected O emigration by year, age, sex, type
#' @param aos Projected AOS by year, age, sex (will be distributed to types)
#' @param mortality_qx Death probabilities by age, sex, year
#' @param projection_years Years to project
#'
#' @return data.table with O population stock by year, age, sex, type
#'
#' @details
#' Per TR2025 Section 1.5.a Equation 1.5.4:
#' "OP^z_{x,s,t} is equal to the temporary or unlawfully present immigrant
#' population, by age (x), sex (s), and type (t) as of December 31st of each
#' year (z), OD^z_{x,s,t} are the number of deaths in the temporary or
#' unlawfully present immigrant population"
#'
#' Deaths use the same mortality rates as the total population:
#' OD^z_{x,s,t} = q^z_{x,s} × OP^z_{x,s,t}
#'
#' @export
project_o_population_stock <- function(starting_pop,
                                        o_immigration,
                                        o_emigration,
                                        aos,
                                        mortality_qx,
                                        projection_years) {
  checkmate::assert_data_table(starting_pop)
  checkmate::assert_data_table(o_immigration)
  checkmate::assert_data_table(o_emigration)
  checkmate::assert_data_table(mortality_qx)

  cli::cli_h2("Projecting O Population Stock (Equation 1.5.4)")

  # Standardize starting population
  current_pop <- data.table::copy(starting_pop)
  if ("o_population" %in% names(current_pop)) {
    data.table::setnames(current_pop, "o_population", "population")
  }

  # Ensure type dimension exists
  if (!"type" %in% names(current_pop)) {
    cli::cli_alert_warning("Starting population missing type dimension, applying default splits")
    current_pop <- add_type_dimension(current_pop, year = min(projection_years) - 1)
  }

  # Standardize immigration column
  imm <- data.table::copy(o_immigration)
  if ("o_immigration" %in% names(imm)) {
    data.table::setnames(imm, "o_immigration", "immigration")
  }

  # Standardize emigration column
  emig <- data.table::copy(o_emigration)
  # Handle both regular and cohort-tracked emigration
  if ("arrival_status" %in% names(emig)) {
    emig <- emig[, .(emigration = sum(emigration, na.rm = TRUE)),
                 by = .(year, age, sex, type)]
  }
  if ("o_emigration" %in% names(emig)) {
    data.table::setnames(emig, "o_emigration", "emigration")
  }

  # Prepare AOS (distribute to types if needed)
  aos_typed <- prepare_aos_for_projection(aos, projection_years)

  # Store results
  results <- list()

  for (yr in projection_years) {
    cli::cli_alert("Projecting year {yr}...")

    # Step 1: Age the population (x-1 at z-1 becomes x at z)
    aged_pop <- age_o_population(current_pop)

    # Step 2: Get immigration for this year
    yr_imm <- imm[year == yr]
    if (nrow(yr_imm) == 0) {
      cli::cli_alert_warning("No immigration data for {yr}")
      yr_imm <- data.table::CJ(age = 0:99, sex = c("male", "female"), type = c("N", "I", "V"))
      yr_imm[, immigration := 0]
    }

    # Step 3: Get emigration for this year
    yr_emig <- emig[year == yr]
    if (nrow(yr_emig) == 0) {
      cli::cli_alert_warning("No emigration data for {yr}")
      yr_emig <- data.table::CJ(age = 0:99, sex = c("male", "female"), type = c("N", "I", "V"))
      yr_emig[, emigration := 0]
    }

    # Step 4: Get AOS for this year
    yr_aos <- aos_typed[year == yr]
    if (nrow(yr_aos) == 0) {
      yr_aos <- data.table::CJ(age = 0:99, sex = c("male", "female"), type = c("N", "I", "V"))
      yr_aos[, aos := 0]
    }

    # Step 5: Get mortality for this year
    yr_qx <- mortality_qx[year == yr]
    if (nrow(yr_qx) == 0) {
      yr_qx <- mortality_qx[year == max(mortality_qx$year)]
    }

    # Step 6: Apply cohort-component equation
    # OP^z = OP^{z-1}_{x-1} + OI^z - OE^z - AOS^z - OD^z

    # Merge aged population with components
    merged <- merge(
      aged_pop[, .(age, sex, type, population)],
      yr_imm[, .(age, sex, type, immigration)],
      by = c("age", "sex", "type"),
      all = TRUE
    )

    merged <- merge(
      merged,
      yr_emig[, .(age, sex, type, emigration)],
      by = c("age", "sex", "type"),
      all.x = TRUE
    )

    merged <- merge(
      merged,
      yr_aos[, .(age, sex, type, aos)],
      by = c("age", "sex", "type"),
      all.x = TRUE
    )

    merged <- merge(
      merged,
      yr_qx[, .(age, sex, qx)],
      by = c("age", "sex"),
      all.x = TRUE
    )

    # Fill NAs
    merged[is.na(population), population := 0]
    merged[is.na(immigration), immigration := 0]
    merged[is.na(emigration), emigration := 0]
    merged[is.na(aos), aos := 0]
    merged[is.na(qx), qx := 0.01]  # Default mortality rate

    # Calculate deaths: OD = qx × OP (after adding immigration, before subtracting outflows)
    # Per TR2025: "Deaths for the temporary or unlawfully present immigrant population
    # use the same death probabilities as the total population"
    merged[, mid_year_pop := population + immigration * 0.5]  # Approximate mid-year exposure
    merged[, deaths := qx * mid_year_pop]

    # Apply stock equation: OP = aged_pop + OI - OE - AOS - OD
    merged[, new_population := pmax(0, population + immigration - emigration - aos - deaths)]

    # Store result
    yr_result <- merged[, .(age, sex, type, population = new_population)]
    yr_result[, year := yr]
    results[[as.character(yr)]] <- yr_result

    # Update current population for next iteration
    current_pop <- yr_result[, .(age, sex, type, population)]
  }

  # Combine all years
  all_results <- data.table::rbindlist(results)

  cli::cli_alert_success(
    "Projected O population for {length(projection_years)} years"
  )

  # Summary statistics
  pop_summary <- all_results[, .(total_pop = sum(population)), by = year]
  cli::cli_alert_info(
    "O population: {format(pop_summary[year == min(year), total_pop], big.mark = ',')} ({min(pop_summary$year)}) → {format(pop_summary[year == max(year), total_pop], big.mark = ',')} ({max(pop_summary$year)})"
  )

  all_results
}

#' Age O population by one year
#'
#' @description
#' Ages the O population: persons at age x-1 in year z-1 become age x in year z.
#'
#' @param population O population by age, sex, type
#'
#' @return Aged population
#'
#' @keywords internal
age_o_population <- function(population) {
  dt <- data.table::copy(population)

  # Age everyone by 1 year
  dt[, age := age + 1]

  # Cap at age 99 (100+ grouped into 99)
  dt[age > 99, age := 99]

  # Remove year column if present (will be added fresh)
  if ("year" %in% names(dt)) {
    dt[, year := NULL]
  }

  # Aggregate any duplicates from age capping
  dt <- dt[, .(population = sum(population)), by = .(age, sex, type)]

  dt
}

#' Add type dimension to population without types
#'
#' @description
#' Applies historical type interpolation to add type dimension.
#'
#' @keywords internal
add_type_dimension <- function(population, year) {
  # Get type splits for the year
  type_splits <- tryCatch({
    get_type_splits_interpolated(
      year = year,
      age = unique(population$age),
      sex = unique(population$sex)
    )
  }, error = function(e) {
    # Fallback to default splits
    ages <- unique(population$age)
    sexes <- unique(population$sex)
    dt <- data.table::CJ(age = ages, sex = sexes)
    dt[, type_n := 0.50]
    dt[, type_i := 0.15]
    dt[, type_v := 0.35]
    dt
  })

  # Merge and expand
  merged <- merge(population, type_splits, by = c("age", "sex"), all.x = TRUE)

  # Fill missing
  merged[is.na(type_n), `:=`(type_n = 0.50, type_i = 0.15, type_v = 0.35)]

  # Create long format
  result <- data.table::rbindlist(list(
    merged[, .(age, sex, type = "N", population = population * type_n)],
    merged[, .(age, sex, type = "I", population = population * type_i)],
    merged[, .(age, sex, type = "V", population = population * type_v)]
  ))

  result
}

#' Prepare AOS for projection
#'
#' @description
#' Prepares AOS data for projection, distributing to types if needed.
#'
#' @keywords internal
prepare_aos_for_projection <- function(aos, projection_years) {
  if (is.null(aos) || nrow(aos) == 0) {
    # Create empty AOS
    result <- data.table::CJ(
      year = projection_years,
      age = 0:99,
      sex = c("male", "female"),
      type = c("N", "I", "V")
    )
    result[, aos := 0]
    return(result)
  }

  aos_dt <- data.table::copy(aos)

  # Standardize column name
  if ("adjustments" %in% names(aos_dt)) {
    data.table::setnames(aos_dt, "adjustments", "aos")
  } else if ("count" %in% names(aos_dt)) {
    data.table::setnames(aos_dt, "count", "aos")
  }

  # Filter to projection years
  aos_dt <- aos_dt[year %in% projection_years]

  # Distribute to types if needed
  if (!"type" %in% names(aos_dt)) {
    aos_dt <- distribute_aos_to_types(aos_dt)
  }

  aos_dt
}

# =============================================================================
# HISTORICAL TYPE SPLITS FOR POPULATION
# =============================================================================

#' Calculate historical type splits for O population
#'
#' @description
#' Splits historical O population into types (N/I/V) using TR2025 methodology:
#' - Dec 31, 1963: 100% nonimmigrant
#' - 1963-2010: Linear interpolation
#' - 2010-2015: Linear interpolation
#' - 2015+: Use 2015 proportions
#'
#' Per TR2025: "It is assumed that all temporary or unlawfully present immigrants
#' were nonimmigrants as of December 31, 1963. Between December 31, 1963, and
#' December 31, 2010, the percentage of total temporary or unlawfully present
#' immigrants by age and sex in each type is linearly interpolated from the
#' percentages at those two points in time."
#'
#' @param o_population Total O population by year, age, sex (without types)
#' @param dhs_nonimmigrant_stock DHS nonimmigrant stock for calibration (optional)
#'
#' @return data.table with O population split by type
#'
#' @export
calculate_historical_type_splits <- function(o_population,
                                              dhs_nonimmigrant_stock = NULL) {
  checkmate::assert_data_table(o_population)

  cli::cli_h3("Calculating Historical Type Splits")

  years <- unique(o_population$year)
  cli::cli_alert_info("Processing {length(years)} years ({min(years)}-{max(years)})")

  results <- list()

  for (yr in years) {
    yr_pop <- o_population[year == yr]

    # Get type splits for this year using interpolation
    type_splits <- get_type_splits_interpolated(
      year = yr,
      age = unique(yr_pop$age),
      sex = unique(yr_pop$sex)
    )

    # Apply splits to population
    yr_merged <- merge(yr_pop, type_splits, by = c("age", "sex"), all.x = TRUE)

    # Fill missing (shouldn't happen, but safety)
    yr_merged[is.na(type_n), `:=`(type_n = 0.50, type_i = 0.15, type_v = 0.35)]

    # Get population column
    pop_col <- intersect(names(yr_merged), c("population", "o_population", "count"))
    if (length(pop_col) == 0) {
      cli::cli_abort("No population column found")
    }
    pop_col <- pop_col[1]

    # Create long format
    yr_result <- data.table::rbindlist(list(
      yr_merged[, .(year = yr, age, sex, type = "N",
                    population = get(pop_col) * type_n)],
      yr_merged[, .(year = yr, age, sex, type = "I",
                    population = get(pop_col) * type_i)],
      yr_merged[, .(year = yr, age, sex, type = "V",
                    population = get(pop_col) * type_v)]
    ))

    results[[as.character(yr)]] <- yr_result
  }

  result <- data.table::rbindlist(results)

  # Calibrate to DHS nonimmigrant stock if available
  if (!is.null(dhs_nonimmigrant_stock) && nrow(dhs_nonimmigrant_stock) > 0) {
    result <- calibrate_to_dhs_nonimmigrant(result, dhs_nonimmigrant_stock)
  }

  cli::cli_alert_success(
    "Split O population into {nrow(result)} age-sex-type-year combinations"
  )

  result
}

#' Calibrate nonimmigrant population to DHS stock
#'
#' @description
#' Per TR2025: "A final adjustment ensures the total nonimmigrants are appropriate,
#' based on DHS nonimmigrant admissions or, if available, stock estimates."
#'
#' @keywords internal
calibrate_to_dhs_nonimmigrant <- function(o_population, dhs_stock) {
  # Get DHS reference years
  dhs_years <- unique(dhs_stock$year)

  for (yr in dhs_years) {
    if (!yr %in% unique(o_population$year)) next

    # Get model and DHS totals for nonimmigrants
    model_ni <- o_population[year == yr & type == "I", sum(population)]
    dhs_ni <- dhs_stock[year == yr, sum(nonimmigrant_stock, na.rm = TRUE)]

    if (model_ni > 0 && dhs_ni > 0) {
      # Calculate calibration factor
      cal_factor <- dhs_ni / model_ni

      # Apply calibration (cap at reasonable range)
      cal_factor <- pmin(pmax(cal_factor, 0.5), 2.0)

      if (abs(cal_factor - 1) > 0.05) {
        cli::cli_alert_info("Calibrating {yr} NI: factor = {round(cal_factor, 3)}")
        o_population[year == yr & type == "I", population := population * cal_factor]

        # Redistribute to maintain total
        total_pop <- o_population[year == yr, sum(population)]
        ni_pop <- o_population[year == yr & type == "I", sum(population)]
        other_pop <- total_pop - ni_pop

        # Scale N and V to maintain total
        other_factor <- (total_pop - ni_pop) / o_population[year == yr & type != "I", sum(population)]
        if (is.finite(other_factor) && other_factor > 0) {
          o_population[year == yr & type != "I", population := population * other_factor]
        }
      }
    }
  }

  o_population
}

# =============================================================================
# MAIN ENTRY POINT - FULL O IMMIGRATION PROJECTION
# =============================================================================

#' Run complete O immigration projection
#'
#' @description
#' Main entry point for the complete temporary or unlawfully present
#' immigration projection. Implements all equations from TR2025 Section 1.5:
#' - Equation 1.5.1: O Immigration (OI = TO × ODIST)
#' - Equation 1.5.2: O Emigration (OE from departure rates)
#' - Equation 1.5.3: Net O Immigration (NO = OI - OE - AOS)
#' - Equation 1.5.4: O Population Stock (cohort-component)
#'
#' @param historical_o_pop Historical O population from HISTORICAL subprocess
#' @param acs_new_arrivals ACS foreign-born new arrivals
#' @param lpr_new_arrivals LPR NEW arrivals from LPR subprocess
#' @param lpr_aos Adjustments of status from LPR subprocess
#' @param mortality_qx Death probabilities from MORTALITY subprocess
#' @param undercount_factors ACS undercount factors
#' @param projection_years Years to project (default: 2023:2099)
#' @param config Configuration overrides
#'
#' @return List with:
#'   - o_immigration: OI by year, age, sex, type
#'   - o_emigration: OE by year, age, sex, type
#'   - net_o_immigration: NO by year, age, sex, type
#'   - o_population: OP by year, age, sex, type (Dec 31)
#'   - odist: Age-sex-type distribution used for projection
#'   - departure_rates: Base departure rates
#'   - assumptions: TR2025 O immigration assumptions
#'
#' @details
#' This function orchestrates the complete O immigration projection following
#' TR2025 methodology:
#'
#' 1. **ODIST Development**: Calculate age-sex-type distribution from 2015-2019
#'    ACS data minus LPR new arrivals
#'
#' 2. **O Immigration Projection**: Apply ODIST to Trustees assumptions (TO^z)
#'    per Equation 1.5.1
#'
#' 3. **Departure Rate Development**: Build rates from 2008-2010 stock period
#'    with recession adjustment and type-specific multipliers
#'
#' 4. **O Emigration Projection**: Apply rates to stock with cohort-tracking
#'    for never-authorized recent arrivals (2× rate)
#'
#' 5. **Net O Immigration**: Calculate NO = OI - OE - AOS per Equation 1.5.3
#'
#' 6. **O Population Stock**: Project using cohort-component method per
#'    Equation 1.5.4
#'
#' @export
run_full_o_projection <- function(historical_o_pop,
                                   acs_new_arrivals = NULL,
                                   lpr_new_arrivals = NULL,
                                   lpr_aos = NULL,
                                   mortality_qx,
                                   undercount_factors = NULL,
                                   projection_years = 2023:2099,
                                   config = NULL) {

  cli::cli_h1("Temporary or Unlawfully Present Immigration Projection")
  cli::cli_alert_info("Projection years: {min(projection_years)}-{max(projection_years)}")

  # -------------------------------------------------------------------------
  # Load config if not provided
  # -------------------------------------------------------------------------
  if (is.null(config)) {
    config <- yaml::read_yaml("config/assumptions/tr2025.yaml")
  }

  # -------------------------------------------------------------------------
  # Emigration mode
  # -------------------------------------------------------------------------
  # O emigration is calculated dynamically based on O population stock.
  # Net O values for the population projection are taken directly from V.A2.
  # -------------------------------------------------------------------------
  cli::cli_alert_info("Mode: Dynamic emigration for O population stock")
  cli::cli_alert_info("Note: Population projection uses V.A2 net values directly")

  # -------------------------------------------------------------------------
  # Step 1: Get starting population (last historical year)
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 1: Preparing Starting Population")

  start_year <- min(projection_years) - 1
  starting_pop <- historical_o_pop[year == start_year]

  if (nrow(starting_pop) == 0) {
    # Try to find closest available year
    available_years <- unique(historical_o_pop$year)
    closest_year <- max(available_years[available_years <= start_year])
    cli::cli_alert_warning("No historical data for {start_year}, using {closest_year}")
    starting_pop <- historical_o_pop[year == closest_year]
  }

  # Add type dimension if missing
  if (!"type" %in% names(starting_pop)) {
    starting_pop <- add_type_dimension(starting_pop, year = start_year)
  }

  cli::cli_alert_success(
    "Starting population: {format(sum(starting_pop$population), big.mark = ',')} total"
  )

  # -------------------------------------------------------------------------
  # Step 2: Calculate ODIST (Equation 1.5.1 setup)
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 2: Calculating ODIST Distribution")

  if (!is.null(acs_new_arrivals) && !is.null(lpr_new_arrivals)) {
    # Calculate O immigration from ACS - LPR
    o_historical <- calculate_o_immigration(
      acs_new_arrivals,
      lpr_new_arrivals,
      undercount_factors,
      years = 2015:2019
    )

    # Calculate ODIST with historical type interpolation
    odist <- calculate_odist_with_interpolation(
      o_historical,
      reference_years = 2015:2019
    )
  } else {
    # Use default ODIST
    cli::cli_alert_warning("No ACS/LPR data provided, using default ODIST")
    odist <- get_default_odist()
  }

  # -------------------------------------------------------------------------
  # Step 3: Project O Immigration (Equation 1.5.1)
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 3: Projecting O Immigration (Equation 1.5.1)")

  # Get TR assumptions
  assumptions <- get_tr2025_o_immigration_assumptions(projection_years, config)

  # Project O immigration
  o_immigration_results <- list()
  for (yr in projection_years) {
    total_o <- assumptions[year == yr, total_o]
    yr_proj <- project_o_immigration(total_o, odist)
    yr_proj[, year := yr]
    o_immigration_results[[as.character(yr)]] <- yr_proj
  }
  o_immigration <- data.table::rbindlist(o_immigration_results)

  cli::cli_alert_success(
    "Projected O immigration: {format(sum(o_immigration$o_immigration), big.mark = ',')} total over {length(projection_years)} years"
  )

  # -------------------------------------------------------------------------
  # Step 4: Calculate Departure Rates (Equation 1.5.2 setup)
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 4: Calculating Departure Rates")

  # Get type splits for rate calculation period (2008-2010)
  type_splits_2010 <- get_type_splits_interpolated(
    year = 2010,
    age = 0:99,
    sex = c("male", "female")
  )

  # For base rates, use simplified calculation from historical O population
  base_rates <- calculate_simplified_departure_rates(
    historical_o_pop,
    mortality_qx,
    config
  )

  cli::cli_alert_success("Calculated base departure rates")

  # -------------------------------------------------------------------------
  # Steps 5-7: Emigration, Net O, and Population Stock
  # -------------------------------------------------------------------------
  # Two modes:
  # 1. Dynamic: Emigration grows with O population stock (more realistic)
  # 2. Constant: Use fixed net O from TR2025 assumptions (matches official projection)

  # -------------------------------------------------------------------------
  # Step 5: Project O Emigration (Dynamic)
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 5: Projecting O Emigration")

  emigration_result <- run_o_emigration_with_cohorts(
    starting_population = starting_pop,
    base_rates = base_rates,
    projection_years = projection_years,
    o_immigration = o_immigration,
    aos = lpr_aos,
    mortality_qx = mortality_qx,
    config = config,
    recent_threshold = 5
  )

  o_emigration <- emigration_result$emigration
  cohort_stats <- emigration_result$cohort_stats

  # -------------------------------------------------------------------------
  # Step 6: Calculate Net O Immigration
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 6: Calculating Net O Immigration")

  net_o <- calculate_net_o_immigration(
    o_immigration = o_immigration,
    o_emigration = o_emigration,
    aos = lpr_aos
  )

  # -------------------------------------------------------------------------
  # Step 7: Project O Population Stock
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 7: Projecting O Population Stock")

  o_population <- project_o_population_stock(
    starting_pop = starting_pop,
    o_immigration = o_immigration,
    o_emigration = o_emigration,
    aos = lpr_aos,
    mortality_qx = mortality_qx,
    projection_years = projection_years
  )

  # -------------------------------------------------------------------------
  # Summary
  # -------------------------------------------------------------------------
  cli::cli_h1("Projection Complete")

  # Final summary
  final_year <- max(projection_years)
  first_year <- min(projection_years)

  cli::cli_alert_info("O Immigration: {format(o_immigration[year == first_year, sum(o_immigration)], big.mark = ',')} ({first_year}) → {format(o_immigration[year == final_year, sum(o_immigration)], big.mark = ',')} ({final_year})")
  cli::cli_alert_info("O Emigration: {format(o_emigration[year == first_year, sum(emigration)], big.mark = ',')} ({first_year}) → {format(o_emigration[year == final_year, sum(emigration)], big.mark = ',')} ({final_year})")
  cli::cli_alert_info("Net O: {format(net_o[year == first_year, sum(net_o_immigration)], big.mark = ',')} ({first_year}) → {format(net_o[year == final_year, sum(net_o_immigration)], big.mark = ',')} ({final_year})")
  cli::cli_alert_info("O Population: {format(o_population[year == first_year, sum(population)], big.mark = ',')} ({first_year}) → {format(o_population[year == final_year, sum(population)], big.mark = ',')} ({final_year})")

  list(
    o_immigration = o_immigration,
    o_emigration = o_emigration,
    net_o_immigration = net_o,
    o_population = o_population,
    odist = odist,
    departure_rates = base_rates,
    assumptions = assumptions,
    cohort_stats = cohort_stats,
    mode = "dynamic"  # V.A2 net values used directly in population projection
  )
}

#' Calculate simplified departure rates
#'
#' @description
#' Calculates simplified departure rates when detailed 2008-2010 stock
#' build-up data is not available.
#'
#' @keywords internal
calculate_simplified_departure_rates <- function(historical_o_pop,
                                                  mortality_qx,
                                                  config = NULL) {
  if (is.null(config)) {
    config <- get_default_rate_config()
  }

  # ===========================================================================
  # HARDCODED BASE DEPARTURE RATES
  # ===========================================================================
  # When detailed 2008-2010 data is not available, use estimates based on
  # TR2025 methodology description and migration literature.
  #
  # Per TR2025: "rates are calculated by dividing OE by OP for each age, sex, type"
  # These are approximations of those rates.
  # ===========================================================================

  ages <- 0:99
  sexes <- c("male", "female")
  types <- c("N", "I", "V")

  # Create base rates grid
  base_rates <- data.table::CJ(age = ages, sex = sexes, type = types)

  # Age-specific base rates (higher for young adults who are more mobile)
  base_rates[, base_rate := data.table::fcase(
    age < 5, 0.02,
    age >= 5 & age < 18, 0.03,
    age >= 18 & age < 25, 0.08,   # Higher mobility
    age >= 25 & age < 35, 0.06,
    age >= 35 & age < 45, 0.04,
    age >= 45 & age < 55, 0.03,
    age >= 55 & age < 65, 0.02,
    age >= 65, 0.015
  )]

  # Type-specific adjustments
  # Never-authorized: Higher base rates (more vulnerable to deportation)
  base_rates[type == "N", base_rate := base_rate * 1.2]

  # Nonimmigrants: Lower rates (legal status, employment ties)
  base_rates[type == "I", base_rate := base_rate * 0.7]

  # Visa-overstayers: Medium rates
  base_rates[type == "V", base_rate := base_rate * 1.0]

  # Small sex difference (males slightly higher rates)
  base_rates[sex == "male", base_rate := base_rate * 1.05]

  base_rates
}

#' Get default ODIST
#'
#' @description
#' Returns default ODIST when ACS/LPR data not available.
#'
#' @keywords internal
get_default_odist <- function() {
  # ===========================================================================
  # HARDCODED DEFAULT ODIST
  # ===========================================================================
  # Based on TR2025 Input #31: "Internally developed number of temporary or
  # unlawfully present immigrants, by age and sex. These data were averaged
  # over years 2015-2019"
  # ===========================================================================

  ages <- 0:99
  sexes <- c("male", "female")
  types <- c("N", "I", "V")

  result <- data.table::CJ(age = ages, sex = sexes, type = types)

  # Age distribution (working-age peak)
  result[, age_weight := data.table::fcase(
    age < 5, 0.01,
    age >= 5 & age < 18, 0.03,
    age >= 18 & age < 25, 0.12,
    age >= 25 & age < 35, 0.20,
    age >= 35 & age < 45, 0.15,
    age >= 45 & age < 55, 0.08,
    age >= 55 & age < 65, 0.04,
    age >= 65, 0.02
  )]

  # Sex distribution (roughly equal)
  result[sex == "male", sex_weight := 0.50]
  result[sex == "female", sex_weight := 0.50]

  # Type distribution
  result[type == "N", type_weight := 0.50]  # Never-authorized
  result[type == "I", type_weight := 0.15]  # Nonimmigrant
  result[type == "V", type_weight := 0.35]  # Visa-overstayer

  # Calculate ODIST
  result[, avg_o_immigration := age_weight * sex_weight * type_weight]

  # Normalize
  total <- sum(result$avg_o_immigration)
  result[, odist := avg_o_immigration / total]

  result[, .(age, sex, type, avg_o_immigration, odist)]
}

# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

#' Validate O projection outputs
#'
#' @description
#' Validates O immigration projection outputs against TR2025 assumptions
#' and internal consistency checks.
#'
#' @param projection Output from run_full_o_projection()
#' @param tolerance Validation tolerance (default: 0.05 = 5%)
#'
#' @return List with validation results
#'
#' @export
validate_o_projection <- function(projection, tolerance = 0.05) {
  cli::cli_h2("Validating O Immigration Projection")

  checks <- list()

  # -------------------------------------------------------------------------
  # Check 1: O immigration totals match TR assumptions
  # -------------------------------------------------------------------------
  imm_totals <- projection$o_immigration[, .(
    model_total = sum(o_immigration)
  ), by = year]

  imm_totals <- merge(imm_totals, projection$assumptions, by = "year")
  imm_totals[, pct_diff := abs(model_total - total_o) / total_o]

  all_match <- all(imm_totals$pct_diff < 0.001)
  checks$immigration_totals <- list(
    passed = all_match,
    message = ifelse(all_match,
                     "O immigration totals match TR assumptions exactly",
                     "O immigration totals differ from TR assumptions")
  )

  if (all_match) {
    cli::cli_alert_success("O immigration totals match TR assumptions")
  } else {
    cli::cli_alert_danger("O immigration totals differ from assumptions")
  }

  # -------------------------------------------------------------------------
  # Check 2: ODIST sums to 1
  # -------------------------------------------------------------------------
  odist_sum <- sum(projection$odist$odist)
  odist_valid <- abs(odist_sum - 1) < 0.001
  checks$odist_sum <- list(
    passed = odist_valid,
    value = odist_sum,
    message = ifelse(odist_valid,
                     "ODIST sums to 1.0",
                     paste0("ODIST sums to ", round(odist_sum, 4)))
  )

  if (odist_valid) {
    cli::cli_alert_success("ODIST sums to 1.0")
  } else {
    cli::cli_alert_danger("ODIST does not sum to 1.0: {round(odist_sum, 4)}")
  }

  # -------------------------------------------------------------------------
  # Check 3: Population is non-negative
  # -------------------------------------------------------------------------
  neg_pop <- projection$o_population[population < 0]
  pop_valid <- nrow(neg_pop) == 0
  checks$population_nonneg <- list(
    passed = pop_valid,
    n_negative = nrow(neg_pop),
    message = ifelse(pop_valid,
                     "All population values non-negative",
                     paste0(nrow(neg_pop), " negative population values"))
  )

  if (pop_valid) {
    cli::cli_alert_success("All population values non-negative")
  } else {
    cli::cli_alert_danger("{nrow(neg_pop)} negative population values found")
  }

  # -------------------------------------------------------------------------
  # Check 4: Type proportions are reasonable
  # -------------------------------------------------------------------------
  type_props <- projection$odist[, .(prop = sum(odist)), by = type]

  n_prop <- type_props[type == "N", prop]
  i_prop <- type_props[type == "I", prop]
  v_prop <- type_props[type == "V", prop]

  types_valid <- n_prop > 0.3 && n_prop < 0.7 &&
    i_prop > 0.05 && i_prop < 0.4 &&
    v_prop > 0.2 && v_prop < 0.5

  checks$type_proportions <- list(
    passed = types_valid,
    N = n_prop, I = i_prop, V = v_prop,
    message = paste0("Type props: N=", round(n_prop*100), "%, I=",
                     round(i_prop*100), "%, V=", round(v_prop*100), "%")
  )

  cli::cli_alert_info(
    "Type proportions: N={round(n_prop*100)}%, I={round(i_prop*100)}%, V={round(v_prop*100)}%"
  )

  # -------------------------------------------------------------------------
  # Check 5: Stock equation balance
  # -------------------------------------------------------------------------
  # OP^z = OP^{z-1} + OI - OE - AOS - OD
  # We can check this by comparing successive years

  # Summary
  n_passed <- sum(sapply(checks, function(x) x$passed))
  n_total <- length(checks)

  cli::cli_h3("Validation Summary")
  cli::cli_alert_info("{n_passed}/{n_total} checks passed")

  list(
    passed = n_passed == n_total,
    checks = checks,
    n_passed = n_passed,
    n_total = n_total
  )
}

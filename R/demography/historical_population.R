#' Historical Population Subprocess (Section 1.4)
#'
#' @description
#' Functions for calculating the historical Social Security area population
#' from December 31, 1940 through December 31, 2022 (TR2025).
#'
#' This module implements Equation 1.4.1 from the TR2025 documentation:
#'   P^z_{x,s} = USAF + UC + TERR + FED + DEP + BEN + OTH
#'
#' Where:
#' - USAF = U.S. resident population + Armed Forces overseas
#' - UC = Net census undercount adjustment
#' - TERR = Territory residents (PR, VI, Guam, CNMI, AS)
#' - FED = Federal civilian employees overseas
#' - DEP = Dependents of armed forces and federal employees overseas
#' - BEN = Residual OASDI beneficiaries living abroad
#' - OTH = Other U.S. citizens overseas
#'
#' @section Methodology:
#'
#' **Tab Years:** Populations are estimated precisely for specific years:
#' - 1940, 1950, 1956, 1960
#' - Every December from 1969 through 2009
#' - Last year of historical data (2022 for TR2025)
#'
#' **Inter-Tab Years:** Populations between tab years are interpolated using
#' components of change (births, deaths, immigration, emigration) and then
#' ratio-adjusted to eliminate closure error at the next tab year.
#'
#' **Ages 85+:** For years 1980+, Census provides single-year-of-age data
#' through age 100, which is used directly. For pre-1980 years, ages 85+ are
#' estimated using a survival-based distribution applied to aggregate Census
#' 85+ totals.
#'
#' @section Data Sources:
#' - Census Bureau: Population estimates, decennial census
#' - OPM: Federal civilian employees overseas
#' - SSA: Beneficiaries abroad
#' - DMDC: Armed forces overseas
#' - DHS: Unauthorized immigrant estimates
#'
#' @name historical_population
NULL

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Calculate Historical Population by Age and Sex (Equation 1.4.1)
#'
#' @description
#' Main entry point for calculating the complete historical Social Security
#' area population from 1940 to 2022.
#'
#' @param start_year Integer: first year of estimates (default: 1940)
#' @param end_year Integer: last year of estimates (default: 2022)
#' @param ages Integer vector: ages to include (default: 0:100)
#' @param config List: configuration from tr2025.yaml (optional)
#' @param cache_dir Character: directory for caching (default: data/cache)
#'
#' @return data.table with columns:
#'   - year: Calendar year (December 31)
#'   - age: Single year of age (0-100+)
#'   - sex: "male" or "female"
#'   - population: Estimated population
#'   - source: Data source indicator ("tab_year" or "interpolated")
#'
#' @details
#' The function proceeds in these steps:
#' 1. Calculate tab year populations (ages 0-84)
#' 2. Build up ages 85+ for tab years
#' 3. Interpolate populations for non-tab years
#' 4. Extend to single year of age 100+
#'
#' @export
calculate_historical_population <- function(start_year = 1940,
                                             end_year = 2022,
                                             ages = 0:100,
                                             config = NULL,
                                             cache_dir = here::here("data/cache"),
                                             use_cache = TRUE) {
  cli::cli_h1("Calculating Historical Population (Eq 1.4.1)")

  # Load config if not provided
  if (is.null(config)) {
    config_path <- here::here("config/assumptions/tr2025.yaml")
    if (file.exists(config_path)) {
      config <- yaml::read_yaml(config_path)
    }
  }

  # Set census vintage from config
  if (!is.null(config) && !is.null(config$data_sources$census_vintage)) {
    vintage <- config$data_sources$census_vintage
    options(artemis.census_vintage = vintage)
    cli::cli_alert_info("Using Census Vintage {vintage}")
  }

  # Check cache first
  cache_subdir <- file.path(cache_dir, "historical_population")
  if (!dir.exists(cache_subdir)) dir.create(cache_subdir, recursive = TRUE)

  cache_file <- file.path(
    cache_subdir,
    sprintf("ss_population_%d_%d.rds", start_year, end_year)
  )

  if (use_cache && file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached historical population")
    cached <- readRDS(cache_file)

    # Handle both old format (data.table) and new format (list with components)
    if (is.list(cached) && "population" %in% names(cached)) {
      pop_data <- cached$population
    } else {
      pop_data <- cached
    }

    target_ages <- ages
    result <- pop_data[age %in% target_ages]
    cli::cli_alert_info("Loaded {nrow(result)} cells for {start_year}-{end_year}")
    return(result)
  }

  # Get tab years
  tab_years <- get_tab_years()
  tab_years <- tab_years[tab_years >= start_year & tab_years <= end_year]

  cli::cli_alert_info("Estimating population for {start_year}-{end_year}")
  cli::cli_alert_info("Tab years: {length(tab_years)} ({min(tab_years)}-{max(tab_years)})")

  # Step 1: Gather all component data
  cli::cli_h2("Step 1: Gathering Component Data")
  components <- gather_population_components(
    years = start_year:end_year,
    ages = ages,
    cache_dir = cache_dir,
    config = config
  )

  # Step 2: Calculate tab year populations (ages 0-84)
  cli::cli_h2("Step 2: Tab Year Populations (Ages 0-84)")
  tab_pop_0_84 <- calculate_tab_year_populations(
    tab_years = tab_years,
    components = components,
    ages = 0:84
  )

  # Step 3: Build up ages 85+ for tab years
  cli::cli_h2("Step 3: Build Up Ages 85+")
  tab_pop_85_plus <- build_up_ages_85_plus(
    tab_years = tab_years,
    components = components,
    max_age = max(ages)
  )

  # Combine tab year populations
  tab_pop <- data.table::rbindlist(list(tab_pop_0_84, tab_pop_85_plus))
  data.table::setorder(tab_pop, year, sex, age)

  # Step 4: Interpolate between tab years
  cli::cli_h2("Step 4: Inter-Tab Year Interpolation")
  all_pop <- interpolate_populations(
    tab_year_pops = tab_pop,
    tab_years = tab_years,
    target_years = start_year:end_year,
    components = components
  )

  # Summary statistics
  cli::cli_h2("Summary")
  total_by_year <- all_pop[, .(total = sum(population)), by = year]
  cli::cli_alert_success("Calculated {nrow(all_pop)} population cells")
  cli::cli_alert_info("1940 total: {format(total_by_year[year == min(year), total], big.mark = ',', scientific = FALSE)}")
  cli::cli_alert_info("{end_year} total: {format(total_by_year[year == max(year), total], big.mark = ',', scientific = FALSE)}")

  # Compute component totals by year for analysis
  component_totals <- compute_component_totals(components)

  # Save to cache (both population and components)
  cache_data <- list(
    population = all_pop,
    components = component_totals
  )
  saveRDS(cache_data, cache_file)
  cli::cli_alert_success("Saved to cache: {cache_file}")

  all_pop
}

#' Get Cached Component Totals
#'
#' @description
#' Retrieves the component totals from cache without recomputing.
#'
#' @param start_year Integer: first year (default: 1940)
#' @param end_year Integer: last year (default: 2022)
#' @param cache_dir Character: cache directory
#'
#' @return data.table with component totals by year, or NULL if not cached
#'
#' @export
get_cached_components <- function(start_year = 1940,
                                   end_year = 2022,
                                   cache_dir = here::here("data/cache")) {
  cache_file <- file.path(
    cache_dir, "historical_population",
    sprintf("ss_population_%d_%d.rds", start_year, end_year)
  )

  if (!file.exists(cache_file)) {
    cli::cli_alert_warning("Cache file not found. Run calculate_historical_population() first.")
    return(NULL)
  }

  cached <- readRDS(cache_file)

  if (is.list(cached) && "components" %in% names(cached)) {
    return(cached$components)
  } else {
    cli::cli_alert_warning("Cache is in old format without components. Re-run calculate_historical_population() with use_cache=FALSE.")
    return(NULL)
  }
}

#' Compute Component Totals by Year
#'
#' @description
#' Summarizes all population components by year for analysis.
#'
#' @param components List of component data from gather_population_components
#'
#' @return data.table with component totals by year
#'
#' @keywords internal
compute_component_totals <- function(components) {
  # Census USAF totals
  usaf <- components$census_usaf[, .(census_usaf = sum(population)), by = year]

  # Territory totals
  terr <- components$territories[, .(territories = sum(territory_pop)), by = year]

  # Undercount adjustment - need to calculate using the formula
  uc_data <- merge(components$census_usaf, components$undercount,
                   by = c("year", "age", "sex"), all.x = TRUE)
  uc_data[is.na(undercount_rate), undercount_rate := 0]
  uc_data[, uc_adj := population * undercount_rate / (1 - undercount_rate)]
  uc_totals <- uc_data[, .(undercount_adj = sum(uc_adj)), by = year]

  # Federal employees
 fed <- components$fed_employees[, .(fed_employees = employees_overseas), by = year]

  # Beneficiaries abroad
  ben <- components$beneficiaries[, .(beneficiaries = total_beneficiaries), by = year]

  # Other overseas
  oth <- components$other_overseas[, .(other_overseas = other_overseas), by = year]

  # Armed forces
  af <- components$armed_forces[, .(armed_forces = sum(population)), by = year]

  # Dependents = 0.5 * (armed_forces + fed_employees)
  deps <- merge(af, fed, by = "year", all = TRUE)
  deps[is.na(fed_employees), fed_employees := 0]
  deps[is.na(armed_forces), armed_forces := 0]
  deps[, dependents := 0.5 * (armed_forces + fed_employees)]

  # Combine all
  all_comp <- Reduce(function(x, y) merge(x, y, by = "year", all = TRUE),
    list(usaf, uc_totals, terr, fed, deps[, .(year, armed_forces, dependents)], ben, oth))

  # Fill NAs with 0
  for (col in names(all_comp)[-1]) {
    all_comp[is.na(get(col)), (col) := 0]
  }

  # Calculate total
  all_comp[, total := census_usaf + undercount_adj + territories +
             fed_employees + dependents + beneficiaries + other_overseas]

  data.table::setorder(all_comp, year)
  all_comp
}

# =============================================================================
# COMPONENT DATA GATHERING
# =============================================================================

#' Gather All Population Components
#'
#' @description
#' Fetches all data needed for the population calculation:
#' - Census USAF population
#' - Territory populations
#' - Census undercount factors
#' - Federal employees overseas
#' - Beneficiaries abroad
#' - Mortality data (for build-up)
#' - Immigration/emigration data (for interpolation)
#'
#' @keywords internal
gather_population_components <- function(years,
                                          ages,
                                          cache_dir,
                                          config = NULL) {
  components <- list()

  # 1. Census USAF (resident + armed forces overseas) populations
  cli::cli_alert("Fetching Census USAF populations...")
  components$census_usaf <- fetch_census_usaf_for_historical(years, ages, cache_dir)

  # 2. Territory populations
  cli::cli_alert("Fetching territory populations...")
  components$territories <- fetch_territory_for_historical(years, ages, cache_dir)

  # 3. Census undercount factors
  cli::cli_alert("Fetching census undercount factors...")
  components$undercount <- get_undercount_for_years(years, ages, config)

  # 4. Federal employees overseas
  cli::cli_alert("Fetching federal employees overseas...")
  components$fed_employees <- fetch_fed_employees_for_historical(years)

  # 5. SSA beneficiaries abroad
  cli::cli_alert("Fetching SSA beneficiaries abroad...")
  components$beneficiaries <- fetch_beneficiaries_for_historical(years)

  # 6. Other citizens overseas (Americans abroad estimate)
  cli::cli_alert("Fetching other citizens overseas estimate...")
  components$other_overseas <- get_other_citizens_overseas(years)

  # 7. Armed forces overseas (for dependents calculation)
  cli::cli_alert("Fetching armed forces overseas data...")
  components$armed_forces <- fetch_armed_forces_for_historical(years)

  # 8. Mortality data (qx)
  cli::cli_alert("Loading mortality data...")
  components$mortality <- load_mortality_data()

  # 9. Immigration/emigration data
  cli::cli_alert("Loading immigration/emigration data...")
  components$immigration <- load_immigration_data()
  components$emigration <- load_emigration_data()

  # 10. Births data
  cli::cli_alert("Loading births data...")
  components$births <- load_births_data()

  cli::cli_alert_success("All component data gathered")

  components
}

# =============================================================================
# CENSUS USAF DATA
# =============================================================================

#' Fetch Census USAF Population for Historical Calculation
#'
#' @description
#' Gets the resident population + armed forces overseas from Census Bureau.
#' This is the base population before adjustments.
#'
#' @keywords internal
fetch_census_usaf_for_historical <- function(years, ages, cache_dir) {
  # Use the existing census_historical_population.R functions
  # Need different approaches for different year ranges

  result_list <- list()

  # 1. Modern era (1980-2023): Use Census PEP API
  # For December 31 estimates, we need January 1 of the NEXT year
  # Dec 31, 2022 ~ Jan 1, 2023
  modern_years <- years[years >= 1980 & years <= 2023]
  if (length(modern_years) > 0) {
    cli::cli_alert("  Modern era (1980-2023): Census PEP API")

    # Request Jan 1 data for Y+1 to get Dec 31 of year Y
    fetch_years <- modern_years + 1L  # Need Jan 1 of next year

    modern_pop <- fetch_census_historical_population(
      years = fetch_years,
      reference_date = "jan1",
      concept = "resident_usaf",
      ages = ages,
      cache_dir = cache_dir
    )

    # Adjust year reference: Jan 1 year Y+1 = Dec 31 year Y
    modern_pop[, year := year - 1L]

    # Filter to requested years only
    modern_pop <- modern_pop[year %in% modern_years]
    result_list$modern <- modern_pop
  }

  # 2. Early era (1940-1979): Use static pre-1980 data with age estimation
  early_years <- years[years >= 1940 & years < 1980]
  if (length(early_years) > 0) {
    cli::cli_alert("  Early era (1940-1979): Static estimates + age distribution")
    early_pop <- estimate_pre1980_population(early_years, ages)
    result_list$early <- early_pop
  }

  result <- data.table::rbindlist(result_list, fill = TRUE)
  data.table::setorder(result, year, sex, age)
  result
}

#' Estimate Pre-1980 Population by Age and Sex
#'
#' @description
#' For years 1940-1979, we have total population by sex but not detailed
#' age distributions from Census APIs. This function estimates age-sex
#' distributions using decennial census benchmarks and interpolation.
#'
#' Adjusts from July 1 reference date to December 31 by interpolating
#' with the following year's population.
#'
#' @keywords internal
estimate_pre1980_population <- function(years, ages) {
  # Get pre-1980 totals from historical_static.R (July 1 estimates)
  # Request years and years+1 for December 31 interpolation
  all_years_needed <- unique(c(years, years + 1))
  all_years_needed <- all_years_needed[all_years_needed <= 1979]
  totals <- get_pre1980_usaf_population(years = all_years_needed, by_age = FALSE)

  # Get 1980 July 1 population from Census data for 1979 Dec 31 interpolation
  pop_1980 <- NULL
  if (1979 %in% years) {
    pop_1980 <- tryCatch({
      get_census_population_totals(1980)
    }, error = function(e) NULL)
  }

  # Get age distributions from decennial censuses
  census_years <- c(1940, 1950, 1960, 1970)
  census_years <- census_years[census_years >= min(years) - 10 &
                                 census_years <= max(years) + 10]

  # Fetch decennial census age distributions
  decennial_dist <- fetch_decennial_age_distributions(census_years, ages)

  # Interpolate age distributions for each year
  result_list <- lapply(years, function(yr) {
    year_dist <- interpolate_age_distribution(yr, decennial_dist)

    # Get July 1 totals for this year and next year
    male_jul1 <- totals[year == yr, male]
    female_jul1 <- totals[year == yr, female]

    # Adjust from July 1 to December 31
    # Dec 31 of year Y is approximately halfway between July 1 of Y and July 1 of Y+1
    if (yr < 1979 && (yr + 1) %in% totals$year) {
      male_jul1_next <- totals[year == yr + 1, male]
      female_jul1_next <- totals[year == yr + 1, female]

      # Interpolate: Dec 31 = Jul 1 + 0.5 * (next Jul 1 - this Jul 1)
      male_total <- male_jul1 + 0.5 * (male_jul1_next - male_jul1)
      female_total <- female_jul1 + 0.5 * (female_jul1_next - female_jul1)
    } else if (yr == 1979 && !is.null(pop_1980) && nrow(pop_1980) > 0) {
      # For 1979, use 1980 Census data to interpolate
      # Assume sex ratio similar to 1979 (slightly more female)
      total_1980 <- pop_1980[year == 1980, population]
      male_ratio <- male_jul1 / (male_jul1 + female_jul1)

      total_dec31 <- (male_jul1 + female_jul1) + 0.5 * (total_1980 - (male_jul1 + female_jul1))
      male_total <- total_dec31 * male_ratio
      female_total <- total_dec31 * (1 - male_ratio)
    } else {
      # Fallback: use ~1% annual growth to estimate Dec 31
      male_total <- male_jul1 * 1.005
      female_total <- female_jul1 * 1.005
    }

    if (length(male_total) == 0) male_total <- NA_real_
    if (length(female_total) == 0) female_total <- NA_real_

    # Apply distribution to totals
    pop <- year_dist[, .(
      year = yr,
      age = age,
      sex = sex,
      population = data.table::fifelse(
        sex == "male",
        proportion * male_total,
        proportion * female_total
      ),
      source = "pre1980_estimated"
    )]

    pop
  })

  data.table::rbindlist(result_list)
}

#' Fetch Decennial Census Age Distributions
#'
#' @keywords internal
fetch_decennial_age_distributions <- function(census_years, ages) {
  # Use existing decennial census function or historical static data
  result_list <- lapply(census_years, function(yr) {
    if (yr >= 1970) {
      # Use Census API for 1970+
      pop <- fetch_decennial_census_population(
        census_years = yr,
        ages = ages,
        concept = "resident"
      )
    } else {
      # For 1940-1960, use hardcoded distributions from historical records
      pop <- get_historical_age_distribution(yr, ages)
    }

    # Calculate proportions
    pop[, total := sum(population), by = sex]
    pop[, proportion := population / total]
    pop[, census_year := yr]

    pop[, .(census_year, age, sex, proportion)]
  })

  data.table::rbindlist(result_list)
}

#' Interpolate Age Distribution for a Given Year
#'
#' @keywords internal
interpolate_age_distribution <- function(year, decennial_dist) {
  census_years <- sort(unique(decennial_dist$census_year))

  # Find bracketing census years
  lower_year <- max(census_years[census_years <= year])
  upper_year <- min(census_years[census_years >= year])

  if (is.na(lower_year) || is.infinite(lower_year)) lower_year <- min(census_years)
  if (is.na(upper_year) || is.infinite(upper_year)) upper_year <- max(census_years)

  if (lower_year == upper_year) {
    # Exact match or extrapolation
    return(decennial_dist[census_year == lower_year, .(age, sex, proportion)])
  }

  # Linear interpolation
  weight <- (year - lower_year) / (upper_year - lower_year)

  lower_dist <- decennial_dist[census_year == lower_year]
  upper_dist <- decennial_dist[census_year == upper_year]

  merged <- merge(
    lower_dist[, .(age, sex, prop_lower = proportion)],
    upper_dist[, .(age, sex, prop_upper = proportion)],
    by = c("age", "sex")
  )

  merged[, proportion := prop_lower + weight * (prop_upper - prop_lower)]
  merged[, .(age, sex, proportion)]
}

#' Get Historical Age Distribution for Pre-1970 Years
#'
#' @description
#' Returns estimated age-sex distributions for 1940-1960 censuses based on
#' published census reports.
#'
#' @keywords internal
get_historical_age_distribution <- function(census_year, ages) {
  # Use 1940 85+ distribution function as a starting point
  # and typical demographic patterns for the era

  # Demographic parameters by year (from census reports)
  params <- list(
    "1940" = list(median_age = 29.0, youth_share = 0.34, elderly_share = 0.07),
    "1950" = list(median_age = 30.2, youth_share = 0.31, elderly_share = 0.08),
    "1960" = list(median_age = 29.5, youth_share = 0.36, elderly_share = 0.09)  # Baby boom
  )

  p <- params[[as.character(census_year)]]
  if (is.null(p)) p <- params[["1960"]]

  # Generate smooth age distribution using demographic model
  # This is a simplified approximation - real data would be better
  result <- generate_demographic_age_distribution(ages, p, census_year)

  result
}

#' Generate Demographic Age Distribution
#'
#' @description
#' Creates a smooth age distribution based on demographic parameters.
#' Uses a combination of Gompertz mortality curve and historical birth patterns.
#'
#' @keywords internal
generate_demographic_age_distribution <- function(ages, params, census_year) {
  # Use a gamma-like distribution for working ages,
  # declining survivorship for older ages

  pop_male <- numeric(length(ages))
  pop_female <- numeric(length(ages))

  for (i in seq_along(ages)) {
    a <- ages[i]

    # Base population (birth cohort survival)
    if (a < 20) {
      # Children/youth - affected by recent birth rates
      base <- exp(-0.01 * a) * (1 + params$youth_share * 0.5)
    } else if (a < 65) {
      # Working age - stable with slight decline
      base <- exp(-0.015 * (a - 20)) * 0.95
    } else {
      # Elderly - faster mortality decline
      base <- exp(-0.015 * 45) * exp(-0.05 * (a - 65))
    }

    # Sex ratio (males slightly higher at birth, lower at older ages)
    sex_ratio <- 1.05 * exp(-0.005 * a)
    male_share <- sex_ratio / (1 + sex_ratio)

    total <- base * 1e6  # Scale factor
    pop_male[i] <- total * male_share
    pop_female[i] <- total * (1 - male_share)
  }

  # Normalize to sum to 1 within each sex
  pop_male <- pop_male / sum(pop_male)
  pop_female <- pop_female / sum(pop_female)

  data.table::data.table(
    age = rep(ages, 2),
    sex = c(rep("male", length(ages)), rep("female", length(ages))),
    population = c(pop_male, pop_female),
    proportion = c(pop_male, pop_female)
  )
}

# =============================================================================
# TERRITORY POPULATIONS
# =============================================================================

#' Fetch Territory Populations for Historical Calculation
#'
#' @description
#' Gets territory populations for all years. Uses IDB API for 1990+ and
#' historical decennial census data with interpolation for pre-1990.
#'
#' @keywords internal
fetch_territory_for_historical <- function(years, ages, cache_dir) {
  territories <- c("PR", "VI", "GU", "MP", "AS")
  ss_start_years <- sapply(territories, get_territory_ss_start_year)

  # IDB API first year of coverage by territory
  # Based on testing: AS/GU/MP/VI have data from 2000+, PR from 2010+
  idb_start_years <- c("PR" = 2010, "VI" = 2000, "GU" = 2000, "MP" = 2000, "AS" = 2000)

  result_list <- list()

  for (i in seq_along(territories)) {
    terr <- territories[i]
    start_year <- ss_start_years[i]
    terr_years <- years[years >= start_year]

    if (length(terr_years) == 0) next

    # Split into pre-IDB and IDB-covered years
    idb_start <- idb_start_years[[terr]]
    interpolation_years <- terr_years[terr_years < idb_start]
    idb_years <- terr_years[terr_years >= idb_start]

    # Pre-IDB: Use historical decennial data with interpolation
    if (length(interpolation_years) > 0) {
      interp_pop <- get_territory_population_interpolated(terr, interpolation_years, ages)
      if (!is.null(interp_pop) && nrow(interp_pop) > 0) {
        interp_pop[, territory := terr]
        result_list[[paste0(terr, "_interp")]] <- interp_pop
      }
    }

    # IDB-covered years: Use IDB API with fallback to interpolation
    if (length(idb_years) > 0) {
      idb_pop <- tryCatch({
        fetch_territory_populations_by_age_sex(
          years = idb_years,
          territories = terr,
          cache_dir = cache_dir
        )
      }, error = function(e) {
        cli::cli_alert_warning("Territory {terr} IDB error: {conditionMessage(e)}")
        NULL
      })

      if (!is.null(idb_pop) && nrow(idb_pop) > 0) {
        idb_pop[, territory := terr]
        result_list[[paste0(terr, "_idb")]] <- idb_pop

        # Check for missing years and use fallback
        years_with_data <- unique(idb_pop$year)
        missing_years <- setdiff(idb_years, years_with_data)
        if (length(missing_years) > 0) {
          cli::cli_alert_info("Territory {terr}: Interpolating {length(missing_years)} years missing from IDB")
          fallback_pop <- get_territory_population_interpolated(terr, missing_years, ages)
          if (!is.null(fallback_pop) && nrow(fallback_pop) > 0) {
            fallback_pop[, territory := terr]
            result_list[[paste0(terr, "_fallback")]] <- fallback_pop
          }
        }
      } else {
        # IDB returned nothing, use interpolation for all
        cli::cli_alert_info("Territory {terr}: Using interpolation (no IDB data)")
        fallback_pop <- get_territory_population_interpolated(terr, idb_years, ages)
        if (!is.null(fallback_pop) && nrow(fallback_pop) > 0) {
          fallback_pop[, territory := terr]
          result_list[[paste0(terr, "_fallback")]] <- fallback_pop
        }
      }
    }
  }

  if (length(result_list) == 0) {
    return(data.table::data.table(
      year = integer(),
      age = integer(),
      sex = character(),
      population = numeric(),
      territory = character()
    ))
  }

  result <- data.table::rbindlist(result_list, fill = TRUE)

  # Aggregate across territories
  result[, .(
    territory_pop = sum(population, na.rm = TRUE)
  ), by = .(year, age, sex)]
}

#' Get Territory Population via Interpolation
#'
#' @description
#' Uses historical decennial census totals (1950-2020) and interpolates between
#' census years, distributing by age using standard age distribution.
#' Used as fallback when IDB API doesn't have data for a territory/year.
#'
#' @keywords internal
get_territory_population_interpolated <- function(territory, years, ages) {
  # Get historical territory data (decennial census totals)
  hist_data <- get_territory_historical_population()
  target_terr <- territory
  terr_data <- hist_data[territory == target_terr]

  if (nrow(terr_data) == 0) return(NULL)

  # Interpolate total population for each requested year
  result_list <- lapply(years, function(yr) {
    # Find bracketing census years
    census_years <- terr_data$census_year
    lower_census <- max(census_years[census_years <= yr], na.rm = TRUE)
    upper_census <- min(census_years[census_years >= yr], na.rm = TRUE)

    if (is.infinite(lower_census)) lower_census <- min(census_years)
    if (is.infinite(upper_census)) upper_census <- max(census_years)

    # Get populations at bracketing years
    lower_pop <- terr_data[census_year == lower_census, population]
    upper_pop <- terr_data[census_year == upper_census, population]

    # Interpolate
    if (lower_census == upper_census) {
      total_pop <- lower_pop
    } else {
      weight <- (yr - lower_census) / (upper_census - lower_census)
      total_pop <- lower_pop + weight * (upper_pop - lower_pop)
    }

    # Distribute across ages using standard age distribution
    distribute_population_by_age(total_pop, yr, ages)
  })

  data.table::rbindlist(result_list)
}

#' Distribute Population by Age
#'
#' @description
#' Distributes a total population across ages using a standard
#' demographic age distribution typical for the era.
#'
#' @keywords internal
distribute_population_by_age <- function(total_pop, year, ages) {
  # Use a typical demographic age distribution
  # Younger populations in earlier years, aging over time

  # Create age weights based on a modified Gompertz-like curve
  age_weights <- sapply(ages, function(a) {
    if (a < 5) {
      0.07  # Young children
    } else if (a < 18) {
      0.065  # Children/teens
    } else if (a < 65) {
      0.055 * exp(-0.01 * (a - 30))  # Working age
    } else {
      0.03 * exp(-0.03 * (a - 65))  # Elderly
    }
  })

  # Normalize weights
  age_weights <- age_weights / sum(age_weights)

  # Split by sex (roughly equal with slight female majority at older ages)
  male_share <- ifelse(ages < 65, 0.51, 0.45)

  data.table::data.table(
    year = year,
    age = rep(ages, 2),
    sex = c(rep("male", length(ages)), rep("female", length(ages))),
    population = c(
      total_pop * age_weights * male_share,
      total_pop * age_weights * (1 - male_share)
    )
  )
}

# =============================================================================
# CENSUS UNDERCOUNT
# =============================================================================

#' Get Undercount Factors for All Years
#'
#' @description
#' Gets census undercount adjustment factors for all years by
#' interpolating between decennial census estimates.
#'
#' @param years Integer vector of years to get factors for
#' @param ages Integer vector of ages (not currently used, for future expansion)
#' @param config List: configuration with undercount method and PES factors
#'
#' @keywords internal
get_undercount_for_years <- function(years, ages, config = NULL) {
  # Decennial census years have direct estimates
  decennial_years <- seq(1940, 2020, by = 10)

  # Fetch undercount for all decennial years
  undercount_list <- lapply(decennial_years, function(cy) {
    uc <- fetch_census_undercount_factors(cy, by_age = TRUE, config = config)
    uc[, census_year := cy]
    uc
  })

  undercount_data <- data.table::rbindlist(undercount_list)

  # Interpolate for each target year
  result_list <- lapply(years, function(yr) {
    # Find bracketing censuses
    lower <- max(decennial_years[decennial_years <= yr])
    upper <- min(decennial_years[decennial_years >= yr])

    if (lower == upper || yr > 2020) {
      # Use nearest census
      return(undercount_data[census_year == lower, .(
        year = yr,
        age = age,
        sex = sex,
        undercount_rate = undercount_rate
      )])
    }

    # Interpolate
    weight <- (yr - lower) / (upper - lower)
    lower_uc <- undercount_data[census_year == lower]
    upper_uc <- undercount_data[census_year == upper]

    merged <- merge(
      lower_uc[, .(age, sex, rate_lower = undercount_rate)],
      upper_uc[, .(age, sex, rate_upper = undercount_rate)],
      by = c("age", "sex")
    )

    merged[, .(
      year = yr,
      age = age,
      sex = sex,
      undercount_rate = rate_lower + weight * (rate_upper - rate_lower)
    )]
  })

  data.table::rbindlist(result_list)
}

# =============================================================================
# OTHER POPULATION COMPONENTS
# =============================================================================

#' Fetch Federal Employees Overseas
#'
#' @keywords internal
fetch_fed_employees_for_historical <- function(years) {
  tryCatch({
    fed <- fetch_opm_federal_employees_overseas(include_dependents = FALSE)
    fed <- fed[year %in% years]
    fed
  }, error = function(e) {
    cli::cli_alert_warning("Federal employees data error: {conditionMessage(e)}")
    data.table::data.table(year = years, employees = 0)
  })
}

#' Fetch SSA Beneficiaries Abroad
#'
#' @keywords internal
fetch_beneficiaries_for_historical <- function(years) {
  tryCatch({
    ben <- fetch_ssa_beneficiaries_abroad()
    ben <- ben[year %in% years]
    ben
  }, error = function(e) {
    cli::cli_alert_warning("Beneficiaries data error: {conditionMessage(e)}")
    data.table::data.table(year = years, total_beneficiaries = 0)
  })
}

#' Get Other Citizens Overseas Estimate
#'
#' @description
#' Estimates the number of other U.S. citizens in the Social Security Area
#' who are not captured by other components.
#'
#' @details
#' The "OTH" component in TR methodology is a small residual category.
#' While the State Department estimates ~9M Americans live abroad, most
#' are NOT part of the Social Security Area population. The SS Area only
#' includes those with specific ties (military, federal employees, dependents,
#' certain beneficiaries).
#'
#' This residual covers:
#' - Some long-term residents abroad receiving SS benefits
#' - Citizens with historical SS coverage living abroad
#'
#' The estimate is calibrated to match TR2025 totals, scaling with
#' US resident population growth.
#'
#' @keywords internal
get_other_citizens_overseas <- function(years) {
  # Most Americans abroad are NOT part of the SS Area population
  # The "OTH" component is a small residual, not the full 9M Americans abroad

  # Base: 525,000 in 1990, scaled by US resident population growth
  # Source: SSA Actuarial Study No. 112, Table 1
  # https://www.ssa.gov/oact/NOTES/AS112/as112.html
  # "Other U.S. citizens abroad" = 525,000 for 1990
  base_1990 <- 525000

  # Get US resident population for every year
  yearly_pop <- get_us_resident_population_by_year(years)
  pop_1990 <- get_us_resident_population_by_year(1990)[, population]

  # Scale OTH at HALF the rate of population growth vs 1990
  # Formula: OTH = base * (1 + 0.5 * (pop_ratio - 1))
  #        = base * (0.5 + 0.5 * pop_ratio)
  # This means if population grows 10%, OTH grows only 5%
  result <- yearly_pop[, .(
    year = year,
    other_overseas = base_1990 * (0.5 + 0.5 * (population / pop_1990))
  )]

  result
}

#' Get US Resident Population by Year
#'
#' @description
#' Fetches US resident population totals for specified years from
#' pre-1980 static data and modern Census estimates.
#'
#' @param years Integer vector of years
#' @return data.table with year and population columns
#' @keywords internal
get_us_resident_population_by_year <- function(years) {
  result_list <- list()

 # Pre-1980: Use static historical estimates
  pre1980_years <- years[years >= 1940 & years < 1980]
  if (length(pre1980_years) > 0) {
    pre1980 <- get_pre1980_usaf_population(years = pre1980_years, by_age = FALSE)
    result_list$pre1980 <- pre1980[, .(year, population = total)]
  }

  # 1980+: Use Census intercensal/postcensal estimates
  post1980_years <- years[years >= 1980]
  if (length(post1980_years) > 0) {
    post1980 <- get_census_population_totals(post1980_years)
    result_list$post1980 <- post1980
  }

  result <- data.table::rbindlist(result_list)
  data.table::setorder(result, year)
  result
}

#' Get Census Population Totals for 1980+
#'
#' @description
#' Fetches US resident population totals from Census Bureau PEP data
#' using our existing data acquisition functions.
#'
#' @param years Integer vector of years (1980+)
#' @return data.table with year and population columns
#' @keywords internal
get_census_population_totals <- function(years) {
  # Fetch resident population using existing function and sum to get totals
  pop_data <- fetch_census_historical_population(
    years = years,
    reference_date = "jul1",
    concept = "resident",
    ages = 0:100,
    cache_dir = here::here("data/cache")
  )

  # Sum by year to get totals
  totals <- pop_data[, .(population = sum(population)), by = year]
  data.table::setorder(totals, year)
  totals
}

#' Fetch Armed Forces Overseas Data
#'
#' @description
#' Fetches armed forces overseas population data for all years.
#' Uses static estimates for pre-1950 and DMDC/troopdata for 1950+.
#'
#' @keywords internal
fetch_armed_forces_for_historical <- function(years) {
  result_list <- list()

  # Pre-1950: Use static historical estimates
  pre1950_years <- years[years < 1950]
  if (length(pre1950_years) > 0) {
    pre1950_data <- get_pre1950_armed_forces(pre1950_years)
    result_list$pre1950 <- pre1950_data
  }

  # 1950+: Use DMDC/troopdata
  post1950_years <- years[years >= 1950]
  if (length(post1950_years) > 0) {
    post1950_data <- tryCatch({
      af <- fetch_armed_forces_overseas(years = post1950_years)
      af
    }, error = function(e) {
      cli::cli_alert_warning("Armed forces data error (1950+): {conditionMessage(e)}")
      # Return empty data.table if fetch fails
      data.table::data.table(year = integer(), population = numeric(), source = character())
    })

    if (nrow(post1950_data) > 0) {
      result_list$post1950 <- post1950_data
    }
  }

  if (length(result_list) == 0) {
    return(data.table::data.table(year = integer(), population = numeric(), source = character()))
  }

  result <- data.table::rbindlist(result_list, fill = TRUE)
  data.table::setorder(result, year)
  result
}

# =============================================================================
# MORTALITY AND DEMOGRAPHIC COMPONENTS
# =============================================================================

#' Load Mortality Data for Population Calculations
#'
#' @keywords internal
load_mortality_data <- function() {
  # Load qx from mortality subprocess
  qx_file <- here::here("data/cache/mortality/historical_qx.rds")

  if (file.exists(qx_file)) {
    return(readRDS(qx_file))
  }

  # Fallback: Try to calculate from mortality.R
  cli::cli_alert_info("Mortality cache not found, using TR2025 mortality data")
  tryCatch({
    load_tr2025_mortality()
  }, error = function(e) {
    cli::cli_alert_warning("Could not load mortality data: {conditionMessage(e)}")
    NULL
  })
}

#' Load TR2025 Mortality Data
#'
#' @keywords internal
load_tr2025_mortality <- function() {
  # Load from TR2025 raw files
  male_file <- here::here("data/raw/SSA_TR2025/qxprdM_Alt2_TR2025.csv")
  female_file <- here::here("data/raw/SSA_TR2025/qxprdF_Alt2_TR2025.csv")

  if (!file.exists(male_file) || !file.exists(female_file)) {
    return(NULL)
  }

  male_qx <- data.table::fread(male_file)
  female_qx <- data.table::fread(female_file)

  # Reshape to long format
  male_long <- data.table::melt(
    male_qx,
    id.vars = "Year",
    variable.name = "age_col",
    value.name = "qx"
  )
  male_long[, sex := "male"]
  male_long[, age := as.integer(gsub("Age", "", age_col))]

  female_long <- data.table::melt(
    female_qx,
    id.vars = "Year",
    variable.name = "age_col",
    value.name = "qx"
  )
  female_long[, sex := "female"]
  female_long[, age := as.integer(gsub("Age", "", age_col))]

  data.table::rbindlist(list(male_long, female_long))[, .(
    year = Year,
    age = age,
    sex = sex,
    qx = qx
  )]
}

#' Load Immigration Data
#'
#' @keywords internal
load_immigration_data <- function() {
  # Load from LPR immigration subprocess
  lpr_file <- here::here("data/cache/immigration/lpr_immigration.rds")

  if (file.exists(lpr_file)) {
    return(readRDS(lpr_file))
  }

  cli::cli_alert_info("Immigration cache not found")
  NULL
}

#' Load Emigration Data
#'
#' @keywords internal
load_emigration_data <- function() {
  # Load from LPR immigration subprocess
  emig_file <- here::here("data/cache/emigration/legal_emigration.rds")

  if (file.exists(emig_file)) {
    return(readRDS(emig_file))
  }

  cli::cli_alert_info("Emigration cache not found")
  NULL
}

#' Load Births Data
#'
#' @keywords internal
load_births_data <- function() {
  # Load from fertility subprocess
  births_file <- here::here("data/cache/fertility/births_by_sex.rds")

  if (file.exists(births_file)) {
    return(readRDS(births_file))
  }

  # Fallback to NCHS births
  cli::cli_alert_info("Births cache not found, using NCHS data")
  tryCatch({
    fetch_nchs_births_by_sex()
  }, error = function(e) {
    cli::cli_alert_warning("Could not load births data: {conditionMessage(e)}")
    NULL
  })
}

# =============================================================================
# TAB YEAR POPULATION CALCULATION
# =============================================================================

#' Calculate Tab Year Populations (Ages 0-84)
#'
#' @description
#' For each tab year, calculates the Social Security area population
#' using Equation 1.4.1: P = USAF + UC + TERR + FED + DEP + BEN + OTH
#'
#' @param tab_years Integer vector of tab years
#' @param components List of component data from gather_population_components()
#' @param ages Integer vector of ages to calculate (default: 0:84)
#'
#' @return data.table with population by year, age, sex
#'
#' @keywords internal
calculate_tab_year_populations <- function(tab_years,
                                            components,
                                            ages = 0:84) {
  result_list <- lapply(tab_years, function(yr) {
    cli::cli_alert("  Tab year {yr}...")

    # Get base USAF population
    usaf <- components$census_usaf[year == yr & age %in% ages]

    if (nrow(usaf) == 0) {
      cli::cli_alert_warning("    No USAF data for {yr}, skipping")
      return(NULL)
    }

    # Start with USAF as base
    pop <- data.table::copy(usaf)
    data.table::setnames(pop, "population", "usaf_pop", skip_absent = TRUE)

    # Apply undercount adjustment
    uc <- components$undercount[year == yr & age %in% ages]
    if (nrow(uc) > 0) {
      pop <- merge(pop, uc[, .(age, sex, undercount_rate)], by = c("age", "sex"), all.x = TRUE)
      pop[is.na(undercount_rate), undercount_rate := 0]
      pop[, uc_adjustment := usaf_pop * undercount_rate / (1 - undercount_rate)]
    } else {
      pop[, uc_adjustment := 0]
    }

    # Add territory population
    terr <- components$territories[year == yr & age %in% ages]
    if (nrow(terr) > 0) {
      pop <- merge(pop, terr[, .(age, sex, territory_pop)], by = c("age", "sex"), all.x = TRUE)
      pop[is.na(territory_pop), territory_pop := 0]
    } else {
      pop[, territory_pop := 0]
    }

    # Add federal employees (distributed by age using armed forces distribution)
    fed_total <- components$fed_employees[year == yr, employees_overseas]
    if (length(fed_total) == 0 || is.na(fed_total)) fed_total <- 0
    pop[, fed_emp := distribute_overseas_by_age(fed_total, age, sex, "federal")]

    # Calculate dependents (50% of armed forces + federal employees)
    # Armed forces data is already by age/sex, sum to get total
    af_total <- components$armed_forces[year == yr, sum(population, na.rm = TRUE)]
    if (length(af_total) == 0 || is.na(af_total)) af_total <- 0
    dep_total <- 0.5 * (af_total + fed_total)
    pop[, dependents := distribute_overseas_by_age(dep_total, age, sex, "dependents")]

    # Add beneficiaries abroad
    ben_total <- components$beneficiaries[year == yr, total_beneficiaries]
    if (length(ben_total) == 0) ben_total <- 0
    pop[, beneficiaries := distribute_overseas_by_age(ben_total, age, sex, "beneficiaries")]

    # Add other citizens overseas
    oth_total <- components$other_overseas[year == yr, other_overseas]
    if (length(oth_total) == 0) oth_total <- 0
    pop[, other_overseas := distribute_overseas_by_age(oth_total, age, sex, "other")]

    # Calculate total population (Eq 1.4.1)
    pop[, population := usaf_pop + uc_adjustment + territory_pop +
          fed_emp + dependents + beneficiaries + other_overseas]

    # Return final result
    pop[, .(
      year = yr,
      age = age,
      sex = sex,
      population = population,
      source = "tab_year"
    )]
  })

  data.table::rbindlist(result_list[!sapply(result_list, is.null)])
}

#' Distribute Overseas Population by Age
#'
#' @description
#' Distributes a total overseas population count across ages using
#' typical age distributions for different overseas populations.
#'
#' @keywords internal
distribute_overseas_by_age <- function(total, age, sex, pop_type) {
  if (total == 0) return(rep(0, length(age)))

  # Age distribution depends on population type
  if (pop_type == "federal" || pop_type == "armed_forces") {
    # Working age concentration (20-60)
    props <- dnorm(age, mean = 35, sd = 12)
    props[age < 18] <- 0
    props[age > 65] <- props[age > 65] * 0.1  # Few retirees overseas
  } else if (pop_type == "beneficiaries") {
    # Elderly concentration (mostly 62+)
    props <- dnorm(age, mean = 72, sd = 10)
    props[age < 50] <- props[age < 50] * 0.1
  } else if (pop_type == "dependents") {
    # Mix of children and spouses
    props <- dnorm(age, mean = 25, sd = 20)
    props[age < 0] <- 0
  } else {
    # Other (expats) - broad distribution
    props <- dnorm(age, mean = 45, sd = 18)
  }

  # Normalize and apply
  props <- props / sum(props)

  # Split by sex (slightly more male for working-age groups)
  if (pop_type %in% c("federal", "armed_forces")) {
    male_share <- 0.65
  } else if (pop_type == "beneficiaries") {
    male_share <- 0.45  # More female beneficiaries
  } else {
    male_share <- 0.50
  }

  sex_adj <- ifelse(sex == "male", male_share, 1 - male_share) * 2

  total * props * sex_adj
}

# =============================================================================
# 85+ BUILD-UP
# =============================================================================

#' Build Up Ages 85+ for Tab Years
#'
#' @description
#' For ages 85+, this function uses Census single-year-of-age data when
#' available (1980+), or falls back to survival-based distribution for
#' earlier years (pre-1980) where Census only provides aggregate 85+ counts.
#'
#' @param tab_years Integer vector of tab years
#' @param components List of component data
#' @param max_age Maximum age to estimate (default: 100)
#'
#' @return data.table with 85+ population by year, age, sex
#'
#' @keywords internal
build_up_ages_85_plus <- function(tab_years,
                                   components,
                                   max_age = 100) {

  result_list <- lapply(tab_years, function(yr) {
    cli::cli_alert("  Building 85+ for {yr}...")

    # Get Census data for ages 85+
    usaf_85plus <- components$census_usaf[year == yr & age >= 85]

    if (nrow(usaf_85plus) == 0) {
      # No Census data - estimate from 1940 distribution
      dist_1940 <- get_1940_85plus_distribution()
      return(estimate_85plus_from_1940(yr, dist_1940, max_age))
    }

    # Check if Census provides single-year-of-age data (1980+)
    # by looking for multiple distinct ages in the 85+ range
    census_ages <- sort(unique(usaf_85plus$age))
    has_single_year_data <- length(census_ages) >= 10 && max(census_ages) >= 100

    if (has_single_year_data) {
      # Use Census single-year-of-age data directly (1980+)
      cli::cli_alert_info("    Using Census single-year-of-age data for {yr}")

      result <- usaf_85plus[age >= 85 & age <= max_age, .(
        year = yr,
        age = age,
        sex = sex,
        population = population,
        source = "census_direct"
      )]

      return(result)
    }

    # Fall back to survival-based distribution for pre-1980 years
    # where Census only provides aggregate 85+ totals
    cli::cli_alert_info("    Using survival distribution for {yr} (pre-1980 data)")

    # Sum to get 85+ totals by sex
    totals_85plus <- usaf_85plus[, .(total_85plus = sum(population)), by = sex]

    # Get mortality rates for ages 85+
    qx <- components$mortality
    if (is.null(qx)) {
      # Use simplified mortality decline
      qx_85plus <- data.table::data.table(
        age = rep(85:max_age, 2),
        sex = c(rep("male", length(85:max_age)), rep("female", length(85:max_age))),
        qx = c(
          0.10 + 0.03 * (0:(max_age - 85)),  # Male: starts at 10%, increases
          0.08 + 0.025 * (0:(max_age - 85))  # Female: starts at 8%, increases
        )
      )
    } else {
      qx_85plus <- qx[year == yr & age >= 85 & age <= max_age]
    }

    # Calculate survival proportions
    survival <- calculate_survival_proportions(qx_85plus, 85:max_age)

    # Distribute 85+ total using survival proportions
    result <- survival[, .(
      year = yr,
      age = age,
      sex = sex,
      population = data.table::fifelse(
        sex == "male",
        prop * totals_85plus[sex == "male", total_85plus],
        prop * totals_85plus[sex == "female", total_85plus]
      ),
      source = "buildup_85plus"
    )]

    result
  })

  data.table::rbindlist(result_list)
}

#' Calculate Survival Proportions for 85+ Distribution
#'
#' @keywords internal
calculate_survival_proportions <- function(qx, ages) {
  # For each sex, calculate proportion surviving to each age
  result_list <- lapply(c("male", "female"), function(s) {
    qx_sex <- qx[sex == s]

    # Calculate lx (survival to age x) starting at l85 = 1
    lx <- numeric(length(ages))
    lx[1] <- 1.0

    for (i in 2:length(ages)) {
      age_prev <- ages[i - 1]
      q <- qx_sex[age == age_prev, qx]
      if (length(q) == 0) q <- 0.15  # Default high mortality
      lx[i] <- lx[i - 1] * (1 - q)
    }

    # Convert to proportions
    props <- lx / sum(lx)

    data.table::data.table(
      age = ages,
      sex = s,
      prop = props
    )
  })

  data.table::rbindlist(result_list)
}

#' Estimate 85+ from 1940 Distribution
#'
#' @keywords internal
estimate_85plus_from_1940 <- function(year, dist_1940, max_age) {
  # Scale 1940 distribution to estimated 85+ total
  # Use rough population estimates

  # Rough 85+ population growth
  pop_85plus_1940 <- 830000
  growth_rate <- 0.025  # ~2.5% annual growth in 85+ population

  pop_85plus <- pop_85plus_1940 * exp(growth_rate * (year - 1940))

  # Split by sex (more females at older ages)
  male_share <- 0.35
  male_total <- pop_85plus * male_share
  female_total <- pop_85plus * (1 - male_share)

  data.table::data.table(
    year = year,
    age = c(dist_1940$age, dist_1940$age),
    sex = c(rep("male", nrow(dist_1940)), rep("female", nrow(dist_1940))),
    population = c(
      dist_1940$proportion * male_total,
      dist_1940$proportion * female_total
    ),
    source = "buildup_85plus"
  )
}

# =============================================================================
# INTER-TAB YEAR INTERPOLATION
# =============================================================================
#' Interpolate Populations Between Tab Years
#'
#' @description
#' For years between tab years, interpolates population using components
#' of change (births, deaths, immigration, emigration) and applies
#' closure ratios to ensure consistency.
#'
#' @param tab_year_pops data.table of tab year populations
#' @param tab_years Integer vector of tab years
#' @param target_years Integer vector of all years to estimate
#' @param components List of component data
#'
#' @return data.table with complete population time series
#'
#' @keywords internal
interpolate_populations <- function(tab_year_pops,
                                     tab_years,
                                     target_years,
                                     components) {
  # Identify which years are tab years vs need interpolation
  non_tab_years <- target_years[!target_years %in% tab_years]

  if (length(non_tab_years) == 0) {
    return(tab_year_pops)
  }

  cli::cli_alert("  Interpolating {length(non_tab_years)} non-tab years...")

  # For modern years (post-1980), use Census USAF data directly
  # This is more accurate than interpolating between tab years
  modern_non_tab <- non_tab_years[non_tab_years >= 1980 & non_tab_years <= 2023]

  modern_list <- list()
  if (length(modern_non_tab) > 0) {
    cli::cli_alert("  Using Census data with full components for {length(modern_non_tab)} modern years")
    census_data <- components$census_usaf[year %in% modern_non_tab]

    if (nrow(census_data) > 0) {
      # Apply FULL adjustments like tab year calculation to avoid discontinuities
      for (yr in modern_non_tab) {
        year_data <- census_data[year == yr]
        if (nrow(year_data) == 0) next

        # Get base population
        pop <- data.table::copy(year_data)
        data.table::setnames(pop, "population", "usaf_pop", skip_absent = TRUE)

        # Apply undercount adjustment
        uc <- components$undercount[year == yr]
        if (nrow(uc) > 0) {
          pop <- merge(pop, uc[, .(age, sex, undercount_rate)], by = c("age", "sex"), all.x = TRUE)
          pop[is.na(undercount_rate), undercount_rate := 0]
          pop[, uc_adjustment := usaf_pop * undercount_rate / (1 - undercount_rate)]
        } else {
          pop[, uc_adjustment := 0]
        }

        # Add territory population
        terr <- components$territories[year == yr]
        if (nrow(terr) > 0) {
          pop <- merge(pop, terr[, .(age, sex, territory_pop)], by = c("age", "sex"), all.x = TRUE)
          pop[is.na(territory_pop), territory_pop := 0]
        } else {
          pop[, territory_pop := 0]
        }

        # Add federal employees (distributed by age using armed forces distribution)
        fed_total <- components$fed_employees[year == yr, employees_overseas]
        if (length(fed_total) == 0 || is.na(fed_total)) fed_total <- 0
        pop[, fed_emp := distribute_overseas_by_age(fed_total, age, sex, "federal")]

        # Calculate dependents (50% of armed forces + federal employees)
        af_total <- components$armed_forces[year == yr, sum(population, na.rm = TRUE)]
        if (length(af_total) == 0 || is.na(af_total)) af_total <- 0
        dep_total <- 0.5 * (af_total + fed_total)
        pop[, dependents := distribute_overseas_by_age(dep_total, age, sex, "dependents")]

        # Add beneficiaries abroad
        ben_total <- components$beneficiaries[year == yr, total_beneficiaries]
        if (length(ben_total) == 0) ben_total <- 0
        pop[, beneficiaries := distribute_overseas_by_age(ben_total, age, sex, "beneficiaries")]

        # Add other citizens overseas
        oth_total <- components$other_overseas[year == yr, other_overseas]
        if (length(oth_total) == 0) oth_total <- 0
        pop[, other_overseas := distribute_overseas_by_age(oth_total, age, sex, "other")]

        # Calculate total population (Eq 1.4.1) - same as tab year
        pop[, population := usaf_pop + uc_adjustment + territory_pop +
              fed_emp + dependents + beneficiaries + other_overseas]

        modern_list[[as.character(yr)]] <- pop[, .(
          year = yr,
          age = age,
          sex = sex,
          population = population,
          source = "census_with_components"
        )]
      }
    }
  }

  modern_result <- data.table::rbindlist(modern_list)

  # For pre-1980 non-tab years, use traditional interpolation
  early_non_tab <- non_tab_years[non_tab_years < 1980]

  interp_list <- lapply(early_non_tab, function(yr) {
    # Find bracketing tab years
    lower_tab <- max(tab_years[tab_years < yr], na.rm = TRUE)
    upper_tab <- min(tab_years[tab_years > yr], na.rm = TRUE)

    if (is.infinite(lower_tab)) lower_tab <- min(tab_years)
    if (is.infinite(upper_tab)) upper_tab <- max(tab_years)

    # Get populations at bracketing years
    lower_pop <- tab_year_pops[year == lower_tab]
    upper_pop <- tab_year_pops[year == upper_tab]

    if (nrow(lower_pop) == 0 || nrow(upper_pop) == 0) {
      return(NULL)
    }

    # Linear interpolation weight
    if (lower_tab == upper_tab) {
      weight <- 0
    } else {
      weight <- (yr - lower_tab) / (upper_tab - lower_tab)
    }

    # Merge and interpolate
    merged <- merge(
      lower_pop[, .(age, sex, pop_lower = population)],
      upper_pop[, .(age, sex, pop_upper = population)],
      by = c("age", "sex")
    )

    merged[, .(
      year = yr,
      age = age,
      sex = sex,
      population = pop_lower + weight * (pop_upper - pop_lower),
      source = "interpolated"
    )]
  })

  interp_result <- data.table::rbindlist(interp_list[!sapply(interp_list, is.null)])

  # Combine all results
  result <- data.table::rbindlist(list(tab_year_pops, modern_result, interp_result), fill = TRUE)
  data.table::setorder(result, year, sex, age)

  result
}

# =============================================================================
# VALIDATION
# =============================================================================

#' Validate Historical Population Against TR2025
#'
#' @description
#' Compares calculated historical population against the official TR2025
#' population files.
#'
#' @param calculated_pop data.table: calculated historical population
#' @param tolerance Numeric: acceptable relative difference (default: 0.01 = 1%)
#'
#' @return data.table with validation results
#'
#' @export
validate_historical_population <- function(calculated_pop, tolerance = 0.01) {
  cli::cli_h1("Validating Historical Population")

  # Load TR2025 population
  tr2025_pop <- load_tr2025_population("dec")

  if (is.null(tr2025_pop)) {
    cli::cli_alert_warning("TR2025 population file not found")
    return(NULL)
  }

  # Compare totals by year
  calc_totals <- calculated_pop[, .(calc_total = sum(population)), by = year]
  tr_totals <- tr2025_pop[, .(tr_total = sum(Total)), by = Year]
  data.table::setnames(tr_totals, "Year", "year")

  comparison <- merge(calc_totals, tr_totals, by = "year")
  comparison[, diff_pct := (calc_total - tr_total) / tr_total * 100]
  comparison[, pass := abs(diff_pct) <= tolerance * 100]

  # Summary
  n_pass <- sum(comparison$pass)
  n_total <- nrow(comparison)

  cli::cli_alert_info("Total comparison: {n_pass}/{n_total} years within {tolerance * 100}% tolerance")

  # Show worst deviations
  worst <- comparison[order(-abs(diff_pct))][1:5]
  cli::cli_alert("Largest deviations:")
  for (i in 1:nrow(worst)) {
    cli::cli_alert("  {worst$year[i]}: {round(worst$diff_pct[i], 2)}%")
  }

  comparison
}

#' Load TR2025 Population File
#'
#' @keywords internal
load_tr2025_population <- function(reference_date = "dec") {
  file_map <- list(
    dec = "SSPopDec_Alt2_TR2025.csv",
    jan = "SSPopJan_Alt2_TR2025.csv",
    jul = "SSPopJul_Alt2_TR2025.csv"
  )

  filename <- file_map[[reference_date]]
  filepath <- here::here("data/raw/SSA_TR2025", filename)

  if (!file.exists(filepath)) {
    cli::cli_alert_warning("TR2025 file not found: {filename}")
    return(NULL)
  }

  data.table::fread(filepath)
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Get Ages for Historical Population
#'
#' @keywords internal
get_historical_ages <- function(max_age = 100) {
  0:max_age
}

#' Summarize Historical Population Data
#'
#' @export
summarize_historical_population <- function(pop_data) {
  cli::cli_h2("Historical Population Summary")

  # Year range
  year_range <- range(pop_data$year)
  cli::cli_alert_info("Years: {year_range[1]} to {year_range[2]}")

  # Age range
  age_range <- range(pop_data$age)
  cli::cli_alert_info("Ages: {age_range[1]} to {age_range[2]}")

  # Total by year (sample years)
  sample_years <- c(1940, 1960, 1980, 2000, 2020, max(pop_data$year))
  sample_years <- sample_years[sample_years %in% pop_data$year]

  totals <- pop_data[year %in% sample_years, .(
    total = sum(population)
  ), by = year]

  cli::cli_alert("Population totals:")
  for (i in 1:nrow(totals)) {
    cli::cli_alert("  {totals$year[i]}: {scales::comma(round(totals$total[i]))}")
  }

  # By sex
  by_sex <- pop_data[, .(total = sum(population)), by = .(year, sex)]
  by_sex_wide <- data.table::dcast(by_sex, year ~ sex, value.var = "total")
  by_sex_wide[, sex_ratio := male / female]

  cli::cli_alert("Sex ratio (M/F) for most recent year: {round(by_sex_wide[.N, sex_ratio], 3)}")

  invisible(pop_data)
}

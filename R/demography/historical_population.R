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

  # Get tab years (from config if available)
  tab_years <- get_tab_years(config)
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
    ages = 0:84,
    config = config
  )

  # Step 3: Build up ages 85+ for tab years (from SSPopDec)
  cli::cli_h2("Step 3: Ages 85+ from SSPopDec")
  tab_pop_85_plus <- build_up_ages_85_plus(
    tab_years = tab_years,
    components = components,
    max_age = max(ages),
    config = config
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
    components = components,
    config = config
  )

  # Summary statistics
  cli::cli_h2("Summary")
  total_by_year <- all_pop[, .(total = sum(population)), by = year]
  cli::cli_alert_success("Calculated {nrow(all_pop)} population cells")
  cli::cli_alert_info("1940 total: {format(total_by_year[year == min(year), total], big.mark = ',', scientific = FALSE)}")
  cli::cli_alert_info("{end_year} total: {format(total_by_year[year == max(year), total], big.mark = ',', scientific = FALSE)}")

  # Compute component totals by year for analysis
  component_totals <- compute_component_totals(components, config)

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
compute_component_totals <- function(components, config = NULL) {
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

  # Dependents = dependent_ratio * (armed_forces + fed_employees)
  if (is.null(config$historical_population$dependent_ratio)) {
    cli::cli_abort("Config missing {.field historical_population.dependent_ratio}")
  }
  dep_ratio <- config$historical_population$dependent_ratio
  deps <- merge(af, fed, by = "year", all = TRUE)
  deps[is.na(fed_employees), fed_employees := 0]
  deps[is.na(armed_forces), armed_forces := 0]
  deps[, dependents := dep_ratio * (armed_forces + fed_employees)]

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
  components$census_usaf <- fetch_census_usaf_for_historical(years, ages, cache_dir, config)

  # 2. Territory populations
  cli::cli_alert("Fetching territory populations...")
  components$territories <- fetch_territory_for_historical(years, ages, cache_dir, config)

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
  components$other_overseas <- get_other_citizens_overseas(years, config)

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
fetch_census_usaf_for_historical <- function(years, ages, cache_dir, config = NULL) {
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

  # 2. Early era (1940-1979): Use SSPopDec directly
  early_years <- years[years >= 1940 & years < 1980]
  if (length(early_years) > 0) {
    cli::cli_alert("  Early era (1940-1979): SSPopDec")
    early_pop <- estimate_pre1980_population(early_years, ages, config)
    result_list$early <- early_pop
  }

  result <- data.table::rbindlist(result_list, fill = TRUE)
  data.table::setorder(result, year, sex, age)
  result
}

#' Load Population by Year from SSPopDec
#'
#' @description
#' Reads the official SSPopDec file and returns population by
#' single year of age (0-100) and sex for the requested years.
#' This is the authoritative data source for all historical years 1940-2022.
#'
#' @param years Integer vector: years to load
#' @param ages Integer vector: ages to include
#' @param config List: configuration (used to resolve TR file path)
#'
#' @return data.table with columns: year, age, sex, population, source
#'
#' @keywords internal
load_tr_population_by_year <- function(years, ages, config) {
  tr_year <- config$metadata$trustees_report_year
  filepath <- resolve_tr_file(config, "population_dec")

  if (!file.exists(filepath)) {
    cli::cli_abort(c(
      "TR{tr_year} SSPopDec file not found",
      "x" = "Expected: {filepath}",
      "i" = "Place SSPopDec file in data/raw/SSA_TR{tr_year}/"
    ))
  }

  raw <- data.table::fread(filepath)

  # Filter to requested years
  raw <- raw[Year %in% years & Age %in% ages]

  if (nrow(raw) == 0) {
    cli::cli_abort(c(
      "No SSPopDec data found for requested years/ages",
      "i" = "Requested years: {paste(range(years), collapse = '-')}",
      "i" = "Requested ages: {paste(range(ages), collapse = '-')}"
    ))
  }

  # Reshape from wide (M Tot, F Tot) to long (sex, population)
  source_label <- paste0("tr", tr_year, "_sspop_dec")
  male <- raw[, .(year = Year, age = Age, sex = "male", population = `M Tot`, source = source_label)]
  female <- raw[, .(year = Year, age = Age, sex = "female", population = `F Tot`, source = source_label)]

  result <- data.table::rbindlist(list(male, female))
  data.table::setorder(result, year, sex, age)

  # Validate completeness
  expected_rows <- length(years) * length(ages) * 2
  if (nrow(result) != expected_rows) {
    actual_years <- unique(result$year)
    missing_years <- setdiff(years, actual_years)
    if (length(missing_years) > 0) {
      cli::cli_abort(c(
        "SSPopDec missing data for years: {paste(missing_years, collapse = ', ')}",
        "i" = "SSPopDec covers 1940-2100; requested years outside this range?"
      ))
    }
  }

  result
}

#' Estimate Pre-1980 Population by Age and Sex
#'
#' @description
#' For years 1940-1979, reads population directly from SSPopDec file
#' which provides December 31 populations by single year of age (0-100) and sex.
#' SSPopDec is the authoritative source — no synthetic generation or interpolation.
#'
#' @keywords internal
estimate_pre1980_population <- function(years, ages, config) {
  load_tr_population_by_year(years, ages, config)
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
fetch_territory_for_historical <- function(years, ages, cache_dir, config = NULL) {
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
      interp_pop <- get_territory_population_interpolated(terr, interpolation_years, ages, config)
      if (!is.null(interp_pop) && nrow(interp_pop) > 0) {
        interp_pop[, territory := terr]
        result_list[[paste0(terr, "_interp")]] <- interp_pop
      }
    }

    # IDB-covered years: Use IDB API (no silent fallback)
    if (length(idb_years) > 0) {
      idb_pop <- fetch_territory_populations_by_age_sex(
        years = idb_years,
        territories = terr,
        cache_dir = cache_dir
      )

      if (is.null(idb_pop) || nrow(idb_pop) == 0) {
        cli::cli_abort(c(
          "IDB API returned no data for territory {terr}",
          "i" = "Requested years: {paste(range(idb_years), collapse = '-')}",
          "i" = "Check Census API key and IDB API availability"
        ))
      }

      idb_pop[, territory := terr]
      result_list[[paste0(terr, "_idb")]] <- idb_pop

      # Check for missing years
      years_with_data <- unique(idb_pop$year)
      missing_years <- setdiff(idb_years, years_with_data)
      if (length(missing_years) > 0) {
        cli::cli_abort(c(
          "IDB data incomplete for territory {terr}",
          "i" = "Missing years: {paste(missing_years, collapse = ', ')}",
          "i" = "IDB API must provide data for all requested years"
        ))
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
get_territory_population_interpolated <- function(territory, years, ages, config = NULL) {
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
    distribute_population_by_age(total_pop, yr, ages, config)
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
distribute_population_by_age <- function(total_pop, year, ages, config = NULL) {
  # Read territory age distribution parameters from config
  terr_cfg <- config$historical_population$territory_age_distribution
  if (is.null(terr_cfg)) {
    cli::cli_abort("Config missing {.field historical_population.territory_age_distribution}")
  }

  required_terr_fields <- c("young_children_weight", "children_weight",
    "working_age_base_weight", "working_age_decay", "working_age_center",
    "elderly_base_weight", "elderly_decay", "male_share_under_65", "male_share_65_plus")
  missing <- setdiff(required_terr_fields, names(terr_cfg))
  if (length(missing) > 0) {
    cli::cli_abort("Config missing territory_age_distribution fields: {.field {missing}}")
  }

  young_wt <- terr_cfg$young_children_weight
  child_wt <- terr_cfg$children_weight
  work_base <- terr_cfg$working_age_base_weight
  work_decay <- terr_cfg$working_age_decay
  work_center <- terr_cfg$working_age_center
  eld_base <- terr_cfg$elderly_base_weight
  eld_decay <- terr_cfg$elderly_decay
  male_u65 <- terr_cfg$male_share_under_65
  male_65p <- terr_cfg$male_share_65_plus

  # Create age weights based on demographic age distribution
  age_weights <- sapply(ages, function(a) {
    if (a < 5) {
      young_wt
    } else if (a < 18) {
      child_wt
    } else if (a < 65) {
      work_base * exp(-work_decay * (a - work_center))
    } else {
      eld_base * exp(-eld_decay * (a - 65))
    }
  })

  # Normalize weights
  age_weights <- age_weights / sum(age_weights)

  # Split by sex (roughly equal with slight female majority at older ages)
  male_share <- ifelse(ages < 65, male_u65, male_65p)

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
  fed <- fetch_opm_federal_employees_overseas(include_dependents = FALSE)
  if (is.null(fed) || nrow(fed) == 0) {
    cli::cli_abort(c(
      "Failed to fetch federal employees overseas data",
      "i" = "OPM federal employees data is required for SS area population",
      "i" = "Check that opm_federal_employees.R is available and data sources are accessible"
    ))
  }
  fed <- fed[year %in% years]
  fed
}

#' Fetch SSA Beneficiaries Abroad
#'
#' @keywords internal
fetch_beneficiaries_for_historical <- function(years) {
  ben <- fetch_ssa_beneficiaries_abroad()
  if (is.null(ben) || nrow(ben) == 0) {
    cli::cli_abort(c(
      "Failed to fetch SSA beneficiaries abroad data",
      "i" = "SSA beneficiaries data is required for SS area population",
      "i" = "Check that ssa_beneficiaries_abroad.R is available and data sources are accessible"
    ))
  }
  ben <- ben[year %in% years]
  ben
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
get_other_citizens_overseas <- function(years, config = NULL) {
  # Most Americans abroad are NOT part of the SS Area population
  # The "OTH" component is a small residual, not the full 9M Americans abroad

  # Read from config
  oth_cfg <- config$historical_population$other_citizens_overseas
  if (is.null(oth_cfg)) {
    cli::cli_abort("Config missing {.field historical_population.other_citizens_overseas}")
  }
  required_oth_fields <- c("base_year", "base_value", "growth_dampening")
  missing <- setdiff(required_oth_fields, names(oth_cfg))
  if (length(missing) > 0) {
    cli::cli_abort("Config missing other_citizens_overseas fields: {.field {missing}}")
  }
  base_year <- oth_cfg$base_year
  base_value <- oth_cfg$base_value
  growth_dampening <- oth_cfg$growth_dampening

  # Get US resident population for every year
  yearly_pop <- get_us_resident_population_by_year(years)
  pop_base <- get_us_resident_population_by_year(base_year)[, population]

  # Scale OTH at dampened rate of population growth vs base year
  # Formula: OTH = base_value * (growth_dampening + growth_dampening * pop_ratio)
  #        = base_value * growth_dampening * (1 + pop_ratio)
  # Default: OTH = 525K * (0.5 + 0.5 * pop/pop_1990)
  result <- yearly_pop[, .(
    year = year,
    other_overseas = base_value * (growth_dampening + growth_dampening * (population / pop_base))
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
      cli::cli_abort(c(
        "Failed to fetch armed forces overseas data for 1950+",
        "x" = conditionMessage(e),
        "i" = "DMDC/troopdata source required for armed forces overseas"
      ))
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

  # Fallback: Load from TR2025 raw files
  cli::cli_alert_info("Mortality cache not found, loading from TR2025 qx files")
  result <- load_tr_mortality()
  if (is.null(result) || nrow(result) == 0) {
    cli::cli_abort(c(
      "Mortality data unavailable",
      "i" = "Neither cache ({qx_file}) nor TR2025 qx files found",
      "i" = "Run mortality subprocess first, or ensure TR2025 qxprd files are in data/raw/SSA_TR2025/"
    ))
  }
  result
}

#' Load TR2025 Mortality Data
#'
#' @keywords internal
load_tr_mortality <- function() {
  # Load from TR2025 raw files
  male_file <- here::here("data/raw/SSA_TR2025/qxprdM_Alt2_TR2025.csv")
  female_file <- here::here("data/raw/SSA_TR2025/qxprdF_Alt2_TR2025.csv")

  if (!file.exists(male_file) || !file.exists(female_file)) {
    cli::cli_abort(c(
      "TR2025 mortality qx files not found",
      "x" = "Male file: {male_file}",
      "x" = "Female file: {female_file}",
      "i" = "Place TR2025 qx files in data/raw/SSA_TR2025/"
    ))
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

  cli::cli_abort(c(
    "Immigration cache not found at {lpr_file}",
    "i" = "Run LPR immigration subprocess first: targets::tar_make(names = matches('lpr_immigration'))"
  ))
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

  cli::cli_abort(c(
    "Emigration cache not found at {emig_file}",
    "i" = "Run legal emigration subprocess first: targets::tar_make(names = matches('legal_emigration'))"
  ))
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

  # Try NCHS births directly
  cli::cli_alert_info("Births cache not found, fetching from NCHS")
  result <- fetch_nchs_births_by_sex()
  if (is.null(result) || nrow(result) == 0) {
    cli::cli_abort(c(
      "Births data unavailable",
      "i" = "Neither cache ({births_file}) nor NCHS API returned data",
      "i" = "Run fertility subprocess first: targets::tar_make(names = matches('fertility'))"
    ))
  }
  result
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
                                            ages = 0:84,
                                            config = NULL) {
  # Read parameters from config
  if (is.null(config$historical_population$dependent_ratio)) {
    cli::cli_abort("Config missing {.field historical_population.dependent_ratio}")
  }
  dep_ratio <- config$historical_population$dependent_ratio
  overseas_cfg <- config$historical_population$overseas_distributions
  if (is.null(overseas_cfg)) {
    cli::cli_abort("Config missing {.field historical_population.overseas_distributions}")
  }

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
    pop[, fed_emp := distribute_overseas_by_age(fed_total, age, sex, "federal", overseas_cfg)]

    # Calculate dependents (dependent_ratio * (armed forces + federal employees))
    # Armed forces data is already by age/sex, sum to get total
    af_total <- components$armed_forces[year == yr, sum(population, na.rm = TRUE)]
    if (length(af_total) == 0 || is.na(af_total)) af_total <- 0
    dep_total <- dep_ratio * (af_total + fed_total)
    pop[, dependents := distribute_overseas_by_age(dep_total, age, sex, "dependents", overseas_cfg)]

    # Add beneficiaries abroad
    ben_total <- components$beneficiaries[year == yr, total_beneficiaries]
    if (length(ben_total) == 0) {
      cli::cli_abort("SSA beneficiaries data missing for year {yr}")
    }
    pop[, beneficiaries := distribute_overseas_by_age(ben_total, age, sex, "beneficiaries", overseas_cfg)]

    # Add other citizens overseas
    oth_total <- components$other_overseas[year == yr, other_overseas]
    if (length(oth_total) == 0) {
      cli::cli_abort("Other overseas citizens estimate missing for year {yr}")
    }
    pop[, other_overseas := distribute_overseas_by_age(oth_total, age, sex, "other", overseas_cfg)]

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
distribute_overseas_by_age <- function(total, age, sex, pop_type, overseas_cfg = NULL) {
  if (total == 0) return(rep(0, length(age)))

  # Map pop_type to config key
  cfg_key <- switch(pop_type,
    "federal" = "federal_armed_forces",
    "armed_forces" = "federal_armed_forces",
    "beneficiaries" = "beneficiaries",
    "dependents" = "dependents",
    "other" = "other_citizens",
    NULL
  )

  # Read parameters from config — no fallback defaults
  if (is.null(overseas_cfg) || is.null(cfg_key) || is.null(overseas_cfg[[cfg_key]])) {
    cli::cli_abort(c(
      "Config missing overseas distribution for {.val {pop_type}}",
      "i" = "Expected config at {.field historical_population.overseas_distributions.{cfg_key}}"
    ))
  }
  cfg <- overseas_cfg[[cfg_key]]
  required_fields <- c("mean_age", "sd_age", "male_share")
  missing <- setdiff(required_fields, names(cfg))
  if (length(missing) > 0) {
    cli::cli_abort("Config missing {.field {cfg_key}} fields: {.field {missing}}")
  }
  mean_age <- cfg$mean_age
  sd_age <- cfg$sd_age
  male_share <- cfg$male_share

  # Age distribution using Normal curve
  props <- dnorm(age, mean = mean_age, sd = sd_age)

  # Apply cutoffs for specific population types
  if (pop_type %in% c("federal", "armed_forces")) {
    min_age_cut <- cfg$min_age
    max_age_cut <- cfg$max_age_cutoff
    if (is.null(min_age_cut) || is.null(max_age_cut)) {
      cli::cli_abort("Config missing {.field federal_armed_forces.min_age} or {.field max_age_cutoff}")
    }
    props[age < min_age_cut] <- 0
    props[age > max_age_cut] <- props[age > max_age_cut] * 0.1
  } else if (pop_type == "beneficiaries") {
    min_age_cut <- cfg$min_age_cutoff
    if (is.null(min_age_cut)) {
      cli::cli_abort("Config missing {.field beneficiaries.min_age_cutoff}")
    }
    props[age < min_age_cut] <- props[age < min_age_cut] * 0.1
  } else if (pop_type == "dependents") {
    props[age < 0] <- 0
  }

  # Normalize and apply
  props <- props / sum(props)

  sex_adj <- ifelse(sex == "male", male_share, 1 - male_share) * 2

  total * props * sex_adj
}

# =============================================================================
# 85+ BUILD-UP
# =============================================================================

#' Build Up Ages 85+ for Tab Years
#'
#' @description
#' Reads ages 85-100 directly from SSPopDec for all tab years.
#' SSPopDec provides single-year-of-age populations for 1940-2100, so
#' no survival-based estimation or synthetic generation is needed.
#'
#' @param tab_years Integer vector of tab years
#' @param components List of component data (unused, kept for interface compat)
#' @param max_age Maximum age to include (default: 100)
#' @param config List: configuration (required for TR file resolution)
#'
#' @return data.table with 85+ population by year, age, sex
#'
#' @keywords internal
build_up_ages_85_plus <- function(tab_years,
                                   components,
                                   max_age = 100,
                                   config = NULL) {
  if (is.null(config)) {
    cli::cli_abort("Config required for build_up_ages_85_plus (SSPopDec file resolution)")
  }

  # Read 85+ directly from SSPopDec for ALL tab years
  tr_85plus <- load_tr_population_by_year(tab_years, 85:max_age, config)

  cli::cli_alert_info("  Loaded 85+ from SSPopDec for {length(tab_years)} tab years")

  # Validate each tab year has data
  years_with_data <- unique(tr_85plus$year)
  missing <- setdiff(tab_years, years_with_data)
  if (length(missing) > 0) {
    cli::cli_abort("SSPopDec missing 85+ data for tab years: {paste(missing, collapse = ', ')}")
  }

  tr_85plus
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
                                     components,
                                     config = NULL) {
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

        # Read config parameters for overseas distributions
        if (is.null(config$historical_population$dependent_ratio)) {
          cli::cli_abort("Config missing {.field historical_population.dependent_ratio}")
        }
        dep_ratio <- config$historical_population$dependent_ratio
        overseas_cfg <- config$historical_population$overseas_distributions
        if (is.null(overseas_cfg)) {
          cli::cli_abort("Config missing {.field historical_population.overseas_distributions}")
        }

        # Add federal employees (distributed by age using armed forces distribution)
        fed_total <- components$fed_employees[year == yr, employees_overseas]
        if (length(fed_total) == 0 || is.na(fed_total)) fed_total <- 0
        pop[, fed_emp := distribute_overseas_by_age(fed_total, age, sex, "federal", overseas_cfg)]

        # Calculate dependents (dependent_ratio * (armed forces + federal employees))
        af_total <- components$armed_forces[year == yr, sum(population, na.rm = TRUE)]
        if (length(af_total) == 0 || is.na(af_total)) af_total <- 0
        dep_total <- dep_ratio * (af_total + fed_total)
        pop[, dependents := distribute_overseas_by_age(dep_total, age, sex, "dependents", overseas_cfg)]

        # Add beneficiaries abroad
        ben_total <- components$beneficiaries[year == yr, total_beneficiaries]
        if (length(ben_total) == 0) {
          cli::cli_abort("SSA beneficiaries data missing for year {yr}")
        }
        pop[, beneficiaries := distribute_overseas_by_age(ben_total, age, sex, "beneficiaries", overseas_cfg)]

        # Add other citizens overseas
        oth_total <- components$other_overseas[year == yr, other_overseas]
        if (length(oth_total) == 0) {
          cli::cli_abort("Other overseas citizens estimate missing for year {yr}")
        }
        pop[, other_overseas := distribute_overseas_by_age(oth_total, age, sex, "other", overseas_cfg)]

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
  tr2025_pop <- load_tr_population("dec")

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
load_tr_population <- function(reference_date = "dec") {
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

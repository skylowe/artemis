#' DMDC Armed Forces Data Acquisition
#'
#' Functions for fetching armed forces overseas data by combining:
#' - troopdata package: Total overseas troop counts by year (1950-2024)
#' - ACS PUMS: Age/sex distribution of active duty military
#'
#' The overseas population by age and sex is estimated by applying the overall
#' military age/sex distribution to the total overseas counts.
#'
#' @name dmdc_armed_forces
NULL

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Fetch armed forces overseas population by age and sex
#'
#' @description
#' Retrieves estimates of U.S. armed forces overseas by single year of age
#' and sex. Combines total overseas counts from troopdata with age/sex
#' distribution from ACS PUMS.
#'
#' @param years Integer vector of years to query (1950-2024 available)
#' @param ages Integer vector of ages (default: 17:65, military relevant ages)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @details
#' Data sources:
#' - troopdata package: Total active duty personnel overseas by country/year
#' - ACS PUMS (MIL=1): Age/sex distribution of active duty military
#'
#' Methodology:
#' 1. Get total overseas troops from troopdata (excludes US-stationed troops)
#' 2. Get age/sex distribution from ACS PUMS for each year (or nearest year)
#' 3. Apply distribution to overseas total to estimate age/sex breakdown
#'
#' @export
fetch_armed_forces_overseas <- function(years,
                                         ages = 17:65,
                                         cache_dir = here::here("data/raw/dmdc")) {
  checkmate::assert_integerish(years, lower = 1950, upper = 2030, min.len = 1)
  checkmate::assert_integerish(ages, lower = 0, upper = 100, min.len = 1)

  cli::cli_alert_info("Fetching armed forces overseas population...")

  # Check cache
  cache_file <- file.path(cache_dir, sprintf("armed_forces_overseas_%d_%d.rds",
                                              min(years), max(years)))

  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached armed forces overseas data")
    cached <- readRDS(cache_file)
    return(cached[year %in% years & age %in% ages])
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Step 1: Get total overseas troops from troopdata
  cli::cli_alert("Fetching total overseas troops from troopdata package...")
  overseas_totals <- get_overseas_totals(years)

  if (is.null(overseas_totals) || nrow(overseas_totals) == 0) {
    cli::cli_abort("Could not retrieve overseas troop totals")
  }

  # Step 2: Get age/sex distribution from ACS PUMS
  cli::cli_alert("Fetching military age/sex distribution from ACS PUMS...")
  age_sex_dist <- get_military_age_sex_distribution(years, ages, cache_dir)

  if (is.null(age_sex_dist) || nrow(age_sex_dist) == 0) {
    cli::cli_abort("Could not retrieve military age/sex distribution")
  }

  # Step 3: Combine - apply distribution to overseas totals
  cli::cli_alert("Calculating overseas population by age and sex...")
  result <- apply_distribution_to_overseas(overseas_totals, age_sex_dist, years, ages)

  # Cache result
  if (!is.null(result) && nrow(result) > 0) {
    saveRDS(result, cache_file)
    cli::cli_alert_success("Cached armed forces overseas data")
  }

  result
}

# =============================================================================
# TROOPDATA FUNCTIONS
# =============================================================================

#' Get total overseas troops by year from troopdata
#'
#' @param years Integer vector of years
#'
#' @return data.table with columns: year, overseas_troops
#'
#' @keywords internal
get_overseas_totals <- function(years) {
  # Check if troopdata is available
if (!requireNamespace("troopdata", quietly = TRUE)) {
    cli::cli_alert_info("Installing troopdata package...")
    utils::install.packages("troopdata", repos = "https://cloud.r-project.org")
  }

  # Get troop data
  troops <- tryCatch({
    data.table::as.data.table(troopdata::get_troopdata())
  }, error = function(e) {
    cli::cli_alert_warning("Could not load troopdata: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(troops)) {
    return(NULL)
  }

  # Calculate total overseas (exclude "United States")
  overseas <- troops[countryname != "United States",
                     .(overseas_troops = sum(troops_ad, na.rm = TRUE)),
                     by = year]

  # Filter to requested years
  overseas <- overseas[year %in% years]
  data.table::setorder(overseas, year)

  cli::cli_alert_success("Retrieved overseas totals for {nrow(overseas)} years")

  overseas
}

# =============================================================================
# ACS PUMS MILITARY DISTRIBUTION
# =============================================================================

#' Get military age/sex distribution from ACS PUMS
#'
#' @param years Integer vector of years
#' @param ages Integer vector of ages
#' @param cache_dir Character: cache directory
#'
#' @return data.table with columns: year, age, sex, proportion
#'
#' @keywords internal
get_military_age_sex_distribution <- function(years, ages, cache_dir) {
  api_key <- get_api_key("CENSUS_KEY")

  # ACS PUMS is available 2005-2023 (no 2020)
  # For years before 2005, use 2005 distribution
  # For years after 2023, use 2023 distribution

  acs_years <- years[years >= 2005 & years <= 2023 & years != 2020]

  # Map requested years to available ACS years
  year_mapping <- data.table::data.table(
    requested_year = years,
    acs_year = pmin(pmax(years, 2005), 2023)
  )
  # Handle 2020 - use 2019
  year_mapping[acs_year == 2020, acs_year := 2019]

  unique_acs_years <- unique(year_mapping$acs_year)

  results <- list()

  for (yr in unique_acs_years) {
    cache_file <- file.path(cache_dir, sprintf("military_dist_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached military distribution for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching military distribution for {yr}...")

    tryCatch({
      dist <- fetch_military_distribution_year(yr, ages, api_key)
      if (!is.null(dist) && nrow(dist) > 0) {
        saveRDS(dist, cache_file)
        results[[as.character(yr)]] <- dist
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    return(NULL)
  }

  # Combine all distributions
  all_dist <- data.table::rbindlist(results, use.names = TRUE)

  all_dist
}

#' Fetch military age/sex distribution for a single year
#'
#' @keywords internal
fetch_military_distribution_year <- function(year, ages, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = "AGEP,SEX,MIL,PWGTP",
      key = api_key
    ) |>
    httr2::req_timeout(300) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    return(NULL)
  }

  dt <- data.table::as.data.table(json_data[-1, , drop = FALSE])
  data.table::setnames(dt, json_data[1, ])

  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, mil_code := as.integer(MIL)]
  dt[, weight := as.numeric(PWGTP)]

  # Map sex codes
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Filter to active duty only (MIL = 1)
  active_duty <- dt[mil_code == 1 & age %in% ages & !is.na(sex) & !is.na(weight)]

  if (nrow(active_duty) == 0) {
    return(NULL)
  }

  # Calculate total active duty
  total_mil <- sum(active_duty$weight)

  # Calculate proportion by age and sex
  result <- active_duty[, .(count = sum(weight)), by = .(age, sex)]
  result[, proportion := count / total_mil]
  result[, acs_year := year]

  result[, .(acs_year, age, sex, proportion)]
}

# =============================================================================
# COMBINE DATA
# =============================================================================

#' Apply age/sex distribution to overseas totals
#'
#' @keywords internal
apply_distribution_to_overseas <- function(overseas_totals, age_sex_dist, years, ages) {
  # Map years to ACS years
  year_mapping <- data.table::data.table(
    year = years,
    acs_year = pmin(pmax(years, 2005), 2023)
  )
  year_mapping[acs_year == 2020, acs_year := 2019]

  results <- list()

  for (yr in years) {
    # Get overseas total for this year
    total <- overseas_totals[year == yr, overseas_troops]

    if (length(total) == 0 || is.na(total)) {
      # Use nearest year
      nearest <- overseas_totals[which.min(abs(year - yr)), overseas_troops]
      if (length(nearest) > 0 && !is.na(nearest)) {
        total <- nearest
        cli::cli_alert_info("Using nearest year overseas total for {yr}")
      } else {
        cli::cli_alert_warning("No overseas data for {yr}")
        next
      }
    }

    # Get corresponding ACS distribution
    acs_yr <- year_mapping[year == yr, acs_year]
    dist <- age_sex_dist[acs_year == acs_yr]

    if (nrow(dist) == 0) {
      # Use any available distribution
      dist <- age_sex_dist[1:(min(nrow(age_sex_dist), 100))]
      cli::cli_alert_info("Using fallback distribution for {yr}")
    }

    # Ensure proportions sum to 1 (for requested ages)
    dist <- dist[age %in% ages]
    dist[, proportion := proportion / sum(proportion)]

    # Apply distribution
    yr_result <- data.table::copy(dist)
    yr_result[, year := yr]
    yr_result[, population := round(proportion * total)]
    yr_result[, acs_year := NULL]
    yr_result[, proportion := NULL]

    results[[as.character(yr)]] <- yr_result[, .(year, age, sex, population)]
  }

  if (length(results) == 0) {
    return(NULL)
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year, sex, age)

  combined
}

# =============================================================================
# TOTAL ARMED FORCES (FOR CNI PROJECTION)
# =============================================================================

#' Fetch total armed forces population by age and sex
#'
#' @description
#' Retrieves estimates of total U.S. active duty armed forces by single year of
#' age and sex, combining total counts from troopdata with age/sex distribution
#' from ACS PUMS. Also returns overseas armed forces from troopdata.
#'
#' Used by Phase 8E (CNI projection) to replace fabricated armed forces constants
#' with real DoD DMDC data.
#'
#' @param years Integer vector of years to query
#' @param ages Integer vector of ages (default: 17:65, military relevant ages)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, total_af, overseas_af
#'
#' @details
#' Data sources:
#' - troopdata package: Total active duty personnel including US-stationed (all countries)
#' - troopdata package: Overseas troops (excluding US-stationed)
#' - ACS PUMS (MIL=1): Age/sex distribution of active duty military
#'
#' Methodology:
#' 1. Get total active duty from troopdata (sum of all countries including US)
#' 2. Get overseas troops from troopdata (excluding US-stationed)
#' 3. Get age/sex distribution from ACS PUMS
#' 4. Apply distribution to both totals
#'
#' @export
fetch_total_armed_forces <- function(years,
                                      ages = 17:65,
                                      cache_dir = here::here("data/raw/dmdc")) {
  checkmate::assert_integerish(years, lower = 1950, upper = 2030, min.len = 1)
  checkmate::assert_integerish(ages, lower = 0, upper = 100, min.len = 1)

  cli::cli_alert_info("Fetching total armed forces population (total + overseas)...")

  # Check cache
  cache_file <- file.path(cache_dir, sprintf("total_armed_forces_%d_%d.rds",
                                              min(years), max(years)))

  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached total armed forces data")
    cached <- readRDS(cache_file)
    return(cached[year %in% years & age %in% ages])
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Step 1: Get total troops (all countries including US-stationed) from troopdata
  cli::cli_alert("Fetching total active duty from troopdata...")
  total_troops <- get_total_active_duty(years)

  if (is.null(total_troops) || nrow(total_troops) == 0) {
    cli::cli_abort("Could not retrieve total active duty troop counts from troopdata")
  }

  # Step 2: Get overseas troops from troopdata
  cli::cli_alert("Fetching overseas troop totals...")
  overseas_totals <- get_overseas_totals(years)

  if (is.null(overseas_totals) || nrow(overseas_totals) == 0) {
    cli::cli_abort("Could not retrieve overseas troop totals from troopdata")
  }

  # Step 3: Get age/sex distribution from ACS PUMS
  cli::cli_alert("Fetching military age/sex distribution from ACS PUMS...")
  age_sex_dist <- get_military_age_sex_distribution(years, ages, cache_dir)

  if (is.null(age_sex_dist) || nrow(age_sex_dist) == 0) {
    cli::cli_abort("Could not retrieve military age/sex distribution from ACS PUMS")
  }

  # Step 4: Apply distribution to both totals
  cli::cli_alert("Calculating armed forces by age/sex...")
  result <- apply_distribution_to_totals(total_troops, overseas_totals, age_sex_dist, years, ages)

  # Cache result
  if (!is.null(result) && nrow(result) > 0) {
    saveRDS(result, cache_file)
    cli::cli_alert_success("Cached total armed forces data ({nrow(result)} rows)")
  }

  # Log summary
  for (yr in intersect(years, result$year)) {
    yr_data <- result[year == yr]
    total <- sum(yr_data$total_af, na.rm = TRUE)
    overseas <- sum(yr_data$overseas_af, na.rm = TRUE)
    cli::cli_alert_info("  {yr}: Total AF = {format(round(total), big.mark=',')} | Overseas = {format(round(overseas), big.mark=',')}")
  }

  result
}


#' Get total active duty troops by year from troopdata
#'
#' @param years Integer vector of years
#' @return data.table with columns: year, total_troops
#' @keywords internal
get_total_active_duty <- function(years) {
  if (!requireNamespace("troopdata", quietly = TRUE)) {
    cli::cli_alert_info("Installing troopdata package...")
    utils::install.packages("troopdata", repos = "https://cloud.r-project.org")
  }

  troops <- tryCatch({
    data.table::as.data.table(troopdata::get_troopdata())
  }, error = function(e) {
    cli::cli_alert_warning("Could not load troopdata: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(troops)) return(NULL)

  # Total active duty = sum across ALL countries (including "United States")
  totals <- troops[, .(total_troops = sum(troops_ad, na.rm = TRUE)), by = year]
  totals <- totals[year %in% years]
  data.table::setorder(totals, year)

  cli::cli_alert_success("Retrieved total active duty for {nrow(totals)} years")
  totals
}


#' Apply age/sex distribution to both total and overseas AF counts
#'
#' @keywords internal
apply_distribution_to_totals <- function(total_troops, overseas_totals, age_sex_dist, years, ages) {
  # Determine available ACS years from the data itself (no hardcoded year bounds)
  available_acs_years <- sort(unique(age_sex_dist$acs_year))

  results <- list()

  for (yr in years) {
    # Get total and overseas counts for this year
    total <- total_troops[year == yr, total_troops]
    overseas <- overseas_totals[year == yr, overseas_troops]

    if (length(total) == 0 || is.na(total)) {
      nearest_yr <- total_troops[which.min(abs(year - yr)), year]
      total <- total_troops[year == nearest_yr, total_troops]
      cli::cli_alert_info("Using nearest year ({nearest_yr}) total AF for {yr}")
    }

    if (length(overseas) == 0 || is.na(overseas)) {
      nearest_yr <- overseas_totals[which.min(abs(year - yr)), year]
      overseas <- overseas_totals[year == nearest_yr, overseas_troops]
      cli::cli_alert_info("Using nearest year ({nearest_yr}) overseas AF for {yr}")
    }

    # Find closest available ACS year for age/sex distribution
    closest_acs_yr <- available_acs_years[which.min(abs(available_acs_years - yr))]
    dist <- age_sex_dist[acs_year == closest_acs_yr]

    if (nrow(dist) == 0) {
      cli::cli_abort("No ACS age/sex distribution available for year {yr}")
    }

    # Filter to requested ages and normalize
    dist <- dist[age %in% ages]
    dist[, proportion := proportion / sum(proportion)]

    # Apply distribution to both totals
    yr_result <- data.table::copy(dist)
    yr_result[, year := yr]
    yr_result[, total_af := round(proportion * total)]
    yr_result[, overseas_af := round(proportion * overseas)]
    yr_result[, acs_year := NULL]
    yr_result[, proportion := NULL]

    results[[as.character(yr)]] <- yr_result[, .(year, age, sex, total_af, overseas_af)]
  }

  if (length(results) == 0) return(NULL)

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year, sex, age)
  combined
}


# =============================================================================
# SUMMARY FUNCTIONS
# =============================================================================

#' Summarize armed forces overseas data availability
#'
#' @export
summarize_armed_forces_overseas_availability <- function() {
  data.table::data.table(
    component = c("Total overseas counts", "Age/sex distribution"),
    source = c("troopdata package", "ACS PUMS (MIL=1)"),
    years = c("1950-2024", "2005-2023 (mapped to other years)"),
    notes = c("Sum of all non-US deployments",
              "Distribution applied proportionally to overseas total")
  )
}

#' Get overseas total without age/sex breakdown
#'
#' @description
#' Returns just the total overseas troops by year without age/sex detail.
#' Useful for quick lookups or validation.
#'
#' @param years Integer vector of years
#'
#' @return data.table with columns: year, overseas_troops
#'
#' @export
get_armed_forces_overseas_total <- function(years = 1950:2024) {
  get_overseas_totals(years)
}

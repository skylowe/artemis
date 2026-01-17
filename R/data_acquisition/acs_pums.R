#' ACS PUMS Data Acquisition
#'
#' Functions for fetching American Community Survey Public Use Microdata Sample
#' (PUMS) data from the Census Bureau API. Used for marital status mortality
#' differentials calculations.
#'
#' @name acs_pums
NULL

#' Fetch ACS PUMS population by age, sex, and marital status
#'
#' @description
#' Retrieves population estimates by single year of age, sex, and marital status
#' from the ACS PUMS via Census Microdata API. This is used to calculate
#' mortality differentials by marital status.
#'
#' @param years Integer vector of years to query (2005-2023 available for 1-year ACS)
#' @param ages Integer vector of ages (default: 15:94, marital status relevant ages)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, marital_status, population
#'   where marital_status is one of: "married", "widowed", "divorced", "separated", "never_married"
#'
#' @details
#' Uses the Census Microdata API endpoint:
#' https://api.census.gov/data/{year}/acs/acs1/pums
#'
#' Key variables:
#' - AGEP: Age (person-level, 0-99)
#' - SEX: Sex (1=Male, 2=Female)
#' - MAR: Marital status (1=Married, 2=Widowed, 3=Divorced, 4=Separated, 5=Never married)
#' - PWGTP: Person weight for population estimates
#'
#' Note: ACS 1-year estimates require population >= 65,000 for geographic areas,
#' but we're using national-level data which is always available.
#'
#' @export
fetch_acs_pums_marital_status <- function(years,
                                           ages = 15:94,
                                           cache_dir = here::here("data/cache/acs_pums")) {
  # Validate inputs
  checkmate::assert_integerish(years, lower = 2005, upper = 2024, min.len = 1)
  checkmate::assert_integerish(ages, lower = 0, upper = 99, min.len = 1)

  # Create cache directory
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Get API key
 api_key <- get_api_key("CENSUS_KEY")

  results <- list()

  for (yr in years) {
    cache_file <- file.path(cache_dir, sprintf("pums_marital_%d.rds", yr))

    # Check cache
    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached ACS PUMS for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS PUMS for {yr}...")

    tryCatch({
      dt <- fetch_acs_pums_year(yr, ages, api_key)

      if (!is.null(dt) && nrow(dt) > 0) {
        # Cache the result
        saveRDS(dt, cache_file)
        cli::cli_alert_success("Cached ACS PUMS for {yr} ({nrow(dt)} rows)")
        results[[as.character(yr)]] <- dt
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No ACS PUMS data retrieved")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year, sex, marital_status, age)

  cli::cli_alert_success(
    "Retrieved ACS PUMS data for {length(unique(combined$year))} years"
  )

  combined
}

#' Fetch ACS PUMS for a single year
#'
#' @param year Integer: year to fetch
#' @param ages Integer vector of ages
#' @param api_key Character: Census API key
#'
#' @return data.table with population by age, sex, marital status
#'
#' @keywords internal
fetch_acs_pums_year <- function(year, ages, api_key) {
  # Census Microdata API endpoint
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  # Build the request
  # Get AGEP (age), SEX, MAR (marital status), PWGTP (person weight)
  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = "AGEP,SEX,MAR,PWGTP",
      key = api_key
    ) |>
    httr2::req_timeout(300) |>  # PUMS queries can be slow
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  # Execute request with retry
  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  # Parse JSON response
  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    cli::cli_alert_warning("No data returned for {year}")
    return(NULL)
  }

  # Convert to data.table
  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  # Parse variables
  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, mar_code := as.integer(MAR)]
  dt[, weight := as.numeric(PWGTP)]

  # Map sex codes to labels
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Map marital status codes to labels
  # 1 = Married, 2 = Widowed, 3 = Divorced, 4 = Separated, 5 = Never married
  dt[mar_code == 1, marital_status := "married"]
  dt[mar_code == 2, marital_status := "widowed"]
  dt[mar_code == 3, marital_status := "divorced"]
  dt[mar_code == 4, marital_status := "separated"]
  dt[mar_code == 5, marital_status := "never_married"]

  # Filter to requested ages and valid data
  dt <- dt[age %in% ages & !is.na(sex) & !is.na(marital_status) & !is.na(weight)]

  # Aggregate by age, sex, marital status using person weights
  result <- dt[, .(population = sum(weight)), by = .(age, sex, marital_status)]
  result[, year := year]

  data.table::setcolorder(result, c("year", "age", "sex", "marital_status", "population"))
  data.table::setorder(result, age, sex, marital_status)

  result
}

#' Calculate marital status population shares
#'
#' @description
#' Calculates the proportion of population in each marital status category
#' by age and sex. Used to derive mortality differentials.
#'
#' @param pums_data data.table from fetch_acs_pums_marital_status
#'
#' @return data.table with columns: year, age, sex, marital_status, population, share
#'
#' @export
calculate_marital_status_shares <- function(pums_data) {
  result <- data.table::copy(pums_data)

  # Calculate total population by age, sex, year
  totals <- result[, .(total_pop = sum(population)), by = .(year, age, sex)]

  # Merge totals back
  result <- result[totals, on = .(year, age, sex)]

  # Calculate share
  result[, share := population / total_pop]

  # Clean up
  result[, total_pop := NULL]

  data.table::setorder(result, year, sex, age, marital_status)

  result
}

#' Get marital status categories
#'
#' @description
#' Returns the standard marital status categories used in mortality calculations.
#'
#' @return Character vector of marital status codes
#'
#' @export
get_marital_status_categories <- function() {
  c("married", "widowed", "divorced", "separated", "never_married")
}

#' Aggregate marital status to SSA categories
#'
#' @description
#' Aggregates detailed marital status to the 4 categories used in SSA mortality
#' calculations: married, widowed, divorced, and never_married (separated is
#' combined with divorced).
#'
#' @param pums_data data.table from fetch_acs_pums_marital_status
#'
#' @return data.table with aggregated marital status categories
#'
#' @export
aggregate_marital_status_ssa <- function(pums_data) {
  result <- data.table::copy(pums_data)

  # Combine separated with divorced (SSA methodology)
  result[marital_status == "separated", marital_status := "divorced"]

  # Re-aggregate
  result <- result[, .(population = sum(population)),
                   by = .(year, age, sex, marital_status)]

  data.table::setorder(result, year, sex, age, marital_status)

  result
}

#' Fetch ACS PUMS for all available years
#'
#' @description
#' Fetches ACS PUMS data for all years used in SSA mortality calculations.
#' Per SSA documentation, this includes 2000-2019 (with additional years
#' available each year). ACS 1-year PUMS is available from 2005.
#'
#' @param years Integer vector of years (default: 2005:2023)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with population by age, sex, marital status
#'
#' @export
fetch_acs_pums_all_years <- function(years = 2005:2023,
                                      cache_dir = here::here("data/cache/acs_pums")) {
  fetch_acs_pums_marital_status(
    years = years,
    ages = 15:94,
    cache_dir = cache_dir
  )
}

#' Fetch ACS PUMS for mortality differential calculation years
#'
#' @description
#' Fetches ACS PUMS data for years used in SSA mortality differential
#' calculations. Per documentation, uses 2015-2019 for relative mortality
#' rate calculations by marital status.
#'
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with population by age, sex, marital status for 2015-2019
#'
#' @export
fetch_acs_pums_mortality_years <- function(cache_dir = here::here("data/cache/acs_pums")) {
  fetch_acs_pums_marital_status(
    years = 2015:2019,
    ages = 15:94,
    cache_dir = cache_dir
  )
}

# =============================================================================
# CIVILIAN AND CIVILIAN NONINSTITUTIONALIZED POPULATION
# =============================================================================

#' Fetch ACS PUMS civilian population by age and sex
#'
#' @description
#' Retrieves civilian population estimates by single year of age and sex
#' from the ACS PUMS via Census Microdata API. Civilian population excludes
#' active duty military personnel.
#'
#' @param years Integer vector of years to query (2005-2023 available)
#' @param ages Integer vector of ages (default: 0:99)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @details
#' Uses the MIL (military service) variable to filter:
#' - MIL = 1: On active duty (EXCLUDED)
#' - MIL = 2-4 or NA: Not on active duty (INCLUDED)
#'
#' Note: For ages < 17, MIL is not applicable (NA), so all persons are civilian.
#'
#' @export
fetch_acs_pums_civilian <- function(years,
                                     ages = 0:99,
                                     cache_dir = here::here("data/cache/acs_pums")) {
  checkmate::assert_integerish(years, lower = 2005, upper = 2024, min.len = 1)
  checkmate::assert_integerish(ages, lower = 0, upper = 99, min.len = 1)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  api_key <- get_api_key("CENSUS_KEY")

  results <- list()

  for (yr in years) {
    cache_file <- file.path(cache_dir, sprintf("pums_civilian_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached ACS PUMS civilian for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS PUMS civilian population for {yr}...")

    tryCatch({
      dt <- fetch_acs_pums_civilian_year(yr, ages, api_key)

      if (!is.null(dt) && nrow(dt) > 0) {
        saveRDS(dt, cache_file)
        cli::cli_alert_success("Cached ACS PUMS civilian for {yr} ({nrow(dt)} rows)")
        results[[as.character(yr)]] <- dt
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No ACS PUMS civilian data retrieved")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year, sex, age)

  cli::cli_alert_success(
    "Retrieved ACS PUMS civilian data for {length(unique(combined$year))} years"
  )

  combined
}

#' Fetch ACS PUMS civilian population for a single year
#'
#' @keywords internal
fetch_acs_pums_civilian_year <- function(year, ages, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  # Query AGEP (age), SEX, MIL (military service), PWGTP (weight)
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
    cli::cli_alert_warning("No data returned for {year}")
    return(NULL)
  }

  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, mil_code := as.integer(MIL)]  # NA for ages < 17
  dt[, weight := as.numeric(PWGTP)]

  # Map sex codes
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Filter to civilian only (MIL != 1, where 1 = "On active duty")
  # For ages < 17, MIL is NA, which means civilian by default
  dt <- dt[is.na(mil_code) | mil_code != 1]

  # Filter to requested ages
  dt <- dt[age %in% ages & !is.na(sex) & !is.na(weight)]

  # Aggregate by age and sex
  result <- dt[, .(population = sum(weight)), by = .(age, sex)]
  result[, year := year]

  data.table::setcolorder(result, c("year", "age", "sex", "population"))
  data.table::setorder(result, age, sex)

  result
}

#' Fetch ACS PUMS civilian noninstitutionalized population by age and sex
#'
#' @description
#' Retrieves civilian noninstitutionalized population estimates by single year
#' of age and sex from the ACS PUMS. Excludes:
#' - Active duty military personnel
#' - Institutionalized populations (prisons, nursing homes, mental facilities)
#'
#' @param years Integer vector of years to query (2005-2023 available)
#' @param ages Integer vector of ages (default: 0:99)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @details
#' Uses two filter variables:
#' - MIL (military service): Excludes MIL = 1 (on active duty)
#' - TYPE (type of unit): Excludes TYPE = 2 (institutional group quarters)
#'
#' TYPE values: 1 = Housing unit, 2 = Institutional GQ, 3 = Noninstitutional GQ
#'
#' Institutional group quarters include: correctional facilities, nursing homes,
#' mental hospitals. Noninstitutional GQ (included) are: college dorms, military
#' barracks, group homes.
#'
#' @export
fetch_acs_pums_civilian_noninst <- function(years,
                                             ages = 0:99,
                                             cache_dir = here::here("data/cache/acs_pums")) {
  checkmate::assert_integerish(years, lower = 2005, upper = 2024, min.len = 1)
  checkmate::assert_integerish(ages, lower = 0, upper = 99, min.len = 1)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  api_key <- get_api_key("CENSUS_KEY")

  results <- list()

  for (yr in years) {
    cache_file <- file.path(cache_dir, sprintf("pums_civilian_noninst_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached ACS PUMS civilian noninst for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS PUMS civilian noninstitutionalized for {yr}...")

    tryCatch({
      dt <- fetch_acs_pums_civilian_noninst_year(yr, ages, api_key)

      if (!is.null(dt) && nrow(dt) > 0) {
        saveRDS(dt, cache_file)
        cli::cli_alert_success("Cached ACS PUMS civilian noninst for {yr} ({nrow(dt)} rows)")
        results[[as.character(yr)]] <- dt
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No ACS PUMS civilian noninstitutionalized data retrieved")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year, sex, age)

  cli::cli_alert_success(
    "Retrieved ACS PUMS civilian noninst data for {length(unique(combined$year))} years"
  )

  combined
}

#' Fetch ACS PUMS civilian noninstitutionalized for a single year
#'
#' @keywords internal
fetch_acs_pums_civilian_noninst_year <- function(year, ages, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  # Variable name changed between years:
  # - 2005-2019: TYPE (1 = Housing unit, 2 = Institutional GQ, 3 = Noninstitutional GQ)
  # - 2021-2023: TYPEHUGQ (same values)
  gq_var <- if (year >= 2021) "TYPEHUGQ" else "TYPE"

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = paste0("AGEP,SEX,MIL,", gq_var, ",PWGTP"),
      key = api_key
    ) |>
    httr2::req_timeout(300) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    cli::cli_alert_warning("No data returned for {year}")
    return(NULL)
  }

  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, mil_code := as.integer(MIL)]
  # Handle both variable names
  if (gq_var == "TYPEHUGQ") {
    dt[, gq_type := as.integer(TYPEHUGQ)]
  } else {
    dt[, gq_type := as.integer(TYPE)]
  }
  dt[, weight := as.numeric(PWGTP)]

  # Map sex codes
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Filter to civilian noninstitutionalized:
  # 1. Civilian: MIL != 1 (not on active duty) or NA (age < 17)
  # 2. Noninstitutionalized: gq_type != 2 (not in institutional GQ)
  #    Values: 1 = Housing unit, 2 = Institutional GQ, 3 = Noninstitutional GQ
  dt <- dt[(is.na(mil_code) | mil_code != 1) & (is.na(gq_type) | gq_type != 2)]

  # Filter to requested ages
  dt <- dt[age %in% ages & !is.na(sex) & !is.na(weight)]

  # Aggregate by age and sex
  result <- dt[, .(population = sum(weight)), by = .(age, sex)]
  result[, year := year]

  data.table::setcolorder(result, c("year", "age", "sex", "population"))
  data.table::setorder(result, age, sex)

  result
}

# =============================================================================
# CIVILIAN NONINSTITUTIONALIZED POPULATION BY MARITAL STATUS
# =============================================================================

#' Fetch ACS PUMS civilian noninstitutionalized population by marital status
#'
#' @description
#' Retrieves civilian noninstitutionalized population estimates by single year
#' of age, sex, and marital status from the ACS PUMS. This is used for
#' Equation 1.4.4 of the Historical Population subprocess.
#'
#' For civilian noninstitutionalized population, "Married" is split into:
#' - married_spouse_present: Living with spouse
#' - separated: Married but not living with spouse
#'
#' @param years Integer vector of years to query (2006-2023 available)
#' @param ages Integer vector of ages (default: 15:99, marital status relevant)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, marital_status, population
#'   where marital_status is: "married_spouse_present", "separated",
#'   "widowed", "divorced", "never_married"
#'
#' @details
#' Uses multiple filter variables:
#' - MIL: Excludes active duty military (MIL = 1)
#' - TYPE/TYPEHUGQ: Excludes institutional group quarters (TYPE = 2)
#' - MAR: Marital status (1=Married, 2=Widowed, 3=Divorced, 4=Separated, 5=Never married)
#' - MSP: Marital status allocation flag (for married spouse present vs absent)
#'
#' Note: MSP (married spouse present) variable available 2008+ for more
#' accurate spouse present/absent distinction.
#'
#' @export
fetch_acs_pums_civilian_noninst_marital <- function(years,
                                                     ages = 15:99,
                                                     cache_dir = here::here("data/cache/acs_pums")) {
  checkmate::assert_integerish(years, lower = 2006, upper = 2024, min.len = 1)
  checkmate::assert_integerish(ages, lower = 0, upper = 99, min.len = 1)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  api_key <- get_api_key("CENSUS_KEY")

  results <- list()

  for (yr in years) {
    # Skip 2020 - ACS not released due to COVID
    if (yr == 2020) {
      cli::cli_alert_warning("Skipping 2020 - ACS 1-year not released due to COVID")
      next
    }

    cache_file <- file.path(cache_dir, sprintf("pums_civnoninst_marital_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached ACS PUMS civ noninst marital for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS PUMS civ noninst marital for {yr}...")

    tryCatch({
      dt <- fetch_acs_pums_civnoninst_marital_year(yr, ages, api_key)

      if (!is.null(dt) && nrow(dt) > 0) {
        saveRDS(dt, cache_file)
        cli::cli_alert_success("Cached ACS PUMS civ noninst marital for {yr} ({nrow(dt)} rows)")
        results[[as.character(yr)]] <- dt
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No ACS PUMS civilian noninst marital data retrieved")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year, sex, marital_status, age)

  cli::cli_alert_success(
    "Retrieved ACS PUMS civ noninst marital data for {length(unique(combined$year))} years"
  )

  combined
}

#' Fetch ACS PUMS civilian noninstitutionalized marital status for a single year
#'
#' @keywords internal
fetch_acs_pums_civnoninst_marital_year <- function(year, ages, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  # Variable name changed between years for group quarters type
  gq_var <- if (year >= 2021) "TYPEHUGQ" else "TYPE"

  # Build variable list - MAR for marital status
  # Note: For detailed spouse present/absent, we use MAR values:
  # MAR = 1 (Married) combined with examining if spouse is in household
  # But simpler approach: MAR = 4 is "Separated" which is married but not living together
  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = paste0("AGEP,SEX,MIL,MAR,", gq_var, ",PWGTP"),
      key = api_key
    ) |>
    httr2::req_timeout(300) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    cli::cli_alert_warning("No data returned for {year}")
    return(NULL)
  }

  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, mil_code := as.integer(MIL)]
  dt[, mar_code := as.integer(MAR)]

  # Handle both GQ variable names
  if (gq_var == "TYPEHUGQ") {
    dt[, gq_type := as.integer(TYPEHUGQ)]
  } else {
    dt[, gq_type := as.integer(TYPE)]
  }
  dt[, weight := as.numeric(PWGTP)]

  # Map sex codes
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Map marital status - for civ noninst, we split married into:
  # - married_spouse_present (MAR = 1, spouse present - we treat MAR=1 as this)
  # - separated (MAR = 4)
  # Note: MAR = 1 includes both spouse present and absent; using separated (4) to capture absent
  dt[mar_code == 1, marital_status := "married_spouse_present"]
  dt[mar_code == 2, marital_status := "widowed"]
  dt[mar_code == 3, marital_status := "divorced"]
  dt[mar_code == 4, marital_status := "separated"]
  dt[mar_code == 5, marital_status := "never_married"]

  # Filter to civilian noninstitutionalized:
  # 1. Civilian: MIL != 1 (not on active duty) or NA (age < 17)
  # 2. Noninstitutionalized: gq_type != 2 (not in institutional GQ)
  dt <- dt[(is.na(mil_code) | mil_code != 1) & (is.na(gq_type) | gq_type != 2)]

  # Filter to requested ages and valid data
  dt <- dt[age %in% ages & !is.na(sex) & !is.na(marital_status) & !is.na(weight)]

  # Aggregate by age, sex, marital status
  result <- dt[, .(population = sum(weight)), by = .(age, sex, marital_status)]
  result[, year := year]

  data.table::setcolorder(result, c("year", "age", "sex", "marital_status", "population"))
  data.table::setorder(result, age, sex, marital_status)

  result
}

# =============================================================================
# MARRIAGE GRIDS (Age-of-Husband × Age-of-Wife)
# =============================================================================

#' Fetch ACS PUMS marriage grid (husband age × wife age)
#'
#' @description
#' Creates a grid of existing marriages by age of husband and age of wife.
#' This is used for balancing married populations to ensure consistency
#' between male and female married counts (Equation 1.4.2).
#'
#' @param years Integer vector of years to query (2006-2023 available)
#' @param min_age Minimum age to include (default: 15)
#' @param max_age Maximum age to include (default: 99)
#' @param include_same_sex Logical: include same-sex marriages (default: TRUE for 2013+)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return list of matrices by year, each with rows = husband age, cols = wife age
#'
#' @details
#' Uses ACS PUMS person-level data linked via household (SERIALNO).
#' For each household, identifies married couples by:
#' 1. Finding persons with MAR = 1 (married)
#' 2. Linking via SERIALNO (household ID)
#' 3. Pairing male and female married persons in same household
#'
#' Note: Same-sex marriages included starting 2013 (federal recognition).
#' Prior to 2013, same-sex marriages were coded differently in some states.
#'
#' @export
fetch_acs_marriage_grids <- function(years,
                                      min_age = 15,
                                      max_age = 99,
                                      include_same_sex = TRUE,
                                      cache_dir = here::here("data/cache/acs_pums")) {
  checkmate::assert_integerish(years, lower = 2006, upper = 2024, min.len = 1)
  checkmate::assert_int(min_age, lower = 0, upper = 99)
  checkmate::assert_int(max_age, lower = min_age, upper = 99)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  api_key <- get_api_key("CENSUS_KEY")

  results <- list()

  for (yr in years) {
    # Skip 2020 - ACS not released due to COVID
    if (yr == 2020) {
      cli::cli_alert_warning("Skipping 2020 - ACS 1-year not released due to COVID")
      next
    }

    cache_file <- file.path(cache_dir, sprintf("marriage_grid_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached marriage grid for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS PUMS marriage data for {yr}...")

    tryCatch({
      grid <- fetch_acs_marriage_grid_year(yr, min_age, max_age, include_same_sex, api_key)

      if (!is.null(grid)) {
        saveRDS(grid, cache_file)
        cli::cli_alert_success("Cached marriage grid for {yr}")
        results[[as.character(yr)]] <- grid
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No marriage grid data retrieved")
  }

  cli::cli_alert_success(
    "Retrieved marriage grids for {length(results)} years"
  )

  results
}

#' Fetch ACS PUMS marriage grid for a single year
#'
#' @keywords internal
fetch_acs_marriage_grid_year <- function(year, min_age, max_age, include_same_sex, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  # Query: SERIALNO (household ID), AGEP (age), SEX, MAR (marital status), PWGTP (weight)
  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = "SERIALNO,AGEP,SEX,MAR,PWGTP",
      # Filter to married persons only (MAR = 1) to reduce data volume
      MAR = "1",
      key = api_key
    ) |>
    httr2::req_timeout(600) |>  # Marriage grid queries are large
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    cli::cli_alert_warning("No data returned for {year}")
    return(NULL)
  }

  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  # Parse variables
  dt[, serialno := as.character(SERIALNO)]
  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, weight := as.numeric(PWGTP)]

  # Map sex codes
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Filter to valid ages
  dt <- dt[age >= min_age & age <= max_age & !is.na(sex)]

  # Identify married couples by linking spouses in same household
  # Split into males and females
  males <- dt[sex == "male", .(serialno, husband_age = age, husband_weight = weight)]
  females <- dt[sex == "female", .(serialno, wife_age = age, wife_weight = weight)]

  # Join on household ID to create couples
  # For households with multiple married people of same sex, this will create multiple pairs
  couples <- merge(males, females, by = "serialno", allow.cartesian = TRUE)

  if (nrow(couples) == 0) {
    cli::cli_alert_warning("No married couples found for {year}")
    return(NULL)
  }

  # Use average of husband and wife weights for the couple
  couples[, couple_weight := (husband_weight + wife_weight) / 2]

  # Create the grid: aggregate by husband age × wife age
  grid_data <- couples[, .(marriages = sum(couple_weight)),
                       by = .(husband_age, wife_age)]

  # Convert to matrix form (rows = husband age, cols = wife age)
  ages <- min_age:max_age
  n_ages <- length(ages)
  grid_matrix <- matrix(0, nrow = n_ages, ncol = n_ages,
                        dimnames = list(husband = ages, wife = ages))

  for (i in seq_len(nrow(grid_data))) {
    h_idx <- grid_data$husband_age[i] - min_age + 1
    w_idx <- grid_data$wife_age[i] - min_age + 1
    if (h_idx >= 1 && h_idx <= n_ages && w_idx >= 1 && w_idx <= n_ages) {
      grid_matrix[h_idx, w_idx] <- grid_data$marriages[i]
    }
  }

  # Add metadata as attributes
  attr(grid_matrix, "year") <- year
  attr(grid_matrix, "total_marriages") <- sum(grid_matrix)
  attr(grid_matrix, "min_age") <- min_age
  attr(grid_matrix, "max_age") <- max_age

  grid_matrix
}

#' Summarize marriage grid
#'
#' @description
#' Provides summary statistics for a marriage grid.
#'
#' @param grid Matrix from fetch_acs_marriage_grids
#'
#' @return List with summary statistics
#'
#' @export
summarize_marriage_grid <- function(grid) {
  # Total marriages
  total <- sum(grid)

  # Average ages
  ages <- as.integer(rownames(grid))
  husband_marginal <- rowSums(grid)
  wife_marginal <- colSums(grid)

  avg_husband_age <- sum(ages * husband_marginal) / sum(husband_marginal)
  avg_wife_age <- sum(ages * wife_marginal) / sum(wife_marginal)

  # Age difference statistics
  age_diffs <- list()
  for (h in seq_along(ages)) {
    for (w in seq_along(ages)) {
      if (grid[h, w] > 0) {
        diff <- ages[h] - ages[w]  # husband - wife
        age_diffs[[length(age_diffs) + 1]] <- list(diff = diff, count = grid[h, w])
      }
    }
  }
  diff_dt <- data.table::rbindlist(age_diffs)
  avg_age_diff <- sum(diff_dt$diff * diff_dt$count) / sum(diff_dt$count)

  list(
    year = attr(grid, "year"),
    total_marriages = total,
    avg_husband_age = avg_husband_age,
    avg_wife_age = avg_wife_age,
    avg_age_difference = avg_age_diff  # positive = husband older
  )
}

#' Convert marriage grid to data.table
#'
#' @description
#' Converts a marriage grid matrix to long-form data.table.
#'
#' @param grid Matrix from fetch_acs_marriage_grids
#'
#' @return data.table with columns: husband_age, wife_age, marriages
#'
#' @export
marriage_grid_to_dt <- function(grid) {
  year <- attr(grid, "year")
  ages <- as.integer(rownames(grid))

  # Convert to long form
  result <- data.table::data.table(
    husband_age = rep(ages, times = length(ages)),
    wife_age = rep(ages, each = length(ages)),
    marriages = as.vector(grid)
  )

  result <- result[marriages > 0]
  result[, year := year]
  data.table::setcolorder(result, c("year", "husband_age", "wife_age", "marriages"))

  result
}

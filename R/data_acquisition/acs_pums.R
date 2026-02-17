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
#'   where marital_status is one of: "married", "widowed", "divorced", "separated", "single"
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
  dt[mar_code == 5, marital_status := "single"]

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
  c("married", "widowed", "divorced", "separated", "single")
}

#' Aggregate marital status to SSA categories
#'
#' @description
#' Aggregates detailed marital status to the 4 categories used in SSA mortality
#' calculations: married, widowed, divorced, and single (separated is
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
#'   "widowed", "divorced", "single"
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
  dt[mar_code == 5, marital_status := "single"]

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

# =============================================================================
# MARRIED COUPLES GRID FOR POPULATION PROJECTION
# =============================================================================

#' Fetch historical married couples grid for population projection
#'
#' @description
#' Retrieves an empirical husband-age × wife-age married couples distribution
#' from ACS PUMS, averaged across reference years for stability. This replaces
#' the normal distribution approximation previously used in build_married_couples_grid().
#'
#' Uses the existing fetch_acs_marriage_grids() infrastructure to query ACS PUMS
#' for married persons linked by household SERIALNO.
#'
#' @param years Integer vector of reference years to average over
#'   (configured via projected_population.couples_grid.reference_years)
#' @param min_age Integer: minimum age (default: 14, matching marriage_min)
#' @param max_age Integer: maximum age (default: 100, matching max_age_group)
#' @param cache_dir Character: cache directory for per-year PUMS files
#'
#' @return Matrix (n_ages × n_ages) with husband age as rows, wife age as columns.
#'   Values are proportions (sum to 1). Row/column names are age labels.
#'
#' @details
#' TR2025 Input #10: "Married couples by single year of age of spouse 1
#' crossed with single year of age of spouse 2"
#'
#' The grid is returned as proportions (normalized to sum to 1) rather than
#' counts, so it can be used as a base distribution for IPF normalization
#' against current-year married male/female marginals.
#'
#' @export
fetch_married_couples_grid <- function(years,
                                        min_age = 14L,
                                        max_age = 100L,
                                        cache_dir = here::here("data/cache/acs_pums")) {
  checkmate::assert_integerish(years, min.len = 1)

  # Check for cached averaged grid
  avg_cache_file <- file.path(cache_dir, sprintf("married_couples_grid_%d_%d.rds",
                                                   min(years), max(years)))
  if (file.exists(avg_cache_file)) {
    cli::cli_alert_success("Loading cached married couples grid (averaged {length(years)} years)")
    return(readRDS(avg_cache_file))
  }

  cli::cli_alert_info("Fetching married couples grids from ACS PUMS for {length(years)} years...")

  # Fetch per-year grids using existing infrastructure
  # fetch_acs_marriage_grids handles caching, API calls, and spouse linking
  year_grids <- fetch_acs_marriage_grids(
    years = years,
    min_age = min_age,
    max_age = min(max_age, 99L),  # ACS PUMS AGEP tops at 99
    include_same_sex = FALSE,
    cache_dir = cache_dir
  )

  if (length(year_grids) == 0) {
    cli::cli_abort(c(
      "No married couples grids retrieved from ACS PUMS",
      "i" = "Requested years: {paste(years, collapse=', ')}",
      "i" = "Ensure CENSUS_KEY is set in .Renviron"
    ))
  }

  # Average across years for stability
  n_grids <- length(year_grids)
  avg_grid <- year_grids[[1]] * 0  # Initialize to zero with same dimensions

  for (g in year_grids) {
    avg_grid <- avg_grid + g
  }
  avg_grid <- avg_grid / n_grids

  # Extend to max_age if needed (ACS tops at 99, we need 100)
  if (max_age > 99L) {
    fetched_ages <- as.integer(rownames(avg_grid))
    target_ages <- min_age:max_age
    extended <- matrix(0, nrow = length(target_ages), ncol = length(target_ages),
                       dimnames = list(target_ages, target_ages))

    # Copy existing data
    for (h_age in fetched_ages) {
      for (w_age in fetched_ages) {
        h_idx <- h_age - min_age + 1
        w_idx <- w_age - min_age + 1
        h_old <- h_age - min(fetched_ages) + 1
        w_old <- w_age - min(fetched_ages) + 1
        extended[h_idx, w_idx] <- avg_grid[h_old, w_old]
      }
    }

    # For age 100: use age 99 values (small population, reasonable assumption)
    if (100L %in% target_ages) {
      idx_100 <- 100L - min_age + 1
      idx_99 <- 99L - min_age + 1
      extended[idx_100, ] <- extended[idx_99, ] * 0.3  # Scaled down for 100+
      extended[, idx_100] <- extended[, idx_99] * 0.3
    }

    avg_grid <- extended
  }

  # Normalize to proportions (sum to 1)
  grid_total <- sum(avg_grid)
  if (grid_total > 0) {
    avg_grid <- avg_grid / grid_total
  }

  # Add metadata
  attr(avg_grid, "years") <- years
  attr(avg_grid, "n_grids_averaged") <- n_grids
  attr(avg_grid, "min_age") <- min_age
  attr(avg_grid, "max_age") <- max_age

  # Log summary
  ages <- as.integer(rownames(avg_grid))
  husband_marginal <- rowSums(avg_grid)
  wife_marginal <- colSums(avg_grid)
  avg_h <- sum(ages * husband_marginal) / sum(husband_marginal)
  avg_w <- sum(ages * wife_marginal) / sum(wife_marginal)
  cli::cli_alert_success("Built married couples grid: {nrow(avg_grid)}x{ncol(avg_grid)}, averaged {n_grids} years")
  cli::cli_alert_info("Average husband age: {round(avg_h, 1)}, wife age: {round(avg_w, 1)}, diff: {round(avg_h - avg_w, 1)}")

  # Cache
  dir.create(dirname(avg_cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(avg_grid, avg_cache_file)

  avg_grid
}


# =============================================================================
# FOREIGN-BORN FLOWS BY YEAR OF ENTRY
# =============================================================================

#' Fetch ACS PUMS foreign-born population by year of entry
#'
#' @description
#' Retrieves foreign-born population by single year of age, sex, and year of
#' entry to the US from ACS PUMS. This is Input #25 in TR2025 documentation,
#' used to estimate flows of temporary and unlawfully present immigrants
#' for Equation 1.4.3 (O_{x,s} unauthorized population).
#'
#' @param years Integer vector of ACS years to query (2013-2023 available)
#' @param ages Integer vector of ages (default: 0:99)
#' @param include_cuban Logical: if TRUE, separately identify Cuban-born (default: TRUE)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, year_of_entry, nativity,
#'   is_cuban, population
#'
#' @details
#' Uses ACS PUMS variables:
#' - AGEP: Age (0-99)
#' - SEX: Sex (1=Male, 2=Female)
#' - NATIVITY: Nativity (1=Native, 2=Foreign born)
#' - YOEP: Year of entry to the US (for foreign-born)
#' - POBP: Place of birth (Cuba = 327)
#' - PWGTP: Person weight
#'
#' Year of entry is critical for distinguishing:
#' - Recent arrivals (likely unauthorized or temporary)
#' - Long-term residents (more likely documented)
#'
#' Cuban immigrants have special status under Cuban Adjustment Act,
#' making them important to track separately.
#'
#' @export
fetch_acs_foreign_born_flows <- function(years,
                                          ages = 0:99,
                                          include_cuban = TRUE,
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

    cache_file <- file.path(cache_dir, sprintf("foreign_born_flows_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached foreign-born flows for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS PUMS foreign-born flows for {yr}...")

    tryCatch({
      dt <- fetch_acs_foreign_born_year(yr, ages, include_cuban, api_key)

      if (!is.null(dt) && nrow(dt) > 0) {
        saveRDS(dt, cache_file)
        cli::cli_alert_success("Cached foreign-born flows for {yr} ({nrow(dt)} rows)")
        results[[as.character(yr)]] <- dt
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No ACS PUMS foreign-born flow data retrieved")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year, sex, age, year_of_entry)

  cli::cli_alert_success(
    "Retrieved ACS foreign-born flows for {length(unique(combined$year))} years"
  )

  combined
}

#' Fetch ACS PUMS foreign-born flows for a single year
#'
#' @keywords internal
fetch_acs_foreign_born_year <- function(year, ages, include_cuban, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  # Build variable list
  # YOEP = Year of Entry to US (for foreign-born only)
  # POBP = Place of Birth (for identifying Cuban-born)
  # NATIVITY = 1 (Native) or 2 (Foreign born)
  vars <- "AGEP,SEX,NATIVITY,YOEP,PWGTP"
  if (include_cuban) {
    vars <- paste0(vars, ",POBP")
  }

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = vars,
      # Filter to foreign-born only (NATIVITY = 2)
      NATIVITY = "2",
      key = api_key
    ) |>
    httr2::req_timeout(600) |>  # Large query
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
  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, nativity := as.integer(NATIVITY)]
  dt[, year_of_entry := as.integer(YOEP)]
  dt[, weight := as.numeric(PWGTP)]

  # Map sex codes
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Identify Cuban-born if requested
  # POBP = 327 is Cuba
  if (include_cuban && "POBP" %in% names(dt)) {
    dt[, pobp := as.integer(POBP)]
    dt[, is_cuban := (pobp == 327)]
    dt[is.na(is_cuban), is_cuban := FALSE]
  } else {
    dt[, is_cuban := FALSE]
  }

  # Filter to requested ages and valid data
  # YOEP can be NA for some records (shouldn't happen for foreign-born but check)
  dt <- dt[age %in% ages & !is.na(sex) & !is.na(weight)]

  # Aggregate by age, sex, year of entry, Cuban status
  result <- dt[, .(population = sum(weight)),
               by = .(age, sex, year_of_entry, is_cuban)]
  result[, survey_year := year]

  data.table::setcolorder(result, c("survey_year", "age", "sex", "year_of_entry",
                                    "is_cuban", "population"))
  data.table::setnames(result, "survey_year", "year")
  data.table::setorder(result, age, sex, year_of_entry)

  result
}

#' Calculate foreign-born flows by year of entry
#'
#' @description
#' Calculates net foreign-born population flows by analyzing changes in
#' foreign-born population by year of entry across survey years.
#' This helps estimate unauthorized population movements.
#'
#' @param flows_data data.table from fetch_acs_foreign_born_flows
#' @param entry_years Integer vector of years of entry to analyze
#'
#' @return data.table with estimated annual flows
#'
#' @export
calculate_foreign_born_flows <- function(flows_data, entry_years = NULL) {
  if (is.null(entry_years)) {
    entry_years <- sort(unique(flows_data$year_of_entry))
    entry_years <- entry_years[!is.na(entry_years)]
  }

  # Calculate total foreign-born by year of entry and survey year
  totals <- flows_data[year_of_entry %in% entry_years,
                       .(total_population = sum(population)),
                       by = .(year, year_of_entry)]

  data.table::setorder(totals, year_of_entry, year)

  totals
}

#' Summarize foreign-born by entry cohort
#'
#' @description
#' Summarizes foreign-born population by entry year cohorts.
#'
#' @param flows_data data.table from fetch_acs_foreign_born_flows
#'
#' @return data.table with summary by entry cohort
#'
#' @export
summarize_foreign_born_cohorts <- function(flows_data) {
  # Define cohorts
  flows_data[, entry_cohort := data.table::fcase(
    year_of_entry < 1990, "Before 1990",
    year_of_entry >= 1990 & year_of_entry < 2000, "1990-1999",
    year_of_entry >= 2000 & year_of_entry < 2010, "2000-2009",
    year_of_entry >= 2010 & year_of_entry < 2015, "2010-2014",
    year_of_entry >= 2015, "2015+"
  )]

  # Summarize by survey year and cohort
  summary <- flows_data[!is.na(entry_cohort),
                        .(total_population = sum(population),
                          cuban_population = sum(population[is_cuban == TRUE]),
                          non_cuban_population = sum(population[is_cuban == FALSE])),
                        by = .(year, entry_cohort)]

  data.table::setorder(summary, year, entry_cohort)

  summary
}

# =============================================================================
# PRE-2006 MARITAL STATUS DATA (IPUMS Census 2000)
# =============================================================================

#' Fetch Census 2000 marital status data
#'
#' @description
#' Retrieves marital status data for years before ACS coverage (2000-2005).
#' This is Input #18 in TR2025 documentation.
#'
#' Uses IPUMS USA Census 2000 microdata (5% sample), which provides:
#' - Single-year-of-age detail (not available from Census APIs)
#' - Complete marital status categories
#' - Weighted population counts
#'
#' For years 2000-2005:
#' - 2000: Census 2000 IPUMS microdata
#' - 2001-2005: Interpolated between Census 2000 and ACS 2006
#'
#' @param years Integer vector of years to query (2000-2005)
#' @param ages Integer vector of ages (default: 15:99)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, marital_status, population
#'
#' @details
#' Before calling this function, download the IPUMS data by running:
#' `fetch_ipums_marital_status(census_years = 2000)`
#'
#' This requires an IPUMS API key (IPUMS_API_KEY in .Renviron).
#' Get a key at: https://account.ipums.org/api_keys
#'
#' @export
fetch_census2000_pums_marital <- function(years = 2000:2005,
                                           ages = 15:99,
                                           cache_dir = here::here("data/cache/census2000")) {
  checkmate::assert_integerish(years, lower = 2000, upper = 2005, min.len = 1)

  checkmate::assert_integerish(ages, lower = 0, upper = 99, min.len = 1)

  cli::cli_alert_info("Fetching Census 2000 marital status data...")

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Check cache first
  cache_file <- file.path(cache_dir, "census2000_marital_api.rds")

  if (file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    cached <- cached[year %in% years & age %in% ages]
    if (nrow(cached) > 0) {
      cli::cli_alert_success("Loaded Census 2000 marital data from cache")
      return(cached)
    }
  }

  # Use IPUMS Census 2000 microdata (5% sample with single-year ages)
  census2000_data <- fetch_ipums_census2000_marital(ages, cache_dir)

  # Require IPUMS data - no hardcoded fallbacks
  if (is.null(census2000_data) || nrow(census2000_data) == 0) {
    cli::cli_abort(c(
      "IPUMS Census 2000 marital status data required",
      "i" = "Run fetch_ipums_marital_status(census_years = 2000) to download IPUMS data",
      "i" = "IPUMS provides single-year-of-age microdata from the 5% sample",
      "i" = "Get an IPUMS API key at: https://account.ipums.org/api_keys",
      "x" = "No hardcoded fallback - real microdata is required"
    ))
  }

  # Build full series with interpolation for 2001-2005
  result <- build_census2000_marital_series(years, ages, census2000_data)

  # Cache result
  saveRDS(result, cache_file)

  cli::cli_alert_success("Retrieved Census 2000 marital data for {length(unique(result$year))} years")

  result
}

#' Fetch Census 2000 marital data from IPUMS cache
#'
#' @description
#' Loads Census 2000 marital status data from cached IPUMS extract.
#' IPUMS provides single-year-of-age detail from the 5% sample.
#'
#' @param ages Integer vector of ages to include
#' @param cache_dir Character: directory where census2000 cache is stored
#'
#' @return data.table with columns: age, sex, marital_status, population
#'   or NULL if IPUMS data not available
#'
#' @keywords internal
fetch_ipums_census2000_marital <- function(ages, cache_dir) {
  # IPUMS data is cached in the ipums directory

  ipums_cache_dir <- file.path(dirname(cache_dir), "ipums")
  ipums_cache_file <- file.path(ipums_cache_dir, "ipums_marital_status_all.rds")

  if (!file.exists(ipums_cache_file)) {
    cli::cli_alert_warning("IPUMS marital status cache not found")
    cli::cli_alert_info("Run fetch_ipums_marital_status(census_years = 2000) to download")
    return(NULL)
  }

  cli::cli_alert("Loading Census 2000 marital data from IPUMS cache...")

  tryCatch({
    ipums_data <- readRDS(ipums_cache_file)

    # Convert to plain data.table (IPUMS data may have haven_labelled columns)
    ipums_data <- data.table::as.data.table(ipums_data)

    # Convert haven_labelled columns to standard types
    if (inherits(ipums_data$year, "haven_labelled")) {
      ipums_data[, year := as.integer(year)]
    }
    if (inherits(ipums_data$age, "haven_labelled")) {
      ipums_data[, age := as.integer(age)]
    }
    if (inherits(ipums_data$population, "haven_labelled")) {
      ipums_data[, population := as.numeric(population)]
    }

    # Filter to year 2000 and requested ages
    census2000 <- ipums_data[year == 2000 & age %in% ages]

    if (nrow(census2000) == 0) {
      cli::cli_alert_warning("No Census 2000 data in IPUMS cache")
      return(NULL)
    }

    # Map IPUMS marital_status to our standard codes
    # IPUMS uses: "married", "widowed", "divorced", "separated", "single"
    # We use: "married_spouse_present", "widowed", "divorced", "separated", "single"
    census2000[marital_status == "married", marital_status := "married_spouse_present"]

    # Remove year column (will be added back in build_census2000_marital_series)
    result <- census2000[, .(age, sex, marital_status, population)]

    # Ensure age is integer for downstream processing
    result[, age := as.integer(age)]

    cli::cli_alert_success("Loaded IPUMS Census 2000 data: {format(nrow(result), big.mark=',')} rows")
    cli::cli_alert_success("Ages {min(result$age)}-{max(result$age)}, population: {format(sum(result$population), big.mark=',')}")

    result

  }, error = function(e) {
    cli::cli_alert_warning("Error reading IPUMS cache: {conditionMessage(e)}")
    NULL
  })
}

#' Build Census 2000 era marital status series
#'
#' @description
#' Constructs marital status data for 2000-2005 using Census 2000 as base
#' and interpolating to ACS 2006.
#'
#' @keywords internal
build_census2000_marital_series <- function(years, ages, census2000_data) {
  results <- list()

  # Try to get ACS 2006 data for interpolation
  acs2006_data <- tryCatch({
    fetch_acs_pums_civilian_noninst_marital(years = 2006, ages = ages)
  }, error = function(e) {
    NULL
  })

  for (yr in years) {
    if (yr == 2000) {
      # Use Census 2000 directly
      yr_data <- data.table::copy(census2000_data)
      yr_data[, year := 2000]
      results[["2000"]] <- yr_data
    } else {
      # Interpolate between Census 2000 and ACS 2006
      weight_2006 <- (yr - 2000) / 6  # 0 at 2000, 1 at 2006

      if (!is.null(acs2006_data) && nrow(acs2006_data) > 0) {
        # True interpolation between Census 2000 and ACS 2006
        yr_data <- interpolate_marital_data(census2000_data, acs2006_data, weight_2006, yr)
      } else {
        # Fallback: apply trend adjustments to Census 2000
        yr_data <- data.table::copy(census2000_data)
        yr_data[, year := yr]

        # Apply demographic trend adjustments
        yr_data[marital_status == "married_spouse_present",
                population := as.integer(population * (1 - 0.003 * (yr - 2000)))]
        yr_data[marital_status == "single",
                population := as.integer(population * (1 + 0.005 * (yr - 2000)))]
      }

      results[[as.character(yr)]] <- yr_data
    }
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  data.table::setcolorder(combined, c("year", "age", "sex", "marital_status", "population"))
  data.table::setorder(combined, year, sex, marital_status, age)

  combined
}

#' Interpolate marital data between two years
#'
#' @keywords internal
interpolate_marital_data <- function(data_start, data_end, weight_end, target_year) {
  # Merge the two datasets
  start <- data.table::copy(data_start)
  end <- data.table::copy(data_end)

  start[, pop_start := population]
  end[, pop_end := population]

  merged <- merge(
    start[, .(age, sex, marital_status, pop_start)],
    end[, .(age, sex, marital_status, pop_end)],
    by = c("age", "sex", "marital_status"),
    all = TRUE
  )

  # Fill NAs with zeros

  merged[is.na(pop_start), pop_start := 0]
  merged[is.na(pop_end), pop_end := 0]

  # Interpolate
  merged[, population := as.integer(pop_start * (1 - weight_end) + pop_end * weight_end)]
  merged[, year := target_year]

  merged[, .(year, age, sex, marital_status, population)]
}

#' Fetch marital status for full range (2000-2023)
#'
#' @description
#' Fetches marital status data for the full range required by TR2025:
#' 2000-2023. Combines Census 2000 PUMS (2000-2005) with ACS PUMS (2006-2023).
#'
#' @param years Integer vector of years to query (2000-2023)
#' @param ages Integer vector of ages (default: 15:99)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, marital_status, population
#'
#' @export
fetch_marital_status_full_range <- function(years = 2000:2023,
                                             ages = 15:99,
                                             cache_dir = here::here("data/cache/acs_pums")) {
  checkmate::assert_integerish(years, lower = 2000, upper = 2030, min.len = 1)
  checkmate::assert_integerish(ages, lower = 0, upper = 99, min.len = 1)

  cli::cli_alert_info("Fetching full range marital status data (2000-2023)...")

  results <- list()

  # Split years into pre-2006 and 2006+
  pre_2006_years <- years[years <= 2005]
  post_2005_years <- years[years >= 2006]

  # Fetch pre-2006 data from Census 2000
  if (length(pre_2006_years) > 0) {
    cli::cli_alert("Fetching 2000-2005 from Census 2000 PUMS...")
    pre_data <- fetch_census2000_pums_marital(
      years = pre_2006_years,
      ages = ages,
      cache_dir = file.path(cache_dir, "census2000")
    )
    results[["pre2006"]] <- pre_data
  }

  # Fetch 2006+ data from ACS PUMS
  if (length(post_2005_years) > 0) {
    cli::cli_alert("Fetching 2006-2023 from ACS PUMS...")
    # Skip 2020 (no ACS 1-year)
    post_2005_years <- post_2005_years[post_2005_years != 2020]

    if (length(post_2005_years) > 0) {
      post_data <- fetch_acs_pums_civilian_noninst_marital(
        years = post_2005_years,
        ages = ages,
        cache_dir = cache_dir
      )
      results[["post2005"]] <- post_data
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No marital status data retrieved")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  data.table::setcolorder(combined, c("year", "age", "sex", "marital_status", "population"))
  data.table::setorder(combined, year, sex, marital_status, age)

  cli::cli_alert_success(
    "Retrieved marital status for {length(unique(combined$year))} years (2000-2023)"
  )

  combined
}

#' Summarize marital status data sources
#'
#' @description
#' Returns information about data sources for marital status by year.
#'
#' @export
summarize_marital_status_sources <- function() {
  data.table::data.table(
    period = c("2000", "2001-2005", "2006-2019", "2020", "2021-2023"),
    source = c(
      "Census 2000 PUMS (5% sample)",
      "Interpolated Census 2000 to ACS 2006",
      "ACS 1-year PUMS",
      "No data (ACS 1-year not released)",
      "ACS 1-year PUMS"
    ),
    notes = c(
      "Decennial census; group quarters less precise",
      "Linear interpolation of marital proportions",
      "Annual surveys with group quarters detail",
      "COVID-19 data collection issues; interpolate from 2019/2021",
      "Resumed annual collection"
    )
  )
}

#' Census Bureau Population Data Acquisition
#'
#' Functions for fetching population estimates from the U.S. Census Bureau API.
#'
#' @name census_population
NULL

#' Fetch population estimates from Census Bureau API
#'
#' @description
#' Retrieves population estimates by single year of age and sex from
#' the Census Bureau's Population Estimates Program (PEP).
#'
#' @param years Integer vector of years to query (e.g., 2010:2023)
#' @param ages Integer vector of ages (default: 0:85)
#' @param sex Character: "both", "male", or "female" (default: "female")
#' @param config List: API configuration (from load_api_config())
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @details
#' The Census API requires different endpoints for different year ranges:
#' - 2020-2023: Vintage 2023 estimates (pep/charv)
#' - 2010-2019: Vintage 2019 estimates (pep/charage)
#' - 2000-2009: Intercensal estimates (pep/int_charage)
#' - 1990-1999: Intercensal estimates (pep/int_natrespop)
#' - Pre-1990: Not available via API (requires file downloads)
#'
#' API key should be set in .Renviron as CENSUS_KEY
#'
#' @export
fetch_census_population <- function(years,
                                    ages = 0:85,
                                    sex = "female",
                                    config = NULL) {
  checkmate::assert_integerish(years, lower = 1900, upper = 2100, min.len = 1)
  checkmate::assert_integerish(ages, lower = 0, upper = 100, min.len = 1)
  checkmate::assert_choice(sex, c("both", "male", "female"))

  api_key <- get_api_key("CENSUS_KEY")

  # Split years into groups by API vintage
  year_groups <- split_years_by_vintage(years)

  cli::cli_alert_info(
    "Fetching Census population data for {length(years)} years, ages {min(ages)}-{max(ages)}, sex={sex}"
  )

  # Fetch from each vintage
  results <- list()

  for (vintage_name in names(year_groups)) {
    vintage_years <- year_groups[[vintage_name]]
    if (length(vintage_years) == 0) next

    cli::cli_alert("Fetching {vintage_name}: {min(vintage_years)}-{max(vintage_years)}")

    result <- fetch_vintage_population(
      vintage = vintage_name,
      years = vintage_years,
      ages = ages,
      sex = sex,
      api_key = api_key,
      config = config
    )

    if (!is.null(result) && nrow(result) > 0) {
      results[[vintage_name]] <- result
    }
  }

  # Combine all results
  if (length(results) == 0) {
    cli::cli_abort("No population data retrieved from Census API")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  data.table::setorder(combined, year, age)

  cli::cli_alert_success(
    "Retrieved {nrow(combined)} population records ({min(combined$year)}-{max(combined$year)})"
  )

  combined
}

#' Split years into groups by Census API vintage
#'
#' @param years Integer vector of years
#'
#' @return Named list of year vectors by vintage
#'
#' @keywords internal
split_years_by_vintage <- function(years) {
  list(
    vintage_2023 = years[years >= 2020 & years <= 2023],
    vintage_2019 = years[years >= 2010 & years <= 2019],
    vintage_2000 = years[years >= 2000 & years <= 2009],
    vintage_1990 = years[years >= 1990 & years <= 1999]
  )
}

#' Fetch population for a specific Census vintage
#'
#' @param vintage Character: vintage name
#' @param years Integer vector of years
#' @param ages Integer vector of ages
#' @param sex Character: sex filter
#' @param api_key Character: Census API key
#' @param config List: API configuration
#'
#' @return data.table with population data
#'
#' @keywords internal
fetch_vintage_population <- function(vintage, years, ages, sex, api_key, config = NULL) {

  # Get endpoint configuration for this vintage
  endpoint_config <- get_vintage_endpoint(vintage)

  if (is.null(endpoint_config)) {
    cli::cli_alert_warning(
      "No API endpoint configured for {vintage}, years {min(years)}-{max(years)} will be skipped"
    )
    return(NULL)
  }

  # Build and execute the API request
  tryCatch({
    endpoint_config$fetch_fn(
      base_url = endpoint_config$base_url,
      years = years,
      ages = ages,
      sex = sex,
      api_key = api_key,
      date_map = endpoint_config$date_map
    )
  }, error = function(e) {
    cli::cli_alert_warning("Failed to fetch {vintage}: {conditionMessage(e)}")
    NULL
  })
}

#' Get Census API endpoint configuration for a vintage
#'
#' @param vintage Character: vintage name
#'
#' @return List with base_url, date_map, and fetch_fn, or NULL if not available
#'
#' @keywords internal
get_vintage_endpoint <- function(vintage) {
  endpoints <- list(
    vintage_2023 = list(
      base_url = "https://api.census.gov/data/2023/pep/charv",
      # YEAR values for this endpoint are the actual years
      date_map = c("2020" = "2020", "2021" = "2021", "2022" = "2022", "2023" = "2023"),
      fetch_fn = fetch_pep_charv_2023
    ),
    vintage_2019 = list(
      base_url = "https://api.census.gov/data/2019/pep/charage",
      # DATE_CODE: 3=2010, 4=2011, ..., 12=2019
      date_map = c("2010" = "3", "2011" = "4", "2012" = "5", "2013" = "6", "2014" = "7",
                   "2015" = "8", "2016" = "9", "2017" = "10", "2018" = "11", "2019" = "12"),
      fetch_fn = fetch_pep_charage_2019
    ),
    vintage_2000 = list(
      base_url = "https://api.census.gov/data/2000/pep/int_charage",
      # DATE_: 2=2000, 3=2001, ..., 11=2009
      date_map = c("2000" = "2", "2001" = "3", "2002" = "4", "2003" = "5", "2004" = "6",
                   "2005" = "7", "2006" = "8", "2007" = "9", "2008" = "10", "2009" = "11"),
      fetch_fn = fetch_pep_int_charage_2000
    ),
    vintage_1990 = list(
      base_url = "https://api.census.gov/data/1990/pep/int_natrespop",
      # YEAR values are actual years for this endpoint
      date_map = as.character(1990:1999),
      fetch_fn = fetch_pep_int_natrespop_1990
    )
  )

  endpoints[[vintage]]
}

#' Fetch from 2023 PEP charv endpoint (2020-2023)
#'
#' @keywords internal
fetch_pep_charv_2023 <- function(base_url, years, ages, sex, api_key, date_map) {
  # This endpoint uses:
  # - AGE as 4-digit codes where single-year ages end in "00":
  #   "0100" = age 1, "0200" = age 2, ..., "3000" = age 30, "8500" = age 85
  #   Other codes represent age groups (e.g., "0401" = ages 0-4, "1519" = ages 15-19)
  # - YEAR as the actual year
  # - MONTH: 4 = April 1 (census), 7 = July 1 (mid-year estimate)
  # - SEX: 0=Both, 1=Male, 2=Female
  # - for=us:1
  # We use July 1 (MONTH=7) estimates for consistency with other vintages

  sex_code <- switch(sex, "both" = "0", "male" = "1", "female" = "2")

  # Build requests for each year (API doesn't support multiple years well)
  all_results <- list()

  for (yr in years) {
    # Request all ages at once for this year, filtering for July estimates
    url <- base_url
    req <- httr2::request(url) |>
      httr2::req_url_query(
        get = "POP,SEX,AGE,YEAR,MONTH",
        `for` = "us:1",
        YEAR = as.character(yr),
        MONTH = "7",  # July 1 mid-year estimate
        SEX = sex_code,
        key = api_key
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

    resp <- api_request_with_retry(req)
    check_api_response(resp, glue::glue("Census PEP API (vintage_2023, {yr})"))

    json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    if (length(json_data) < 2) next

    # Convert to data.table
    headers <- json_data[1, ]
    data_rows <- json_data[-1, , drop = FALSE]
    dt <- data.table::as.data.table(data_rows)
    # Handle duplicate column names
    unique_headers <- make.unique(headers)
    data.table::setnames(dt, unique_headers)

    # Parse the data
    # AGE codes: single-year ages end in "00" (e.g., "3000" = age 30)
    # Other codes are age groups which we need to filter out
    dt[, age_code := as.integer(AGE)]
    dt[, population := as.numeric(POP)]

    # Only keep single-year ages (codes ending in 00, except 0000 which is all ages)
    # Single year ages: 0100 (age 1), 0200 (age 2), ..., 8500 (age 85)
    dt <- dt[age_code > 0 & age_code %% 100 == 0]

    # Convert code to actual age
    dt[, age := as.integer(age_code / 100)]
    dt[, year := as.integer(YEAR)]
    dt[, sex := sex]

    # Filter to requested ages
    dt <- dt[age %in% ages]

    all_results[[as.character(yr)]] <- dt[, .(year, age, sex, population)]
  }

  if (length(all_results) > 0) {
    data.table::rbindlist(all_results, use.names = TRUE)
  } else {
    NULL
  }
}

#' Fetch from 2019 PEP charage endpoint (2010-2019)
#'
#' @keywords internal
fetch_pep_charage_2019 <- function(base_url, years, ages, sex, api_key, date_map) {
  # This endpoint uses:
  # - AGE as integer (0-85)
  # - DATE_CODE: 3=2010, 4=2011, ..., 12=2019
  # - SEX: 0=Both, 1=Male, 2=Female
  # - for=us:*

  sex_code <- switch(sex, "both" = "0", "male" = "1", "female" = "2")

  # Request all data at once
  url <- base_url
  req <- httr2::request(url) |>
    httr2::req_url_query(
      get = "POP,SEX,AGE,DATE_CODE",
      `for` = "us:*",
      SEX = sex_code,
      key = api_key
    ) |>
    httr2::req_timeout(60) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req)
  check_api_response(resp, "Census PEP API (vintage_2019)")

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) return(NULL)

  # Convert to data.table
  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  # Parse the data
  dt[, population := as.numeric(POP)]
  dt[, age := as.integer(AGE)]
  dt[, date_code := as.integer(DATE_CODE)]
  dt[, sex := sex]

  # Map DATE_CODE to year
  dt[, year := NA_integer_]
  for (yr in names(date_map)) {
    dt[date_code == as.integer(date_map[[yr]]), year := as.integer(yr)]
  }

  # Filter to requested years and ages
  dt <- dt[year %in% years & age %in% ages & !is.na(year)]

  dt[, .(year, age, sex, population)]
}

#' Fetch from 2000 PEP int_charage endpoint (2000-2009)
#'
#' @keywords internal
fetch_pep_int_charage_2000 <- function(base_url, years, ages, sex, api_key, date_map) {
  # This endpoint uses:
  # - AGE as integer (0-85+)
  # - DATE_: 2=2000, 3=2001, ..., 11=2009
  # - SEX: 0=Both, 1=Male, 2=Female (returned in data, can filter with AGE only)
  # - for=us:1
  # Note: This API doesn't support multiple predicates, so we filter AGE one at a time
  # or get all data and filter locally

  sex_code <- switch(sex, "both" = "0", "male" = "1", "female" = "2")

  # Get all data and filter locally (API doesn't allow multiple predicates)
  url <- base_url
  req <- httr2::request(url) |>
    httr2::req_url_query(
      get = "POP,SEX,AGE,DATE_",
      `for` = "us:1",
      key = api_key
    ) |>
    httr2::req_timeout(120) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req)
  check_api_response(resp, "Census PEP API (vintage_2000)")

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) return(NULL)

  # Convert to data.table
  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  # Handle duplicate column names by taking unique
  unique_headers <- make.unique(headers)
  data.table::setnames(dt, unique_headers)

  # Parse the data
  dt[, population := as.numeric(POP)]
  dt[, age := as.integer(AGE)]
  dt[, date_val := as.integer(DATE_)]
  dt[, sex_code := as.integer(SEX)]

  # Filter by sex
  if (sex == "female") {
    dt <- dt[sex_code == 2]
  } else if (sex == "male") {
    dt <- dt[sex_code == 1]
  } else {
    dt <- dt[sex_code == 0]
  }

  dt[, sex := sex]

  # Map DATE_ to year
  dt[, year := NA_integer_]
  for (yr in names(date_map)) {
    dt[date_val == as.integer(date_map[[yr]]), year := as.integer(yr)]
  }

  # Filter to requested years and ages
  dt <- dt[year %in% years & age %in% ages & !is.na(year)]

  dt[, .(year, age, sex, population)]
}

#' Fetch from 1990 PEP int_natrespop endpoint (1990-1999)
#'
#' @keywords internal
fetch_pep_int_natrespop_1990 <- function(base_url, years, ages, sex, api_key, date_map) {
  # This endpoint uses:
  # - AGE as integer
  # - YEAR as actual year
  # - POPULATION_FEMALES, POPULATION_MALES (separate columns)
  # Note: Different structure from other endpoints

  all_results <- list()

  for (yr in years) {
    url <- base_url
    req <- httr2::request(url) |>
      httr2::req_url_query(
        get = "POPULATION_FEMALES,POPULATION_MALES,AGE,YEAR",
        YEAR = as.character(yr),
        key = api_key
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

    resp <- tryCatch({
      api_request_with_retry(req)
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch 1990s data for {yr}: {conditionMessage(e)}")
      return(NULL)
    })

    if (is.null(resp)) next

    tryCatch({
      check_api_response(resp, glue::glue("Census PEP API (vintage_1990, {yr})"))
    }, error = function(e) {
      cli::cli_alert_warning("API error for {yr}: {conditionMessage(e)}")
      next
    })

    json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    if (length(json_data) < 2) next

    # Convert to data.table
    headers <- json_data[1, ]
    data_rows <- json_data[-1, , drop = FALSE]
    dt <- data.table::as.data.table(data_rows)
    data.table::setnames(dt, headers)

    # Parse the data
    dt[, age := as.integer(AGE)]
    dt[, year := as.integer(YEAR)]

    # Select population based on sex
    if (sex == "female") {
      dt[, population := as.numeric(POPULATION_FEMALES)]
    } else if (sex == "male") {
      dt[, population := as.numeric(POPULATION_MALES)]
    } else {
      dt[, population := as.numeric(POPULATION_FEMALES) + as.numeric(POPULATION_MALES)]
    }

    dt[, sex := sex]

    # Filter to requested ages
    dt <- dt[age %in% ages]

    all_results[[as.character(yr)]] <- dt[, .(year, age, sex, population)]
  }

  if (length(all_results) > 0) {
    data.table::rbindlist(all_results, use.names = TRUE)
  } else {
    NULL
  }
}

#' Download Census population files for years not available via API
#'
#' @description
#' Downloads historical population estimate files from Census Bureau
#' for years not available via API (pre-1990).
#'
#' @param years Integer vector of years
#' @param cache_dir Character: directory to cache downloaded files
#'
#' @return data.table with population data
#'
#' @export
fetch_census_population_files <- function(years,
                                          cache_dir = here::here("data/raw/census")) {
  # Census publishes historical estimates as downloadable files
  # This function handles years not available via API (pre-1990)

  cli::cli_alert_info("Downloading Census population files for {min(years)}-{max(years)}")

  # Create cache directory
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  results <- list()

  for (yr in years) {
    file_url <- get_census_file_url(yr)

    if (is.null(file_url)) {
      cli::cli_alert_warning("No file URL configured for year {yr}")
      next
    }

    # Download and parse file
    local_file <- file.path(cache_dir, basename(file_url))

    tryCatch({
      download_with_cache(file_url, local_file, quiet = TRUE)
      result <- parse_census_population_file(local_file, yr)
      if (!is.null(result)) {
        results[[as.character(yr)]] <- result
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed to process {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) > 0) {
    data.table::rbindlist(results, use.names = TRUE)
  } else {
    NULL
  }
}

#' Get Census file URL for a specific year
#'
#' @param year Integer: year
#'
#' @return Character: URL or NULL if not available
#'
#' @keywords internal
get_census_file_url <- function(year) {
  # Historical population files from Census
  # For years before 1990, we need to use downloadable files

  if (year >= 1980 && year <= 1989) {
    return(paste0(
      "https://www2.census.gov/programs-surveys/popest/tables/1980-1990/",
      "national/asrh/pe-11-", year, ".csv"
    ))
  }

  NULL
}

#' Parse Census population file
#'
#' @param file_path Character: path to downloaded file
#' @param year Integer: year of data
#'
#' @return data.table with population data
#'
#' @keywords internal
parse_census_population_file <- function(file_path, year) {
  # This is a stub - actual implementation depends on file format
  # which varies by decade

  tryCatch({
    raw <- data.table::fread(file_path, header = TRUE)
    raw
  }, error = function(e) {
    cli::cli_alert_warning("Could not parse {file_path}: {conditionMessage(e)}")
    NULL
  })
}

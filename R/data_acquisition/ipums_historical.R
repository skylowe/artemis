#' IPUMS Historical Census Data Acquisition
#'
#' Functions for fetching historical decennial census data from IPUMS USA
#' for marital status distributions (1940-2000). Uses the IPUMS API via
#' the ipumsr package.
#'
#' @name ipums_historical
NULL

#' Fetch historical marital status distributions from IPUMS
#'
#' @description
#' Downloads decennial census microdata from IPUMS USA for historical
#' marital status distributions. This is used for Equation 1.4.2 to
#' calculate historical population by marital status for years before
#' ACS data is available (pre-2006).
#'
#' @param census_years Integer vector of decennial census years
#'   (1940, 1950, 1960, 1970, 1980, 1990, 2000)
#' @param cache_dir Character: directory for caching IPUMS extracts
#' @param wait_for_extract Logical: if TRUE, wait for extract completion.
#'   If FALSE, submit extract and return extract ID for later download.
#' @param timeout_hours Numeric: maximum hours to wait for extract (default: 2)
#'
#' @return data.table with columns: year, age, sex, marital_status, population
#'   where marital_status is: "married", "widowed", "divorced", "separated",
#'   "never_married"
#'
#' @details
#' IPUMS samples used:
#' - us1940a: 1940 1% sample
#' - us1950a: 1950 1% sample
#' - us1960a: 1960 1% sample
#' - us1970a: 1970 Form 1 State sample
#' - us1980a: 1980 5% sample
#' - us1990a: 1990 5% sample
#' - us2000a: 2000 5% sample
#'
#' Variables requested:
#' - AGE: Age
#' - SEX: Sex (1=Male, 2=Female)
#' - MARST: Marital status (1=Married spouse present, 2=Married spouse absent,
#'   3=Separated, 4=Divorced, 5=Widowed, 6=Never married)
#' - PERWT: Person weight
#'
#' Note: IPUMS extracts can take significant time to process (hours).
#' Once downloaded, data is cached locally for future use.
#'
#' @export
fetch_ipums_marital_status <- function(census_years = c(1940, 1950, 1960, 1970, 1980, 1990, 2000),
                                        cache_dir = here::here("data/cache/ipums"),
                                        wait_for_extract = TRUE,
                                        timeout_hours = 2) {

  # Validate inputs
  valid_years <- c(1940, 1950, 1960, 1970, 1980, 1990, 2000)
  checkmate::assert_subset(census_years, valid_years)

  # Check for ipumsr package
  if (!requireNamespace("ipumsr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ipumsr} is required. Install with: renv::install('ipumsr')")
  }

  # Check for API key
  api_key <- Sys.getenv("IPUMS_API_KEY")
  if (nchar(api_key) == 0) {
    cli::cli_abort(c(
      "IPUMS API key not found.",
      "i" = "Set IPUMS_API_KEY in .Renviron file",
      "i" = "Get a key at: https://account.ipums.org/api_keys"
    ))
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Check if we have cached processed data
  cache_file <- file.path(cache_dir, "ipums_marital_status_all.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached IPUMS marital status data")
    data <- readRDS(cache_file)
    # Filter to requested years
    data <- data[year %in% census_years]
    return(data)
  }

  # Define sample IDs for each census year
  sample_ids <- get_ipums_sample_ids(census_years)

  cli::cli_alert_info("IPUMS extract required for {length(census_years)} census years")
  cli::cli_alert_info("Samples: {paste(sample_ids, collapse = ', ')}")

  # Define the extract
  extract <- ipumsr::define_extract_micro(
    collection = "usa",
    description = "ARTEMIS: Historical marital status distributions (1940-2000)",
    samples = sample_ids,
    variables = c("AGE", "SEX", "MARST", "PERWT")
  )

  # Submit the extract
  cli::cli_alert("Submitting IPUMS extract request...")
  submitted <- ipumsr::submit_extract(extract)

  extract_id <- submitted$number
  cli::cli_alert_success("Extract submitted with ID: {extract_id}")

  if (!wait_for_extract) {
    cli::cli_alert_info("Extract processing in background. Use fetch_ipums_extract() to download later.")
    return(invisible(list(extract_id = extract_id, collection = "usa")))
  }

  # Wait for extract completion
  cli::cli_alert("Waiting for IPUMS extract to complete (this may take a while)...")
  timeout_secs <- timeout_hours * 3600

  ready <- ipumsr::wait_for_extract(
    submitted,
    timeout = timeout_secs,
    verbose = TRUE
  )

  if (!ipumsr::is_extract_ready(ready)) {
    cli::cli_abort(c(
      "IPUMS extract timed out after {timeout_hours} hours",
      "i" = "Extract ID: {extract_id}",
      "i" = "Check status at: https://usa.ipums.org/usa-action/extract_requests/download"
    ))
  }

  # Download the extract
  cli::cli_alert("Downloading IPUMS extract...")
  extract_path <- file.path(cache_dir, sprintf("ipums_extract_%d", extract_id))
  dir.create(extract_path, showWarnings = FALSE)

  downloaded <- ipumsr::download_extract(
    ready,
    download_dir = extract_path
  )

  # Read the microdata
  cli::cli_alert("Reading IPUMS microdata...")
  data <- ipumsr::read_ipums_micro(downloaded)

  # Process to aggregated format
  result <- process_ipums_marital_data(data)

  # Cache the processed result
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached processed IPUMS marital status data")

  result[year %in% census_years]
}

#' Get IPUMS sample IDs for census years
#'
#' @param years Integer vector of census years
#' @return Character vector of IPUMS sample IDs
#' @keywords internal
get_ipums_sample_ids <- function(years) {
  sample_map <- c(
    "1940" = "us1940a",  # 1940 1%
    "1950" = "us1950a",  # 1950 1%
    "1960" = "us1960a",  # 1960 1%
    "1970" = "us1970a",  # 1970 Form 1 State
    "1980" = "us1980a",  # 1980 5%
    "1990" = "us1990a",  # 1990 5%
    "2000" = "us2000a"   # 2000 5%
  )
  sample_map[as.character(years)]
}

#' Process IPUMS microdata to aggregated marital status
#'
#' @param data IPUMS microdata tibble
#' @return data.table with aggregated marital status by age, sex, year
#' @keywords internal
process_ipums_marital_data <- function(data) {
  dt <- data.table::as.data.table(data)

  # Map MARST codes to standard categories
  # IPUMS MARST: 1=Married spouse present, 2=Married spouse absent,
  #              3=Separated, 4=Divorced, 5=Widowed, 6=Never married
  dt[MARST == 1, marital_status := "married"]
  dt[MARST == 2, marital_status := "married"]  # Spouse absent still married
  dt[MARST == 3, marital_status := "separated"]
  dt[MARST == 4, marital_status := "divorced"]
  dt[MARST == 5, marital_status := "widowed"]
  dt[MARST == 6, marital_status := "never_married"]

  # Map SEX codes
  dt[SEX == 1, sex := "male"]
  dt[SEX == 2, sex := "female"]

  # Extract year from YEAR variable (should be present in IPUMS data)
  if ("YEAR" %in% names(dt)) {
    dt[, year := as.integer(YEAR)]
  } else if ("SAMPLE" %in% names(dt)) {
    # Extract year from sample ID (e.g., "us1940a" -> 1940)
    dt[, year := as.integer(substr(SAMPLE, 3, 6))]
  }

  # Filter to valid data
  dt <- dt[!is.na(marital_status) & !is.na(sex) & !is.na(AGE) & !is.na(PERWT)]

  # Aggregate by year, age, sex, marital status
  result <- dt[, .(population = sum(PERWT)),
               by = .(year, age = AGE, sex, marital_status)]

  data.table::setorder(result, year, sex, marital_status, age)

  result
}

#' Fetch a previously submitted IPUMS extract
#'
#' @description
#' Downloads a previously submitted IPUMS extract by ID.
#'
#' @param extract_id Integer: the extract ID
#' @param collection Character: IPUMS collection (default: "usa")
#' @param cache_dir Character: directory for caching downloads
#'
#' @return data.table with marital status data
#'
#' @export
fetch_ipums_extract <- function(extract_id,
                                 collection = "usa",
                                 cache_dir = here::here("data/cache/ipums")) {

  if (!requireNamespace("ipumsr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ipumsr} is required")
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Get extract info
  extract_info <- ipumsr::get_extract_info(collection, extract_id)

  if (!ipumsr::is_extract_ready(extract_info)) {
    cli::cli_alert_warning("Extract {extract_id} is not ready yet")
    return(invisible(NULL))
  }

  # Download
  extract_path <- file.path(cache_dir, sprintf("ipums_extract_%d", extract_id))
  dir.create(extract_path, showWarnings = FALSE)

  downloaded <- ipumsr::download_extract(
    extract_info,
    download_dir = extract_path
  )

  # Read and process
  data <- ipumsr::read_ipums_micro(downloaded)
  result <- process_ipums_marital_data(data)

  # Cache
  cache_file <- file.path(cache_dir, "ipums_marital_status_all.rds")
  saveRDS(result, cache_file)

  result
}

#' Get IPUMS extract status
#'
#' @description
#' Checks the status of a submitted IPUMS extract.
#'
#' @param extract_id Integer: the extract ID
#' @param collection Character: IPUMS collection (default: "usa")
#'
#' @return List with extract status information
#'
#' @export
get_ipums_extract_status <- function(extract_id, collection = "usa") {
  if (!requireNamespace("ipumsr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ipumsr} is required")
  }

  info <- ipumsr::get_extract_info(collection, extract_id)

  list(
    extract_id = extract_id,
    collection = collection,
    is_ready = ipumsr::is_extract_ready(info),
    status = info$status
  )
}

#' Fetch historical marriage grids from IPUMS
#'
#' @description
#' Creates marriage grids (husband age × wife age) from historical
#' decennial census data. Requires household-level linking.
#'
#' @param census_years Integer vector of decennial census years
#' @param min_age Minimum age to include (default: 15)
#' @param max_age Maximum age to include (default: 99)
#' @param cache_dir Character: directory for caching
#'
#' @return list of matrices by census year
#'
#' @details
#' This function requires additional IPUMS variables beyond basic
#' marital status: SPLOC (spouse location), RELATE (relationship to head).
#'
#' Marriage grids are created by linking married persons to their spouses
#' within households.
#'
#' Note: For pre-ACS years, marriage grid construction is more complex
#' and may require manual extract creation with specific household variables.
#'
#' @export
fetch_ipums_marriage_grids <- function(census_years = c(1940, 1950, 1960, 1970, 1980, 1990, 2000),
                                        min_age = 15,
                                        max_age = 99,
                                        cache_dir = here::here("data/cache/ipums")) {

  cli::cli_alert_warning(
    "IPUMS marriage grid extraction requires additional variables (SPLOC, RELATE)"
  )
  cli::cli_alert_info("This function is a placeholder - implementation pending")

  # Return empty list for now
  # Full implementation would require:
  # 1. Define extract with SPLOC, RELATE, SERIAL (household ID)
  # 2. Link spouses within households
  # 3. Create age grids

  invisible(list())
}

#' Calculate marital status proportions from IPUMS data
#'
#' @description
#' Converts IPUMS population counts to proportions within each age-sex group.
#'
#' @param ipums_data data.table from fetch_ipums_marital_status
#'
#' @return data.table with marital_proportion column added
#'
#' @export
calculate_ipums_marital_proportions <- function(ipums_data) {
  result <- data.table::copy(ipums_data)

  # Calculate total by year, age, sex
  totals <- result[, .(total = sum(population)), by = .(year, age, sex)]

  # Merge and calculate proportion

  result <- result[totals, on = .(year, age, sex)]
  result[, marital_proportion := population / total]
  result[, total := NULL]

  data.table::setorder(result, year, sex, age, marital_status)

  result
}

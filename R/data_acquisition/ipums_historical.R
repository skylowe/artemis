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
#'   "single"
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
  dt[MARST == 6, marital_status := "single"]

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

  # Get extract info - format as "collection:number" string
  extract_str <- paste0(collection, ":", extract_id)
  extract_info <- ipumsr::get_extract_info(extract_str)

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

  # Format as "collection:number" string
  extract_str <- paste0(collection, ":", extract_id)
  info <- ipumsr::get_extract_info(extract_str)

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
#' decennial census data. Uses SPLOC (spouse location) to link spouses.
#'
#' @param census_years Integer vector of decennial census years
#' @param min_age Minimum age to include (default: 15)
#' @param max_age Maximum age to include (default: 99)
#' @param cache_dir Character: directory for caching
#' @param wait_for_extract Logical: if TRUE, wait for extract completion
#' @param timeout_hours Numeric: maximum hours to wait (default: 2)
#'
#' @return list of matrices by census year, each with rows = husband age,
#'   cols = wife age
#'
#' @details
#' Uses IPUMS variables:
#' - AGE: Person's age
#' - SEX: Person's sex
#' - MARST: Marital status (filter to married)
#' - SPLOC: Spouse's location (person number within household)
#' - SERIAL: Household serial number
#' - PERNUM: Person number within household
#' - PERWT: Person weight
#'
#' Marriage grids are created by:
#' 1. Filtering to married persons (MARST = 1 or 2)
#' 2. Linking to spouse using SPLOC within household (SERIAL)
#' 3. Creating age grid from husband-wife pairs
#'
#' @export
fetch_ipums_marriage_grids <- function(census_years = c(1940, 1950, 1960, 1970, 1980, 1990, 2000),
                                        min_age = 15,
                                        max_age = 99,
                                        cache_dir = here::here("data/cache/ipums"),
                                        wait_for_extract = TRUE,
                                        timeout_hours = 2) {

  # Check for cached processed data
  cache_file <- file.path(cache_dir, "ipums_marriage_grids_all.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached IPUMS marriage grids")
    grids <- readRDS(cache_file)
    # Filter to requested years
    requested_years <- as.character(census_years)
    return(grids[names(grids) %in% requested_years])
  }

  # Check for ipumsr
 if (!requireNamespace("ipumsr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ipumsr} is required")
  }

  # Check API key
  api_key <- Sys.getenv("IPUMS_API_KEY")
  if (nchar(api_key) == 0) {
    cli::cli_abort("IPUMS_API_KEY not found in environment")
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Check for pending marriage grid extract
  pending_file <- file.path(cache_dir, "pending_marriage_grid_extract.rds")
  if (file.exists(pending_file)) {
    pending <- readRDS(pending_file)
    cli::cli_alert_info("Found pending marriage grid extract (ID: {pending$extract_id})")

    # Check if ready
    status <- get_ipums_extract_status(pending$extract_id)
    if (status$is_ready) {
      cli::cli_alert_success("Extract is ready - downloading...")
      return(download_and_process_marriage_grids(
        pending$extract_id, census_years, min_age, max_age, cache_dir
      ))
    } else {
      cli::cli_alert_warning("Extract still processing (status: {status$status})")
      if (!wait_for_extract) {
        return(invisible(list(extract_id = pending$extract_id, status = "processing")))
      }
    }
  }

  # Define sample IDs
  sample_ids <- get_ipums_sample_ids(census_years)

  cli::cli_alert_info("Defining IPUMS marriage grid extract...")
  cli::cli_alert_info("Samples: {paste(sample_ids, collapse = ', ')}")

  # Define extract with marriage linking variables
  # SPLOC = spouse's person number within household
  # SERIAL = household ID, PERNUM = person number
  extract <- ipumsr::define_extract_micro(
    collection = "usa",
    description = "ARTEMIS: Historical marriage grids (1940-2000)",
    samples = sample_ids,
    variables = c("AGE", "SEX", "MARST", "SPLOC", "SERIAL", "PERNUM", "PERWT")
  )

  # Submit
  cli::cli_alert("Submitting IPUMS marriage grid extract...")
  submitted <- ipumsr::submit_extract(extract)

  extract_id <- submitted$number
  cli::cli_alert_success("Extract submitted with ID: {extract_id}")

  # Save pending info
  saveRDS(
    list(extract_id = extract_id, submitted_at = Sys.time(), samples = sample_ids),
    pending_file
  )

  if (!wait_for_extract) {
    cli::cli_alert_info("Extract processing in background")
    return(invisible(list(extract_id = extract_id, status = "submitted")))
  }

  # Wait for completion
  cli::cli_alert("Waiting for IPUMS extract (this may take a while)...")
  timeout_secs <- timeout_hours * 3600

  ready <- ipumsr::wait_for_extract(submitted, timeout = timeout_secs, verbose = TRUE)

  if (!ipumsr::is_extract_ready(ready)) {
    cli::cli_abort("IPUMS extract timed out after {timeout_hours} hours")
  }

  # Download and process
  download_and_process_marriage_grids(extract_id, census_years, min_age, max_age, cache_dir)
}

#' Download and process IPUMS marriage grid extract
#'
#' @keywords internal
download_and_process_marriage_grids <- function(extract_id, census_years, min_age, max_age, cache_dir) {

  cli::cli_alert("Downloading IPUMS marriage grid extract...")

  extract_str <- paste0("usa:", extract_id)
  extract_info <- ipumsr::get_extract_info(extract_str)
  extract_path <- file.path(cache_dir, sprintf("marriage_grid_extract_%d", extract_id))
  dir.create(extract_path, showWarnings = FALSE)

  downloaded <- ipumsr::download_extract(extract_info, download_dir = extract_path)

  cli::cli_alert("Reading IPUMS microdata...")
  data <- ipumsr::read_ipums_micro(downloaded)

  # Process to marriage grids
  cli::cli_alert("Processing marriage grids...")
  grids <- process_ipums_marriage_grids(data, census_years, min_age, max_age)

  # Cache
  cache_file <- file.path(cache_dir, "ipums_marriage_grids_all.rds")
  saveRDS(grids, cache_file)

  # Remove pending file
  pending_file <- file.path(cache_dir, "pending_marriage_grid_extract.rds")
  if (file.exists(pending_file)) file.remove(pending_file)

  cli::cli_alert_success("Cached IPUMS marriage grids for {length(grids)} census years")

  grids
}

#' Process IPUMS microdata to marriage grids
#'
#' @param data IPUMS microdata tibble
#' @param census_years Years to process
#' @param min_age Minimum age
#' @param max_age Maximum age
#' @return list of matrices by year
#' @keywords internal
process_ipums_marriage_grids <- function(data, census_years, min_age, max_age) {
  dt <- data.table::as.data.table(data)

  # Get year from YEAR variable
  if ("YEAR" %in% names(dt)) {
    dt[, year := as.integer(YEAR)]
  }

  # Filter to married persons only (MARST = 1 spouse present, 2 spouse absent)
  dt <- dt[MARST %in% c(1, 2)]

  # Filter to valid ages
  dt <- dt[AGE >= min_age & AGE <= max_age]

  # Filter to those with a spouse in household (SPLOC > 0)
  # SPLOC = 0 means no spouse in household
  dt <- dt[SPLOC > 0]

  # Map sex
  dt[SEX == 1, sex := "male"]
  dt[SEX == 2, sex := "female"]

  results <- list()

  for (yr in census_years) {
    cli::cli_alert("Processing {yr} marriage grid...")

    year_data <- dt[year == yr]

    if (nrow(year_data) == 0) {
      cli::cli_alert_warning("No data for {yr}")
      next
    }

    # Create household-person key for linking
    year_data[, hh_key := paste(SERIAL, PERNUM, sep = "_")]
    year_data[, spouse_key := paste(SERIAL, SPLOC, sep = "_")]

    # Self-join to link to spouse
    # Each person has a spouse_key pointing to their spouse's hh_key
    spouse_data <- year_data[, .(spouse_key = hh_key, spouse_age = AGE, spouse_sex = sex)]

    linked <- merge(
      year_data[, .(hh_key, spouse_key, age = AGE, sex, weight = PERWT)],
      spouse_data,
      by = "spouse_key",
      all.x = FALSE
    )

    # Filter to husband-wife pairs (male linked to female spouse)
    couples <- linked[sex == "male" & spouse_sex == "female",
                      .(husband_age = age, wife_age = spouse_age, weight)]

    if (nrow(couples) == 0) {
      cli::cli_alert_warning("No couples found for {yr}")
      next
    }

    # Aggregate by husband-wife age
    grid_data <- couples[, .(marriages = sum(weight)), by = .(husband_age, wife_age)]

    # Create matrix
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

    # Add metadata
    attr(grid_matrix, "year") <- yr
    attr(grid_matrix, "total_marriages") <- sum(grid_matrix)
    attr(grid_matrix, "min_age") <- min_age
    attr(grid_matrix, "max_age") <- max_age

    results[[as.character(yr)]] <- grid_matrix

    cli::cli_alert_success("{yr}: {format(sum(grid_matrix), big.mark = ',')} marriages")
  }

  results
}

#' Submit IPUMS marriage grid extract without waiting
#'
#' @description
#' Submits an IPUMS extract for marriage grid data without waiting for completion.
#' Use get_ipums_extract_status() to check progress and fetch_ipums_marriage_grids()
#' to download when ready.
#'
#' @param census_years Integer vector of decennial census years
#' @param cache_dir Character: directory for caching
#'
#' @return List with extract_id
#'
#' @export
submit_ipums_marriage_grid_extract <- function(census_years = c(1940, 1950, 1960, 1970, 1980, 1990, 2000),
                                                cache_dir = here::here("data/cache/ipums")) {
  fetch_ipums_marriage_grids(
    census_years = census_years,
    cache_dir = cache_dir,
    wait_for_extract = FALSE
  )
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

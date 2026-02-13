#' IPUMS CPS Data Acquisition
#'
#' Functions for fetching Current Population Survey data from IPUMS CPS
#' for historical unmarried population estimates (1957-1995).
#'
#' This data is used in the MARRIAGE subprocess (Item 14) for MRA
#' (Marriage Registration Area) adjustment calculations.
#'
#' @name ipums_cps
NULL

# =============================================================================
# CONSTANTS
# =============================================================================

#' TR2025 age groups for marriage data
#' @keywords internal
CPS_MARRIAGE_AGE_GROUPS <- list(
  "14-19" = 14:19,
  "20-24" = 20:24,
  "25-29" = 25:29,
  "30-34" = 30:34,
  "35-44" = 35:44,
  "45-54" = 45:54,
  "55-64" = 55:64,
  "65+"   = 65:120
)

# =============================================================================
# MAIN FUNCTIONS
# =============================================================================

#' Fetch CPS unmarried population (1962-1995)
#'
#' @description
#' Downloads March CPS (ASEC) microdata from IPUMS CPS for historical
#' unmarried population estimates. This is Item 14 from TR2025 documentation,
#' used for MRA adjustment calculations.
#'
#' Note: IPUMS CPS ASEC data begins in 1962. TR2025 documentation mentions
#' 1957-1995, but IPUMS data is only available from 1962 onwards.
#'
#' @param years Integer vector of years (1962-1995). Default is all available years.
#' @param cache_dir Character: directory for caching IPUMS extracts
#' @param wait_for_extract Logical: if TRUE, wait for extract completion.
#'   If FALSE, submit extract and return extract ID for later download.
#' @param timeout_hours Numeric: maximum hours to wait for extract (default: 4)
#'
#' @return data.table with columns: year, age_group, sex, unmarried_population
#'   where age_group is one of: "14-19", "20-24", "25-29", "30-34",
#'   "35-44", "45-54", "55-64", "65+"
#'
#' @details
#' Uses IPUMS CPS March/ASEC supplements. Variables requested:
#' - AGE: Age
#' - SEX: Sex (1=Male, 2=Female)
#' - MARST: Marital status
#' - ASECWT: ASEC person weight (or WTFINL for earlier years)
#'
#' Note: IPUMS extracts can take significant time to process (hours).
#' Once downloaded, data is cached locally for future use.
#'
#' The documentation notes "These data are not updated" - this is historical
#' static data used only for the marriage subprocess.
#'
#' @export
fetch_cps_unmarried_population <- function(
    years = 1962:1995,
    cache_dir = here::here("data/cache/ipums_cps"),
    wait_for_extract = TRUE,
    timeout_hours = 4
) {
  # Validate inputs - IPUMS CPS ASEC starts in 1962
  checkmate::assert_integerish(years, lower = 1962, upper = 1995, min.len = 1)

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
  cache_file <- file.path(cache_dir, "cps_unmarried_1957_1995.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached CPS unmarried population data")
    data <- readRDS(cache_file)
    # Filter to requested years
    data <- data[year %in% years]
    return(data)
  }

  # Get sample IDs for CPS March supplements
  sample_ids <- get_cps_march_sample_ids(years)

  cli::cli_alert_info("IPUMS CPS extract required for {length(years)} years")
  cli::cli_alert_info("This will download March CPS supplements from 1957-1995")

  # Define the extract
  # Note: IPUMS CPS uses different variable names than USA
  # ASECWT is the weight for ASEC samples, WTFINL for basic monthly
  extract <- ipumsr::define_extract_micro(
    collection = "cps",
    description = "ARTEMIS: CPS unmarried population 1957-1995 (Marriage Item 14)",
    samples = sample_ids,
    variables = c("AGE", "SEX", "MARST", "ASECWT")
  )

  # Submit the extract
  cli::cli_alert("Submitting IPUMS CPS extract request...")
  submitted <- ipumsr::submit_extract(extract)

  extract_id <- submitted$number
  cli::cli_alert_success("Extract submitted with ID: {extract_id}")

  # Save pending info
  pending_file <- file.path(cache_dir, "pending_cps_extract.rds")
  saveRDS(
    list(extract_id = extract_id, submitted_at = Sys.time(), years = years),
    pending_file
  )

  if (!wait_for_extract) {
    cli::cli_alert_info("Extract processing in background. Use fetch_cps_extract() to download later.")
    return(invisible(list(extract_id = extract_id, collection = "cps")))
  }

  # Wait for extract completion
  cli::cli_alert("Waiting for IPUMS CPS extract to complete (this may take a while)...")
  timeout_secs <- timeout_hours * 3600

  ready <- ipumsr::wait_for_extract(
    submitted,
    timeout = timeout_secs,
    verbose = TRUE
  )

  if (!ipumsr::is_extract_ready(ready)) {
    cli::cli_abort(c(
      "IPUMS CPS extract timed out after {timeout_hours} hours",
      "i" = "Extract ID: {extract_id}",
      "i" = "Check status at: https://cps.ipums.org/cps-action/extract_requests"
    ))
  }

  # Download and process
  download_and_process_cps_unmarried(extract_id, years, cache_dir)
}

#' Get IPUMS CPS March/ASEC sample IDs
#'
#' @description
#' Returns IPUMS CPS sample identifiers for March (ASEC) supplements.
#'
#' @param years Integer vector of years
#' @return Character vector of IPUMS CPS sample IDs
#' @keywords internal
get_cps_march_sample_ids <- function(years) {
  # IPUMS CPS March/ASEC sample naming convention:
 # Pre-1976: cpsYYYY_03s (March supplement)
  # 1976+: March ASEC samples
  # Format varies by year - IPUMS uses "cps{year}_03s" for March supplements

  sample_ids <- sprintf("cps%d_03s", years)

  # Some years may have different sample IDs - check IPUMS documentation
  # For 1962 and 1963, data may be limited
  # Note: Not all years have March supplements - IPUMS will error on invalid

  sample_ids
}

#' Download and process CPS unmarried extract
#'
#' @keywords internal
download_and_process_cps_unmarried <- function(extract_id, years, cache_dir) {
  cli::cli_alert("Downloading IPUMS CPS extract...")

  extract_str <- paste0("cps:", extract_id)
  extract_info <- ipumsr::get_extract_info(extract_str)
  extract_path <- file.path(cache_dir, sprintf("cps_extract_%d", extract_id))
  dir.create(extract_path, showWarnings = FALSE)

  downloaded <- ipumsr::download_extract(extract_info, download_dir = extract_path)

  cli::cli_alert("Reading IPUMS CPS microdata...")
  data <- ipumsr::read_ipums_micro(downloaded)

  # Process to unmarried population by age group
  cli::cli_alert("Processing unmarried population by age group...")
  result <- process_cps_unmarried_data(data, years)

  # Cache
  cache_file <- file.path(cache_dir, "cps_unmarried_1957_1995.rds")
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached CPS unmarried population data")

  # Remove pending file
  pending_file <- file.path(cache_dir, "pending_cps_extract.rds")
  if (file.exists(pending_file)) file.remove(pending_file)

  result
}

#' Process CPS microdata to unmarried population by age group
#'
#' @param data IPUMS CPS microdata tibble
#' @param years Years to process
#' @return data.table with unmarried population by age group and sex
#' @keywords internal
process_cps_unmarried_data <- function(data, years) {
  dt <- data.table::as.data.table(data)

  # Get year from YEAR variable
  if ("YEAR" %in% names(dt)) {
    dt[, year := as.integer(YEAR)]
  }

  # Filter to requested years
  dt <- dt[year %in% years]

  # Map MARST codes to married/unmarried
 # IPUMS MARST: 1=Married spouse present, 2=Married spouse absent,
  #              3=Separated, 4=Divorced, 5=Widowed, 6=Never married
  # Unmarried = 3, 4, 5, 6 (separated, divorced, widowed, never married)
  dt[, is_unmarried := MARST %in% c(3, 4, 5, 6)]

  # Map SEX codes
  dt[SEX == 1, sex := "male"]
  dt[SEX == 2, sex := "female"]

  # Assign age groups
  dt[, age_group := NA_character_]
  for (grp_name in names(CPS_MARRIAGE_AGE_GROUPS)) {
    ages <- CPS_MARRIAGE_AGE_GROUPS[[grp_name]]
    dt[AGE %in% ages, age_group := grp_name]
  }

  # Filter to valid data
  dt <- dt[!is.na(age_group) & !is.na(sex) & is_unmarried == TRUE]

  # Use appropriate weight variable
  weight_var <- if ("ASECWT" %in% names(dt)) "ASECWT" else "WTFINL"

  if (!(weight_var %in% names(dt))) {
    cli::cli_alert_warning("Weight variable not found, using unweighted counts")
    dt[, weight := 1]
  } else {
    dt[, weight := get(weight_var)]
  }

  # Aggregate by year, age group, sex
  result <- dt[, .(unmarried_population = sum(weight, na.rm = TRUE)),
               by = .(year, age_group, sex)]

  # Order age groups properly
  age_group_order <- names(CPS_MARRIAGE_AGE_GROUPS)
  result[, age_group := factor(age_group, levels = age_group_order)]
  data.table::setorder(result, year, sex, age_group)
  result[, age_group := as.character(age_group)]

  result
}

#' Fetch a previously submitted CPS extract
#'
#' @description
#' Downloads a previously submitted IPUMS CPS extract by ID.
#'
#' @param extract_id Integer: the extract ID
#' @param cache_dir Character: directory for caching downloads
#'
#' @return data.table with unmarried population data
#'
#' @export
fetch_cps_extract <- function(extract_id,
                               cache_dir = here::here("data/cache/ipums_cps")) {

  if (!requireNamespace("ipumsr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ipumsr} is required")
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Get extract info
  extract_str <- paste0("cps:", extract_id)
  extract_info <- ipumsr::get_extract_info(extract_str)

  if (!ipumsr::is_extract_ready(extract_info)) {
    cli::cli_alert_warning("Extract {extract_id} is not ready yet")
    status <- get_cps_extract_status(extract_id)
    cli::cli_alert_info("Status: {status$status}")
    return(invisible(NULL))
  }

  # Download and process
  download_and_process_cps_unmarried(extract_id, 1957:1995, cache_dir)
}

#' Get CPS extract status
#'
#' @description
#' Checks the status of a submitted IPUMS CPS extract.
#'
#' @param extract_id Integer: the extract ID
#'
#' @return List with extract status information
#'
#' @export
get_cps_extract_status <- function(extract_id) {
  if (!requireNamespace("ipumsr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ipumsr} is required")
  }

  extract_str <- paste0("cps:", extract_id)
  info <- ipumsr::get_extract_info(extract_str)

  list(
    extract_id = extract_id,
    collection = "cps",
    is_ready = ipumsr::is_extract_ready(info),
    status = info$status
  )
}

#' Submit CPS extract without waiting
#'
#' @description
#' Submits an IPUMS CPS extract without waiting for completion.
#' Use get_cps_extract_status() to check progress and fetch_cps_extract()
#' to download when ready.
#'
#' @param years Integer vector of years (default: 1962:1995)
#' @param cache_dir Character: directory for caching
#'
#' @return List with extract_id
#'
#' @export
submit_cps_unmarried_extract <- function(years = 1962:1995,
                                          cache_dir = here::here("data/cache/ipums_cps")) {
  fetch_cps_unmarried_population(
    years = years,
    cache_dir = cache_dir,
    wait_for_extract = FALSE
  )
}

#' Extract CPS unmarried population by prior marital status
#'
#' @description
#' Loads the existing CPS extract and disaggregates the unmarried population
#' by prior marital status (single/never-married, divorced, widowed).
#'
#' Used for TR2025 Step 11: computing rate-based prior status differentials.
#' TR2025 uses MRA-specific population (Input #10), but CPS national data is
#' the best publicly available substitute.
#'
#' @param years Integer vector: years to extract (default: c(1979, 1981:1988)
#'   matching Input #9 prior status marriage years)
#' @param cache_dir Character: directory containing the CPS extract
#'
#' @return data.table with columns: year, age_group, sex, prior_status, unmarried_population
#'
#' @export
extract_cps_unmarried_by_prior_status <- function(
    years = c(1979, 1981:1988),
    cache_dir = here::here("data/cache/ipums_cps")
) {
  # Check for cached result
  cache_file <- file.path(cache_dir, "cps_unmarried_by_prior_status.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached CPS unmarried population by prior status")
    data <- readRDS(cache_file)
    return(data[year %in% years])
  }

  # Load raw CPS extract
  extract_path <- file.path(cache_dir, "cps_extract_2")
  xml_file <- file.path(extract_path, "cps_00002.xml")

  if (!file.exists(xml_file)) {
    cli::cli_abort(c(
      "CPS extract not found at {.path {extract_path}}",
      "i" = "Run fetch_cps_unmarried_population() first to download the extract"
    ))
  }

  if (!requireNamespace("ipumsr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ipumsr} is required")
  }

  cli::cli_alert_info("Reading CPS microdata for prior status disaggregation...")
  data <- ipumsr::read_ipums_micro(xml_file)
  dt <- data.table::as.data.table(data)

  # Filter to requested years
  if ("YEAR" %in% names(dt)) {
    dt[, year := as.integer(YEAR)]
  }
  dt <- dt[year %in% years]

  # Map MARST to prior marital status
  # IPUMS MARST: 1=Married spouse present, 2=Married spouse absent,
  #              3=Separated, 4=Divorced, 5=Widowed, 6=Never married
  # Unmarried prior statuses:
  #   single (never-married) = 6
  #   divorced (incl separated) = 3, 4
  #   widowed = 5
  dt[, prior_status := NA_character_]
  dt[MARST == 6, prior_status := "single"]
  dt[MARST %in% c(3, 4), prior_status := "divorced"]
  dt[MARST == 5, prior_status := "widowed"]

  # Filter to unmarried only
  dt <- dt[!is.na(prior_status)]

  # Map SEX codes
  dt[SEX == 1, sex := "male"]
  dt[SEX == 2, sex := "female"]

  # Assign age groups (same as CPS_MARRIAGE_AGE_GROUPS)
  dt[, age_group := NA_character_]
  for (grp_name in names(CPS_MARRIAGE_AGE_GROUPS)) {
    ages <- CPS_MARRIAGE_AGE_GROUPS[[grp_name]]
    dt[AGE %in% ages, age_group := grp_name]
  }

  # Filter to valid data
  dt <- dt[!is.na(age_group) & !is.na(sex)]

  # Use appropriate weight variable
  weight_var <- if ("ASECWT" %in% names(dt)) "ASECWT" else "WTFINL"
  if (!(weight_var %in% names(dt))) {
    cli::cli_warn("Weight variable not found, using unweighted counts")
    dt[, weight := 1]
  } else {
    dt[, weight := get(weight_var)]
  }

  # Aggregate by year, age group, sex, prior status
  result <- dt[, .(unmarried_population = sum(weight, na.rm = TRUE)),
               by = .(year, age_group, sex, prior_status)]

  # Order properly
  age_group_order <- names(CPS_MARRIAGE_AGE_GROUPS)
  result[, age_group := factor(age_group, levels = age_group_order)]
  data.table::setorder(result, year, sex, age_group, prior_status)
  result[, age_group := as.character(age_group)]

  cli::cli_alert_success("Extracted CPS unmarried population by prior status: {nrow(result)} rows")
  cli::cli_alert_info("Years: {paste(sort(unique(result$year)), collapse=', ')}")

  # Cache
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached to {.path {cache_file}}")

  result
}

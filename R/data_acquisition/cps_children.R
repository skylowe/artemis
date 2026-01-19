#' CPS Children Per Married Couple Data Acquisition
#'
#' @description
#' Functions for fetching Current Population Survey data from IPUMS CPS for
#' historical mean number of children per married couple by age of householder.
#'
#' This data is used in the PROJECTED POPULATION subprocess (Equation 1.8.6)
#' for projecting children by parent survival status.
#'
#' TR2025 Documentation Reference:
#' "CPS data on the average number of children per married couple with children
#' by age group of householder (age groups 20-24, 25-29, 30-34, 35-39, 40-44,
#' 45-49, 50-54, and 55-64) for 1960-2022."
#'
#' Note: IPUMS CPS March/ASEC supplements begin in 1962, so we use 1962-2022.
#'
#' @name cps_children
NULL

# =============================================================================
# CONSTANTS
# =============================================================================

#' Age groups for CPS children data
#' @keywords internal
CPS_CHILDREN_AGE_GROUPS <- list(
  "20-24" = 20:24,
  "25-29" = 25:29,
  "30-34" = 30:34,
  "35-39" = 35:39,
  "40-44" = 40:44,
  "45-49" = 45:49,
  "50-54" = 50:54,
  "55-59" = 55:59,  # Split from 55-64

"60-64" = 60:64   # Split from 55-64
)

# =============================================================================
# MAIN FUNCTIONS
# =============================================================================

#' Fetch CPS mean children per married couple (1962-2022)
#'
#' @description
#' Downloads March CPS (ASEC) microdata from IPUMS CPS for calculating
#' the average number of children per married couple with children by
#' age group of householder.
#'
#' This is Input Item 29 from TR2025 documentation.
#'
#' @param years Integer vector of years (1962-2022). Default is all available years.
#' @param cache_dir Character: directory for caching IPUMS extracts
#' @param wait_for_extract Logical: if TRUE, wait for extract completion.
#' @param timeout_hours Numeric: maximum hours to wait for extract (default: 4)
#'
#' @return data.table with columns: year, age_group, mean_children, n_couples
#'   where age_group is one of the CPS_CHILDREN_AGE_GROUPS
#'
#' @details
#' Uses IPUMS CPS March/ASEC supplements. Variables requested:
#' - AGE: Age of householder
#' - SEX: Sex
#' - MARST: Marital status (filtering to married couples)
#' - NCHILD: Number of own children in household
#' - RELATE: Relationship to householder
#' - ASECWT: ASEC person weight
#'
#' @export
fetch_cps_children_per_couple <- function(
    years = 1962:2022,
    cache_dir = here::here("data/cache/ipums_cps"),
    wait_for_extract = TRUE,
    timeout_hours = 4
) {
  # Validate inputs
  checkmate::assert_integerish(years, lower = 1962, upper = 2022, min.len = 1)

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

  # Check for cached processed data
  cache_file <- file.path(cache_dir, "cps_children_per_couple_1962_2022.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached CPS children per couple data")
    data <- readRDS(cache_file)
    # Filter to requested years
    data <- data[year %in% years]
    return(data)
  }

  # Get sample IDs for CPS March supplements
  sample_ids <- get_cps_march_sample_ids_children(years)

  cli::cli_alert_info("IPUMS CPS extract required for {length(years)} years")
  cli::cli_alert_info("This will download March CPS supplements from 1962-2022")

  # Define the extract
  # Note: Need household-level variables for children count
  extract <- ipumsr::define_extract_micro(
    collection = "cps",
    description = "ARTEMIS: CPS children per married couple 1962-2022 (Projected Population Item 29)",
    samples = sample_ids,
    variables = c("YEAR", "AGE", "SEX", "MARST", "NCHILD", "RELATE", "ASECWT", "SERIAL", "PERNUM")
  )

  # Submit the extract
  cli::cli_alert("Submitting IPUMS CPS extract request...")
  submitted <- ipumsr::submit_extract(extract)

  extract_id <- submitted$number
  cli::cli_alert_success("Extract submitted with ID: {extract_id}")

  # Save pending info
  pending_file <- file.path(cache_dir, "pending_cps_children_extract.rds")
  saveRDS(
    list(extract_id = extract_id, submitted_at = Sys.time(), years = years),
    pending_file
  )

  if (!wait_for_extract) {
    cli::cli_alert_info("Extract processing in background. Use fetch_cps_children_extract() to download later.")
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
  download_and_process_cps_children(extract_id, years, cache_dir)
}

#' Get IPUMS CPS March/ASEC sample IDs for children data
#'
#' @param years Integer vector of years
#' @return Character vector of IPUMS CPS sample IDs
#' @keywords internal
get_cps_march_sample_ids_children <- function(years) {
  # IPUMS CPS March/ASEC sample naming convention
  # Format: cps{year}_03s for March supplements
  sample_ids <- sprintf("cps%d_03s", years)
  sample_ids
}

#' Download and process CPS children extract
#'
#' @keywords internal
download_and_process_cps_children <- function(extract_id, years, cache_dir) {
  cli::cli_alert("Downloading IPUMS CPS extract...")

  extract_str <- paste0("cps:", extract_id)
  extract_info <- ipumsr::get_extract_info(extract_str)
  extract_path <- file.path(cache_dir, sprintf("cps_children_extract_%d", extract_id))
  dir.create(extract_path, showWarnings = FALSE)

  downloaded <- ipumsr::download_extract(extract_info, download_dir = extract_path)

  cli::cli_alert("Reading IPUMS CPS microdata...")
  data <- ipumsr::read_ipums_micro(downloaded)

  # Process to mean children per couple by age group
  cli::cli_alert("Processing mean children per couple by age group...")
  result <- process_cps_children_data(data, years)

  # Cache
  cache_file <- file.path(cache_dir, "cps_children_per_couple_1962_2022.rds")
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached CPS children per couple data")

  # Remove pending file
  pending_file <- file.path(cache_dir, "pending_cps_children_extract.rds")
  if (file.exists(pending_file)) file.remove(pending_file)

  result
}

#' Process CPS microdata to mean children per couple by age group
#'
#' @description
#' Calculates the average number of children per married couple with children
#' by age group of householder.
#'
#' @param data IPUMS CPS microdata tibble
#' @param years Years to process
#' @return data.table with mean children by age group and year
#' @keywords internal
process_cps_children_data <- function(data, years) {
  dt <- data.table::as.data.table(data)

  # Get year from YEAR variable
  if ("YEAR" %in% names(dt)) {
    dt[, year := as.integer(YEAR)]
  }

  # Filter to requested years
  dt <- dt[year %in% years]

  # Filter to householders only (RELATE == 1)
  # IPUMS RELATE: 0101 = Head/Householder (labeled codes vary by year)
  dt <- dt[RELATE == 101 | RELATE == 1 | RELATE == 0101]

  # Filter to married couples (spouse present)
  # IPUMS MARST: 1 = Married, spouse present
  dt <- dt[MARST == 1]

  # Filter to householders with children (NCHILD > 0)
  dt <- dt[NCHILD > 0]

  # Filter to age range for householders (20-64)
  dt <- dt[AGE >= 20 & AGE <= 64]

  # Assign age groups
  dt[, age_group := NA_character_]
  for (grp_name in names(CPS_CHILDREN_AGE_GROUPS)) {
    ages <- CPS_CHILDREN_AGE_GROUPS[[grp_name]]
    dt[AGE %in% ages, age_group := grp_name]
  }

  # Filter to valid age groups
  dt <- dt[!is.na(age_group)]

  # Use appropriate weight variable
  weight_var <- if ("ASECWT" %in% names(dt)) "ASECWT" else "WTFINL"

  if (!(weight_var %in% names(dt))) {
    cli::cli_alert_warning("Weight variable not found, using unweighted counts")
    dt[, weight := 1]
  } else {
    dt[, weight := get(weight_var)]
  }

  # Calculate weighted mean children per couple by year and age group
  result <- dt[, .(
    mean_children = sum(NCHILD * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    n_couples = sum(weight, na.rm = TRUE),
    n_unweighted = .N
  ), by = .(year, age_group)]

  # Order age groups properly
  age_group_order <- names(CPS_CHILDREN_AGE_GROUPS)
  result[, age_group := factor(age_group, levels = age_group_order)]
  data.table::setorder(result, year, age_group)
  result[, age_group := as.character(age_group)]

  cli::cli_alert_success(
    "Processed {nrow(result)} year × age_group combinations"
  )
  cli::cli_alert_info(
    "Mean children range: {round(min(result$mean_children), 2)} - {round(max(result$mean_children), 2)}"
  )

  result
}

#' Fetch a previously submitted CPS children extract
#'
#' @description
#' Downloads a previously submitted IPUMS CPS extract by ID.
#'
#' @param extract_id Integer: the extract ID
#' @param cache_dir Character: directory for caching downloads
#'
#' @return data.table with children per couple data
#'
#' @export
fetch_cps_children_extract <- function(extract_id,
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
    status <- get_cps_children_extract_status(extract_id)
    cli::cli_alert_info("Status: {status$status}")
    return(invisible(NULL))
  }

  # Download and process
  download_and_process_cps_children(extract_id, 1962:2022, cache_dir)
}

#' Get CPS children extract status
#'
#' @description
#' Checks the status of a submitted IPUMS CPS extract.
#'
#' @param extract_id Integer: the extract ID
#'
#' @return List with extract status information
#'
#' @export
get_cps_children_extract_status <- function(extract_id) {
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

#' Submit CPS children extract without waiting
#'
#' @description
#' Submits an IPUMS CPS extract without waiting for completion.
#'
#' @param years Integer vector of years (default: 1962:2022)
#' @param cache_dir Character: directory for caching
#'
#' @return List with extract_id
#'
#' @export
submit_cps_children_extract <- function(years = 1962:2022,
                                         cache_dir = here::here("data/cache/ipums_cps")) {
  fetch_cps_children_per_couple(
    years = years,
    cache_dir = cache_dir,
    wait_for_extract = FALSE
  )
}

#' Load cached CPS children per couple data
#'
#' @description
#' Loads the cached CPS children per couple data without making an API request.
#'
#' @param cache_dir Character: directory with cached data
#' @param years Integer vector of years to return (NULL = all)
#'
#' @return data.table with children per couple data, or NULL if not cached
#'
#' @export
load_cps_children_per_couple <- function(cache_dir = here::here("data/cache/ipums_cps"),
                                          years = NULL) {
  cache_file <- file.path(cache_dir, "cps_children_per_couple_1962_2022.rds")

  if (!file.exists(cache_file)) {
    cli::cli_alert_warning("CPS children data not cached")
    cli::cli_alert_info("Run fetch_cps_children_per_couple() to download data")
    return(NULL)
  }

  data <- readRDS(cache_file)

  if (!is.null(years)) {
    data <- data[year %in% years]
  }

  data
}


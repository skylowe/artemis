#' NCHS Marriage Data Acquisition
#'
#' Functions for fetching historical marriage data from the National Center for
#' Health Statistics (NCHS) via NBER archive. This includes Marriage Registration
#' Area (MRA) microdata for building the MarGrid matrix.
#'
#' Data sources:
#' - NBER Marriage and Divorce Data 1968-1995: https://www.nber.org/research/data/marriage-and-divorce-data-1968-1995
#' - 1978-1988: Individual year files (marr78.zip through marr88.zip)
#' - 1989-1995: Combined file (cpmarr.zip)
#'
#' @name nchs_marriage
NULL

# =============================================================================
# CONSTANTS
# =============================================================================

#' NBER data URLs
#' @keywords internal
NBER_MARRIAGE_BASE_URL <- "https://data.nber.org/marriage"
NBER_MARRDIVO_BASE_URL <- "https://data.nber.org/marrdivo"

#' TR2025 age groups for marriage data (9-category recode)
#' Maps GAGER9/BAGER9 codes to age group labels
#' @keywords internal
NCHS_MARRIAGE_AGE_GROUPS_9 <- list(
  "1" = list(label = "12-17", min = 12, max = 17),
  "2" = list(label = "18-19", min = 18, max = 19),
  "3" = list(label = "20-24", min = 20, max = 24),
  "4" = list(label = "25-29", min = 25, max = 29),
  "5" = list(label = "30-34", min = 30, max = 34),
  "6" = list(label = "35-44", min = 35, max = 44),
  "7" = list(label = "45-54", min = 45, max = 54),
  "8" = list(label = "55-64", min = 55, max = 64),
  "9" = list(label = "65+",   min = 65, max = 120)
)

#' TR2025 age groups (8-category, used in documentation)
#' Note: TR2025 uses 14-19 instead of 12-17
#' @keywords internal
NCHS_MARRIAGE_AGE_GROUPS_8 <- c(
  "14-19", "20-24", "25-29", "30-34", "35-44", "45-54", "55-64", "65+"
)

#' Prior marital status codes (GPMSR5/BPMSR5)
#' @keywords internal
NCHS_PRIOR_STATUS_CODES <- list(
  "1" = "single",
  "2" = "widowed",
  "3" = "divorced",
  "4" = "unknown",
  "5" = "not_stated"
)

#' MRA states and their admission years
#' @keywords internal
MRA_STATES <- list(
  "01" = list(name = "Alabama", year = 1957),
  "02" = list(name = "Alaska", year = 1957),
  "05" = list(name = "California", year = 1957),
  "06" = list(name = "Colorado", year = 1979),
  "07" = list(name = "Connecticut", year = 1957),
  "08" = list(name = "Delaware", year = 1957),
  "09" = list(name = "District of Columbia", year = 1961),
  "10" = list(name = "Florida", year = 1957),
  "11" = list(name = "Georgia", year = 1957),
  "12" = list(name = "Hawaii", year = 1957),
  "13" = list(name = "Idaho", year = 1957),
  "14" = list(name = "Illinois", year = 1964),
  "15" = list(name = "Indiana", year = 1961),
  "16" = list(name = "Iowa", year = 1957),
  "17" = list(name = "Kansas", year = 1957),
  "18" = list(name = "Kentucky", year = 1959),
  "19" = list(name = "Louisiana", year = 1957),
  "20" = list(name = "Maine", year = 1957),
  "21" = list(name = "Maryland", year = 1957),
  "22" = list(name = "Massachusetts", year = 1961),
  "23" = list(name = "Michigan", year = 1957),
  "24" = list(name = "Minnesota", year = 1971),
  "25" = list(name = "Mississippi", year = 1957),
  "26" = list(name = "Missouri", year = 1968),
  "27" = list(name = "Montana", year = 1957),
  "28" = list(name = "Nebraska", year = 1957),
  "30" = list(name = "New Hampshire", year = 1957),
  "31" = list(name = "New Jersey", year = 1957),
  "32" = list(name = "New York State", year = 1957),
  "33" = list(name = "New York City", year = 1965),
  "34" = list(name = "North Carolina", year = 1964),
  "36" = list(name = "Ohio", year = 1957),
  "38" = list(name = "Oregon", year = 1957),
  "39" = list(name = "Pennsylvania", year = 1957),
  "40" = list(name = "Rhode Island", year = 1957),
  "41" = list(name = "South Carolina", year = 1971),
  "42" = list(name = "South Dakota", year = 1957),
  "43" = list(name = "Tennessee", year = 1957),
  "45" = list(name = "Utah", year = 1957),
  "46" = list(name = "Vermont", year = 1957),
  "47" = list(name = "Virginia", year = 1957),
  "49" = list(name = "West Virginia", year = 1965),
  "50" = list(name = "Wisconsin", year = 1957),
  "51" = list(name = "Wyoming", year = 1957)
)

# =============================================================================
# DATA DOWNLOADING
# =============================================================================

#' Download NBER marriage data file
#'
#' @param year Integer year (1968-1995)
#' @param cache_dir Character path to cache directory
#' @param force Logical: force re-download if file exists
#'
#' @return Path to extracted data file
#' @keywords internal
download_nber_marriage_file <- function(year,
                                         cache_dir = here::here("data/cache/nber_marriage"),
                                         force = FALSE) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  if (year >= 1989 && year <= 1995) {
    # Use combined file for 1989-1995
    zip_file <- file.path(cache_dir, "cpmarr.zip")
    dat_file <- file.path(cache_dir, "cpmarr.dat")
    url <- paste0(NBER_MARRDIVO_BASE_URL, "/cpmarr.zip")
  } else if (year >= 1968 && year <= 1990) {
    # Individual year files
    year_suffix <- sprintf("%02d", year %% 100)
    zip_file <- file.path(cache_dir, paste0("marr", year_suffix, ".zip"))
    dat_file <- file.path(cache_dir, paste0("marr", year_suffix, ".dat"))
    url <- paste0(NBER_MARRIAGE_BASE_URL, "/marr", year_suffix, ".zip")
  } else {
    cli::cli_abort("Year {year} is not available in NBER marriage data (1968-1995)")
  }

  # Download if needed
  if (!file.exists(zip_file) || force) {
    cli::cli_alert("Downloading NBER marriage data for {year}...")
    tryCatch({
      utils::download.file(url, zip_file, mode = "wb", quiet = TRUE)
    }, error = function(e) {
      cli::cli_abort("Failed to download {url}: {e$message}")
    })
  }

  # Extract if needed
  if (!file.exists(dat_file) || force) {
    cli::cli_alert("Extracting {basename(zip_file)}...")
    utils::unzip(zip_file, exdir = cache_dir, overwrite = TRUE)
  }

  dat_file
}

#' Download NBER marriage documentation
#'
#' @param cache_dir Character path to cache directory
#' @param force Logical: force re-download
#'
#' @return Path to documentation directory
#' @keywords internal
download_nber_marriage_docs <- function(cache_dir = here::here("data/cache/nber_marriage"),
                                         force = FALSE) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  doc_zip <- file.path(cache_dir, "md_doc.zip")

  if (!file.exists(doc_zip) || force) {
    cli::cli_alert("Downloading NBER marriage documentation...")
    utils::download.file(
      paste0(NBER_MARRDIVO_BASE_URL, "/md_doc.zip"),
      doc_zip,
      mode = "wb",
      quiet = TRUE
    )
    utils::unzip(doc_zip, exdir = cache_dir, overwrite = TRUE)
  }

  cache_dir
}

# =============================================================================
# 1989-1995 DATA PARSING (cpmarr.dat format)
# =============================================================================

#' Parse 1989-1995 marriage data (cpmarr.dat format)
#'
#' @description
#' Parses the fixed-width format NCHS marriage data file for 1989-1995.
#' Record layout from cpmarr.cbk and marrlyo.txt.
#'
#' @param file_path Path to cpmarr.dat file
#' @param years Integer vector of years to extract (default: 1989:1995)
#'
#' @return data.table with marriage records
#' @keywords internal
parse_cpmarr_data <- function(file_path, years = 1989:1995) {
  checkmate::assert_file_exists(file_path)

  file_size_mb <- round(file.size(file_path) / 1e6, 1)
  cli::cli_alert("Reading cpmarr.dat ({file_size_mb} MB)...")

  # Read fixed-width file based on marrlyo.txt layout (67-character records)
  # Key positions (1-indexed):
  # 1-4:   DATAYEAR (4 chars)
  # 14-16: WEIGHT (3 chars)
  # 32-33: GAGEDET (2 chars) - groom detailed age
  # 34:    GAGER9 (1 char) - groom age group 9-category
  # 26:    GPMSR5 (1 char) - groom prior marital status 5-category
  # 56-57: BAGEDET (2 chars) - bride detailed age
  # 58:    BAGER9 (1 char) - bride age group 9-category
  # 50:    BPMSR5 (1 char) - bride prior marital status 5-category

  col_positions <- readr::fwf_positions(
    start = c(1, 14, 32, 34, 26, 56, 58, 50),
    end   = c(4, 16, 33, 34, 26, 57, 58, 50),
    col_names = c("year", "weight", "groom_age_det", "groom_age_grp",
                  "groom_prior_status", "bride_age_det", "bride_age_grp",
                  "bride_prior_status")
  )

  # Use readr for efficient fixed-width reading
  if (!requireNamespace("readr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg readr} is required for parsing NCHS data")
  }

  raw_data <- readr::read_fwf(
    file_path,
    col_positions = col_positions,
    col_types = readr::cols(
      year = readr::col_integer(),
      weight = readr::col_integer(),
      groom_age_det = readr::col_integer(),
      groom_age_grp = readr::col_character(),
      groom_prior_status = readr::col_character(),
      bride_age_det = readr::col_integer(),
      bride_age_grp = readr::col_character(),
      bride_prior_status = readr::col_character()
    ),
    progress = FALSE
  )

  dt <- data.table::as.data.table(raw_data)

  # Filter to requested years
  dt <- dt[year %in% years]

  # Convert weight codes to actual weights
  # 001=100%, 002=50% (weight=2), 005=20% (weight=5), etc.
  dt[, weight := as.integer(weight)]

  # Map age group codes to labels
  dt[, groom_age_group := sapply(groom_age_grp, function(x) {
    if (x %in% names(NCHS_MARRIAGE_AGE_GROUPS_9)) {
      NCHS_MARRIAGE_AGE_GROUPS_9[[x]]$label
    } else {
      NA_character_
    }
  })]

  dt[, bride_age_group := sapply(bride_age_grp, function(x) {
    if (x %in% names(NCHS_MARRIAGE_AGE_GROUPS_9)) {
      NCHS_MARRIAGE_AGE_GROUPS_9[[x]]$label
    } else {
      NA_character_
    }
  })]

  # Map prior status codes
  dt[, groom_prior := sapply(groom_prior_status, function(x) {
    if (x %in% names(NCHS_PRIOR_STATUS_CODES)) {
      NCHS_PRIOR_STATUS_CODES[[x]]
    } else {
      NA_character_
    }
  })]

  dt[, bride_prior := sapply(bride_prior_status, function(x) {
    if (x %in% names(NCHS_PRIOR_STATUS_CODES)) {
      NCHS_PRIOR_STATUS_CODES[[x]]
    } else {
      NA_character_
    }
  })]

  n_records <- format(nrow(dt), big.mark = ",")
  n_years <- length(unique(dt$year))
  cli::cli_alert_success("Parsed {n_records} marriage records for {n_years} years")

  dt
}

# =============================================================================
# 1978-1988 DATA PARSING (individual year files)
# =============================================================================

#' Parse 1978-1988 marriage data (marrYY.dat format)
#'
#' @description
#' Parses the fixed-width format NCHS marriage data files for 1978-1988.
#' These files have a different layout (140 characters) than 1989-1995.
#' Record layout derived from marr88.pdf documentation.
#'
#' @param file_path Path to marrYY.dat file
#' @param year Integer year (needed to set year since file only has last digit)
#'
#' @return data.table with marriage records
#' @keywords internal
parse_marr_data_1978_1988 <- function(file_path, year) {
  checkmate::assert_file_exists(file_path)
  checkmate::assert_integerish(year, lower = 1978, upper = 1988, len = 1)

  file_size_mb <- round(file.size(file_path) / 1e6, 1)
  cli::cli_alert("Reading marr{year %% 100}.dat ({file_size_mb} MB)...")

  # 1978-1988 record layout (140 characters per record)
  # Key positions (1-indexed):
  # 1:     Year identifier (last digit: 8=1988, 7=1987, etc.)
  # 8-9:   State code
  # 18-20: Weight (001=100%, 002=50%, 005=20%, 010=10%, 020=5%)
  # 41-42: Groom detail age (12-94)
  # 43:    Groom age recode 9
  # 35:    Groom prior marital status (GPMSR5)
  # 70-71: Bride detail age (12-94)
  # 72:    Bride age recode 9
  # 64:    Bride prior marital status (BPMSR5)

  col_positions <- readr::fwf_positions(
    start = c(18, 41, 43, 35, 70, 72, 64),
    end   = c(20, 42, 43, 35, 71, 72, 64),
    col_names = c("weight", "groom_age_det", "groom_age_grp",
                  "groom_prior_status", "bride_age_det", "bride_age_grp",
                  "bride_prior_status")
  )

  if (!requireNamespace("readr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg readr} is required for parsing NCHS data")
  }

  raw_data <- readr::read_fwf(
    file_path,
    col_positions = col_positions,
    col_types = readr::cols(
      weight = readr::col_integer(),
      groom_age_det = readr::col_integer(),
      groom_age_grp = readr::col_character(),
      groom_prior_status = readr::col_character(),
      bride_age_det = readr::col_integer(),
      bride_age_grp = readr::col_character(),
      bride_prior_status = readr::col_character()
    ),
    progress = FALSE
  )

  dt <- data.table::as.data.table(raw_data)
  dt[, year := year]

  # Map age group codes to labels (same mapping as 1989-1995)
  dt[, groom_age_group := sapply(groom_age_grp, function(x) {
    if (x %in% names(NCHS_MARRIAGE_AGE_GROUPS_9)) {
      NCHS_MARRIAGE_AGE_GROUPS_9[[x]]$label
    } else {
      NA_character_
    }
  })]

  dt[, bride_age_group := sapply(bride_age_grp, function(x) {
    if (x %in% names(NCHS_MARRIAGE_AGE_GROUPS_9)) {
      NCHS_MARRIAGE_AGE_GROUPS_9[[x]]$label
    } else {
      NA_character_
    }
  })]

  # Map prior status codes
  dt[, groom_prior := sapply(groom_prior_status, function(x) {
    if (x %in% names(NCHS_PRIOR_STATUS_CODES)) {
      NCHS_PRIOR_STATUS_CODES[[x]]
    } else {
      NA_character_
    }
  })]

  dt[, bride_prior := sapply(bride_prior_status, function(x) {
    if (x %in% names(NCHS_PRIOR_STATUS_CODES)) {
      NCHS_PRIOR_STATUS_CODES[[x]]
    } else {
      NA_character_
    }
  })]

  n_records <- format(nrow(dt), big.mark = ",")
  cli::cli_alert_success("Parsed {n_records} marriage records for {year}")

  dt
}

#' Fetch NCHS MRA marriages by age group (1978-1988)
#'
#' @description
#' Downloads and aggregates NCHS marriage data by age-group-of-husband crossed
#' with age-group-of-wife for the MRA period 1978-1988.
#' This is Items 4-5 from TR2025 documentation.
#'
#' Note: TR2025 documentation mentions excluding 1980, but 1980 data is
#' available from NBER. This function includes 1980 by default.
#'
#' @param years Integer vector of years (default: 1978:1988)
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, husband_age_group, wife_age_group, marriages
#'
#' @export
fetch_nchs_mra_marriages_1978_1988 <- function(
    years = 1978:1988,
    cache_dir = here::here("data/cache/nber_marriage")
) {
  # Validate years
  valid_years <- 1978:1988
  if (!all(years %in% valid_years)) {
    invalid <- years[!years %in% valid_years]
    cli::cli_abort("Invalid years: {invalid}. Valid range: 1978-1988.")
  }

  # Check for cached aggregated data
  cache_file <- file.path(cache_dir, "nchs_mra_marriages_1978_1988.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached MRA marriages (1978-1988)")
    data <- readRDS(cache_file)
    return(data[year %in% years])
  }

  cli::cli_alert_info("Downloading and parsing 1978-1988 marriage data...")
  cli::cli_alert_info("This may take a while (11 files, ~1.1GB total)")

  all_data <- list()

  for (yr in 1978:1988) {
    # Download file
    dat_file <- download_nber_marriage_file(yr, cache_dir)

    # Parse file
    yr_data <- parse_marr_data_1978_1988(dat_file, yr)
    all_data[[as.character(yr)]] <- yr_data
  }

  # Combine all years
  combined <- data.table::rbindlist(all_data, use.names = TRUE)

  # Aggregate by year, husband age group, wife age group
  cli::cli_alert("Aggregating marriages by age groups...")

  result <- combined[
    !is.na(groom_age_group) & !is.na(bride_age_group),
    .(marriages = sum(weight, na.rm = TRUE)),
    by = .(year, husband_age_group = groom_age_group, wife_age_group = bride_age_group)
  ]

  # Order age groups properly
  age_order <- c("12-17", "18-19", "20-24", "25-29", "30-34", "35-44", "45-54", "55-64", "65+")
  result[, husband_age_group := factor(husband_age_group, levels = age_order)]
  result[, wife_age_group := factor(wife_age_group, levels = age_order)]
  data.table::setorder(result, year, husband_age_group, wife_age_group)
  result[, husband_age_group := as.character(husband_age_group)]
  result[, wife_age_group := as.character(wife_age_group)]

  # Cache result
  saveRDS(result, cache_file)
  n_rows <- format(nrow(result), big.mark = ",")
  cli::cli_alert_success("Cached MRA marriages (1978-1988): {n_rows} rows")

  # Return requested years
  result[year %in% years]
}

#' Fetch NCHS marriages with detailed ages (1978-1988)
#'
#' @description
#' Downloads and aggregates NCHS marriage data by detailed single-year ages
#' for husband and wife for 1978-1988.
#'
#' @param years Integer vector of years (default: 1978:1988)
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, husband_age, wife_age, marriages
#'
#' @export
fetch_nchs_mra_marriages_detailed_1978_1988 <- function(
    years = 1978:1988,
    cache_dir = here::here("data/cache/nber_marriage")
) {
  # Validate
  valid_years <- 1978:1988
  if (!all(years %in% valid_years)) {
    invalid <- years[!years %in% valid_years]
    cli::cli_abort("Invalid years: {invalid}. Valid range: 1978-1988.")
  }

  # Check for cached data
  cache_file <- file.path(cache_dir, "nchs_mra_marriages_detailed_1978_1988.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached detailed MRA marriages (1978-1988)")
    data <- readRDS(cache_file)
    return(data[year %in% years])
  }

  cli::cli_alert_info("Downloading and parsing 1978-1988 marriage data...")

  all_data <- list()

  for (yr in 1978:1988) {
    dat_file <- download_nber_marriage_file(yr, cache_dir)
    yr_data <- parse_marr_data_1978_1988(dat_file, yr)
    all_data[[as.character(yr)]] <- yr_data
  }

  combined <- data.table::rbindlist(all_data, use.names = TRUE)

  cli::cli_alert("Aggregating marriages by detailed ages...")

  # Aggregate by detailed ages
  result <- combined[
    !is.na(groom_age_det) & !is.na(bride_age_det),
    .(marriages = sum(weight, na.rm = TRUE)),
    by = .(year, husband_age = groom_age_det, wife_age = bride_age_det)
  ]

  data.table::setorder(result, year, husband_age, wife_age)

  # Cache
  saveRDS(result, cache_file)
  n_rows <- format(nrow(result), big.mark = ",")
  cli::cli_alert_success("Cached detailed marriages (1978-1988): {n_rows} rows")

  result[year %in% years]
}

#' Get total MRA marriages by year (1978-1988)
#'
#' @param cache_dir Character path to cache directory
#' @return data.table with columns: year, total_marriages
#' @export
get_nchs_mra_totals_1978_1988 <- function(
    cache_dir = here::here("data/cache/nber_marriage")
) {
  data <- fetch_nchs_mra_marriages_1978_1988(cache_dir = cache_dir)
  totals <- data[, .(total_marriages = sum(marriages, na.rm = TRUE)), by = year]
  data.table::setorder(totals, year)
  totals
}

# =============================================================================
# MAIN FETCH FUNCTIONS
# =============================================================================

#' Fetch NCHS MRA marriages by age group (1989-1995)
#'
#' @description
#' Downloads and aggregates NCHS marriage data by age-group-of-husband crossed
#' with age-group-of-wife for the MRA subset period 1989-1995.
#' This is Item 6 from TR2025 documentation.
#'
#' @param years Integer vector of years (default: 1989:1995)
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, husband_age_group, wife_age_group, marriages
#'
#' @export
fetch_nchs_mra_marriages_1989_1995 <- function(
    years = 1989:1995,
    cache_dir = here::here("data/cache/nber_marriage")
) {
  checkmate::assert_integerish(years, lower = 1989, upper = 1995, min.len = 1)

  # Check for cached aggregated data
  cache_file <- file.path(cache_dir, "nchs_mra_marriages_1989_1995.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached MRA marriages (1989-1995)")
    data <- readRDS(cache_file)
    return(data[year %in% years])
  }

  # Download and parse raw data
  dat_file <- download_nber_marriage_file(1989, cache_dir)
  raw_data <- parse_cpmarr_data(dat_file, years = 1989:1995)

  # Aggregate by year, husband age group, wife age group
  # Apply weights to get estimated marriage counts
  cli::cli_alert("Aggregating marriages by age groups...")

  result <- raw_data[
    !is.na(groom_age_group) & !is.na(bride_age_group),
    .(marriages = sum(weight, na.rm = TRUE)),
    by = .(year, husband_age_group = groom_age_group, wife_age_group = bride_age_group)
  ]

  # Order age groups properly
  age_order <- c("12-17", "18-19", "20-24", "25-29", "30-34", "35-44", "45-54", "55-64", "65+")
  result[, husband_age_group := factor(husband_age_group, levels = age_order)]
  result[, wife_age_group := factor(wife_age_group, levels = age_order)]
  data.table::setorder(result, year, husband_age_group, wife_age_group)
  result[, husband_age_group := as.character(husband_age_group)]
  result[, wife_age_group := as.character(wife_age_group)]

  # Cache result
  saveRDS(result, cache_file)
  n_rows <- format(nrow(result), big.mark = ",")
  cli::cli_alert_success("Cached MRA marriages (1989-1995): {n_rows} rows")

  # Return requested years
  result[year %in% years]
}

#' Fetch NCHS marriages by prior marital status (1989-1995)
#'
#' @description
#' Downloads and aggregates NCHS marriage data by age group, sex, and prior
#' marital status. This provides data comparable to Items 9-11 from TR2025.
#'
#' Note: TR2025 uses 1979 and 1981-88 for prior status differentials, but
#' that data format is different. This provides 1989-1995 as a reference.
#'
#' @param years Integer vector of years (default: 1989:1995)
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, age_group, sex, prior_status, marriages
#'
#' @export
fetch_nchs_marriages_by_prior_status <- function(
    years = 1989:1995,
    cache_dir = here::here("data/cache/nber_marriage")
) {
  checkmate::assert_integerish(years, lower = 1989, upper = 1995, min.len = 1)

  # Check for cached data
  cache_file <- file.path(cache_dir, "nchs_marriages_by_prior_status_1989_1995.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached marriages by prior status")
    data <- readRDS(cache_file)
    return(data[year %in% years])
  }

  # Download and parse raw data
  dat_file <- download_nber_marriage_file(1989, cache_dir)
  raw_data <- parse_cpmarr_data(dat_file, years = 1989:1995)

  cli::cli_alert("Aggregating marriages by prior marital status...")

  # Aggregate grooms (husbands)
  groom_data <- raw_data[
    !is.na(groom_age_group) & !is.na(groom_prior),
    .(marriages = sum(weight, na.rm = TRUE)),
    by = .(year, age_group = groom_age_group, prior_status = groom_prior)
  ]
  groom_data[, sex := "male"]

  # Aggregate brides (wives)
  bride_data <- raw_data[
    !is.na(bride_age_group) & !is.na(bride_prior),
    .(marriages = sum(weight, na.rm = TRUE)),
    by = .(year, age_group = bride_age_group, prior_status = bride_prior)
  ]
  bride_data[, sex := "female"]

  # Combine
  result <- data.table::rbindlist(list(groom_data, bride_data))

  # Filter to known statuses (single, widowed, divorced)
  result <- result[prior_status %in% c("single", "widowed", "divorced")]

  # Order properly
  age_order <- c("12-17", "18-19", "20-24", "25-29", "30-34", "35-44", "45-54", "55-64", "65+")
  result[, age_group := factor(age_group, levels = age_order)]
  data.table::setorder(result, year, sex, age_group, prior_status)
  result[, age_group := as.character(age_group)]

  # Cache
  saveRDS(result, cache_file)
  n_rows <- format(nrow(result), big.mark = ",")
  cli::cli_alert_success("Cached marriages by prior status: {n_rows} rows")

  result[year %in% years]
}

#' Fetch NCHS marriages with detailed ages (1989-1995)
#'
#' @description
#' Downloads and aggregates NCHS marriage data by detailed single-year ages
#' for husband and wife. This provides data for building detailed marriage grids.
#'
#' @param years Integer vector of years (default: 1989:1995)
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, husband_age, wife_age, marriages
#'
#' @export
fetch_nchs_mra_marriages_detailed <- function(
    years = 1989:1995,
    cache_dir = here::here("data/cache/nber_marriage")
) {
  checkmate::assert_integerish(years, lower = 1989, upper = 1995, min.len = 1)

  # Check for cached data
  cache_file <- file.path(cache_dir, "nchs_mra_marriages_detailed_1989_1995.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached detailed MRA marriages")
    data <- readRDS(cache_file)
    return(data[year %in% years])
  }

  # Download and parse raw data
  dat_file <- download_nber_marriage_file(1989, cache_dir)
  raw_data <- parse_cpmarr_data(dat_file, years = 1989:1995)

  cli::cli_alert("Aggregating marriages by detailed ages...")

  # Aggregate by detailed ages
  result <- raw_data[
    !is.na(groom_age_det) & !is.na(bride_age_det),
    .(marriages = sum(weight, na.rm = TRUE)),
    by = .(year, husband_age = groom_age_det, wife_age = bride_age_det)
  ]

  data.table::setorder(result, year, husband_age, wife_age)

  # Cache
  saveRDS(result, cache_file)
  n_rows <- format(nrow(result), big.mark = ",")
  cli::cli_alert_success("Cached detailed marriages: {n_rows} rows")

  result[year %in% years]
}

#' Get total MRA marriages by year (1989-1995)
#'
#' @description
#' Returns total marriage counts in the MRA for 1989-1995.
#'
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, total_marriages
#'
#' @export
get_nchs_mra_totals_1989_1995 <- function(
    cache_dir = here::here("data/cache/nber_marriage")
) {
  # Get aggregated data
  data <- fetch_nchs_mra_marriages_1989_1995(cache_dir = cache_dir)

  # Sum by year
  totals <- data[, .(total_marriages = sum(marriages, na.rm = TRUE)), by = year]
  data.table::setorder(totals, year)

  totals
}

# =============================================================================
# TOTAL U.S. MARRIAGES (Item 8)
# =============================================================================

#' Fetch total U.S. marriages from NCHS (1989-2022)
#'
#' @description
#' Returns total U.S. marriage counts from NCHS National Vital Statistics.
#' This is Item 8 from TR2025 documentation.
#'
#' Note: For years 1989-1995, uses NBER data totals. For 1996-2022, uses
#' CDC WONDER or published NVSS reports.
#'
#' @param years Integer vector of years
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, total_marriages, source
#'
#' @details
#' Data sources:
#' - 1989-1995: NBER marriage microdata (weighted totals)
#' - 1996-2022: CDC National Vital Statistics Reports
#'
#' Reference: CDC/NCHS National Marriage and Divorce Rate Trends
#' https://www.cdc.gov/nchs/nvss/marriage-divorce.htm
#'
#' @export
fetch_nchs_us_total_marriages <- function(
    years = 1989:2022,
    cache_dir = here::here("data/cache/nber_marriage")
) {
  # Published NCHS total U.S. marriages
  # Source: CDC/NCHS National Vital Statistics Reports
  # https://www.cdc.gov/nchs/data/nvss/marriage_divorce_tables.htm
  nchs_totals <- data.table::data.table(
    year = 1989:2022,
    total_marriages = c(
      # 1989-1999 (National Vital Statistics Reports)
      2403268, 2443489, 2371000, 2362000, 2334000, 2362000, 2336000, 2344000, 2384000, 2244000, 2358000,
      # 2000-2009
      2315000, 2326596, 2254000, 2245000, 2279000, 2249000, 2193000, 2197000, 2157000, 2080000,
      # 2010-2019
      2096000, 2118000, 2131000, 2081000, 2140000, 2221579, 2245404, 2236496, 2132853, 2015603,
      # 2020-2022 (COVID impact)
      1676911, 1985072, 2065905
    ),
    source = c(
      rep("NCHS NVSR", 11),  # 1989-1999
      rep("NCHS NVSR", 10),  # 2000-2009
      rep("NCHS NVSR", 10),  # 2010-2019
      rep("NCHS NVSR", 3)    # 2020-2022
    )
  )

  # Filter to requested years
  nchs_totals[year %in% years]
}

# =============================================================================
# SUMMARY FUNCTIONS
# =============================================================================

#' Summary of available NCHS marriage data
#'
#' @description
#' Returns a summary of available NCHS marriage data and its status.
#'
#' @param cache_dir Character path to cache directory
#'
#' @return List with data availability information
#'
#' @export
summarize_nchs_marriage_data <- function(
    cache_dir = here::here("data/cache/nber_marriage")
) {
  # Check what's cached
  cached_files <- list(
    mra_1989_1995 = file.exists(file.path(cache_dir, "nchs_mra_marriages_1989_1995.rds")),
    detailed_1989_1995 = file.exists(file.path(cache_dir, "nchs_mra_marriages_detailed_1989_1995.rds")),
    prior_status = file.exists(file.path(cache_dir, "nchs_marriages_by_prior_status_1989_1995.rds")),
    cpmarr_dat = file.exists(file.path(cache_dir, "cpmarr.dat"))
  )

  summary_info <- list(
    available_years = list(
      mra_by_age_group = 1989:1995,
      detailed_ages = 1989:1995,
      prior_status = 1989:1995,
      us_totals = 1989:2022
    ),
    cached = cached_files,
    tr2025_items = list(
      item_4 = "Marriages by age-of-husband × age-of-wife (1978-1988): NOT YET IMPLEMENTED",
      item_5 = "Unmarried MRA population (1978-1988): NOT YET IMPLEMENTED",
      item_6 = "MRA subset marriages by age group (1989-1995): AVAILABLE",
      item_7 = "Total MRA marriages (1957-1988): NOT YET IMPLEMENTED",
      item_8 = "Total U.S. marriages (1989-2022): AVAILABLE",
      item_9 = "Marriages by prior status (1989-1995): AVAILABLE (different years than TR2025)",
      item_10 = "Unmarried by prior status (1982-1988): NOT YET IMPLEMENTED",
      item_11 = "Total marriages and remarriages (1979, 1981-88): NOT YET IMPLEMENTED"
    )
  )

  cli::cli_h2("NCHS Marriage Data Summary")
  cli::cli_alert_info("Available data periods:")
  cli::cli_bullets(c(
    "*" = "MRA marriages by age group: 1989-1995",
    "*" = "Detailed single-year ages: 1989-1995",
    "*" = "Prior marital status: 1989-1995",
    "*" = "U.S. total marriages: 1989-2022"
  ))

  cli::cli_alert_info("Cached files: {sum(unlist(cached_files))}/{length(cached_files)}")

  invisible(summary_info)
}

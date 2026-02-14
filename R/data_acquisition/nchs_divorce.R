#' NCHS Divorce Data Acquisition
#'
#' Functions for fetching historical divorce data from the National Center for
#' Health Statistics (NCHS) via NBER archive. This includes Divorce Registration
#' Area (DRA) microdata for building the DivGrid matrix.
#'
#' Data sources:
#' - NBER Marriage and Divorce Data 1968-1995: https://www.nber.org/research/data/marriage-and-divorce-data-1968-1995
#' - 1979-1988: Individual year files (div79.zip through div88.zip)
#' - 1989-1995: Combined file (cpdivo.zip)
#'
#' @name nchs_divorce
NULL

# =============================================================================
# CONSTANTS
# =============================================================================

#' NBER data URLs
#' @keywords internal
NBER_DIVORCE_BASE_URL <- "https://data.nber.org/divorce"
NBER_MARRDIVO_BASE_URL <- "https://data.nber.org/marrdivo"

#' DRA states and their admission years (1988)
#' DRA covered ~48% of U.S. divorces in 1988
#' @keywords internal
DRA_STATES <- list(
  "01" = list(name = "Alabama", year = 1958),
  "02" = list(name = "Alaska", year = 1958),
  "07" = list(name = "Connecticut", year = 1968),
  "08" = list(name = "Delaware", year = 1981),
  "09" = list(name = "District of Columbia", year = 1986),
  "11" = list(name = "Georgia", year = 1958),
  "12" = list(name = "Hawaii", year = 1958),
  "13" = list(name = "Idaho", year = 1958),
  "14" = list(name = "Illinois", year = 1968),
  "16" = list(name = "Iowa", year = 1958),
  "17" = list(name = "Kansas", year = 1959),
  "18" = list(name = "Kentucky", year = 1969),
  "21" = list(name = "Maryland", year = 1959),
  "22" = list(name = "Massachusetts", year = 1979),
  "23" = list(name = "Michigan", year = 1961),
  "26" = list(name = "Missouri", year = 1961),
  "27" = list(name = "Montana", year = 1958),
  "28" = list(name = "Nebraska", year = 1958),

  "30" = list(name = "New Hampshire", year = 1979),
  "33" = list(name = "New York", year = 1969),
  "36" = list(name = "Ohio", year = 1962),
  "38" = list(name = "Oregon", year = 1958),
  "39" = list(name = "Pennsylvania", year = 1958),
  "40" = list(name = "Rhode Island", year = 1963),
  "41" = list(name = "South Carolina", year = 1971),
  "42" = list(name = "South Dakota", year = 1958),
  "43" = list(name = "Tennessee", year = 1958),
  "45" = list(name = "Utah", year = 1958),
  "46" = list(name = "Vermont", year = 1968),
  "47" = list(name = "Virginia", year = 1958),
  "50" = list(name = "Wisconsin", year = 1958),
  "51" = list(name = "Wyoming", year = 1958),
  "53" = list(name = "Virgin Islands", year = 1958)
)

#' Age group recode (16 categories) for divorce data
#' Maps HAGER16/WAGER16 codes to age ranges
#' @keywords internal
NCHS_DIVORCE_AGE_GROUPS_16 <- list(
  "01" = list(label = "<20", min = 14, max = 19),
  "02" = list(label = "20-24", min = 20, max = 24),
  "03" = list(label = "25-29", min = 25, max = 29),
  "04" = list(label = "30-34", min = 30, max = 34),
  "05" = list(label = "35-39", min = 35, max = 39),
  "06" = list(label = "40-44", min = 40, max = 44),
  "07" = list(label = "45-49", min = 45, max = 49),
  "08" = list(label = "50-54", min = 50, max = 54),
  "09" = list(label = "55-59", min = 55, max = 59),
  "10" = list(label = "60-64", min = 60, max = 64),
  "11" = list(label = "65-69", min = 65, max = 69),
  "12" = list(label = "70-74", min = 70, max = 74),
  "13" = list(label = "75-79", min = 75, max = 79),
  "14" = list(label = "80-84", min = 80, max = 84),
  "15" = list(label = "85+", min = 85, max = 120),
  "16" = list(label = "Not stated", min = NA, max = NA)
)

#' Age group recode (9 categories) matching marriage TR2025 groups
#' @keywords internal
NCHS_DIVORCE_AGE_GROUPS_9 <- c(
  "<20", "20-24", "25-29", "30-34", "35-44", "45-54", "55-64", "65+"
)

# =============================================================================
# DATA DOWNLOADING
# =============================================================================

#' Download NBER divorce data file
#'
#' @param year Integer year (1968-1995)
#' @param cache_dir Character path to cache directory
#' @param force Logical: force re-download if file exists
#'
#' @return Path to extracted data file
#' @keywords internal
download_nber_divorce_file <- function(year,
                                        cache_dir = here::here("data/cache/nber_divorce"),
                                        force = FALSE) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  if (year >= 1989 && year <= 1995) {
    # Use combined file for 1989-1995
    zip_file <- file.path(cache_dir, "cpdivo.zip")
    dat_file <- file.path(cache_dir, "cpdivo.dat")
    url <- paste0(NBER_MARRDIVO_BASE_URL, "/cpdivo.zip")
  } else if (year >= 1968 && year <= 1988) {
    # Individual year files
    year_suffix <- sprintf("%02d", year %% 100)
    zip_file <- file.path(cache_dir, paste0("div", year_suffix, ".zip"))
    dat_file <- file.path(cache_dir, paste0("div", year_suffix, ".dat"))
    url <- paste0(NBER_DIVORCE_BASE_URL, "/div", year_suffix, ".zip")
  } else {
    cli::cli_abort("Year {year} is not available in NBER divorce data (1968-1995)")
  }

  # Download if needed
  if (!file.exists(zip_file) || force) {
    cli::cli_alert("Downloading NBER divorce data for {year}...")
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

# =============================================================================
# 1989-1995 DATA PARSING (cpdivo.dat format)
# =============================================================================

#' Parse 1989-1995 divorce data (cpdivo.dat format)
#'
#' @description
#' Parses the fixed-width format NCHS divorce data file for 1989-1995.
#' Record layout from divrlyo.txt (106-character records, original format).
#' Note: cpdivo.cbk describes an export format with different positions.
#'
#' @param file_path Path to cpdivo.dat file
#' @param years Integer vector of years to extract (default: 1989:1995)
#'
#' @return data.table with divorce records
#' @keywords internal
parse_cpdivo_data <- function(file_path, years = 1989:1995) {
  checkmate::assert_file_exists(file_path)

  file_size_mb <- round(file.size(file_path) / 1e6, 1)
  cli::cli_alert("Reading cpdivo.dat ({file_size_mb} MB)...")

  # Read fixed-width file based on divrlyo.txt layout (106-character records)
  # Original layout positions (not export format):
  # 1-4:    DATAYEAR (4 chars) - Year of divorce
  # 8-9:    WEIGHT (2 chars) - Record weight (01=100%, 02=50%, 05=20%, 10=10%, 20=5%)
  # 57-58:  HAGEDET (2 chars) - Husband combined age at decree (12-98, 99=not stated)
  # 59-60:  HAGER16 (2 chars) - Husband age recode 16 categories
  # 84-85:  WAGEDET (2 chars) - Wife combined age at decree (12-98, 99=not stated)
  # 86-87:  WAGER16 (2 chars) - Wife age recode 16 categories

  col_positions <- readr::fwf_positions(
    start = c(1, 8, 57, 59, 84, 86),
    end   = c(4, 9, 58, 60, 85, 87),
    col_names = c("year", "weight", "husband_age_det", "husband_age_grp16",
                  "wife_age_det", "wife_age_grp16")
  )

  if (!requireNamespace("readr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg readr} is required for parsing NCHS data")
  }

  raw_data <- readr::read_fwf(
    file_path,
    col_positions = col_positions,
    col_types = readr::cols(
      year = readr::col_integer(),
      weight = readr::col_integer(),
      husband_age_det = readr::col_integer(),
      husband_age_grp16 = readr::col_character(),
      wife_age_det = readr::col_integer(),
      wife_age_grp16 = readr::col_character()
    ),
    progress = FALSE
  )

  dt <- data.table::as.data.table(raw_data)

  # Filter to requested years
  dt <- dt[year %in% years]

  # Handle age values: 99 = not stated
  dt[husband_age_det == 99, husband_age_det := NA_integer_]
  dt[wife_age_det == 99, wife_age_det := NA_integer_]

  # Map age group codes to labels
  dt[, husband_age_group := sapply(husband_age_grp16, function(x) {
    if (x %in% names(NCHS_DIVORCE_AGE_GROUPS_16)) {
      NCHS_DIVORCE_AGE_GROUPS_16[[x]]$label
    } else {
      NA_character_
    }
  })]

  dt[, wife_age_group := sapply(wife_age_grp16, function(x) {
    if (x %in% names(NCHS_DIVORCE_AGE_GROUPS_16)) {
      NCHS_DIVORCE_AGE_GROUPS_16[[x]]$label
    } else {
      NA_character_
    }
  })]

  n_records <- format(nrow(dt), big.mark = ",")
  n_years <- length(unique(dt$year))
  cli::cli_alert_success("Parsed {n_records} divorce records for {n_years} years")

  dt
}

# =============================================================================
# 1979-1988 DATA PARSING (individual year files)
# =============================================================================

#' Parse 1979-1988 divorce data (divYY.dat format)
#'
#' @description
#' Parses the fixed-width format NCHS divorce data files for 1979-1988.
#' These files have 117-character records with a different layout than 1989-1995.
#' Verified field positions from data analysis:
#' - Position 8-9: Weight
#' - Position 50-51: Husband age at decree
#' - Position 71-72: Wife age at decree
#'
#' @param file_path Path to divYY.dat file
#' @param year Integer year (needed to set year)
#'
#' @return data.table with divorce records
#' @keywords internal
parse_div_data_1979_1988 <- function(file_path, year) {
  checkmate::assert_file_exists(file_path)
  checkmate::assert_integerish(year, lower = 1979, upper = 1988, len = 1)

  file_size_mb <- round(file.size(file_path) / 1e6, 1)
  cli::cli_alert("Reading div{year %% 100}.dat ({file_size_mb} MB)...")

  # 1979-1988 divorce record layout (117 characters, NBER format)
  # NBER files differ from NCHS tape format (140 chars).
  # Verified positions from NCHS div88.pdf documentation and data analysis:
  # Position 1:      Year identifier (last digit: 9=1979, 0=1980, ..., 8=1988)
  # Positions 8-9:   State code (e.g., 01=Alabama, 36=Ohio) -- NOT weight!
  # Positions 40-41: Weight (01=100%, 02=50%, 05=20%, 10=10%, 20=5%)
  # Positions 50-51: Husband combined age at decree (15-98, 99=not stated)
  # Positions 71-72: Wife combined age at decree (15-98, 99=not stated)

  col_positions <- readr::fwf_positions(
    start = c(40, 50, 71),
    end   = c(41, 51, 72),
    col_names = c("weight", "husband_age_det", "wife_age_det")
  )

  if (!requireNamespace("readr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg readr} is required for parsing NCHS data")
  }

  raw_data <- readr::read_fwf(
    file_path,
    col_positions = col_positions,
    col_types = readr::cols(
      weight = readr::col_integer(),
      husband_age_det = readr::col_integer(),
      wife_age_det = readr::col_integer()
    ),
    progress = FALSE
  )

  dt <- data.table::as.data.table(raw_data)
  dt[, year := year]

  # Handle age values: 99 = not stated
  dt[husband_age_det == 99, husband_age_det := NA_integer_]
  dt[wife_age_det == 99, wife_age_det := NA_integer_]

  # Map ages to 16-category age groups for consistency with 1989-1995
  age_to_group16 <- function(age) {
    dplyr::case_when(
      is.na(age) ~ NA_character_,
      age < 20 ~ "<20",
      age <= 24 ~ "20-24",
      age <= 29 ~ "25-29",
      age <= 34 ~ "30-34",
      age <= 39 ~ "35-39",
      age <= 44 ~ "40-44",
      age <= 49 ~ "45-49",
      age <= 54 ~ "50-54",
      age <= 59 ~ "55-59",
      age <= 64 ~ "60-64",
      age <= 69 ~ "65-69",
      age <= 74 ~ "70-74",
      age <= 79 ~ "75-79",
      age <= 84 ~ "80-84",
      TRUE ~ "85+"
    )
  }

  dt[, husband_age_group := age_to_group16(husband_age_det)]
  dt[, wife_age_group := age_to_group16(wife_age_det)]

  n_records <- format(nrow(dt), big.mark = ",")
  n_valid <- sum(!is.na(dt$husband_age_det) & !is.na(dt$wife_age_det))
  cli::cli_alert_success("Parsed {n_records} divorce records for {year} ({format(n_valid, big.mark=',')} with valid ages)")

  dt
}

# =============================================================================
# MAIN FETCH FUNCTIONS - 1979-1988
# =============================================================================

#' Fetch NCHS DRA divorces by age group (1979-1988)
#'
#' @description
#' Downloads and aggregates NCHS divorce data by age-of-husband crossed
#' with age-of-wife for the DRA period 1979-1988.
#' This is Item 7 from TR2025 documentation.
#'
#' @param years Integer vector of years (default: 1979:1988)
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, husband_age_group, wife_age_group, divorces
#'
#' @export
fetch_nchs_dra_divorces_1979_1988 <- function(
    years = 1979:1988,
    cache_dir = here::here("data/cache/nber_divorce")
) {
  # Validate years
  valid_years <- 1979:1988
  if (!all(years %in% valid_years)) {
    invalid <- years[!years %in% valid_years]
    cli::cli_abort("Invalid years: {invalid}. Valid range: 1979-1988.")
  }

  # Check for cached aggregated data
  cache_file <- file.path(cache_dir, "nchs_dra_divorces_1979_1988.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached DRA divorces (1979-1988)")
    data <- readRDS(cache_file)
    return(data[year %in% years])
  }

  cli::cli_alert_info("Downloading and parsing 1979-1988 divorce data...")
  cli::cli_alert_info("This may take a while (10 files)")

  all_data <- list()

  for (yr in 1979:1988) {
    # Download file
    dat_file <- download_nber_divorce_file(yr, cache_dir)

    # Parse file
    yr_data <- parse_div_data_1979_1988(dat_file, yr)
    all_data[[as.character(yr)]] <- yr_data
  }

  # Combine all years
  combined <- data.table::rbindlist(all_data, use.names = TRUE, fill = TRUE)

  # Aggregate by year, husband age group, wife age group
  cli::cli_alert("Aggregating divorces by age groups...")

  result <- combined[
    !is.na(husband_age_group) & !is.na(wife_age_group),
    .(divorces = sum(weight, na.rm = TRUE)),
    by = .(year, husband_age_group, wife_age_group)
  ]

  # Order age groups properly
  age_order <- c("<20", "20-24", "25-29", "30-34", "35-39", "40-44",
                 "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                 "75-79", "80-84", "85+")
  result[, husband_age_group := factor(husband_age_group, levels = age_order)]
  result[, wife_age_group := factor(wife_age_group, levels = age_order)]
  data.table::setorder(result, year, husband_age_group, wife_age_group)
  result[, husband_age_group := as.character(husband_age_group)]
  result[, wife_age_group := as.character(wife_age_group)]

  # Cache result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_file)
  n_rows <- format(nrow(result), big.mark = ",")
  cli::cli_alert_success("Cached DRA divorces (1979-1988): {n_rows} rows")

  # Return requested years
  result[year %in% years]
}

#' Fetch NCHS divorces with detailed ages (1979-1988)
#'
#' @description
#' Downloads and aggregates NCHS divorce data by detailed single-year ages
#' for husband and wife for 1979-1988.
#'
#' @param years Integer vector of years (default: 1979:1988)
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, husband_age, wife_age, divorces
#'
#' @export
fetch_nchs_dra_divorces_detailed_1979_1988 <- function(
    years = 1979:1988,
    cache_dir = here::here("data/cache/nber_divorce")
) {
  # Validate
  valid_years <- 1979:1988
  if (!all(years %in% valid_years)) {
    invalid <- years[!years %in% valid_years]
    cli::cli_abort("Invalid years: {invalid}. Valid range: 1979-1988.")
  }

  # Check for cached data
  cache_file <- file.path(cache_dir, "nchs_dra_divorces_detailed_1979_1988.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached detailed DRA divorces (1979-1988)")
    data <- readRDS(cache_file)
    return(data[year %in% years])
  }

  cli::cli_alert_info("Downloading and parsing 1979-1988 divorce data...")

  all_data <- list()

  for (yr in 1979:1988) {
    dat_file <- download_nber_divorce_file(yr, cache_dir)
    yr_data <- parse_div_data_1979_1988(dat_file, yr)
    all_data[[as.character(yr)]] <- yr_data
  }

  combined <- data.table::rbindlist(all_data, use.names = TRUE, fill = TRUE)

  cli::cli_alert("Aggregating divorces by detailed ages...")

  # Aggregate by detailed ages
  result <- combined[
    !is.na(husband_age_det) & !is.na(wife_age_det),
    .(divorces = sum(weight, na.rm = TRUE)),
    by = .(year, husband_age = husband_age_det, wife_age = wife_age_det)
  ]

  data.table::setorder(result, year, husband_age, wife_age)

  # Cache
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_file)
  n_rows <- format(nrow(result), big.mark = ",")
  cli::cli_alert_success("Cached detailed divorces (1979-1988): {n_rows} rows")

  result[year %in% years]
}

#' Get total DRA divorces by year (1979-1988)
#'
#' @param cache_dir Character path to cache directory
#' @return data.table with columns: year, total_divorces
#' @export
get_nchs_dra_totals_1979_1988 <- function(
    cache_dir = here::here("data/cache/nber_divorce")
) {
  data <- fetch_nchs_dra_divorces_1979_1988(cache_dir = cache_dir)
  totals <- data[, .(total_divorces = sum(divorces, na.rm = TRUE)), by = year]
  data.table::setorder(totals, year)
  totals
}

# =============================================================================
# MAIN FETCH FUNCTIONS - 1989-1995
# =============================================================================

#' Fetch NCHS DRA divorces by age group (1989-1995)
#'
#' @description
#' Downloads and aggregates NCHS divorce data by age-of-husband crossed
#' with age-of-wife for the DRA period 1989-1995.
#'
#' @param years Integer vector of years (default: 1989:1995)
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, husband_age_group, wife_age_group, divorces
#'
#' @export
fetch_nchs_dra_divorces_1989_1995 <- function(
    years = 1989:1995,
    cache_dir = here::here("data/cache/nber_divorce")
) {
  checkmate::assert_integerish(years, lower = 1989, upper = 1995, min.len = 1)

  # Check for cached aggregated data
  cache_file <- file.path(cache_dir, "nchs_dra_divorces_1989_1995.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached DRA divorces (1989-1995)")
    data <- readRDS(cache_file)
    return(data[year %in% years])
  }

  # Download and parse raw data
  dat_file <- download_nber_divorce_file(1989, cache_dir)
  raw_data <- parse_cpdivo_data(dat_file, years = 1989:1995)

  # Aggregate by year, husband age group, wife age group
  # Apply weights to get estimated divorce counts
  cli::cli_alert("Aggregating divorces by age groups...")

  result <- raw_data[
    !is.na(husband_age_group) & !is.na(wife_age_group),
    .(divorces = sum(weight, na.rm = TRUE)),
    by = .(year, husband_age_group, wife_age_group)
  ]

  # Order age groups properly
  age_order <- c("<20", "20-24", "25-29", "30-34", "35-39", "40-44",
                 "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                 "75-79", "80-84", "85+")
  result[, husband_age_group := factor(husband_age_group, levels = age_order)]
  result[, wife_age_group := factor(wife_age_group, levels = age_order)]
  data.table::setorder(result, year, husband_age_group, wife_age_group)
  result[, husband_age_group := as.character(husband_age_group)]
  result[, wife_age_group := as.character(wife_age_group)]

  # Cache result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_file)
  n_rows <- format(nrow(result), big.mark = ",")
  cli::cli_alert_success("Cached DRA divorces (1989-1995): {n_rows} rows")

  # Return requested years
  result[year %in% years]
}

#' Fetch NCHS divorces with detailed ages (1989-1995)
#'
#' @description
#' Downloads and aggregates NCHS divorce data by detailed single-year ages
#' for husband and wife. This provides data for building detailed divorce grids.
#'
#' @param years Integer vector of years (default: 1989:1995)
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, husband_age, wife_age, divorces
#'
#' @export
fetch_nchs_dra_divorces_detailed <- function(
    years = 1989:1995,
    cache_dir = here::here("data/cache/nber_divorce")
) {
  checkmate::assert_integerish(years, lower = 1989, upper = 1995, min.len = 1)

  # Check for cached data
  cache_file <- file.path(cache_dir, "nchs_dra_divorces_detailed_1989_1995.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached detailed DRA divorces")
    data <- readRDS(cache_file)
    return(data[year %in% years])
  }

  # Download and parse raw data
  dat_file <- download_nber_divorce_file(1989, cache_dir)
  raw_data <- parse_cpdivo_data(dat_file, years = 1989:1995)

  cli::cli_alert("Aggregating divorces by detailed ages...")

  # Aggregate by detailed ages
  result <- raw_data[
    !is.na(husband_age_det) & !is.na(wife_age_det),
    .(divorces = sum(weight, na.rm = TRUE)),
    by = .(year, husband_age = husband_age_det, wife_age = wife_age_det)
  ]

  data.table::setorder(result, year, husband_age, wife_age)

  # Cache
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_file)
  n_rows <- format(nrow(result), big.mark = ",")
  cli::cli_alert_success("Cached detailed divorces: {n_rows} rows")

  result[year %in% years]
}

#' Get total DRA divorces by year (1989-1995)
#'
#' @description
#' Returns total divorce counts in the DRA for 1989-1995.
#'
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, total_divorces
#'
#' @export
get_nchs_dra_totals_1989_1995 <- function(
    cache_dir = here::here("data/cache/nber_divorce")
) {
  # Get aggregated data
  data <- fetch_nchs_dra_divorces_1989_1995(cache_dir = cache_dir)

  # Sum by year
  totals <- data[, .(total_divorces = sum(divorces, na.rm = TRUE)), by = year]
  data.table::setorder(totals, year)

  totals
}

# =============================================================================
# TOTAL U.S. DIVORCES (Item 8 for Divorce)
# =============================================================================

#' Fetch total U.S. divorces from NCHS (1979-2022)
#'
#' @description
#' Returns total U.S. divorce counts from NCHS National Vital Statistics.
#' This is Item 8 from TR2025 documentation for the divorce subprocess.
#'
#' Note: For years 1992 and later, the number of divorces is derived by
#' multiplying the rate times the population.
#'
#' @param years Integer vector of years
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, total_divorces, rate_per_1000, source
#'
#' @details
#' Data sources:
#' - CDC/NCHS National Marriage and Divorce Rate Trends
#' - https://www.cdc.gov/nchs/nvss/marriage-divorce.htm
#' - NVSS provisional reports for recent years
#'
#' @export
fetch_nchs_us_total_divorces <- function(
    years = 1979:2022,
    cache_dir = here::here("data/cache/nber_divorce")
) {
  # Published NCHS total U.S. divorces and annulments
  # Source: CDC/NCHS National Vital Statistics Reports
  # Data stored in data/processed/nchs_us_total_divorces.csv
  # See nchs_us_total_divorces_SOURCE.md for provenance

  csv_path <- here::here("data/processed/nchs_us_total_divorces.csv")
  if (!file.exists(csv_path)) {
    cli::cli_abort("NCHS US total divorces CSV not found: {csv_path}")
  }

  nchs_totals <- data.table::fread(csv_path)

  # Filter to requested years
  nchs_totals[year %in% years]
}

# =============================================================================
# PR/VI DIVORCE DATA
# =============================================================================

#' Fetch PR and VI divorce data
#'
#' @description
#' Returns divorce counts for Puerto Rico and Virgin Islands.
#' Item 9 from TR2025: Total divorces in PR and VI for 1988, 1998-2000.
#'
#' Note: For 2008-2022, ACS PUMS data should be used (see acs_divorce.R).
#'
#' @param years Integer vector of years
#'
#' @return data.table with columns: year, territory, divorces, source
#'
#' @export
fetch_pr_vi_divorces <- function(years = c(1988, 1998:2000)) {
  # TR2025 Item 9: "Total number of divorces in Puerto Rico and the Virgin Islands
  # for years 1988, 1998, 1999, and 2000"
  # Data stored in data/processed/nchs_pr_vi_divorces.csv
  # See nchs_pr_vi_divorces_SOURCE.md for provenance

  csv_path <- here::here("data/processed/nchs_pr_vi_divorces.csv")
  if (!file.exists(csv_path)) {
    cli::cli_abort("NCHS PR/VI divorces CSV not found: {csv_path}")
  }

  pr_vi_data <- data.table::fread(csv_path)

  pr_vi_data[year %in% years]
}

# =============================================================================
# SUMMARY FUNCTIONS
# =============================================================================

#' Summary of available NCHS divorce data
#'
#' @description
#' Returns a summary of available NCHS divorce data and its status.
#'
#' @param cache_dir Character path to cache directory
#'
#' @return List with data availability information
#'
#' @export
summarize_nchs_divorce_data <- function(
    cache_dir = here::here("data/cache/nber_divorce")
) {
  # Check what's cached
  cached_files <- list(
    dra_1989_1995 = file.exists(file.path(cache_dir, "nchs_dra_divorces_1989_1995.rds")),
    detailed_1989_1995 = file.exists(file.path(cache_dir, "nchs_dra_divorces_detailed_1989_1995.rds")),
    cpdivo_dat = file.exists(file.path(cache_dir, "cpdivo.dat"))
  )

  summary_info <- list(
    available_years = list(
      dra_by_age_group = 1989:1995,
      detailed_ages = 1989:1995,
      us_totals = 1979:2022,
      pr_vi = c(1988, 1998:2000)
    ),
    cached = cached_files,
    tr2025_items = list(
      item_7 = "Divorces by age-of-husband × age-of-wife (1979-1988): TO IMPLEMENT",
      item_8 = "Total U.S. divorces (1979-2022): AVAILABLE",
      item_9 = "Total PR/VI divorces (1988, 1998-2000): PARTIAL"
    ),
    notes = list(
      "DRA coverage is approximately 48% of U.S. divorces",
      "1989-1995 data uses cpdivo.dat from NBER",
      "1979-1988 individual year files may have different formats"
    )
  )

  cli::cli_h2("NCHS Divorce Data Summary")
  cli::cli_alert_info("Available data periods:")
  cli::cli_bullets(c(
    "*" = "DRA divorces by age group: 1989-1995",
    "*" = "Detailed single-year ages: 1989-1995",
    "*" = "U.S. total divorces: 1979-2022",
    "*" = "PR/VI divorces: 1988, 1998-2000"
  ))

  cli::cli_alert_info("Cached files: {sum(unlist(cached_files))}/{length(cached_files)}")

  invisible(summary_info)
}

#' Validate NCHS divorce data completeness
#'
#' @description
#' Checks that all required divorce data is available and consistent.
#'
#' @param cache_dir Character path to cache directory
#'
#' @return List with validation results
#'
#' @export
validate_nchs_divorce_data <- function(
    cache_dir = here::here("data/cache/nber_divorce")
) {
  results <- list(
    checks = list(),
    passed = 0,
    failed = 0
  )

  # Check 1: 1989-1995 DRA data exists
  tryCatch({
    dra_data <- fetch_nchs_dra_divorces_1989_1995(cache_dir = cache_dir)
    n_years <- length(unique(dra_data$year))
    n_rows <- nrow(dra_data)
    results$checks$dra_1989_1995 <- list(
      passed = n_years == 7 && n_rows > 1000,
      message = paste0("1989-1995 DRA data: ", n_years, " years, ", format(n_rows, big.mark = ","), " rows")
    )
    if (results$checks$dra_1989_1995$passed) results$passed <- results$passed + 1
    else results$failed <- results$failed + 1
  }, error = function(e) {
    results$checks$dra_1989_1995 <- list(
      passed = FALSE,
      message = paste0("Failed to load 1989-1995 DRA data: ", e$message)
    )
    results$failed <- results$failed + 1
  })

  # Check 2: Total U.S. divorces
  us_totals <- fetch_nchs_us_total_divorces()
  n_years <- sum(!is.na(us_totals$total_divorces) | !is.na(us_totals$rate_per_1000))
  results$checks$us_totals <- list(
    passed = n_years >= 30,
    message = paste0("U.S. totals: ", n_years, " years with data")
  )
  if (results$checks$us_totals$passed) results$passed <- results$passed + 1
  else results$failed <- results$failed + 1

  # Check 3: DRA totals reasonable
  if (exists("dra_data")) {
    dra_totals <- dra_data[, .(total = sum(divorces)), by = year]
    # DRA should have 400K-700K divorces per year (roughly 48% of ~1M US total)
    in_range <- all(dra_totals$total >= 300000 & dra_totals$total <= 800000)
    results$checks$dra_totals_reasonable <- list(
      passed = in_range,
      message = paste0("DRA annual totals: ", format(min(dra_totals$total), big.mark = ","),
                       " - ", format(max(dra_totals$total), big.mark = ","))
    )
    if (results$checks$dra_totals_reasonable$passed) results$passed <- results$passed + 1
    else results$failed <- results$failed + 1
  }

  # Report results
  cli::cli_h2("NCHS Divorce Data Validation")
  for (check_name in names(results$checks)) {
    check <- results$checks[[check_name]]
    if (check$passed) {
      cli::cli_alert_success("{check$message}")
    } else {
      cli::cli_alert_danger("{check$message}")
    }
  }
  cli::cli_alert_info("Passed: {results$passed}/{results$passed + results$failed}")

  invisible(results)
}

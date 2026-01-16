#' NCHS Natality Data Acquisition
#'
#' Functions for downloading and processing birth data from NCHS via NBER.
#' Uses Stata .dta files which include variable names and labels.
#'
#' @name nchs_births
NULL

#' Fetch births by single year of mother's age from NCHS
#'
#' @description
#' Downloads NCHS natality microdata from NBER and aggregates to birth counts
#' by mother's single year of age. Results are cached locally to avoid
#' re-downloading large files.
#'
#' @param year Integer: year to fetch (1968-2024 available)
#' @param cache_dir Character: directory to cache downloaded/processed data
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, age, births
#'
#' @details
#' Data source: NBER mirror of NCHS natality microdata
#' URL pattern: https://data.nber.org/nvss/natality/dta/{year}/natality{year}us.dta
#'
#' Variable names differ by year:
#' - 2004-2024: `mager` (mother's single year of age)
#' - 2003: `mager41` (mother's age 41 categories, coded 01-41)
#' - 1968-2002: `dmage` (detail mother's age)
#'
#' Sampling weights:
#' - Pre-1972: 50% sample from all states, no weight variable (multiply by 2)
#' - 1972-2005: Use `recwt` weight variable (mixed sampling through 1984, then 100%)
#' - 2006+: 100% sample, no `recwt` variable (each record = 1 birth)
#'
#' Note: 2005 DTA is unavailable from NBER (403); CSV is used instead.
#'
#' Files are large (500MB-900MB) so results are cached after first download.
#'
#' @export
fetch_nchs_births_by_age <- function(year, cache_dir = "data/raw/nchs", force_download = FALSE) {
  # Validate year
  if (year < 1968 || year > 2024) {
    cli::cli_abort("Year must be between 1968 and 2024")
  }

  # Ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Check for cached aggregated results
  cache_file <- file.path(cache_dir, sprintf("births_by_age_%d.rds", year))
  if (file.exists(cache_file) && !force_download) {
    cli::cli_alert_success("Loading cached data for {year}")
    return(readRDS(cache_file))
  }

  # Download and process
  cli::cli_alert_info("Downloading NCHS natality data for {year}...")
  cli::cli_alert_warning("This may take several minutes (files are 500-900 MB)")



  # File naming conventions vary by year:
  # 1968-1969: natl{year}.dta
  # 1970-1993: natalityus{year}.dta
  # 1994+: natality{year}us.dta
  # Special case: 2005 DTA is unavailable (403), use CSV instead
  if (year == 2005) {
    filename <- "natality2005us.csv"
    url <- sprintf("https://data.nber.org/nvss/natality/csv/2005/%s", filename)
    file_format <- "csv"
  } else if (year <= 1969) {
    filename <- sprintf("natl%d.dta", year)
    url <- sprintf("https://data.nber.org/nvss/natality/dta/%d/%s", year, filename)
    file_format <- "dta"
  } else if (year <= 1993) {
    filename <- sprintf("natalityus%d.dta", year)
    url <- sprintf("https://data.nber.org/nvss/natality/dta/%d/%s", year, filename)
    file_format <- "dta"
  } else {
    filename <- sprintf("natality%dus.dta", year)
    url <- sprintf("https://data.nber.org/nvss/natality/dta/%d/%s", year, filename)
    file_format <- "dta"
  }

  # Download to temp file first
  temp_ext <- if (file_format == "csv") ".csv" else ".dta"
  temp_file <- tempfile(fileext = temp_ext)
  on.exit(unlink(temp_file), add = TRUE)

  # Set longer timeout for large files (default is 60 seconds)
  # 2005 CSV is 2.6GB, needs extra time
  old_timeout <- getOption("timeout")
  timeout_seconds <- if (year == 2005) 1800 else 600  # 30 min for 2005, 10 min for others
  options(timeout = timeout_seconds)
  on.exit(options(timeout = old_timeout), add = TRUE)

  tryCatch({
    download.file(url, temp_file, mode = "wb", quiet = FALSE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download NCHS data for {year}",
      "x" = conditionMessage(e),
      "i" = "URL: {url}"
    ))
  })

  # Read only the columns we need
  cli::cli_alert("Reading and processing data...")

  # Determine which age variable to use based on year
  # 2004+: "mager" (mother's single year of age)
  # 2003: "mager41" (coded 01-41, requires conversion)
  # Pre-2003: "dmage"
  if (year >= 2004) {
    age_var <- "mager"
  } else if (year == 2003) {
    age_var <- "mager41"
  } else {
    age_var <- "dmage"
  }

  # Determine weighting strategy:
  # - Pre-1972: 50% sample, no weight var (multiply by 2)
  # - 1972-2005: Use recwt weight variable
  # - 2006+: 100% sample, no recwt (each record = 1 birth)
  if (year < 1972) {
    cols_to_read <- age_var
    weight_method <- "multiply_by_2"
  } else if (year <= 2005) {
    cols_to_read <- c(age_var, "recwt")
    weight_method <- "use_recwt"
  } else {
    cols_to_read <- age_var
    weight_method <- "count_records"
  }

  # Read the data file (DTA or CSV)
  if (file_format == "csv") {
    # For CSV, use data.table::fread for efficiency
    raw_data <- data.table::fread(temp_file, select = cols_to_read)
  } else {
    # For DTA, use haven::read_dta
    raw_data <- haven::read_dta(temp_file, col_select = dplyr::all_of(cols_to_read))
  }

  # Aggregate to counts by age
  dt <- data.table::as.data.table(raw_data)
  data.table::setnames(dt, age_var, "age")

  # Convert mager41 codes to actual ages for 2003 only
  # mager41 coding: 01=Under 15, 02=15, 03=16, ..., 41=54
  # Formula: actual_age = mager41 + 13 (code 01 maps to 14)
  # Note: 2004+ uses mager which is already actual age
  if (year == 2003) {
    dt[, age := age + 13L]
    dt[age < 14, age := 14L]  # Code 01 "Under 15" -> age 14
  }

  if (weight_method == "use_recwt") {
    # Use record weights for weighted sum
    result <- dt[, .(births = sum(recwt)), by = age]
  } else if (weight_method == "multiply_by_2") {
    # Pre-1972: 50% sample, multiply counts by 2
    result <- dt[, .(births = .N * 2L), by = age]
  } else {
    # 2016+: 100% sample, each record is one birth
    result <- dt[, .(births = .N), by = age]
  }
  result[, year := year]
  data.table::setcolorder(result, c("year", "age", "births"))
  data.table::setorder(result, age)

  # Remove any invalid ages (coded as 99 or similar)
  result <- result[age >= 10 & age <= 54]

  # Cache the result

  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached results to {cache_file}")

  cli::cli_alert_success("Retrieved {sum(result$births)} total births for {year}")
  result
}

#' Fetch births by age for multiple years
#'
#' @description
#' Downloads and aggregates birth data for multiple years.
#'
#' @param years Integer vector: years to fetch
#' @param cache_dir Character: directory to cache data
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, age, births
#'
#' @export
fetch_nchs_births_by_age_multi <- function(years, cache_dir = "data/raw/nchs", force_download = FALSE) {
  results <- list()


  for (yr in years) {
    cli::cli_alert("Processing year {yr}...")
    tryCatch({
      results[[as.character(yr)]] <- fetch_nchs_births_by_age(
        year = yr,
        cache_dir = cache_dir,
        force_download = force_download
      )
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No data retrieved")
  }

  data.table::rbindlist(results, use.names = TRUE)
}

#' Get available NCHS natality years
#'
#' @description
#' Returns the range of years available from NCHS natality data.
#'
#' @return Integer vector of available years
#'
#' @export
get_nchs_natality_years <- function() {
  1968:2024
}

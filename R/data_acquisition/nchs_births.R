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
  timeout_seconds <- if (year %in% 2003:2005) 1800 else 900  # 30 min for large files (2003-2005), 15 min for others  # 30 min for 2005, 10 min for others
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

#' Fetch births by month from NCHS
#'
#' @description
#' Downloads NCHS natality microdata from NBER and aggregates to birth counts
#' by month. Results are cached locally. This is used for infant mortality
#' calculations which require monthly birth data.
#'
#' @param year Integer: year to fetch (1968-2024 available)
#' @param cache_dir Character: directory to cache downloaded/processed data
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, month, births
#'
#' @details
#' Data source: NBER mirror of NCHS natality microdata
#'
#' Variable names for birth month differ by year:
#' - 2003+: `dob_mm` (date of birth month)
#' - Pre-2003: `birmon` (birth month)
#'
#' Sampling weights are applied as in fetch_nchs_births_by_age:
#' - Pre-1972: 50% sample (multiply by 2)
#' - 1972-2005: Use `recwt` weight variable
#' - 2006+: 100% sample (no weighting needed)
#'
#' @export
fetch_nchs_births_by_month <- function(year, cache_dir = "data/raw/nchs", force_download = FALSE) {
  # Validate year
  if (year < 1968 || year > 2024) {
    cli::cli_abort("Year must be between 1968 and 2024")
  }

  # Ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Check for cached aggregated results
  cache_file <- file.path(cache_dir, sprintf("births_by_month_%d.rds", year))
  if (file.exists(cache_file) && !force_download) {
    cli::cli_alert_success("Loading cached monthly births for {year}")
    return(readRDS(cache_file))
  }

  # Download and process
  cli::cli_alert_info("Downloading NCHS natality data for monthly births {year}...")
  cli::cli_alert_warning("This may take several minutes (files are 500-900 MB)")

  # Get file URL - same logic as fetch_nchs_births_by_age
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

  # Download to temp file
  temp_ext <- if (file_format == "csv") ".csv" else ".dta"
  temp_file <- tempfile(fileext = temp_ext)
  on.exit(unlink(temp_file), add = TRUE)

  old_timeout <- getOption("timeout")
  timeout_seconds <- if (year %in% 2003:2005) 1800 else 900  # 30 min for large files (2003-2005), 15 min for others
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

  cli::cli_alert("Reading and processing monthly birth data...")

  # Determine month variable name based on year
  # 2003+ uses dob_mm (date of birth month)
  # Pre-2003 uses birmon (birth month)
  if (year >= 2003) {
    month_var <- "dob_mm"
  } else {
    month_var <- "birmon"
  }

  # Determine weighting strategy
  if (year < 1972) {
    cols_to_read <- month_var
    weight_method <- "multiply_by_2"
  } else if (year <= 2005) {
    cols_to_read <- c(month_var, "recwt")
    weight_method <- "use_recwt"
  } else {
    cols_to_read <- month_var
    weight_method <- "count_records"
  }

  # Read the data file
  if (file_format == "csv") {
    raw_data <- data.table::fread(temp_file, select = cols_to_read)
  } else {
    raw_data <- haven::read_dta(temp_file, col_select = dplyr::all_of(cols_to_read))
  }

  # Aggregate to counts by month
  dt <- data.table::as.data.table(raw_data)
  data.table::setnames(dt, month_var, "month")

  if (weight_method == "use_recwt") {
    result <- dt[, .(births = sum(recwt)), by = month]
  } else if (weight_method == "multiply_by_2") {
    result <- dt[, .(births = .N * 2L), by = month]
  } else {
    result <- dt[, .(births = .N), by = month]
  }

  result[, year := year]
  data.table::setcolorder(result, c("year", "month", "births"))
  data.table::setorder(result, month)

  # Remove invalid months
  result <- result[month >= 1 & month <= 12]

  # Cache the result
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached monthly births to {cache_file}")
  cli::cli_alert_success("Retrieved {sum(result$births)} total births for {year}")

  result
}

#' Fetch monthly births for multiple years
#'
#' @description
#' Downloads and aggregates monthly birth data for multiple years.
#'
#' @param years Integer vector: years to fetch
#' @param cache_dir Character: directory to cache data
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, month, births
#'
#' @export
fetch_nchs_births_by_month_multi <- function(years, cache_dir = "data/raw/nchs",
                                              force_download = FALSE) {
  results <- list()

  for (yr in years) {
    cli::cli_alert("Processing monthly births for {yr}...")
    tryCatch({
      results[[as.character(yr)]] <- fetch_nchs_births_by_month(
        year = yr,
        cache_dir = cache_dir,
        force_download = force_download
      )
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No monthly birth data retrieved")
  }

  data.table::rbindlist(results, use.names = TRUE)
}

#' Fetch births by month and sex from NCHS
#'
#' @description
#' Downloads NCHS natality microdata from NBER and aggregates to birth counts
#' by month and infant sex. Results are cached locally. This provides the
#' sex-specific birth data needed to match SSA methodology.
#'
#' @param year Integer: year to fetch (1968-2024 available)
#' @param cache_dir Character: directory to cache downloaded/processed data
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, month, sex, births
#'
#' @details
#' Data source: NBER mirror of NCHS natality microdata
#'
#' Variable names for infant sex:
#' - All years: `sex` (values: M=Male, F=Female)
#'
#' Sampling weights are applied as in fetch_nchs_births_by_month:
#' - Pre-1972: 50% sample (multiply by 2)
#' - 1972-2005: Use `recwt` weight variable
#' - 2006+: 100% sample (no weighting needed)
#'
#' @export
fetch_nchs_births_by_month_sex <- function(year, cache_dir = "data/raw/nchs", force_download = FALSE) {
  # Validate year
  if (year < 1968 || year > 2024) {
    cli::cli_abort("Year must be between 1968 and 2024")
  }

  # Ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Check for cached aggregated results
  cache_file <- file.path(cache_dir, sprintf("births_by_month_sex_%d.rds", year))
  if (file.exists(cache_file) && !force_download) {
    cli::cli_alert_success("Loading cached monthly births by sex for {year}")
    return(readRDS(cache_file))
  }

  # Download and process
  cli::cli_alert_info("Downloading NCHS natality data for monthly births by sex {year}...")
  cli::cli_alert_warning("This may take several minutes (files are 500-900 MB)")

  # Get file URL - same logic as fetch_nchs_births_by_age
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

  # Download to temp file
  temp_ext <- if (file_format == "csv") ".csv" else ".dta"
  temp_file <- tempfile(fileext = temp_ext)
  on.exit(unlink(temp_file), add = TRUE)

  old_timeout <- getOption("timeout")
  timeout_seconds <- if (year %in% 2003:2005) 1800 else 900  # 30 min for large files (2003-2005), 15 min for others
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

  cli::cli_alert("Reading and processing monthly birth data by sex...")

  # Determine month variable name based on year
  # 2003+ uses dob_mm (date of birth month)
  # Pre-2003 uses birmon (birth month)
  if (year >= 2003) {
    month_var <- "dob_mm"
  } else {
    month_var <- "birmon"
  }

  # Sex variable name varies by year
  # 2005+: "sex" (infant sex)
  # 2003-2004: "sex" (NBER changed naming convention)
  # 1968-2002: "csex" (child sex)
  if (year >= 2003) {
    sex_var <- "sex"
  } else {
    sex_var <- "csex"
  }

  # Determine weighting strategy
  if (year < 1972) {
    cols_to_read <- c(month_var, sex_var)
    weight_method <- "multiply_by_2"
  } else if (year <= 2005) {
    cols_to_read <- c(month_var, sex_var, "recwt")
    weight_method <- "use_recwt"
  } else {
    cols_to_read <- c(month_var, sex_var)
    weight_method <- "count_records"
  }

  # Read the data file
  if (file_format == "csv") {
    raw_data <- data.table::fread(temp_file, select = cols_to_read)
  } else {
    raw_data <- haven::read_dta(temp_file, col_select = dplyr::all_of(cols_to_read))
  }

  # Aggregate to counts by month and sex
  dt <- data.table::as.data.table(raw_data)
  data.table::setnames(dt, month_var, "month")
  data.table::setnames(dt, sex_var, "sex")

  # Standardize sex values to "male"/"female"
  dt[, sex := data.table::fcase(
    sex %in% c("M", "1", 1), "male",
    sex %in% c("F", "2", 2), "female",
    default = NA_character_
  )]

  # Remove records with unknown sex
  dt <- dt[!is.na(sex)]

  if (weight_method == "use_recwt") {
    result <- dt[, .(births = sum(recwt)), by = .(month, sex)]
  } else if (weight_method == "multiply_by_2") {
    result <- dt[, .(births = .N * 2L), by = .(month, sex)]
  } else {
    result <- dt[, .(births = .N), by = .(month, sex)]
  }

  result[, year := year]
  data.table::setcolorder(result, c("year", "month", "sex", "births"))
  data.table::setorder(result, month, sex)

  # Remove invalid months
  result <- result[month >= 1 & month <= 12]

  # Cache the result
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached monthly births by sex to {cache_file}")

  # Report totals by sex
  totals <- result[, .(total = sum(births)), by = sex]
  cli::cli_alert_success("Retrieved {sum(result$births)} total births for {year}")
  cli::cli_alert_info("  Male: {totals[sex == 'male', total]}, Female: {totals[sex == 'female', total]}")

  result
}

#' Fetch monthly births by sex for multiple years
#'
#' @description
#' Downloads and aggregates monthly birth data by sex for multiple years.
#'
#' @param years Integer vector: years to fetch
#' @param cache_dir Character: directory to cache data
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, month, sex, births
#'
#' @export
fetch_nchs_births_by_month_sex_multi <- function(years, cache_dir = "data/raw/nchs",
                                                  force_download = FALSE) {
  results <- list()

  for (yr in years) {
    cli::cli_alert("Processing monthly births by sex for {yr}...")
    tryCatch({
      results[[as.character(yr)]] <- fetch_nchs_births_by_month_sex(
        year = yr,
        cache_dir = cache_dir,
        force_download = force_download
      )
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No monthly birth data by sex retrieved")
  }

  data.table::rbindlist(results, use.names = TRUE)
}

#' Load Historical Births by Sex (1940-1967)
#'
#' @description
#' Loads pre-1968 births by sex from processed CSV file. These data come from
#' CDC NCHS NVSR Vol. 53 No. 20, Table 1 ("Number of male and female births...
#' United States, 1940-2002").
#'
#' TR2025 Input Data Items 35 (Section 1.4.b) and 32 (Section 1.5.b) require
#' "births by month and sex of child, for years 1931-2023". The 1968+ data
#' comes from NBER microdata; this function provides the 1940-1967 subset.
#'
#' @param file_path Character: path to the CSV file.
#'
#' @return data.table with columns: year (integer), sex (character), births (numeric).
#'   Matches the schema of the post-1968 data from the nchs_births_by_sex target.
#'
#' @export
load_historical_births_by_sex <- function(
    file_path = here::here("data/processed/nchs_births_by_sex_1940_1967.csv")
) {
  if (!file.exists(file_path)) {
    cli::cli_abort(c(
      "Historical births CSV not found at {.path {file_path}}",
      "i" = "Source: CDC NCHS NVSR Vol. 53 No. 20, Table 1",
      "i" = "See data/processed/nchs_births_by_sex_1940_1967_SOURCE.md for details"
    ))
  }
  dt <- data.table::fread(file_path)

  # Validate expected columns
  required_cols <- c("year", "sex", "births")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Historical births CSV missing required columns: {.field {missing_cols}}",
      "i" = "Expected columns: year, sex, births"
    ))
  }

  # Validate data range
  if (min(dt$year) != 1940L || max(dt$year) != 1967L) {
    cli::cli_abort(c(
      "Historical births CSV should cover 1940-1967",
      "i" = "Found years {min(dt$year)}-{max(dt$year)}"
    ))
  }

  # Ensure correct types
  dt[, year := as.integer(year)]
  dt[, births := as.numeric(births)]

  dt
}

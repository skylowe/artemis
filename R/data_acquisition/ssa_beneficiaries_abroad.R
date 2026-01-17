#' SSA Beneficiaries Abroad Data Acquisition
#'
#' Functions for fetching OASDI beneficiaries living abroad from
#' SSA Annual Statistical Supplement data.
#'
#' Data sources:
#' - Table 5.J11: Beneficiaries in foreign countries (2000-2023)
#'   - supplement13.xlsx through supplement24.xlsx for 2013-2023
#'   - 5.J11_2000_2012.xlsx for 2000-2012
#' - Table 5.M1: International agreement beneficiaries (historical 1983-2023)
#'
#' @name ssa_beneficiaries_abroad
NULL

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Fetch OASDI beneficiaries living abroad
#'
#' @description
#' Retrieves estimates of OASDI beneficiaries living in foreign countries.
#' Uses SSA Annual Statistical Supplement data from Table 5.J11.
#'
#' @param years Integer vector of years to query (2000-2023 available)
#' @param data_dir Character: directory containing SSA supplement files
#'
#' @return data.table with columns: year, total_beneficiaries, retired_workers,
#'   disabled_workers, spouses, survivors, children
#'
#' @details
#' Data sources:
#' - Years 2000-2012: 5.J11_2000_2012.xlsx (pre-compiled historical data)
#' - Years 2013-2023: Individual supplement files (supplement13.xlsx, etc.)
#'
#' @export
fetch_ssa_beneficiaries_abroad <- function(years = 2000:2023,
                                            data_dir = here::here("data/raw/SSA_supplemental")) {
  checkmate::assert_integerish(years, lower = 1983, upper = 2030, min.len = 1)
  checkmate::assert_directory_exists(data_dir)

  cli::cli_alert_info("Fetching SSA beneficiaries abroad data...")

  results <- list()

  # Split years into pre-2013 and 2013+
  years_historical <- years[years >= 2000 & years <= 2012]
  years_recent <- years[years >= 2013 & years <= 2023]

  # Read 2000-2012 from compiled historical file
  if (length(years_historical) > 0) {
    hist_file <- file.path(data_dir, "5.J11_2000_2012.xlsx")
    if (file.exists(hist_file)) {
      cli::cli_alert("Reading historical data (2000-2012) from 5.J11_2000_2012.xlsx...")
      hist_data <- read_5j11_historical(hist_file, years_historical)
      if (!is.null(hist_data) && nrow(hist_data) > 0) {
        results[["historical"]] <- hist_data
        cli::cli_alert_success("Read {nrow(hist_data)} years of historical data")
      }
    } else {
      cli::cli_alert_warning("Historical file not found: {hist_file}")
    }
  }

  # Read 2013-2023 from individual supplement files
  if (length(years_recent) > 0) {
    cli::cli_alert("Reading recent data (2013-2023) from supplement files...")
    recent_data <- read_5j11_supplements(data_dir, years_recent)
    if (!is.null(recent_data) && nrow(recent_data) > 0) {
      results[["recent"]] <- recent_data
      cli::cli_alert_success("Read {nrow(recent_data)} years of recent data")
    }
  }

  # Combine results
  if (length(results) == 0) {
    cli::cli_alert_warning("No data retrieved")
    return(data.table::data.table())
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  data.table::setorder(combined, year)

  # Add source information
  combined[, source := "ssa_supplement_5j11"]

  cli::cli_alert_success("Retrieved beneficiaries abroad for {nrow(combined)} years ({min(combined$year)}-{max(combined$year)})")

  combined
}

# =============================================================================
# TABLE READERS
# =============================================================================

#' Read Table 5.J11 from historical compiled file (2000-2012)
#'
#' @keywords internal
read_5j11_historical <- function(file_path, years) {
  sheets <- readxl::excel_sheets(file_path)

  results <- list()

  for (yr in years) {
    sheet_name <- as.character(yr)
    if (!sheet_name %in% sheets) {
      cli::cli_alert_warning("Sheet {sheet_name} not found in historical file")
      next
    }

    tryCatch({
      dt <- data.table::as.data.table(
        readxl::read_excel(file_path, sheet = sheet_name)
      )

      # Find the Total row - different years use "Total" or "All countries"
      total_row <- dt[grepl("^(Total|All countries)$", dt[[1]], ignore.case = TRUE)]

      if (nrow(total_row) == 0) {
        cli::cli_alert_warning("No Total row found for year {yr}")
        next
      }

      # Extract values - column names from the historical file
      results[[as.character(yr)]] <- data.table::data.table(
        year = as.integer(yr),
        total_beneficiaries = as.integer(total_row$num_all_beneficiaries),
        retired_workers = as.integer(total_row$num_retired_workers),
        disabled_workers = as.integer(total_row$num_disabled_workers),
        survivors = as.integer(total_row$num_widowers_parents),
        spouses = as.integer(total_row$num_wives_husbands),
        children = as.integer(total_row$num_children)
      )
    }, error = function(e) {
      cli::cli_alert_warning("Error reading year {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) return(NULL)

  data.table::rbindlist(results, use.names = TRUE)
}

#' Read Table 5.J11 from individual supplement files (2013-2023)
#'
#' @keywords internal
read_5j11_supplements <- function(data_dir, years) {
  results <- list()

  for (yr in years) {
    # Supplement numbering: supplement24 has Dec 2023 data, supplement14 has Dec 2013 data
    # So supplement number = data year - 1999
    supp_num <- yr - 1999  # 2023 -> 24, 2013 -> 14

    file_name <- sprintf("supplement%02d.xlsx", supp_num)
    file_path <- file.path(data_dir, file_name)

    if (!file.exists(file_path)) {
      cli::cli_alert_warning("File not found: {file_name}")
      next
    }

    tryCatch({
      dt <- data.table::as.data.table(
        suppressMessages(readxl::read_excel(file_path, sheet = "5.J11", skip = 2))
      )

      # Column structure (after skip=2):
      # 1: Region and country a
      # 2: ...2 (country detail)
      # 3: ...3 (contains "Total" marker)
      # 4: Number / All beneficiaries
      # 5: Retired workers
      # 6: Disabled workers
      # 7: Spouses
      # 8: Widow(er)s and parents
      # 9: Children
      # 10-11: Benefits columns

      # Rename columns for clarity
      col_names <- c("region", "country", "marker", "total", "retired",
                     "disabled", "spouses", "survivors", "children")
      if (ncol(dt) >= 9) {
        data.table::setnames(dt, 1:9, col_names)
      }

      # Find the Total row - has "Total" in the marker column (col 3)
      total_row <- dt[marker == "Total"]

      if (nrow(total_row) == 0) {
        # Alternative: look for row with large total value
        dt[, total_num := suppressWarnings(as.numeric(total))]
        total_row <- dt[total_num > 400000][1]
      }

      if (nrow(total_row) == 0 || is.null(total_row) || all(is.na(total_row))) {
        cli::cli_alert_warning("No Total row found for year {yr} in {file_name}")
        next
      }

      results[[as.character(yr)]] <- data.table::data.table(
        year = as.integer(yr),
        total_beneficiaries = as.integer(as.numeric(total_row$total)),
        retired_workers = as.integer(as.numeric(total_row$retired)),
        disabled_workers = as.integer(as.numeric(total_row$disabled)),
        survivors = as.integer(as.numeric(total_row$survivors)),
        spouses = as.integer(as.numeric(total_row$spouses)),
        children = as.integer(as.numeric(total_row$children))
      )
    }, error = function(e) {
      cli::cli_alert_warning("Error reading {file_name}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) return(NULL)

  data.table::rbindlist(results, use.names = TRUE)
}

# =============================================================================
# HISTORICAL EXTENSION (pre-2000)
# =============================================================================

#' Get international agreement beneficiaries (for pre-2000 estimates)
#'
#' @description
#' Reads Table 5.M1 for international agreement beneficiaries,
#' which provides a time series back to 1983. This can be used
#' to estimate total abroad for years before 2000.
#'
#' @param data_dir Character: directory containing SSA supplement files
#' @param years Integer vector of years
#'
#' @return data.table with international agreement beneficiary counts
#'
#' @export
fetch_international_agreement_beneficiaries <- function(data_dir = here::here("data/raw/SSA_supplemental"),
                                                         years = 1983:2023) {
  supplement_file <- file.path(data_dir, "supplement24.xlsx")

  if (!file.exists(supplement_file)) {
    cli::cli_abort("Supplement file not found: {supplement_file}")
  }

  cli::cli_alert("Reading Table 5.M1 (international agreements)...")

  dt <- data.table::as.data.table(
    readxl::read_excel(supplement_file, sheet = "5.M1", skip = 2)
  )

  # Set column names
  if (ncol(dt) >= 8) {
    data.table::setnames(dt, 1:8, c(
      "year_country", "note", "total", "retired", "disabled",
      "spouses", "survivors", "children"
    ))
  }

  # Extract year rows only (4-digit years)
  dt <- dt[!is.na(year_country)]
  year_rows <- dt[grepl("^[0-9]{4}$", year_country)]

  if (nrow(year_rows) == 0) {
    return(NULL)
  }

  # Parse and filter to counts (not average benefits)
  year_rows[, year := as.integer(year_country)]
  year_rows[, total_num := as.numeric(total)]

  # Counts section has values > 1000
  counts_section <- year_rows[total_num > 1000]

  result <- counts_section[, .(
    year = year,
    total_international = as.integer(total_num),
    retired_workers = as.integer(as.numeric(retired)),
    disabled_workers = as.integer(as.numeric(disabled)),
    spouses = as.integer(as.numeric(spouses)),
    survivors = as.integer(as.numeric(survivors)),
    children = as.integer(as.numeric(children))
  )]

  result <- result[year %in% years]
  data.table::setorder(result, year)

  cli::cli_alert_success("Retrieved international agreement data for {nrow(result)} years")

  result
}

#' Estimate beneficiaries abroad for pre-2000 years
#'
#' @description
#' Estimates total beneficiaries abroad for years before 2000
#' by scaling international agreement data using the observed
#' ratio in early 2000s.
#'
#' @param years Integer vector of years to estimate
#' @param data_dir Character: directory containing SSA supplement files
#'
#' @return data.table with estimated beneficiaries abroad
#'
#' @export
estimate_beneficiaries_abroad_pre2000 <- function(years = 1983:1999,
                                                   data_dir = here::here("data/raw/SSA_supplemental")) {
  # Get actual data for 2000-2005 to establish ratio
  actual <- fetch_ssa_beneficiaries_abroad(years = 2000:2005, data_dir = data_dir)
  intl <- fetch_international_agreement_beneficiaries(data_dir = data_dir, years = 2000:2005)

  if (nrow(actual) == 0 || nrow(intl) == 0) {
    cli::cli_abort("Cannot estimate: missing reference data")
  }

  # Calculate average ratio of total to international
  merged <- merge(actual, intl, by = "year", suffixes = c("_total", "_intl"))
  merged[, ratio := total_beneficiaries / total_international]
  avg_ratio <- mean(merged$ratio, na.rm = TRUE)

  cli::cli_alert_info("Average ratio (total/international): {round(avg_ratio, 2)}")

  # Get international data for requested years
  intl_pre2000 <- fetch_international_agreement_beneficiaries(data_dir = data_dir, years = years)

  if (nrow(intl_pre2000) == 0) {
    return(data.table::data.table())
  }

  # Apply ratio to estimate total
  result <- data.table::copy(intl_pre2000)
  result[, total_beneficiaries := as.integer(total_international * avg_ratio)]
  result[, retired_workers := as.integer(retired_workers * avg_ratio)]
  result[, disabled_workers := as.integer(disabled_workers * avg_ratio)]
  result[, spouses := as.integer(spouses * avg_ratio)]
  result[, survivors := as.integer(survivors * avg_ratio)]
  result[, children := as.integer(children * avg_ratio)]
  result[, source := "estimated_from_5m1"]
  result[, total_international := NULL]

  result
}

# =============================================================================
# SUMMARY FUNCTIONS
# =============================================================================

#' Get beneficiaries abroad total for a single year
#'
#' @param year Integer: year to query
#' @param data_dir Character: directory containing SSA supplement files
#'
#' @return Integer: total beneficiaries abroad
#'
#' @export
get_beneficiaries_abroad_total <- function(year,
                                            data_dir = here::here("data/raw/SSA_supplemental")) {
  if (year >= 2000 && year <= 2023) {
    data <- fetch_ssa_beneficiaries_abroad(years = year, data_dir = data_dir)
  } else if (year >= 1983 && year < 2000) {
    data <- estimate_beneficiaries_abroad_pre2000(years = year, data_dir = data_dir)
  } else {
    cli::cli_alert_warning("Year {year} outside available range (1983-2023)")
    return(NA_integer_)
  }

  if (nrow(data) == 0 || !year %in% data$year) {
    return(NA_integer_)
  }

  data[year == year, total_beneficiaries]
}

#' Summarize beneficiaries abroad data availability
#'
#' @export
summarize_beneficiaries_abroad_availability <- function() {
  data.table::data.table(
    source = c("5.J11 (historical)", "5.J11 (supplements)", "5.M1 (international)"),
    file = c("5.J11_2000_2012.xlsx", "supplement13-24.xlsx", "supplement24.xlsx"),
    years = c("2000-2012", "2013-2023", "1983-2023"),
    description = c(
      "Total beneficiaries in foreign countries by type",
      "Same data from annual supplements",
      "International agreement beneficiaries (subset of total)"
    ),
    notes = c(
      "Direct counts from compiled historical data",
      "Extracted from sheet 5.J11 in each supplement",
      "Can be scaled to estimate total for pre-2000 years"
    )
  )
}

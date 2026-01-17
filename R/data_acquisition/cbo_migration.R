#' CBO Migration Data Acquisition
#'
#' Functions for loading and processing CBO migration data.
#' CBO provides gross migration flows by age, sex, immigration status, and flow type.
#'
#' @name cbo_migration
NULL

#' Load CBO gross migration data
#'
#' @description
#' Loads the CBO gross migration data from the January 2026 Demographic Outlook.
#' Data includes immigration and emigration by single year of age, sex, and
#' immigration status (LPR+, INA nonimmigrant, OFN).
#'
#' @param file_path Character: path to CSV file
#'
#' @return data.table with columns: year, age, sex, immigration_status,
#'   migration_flow, number_of_people
#'
#' @details
#' Data source: CBO January 2026 report "The Demographic Outlook: 2026 to 2056"
#' https://www.cbo.gov/publication/61879
#'
#' Immigration statuses:
#' - LPR+: Lawful permanent residents (and naturalized citizens)
#' - INA nonimmigrant: Temporary visitors under Immigration and Nationality Act
#' - OFN: Other foreign nationals
#'
#' Age handling:
#' - CBO uses -1 for births during year
#' - "100+" is converted to 100
#' - Age is converted to integer
#'
#' @export
load_cbo_migration <- function(file_path = "data/raw/cbo/grossMigration_byYearAgeSexStatusFlow.csv") {
  if (!file.exists(file_path)) {
    cli::cli_abort("CBO migration file not found: {file_path}")
  }

  dt <- data.table::fread(file_path)

  # Validate expected columns
  expected_cols <- c("year", "age", "sex", "immigration_status", "migration_flow", "number_of_people")
  missing <- setdiff(expected_cols, names(dt))
  if (length(missing) > 0) {
    cli::cli_abort("Missing columns in CBO data: {paste(missing, collapse = ', ')}")
  }

  # Convert age to integer (handle "100+" and "-1")
  dt[, age := as.character(age)]
  dt[age == "100+", age := "100"]
  dt[, age := as.integer(age)]

  cli::cli_alert_success("Loaded CBO migration data: {nrow(dt)} rows, years {min(dt$year)}-{max(dt$year)}")
  dt
}

#' Get LPR+ emigration data from CBO
#'
#' @description
#' Extracts LPR+ emigration data from CBO migration data.
#'
#' @param cbo_data data.table from load_cbo_migration()
#' @param years Integer vector: years to include (default: all available)
#'
#' @return data.table with columns: year, age, sex, emigration
#'
#' @export
get_cbo_lpr_emigration <- function(cbo_data, years = NULL) {
  # Filter to LPR+ emigration
  result <- cbo_data[immigration_status == "LPR+" & migration_flow == "emigration"]

  if (!is.null(years)) {
    result <- result[year %in% years]
  }

  # Filter to valid ages (0+)
  result <- result[age >= 0]

  # Rename and select columns
  result <- result[, .(year, age, sex, emigration = number_of_people)]

  result
}

#' Calculate emigration age-sex distribution from CBO data
#'
#' @description
#' Calculates the age-sex distribution of LPR+ emigration from CBO data.
#' This distribution is used to allocate aggregate emigration totals
#' by age and sex for projections.
#'
#' @param cbo_data data.table from load_cbo_migration()
#' @param reference_years Integer vector: years to use for distribution
#'   (default: 2021:2024, the historical/near-term years)
#'
#' @return data.table with columns: age, sex, distribution
#'   where distribution sums to 1.0
#'
#' @details
#' This function extracts the emigration pattern from CBO's historical
#' estimates (2021-2024) to use as a distribution for projecting
#' emigration under TR methodology.
#'
#' Note: This deviates from TR2025 methodology which uses unpublished

#' Census 1980-1990 emigration estimates. We use CBO 2021-2024 data
#' instead because:
#' 1. Census data is unpublished and unavailable
#' 2. CBO data is more recent
#' 3. CBO provides single-year-of-age (no Beers interpolation needed)
#'
#' @export
calculate_cbo_emigration_distribution <- function(cbo_data,
                                                   reference_years = 2021:2024) {
  # Get LPR+ emigration for reference years
  emig <- cbo_data[immigration_status == "LPR+" &
                   migration_flow == "emigration" &
                   year %in% reference_years &
                   age >= 0]

  if (nrow(emig) == 0) {
    cli::cli_abort("No emigration data found for reference years {paste(reference_years, collapse=', ')}")
  }

  # Aggregate across reference years
  dist <- emig[, .(count = sum(number_of_people)), by = .(age, sex)]

  # Calculate distribution (sums to 1.0)
  total <- sum(dist$count)
  dist[, distribution := count / total]

  # Report summary
  by_sex <- dist[, .(total = sum(count), pct = sum(distribution) * 100), by = sex]
  cli::cli_alert_info("Emigration distribution from {min(reference_years)}-{max(reference_years)}")
  cli::cli_alert_info("Sex split: {by_sex[sex=='female', round(pct, 1)]}% female, {by_sex[sex=='male', round(pct, 1)]}% male")

  dist[, .(age, sex, distribution)]
}

#' Get CBO LPR+ immigration data
#'
#' @description
#' Extracts LPR+ immigration data from CBO migration data.
#'
#' @param cbo_data data.table from load_cbo_migration()
#' @param years Integer vector: years to include (default: all available)
#'
#' @return data.table with columns: year, age, sex, immigration
#'
#' @export
get_cbo_lpr_immigration <- function(cbo_data, years = NULL) {
  # Filter to LPR+ immigration
  result <- cbo_data[immigration_status == "LPR+" & migration_flow == "immigration"]

  if (!is.null(years)) {
    result <- result[year %in% years]
  }

  # Filter to valid ages (0+)
  result <- result[age >= 0]

  # Rename and select columns
  result <- result[, .(year, age, sex, immigration = number_of_people)]

  result
}

#' Get available CBO migration years
#'
#' @description
#' Returns the range of years available in CBO migration data.
#'
#' @param cbo_data data.table from load_cbo_migration()
#'
#' @return Integer vector of available years
#'
#' @export
get_cbo_migration_years <- function(cbo_data) {
  sort(unique(cbo_data$year))
}

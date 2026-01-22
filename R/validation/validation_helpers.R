#' Validation Helper Functions
#'
#' @description
#' Common helper functions for validation targets to reduce inline code
#' in target factories.
#'
#' @name validation_helpers
NULL

#' Create validation comparison
#'
#' @description
#' Creates a standard comparison between calculated and reference values.
#' Computes absolute and percentage differences.
#'
#' @param calculated data.table with calculated values
#' @param reference data.table with reference values
#' @param by Character vector of columns to join by
#' @param calc_col Name of calculated value column
#' @param ref_col Name of reference value column
#'
#' @return data.table with comparison columns: calculated, reference, diff, pct_diff
#'
#' @export
create_validation_comparison <- function(calculated, reference, by,
                                          calc_col = "calculated",
                                          ref_col = "reference") {
  # Merge calculated and reference
  comparison <- merge(calculated, reference, by = by, all.x = TRUE)

  # Rename columns for clarity
  if (calc_col != "calculated" && calc_col %in% names(comparison)) {
    data.table::setnames(comparison, calc_col, "calculated")
  }
  if (ref_col != "reference" && ref_col %in% names(comparison)) {
    data.table::setnames(comparison, ref_col, "reference")
  }

  # Calculate differences
  comparison[, diff := abs(calculated - reference)]
  comparison[reference != 0, pct_diff := diff / abs(reference) * 100]
  comparison[reference == 0 & calculated == 0, pct_diff := 0]

  comparison
}

#' Validate totals match
#'
#' @description
#' Validates that aggregated totals match between two data.tables within tolerance.
#'
#' @param data1 First data.table
#' @param data2 Second data.table
#' @param by Character vector of columns to group by
#' @param value_col Name of value column to sum
#' @param tolerance Maximum allowed percentage difference
#' @param label1 Label for first data source (for messages)
#' @param label2 Label for second data source (for messages)
#'
#' @return List with validation results: valid, max_pct_diff, comparison
#'
#' @export
validate_totals_match <- function(data1, data2, by, value_col,
                                   tolerance = 0.01,
                                   label1 = "calculated",
                                   label2 = "reference") {
  # Aggregate both data.tables
  totals1 <- data1[, .(total1 = sum(get(value_col), na.rm = TRUE)), by = by]
  totals2 <- data2[, .(total2 = sum(get(value_col), na.rm = TRUE)), by = by]

  # Merge and compare
  comparison <- merge(totals1, totals2, by = by, all = TRUE)
  comparison[is.na(total1), total1 := 0]
  comparison[is.na(total2), total2 := 0]
  comparison[, diff := abs(total1 - total2)]
  comparison[total2 > 0, pct_diff := diff / total2 * 100]

  max_pct_diff <- comparison[, max(pct_diff, na.rm = TRUE)]
  valid <- max_pct_diff < tolerance

  if (valid) {
    cli::cli_alert_success("PASS: {label1} totals match {label2} (max diff: {round(max_pct_diff, 4)}%)")
  } else {
    cli::cli_alert_warning("WARNING: {label1} totals differ from {label2} by up to {round(max_pct_diff, 2)}%")
  }

  list(
    valid = valid,
    max_pct_diff = max_pct_diff,
    comparison = comparison
  )
}

#' Validate year totals against TR2025
#'
#' @description
#' Validates population totals by year against TR2025 reference data.
#'
#' @param population data.table with columns year, population
#' @param tr2025_pop data.table with TR2025 population (Year, Total columns)
#' @param years Years to validate (NULL = all available)
#' @param tolerance Maximum allowed percentage difference
#'
#' @return List with validation results
#'
#' @export
validate_year_totals_vs_tr2025 <- function(population, tr2025_pop,
                                            years = NULL, tolerance = 0.02) {
  if (is.null(tr2025_pop)) {
    cli::cli_warn("Skipping validation - TR2025 data not available")
    return(list(validated = FALSE, reason = "TR2025 data not available"))
  }

  # Calculate totals by year
  calc_totals <- population[, .(calculated = sum(population)), by = year]

  # Get TR2025 totals
  tr_totals <- tr2025_pop[, .(tr2025 = sum(Total)), by = Year]
  data.table::setnames(tr_totals, "Year", "year")

  # Filter years if specified
  if (!is.null(years)) {
    calc_totals <- calc_totals[year %in% years]
    tr_totals <- tr_totals[year %in% years]
  }

  # Compare
  comparison <- merge(calc_totals, tr_totals, by = "year", all.x = TRUE)
  comparison[, diff_pct := (calculated - tr2025) / tr2025 * 100]

  valid_rows <- comparison[!is.na(tr2025)]
  mean_abs_error <- mean(abs(valid_rows$diff_pct))
  max_abs_error <- max(abs(valid_rows$diff_pct))
  valid <- max_abs_error < tolerance * 100

  if (valid) {
    cli::cli_alert_success("PASS: Population totals match TR2025 (mean error: {round(mean_abs_error, 2)}%)")
  } else {
    cli::cli_alert_warning("WARNING: Population totals differ from TR2025 by up to {round(max_abs_error, 2)}%")
  }

  list(
    validated = TRUE,
    valid = valid,
    mean_abs_error = mean_abs_error,
    max_abs_error = max_abs_error,
    comparison = comparison
  )
}

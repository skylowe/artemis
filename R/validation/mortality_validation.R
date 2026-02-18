#' Mortality Validation Against TR2025
#'
#' Functions for comparing calculated mortality outputs against
#' the official SSA 2025 Trustees Report values.
#'
#' @name mortality_validation
NULL

#' Validate qx against TR2025 historical death probabilities
#'
#' @description
#' Compares calculated death probabilities (qx) against official TR2025 values
#' and reports differences by age group.
#'
#' @param qx_calculated data.table with calculated qx (year, age, sex, qx)
#' @param years Integer vector of years to validate (default: all overlapping years)
#' @param tolerance Numeric: acceptable relative difference (default: 0.02 = 2%)
#' @param data_dir Character: directory containing TR2025 files
#'
#' @return List with:
#'   - comparison: data.table with calculated, tr2025, and diff values
#'   - summary: data.table with summary statistics by age group
#'   - pass: logical indicating if validation passed
#'
#' @export
validate_qx_against_tr <- function(qx_calculated,
                                        years = NULL,
                                        tolerance = 0.02,
                                        data_dir = NULL) {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
  checkmate::assert_data_table(qx_calculated)
  checkmate::assert_names(names(qx_calculated), must.include = c("year", "age", "sex", "qx"))

  # Load TR2025 historical qx
  source("R/data_acquisition/tr_data.R")
  qx_tr2025 <- load_tr_death_probs_hist(sex = "both", data_dir = data_dir)

  # Filter to requested years
  if (!is.null(years)) {
    qx_calculated <- qx_calculated[year %in% years]
    qx_tr2025 <- qx_tr2025[year %in% years]
  }

  # Find overlapping years
  overlap_years <- intersect(unique(qx_calculated$year), unique(qx_tr2025$year))
  if (length(overlap_years) == 0) {
    cli::cli_abort("No overlapping years between calculated qx and TR2025")
  }

  cli::cli_alert_info("Validating qx for years {min(overlap_years)}-{max(overlap_years)}")

  # Merge on year, age, sex
  qx_calculated <- qx_calculated[year %in% overlap_years]
  data.table::setnames(qx_calculated, "qx", "qx_calc")

  qx_tr2025 <- qx_tr2025[year %in% overlap_years]
  data.table::setnames(qx_tr2025, "qx", "qx_tr2025")

  comparison <- merge(
    qx_calculated[, .(year, age, sex, qx_calc)],
    qx_tr2025[, .(year, age, sex, qx_tr2025)],
    by = c("year", "age", "sex"),
    all = FALSE
  )

  if (nrow(comparison) == 0) {
    cli::cli_abort("No matching records found between calculated qx and TR2025")
  }

  # Calculate differences
  comparison[, `:=`(
    abs_diff = qx_calc - qx_tr2025,
    rel_diff = (qx_calc - qx_tr2025) / qx_tr2025
  )]

  # Define age groups for summary
  comparison[, age_group := data.table::fcase(
    age == 0, "0 (infant)",
    age == 1, "1",
    age <= 14, "2-14",
    age <= 49, "15-49",
    age <= 64, "50-64",
    age <= 84, "65-84",
    age <= 99, "85-99",
    default = "100+"
  )]

  # Summary statistics by age group and sex
  summary_stats <- comparison[, .(
    n_obs = .N,
    mean_rel_diff = mean(rel_diff, na.rm = TRUE),
    median_rel_diff = median(rel_diff, na.rm = TRUE),
    max_rel_diff = max(abs(rel_diff), na.rm = TRUE),
    rmse = sqrt(mean(abs_diff^2, na.rm = TRUE)),
    within_tolerance = mean(abs(rel_diff) <= tolerance, na.rm = TRUE)
  ), by = .(age_group, sex)]

  data.table::setorder(summary_stats, sex, age_group)

  # Overall pass/fail
  overall_within <- mean(abs(comparison$rel_diff) <= tolerance, na.rm = TRUE)
  pass <- overall_within >= 0.95  # 95% of observations within tolerance

  # Report results
  cli::cli_h2("qx Validation Results")
  cli::cli_alert_info("Compared {nrow(comparison)} qx values")
  cli::cli_alert_info("Tolerance: {tolerance * 100}% relative difference")

  if (pass) {
    cli::cli_alert_success("PASS: {round(overall_within * 100, 1)}% of values within tolerance")
  } else {
    cli::cli_alert_danger("FAIL: Only {round(overall_within * 100, 1)}% of values within tolerance")
  }

  # Show summary by age group
  cli::cli_h3("Summary by Age Group")
  print(summary_stats)

  list(
    comparison = comparison,
    summary = summary_stats,
    overall_within_tolerance = overall_within,
    pass = pass
  )
}

#' Validate life expectancy against TR2025
#'
#' @description
#' Compares calculated life expectancy (ex) against official TR2025 values.
#'
#' @param life_table data.table from calculate_life_table() with ex column
#' @param at_ages Integer vector of ages to validate (default: c(0, 65))
#' @param tolerance Numeric: acceptable absolute difference in years (default: 0.1)
#' @param data_dir Character: directory containing TR2025 files
#'
#' @return List with comparison, summary, and pass flag
#'
#' @export
validate_life_expectancy_against_tr <- function(life_table,
                                                     at_ages = c(0, 65),
                                                     tolerance = 0.1,
                                                     data_dir = NULL) {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
  checkmate::assert_data_table(life_table)
  checkmate::assert_names(names(life_table), must.include = c("age", "sex", "ex"))

  # Load TR2025 life tables
  source("R/data_acquisition/tr_data.R")
  lt_tr2025 <- load_tr_life_tables_hist(sex = "both", data_dir = data_dir)

  # Filter to requested ages
  life_table_sub <- life_table[age %in% at_ages]
  lt_tr2025_sub <- lt_tr2025[age %in% at_ages]

  has_year <- "year" %in% names(life_table_sub)

  if (has_year) {
    # Find overlapping years
    overlap_years <- intersect(unique(life_table_sub$year), unique(lt_tr2025_sub$year))
    if (length(overlap_years) == 0) {
      cli::cli_abort("No overlapping years between calculated life table and TR2025")
    }

    cli::cli_alert_info("Validating life expectancy for years {min(overlap_years)}-{max(overlap_years)}")

    life_table_sub <- life_table_sub[year %in% overlap_years]
    lt_tr2025_sub <- lt_tr2025_sub[year %in% overlap_years]

    data.table::setnames(life_table_sub, "ex", "ex_calc")
    data.table::setnames(lt_tr2025_sub, "ex", "ex_tr2025")

    comparison <- merge(
      life_table_sub[, .(year, age, sex, ex_calc)],
      lt_tr2025_sub[, .(year, age, sex, ex_tr2025)],
      by = c("year", "age", "sex"),
      all = FALSE
    )
  } else {
    # Single period comparison (e.g., projections)
    cli::cli_alert_info("Comparing life expectancy (no year dimension)")

    data.table::setnames(life_table_sub, "ex", "ex_calc")
    # Use latest TR2025 year as reference
    latest_year <- max(lt_tr2025_sub$year)
    lt_tr2025_latest <- lt_tr2025_sub[year == latest_year]
    data.table::setnames(lt_tr2025_latest, "ex", "ex_tr2025")

    comparison <- merge(
      life_table_sub[, .(age, sex, ex_calc)],
      lt_tr2025_latest[, .(age, sex, ex_tr2025)],
      by = c("age", "sex"),
      all = FALSE
    )
  }

  if (nrow(comparison) == 0) {
    cli::cli_abort("No matching records found between calculated life table and TR2025")
  }

  # Calculate differences
  comparison[, `:=`(
    diff = ex_calc - ex_tr2025,
    abs_diff = abs(ex_calc - ex_tr2025)
  )]

  # Summary statistics
  summary_stats <- comparison[, .(
    n_obs = .N,
    mean_diff = mean(diff, na.rm = TRUE),
    mean_abs_diff = mean(abs_diff, na.rm = TRUE),
    max_abs_diff = max(abs_diff, na.rm = TRUE),
    within_tolerance = mean(abs_diff <= tolerance, na.rm = TRUE)
  ), by = .(age, sex)]

  data.table::setorder(summary_stats, sex, age)

  # Overall pass/fail
  overall_within <- mean(comparison$abs_diff <= tolerance, na.rm = TRUE)
  pass <- overall_within >= 0.95

  # Report results
  cli::cli_h2("Life Expectancy Validation Results")
  cli::cli_alert_info("Compared {nrow(comparison)} life expectancy values at ages {paste(at_ages, collapse = ', ')}")
  cli::cli_alert_info("Tolerance: {tolerance} years absolute difference")

  if (pass) {
    cli::cli_alert_success("PASS: {round(overall_within * 100, 1)}% of values within tolerance")
  } else {
    cli::cli_alert_danger("FAIL: Only {round(overall_within * 100, 1)}% of values within tolerance")
  }

  # Show summary
  cli::cli_h3("Summary by Age and Sex")
  print(summary_stats)

  list(
    comparison = comparison,
    summary = summary_stats,
    overall_within_tolerance = overall_within,
    pass = pass
  )
}

#' Create validation report for mortality
#'
#' @description
#' Runs all mortality validations and produces a summary report.
#'
#' @param qx_calculated data.table with calculated qx
#' @param life_table data.table from calculate_life_table()
#' @param years Integer vector of years to validate
#' @param qx_tolerance Relative tolerance for qx (default: 0.02 = 2%)
#' @param ex_tolerance Absolute tolerance for ex in years (default: 0.1)
#'
#' @return List with all validation results
#'
#' @export
run_mortality_validation <- function(qx_calculated,
                                      life_table = NULL,
                                      years = NULL,
                                      qx_tolerance = 0.02,
                                      ex_tolerance = 0.1) {
  cli::cli_h1("Mortality Validation Against TR2025")

  results <- list()

  # Validate qx
  cli::cli_h2("1. Death Probability (qx) Validation")
  results$qx <- validate_qx_against_tr(
    qx_calculated = qx_calculated,
    years = years,
    tolerance = qx_tolerance
  )

  # Validate life expectancy if life table provided
  if (!is.null(life_table)) {
    cli::cli_h2("2. Life Expectancy (ex) Validation")
    results$ex <- validate_life_expectancy_against_tr(
      life_table = life_table,
      at_ages = c(0, 65),
      tolerance = ex_tolerance
    )
  }

  # Overall summary
  cli::cli_h1("Overall Validation Summary")

  if (results$qx$pass) {
    cli::cli_alert_success("qx validation: PASSED")
  } else {
    cli::cli_alert_danger("qx validation: FAILED")
  }

  if (!is.null(life_table)) {
    if (results$ex$pass) {
      cli::cli_alert_success("Life expectancy validation: PASSED")
    } else {
      cli::cli_alert_danger("Life expectancy validation: FAILED")
    }

    results$overall_pass <- results$qx$pass && results$ex$pass
  } else {
    results$overall_pass <- results$qx$pass
  }

  if (results$overall_pass) {
    cli::cli_alert_success("OVERALL: All validations PASSED")
  } else {
    cli::cli_alert_danger("OVERALL: Some validations FAILED")
  }

  invisible(results)
}

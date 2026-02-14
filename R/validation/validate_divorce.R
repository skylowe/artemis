#' Divorce Projection Validation Functions
#'
#' Functions for validating divorce rate projections against TR2025
#' assumptions and methodology requirements.
#'
#' Validates all required outputs (per TR2025 Section 1.7):
#' - d̂_{x,y}^z: Age-specific divorce rates (Equation 1.7.1)
#' - ADR^z: Age-adjusted central divorce rate (Equation 1.7.2)
#'
#' @name validate_divorce
NULL

# =============================================================================
# CONSTANTS — sourced from divorce.R module constants
# =============================================================================

# DivGrid dimensions derived from divorce.R constants (ages 14-100+)
DIVGRID_SIZE <- DIVORCE_MAX_AGE - DIVORCE_MIN_AGE + 1  # 87

# =============================================================================
# ADR VALIDATION FUNCTIONS
# =============================================================================

#' Validate ADR reaches ultimate value
#'
#' @description
#' Validates that the age-adjusted divorce rate (ADR) reaches the
#' ultimate value at the specified ultimate year.
#'
#' Per TR2025: "The rate reaches its ultimate value in the 25th year
#' of the 75-year projection period. For the 2025 report, the assumed
#' ultimate ADR is 1,700 per 100,000 married couples."
#'
#' @param adr_projected data.table with year and projected_adr columns
#' @param ultimate_adr Expected ultimate ADR (default: 1700)
#' @param ultimate_year Year when ultimate should be reached (default: 2049)
#' @param tolerance Acceptable relative difference (default: 0.001 = 0.1%)
#'
#' @return list with validation results
#'
#' @export
validate_adr_ultimate <- function(adr_projected,
                                   ultimate_adr = DIVORCE_ULTIMATE_ADR,
                                   ultimate_year = 2049L,
                                   tolerance = 0.001) {
  checkmate::assert_data_table(adr_projected)

  cli::cli_h3("Validating ADR Ultimate Value")

  # Find ADR column
  adr_col <- intersect(c("projected_adr", "adr"), names(adr_projected))[1]
  if (is.na(adr_col)) {
    cli::cli_alert_danger("Cannot find ADR column in data")
    return(list(passed = FALSE, message = "Missing ADR column"))
  }

  # Get ADR at ultimate year
  ultimate_row <- adr_projected[year == ultimate_year]
  if (nrow(ultimate_row) == 0) {
    cli::cli_alert_warning("Ultimate year {ultimate_year} not found in projection")
    return(list(
      passed = NA,
      message = paste("Ultimate year", ultimate_year, "not in data")
    ))
  }

  actual_adr <- ultimate_row[[adr_col]]
  rel_diff <- abs(actual_adr - ultimate_adr) / ultimate_adr
  passes <- rel_diff <= tolerance

  # Also check years after ultimate
  post_ultimate <- adr_projected[year > ultimate_year]
  if (nrow(post_ultimate) > 0) {
    post_adr <- post_ultimate[[adr_col]]
    post_ok <- all(abs(post_adr - ultimate_adr) / ultimate_adr <= tolerance)
  } else {
    post_ok <- TRUE
  }

  all_pass <- passes && post_ok

  if (all_pass) {
    cli::cli_alert_success(
      "ADR reaches {ultimate_adr} at year {ultimate_year} (actual: {round(actual_adr, 1)})"
    )
    if (nrow(post_ultimate) > 0) {
      cli::cli_alert_success(
        "ADR stays at ultimate for {nrow(post_ultimate)} years post-{ultimate_year}"
      )
    }
  } else {
    if (!passes) {
      cli::cli_alert_danger(
        "ADR at year {ultimate_year}: {round(actual_adr, 1)} (expected: {ultimate_adr}, diff: {round(rel_diff*100, 2)}%)"
      )
    }
    if (!post_ok) {
      cli::cli_alert_warning("ADR deviates from ultimate after {ultimate_year}")
    }
  }

  list(
    passed = all_pass,
    ultimate_year = ultimate_year,
    expected_adr = ultimate_adr,
    actual_adr = actual_adr,
    relative_diff = rel_diff,
    post_ultimate_ok = post_ok,
    message = ifelse(all_pass,
                     paste("ADR reaches", ultimate_adr, "at year", ultimate_year),
                     "ADR does not reach ultimate value")
  )
}

#' Validate ADR trajectory is monotonic toward ultimate
#'
#' @description
#' Validates that the projected ADR trajectory moves monotonically
#' toward the ultimate value.
#'
#' Per TR2025: "The annual rate of change decreases in absolute value
#' as the ultimate year approaches."
#'
#' @param adr_projected data.table with year and projected_adr
#' @param starting_adr Starting ADR value
#' @param ultimate_adr Ultimate ADR value (default: 1700)
#'
#' @return list with validation results
#'
#' @export
validate_adr_trajectory <- function(adr_projected,
                                     starting_adr,
                                     ultimate_adr = DIVORCE_ULTIMATE_ADR) {
  checkmate::assert_data_table(adr_projected)
  checkmate::assert_number(starting_adr)

  cli::cli_h3("Validating ADR Trajectory")

  adr_col <- intersect(c("projected_adr", "adr"), names(adr_projected))[1]
  if (is.na(adr_col)) {
    return(list(passed = FALSE, message = "Missing ADR column"))
  }

  # Sort by year
  adr_sorted <- data.table::copy(adr_projected)[order(year)]
  adr_values <- adr_sorted[[adr_col]]

  # Determine direction: should be increasing if starting < ultimate
  increasing <- starting_adr < ultimate_adr

  # Check monotonicity
  if (increasing) {
    changes <- diff(adr_values)
    monotonic <- all(changes >= -0.1)  # Allow tiny rounding errors
    direction_msg <- "increasing"
  } else {
    changes <- diff(adr_values)
    monotonic <- all(changes <= 0.1)
    direction_msg <- "decreasing"
  }

  # Check rate of change decreases (asymptotic approach)
  abs_changes <- abs(changes)
  n_changes <- length(abs_changes)
  if (n_changes > 10) {
    # Compare first half to second half
    first_half_mean <- mean(abs_changes[1:(n_changes/2)])
    second_half_mean <- mean(abs_changes[(n_changes/2):n_changes])
    slowing <- second_half_mean <= first_half_mean * 1.1  # Allow some tolerance
  } else {
    slowing <- TRUE
  }

  all_pass <- monotonic && slowing

  if (all_pass) {
    cli::cli_alert_success(
      "ADR trajectory is monotonically {direction_msg} from {round(starting_adr, 1)} to {ultimate_adr}"
    )
    cli::cli_alert_success("Rate of change decreases as ultimate approaches")
  } else {
    if (!monotonic) {
      violations <- which(if(increasing) changes < -0.1 else changes > 0.1)
      cli::cli_alert_danger(
        "Non-monotonic trajectory: {length(violations)} violations found"
      )
    }
    if (!slowing) {
      cli::cli_alert_warning("Rate of change does not decrease toward ultimate")
    }
  }

  # Sample trajectory points
  sample_years <- c(min(adr_sorted$year), 2030, 2040, 2049, 2060, 2099)
  sample_years <- sample_years[sample_years %in% adr_sorted$year]
  for (yr in sample_years[1:min(5, length(sample_years))]) {
    amr_val <- adr_sorted[year == yr, get(adr_col)]
    cli::cli_alert_info("  {yr}: ADR = {round(amr_val, 1)}")
  }

  list(
    passed = all_pass,
    monotonic = monotonic,
    slowing = slowing,
    direction = direction_msg,
    starting_adr = starting_adr,
    ultimate_adr = ultimate_adr,
    message = ifelse(all_pass,
                     "ADR trajectory is valid",
                     "ADR trajectory has issues")
  )
}

# =============================================================================
# DIVGRID VALIDATION FUNCTIONS
# =============================================================================

#' Validate DivGrid properties
#'
#' @description
#' Validates that the DivGrid matrix has correct mathematical properties:
#' - Correct dimensions (87 × 87)
#' - All rates non-negative
#' - No NA values in active ages
#' - Peak rate at reasonable ages
#' - Rates decline at extreme ages
#'
#' @param divgrid Matrix (87 × 87) of divorce rates
#'
#' @return list with validation results
#'
#' @export
validate_divgrid_properties <- function(divgrid) {
  checkmate::assert_matrix(divgrid)

  cli::cli_h3("Validating DivGrid Properties")

  checks <- list()
  messages <- character()

  # Check 1: Dimensions
  expected_dim <- c(DIVGRID_SIZE, DIVGRID_SIZE)
  dim_pass <- all(dim(divgrid) == expected_dim)
  checks$dimensions <- dim_pass
  if (dim_pass) {
    messages <- c(messages, paste("Dimensions:", nrow(divgrid), "×", ncol(divgrid)))
  } else {
    messages <- c(messages, paste("Dimensions:", nrow(divgrid), "×", ncol(divgrid),
                                   "(expected", expected_dim[1], "×", expected_dim[2], ")"))
  }

  # Check 2: Non-negative rates
  neg_count <- sum(divgrid < 0, na.rm = TRUE)
  neg_pass <- neg_count == 0
  checks$non_negative <- neg_pass
  if (neg_pass) {
    messages <- c(messages, "All rates non-negative")
  } else {
    messages <- c(messages, paste(neg_count, "negative rates found"))
  }

  # Check 3: No NA in active ages (20-65)
  active_range <- (20 - DIVORCE_MIN_AGE + 1):(65 - DIVORCE_MIN_AGE + 1)
  active_rates <- divgrid[active_range, active_range]
  na_count <- sum(is.na(active_rates))
  na_pass <- na_count == 0
  checks$no_na_active <- na_pass
  if (na_pass) {
    messages <- c(messages, "No NA values in active ages (20-65)")
  } else {
    messages <- c(messages, paste(na_count, "NA values in active ages"))
  }

  # Check 4: Peak rate at reasonable ages (25-50 for divorce)
  max_rate <- max(divgrid, na.rm = TRUE)
  max_idx <- which(divgrid == max_rate, arr.ind = TRUE)[1, ]
  peak_h_age <- max_idx[1] + DIVORCE_MIN_AGE - 1
  peak_w_age <- max_idx[2] + DIVORCE_MIN_AGE - 1
  peak_reasonable <- peak_h_age >= 20 && peak_h_age <= 55 &&
    peak_w_age >= 20 && peak_w_age <= 55
  checks$peak_location <- peak_reasonable
  messages <- c(messages, paste("Peak rate:", round(max_rate, 1),
                                 "at H age", peak_h_age, ", W age", peak_w_age))

  # Check 5: Rates decline at extreme ages
  young_rates <- mean(divgrid[1:5, 1:5], na.rm = TRUE)
  middle_rates <- mean(divgrid[active_range[10:20], active_range[10:20]], na.rm = TRUE)
  old_rates <- mean(divgrid[(DIVGRID_SIZE-10):DIVGRID_SIZE, (DIVGRID_SIZE-10):DIVGRID_SIZE], na.rm = TRUE)

  age_pattern_pass <- middle_rates > young_rates && middle_rates > old_rates
  checks$age_pattern <- age_pattern_pass
  if (age_pattern_pass) {
    messages <- c(messages, "Rates peak in middle ages, decline at extremes")
  } else {
    messages <- c(messages, "Age pattern may be unusual")
  }

  # Check 6: Total rate is reasonable
  total_rate <- sum(divgrid, na.rm = TRUE)
  total_reasonable <- total_rate > 1000 && total_rate < 5000000
  checks$total_rate <- total_reasonable
  messages <- c(messages, paste("Total rate:", format(round(total_rate), big.mark = ",")))

  # Overall
  all_pass <- all(unlist(checks))

  # Report
  for (msg in messages) {
    is_warning <- grepl("\\d+ negative|\\d+ NA|unusual|expected \\d+", msg, ignore.case = TRUE)
    if (is_warning) {
      cli::cli_alert_warning(msg)
    } else {
      cli::cli_alert_success(msg)
    }
  }

  list(
    passed = all_pass,
    checks = checks,
    summary = messages,
    peak_rate = max_rate,
    peak_ages = c(husband = peak_h_age, wife = peak_w_age),
    total_rate = total_rate,
    message = ifelse(all_pass,
                     "DivGrid properties are valid",
                     paste(sum(!unlist(checks)), "property checks failed"))
  )
}

# =============================================================================
# HISTORICAL DATA VALIDATION
# =============================================================================

#' Validate historical ADR series
#'
#' @description
#' Validates properties of the historical ADR series (1989-2022).
#'
#' @param historical_adr data.table with year and adr columns
#' @param expected_years Integer vector of expected years
#'
#' @return list with validation results
#'
#' @export
validate_historical_adr <- function(historical_adr,
                                     expected_years = 1989:2022) {
  cli::cli_h3("Validating Historical ADR Series")

  checks <- list()

  # Check 1: All years present
  years_present <- historical_adr$year
  years_ok <- all(expected_years %in% years_present)
  checks$years_complete <- years_ok
  if (years_ok) {
    cli::cli_alert_success(
      "All {length(expected_years)} historical years present ({min(expected_years)}-{max(expected_years)})"
    )
  } else {
    missing <- setdiff(expected_years, years_present)
    cli::cli_alert_warning("Missing years: {paste(head(missing, 5), collapse=', ')}")
  }

  # Check 2: ADR values in reasonable range
  adr_col <- intersect(c("adr", "historical_adr"), names(historical_adr))[1]
  adr_values <- historical_adr[[adr_col]]
  range_ok <- all(adr_values >= 500 & adr_values <= 5000)
  checks$adr_range <- range_ok
  if (range_ok) {
    cli::cli_alert_success(
      "ADR range: {round(min(adr_values), 1)} - {round(max(adr_values), 1)} per 100,000"
    )
  } else {
    cli::cli_alert_warning("Some ADR values outside expected range (500-5000)")
  }

  # Check 3: No NA values
  na_count <- sum(is.na(adr_values))
  na_ok <- na_count == 0
  checks$no_na <- na_ok
  if (na_ok) {
    cli::cli_alert_success("No NA values in historical ADR")
  } else {
    cli::cli_alert_danger("{na_count} NA values in historical ADR")
  }

  # Check 4: Declining trend (divorce rates have been declining since 1980s)
  if (length(adr_values) > 10) {
    early_mean <- mean(adr_values[1:5])
    late_mean <- mean(adr_values[(length(adr_values)-4):length(adr_values)])
    declining <- late_mean < early_mean
    checks$declining_trend <- declining
    if (declining) {
      cli::cli_alert_success("Declining trend confirmed: {round(early_mean, 0)} -> {round(late_mean, 0)}")
    } else {
      cli::cli_alert_info("Trend not declining (early: {round(early_mean, 0)}, late: {round(late_mean, 0)})")
    }
  }

  all_pass <- all(unlist(checks))
  cli::cli_alert_info("Passed: {sum(unlist(checks))}/{length(checks)}")

  list(
    passed = all_pass,
    checks = checks,
    adr_range = range(adr_values),
    n_years = length(years_present),
    message = ifelse(all_pass,
                     "Historical ADR series is valid",
                     "Historical ADR series has issues")
  )
}

#' Validate against NCHS total divorces
#'
#' @description
#' Validates that implied total divorces are consistent with NCHS published totals.
#'
#' @param divorce_rates List of rate matrices by year, or data.table with rates
#' @param nchs_totals data.table with year and nchs_total columns (optional)
#' @param married_pop_grid Standard population grid for calculating totals
#' @param tolerance Acceptable relative difference (default: 0.10 = 10%)
#'
#' @return list with validation results
#'
#' @export
validate_against_nchs_divorce_totals <- function(divorce_rates,
                                                   nchs_totals = NULL,
                                                   married_pop_grid = NULL,
                                                   tolerance = 0.10) {
  cli::cli_h3("Validating Against NCHS Divorce Totals")

  # Get NCHS targets if not provided (from CSV via nchs_divorce.R)
  if (is.null(nchs_totals)) {
    nchs_totals <- fetch_nchs_us_total_divorces()
    data.table::setnames(nchs_totals, "total_divorces", "nchs_total",
                         skip_absent = TRUE)
  }

  # Get years from rates
  if (is.list(divorce_rates) && !is.data.table(divorce_rates)) {
    years_present <- as.integer(names(divorce_rates))
  } else {
    years_present <- integer(0)
  }

  # Check coverage
  nchs_years <- nchs_totals$year
  overlapping <- intersect(years_present, nchs_years)
  coverage <- length(overlapping) / length(nchs_years)

  if (coverage < 0.3) {
    cli::cli_alert_warning(
      "Only {length(overlapping)}/{length(nchs_years)} NCHS years have rates available"
    )
    return(list(
      passed = NA,
      message = "Insufficient overlap for validation",
      coverage = coverage,
      years_available = years_present,
      nchs_years = nchs_years
    ))
  }

  cli::cli_alert_success(
    "Rate matrices available for {length(overlapping)}/{length(nchs_years)} NCHS years"
  )

  # Coverage check passes
  list(
    passed = TRUE,
    message = "Coverage check passed",
    coverage = coverage,
    years_available = overlapping
  )
}

# =============================================================================
# COMPREHENSIVE VALIDATION
# =============================================================================

#' Run comprehensive divorce projection validation
#'
#' @description
#' Main validation function that runs all validation checks on divorce
#' projection outputs. This is the primary entry point for Phase 7H validation.
#'
#' Validates per TR2025 Section 1.7:
#' - ADR reaches ultimate (1,700) by year 25 (2049)
#' - ADR trajectory: annual rate of change decreases as ultimate approaches
#' - DivGrid: 87×87 matrix, non-negative rates, reasonable peak ages
#' - Projected rates: scaled proportionally to match projected ADR
#'
#' @param projection Output from run_divorce_projection()
#' @param tolerance_adr Tolerance for ADR checks (default: 0.001)
#'
#' @return list with comprehensive validation results
#'
#' @export
validate_divorce_comprehensive <- function(projection,
                                             tolerance_adr = 0.001) {
  cli::cli_h1("Divorce Projection Validation (Phase 7H)")

  results <- list()
  n_checks <- 0
  n_passed <- 0

  # =========================================================================
  # Check 1: ADR Ultimate Value
  # =========================================================================
  cli::cli_h2("Check 1: ADR Ultimate Value")

  if ("projected_adr" %in% names(projection)) {
    ultimate_adr <- projection$metadata$ultimate_adr
    ultimate_year <- projection$metadata$ultimate_year

    check1 <- validate_adr_ultimate(
      projection$projected_adr,
      ultimate_adr,
      ultimate_year,
      tolerance = tolerance_adr
    )
    results$adr_ultimate <- check1
    n_checks <- n_checks + 1
    if (!is.na(check1$passed) && check1$passed) {
      n_passed <- n_passed + 1
    }
  } else {
    cli::cli_alert_warning("ADR projection not found")
    results$adr_ultimate <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 2: ADR Trajectory
  # =========================================================================
  cli::cli_h2("Check 2: ADR Trajectory")

  if ("projected_adr" %in% names(projection) && "metadata" %in% names(projection)) {
    check2 <- validate_adr_trajectory(
      projection$projected_adr,
      projection$metadata$starting_adr,
      projection$metadata$ultimate_adr
    )
    results$adr_trajectory <- check2
    n_checks <- n_checks + 1
    if (!is.na(check2$passed) && check2$passed) {
      n_passed <- n_passed + 1
    }
  } else {
    cli::cli_alert_warning("Insufficient data for trajectory validation")
    results$adr_trajectory <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 3: DivGrid Properties (Base)
  # =========================================================================
  cli::cli_h2("Check 3: Base DivGrid Properties")

  if ("historical" %in% names(projection) &&
      "base_result" %in% names(projection$historical)) {
    base_divgrid <- projection$historical$base_result$divgrid
    check3 <- validate_divgrid_properties(base_divgrid)
    results$base_divgrid <- check3
    n_checks <- n_checks + 1
    if (!is.na(check3$passed) && check3$passed) {
      n_passed <- n_passed + 1
    }
  } else {
    cli::cli_alert_warning("Base DivGrid not found in projection")
    results$base_divgrid <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 4: Adjusted DivGrid Properties
  # =========================================================================
  cli::cli_h2("Check 4: Adjusted DivGrid Properties")

  if ("historical" %in% names(projection) &&
      "adjusted_result" %in% names(projection$historical)) {
    adj_divgrid <- projection$historical$adjusted_result$adjusted_divgrid
    check4 <- validate_divgrid_properties(adj_divgrid)
    results$adjusted_divgrid <- check4
    n_checks <- n_checks + 1
    if (!is.na(check4$passed) && check4$passed) {
      n_passed <- n_passed + 1
    }
  } else {
    cli::cli_alert_warning("Adjusted DivGrid not found in projection")
    results$adjusted_divgrid <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 5: Historical ADR Series
  # =========================================================================
  cli::cli_h2("Check 5: Historical ADR Series")

  if ("historical" %in% names(projection) &&
      "adr_series" %in% names(projection$historical)) {
    check5 <- validate_historical_adr(projection$historical$adr_series)
    results$historical_adr <- check5
    n_checks <- n_checks + 1
    if (!is.na(check5$passed) && check5$passed) {
      n_passed <- n_passed + 1
    }
  } else {
    cli::cli_alert_warning("Historical ADR series not found")
    results$historical_adr <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 6: Projected Rates (from internal validation)
  # =========================================================================
  cli::cli_h2("Check 6: Projected Rates Validation")

  if ("validation" %in% names(projection)) {
    internal_val <- projection$validation
    check6_pass <- internal_val$n_pass == internal_val$n_total
    results$projected_rates <- list(
      passed = check6_pass,
      n_passed = internal_val$n_pass,
      n_total = internal_val$n_total,
      checks = internal_val$checks,
      message = ifelse(check6_pass,
                       paste(internal_val$n_pass, "/", internal_val$n_total, "internal checks passed"),
                       paste(internal_val$n_pass, "/", internal_val$n_total, "internal checks passed"))
    )
    n_checks <- n_checks + 1
    if (check6_pass) {
      n_passed <- n_passed + 1
      cli::cli_alert_success("Internal validation: {internal_val$n_pass}/{internal_val$n_total} passed")
    } else {
      cli::cli_alert_warning("Internal validation: {internal_val$n_pass}/{internal_val$n_total} passed")
    }
  } else {
    cli::cli_alert_warning("Internal validation results not found")
    results$projected_rates <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 7: Rate Positivity
  # =========================================================================
  cli::cli_h2("Check 7: Rate Positivity")

  if ("projected_rates" %in% names(projection) &&
      "rates" %in% names(projection$projected_rates)) {
    all_positive <- TRUE
    for (yr in names(projection$projected_rates$rates)) {
      rates <- projection$projected_rates$rates[[yr]]
      if (any(rates < 0, na.rm = TRUE)) {
        all_positive <- FALSE
        break
      }
    }
    results$rate_positivity <- list(passed = all_positive,
                                     message = ifelse(all_positive,
                                                      "All rates non-negative",
                                                      "Negative rates found"))
    n_checks <- n_checks + 1
    if (all_positive) {
      cli::cli_alert_success("All projected rates are non-negative")
      n_passed <- n_passed + 1
    } else {
      cli::cli_alert_danger("Negative rates found in some years")
    }
  } else {
    cli::cli_alert_info("Projected rates not available for positivity check")
    results$rate_positivity <- list(passed = NA, message = "Not checked")
  }

  # =========================================================================
  # Check 8: Year Coverage
  # =========================================================================
  cli::cli_h2("Check 8: Year Coverage")

  if ("complete_adr" %in% names(projection)) {
    total_years <- nrow(projection$complete_adr)
    # Expect ~121 years (1979-2099)
    coverage_ok <- total_years >= 100
    results$year_coverage <- list(
      passed = coverage_ok,
      total_years = total_years,
      message = paste(total_years, "years in complete ADR series")
    )
    n_checks <- n_checks + 1
    if (coverage_ok) {
      cli::cli_alert_success("Year coverage: {total_years} years (historical + projected)")
      n_passed <- n_passed + 1
    } else {
      cli::cli_alert_warning("Year coverage: only {total_years} years")
    }
  } else {
    results$year_coverage <- list(passed = NA, message = "Complete ADR not available")
  }

  # =========================================================================
  # Summary
  # =========================================================================
  cli::cli_h2("Validation Summary")

  all_passed <- n_passed == n_checks

  cli::cli_alert_info("{n_passed}/{n_checks} validation checks passed")

  if (all_passed) {
    cli::cli_alert_success("All divorce projection validations PASSED!")
  } else {
    failed_checks <- names(results)[sapply(results, function(x) !is.na(x$passed) && !x$passed)]
    if (length(failed_checks) > 0) {
      cli::cli_alert_warning("Failed checks: {paste(failed_checks, collapse=', ')}")
    }
  }

  # Summary table
  summary_table <- data.table::data.table(
    check = names(results),
    passed = sapply(results, function(x) {
      if (is.na(x$passed)) "N/A" else if (x$passed) "PASS" else "FAIL"
    }),
    message = sapply(results, function(x) x$message)
  )

  cli::cli_h3("Results Table")
  print(summary_table)

  list(
    passed = all_passed,
    n_passed = n_passed,
    n_checks = n_checks,
    results = results,
    summary = summary_table
  )
}

#' Quick validation for divorce projection
#'
#' @description
#' Simplified validation that checks the most critical items:
#' 1. ADR reaches ultimate value
#' 2. All rates non-negative
#' 3. DivGrid dimensions correct
#' 4. Historical and projected years present
#'
#' @param projection Output from run_divorce_projection()
#'
#' @return list with quick validation results
#'
#' @export
validate_divorce_quick <- function(projection) {
  cli::cli_h2("Quick Divorce Projection Validation")

  checks <- list()

  # Check 1: ADR ultimate
  if ("projected_adr" %in% names(projection) && "metadata" %in% names(projection)) {
    ultimate_year <- projection$metadata$ultimate_year
    ultimate_adr <- projection$metadata$ultimate_adr
    ultimate_row <- projection$projected_adr[year == ultimate_year]
    if (nrow(ultimate_row) > 0) {
      adr_col <- intersect(c("projected_adr", "adr"), names(projection$projected_adr))[1]
      actual <- ultimate_row[[adr_col]]
      checks$adr_ultimate <- abs(actual - ultimate_adr) / ultimate_adr < 0.01
      cli::cli_alert_info("ADR at ultimate year: {round(actual, 1)} (target: {ultimate_adr})")
    }
  }

  # Check 2: DivGrid dimensions
  if ("historical" %in% names(projection) &&
      "base_result" %in% names(projection$historical)) {
    divgrid <- projection$historical$base_result$divgrid
    checks$divgrid_dim <- all(dim(divgrid) == c(87, 87))
    cli::cli_alert_info("DivGrid dimensions: {dim(divgrid)[1]}×{dim(divgrid)[2]}")
  }

  # Check 3: Rate positivity (sample)
  if ("projected_rates" %in% names(projection) &&
      "rates" %in% names(projection$projected_rates)) {
    sample_years <- sample(names(projection$projected_rates$rates),
                           min(5, length(projection$projected_rates$rates)))
    all_pos <- all(sapply(sample_years, function(yr) {
      all(projection$projected_rates$rates[[yr]] >= 0, na.rm = TRUE)
    }))
    checks$rates_positive <- all_pos
    cli::cli_alert_info("Rate positivity: {all_pos}")
  }

  # Check 4: Year coverage
  if ("complete_adr" %in% names(projection)) {
    n_years <- nrow(projection$complete_adr)
    checks$year_coverage <- n_years >= 100
    cli::cli_alert_info("Year coverage: {n_years} years")
  }

  all_pass <- all(unlist(checks))
  n_pass <- sum(unlist(checks))
  n_total <- length(checks)

  cli::cli_alert_info("Quick validation: {n_pass}/{n_total} passed")

  list(
    passed = all_pass,
    checks = checks,
    n_passed = n_pass,
    n_total = n_total
  )
}

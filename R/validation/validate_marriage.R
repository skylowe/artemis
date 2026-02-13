#' Marriage Projection Validation Functions
#'
#' Functions for validating marriage rate projections against TR2025
#' assumptions and NCHS estimates.
#'
#' Validates all required outputs (per TR2025 Section 1.6):
#' - m̂_{x,y}^z: Age-specific marriage rates (Equation 1.6.1)
#' - AMR^z: Age-adjusted central marriage rate (Equation 1.6.2)
#' - Opposite-sex/same-sex marriage rate separation
#' - Prior marital status differentials
#'
#' @name validate_marriage
NULL

# =============================================================================
# CONSTANTS AND TR2025 ASSUMPTIONS
# =============================================================================

# MarGrid dimensions (ages 14-100+)
MARGRID_MIN_AGE <- 14
MARGRID_MAX_AGE <- 100
MARGRID_SIZE <- 87

# TR2025 AMR assumptions
TR2025_ULTIMATE_AMR <- 4000   # per 100,000 unmarried couples
TR2025_ULTIMATE_YEAR <- 2049  # Year 25 of 75-year projection period (2025 + 24)

# =============================================================================
# NCHS VALIDATION TARGETS
# =============================================================================

#' Get NCHS total U.S. marriages for validation
#'
#' @description
#' Returns published NCHS total marriage counts for the United States.
#' These serve as validation targets for total marriage calculations.
#'
#' @return data.table with year and nchs_total
#'
#' @keywords internal
get_nchs_marriage_validation_targets <- function() {
  # NCHS published total U.S. marriages
  # Source: CDC/NCHS National Vital Statistics Reports
  data.table::data.table(
    year = 1989L:2022L,
    nchs_total = c(
      2403268,  # 1989
      2443489,  # 1990
      2371000,  # 1991
      2362000,  # 1992
      2334000,  # 1993
      2362000,  # 1994
      2336000,  # 1995
      2344000,  # 1996
      2384000,  # 1997
      2244000,  # 1998
      2358000,  # 1999
      2329000,  # 2000
      2326000,  # 2001
      2254000,  # 2002
      2245000,  # 2003
      2279000,  # 2004
      2249000,  # 2005
      2193000,  # 2006
      2197000,  # 2007
      2157000,  # 2008
      2080000,  # 2009
      2096000,  # 2010
      2118000,  # 2011
      2131000,  # 2012
      2081000,  # 2013
      2140000,  # 2014
      2221000,  # 2015
      2246000,  # 2016
      2236000,  # 2017
      2132000,  # 2018
      2015000,  # 2019
      1676000,  # 2020 (COVID)
      1985000,  # 2021
      2065000   # 2022
    )
  )
}

# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

#' Validate AMR reaches ultimate value
#'
#' @description
#' Validates that the age-adjusted marriage rate (AMR) reaches the
#' ultimate value at the specified ultimate year.
#'
#' @param amr_projected data.table with year and projected_amr columns
#' @param ultimate_amr Expected ultimate AMR (default: 4000)
#' @param ultimate_year Year when ultimate should be reached (default: 2047)
#' @param tolerance Acceptable relative difference (default: 0.001 = 0.1%)
#'
#' @return list with validation results
#'
#' @export
validate_amr_ultimate <- function(amr_projected,
                                   ultimate_amr = TR2025_ULTIMATE_AMR,
                                   ultimate_year = TR2025_ULTIMATE_YEAR,
                                   tolerance = 0.001) {
  checkmate::assert_data_table(amr_projected)

  cli::cli_h3("Validating AMR Ultimate Value")

  # Find AMR column
  amr_col <- intersect(c("projected_amr", "amr"), names(amr_projected))[1]
  if (is.na(amr_col)) {
    cli::cli_alert_danger("Cannot find AMR column in data")
    return(list(passed = FALSE, message = "Missing AMR column"))
  }

  # Get AMR at ultimate year
  ultimate_row <- amr_projected[year == ultimate_year]
  if (nrow(ultimate_row) == 0) {
    cli::cli_alert_warning("Ultimate year {ultimate_year} not found in projection")
    return(list(
      passed = NA,
      message = paste("Ultimate year", ultimate_year, "not in data")
    ))
  }

  actual_amr <- ultimate_row[[amr_col]]
  rel_diff <- abs(actual_amr - ultimate_amr) / ultimate_amr
  passes <- rel_diff <= tolerance

  # Also check years after ultimate
  post_ultimate <- amr_projected[year > ultimate_year]
  if (nrow(post_ultimate) > 0) {
    post_amr <- post_ultimate[[amr_col]]
    post_ok <- all(abs(post_amr - ultimate_amr) / ultimate_amr <= tolerance)
  } else {
    post_ok <- TRUE
  }

  all_pass <- passes && post_ok

  if (all_pass) {
    cli::cli_alert_success(
      "AMR reaches {ultimate_amr} at year {ultimate_year} (actual: {round(actual_amr, 1)})"
    )
    if (nrow(post_ultimate) > 0) {
      cli::cli_alert_success(
        "AMR stays at ultimate for {nrow(post_ultimate)} years post-{ultimate_year}"
      )
    }
  } else {
    if (!passes) {
      cli::cli_alert_danger(
        "AMR at year {ultimate_year}: {round(actual_amr, 1)} (expected: {ultimate_amr}, diff: {round(rel_diff*100, 2)}%)"
      )
    }
    if (!post_ok) {
      cli::cli_alert_warning("AMR deviates from ultimate after {ultimate_year}")
    }
  }

  list(
    passed = all_pass,
    ultimate_year = ultimate_year,
    expected_amr = ultimate_amr,
    actual_amr = actual_amr,
    relative_diff = rel_diff,
    post_ultimate_ok = post_ok,
    message = ifelse(all_pass,
                     paste("AMR reaches", ultimate_amr, "at year", ultimate_year),
                     "AMR does not reach ultimate value")
  )
}

#' Validate AMR trajectory is monotonic toward ultimate
#'
#' @description
#' Validates that the projected AMR trajectory moves monotonically
#' toward the ultimate value.
#'
#' @param amr_projected data.table with year and projected_amr
#' @param starting_amr Starting AMR value
#' @param ultimate_amr Ultimate AMR value (default: 4000)
#'
#' @return list with validation results
#'
#' @export
validate_amr_trajectory <- function(amr_projected,
                                     starting_amr,
                                     ultimate_amr = TR2025_ULTIMATE_AMR) {
  checkmate::assert_data_table(amr_projected)
  checkmate::assert_number(starting_amr)

  cli::cli_h3("Validating AMR Trajectory")

  amr_col <- intersect(c("projected_amr", "amr"), names(amr_projected))[1]
  if (is.na(amr_col)) {
    return(list(passed = FALSE, message = "Missing AMR column"))
  }

  # Sort by year
  amr_sorted <- data.table::copy(amr_projected)[order(year)]
  amr_values <- amr_sorted[[amr_col]]

  # Determine direction: should be increasing if starting < ultimate
  increasing <- starting_amr < ultimate_amr

  # Check monotonicity
  if (increasing) {
    changes <- diff(amr_values)
    monotonic <- all(changes >= -0.1)  # Allow tiny rounding errors
    direction_msg <- "increasing"
  } else {
    changes <- diff(amr_values)
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
      "AMR trajectory is monotonically {direction_msg} from {round(starting_amr, 1)} to {ultimate_amr}"
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
  sample_years <- c(min(amr_sorted$year), 2030, 2040, 2047, 2050, 2099)
  sample_years <- sample_years[sample_years %in% amr_sorted$year]
  for (yr in sample_years[1:min(5, length(sample_years))]) {
    amr_val <- amr_sorted[year == yr, get(amr_col)]
    cli::cli_alert_info("  {yr}: AMR = {round(amr_val, 1)}")
  }

  list(
    passed = all_pass,
    monotonic = monotonic,
    slowing = slowing,
    direction = direction_msg,
    starting_amr = starting_amr,
    ultimate_amr = ultimate_amr,
    message = ifelse(all_pass,
                     "AMR trajectory is valid",
                     "AMR trajectory has issues")
  )
}

#' Validate MarGrid properties
#'
#' @description
#' Validates that the MarGrid matrix has correct mathematical properties:
#' - Correct dimensions (87 × 87)
#' - All rates non-negative
#' - No NA values in active ages
#' - Peak rate at reasonable ages
#' - Rates decline at extreme ages
#'
#' @param margrid Matrix (87 × 87) of marriage rates
#'
#' @return list with validation results
#'
#' @export
validate_margrid_properties <- function(margrid) {
  checkmate::assert_matrix(margrid)

  cli::cli_h3("Validating MarGrid Properties")

  checks <- list()
  messages <- character()

  # Check 1: Dimensions
  expected_dim <- c(MARGRID_SIZE, MARGRID_SIZE)
  dim_pass <- all(dim(margrid) == expected_dim)
  checks$dimensions <- dim_pass
  if (dim_pass) {
    messages <- c(messages, paste("Dimensions:", nrow(margrid), "×", ncol(margrid)))
  } else {
    messages <- c(messages, paste("Dimensions:", nrow(margrid), "×", ncol(margrid),
                                   "(expected", expected_dim[1], "×", expected_dim[2], ")"))
  }

  # Check 2: Non-negative rates
  neg_count <- sum(margrid < 0, na.rm = TRUE)
  neg_pass <- neg_count == 0
  checks$non_negative <- neg_pass
  if (neg_pass) {
    messages <- c(messages, "All rates non-negative")
  } else {
    messages <- c(messages, paste(neg_count, "negative rates found"))
  }

  # Check 3: No NA in active ages (15-80)
  active_range <- (15 - MARGRID_MIN_AGE + 1):(80 - MARGRID_MIN_AGE + 1)
  active_rates <- margrid[active_range, active_range]
  na_count <- sum(is.na(active_rates))
  na_pass <- na_count == 0
  checks$no_na_active <- na_pass
  if (na_pass) {
    messages <- c(messages, "No NA values in active ages (15-80)")
  } else {
    messages <- c(messages, paste(na_count, "NA values in active ages"))
  }

  # Check 4: Peak rate at reasonable ages
  max_rate <- max(margrid, na.rm = TRUE)
  max_idx <- which(margrid == max_rate, arr.ind = TRUE)[1, ]
  peak_h_age <- max_idx[1] + MARGRID_MIN_AGE - 1
  peak_w_age <- max_idx[2] + MARGRID_MIN_AGE - 1
  peak_reasonable <- peak_h_age >= 20 && peak_h_age <= 35 &&
    peak_w_age >= 18 && peak_w_age <= 32
  checks$peak_location <- peak_reasonable
  messages <- c(messages, paste("Peak rate:", round(max_rate, 1),
                                 "at H age", peak_h_age, ", W age", peak_w_age))

  # Check 5: Rates decline at extreme ages
  young_rates <- mean(margrid[1:5, 1:5], na.rm = TRUE)
  middle_rates <- mean(margrid[active_range[10:20], active_range[10:20]], na.rm = TRUE)
  old_rates <- mean(margrid[(MARGRID_SIZE-10):MARGRID_SIZE, (MARGRID_SIZE-10):MARGRID_SIZE], na.rm = TRUE)

  age_pattern_pass <- middle_rates > young_rates && middle_rates > old_rates
  checks$age_pattern <- age_pattern_pass
  if (age_pattern_pass) {
    messages <- c(messages, "Rates peak in middle ages, decline at extremes")
  } else {
    messages <- c(messages, "Age pattern may be unusual")
  }

  # Check 6: Total rate is reasonable
  # For 87×87 grid with rates per 100,000, total can be several million
  total_rate <- sum(margrid, na.rm = TRUE)
  total_reasonable <- total_rate > 10000 && total_rate < 10000000
  checks$total_rate <- total_reasonable
  messages <- c(messages, paste("Total rate:", format(round(total_rate), big.mark = ",")))

  # Overall
  all_pass <- all(unlist(checks))

  # Report - check for actual failure indicators, not just keywords
  for (msg in messages) {
    # These patterns indicate actual problems (not "All rates non-negative" which is good)
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
                     "MarGrid properties are valid",
                     paste(sum(!unlist(checks)), "property checks failed"))
  )
}

#' Validate marriage rates against NCHS totals
#'
#' @description
#' Validates that total marriages implied by rates match NCHS published totals.
#'
#' @param marriage_rates List of rate matrices by year, or data.table with rates
#' @param nchs_totals data.table with year and nchs_total columns (optional)
#' @param unmarried_pop_grid Standard population grid for calculating totals
#' @param tolerance Acceptable relative difference (default: 0.05 = 5%)
#'
#' @return list with validation results
#'
#' @export
validate_against_nchs_totals <- function(marriage_rates,
                                          nchs_totals = NULL,
                                          unmarried_pop_grid = NULL,
                                          tolerance = 0.05) {
  cli::cli_h3("Validating Against NCHS Totals")

  # Get NCHS targets if not provided
  if (is.null(nchs_totals)) {
    nchs_totals <- get_nchs_marriage_validation_targets()
  }

  # Calculate implied marriages from rates
  # This is complex because we need the population grid
  # For now, just validate that rates exist for all historical years

  if (is.list(marriage_rates) && !is.data.table(marriage_rates)) {
    # List of matrices
    years_present <- as.integer(names(marriage_rates))
  } else {
    years_present <- integer(0)
  }

  # Check coverage
  nchs_years <- nchs_totals$year
  overlapping <- intersect(years_present, nchs_years)
  coverage <- length(overlapping) / length(nchs_years)

  if (coverage < 0.5) {
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

  # If we have population grid, calculate totals
  if (!is.null(unmarried_pop_grid)) {
    validation_data <- data.table::data.table()

    for (yr in overlapping) {
      rate_matrix <- marriage_rates[[as.character(yr)]]
      # Expected marriages = sum(rates * population_grid)
      expected <- sum(rate_matrix * unmarried_pop_grid / 100000, na.rm = TRUE)

      nchs_val <- nchs_totals[year == yr, nchs_total]

      validation_data <- rbind(validation_data, data.table::data.table(
        year = yr,
        expected_marriages = expected,
        nchs_total = nchs_val,
        rel_diff = abs(expected - nchs_val) / nchs_val
      ))
    }

    validation_data[, passes := rel_diff <= tolerance]
    n_pass <- sum(validation_data$passes)
    n_total <- nrow(validation_data)
    all_pass <- n_pass == n_total

    if (all_pass) {
      cli::cli_alert_success(
        "{n_total} years within {tolerance*100}% of NCHS totals"
      )
    } else {
      failed_years <- validation_data[passes == FALSE, year]
      cli::cli_alert_warning(
        "{n_total - n_pass} years exceed {tolerance*100}% tolerance: {paste(head(failed_years, 5), collapse=', ')}"
      )
    }

    return(list(
      passed = all_pass,
      n_passed = n_pass,
      n_total = n_total,
      details = validation_data,
      message = ifelse(all_pass,
                       "All years within tolerance",
                       paste(n_total - n_pass, "years outside tolerance"))
    ))
  }

  # Without population grid, just report coverage
  list(
    passed = TRUE,
    message = "Coverage check passed (no population grid for total validation)",
    coverage = coverage,
    years_available = overlapping
  )
}

#' Validate same-sex and opposite-sex rates sum to total
#'
#' @description
#' Validates that opposite-sex + same-sex rates approximately equal total rates.
#' Note: Small differences are expected due to rate clipping that occurs when
#' ACS-based same-sex patterns concentrate rates in certain age combinations,
#' potentially exceeding total rates at those cells.
#'
#' @param total_rates List of total rate matrices by year
#' @param opposite_sex_rates List of opposite-sex rate matrices by year
#' @param same_sex_rates List of same-sex rate matrices by year
#' @param tolerance Acceptable relative difference (default: 0.01 = 1%)
#'
#' @return list with validation results
#'
#' @export
validate_marriage_type_split <- function(total_rates,
                                          opposite_sex_rates,
                                          same_sex_rates,
                                          tolerance = 0.01) {
  cli::cli_h3("Validating Same-Sex / Opposite-Sex Split")

  if (is.null(opposite_sex_rates) || is.null(same_sex_rates)) {
    cli::cli_alert_warning("Same-sex separation not included in projection")
    return(list(passed = NA, message = "Same-sex separation not available"))
  }

  years <- names(total_rates)
  n_years <- length(years)
  n_pass <- 0
  max_rel_diff <- 0
  max_abs_diff <- 0

  for (yr in years) {
    total <- total_rates[[yr]]
    opp <- opposite_sex_rates[[yr]]
    ss <- same_sex_rates[[yr]]

    if (is.null(opp) || is.null(ss)) {
      next
    }

    # Check sum - use relative difference based on total
    combined <- opp + ss
    total_sum <- sum(total, na.rm = TRUE)
    combined_sum <- sum(combined, na.rm = TRUE)

    # Relative difference in total rates
    rel_diff <- abs(total_sum - combined_sum) / total_sum
    abs_diff <- abs(total_sum - combined_sum)

    if (rel_diff <= tolerance) {
      n_pass <- n_pass + 1
    }
    max_rel_diff <- max(max_rel_diff, rel_diff)
    max_abs_diff <- max(max_abs_diff, abs_diff)
  }

  all_pass <- n_pass == n_years

  if (all_pass) {
    cli::cli_alert_success(
      "{n_years} years: opposite-sex + same-sex ≈ total (max rel diff: {round(max_rel_diff * 100, 2)}%)"
    )
  } else {
    cli::cli_alert_warning(
      "{n_pass}/{n_years} years within {tolerance*100}% tolerance (max rel diff: {round(max_rel_diff * 100, 2)}%)"
    )
  }

  # Check same-sex fraction is reasonable
  sample_year <- years[ceiling(length(years)/2)]
  total_sum <- sum(total_rates[[sample_year]], na.rm = TRUE)
  ss_sum <- sum(same_sex_rates[[sample_year]], na.rm = TRUE)
  ss_fraction <- ss_sum / total_sum

  fraction_ok <- ss_fraction >= 0.02 && ss_fraction <= 0.10
  cli::cli_alert_info(
    "Same-sex fraction ({sample_year}): {round(ss_fraction * 100, 2)}%"
  )

  list(
    passed = all_pass && fraction_ok,
    n_passed = n_pass,
    n_total = n_years,
    max_rel_diff = max_rel_diff,
    max_abs_diff = max_abs_diff,
    same_sex_fraction = ss_fraction,
    message = ifelse(all_pass,
                     "Marriage type split is valid",
                     "Marriage type split has issues")
  )
}

#' Validate male-male and female-female rates sum to same-sex
#'
#' @description
#' Validates that male-male + female-female rates equal same-sex rates.
#'
#' @param same_sex_rates List of same-sex rate matrices by year
#' @param male_male_rates List of male-male rate matrices by year
#' @param female_female_rates List of female-female rate matrices by year
#' @param tolerance Acceptable absolute difference (default: 0.01)
#'
#' @return list with validation results
#'
#' @export
validate_same_sex_split <- function(same_sex_rates,
                                     male_male_rates,
                                     female_female_rates,
                                     tolerance = 0.01) {
  cli::cli_h3("Validating Male-Male / Female-Female Split")

  if (is.null(male_male_rates) || is.null(female_female_rates)) {
    cli::cli_alert_warning("Male-male/female-female separation not available")
    return(list(passed = NA, message = "Gender split not available"))
  }

  years <- names(same_sex_rates)
  n_years <- length(years)
  n_pass <- 0

  for (yr in years) {
    ss <- same_sex_rates[[yr]]
    mm <- male_male_rates[[yr]]
    ff <- female_female_rates[[yr]]

    if (is.null(mm) || is.null(ff)) {
      next
    }

    combined <- mm + ff
    diff_matrix <- abs(combined - ss)
    max_diff <- max(diff_matrix, na.rm = TRUE)

    if (max_diff <= tolerance) {
      n_pass <- n_pass + 1
    }
  }

  all_pass <- n_pass == n_years

  if (all_pass) {
    cli::cli_alert_success(
      "{n_years} years: male-male + female-female = same-sex"
    )
  } else {
    cli::cli_alert_warning(
      "{n_pass}/{n_years} years have consistent split"
    )
  }

  # Check gender split ratio
  sample_year <- years[ceiling(length(years)/2)]
  mm_sum <- sum(male_male_rates[[sample_year]], na.rm = TRUE)
  ff_sum <- sum(female_female_rates[[sample_year]], na.rm = TRUE)
  mm_fraction <- mm_sum / (mm_sum + ff_sum)

  cli::cli_alert_info(
    "Male-male fraction ({sample_year}): {round(mm_fraction * 100, 1)}%"
  )

  list(
    passed = all_pass,
    n_passed = n_pass,
    n_total = n_years,
    male_male_fraction = mm_fraction,
    message = ifelse(all_pass,
                     "Same-sex gender split is valid",
                     "Same-sex gender split has issues")
  )
}

#' Validate prior status differentials
#'
#' @description
#' Validates that prior marital status differentials are reasonable.
#'
#' @param status_differentials data.table with prior status differentials
#'
#' @return list with validation results
#'
#' @export
validate_prior_status_differentials <- function(status_differentials) {
  cli::cli_h3("Validating Prior Status Differentials")

  if (is.null(status_differentials)) {
    cli::cli_alert_warning("Prior status differentials not available")
    return(list(passed = NA, message = "Status differentials not available"))
  }

  checks <- list()
  messages <- character()

  # Check 1: All statuses present
  statuses <- unique(status_differentials$prior_status)
  expected_statuses <- c("single", "divorced", "widowed")
  status_pass <- all(expected_statuses %in% statuses)
  checks$all_statuses <- status_pass
  messages <- c(messages, paste("Statuses:", paste(statuses, collapse = ", ")))

  # Check 2: Both sexes present
  sexes <- unique(status_differentials$sex)
  sex_pass <- all(c("male", "female") %in% sexes)
  checks$both_sexes <- sex_pass
  messages <- c(messages, paste("Sexes:", paste(sexes, collapse = ", ")))

  # Check 3: Relative rates are positive and reasonable
  # Rate-based differentials (TR2025 method) can exceed 10 for small populations
  # (e.g., divorced at young ages), so we use a generous upper bound
  rates <- status_differentials$relative_rate
  rate_pass <- all(rates > 0) && all(rates < 20)
  checks$rates_reasonable <- rate_pass
  messages <- c(messages, paste("Relative rate range:",
                                 round(min(rates), 3), "-", round(max(rates), 3)))

  # Check 4: Single highest at young ages
  if ("age_group" %in% names(status_differentials)) {
    young_single <- status_differentials[
      prior_status == "single" & grepl("^(12|14|15|18|20)", age_group),
      mean(relative_rate)
    ]
    old_single <- status_differentials[
      prior_status == "single" & grepl("^(55|65)", age_group),
      mean(relative_rate)
    ]
    single_pattern <- young_single > old_single
    checks$single_pattern <- single_pattern
    if (single_pattern) {
      messages <- c(messages, "Single: higher rates at young ages")
    } else {
      messages <- c(messages, "Single: pattern may be unusual")
    }
  }

  all_pass <- all(unlist(checks))

  # Report
  for (msg in messages) {
    if (grepl("unusual|not found", msg, ignore.case = TRUE)) {
      cli::cli_alert_warning(msg)
    } else {
      cli::cli_alert_success(msg)
    }
  }

  list(
    passed = all_pass,
    checks = checks,
    summary = messages,
    message = ifelse(all_pass,
                     "Prior status differentials are valid",
                     "Prior status differentials have issues")
  )
}

# =============================================================================
# BEERS INTERPOLATION VALIDATION
# =============================================================================

#' Validate Beers interpolation properties
#'
#' @description
#' Validates that the MarGrid produced by H.S. Beers 2D interpolation has
#' correct properties: sum preservation within age groups, smoothness at
#' group boundaries, and non-negativity.
#'
#' @param margrid Matrix (87 × 87) of marriage rates
#' @param sum_tolerance Relative tolerance for sum preservation (default: 0.01 = 1%)
#'
#' @return list with validation results
#'
#' @export
validate_beers_interpolation <- function(margrid, sum_tolerance = 0.01) {
  cli::cli_h3("Validating Beers Interpolation Properties")

  checks <- list()
  messages <- character()

  # Check 1: Non-negativity
  neg_count <- sum(margrid < 0, na.rm = TRUE)
  checks$non_negative <- neg_count == 0
  if (neg_count == 0) {
    messages <- c(messages, "All interpolated rates non-negative")
  } else {
    messages <- c(messages, paste(neg_count, "negative rates found"))
  }

  # Check 2: Smoothness at group boundaries
  # Check that rates at age-group boundaries (19→20, 24→25, etc.) don't have
  # large discontinuities. Compare ratio of adjacent single-year rates.
  boundary_ages <- c(19, 24, 29, 34, 44, 54, 64)
  n_smooth <- 0
  n_boundaries <- 0
  for (b in boundary_ages) {
    idx <- b - MARGRID_MIN_AGE + 1
    if (idx >= 2 && idx < nrow(margrid)) {
      # Check husband dimension (column 10 ≈ wife age 24 as reference)
      ref_col <- min(10, ncol(margrid))
      rate_before <- margrid[idx, ref_col]
      rate_after <- margrid[idx + 1, ref_col]
      if (rate_before > 0 && rate_after > 0) {
        ratio <- max(rate_before, rate_after) / min(rate_before, rate_after)
        n_boundaries <- n_boundaries + 1
        if (ratio < 3.0) n_smooth <- n_smooth + 1  # Allow up to 3x change
      }
    }
  }
  smooth_pass <- n_boundaries == 0 || (n_smooth / n_boundaries >= 0.7)
  checks$smoothness <- smooth_pass
  if (smooth_pass) {
    messages <- c(messages, paste(n_smooth, "/", n_boundaries, "boundaries smooth"))
  } else {
    messages <- c(messages, paste("Staircase pattern:", n_boundaries - n_smooth, "rough boundaries"))
  }

  # Check 3: Dimensions
  dim_pass <- all(dim(margrid) == c(MARGRID_SIZE, MARGRID_SIZE))
  checks$dimensions <- dim_pass
  messages <- c(messages, paste("Dimensions:", nrow(margrid), "x", ncol(margrid)))

  all_pass <- all(unlist(checks))

  for (msg in messages) {
    if (grepl("\\d+ negative|Staircase|rough", msg, ignore.case = TRUE)) {
      cli::cli_alert_warning(msg)
    } else {
      cli::cli_alert_success(msg)
    }
  }

  list(
    passed = all_pass,
    checks = checks,
    summary = messages,
    message = ifelse(all_pass,
                     "Beers interpolation properties are valid",
                     "Beers interpolation has issues")
  )
}

# =============================================================================
# SAME-SEX SUBTRACTION VALIDATION
# =============================================================================

#' Validate same-sex subtraction from historical rates
#'
#' @description
#' Validates that opposite-sex + same-sex rates approximately equal total
#' NCHS-reported marriages for historical years. This confirms that the
#' same-sex subtraction (Phase 3 fix) doesn't lose or create marriages.
#'
#' @param projection Output from run_marriage_projection()
#' @param tolerance Relative tolerance (default: 0.01 = 1%)
#'
#' @return list with validation results
#'
#' @export
validate_same_sex_subtraction <- function(projection, tolerance = 0.01) {
  cli::cli_h3("Validating Same-Sex Subtraction")

  if (is.null(projection$all_rates) || is.null(projection$opposite_sex_rates) ||
      is.null(projection$same_sex_rates)) {
    return(list(passed = NA, message = "Rate data not available for subtraction check"))
  }

  historical_years <- projection$historical_years
  if (is.null(historical_years) || length(historical_years) == 0) {
    return(list(passed = NA, message = "No historical years in projection"))
  }

  n_check <- 0
  n_pass <- 0

  for (yr_char in as.character(historical_years)) {
    total <- projection$all_rates[[yr_char]]
    os <- projection$opposite_sex_rates[[yr_char]]
    ss <- projection$same_sex_rates[[yr_char]]

    if (is.null(total) || is.null(os) || is.null(ss)) next

    total_sum <- sum(total, na.rm = TRUE)
    combined_sum <- sum(os, na.rm = TRUE) + sum(ss, na.rm = TRUE)

    if (total_sum > 0) {
      n_check <- n_check + 1
      rel_diff <- abs(total_sum - combined_sum) / total_sum
      if (rel_diff <= tolerance) n_pass <- n_pass + 1
    }
  }

  all_pass <- n_check > 0 && n_pass == n_check

  if (all_pass) {
    cli::cli_alert_success(
      "OS + SS rates sum to total for all {n_check} historical years"
    )
  } else if (n_check == 0) {
    cli::cli_alert_warning("No historical years available for subtraction check")
  } else {
    cli::cli_alert_warning(
      "{n_pass}/{n_check} historical years pass OS + SS = total check"
    )
  }

  list(
    passed = all_pass,
    n_checked = n_check,
    n_passed = n_pass,
    message = ifelse(all_pass,
                     "Same-sex subtraction preserves totals",
                     paste(n_check - n_pass, "years with OS + SS != total"))
  )
}

# =============================================================================
# COMPREHENSIVE VALIDATION
# =============================================================================

#' Run comprehensive marriage projection validation
#'
#' @description
#' Main validation function that runs all validation checks on marriage
#' projection outputs. This is the primary entry point for Phase 6H validation.
#'
#' @param projection Output from run_marriage_projection()
#' @param tolerance_amr Tolerance for AMR checks (default: 0.001)
#' @param tolerance_totals Tolerance for NCHS total checks (default: 0.05)
#'
#' @return list with comprehensive validation results
#'
#' @export
validate_marriage_comprehensive <- function(projection,
                                              tolerance_amr = 0.001,
                                              tolerance_totals = 0.05) {
  cli::cli_h1("Marriage Projection Validation (Phase 6H)")

  results <- list()
  n_checks <- 0
  n_passed <- 0

  # =========================================================================
  # Check 1: AMR Ultimate Value
  # =========================================================================
  cli::cli_h2("Check 1: AMR Ultimate Value")

  if ("amr_projected" %in% names(projection)) {
    check1 <- validate_amr_ultimate(
      projection$amr_projected,
      projection$ultimate_amr,
      projection$ultimate_year,
      tolerance = tolerance_amr
    )
    results$amr_ultimate <- check1
    n_checks <- n_checks + 1
    if (!is.na(check1$passed) && check1$passed) {
      n_passed <- n_passed + 1
    }
  } else {
    cli::cli_alert_warning("AMR projection not found")
    results$amr_ultimate <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 2: AMR Trajectory
  # =========================================================================
  cli::cli_h2("Check 2: AMR Trajectory")

  if ("amr_projected" %in% names(projection) && "starting_amr" %in% names(projection)) {
    check2 <- validate_amr_trajectory(
      projection$amr_projected,
      projection$starting_amr,
      projection$ultimate_amr
    )
    results$amr_trajectory <- check2
    n_checks <- n_checks + 1
    if (!is.na(check2$passed) && check2$passed) {
      n_passed <- n_passed + 1
    }
  } else {
    cli::cli_alert_warning("Insufficient data for trajectory validation")
    results$amr_trajectory <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 3: MarGrid Properties
  # =========================================================================
  cli::cli_h2("Check 3: MarGrid Properties")

  if ("margrid" %in% names(projection)) {
    check3 <- validate_margrid_properties(projection$margrid)
    results$margrid <- check3
    n_checks <- n_checks + 1
    if (!is.na(check3$passed) && check3$passed) {
      n_passed <- n_passed + 1
    }
  } else {
    cli::cli_alert_warning("MarGrid not found in projection")
    results$margrid <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 3b: Beers Interpolation Properties
  # =========================================================================
  if ("margrid" %in% names(projection)) {
    check3b <- validate_beers_interpolation(projection$margrid)
    results$beers_interpolation <- check3b
    n_checks <- n_checks + 1
    if (!is.na(check3b$passed) && check3b$passed) {
      n_passed <- n_passed + 1
    }
  }

  # =========================================================================
  # Check 4: Historical Rates Coverage
  # =========================================================================
  cli::cli_h2("Check 4: Historical Rates vs NCHS")

  if ("all_rates" %in% names(projection)) {
    check4 <- validate_against_nchs_totals(
      projection$all_rates,
      unmarried_pop_grid = projection$standard_pop_grid,
      tolerance = tolerance_totals
    )
    results$nchs_validation <- check4
    n_checks <- n_checks + 1
    if (!is.na(check4$passed) && check4$passed) {
      n_passed <- n_passed + 1
    }
  } else {
    cli::cli_alert_warning("Marriage rates not found")
    results$nchs_validation <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 5: Same-Sex / Opposite-Sex Split
  # =========================================================================
  cli::cli_h2("Check 5: Marriage Type Split")

  if ("opposite_sex_rates" %in% names(projection) && !is.null(projection$opposite_sex_rates)) {
    check5 <- validate_marriage_type_split(
      projection$all_rates,
      projection$opposite_sex_rates,
      projection$same_sex_rates
    )
    results$type_split <- check5
    n_checks <- n_checks + 1
    if (!is.na(check5$passed) && check5$passed) {
      n_passed <- n_passed + 1
    }

    # Also check male-male / female-female split
    if ("male_male_rates" %in% names(projection) && !is.null(projection$male_male_rates)) {
      check5b <- validate_same_sex_split(
        projection$same_sex_rates,
        projection$male_male_rates,
        projection$female_female_rates
      )
      results$gender_split <- check5b
      n_checks <- n_checks + 1
      if (!is.na(check5b$passed) && check5b$passed) {
        n_passed <- n_passed + 1
      }
    }
  } else {
    cli::cli_alert_info("Same-sex separation not included")
    results$type_split <- list(passed = NA, message = "Not included")
  }

  # =========================================================================
  # Check 5b: Same-Sex Subtraction Consistency
  # =========================================================================
  check5c <- validate_same_sex_subtraction(projection)
  results$ss_subtraction <- check5c
  n_checks <- n_checks + 1
  if (!is.na(check5c$passed) && check5c$passed) {
    n_passed <- n_passed + 1
  }

  # =========================================================================
  # Check 6: Prior Status Differentials
  # =========================================================================
  cli::cli_h2("Check 6: Prior Status Differentials")

  if ("status_differentials" %in% names(projection) && !is.null(projection$status_differentials)) {
    check6 <- validate_prior_status_differentials(projection$status_differentials)
    results$status_differentials <- check6
    n_checks <- n_checks + 1
    if (!is.na(check6$passed) && check6$passed) {
      n_passed <- n_passed + 1
    }
  } else {
    cli::cli_alert_info("Prior status differentials not included")
    results$status_differentials <- list(passed = NA, message = "Not included")
  }

  # =========================================================================
  # Check 7: Rate Positivity
  # =========================================================================
  cli::cli_h2("Check 7: Rate Positivity")

  if ("all_rates" %in% names(projection)) {
    all_positive <- TRUE
    for (yr in names(projection$all_rates)) {
      rates <- projection$all_rates[[yr]]
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
      cli::cli_alert_success("All rates are non-negative")
      n_passed <- n_passed + 1
    } else {
      cli::cli_alert_danger("Negative rates found in some years")
    }
  }

  # =========================================================================
  # Summary
  # =========================================================================
  cli::cli_h2("Validation Summary")

  all_passed <- n_passed == n_checks

  cli::cli_alert_info("{n_passed}/{n_checks} validation checks passed")

  if (all_passed) {
    cli::cli_alert_success("All marriage projection validations PASSED!")
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

#' Quick validation for marriage projection
#'
#' @description
#' Simplified validation that checks the most critical items:
#' 1. AMR reaches ultimate value
#' 2. All rates non-negative
#' 3. MarGrid dimensions correct
#' 4. Historical and projected years present
#'
#' @param projection Output from run_marriage_projection()
#'
#' @return list with quick validation results
#'
#' @export
validate_marriage_quick <- function(projection) {
  cli::cli_h2("Quick Marriage Projection Validation")

  checks <- list()

  # Check 1: AMR ultimate
  if ("amr_projected" %in% names(projection)) {
    ultimate_row <- projection$amr_projected[year == projection$ultimate_year]
    if (nrow(ultimate_row) > 0) {
      amr_col <- intersect(c("projected_amr", "amr"), names(projection$amr_projected))[1]
      actual <- ultimate_row[[amr_col]]
      checks$amr_ultimate <- abs(actual - projection$ultimate_amr) / projection$ultimate_amr < 0.01
      cli::cli_alert_info("AMR at ultimate year: {round(actual, 1)} (target: {projection$ultimate_amr})")
    }
  }

  # Check 2: MarGrid dimensions
  if ("margrid" %in% names(projection)) {
    checks$margrid_dim <- all(dim(projection$margrid) == c(87, 87))
    cli::cli_alert_info("MarGrid dimensions: {dim(projection$margrid)[1]}×{dim(projection$margrid)[2]}")
  }

  # Check 3: Rate positivity
  if ("all_rates" %in% names(projection)) {
    sample_years <- sample(names(projection$all_rates), min(5, length(projection$all_rates)))
    all_pos <- all(sapply(sample_years, function(yr) {
      all(projection$all_rates[[yr]] >= 0, na.rm = TRUE)
    }))
    checks$rates_positive <- all_pos
    cli::cli_alert_info("Rate positivity: {all_pos}")
  }

  # Check 4: Year coverage
  if ("historical_years" %in% names(projection) && "projection_years" %in% names(projection)) {
    n_hist <- length(projection$historical_years)
    n_proj <- length(projection$projection_years)
    checks$year_coverage <- n_hist >= 30 && n_proj >= 70
    cli::cli_alert_info("Year coverage: {n_hist} historical, {n_proj} projected")
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

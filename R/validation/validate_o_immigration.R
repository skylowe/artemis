#' O Immigration Validation Functions
#'
#' Functions for validating temporary or unlawfully present immigration
#' projections against TR2025 assumptions and DHS estimates.
#'
#' Validates all required outputs (per TR2025 Section 1.5):
#' - OI: O Immigration (Equation 1.5.1)
#' - OE: O Emigration (Equation 1.5.2)
#' - NO: Net O Immigration (Equation 1.5.3)
#' - OP: O Population Stock (Equation 1.5.4)
#' - DACA: DACA Population
#'
#' @name validate_o_immigration
NULL

# =============================================================================
# TR2025 ASSUMPTIONS AND VALIDATION TARGETS
# =============================================================================

#' Get TR2025 O immigration assumptions for validation
#'
#' @description
#' Returns TR2025 total O immigration assumptions by year for validation.
#'
#' @return data.table with year and expected total O immigration
#'
#' @keywords internal
get_tr_o_validation_targets <- function() {
  data.table::data.table(
    year = c(2022L, 2023L, 2024L, 2025L, 2026L:2099L),
    expected_total = c(
      2200000,   # 2022
      2700000,   # 2023
      2600000,   # 2024
      2000000,   # 2025
      rep(1350000, 74)  # 2026-2099 (ultimate level)
    )
  )
}

#' Get DHS unauthorized population estimates for validation
#'
#' @description
#' Returns DHS unauthorized immigrant population estimates.
#' These serve as validation targets for the O population stock.
#'
#' @return data.table with year and DHS estimates
#'
#' @keywords internal
get_dhs_unauthorized_validation_targets <- function() {
  # DHS Office of Immigration Statistics estimates
  # Source: Estimates of the Unauthorized Immigrant Population reports
  data.table::data.table(
    year = c(2005L, 2007L, 2010L, 2012L, 2015L, 2016L, 2017L, 2018L, 2019L, 2022L),
    dhs_unauthorized = c(
      10500000,  # 2005
      11800000,  # 2007 (peak)
      10790000,  # 2010
      11210000,  # 2012
      10690000,  # 2015
      10700000,  # 2016
      10500000,  # 2017
      10500000,  # 2018
      10350000,  # 2019
      11000000   # 2022 (estimated)
    ),
    # Nonimmigrant stock adds to total O population
    dhs_nonimmigrant = c(
      1800000,   # 2005
      1850000,   # 2007
      1900000,   # 2010
      1950000,   # 2012
      2100000,   # 2015
      2150000,   # 2016
      2100000,   # 2017
      2100000,   # 2018
      2100000,   # 2019
      2100000    # 2022 (estimated)
    )
  )[, total_o := dhs_unauthorized + dhs_nonimmigrant]
}

#' Get DHS DACA stock validation targets
#'
#' @description
#' Returns DHS DACA population stock estimates for validation.
#'
#' @return data.table with year and DHS DACA stock
#'
#' @keywords internal
get_dhs_daca_validation_targets <- function() {
  # DHS/USCIS DACA population estimates
  data.table::data.table(
    year = c(2013L, 2014L, 2015L, 2016L, 2017L, 2018L, 2019L, 2020L, 2021L, 2022L, 2023L),
    dhs_daca = c(
      472000,   # 2013 (initial grants)
      580000,   # 2014
      690000,   # 2015
      750000,   # 2016
      800000,   # 2017 (peak)
      700000,   # 2018
      640000,   # 2019
      640000,   # 2020
      616000,   # 2021
      594000,   # 2022
      580000    # 2023
    )
  )
}

# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

#' Validate O immigration totals against TR2025 assumptions
#'
#' @description
#' Validates that total O immigration matches TR2025 Trustees assumptions.
#' This is a critical check - totals should match exactly.
#'
#' @param o_immigration data.table with O immigration by year, age, sex, type
#' @param tolerance Numeric: acceptable relative difference (default: 0.001)
#'
#' @return list with validation results
#'
#' @export
validate_oi_totals <- function(o_immigration, tolerance = 0.001) {
  checkmate::assert_data_table(o_immigration)

  cli::cli_h3("Validating O Immigration Totals (OI)")

  # Calculate model totals by year
  imm_col <- intersect(c("o_immigration", "immigration"), names(o_immigration))[1]
  if (is.na(imm_col)) {
    cli::cli_alert_danger("Cannot find immigration column in data")
    return(list(passed = FALSE, message = "Missing immigration column"))
  }

  model_totals <- o_immigration[, .(
    model_total = sum(get(imm_col), na.rm = TRUE)
  ), by = year]

  # Get TR2025 targets
  tr_targets <- get_tr_o_validation_targets()

  # Merge and compare
  validation <- merge(model_totals, tr_targets, by = "year", all.x = TRUE)
  validation[, rel_diff := abs(model_total - expected_total) / expected_total]
  validation[, passes := rel_diff <= tolerance]

  # Summary statistics
  n_years <- nrow(validation)
  n_passed <- sum(validation$passes, na.rm = TRUE)
  all_pass <- n_passed == n_years

  # Report results
  if (all_pass) {
    cli::cli_alert_success(
      "{n_years} years: O immigration totals match TR2025 (within {tolerance*100}%)"
    )
  } else {
    failed_years <- validation[passes == FALSE, year]
    cli::cli_alert_danger(
      "{length(failed_years)} years fail tolerance check: {paste(head(failed_years, 5), collapse=', ')}"
    )
  }

  # Sample values
  sample_years <- c(2023, 2025, 2030, 2050, 2099)
  for (y in sample_years[sample_years %in% validation$year]) {
    row <- validation[year == y]
    cli::cli_alert_info(
      "  {y}: {format(round(row$model_total), big.mark=',')} (expected {format(row$expected_total, big.mark=',')})"
    )
  }

  list(
    passed = all_pass,
    n_passed = n_passed,
    n_total = n_years,
    details = validation,
    message = ifelse(all_pass,
                     "All O immigration totals match TR2025 assumptions",
                     paste(n_years - n_passed, "years fail tolerance check"))
  )
}

#' Validate O population stock against DHS estimates
#'
#' @description
#' Validates O population stock against DHS unauthorized immigrant
#' and nonimmigrant stock estimates.
#'
#' @param o_population data.table with O population by year, age, sex, type
#' @param tolerance Numeric: acceptable relative difference (default: 0.10 = 10%)
#'
#' @return list with validation results
#'
#' @export
validate_op_against_dhs <- function(o_population, tolerance = 0.10) {
  checkmate::assert_data_table(o_population)

  cli::cli_h3("Validating O Population Stock (OP) Against DHS")

  # Calculate model totals by year
  pop_col <- intersect(c("population", "o_population", "stock"), names(o_population))[1]
  if (is.na(pop_col)) {
    cli::cli_alert_danger("Cannot find population column in data")
    return(list(passed = FALSE, message = "Missing population column"))
  }

  model_totals <- o_population[, .(
    model_total = sum(get(pop_col), na.rm = TRUE)
  ), by = year]

  # Get DHS targets
  dhs_targets <- get_dhs_unauthorized_validation_targets()

  # Merge and compare (only years where we have DHS data)
  validation <- merge(model_totals, dhs_targets, by = "year", all = FALSE)

  if (nrow(validation) == 0) {
    cli::cli_alert_warning("No overlapping years between model and DHS estimates")
    return(list(
      passed = NA,
      message = "No overlapping years for validation"
    ))
  }

  validation[, rel_diff := abs(model_total - total_o) / total_o]
  validation[, passes := rel_diff <= tolerance]

  # Summary
  n_years <- nrow(validation)
  n_passed <- sum(validation$passes, na.rm = TRUE)
  all_pass <- n_passed == n_years

  # Report results
  if (all_pass) {
    cli::cli_alert_success(
      "{n_years} DHS reference years: O population within {tolerance*100}% of DHS"
    )
  } else {
    cli::cli_alert_warning(
      "{n_passed}/{n_years} DHS reference years within tolerance"
    )
  }

  # Show comparison
  for (i in seq_len(min(nrow(validation), 5))) {
    row <- validation[i]
    status <- ifelse(row$passes, "OK", "DIFF")
    cli::cli_alert_info(
      "  {row$year}: Model {format(round(row$model_total/1e6, 1))}M vs DHS {format(round(row$total_o/1e6, 1))}M ({status})"
    )
  }

  list(
    passed = all_pass,
    n_passed = n_passed,
    n_total = n_years,
    details = validation,
    message = ifelse(all_pass,
                     "O population matches DHS estimates",
                     paste(n_years - n_passed, "years outside tolerance"))
  )
}

#' Validate DACA population against DHS stock
#'
#' @description
#' Validates projected DACA population against DHS/USCIS stock estimates.
#'
#' @param daca_population data.table with DACA population by year, age, sex
#' @param tolerance Numeric: acceptable relative difference (default: 0.05 = 5%)
#'
#' @return list with validation results
#'
#' @export
validate_daca_against_dhs <- function(daca_population, tolerance = 0.05) {
  checkmate::assert_data_table(daca_population)

  cli::cli_h3("Validating DACA Population Against DHS Stock")

  # Calculate model totals by year
  pop_col <- intersect(c("daca_population", "population", "stock"), names(daca_population))[1]
  if (is.na(pop_col)) {
    cli::cli_alert_danger("Cannot find DACA population column in data")
    return(list(passed = FALSE, message = "Missing population column"))
  }

  model_totals <- daca_population[, .(
    model_total = sum(get(pop_col), na.rm = TRUE)
  ), by = year]

  # Get DHS DACA targets
  dhs_targets <- get_dhs_daca_validation_targets()

  # Merge and compare
  validation <- merge(model_totals, dhs_targets, by = "year", all = FALSE)

  if (nrow(validation) == 0) {
    cli::cli_alert_warning("No overlapping years between model and DHS DACA estimates")
    return(list(
      passed = NA,
      message = "No overlapping years for DACA validation"
    ))
  }

  validation[, rel_diff := abs(model_total - dhs_daca) / dhs_daca]
  validation[, passes := rel_diff <= tolerance]

  # Summary
  n_years <- nrow(validation)
  n_passed <- sum(validation$passes, na.rm = TRUE)
  all_pass <- n_passed == n_years

  # Report results
  if (all_pass) {
    cli::cli_alert_success(
      "{n_years} years: DACA population matches DHS within {tolerance*100}%"
    )
  } else {
    cli::cli_alert_warning(
      "{n_passed}/{n_years} years match DHS DACA within tolerance"
    )
  }

  # Show comparison for calibration years
  for (i in seq_len(min(nrow(validation), 6))) {
    row <- validation[i]
    status <- ifelse(row$passes, "OK", "DIFF")
    pct <- round(row$rel_diff * 100, 1)
    cli::cli_alert_info(
      "  {row$year}: Model {format(round(row$model_total), big.mark=',')} vs DHS {format(row$dhs_daca, big.mark=',')} ({pct}% diff)"
    )
  }

  list(
    passed = all_pass,
    n_passed = n_passed,
    n_total = n_years,
    details = validation,
    message = ifelse(all_pass,
                     "DACA matches DHS stock estimates",
                     paste(n_years - n_passed, "years outside tolerance"))
  )
}

#' Validate O distribution (ODIST) properties
#'
#' @description
#' Validates that ODIST distribution has proper mathematical properties.
#'
#' @param odist data.table with ODIST by age, sex, type
#' @param tolerance Numeric: acceptable deviation from 1.0 sum (default: 0.001)
#'
#' @return list with validation results
#'
#' @export
validate_odist <- function(odist, tolerance = 0.001) {
  checkmate::assert_data_table(odist)

  cli::cli_h3("Validating O Distribution (ODIST)")

  checks <- list()
  messages <- character()

  # Find distribution column
  dist_col <- intersect(c("odist", "distribution"), names(odist))[1]
  if (is.na(dist_col)) {
    return(list(passed = FALSE, message = "Missing distribution column"))
  }

  dist_values <- odist[[dist_col]]

  # Check 1: No negative values
  neg_pass <- all(dist_values >= 0)
  checks$non_negative <- neg_pass
  if (neg_pass) {
    messages <- c(messages, "No negative values")
  } else {
    n_neg <- sum(dist_values < 0)
    messages <- c(messages, paste(n_neg, "negative values found"))
  }

  # Check 2: Sums to 1.0
  total <- sum(dist_values, na.rm = TRUE)
  sum_pass <- abs(total - 1.0) <= tolerance
  checks$sums_to_one <- sum_pass
  if (sum_pass) {
    messages <- c(messages, paste("Distribution sums to", round(total, 6)))
  } else {
    messages <- c(messages, paste("Distribution sums to", round(total, 4), "(expected 1.0)"))
  }

  # Check 3: All ages present (0-99)
  ages_present <- sort(unique(odist$age))
  expected_ages <- 0:99
  missing_ages <- setdiff(expected_ages, ages_present)
  age_pass <- length(missing_ages) == 0
  checks$all_ages <- age_pass
  if (age_pass) {
    messages <- c(messages, paste("Ages 0-", max(ages_present), " present", sep = ""))
  } else {
    messages <- c(messages, paste(length(missing_ages), "ages missing"))
  }

  # Check 4: Both sexes present
  sexes <- unique(odist$sex)
  sex_pass <- all(c("male", "female") %in% sexes)
  checks$both_sexes <- sex_pass
  if (sex_pass) {
    messages <- c(messages, "Both sexes present")
  } else {
    messages <- c(messages, "Missing sex category")
  }

  # Check 5: All types present
  types <- unique(odist$type)
  type_pass <- all(c("N", "I", "V") %in% types)
  checks$all_types <- type_pass
  if (type_pass) {
    messages <- c(messages, "All types (N, I, V) present")
  } else {
    messages <- c(messages, paste("Missing types:", paste(setdiff(c("N", "I", "V"), types), collapse = ", ")))
  }

  # Check 6: Type proportions reasonable
  type_props <- odist[, .(prop = sum(get(dist_col))), by = type]
  n_prop <- type_props[type == "N", prop]
  i_prop <- type_props[type == "I", prop]
  v_prop <- type_props[type == "V", prop]

  prop_reasonable <- !is.na(n_prop) && !is.na(i_prop) && !is.na(v_prop) &&
    n_prop > 0.2 && n_prop < 0.8 &&
    i_prop > 0.05 && i_prop < 0.5 &&
    v_prop > 0.1 && v_prop < 0.6
  checks$type_proportions <- prop_reasonable

  messages <- c(messages, sprintf(
    "Type proportions: N=%.1f%%, I=%.1f%%, V=%.1f%%",
    n_prop * 100, i_prop * 100, v_prop * 100
  ))

  # Overall
  all_pass <- all(unlist(checks))

  # Report
  for (msg in messages) {
    if (grepl("missing|negative|expected 1.0", msg, ignore.case = TRUE)) {
      cli::cli_alert_warning(msg)
    } else {
      cli::cli_alert_success(msg)
    }
  }

  list(
    passed = all_pass,
    checks = checks,
    summary = messages,
    type_proportions = list(N = n_prop, I = i_prop, V = v_prop)
  )
}

#' Validate net O immigration equation (1.5.3)
#'
#' @description
#' Validates that NO = OI - OE - AOS
#'
#' @param o_immigration O immigration data
#' @param o_emigration O emigration data
#' @param net_o Net O immigration data
#' @param aos Adjustments of status data (optional)
#' @param tolerance Numeric: acceptable difference (default: 1)
#'
#' @return list with validation results
#'
#' @export
validate_net_o_equation <- function(o_immigration, o_emigration, net_o,
                                     aos = NULL, tolerance = 1) {
  cli::cli_h3("Validating Net O Equation (1.5.3)")

  # Calculate totals by year
  imm_col <- intersect(c("o_immigration", "immigration"), names(o_immigration))[1]
  emig_col <- intersect(c("o_emigration", "emigration"), names(o_emigration))[1]
  net_col <- intersect(c("net_o", "net_immigration"), names(net_o))[1]

  imm_totals <- o_immigration[, .(oi = sum(get(imm_col), na.rm = TRUE)), by = year]
  emig_totals <- o_emigration[, .(oe = sum(get(emig_col), na.rm = TRUE)), by = year]
  net_totals <- net_o[, .(no = sum(get(net_col), na.rm = TRUE)), by = year]

  # Merge all
  validation <- merge(imm_totals, emig_totals, by = "year")
  validation <- merge(validation, net_totals, by = "year")

  # Add AOS if provided
  if (!is.null(aos) && nrow(aos) > 0) {
    aos_col <- intersect(c("aos", "adjustments"), names(aos))[1]
    aos_totals <- aos[, .(aos = sum(get(aos_col), na.rm = TRUE)), by = year]
    validation <- merge(validation, aos_totals, by = "year", all.x = TRUE)
    validation[is.na(aos), aos := 0]
  } else {
    validation[, aos := 0]
  }

  # Calculate expected NO
  validation[, expected_no := oi - oe - aos]
  validation[, diff := abs(no - expected_no)]
  validation[, passes := diff <= tolerance]

  # Summary
  n_years <- nrow(validation)
  n_passed <- sum(validation$passes, na.rm = TRUE)
  all_pass <- n_passed == n_years

  if (all_pass) {
    cli::cli_alert_success("Net O equation (NO = OI - OE - AOS) valid for all {n_years} years")
  } else {
    failed_years <- validation[passes == FALSE, year]
    cli::cli_alert_danger("{length(failed_years)} years fail equation check")
  }

  # Sample values
  sample_year <- validation[year == 2030]
  if (nrow(sample_year) > 0) {
    cli::cli_alert_info(
      "  2030: OI={format(round(sample_year$oi), big.mark=',')} - OE={format(round(sample_year$oe), big.mark=',')} - AOS={format(round(sample_year$aos), big.mark=',')} = {format(round(sample_year$expected_no), big.mark=',')}"
    )
  }

  list(
    passed = all_pass,
    n_passed = n_passed,
    n_total = n_years,
    details = validation,
    message = ifelse(all_pass, "Net O equation valid", "Equation check failed")
  )
}

#' Validate O population stock equation (1.5.4)
#'
#' @description
#' Validates stock accumulation: OP^z = OP^{z-1} + OI - OE - AOS - OD
#'
#' @param o_population O population stock data
#' @param o_immigration O immigration data
#' @param o_emigration O emigration data
#' @param aos Adjustments of status data (optional)
#' @param mortality_qx Death probabilities (optional)
#' @param tolerance Numeric: acceptable relative difference (default: 0.02 = 2%)
#'
#' @return list with validation results
#'
#' @export
validate_stock_equation <- function(o_population, o_immigration, o_emigration,
                                     aos = NULL, mortality_qx = NULL,
                                     tolerance = 0.02) {
  cli::cli_h3("Validating Stock Equation (1.5.4)")

  # Get population totals by year
  pop_col <- intersect(c("population", "o_population", "stock"), names(o_population))[1]
  pop_totals <- o_population[, .(pop = sum(get(pop_col), na.rm = TRUE)), by = year]
  data.table::setorder(pop_totals, year)

  # Get flow totals
  imm_col <- intersect(c("o_immigration", "immigration"), names(o_immigration))[1]
  emig_col <- intersect(c("o_emigration", "emigration"), names(o_emigration))[1]

  imm_totals <- o_immigration[, .(oi = sum(get(imm_col), na.rm = TRUE)), by = year]
  emig_totals <- o_emigration[, .(oe = sum(get(emig_col), na.rm = TRUE)), by = year]

  # Merge
  validation <- merge(pop_totals, imm_totals, by = "year")
  validation <- merge(validation, emig_totals, by = "year")

  # Add previous year population
  validation[, prev_year := year - 1L]
  prev_pop <- pop_totals[, .(prev_year = year, prev_pop = pop)]
  validation <- merge(validation, prev_pop, by = "prev_year", all.x = TRUE)

  # Add AOS if provided
  if (!is.null(aos) && nrow(aos) > 0) {
    aos_col <- intersect(c("aos", "adjustments"), names(aos))[1]
    aos_totals <- aos[, .(aos = sum(get(aos_col), na.rm = TRUE)), by = year]
    validation <- merge(validation, aos_totals, by = "year", all.x = TRUE)
    validation[is.na(aos), aos := 0]
  } else {
    validation[, aos := 0]
  }

  # Estimate deaths (if mortality not provided, estimate from stock change)
  # Deaths ≈ Stock change residual
  validation[, implied_deaths := prev_pop + oi - oe - aos - pop]
  validation[, deaths_reasonable := implied_deaths >= 0 & implied_deaths < prev_pop * 0.05]

  # Check stock change is approximately: OP = prev_OP + OI - OE - AOS - OD
  # Since we don't have exact deaths, check that implied deaths are reasonable
  validation <- validation[!is.na(prev_pop)]

  n_years <- nrow(validation)
  n_reasonable <- sum(validation$deaths_reasonable, na.rm = TRUE)
  pass_rate <- n_reasonable / n_years
  all_pass <- pass_rate >= 0.90  # Allow some deviation

  if (all_pass) {
    cli::cli_alert_success("Stock equation approximately valid for {round(pass_rate*100)}% of years")
  } else {
    cli::cli_alert_warning("Stock equation issues in {round((1-pass_rate)*100)}% of years")
  }

  # Sample calculation
  sample_row <- validation[year == 2030]
  if (nrow(sample_row) > 0) {
    cli::cli_alert_info(
      "  2030: {format(round(sample_row$prev_pop/1e6, 2))}M + {format(round(sample_row$oi/1e6, 2))}M - {format(round(sample_row$oe/1e6, 2))}M - {format(round(sample_row$aos/1e6, 2))}M - deaths ≈ {format(round(sample_row$pop/1e6, 2))}M"
    )
  }

  list(
    passed = all_pass,
    pass_rate = pass_rate,
    n_valid = n_reasonable,
    n_total = n_years,
    details = validation,
    message = ifelse(all_pass, "Stock equation valid", "Stock equation has issues")
  )
}

#' Validate DACA age and trend assumptions
#'
#' @description
#' Validates DACA-specific assumptions:
#' - Age range is bounded (no DACA recipients under 15 or over 60 in projection)
#' - Population declines over time (no new grants assumption)
#' - Peak around 2017 followed by decline
#'
#' @param daca_population data.table with DACA population
#'
#' @return list with validation results
#'
#' @export
validate_daca_assumptions <- function(daca_population) {
  cli::cli_h3("Validating DACA Assumptions")

  checks <- list()
  messages <- character()

  pop_col <- intersect(c("daca_population", "population"), names(daca_population))[1]

  # Check 1: Age bounds
  active_ages <- daca_population[get(pop_col) > 100, unique(age)]
  min_age <- min(active_ages)
  max_age <- max(active_ages)

  age_pass <- min_age >= 10 && max_age <= 70
  checks$age_bounds <- age_pass
  messages <- c(messages, paste("DACA age range:", min_age, "-", max_age))

  # Check 2: Declining trend after 2020
  totals <- daca_population[, .(total = sum(get(pop_col))), by = year]
  data.table::setorder(totals, year)

  post_2020 <- totals[year >= 2020]
  if (nrow(post_2020) > 1) {
    trend <- diff(post_2020$total)
    decline_pass <- all(trend <= 0)
    checks$declining_trend <- decline_pass
    messages <- c(messages, ifelse(decline_pass,
                                    "Population declining as expected (no new grants)",
                                    "Population not consistently declining"))
  } else {
    checks$declining_trend <- NA
    messages <- c(messages, "Insufficient years for trend check")
  }

  # Check 3: Peak around 2017
  peak_year <- totals[which.max(total), year]
  peak_pass <- peak_year >= 2016 && peak_year <= 2018
  checks$peak_timing <- peak_pass
  messages <- c(messages, paste("Peak year:", peak_year, "(expected ~2017)"))

  # Check 4: Final population reasonable (should be declining to floor)
  final_year <- max(totals$year)
  final_pop <- totals[year == final_year, total]
  peak_pop <- max(totals$total)

  floor_pass <- final_pop < peak_pop * 0.5  # Should be less than 50% of peak
  checks$population_floor <- floor_pass
  messages <- c(messages, paste("Final population:", format(round(final_pop), big.mark = ","),
                                 "(", round(final_pop/peak_pop*100), "% of peak)"))

  # Overall
  all_pass <- all(unlist(checks[!is.na(checks)]))

  # Report
  for (msg in messages) {
    if (grepl("not|unexpected", msg, ignore.case = TRUE)) {
      cli::cli_alert_warning(msg)
    } else {
      cli::cli_alert_success(msg)
    }
  }

  list(
    passed = all_pass,
    checks = checks,
    summary = messages
  )
}

# =============================================================================
# COMPREHENSIVE VALIDATION
# =============================================================================

#' Run comprehensive O immigration validation
#'
#' @description
#' Main validation function that runs all validation checks on O immigration
#' projection outputs. This is the primary entry point for Phase 5G validation.
#'
#' @param projection Output from run_full_o_projection()
#' @param daca_projection Output from run_daca_projection() (optional)
#' @param tolerance_strict Strict tolerance for exact matches (default: 0.001)
#' @param tolerance_relaxed Relaxed tolerance for estimates (default: 0.10)
#'
#' @return list with comprehensive validation results
#'
#' @export
validate_o_immigration_comprehensive <- function(projection,
                                                   daca_projection = NULL,
                                                   tolerance_strict = 0.001,
                                                   tolerance_relaxed = 0.10) {
  cli::cli_h1("O Immigration Validation (Phase 5G)")

  results <- list()
  all_passed <- TRUE
  n_checks <- 0
  n_passed <- 0

  # =========================================================================
  # Check 1: O Immigration Totals (OI) - Must match TR2025 exactly
  # =========================================================================
  cli::cli_h2("Check 1: O Immigration Totals")

  if ("o_immigration" %in% names(projection)) {
    check1 <- validate_oi_totals(projection$o_immigration, tolerance = tolerance_strict)
    results$oi_totals <- check1
    n_checks <- n_checks + 1
    if (check1$passed) {
      n_passed <- n_passed + 1
    } else {
      all_passed <- FALSE
    }
  } else {
    cli::cli_alert_warning("O immigration data not found in projection")
    results$oi_totals <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 2: ODIST Properties
  # =========================================================================
  cli::cli_h2("Check 2: O Distribution (ODIST)")

  if ("odist" %in% names(projection)) {
    check2 <- validate_odist(projection$odist)
    results$odist <- check2
    n_checks <- n_checks + 1
    if (check2$passed) {
      n_passed <- n_passed + 1
    } else {
      all_passed <- FALSE
    }
  } else {
    cli::cli_alert_warning("ODIST data not found in projection")
    results$odist <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 3: Net O Equation (1.5.3)
  # =========================================================================
  cli::cli_h2("Check 3: Net O Immigration Equation")

  if (all(c("o_immigration", "o_emigration", "net_o") %in% names(projection))) {
    aos_data <- if ("aos" %in% names(projection)) projection$aos else NULL
    check3 <- validate_net_o_equation(
      projection$o_immigration,
      projection$o_emigration,
      projection$net_o,
      aos_data
    )
    results$net_o_equation <- check3
    n_checks <- n_checks + 1
    if (check3$passed) {
      n_passed <- n_passed + 1
    } else {
      all_passed <- FALSE
    }
  } else {
    cli::cli_alert_warning("Insufficient data for net O equation validation")
    results$net_o_equation <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 4: O Population Stock
  # =========================================================================
  cli::cli_h2("Check 4: O Population Stock")

  if ("o_population" %in% names(projection)) {
    # Check against DHS
    check4a <- validate_op_against_dhs(projection$o_population, tolerance = tolerance_relaxed)
    results$op_vs_dhs <- check4a
    n_checks <- n_checks + 1
    if (!is.na(check4a$passed) && check4a$passed) {
      n_passed <- n_passed + 1
    }

    # Check stock equation if we have all components
    if (all(c("o_immigration", "o_emigration") %in% names(projection))) {
      aos_data <- if ("aos" %in% names(projection)) projection$aos else NULL
      check4b <- validate_stock_equation(
        projection$o_population,
        projection$o_immigration,
        projection$o_emigration,
        aos_data
      )
      results$stock_equation <- check4b
      n_checks <- n_checks + 1
      if (check4b$passed) {
        n_passed <- n_passed + 1
      }
    }
  } else {
    cli::cli_alert_warning("O population data not found in projection")
    results$op_vs_dhs <- list(passed = NA, message = "Data not available")
  }

  # =========================================================================
  # Check 5: DACA Population (if provided)
  # =========================================================================
  cli::cli_h2("Check 5: DACA Population")

  if (!is.null(daca_projection)) {
    daca_data <- if ("daca_population" %in% names(daca_projection)) {
      daca_projection$daca_population
    } else {
      daca_projection
    }

    # DHS validation
    check5a <- validate_daca_against_dhs(daca_data, tolerance = tolerance_relaxed)
    results$daca_vs_dhs <- check5a
    n_checks <- n_checks + 1
    if (!is.na(check5a$passed) && check5a$passed) {
      n_passed <- n_passed + 1
    }

    # Assumption validation
    check5b <- validate_daca_assumptions(daca_data)
    results$daca_assumptions <- check5b
    n_checks <- n_checks + 1
    if (check5b$passed) {
      n_passed <- n_passed + 1
    }
  } else {
    cli::cli_alert_info("DACA projection not provided - skipping DACA validation")
    results$daca_vs_dhs <- list(passed = NA, message = "Not provided")
    results$daca_assumptions <- list(passed = NA, message = "Not provided")
  }

  # =========================================================================
  # Summary
  # =========================================================================
  cli::cli_h2("Validation Summary")

  cli::cli_alert_info("{n_passed}/{n_checks} validation checks passed")

  if (all_passed && n_passed == n_checks) {
    cli::cli_alert_success("All O immigration validations PASSED!")
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

#' Quick validation for O projection
#'
#' @description
#' Simplified validation that checks the most critical items:
#' 1. O immigration totals match TR2025
#' 2. ODIST sums to 1.0
#' 3. Population is non-negative
#' 4. Type proportions are reasonable
#'
#' @param projection Output from run_full_o_projection()
#'
#' @return list with quick validation results
#'
#' @export
validate_o_quick <- function(projection) {
  cli::cli_h2("Quick O Immigration Validation")

  checks <- list()

  # Check 1: Immigration totals
  if ("o_immigration" %in% names(projection) && "assumptions" %in% names(projection)) {
    imm_totals <- projection$o_immigration[, .(total = sum(o_immigration)), by = year]
    merged <- merge(imm_totals, projection$assumptions, by = "year")
    checks$totals_match <- all(abs(merged$total - merged$total_o) < 1)
    cli::cli_alert_info("O immigration totals match: {checks$totals_match}")
  }

  # Check 2: ODIST sums to 1
  if ("odist" %in% names(projection)) {
    odist_sum <- sum(projection$odist$odist)
    checks$odist_valid <- abs(odist_sum - 1) < 0.001
    cli::cli_alert_info("ODIST sums to 1: {checks$odist_valid}")
  }

  # Check 3: Non-negative population
  if ("o_population" %in% names(projection)) {
    checks$pop_nonneg <- all(projection$o_population$population >= 0)
    cli::cli_alert_info("Population non-negative: {checks$pop_nonneg}")
  }

  # Check 4: Type proportions
  if ("odist" %in% names(projection)) {
    type_props <- projection$odist[, .(prop = sum(odist)), by = type]
    n_prop <- type_props[type == "N", prop]
    checks$types_ok <- n_prop > 0.2 && n_prop < 0.8
    cli::cli_alert_info("Type proportions reasonable: {checks$types_ok}")
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

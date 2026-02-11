#' LPR Immigration Validation Functions
#'
#' Functions for validating LPR immigration and emigration projections
#' against TR2025 assumptions and data quality requirements.
#'
#' Validates all 5 required outputs (per TR2025 Section 1.3):
#' - L: Total LPR Immigration
#' - NEW: New Arrivals
#' - AOS: Adjustments of Status
#' - E: Legal Emigration
#' - NL: Net LPR Immigration
#'
#' @name validate_lpr_immigration
NULL

#' Validate LPR immigration outputs against TR2025
#'
#' @description
#' Comprehensive validation of LPR immigration projections against
#' Trustees Report assumptions and data quality requirements.
#'
#' @param lpr_projected data.table with projected LPR immigration by year, age, sex
#' @param emigration_projected data.table with projected emigration (optional)
#' @param tolerance Numeric: acceptable relative difference (default: 0.01)
#'
#' @return list with validation results:
#'   - passed: logical, TRUE if all validations pass
#'   - checks: data.table with individual check results
#'   - summary: character vector of validation messages
#'
#' @export
validate_lpr_immigration_outputs <- function(lpr_projected,
                                              emigration_projected = NULL,
                                              assumptions = NULL,
                                              tolerance = 0.01) {
  lpr_projected <- data.table::as.data.table(lpr_projected)

  results <- list()
  checks <- list()
  messages <- character()

  # Check 1: Total LPR matches Trustees assumptions
  totals_by_year <- lpr_projected[, .(total = sum(immigration)), by = year]

  # Use V.A2 assumptions if provided, otherwise fall back to approximate values
  if (!is.null(assumptions)) {
    assumptions <- data.table::as.data.table(assumptions)
    expected <- assumptions[, .(year, expected_total = total_lpr)]
  } else {
    expected <- data.table::data.table(year = unique(totals_by_year$year))
    expected[, expected_total := data.table::fcase(
      year == 2024, 1263000,
      year %in% 2025:2026, 1213000,
      year >= 2027, 1050000,
      default = 1050000
    )]
  }

  validation <- merge(totals_by_year, expected, by = "year")
  validation[, rel_diff := abs(total - expected_total) / expected_total]
  validation[, passes := rel_diff <= tolerance]

  all_pass <- all(validation$passes)
  checks$totals <- list(
    name = "Total LPR matches Trustees assumptions",
    passed = all_pass,
    details = validation
  )

  if (all_pass) {
    messages <- c(messages, cli::format_message(
      "{.val {nrow(validation)}} years: Total LPR matches TR2025 assumptions (within {tolerance*100}%)"
    ))
  } else {
    failed_years <- validation[passes == FALSE, year]
    messages <- c(messages, cli::format_message(
      "{.alert Warning: {length(failed_years)} years fail tolerance check: {paste(failed_years, collapse=', ')}}"
    ))
  }

  # Check 2: Age range validation (0-99)
  age_range <- range(lpr_projected$age)
  age_pass <- age_range[1] == 0 && age_range[2] >= 99
  checks$age_range <- list(
    name = "Age range 0-99+",
    passed = age_pass,
    details = list(min = age_range[1], max = age_range[2])
  )

  if (age_pass) {
    messages <- c(messages, cli::format_message(
      "Age range: {age_range[1]}-{age_range[2]}"
    ))
  } else {
    messages <- c(messages, cli::format_message(
      "{.alert Warning: Age range {age_range[1]}-{age_range[2]} (expected 0-99+)}"
    ))
  }

  # Check 3: Both sexes present
  sexes <- unique(lpr_projected$sex)
  sex_pass <- all(c("male", "female") %in% sexes)
  checks$sexes <- list(
    name = "Both sexes present",
    passed = sex_pass,
    details = sexes
  )

  if (sex_pass) {
    messages <- c(messages, "Both sexes present")
  } else {
    messages <- c(messages, cli::format_message(
      "{.alert Warning: Missing sex categories}"
    ))
  }

  # Check 4: No negative values
  neg_values <- lpr_projected[immigration < 0]
  neg_pass <- nrow(neg_values) == 0
  checks$non_negative <- list(
    name = "No negative immigration values",
    passed = neg_pass,
    details = if(nrow(neg_values) > 0) neg_values else "No negative values"
  )

  if (neg_pass) {
    messages <- c(messages, "No negative immigration values")
  } else {
    messages <- c(messages, cli::format_message(
      "{.alert Warning: {nrow(neg_values)} negative immigration values found}"
    ))
  }

  # Check 5: Distribution by year sums correctly
  year_totals <- lpr_projected[, .(
    total = sum(immigration),
    n_cells = .N
  ), by = year]
  cells_per_year <- year_totals$n_cells[1]
  all_same <- all(year_totals$n_cells == cells_per_year)

  checks$year_structure <- list(
    name = "Consistent structure across years",
    passed = all_same,
    details = year_totals
  )

  if (all_same) {
    messages <- c(messages, cli::format_message(
      "Consistent structure: {cells_per_year} age-sex cells per year"
    ))
  }

  # Check 6: Emigration validation (if provided)
  if (!is.null(emigration_projected)) {
    emigration_projected <- data.table::as.data.table(emigration_projected)

    emig_totals <- emigration_projected[, .(total_emig = sum(emigration)), by = year]
    lpr_totals_for_emig <- lpr_projected[, .(total_lpr = sum(immigration)), by = year]

    emig_check <- merge(emig_totals, lpr_totals_for_emig, by = "year")
    emig_check[, expected_emig := total_lpr * 0.25]
    emig_check[, rel_diff := abs(total_emig - expected_emig) / expected_emig]
    emig_check[, passes := rel_diff <= tolerance]

    emig_pass <- all(emig_check$passes)
    checks$emigration_ratio <- list(
      name = "Emigration = 25% of LPR immigration",
      passed = emig_pass,
      details = emig_check
    )

    if (emig_pass) {
      messages <- c(messages, "Emigration = 25% of LPR immigration")
    } else {
      messages <- c(messages, cli::format_message(
        "{.alert Warning: Emigration ratio deviates from 25%}"
      ))
    }
  }

  # Overall result
  all_passed <- all(sapply(checks, function(x) x$passed))

  list(
    passed = all_passed,
    checks = checks,
    summary = messages
  )
}

#' Validate age-sex distribution properties
#'
#' @description
#' Validates that an age-sex distribution has proper properties.
#'
#' @param distribution data.table with age, sex, distribution columns
#' @param tolerance Numeric: acceptable deviation from 1.0 sum (default: 0.001)
#'
#' @return list with validation results
#'
#' @export
validate_distribution <- function(distribution, tolerance = 0.001) {
  distribution <- data.table::as.data.table(distribution)

  checks <- list()
  messages <- character()

  # Check 1: No negative values
  neg_pass <- all(distribution$distribution >= 0)
  checks$non_negative <- list(
    passed = neg_pass,
    n_negative = sum(distribution$distribution < 0)
  )

  # Check 2: Sums to 1.0
  total <- sum(distribution$distribution)
  sum_pass <- abs(total - 1.0) <= tolerance
  checks$sums_to_one <- list(
    passed = sum_pass,
    total = total,
    deviation = abs(total - 1.0)
  )

  # Check 3: Age range
  age_range <- range(distribution$age)
  age_pass <- age_range[1] == 0 && age_range[2] >= 99
  checks$age_range <- list(
    passed = age_pass,
    min = age_range[1],
    max = age_range[2]
  )

  # Check 4: Both sexes
  sexes <- unique(distribution$sex)
  sex_pass <- all(c("male", "female") %in% sexes)
  checks$both_sexes <- list(
    passed = sex_pass,
    sexes = sexes
  )

  # Check 5: No missing ages
  expected_ages <- 0:max(distribution$age)
  for (s in c("male", "female")) {
    present_ages <- distribution[sex == s, age]
    missing_ages <- setdiff(expected_ages, present_ages)
    if (length(missing_ages) > 0) {
      messages <- c(messages, cli::format_message(
        "Missing ages for {s}: {paste(head(missing_ages, 5), collapse=', ')}{if(length(missing_ages) > 5) '...' else ''}"
      ))
    }
  }

  # Overall
  all_passed <- all(sapply(checks, function(x) x$passed))

  # Summary messages
  if (neg_pass) messages <- c(messages, "No negative values")
  else messages <- c(messages, cli::format_message("{.alert {checks$non_negative$n_negative} negative values}"))

  if (sum_pass) messages <- c(messages, cli::format_message("Distribution sums to {round(total, 6)}"))
  else messages <- c(messages, cli::format_message("{.alert Distribution sums to {round(total, 4)} (expected 1.0)}"))

  if (age_pass) messages <- c(messages, cli::format_message("Age range: {age_range[1]}-{age_range[2]}"))

  if (sex_pass) messages <- c(messages, "Both sexes present")

  list(
    passed = all_passed,
    checks = checks,
    summary = messages
  )
}

#' Run full LPR immigration validation
#'
#' @description
#' Convenience function to run all validations on LPR immigration projection results.
#'
#' @param projection_result list from run_lpr_immigration_projection()
#'
#' @return list with comprehensive validation results
#'
#' @export
run_lpr_validation <- function(projection_result) {
  cli::cli_h2("LPR Immigration Validation")

  # Validate immigration projection
  cli::cli_h3("Immigration Totals")
  imm_val <- validate_lpr_immigration_outputs(projection_result$immigration)
  for (msg in imm_val$summary) {
    if (grepl("Warning", msg)) cli::cli_alert_warning(msg)
    else cli::cli_alert_success(msg)
  }

  # Validate distribution
  cli::cli_h3("Age-Sex Distribution")
  dist_val <- validate_distribution(projection_result$distribution)
  for (msg in dist_val$summary) {
    if (grepl("Warning", msg) || grepl("alert", msg)) cli::cli_alert_warning(msg)
    else cli::cli_alert_success(msg)
  }

  # Summary
  cli::cli_h3("Summary")
  all_passed <- imm_val$passed && dist_val$passed

  if (all_passed) {
    cli::cli_alert_success("All validations passed!")
  } else {
    cli::cli_alert_danger("Some validations failed - see details above")
  }

  list(
    passed = all_passed,
    immigration = imm_val,
    distribution = dist_val
  )
}

#' Compare LPR immigration to net immigration totals
#'
#' @description
#' Compares our LPR immigration/emigration projections to TR2025 net immigration
#' if available in raw data files.
#'
#' @param net_lpr data.table with net LPR immigration projections
#' @param tr_data data.table with TR2025 net immigration (optional)
#'
#' @return list with comparison results
#'
#' @export
compare_to_tr_net_immigration <- function(net_lpr, tr_data = NULL) {
  net_lpr <- data.table::as.data.table(net_lpr)

  # Calculate our totals
  our_totals <- net_lpr[, .(net_lpr = sum(net_lpr)), by = year]

  # TR2025 expected net LPR (LPR - 25% emigration = 75% of LPR)
  expected <- data.table::data.table(year = our_totals$year)
  expected[, gross_lpr := data.table::fcase(
    year == 2024, 1263000,
    year %in% 2025:2026, 1213000,
    year >= 2027, 1050000,
    default = 1050000
  )]
  expected[, net_lpr_expected := gross_lpr * 0.75]

  comparison <- merge(our_totals, expected, by = "year")
  comparison[, diff := net_lpr - net_lpr_expected]
  comparison[, rel_diff := diff / net_lpr_expected]

  cli::cli_h3("Net LPR Immigration Comparison")
  cli::cli_alert_info("Our net LPR vs expected (75% of gross LPR):")

  sample_years <- c(2025, 2030, 2050, 2075, 2099)
  for (y in sample_years[sample_years %in% comparison$year]) {
    row <- comparison[year == y]
    cli::cli_alert_info(
      "{y}: {format(round(row$net_lpr), big.mark=',')} (expected {format(round(row$net_lpr_expected), big.mark=',')})"
    )
  }

  comparison
}

#' Validate all LPR outputs (comprehensive)
#'
#' @description
#' Validates all 5 required outputs from run_lpr_projection():
#' - L (Total LPR) matches TR2025 assumptions
#' - NEW + AOS = L (exact)
#' - E = 25% of L
#' - NL = L - E
#'
#' @param projection_result list from run_lpr_projection()
#' @param tolerance Numeric: acceptable relative difference (default: 0.001)
#'
#' @return list with validation results
#'
#' @export
validate_lpr_outputs <- function(projection_result, tolerance = 0.001) {
  cli::cli_h2("LPR Immigration Validation (All 5 Outputs)")

  checks <- list()
  messages <- character()
  all_passed <- TRUE

  # Extract data from projection result
  lpr <- data.table::as.data.table(projection_result$lpr_immigration)
  new_arrivals <- data.table::as.data.table(projection_result$new_arrivals)
  aos <- data.table::as.data.table(projection_result$aos)
  emigration <- data.table::as.data.table(projection_result$emigration)
  net_lpr <- data.table::as.data.table(projection_result$net_lpr)
  new_aos_ratio <- projection_result$new_aos_ratio

  # =========================================================================
  # Check 1: Total LPR (L) matches TR2025 assumptions
  # =========================================================================
  cli::cli_h3("Check 1: Total LPR matches TR2025")

  lpr_totals <- lpr[, .(total = sum(immigration)), by = year]

  # Use V.A2 assumptions from projection result (not hardcoded)
  assumptions <- data.table::as.data.table(projection_result$assumptions)
  expected_lpr <- assumptions[, .(year, expected = total_lpr)]

  lpr_check <- merge(lpr_totals, expected_lpr, by = "year")
  lpr_check[, rel_diff := abs(total - expected) / expected]
  lpr_check[, passes := rel_diff <= tolerance]

  check1_pass <- all(lpr_check$passes)
  checks$lpr_totals <- list(
    name = "Total LPR matches TR2025",
    passed = check1_pass,
    details = lpr_check[passes == FALSE]
  )

  if (check1_pass) {
    cli::cli_alert_success("All {nrow(lpr_check)} years match TR2025 assumptions")
    messages <- c(messages, "Total LPR matches TR2025")
  } else {
    all_passed <- FALSE
    failed_years <- lpr_check[passes == FALSE, year]
    cli::cli_alert_danger("Failed years: {paste(failed_years, collapse=', ')}")
    messages <- c(messages, paste("LPR total check FAILED for", length(failed_years), "years"))
  }

  # =========================================================================
  # Check 2: NEW + AOS = L
  # =========================================================================
  cli::cli_h3("Check 2: NEW + AOS = L")

  new_totals <- new_arrivals[, .(new_total = sum(new_arrivals)), by = year]
  aos_totals <- aos[, .(aos_total = sum(aos)), by = year]

  new_aos_check <- merge(lpr_totals, new_totals, by = "year")
  new_aos_check <- merge(new_aos_check, aos_totals, by = "year")
  new_aos_check[, sum_new_aos := new_total + aos_total]
  new_aos_check[, diff := abs(total - sum_new_aos)]
  new_aos_check[, passes := diff <= 1]  # Allow for rounding

  check2_pass <- all(new_aos_check$passes)
  checks$new_aos_sum <- list(
    name = "NEW + AOS = L",
    passed = check2_pass,
    details = new_aos_check[passes == FALSE]
  )

  if (check2_pass) {
    cli::cli_alert_success("NEW + AOS = L for all years")
    messages <- c(messages, "NEW + AOS = L")
  } else {
    all_passed <- FALSE
    cli::cli_alert_danger("NEW + AOS != L for some years")
    messages <- c(messages, "NEW + AOS check FAILED")
  }

  # =========================================================================
  # Check 3: Emigration = 25% of LPR
  # =========================================================================
  cli::cli_h3("Check 3: Emigration = 25% of LPR")

  emig_totals <- emigration[, .(emig_total = sum(emigration)), by = year]

  # Use V.A2 emigration totals if available, otherwise fall back to 25% of LPR
  expected_emig <- assumptions[, .(year, expected_emig = total_emigration)]
  emig_check <- merge(emig_totals, expected_emig, by = "year")
  emig_check[, rel_diff := abs(emig_total - expected_emig) / expected_emig]
  emig_check[, passes := rel_diff <= tolerance]

  check3_pass <- all(emig_check$passes)
  checks$emigration_totals <- list(
    name = "Emigration matches V.A2 totals",
    passed = check3_pass,
    details = emig_check[passes == FALSE]
  )

  if (check3_pass) {
    cli::cli_alert_success("Emigration matches V.A2 totals for all years")
    messages <- c(messages, "Emigration matches V.A2")
  } else {
    all_passed <- FALSE
    cli::cli_alert_danger("Emigration deviates from V.A2 for some years")
    messages <- c(messages, "Emigration totals check FAILED")
  }

  # =========================================================================
  # Check 4: Net LPR = L - E
  # =========================================================================
  cli::cli_h3("Check 4: Net LPR = L - E")

  net_totals <- net_lpr[, .(net_total = sum(net_lpr)), by = year]
  net_check <- merge(lpr_totals, emig_totals, by = "year")
  net_check <- merge(net_check, net_totals, by = "year")
  net_check[, expected_net := total - emig_total]
  net_check[, diff := abs(net_total - expected_net)]
  net_check[, passes := diff <= 1]

  check4_pass <- all(net_check$passes)
  checks$net_lpr <- list(
    name = "Net LPR = L - E",
    passed = check4_pass,
    details = net_check[passes == FALSE]
  )

  if (check4_pass) {
    cli::cli_alert_success("Net LPR = L - E for all years")
    messages <- c(messages, "Net LPR = L - E")
  } else {
    all_passed <- FALSE
    cli::cli_alert_danger("Net LPR != L - E for some years")
    messages <- c(messages, "Net LPR check FAILED")
  }

  # =========================================================================
  # Check 5: Distribution properties
  # =========================================================================
  cli::cli_h3("Check 5: Distribution properties")

  # Check LPR distribution
  lpr_dist <- projection_result$distributions$lpr
  dist_sum <- sum(lpr_dist$distribution)
  ages_present <- length(unique(lpr_dist$age))
  sexes_present <- unique(lpr_dist$sex)

  dist_checks <- list(
    sums_to_one = abs(dist_sum - 1.0) <= 0.001,
    all_ages = ages_present >= 100,
    both_sexes = all(c("male", "female") %in% sexes_present),
    no_negative = all(lpr_dist$distribution >= 0)
  )

  check5_pass <- all(unlist(dist_checks))
  checks$distribution <- list(
    name = "Distribution properties",
    passed = check5_pass,
    details = dist_checks
  )

  if (check5_pass) {
    cli::cli_alert_success("Distribution valid: sums to 1, all ages 0-99, both sexes")
    messages <- c(messages, "Distribution properties valid")
  } else {
    all_passed <- FALSE
    failed <- names(dist_checks)[!unlist(dist_checks)]
    cli::cli_alert_danger("Distribution issues: {paste(failed, collapse=', ')}")
    messages <- c(messages, "Distribution check FAILED")
  }

  # =========================================================================
  # Check 6: NEW/AOS ratio validation
  # =========================================================================
  cli::cli_h3("Check 6: NEW/AOS ratio")

  new_ratio <- new_aos_ratio$new_ratio
  aos_ratio <- new_aos_ratio$aos_ratio

  ratio_checks <- list(
    new_ratio_valid = new_ratio > 0 && new_ratio < 1,
    aos_ratio_valid = aos_ratio > 0 && aos_ratio < 1,
    sum_to_one = abs(new_ratio + aos_ratio - 1.0) <= 0.001
  )

  check6_pass <- all(unlist(ratio_checks))
  checks$new_aos_ratio <- list(
    name = "NEW/AOS ratio",
    passed = check6_pass,
    details = list(
      new_ratio = new_ratio,
      aos_ratio = aos_ratio,
      reference_years = new_aos_ratio$reference_years
    )
  )

  if (check6_pass) {
    cli::cli_alert_success("NEW/AOS ratio valid: {round(new_ratio*100, 1)}% / {round(aos_ratio*100, 1)}%")
    messages <- c(messages, paste0("NEW/AOS = ", round(new_ratio*100, 1), "% / ", round(aos_ratio*100, 1), "%"))
  } else {
    all_passed <- FALSE
    cli::cli_alert_danger("NEW/AOS ratio invalid")
    messages <- c(messages, "NEW/AOS ratio FAILED")
  }

  # =========================================================================
  # Summary
  # =========================================================================
  cli::cli_h3("Summary")

  n_checks <- length(checks)
  n_passed <- sum(sapply(checks, function(x) x$passed))

  if (all_passed) {
    cli::cli_alert_success("All {n_checks} validation checks passed!")
  } else {
    cli::cli_alert_danger("{n_passed}/{n_checks} checks passed")
    failed_checks <- names(checks)[!sapply(checks, function(x) x$passed)]
    cli::cli_alert_danger("Failed: {paste(failed_checks, collapse=', ')}")
  }

  # Sample values for quick reference
  cli::cli_alert_info("Sample totals for 2030:")
  cli::cli_alert_info("  L (Total LPR): {format(round(lpr_totals[year==2030, total]), big.mark=',')}")
  cli::cli_alert_info("  NEW: {format(round(new_totals[year==2030, new_total]), big.mark=',')}")
  cli::cli_alert_info("  AOS: {format(round(aos_totals[year==2030, aos_total]), big.mark=',')}")
  cli::cli_alert_info("  E: {format(round(emig_totals[year==2030, emig_total]), big.mark=',')}")
  cli::cli_alert_info("  NL: {format(round(net_totals[year==2030, net_total]), big.mark=',')}")

  list(
    passed = all_passed,
    checks = checks,
    summary = messages
  )
}

#' Validate separate NEW/AOS distributions
#'
#' @description
#' When using separate NEW/AOS distributions (TR2025 Section 1.3 methodology),
#' validates that each distribution and the resulting projections are correct.
#'
#' @param new_distribution data.table with age, sex, distribution
#' @param aos_distribution data.table with age, sex, distribution
#' @param new_arrivals data.table with year, age, sex, new_arrivals
#' @param aos data.table with year, age, sex, aos
#' @param lpr_immigration data.table with year, age, sex, immigration
#' @param assumptions data.table with year, lpr_new, lpr_aos, total_lpr
#' @param tolerance Numeric: acceptable deviation (default: 0.001)
#'
#' @return list with validation results
#'
#' @export
validate_separate_distributions <- function(new_distribution = NULL,
                                             aos_distribution = NULL,
                                             new_arrivals = NULL,
                                             aos = NULL,
                                             lpr_immigration = NULL,
                                             assumptions = NULL,
                                             tolerance = 0.001) {
  checks <- list()
  messages <- character()
  all_passed <- TRUE

  cli::cli_h3("Separate Distribution Validation (TR2025 Section 1.3)")

  # Check 1: NEW distribution sums to 1.0
  if (!is.null(new_distribution)) {
    new_sum <- sum(new_distribution$distribution)
    new_pass <- abs(new_sum - 1.0) <= tolerance
    checks$new_dist_sum <- list(name = "NEW dist sums to 1.0", passed = new_pass)
    if (new_pass) {
      cli::cli_alert_success("NEW distribution sums to {round(new_sum, 6)}")
    } else {
      all_passed <- FALSE
      cli::cli_alert_danger("NEW distribution sums to {round(new_sum, 4)}")
    }
  }

  # Check 2: AOS distribution sums to 1.0
  if (!is.null(aos_distribution)) {
    aos_sum <- sum(aos_distribution$distribution)
    aos_pass <- abs(aos_sum - 1.0) <= tolerance
    checks$aos_dist_sum <- list(name = "AOS dist sums to 1.0", passed = aos_pass)
    if (aos_pass) {
      cli::cli_alert_success("AOS distribution sums to {round(aos_sum, 6)}")
    } else {
      all_passed <- FALSE
      cli::cli_alert_danger("AOS distribution sums to {round(aos_sum, 4)}")
    }
  }

  # Check 3: NEW + AOS = L for each year
  if (!is.null(new_arrivals) && !is.null(aos) && !is.null(lpr_immigration)) {
    new_totals <- new_arrivals[, .(new_total = sum(new_arrivals)), by = year]
    aos_totals <- aos[, .(aos_total = sum(aos)), by = year]
    lpr_totals <- lpr_immigration[, .(lpr_total = sum(immigration)), by = year]

    combined <- merge(merge(new_totals, aos_totals, by = "year"), lpr_totals, by = "year")
    combined[, diff := abs((new_total + aos_total) - lpr_total)]
    combined[, passes := diff <= 1]  # Allow rounding

    sum_pass <- all(combined$passes)
    checks$new_aos_equals_l <- list(name = "NEW + AOS = L", passed = sum_pass)
    if (sum_pass) {
      cli::cli_alert_success("NEW + AOS = L for all {nrow(combined)} years")
    } else {
      all_passed <- FALSE
      failed <- combined[passes == FALSE]
      cli::cli_alert_danger("NEW + AOS != L for years: {paste(failed$year, collapse=', ')}")
    }
  }

  # Check 4: NEW totals match V.A2 assumptions
  if (!is.null(new_arrivals) && !is.null(assumptions) && "lpr_new" %in% names(assumptions)) {
    new_totals <- new_arrivals[, .(projected = sum(new_arrivals)), by = year]
    check <- merge(new_totals, assumptions[, .(year, expected = lpr_new)], by = "year")
    check[, rel_diff := abs(projected - expected) / expected]
    check[, passes := rel_diff <= tolerance]

    new_match <- all(check$passes)
    checks$new_matches_va2 <- list(name = "NEW matches V.A2", passed = new_match)
    if (new_match) {
      cli::cli_alert_success("NEW totals match V.A2 assumptions")
    } else {
      all_passed <- FALSE
      cli::cli_alert_danger("NEW totals deviate from V.A2")
    }
  }

  n_checks <- length(checks)
  n_passed <- sum(sapply(checks, function(x) x$passed))

  if (all_passed) {
    cli::cli_alert_success("All {n_checks} separate distribution checks passed")
  } else {
    cli::cli_alert_danger("{n_passed}/{n_checks} separate distribution checks passed")
  }

  list(passed = all_passed, checks = checks, summary = messages)
}

#' Validate emigration distribution properties
#'
#' @description
#' Validates emigration distribution regardless of source (CBO or DHS).
#'
#' @param distribution data.table with age, sex, distribution
#' @param source Character: "cbo" or "dhs" for reporting
#' @param tolerance Numeric: acceptable deviation from 1.0 sum
#'
#' @return list with validation results
#'
#' @export
validate_emigration_distribution <- function(distribution,
                                              source = "unknown",
                                              tolerance = 0.001) {
  checks <- list()

  dist_sum <- sum(distribution$distribution)
  sum_pass <- abs(dist_sum - 1.0) <= tolerance

  age_range <- range(distribution$age)
  age_pass <- age_range[1] == 0 && age_range[2] >= 99

  sexes <- unique(distribution$sex)
  sex_pass <- all(c("male", "female") %in% sexes)

  no_neg <- all(distribution$distribution >= 0)

  all_passed <- sum_pass && age_pass && sex_pass && no_neg

  if (all_passed) {
    cli::cli_alert_success("Emigration distribution ({source}): valid (ages {age_range[1]}-{age_range[2]}, sums to {round(dist_sum, 6)})")
  } else {
    if (!sum_pass) cli::cli_alert_danger("Emigration distribution sums to {round(dist_sum, 4)}")
    if (!age_pass) cli::cli_alert_danger("Emigration age range: {age_range[1]}-{age_range[2]}")
    if (!sex_pass) cli::cli_alert_danger("Missing sex categories")
    if (!no_neg) cli::cli_alert_danger("Negative values in distribution")
  }

  list(
    passed = all_passed,
    checks = list(
      sums_to_one = sum_pass,
      age_range = age_pass,
      both_sexes = sex_pass,
      non_negative = no_neg
    ),
    source = source
  )
}

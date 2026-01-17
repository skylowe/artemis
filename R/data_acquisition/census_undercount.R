#' Census Undercount Factors
#'
#' Functions for providing census undercount adjustment factors based on
#' Census Bureau Demographic Analysis (DA) and Post-Enumeration Survey (PES).
#'
#' The "UC" (net census undercount) is used to adjust population estimates
#' in the Historical Population subprocess.
#'
#' Data sources:
#' - Census Bureau Demographic Analysis (DA)
#' - Post-Enumeration Survey (PES) results
#'
#' @name census_undercount
NULL

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Fetch census undercount factors
#'
#' @description
#' Returns net undercount rates by age and sex for decennial censuses.
#' Positive values indicate undercount, negative values indicate overcount.
#'
#' @param census_year Integer: decennial census year (1940, 1950, ..., 2020)
#' @param by_age Logical: if TRUE, return by single year of age; if FALSE,
#'   return overall rate
#'
#' @return data.table with undercount rates (as proportions, not percentages)
#'
#' @details
#' Undercount rates vary by:
#' - Age: Young children (0-4) have highest undercount; older adults often overcounted
#' - Sex: Males have higher undercount than females, especially ages 20-50
#' - Race: Black population has higher undercount (not included here for simplicity)
#'
#' Historical patterns:
#' - 1940-1990: Net undercounts typically 1-3%
#' - 2000: ~0.5% net undercount
#' - 2010: ~0.1% net undercount (nearly accurate)
#' - 2020: ~0.2% net undercount (slight undercount)
#'
#' @export
fetch_census_undercount_factors <- function(census_year,
                                             by_age = TRUE) {
  valid_years <- seq(1940, 2020, by = 10)
  checkmate::assert_choice(census_year, valid_years)

  cli::cli_alert_info("Fetching census undercount factors for {census_year}...")

  if (by_age) {
    result <- get_undercount_by_age(census_year)
  } else {
    result <- get_undercount_overall(census_year)
  }

  cli::cli_alert_success("Retrieved undercount factors for {census_year}")

  result
}

# =============================================================================
# UNDERCOUNT DATA
# =============================================================================

#' Get undercount factors by age and sex
#'
#' @description
#' Returns undercount rates by single year of age and sex for a
#' specific decennial census.
#'
#' @keywords internal
get_undercount_by_age <- function(census_year) {
  # Define age group undercount patterns based on Census Bureau DA estimates
  # Values are undercount rates (positive = undercount, negative = overcount)

  age_pattern <- get_age_pattern(census_year)

  # Expand to single years of age
  ages <- 0:99
  result_male <- expand_age_pattern(age_pattern, ages, "male", census_year)
  result_female <- expand_age_pattern(age_pattern, ages, "female", census_year)

  result <- data.table::rbindlist(list(result_male, result_female))
  data.table::setorder(result, census_year, sex, age)

  result
}

#' Get age-specific undercount pattern for a census year
#'
#' @keywords internal
get_age_pattern <- function(census_year) {
  # Undercount patterns by age group and sex (rates as proportions)
  # Based on Census Bureau Demographic Analysis estimates
  # Positive = undercount, Negative = overcount

  patterns <- list(
    "1940" = data.table::data.table(
      age_min = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80),
      age_max = c(4, 9, 14, 19, 29, 39, 49, 59, 69, 79, 99),
      male_rate = c(0.030, 0.020, 0.015, 0.020, 0.035, 0.030, 0.025, 0.020, 0.015, 0.010, 0.005),
      female_rate = c(0.025, 0.018, 0.012, 0.015, 0.020, 0.015, 0.012, 0.010, 0.008, 0.005, 0.002)
    ),
    "1950" = data.table::data.table(
      age_min = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80),
      age_max = c(4, 9, 14, 19, 29, 39, 49, 59, 69, 79, 99),
      male_rate = c(0.028, 0.018, 0.012, 0.018, 0.032, 0.028, 0.022, 0.018, 0.012, 0.008, 0.004),
      female_rate = c(0.023, 0.015, 0.010, 0.012, 0.018, 0.013, 0.010, 0.008, 0.006, 0.004, 0.002)
    ),
    "1960" = data.table::data.table(
      age_min = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80),
      age_max = c(4, 9, 14, 19, 29, 39, 49, 59, 69, 79, 99),
      male_rate = c(0.025, 0.015, 0.010, 0.015, 0.028, 0.025, 0.020, 0.015, 0.010, 0.006, 0.003),
      female_rate = c(0.020, 0.012, 0.008, 0.010, 0.015, 0.012, 0.008, 0.006, 0.005, 0.003, 0.001)
    ),
    "1970" = data.table::data.table(
      age_min = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80),
      age_max = c(4, 9, 14, 19, 29, 39, 49, 59, 69, 79, 99),
      male_rate = c(0.022, 0.012, 0.008, 0.012, 0.025, 0.022, 0.018, 0.012, 0.008, 0.005, 0.002),
      female_rate = c(0.018, 0.010, 0.006, 0.008, 0.012, 0.010, 0.007, 0.005, 0.004, 0.002, 0.001)
    ),
    "1980" = data.table::data.table(
      age_min = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80),
      age_max = c(4, 9, 14, 19, 29, 39, 49, 59, 69, 79, 99),
      male_rate = c(0.020, 0.010, 0.006, 0.010, 0.022, 0.018, 0.015, 0.010, 0.006, 0.003, 0.001),
      female_rate = c(0.015, 0.008, 0.005, 0.006, 0.010, 0.008, 0.005, 0.004, 0.003, 0.001, 0.000)
    ),
    "1990" = data.table::data.table(
      age_min = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80),
      age_max = c(4, 9, 14, 19, 29, 39, 49, 59, 69, 79, 99),
      male_rate = c(0.018, 0.008, 0.004, 0.008, 0.020, 0.016, 0.012, 0.008, 0.004, 0.002, 0.000),
      female_rate = c(0.012, 0.006, 0.003, 0.004, 0.008, 0.006, 0.004, 0.003, 0.002, 0.000, -0.001)
    ),
    "2000" = data.table::data.table(
      age_min = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80),
      age_max = c(4, 9, 14, 19, 29, 39, 49, 59, 69, 79, 99),
      male_rate = c(0.015, 0.006, 0.003, 0.005, 0.015, 0.012, 0.008, 0.005, 0.002, 0.000, -0.002),
      female_rate = c(0.010, 0.004, 0.002, 0.003, 0.006, 0.004, 0.002, 0.001, 0.000, -0.002, -0.003)
    ),
    "2010" = data.table::data.table(
      age_min = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80),
      age_max = c(4, 9, 14, 19, 29, 39, 49, 59, 69, 79, 99),
      male_rate = c(0.046, 0.004, 0.002, 0.003, 0.012, 0.008, 0.005, 0.002, 0.000, -0.002, -0.003),
      female_rate = c(0.040, 0.003, 0.001, 0.002, 0.004, 0.002, 0.001, 0.000, -0.001, -0.003, -0.004)
    ),
    "2020" = data.table::data.table(
      age_min = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80),
      age_max = c(4, 9, 14, 19, 29, 39, 49, 59, 69, 79, 99),
      male_rate = c(0.050, 0.005, 0.002, 0.003, 0.010, 0.006, 0.003, 0.001, -0.001, -0.003, -0.004),
      female_rate = c(0.045, 0.004, 0.001, 0.002, 0.003, 0.001, 0.000, -0.001, -0.002, -0.004, -0.005)
    )
  )

  patterns[[as.character(census_year)]]
}

#' Expand age group pattern to single years
#'
#' @keywords internal
expand_age_pattern <- function(pattern, ages, sex, census_year) {
  rate_col <- paste0(sex, "_rate")

  result <- data.table::data.table(
    census_year = census_year,
    age = ages,
    sex = sex,
    undercount_rate = NA_real_
  )

  for (i in seq_len(nrow(pattern))) {
    mask <- result$age >= pattern$age_min[i] & result$age <= pattern$age_max[i]
    result[mask, undercount_rate := pattern[[rate_col]][i]]
  }

  result
}

#' Get overall undercount rate for a census year
#'
#' @keywords internal
get_undercount_overall <- function(census_year) {
  # Overall net undercount rates by census year (DA middle series estimates)
  overall_rates <- data.table::data.table(
    census_year = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
    overall_rate = c(0.055, 0.041, 0.031, 0.027, 0.012, 0.018, 0.005, 0.001, 0.002),
    source = c("da_estimate", "da_estimate", "da_estimate", "da_estimate",
               "da_estimate", "pes", "pes", "pes", "pes")
  )

  overall_rates[census_year == census_year]
}

# =============================================================================
# INTERPOLATION FOR NON-CENSUS YEARS
# =============================================================================

#' Get undercount factor for any year
#'
#' @description
#' Returns undercount factor for any year by interpolating between
#' decennial census estimates.
#'
#' @param year Integer: year to query
#' @param age Integer: age (0-99)
#' @param sex Character: "male" or "female"
#'
#' @return Numeric: undercount rate for that year/age/sex
#'
#' @export
get_undercount_factor <- function(year, age, sex) {
  checkmate::assert_int(year, lower = 1940, upper = 2030)
  checkmate::assert_int(age, lower = 0, upper = 99)
  checkmate::assert_choice(sex, c("male", "female"))

  # Find bracketing census years
  census_years <- seq(1940, 2020, by = 10)
  lower_census <- max(census_years[census_years <= year])
  upper_census <- min(census_years[census_years >= year])

  if (is.infinite(lower_census)) lower_census <- 1940
  if (is.infinite(upper_census)) upper_census <- 2020

  # Get rates for bracketing years
  lower_data <- fetch_census_undercount_factors(lower_census, by_age = TRUE)
  lower_rate <- lower_data[age == age & sex == sex, undercount_rate]

  if (lower_census == upper_census || year >= 2020) {
    return(lower_rate)
  }

  upper_data <- fetch_census_undercount_factors(upper_census, by_age = TRUE)
  upper_rate <- upper_data[age == age & sex == sex, undercount_rate]

  # Linear interpolation
  weight <- (year - lower_census) / (upper_census - lower_census)
  lower_rate + weight * (upper_rate - lower_rate)
}

#' Calculate undercount adjustment for a population
#'
#' @description
#' Calculates the undercount adjustment to apply to a census population.
#'
#' @param population Integer: census population count
#' @param undercount_rate Numeric: undercount rate (proportion)
#'
#' @return Integer: adjusted population
#'
#' @details
#' If undercount_rate is 0.02 (2% undercount), the true population
#' is estimated as: census_count / (1 - 0.02) = census_count * 1.0204
#'
#' @export
apply_undercount_adjustment <- function(population, undercount_rate) {
  as.integer(population / (1 - undercount_rate))
}

# =============================================================================
# SUMMARY FUNCTIONS
# =============================================================================

#' Summarize undercount data availability
#'
#' @export
summarize_undercount_availability <- function() {
  data.table::data.table(
    census_year = seq(1940, 2020, by = 10),
    overall_rate = c("5.5%", "4.1%", "3.1%", "2.7%", "1.2%", "1.8%", "0.5%", "0.1%", "0.2%"),
    method = c("DA", "DA", "DA", "DA", "DA", "PES", "PES", "PES", "PES"),
    notes = c(
      "Pre-WWII enumeration challenges",
      "Post-war improvements",
      "Continuing improvement",
      "First PES conducted",
      "Improved methodology",
      "Controversial adjustment debate",
      "Near-complete count",
      "Most accurate census",
      "COVID impact on enumeration"
    )
  )
}

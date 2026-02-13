#' O Immigration Static Data
#'
#' Loading functions for extracted CSV data used in the O (temporary/unlawfully
#' present) immigration subprocess. All data files live in data/processed/ with
#' companion _SOURCE.md provenance documents.
#'
#' @name o_immigration_static
NULL

# =============================================================================
# OVERSTAY PERCENTAGES
# =============================================================================

#' Load overstay percentages by age
#'
#' @description
#' Loads age-specific overstay percentages from CSV. These are used to split
#' the unauthorized population into never-authorized (N) and visa-overstayer (V)
#' types at the 2010 and 2015 anchor points.
#'
#' @return data.table with age_min, age_max, overstay_pct, source
#'
#' @section TR2025 Reference:
#' Input #25: "Internally developed overstay percentages by age."
#'
#' @export
load_overstay_percentages <- function() {
  csv_path <- here::here("data/processed/o_overstay_pct_by_age.csv")
  if (!file.exists(csv_path)) {
    cli::cli_abort(c(
      "Overstay percentages CSV not found at {.path {csv_path}}",
      "i" = "See data/processed/o_overstay_pct_by_age_SOURCE.md for provenance"
    ))
  }
  data.table::fread(csv_path)
}

#' Expand overstay percentages to single-year ages
#'
#' @param overstay_data data.table from load_overstay_percentages()
#'
#' @return data.table with age (0-100) and overstay_pct
#'
#' @keywords internal
expand_overstay_to_single_age <- function(overstay_data) {
  ages <- 0:100
  result <- data.table::data.table(age = ages)
  result[, overstay_pct := NA_real_]

  for (i in seq_len(nrow(overstay_data))) {
    row <- overstay_data[i]
    result[age >= row$age_min & age <= row$age_max,
           overstay_pct := row$overstay_pct]
  }

  # Fill any remaining NAs with last group's value
  result[is.na(overstay_pct), overstay_pct := overstay_data[.N, overstay_pct]]

  result
}

# =============================================================================
# NONIMMIGRANT AGE DISTRIBUTION
# =============================================================================

#' Load nonimmigrant age distribution
#'
#' @description
#' Loads age-group weights and sex skew factors for the nonimmigrant (type I)
#' component of the O population.
#'
#' @return data.table with age_min, age_max, ni_weight, male_skew, source
#'
#' @export
load_nonimmigrant_age_distribution <- function() {
  csv_path <- here::here("data/processed/o_nonimmigrant_age_distribution.csv")
  if (!file.exists(csv_path)) {
    cli::cli_abort(c(
      "Nonimmigrant age distribution CSV not found at {.path {csv_path}}",
      "i" = "See data/processed/o_nonimmigrant_age_distribution_SOURCE.md for provenance"
    ))
  }
  data.table::fread(csv_path)
}

#' Expand nonimmigrant distribution to single-year ages by sex
#'
#' @param ni_data data.table from load_nonimmigrant_age_distribution()
#'
#' @return data.table with age, sex, ni_age_pct (normalized to sum to 1)
#'
#' @keywords internal
expand_ni_to_single_age <- function(ni_data) {
  ages <- 0:100
  sexes <- c("male", "female")
  result <- data.table::CJ(age = ages, sex = sexes)

  # Assign weights by age group
  result[, ni_weight := NA_real_]
  result[, male_skew := NA_real_]

  for (i in seq_len(nrow(ni_data))) {
    row <- ni_data[i]
    result[age >= row$age_min & age <= row$age_max,
           `:=`(ni_weight = row$ni_weight, male_skew = row$male_skew)]
  }

  # Fill any NAs
  result[is.na(ni_weight), ni_weight := 0.01]
  result[is.na(male_skew), male_skew := 1.0]

  # Apply sex skew
  result[sex == "male", ni_age_pct := ni_weight * male_skew]
  result[sex == "female", ni_age_pct := ni_weight * (2 - male_skew)]

  # Normalize to sum to 1
  result[, ni_age_pct := ni_age_pct / sum(ni_age_pct)]

  result[, .(age, sex, ni_age_pct)]
}

# =============================================================================
# DHS ANCHOR POINTS
# =============================================================================

#' Load DHS anchor point totals
#'
#' @description
#' Loads DHS unauthorized and nonimmigrant stock totals for the 2010 and 2015
#' type interpolation anchor points.
#'
#' @return data.table with year, total_unauthorized, total_nonimmigrant,
#'   overstay_pct_overall, source
#'
#' @export
load_dhs_anchor_points <- function() {
  csv_path <- here::here("data/processed/o_dhs_anchor_points.csv")
  if (!file.exists(csv_path)) {
    cli::cli_abort(c(
      "DHS anchor points CSV not found at {.path {csv_path}}",
      "i" = "See data/processed/o_dhs_anchor_points_SOURCE.md for provenance"
    ))
  }
  data.table::fread(csv_path)
}

# =============================================================================
# BASE DEPARTURE RATES
# =============================================================================

#' Load base departure rates
#'
#' @description
#' Loads simplified base departure rates by age group with type and sex
#' multipliers. Used when the detailed 2008-2010 stock build-up is unavailable.
#'
#' @return data.table with age_min, age_max, base_rate, n_multiplier,
#'   i_multiplier, v_multiplier, male_multiplier, source
#'
#' @export
load_base_departure_rates <- function() {
  csv_path <- here::here("data/processed/o_base_departure_rates.csv")
  if (!file.exists(csv_path)) {
    cli::cli_abort(c(
      "Base departure rates CSV not found at {.path {csv_path}}",
      "i" = "See data/processed/o_base_departure_rates_SOURCE.md for provenance"
    ))
  }
  data.table::fread(csv_path)
}

#' Expand base departure rates to single-year ages by sex and type
#'
#' @param rate_data data.table from load_base_departure_rates()
#'
#' @return data.table with age, sex, type, base_rate
#'
#' @keywords internal
expand_departure_rates <- function(rate_data) {
  ages <- 0:99
  sexes <- c("male", "female")
  types <- c("N", "I", "V")

  result <- data.table::CJ(age = ages, sex = sexes, type = types)

  # Assign base rate by age group
  result[, base_rate := NA_real_]
  for (i in seq_len(nrow(rate_data))) {
    row <- rate_data[i]
    result[age >= row$age_min & age <= row$age_max, base_rate := row$base_rate]
  }
  result[is.na(base_rate), base_rate := rate_data[.N, base_rate]]

  # Look up multipliers for each age (use first matching row)
  result[, row_idx := NA_integer_]
  for (i in seq_len(nrow(rate_data))) {
    row <- rate_data[i]
    result[age >= row$age_min & age <= row$age_max & is.na(row_idx),
           row_idx := i]
  }
  result[is.na(row_idx), row_idx := nrow(rate_data)]

  # Apply type multipliers
  result[type == "N", base_rate := base_rate * rate_data$n_multiplier[row_idx]]
  result[type == "I", base_rate := base_rate * rate_data$i_multiplier[row_idx]]
  result[type == "V", base_rate := base_rate * rate_data$v_multiplier[row_idx]]

  # Apply sex multiplier
  result[sex == "male", base_rate := base_rate * rate_data$male_multiplier[row_idx]]

  result[, row_idx := NULL]
  result
}

# =============================================================================
# DACA HISTORICAL STOCK
# =============================================================================

#' Load DACA historical stock
#'
#' @description
#' Loads annual DACA active recipient totals (2013-2022) from USCIS data.
#'
#' @return data.table with year, total_active, source
#'
#' @export
load_daca_historical_stock <- function() {
  csv_path <- here::here("data/processed/daca_historical_stock.csv")
  if (!file.exists(csv_path)) {
    cli::cli_abort(c(
      "DACA historical stock CSV not found at {.path {csv_path}}",
      "i" = "See data/processed/daca_historical_stock_SOURCE.md for provenance"
    ))
  }
  data.table::fread(csv_path)
}

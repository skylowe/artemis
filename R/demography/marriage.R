#' Marriage Subprocess Functions
#'
#' Functions for projecting annual age-specific marriage rates by husband age ×
#' wife age. Implements TR2025 Section 1.6 Marriage methodology.
#'
#' Key outputs:
#' - m̂_{x,y}^z: Age-specific marriage rates (Eq 1.6.1)
#' - AMR^z: Age-adjusted central marriage rate (Eq 1.6.2)
#' - MarGrid: 87×87 matrix of marriage rates (ages 14-100+)
#'
#' @name marriage
NULL

# =============================================================================
# CONSTANTS
# =============================================================================
#' Age range for MarGrid (14-100+)
#' @keywords internal
MARGRID_MIN_AGE <- 14
MARGRID_MAX_AGE <- 100

#' MarGrid dimensions (87 single-year ages: 14-100)
#' @keywords internal
MARGRID_SIZE <- MARGRID_MAX_AGE - MARGRID_MIN_AGE + 1

#' TR2025 age groups used in NCHS data
#' @keywords internal
MARRIAGE_AGE_GROUPS <- list(
  "12-17" = c(12, 17),
  "18-19" = c(18, 19),
  "20-24" = c(20, 24),
  "25-29" = c(25, 29),

  "30-34" = c(30, 34),
  "35-44" = c(35, 44),
  "45-54" = c(45, 54),
  "55-64" = c(55, 64),
  "65+"   = c(65, 100)
)

# =============================================================================
# SS AREA FACTOR CALCULATION
# =============================================================================

#' Calculate SS Area Adjustment Factor
#'
#' @description
#' Calculates the Social Security area adjustment factor for converting U.
#' marriage totals to SS area totals. The factor is calculated as:
#'   SS Area Factor = SS Area Population / U.S. Resident Population
#'
#' Per TR2025 documentation: "This estimate is obtained by increasing the number
#' of marriages reported in the U.S. to reflect the difference between the
#' Social Security area population and the U.S. population."
#'
#' @param year Integer: year for which to calculate factor
#' @param cache_dir Character: directory containing historical population cache
#'
#' @return Numeric: SS area adjustment factor (typically ~1.02)
#'
#' @details
#' The SS Area includes:
#' - U.S. resident population
#' - Armed Forces overseas
#' - Territory residents (PR, VI, Guam, CNMI, AS)
#' - Federal civilian employees overseas
#' - Dependents of armed forces and federal employees overseas
#' - OASDI beneficiaries living abroad
#' - Other U.S. citizens overseas
#'
#' The factor ranges from ~1.019 (2022) to ~1.027 (1989) as the overseas
#' and territory populations have changed relative to the U.S. total.
#'
#' @export
get_ss_area_factor <- function(target_year, cache_dir = here::here("data/cache")) {
  # Try to load cached historical population components
  cache_file <- file.path(cache_dir, "historical_population", "ss_population_1940_2022.rds")

  if (!file.exists(cache_file)) {
    cli::cli_warn("Historical population cache not found at {cache_file}. Using fallback factor 1.02.")
    return(1.02)
  }

  pop_data <- readRDS(cache_file)

  if (!"components" %in% names(pop_data)) {
    cli::cli_warn("Historical population cache missing components. Using fallback factor 1.02.")
    return(1.02)
  }

  components <- data.table::as.data.table(pop_data$components)

  # Check if year is in range
  available_years <- components$year
  min_year <- min(available_years)
  max_year <- max(available_years)

  if (target_year < min_year) {
    # Use earliest available factor
    factor_val <- components[year == min_year, total / census_usaf]
    cli::cli_alert_info("Year {target_year} before historical data; using {min_year} factor: {round(factor_val, 5)}")
    return(factor_val)
  }

  if (target_year > max_year) {
    # Use latest available factor for projection years
    factor_val <- components[year == max_year, total / census_usaf]
    return(factor_val)
  }

  # Return factor for requested year
  factor_val <- components[year == target_year, total / census_usaf]

  if (length(factor_val) == 0 || is.na(factor_val)) {
    cli::cli_warn("No data for year {target_year}. Using fallback factor 1.02.")
    return(1.02)
  }

  factor_val
}

#' Get SS Area Factors for Multiple Years
#'
#' @description
#' Retrieves SS area adjustment factors for a range of years, using cached
#' historical population data.
#'
#' @param years Integer vector: years for which to get factors
#' @param cache_dir Character: directory containing historical population cache
#'
#' @return Named numeric vector with SS area factors keyed by year
#'
#' @export
get_ss_area_factors <- function(years, cache_dir = here::here("data/cache")) {
  # Load cache once for efficiency
  cache_file <- file.path(cache_dir, "historical_population", "ss_population_1940_2022.rds")

  if (!file.exists(cache_file)) {
    cli::cli_warn("Historical population cache not found. Using fallback factor 1.02 for all years.")
    factors <- rep(1.02, length(years))
    names(factors) <- as.character(years)
    return(factors)
  }

  pop_data <- readRDS(cache_file)

  if (!"components" %in% names(pop_data)) {
    cli::cli_warn("Historical population cache missing components. Using fallback factor 1.02.")
    factors <- rep(1.02, length(years))
    names(factors) <- as.character(years)
    return(factors)
  }

  components <- data.table::as.data.table(pop_data$components)
  components[, ss_area_factor := total / census_usaf]

  min_year <- min(components$year)
  max_year <- max(components$year)

  factors <- sapply(years, function(yr) {
    if (yr < min_year) {
      return(components[year == min_year, ss_area_factor])
    } else if (yr > max_year) {
      return(components[year == max_year, ss_area_factor])
    } else {
      f <- components[year == yr, ss_area_factor]
      if (length(f) == 0 || is.na(f)) return(1.02)
      return(f)
    }
  })

  names(factors) <- as.character(years)
  factors
}

# =============================================================================
# MARRIAGE RATE CALCULATION (6C.1)
# =============================================================================

#' Calculate marriage rates from marriages and unmarried population
#'
#' @description
#' Calculates age-specific marriage rates using the formula:
#' m̂_{x,y}^z = M_{x,y}^z / P_{x,y}^z
#' where P_{x,y}^z is the geometric mean of unmarried male and female populations.
#'
#' @param marriages data.table with marriage counts by husband/wife age group
#'   Required columns: year, husband_age_group, wife_age_group, marriages
#' @param unmarried_pop data.table with unmarried population by age group and sex
#'   Required columns: year, age_group, sex, unmarried_population
#'
#' @return data.table with columns: year, husband_age_group, wife_age_group, rate
#'
#' @export
calculate_marriage_rates <- function(marriages, unmarried_pop) {
  checkmate::assert_data_table(marriages)
  checkmate::assert_data_table(unmarried_pop)
  checkmate::assert_names(names(marriages),
                          must.include = c("year", "husband_age_group", "wife_age_group", "marriages"))
  checkmate::assert_names(names(unmarried_pop),
                          must.include = c("year", "age_group", "sex", "unmarried_population"))

  # Separate male and female unmarried populations
  male_pop <- unmarried_pop[sex == "male", .(year, age_group, male_unmarried = unmarried_population)]
  female_pop <- unmarried_pop[sex == "female", .(year, age_group, female_unmarried = unmarried_population)]

  # Join marriages with male population (husband age)
  result <- data.table::copy(marriages)
  result <- merge(result, male_pop,
                  by.x = c("year", "husband_age_group"),
                  by.y = c("year", "age_group"),
                  all.x = TRUE)

  # Join with female population (wife age)
  result <- merge(result, female_pop,
                  by.x = c("year", "wife_age_group"),
                  by.y = c("year", "age_group"),
                  all.x = TRUE)

  # Calculate geometric mean of unmarried populations
  # P_{x,y}^z = sqrt(male_x × female_y)
  result[, geometric_mean := sqrt(male_unmarried * female_unmarried)]

  # Calculate marriage rate (per 100,000)
  result[, rate := (marriages / geometric_mean) * 100000]

  # Handle cases where population is 0 or NA

  result[is.na(rate) | is.infinite(rate), rate := 0]

  # Select and order output columns
  result <- result[, .(year, husband_age_group, wife_age_group, marriages,
                       geometric_mean, rate)]
  data.table::setorder(result, year, husband_age_group, wife_age_group)

  result
}

#' Calculate marriage rates for a single year
#'
#' @param marriages_year data.table of marriages for one year
#' @param unmarried_pop_year data.table of unmarried population for one year
#'
#' @return data.table with rates
#' @keywords internal
calculate_marriage_rates_year <- function(marriages_year, unmarried_pop_year) {
  # Separate male and female

male_pop <- unmarried_pop_year[sex == "male"]
  female_pop <- unmarried_pop_year[sex == "female"]

  result <- data.table::copy(marriages_year)

  # Join populations
  result <- merge(result, male_pop[, .(age_group, male_unmarried = unmarried_population)],
                  by.x = "husband_age_group", by.y = "age_group", all.x = TRUE)
  result <- merge(result, female_pop[, .(age_group, female_unmarried = unmarried_population)],
                  by.x = "wife_age_group", by.y = "age_group", all.x = TRUE)

  # Calculate rate
  result[, geometric_mean := sqrt(male_unmarried * female_unmarried)]
  result[, rate := (marriages / geometric_mean) * 100000]
  result[is.na(rate) | is.infinite(rate), rate := 0]

  result
}

# =============================================================================
# AGE GROUP TO SINGLE YEAR INTERPOLATION (H.S. Beers Method)
# =============================================================================

#' Map age group label to age range
#'
#' @param age_group Character age group label (e.g., "20-24", "65+")
#' @return Named list with min and max ages
#' @keywords internal
get_age_range <- function(age_group) {
  if (age_group %in% names(MARRIAGE_AGE_GROUPS)) {
    range <- MARRIAGE_AGE_GROUPS[[age_group]]
    return(list(min = range[1], max = range[2]))
  }
  # Try to parse directly
  if (grepl("\\+$", age_group)) {
    min_age <- as.integer(gsub("\\+$", "", age_group))
    return(list(min = min_age, max = 100))
  }
  parts <- as.integer(strsplit(age_group, "-")[[1]])
  list(min = parts[1], max = parts[2])
}

#' Marriage age group definitions for MarGrid (8-category)
#'
#' @description
#' The 8 age groups used in the marriage grid, with min, max, and width.
#' Used for Beers interpolation pre-processing, standard population grid,
#' age group aggregation, and MarGrid adjustment. Defined once here and
#' referenced throughout the file.
#'
#' @keywords internal
MARGRID_AGE_GROUPS <- list(
  "14-19" = list(min = 14, max = 19, width = 6),
  "20-24" = list(min = 20, max = 24, width = 5),
  "25-29" = list(min = 25, max = 29, width = 5),
  "30-34" = list(min = 30, max = 34, width = 5),
  "35-44" = list(min = 35, max = 44, width = 10),
  "45-54" = list(min = 45, max = 54, width = 10),
  "55-64" = list(min = 55, max = 64, width = 10),
  "65+"   = list(min = 65, max = 100, width = 36)
)

#' Apply correct H.S. Beers interpolation to standard 5-year pseudo-groups
#'
#' @description
#' Applies H.S. Beers ordinary interpolation to a vector of standard 5-year
#' group totals. Uses the canonical coefficients from
#' `historical_marital_status.R:get_beers_coefficients()`.
#'
#' @param group_totals Numeric vector of 5-year group totals (>= 6 groups)
#'
#' @return Numeric vector of single-year values (length = n_groups * 5)
#' @keywords internal
apply_beers_standard <- function(group_totals) {
  n_groups <- length(group_totals)

  if (n_groups < 6) {
    cli::cli_abort("Beers interpolation requires at least 6 groups, got {n_groups}")
  }

  coef <- get_beers_coefficients()
  n_single <- n_groups * 5
  result <- numeric(n_single)

  # Opening coefficients: first 10 single years from first 6 groups
  for (i in 1:10) {
    result[i] <- sum(coef$opening[i, ] * group_totals[1:6])
  }

  # Interior coefficients: single years 11 to (n_single - 10)
  # Each application produces 5 single years from a sliding window of 6 groups
  for (j in seq(11, n_single - 10, by = 5)) {
    # Determine which group center we're in (1-indexed)
    g <- (j - 1) %/% 5 + 1

    # Window of 6 groups, clamped to valid range
    g1 <- max(1, g - 2)
    g2 <- min(n_groups, g1 + 5)
    g1 <- g2 - 5  # Ensure exactly 6 groups

    for (k in 0:4) {
      result[j + k] <- sum(coef$interior[k + 1, ] * group_totals[g1:g2])
    }
  }

  # Closing coefficients: last 10 single years from last 6 groups
  for (i in 1:10) {
    result[n_single - 10 + i] <- sum(coef$closing[i, ] * group_totals[(n_groups - 5):n_groups])
  }

  result
}

#' Pre-process marriage age groups into standard 5-year pseudo-groups
#'
#' @description
#' Converts the 8 non-standard marriage age groups into ~17 standard 5-year
#' pseudo-groups suitable for Beers interpolation. Non-standard groups are
#' split via uniform allocation:
#' - 14-19 (6yr) -> 15-19 (5yr), with age 14 handled separately
#' - 35-44 (10yr) -> 35-39, 40-44
#' - 45-54 (10yr) -> 45-49, 50-54
#' - 55-64 (10yr) -> 55-59, 60-64
#' - 65+ (36yr) -> 65-69, 70-74, ..., 95-99 (7 sub-groups)
#'
#' @param group_totals Named numeric vector of group totals (rate * width)
#'   Names must match MARGRID_AGE_GROUPS (e.g., "14-19", "20-24", ...)
#'
#' @return List with:
#'   - pseudo_totals: numeric vector of 17 pseudo-group totals
#'   - pseudo_labels: character vector of pseudo-group labels
#'   - age14_value: estimated value for age 14 (1/6 of 14-19 total)
#' @keywords internal
preprocess_marriage_to_5year <- function(group_totals) {
  # Expected 8 groups in order
  expected <- names(MARGRID_AGE_GROUPS)
  if (is.null(names(group_totals))) {
    names(group_totals) <- expected
  }

  pseudo_totals <- numeric(0)
  pseudo_labels <- character(0)

  # Age 14 gets 1/6 of the 14-19 total (handled separately after Beers)
  total_14_19 <- group_totals["14-19"]
  age14_value <- total_14_19 / 6

  # 15-19: remaining 5/6 of 14-19 total
  pseudo_totals <- c(pseudo_totals, total_14_19 * 5 / 6)
  pseudo_labels <- c(pseudo_labels, "15-19")

  # 20-24, 25-29, 30-34: already 5-year groups, pass through
  for (g in c("20-24", "25-29", "30-34")) {
    pseudo_totals <- c(pseudo_totals, group_totals[g])
    pseudo_labels <- c(pseudo_labels, g)
  }

  # 35-44: split into 35-39 and 40-44 (uniform)
  total_35_44 <- group_totals["35-44"]
  pseudo_totals <- c(pseudo_totals, total_35_44 / 2, total_35_44 / 2)
  pseudo_labels <- c(pseudo_labels, "35-39", "40-44")

  # 45-54: split into 45-49 and 50-54
  total_45_54 <- group_totals["45-54"]
  pseudo_totals <- c(pseudo_totals, total_45_54 / 2, total_45_54 / 2)
  pseudo_labels <- c(pseudo_labels, "45-49", "50-54")

  # 55-64: split into 55-59 and 60-64
  total_55_64 <- group_totals["55-64"]
  pseudo_totals <- c(pseudo_totals, total_55_64 / 2, total_55_64 / 2)
  pseudo_labels <- c(pseudo_labels, "55-59", "60-64")

  # 65+: split into 7 sub-groups of 5 years each (65-69 through 95-99)
  # Original width = 36 (ages 65-100), 7 sub-groups of 5 = 35 years
  # Age 100 is handled separately after Beers
  total_65_plus <- group_totals["65+"]
  n_sub <- 7
  sub_total <- total_65_plus * 5 / 36  # Each sub-group gets 5/36 of total
  for (start in seq(65, 95, by = 5)) {
    pseudo_totals <- c(pseudo_totals, sub_total)
    pseudo_labels <- c(pseudo_labels, paste0(start, "-", start + 4))
  }

  names(pseudo_totals) <- pseudo_labels

  list(
    pseudo_totals = pseudo_totals,
    pseudo_labels = pseudo_labels,
    age14_value = age14_value,
    age100_share = total_65_plus / 36  # 1/36 of 65+ total for age 100
  )
}

#' Apply 1D Beers interpolation to marriage age groups
#'
#' @description
#' Converts 8 non-standard marriage age groups to 87 single-year values
#' (ages 14-100) using H.S. Beers ordinary interpolation. Pre-processes
#' groups into standard 5-year pseudo-groups, applies Beers, extrapolates
#' boundary ages, and re-normalizes to preserve original group totals.
#'
#' @param group_rates Numeric vector of 8 group-level rates
#' @param group_names Character vector of group names (default: MARGRID_AGE_GROUPS names)
#'
#' @return Named numeric vector of 87 single-year values (ages 14-100)
#' @keywords internal
beers_interpolate_marriage_1d <- function(group_rates, group_names = NULL) {
  if (is.null(group_names)) {
    group_names <- names(MARGRID_AGE_GROUPS)
  }
  names(group_rates) <- group_names

  # Get group widths
  group_widths <- sapply(MARGRID_AGE_GROUPS, function(g) g$width)

  # Convert rates to group totals (rate * width)
  group_totals <- group_rates * group_widths

  # Pre-process into 17 standard 5-year pseudo-groups

  pp <- preprocess_marriage_to_5year(group_totals)

  # Apply Beers to 17 pseudo-groups -> 85 single-year values (ages 15-99)
  beers_result <- apply_beers_standard(pp$pseudo_totals)

  # Enforce non-negativity
  beers_result <- pmax(beers_result, 0)

  # Build full 87-value result (ages 14-100)
  result <- numeric(87)
  names(result) <- as.character(14:100)

  # Ages 15-99 from Beers (85 values)
  result[as.character(15:99)] <- beers_result

  # Age 14: linear extrapolation from ages 15-16
  if (beers_result[1] > 0 && beers_result[2] > 0) {
    result["14"] <- max(0, 2 * beers_result[1] - beers_result[2])
  } else {
    result["14"] <- beers_result[1]  # Same as age 15 if extrapolation fails
  }

  # Age 100: extrapolation from ages 98-99
  n_beers <- length(beers_result)
  if (beers_result[n_beers] > 0) {
    result["100"] <- max(0, 2 * beers_result[n_beers] - beers_result[n_beers - 1])
  } else {
    result["100"] <- 0
  }

  # Re-normalize within each original group to preserve group totals
  for (grp_name in group_names) {
    grp <- MARGRID_AGE_GROUPS[[grp_name]]
    ages <- grp$min:grp$max
    age_chars <- as.character(ages)
    current_sum <- sum(result[age_chars])
    target_sum <- group_totals[grp_name]

    if (current_sum > 0 && target_sum > 0) {
      result[age_chars] <- result[age_chars] * (target_sum / current_sum)
    } else if (target_sum == 0) {
      result[age_chars] <- 0
    }
  }

  # Enforce non-negativity after re-normalization
  result <- pmax(result, 0)

  result
}

#' Apply 2D Beers interpolation to age group marriage rates
#'
#' @description
#' Converts marriage rates by age group to single-year ages using
#' two-dimensional H.S. Beers interpolation per TR2025 Section 1.6.a:
#' "The age-of-husband crossed with age-of-wife marriage grid rates are
#' transformed from age grouped numbers to single year of age figures
#' from ages 14 to 100+ for both husband and wife using the two-dimensional
#' H.S. Beers method of interpolation."
#'
#' The method applies Beers interpolation first along husband ages (rows),
#' then along wife ages (columns).
#'
#' @param rates_by_group data.table with husband_age_group, wife_age_group, rate
#' @param min_age Minimum output age (default: 14)
#' @param max_age Maximum output age (default: 100)
#'
#' @return data.table with husband_age, wife_age, rate (single years)
#'
#' @export
beers_interpolate_2d <- function(rates_by_group, min_age = 14, max_age = 100) {
  group_names <- names(MARGRID_AGE_GROUPS)

  # Create 8x8 rate matrix by age groups
  rate_matrix <- data.table::dcast(rates_by_group,
                                    husband_age_group ~ wife_age_group,
                                    value.var = "rate",
                                    fill = 0)
  h_group_order <- rate_matrix$husband_age_group
  rate_matrix[, husband_age_group := NULL]
  rate_mat <- as.matrix(rate_matrix)
  rownames(rate_mat) <- h_group_order

  # Ensure column order matches MARGRID_AGE_GROUPS
  w_groups_present <- intersect(group_names, colnames(rate_mat))
  rate_mat <- rate_mat[intersect(group_names, rownames(rate_mat)),
                       w_groups_present, drop = FALSE]

  n_groups <- nrow(rate_mat)
  all_ages <- min_age:max_age
  n_ages <- length(all_ages)

  # =========================================================================
  # FIRST PASS: Interpolate husband dimension (rows) for each wife group
  # =========================================================================
  # Result: 87 husband ages x 8 wife groups
  intermediate <- matrix(0, nrow = n_ages, ncol = ncol(rate_mat))
  rownames(intermediate) <- all_ages
  colnames(intermediate) <- colnames(rate_mat)

  for (col in seq_len(ncol(rate_mat))) {
    # Get column of group rates (one rate per husband age group)
    col_rates <- rate_mat[, col]
    # Apply 1D Beers to expand from 8 husband groups to 87 husband ages
    intermediate[, col] <- beers_interpolate_marriage_1d(col_rates, group_names)
  }

  # =========================================================================
  # SECOND PASS: Interpolate wife dimension (columns) for each husband age
  # =========================================================================
  # Result: 87 x 87
  single_year_mat <- matrix(0, nrow = n_ages, ncol = n_ages)
  rownames(single_year_mat) <- all_ages
  colnames(single_year_mat) <- all_ages

  for (row in seq_len(n_ages)) {
    # Get row of rates (one rate per wife age group)
    row_rates <- intermediate[row, ]
    # Apply 1D Beers to expand from 8 wife groups to 87 wife ages
    single_year_mat[row, ] <- beers_interpolate_marriage_1d(row_rates, group_names)
  }

  # Enforce non-negativity
  single_year_mat[single_year_mat < 0] <- 0

  # Convert back to data.table
  result <- data.table::as.data.table(single_year_mat, keep.rownames = "husband_age")
  result <- data.table::melt(result, id.vars = "husband_age",
                              variable.name = "wife_age", value.name = "rate")
  result[, husband_age := as.integer(husband_age)]
  result[, wife_age := as.integer(as.character(wife_age))]

  data.table::setorder(result, husband_age, wife_age)
  result
}

#' Expand age group rates to single-year ages (wrapper)
#'
#' @description
#' Converts age-group marriage rates to single-year rates using
#' 2D interpolation method per TR2025.
#'
#' @param rates_by_group data.table with husband_age_group, wife_age_group, rate
#' @param min_age Minimum age for output (default: 14)
#' @param max_age Maximum age for output (default: 100)
#' @param method Interpolation method: "beers" (TR2025, default) or "uniform" (legacy)
#'
#' @return data.table with husband_age, wife_age, rate
#' @keywords internal
expand_age_groups <- function(rates_by_group, min_age = 14, max_age = 100,
                               method = "beers") {
  if (method == "uniform") {
    cli::cli_alert_info("Using UNIFORM age group expansion (legacy method)")
    return(expand_age_groups_uniform(rates_by_group, min_age, max_age))
  }

  cli::cli_alert_info("Using H.S. Beers 2D interpolation (TR2025 method)")
  beers_interpolate_2d(rates_by_group, min_age, max_age)
}

#' Expand age group rates using uniform distribution (fallback)
#' @keywords internal
expand_age_groups_uniform <- function(rates_by_group, min_age = 14, max_age = 100) {
  # Create all single-year combinations
  all_ages <- min_age:max_age
  grid <- data.table::CJ(husband_age = all_ages, wife_age = all_ages)

  map_to_group <- function(age) {
    for (grp_name in names(MARGRID_AGE_GROUPS)) {
      info <- MARGRID_AGE_GROUPS[[grp_name]]
      if (age >= info$min && age <= info$max) return(grp_name)
    }
    NA_character_
  }

  grid[, husband_age_group := sapply(husband_age, map_to_group)]
  grid[, wife_age_group := sapply(wife_age, map_to_group)]

  result <- merge(grid, rates_by_group[, .(husband_age_group, wife_age_group, rate)],
                  by = c("husband_age_group", "wife_age_group"), all.x = TRUE)
  result[is.na(rate), rate := 0]

  result[, .(husband_age, wife_age, rate)]
}

# =============================================================================
# MARGRID BUILDING (6C.3)
# =============================================================================

#' Build base MarGrid from historical marriage data
#'
#' @description
#' Creates the 87×87 MarGrid matrix from NCHS marriage data and CPS unmarried
#' population. Averages rates across specified years and optionally applies
#' smoothing.
#'
#' Steps:
#' 1. Calculate marriage rates for each year
#' 2. Average rates across years
#' 3. Expand age groups to single years
#' 4. Apply 2D Whittaker-Henderson smoothing (optional)
#'
#' @param nchs_marriages data.table of NCHS marriages by age group
#'   From fetch_nchs_mra_marriages_1978_1988()
#' @param cps_unmarried data.table of CPS unmarried population by age group
#'   From fetch_cps_unmarried_population()
#' @param years Integer vector of years to average (default: 1978:1988)
#' @param smooth Logical: apply Whittaker-Henderson smoothing (default: TRUE)
#' @param smooth_params List with h_param and w_param for smoothing
#'
#' @return List with:
#'   - margrid: 87×87 matrix of marriage rates
#'   - rates_by_group: Averaged rates by age group
#'   - rates_by_year: Rates for each year
#'
#' @export
build_base_margrid <- function(nchs_marriages,
                                cps_unmarried,
                                years = 1978:1988,
                                smooth = TRUE,
                                smooth_params = list(h_param = 1, w_param = 1)) {
  checkmate::assert_data_table(nchs_marriages)
  checkmate::assert_data_table(cps_unmarried)
  checkmate::assert_integerish(years, min.len = 1)

  cli::cli_h2("Building Base MarGrid")

  # Filter to requested years
  marriages <- nchs_marriages[year %in% years]
  unmarried <- cps_unmarried[year %in% years]

  cli::cli_alert_info("Using {length(years)} years: {min(years)}-{max(years)}")

  # Need to align age groups between NCHS (9 groups) and CPS (8 groups)
  # NCHS: 12-17, 18-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 65+
  # CPS:  14-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 65+

  # Combine NCHS 12-17 and 18-19 to match CPS 14-19
  marriages_aligned <- align_nchs_to_cps_age_groups(marriages)

  # Calculate rates for each year
  cli::cli_alert("Calculating marriage rates by year...")
  rates_by_year <- calculate_marriage_rates(marriages_aligned, unmarried)

  n_rates <- nrow(rates_by_year)
  cli::cli_alert_success("Calculated {n_rates} rate observations")

  # Average rates across years
  cli::cli_alert("Averaging rates across years...")
  avg_rates <- rates_by_year[, .(
    avg_marriages = mean(marriages, na.rm = TRUE),
    avg_geometric_mean = mean(geometric_mean, na.rm = TRUE),
    avg_rate = mean(rate, na.rm = TRUE)
  ), by = .(husband_age_group, wife_age_group)]


  # Expand to single-year ages using 2D Beers interpolation per TR2025
  cli::cli_alert("Expanding to single-year ages (2D Beers interpolation)...")
  single_year_rates <- expand_age_groups(
    avg_rates[, .(husband_age_group, wife_age_group, rate = avg_rate)],
    min_age = MARGRID_MIN_AGE,
    max_age = MARGRID_MAX_AGE,
    method = "beers"
  )

  # Convert to matrix
  margrid <- dcast_to_matrix(single_year_rates)

  # Apply smoothing if requested
  if (smooth) {
    cli::cli_alert("Applying 2D Whittaker-Henderson smoothing...")
    margrid <- whittaker_henderson_2d(margrid,
                                       h_param = smooth_params$h_param,
                                       w_param = smooth_params$w_param)
  }

  cli::cli_alert_success("Built MarGrid: {nrow(margrid)} × {ncol(margrid)}")

  list(
    margrid = margrid,
    rates_by_group = avg_rates,
    rates_by_year = rates_by_year
  )
}

#' Align NCHS age groups to CPS age groups
#'
#' @description
#' NCHS uses 9 age groups (12-17, 18-19, ...) while CPS uses 8 (14-19, ...).
#' This function combines NCHS 12-17 and 18-19 into 14-19 to match CPS.
#'
#' @param nchs_marriages data.table with NCHS marriage data
#' @return data.table with aligned age groups
#' @keywords internal
align_nchs_to_cps_age_groups <- function(nchs_marriages) {
  dt <- data.table::copy(nchs_marriages)

  # Map NCHS groups to CPS groups
  group_map <- c(
    "12-17" = "14-19",
    "18-19" = "14-19",
    "20-24" = "20-24",
    "25-29" = "25-29",
    "30-34" = "30-34",
    "35-44" = "35-44",
    "45-54" = "45-54",
    "55-64" = "55-64",
    "65+"   = "65+"
  )

  dt[, husband_age_group := group_map[husband_age_group]]
  dt[, wife_age_group := group_map[wife_age_group]]

  # Aggregate combined groups
  dt <- dt[, .(marriages = sum(marriages, na.rm = TRUE)),
           by = .(year, husband_age_group, wife_age_group)]

  dt
}

#' Convert rate data.table to matrix
#'
#' @param rates data.table with husband_age, wife_age, rate columns
#' @return Matrix with row names = husband ages, col names = wife ages
#' @keywords internal
dcast_to_matrix <- function(rates) {
  wide <- data.table::dcast(rates, husband_age ~ wife_age, value.var = "rate")
  row_names <- wide$husband_age
  wide[, husband_age := NULL]
  mat <- as.matrix(wide)
  rownames(mat) <- row_names
  mat
}

# =============================================================================
# WHITTAKER-HENDERSON SMOOTHING (6C.2)
# =============================================================================

#' Apply two-dimensional Whittaker-Henderson graduation
#'
#' @description
#' Smooths a 2D matrix of rates using the Whittaker-Henderson method.
#' This minimizes:
#'   Σ (fitted - observed)² + λ₁ Σ Δ²(rows) + λ₂ Σ Δ²(cols)
#'
#' For initial implementation, uses iterative row-then-column smoothing.
#'
#' @param grid Matrix of rates (husband age × wife age)
#' @param h_param Smoothing parameter for husband (row) dimension (default: 1)
#' @param w_param Smoothing parameter for wife (column) dimension (default: 1)
#' @param max_iter Maximum iterations (default: 100)
#' @param tol Convergence tolerance (default: 1e-6)
#'
#' @return Smoothed matrix of same dimensions
#'
#' @export
whittaker_henderson_2d <- function(grid,
                                    h_param = 1,
                                    w_param = 1,
                                    max_iter = 100,
                                    tol = 1e-6) {
  checkmate::assert_matrix(grid, mode = "numeric")

  n_rows <- nrow(grid)
  n_cols <- ncol(grid)

  # Store original for comparison
  original <- grid
  smoothed <- grid

  for (iter in seq_len(max_iter)) {
    prev <- smoothed

    # Smooth each row
    for (i in seq_len(n_rows)) {
      smoothed[i, ] <- whittaker_henderson_1d(smoothed[i, ], lambda = h_param)
    }

    # Smooth each column
    for (j in seq_len(n_cols)) {
      smoothed[, j] <- whittaker_henderson_1d(smoothed[, j], lambda = w_param)
    }

    # Check convergence
    change <- max(abs(smoothed - prev), na.rm = TRUE)
    if (change < tol) {
      cli::cli_alert_info("Converged after {iter} iterations")
      break
    }
  }

  # Preserve row/col names
  rownames(smoothed) <- rownames(grid)
  colnames(smoothed) <- colnames(grid)

  smoothed
}

#' Apply one-dimensional Whittaker-Henderson smoothing
#'
#' @description
#' Smooths a 1D vector using Whittaker-Henderson method with second-order
#' differences. Uses the closed-form solution via matrix operations.
#'
#' @param y Numeric vector to smooth
#' @param lambda Smoothing parameter (higher = smoother)
#' @param d Order of differences (default: 2)
#'
#' @return Smoothed numeric vector
#' @keywords internal
whittaker_henderson_1d <- function(y, lambda = 1, d = 2) {
  n <- length(y)

  if (n <= d + 1) {
    return(y)  # Too short to smooth
  }

  # Handle NA values
  valid <- !is.na(y)
  if (sum(valid) <= d + 1) {
    return(y)
  }

  # Create difference matrix D
  D <- diff(diag(n), differences = d)

  # Weights (1 for valid observations, 0 for NA)
  W <- diag(as.numeric(valid))

  # Solve: (W + λ * D'D) * z = W * y
  # Simplified: assume all weights = 1 for non-NA
  A <- diag(n) + lambda * crossprod(D)

  # Replace NA with 0 for computation
  y_filled <- y
  y_filled[is.na(y_filled)] <- 0

  # Solve
  z <- solve(A, y_filled)

  # Restore NA where original was NA
  z[!valid] <- NA

  # Ensure non-negative rates
  z[z < 0] <- 0

  z
}

# =============================================================================
# AGE-ADJUSTED MARRIAGE RATE (AMR) - Equation 1.6.2
# =============================================================================

#' Calculate age-adjusted marriage rate (AMR) from age group data
#'
#' @description
#' Calculates the age-adjusted central marriage rate per TR2025 Section 1.6:
#'
#' "The AMR is obtained by dividing:
#'  - The expected number of marriages by
#'  - The geometric mean of the number of unmarried men, ages 15+, and
#'    the unmarried women, ages 15+, in the standard population."
#'
#' Formula:
#'   Expected marriages = Σ P_{x,y}^S × m̂_{x,y}^z / 100,000
#'   AMR = (Expected / sqrt(total_male × total_female)) × 100,000
#'
#' @param rates_by_group data.table with husband_age_group, wife_age_group, rate
#' @param std_pop_by_group data.table with age_group, sex, unmarried_population
#'
#' @return Numeric AMR value (per 100,000 unmarried couples)
#'
#' @export
calculate_amr <- function(rates_by_group, std_pop_by_group) {
  checkmate::assert_data_table(rates_by_group)
  checkmate::assert_data_table(std_pop_by_group)

  # Make a copy to avoid modifying input
  rates <- data.table::copy(rates_by_group)

  # Get male and female population by group
  male_pop <- std_pop_by_group[sex == "male"]
  female_pop <- std_pop_by_group[sex == "female"]

  # Total population by sex (for denominator)
  total_male <- sum(male_pop$unmarried_population)
  total_female <- sum(female_pop$unmarried_population)

  # Create lookups for cell-level P_{x,y}^S calculation
  male_lookup <- setNames(male_pop$unmarried_population, male_pop$age_group)
  female_lookup <- setNames(female_pop$unmarried_population, female_pop$age_group)

  # Calculate P_{x,y}^S for each age group combination
  # P_{x,y}^S = sqrt(male_x × female_y)
  rates[, male_pop := male_lookup[husband_age_group]]
  rates[, female_pop := female_lookup[wife_age_group]]
  rates[, P_xy := sqrt(male_pop * female_pop)]

  # Numerator: Expected marriages = Σ P_{x,y}^S × m̂_{x,y}^z / 100,000
  # (rates are per 100,000, so divide by 100,000 to get actual expected marriages)
  expected_marriages <- sum(rates$P_xy * rates$rate, na.rm = TRUE) / 100000

  # Denominator: sqrt(total_male × total_female) per TR2025 text
  total_geometric_mean <- sqrt(total_male * total_female)

  # AMR = (expected marriages / total geometric mean) × 100,000
  amr <- (expected_marriages / total_geometric_mean) * 100000

  amr
}

#' Calculate AMR from single-year rate matrix
#'
#' @description
#' Calculates the age-adjusted central marriage rate per TR2025 Equation 1.6.2:
#'
#' AMR = (Σ P_{x,y}^S × m̂_{x,y}^z / 100,000) / sqrt(Σ male × Σ female) × 100,000
#'
#' Where:
#' - P_{x,y}^S = sqrt(male_x × female_y) is the standard population
#' - m̂_{x,y}^z are rates per 100,000
#'
#' Note: The standard_pop_grid should have an attribute "total_male" and "total_female"
#' containing the total unmarried population by sex. If not present, the function
#' will estimate using the grid values.
#'
#' @param rates Matrix of marriage rates (husband_age × wife_age), per 100,000
#' @param standard_pop_grid Matrix of P_{x,y}^S values (geometric means)
#'
#' @return Numeric AMR value (per 100,000)
#'
#' @keywords internal
calculate_amr_from_matrix <- function(rates, standard_pop_grid) {
  checkmate::assert_matrix(rates, mode = "numeric")
  checkmate::assert_matrix(standard_pop_grid, mode = "numeric")

  if (!all(dim(rates) == dim(standard_pop_grid))) {
    cli::cli_abort("Rate matrix and standard population must have same dimensions")
  }

  # Expected marriages = Σ P_{x,y}^S × m̂_{x,y}^z / 100,000
  # (rates are per 100,000, so divide to get actual expected marriages)
  expected_marriages <- sum(standard_pop_grid * rates, na.rm = TRUE) / 100000

  # Get total male and female from attributes if available
  total_male <- attr(standard_pop_grid, "total_male")
  total_female <- attr(standard_pop_grid, "total_female")

  if (!is.null(total_male) && !is.null(total_female)) {
    # Use exact denominator: sqrt(total_male × total_female)
    denominator <- sqrt(total_male * total_female)
  } else {
    # Estimate from the grid
    # P_{x,y}^S = sqrt(male_x × female_y)
    # Row marginal: Σ_y P_{x,y} = sqrt(male_x) × Σ_y sqrt(female_y)
    # Column marginal: Σ_x P_{x,y} = Σ_x sqrt(male_x) × sqrt(female_y)

    # For uniform-ish populations:
    # Σ P_{x,y} ≈ n × sqrt(avg_male × avg_female)
    # sqrt(total_male × total_female) = sqrt(n × avg_male × n × avg_female)
    #                                 = n × sqrt(avg_male × avg_female)
    #                                 ≈ Σ P_{x,y} / n

    n_ages <- nrow(rates)
    total_P <- sum(standard_pop_grid, na.rm = TRUE)
    denominator <- total_P / n_ages
  }

  amr <- (expected_marriages / denominator) * 100000
  amr
}

#' Calculate simple AMR from marriages and population
#'
#' @description
#' Calculates the simple age-adjusted marriage rate:
#' AMR = (Total Marriages / sqrt(Total Male Unmarried × Total Female Unmarried)) × 100,000
#'
#' @param total_marriages Total number of marriages
#' @param total_male_unmarried Total unmarried males 15+
#' @param total_female_unmarried Total unmarried females 15+
#'
#' @return Numeric AMR value (per 100,000)
#'
#' @export
calculate_simple_amr <- function(total_marriages, total_male_unmarried, total_female_unmarried) {
  geom_mean <- sqrt(total_male_unmarried * total_female_unmarried)
  (total_marriages / geom_mean) * 100000
}

#' Calculate historical AMR from marriage and population data
#'
#' @description
#' Calculates AMR for each year using the simple formula per TR2025.
#'
#' @param marriages data.table with year and marriages columns
#' @param unmarried_pop data.table with year, sex, age_group, unmarried_population
#' @param years Years to calculate (default: all in marriages data)
#'
#' @return data.table with year and amr columns
#'
#' @export
calculate_historical_amr <- function(marriages, unmarried_pop, years = NULL) {
  if (is.null(years)) {
    years <- sort(unique(marriages$year))
  }

  # Aggregate marriages by year
  marriages_by_year <- marriages[, .(total_marriages = sum(marriages, na.rm = TRUE)), by = year]

  result <- lapply(years, function(yr) {
    m <- marriages_by_year[year == yr, total_marriages]
    if (length(m) == 0 || is.na(m)) return(data.table(year = yr, amr = NA_real_))

    pop <- unmarried_pop[year == yr]
    if (nrow(pop) == 0) return(data.table(year = yr, amr = NA_real_))

    total_male <- sum(pop[sex == "male", unmarried_population])
    total_female <- sum(pop[sex == "female", unmarried_population])

    amr <- calculate_simple_amr(m, total_male, total_female)
    data.table(year = yr, amr = amr)
  })

  data.table::rbindlist(result)
}

#' Build standard population grid for AMR calculation
#'
#' @description
#' Creates a matrix of geometric means of unmarried population by age
#' for use in AMR calculation. Uses 2010 as the standard year per TR2025.
#'
#' P_{x,y}^S = sqrt(unmarried_male_x × unmarried_female_y)
#'
#' @param unmarried_pop data.table of unmarried population by age group and sex
#' @param std_year Standard year (default: 2010)
#' @param min_age Minimum age (default: 14)
#' @param max_age Maximum age (default: 100)
#'
#' @return Matrix with husband_age rows × wife_age columns
#'
#' @export
build_standard_population_grid <- function(unmarried_pop,
                                            std_year = 2010,
                                            min_age = 14,
                                            max_age = 100) {
  # Filter to standard year
  pop_year <- unmarried_pop[year == std_year]

  if (nrow(pop_year) == 0) {
    cli::cli_abort("No data for standard year {std_year}")
  }

  # Get male and female populations by age group
  male_pop <- pop_year[sex == "male"]
  female_pop <- pop_year[sex == "female"]

  # Create lookup tables
  male_lookup <- setNames(male_pop$unmarried_population, male_pop$age_group)
  female_lookup <- setNames(female_pop$unmarried_population, female_pop$age_group)

  # Map single ages to age groups using canonical definition
  map_to_group <- function(age) {
    for (grp in names(MARGRID_AGE_GROUPS)) {
      info <- MARGRID_AGE_GROUPS[[grp]]
      if (age >= info$min && age <= info$max) return(grp)
    }
    NA_character_
  }

  # Create grid of geometric means
  ages <- min_age:max_age
  n_ages <- length(ages)

  grid <- matrix(0, nrow = n_ages, ncol = n_ages)
  rownames(grid) <- ages
  colnames(grid) <- ages

  for (i in seq_along(ages)) {
    h_age <- ages[i]
    h_group <- map_to_group(h_age)
    if (is.na(h_group)) next

    h_pop_total <- male_lookup[h_group]
    if (is.na(h_pop_total)) h_pop_total <- 0
    h_width <- MARGRID_AGE_GROUPS[[h_group]]$width
    h_pop_single <- h_pop_total / h_width

    for (j in seq_along(ages)) {
      w_age <- ages[j]
      w_group <- map_to_group(w_age)
      if (is.na(w_group)) next

      w_pop_total <- female_lookup[w_group]
      if (is.na(w_pop_total)) w_pop_total <- 0
      w_width <- MARGRID_AGE_GROUPS[[w_group]]$width
      w_pop_single <- w_pop_total / w_width

      # Geometric mean of single-year populations
      grid[i, j] <- sqrt(h_pop_single * w_pop_single)
    }
  }

  # Calculate total unmarried population by sex for AMR calculation
  # Sum across all age groups
  total_male <- sum(sapply(names(male_lookup), function(g) {
    if (is.na(male_lookup[g])) 0 else male_lookup[g]
  }))
  total_female <- sum(sapply(names(female_lookup), function(g) {
    if (is.na(female_lookup[g])) 0 else female_lookup[g]
  }))

  # Add as attributes for AMR calculation
  attr(grid, "total_male") <- total_male
  attr(grid, "total_female") <- total_female
  attr(grid, "standard_year") <- std_year

  grid
}

# =============================================================================
# MARGRID SCALING (6C.5)
# =============================================================================

#' Scale MarGrid to match target total marriages
#'
#' @description
#' Proportionally scales all MarGrid rates so that expected total marriages
#' (rates × unmarried population) matches a target.
#'
#' @param margrid MarGrid matrix (husband × wife ages)
#' @param unmarried_pop Matrix of unmarried population geometric means
#' @param target_total Target total marriages
#'
#' @return Scaled MarGrid matrix
#'
#' @export
scale_margrid_to_total <- function(margrid, unmarried_pop, target_total) {
  checkmate::assert_matrix(margrid, mode = "numeric")
  checkmate::assert_matrix(unmarried_pop, mode = "numeric")
  checkmate::assert_number(target_total, lower = 0)

  # Calculate current expected marriages
  # Expected = sum(rate × population) / 100000 (since rates are per 100k)
  current_expected <- sum(margrid * unmarried_pop, na.rm = TRUE) / 100000

  if (current_expected == 0) {
    cli::cli_warn("Current expected marriages is 0, cannot scale")
    return(margrid)
  }

  # Scale factor
  scale_factor <- target_total / current_expected

  # Apply scaling
  scaled <- margrid * scale_factor

  cli::cli_alert_info("Scaled MarGrid by factor {round(scale_factor, 4)}")

  scaled
}

#' Scale MarGrid to target AMR
#'
#' @description
#' Proportionally scales MarGrid rates to produce a target AMR when applied
#' to the standard population.
#'
#' @param margrid MarGrid matrix
#' @param target_amr Target AMR value (per 100,000)
#' @param standard_pop Standard population matrix (2010)
#'
#' @return Scaled MarGrid matrix
#'
#' @export
scale_margrid_to_amr <- function(margrid, target_amr, standard_pop) {
  checkmate::assert_matrix(margrid, mode = "numeric")
  checkmate::assert_number(target_amr, lower = 0)
  checkmate::assert_matrix(standard_pop, mode = "numeric")

  # Calculate current AMR
  current_amr <- calculate_amr(margrid, standard_pop)

  if (current_amr == 0) {
    cli::cli_warn("Current AMR is 0, cannot scale")
    return(margrid)
  }

  # Scale factor
  scale_factor <- target_amr / current_amr

  # Apply scaling
  scaled <- margrid * scale_factor

  cli::cli_alert_info("Scaled MarGrid: AMR {round(current_amr, 1)} → {round(target_amr, 1)}")

  scaled
}

# =============================================================================
# MARGRID ADJUSTMENT (6C.4)
# =============================================================================

#' Adjust MarGrid to match age group totals
#'
#' @description
#' Adjusts detailed MarGrid rates within each age group so that when summed,
#' they match external totals (e.g., from NCHS subset data).
#'
#' @param margrid Base MarGrid matrix (single-year ages)
#' @param group_totals Marriage counts by age group
#'   Required columns: husband_age_group, wife_age_group, marriages
#' @param unmarried_pop Unmarried population (for computing expected from rates)
#'
#' @return Adjusted MarGrid matrix
#'
#' @export
adjust_margrid_to_groups <- function(margrid, group_totals, unmarried_pop) {
  checkmate::assert_matrix(margrid, mode = "numeric")
  checkmate::assert_data_table(group_totals)

  adjusted <- margrid

  # For each age group combination
  for (i in seq_len(nrow(group_totals))) {
    h_group <- group_totals$husband_age_group[i]
    w_group <- group_totals$wife_age_group[i]
    target_marriages <- group_totals$marriages[i]

    # Get age ranges
    h_range <- get_age_range(h_group)
    w_range <- get_age_range(w_group)

    # Clip to MarGrid range
    h_min <- max(h_range$min, MARGRID_MIN_AGE)
    h_max <- min(h_range$max, MARGRID_MAX_AGE)
    w_min <- max(w_range$min, MARGRID_MIN_AGE)
    w_max <- min(w_range$max, MARGRID_MAX_AGE)

    # Row/col indices
    h_idx <- (h_min - MARGRID_MIN_AGE + 1):(h_max - MARGRID_MIN_AGE + 1)
    w_idx <- (w_min - MARGRID_MIN_AGE + 1):(w_max - MARGRID_MIN_AGE + 1)

    # Calculate current expected marriages for this group
    current_rates <- adjusted[h_idx, w_idx, drop = FALSE]
    current_pop <- unmarried_pop[h_idx, w_idx, drop = FALSE]
    current_expected <- sum(current_rates * current_pop, na.rm = TRUE) / 100000

    if (current_expected > 0 && target_marriages > 0) {
      scale <- target_marriages / current_expected
      adjusted[h_idx, w_idx] <- current_rates * scale
    }
  }

  adjusted
}

# =============================================================================
# PHASE 6D: HISTORICAL PERIOD (1989-2022)
# =============================================================================

#' Calculate historical marriage rates for 1989-1995
#'
#' @description
#' Per TR2025: For 1989-1995, NCHS provided data by age-group-of-husband crossed
#' with age-group-of-wife. These data are used to change the distribution of
#' MarGrid by these age groups.
#'
#' Steps:
#' 1. Use base MarGrid from 1978-1988
#' 2. Adjust within-group rates to match NCHS subset distributions
#' 3. Scale to NCHS U.S. totals (adjusted for SS area)
#' 4. Graduate using 2D Whittaker-Henderson
#'
#' @param base_margrid Matrix: base MarGrid from build_base_margrid()
#' @param nchs_subset data.table: NCHS MRA subset marriages (1989-1995)
#'   From fetch_nchs_mra_marriages_1989_1995()
#' @param nchs_us_totals data.table: NCHS U.S. total marriages
#'   From fetch_nchs_us_total_marriages()
#' @param unmarried_pop_grid Matrix: unmarried population geometric means
#' @param ss_area_factor Numeric or NULL: SS area adjustment factor. If NULL
#'   (default), calculates dynamically from historical population data.
#' @param smooth Logical: apply graduation after adjustment (default: TRUE)
#'
#' @return list with:
#'   - rates: List of rate matrices by year
#'   - amr: data.table with AMR values by year
#'
#' @export
calculate_historical_rates_1989_1995 <- function(base_margrid,
                                                   nchs_subset,
                                                   nchs_us_totals,
                                                   unmarried_pop_grid,
                                                   ss_area_factor = NULL,
                                                   smooth = TRUE,
                                                   same_sex_estimates = NULL) {
  checkmate::assert_matrix(base_margrid, mode = "numeric")
  checkmate::assert_data_table(nchs_subset)
  checkmate::assert_data_table(nchs_us_totals)
  checkmate::assert_matrix(unmarried_pop_grid, mode = "numeric")

  cli::cli_h2("Calculating Historical Rates (1989-1995)")

  years <- sort(unique(nchs_subset$year))
  cli::cli_alert_info("Processing {length(years)} years: {min(years)}-{max(years)}")

  rates <- list()
  amr_values <- list()

  for (yr in years) {
    cli::cli_alert("Processing {yr}...")

    # Get NCHS subset data for this year
    yr_data <- nchs_subset[year == yr]

    # Align age groups from NCHS 9-category to our 8-category
    yr_data <- align_nchs_subset_age_groups(yr_data)

    # Adjust MarGrid distribution to match NCHS subset
    adjusted_grid <- adjust_margrid_to_groups(
      base_margrid,
      yr_data,
      unmarried_pop_grid
    )

    # Get NCHS U.S. total for this year
    nchs_total <- nchs_us_totals[year == yr, total_marriages]
    if (length(nchs_total) == 0 || is.na(nchs_total)) {
      cli::cli_warn("No NCHS total for {yr}, using MRA sum")
      nchs_total <- sum(yr_data$marriages, na.rm = TRUE)
    }

    # Per TR2025 Step 6: subtract same-sex marriages before computing rates
    ss_count <- 0
    if (!is.null(same_sex_estimates)) {
      ss_row <- same_sex_estimates[year == yr]
      if (nrow(ss_row) > 0 && !is.na(ss_row$ss_marriages)) {
        ss_count <- ss_row$ss_marriages
        nchs_total <- nchs_total - ss_count
        if (ss_count > 0) {
          cli::cli_alert_info("  Subtracted {format(ss_count, big.mark=',')} same-sex marriages from {yr} total")
        }
      }
    }

    # Get SS area factor (calculate dynamically if not provided)
    yr_factor <- if (is.null(ss_area_factor)) {
      get_ss_area_factor(yr)
    } else {
      ss_area_factor
    }

    # Scale to SS area total (U.S. total × adjustment factor)
    ss_total <- nchs_total * yr_factor

    # Scale rates to match total marriages
    scaled_grid <- scale_margrid_to_total(adjusted_grid, unmarried_pop_grid, ss_total)

    # Apply graduation if requested
    if (smooth) {
      scaled_grid <- whittaker_henderson_2d(scaled_grid, h_param = 0.5, w_param = 0.5)
    }

    rates[[as.character(yr)]] <- scaled_grid

    # Calculate AMR for this year
    amr <- calculate_amr_from_matrix(scaled_grid, unmarried_pop_grid)
    amr_values[[as.character(yr)]] <- data.table::data.table(
      year = yr,
      amr = amr,
      total_marriages = ss_total,
      source = "NCHS_subset"
    )
  }

  cli::cli_alert_success("Completed 1989-1995 historical rates")

  list(
    rates = rates,
    amr = data.table::rbindlist(amr_values)
  )
}

#' Align NCHS subset age groups to standard 8-category
#'
#' @description
#' NCHS subset uses 15-19 but our base uses 14-19. Combine groups as needed.
#'
#' @param nchs_data data.table with NCHS subset data
#' @return data.table with aligned age groups
#' @keywords internal
align_nchs_subset_age_groups <- function(nchs_data) {
  dt <- data.table::copy(nchs_data)

  # Map NCHS groups to standard groups
  # NCHS subset uses: 12-17, 18-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 65+
  # We need: 14-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 65+
  group_map <- c(
    "12-17" = "14-19",
    "15-19" = "14-19",
    "18-19" = "14-19",
    "20-24" = "20-24",
    "25-29" = "25-29",
    "30-34" = "30-34",
    "35-44" = "35-44",
    "45-54" = "45-54",
    "55-64" = "55-64",
    "65+"   = "65+"
  )

  # Map both husband and wife age groups
  dt[, husband_age_group := group_map[husband_age_group]]
  dt[, wife_age_group := group_map[wife_age_group]]

  # Aggregate any combined groups
  dt <- dt[!is.na(husband_age_group) & !is.na(wife_age_group),
           .(marriages = sum(marriages, na.rm = TRUE)),
           by = .(year, husband_age_group, wife_age_group)]

  dt
}

#' Interpolate marriage rate grids between two years
#'
#' @description
#' Per TR2025: For years 1996-2007, marriage grids were linearly interpolated
#' between 1995 and 2008.
#'
#' @param grid_start Matrix: marriage rate grid for start year (e.g., 1995)
#' @param grid_end Matrix: marriage rate grid for end year (e.g., 2008)
#' @param year_start Integer: start year
#' @param year_end Integer: end year
#' @param years Integer vector: years to interpolate (between start and end)
#' @param nchs_us_totals data.table: NCHS U.S. total marriages for scaling
#' @param unmarried_pop_grid Matrix: unmarried population for scaling
#' @param ss_area_factor Numeric or NULL: SS area adjustment factor. If NULL
#'   (default), calculates dynamically from historical population data.
#' @param same_sex_estimates data.table or NULL: estimated same-sex marriages by year.
#'   If provided, same-sex count is subtracted from NCHS total before scaling (TR2025 Step 6).
#'
#' @return list of interpolated rate matrices by year
#'
#' @export
interpolate_marriage_grids <- function(grid_start,
                                         grid_end,
                                         year_start,
                                         year_end,
                                         years,
                                         nchs_us_totals = NULL,
                                         unmarried_pop_grid = NULL,
                                         ss_area_factor = NULL,
                                         same_sex_estimates = NULL) {
  checkmate::assert_matrix(grid_start, mode = "numeric")
  checkmate::assert_matrix(grid_end, mode = "numeric")
  checkmate::assert_integerish(years)

  cli::cli_h2("Interpolating Marriage Grids ({year_start} to {year_end})")

  # Calculate interpolation weights for each year
  year_range <- year_end - year_start

  results <- list()
  amr_values <- list()

  for (yr in years) {
    # Linear interpolation weight (0 = start year, 1 = end year)
    weight <- (yr - year_start) / year_range

    # Interpolate rate grids
    interpolated_grid <- grid_start * (1 - weight) + grid_end * weight

    # Get SS area factor (calculate dynamically if not provided)
    yr_factor <- if (is.null(ss_area_factor)) {
      get_ss_area_factor(yr)
    } else {
      ss_area_factor
    }

    # Scale to NCHS total if provided
    if (!is.null(nchs_us_totals) && !is.null(unmarried_pop_grid)) {
      nchs_total <- nchs_us_totals[year == yr, total_marriages]
      if (length(nchs_total) > 0 && !is.na(nchs_total)) {
        # Per TR2025 Step 6: subtract same-sex marriages before computing rates
        if (!is.null(same_sex_estimates)) {
          ss_row <- same_sex_estimates[year == yr]
          if (nrow(ss_row) > 0 && !is.na(ss_row$ss_marriages) && ss_row$ss_marriages > 0) {
            nchs_total <- nchs_total - ss_row$ss_marriages
            cli::cli_alert_info("  Subtracted {format(ss_row$ss_marriages, big.mark=',')} same-sex marriages from {yr} total")
          }
        }
        ss_total <- nchs_total * yr_factor
        interpolated_grid <- scale_margrid_to_total(
          interpolated_grid, unmarried_pop_grid, ss_total
        )
      }
    }

    results[[as.character(yr)]] <- interpolated_grid

    # Calculate AMR
    if (!is.null(unmarried_pop_grid)) {
      amr <- calculate_amr_from_matrix(interpolated_grid, unmarried_pop_grid)

      # Get total marriages for this year
      total_mar <- NA_real_
      if (!is.null(nchs_us_totals)) {
        nchs_total <- nchs_us_totals[year == yr, total_marriages]
        if (length(nchs_total) > 0 && !is.na(nchs_total)) {
          total_mar <- nchs_total * yr_factor
        }
      }

      amr_values[[as.character(yr)]] <- data.table::data.table(
        year = yr,
        amr = amr,
        total_marriages = total_mar,
        source = "interpolated"
      )
    }

    cli::cli_alert_success("Interpolated {yr} (weight: {round(weight, 3)})")
  }

  list(
    rates = results,
    amr = data.table::rbindlist(amr_values)
  )
}

#' Calculate historical marriage rates from ACS (2008-2022)
#'
#' @description
#' Per TR2025: Starting with 2007, ACS provides data on marriages by
#' age-of-husband crossed with age-of-wife. These data are used to change
#' the distribution of MarGrid by age groups.
#'
#' @param base_margrid Matrix: base MarGrid
#' @param acs_grids List of ACS marriage grid matrices by year
#' @param nchs_us_totals data.table: NCHS U.S. total marriages
#' @param unmarried_pop_grid Matrix: unmarried population geometric means
#' @param ss_area_factor Numeric or NULL: SS area adjustment factor. If NULL
#'   (default), calculates dynamically from historical population data.
#' @param smooth Logical: apply graduation after adjustment
#'
#' @return list with rates and AMR values
#'
#' @export
calculate_historical_rates_2008_2022 <- function(base_margrid,
                                                   acs_grids,
                                                   nchs_us_totals,
                                                   unmarried_pop_grid,
                                                   ss_area_factor = NULL,
                                                   smooth = TRUE,
                                                   same_sex_estimates = NULL) {
  checkmate::assert_matrix(base_margrid, mode = "numeric")
  checkmate::assert_list(acs_grids)
  checkmate::assert_data_table(nchs_us_totals)
  checkmate::assert_matrix(unmarried_pop_grid, mode = "numeric")

  cli::cli_h2("Calculating Historical Rates from ACS (2008-2022)")

  years <- sort(as.integer(names(acs_grids)))
  cli::cli_alert_info("Processing {length(years)} years")

  rates <- list()
  amr_values <- list()

  for (yr in years) {
    cli::cli_alert("Processing {yr}...")

    acs_grid <- acs_grids[[as.character(yr)]]

    # Convert ACS grid to age groups for MarGrid adjustment
    acs_by_group <- aggregate_grid_to_age_groups(acs_grid)

    # Adjust MarGrid distribution to match ACS
    adjusted_grid <- adjust_margrid_to_groups(
      base_margrid,
      acs_by_group,
      unmarried_pop_grid
    )

    # Get NCHS U.S. total for this year
    nchs_total <- nchs_us_totals[year == yr, total_marriages]
    if (length(nchs_total) == 0 || is.na(nchs_total)) {
      # Use ACS total if NCHS not available
      cli::cli_warn("No NCHS total for {yr}, using ACS sum")
      nchs_total <- sum(acs_grid, na.rm = TRUE)
    }

    # Per TR2025 Step 6: subtract same-sex marriages before computing rates
    ss_count <- 0
    if (!is.null(same_sex_estimates)) {
      ss_row <- same_sex_estimates[year == yr]
      if (nrow(ss_row) > 0 && !is.na(ss_row$ss_marriages)) {
        ss_count <- ss_row$ss_marriages
        nchs_total <- nchs_total - ss_count
        if (ss_count > 0) {
          cli::cli_alert_info("  Subtracted {format(ss_count, big.mark=',')} same-sex marriages from {yr} total")
        }
      }
    }

    # Get SS area factor (calculate dynamically if not provided)
    yr_factor <- if (is.null(ss_area_factor)) {
      get_ss_area_factor(yr)
    } else {
      ss_area_factor
    }

    # Scale to SS area total
    ss_total <- nchs_total * yr_factor

    # Scale rates to match total marriages
    scaled_grid <- scale_margrid_to_total(adjusted_grid, unmarried_pop_grid, ss_total)

    # Apply graduation if requested
    if (smooth) {
      scaled_grid <- whittaker_henderson_2d(scaled_grid, h_param = 0.5, w_param = 0.5)
    }

    rates[[as.character(yr)]] <- scaled_grid

    # Calculate AMR
    amr <- calculate_amr_from_matrix(scaled_grid, unmarried_pop_grid)
    amr_values[[as.character(yr)]] <- data.table::data.table(
      year = yr,
      amr = amr,
      total_marriages = ss_total,
      source = "ACS"
    )
  }

  cli::cli_alert_success("Completed ACS-based historical rates")

  list(
    rates = rates,
    amr = data.table::rbindlist(amr_values)
  )
}

#' Aggregate single-year grid to age groups
#'
#' @description
#' Aggregates a single-year age grid to TR2025 8-category age groups
#' for use in MarGrid adjustment.
#'
#' @param grid Matrix: marriage grid with single-year ages
#'
#' @return data.table with husband_age_group, wife_age_group, marriages
#'
#' @keywords internal
aggregate_grid_to_age_groups <- function(grid) {
  ages <- as.integer(rownames(grid))
  if (is.null(ages)) {
    ages <- as.integer(colnames(grid))
  }
  if (is.null(ages)) {
    ages <- seq_len(nrow(grid)) + 14  # Assume starting at 15 if no names
  }

  # Use canonical MARGRID_AGE_GROUPS definition
  results <- list()

  for (h_group in names(MARGRID_AGE_GROUPS)) {
    h_info <- MARGRID_AGE_GROUPS[[h_group]]
    h_ages <- h_info$min:h_info$max
    h_idx <- which(ages %in% h_ages)

    if (length(h_idx) == 0) next

    for (w_group in names(MARGRID_AGE_GROUPS)) {
      w_info <- MARGRID_AGE_GROUPS[[w_group]]
      w_ages <- w_info$min:w_info$max
      w_idx <- which(ages %in% w_ages)

      if (length(w_idx) == 0) next

      marriages <- sum(grid[h_idx, w_idx], na.rm = TRUE)

      results[[length(results) + 1]] <- data.table::data.table(
        husband_age_group = h_group,
        wife_age_group = w_group,
        marriages = marriages
      )
    }
  }

  data.table::rbindlist(results)
}

#' Calculate complete historical period (1989-2022)
#'
#' @description
#' Main orchestration function for Phase 6D. Calculates historical marriage
#' rates for the entire 1989-2022 period per TR2025 methodology:
#' - 1989-1995: NCHS subset data adjusted MarGrid
#' - 1996-2007: Linear interpolation
#' - 2008-2022: ACS data adjusted MarGrid
#'
#' Results are cached to disk for performance. Use `force_recompute = TRUE`
#' to regenerate cached results.
#'
#' @param base_margrid Matrix: base MarGrid from build_base_margrid()
#' @param nchs_subset data.table: NCHS MRA subset marriages (1989-1995)
#' @param acs_grids List of ACS marriage grids (2008-2022)
#' @param nchs_us_totals data.table: NCHS U.S. total marriages
#' @param unmarried_pop_grid Matrix: 2010 standard population geometric means
#' @param ss_area_factor Numeric or NULL: SS area adjustment factor. If NULL
#'   (default), calculates dynamically from historical population data.
#' @param smooth Logical: apply graduation after adjustment (default: TRUE)
#' @param cache_dir Character: directory for caching results
#' @param force_recompute Logical: force recomputation even if cache exists
#'
#' @return list with:
#'   - rates: List of rate matrices by year (1989-2022)
#'   - amr: data.table with AMR values by year
#'   - summary: Summary statistics
#'
#' @export
calculate_historical_period <- function(base_margrid,
                                          nchs_subset,
                                          acs_grids,
                                          nchs_us_totals,
                                          unmarried_pop_grid,
                                          acs_start = 2008,
                                          acs_end = 2022,
                                          ss_area_factor = NULL,
                                          smooth = TRUE,
                                          cache_dir = here::here("data/cache/marriage"),
                                          force_recompute = FALSE,
                                          same_sex_estimates = NULL) {
  # Check for cached results
  cache_file <- file.path(cache_dir, sprintf("historical_rates_1989_%d.rds", acs_end))
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  if (file.exists(cache_file) && !force_recompute) {
    cli::cli_alert_success("Loading cached historical rates (1989-{acs_end})")
    cached <- readRDS(cache_file)

    # Verify cache integrity
    if (all(c("rates", "amr", "summary") %in% names(cached))) {
      cli::cli_alert_info(
        "Cached AMR range: {round(min(cached$amr$amr))} - {round(max(cached$amr$amr))}"
      )
      return(cached)
    }
    cli::cli_warn("Cache file corrupt, recomputing...")
  }

  cli::cli_h1("Phase 6D: Historical Period (1989-{acs_end})")

  # =========================================================================
  # STEP 1: 1989-1995 from NCHS subset
  # =========================================================================
  result_1989_1995 <- calculate_historical_rates_1989_1995(
    base_margrid = base_margrid,
    nchs_subset = nchs_subset,
    nchs_us_totals = nchs_us_totals,
    unmarried_pop_grid = unmarried_pop_grid,
    ss_area_factor = ss_area_factor,
    smooth = smooth,
    same_sex_estimates = same_sex_estimates
  )

  # =========================================================================
  # STEP 2: ACS historical rates
  # =========================================================================
  # Filter ACS grids to configured range
  acs_years <- as.integer(names(acs_grids))
  acs_grids_filtered <- acs_grids[acs_years >= acs_start & acs_years <= acs_end]

  result_acs <- calculate_historical_rates_2008_2022(
    base_margrid = base_margrid,
    acs_grids = acs_grids_filtered,
    nchs_us_totals = nchs_us_totals,
    unmarried_pop_grid = unmarried_pop_grid,
    ss_area_factor = ss_area_factor,
    smooth = smooth,
    same_sex_estimates = same_sex_estimates
  )

  # =========================================================================
  # STEP 3: Interpolation between NCHS subset end (1995) and ACS start
  # =========================================================================
  # Get 1995 grid from step 1 and first ACS grid from step 2
  grid_1995 <- result_1989_1995$rates[["1995"]]
  grid_acs_start <- result_acs$rates[[as.character(acs_start)]]

  if (is.null(grid_1995) || is.null(grid_acs_start)) {
    cli::cli_abort("Need both 1995 and {acs_start} grids for interpolation")
  }

  interpolation_years <- 1996:(acs_start - 1)
  result_interpolated <- interpolate_marriage_grids(
    grid_start = grid_1995,
    grid_end = grid_acs_start,
    year_start = 1995,
    year_end = acs_start,
    years = interpolation_years,
    nchs_us_totals = nchs_us_totals,
    unmarried_pop_grid = unmarried_pop_grid,
    ss_area_factor = ss_area_factor,
    same_sex_estimates = same_sex_estimates
  )

  # =========================================================================
  # COMBINE ALL RESULTS
  # =========================================================================
  all_rates <- c(
    result_1989_1995$rates,
    result_interpolated$rates,
    result_acs$rates
  )

  # Sort by year
  year_order <- order(as.integer(names(all_rates)))
  all_rates <- all_rates[year_order]

  all_amr <- data.table::rbindlist(list(
    result_1989_1995$amr,
    result_interpolated$amr,
    result_acs$amr
  ))
  data.table::setorder(all_amr, year)

  # =========================================================================
  # SUMMARY
  # =========================================================================
  interp_label <- sprintf("1996-%d", acs_start - 1)
  acs_label <- sprintf("%d-%d", acs_start, acs_end)
  summary_stats <- data.table::data.table(
    period = c("1989-1995", interp_label, acs_label, "Total"),
    n_years = c(
      length(result_1989_1995$rates),
      length(result_interpolated$rates),
      length(result_acs$rates),
      length(all_rates)
    ),
    mean_amr = c(
      mean(result_1989_1995$amr$amr, na.rm = TRUE),
      mean(result_interpolated$amr$amr, na.rm = TRUE),
      mean(result_acs$amr$amr, na.rm = TRUE),
      mean(all_amr$amr, na.rm = TRUE)
    ),
    min_amr = c(
      min(result_1989_1995$amr$amr, na.rm = TRUE),
      min(result_interpolated$amr$amr, na.rm = TRUE),
      min(result_acs$amr$amr, na.rm = TRUE),
      min(all_amr$amr, na.rm = TRUE)
    ),
    max_amr = c(
      max(result_1989_1995$amr$amr, na.rm = TRUE),
      max(result_interpolated$amr$amr, na.rm = TRUE),
      max(result_acs$amr$amr, na.rm = TRUE),
      max(all_amr$amr, na.rm = TRUE)
    )
  )

  cli::cli_h2("Historical Period Summary")
  print(summary_stats)

  cli::cli_alert_success(
    "Completed historical period: {length(all_rates)} years, AMR range {round(min(all_amr$amr))} - {round(max(all_amr$amr))}"
  )

  result <- list(
    rates = all_rates,
    amr = all_amr,
    summary = summary_stats
  )

  # Cache results for future use
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached historical rates to {cache_file}")

  result
}

# =============================================================================
# PHASE 6E: AMR PROJECTION (2023-2099)
# =============================================================================

#' Calculate starting AMR for projection
#'
#' @description
#' Per TR2025: The starting AMR is the 5-year weighted average of the most
#' recent historical AMR values. This provides a stable starting point for
#' projection that smooths out year-to-year volatility.
#'
#' Default uses 2018-2022 (excluding 2020 if not available).
#'
#' @param historical_amr data.table with year and amr columns
#' @param n_years Integer: number of years to average (default: 5)
#' @param weights Numeric vector: weights for averaging (default: equal weights)
#'
#' @return Numeric: starting AMR value
#'
#' @export
calculate_starting_amr <- function(historical_amr, n_years = 5, weights = NULL) {
  checkmate::assert_data_table(historical_amr)
  checkmate::assert_names(names(historical_amr), must.include = c("year", "amr"))

  # Get most recent n_years
  data.table::setorder(historical_amr, -year)
  recent <- historical_amr[1:min(n_years, nrow(historical_amr))]

  if (nrow(recent) == 0) {
    cli::cli_abort("No historical AMR data available")
  }

  # Default to equal weights
  if (is.null(weights)) {
    weights <- rep(1 / nrow(recent), nrow(recent))
  } else {
    # Normalize weights
    weights <- weights[1:nrow(recent)]
    weights <- weights / sum(weights)
  }

  # Calculate weighted average
  starting_amr <- sum(recent$amr * weights, na.rm = TRUE)

  cli::cli_alert_info(
    "Starting AMR: {round(starting_amr, 1)} (weighted average of {min(recent$year)}-{max(recent$year)})"
  )

  starting_amr
}

#' Project AMR from starting value to ultimate
#'
#' @description
#' Per TR2025: "The annual rate of change decreases in absolute value as the
#' ultimate year approaches."
#'
#' This implements a decreasing rate of change projection where the AMR
#' converges from the starting value to the ultimate value by the ultimate
#' year. The rate of change is highest in early years and decreases as the
#' ultimate year approaches.
#'
#' The projection formula uses:
#'   AMR(t) = ultimate + (starting - ultimate) * (1 - progress(t))^exponent
#'
#' where progress(t) = (t - start_year) / (ultimate_year - start_year)
#'
#' @param starting_amr Numeric: starting AMR value
#' @param ultimate_amr Numeric: ultimate AMR value
#' @param start_year Integer: first projection year
#' @param ultimate_year Integer: year when ultimate is reached
#' @param end_year Integer: final projection year
#' @param convergence_exp Numeric: exponent for convergence curve.
#'   Higher values = more gradual early change, faster late change
#'
#' @return data.table with year and projected_amr columns
#'
#' @export
project_amr <- function(starting_amr,
                        ultimate_amr,
                        start_year,
                        ultimate_year,
                        end_year,
                        convergence_exp) {
  checkmate::assert_number(starting_amr, lower = 0)
  checkmate::assert_number(ultimate_amr, lower = 0)
  checkmate::assert_integerish(start_year)
  checkmate::assert_integerish(ultimate_year)
  checkmate::assert_integerish(end_year)
  checkmate::assert_number(convergence_exp, lower = 0.1)

  cli::cli_h2("Projecting AMR ({start_year} to {end_year})")
  cli::cli_alert_info("Starting: {round(starting_amr, 1)}, Ultimate: {ultimate_amr}, Ultimate year: {ultimate_year}")

  years <- start_year:end_year
  n_convergence <- ultimate_year - start_year

  projected_amr <- sapply(years, function(yr) {
    if (yr >= ultimate_year) {
      # At or past ultimate year: hold at ultimate
      return(ultimate_amr)
    }

    # Calculate progress toward ultimate (0 to 1)
    progress <- (yr - start_year) / n_convergence

    # Apply convergence formula with decreasing rate of change
    # Uses complement: remaining_gap * (1 - progress)^exp
    # Higher exponent = front-loaded (faster start, gradual finish)
    remaining_factor <- (1 - progress)^convergence_exp

    # Linear interpolation with non-linear progress
    amr <- ultimate_amr + (starting_amr - ultimate_amr) * remaining_factor

    amr
  })

  result <- data.table::data.table(
    year = years,
    projected_amr = projected_amr
  )

  # Add rate of change for validation
  result[, amr_change := c(NA, diff(projected_amr))]

  cli::cli_alert_success(
    "Projected {length(years)} years, AMR: {round(starting_amr, 1)} → {ultimate_amr}"
  )

  result
}

#' Scale MarGrid to target AMR
#'
#' @description
#' Scales all rates in a MarGrid proportionally so that the resulting
#' AMR matches a target value.
#'
#' @param base_margrid Matrix: base marriage rate grid
#' @param target_amr Numeric: target AMR value
#' @param standard_pop_grid Matrix: standard population grid for AMR calculation
#'
#' @return Scaled MarGrid matrix
#'
#' @export
scale_margrid_to_target_amr <- function(base_margrid, target_amr, standard_pop_grid) {
  checkmate::assert_matrix(base_margrid, mode = "numeric")
  checkmate::assert_number(target_amr, lower = 0)
  checkmate::assert_matrix(standard_pop_grid, mode = "numeric")

  # Calculate current AMR

  current_amr <- calculate_amr_from_matrix(base_margrid, standard_pop_grid)

  if (current_amr == 0) {
    cli::cli_warn("Current AMR is 0, cannot scale")
    return(base_margrid)
  }

  # Calculate scale factor
  scale_factor <- target_amr / current_amr

  # Apply scaling
  scaled_grid <- base_margrid * scale_factor

  scaled_grid
}

#' Project marriage rates for future years
#'
#' @description
#' Main Phase 6E function. Projects marriage rate grids from the last
#' historical year to the end of the projection period (2099).
#'
#' Per TR2025:
#' 1. Calculate starting AMR from recent historical data
#' 2. Project AMR to ultimate value (4,000) by ultimate year (2047)
#' 3. Scale base MarGrid to match projected AMR for each year
#'
#' Results are cached to disk for performance.
#'
#' @param base_margrid Matrix: base MarGrid to scale
#' @param historical_amr data.table: historical AMR values for starting calculation
#' @param standard_pop_grid Matrix: 2010 standard population for AMR calculation
#' @param ultimate_amr Numeric: ultimate AMR target
#' @param start_year Integer: first projection year
#' @param ultimate_year Integer: year when ultimate is reached
#' @param end_year Integer: final projection year
#' @param convergence_exp Numeric: convergence exponent
#' @param starting_amr_n_years Integer: number of years for starting AMR average
#' @param starting_amr_weights Numeric vector or NULL: weights for starting AMR average
#' @param cache_dir Character: directory for caching results
#' @param force_recompute Logical: force recomputation even if cache exists
#'
#' @return list with:
#'   - rates: List of rate matrices by year (2023-2099)
#'   - amr: data.table with projected AMR values by year
#'   - starting_amr: The calculated starting AMR
#'
#' @export
project_marriage_rates <- function(base_margrid,
                                    historical_amr,
                                    standard_pop_grid,
                                    ultimate_amr,
                                    start_year,
                                    ultimate_year,
                                    end_year,
                                    convergence_exp,
                                    starting_amr_n_years,
                                    starting_amr_weights = NULL,
                                    cache_dir = here::here("data/cache/marriage"),
                                    force_recompute = FALSE) {
  # Check for cached results
  cache_file <- file.path(cache_dir, get_cache_filename("projected_rates", start_year, end_year))
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  if (file.exists(cache_file) && !force_recompute) {
    cli::cli_alert_success("Loading cached projected rates ({start_year}-{end_year})")
    cached <- readRDS(cache_file)

    # Verify cache integrity
    if (all(c("rates", "amr", "starting_amr") %in% names(cached))) {
      cli::cli_alert_info(
        "Cached projection: AMR {round(cached$starting_amr, 1)} → {ultimate_amr}"
      )
      return(cached)
    }
    cli::cli_warn("Cache file corrupt, recomputing...")
  }

  cli::cli_h1("Phase 6E: AMR Projection ({start_year}-{end_year})")

  # =========================================================================
  # STEP 1: Calculate starting AMR
  # =========================================================================
  starting_amr <- calculate_starting_amr(historical_amr,
                                          n_years = starting_amr_n_years,
                                          weights = starting_amr_weights)

  # =========================================================================
  # STEP 2: Project AMR to ultimate
  # =========================================================================
  amr_projection <- project_amr(
    starting_amr = starting_amr,
    ultimate_amr = ultimate_amr,
    start_year = start_year,
    ultimate_year = ultimate_year,
    end_year = end_year,
    convergence_exp = convergence_exp
  )

  # =========================================================================
  # STEP 3: Scale MarGrid for each year
  # =========================================================================
  cli::cli_h2("Scaling MarGrid to Projected AMR")

  rates <- list()
  years <- start_year:end_year

  for (i in seq_along(years)) {
    yr <- years[i]
    target_amr <- amr_projection[year == yr, projected_amr]

    # Scale base grid to target AMR
    scaled_grid <- scale_margrid_to_target_amr(
      base_margrid,
      target_amr,
      standard_pop_grid
    )

    rates[[as.character(yr)]] <- scaled_grid

    # Progress indicator every 10 years
    if (yr %% 10 == 0 || yr == start_year || yr == end_year) {
      cli::cli_alert("{yr}: AMR = {round(target_amr, 1)}")
    }
  }

  # =========================================================================
  # SUMMARY
  # =========================================================================
  cli::cli_h2("Projection Summary")
  cli::cli_alert_success("Projected {length(years)} years ({start_year}-{end_year})")
  cli::cli_alert_info("Starting AMR: {round(starting_amr, 1)}")
  cli::cli_alert_info("Ultimate AMR: {ultimate_amr} (reached by {ultimate_year})")
  cli::cli_alert_info("Years to ultimate: {ultimate_year - start_year}")

  result <- list(
    rates = rates,
    amr = amr_projection,
    starting_amr = starting_amr,
    ultimate_amr = ultimate_amr,
    ultimate_year = ultimate_year
  )

  # Cache results
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached projected rates to {cache_file}")

  result
}

# =============================================================================
# PHASE 6F: MARRIAGE RATE PROJECTION (PRIOR STATUS & MAIN ENTRY POINT)
# =============================================================================

#' Calculate prior marital status differentials
#'
#' @description
#' Per TR2025: "Future relative differences in marriage rates by prior marital
#' status are assumed to be the same as the average of those experienced during
#' 1979 and 1981-88."
#'
#' Calculates relative marriage rate differentials by prior marital status
#' (single, widowed, divorced) using NCHS 1979, 1981-1988 data.
#'
#' The differential is the ratio of marriage rate for each status to the
#' overall marriage rate within each age group and sex.
#'
#' @param marriages_by_status data.table with prior status marriage counts
#'   From fetch_nchs_marriages_by_prior_status_1978_1988()
#'   Required columns: year, age_group, sex, prior_status, marriages
#' @param years Years to use for averaging (default: c(1979, 1981:1988) per TR2025)
#'
#' @return data.table with columns: age_group, sex, prior_status, relative_rate
#'   where relative_rate is the multiplicative factor to apply to base rates
#'
#' @export
calculate_prior_status_differentials <- function(marriages_by_status,
                                                   years = c(1979, 1981:1988)) {
  checkmate::assert_data_table(marriages_by_status)
  checkmate::assert_names(names(marriages_by_status),
                          must.include = c("year", "age_group", "sex", "prior_status", "marriages"))

  cli::cli_h2("Calculating Prior Marital Status Differentials")

  # Filter to specified years
  dt <- marriages_by_status[year %in% years]
  cli::cli_alert_info("Using {length(unique(dt$year))} years: {paste(unique(dt$year), collapse=', ')}")

  # Calculate total marriages by age group, sex, and year (across all statuses)
  totals_by_group <- dt[, .(total_marriages = sum(marriages, na.rm = TRUE)),
                         by = .(year, age_group, sex)]

  # Calculate marriages by age group, sex, year, and prior status
  by_status <- dt[, .(status_marriages = sum(marriages, na.rm = TRUE)),
                   by = .(year, age_group, sex, prior_status)]

  # Merge to get proportion of each status
  merged <- merge(by_status, totals_by_group, by = c("year", "age_group", "sex"))
  merged[, proportion := status_marriages / total_marriages]

  # Average proportions across years
  avg_proportions <- merged[, .(
    avg_proportion = mean(proportion, na.rm = TRUE),
    n_years = .N
  ), by = .(age_group, sex, prior_status)]

  # Calculate relative rates

  # The relative rate is: status_proportion / (1/3)
  # This means if a status has 50% of marriages, relative_rate = 1.5
  # And sum of (relative_rate × base_rate × population_share) ≈ base_rate

  # Actually, per TR2025, we want rates to reflect actual likelihood differences
  # The relative rate should be normalized so weighted sum = 1
  # relative_rate = proportion / (1/n_statuses) = proportion × n_statuses

  n_statuses <- 3  # single, widowed, divorced
  avg_proportions[, relative_rate := avg_proportion * n_statuses]

  # Normalize within each age_group × sex so sum = n_statuses
  # (This ensures weighted average of relative rates = 1)
  avg_proportions[, sum_relative := sum(relative_rate), by = .(age_group, sex)]
  avg_proportions[, relative_rate := relative_rate * n_statuses / sum_relative]

  result <- avg_proportions[, .(age_group, sex, prior_status, relative_rate,
                                 avg_proportion)]

  # Order properly
  age_order <- c("12-17", "14-19", "18-19", "20-24", "25-29", "30-34",
                 "35-44", "45-54", "55-64", "65+")
  result[, age_group := factor(age_group, levels = age_order)]
  data.table::setorder(result, sex, age_group, prior_status)
  result[, age_group := as.character(age_group)]

  cli::cli_alert_success("Calculated differentials for {nrow(result)} age×sex×status combinations")

  # Show summary
  summary_rates <- result[, .(
    min_rate = round(min(relative_rate), 3),
    max_rate = round(max(relative_rate), 3),
    mean_rate = round(mean(relative_rate), 3)
  ), by = prior_status]
  cli::cli_alert_info("Relative rate ranges by status:")
  for (i in seq_len(nrow(summary_rates))) {
    cli::cli_alert("  {summary_rates$prior_status[i]}: {summary_rates$min_rate[i]} - {summary_rates$max_rate[i]} (mean: {summary_rates$mean_rate[i]})")
  }

  result
}

#' Apply prior marital status differentials to marriage rates
#'
#' @description
#' Creates marriage rate variants by prior marital status by applying
#' historical differentials to projected rates.
#'
#' @param marriage_rates List of marriage rate matrices by year
#'   From project_marriage_rates() or calculate_historical_period()
#' @param status_differentials data.table of relative rates by status
#'   From calculate_prior_status_differentials()
#' @param years Years to process (default: all years in marriage_rates)
#'
#' @return List with:
#'   - rates_by_status: List of rate matrices by year and prior_status
#'   - summary: Summary of rates by status
#'
#' @export
apply_prior_status_rates <- function(marriage_rates,
                                      status_differentials,
                                      years = NULL) {
  checkmate::assert_list(marriage_rates)
  checkmate::assert_data_table(status_differentials)

  if (is.null(years)) {
    years <- as.integer(names(marriage_rates))
  }

  cli::cli_h2("Applying Prior Marital Status Differentials")

  statuses <- unique(status_differentials$prior_status)
  cli::cli_alert_info("Statuses: {paste(statuses, collapse=', ')}")

  # Create lookup for relative rates by age group and sex
  # Map age groups to MarGrid age ranges
  age_group_ranges <- list(
    "12-17" = 12:17,
    "14-19" = 14:19,
    "18-19" = 18:19,
    "20-24" = 20:24,
    "25-29" = 25:29,
    "30-34" = 30:34,
    "35-44" = 35:44,
    "45-54" = 45:54,
    "55-64" = 55:64,
    "65+"   = 65:100
  )

  # Build lookup matrices for each status (husband dimension = male, wife = female)
  status_factors <- list()

  for (status in statuses) {
    # Get factors for this status
    male_factors <- status_differentials[sex == "male" & prior_status == status]
    female_factors <- status_differentials[sex == "female" & prior_status == status]

    # Create factor matrix matching MarGrid dimensions
    n_ages <- MARGRID_SIZE
    ages <- MARGRID_MIN_AGE:MARGRID_MAX_AGE

    factor_matrix <- matrix(1, nrow = n_ages, ncol = n_ages)
    rownames(factor_matrix) <- ages
    colnames(factor_matrix) <- ages

    # Apply husband (row) factors
    for (i in seq_len(nrow(male_factors))) {
      grp <- male_factors$age_group[i]
      if (grp %in% names(age_group_ranges)) {
        age_range <- age_group_ranges[[grp]]
        row_idx <- which(ages %in% age_range)
        # Scale rows by male factor
        factor_matrix[row_idx, ] <- factor_matrix[row_idx, ] * sqrt(male_factors$relative_rate[i])
      }
    }

    # Apply wife (column) factors
    for (i in seq_len(nrow(female_factors))) {
      grp <- female_factors$age_group[i]
      if (grp %in% names(age_group_ranges)) {
        age_range <- age_group_ranges[[grp]]
        col_idx <- which(ages %in% age_range)
        # Scale columns by female factor
        factor_matrix[, col_idx] <- factor_matrix[, col_idx] * sqrt(female_factors$relative_rate[i])
      }
    }

    status_factors[[status]] <- factor_matrix
  }

  # Apply factors to each year's rates
  rates_by_status <- list()

  for (yr in years) {
    yr_char <- as.character(yr)
    if (!(yr_char %in% names(marriage_rates))) next

    base_rates <- marriage_rates[[yr_char]]
    rates_by_status[[yr_char]] <- list()

    for (status in statuses) {
      # Apply status factor matrix element-wise
      status_rates <- base_rates * status_factors[[status]]
      rates_by_status[[yr_char]][[status]] <- status_rates
    }

    if (yr %% 20 == 0 || yr == min(years) || yr == max(years)) {
      cli::cli_alert("Processed {yr}")
    }
  }

  cli::cli_alert_success("Applied status differentials to {length(rates_by_status)} years")

  list(
    rates_by_status = rates_by_status,
    status_factors = status_factors,
    statuses = statuses
  )
}

#' Estimate same-sex marriage counts by year for historical period
#'
#' @description
#' Per TR2025 Section 1.6.c Step 6: "We also subtract out same-sex marriages
#' from the NCHS data, as we handle those in a later step."
#'
#' Estimates the number of same-sex marriages for each historical year:
#' - Pre-2004: 0 (no legal same-sex marriage in any U.S. state)
#' - 2004-2014: linear ramp from 0 to `default_fraction` as states legalized
#' - 2015+: uses `default_fraction` (post-Obergefell nationwide legalization)
#'
#' @param nchs_us_totals data.table with year and total_marriages columns
#' @param config List: optional config with same_sex.default_fraction
#'
#' @return data.table with year, total_marriages, ss_marriages, os_marriages
#'
#' @keywords internal
estimate_same_sex_marriages_by_year <- function(nchs_us_totals, config = NULL) {
  ss_fraction <- 0.045
  if (!is.null(config) && !is.null(config$marriage$same_sex$default_fraction)) {
    ss_fraction <- config$marriage$same_sex$default_fraction
  }

  dt <- data.table::copy(nchs_us_totals)

  dt[, ss_fraction := data.table::fifelse(
    year < 2004, 0,
    data.table::fifelse(
      year < 2015,
      ss_fraction * (year - 2004) / (2015 - 2004),
      ss_fraction
    )
  )]
  dt[, ss_marriages := round(total_marriages * ss_fraction)]
  dt[, os_marriages := total_marriages - ss_marriages]

  cli::cli_alert_info(
    "Estimated same-sex marriages: 0 (pre-2004), {round(ss_fraction * 100, 1)}% (2015+)"
  )

  dt
}

#' Separate same-sex and opposite-sex marriage rates
#'
#' @description
#' Per TR2025: "The MarGrid rates are then adjusted to produce two sets of
#' marriage rates: opposite-sex marriage rates and same-sex marriage rates."
#'
#' This implementation uses a PREVALENCE-BASED approach: at each age cell,
#' the same-sex rate equals the total rate times the local prevalence
#' (probability that a marriage at that age is same-sex).
#'
#' This guarantees that ss_rate <= total_rate at every cell, ensuring
#' opposite_sex + same_sex = total exactly.
#'
#' @param marriage_rates List of marriage rate matrices by year
#' @param prevalence_grids Result from calculate_same_sex_prevalence_grids() (optional)
#'   Contains ss_prevalence, mm_prevalence, ff_prevalence matrices
#' @param same_sex_data Result from fetch_acs_same_sex_grids() (optional, legacy)
#'   Used to calculate prevalence grids if prevalence_grids not provided
#' @param opposite_sex_grids ACS opposite-sex grids (needed if using same_sex_data)
#' @param same_sex_fraction Fallback fraction if no data provided (default: 0.045)
#' @param years Years to process (default: all years)
#'
#' @return List with:
#'   - opposite_sex: Rate matrices for opposite-sex marriages
#'   - same_sex: Rate matrices for same-sex marriages
#'   - male_male: Rate matrices for male-male marriages
#'   - female_female: Rate matrices for female-female marriages
#'   - same_sex_fraction: Overall same-sex fraction
#'   - method: "prevalence", "acs_pattern", or "constant_fraction"
#'
#' @export
separate_marriage_types <- function(marriage_rates,
                                     prevalence_grids = NULL,
                                     same_sex_data = NULL,
                                     opposite_sex_grids = NULL,
                                     same_sex_fraction = 0.045,
                                     years = NULL) {
  checkmate::assert_list(marriage_rates)

  if (is.null(years)) {
    years <- as.integer(names(marriage_rates))
  }

  cli::cli_h2("Separating Same-Sex and Opposite-Sex Marriage Rates")

  opposite_sex <- list()
  same_sex <- list()
  male_male <- list()
  female_female <- list()

  # ==========================================================================
  # METHOD 1: Prevalence-based approach (PREFERRED)
  # ==========================================================================
  # If prevalence_grids provided, use them directly
  # If same_sex_data + opposite_sex_grids provided, calculate prevalence first

  if (!is.null(prevalence_grids) && "ss_prevalence" %in% names(prevalence_grids)) {
    cli::cli_alert_info("Using PREVALENCE-BASED separation (guarantees exact split)")

    ss_prev <- prevalence_grids$ss_prevalence
    mm_prev <- prevalence_grids$mm_prevalence
    ff_prev <- prevalence_grids$ff_prevalence
    prev_ages <- prevalence_grids$ages

    overall_ss <- prevalence_grids$overall_ss_fraction
    overall_mm <- prevalence_grids$overall_mm_fraction
    overall_ff <- prevalence_grids$overall_ff_fraction

    cli::cli_alert_info("Overall same-sex fraction: {round(overall_ss * 100, 2)}%")

    for (yr in years) {
      yr_char <- as.character(yr)
      if (!(yr_char %in% names(marriage_rates))) next

      total_rates <- marriage_rates[[yr_char]]
      grid_ages <- as.integer(rownames(total_rates))
      n_ages <- length(grid_ages)

      # Initialize output matrices
      ss_grid <- matrix(0, nrow = n_ages, ncol = n_ages,
                        dimnames = list(grid_ages, grid_ages))
      mm_grid <- matrix(0, nrow = n_ages, ncol = n_ages,
                        dimnames = list(grid_ages, grid_ages))
      ff_grid <- matrix(0, nrow = n_ages, ncol = n_ages,
                        dimnames = list(grid_ages, grid_ages))

      # Apply prevalence at each cell
      for (i in seq_along(grid_ages)) {
        for (j in seq_along(grid_ages)) {
          a1 <- grid_ages[i]
          a2 <- grid_ages[j]

          # Find prevalence at this age combination
          prev_i <- which(prev_ages == a1)
          prev_j <- which(prev_ages == a2)

          if (length(prev_i) > 0 && length(prev_j) > 0) {
            # Use local prevalence
            ss_grid[i, j] <- total_rates[i, j] * ss_prev[prev_i, prev_j]
            mm_grid[i, j] <- total_rates[i, j] * mm_prev[prev_i, prev_j]
            ff_grid[i, j] <- total_rates[i, j] * ff_prev[prev_i, prev_j]
          } else {
            # Age outside prevalence grid - use overall fraction
            ss_grid[i, j] <- total_rates[i, j] * overall_ss
            mm_grid[i, j] <- total_rates[i, j] * overall_mm
            ff_grid[i, j] <- total_rates[i, j] * overall_ff
          }
        }
      }

      # Opposite-sex is exactly total minus same-sex (guaranteed non-negative)
      os_grid <- total_rates - ss_grid

      opposite_sex[[yr_char]] <- os_grid
      same_sex[[yr_char]] <- ss_grid
      male_male[[yr_char]] <- mm_grid
      female_female[[yr_char]] <- ff_grid
    }

    # Calculate actual resulting same-sex fraction
    sample_yr <- as.character(years[ceiling(length(years)/2)])
    actual_ss_frac <- sum(same_sex[[sample_yr]]) / sum(marriage_rates[[sample_yr]])

    cli::cli_alert_success("Separated rates for {length(opposite_sex)} years using PREVALENCE method")
    cli::cli_alert_info("Resulting same-sex fraction ({sample_yr}): {round(actual_ss_frac * 100, 2)}%")

    result <- list(
      opposite_sex = opposite_sex,
      same_sex = same_sex,
      male_male = male_male,
      female_female = female_female,
      same_sex_fraction = actual_ss_frac,
      overall_ss_fraction = overall_ss,
      method = "prevalence"
    )

  # ==========================================================================
  # METHOD 2: Calculate prevalence from same_sex_data + opposite_sex_grids
  # ==========================================================================
  } else if (!is.null(same_sex_data) && !is.null(opposite_sex_grids) &&
             "average_male_male" %in% names(same_sex_data)) {

    cli::cli_alert_info("Calculating prevalence grids from ACS data...")

    # Calculate prevalence grids
    prev_grids <- calculate_same_sex_prevalence_grids(
      same_sex_grids = same_sex_data,
      opposite_sex_grids = opposite_sex_grids,
      years = same_sex_data$years,
      smooth = TRUE,
      min_count = 10
    )

    # Recursive call with calculated prevalence
    return(separate_marriage_types(
      marriage_rates = marriage_rates,
      prevalence_grids = prev_grids,
      same_sex_fraction = same_sex_fraction,
      years = years
    ))

  # ==========================================================================
  # METHOD 3: Constant fraction fallback
  # ==========================================================================
  } else {
    cli::cli_alert_warning("Using CONSTANT FRACTION separation (prevalence data not available)")
    cli::cli_alert_info("Same-sex fraction: {same_sex_fraction * 100}%")

    # Get male-male/female-female split from same_sex_data if available
    if (!is.null(same_sex_data) && "totals" %in% names(same_sex_data)) {
      totals <- same_sex_data$totals
      total_ss <- sum(totals$marriages)
      mm_share <- sum(totals[sex_combo == "male_male", marriages]) / total_ss
      ff_share <- sum(totals[sex_combo == "female_female", marriages]) / total_ss
      cli::cli_alert_info("Using ACS-derived MM/FF split: {round(mm_share*100, 1)}% / {round(ff_share*100, 1)}%")
    } else {
      # Default from ACS 2015-2022 average (documented in CLAUDE.md)
      mm_share <- 0.459
      ff_share <- 0.541
      cli::cli_alert_info("Using default MM/FF split: {round(mm_share*100, 1)}% / {round(ff_share*100, 1)}%")
    }

    for (yr in years) {
      yr_char <- as.character(yr)
      if (!(yr_char %in% names(marriage_rates))) next

      total_rates <- marriage_rates[[yr_char]]
      grid_ages <- as.integer(rownames(total_rates))
      n_ages <- length(grid_ages)

      # Simple split: same-sex gets fraction, opposite-sex gets remainder
      ss_grid <- total_rates * same_sex_fraction
      os_grid <- total_rates * (1 - same_sex_fraction)

      # Split same-sex into male-male and female-female
      mm_grid <- ss_grid * mm_share
      ff_grid <- ss_grid * ff_share

      opposite_sex[[yr_char]] <- os_grid
      same_sex[[yr_char]] <- ss_grid
      male_male[[yr_char]] <- mm_grid
      female_female[[yr_char]] <- ff_grid
    }

    cli::cli_alert_success("Separated rates for {length(opposite_sex)} years using constant fraction")

    result <- list(
      opposite_sex = opposite_sex,
      same_sex = same_sex,
      male_male = male_male,
      female_female = female_female,
      same_sex_fraction = same_sex_fraction,
      mm_share = mm_share,
      ff_share = ff_share,
      method = "constant_fraction"
    )
  }

  result
}

#' Run complete marriage projection (main entry point)
#'
#' @description
#' Main function orchestrating the complete marriage projection per TR2025
#' Section 1.6. Implements Equations 1.6.1 (age-specific rates) and 1.6.2 (AMR).
#'
#' Steps:
#' 1. Build base MarGrid from 1978-1988 NCHS data
#' 2. Calculate historical rates (1989-2022)
#' 3. Project AMR to ultimate (4,000 by year 25)
#' 4. Scale MarGrid to projected AMR (2023-2099)
#' 5. Apply prior marital status differentials
#' 6. Separate same-sex and opposite-sex rates (optional)
#'
#' @param nchs_marriages_1978_1988 NCHS marriages by age group (1978-1988)
#'   From fetch_nchs_mra_marriages_1978_1988()
#' @param cps_unmarried CPS unmarried population (1978-1995)
#'   From fetch_cps_unmarried_population()
#' @param nchs_subset NCHS MRA subset marriages (1989-1995)
#'   From fetch_nchs_mra_marriages_1989_1995()
#' @param acs_grids List of ACS marriage grids (2007-2022)
#'   From fetch_acs_new_marriages()
#' @param nchs_us_totals NCHS U.S. total marriages (1989-2022)
#'   From fetch_nchs_us_total_marriages()
#' @param standard_pop_by_group 2010 standard population by age group
#'   From get_2010_standard_population()
#' @param prior_status_data Prior marital status data (1979, 1981-88)
#'   From fetch_nchs_marriages_by_prior_status_1978_1988()
#' @param same_sex_data ACS same-sex marriage grids
#' @param same_sex_fraction Numeric: overall same-sex marriage fraction
#' @param config List: configuration object (required). All projection parameters
#'   (ultimate_amr, ultimate_year, end_year, smoothing, convergence, etc.) are
#'   read from config$marriage.
#' @param include_same_sex Logical: separate same-sex rates (default: TRUE)
#' @param include_prior_status Logical: apply prior status differentials (default: TRUE)
#' @param cache_dir Directory for caching results
#' @param force_recompute Force recomputation even if cache exists
#'
#' @return list with:
#'   - historical_rates: Rate matrices for 1989-2022
#'   - projected_rates: Rate matrices for 2023-2099
#'   - amr_historical: Historical AMR values
#'   - amr_projected: Projected AMR values
#'   - rates_by_status: Rates by prior marital status (if requested)
#'   - opposite_sex_rates: Opposite-sex rates (if requested)
#'   - same_sex_rates: Same-sex rates (if requested)
#'   - margrid: Base MarGrid matrix
#'   - standard_pop_grid: 2010 standard population grid
#'   - status_differentials: Prior status differentials
#'
#' @export
run_marriage_projection <- function(nchs_marriages_1978_1988,
                                     cps_unmarried,
                                     nchs_subset,
                                     acs_grids,
                                     nchs_us_totals,
                                     standard_pop_by_group,
                                     prior_status_data = NULL,
                                     same_sex_data = NULL,
                                     same_sex_fraction = 0.045,
                                     config,
                                     include_same_sex = TRUE,
                                     include_prior_status = TRUE,
                                     cache_dir = here::here("data/cache/marriage"),
                                     force_recompute = FALSE) {

  # Check for complete cached result
  cache_file <- file.path(cache_dir, "marriage_projection_complete.rds")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  if (file.exists(cache_file) && !force_recompute) {
    cli::cli_alert_success("Loading cached complete marriage projection")
    cached <- readRDS(cache_file)
    return(cached)
  }

  cli::cli_h1("Running Complete Marriage Projection (TR2025 Section 1.6)")

  start_time <- Sys.time()

  # =========================================================================
  # STEP 1: Build base MarGrid from 1978-1988
  # =========================================================================
  cli::cli_h2("Step 1: Building Base MarGrid (1978-1988)")

  # Read config-driven parameters (config is required)
  if (is.null(config) || is.null(config$marriage)) {
    cli::cli_abort("config with marriage section is required for run_marriage_projection()")
  }
  mc <- config$marriage

  base_smooth_params <- list(
    h_param = require_config_value(config, "marriage", "smoothing", "base", "h_param"),
    w_param = require_config_value(config, "marriage", "smoothing", "base", "w_param")
  )
  hist_smooth_params <- list(
    h_param = require_config_value(config, "marriage", "smoothing", "historical", "h_param"),
    w_param = require_config_value(config, "marriage", "smoothing", "historical", "w_param")
  )
  interp_method <- require_config_value(config, "marriage", "interpolation_method")
  std_year <- require_config_value(config, "marriage", "standard_population_year")
  convergence_exp <- require_config_value(config, "marriage", "convergence_exponent")
  starting_amr_n_years <- require_config_value(config, "marriage", "starting_amr", "n_years")
  starting_amr_weights <- mc$starting_amr$weights
  prior_status_years <- require_config_value(config, "marriage", "prior_status", "years")
  ultimate_amr <- require_config_value(config, "marriage", "ultimate_amr")
  ultimate_year <- require_config_value(config, "marriage", "ultimate_year")
  start_year <- require_config_value(config, "metadata", "projection_period", "start_year")
  end_year <- require_config_value(config, "metadata", "projection_period", "end_year")
  min_age <- require_config_value(config, "marriage", "min_age")
  max_age <- require_config_value(config, "marriage", "max_age")
  acs_start <- require_config_value(config, "marriage", "acs_start")
  acs_end <- require_config_value(config, "marriage", "acs_end")

  margrid_result <- build_base_margrid(
    nchs_marriages = nchs_marriages_1978_1988,
    cps_unmarried = cps_unmarried,
    years = 1978:1988,
    smooth = TRUE,
    smooth_params = base_smooth_params
  )
  base_margrid <- margrid_result$margrid

  # =========================================================================
  # STEP 2: Build standard population grid
  # =========================================================================
  cli::cli_h2("Step 2: Building Standard Population Grid ({std_year})")

  # Ensure year column exists
  std_pop <- data.table::copy(standard_pop_by_group)
  if (!"year" %in% names(std_pop)) {
    std_pop[, year := std_year]
  }

  standard_pop_grid <- build_standard_population_grid(
    unmarried_pop = std_pop,
    std_year = std_year,
    min_age = min_age,
    max_age = max_age
  )

  # =========================================================================
  # STEP 2B: Estimate same-sex marriages by year (TR2025 Step 6)
  # =========================================================================
  same_sex_estimates <- estimate_same_sex_marriages_by_year(
    nchs_us_totals = nchs_us_totals,
    config = config
  )
  cli::cli_alert_info("Same-sex subtraction: {sum(same_sex_estimates$ss_marriages)} total across {nrow(same_sex_estimates[ss_marriages > 0])} years")

  # =========================================================================
  # STEP 3: Calculate historical rates (1989-2022)
  # =========================================================================
  cli::cli_h2("Step 3: Calculating Historical Rates (1989-2022)")

  historical_result <- calculate_historical_period(
    base_margrid = base_margrid,
    nchs_subset = nchs_subset,
    acs_grids = acs_grids,
    nchs_us_totals = nchs_us_totals,
    unmarried_pop_grid = standard_pop_grid,
    acs_start = acs_start,
    acs_end = acs_end,
    cache_dir = cache_dir,
    force_recompute = force_recompute,
    same_sex_estimates = same_sex_estimates
  )

  # =========================================================================
  # STEP 4: Project rates (2023-2099)
  # =========================================================================
  cli::cli_h2("Step 4: Projecting Marriage Rates (2023-{end_year})")

  # Use most recent historical grid as base for projection
  recent_year <- max(as.integer(names(historical_result$rates)))
  projection_base <- historical_result$rates[[as.character(recent_year)]]

  projected_result <- project_marriage_rates(
    base_margrid = projection_base,
    historical_amr = historical_result$amr,
    standard_pop_grid = standard_pop_grid,
    ultimate_amr = ultimate_amr,
    start_year = start_year,
    ultimate_year = ultimate_year,
    end_year = end_year,
    convergence_exp = convergence_exp,
    starting_amr_n_years = starting_amr_n_years,
    starting_amr_weights = starting_amr_weights,
    cache_dir = cache_dir,
    force_recompute = force_recompute
  )

  # Combine all rates
  all_rates <- c(historical_result$rates, projected_result$rates)

  # =========================================================================
  # STEP 5: Prior marital status differentials (optional)
  # =========================================================================
  status_differentials <- NULL
  rates_by_status <- NULL

  if (include_prior_status && !is.null(prior_status_data)) {
    cli::cli_h2("Step 5: Applying Prior Marital Status Differentials")

    status_differentials <- calculate_prior_status_differentials(
      marriages_by_status = prior_status_data,
      years = prior_status_years
    )

    status_result <- apply_prior_status_rates(
      marriage_rates = all_rates,
      status_differentials = status_differentials
    )
    rates_by_status <- status_result$rates_by_status
  } else if (include_prior_status) {
    cli::cli_alert_warning("Prior status data not provided, skipping status differentials")
  }

  # =========================================================================
  # STEP 6: Same-sex separation (optional)
  # =========================================================================
  opposite_sex_rates <- NULL
  same_sex_rates <- NULL
  male_male_rates <- NULL
  female_female_rates <- NULL
  prevalence_grids <- NULL

  if (include_same_sex) {
    cli::cli_h2("Step 6: Separating Same-Sex and Opposite-Sex Rates")

    # Calculate prevalence grids if we have both same-sex and opposite-sex data
    if (!is.null(same_sex_data) && !is.null(acs_grids) &&
        "average_male_male" %in% names(same_sex_data)) {
      cli::cli_alert_info("Calculating same-sex prevalence grids from ACS data...")

      prevalence_grids <- calculate_same_sex_prevalence_grids(
        same_sex_grids = same_sex_data,
        opposite_sex_grids = acs_grids,
        years = intersect(same_sex_data$years, as.integer(names(acs_grids))),
        smooth = TRUE,
        min_count = 10
      )
    }

    # Separate using prevalence-based method (or fallback)
    sex_result <- separate_marriage_types(
      marriage_rates = all_rates,
      prevalence_grids = prevalence_grids,
      same_sex_data = same_sex_data,
      opposite_sex_grids = acs_grids,
      same_sex_fraction = same_sex_fraction
    )
    opposite_sex_rates <- sex_result$opposite_sex
    same_sex_rates <- sex_result$same_sex
    male_male_rates <- sex_result$male_male
    female_female_rates <- sex_result$female_female
  }

  # =========================================================================
  # COMPILE RESULTS
  # =========================================================================
  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)

  result <- list(
    # Rate outputs
    historical_rates = historical_result$rates,
    projected_rates = projected_result$rates,
    all_rates = all_rates,

    # AMR outputs
    amr_historical = historical_result$amr,
    amr_projected = projected_result$amr,
    starting_amr = projected_result$starting_amr,
    ultimate_amr = ultimate_amr,
    ultimate_year = ultimate_year,

    # Prior status outputs
    rates_by_status = rates_by_status,
    status_differentials = status_differentials,

    # Same-sex outputs
    opposite_sex_rates = opposite_sex_rates,
    same_sex_rates = same_sex_rates,
    male_male_rates = male_male_rates,
    female_female_rates = female_female_rates,

    # Reference data
    margrid = base_margrid,
    standard_pop_grid = standard_pop_grid,

    # Metadata
    projection_years = start_year:end_year,
    historical_years = as.integer(names(historical_result$rates)),
    computed_at = Sys.time(),
    elapsed_minutes = as.numeric(elapsed)
  )

  # Cache complete result
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached complete projection to {cache_file}")

  cli::cli_h1("Marriage Projection Complete")
  cli::cli_alert_success("Historical: {length(result$historical_years)} years ({min(result$historical_years)}-{max(result$historical_years)})")
  cli::cli_alert_success("Projected: {length(result$projection_years)} years ({min(result$projection_years)}-{max(result$projection_years)})")
  cli::cli_alert_info("Starting AMR: {round(result$starting_amr, 1)}")
  cli::cli_alert_info("Ultimate AMR: {result$ultimate_amr} (by {result$ultimate_year})")
  cli::cli_alert_info("Elapsed time: {elapsed} minutes")

  result
}

#' Divorce Subprocess
#'
#' @description
#' Projects annual age-specific divorce rates by age-of-husband crossed with
#' age-of-wife for the Social Security area population.
#'
#' Implements TR2025 Section 1.7 DIVORCE equations:
#' - Equation 1.7.1: Age-specific divorce rates d̂_{x,y}^z
#' - Equation 1.7.2: Age-adjusted central divorce rate ADR^z
#' - Equation 1.7.3: Historical rate calculation
#'
#' @name divorce
NULL

# =============================================================================
# CONSTANTS
# =============================================================================

#' Age range for DivGrid (TR2025: ages 14-100+)
#' @keywords internal
DIVORCE_MIN_AGE <- 14
DIVORCE_MAX_AGE <- 100

#' Ultimate ADR assumption (TR2025: 1,700 per 100,000 married couples)
#' @keywords internal
DIVORCE_ULTIMATE_ADR <- 1700

#' Years to reach ultimate ADR (TR2025: 25th year of projection)
#' @keywords internal
DIVORCE_ULTIMATE_YEAR_OFFSET <- 25

# =============================================================================
# MARRIED COUPLES GRID FUNCTIONS
# =============================================================================

#' Build married couples grid for a given year
#'
#' @description
#' Creates an 87×87 matrix of married couples by age-of-husband × age-of-wife
#' using the geometric mean approach: P_{x,y}^z = sqrt(married_male_x × married_female_y)
#'
#' This is the denominator for divorce rate calculations (Equation 1.7.3).
#'
#' @param marital_pop data.table with columns: year, age, sex, marital_status, population
#' @param target_year Integer year for which to build the grid
#' @param min_age Integer minimum age (default: 14)
#' @param max_age Integer maximum age (default: 100)
#'
#' @return Matrix (87×87) with married couples, rows=husband age, cols=wife age
#'
#' @details
#' Per TR2025, the married population by age-of-husband × age-of-wife is
#' computed using geometric means of married males and females at each age.
#' This assumes independence in age distribution, which is a simplification.
#'
#' @export
build_married_couples_grid <- function(marital_pop,
                                        target_year,
                                        min_age = DIVORCE_MIN_AGE,
                                        max_age = DIVORCE_MAX_AGE) {
  checkmate::assert_data_table(marital_pop)
  checkmate::assert_integerish(target_year, len = 1)

  # Filter to target year and married status
  married <- marital_pop[year == target_year & marital_status == "married"]

  if (nrow(married) == 0) {
    cli::cli_abort("No married population data for year {target_year}")
  }

  # Aggregate by age and sex (sum across orientations if present)
  married_by_age_sex <- married[, .(population = sum(population, na.rm = TRUE)),
                                 by = .(age, sex)]

  # Get male and female populations
  male_pop <- married_by_age_sex[sex == "male"]
  female_pop <- married_by_age_sex[sex == "female"]

  # Create lookup vectors
  ages <- min_age:max_age
  n_ages <- length(ages)

  male_lookup <- rep(0, n_ages)
  names(male_lookup) <- as.character(ages)
  for (i in seq_len(nrow(male_pop))) {
    a <- male_pop$age[i]
    if (a >= min_age && a <= max_age) {
      male_lookup[as.character(a)] <- male_pop$population[i]
    }
  }

  female_lookup <- rep(0, n_ages)
  names(female_lookup) <- as.character(ages)
  for (i in seq_len(nrow(female_pop))) {
    a <- female_pop$age[i]
    if (a >= min_age && a <= max_age) {
      female_lookup[as.character(a)] <- female_pop$population[i]
    }
  }

  # Build grid using geometric means
  # P_{x,y} = sqrt(married_male_x × married_female_y)
  grid <- matrix(0, nrow = n_ages, ncol = n_ages)
  rownames(grid) <- ages
  colnames(grid) <- ages

  for (i in seq_along(ages)) {
    h_pop <- male_lookup[i]
    for (j in seq_along(ages)) {
      w_pop <- female_lookup[j]
      grid[i, j] <- sqrt(h_pop * w_pop)
    }
  }

  # Store totals as attributes for validation
  attr(grid, "total_married_male") <- sum(male_lookup)
  attr(grid, "total_married_female") <- sum(female_lookup)
  attr(grid, "year") <- target_year

  grid
}


#' Build standard married couples population (P^S_{x,y})
#'
#' @description
#' Creates the standard population for ADR calculation per TR2025:
#' "The standard population is based on the averaged 2009 and 2010
#' December 31 marriage grids from the 2015 TR."
#'
#' For divorce, this is July 1, 2010 married couples by age-of-husband × age-of-wife.
#' We implement this as the average of 2009 and 2010 December 31 grids.
#'
#' @param marital_pop data.table with marital status population
#' @param years Integer vector of years to average (default: c(2009, 2010))
#' @param min_age Minimum age (default: 14)
#' @param max_age Maximum age (default: 100)
#'
#' @return Matrix (87×87) with standard population of married couples
#'
#' @export
build_standard_married_population <- function(marital_pop,
                                               years = c(2009, 2010),
                                               min_age = DIVORCE_MIN_AGE,
                                               max_age = DIVORCE_MAX_AGE) {
  # Build grid for each year and average
  grids <- lapply(years, function(yr) {
    build_married_couples_grid(marital_pop, yr, min_age, max_age)
  })

  # Average the grids
  n_ages <- max_age - min_age + 1
  avg_grid <- matrix(0, nrow = n_ages, ncol = n_ages)

  for (g in grids) {
    avg_grid <- avg_grid + g
  }
  avg_grid <- avg_grid / length(grids)

  # Set row/col names
  ages <- min_age:max_age
  rownames(avg_grid) <- ages
  colnames(avg_grid) <- ages

  # Calculate totals from averaged grids
  total_male <- mean(sapply(grids, function(g) attr(g, "total_married_male")))
  total_female <- mean(sapply(grids, function(g) attr(g, "total_married_female")))

  attr(avg_grid, "total_married_male") <- total_male
  attr(avg_grid, "total_married_female") <- total_female
  attr(avg_grid, "standard_years") <- years

  avg_grid
}


# =============================================================================
# SS AREA ADJUSTMENT FUNCTIONS
# =============================================================================

#' Get SS area adjustment factor for divorce
#'
#' @description
#' Calculates the adjustment factor to convert US divorces to SS area divorces.
#' Per TR2025: SS_area_divorces = US_divorces × (1 + PR_VI_ratio) × ss_area_factor
#'
#' The SS area factor = SS_area_population / (US_resident + armed_forces_overseas)
#'
#' This function reuses the marriage subprocess logic since the factor is
#' the same for both.
#'
#' @param target_year Integer year
#' @param cache_dir Character path to cache directory
#'
#' @return Numeric SS area adjustment factor (typically 1.01-1.03)
#'
#' @export
get_divorce_ss_area_factor <- function(target_year,
                                        cache_dir = here::here("data/cache")) {
  # Reuse the marriage subprocess function
  # The SS area factor is the same: total / census_usaf
  get_ss_area_factor(target_year, cache_dir)
}


#' Get SS area adjustment factors for multiple years
#'
#' @param years Integer vector of years
#' @param cache_dir Character path to cache directory
#'
#' @return Named numeric vector with factors keyed by year
#'
#' @export
get_divorce_ss_area_factors <- function(years,
                                         cache_dir = here::here("data/cache")) {
  # Reuse the marriage subprocess function
  get_ss_area_factors(years, cache_dir)
}


# =============================================================================
# POPULATION TOTALS FOR SS AREA CALCULATION
# =============================================================================

#' Get population totals for SS area adjustment
#'
#' @description
#' Returns population totals needed for SS area divorce adjustment per TR2025:
#' - Total July 1 SS area population
#' - Total July 1 US resident + armed forces overseas
#' - Total July 1 PR + VI population
#'
#' Only specific years are used: 1979-1988, 1998-2000, 2008-2022
#'
#' @param years Integer vector of years
#' @param cache_dir Character path to cache directory
#'
#' @return data.table with columns: year, ss_area_pop, us_resident_pop, pr_vi_pop, ss_area_factor
#'
#' @export
get_population_totals_for_divorce <- function(years,
                                               cache_dir = here::here("data/cache")) {
  # Load historical population data
  cache_file <- file.path(cache_dir, "historical_population", "ss_population_1940_2022.rds")

  if (!file.exists(cache_file)) {
    cli::cli_abort("Historical population cache not found at {cache_file}")
  }

  pop_data <- readRDS(cache_file)

  if (!"components" %in% names(pop_data)) {
    cli::cli_abort("Historical population cache missing components")
  }

  components <- data.table::as.data.table(pop_data$components)

  # Calculate SS area factor
  components[, ss_area_factor := total / census_usaf]

  # Select relevant columns and filter to requested years
  result <- components[year %in% years, .(
    year = year,
    ss_area_pop = total,
    us_resident_pop = census_usaf,
    territories_pop = territories,
    ss_area_factor = ss_area_factor
  )]

  data.table::setorder(result, year)
  result
}


# =============================================================================
# ADR CALCULATION (EQUATION 1.7.2)
# =============================================================================

#' Calculate age-adjusted divorce rate (ADR)
#'
#' @description
#' Calculates the age-adjusted central divorce rate using Equation 1.7.2:
#'
#' ADR^z = sum(P^S_{x,y} × d̂_{x,y}^z) / sum(P^S_{x,y})
#'
#' Where:
#' - P^S_{x,y} = standard population of married couples (July 1, 2010)
#' - d̂_{x,y}^z = age-specific divorce rates for year z
#'
#' Note: When using geometric means for P^S_{x,y}, the denominator is
#' sqrt(total_married_male × total_married_female), not sum(P^S_{x,y}).
#'
#' @param rates Matrix (87×87) of divorce rates (per 100,000)
#' @param standard_pop Matrix (87×87) of standard population (geometric means)
#'
#' @return Numeric ADR value (per 100,000 married couples)
#'
#' @export
calculate_adr <- function(rates, standard_pop) {
  checkmate::assert_matrix(rates, mode = "numeric")
  checkmate::assert_matrix(standard_pop, mode = "numeric")

  if (!all(dim(rates) == dim(standard_pop))) {
    cli::cli_abort("Rate matrix and standard population must have same dimensions")
  }

  # Expected divorces (numerator)
  # Rates are per 100,000, so divide by 100,000 to get actual divorces
  expected_divorces <- sum(standard_pop * rates, na.rm = TRUE) / 100000

  # Get total married male and female from attributes if available
  total_male <- attr(standard_pop, "total_married_male")
  total_female <- attr(standard_pop, "total_married_female")

  if (!is.null(total_male) && !is.null(total_female)) {
    # Use exact denominator: sqrt(total_male × total_female)
    # This matches how the standard_pop grid is constructed (geometric means)
    denominator <- sqrt(total_male * total_female)
  } else {
    # Estimate from the grid
    # P_{x,y}^S = sqrt(male_x × female_y)
    # For the total, we need sqrt(total_male × total_female)
    # Approximate as sum(P) / n_ages (similar to marriage subprocess)
    n_ages <- nrow(rates)
    total_P <- sum(standard_pop, na.rm = TRUE)
    denominator <- total_P / n_ages
  }

  if (denominator == 0) {
    cli::cli_abort("Standard population denominator is zero")
  }

  # ADR = expected divorces / total couples × 100,000
  adr <- (expected_divorces / denominator) * 100000

  adr
}


#' Calculate ADR from DivGrid
#'
#' @description
#' Convenience function to calculate ADR from a DivGrid matrix
#' using the standard married population.
#'
#' @param divgrid Matrix of divorce rates
#' @param standard_pop Standard married population (from build_standard_married_population)
#'
#' @return Numeric ADR value
#'
#' @export
calculate_adr_from_divgrid <- function(divgrid, standard_pop) {
  calculate_adr(divgrid, standard_pop)
}


# =============================================================================
# DIVORCE RATE CALCULATION (EQUATION 1.7.3)
# =============================================================================

#' Calculate divorce rates from divorces and married population
#'
#' @description
#' Calculates age-specific divorce rates using Equation 1.7.3:
#'
#' d̂_{x,y}^z = D̂_{x,y}^z / P_{x,y}^z
#'
#' Where:
#' - D̂_{x,y}^z = divorces in SS area for husband age x, wife age y, year z
#' - P_{x,y}^z = married couples in SS area at those ages
#'
#' @param divorces Matrix or data.table of divorce counts by husband/wife age
#' @param married_couples Matrix of married couples by husband/wife age
#'
#' @return Matrix of divorce rates (per 100,000 married couples)
#'
#' @export
calculate_divorce_rates <- function(divorces, married_couples) {
  # Handle data.table input
  if (data.table::is.data.table(divorces)) {
    # Convert to matrix
    ages <- DIVORCE_MIN_AGE:DIVORCE_MAX_AGE
    n_ages <- length(ages)

    div_matrix <- matrix(0, nrow = n_ages, ncol = n_ages)
    rownames(div_matrix) <- ages
    colnames(div_matrix) <- ages

    for (i in seq_len(nrow(divorces))) {
      h_age <- divorces$husband_age[i]
      w_age <- divorces$wife_age[i]
      div_count <- divorces$divorces[i]

      if (h_age >= DIVORCE_MIN_AGE && h_age <= DIVORCE_MAX_AGE &&
          w_age >= DIVORCE_MIN_AGE && w_age <= DIVORCE_MAX_AGE) {
        h_idx <- h_age - DIVORCE_MIN_AGE + 1
        w_idx <- w_age - DIVORCE_MIN_AGE + 1
        div_matrix[h_idx, w_idx] <- div_count
      }
    }
    divorces <- div_matrix
  }

  checkmate::assert_matrix(divorces, mode = "numeric")
  checkmate::assert_matrix(married_couples, mode = "numeric")

  if (!all(dim(divorces) == dim(married_couples))) {
    cli::cli_abort("Divorce matrix and married couples must have same dimensions")
  }

  # Calculate rates (per 100,000)
  # Avoid division by zero
  rates <- divorces / pmax(married_couples, 1e-10) * 100000

  # Set rates to 0 where there are no married couples
  rates[married_couples < 1] <- 0

  rates
}


# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

#' Validate married couples grid
#'
#' @description
#' Checks that the married couples grid has expected properties.
#'
#' @param grid Matrix to validate
#'
#' @return List with validation results
#'
#' @export
validate_married_couples_grid <- function(grid) {
  results <- list(
    checks = list(),
    passed = 0,
    failed = 0
  )

  # Check 1: Correct dimensions (87 × 87)
  expected_dim <- c(87, 87)
  actual_dim <- dim(grid)
  check1_passed <- all(actual_dim == expected_dim)
  results$checks$dimensions <- list(
    passed = check1_passed,
    message = sprintf("Dimensions: %d × %d (expected %d × %d)",
                      actual_dim[1], actual_dim[2], expected_dim[1], expected_dim[2])
  )
  if (check1_passed) results$passed <- results$passed + 1 else results$failed <- results$failed + 1

  # Check 2: All values non-negative
  check2_passed <- all(grid >= 0, na.rm = TRUE)
  results$checks$non_negative <- list(
    passed = check2_passed,
    message = sprintf("Non-negative values: %s", if (check2_passed) "PASS" else "FAIL")
  )
  if (check2_passed) results$passed <- results$passed + 1 else results$failed <- results$failed + 1

  # Check 3: Total grid sum reasonable
  total <- sum(grid, na.rm = TRUE)
  # Note: Grid contains geometric means (sqrt(male × female) for each cell)
  # Sum across 87×87 cells will be much larger than actual married population
  # With ~62M married males and ~62M married females, geometric mean per cell
  # averages around sqrt(62M/87)^2 ≈ 714K per cell × 7569 cells ≈ 5.4B
  # Accept range 1B to 10B
  check3_passed <- total > 1e9 && total < 1e10
  results$checks$total_reasonable <- list(
    passed = check3_passed,
    message = sprintf("Grid sum: %s (expected 1B-10B)", format(total, big.mark = ","))
  )
  if (check3_passed) results$passed <- results$passed + 1 else results$failed <- results$failed + 1

  # Check 4: Peak values at reasonable ages (25-50)
  max_idx <- which(grid == max(grid, na.rm = TRUE), arr.ind = TRUE)[1, ]
  peak_h_age <- as.integer(rownames(grid)[max_idx[1]])
  peak_w_age <- as.integer(colnames(grid)[max_idx[2]])
  check4_passed <- peak_h_age >= 25 && peak_h_age <= 55 &&
    peak_w_age >= 25 && peak_w_age <= 55
  results$checks$peak_ages <- list(
    passed = check4_passed,
    message = sprintf("Peak at husband age %d, wife age %d", peak_h_age, peak_w_age)
  )
  if (check4_passed) results$passed <- results$passed + 1 else results$failed <- results$failed + 1

  results
}


#' Validate population data for divorce subprocess
#'
#' @description
#' Validates all population data needed for the divorce subprocess.
#'
#' @param marital_pop Marital status population data
#' @param cache_dir Cache directory path
#'
#' @return List with validation results
#'
#' @export
validate_divorce_population_data <- function(marital_pop,
                                              cache_dir = here::here("data/cache")) {
  results <- list(
    checks = list(),
    passed = 0,
    failed = 0
  )

  cli::cli_h2("Divorce Population Data Validation")

  # Check 1: Marital population has required years
  required_years <- c(2009, 2010)  # For standard population
  available_years <- unique(marital_pop$year)
  check1_passed <- all(required_years %in% available_years)
  results$checks$standard_pop_years <- list(
    passed = check1_passed,
    message = sprintf("Standard population years (2009, 2010): %s",
                      if (check1_passed) "Available" else "Missing")
  )
  if (check1_passed) {
    cli::cli_alert_success(results$checks$standard_pop_years$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_danger(results$checks$standard_pop_years$message)
    results$failed <- results$failed + 1
  }

  # Check 2: Standard population grid can be built
  tryCatch({
    std_pop <- build_standard_married_population(marital_pop)
    val <- validate_married_couples_grid(std_pop)
    check2_passed <- val$failed == 0
    results$checks$standard_pop_grid <- list(
      passed = check2_passed,
      message = sprintf("Standard population grid: %d/%d checks passed",
                        val$passed, val$passed + val$failed)
    )
    if (check2_passed) {
      cli::cli_alert_success(results$checks$standard_pop_grid$message)
      results$passed <- results$passed + 1
    } else {
      cli::cli_alert_warning(results$checks$standard_pop_grid$message)
      results$failed <- results$failed + 1
    }
  }, error = function(e) {
    results$checks$standard_pop_grid <- list(
      passed = FALSE,
      message = sprintf("Standard population grid: Failed - %s", e$message)
    )
    cli::cli_alert_danger(results$checks$standard_pop_grid$message)
    results$failed <- results$failed + 1
  })

  # Check 3: SS area factors available
  tryCatch({
    factors <- get_divorce_ss_area_factors(c(1988, 2010, 2022), cache_dir)
    check3_passed <- length(factors) == 3 && all(factors > 1.0) && all(factors < 1.1)
    results$checks$ss_area_factors <- list(
      passed = check3_passed,
      message = sprintf("SS area factors: %.4f - %.4f (expected 1.01-1.05)",
                        min(factors), max(factors))
    )
    if (check3_passed) {
      cli::cli_alert_success(results$checks$ss_area_factors$message)
      results$passed <- results$passed + 1
    } else {
      cli::cli_alert_warning(results$checks$ss_area_factors$message)
      results$failed <- results$failed + 1
    }
  }, error = function(e) {
    results$checks$ss_area_factors <- list(
      passed = FALSE,
      message = sprintf("SS area factors: Failed - %s", e$message)
    )
    cli::cli_alert_danger(results$checks$ss_area_factors$message)
    results$failed <- results$failed + 1
  })

  # Check 4: Population totals available for key years
  # Expected: 1979-1988 (10), 1998-2000 (3), 2008-2022 (15) = 28 years
  key_years <- c(1979:1988, 1998:2000, 2008:2022)
  tryCatch({
    pop_totals <- get_population_totals_for_divorce(key_years, cache_dir)
    n_years <- nrow(pop_totals)
    check4_passed <- n_years >= 25  # Allow some missing years
    results$checks$population_totals <- list(
      passed = check4_passed,
      message = sprintf("Population totals: %d years available", n_years)
    )
    if (check4_passed) {
      cli::cli_alert_success(results$checks$population_totals$message)
      results$passed <- results$passed + 1
    } else {
      cli::cli_alert_warning(results$checks$population_totals$message)
      results$failed <- results$failed + 1
    }
  }, error = function(e) {
    results$checks$population_totals <- list(
      passed = FALSE,
      message = sprintf("Population totals: Failed - %s", e$message)
    )
    cli::cli_alert_danger(results$checks$population_totals$message)
    results$failed <- results$failed + 1
  })

  cli::cli_alert_info("Passed: {results$passed}/{results$passed + results$failed}")

  invisible(results)
}


# =============================================================================
# DIVGRID BUILDING FUNCTIONS (TR2025 Section 1.7.c)
# =============================================================================

#' Convert divorce counts to matrix format
#'
#' @description
#' Converts a data.table of divorce counts by husband/wife age to an 87×87 matrix.
#'
#' @param divorces data.table with columns: husband_age, wife_age, divorces
#' @param min_age Minimum age (default: 14)
#' @param max_age Maximum age (default: 100)
#'
#' @return Matrix (87×87) with divorce counts
#'
#' @keywords internal
convert_divorces_to_matrix <- function(divorces,
                                        min_age = DIVORCE_MIN_AGE,
                                        max_age = DIVORCE_MAX_AGE) {
  ages <- min_age:max_age
  n_ages <- length(ages)

  # Initialize matrix
  mat <- matrix(0, nrow = n_ages, ncol = n_ages)
  rownames(mat) <- ages
  colnames(mat) <- ages

  # Fill in values
  for (i in seq_len(nrow(divorces))) {
    h_age <- divorces$husband_age[i]
    w_age <- divorces$wife_age[i]
    div_count <- divorces$divorces[i]

    # Handle ages outside range
    if (h_age < min_age) h_age <- min_age
    if (h_age > max_age) h_age <- max_age
    if (w_age < min_age) w_age <- min_age
    if (w_age > max_age) w_age <- max_age

    h_idx <- h_age - min_age + 1
    w_idx <- w_age - min_age + 1
    mat[h_idx, w_idx] <- mat[h_idx, w_idx] + div_count
  }

  mat
}


#' Inflate DRA divorces to SS area estimate
#'
#' @description
#' Inflates Divorce Registration Area (DRA) divorce counts to represent
#' an estimate of total divorces in the Social Security area.
#'
#' Per TR2025: "These data are then inflated to represent an estimate of the
#' total number of divorces in the Social Security area."
#'
#' The inflation is based on:
#' 1. Total U.S. divorces (from NCHS)
#' 2. PR + VI divorces
#' 3. SS area adjustment factor
#'
#' @param dra_divorces Matrix or numeric: DRA divorce counts
#' @param dra_total Numeric: total DRA divorces
#' @param us_total Numeric: total U.S. divorces (from NCHS)
#' @param pr_vi_divorces Numeric: PR + VI divorces (default: 0)
#' @param ss_area_factor Numeric: SS area adjustment factor
#'
#' @return Inflated divorce counts (same structure as input)
#'
#' @export
inflate_divorces_to_ss_area <- function(dra_divorces,
                                         dra_total,
                                         us_total,
                                         pr_vi_divorces = 0,
                                         ss_area_factor = 1.0) {
  # Calculate inflation factor
  # SS area divorces = (US divorces + PR/VI divorces) × ss_area_factor
  ss_area_total <- (us_total + pr_vi_divorces) * ss_area_factor

  # Inflation ratio: SS area / DRA
  inflation_factor <- ss_area_total / dra_total

  # Apply to divorce counts
  dra_divorces * inflation_factor
}


#' Calculate divorce rates for a single year (Equation 1.7.3)
#'
#' @description
#' Calculates age-specific divorce rates for a single year using:
#' d̂_{x,y}^z = D̂_{x,y}^z / P_{x,y}^z
#'
#' Per TR2025: "the age-specific divorce rates... are calculated for the
#' period 1979-1988 by taking the number of divorces (inflated to represent
#' the Social Security area) and dividing by the married population."
#'
#' @param divorces_matrix Matrix of SS area divorce counts
#' @param married_grid Matrix of married couples by age
#'
#' @return Matrix of divorce rates (per 100,000 married couples)
#'
#' @export
calculate_year_divorce_rates <- function(divorces_matrix, married_grid) {
  checkmate::assert_matrix(divorces_matrix, mode = "numeric")
  checkmate::assert_matrix(married_grid, mode = "numeric")

  if (!all(dim(divorces_matrix) == dim(married_grid))) {
    cli::cli_abort("Divorce matrix and married grid must have same dimensions")
  }

  # Calculate rates per 100,000
  # Avoid division by zero
  rates <- divorces_matrix / pmax(married_grid, 1e-10) * 100000

  # Set rates to 0 where there are no married couples
  rates[married_grid < 1] <- 0

  # Cap extreme rates (> 50,000 per 100,000 is implausible)
  rates[rates > 50000] <- 50000

  rates
}


#' Apply Whittaker-Henderson 2D graduation to rate matrix
#'
#' @description
#' Applies two-dimensional Whittaker-Henderson smoothing to graduate
#' divorce rates. Per TR2025 footnote 13: "Using a two-dimensional
#' Whittaker-Henderson method of graduation."
#'
#' This function wraps the implementation from the marriage subprocess.
#'
#' @param grid Matrix to smooth
#' @param h_param Smoothing parameter for husband dimension (default: 1)
#' @param w_param Smoothing parameter for wife dimension (default: 1)
#' @param max_iter Maximum iterations (default: 100)
#' @param tol Convergence tolerance (default: 1e-6)
#'
#' @return Smoothed matrix
#'
#' @export
graduate_divgrid <- function(grid, h_param = 1, w_param = 1,
                              max_iter = 100, tol = 1e-6) {
  # Use the Whittaker-Henderson function from marriage.R
  whittaker_henderson_2d(grid, h_param, w_param, max_iter, tol)
}


#' Build base DivGrid from 1979-1988 data
#'
#' @description
#' Creates the base 87×87 DivGrid matrix from NCHS DRA data (1979-1988).
#'
#' Per TR2025 Section 1.7.c:
#' 1. Disaggregate NCHS age-group data to single years (using H.S. Beers)
#'    - Note: We already have single-year data, so this step is skipped
#' 2. Calculate age-specific rates for each year (Eq 1.7.3)
#' 3. Average rates across 1979-1988
#' 4. Graduate using 2D Whittaker-Henderson
#' 5. Store as 87×87 DivGrid matrix
#'
#' @param detailed_divorces data.table from fetch_nchs_dra_divorces_detailed_1979_1988()
#' @param marital_pop data.table of marital status population
#' @param us_totals data.table from fetch_nchs_us_total_divorces()
#' @param cache_dir Character path to cache directory
#' @param smooth Logical: apply Whittaker-Henderson graduation (default: TRUE)
#' @param h_param Smoothing parameter for husband dimension (default: 1)
#' @param w_param Smoothing parameter for wife dimension (default: 1)
#'
#' @return List with:
#'   - divgrid: 87×87 matrix of divorce rates (per 100,000)
#'   - yearly_rates: List of rate matrices by year
#'   - yearly_totals: data.table of divorce totals by year
#'   - metadata: List with processing details
#'
#' @export
build_base_divgrid <- function(detailed_divorces,
                                marital_pop,
                                us_totals,
                                cache_dir = here::here("data/cache"),
                                smooth = TRUE,
                                h_param = 1,
                                w_param = 1) {
  cli::cli_h2("Building Base DivGrid (1979-1988)")

  years <- 1979:1988
  n_years <- length(years)

  # Get SS area factors
  ss_factors <- get_divorce_ss_area_factors(years, cache_dir)

  # Get PR/VI divorces (only have 1988)
  # For other years, estimate based on 1988 ratio
  pr_vi_1988 <- 13380  # Approximate from available data

  # Initialize storage
  yearly_rates <- list()
  yearly_totals <- data.table::data.table(
    year = integer(),
    dra_divorces = numeric(),
    us_divorces = numeric(),
    ss_area_divorces = numeric(),
    inflation_factor = numeric()
  )

  # Initialize accumulator for averaging
  ages <- DIVORCE_MIN_AGE:DIVORCE_MAX_AGE
  n_ages <- length(ages)
  rate_sum <- matrix(0, nrow = n_ages, ncol = n_ages)
  rownames(rate_sum) <- ages
  colnames(rate_sum) <- ages

  cli::cli_progress_bar("Processing years", total = n_years)

  for (yr in years) {
    cli::cli_progress_update()

    # Get divorces for this year
    yr_divorces <- detailed_divorces[year == yr]
    if (nrow(yr_divorces) == 0) {
      cli::cli_warn("No divorce data for year {yr}")
      next
    }

    # Convert to matrix
    div_matrix <- convert_divorces_to_matrix(yr_divorces)
    dra_total <- sum(div_matrix, na.rm = TRUE)

    # Get US total for this year
    us_total <- us_totals[year == yr, total_divorces]
    if (length(us_total) == 0 || is.na(us_total)) {
      cli::cli_warn("No US total for year {yr}, using DRA estimate")
      us_total <- dra_total / 0.48  # Approximate DRA coverage
    }

    # Estimate PR/VI for this year (scale by US total ratio to 1988)
    pr_vi <- pr_vi_1988 * (us_total / us_totals[year == 1988, total_divorces])

    # Get SS area factor
    ss_factor <- ss_factors[as.character(yr)]

    # Inflate DRA to SS area
    ss_div_matrix <- inflate_divorces_to_ss_area(
      div_matrix, dra_total, us_total, pr_vi, ss_factor
    )

    # Get married couples for this year
    married_grid <- build_married_couples_grid(marital_pop, yr)

    # Calculate rates
    rates <- calculate_year_divorce_rates(ss_div_matrix, married_grid)

    # Store
    yearly_rates[[as.character(yr)]] <- rates
    rate_sum <- rate_sum + rates

    # Record totals
    yearly_totals <- rbind(yearly_totals, data.table::data.table(
      year = yr,
      dra_divorces = dra_total,
      us_divorces = us_total,
      ss_area_divorces = sum(ss_div_matrix, na.rm = TRUE),
      inflation_factor = sum(ss_div_matrix, na.rm = TRUE) / dra_total
    ))
  }

  cli::cli_progress_done()

  # Average rates across years
  avg_rates <- rate_sum / n_years

  # Apply graduation (smoothing)
  if (smooth) {
    cli::cli_alert("Applying Whittaker-Henderson graduation...")
    divgrid <- graduate_divgrid(avg_rates, h_param, w_param)
  } else {
    divgrid <- avg_rates
  }

  # Ensure non-negative
  divgrid[divgrid < 0] <- 0

  # Calculate summary statistics
  total_rate <- sum(divgrid, na.rm = TRUE)
  max_rate <- max(divgrid, na.rm = TRUE)
  max_idx <- which(divgrid == max_rate, arr.ind = TRUE)[1, ]
  peak_h_age <- as.integer(rownames(divgrid)[max_idx[1]])
  peak_w_age <- as.integer(colnames(divgrid)[max_idx[2]])

  cli::cli_alert_success("Built base DivGrid: {n_ages}×{n_ages} matrix")
  cli::cli_alert_info("Peak rate: {round(max_rate, 1)} at husband age {peak_h_age}, wife age {peak_w_age}")

  metadata <- list(
    base_years = years,
    n_years_averaged = n_years,
    smoothed = smooth,
    h_param = h_param,
    w_param = w_param,
    peak_rate = max_rate,
    peak_husband_age = peak_h_age,
    peak_wife_age = peak_w_age,
    created = Sys.time()
  )

  list(
    divgrid = divgrid,
    yearly_rates = yearly_rates,
    yearly_totals = yearly_totals,
    metadata = metadata
  )
}


#' Scale DivGrid to match target total divorces
#'
#' @description
#' Proportionally scales all DivGrid rates so that when applied to the
#' married population, they yield the target total divorces.
#'
#' Per TR2025: "The rates in DivGrid are then proportionally adjusted so
#' that they would yield an estimate of the total number of divorces."
#'
#' @param divgrid Matrix of divorce rates
#' @param married_grid Matrix of married couples
#' @param target_total Target total divorces in SS area
#'
#' @return Scaled DivGrid matrix
#'
#' @export
scale_divgrid_to_total <- function(divgrid, married_grid, target_total) {
  checkmate::assert_matrix(divgrid, mode = "numeric")
  checkmate::assert_matrix(married_grid, mode = "numeric")
  checkmate::assert_number(target_total, lower = 0)

  # Calculate expected divorces with current rates
  # Rates are per 100,000, so divide by 100,000
  expected_divorces <- sum(married_grid * divgrid, na.rm = TRUE) / 100000

  if (expected_divorces == 0) {
    cli::cli_warn("Expected divorces is zero, cannot scale")
    return(divgrid)
  }

  # Calculate scaling factor
  scale_factor <- target_total / expected_divorces

  # Apply scaling
  scaled_grid <- divgrid * scale_factor

  attr(scaled_grid, "scale_factor") <- scale_factor
  attr(scaled_grid, "target_total") <- target_total

  scaled_grid
}


#' Scale DivGrid to produce target ADR
#'
#' @description
#' Proportionally scales DivGrid rates to produce a target age-adjusted
#' divorce rate when applied to the standard population.
#'
#' Per TR2025: "the age-of-husband-age-of-wife-specific rates in DivGrid
#' are adjusted proportionally so as to produce the age-adjusted rate
#' assumed for that particular year."
#'
#' @param divgrid Base DivGrid matrix
#' @param target_adr Target ADR value (per 100,000)
#' @param standard_pop Standard married population matrix
#'
#' @return Scaled DivGrid matrix
#'
#' @export
scale_divgrid_to_adr <- function(divgrid, target_adr, standard_pop) {
  checkmate::assert_matrix(divgrid, mode = "numeric")
  checkmate::assert_number(target_adr, lower = 0)
  checkmate::assert_matrix(standard_pop, mode = "numeric")

  # Calculate current ADR
  current_adr <- calculate_adr(divgrid, standard_pop)

  if (current_adr == 0) {
    cli::cli_warn("Current ADR is zero, cannot scale")
    return(divgrid)
  }

  # Calculate scaling factor
  scale_factor <- target_adr / current_adr

  # Apply scaling
  scaled_grid <- divgrid * scale_factor

  attr(scaled_grid, "scale_factor") <- scale_factor
  attr(scaled_grid, "target_adr") <- target_adr
  attr(scaled_grid, "achieved_adr") <- calculate_adr(scaled_grid, standard_pop)

  scaled_grid
}


#' Validate DivGrid properties
#'
#' @description
#' Validates that a DivGrid matrix has expected properties.
#'
#' @param divgrid Matrix to validate
#' @param standard_pop Optional standard population for ADR check
#'
#' @return List with validation results
#'
#' @export
validate_divgrid <- function(divgrid, standard_pop = NULL) {
  results <- list(
    checks = list(),
    passed = 0,
    failed = 0
  )

  cli::cli_h2("DivGrid Validation")

  # Check 1: Dimensions (87 × 87)
  expected_dim <- c(87, 87)
  actual_dim <- dim(divgrid)
  check1_passed <- all(actual_dim == expected_dim)
  results$checks$dimensions <- list(
    passed = check1_passed,
    message = sprintf("Dimensions: %d × %d (expected %d × %d)",
                      actual_dim[1], actual_dim[2], expected_dim[1], expected_dim[2])
  )
  if (check1_passed) {
    cli::cli_alert_success(results$checks$dimensions$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_danger(results$checks$dimensions$message)
    results$failed <- results$failed + 1
  }

  # Check 2: All rates non-negative
  check2_passed <- all(divgrid >= 0, na.rm = TRUE)
  results$checks$non_negative <- list(
    passed = check2_passed,
    message = sprintf("Non-negative rates: %s", if (check2_passed) "PASS" else "FAIL")
  )
  if (check2_passed) {
    cli::cli_alert_success(results$checks$non_negative$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_danger(results$checks$non_negative$message)
    results$failed <- results$failed + 1
  }

  # Check 3: Peak rate at reasonable ages (25-50 for both)
  max_rate <- max(divgrid, na.rm = TRUE)
  max_idx <- which(divgrid == max_rate, arr.ind = TRUE)[1, ]
  peak_h_age <- as.integer(rownames(divgrid)[max_idx[1]])
  peak_w_age <- as.integer(colnames(divgrid)[max_idx[2]])
  check3_passed <- peak_h_age >= 20 && peak_h_age <= 50 &&
    peak_w_age >= 20 && peak_w_age <= 50
  results$checks$peak_ages <- list(
    passed = check3_passed,
    message = sprintf("Peak rate %.1f at husband %d, wife %d",
                      max_rate, peak_h_age, peak_w_age)
  )
  if (check3_passed) {
    cli::cli_alert_success(results$checks$peak_ages$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$peak_ages$message)
    results$failed <- results$failed + 1
  }

  # Check 4: Rates decline at extreme ages
  # Young ages (14-17) should have lower rates than peak
  young_rates <- mean(divgrid[1:4, 1:4], na.rm = TRUE)
  check4_passed <- young_rates < max_rate * 0.5
  results$checks$young_ages_lower <- list(
    passed = check4_passed,
    message = sprintf("Young age rates (%.1f) < 50%% of peak: %s",
                      young_rates, if (check4_passed) "PASS" else "FAIL")
  )
  if (check4_passed) {
    cli::cli_alert_success(results$checks$young_ages_lower$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$young_ages_lower$message)
    results$failed <- results$failed + 1
  }

  # Check 5: Old ages (85+) should have lower rates
  old_rates <- mean(divgrid[72:87, 72:87], na.rm = TRUE)  # Ages 85-100
  check5_passed <- old_rates < max_rate * 0.5
  results$checks$old_ages_lower <- list(
    passed = check5_passed,
    message = sprintf("Old age rates (%.1f) < 50%% of peak: %s",
                      old_rates, if (check5_passed) "PASS" else "FAIL")
  )
  if (check5_passed) {
    cli::cli_alert_success(results$checks$old_ages_lower$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$old_ages_lower$message)
    results$failed <- results$failed + 1
  }

  # Check 6: ADR in reasonable range (if standard_pop provided)
  if (!is.null(standard_pop)) {
    adr <- calculate_adr(divgrid, standard_pop)
    # Historical ADR typically 1500-3000 per 100,000
    check6_passed <- adr >= 500 && adr <= 5000
    results$checks$adr_reasonable <- list(
      passed = check6_passed,
      message = sprintf("ADR: %.1f per 100,000 (expected 500-5000)", adr)
    )
    if (check6_passed) {
      cli::cli_alert_success(results$checks$adr_reasonable$message)
      results$passed <- results$passed + 1
    } else {
      cli::cli_alert_warning(results$checks$adr_reasonable$message)
      results$failed <- results$failed + 1
    }
  }

  cli::cli_alert_info("Passed: {results$passed}/{results$passed + results$failed}")

  invisible(results)
}


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

#' Fetch or build base DivGrid with caching
#'
#' @description
#' Loads cached DivGrid if available, otherwise builds from source data.
#' This is the main entry point for obtaining the base DivGrid.
#'
#' @param cache_dir Character path to cache directory
#' @param force Logical: force rebuild even if cached (default: FALSE)
#'
#' @return List with divgrid matrix and metadata
#'
#' @export
fetch_base_divgrid <- function(cache_dir = here::here("data/cache"),
                                force = FALSE) {
  cache_file <- file.path(cache_dir, "divorce", "base_divgrid_1979_1988.rds")

  # Return cached if available
  if (file.exists(cache_file) && !force) {
    cli::cli_alert_success("Loading cached base DivGrid (1979-1988)")
    return(readRDS(cache_file))
  }

  # Build from source data
  cli::cli_alert_info("Building base DivGrid from source data...")

  # Load required data
  marital_pop <- readRDS(file.path(cache_dir, "historical_population",
                                    "ss_population_marital_1940_2022.rds"))
  marital_pop <- data.table::as.data.table(marital_pop)

  detailed_divorces <- fetch_nchs_dra_divorces_detailed_1979_1988(cache_dir = cache_dir)
  us_totals <- fetch_nchs_us_total_divorces(years = 1979:1988)

  # Build DivGrid
  result <- build_base_divgrid(
    detailed_divorces = detailed_divorces,
    marital_pop = marital_pop,
    us_totals = us_totals,
    cache_dir = cache_dir,
    smooth = TRUE
  )

  # Add standard population ADR
  std_pop <- build_standard_married_population(marital_pop)
  result$base_adr <- calculate_adr(result$divgrid, std_pop)
  result$standard_pop <- std_pop

  # Cache result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached base DivGrid to {cache_file}")

  result
}

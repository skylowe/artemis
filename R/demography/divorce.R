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
#' @param rates Matrix (87×87) of divorce rates (per 100,000)
#' @param standard_pop Matrix (87×87) of standard population
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

  # Total standard population (denominator)
  total_std_pop <- sum(standard_pop, na.rm = TRUE)

  if (total_std_pop == 0) {
    cli::cli_abort("Standard population sum is zero")
  }

  # ADR = expected divorces / total couples × 100,000
  adr <- (expected_divorces / total_std_pop) * 100000

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

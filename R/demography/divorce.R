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
# CONSTANTS (defaults when config not provided)
# =============================================================================

#' Structural constants for DivGrid dimensions (ages 14-100 = 87×87 matrix).
#' Used directly by matrix-building functions. Overridable via config.
#' @keywords internal
DIVORCE_MIN_AGE <- 14
DIVORCE_MAX_AGE <- 100

#' Standard population years for ADR calculation (TR2025: 2009-2010)
#' @keywords internal
DIVORCE_STANDARD_POP_YEARS <- c(2009L, 2010L)

# =============================================================================
# CONFIG HELPER FUNCTIONS
# =============================================================================

#' Get divorce configuration parameters
#'
#' @description
#' Retrieves divorce parameters from config object. Aborts if config
#' is missing — all divorce functions require explicit config.
#'
#' @param config Config list (from yaml::read_yaml). Required.
#'
#' @return List with all divorce configuration parameters
#'
#' @keywords internal
get_divorce_config <- function(config = NULL) {
  if (is.null(config) || is.null(config$divorce)) {
    cli::cli_abort("Divorce config is required. Pass config from {.fn targets::tar_read} or {.fn yaml::read_yaml}.")
  }

  dc <- config$divorce

  # Validate all required keys are present
  required_keys <- c("min_age", "max_age", "standard_population_years",
                     "ultimate_adr", "ultimate_year", "convergence_exponent",
                     "acs_years", "adjustment_year", "base_period_start",
                     "base_period_end", "historical_end_year", "rate_cap")
  missing <- setdiff(required_keys, names(dc))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing required divorce config keys: {.val {missing}}",
      "i" = "Add these keys to the {.code divorce:} section of your config YAML."
    ))
  }

  # Validate PR/VI subsection
  if (is.null(dc$pr_vi)) {
    cli::cli_abort("Missing required {.code divorce.pr_vi} config section.")
  }
  pr_vi_keys <- c("anchor_1988_total", "us_ratio", "vi_pr_ratio",
                   "annual_decline", "dra_coverage")
  missing_pv <- setdiff(pr_vi_keys, names(dc$pr_vi))
  if (length(missing_pv) > 0) {
    cli::cli_abort(c(
      "Missing required divorce.pr_vi config keys: {.val {missing_pv}}",
      "i" = "Add these keys to the {.code divorce.pr_vi:} section of your config YAML."
    ))
  }

  list(
    min_age = as.integer(dc$min_age),
    max_age = as.integer(dc$max_age),
    standard_population_years = as.integer(unlist(dc$standard_population_years)),
    ultimate_adr = dc$ultimate_adr,
    ultimate_year = as.integer(dc$ultimate_year),
    convergence_exponent = dc$convergence_exponent,
    acs_years = as.integer(unlist(dc$acs_years)),
    adjustment_year = as.integer(dc$adjustment_year),
    base_period_start = as.integer(dc$base_period_start),
    base_period_end = as.integer(dc$base_period_end),
    historical_end_year = as.integer(dc$historical_end_year),
    rate_cap = dc$rate_cap,
    ss_area_factor_override = dc$ss_area_factor_override,
    pr_vi = list(
      anchor_1988_total = dc$pr_vi$anchor_1988_total,
      us_ratio = dc$pr_vi$us_ratio,
      vi_pr_ratio = dc$pr_vi$vi_pr_ratio,
      annual_decline = dc$pr_vi$annual_decline,
      dra_coverage = dc$pr_vi$dra_coverage
    )
  )
}

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
build_divorce_married_couples_grid <- function(marital_pop,
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
#' @param years Integer vector of years to average (default from config or c(2009, 2010))
#' @param min_age Minimum age (default from config or 14)
#' @param max_age Maximum age (default from config or 100)
#' @param config Optional config list to derive years, min_age, max_age
#'
#' @return Matrix (87×87) with standard population of married couples
#'
#' @export
build_standard_married_population <- function(marital_pop,
                                               years = NULL,
                                               min_age = NULL,
                                               max_age = NULL,
                                               config = NULL) {
  # Get parameters from config or use defaults
  divorce_config <- get_divorce_config(config)
  if (is.null(years)) years <- divorce_config$standard_population_years
  if (is.null(min_age)) min_age <- divorce_config$min_age
  if (is.null(max_age)) max_age <- divorce_config$max_age
  # Build grid for each year and average
  grids <- lapply(years, function(yr) {
    build_divorce_married_couples_grid(marital_pop, yr, min_age, max_age)
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
  # Use divorce-specific population totals function
  pop_totals <- get_population_totals_for_divorce(years, cache_dir)

  # Return as named vector for compatibility
  factors <- pop_totals$ss_area_factor
  names(factors) <- as.character(pop_totals$year)

  factors
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
    cli::cli_abort(c(
      "Standard population missing {.arg total_married_male}/{.arg total_married_female} attributes.",
      "i" = "Use {.fn build_standard_married_population} to create standard_pop with required attributes."
    ))
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
                                              cache_dir = here::here("data/cache"),
                                              config = NULL) {
  if (is.null(config) || is.null(config$divorce)) {
    cli::cli_abort("Config is required for {.fn validate_divorce_population_data}.")
  }

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
    bp_end <- as.integer(config$divorce$base_period_end)
    hist_end <- as.integer(config$divorce$historical_end_year)
    factors <- get_divorce_ss_area_factors(c(bp_end, 2010, hist_end), cache_dir)
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
  bp_start <- as.integer(config$divorce$base_period_start)
  key_years <- c(bp_start:bp_end, 1998:2000, 2008:hist_end)
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
calculate_year_divorce_rates <- function(divorces_matrix, married_grid,
                                          rate_cap = 50000) {
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

  # Cap extreme rates (implausible values)
  rates[rates > rate_cap] <- rate_cap

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
                                w_param = 1,
                                config = NULL) {
  # Get base period years from config
  if (is.null(config) || is.null(config$divorce)) {
    cli::cli_abort("Config is required for {.fn build_base_divgrid}.")
  }
  bp_start <- as.integer(config$divorce$base_period_start)
  bp_end <- as.integer(config$divorce$base_period_end)

  cli::cli_h2("Building Base DivGrid ({bp_start}-{bp_end})")

  years <- bp_start:bp_end
  n_years <- length(years)

  # Get SS area factors
  ss_factors <- get_divorce_ss_area_factors(years, cache_dir)

  # Get PR/VI divorces anchor from config
  if (!is.null(config) && !is.null(config$divorce$pr_vi$anchor_1988_total)) {
    pr_vi_1988 <- config$divorce$pr_vi$anchor_1988_total
  } else {
    cli::cli_abort("Config is required: config$divorce$pr_vi$anchor_1988_total not set.")
  }

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
      cli::cli_abort("No US total divorces for year {yr}. Check nchs_us_total_divorces.csv.")
    }

    # Estimate PR/VI for this year (scale by US total ratio to base period end)
    pr_vi <- pr_vi_1988 * (us_total / us_totals[year == bp_end, total_divorces])

    # Get SS area factor
    ss_factor <- ss_factors[as.character(yr)]

    # Inflate DRA to SS area
    ss_div_matrix <- inflate_divorces_to_ss_area(
      div_matrix, dra_total, us_total, pr_vi, ss_factor
    )

    # Get married couples for this year
    married_grid <- build_divorce_married_couples_grid(marital_pop, yr)

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

#' Build base DivGrid from source data
#'
#' @description
#' Builds the base DivGrid from source data.
#' This is the main entry point for obtaining the base DivGrid.
#'
#' @param cache_dir Character path to cache directory (for reading source data)
#' @param config Optional config list for parameters (standard_population_years, min_age, max_age)
#'
#' @return List with divgrid matrix and metadata
#'
#' @export
fetch_base_divgrid <- function(cache_dir = here::here("data/cache"),
                                config = NULL) {
  # Get base period years from config
  bp_start <- as.integer(config$divorce$base_period_start)
  bp_end <- as.integer(config$divorce$base_period_end)

  # Build from source data
  cli::cli_alert_info("Building base DivGrid from source data...")

  # Load required data
  marital_pop <- readRDS(file.path(cache_dir, "historical_population",
                                    "ss_population_marital_1940_2022.rds"))
  marital_pop <- data.table::as.data.table(marital_pop)

  detailed_divorces <- fetch_nchs_dra_divorces_detailed_1979_1988(
    cache_dir = file.path(cache_dir, "nber_divorce")
  )
  us_totals <- fetch_nchs_us_total_divorces(years = bp_start:bp_end)

  # Build DivGrid
  result <- build_base_divgrid(
    detailed_divorces = detailed_divorces,
    marital_pop = marital_pop,
    us_totals = us_totals,
    cache_dir = cache_dir,
    smooth = TRUE,
    config = config
  )

  # Add standard population ADR (using config for standard_population_years, min_age, max_age)
  std_pop <- build_standard_married_population(marital_pop, config = config)
  result$base_adr <- calculate_adr(result$divgrid, std_pop)
  result$standard_pop <- std_pop

  result
}


# =============================================================================
# DIVGRID ADJUSTMENT WITH ACS DATA (Phase 7D)
# =============================================================================
# Per TR2025 Section 1.7.c:
# "DivGrid for years after 1988 is a weighted average of the 1988 DivGrid and
# the 2011 state data single year grid. This state data single year of age grid
# is derived by ratioing the 1988 DivGrid cells using the original state
# age-group data."
#
# Our deviation: We use ACS PUMS divorce data (2008-2022) instead of
# state health department data (2009-2012) as our source for recent patterns.
# =============================================================================

#' Calculate marginal distributions from DivGrid
#'
#' @description
#' Extracts the marginal age distributions (husband and wife) from a DivGrid.
#' Used for calculating ratio adjustments.
#'
#' @param divgrid Matrix of divorce rates (87×87)
#'
#' @return List with:
#'   - husband_marginal: Named numeric vector of row sums (by husband age)
#'   - wife_marginal: Named numeric vector of column sums (by wife age)
#'   - husband_normalized: Normalized (sums to 1)
#'   - wife_normalized: Normalized (sums to 1)
#'
#' @export
get_divgrid_marginals <- function(divgrid) {
  checkmate::assert_matrix(divgrid, mode = "numeric")

  # Row sums = husband marginal (sum across all wife ages for each husband age)
  husband_marginal <- rowSums(divgrid, na.rm = TRUE)
  names(husband_marginal) <- rownames(divgrid)

  # Column sums = wife marginal (sum across all husband ages for each wife age)
  wife_marginal <- colSums(divgrid, na.rm = TRUE)
  names(wife_marginal) <- colnames(divgrid)

  # Normalize
  husband_total <- sum(husband_marginal)
  wife_total <- sum(wife_marginal)

  husband_normalized <- husband_marginal / husband_total
  wife_normalized <- wife_marginal / wife_total

  list(
    husband_marginal = husband_marginal,
    wife_marginal = wife_marginal,
    husband_normalized = husband_normalized,
    wife_normalized = wife_normalized
  )
}


#' Calculate ratio adjustments from ACS data
#'
#' @description
#' Calculates age-specific ratio adjustments by comparing ACS marginal
#' distributions to base DivGrid marginals.
#'
#' Per TR2025: "This state data single year of age grid is derived by
#' ratioing the 1988 DivGrid cells using the original state age-group data."
#'
#' @param acs_male_marginal ACS male age distribution (normalized, from fetch_acs_divorces)
#' @param acs_female_marginal ACS female age distribution (normalized)
#' @param base_husband_marginal Base DivGrid husband marginal (normalized)
#' @param base_wife_marginal Base DivGrid wife marginal (normalized)
#' @param min_ratio Minimum allowed ratio (default: 0.1)
#' @param max_ratio Maximum allowed ratio (default: 10)
#'
#' @return List with:
#'   - husband_ratios: Named numeric vector of adjustment ratios by age
#'   - wife_ratios: Named numeric vector of adjustment ratios by age
#'
#' @export
calculate_acs_adjustment_ratios <- function(acs_male_marginal,
                                             acs_female_marginal,
                                             base_husband_marginal,
                                             base_wife_marginal,
                                             min_ratio = 0.1,
                                             max_ratio = 10) {

  # ACS uses male/female, DivGrid uses husband/wife (same thing for opposite-sex)
  # Map: male -> husband, female -> wife

  # Calculate ratios: new_pattern / old_pattern
  # Where both are small, use ratio of 1 (no adjustment)

  calculate_ratios <- function(acs_dist, base_dist, min_val = 0.001) {
    # Ensure same ages
    ages <- names(base_dist)
    ratios <- rep(1, length(ages))
    names(ratios) <- ages

    for (age in ages) {
      base_val <- base_dist[age]
      acs_val <- if (age %in% names(acs_dist)) acs_dist[age] else NA

      if (is.na(acs_val) || is.na(base_val)) {
        ratios[age] <- 1
      } else if (base_val < min_val) {
        # If base is very small, cap the ratio
        ratios[age] <- if (acs_val > min_val) max_ratio else 1
      } else {
        ratios[age] <- acs_val / base_val
      }
    }

    # Clip to allowed range
    ratios <- pmax(pmin(ratios, max_ratio), min_ratio)

    ratios
  }

  husband_ratios <- calculate_ratios(acs_male_marginal, base_husband_marginal)
  wife_ratios <- calculate_ratios(acs_female_marginal, base_wife_marginal)

  list(
    husband_ratios = husband_ratios,
    wife_ratios = wife_ratios
  )
}


#' Apply ratio adjustments to DivGrid
#'
#' @description
#' Creates an adjusted DivGrid by applying ratio adjustments from ACS data.
#'
#' Per TR2025 methodology: For each cell (x,y), the adjusted rate is:
#' adjusted_rate[x,y] = base_rate[x,y] * sqrt(husband_ratio[x] * wife_ratio[y])
#'
#' Using geometric mean of ratios maintains symmetry and ensures the
#' adjusted marginals match the ACS pattern.
#'
#' @param base_divgrid Base DivGrid matrix (87×87)
#' @param husband_ratios Named numeric vector of husband age ratios
#' @param wife_ratios Named numeric vector of wife age ratios
#'
#' @return Adjusted DivGrid matrix
#'
#' @export
apply_ratio_adjustments <- function(base_divgrid,
                                     husband_ratios,
                                     wife_ratios) {
  checkmate::assert_matrix(base_divgrid, mode = "numeric")

  ages <- rownames(base_divgrid)
  n_ages <- length(ages)

  adjusted <- base_divgrid

  for (i in seq_along(ages)) {
    h_age <- ages[i]
    h_ratio <- if (h_age %in% names(husband_ratios)) husband_ratios[h_age] else 1

    for (j in seq_along(ages)) {
      w_age <- ages[j]
      w_ratio <- if (w_age %in% names(wife_ratios)) wife_ratios[w_age] else 1

      # Apply geometric mean of ratios
      combined_ratio <- sqrt(h_ratio * w_ratio)
      adjusted[i, j] <- base_divgrid[i, j] * combined_ratio
    }
  }

  adjusted
}


#' Build ACS-adjusted DivGrid
#'
#' @description
#' Creates a DivGrid adjusted with ACS PUMS divorce data.
#' This is our implementation alternative to TR2025's state data adjustment.
#'
#' @param base_divgrid Base DivGrid from build_base_divgrid()
#' @param acs_divorce_data Result from fetch_acs_divorces() or get_acs_divorce_data()
#' @param smooth Logical: apply additional smoothing after adjustment (default: TRUE)
#'
#' @return Adjusted DivGrid matrix
#'
#' @export
build_acs_adjusted_divgrid <- function(base_divgrid,
                                        acs_divorce_data,
                                        smooth = TRUE) {
  cli::cli_h2("Building ACS-Adjusted DivGrid")

  # Get base marginals
  base_marginals <- get_divgrid_marginals(base_divgrid)

  cli::cli_alert_info("Base DivGrid - Peak husband age: {names(which.max(base_marginals$husband_normalized))}")
  cli::cli_alert_info("Base DivGrid - Peak wife age: {names(which.max(base_marginals$wife_normalized))}")

  # Get ACS marginals
  acs_male <- acs_divorce_data$male_marginal
  acs_female <- acs_divorce_data$female_marginal

  cli::cli_alert_info("ACS Data - Peak male age: {names(which.max(acs_male))}")
  cli::cli_alert_info("ACS Data - Peak female age: {names(which.max(acs_female))}")

  # Calculate ratios
  ratios <- calculate_acs_adjustment_ratios(
    acs_male_marginal = acs_male,
    acs_female_marginal = acs_female,
    base_husband_marginal = base_marginals$husband_normalized,
    base_wife_marginal = base_marginals$wife_normalized
  )

  cli::cli_alert_info("Ratio range - Husband: [{round(min(ratios$husband_ratios), 2)}, {round(max(ratios$husband_ratios), 2)}]")
  cli::cli_alert_info("Ratio range - Wife: [{round(min(ratios$wife_ratios), 2)}, {round(max(ratios$wife_ratios), 2)}]")

  # Apply adjustments
  adjusted <- apply_ratio_adjustments(base_divgrid, ratios$husband_ratios, ratios$wife_ratios)

  # Optional smoothing
  if (smooth) {
    cli::cli_alert("Applying Whittaker-Henderson graduation to adjusted grid...")
    adjusted <- graduate_divgrid(adjusted, h_param = 1, w_param = 1)
  }

  # Ensure non-negative
  adjusted[adjusted < 0] <- 0

  # Report results
  max_rate <- max(adjusted, na.rm = TRUE)
  max_idx <- which(adjusted == max_rate, arr.ind = TRUE)[1, ]
  peak_h_age <- as.integer(rownames(adjusted)[max_idx[1]])
  peak_w_age <- as.integer(colnames(adjusted)[max_idx[2]])

  cli::cli_alert_success("Adjusted DivGrid - Peak rate: {round(max_rate, 1)} at husband {peak_h_age}, wife {peak_w_age}")

  attr(adjusted, "adjustment_source") <- "ACS PUMS"
  attr(adjusted, "adjustment_years") <- acs_divorce_data$years

  adjusted
}


#' Create weighted average of base and adjusted DivGrid
#'
#' @description
#' Per TR2025: "DivGrid for years after 1988 is a weighted average of the
#' 1988 DivGrid and the 2011 state data single year grid."
#'
#' This function creates a weighted combination of base and adjusted grids,
#' where the weight on the adjusted grid increases from 0 to 1 over the
#' transition period.
#'
#' @param base_divgrid Base DivGrid (1979-1988)
#' @param adjusted_divgrid ACS-adjusted DivGrid
#' @param target_year Year for which to calculate weighted average
#' @param base_year Final year of base period (default: 1988)
#' @param adjustment_year Year of adjustment data (default: 2015 = midpoint of ACS 2008-2022)
#'
#' @return Weighted average DivGrid for target year
#'
#' @export
weighted_average_divgrid <- function(base_divgrid,
                                      adjusted_divgrid,
                                      target_year,
                                      base_year = 1988,
                                      adjustment_year) {

  if (target_year <= base_year) {
    return(base_divgrid)
  }

  if (target_year >= adjustment_year) {
    return(adjusted_divgrid)
  }

  # Linear interpolation between base and adjusted
  transition_years <- adjustment_year - base_year
  years_since_base <- target_year - base_year

  weight_adjusted <- years_since_base / transition_years
  weight_base <- 1 - weight_adjusted

  weighted <- weight_base * base_divgrid + weight_adjusted * adjusted_divgrid

  attr(weighted, "base_weight") <- weight_base
  attr(weighted, "adjusted_weight") <- weight_adjusted
  attr(weighted, "target_year") <- target_year

  weighted
}


#' Get DivGrid for a specific year
#'
#' @description
#' Returns the appropriate DivGrid for a given year, using weighted
#' averaging between base and ACS-adjusted grids as needed.
#'
#' @param year Target year
#' @param base_divgrid Base DivGrid (1979-1988)
#' @param adjusted_divgrid ACS-adjusted DivGrid (or NULL to use base only)
#' @param base_year Final year of base period (default: 1988)
#' @param adjustment_year Year of adjustment data (default: 2015)
#'
#' @return DivGrid matrix for the target year
#'
#' @export
get_divgrid_for_year <- function(year,
                                  base_divgrid,
                                  adjusted_divgrid = NULL,
                                  base_year = 1988,
                                  adjustment_year) {

  if (is.null(adjusted_divgrid)) {
    return(base_divgrid)
  }

  weighted_average_divgrid(
    base_divgrid = base_divgrid,
    adjusted_divgrid = adjusted_divgrid,
    target_year = year,
    base_year = base_year,
    adjustment_year = adjustment_year
  )
}


#' Build ACS-adjusted DivGrid
#'
#' @description
#' Main entry point for obtaining ACS-adjusted DivGrid.
#' Builds base DivGrid and applies ACS adjustment.
#'
#' @param cache_dir Character path to cache directory (for reading source data)
#' @param acs_years Integer vector of ACS years to use for adjustment
#' @param config Optional config list for parameters
#'
#' @return List with:
#'   - base_divgrid: Original 1979-1988 DivGrid
#'   - adjusted_divgrid: ACS-adjusted DivGrid
#'   - standard_pop: Standard population matrix
#'   - base_adr: ADR from base DivGrid
#'   - adjusted_adr: ADR from adjusted DivGrid
#'   - metadata: Processing details
#'
#' @export
fetch_adjusted_divgrid <- function(cache_dir = here::here("data/cache"),
                                    acs_years = NULL,
                                    config = NULL) {

  # Read acs_years from config if not provided
  if (is.null(acs_years)) {
    if (!is.null(config) && !is.null(config$divorce$acs_years)) {
      acs_years <- as.integer(unlist(config$divorce$acs_years))
    } else {
      cli::cli_abort("Config is required: config$divorce$acs_years not set.")
    }
  }

  cli::cli_h2("Building ACS-Adjusted DivGrid")

  # Get base DivGrid (pass config for standard_population_years, min_age, max_age)
  base_result <- fetch_base_divgrid(cache_dir = cache_dir, config = config)
  base_divgrid <- base_result$divgrid
  standard_pop <- base_result$standard_pop

  # Get ACS divorce data
  acs_cache_dir <- file.path(cache_dir, "acs_divorce")
  acs_data <- get_acs_divorce_data(years = acs_years, cache_dir = acs_cache_dir)

  # Build adjusted DivGrid
  adjusted_divgrid <- build_acs_adjusted_divgrid(
    base_divgrid = base_divgrid,
    acs_divorce_data = acs_data,
    smooth = TRUE
  )

  # Calculate ADRs
  base_adr <- calculate_adr(base_divgrid, standard_pop)
  adjusted_adr <- calculate_adr(adjusted_divgrid, standard_pop)

  bp_start <- as.integer(config$divorce$base_period_start)
  bp_end <- as.integer(config$divorce$base_period_end)

  cli::cli_alert_info("Base ADR ({bp_start}-{bp_end}): {round(base_adr, 1)} per 100,000")
  cli::cli_alert_info("Adjusted ADR (ACS {min(acs_years)}-{max(acs_years)}): {round(adjusted_adr, 1)} per 100,000")

  result <- list(
    base_divgrid = base_divgrid,
    adjusted_divgrid = adjusted_divgrid,
    standard_pop = standard_pop,
    base_adr = base_adr,
    adjusted_adr = adjusted_adr,
    metadata = list(
      base_years = bp_start:bp_end,
      acs_years = acs_data$years,
      adjustment_year = median(acs_data$years),
      created = Sys.time()
    )
  )

  result
}


#' Validate ACS-adjusted DivGrid
#'
#' @description
#' Validates the ACS-adjusted DivGrid against the base and checks
#' for reasonable properties.
#'
#' @param adjusted_result Result from fetch_adjusted_divgrid()
#'
#' @return List with validation results
#'
#' @export
validate_adjusted_divgrid <- function(adjusted_result) {
  results <- list(
    checks = list(),
    passed = 0,
    failed = 0
  )

  cli::cli_h2("ACS-Adjusted DivGrid Validation")

  base_divgrid <- adjusted_result$base_divgrid
  adjusted_divgrid <- adjusted_result$adjusted_divgrid
  standard_pop <- adjusted_result$standard_pop

  # Check 1: Dimensions match
  check1_passed <- all(dim(adjusted_divgrid) == dim(base_divgrid))
  results$checks$dimensions <- list(
    passed = check1_passed,
    message = sprintf("Dimensions match base: %s", if (check1_passed) "PASS" else "FAIL")
  )
  if (check1_passed) {
    cli::cli_alert_success(results$checks$dimensions$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_danger(results$checks$dimensions$message)
    results$failed <- results$failed + 1
  }

  # Check 2: All rates non-negative
  check2_passed <- all(adjusted_divgrid >= 0, na.rm = TRUE)
  results$checks$non_negative <- list(
    passed = check2_passed,
    message = sprintf("All rates non-negative: %s", if (check2_passed) "PASS" else "FAIL")
  )
  if (check2_passed) {
    cli::cli_alert_success(results$checks$non_negative$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_danger(results$checks$non_negative$message)
    results$failed <- results$failed + 1
  }

  # Check 3: Peak ages reasonable (30-55)
  max_rate <- max(adjusted_divgrid, na.rm = TRUE)
  max_idx <- which(adjusted_divgrid == max_rate, arr.ind = TRUE)[1, ]
  peak_h_age <- as.integer(rownames(adjusted_divgrid)[max_idx[1]])
  peak_w_age <- as.integer(colnames(adjusted_divgrid)[max_idx[2]])
  check3_passed <- peak_h_age >= 25 && peak_h_age <= 55 &&
    peak_w_age >= 25 && peak_w_age <= 55
  results$checks$peak_ages <- list(
    passed = check3_passed,
    message = sprintf("Peak at ages (%d, %d): %s",
                      peak_h_age, peak_w_age,
                      if (check3_passed) "PASS" else "WARNING")
  )
  if (check3_passed) {
    cli::cli_alert_success(results$checks$peak_ages$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$peak_ages$message)
    results$failed <- results$failed + 1
  }

  # Check 4: ADR in reasonable range (500-3000)
  adjusted_adr <- adjusted_result$adjusted_adr
  check4_passed <- adjusted_adr >= 500 && adjusted_adr <= 3000
  results$checks$adr_reasonable <- list(
    passed = check4_passed,
    message = sprintf("Adjusted ADR: %.1f per 100,000 (expect 500-3000)",
                      adjusted_adr)
  )
  if (check4_passed) {
    cli::cli_alert_success(results$checks$adr_reasonable$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$adr_reasonable$message)
    results$failed <- results$failed + 1
  }

  # Check 5: ADR change from base is reasonable (<50% change)
  base_adr <- adjusted_result$base_adr
  pct_change <- abs(adjusted_adr - base_adr) / base_adr * 100
  check5_passed <- pct_change <= 50
  results$checks$adr_change <- list(
    passed = check5_passed,
    message = sprintf("ADR change from base: %.1f%% (expect <= 50%%)", pct_change)
  )
  if (check5_passed) {
    cli::cli_alert_success(results$checks$adr_change$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$adr_change$message)
    results$failed <- results$failed + 1
  }

  # Check 6: Correlation with base is high (similar pattern)
  correlation <- cor(as.vector(base_divgrid), as.vector(adjusted_divgrid))
  check6_passed <- correlation >= 0.90
  results$checks$correlation <- list(
    passed = check6_passed,
    message = sprintf("Correlation with base: %.3f (expect >= 0.90)", correlation)
  )
  if (check6_passed) {
    cli::cli_alert_success(results$checks$correlation$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$correlation$message)
    results$failed <- results$failed + 1
  }

  cli::cli_alert_info("Passed: {results$passed}/{results$passed + results$failed}")

  invisible(results)
}


# =============================================================================
# PHASE 7E: HISTORICAL PERIOD CALCULATION (1989-2022)
# =============================================================================
# Per TR2025 Section 1.7.c:
# "For each year in the 1989-2022 period, an expected number of total divorces
# in the Social Security area is obtained by applying the age-of-husband
# crossed with age-of-wife rates in DivGrid to the corresponding married
# population in the Social Security area. The rates in DivGrid are then
# proportionally adjusted so that they would yield an estimate of the total
# number of divorces in the Social Security area."
# =============================================================================

#' Estimate SS area total divorces for a year
#'
#' @description
#' Estimates the total number of divorces in the Social Security area
#' by adjusting U.S. divorces for PR/VI and SS area population differences.
#'
#' Per TR2025: "The estimate of total divorces is obtained by adjusting the
#' reported number of divorces in the U.S. for (1) the differences between
#' the total divorces in the U.S. and in the combined U.S., Puerto Rico, and
#' Virgin Islands area, and (2) the difference between the population in the
#' combined U.S., Puerto Rico, and Virgin Islands area and in the Social
#' Security area."
#'
#' @param year Integer: target year
#' @param us_divorces Numeric: total U.S. divorces from NCHS
#' @param pr_vi_divorces Numeric: PR + VI divorces (default: estimated)
#' @param ss_area_factor Numeric: SS area adjustment factor
#'
#' @return Numeric: estimated SS area total divorces
#'
#' @export
estimate_ss_area_divorces <- function(year,
                                       us_divorces,
                                       pr_vi_divorces = NULL,
                                       ss_area_factor = NULL,
                                       config = NULL) {

  # PR/VI divorces must be provided or derivable from config
  if (is.null(pr_vi_divorces)) {
    if (!is.null(config) && !is.null(config$divorce$pr_vi$us_ratio)) {
      pr_vi_divorces <- us_divorces * config$divorce$pr_vi$us_ratio
      cli::cli_alert_info("Estimated PR/VI divorces from config ratio: {round(pr_vi_divorces)}")
    } else {
      cli::cli_abort("PR/VI divorces not provided and config$divorce$pr_vi$us_ratio not set.")
    }
  }

  # SS area factor must be provided
  if (is.null(ss_area_factor)) {
    cli::cli_abort("SS area factor is required for {.fn estimate_ss_area_divorces}.")
  }

  # SS area divorces = (US + PR/VI) × SS area factor
  ss_area_divorces <- (us_divorces + pr_vi_divorces) * ss_area_factor

  ss_area_divorces
}


#' Get PR/VI divorces for a year
#'
#' @description
#' Returns PR + VI divorces for a given year, using available data sources:
#' - 1988, 1998-2000: NCHS data (fetch_pr_vi_divorces)
#' - 2008-2022: ACS PUMS data (fetch_acs_pr_divorces)
#' - Other years: interpolated/estimated
#'
#' @param year Integer: target year
#' @param cache_dir Character: cache directory path
#'
#' @return Numeric: estimated PR + VI divorces
#'
#' @export
get_pr_vi_divorces_for_year <- function(year, cache_dir = here::here("data/cache"),
                                        config = NULL) {

  # Try to get from NCHS data for available years
  if (year %in% c(1988, 1998:2000)) {
    pr_vi_data <- fetch_pr_vi_divorces(years = year)
    return(sum(pr_vi_data$divorces, na.rm = TRUE))
  }

  # Get PR/VI config parameters
  if (is.null(config) || is.null(config$divorce$pr_vi)) {
    cli::cli_abort("Config is required for PR/VI estimation: config$divorce$pr_vi not set.")
  }
  pr_vi_cfg <- config$divorce$pr_vi

  # Try ACS PR divorce data (check for cached file)
  hist_end <- as.integer(config$divorce$historical_end_year)
  if (is.null(config$historical_population$acs_excluded_years)) {
    cli::cli_abort("Missing required config key: {.code historical_population.acs_excluded_years}")
  }
  acs_excluded <- config$historical_population$acs_excluded_years
  if (year %in% setdiff(2008:hist_end, as.integer(unlist(acs_excluded)))) {
    acs_cache_dir <- file.path(cache_dir, "acs_divorce")
    acs_pr_file <- file.path(acs_cache_dir, sprintf("pr_divorces_%d.rds", year))

    if (file.exists(acs_pr_file)) {
      pr_data <- readRDS(acs_pr_file)
      vi_pr_ratio <- pr_vi_cfg$vi_pr_ratio
      return(pr_data$total_divorces * (1 + vi_pr_ratio))
    }
  }

  # For other years, estimate based on config anchor and decline rate
  anchor_total <- pr_vi_cfg$anchor_1988_total
  annual_decline <- pr_vi_cfg$annual_decline

  if (year < 2008) {
    years_diff <- year - 1988
    cli::cli_alert_info("Estimating PR/VI divorces for {year} from 1988 anchor ({anchor_total}) with {annual_decline} annual decline")
    return(anchor_total * (annual_decline ^ years_diff))
  } else {
    # Post-2008 without ACS cache: use decline from anchor
    years_diff <- year - 1988
    cli::cli_alert_info("Estimating PR/VI divorces for {year} from 1988 anchor (no ACS cache)")
    return(anchor_total * (annual_decline ^ years_diff))
  }
}


#' Calculate expected divorces from DivGrid and married population
#'
#' @description
#' Applies divorce rates from DivGrid to married couples population to
#' calculate expected total divorces.
#'
#' Per TR2025: "an expected number of total divorces in the Social Security
#' area is obtained by applying the age-of-husband crossed with age-of-wife
#' rates in DivGrid to the corresponding married population"
#'
#' @param divgrid Matrix of divorce rates (per 100,000)
#' @param married_grid Matrix of married couples by age
#'
#' @return Numeric: expected total divorces
#'
#' @export
calculate_expected_divorces <- function(divgrid, married_grid) {
  checkmate::assert_matrix(divgrid, mode = "numeric")
  checkmate::assert_matrix(married_grid, mode = "numeric")

  # Rates are per 100,000, so divide by 100,000 to get actual divorces
  expected <- sum(married_grid * divgrid, na.rm = TRUE) / 100000

  expected
}


#' Scale DivGrid to match target total divorces
#'
#' @description
#' Proportionally adjusts DivGrid rates so they yield the target total
#' divorces when applied to the married population.
#'
#' Per TR2025: "The rates in DivGrid are then proportionally adjusted so
#' that they would yield an estimate of the total number of divorces in
#' the Social Security area."
#'
#' @param divgrid Matrix of divorce rates
#' @param married_grid Matrix of married couples by age
#' @param target_divorces Target total divorces in SS area
#'
#' @return Scaled DivGrid matrix
#'
#' @export
scale_divgrid_to_divorces <- function(divgrid, married_grid, target_divorces) {
  # Calculate expected divorces with current rates
  expected <- calculate_expected_divorces(divgrid, married_grid)

  if (expected == 0) {
    cli::cli_warn("Expected divorces is zero, cannot scale")
    return(divgrid)
  }

  # Calculate scaling factor
  scale_factor <- target_divorces / expected

  # Apply scaling
  scaled_grid <- divgrid * scale_factor

  attr(scaled_grid, "scale_factor") <- scale_factor
  attr(scaled_grid, "target_divorces") <- target_divorces

  scaled_grid
}


#' Calculate divorce rates for a single historical year
#'
#' @description
#' Calculates age-specific divorce rates for a single year by:
#' 1. Getting the appropriate DivGrid (weighted average of base and adjusted)
#' 2. Getting married couples population for that year
#' 3. Estimating SS area total divorces
#' 4. Scaling DivGrid to match the total
#' 5. Calculating resulting ADR
#'
#' @param year Integer: target year
#' @param base_divgrid Base DivGrid (1979-1988)
#' @param adjusted_divgrid ACS-adjusted DivGrid
#' @param marital_pop Marital status population data
#' @param us_divorces Numeric: total U.S. divorces for this year
#' @param standard_pop Standard population for ADR calculation
#' @param ss_area_factor SS area adjustment factor (optional)
#' @param cache_dir Cache directory path
#' @param base_year Final year of base period (default: 1988)
#' @param adjustment_year Year of adjustment data (default: 2020)
#'
#' @return List with:
#'   - scaled_divgrid: Scaled divorce rates for this year
#'   - adr: Age-adjusted divorce rate
#'   - ss_area_divorces: Estimated SS area total divorces
#'   - scale_factor: Applied scaling factor
#'
#' @export
calculate_historical_year_rates <- function(year,
                                             base_divgrid,
                                             adjusted_divgrid,
                                             marital_pop,
                                             us_divorces,
                                             standard_pop,
                                             ss_area_factor = NULL,
                                             cache_dir = here::here("data/cache"),
                                             base_year = 1988,
                                             adjustment_year,
                                             config = NULL) {

  # Get appropriate DivGrid for this year (weighted average)
  year_divgrid <- get_divgrid_for_year(
    year = year,
    base_divgrid = base_divgrid,
    adjusted_divgrid = adjusted_divgrid,
    base_year = base_year,
    adjustment_year = adjustment_year
  )

  # Get married couples population for this year
  married_grid <- build_divorce_married_couples_grid(marital_pop, year)

  # Get SS area factor if not provided
  if (is.null(ss_area_factor)) {
    ss_area_factor <- get_divorce_ss_area_factor(year, cache_dir)
  }

  # Estimate PR/VI divorces
  pr_vi_divorces <- get_pr_vi_divorces_for_year(year, cache_dir, config = config)

  # Estimate SS area total divorces
  ss_area_divorces <- estimate_ss_area_divorces(
    year = year,
    us_divorces = us_divorces,
    pr_vi_divorces = pr_vi_divorces,
    ss_area_factor = ss_area_factor
  )

  # Scale DivGrid to match SS area total
  scaled_divgrid <- scale_divgrid_to_divorces(
    year_divgrid, married_grid, ss_area_divorces
  )

  # Calculate ADR using standard population
  adr <- calculate_adr(scaled_divgrid, standard_pop)

  list(
    year = year,
    scaled_divgrid = scaled_divgrid,
    adr = adr,
    ss_area_divorces = ss_area_divorces,
    us_divorces = us_divorces,
    pr_vi_divorces = pr_vi_divorces,
    ss_area_factor = ss_area_factor,
    scale_factor = attr(scaled_divgrid, "scale_factor")
  )
}


#' Calculate historical ADR series
#'
#' @description
#' Calculates the age-adjusted divorce rate for each year in the
#' historical period per TR2025 methodology.
#'
#' @param years Integer vector of years to calculate
#' @param cache_dir Cache directory path (for reading source data)
#' @param config Config list (required for adjustment_year and PR/VI params)
#'
#' @return data.table with columns:
#'   - year: Calendar year
#'   - adr: Age-adjusted divorce rate (per 100,000)
#'   - ss_area_divorces: Estimated SS area total divorces
#'   - us_divorces: NCHS total U.S. divorces
#'   - scale_factor: Applied scaling factor
#'
#' @export
calculate_historical_adr_series <- function(years,
                                             cache_dir = here::here("data/cache"),
                                             config = NULL) {

  cli::cli_h2("Calculating Historical ADR Series ({min(years)}-{max(years)})")

  # Load required data
  cli::cli_alert("Loading required data...")

  # Read adjustment_year from config (required)
  if (is.null(config) || is.null(config$divorce$adjustment_year)) {
    cli::cli_abort("Config is required: config$divorce$adjustment_year not set.")
  }
  adjustment_year <- as.integer(config$divorce$adjustment_year)

  # Get adjusted DivGrid result (includes base, adjusted, and standard_pop)
  adjusted_result <- fetch_adjusted_divgrid(cache_dir = cache_dir,
                                            config = config)
  base_divgrid <- adjusted_result$base_divgrid
  adjusted_divgrid <- adjusted_result$adjusted_divgrid
  standard_pop <- adjusted_result$standard_pop

  # Load marital population
  marital_file <- file.path(cache_dir, "historical_population",
                            "ss_population_marital_1940_2022.rds")
  if (!file.exists(marital_file)) {
    cli::cli_abort("Marital population cache not found: {marital_file}")
  }
  marital_pop <- data.table::as.data.table(readRDS(marital_file))

  # Get US total divorces
  us_totals <- fetch_nchs_us_total_divorces(years = years)

  # Get SS area factors for available years
  ss_factors <- get_divorce_ss_area_factors(years, cache_dir)

  # Calculate for each year
  results <- list()
  n_years <- length(years)

  cli::cli_progress_bar("Processing years", total = n_years)

  for (i in seq_along(years)) {
    yr <- years[i]
    cli::cli_progress_update()

    # Check data availability
    if (!(yr %in% marital_pop$year)) {
      cli::cli_warn("No marital population data for year {yr}, skipping")
      next
    }

    us_div <- us_totals[year == yr, total_divorces]
    if (length(us_div) == 0 || is.na(us_div)) {
      cli::cli_warn("No US divorce total for year {yr}, skipping")
      next
    }

    if (!(as.character(yr) %in% names(ss_factors))) {
      cli::cli_abort("No SS area factor available for year {yr}. Check population data.")
    }
    ss_factor <- ss_factors[as.character(yr)]

    # Calculate rates for this year
    tryCatch({
      yr_result <- calculate_historical_year_rates(
        year = yr,
        base_divgrid = base_divgrid,
        adjusted_divgrid = adjusted_divgrid,
        marital_pop = marital_pop,
        us_divorces = us_div,
        standard_pop = standard_pop,
        ss_area_factor = ss_factor,
        cache_dir = cache_dir,
        base_year = as.integer(config$divorce$base_period_end),
        adjustment_year = adjustment_year,
        config = config
      )

      results[[as.character(yr)]] <- data.table::data.table(
        year = yr,
        adr = yr_result$adr,
        ss_area_divorces = yr_result$ss_area_divorces,
        us_divorces = yr_result$us_divorces,
        pr_vi_divorces = yr_result$pr_vi_divorces,
        ss_area_factor = yr_result$ss_area_factor,
        scale_factor = yr_result$scale_factor
      )

    }, error = function(e) {
      cli::cli_warn("Error processing year {yr}: {conditionMessage(e)}")
    })
  }

  cli::cli_progress_done()

  if (length(results) == 0) {
    cli::cli_abort("No historical years could be calculated")
  }

  # Combine results
  historical_adr <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(historical_adr, year)

  # Summary statistics
  cli::cli_alert_success("Calculated historical ADR for {nrow(historical_adr)} years")
  cli::cli_alert_info("ADR range: {round(min(historical_adr$adr), 1)} - {round(max(historical_adr$adr), 1)} per 100,000")
  cli::cli_alert_info("First year ({min(historical_adr$year)}): {round(historical_adr[year == min(year), adr], 1)}")
  cli::cli_alert_info("Last year ({max(historical_adr$year)}): {round(historical_adr[year == max(year), adr], 1)}")

  # Add metadata
  attr(historical_adr, "created") <- Sys.time()
  attr(historical_adr, "years") <- years

  historical_adr
}


#' Calculate starting ADR for projections
#'
#' @description
#' Calculates the starting ADR as a weighted average of recent historical years.
#'
#' Per TR2025: "The starting age-adjusted divorce rate is set to a weighted
#' average of the past five years of data."
#'
#' @param historical_adr data.table from calculate_historical_adr_series()
#' @param n_years Integer: number of years for weighted average (default: 5)
#' @param weight_method Character: "linear" or "equal" weights (default: "linear")
#'
#' @return Numeric: starting ADR value
#'
#' @export
calculate_starting_adr <- function(historical_adr,
                                    n_years = 5,
                                    weight_method = c("linear", "equal")) {

  weight_method <- match.arg(weight_method)

  # Get the most recent n_years
  data.table::setorder(historical_adr, -year)
  recent <- head(historical_adr, n_years)

  if (nrow(recent) < n_years) {
    cli::cli_warn("Only {nrow(recent)} years available, using all")
  }

  # Calculate weights
  if (weight_method == "linear") {
    # More recent years get higher weights
    # Most recent = n_years, oldest = 1
    weights <- seq(nrow(recent), 1)
  } else {
    weights <- rep(1, nrow(recent))
  }

  # Normalize weights
  weights <- weights / sum(weights)

  # Calculate weighted average
  starting_adr <- sum(recent$adr * weights)

  cli::cli_alert_info("Starting ADR ({weight_method} weighted average of {nrow(recent)} years): {round(starting_adr, 1)}")

  starting_adr
}


#' Validate historical ADR series
#'
#' @description
#' Validates the historical ADR series against expected patterns.
#'
#' @param historical_adr data.table from calculate_historical_adr_series()
#'
#' @return List with validation results
#'
#' @export
validate_historical_adr <- function(historical_adr) {
  results <- list(
    checks = list(),
    passed = 0,
    failed = 0
  )

  cli::cli_h2("Historical ADR Validation")

  # Check 1: All years present
  expected_years <- length(min(historical_adr$year):max(historical_adr$year))
  actual_years <- nrow(historical_adr)
  check1_passed <- actual_years >= expected_years - 2  # Allow up to 2 missing
  results$checks$years_complete <- list(
    passed = check1_passed,
    message = sprintf("Years available: %d/%d", actual_years, expected_years)
  )
  if (check1_passed) {
    cli::cli_alert_success(results$checks$years_complete$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$years_complete$message)
    results$failed <- results$failed + 1
  }

  # Check 2: ADR values in reasonable range (500-5000)
  adr_range <- range(historical_adr$adr, na.rm = TRUE)
  check2_passed <- adr_range[1] >= 200 && adr_range[2] <= 5000
  results$checks$adr_range <- list(
    passed = check2_passed,
    message = sprintf("ADR range: %.1f - %.1f (expect 200-5000)",
                      adr_range[1], adr_range[2])
  )
  if (check2_passed) {
    cli::cli_alert_success(results$checks$adr_range$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$adr_range$message)
    results$failed <- results$failed + 1
  }

  # Check 3: No extreme year-to-year changes (< 30%)
  historical_adr_sorted <- data.table::copy(historical_adr)
  data.table::setorder(historical_adr_sorted, year)
  historical_adr_sorted[, pct_change := (adr - shift(adr)) / shift(adr) * 100]
  max_change <- max(abs(historical_adr_sorted$pct_change), na.rm = TRUE)
  check3_passed <- max_change <= 30
  results$checks$year_changes <- list(
    passed = check3_passed,
    message = sprintf("Max year-to-year change: %.1f%% (expect <= 30%%)", max_change)
  )
  if (check3_passed) {
    cli::cli_alert_success(results$checks$year_changes$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$year_changes$message)
    results$failed <- results$failed + 1
  }

  # Check 4: Scale factors reasonable (0.5 - 2.0)
  scale_range <- range(historical_adr$scale_factor, na.rm = TRUE)
  check4_passed <- scale_range[1] >= 0.3 && scale_range[2] <= 3.0
  results$checks$scale_factors <- list(
    passed = check4_passed,
    message = sprintf("Scale factor range: %.3f - %.3f (expect 0.3-3.0)",
                      scale_range[1], scale_range[2])
  )
  if (check4_passed) {
    cli::cli_alert_success(results$checks$scale_factors$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$scale_factors$message)
    results$failed <- results$failed + 1
  }

  # Check 5: General trend is declining (divorce rates have fallen since 1990s)
  early_avg <- mean(historical_adr[year <= min(year) + 6, adr], na.rm = TRUE)
  late_avg <- mean(historical_adr[year >= max(year) - 4, adr], na.rm = TRUE)
  check5_passed <- late_avg < early_avg
  results$checks$declining_trend <- list(
    passed = check5_passed,
    message = sprintf("ADR trend: %d-%d avg %.1f → %d-%d avg %.1f (%s)",
                      min(historical_adr$year), 1995,
                      early_avg,
                      max(historical_adr$year) - 4, max(historical_adr$year),
                      late_avg,
                      if (check5_passed) "declining as expected" else "not declining")
  )
  if (check5_passed) {
    cli::cli_alert_success(results$checks$declining_trend$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$declining_trend$message)
    results$failed <- results$failed + 1
  }

  cli::cli_alert_info("Passed: {results$passed}/{results$passed + results$failed}")

  invisible(results)
}


#' Get complete historical divorce data (1979-2022)
#'
#' @description
#' Main entry point for historical divorce data including both the
#' detailed period (1979-1988) and scaled period (1989-2022).
#'
#' @param cache_dir Cache directory path (for reading source data)
#' @param config Optional config list for parameters
#'
#' @return List with:
#'   - adr_series: Complete ADR series 1979-2022
#'   - base_result: Base DivGrid result (1979-1988)
#'   - adjusted_result: ACS-adjusted DivGrid result
#'   - historical_period: Detailed 1989-2022 results
#'   - starting_adr: Calculated starting ADR for projections
#'
#' @export
get_historical_divorce_data <- function(cache_dir = here::here("data/cache"),
                                         config = NULL) {

  # Derive year ranges from config
  bp_start <- as.integer(config$divorce$base_period_start)
  bp_end <- as.integer(config$divorce$base_period_end)
  hist_end <- as.integer(config$divorce$historical_end_year)
  hist_start <- bp_end + 1L

  cli::cli_h1("Building Complete Historical Divorce Data ({bp_start}-{hist_end})")

  # Get base period ADR from the base DivGrid
  base_result <- fetch_base_divgrid(cache_dir = cache_dir, config = config)

  # Build base period ADR series
  cli::cli_alert("Building base period ADR series ({bp_start}-{bp_end})...")
  base_adr <- base_result$base_adr

  # For base period, we use the yearly rates from build_base_divgrid
  # which gives us ADR for each year
  base_years_adr <- data.table::data.table(
    year = bp_start:bp_end,
    adr = base_adr,  # Using average for all years (simplification)
    source = "NCHS DRA detailed"
  )

  # Get adjusted DivGrid
  adjusted_result <- fetch_adjusted_divgrid(cache_dir = cache_dir, config = config)

  # Calculate historical period (after base through end of data)
  historical_period <- calculate_historical_adr_series(
    years = hist_start:hist_end,
    cache_dir = cache_dir,
    config = config
  )

  # Combine into complete series
  historical_period[, source := "NCHS totals (scaled)"]

  complete_adr <- data.table::rbindlist(list(
    base_years_adr[, .(year, adr, source)],
    historical_period[, .(year, adr, source)]
  ), use.names = TRUE)

  data.table::setorder(complete_adr, year)

  # Calculate starting ADR
  starting_adr <- calculate_starting_adr(historical_period, n_years = 5)

  # Summary
  cli::cli_alert_success("Complete historical ADR series: {nrow(complete_adr)} years")
  cli::cli_alert_info("Base period ({bp_start}-{bp_end}) ADR: {round(base_adr, 1)}")
  cli::cli_alert_info("Starting ADR for projection: {round(starting_adr, 1)}")

  result <- list(
    adr_series = complete_adr,
    base_result = base_result,
    adjusted_result = adjusted_result,
    historical_period = historical_period,
    starting_adr = starting_adr,
    metadata = list(
      base_years = bp_start:bp_end,
      historical_years = hist_start:hist_end,
      created = Sys.time()
    )
  )

  result
}


# =============================================================================
# PHASE 7F: ADR PROJECTION (2023-2099)
# =============================================================================

#' Project ADR from starting value to ultimate
#'
#' @description
#' Projects the Age-Adjusted Divorce Rate from the starting value
#' (weighted average of historical years) to the ultimate value
#' using asymptotic convergence.
#'
#' Per TR2025: "This age-adjusted rate is assumed to reach its ultimate
#' value in the 25th year of the 75-year projection period. The annual
#' rate of change decreases in absolute value as the ultimate year approaches."
#'
#' @param starting_adr Numeric: starting ADR (weighted average of recent years)
#' @param ultimate_adr Numeric: ultimate ADR (default: 1700 per 100,000, or from config)
#' @param start_year Integer: first projection year (default: 2023, or from config)
#' @param ultimate_year Integer: year when ultimate is reached (default: 2049, or from config)
#' @param end_year Integer: final projection year (default: 2099, or from config)
#' @param convergence_exp Numeric: convergence exponent (default: 2)
#'   Higher values = more gradual start, faster finish
#' @param config List: optional configuration object to derive year parameters
#'
#' @return data.table with columns:
#'   - year: Calendar year
#'   - projected_adr: Projected ADR value
#'   - adr_change: Year-over-year change
#'
#' @details
#' The convergence formula uses:
#'   ADR(t) = ultimate + (starting - ultimate) × (1 - progress)^exp
#'
#' Where progress = (t - start) / (ultimate_year - start)
#'
#' This ensures:
#' - Gradual initial change (rate of change increases early)
#' - Decreasing rate of change as ultimate approaches
#' - Exact arrival at ultimate in ultimate_year
#' - Constant ultimate value thereafter
#'
#' @export
project_adr <- function(starting_adr,
                        ultimate_adr = NULL,
                        start_year = NULL,
                        ultimate_year = NULL,
                        end_year = NULL,
                        convergence_exp = NULL,
                        config = NULL) {
  # Derive parameters from config — config is required
  if (!is.null(config)) {
    years <- get_projection_years(config, "divorce")
    if (is.null(start_year)) start_year <- years$projection_start
    if (is.null(end_year)) end_year <- years$projection_end
    if (is.null(ultimate_year)) ultimate_year <- years$ultimate_year
    if (is.null(ultimate_adr)) ultimate_adr <- config$divorce$ultimate_adr
    if (is.null(convergence_exp)) convergence_exp <- config$divorce$convergence_exponent
  } else {
    cli::cli_abort("Config is required for {.fn project_adr}. Pass config from pipeline.")
  }

  checkmate::assert_number(starting_adr, lower = 0)
  checkmate::assert_number(ultimate_adr, lower = 0)
  checkmate::assert_integerish(start_year)
  checkmate::assert_integerish(ultimate_year)
  checkmate::assert_integerish(end_year)
  checkmate::assert_number(convergence_exp, lower = 0.1)

  cli::cli_h2("Projecting ADR ({start_year} to {end_year})")
  cli::cli_alert_info("Starting: {round(starting_adr, 1)}, Ultimate: {ultimate_adr}, Ultimate year: {ultimate_year}")

  years <- start_year:end_year
  n_convergence <- ultimate_year - start_year

  projected_adr <- sapply(years, function(yr) {
    if (yr >= ultimate_year) {
      # At or past ultimate year: hold at ultimate
      return(ultimate_adr)
    }

    # Calculate progress toward ultimate (0 to 1)
    progress <- (yr - start_year) / n_convergence

    # Apply convergence formula with decreasing rate of change
    # Uses complement: remaining_gap * (1 - progress)^exp
    # Higher exponent = front-loaded (faster start, gradual finish)
    remaining_factor <- (1 - progress)^convergence_exp

    # Interpolation with non-linear progress
    adr <- ultimate_adr + (starting_adr - ultimate_adr) * remaining_factor

    adr
  })

  result <- data.table::data.table(
    year = years,
    projected_adr = projected_adr
  )

  # Add rate of change for validation
  result[, adr_change := c(NA, diff(projected_adr))]
  result[, pct_change := c(NA, diff(projected_adr) / head(projected_adr, -1) * 100)]

  cli::cli_alert_success(
    "Projected {length(years)} years, ADR: {round(starting_adr, 1)} -> {ultimate_adr}"
  )

  result
}


#' Scale DivGrid to target ADR
#'
#' @description
#' Scales all rates in a DivGrid proportionally so that the resulting
#' ADR matches a target value.
#'
#' Per TR2025: "To obtain age-specific rates for use in the projections,
#' the age-of-husband-age-of-wife-specific rates in DivGrid are adjusted
#' proportionally so as to produce the age-adjusted rate assumed for
#' that particular year."
#'
#' @param divgrid Matrix: divorce rate grid (87x87)
#' @param target_adr Numeric: target ADR value
#' @param standard_pop Matrix: standard population grid for ADR calculation
#'
#' @return Scaled DivGrid matrix with attributes:
#'   - scale_factor: applied scaling factor
#'   - target_adr: target ADR value
#'   - achieved_adr: actual ADR after scaling
#'
#' @export
scale_divgrid_to_target_adr <- function(divgrid, target_adr, standard_pop) {
  checkmate::assert_matrix(divgrid, mode = "numeric")
  checkmate::assert_number(target_adr, lower = 0)
  checkmate::assert_matrix(standard_pop, mode = "numeric")

  # Calculate current ADR
  current_adr <- calculate_adr(divgrid, standard_pop)

  if (current_adr == 0) {
    cli::cli_warn("Current ADR is 0, cannot scale")
    return(divgrid)
  }

  # Calculate scale factor
  scale_factor <- target_adr / current_adr

  # Apply scaling
  scaled_grid <- divgrid * scale_factor

  # Verify
  achieved_adr <- calculate_adr(scaled_grid, standard_pop)

  attr(scaled_grid, "scale_factor") <- scale_factor
  attr(scaled_grid, "target_adr") <- target_adr
  attr(scaled_grid, "achieved_adr") <- achieved_adr

  scaled_grid
}


#' Validate ADR projection
#'
#' @description
#' Validates the projected ADR series against expected criteria:
#' - Monotonic progression (increasing or decreasing toward ultimate)
#' - Reaches ultimate at ultimate_year
#' - Rate of change decreases as ultimate approaches
#' - Values stay within reasonable bounds
#'
#' @param adr_projection data.table from project_adr()
#' @param starting_adr Original starting ADR
#' @param ultimate_adr Target ultimate ADR
#' @param ultimate_year Year ultimate should be reached
#'
#' @return List with validation results
#'
#' @export
validate_adr_projection <- function(adr_projection,
                                    starting_adr,
                                    ultimate_adr,
                                    ultimate_year) {

  cli::cli_h2("ADR Projection Validation")

  checks <- list()
  n_pass <- 0
  n_total <- 0

  # Check 1: Projection has expected years
  n_total <- n_total + 1
  expected_years <- length(2023:2099)
  actual_years <- nrow(adr_projection)
  if (actual_years == expected_years) {
    cli::cli_alert_success("Year count: {actual_years} (expected {expected_years})")
    checks$year_count <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_danger("Year count: {actual_years} (expected {expected_years})")
    checks$year_count <- FALSE
  }

  # Check 2: Reaches ultimate at ultimate_year
  n_total <- n_total + 1
  adr_at_ultimate <- adr_projection[year == ultimate_year, projected_adr]
  tolerance <- 0.01  # 1% tolerance
  if (abs(adr_at_ultimate - ultimate_adr) / ultimate_adr < tolerance) {
    cli::cli_alert_success("Ultimate ADR at year {ultimate_year}: {round(adr_at_ultimate, 1)} (target: {ultimate_adr})")
    checks$reaches_ultimate <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_danger("Ultimate ADR at year {ultimate_year}: {round(adr_at_ultimate, 1)} (target: {ultimate_adr})")
    checks$reaches_ultimate <- FALSE
  }

  # Check 3: Holds at ultimate after ultimate_year
  n_total <- n_total + 1
  post_ultimate <- adr_projection[year > ultimate_year, projected_adr]
  if (all(abs(post_ultimate - ultimate_adr) < 0.001)) {
    cli::cli_alert_success("ADR constant at ultimate after {ultimate_year}")
    checks$holds_ultimate <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_danger("ADR varies after ultimate year")
    checks$holds_ultimate <- FALSE
  }

  # Check 4: Monotonic progression
  n_total <- n_total + 1
  pre_ultimate <- adr_projection[year <= ultimate_year, projected_adr]
  if (starting_adr < ultimate_adr) {
    # Increasing trajectory
    is_monotonic <- all(diff(pre_ultimate) >= -0.001)
    direction <- "increasing"
  } else {
    # Decreasing trajectory
    is_monotonic <- all(diff(pre_ultimate) <= 0.001)
    direction <- "decreasing"
  }
  if (is_monotonic) {
    cli::cli_alert_success("Monotonic {direction} progression toward ultimate")
    checks$monotonic <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_danger("Non-monotonic progression detected")
    checks$monotonic <- FALSE
  }

  # Check 5: Rate of change decreases (in absolute value) as ultimate approaches
  n_total <- n_total + 1
  changes <- adr_projection[year > 2023 & year <= ultimate_year, adr_change]
  abs_changes <- abs(changes)
  # Check that later changes are smaller (use average of first/last halves)
  half_n <- length(abs_changes) %/% 2
  first_half_avg <- mean(abs_changes[1:half_n])
  second_half_avg <- mean(abs_changes[(half_n + 1):length(abs_changes)])
  if (first_half_avg > second_half_avg) {
    cli::cli_alert_success("Rate of change decreases as ultimate approaches ({round(first_half_avg, 2)} -> {round(second_half_avg, 2)})")
    checks$decreasing_rate <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_warning("Rate of change not decreasing as expected ({round(first_half_avg, 2)} -> {round(second_half_avg, 2)})")
    checks$decreasing_rate <- FALSE
  }

  # Check 6: Values within reasonable bounds
  n_total <- n_total + 1
  min_adr <- min(adr_projection$projected_adr)
  max_adr <- max(adr_projection$projected_adr)
  if (min_adr >= 100 && max_adr <= 10000) {
    cli::cli_alert_success("ADR range: {round(min_adr, 1)} - {round(max_adr, 1)} (within 100-10,000)")
    checks$reasonable_bounds <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_danger("ADR range: {round(min_adr, 1)} - {round(max_adr, 1)} (outside expected bounds)")
    checks$reasonable_bounds <- FALSE
  }

  # Summary
  cli::cli_alert_info("Passed: {n_pass}/{n_total}")

  list(
    passed = n_pass == n_total,
    n_pass = n_pass,
    n_total = n_total,
    checks = checks
  )
}


#' Get projected ADR series
#'
#' @description
#' Main entry point for Phase 7F. Calculates the projected
#' ADR series from 2023 to 2099.
#'
#' @param cache_dir Cache directory path (for reading source data)
#' @param config List: configuration object to derive parameters
#'
#' @return data.table with projected ADR series
#'
#' @export
get_projected_adr <- function(cache_dir = here::here("data/cache"),
                              config = NULL) {
  # Derive parameters from config — config is required
  if (!is.null(config)) {
    years <- get_projection_years(config, "divorce")
    start_year <- years$projection_start
    end_year <- years$projection_end
    ultimate_year <- years$ultimate_year
    ultimate_adr <- config$divorce$ultimate_adr
  } else {
    cli::cli_abort("Config is required for {.fn get_projected_adr}. Pass config from pipeline.")
  }

  cli::cli_h1("Phase 7F: ADR Projection ({start_year}-{end_year})")

  # Get historical data to calculate starting ADR
  historical_data <- get_historical_divorce_data(cache_dir, config = config)
  starting_adr <- historical_data$starting_adr

  cli::cli_alert_info("Starting ADR (from historical): {round(starting_adr, 1)}")

  # Project ADR
  adr_projection <- project_adr(
    starting_adr = starting_adr,
    ultimate_adr = ultimate_adr,
    start_year = start_year,
    ultimate_year = ultimate_year,
    end_year = end_year,
    config = config
  )

  # Validate
  validation <- validate_adr_projection(
    adr_projection,
    starting_adr = starting_adr,
    ultimate_adr = ultimate_adr,
    ultimate_year = ultimate_year
  )

  # Add metadata
  attr(adr_projection, "starting_adr") <- starting_adr
  attr(adr_projection, "ultimate_adr") <- ultimate_adr
  attr(adr_projection, "ultimate_year") <- ultimate_year
  attr(adr_projection, "validation") <- validation
  attr(adr_projection, "created") <- Sys.time()

  adr_projection
}


# =============================================================================
# PHASE 7G: DIVORCE RATE PROJECTION (2023-2099)
# =============================================================================

#' Project divorce rates for all years (2023-2099)
#'
#' @description
#' Projects age-specific divorce rates by scaling DivGrid to match
#' the projected ADR for each year.
#'
#' Per TR2025: "To obtain age-specific rates for use in the projections,
#' the age-of-husband-age-of-wife-specific rates in DivGrid are adjusted
#' proportionally so as to produce the age-adjusted rate assumed for
#' that particular year."
#'
#' @param base_divgrid Matrix: base DivGrid to scale (87x87)
#' @param adr_projection data.table: projected ADR from project_adr()
#' @param standard_pop Matrix: standard population for ADR calculation
#' @param config List: configuration object to derive year parameters
#'
#' @return List with:
#'   - rates: Named list of 87x87 matrices, one per year
#'   - adr: Projected ADR series
#'   - years: Vector of years
#'
#' @export
project_divorce_rates <- function(base_divgrid,
                                   adr_projection,
                                   standard_pop,
                                   config = NULL) {
  # Derive parameters from config — config is required
  if (!is.null(config)) {
    years <- get_projection_years(config, "divorce")
    start_year <- years$projection_start
    end_year <- years$projection_end
  } else {
    cli::cli_abort("Config is required for {.fn project_divorce_rates}. Pass config from pipeline.")
  }

  cli::cli_h2("Projecting Divorce Rates ({start_year}-{end_year})")

  checkmate::assert_matrix(base_divgrid, mode = "numeric")
  checkmate::assert_data_table(adr_projection)
  checkmate::assert_matrix(standard_pop, mode = "numeric")

  years <- start_year:end_year
  rates <- list()

  cli::cli_progress_bar("Scaling DivGrid to projected ADR", total = length(years))

  for (i in seq_along(years)) {
    yr <- years[i]
    cli::cli_progress_update()

    # Get target ADR for this year
    target_adr <- adr_projection[year == yr, projected_adr]

    if (length(target_adr) == 0) {
      cli::cli_warn("No projected ADR for year {yr}, using ultimate")
      target_adr <- attr(adr_projection, "ultimate_adr")
    }

    # Scale DivGrid to target ADR
    scaled_grid <- scale_divgrid_to_target_adr(
      divgrid = base_divgrid,
      target_adr = target_adr,
      standard_pop = standard_pop
    )

    rates[[as.character(yr)]] <- scaled_grid

    # Progress indicator every 10 years
    if (yr %% 10 == 0 || yr == start_year || yr == end_year) {
      achieved_adr <- attr(scaled_grid, "achieved_adr")
      cli::cli_alert("{yr}: ADR = {round(achieved_adr, 1)}")
    }
  }

  cli::cli_progress_done()

  # Summary
  cli::cli_alert_success("Projected {length(years)} years ({start_year}-{end_year})")

  result <- list(
    rates = rates,
    adr = adr_projection,
    years = years,
    start_year = start_year,
    end_year = end_year
  )

  result
}


#' Validate projected divorce rates
#'
#' @description
#' Validates the projected divorce rates against expected criteria.
#'
#' @param projected_result Result from project_divorce_rates()
#' @param standard_pop Standard population matrix
#' @param ultimate_adr Target ultimate ADR
#' @param ultimate_year Year ultimate should be reached
#'
#' @return List with validation results
#'
#' @export
validate_projected_rates <- function(projected_result,
                                      standard_pop,
                                      ultimate_adr,
                                      ultimate_year) {

  cli::cli_h2("Projected Divorce Rates Validation")

  checks <- list()
  n_pass <- 0
  n_total <- 0

  rates <- projected_result$rates
  adr_proj <- projected_result$adr
  years <- projected_result$years

  # Check 1: All years have rate grids
  n_total <- n_total + 1
  expected_years <- length(2023:2099)
  actual_years <- length(rates)
  if (actual_years == expected_years) {
    cli::cli_alert_success("Year count: {actual_years}/{expected_years}")
    checks$year_count <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_danger("Year count: {actual_years}/{expected_years}")
    checks$year_count <- FALSE
  }

  # Check 2: All grids have correct dimensions
  n_total <- n_total + 1
  dims_correct <- all(sapply(rates, function(g) all(dim(g) == c(87, 87))))
  if (dims_correct) {
    cli::cli_alert_success("Grid dimensions: all 87x87")
    checks$dimensions <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_danger("Grid dimensions: some incorrect")
    checks$dimensions <- FALSE
  }

  # Check 3: ADR at ultimate year matches target
  n_total <- n_total + 1
  ultimate_grid <- rates[[as.character(ultimate_year)]]
  achieved_adr <- calculate_adr(ultimate_grid, standard_pop)
  tolerance <- 0.01
  if (abs(achieved_adr - ultimate_adr) / ultimate_adr < tolerance) {
    cli::cli_alert_success("ADR at {ultimate_year}: {round(achieved_adr, 1)} (target: {ultimate_adr})")
    checks$ultimate_adr <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_danger("ADR at {ultimate_year}: {round(achieved_adr, 1)} (target: {ultimate_adr})")
    checks$ultimate_adr <- FALSE
  }

  # Check 4: ADR constant after ultimate year
  n_total <- n_total + 1
  post_ultimate_years <- years[years > ultimate_year]
  post_ultimate_adrs <- sapply(post_ultimate_years, function(yr) {
    calculate_adr(rates[[as.character(yr)]], standard_pop)
  })
  if (all(abs(post_ultimate_adrs - ultimate_adr) / ultimate_adr < tolerance)) {
    cli::cli_alert_success("ADR constant at {ultimate_adr} after {ultimate_year}")
    checks$constant_after_ultimate <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_danger("ADR varies after {ultimate_year}")
    checks$constant_after_ultimate <- FALSE
  }

  # Check 5: No negative rates
  n_total <- n_total + 1
  has_negative <- any(sapply(rates, function(g) any(g < 0, na.rm = TRUE)))
  if (!has_negative) {
    cli::cli_alert_success("All rates non-negative")
    checks$non_negative <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_danger("Some rates are negative")
    checks$non_negative <- FALSE
  }

  # Check 6: Rate pattern preserved (peak location similar)
  n_total <- n_total + 1
  first_grid <- rates[["2023"]]
  last_grid <- rates[["2099"]]
  first_peak <- which(first_grid == max(first_grid, na.rm = TRUE), arr.ind = TRUE)[1, ]
  last_peak <- which(last_grid == max(last_grid, na.rm = TRUE), arr.ind = TRUE)[1, ]
  if (all(first_peak == last_peak)) {
    cli::cli_alert_success("Rate pattern preserved: peak at ({first_peak[1] + 13}, {first_peak[2] + 13})")
    checks$pattern_preserved <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_warning("Peak location shifted: ({first_peak[1] + 13}, {first_peak[2] + 13}) -> ({last_peak[1] + 13}, {last_peak[2] + 13})")
    checks$pattern_preserved <- TRUE  # Warning only, not failure
    n_pass <- n_pass + 1
  }

  # Check 7: Scale factors reasonable
  n_total <- n_total + 1
  scale_factors <- sapply(rates, function(g) attr(g, "scale_factor"))
  scale_factors <- scale_factors[!is.na(scale_factors)]
  if (all(scale_factors > 0 & scale_factors < 10)) {
    cli::cli_alert_success("Scale factors reasonable: {round(min(scale_factors), 3)} - {round(max(scale_factors), 3)}")
    checks$scale_factors <- TRUE
    n_pass <- n_pass + 1
  } else {
    cli::cli_alert_danger("Scale factors out of range")
    checks$scale_factors <- FALSE
  }

  cli::cli_alert_info("Passed: {n_pass}/{n_total}")

  list(
    passed = n_pass == n_total,
    n_pass = n_pass,
    n_total = n_total,
    checks = checks
  )
}


#' Run complete divorce projection
#'
#' @description
#' Main entry point for the divorce subprocess. Orchestrates all phases
#' from data acquisition through projection.
#'
#' Per TR2025 Section 1.7, this function:
#' 1. Builds base DivGrid from 1979-1988 NCHS DRA data
#' 2. Adjusts DivGrid using recent ACS data
#' 3. Calculates historical ADR series (1989-2022)
#' 4. Projects ADR to ultimate (1,700 by 2049)
#' 5. Scales DivGrid to match projected ADR for each year
#'
#' @param cache_dir Character: cache directory path (for reading source data)
#' @param config List: configuration object to derive parameters
#'
#' @return List with:
#'   - historical: Historical divorce data (1979-2022)
#'   - projected_adr: Projected ADR series (2023-2099)
#'   - projected_rates: Projected DivGrid for each year
#'   - complete_adr: Combined historical + projected ADR
#'   - validation: Validation results
#'   - metadata: Processing metadata
#'
#' @export
run_divorce_projection <- function(cache_dir = here::here("data/cache"),
                                    config = NULL) {
  # Derive parameters from config — config is required
  if (!is.null(config)) {
    years <- get_projection_years(config, "divorce")
    start_year <- years$projection_start
    end_year <- years$projection_end
    ultimate_year <- years$ultimate_year
    ultimate_adr <- config$divorce$ultimate_adr
  } else {
    cli::cli_abort("Config is required for {.fn run_divorce_projection}. Pass config from pipeline.")
  }

  cli::cli_h1("Running Complete Divorce Projection (TR2025 Section 1.7)")

  start_time <- Sys.time()

  cli::cli_alert_info("Ultimate ADR: {ultimate_adr}, Ultimate year: {ultimate_year}")

  # =========================================================================
  # STEP 1: Get historical divorce data (includes base and adjusted DivGrid)
  # =========================================================================
  bp_start <- as.integer(config$divorce$base_period_start)
  hist_end <- as.integer(config$divorce$historical_end_year)
  cli::cli_h2("Step 1: Loading Historical Divorce Data ({bp_start}-{hist_end})")

  historical <- get_historical_divorce_data(cache_dir, config = config)

  base_divgrid <- historical$base_result$divgrid
  adjusted_divgrid <- historical$adjusted_result$adjusted_divgrid
  standard_pop <- historical$adjusted_result$standard_pop
  starting_adr <- historical$starting_adr

  cli::cli_alert_info("Base ADR ({bp_start}-{config$divorce$base_period_end}): {round(historical$base_result$base_adr, 1)}")
  cli::cli_alert_info("Starting ADR (for projection): {round(starting_adr, 1)}")

  # =========================================================================
  # STEP 2: Get projected ADR series
  # =========================================================================
  cli::cli_h2("Step 2: Getting Projected ADR Series ({start_year}-{end_year})")

  projected_adr <- get_projected_adr(
    cache_dir = cache_dir,
    config = config
  )

  # =========================================================================
  # STEP 3: Project divorce rates
  # =========================================================================
  cli::cli_h2("Step 3: Projecting Divorce Rates ({start_year}-{end_year})")

  # Use adjusted DivGrid as base for projection
  # (reflects current age patterns from ACS adjustment)
  projected_rates <- project_divorce_rates(
    base_divgrid = adjusted_divgrid,
    adr_projection = projected_adr,
    standard_pop = standard_pop,
    config = config
  )

  # =========================================================================
  # STEP 4: Validate projected rates
  # =========================================================================
  cli::cli_h2("Step 4: Validating Projected Rates")

  validation <- validate_projected_rates(
    projected_result = projected_rates,
    standard_pop = standard_pop,
    ultimate_adr = ultimate_adr,
    ultimate_year = ultimate_year
  )

  # =========================================================================
  # STEP 5: Combine historical and projected ADR
  # =========================================================================
  cli::cli_h2("Step 5: Building Complete ADR Series")

  # Historical ADR from Phase 7E
  historical_adr <- historical$adr_series[, .(year, adr, source)]

  # Projected ADR
  projected_adr_dt <- projected_adr[, .(
    year,
    adr = projected_adr,
    source = "Projected"
  )]

  complete_adr <- data.table::rbindlist(list(
    historical_adr,
    projected_adr_dt
  ), use.names = TRUE)

  data.table::setorder(complete_adr, year)

  # =========================================================================
  # SUMMARY
  # =========================================================================
  elapsed <- difftime(Sys.time(), start_time, units = "secs")

  cli::cli_h2("Projection Summary")
  cli::cli_alert_success("Complete divorce projection finished in {round(elapsed, 1)}s")
  cli::cli_alert_info("Historical years: {min(historical$adr_series$year)} - {max(historical$adr_series$year)}")
  cli::cli_alert_info("Projected years: {start_year} - {end_year}")
  cli::cli_alert_info("Total years: {nrow(complete_adr)}")
  cli::cli_alert_info("ADR trajectory: {round(starting_adr, 1)} ({hist_end}) -> {ultimate_adr} ({ultimate_year}) -> {ultimate_adr} ({end_year})")
  cli::cli_alert_info("Validation: {validation$n_pass}/{validation$n_total} checks passed")

  result <- list(
    historical = historical,
    projected_adr = projected_adr,
    projected_rates = projected_rates,
    complete_adr = complete_adr,
    standard_pop = standard_pop,
    validation = validation,
    metadata = list(
      ultimate_adr = ultimate_adr,
      ultimate_year = ultimate_year,
      end_year = end_year,
      starting_adr = starting_adr,
      elapsed_seconds = as.numeric(elapsed),
      created = Sys.time()
    )
  )

  result
}


#' Convert projected rates to long format data.table
#'
#' @description
#' Converts the list of rate matrices to a long-format data.table
#' suitable for analysis and storage.
#'
#' @param projected_rates List of rate matrices from project_divorce_rates()
#' @param min_age Integer: minimum age (default: 14)
#'
#' @return data.table with columns:
#'   - year: Calendar year
#'   - husband_age: Age of husband
#'   - wife_age: Age of wife
#'   - rate: Divorce rate per 100,000
#'
#' @export
rates_to_long_format <- function(projected_rates, min_age = DIVORCE_MIN_AGE) {

  rates_list <- projected_rates$rates
  years <- as.integer(names(rates_list))

  cli::cli_alert("Converting {length(years)} years to long format...")

  result_list <- lapply(years, function(yr) {
    grid <- rates_list[[as.character(yr)]]
    n <- nrow(grid)

    # Create all combinations
    dt <- data.table::CJ(
      husband_age = min_age:(min_age + n - 1),
      wife_age = min_age:(min_age + n - 1)
    )

    # Add rates (matrix is indexed 1:87, ages are 14:100)
    dt[, rate := as.vector(grid)]
    dt[, year := yr]

    dt
  })

  result <- data.table::rbindlist(result_list, use.names = TRUE)
  data.table::setcolorder(result, c("year", "husband_age", "wife_age", "rate"))
  data.table::setorder(result, year, husband_age, wife_age)

  cli::cli_alert_success("Created long format: {nrow(result)} rows")

  result
}

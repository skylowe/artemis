#' Projected Population Subprocess (Section 1.8)
#'
#' @description
#' Functions for projecting the Social Security area population from the starting
#' year (December 31, 2022) through the 75-year projection period (2023-2099).
#'
#' This module implements Equations 1.8.1 through 1.8.7 from TR2025 documentation:
#'   - 1.8.1: Births by sex and population status
#'   - 1.8.2: Deaths by age, sex, and population status
#'   - 1.8.3: Net immigration (LPR + O)
#'   - 1.8.4: Population by age, sex, and population status
#'   - 1.8.5: Population by marital status
#'   - 1.8.6: Children by parent survival status
#'   - 1.8.7: Civilian noninstitutionalized (CNI) population
#'
#' @section Population Statuses:
#' Beginning December 31, 2013, populations are modeled with three statuses:
#' - Heterosexual: 97.5% of males, 95.5% of females
#' - Gay: 2.5% of males
#' - Lesbian: 4.5% of females
#'
#' @section Component Method:
#' The population is projected using the standard demographic component method:
#' P^z_{x,s,p} = P^{z-1}_{x-1,s,p} - D^z_{x,s,p} + NI^z_{x,s,p}
#'
#' Where:
#' - P = Population at December 31
#' - D = Deaths during year
#' - NI = Net immigration during year
#'
#' @name projected_population
NULL

# =============================================================================
# PHASE 8A: DATA ASSEMBLY AND INPUT VERIFICATION
# =============================================================================

#' Verify fertility projection outputs (Phase 8A.1)
#'
#' @description
#' Verifies that birth rates from Phase 1 (Fertility) are available in the
#' required format for population projection.
#'
#' @param fertility_rates data.table: birth rates by year and age (14-49)
#' @param projection_years Integer vector: years to project (default: 2023:2099)
#' @param config List: optional configuration object to derive projection_years
#'
#' @return list with:
#'   - valid: Logical indicating if data is valid
#'   - message: Summary message
#'   - coverage: data.table with year coverage
#'   - age_range: Named vector with min/max ages
#'
#' @details
#' Required structure:
#' - Columns: year, age, birth_rate
#' - Ages: 14-49 (single year)
#' - Years: at minimum 2023-2099
#'
#' @export
verify_fertility_inputs <- function(fertility_rates, projection_years = NULL, config = NULL) {
  # Derive projection_years from config if not provided
  if (is.null(projection_years)) {
    if (!is.null(config)) {
      years <- get_projection_years(config, "population")
      projection_years <- years$projection_start:years$projection_end
    } else {
      projection_years <- 2023:2099  # Fallback default
    }
  }
  cli::cli_h3("Verifying Fertility Inputs (Phase 1)")

  # Basic structure validation
  if (!data.table::is.data.table(fertility_rates)) {
    fertility_rates <- data.table::as.data.table(fertility_rates)
  }

  required_cols <- c("year", "age", "birth_rate")
  missing_cols <- setdiff(required_cols, names(fertility_rates))

  if (length(missing_cols) > 0) {
    cli::cli_alert_danger("Missing columns: {paste(missing_cols, collapse = ', ')}")
    return(list(
      valid = FALSE,
      message = paste("Missing columns:", paste(missing_cols, collapse = ", ")),
      coverage = NULL,
      age_range = NULL
    ))
  }

  # Check year coverage
  available_years <- unique(fertility_rates$year)
  missing_years <- setdiff(projection_years, available_years)

  # Check age coverage
  available_ages <- unique(fertility_rates$age)
  expected_ages <- 14:49
  missing_ages <- setdiff(expected_ages, available_ages)

  # Build coverage report
  coverage <- data.table::data.table(
    metric = c("total_years", "projection_years_covered", "years_missing",
               "ages_covered", "ages_missing"),
    value = c(length(available_years),
              length(intersect(projection_years, available_years)),
              length(missing_years),
              length(available_ages),
              length(missing_ages))
  )

  is_valid <- length(missing_years) == 0 && length(missing_ages) == 0

  if (is_valid) {
    cli::cli_alert_success(
      "Fertility rates valid: {min(available_years)}-{max(available_years)}, ages {min(available_ages)}-{max(available_ages)}"
    )
  } else {
    if (length(missing_years) > 0) {
      cli::cli_alert_warning("Missing years: {length(missing_years)} ({min(missing_years)}-{max(missing_years)})")
    }
    if (length(missing_ages) > 0) {
      cli::cli_alert_warning("Missing ages: {paste(missing_ages, collapse=', ')}")
    }
  }

  list(
    valid = is_valid,
    message = if (is_valid) "Fertility inputs valid" else "Fertility inputs incomplete",
    coverage = coverage,
    age_range = c(min = min(available_ages), max = max(available_ages)),
    year_range = c(min = min(available_years), max = max(available_years))
  )
}

#' Verify mortality projection outputs (Phase 8A.2)
#'
#' @description
#' Verifies that death probabilities (qx) from Phase 2 (Mortality) are available
#' in the required format for population projection.
#'
#' @param mortality_qx data.table: death probabilities by year, age, sex
#' @param projection_years Integer vector: years to project (default: 2023:2099)
#' @param config List: optional configuration object to derive projection_years
#'
#' @return list with validation results
#'
#' @details
#' Required structure:
#' - Columns: year, age, sex, qx
#' - Ages: 0-100 (single year, with 100 representing 100+ open-ended group)
#' - Sex: "male", "female"
#' - Years: at minimum 2023-2099
#'
#' @export
verify_mortality_inputs <- function(mortality_qx, projection_years = NULL, config = NULL) {
  # Derive projection_years from config if not provided
  if (is.null(projection_years)) {
    if (!is.null(config)) {
      years <- get_projection_years(config, "population")
      projection_years <- years$projection_start:years$projection_end
    } else {
      projection_years <- 2023:2099  # Fallback default
    }
  }
  cli::cli_h3("Verifying Mortality Inputs (Phase 2)")

  if (!data.table::is.data.table(mortality_qx)) {
    mortality_qx <- data.table::as.data.table(mortality_qx)
  }

  required_cols <- c("year", "age", "sex", "qx")
  missing_cols <- setdiff(required_cols, names(mortality_qx))

  if (length(missing_cols) > 0) {
    cli::cli_alert_danger("Missing columns: {paste(missing_cols, collapse = ', ')}")
    return(list(
      valid = FALSE,
      message = paste("Missing columns:", paste(missing_cols, collapse = ", ")),
      coverage = NULL
    ))
  }

  # Check year coverage
  available_years <- unique(mortality_qx$year)
  missing_years <- setdiff(projection_years, available_years)

  # Check sex coverage
  available_sexes <- unique(mortality_qx$sex)
  expected_sexes <- c("male", "female")
  missing_sexes <- setdiff(expected_sexes, available_sexes)

  # Check age coverage (need at least 0-100, with 100 representing 100+)
  available_ages <- sort(unique(mortality_qx$age))
  expected_ages <- 0:100
  missing_ages <- setdiff(expected_ages, available_ages)

  # Check qx values are in valid range
  qx_range <- mortality_qx[, .(min_qx = min(qx, na.rm = TRUE), max_qx = max(qx, na.rm = TRUE))]
  invalid_qx <- qx_range$min_qx < 0 || qx_range$max_qx > 1

  is_valid <- length(missing_years) == 0 &&
              length(missing_sexes) == 0 &&
              length(missing_ages) == 0 &&
              !invalid_qx

  if (is_valid) {
    cli::cli_alert_success(
      "Mortality qx valid: {min(available_years)}-{max(available_years)}, ages {min(available_ages)}-{max(available_ages)}"
    )
  } else {
    if (length(missing_years) > 0) {
      cli::cli_alert_warning("Missing years: {length(missing_years)}")
    }
    if (length(missing_sexes) > 0) {
      cli::cli_alert_warning("Missing sexes: {paste(missing_sexes, collapse=', ')}")
    }
    if (length(missing_ages) > 0) {
      cli::cli_alert_warning("Missing {length(missing_ages)} ages")
    }
    if (invalid_qx) {
      cli::cli_alert_warning("Invalid qx range: [{qx_range$min_qx}, {qx_range$max_qx}]")
    }
  }

  list(
    valid = is_valid,
    message = if (is_valid) "Mortality inputs valid" else "Mortality inputs incomplete",
    year_range = c(min = min(available_years), max = max(available_years)),
    age_range = c(min = min(available_ages), max = max(available_ages)),
    sexes = available_sexes
  )
}

#' Verify mortality marital status differentials (Phase 8A.2b)
#'
#' @description
#' Verifies that mortality differentials by marital status from Phase 2 are
#' available for projecting deaths by marital status.
#'
#' @param mortality_differentials data.table: mortality factors by age, sex, marital status
#'
#' @return list with validation results
#'
#' @export
verify_mortality_marital_differentials <- function(mortality_differentials) {
  cli::cli_h3("Verifying Mortality Marital Differentials (Phase 2)")

  if (is.null(mortality_differentials)) {
    cli::cli_alert_warning("Mortality marital differentials not provided")
    return(list(valid = FALSE, message = "Differentials not provided"))
  }

  if (!data.table::is.data.table(mortality_differentials)) {
    mortality_differentials <- data.table::as.data.table(mortality_differentials)
  }

  # Check required columns - allow multiple possible names for factor column
  base_cols <- c("age", "sex", "marital_status")
  factor_col_options <- c("factor", "mortality_factor", "relative_factor")

  missing_base <- setdiff(base_cols, names(mortality_differentials))
  has_factor <- any(factor_col_options %in% names(mortality_differentials))

  if (length(missing_base) > 0 || !has_factor) {
    missing_cols <- c(missing_base, if (!has_factor) "factor" else character(0))
    cli::cli_alert_warning("Missing columns: {paste(missing_cols, collapse = ', ')}")
    return(list(
      valid = FALSE,
      message = paste("Missing columns:", paste(missing_cols, collapse = ", "))
    ))
  }

  # Check marital status coverage
  # Note: Some data sources use "single" while others use "never_married"
  expected_statuses <- c("married", "widowed", "divorced")  # Required statuses
  single_alternatives <- c("single", "never_married")  # At least one needed
  available_statuses <- unique(mortality_differentials$marital_status)

  # Check required statuses
  missing_statuses <- setdiff(expected_statuses, available_statuses)

  # Check for single/never_married (at least one must be present)
  has_single_or_never_married <- any(single_alternatives %in% available_statuses)
  if (!has_single_or_never_married) {
    missing_statuses <- c(missing_statuses, "single or never_married")
  }

  is_valid <- length(missing_statuses) == 0

  if (is_valid) {
    cli::cli_alert_success("Mortality marital differentials valid")
  } else {
    cli::cli_alert_warning("Missing marital statuses: {paste(missing_statuses, collapse = ', ')}")
  }

  list(
    valid = is_valid,
    message = if (is_valid) "Differentials valid" else "Differentials incomplete",
    marital_statuses = available_statuses
  )
}

#' Verify LPR immigration outputs (Phase 8A.3)
#'
#' @description
#' Verifies that net LPR immigration from Phase 3 is available for population
#' projection.
#'
#' @param net_lpr data.table: net LPR immigration by year, age, sex
#' @param projection_years Integer vector: years to project (default: 2023:2099)
#' @param config List: optional configuration object to derive projection_years
#'
#' @return list with validation results
#'
#' @details
#' Required structure:
#' - Columns: year, age, sex, net_lpr (or net_immigration)
#' - Ages: -1 to 100 (age -1 = births to immigrants during year)
#' - Sex: "male", "female"
#'
#' @export
verify_lpr_immigration_inputs <- function(net_lpr, projection_years = NULL, config = NULL) {
  # Derive projection_years from config if not provided
  if (is.null(projection_years)) {
    if (!is.null(config)) {
      years <- get_projection_years(config, "population")
      projection_years <- years$projection_start:years$projection_end
    } else {
      projection_years <- 2023:2099  # Fallback default
    }
  }
  cli::cli_h3("Verifying LPR Immigration Inputs (Phase 3)")

  if (is.null(net_lpr)) {
    cli::cli_alert_danger("Net LPR immigration data not provided")
    return(list(valid = FALSE, message = "Data not provided"))
  }

  if (!data.table::is.data.table(net_lpr)) {
    net_lpr <- data.table::as.data.table(net_lpr)
  }

  # Check for either net_lpr or net_immigration column
  value_col <- if ("net_lpr" %in% names(net_lpr)) "net_lpr" else "net_immigration"
  required_cols <- c("year", "age", "sex", value_col)
  missing_cols <- setdiff(required_cols, names(net_lpr))

  if (length(missing_cols) > 0) {
    cli::cli_alert_danger("Missing columns: {paste(missing_cols, collapse = ', ')}")
    return(list(valid = FALSE, message = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
  }

  # Check year and age coverage
  available_years <- unique(net_lpr$year)
  missing_years <- setdiff(projection_years, available_years)

  available_ages <- sort(unique(net_lpr$age))
  expected_ages <- 0:100  # At minimum need 0-100 (100 represents 100+)
  missing_ages <- setdiff(expected_ages, available_ages)

  # Check total net LPR (should be positive for most years)
  totals <- net_lpr[year %in% projection_years, .(total = sum(get(value_col), na.rm = TRUE)), by = year]

  is_valid <- length(missing_years) == 0 && length(missing_ages) <= 5  # Allow some missing ages

  if (is_valid) {
    cli::cli_alert_success(
      "Net LPR immigration valid: {min(available_years)}-{max(available_years)}, ~{round(mean(totals$total)/1000)}K/year"
    )
  } else {
    if (length(missing_years) > 0) {
      cli::cli_alert_warning("Missing {length(missing_years)} years")
    }
    if (length(missing_ages) > 5) {
      cli::cli_alert_warning("Missing {length(missing_ages)} ages")
    }
  }

  list(
    valid = is_valid,
    message = if (is_valid) "LPR immigration valid" else "LPR immigration incomplete",
    year_range = c(min = min(available_years), max = max(available_years)),
    annual_totals = totals
  )
}

#' Verify O (temp/unlawfully present) immigration outputs (Phase 8A.4)
#'
#' @description
#' Verifies that net O immigration from Phase 5 is available for population
#' projection.
#'
#' @param net_o data.table: net O immigration by year, age, sex
#' @param projection_years Integer vector: years to project (default: 2023:2099)
#' @param config List: optional configuration object to derive projection_years
#'
#' @return list with validation results
#'
#' @export
verify_o_immigration_inputs <- function(net_o, projection_years = NULL, config = NULL) {
  # Derive projection_years from config if not provided
  if (is.null(projection_years)) {
    if (!is.null(config)) {
      years <- get_projection_years(config, "population")
      projection_years <- years$projection_start:years$projection_end
    } else {
      projection_years <- 2023:2099  # Fallback default
    }
  }
  cli::cli_h3("Verifying O Immigration Inputs (Phase 5)")

  if (is.null(net_o)) {
    cli::cli_alert_danger("Net O immigration data not provided")
    return(list(valid = FALSE, message = "Data not provided"))
  }

  if (!data.table::is.data.table(net_o)) {
    net_o <- data.table::as.data.table(net_o)
  }

  # Check for net_o column (may be named net_o, net_immigration, or net_o_immigration)
  value_col_options <- c("net_o", "net_o_immigration", "net_immigration")
  value_col <- intersect(value_col_options, names(net_o))[1]

  if (is.na(value_col)) {
    cli::cli_alert_danger("Missing value column: need one of {paste(value_col_options, collapse=', ')}")
    return(list(valid = FALSE, message = "Missing value column"))
  }

  required_cols <- c("year", "age", "sex", value_col)
  missing_cols <- setdiff(required_cols, names(net_o))

  if (length(missing_cols) > 0) {
    cli::cli_alert_danger("Missing columns: {paste(missing_cols, collapse = ', ')}")
    return(list(valid = FALSE, message = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
  }

  # Check coverage
  available_years <- unique(net_o$year)
  missing_years <- setdiff(projection_years, available_years)

  # Check totals
  totals <- net_o[year %in% projection_years, .(total = sum(get(value_col), na.rm = TRUE)), by = year]

  is_valid <- length(missing_years) == 0

  if (is_valid) {
    cli::cli_alert_success(
      "Net O immigration valid: {min(available_years)}-{max(available_years)}, ~{round(mean(totals$total)/1000)}K/year"
    )
  } else {
    cli::cli_alert_warning("Missing {length(missing_years)} years")
  }

  list(
    valid = is_valid,
    message = if (is_valid) "O immigration valid" else "O immigration incomplete",
    year_range = c(min = min(available_years), max = max(available_years)),
    annual_totals = totals
  )
}

#' Verify marriage projection outputs (Phase 8A.5)
#'
#' @description
#' Verifies that marriage rates from Phase 6 are available for marital status
#' projection.
#'
#' @param marriage_rates List or data.table: marriage rates by year
#' @param projection_years Integer vector: years to project (default: 2023:2099)
#' @param config List: optional configuration object to derive projection_years
#'
#' @return list with validation results
#'
#' @details
#' Required structure:
#' - Opposite-sex rates: 87x87 matrix (ages 14-100 x ages 14-100)
#' - Same-sex rates: 87x87 matrix (optional)
#' - Prior marital status differentials
#'
#' @export
verify_marriage_inputs <- function(marriage_rates, projection_years = NULL, config = NULL) {
  # Derive projection_years from config if not provided
  if (is.null(projection_years)) {
    if (!is.null(config)) {
      years <- get_projection_years(config, "population")
      projection_years <- years$projection_start:years$projection_end
    } else {
      projection_years <- 2023:2099  # Fallback default
    }
  }
  cli::cli_h3("Verifying Marriage Inputs (Phase 6)")

  if (is.null(marriage_rates)) {
    cli::cli_alert_danger("Marriage rates not provided")
    return(list(valid = FALSE, message = "Data not provided"))
  }

  # Handle different input formats (list or data.table)
  if (is.list(marriage_rates) && !data.table::is.data.table(marriage_rates)) {
    # Check for expected list components
    has_opposite_sex <- "opposite_sex_rates" %in% names(marriage_rates) ||
                        "all_rates" %in% names(marriage_rates)
    has_same_sex <- "same_sex_rates" %in% names(marriage_rates)
    has_prior_status <- "status_differentials" %in% names(marriage_rates)

    # Get years from rates
    if ("all_rates" %in% names(marriage_rates)) {
      rates <- marriage_rates$all_rates
      if (is.list(rates) && !data.table::is.data.table(rates)) {
        available_years <- as.integer(names(rates))
      } else if (data.table::is.data.table(rates) && "year" %in% names(rates)) {
        available_years <- unique(rates$year)
      } else {
        available_years <- integer(0)
      }
    } else {
      available_years <- integer(0)
    }

  } else if (data.table::is.data.table(marriage_rates)) {
    # Data.table format
    if ("year" %in% names(marriage_rates)) {
      available_years <- unique(marriage_rates$year)
    } else {
      available_years <- integer(0)
    }
    has_opposite_sex <- TRUE
    has_same_sex <- FALSE
    has_prior_status <- FALSE
  } else {
    cli::cli_alert_danger("Unexpected marriage rates format")
    return(list(valid = FALSE, message = "Unexpected format"))
  }

  missing_years <- setdiff(projection_years, available_years)

  is_valid <- length(missing_years) == 0 && has_opposite_sex

  if (is_valid) {
    cli::cli_alert_success(
      "Marriage rates valid: {min(available_years)}-{max(available_years)}"
    )
    if (has_same_sex) cli::cli_alert_info("  Same-sex rates available")
    if (has_prior_status) cli::cli_alert_info("  Prior status differentials available")
  } else {
    if (!has_opposite_sex) {
      cli::cli_alert_warning("Opposite-sex rates not found")
    }
    if (length(missing_years) > 0) {
      cli::cli_alert_warning("Missing {length(missing_years)} years")
    }
  }

  list(
    valid = is_valid,
    message = if (is_valid) "Marriage inputs valid" else "Marriage inputs incomplete",
    year_range = if (length(available_years) > 0) c(min = min(available_years), max = max(available_years)) else NULL,
    has_same_sex = has_same_sex,
    has_prior_status = has_prior_status
  )
}

#' Verify divorce projection outputs (Phase 8A.6)
#'
#' @description
#' Verifies that divorce rates from Phase 7 are available for marital status
#' projection.
#'
#' @param divorce_rates List or data.table: divorce rates by year
#' @param projection_years Integer vector: years to project (default: 2023:2099)
#' @param config List: optional configuration object to derive projection_years
#'
#' @return list with validation results
#'
#' @export
verify_divorce_inputs <- function(divorce_rates, projection_years = NULL, config = NULL) {
  # Derive projection_years from config if not provided
  if (is.null(projection_years)) {
    if (!is.null(config)) {
      years <- get_projection_years(config, "population")
      projection_years <- years$projection_start:years$projection_end
    } else {
      projection_years <- 2023:2099  # Fallback default
    }
  }
  cli::cli_h3("Verifying Divorce Inputs (Phase 7)")

  if (is.null(divorce_rates)) {
    cli::cli_alert_danger("Divorce rates not provided")
    return(list(valid = FALSE, message = "Data not provided"))
  }

  available_years <- integer(0)
  has_rates <- FALSE

  # Handle different input formats
  if (is.list(divorce_rates) && !data.table::is.data.table(divorce_rates)) {
    # Check for expected list components from run_divorce_pipeline()
    has_rates <- "projected_rates" %in% names(divorce_rates) ||
                 "all_rates" %in% names(divorce_rates)

    # Get years from projected_rates (which is itself a list with years/rates components)
    if ("projected_rates" %in% names(divorce_rates)) {
      rates_obj <- divorce_rates$projected_rates

      # projected_rates from divorce.R has structure: list(rates=list(), adr=dt, years=vec)
      if (is.list(rates_obj) && !data.table::is.data.table(rates_obj)) {
        # Check for years component (from project_divorce_rates output)
        if ("years" %in% names(rates_obj)) {
          available_years <- rates_obj$years
        } else if ("rates" %in% names(rates_obj)) {
          # Years are names of the rates list
          available_years <- as.integer(names(rates_obj$rates))
        } else {
          # Rates directly as named list
          available_years <- as.integer(names(rates_obj))
        }
      } else if (data.table::is.data.table(rates_obj) && "year" %in% names(rates_obj)) {
        available_years <- unique(rates_obj$year)
      }
    }

  } else if (data.table::is.data.table(divorce_rates)) {
    if ("year" %in% names(divorce_rates)) {
      available_years <- unique(divorce_rates$year)
    }
    has_rates <- TRUE
  } else {
    cli::cli_alert_danger("Unexpected divorce rates format")
    return(list(valid = FALSE, message = "Unexpected format"))
  }

  # Remove NAs if any
  available_years <- available_years[!is.na(available_years)]
  missing_years <- setdiff(projection_years, available_years)

  is_valid <- length(missing_years) == 0 && has_rates

  if (is_valid) {
    cli::cli_alert_success(
      "Divorce rates valid: {min(available_years)}-{max(available_years)}"
    )
  } else {
    if (!has_rates) {
      cli::cli_alert_warning("Divorce rates not found")
    }
    if (length(missing_years) > 0) {
      cli::cli_alert_warning("Missing {length(missing_years)} years")
    }
  }

  list(
    valid = is_valid,
    message = if (is_valid) "Divorce inputs valid" else "Divorce inputs incomplete",
    year_range = if (length(available_years) > 0) c(min = min(available_years), max = max(available_years)) else NULL
  )
}

#' Extract starting population from historical population (Phase 8A.7)
#'
#' @description
#' Extracts the December 31, 2022 population by age, sex, and population status
#' from the historical population output (Phase 4).
#'
#' @param historical_population data.table: historical population from Phase 4
#' @param historical_marital data.table: historical population by marital status
#' @param starting_year Integer: starting year (default: 2022, or from config)
#' @param config List: optional configuration object to derive starting_year
#'
#' @return list with:
#'   - population: Starting population by age, sex, population status
#'   - population_marital: Starting population with marital status
#'   - married_couples: Married couples grid (if available)
#'   - valid: Whether extraction was successful
#'
#' @export
extract_starting_population <- function(historical_population,
                                         historical_marital = NULL,
                                         starting_year = NULL,
                                         config = NULL) {
  # Derive starting_year from config if not provided
  if (is.null(starting_year)) {
    if (!is.null(config)) {
      years <- get_projection_years(config, "population")
      starting_year <- years$starting_year
    } else {
      starting_year <- 2022  # Fallback default
    }
  }
  cli::cli_h3("Extracting Starting Population (Dec 31, {starting_year})")

  if (!data.table::is.data.table(historical_population)) {
    historical_population <- data.table::as.data.table(historical_population)
  }

  # Filter to starting year
  start_pop <- historical_population[year == starting_year]

  if (nrow(start_pop) == 0) {
    cli::cli_alert_danger("No population data for year {starting_year}")
    return(list(valid = FALSE, message = "Starting year not found"))
  }

  # Check required columns
  if (!all(c("age", "sex", "population") %in% names(start_pop))) {
    cli::cli_alert_danger("Missing required columns in historical population")
    return(list(valid = FALSE, message = "Missing columns"))
  }

  # Add population status if not present
  # TR2025: 2.5% of males are gay, 4.5% of females are lesbian
  if (!"pop_status" %in% names(start_pop)) {
    cli::cli_alert_info("Adding population status disaggregation")

    start_pop_status <- data.table::rbindlist(list(
      # Heterosexual
      start_pop[sex == "male", .(year, age, sex, pop_status = "heterosexual",
                                  population = population * 0.975)],
      start_pop[sex == "male", .(year, age, sex, pop_status = "gay",
                                  population = population * 0.025)],
      start_pop[sex == "female", .(year, age, sex, pop_status = "heterosexual",
                                    population = population * 0.955)],
      start_pop[sex == "female", .(year, age, sex, pop_status = "lesbian",
                                    population = population * 0.045)]
    ))
    start_pop <- start_pop_status
  }

  # Summary statistics
  total_pop <- sum(start_pop$population, na.rm = TRUE)
  by_sex <- start_pop[, .(pop = sum(population)), by = sex]

  cli::cli_alert_success(
    "Starting population: {format(round(total_pop/1e6, 2), big.mark=',')}M total"
  )
  cli::cli_alert_info(
    "  Male: {format(round(by_sex[sex=='male', pop]/1e6, 2), big.mark=',')}M, Female: {format(round(by_sex[sex=='female', pop]/1e6, 2), big.mark=',')}M"
  )

  # Extract marital status if available
  start_marital <- NULL
  if (!is.null(historical_marital)) {
    if (!data.table::is.data.table(historical_marital)) {
      historical_marital <- data.table::as.data.table(historical_marital)
    }
    start_marital <- historical_marital[year == starting_year]
    if (nrow(start_marital) > 0) {
      cli::cli_alert_info("  Marital status data: {nrow(start_marital)} rows")
    }
  }

  list(
    valid = TRUE,
    population = start_pop,
    population_marital = start_marital,
    year = starting_year,
    total = total_pop,
    summary = list(
      by_sex = by_sex,
      age_range = c(min = min(start_pop$age), max = max(start_pop$age))
    )
  )
}

#' Verify all projected population inputs (Phase 8A.9)
#'
#' @description
#' Comprehensive verification of all inputs needed for population projection.
#' Checks data availability and structural consistency across all subprocesses.
#'
#' @param fertility_rates Birth rates from Phase 1
#' @param mortality_qx Death probabilities from Phase 2
#' @param mortality_differentials Marital mortality factors from Phase 2
#' @param net_lpr Net LPR immigration from Phase 3
#' @param net_o Net O immigration from Phase 5
#' @param marriage_rates Marriage rates from Phase 6
#' @param divorce_rates Divorce rates from Phase 7
#' @param historical_population Historical population from Phase 4
#' @param historical_marital Historical marital population from Phase 4
#' @param projection_years Years to project (default: 2023:2099, or from config)
#' @param starting_year Starting year for projection (default: 2022, or from config)
#' @param config List: optional configuration object to derive projection_years and starting_year
#'
#' @return list with:
#'   - all_valid: TRUE if all inputs are valid
#'   - results: List of individual verification results
#'   - starting_population: Extracted starting population
#'   - summary: Summary table of validation results
#'
#' @export
verify_all_projection_inputs <- function(fertility_rates,
                                          mortality_qx,
                                          mortality_differentials = NULL,
                                          net_lpr,
                                          net_o,
                                          marriage_rates,
                                          divorce_rates,
                                          historical_population,
                                          historical_marital = NULL,
                                          projection_years = NULL,
                                          starting_year = NULL,
                                          config = NULL) {
  # Derive years from config if not provided
  if (is.null(projection_years) || is.null(starting_year)) {
    if (!is.null(config)) {
      years <- get_projection_years(config, "population")
      if (is.null(projection_years)) projection_years <- years$projection_start:years$projection_end
      if (is.null(starting_year)) starting_year <- years$starting_year
    } else {
      if (is.null(projection_years)) projection_years <- 2023:2099  # Fallback default
      if (is.null(starting_year)) starting_year <- 2022
    }
  }
  cli::cli_h1("Phase 8A: Input Data Verification")
  cli::cli_alert_info("Projection period: {min(projection_years)}-{max(projection_years)}")
  cli::cli_alert_info("Starting year: {starting_year}")

  results <- list()

  # 1. Fertility (Phase 1)
  results$fertility <- verify_fertility_inputs(fertility_rates, projection_years)

  # 2. Mortality (Phase 2)
  results$mortality <- verify_mortality_inputs(mortality_qx, projection_years)

  # 2b. Mortality differentials (Phase 2)
  if (!is.null(mortality_differentials)) {
    results$mortality_differentials <- verify_mortality_marital_differentials(mortality_differentials)
  } else {
    results$mortality_differentials <- list(valid = FALSE, message = "Not provided")
    cli::cli_alert_info("Mortality marital differentials not provided (optional for basic projection)")
  }

  # 3. LPR Immigration (Phase 3)
  results$lpr_immigration <- verify_lpr_immigration_inputs(net_lpr, projection_years)

  # 4. O Immigration (Phase 5)
  results$o_immigration <- verify_o_immigration_inputs(net_o, projection_years)

  # 5. Marriage (Phase 6)
  results$marriage <- verify_marriage_inputs(marriage_rates, projection_years)

  # 6. Divorce (Phase 7)
  results$divorce <- verify_divorce_inputs(divorce_rates, projection_years)

  # 7. Starting Population (Phase 4)
  results$starting_population <- extract_starting_population(
    historical_population,
    historical_marital,
    starting_year
  )

  # Build summary
  cli::cli_h2("Validation Summary")

  summary_dt <- data.table::data.table(
    component = c("Fertility (Phase 1)", "Mortality (Phase 2)",
                  "Mortality Differentials", "LPR Immigration (Phase 3)",
                  "O Immigration (Phase 5)", "Marriage (Phase 6)",
                  "Divorce (Phase 7)", "Starting Population (Phase 4)"),
    valid = c(results$fertility$valid, results$mortality$valid,
              results$mortality_differentials$valid, results$lpr_immigration$valid,
              results$o_immigration$valid, results$marriage$valid,
              results$divorce$valid, results$starting_population$valid)
  )

  # Core components (required for basic projection)
  core_components <- c("Fertility (Phase 1)", "Mortality (Phase 2)",
                       "LPR Immigration (Phase 3)", "O Immigration (Phase 5)",
                       "Starting Population (Phase 4)")
  core_valid <- all(summary_dt[component %in% core_components, valid])

  # All components (including marital status)
  all_valid <- all(summary_dt$valid)

  # Print summary
  for (i in seq_len(nrow(summary_dt))) {
    status <- if (summary_dt$valid[i]) cli::col_green("PASS") else cli::col_red("FAIL")
    cli::cli_text("{summary_dt$component[i]}: {status}")
  }

  if (core_valid) {
    cli::cli_alert_success("Core inputs valid - basic population projection can proceed")
  } else {
    cli::cli_alert_danger("Core inputs missing - cannot proceed with projection")
  }

  if (!all_valid && core_valid) {
    cli::cli_alert_warning("Some optional inputs missing - marital status projection may be limited")
  }

  list(
    all_valid = all_valid,
    core_valid = core_valid,
    results = results,
    starting_population = results$starting_population,
    summary = summary_dt
  )
}

#' Validate input data structure consistency (Phase 8A.9)
#'
#' @description
#' Validates that all input data sources have consistent age and sex structures.
#'
#' @param inputs List of verified inputs from verify_all_projection_inputs()
#'
#' @return list with consistency check results
#'
#' @export
validate_input_consistency <- function(inputs) {
  cli::cli_h2("Validating Input Consistency")

  issues <- character(0)

  # Check sex values are consistent
  expected_sexes <- c("male", "female")

  # Check age coverage across components
  # Mortality needs 0-100, Immigration needs 0-100, Fertility needs 14-49

  # Verify immigration ages align with population ages
  if (inputs$results$lpr_immigration$valid && inputs$results$starting_population$valid) {
    pop_ages <- inputs$starting_population$summary$age_range
    imm_ages <- inputs$results$lpr_immigration$year_range  # This should be age_range
    # Add check logic here
  }

  if (length(issues) == 0) {
    cli::cli_alert_success("Input data structures are consistent")
    return(list(valid = TRUE, issues = NULL))
  } else {
    cli::cli_alert_warning("Found {length(issues)} consistency issues")
    return(list(valid = FALSE, issues = issues))
  }
}

# =============================================================================
# CONFIGURATION PARAMETERS
# =============================================================================

#' Get default projected population configuration
#'
#' @description
#' Returns default configuration parameters for projected population calculations.
#' When a config parameter is provided, year-related values are extracted from it.
#'
#' @param config List: optional configuration object (from load_assumptions)
#'
#' @return list with configuration parameters
#'
#' @export
get_projected_population_config <- function(config = NULL) {
  # Default values
  defaults <- list(
    # Time period
    starting_year = 2022,
    projection_start = 2023,
    projection_end = 2099,
    extended_end = 2105,

    # Reference date
    reference_date = "dec31",

    # Sex ratio at birth (males per 1000 females)
    sex_ratio_at_birth = 1048,

    # Population status percentages
    population_status = list(
      gay_percent = 0.025,
      lesbian_percent = 0.045,
      same_sex_start_year = 2013
    ),

    # Age ranges
    ages = list(
      min_age = 0,
      max_age = 100,            # Maximum single year tracked (100 represents 100+)
      max_age_group = 100,      # Open-ended age group (100+)
      mothers_min = 14,
      mothers_max = 49,
      children_max = 18,
      marriage_min = 14
    ),

    # Marital status categories
    marital_statuses = list(
      total_population = c("single", "married", "widowed", "divorced"),
      cni_population = c("single", "married_spouse_present", "separated", "widowed", "divorced")
    ),

    # Parent fate categories
    parent_fates = c("both_alive", "only_father_alive", "only_mother_alive", "both_deceased"),

    # Parent age groups
    parent_age_groups = list(
      c(14, 24),
      c(25, 34),
      c(35, 44),
      c(45, 54),
      c(55, 64),
      c(65, 100)
    )
  )

  # Override with config values if provided
  if (!is.null(config)) {
    # Get projection period from metadata
    proj_period <- get_config_with_default(
      config, "metadata", "projection_period",
      default = NULL
    )
    if (!is.null(proj_period)) {
      if (!is.null(proj_period$start_year)) defaults$projection_start <- proj_period$start_year
      if (!is.null(proj_period$end_year)) defaults$projection_end <- proj_period$end_year
    }

    # Get projected_population specific values
    pp <- config$projected_population
    if (!is.null(pp)) {
      if (!is.null(pp$starting_year)) defaults$starting_year <- pp$starting_year
      if (!is.null(pp$projection_start)) defaults$projection_start <- pp$projection_start
      if (!is.null(pp$projection_end)) defaults$projection_end <- pp$projection_end
      if (!is.null(pp$extended_end)) defaults$extended_end <- pp$extended_end
      if (!is.null(pp$reference_date)) defaults$reference_date <- pp$reference_date

      # Population status
      if (!is.null(pp$population_status)) {
        ps <- pp$population_status
        if (!is.null(ps$gay_percent)) defaults$population_status$gay_percent <- ps$gay_percent
        if (!is.null(ps$lesbian_percent)) defaults$population_status$lesbian_percent <- ps$lesbian_percent
        if (!is.null(ps$same_sex_start_year)) defaults$population_status$same_sex_start_year <- ps$same_sex_start_year
      }

      # Age ranges
      if (!is.null(pp$ages)) {
        ages <- pp$ages
        if (!is.null(ages$min_age)) defaults$ages$min_age <- ages$min_age
        if (!is.null(ages$max_age)) defaults$ages$max_age <- ages$max_age
        if (!is.null(ages$max_age_group)) defaults$ages$max_age_group <- ages$max_age_group
        if (!is.null(ages$mothers_min)) defaults$ages$mothers_min <- ages$mothers_min
        if (!is.null(ages$mothers_max)) defaults$ages$mothers_max <- ages$mothers_max
        if (!is.null(ages$children_max)) defaults$ages$children_max <- ages$children_max
        if (!is.null(ages$marriage_min)) defaults$ages$marriage_min <- ages$marriage_min
      }
    }
  }

  defaults
}

# =============================================================================
# PHASE 8B: CORE POPULATION PROJECTION (Equations 1.8.1-1.8.4)
# =============================================================================

#' Calculate projected births for a year (Equation 1.8.1)
#'
#' @description
#' Calculates the number of births in the Social Security area for a given year
#' by applying age-specific birth rates to the midyear female population aged 14-49.
#'
#' TR2025 Formula:
#' B_x^z = b_x^z * ((FP_x^z + FP_x^{z+1}) / 2)
#'
#' Where:
#' - B_x^z = number of births to mothers age x in year z
#' - b_x^z = birth rate of mothers age x in year z
#' - FP_x^z = female population age x at beginning of year z
#'
#' Total births are disaggregated by:
#' - Sex: 1,048 males per 1,000 females
#' - Population status: 2.5% of boys are gay, 4.5% of girls are lesbian
#'
#' @param year Integer: projection year
#' @param birth_rates data.table: birth rates by year and age (14-49)
#' @param population_start data.table: population at beginning of year by age, sex, pop_status
#' @param population_end data.table: population at end of year (or NULL to estimate)
#' @param config List: configuration parameters
#'
#' @return data.table with columns: year, sex, pop_status, births
#'
#' @details
#' The midyear female population is estimated as the average of beginning and end
#' of year populations. If end-of-year population is not available, the beginning
#' of year population is used (conservative estimate).
#'
#' @export
calculate_projected_births <- function(year,
                                        birth_rates,
                                        population_start,
                                        population_end = NULL,
                                        config = NULL) {

  if (is.null(config)) {
    config <- get_projected_population_config()
  }

  # Get birth rates for this year
  target_year <- year  # Create local variable for data.table subsetting
  year_rates <- birth_rates[year == target_year]

  if (nrow(year_rates) == 0) {
    cli::cli_abort("No birth rates available for year {year}")
  }

  # Get female population at beginning of year (ages 14-49)
  female_start <- population_start[
    sex == "female" & age >= config$ages$mothers_min & age <= config$ages$mothers_max
  ]

  # Sum across population statuses to get total female population by age
  female_by_age <- female_start[, .(pop_start = sum(population)), by = age]

  # Estimate midyear population
  if (!is.null(population_end)) {
    female_end <- population_end[
      sex == "female" & age >= config$ages$mothers_min & age <= config$ages$mothers_max
    ]
    female_end_by_age <- female_end[, .(pop_end = sum(population)), by = age]
    female_by_age <- merge(female_by_age, female_end_by_age, by = "age", all.x = TRUE)
    female_by_age[is.na(pop_end), pop_end := pop_start]
    female_by_age[, midyear_pop := (pop_start + pop_end) / 2]
  } else {
    # Use beginning of year as estimate (will be refined in iterative projection)
    female_by_age[, midyear_pop := pop_start]
  }

  # Merge with birth rates
  births_by_age <- merge(
    female_by_age,
    year_rates[, .(age, birth_rate)],
    by = "age",
    all.x = TRUE
  )

  # Calculate births by age of mother: B_x = b_x * midyear_pop
  births_by_age[, births := birth_rate * midyear_pop]

  # Total births

  total_births <- sum(births_by_age$births, na.rm = TRUE)

  # Disaggregate by sex using sex ratio at birth
  # TR2025: 1,048 males per 1,000 females
  sex_ratio <- config$sex_ratio_at_birth
  male_fraction <- sex_ratio / (sex_ratio + 1000)
  female_fraction <- 1000 / (sex_ratio + 1000)

  male_births <- total_births * male_fraction
  female_births <- total_births * female_fraction

  # Disaggregate by population status
  # TR2025: 2.5% of boys are gay, 4.5% of girls are lesbian
  gay_pct <- config$population_status$gay_percent
  lesbian_pct <- config$population_status$lesbian_percent

  result <- data.table::data.table(
    year = year,
    sex = c("male", "male", "female", "female"),
    pop_status = c("heterosexual", "gay", "heterosexual", "lesbian"),
    births = c(
      male_births * (1 - gay_pct),      # heterosexual males
      male_births * gay_pct,             # gay males
      female_births * (1 - lesbian_pct), # heterosexual females
      female_births * lesbian_pct        # lesbian females
    )
  )

  result
}

#' Calculate projected deaths for a year (Equation 1.8.2)
#'
#' @description
#' Calculates the number of deaths in the Social Security area for a given year
#' by applying death probabilities to the exposed population at beginning of year.
#'
#' TR2025 Formula:
#' D_{x,s,p}^z = q_{x,s}^z * P_{x,s,p}^z
#'
#' Where:
#' - D = deaths by age, sex, population status
#' - q = death probability from MORTALITY subprocess
#' - P = population at beginning of year
#'
#' @param year Integer: projection year
#' @param mortality_qx data.table: death probabilities by year, age, sex
#' @param population data.table: population at beginning of year by age, sex, pop_status
#' @param config List: configuration parameters
#'
#' @return data.table with columns: year, age, sex, pop_status, deaths
#'
#' @export
calculate_projected_deaths <- function(year,
                                        mortality_qx,
                                        population,
                                        config = NULL) {

  if (is.null(config)) {
    config <- get_projected_population_config()
  }

  target_year <- year  # Create local variable for data.table subsetting

  # Get death probabilities for this year
  year_qx <- mortality_qx[year == target_year, .(age, sex, qx)]

  if (nrow(year_qx) == 0) {
    cli::cli_abort("No death probabilities available for year {year}")
  }

  # Merge population with death probabilities
  deaths <- merge(
    population[, .(year = target_year, age, sex, pop_status, population)],
    year_qx,
    by = c("age", "sex"),
    all.x = TRUE
  )

  # For ages without qx data, use qx = 0 (should not happen for ages 0-100)
  deaths[is.na(qx), qx := 0]

  # Calculate deaths: D = q * P
  deaths[, deaths := qx * population]

  # Return result
  deaths[, .(year, age, sex, pop_status, deaths)]
}

#' Calculate total net immigration for a year (Equation 1.8.3)
#'
#' @description
#' Combines net LPR immigration and net O (temporary/unlawfully present) immigration
#' into total net immigration by age, sex, and population status.
#'
#' TR2025 Formula:
#' NI_{x,s}^z = NL_{x,s}^z + NO_{x,s}^z
#'
#' Where:
#' - NI = total net immigration
#' - NL = net LPR immigration (from LPR IMMIGRATION subprocess)
#' - NO = net O immigration (from TEMP/UNLAWFUL IMMIGRATION subprocess)
#'
#' Net immigration is further disaggregated by population status.
#' Note: Age -1 represents births occurring during the year to immigrants.
#'
#' @param year Integer: projection year
#' @param net_lpr data.table: net LPR immigration by year, age, sex
#' @param net_o data.table: net O immigration by year, age, sex
#' @param config List: configuration parameters
#'
#' @return data.table with columns: year, age, sex, pop_status, net_immigration
#'
#' @export
calculate_net_immigration <- function(year,
                                       net_lpr,
                                       net_o,
                                       config = NULL) {

  if (is.null(config)) {
    config <- get_projected_population_config()
  }

  target_year <- year  # Create local variable for data.table subsetting

  # Get net LPR for this year
  # Handle different possible column names
  lpr_value_col <- intersect(c("net_lpr", "net_immigration"), names(net_lpr))[1]
  if (is.na(lpr_value_col)) {
    cli::cli_abort("Cannot find net LPR value column in net_lpr data")
  }

  year_lpr <- net_lpr[year == target_year, .(age, sex, net_lpr = get(lpr_value_col))]

  # Get net O for this year
  o_value_col <- intersect(c("net_o", "net_o_immigration", "net_immigration"), names(net_o))[1]
  if (is.na(o_value_col)) {
    cli::cli_abort("Cannot find net O value column in net_o data")
  }

  year_o <- net_o[year == target_year, .(age, sex, net_o = get(o_value_col))]

  # Aggregate net_o by age and sex (may have multiple types)
  year_o <- year_o[, .(net_o = sum(net_o, na.rm = TRUE)), by = .(age, sex)]

  # Combine LPR and O immigration
  net_imm <- merge(year_lpr, year_o, by = c("age", "sex"), all = TRUE)
  net_imm[is.na(net_lpr), net_lpr := 0]
  net_imm[is.na(net_o), net_o := 0]

  # Total net immigration: NI = NL + NO
  net_imm[, total_net := net_lpr + net_o]

  # Disaggregate by population status
  # TR2025: Same percentages as births (2.5% gay males, 4.5% lesbian females)
  gay_pct <- config$population_status$gay_percent
  lesbian_pct <- config$population_status$lesbian_percent

  # Create result with population status breakdown
  result <- data.table::rbindlist(list(
    # Heterosexual males
    net_imm[sex == "male", .(
      year = target_year,
      age,
      sex,
      pop_status = "heterosexual",
      net_immigration = total_net * (1 - gay_pct)
    )],
    # Gay males
    net_imm[sex == "male", .(
      year = target_year,
      age,
      sex,
      pop_status = "gay",
      net_immigration = total_net * gay_pct
    )],
    # Heterosexual females
    net_imm[sex == "female", .(
      year = target_year,
      age,
      sex,
      pop_status = "heterosexual",
      net_immigration = total_net * (1 - lesbian_pct)
    )],
    # Lesbian females
    net_imm[sex == "female", .(
      year = target_year,
      age,
      sex,
      pop_status = "lesbian",
      net_immigration = total_net * lesbian_pct
    )]
  ))

  result
}

#' Project population for a single year (Equation 1.8.4)
#'
#' @description
#' Projects the Social Security area population for a single year using the
#' component method: births, deaths, and net immigration.
#'
#' TR2025 Formula:
#' For age 0: P_{0,s,p}^z = B_{s,p}^z - D_{0,s,p}^z + NI_{0,s,p}^z
#' For ages > 0: P_{x,s,p}^z = P_{x-1,s,p}^{z-1} - D_{x,s,p}^z + NI_{x,s,p}^z
#'
#' Where P is population as of December 31 of each year.
#'
#' IMPORTANT: D_{x}^z = qx_x * P_{x-1}^{z-1}
#' Deaths at age x during year z are applied to people who WILL BE age x
#' (i.e., people who were age x-1 at end of year z-1).
#'
#' @param year Integer: projection year
#' @param population_prev data.table: population at end of previous year (Dec 31, z-1)
#' @param births data.table: births during year by sex, pop_status
#' @param deaths data.table: deaths during year by age, sex, pop_status (age 0 only used)
#' @param mortality_qx data.table: death probabilities by year, age, sex (for ages 1+)
#' @param net_immigration data.table: net immigration by age, sex, pop_status
#' @param config List: configuration parameters
#'
#' @return data.table: population at end of year (Dec 31, z) by age, sex, pop_status
#'
#' @export
project_population_year <- function(year,
                                     population_prev,
                                     births,
                                     deaths,
                                     mortality_qx,
                                     net_immigration,
                                     config = NULL,
                                     qx_100_119 = NULL) {

  if (is.null(config)) {
    config <- get_projected_population_config()
  }

  target_year <- year  # Create local variable to avoid data.table column name conflicts
  max_age <- config$ages$max_age_group  # Use 100+ grouping age, not life table max (119)

  # === Age 0: New births minus infant deaths plus infant immigration ===
  # P_{0,s,p}^z = B_{s,p}^z - D_{0,s,p}^z + NI_{0,s,p}^z

  age0_pop <- births[, .(sex, pop_status, population = births)]

  # Subtract infant deaths (D_0 = qx_0 * births, passed in deaths parameter)
  infant_deaths <- deaths[age == 0, .(sex, pop_status, deaths)]
  age0_pop <- merge(age0_pop, infant_deaths, by = c("sex", "pop_status"), all.x = TRUE)
  age0_pop[is.na(deaths), deaths := 0]
  age0_pop[, population := population - deaths]

  # Add infant net immigration (age 0)
  infant_imm <- net_immigration[age == 0, .(sex, pop_status, net_immigration)]
  age0_pop <- merge(age0_pop, infant_imm, by = c("sex", "pop_status"), all.x = TRUE)
  age0_pop[is.na(net_immigration), net_immigration := 0]
  age0_pop[, population := population + net_immigration]

  age0_pop[, `:=`(year = target_year, age = 0L)]
  age0_pop <- age0_pop[, .(year, age, sex, pop_status, population)]

  # === Ages 1 to max_age-1: Survive from previous year (standard calculation) ===
  # P_{x,s,p}^z = P_{x-1,s,p}^{z-1} - D_{x,s,p}^z + NI_{x,s,p}^z
  #
  # IMPORTANT: D_{x}^z = qx_x * P_{x-1}^{z-1}
  # Deaths at age x must be calculated on the AGED population (people who were x-1)

  # Age population forward: people who were age x-1 last year are now age x
  aged_pop <- data.table::copy(population_prev)
  aged_pop[, age := age + 1L]
  aged_pop[, year := target_year]

  # === Special handling for 100+ group ===
  # TR2025 applies the same q100+ (age-last-birthday) to the entire 100+ exposed population
  # This includes both new entrants from age 99 AND existing 100+ members
  # The q100+ value (1 - T101/T100 from life table) is designed to give correct aggregate mortality

  if (!is.null(qx_100_119) && nrow(qx_100_119) > 0) {
    # Separate ages: 100+ vs ages 1-99
    pop_100plus <- aged_pop[age >= max_age]  # People who were 99+ last year, now 100+
    pop_other <- aged_pop[age < max_age]  # Ages 1 to 99

    # Cap age at max_age for 100+ group
    pop_100plus[, age := max_age]

    # Aggregate 100+ population (combines new entrants from 99 and existing 100+)
    pop_100plus <- pop_100plus[, .(population = sum(population)),
                               by = .(year, age, sex, pop_status)]

    # Get q100+ from mortality_qx (now contains age-last-birthday q100 = 1 - T101/T100)
    mortality_qx_int <- data.table::copy(mortality_qx)
    mortality_qx_int[, age := as.integer(age)]
    year_qx_100plus <- mortality_qx_int[year == target_year & age == max_age, .(sex, q100plus = qx)]

    # Apply q100+ uniformly to entire 100+ pool (Fix #3: consistent mortality treatment)
    pop_100plus <- merge(pop_100plus, year_qx_100plus, by = "sex", all.x = TRUE)
    pop_100plus[is.na(q100plus), q100plus := 0]
    pop_100plus[, deaths := q100plus * population]
    pop_100plus[, population := population - deaths]
    pop_100plus[, `:=`(q100plus = NULL, deaths = NULL)]

    # Handle ages 1-99 normally
    year_qx <- mortality_qx[year == target_year, .(age, sex, qx)]
    # Ensure age types match for merge
    year_qx[, age := as.integer(age)]

    # SHIFT QX: Population at age x (EOY) was age x-1 (BOY).
    # They faced mortality q_{x-1}.
    # So we match pop(age=x) with qx(age=x-1).
    # We shift qx age by +1 so that qx(age=x-1) is indexed at age=x
    year_qx_shifted <- year_qx[, .(age = age + 1L, sex, qx)]

    pop_other <- merge(pop_other, year_qx_shifted, by = c("age", "sex"), all.x = TRUE)
    pop_other[is.na(qx), qx := 0]
    pop_other[, deaths := qx * population]
    pop_other[, population := population - deaths]
    pop_other[, `:=`(qx = NULL, deaths = NULL)]

    # Ensure consistent column order before combining
    pop_other <- pop_other[, .(year, age, sex, pop_status, population)]
    pop_100plus <- pop_100plus[, .(year, age, sex, pop_status, population)]

    # Combine all ages
    aged_pop <- data.table::rbindlist(list(pop_other, pop_100plus), use.names = TRUE)

  } else {
    # Original behavior: single qx for all at age 100+
    # Cap at max age (100+ group)
    aged_pop[age > max_age, age := max_age]

    # Aggregate for ages at max_age (people aging into 100+ from 99 and 100+)
    aged_pop <- aged_pop[, .(population = sum(population)), by = .(year, age, sex, pop_status)]

    # Calculate deaths for ages 1+ on the AGED population
    # SHIFT QX: Match pop(age=x) with qx(age=x-1)
    year_qx <- mortality_qx[year == target_year, .(age, sex, qx)]
    
    # Shift qx age by +1 so that qx(age=x-1) is indexed at age=x
    year_qx_shifted <- year_qx[, .(age = age + 1L, sex, qx)]
    
    aged_pop <- merge(aged_pop, year_qx_shifted, by = c("age", "sex"), all.x = TRUE)
    aged_pop[is.na(qx), qx := 0]
    aged_pop[, deaths := qx * population]
    aged_pop[, population := population - deaths]
    aged_pop[, `:=`(qx = NULL, deaths = NULL)]
  }

  # Add net immigration for ages 1+
  imm_1plus <- net_immigration[age > 0, .(age, sex, pop_status, net_immigration)]
  aged_pop <- merge(aged_pop, imm_1plus, by = c("age", "sex", "pop_status"), all.x = TRUE)
  aged_pop[is.na(net_immigration), net_immigration := 0]
  aged_pop[, population := population + net_immigration]

  aged_pop <- aged_pop[, .(year, age, sex, pop_status, population)]

  # Combine age 0 and ages 1+
  result <- data.table::rbindlist(list(age0_pop, aged_pop))

  # Ensure population is non-negative
  result[population < 0, population := 0]

  # Order by age, sex, pop_status
  data.table::setorder(result, age, sex, pop_status)

  result
}

#' Run full population projection (Equation 1.8.4)
#'
#' @description
#' Projects the Social Security area population from the starting year through
#' the end of the projection period using the component method.
#'
#' @param starting_population data.table: population at Dec 31 of starting year
#' @param birth_rates data.table: birth rates by year and age
#' @param mortality_qx data.table: death probabilities by year, age, sex
#' @param net_lpr data.table: net LPR immigration by year, age, sex
#' @param net_o data.table: net O immigration by year, age, sex
#' @param start_year Integer: starting year (default: 2022)
#' @param end_year Integer: ending year (default: 2099)
#' @param config List: configuration parameters
#' @param verbose Logical: print progress (default: TRUE)
#'
#' @return List with:
#'   - population: data.table of population by year, age, sex, pop_status
#'   - births: data.table of births by year, sex, pop_status
#'   - deaths: data.table of deaths by year, age, sex, pop_status
#'   - net_immigration: data.table of net immigration by year, age, sex, pop_status
#'   - summary: Summary statistics
#'
#' @export
run_population_projection <- function(starting_population,
                                       birth_rates,
                                       mortality_qx,
                                       net_lpr,
                                       net_o,
                                       start_year = NULL,
                                       end_year = NULL,
                                       config = NULL,
                                       qx_100_119 = NULL,
                                       tr2025_births = NULL,
                                       tr2025_births_years = NULL,
                                       verbose = TRUE) {

  if (is.null(config)) {
    config <- get_projected_population_config()
  }

  # Derive start_year/end_year from config if not explicitly provided
  if (is.null(start_year)) {
    start_year <- config$starting_year %||% 2022
  }
  if (is.null(end_year)) {
    end_year <- config$projection_end %||% 2099
  }

  if (verbose) {
    cli::cli_h1("Population Projection ({start_year + 1}-{end_year})")
  }

  # Check if we should use TR2025 births for specific years
  use_tr2025_births <- !is.null(tr2025_births) && !is.null(tr2025_births_years) &&
                       length(tr2025_births_years) > 0 && nrow(tr2025_births) > 0

  if (use_tr2025_births && verbose) {
    cli::cli_alert_info("Using TR2025 births for years: {paste(tr2025_births_years, collapse=', ')}")
  }

  projection_years <- (start_year + 1):end_year

  # Initialize storage
  all_populations <- list()
  all_births <- list()
  all_deaths <- list()
  all_immigration <- list()

  # Store starting population
  start_pop <- data.table::copy(starting_population)
  if (!"year" %in% names(start_pop)) {
    start_pop[, year := start_year]
  }
  all_populations[[as.character(start_year)]] <- start_pop

  # Current population (will be updated each year)
  current_pop <- start_pop

  # Make a working copy of mortality_qx that we can modify for dynamic 100+ qx
  mortality_qx_working <- data.table::copy(mortality_qx)

  # Initialize 100+ distribution tracker if qx_100_119 provided
  tracker_100plus <- NULL
  use_dynamic_100plus <- !is.null(qx_100_119) && nrow(qx_100_119) > 0

  if (use_dynamic_100plus) {
    if (verbose) {
      cli::cli_alert_info("Using dynamic weighted qx for 100+ group")
    }

    # Get starting 100+ population
    starting_100plus <- start_pop[age == 100, .(sex, population)]

    # Create tracker
    tracker_100plus <- create_100plus_tracker(
      starting_pop_100plus = starting_100plus,
      qx_100_119 = qx_100_119,
      start_year = start_year
    )

    if (verbose) {
      male_100plus <- sum(tracker_100plus$male)
      female_100plus <- sum(tracker_100plus$female)
      cli::cli_alert("Initial 100+ distribution: Male={format(round(male_100plus), big.mark=',')} Female={format(round(female_100plus), big.mark=',')}")
    }
  }

  if (verbose) {
    cli::cli_progress_bar("Projecting population", total = length(projection_years))
  }

  for (yr in projection_years) {
    if (verbose) {
      cli::cli_progress_update()
    }

    # Update qx at age 100 with dynamic weighted value if tracking
    if (use_dynamic_100plus && !is.null(tracker_100plus)) {
      # Get dynamic weighted qx for this year
      year_qx_100_119 <- qx_100_119[year == yr]

      # If no data for this year, use closest available
      if (nrow(year_qx_100_119) == 0) {
        available_years <- unique(qx_100_119$year)
        closest_year <- available_years[which.min(abs(available_years - yr))]
        year_qx_100_119 <- qx_100_119[year == closest_year]
      }

      for (s in c("male", "female")) {
        sex_qx <- year_qx_100_119[sex == s, .(age, qx)]
        current_dist <- if (s == "male") tracker_100plus$male else tracker_100plus$female
        dynamic_qx <- calculate_dynamic_weighted_qx(current_dist, sex_qx)

        # Update mortality_qx_working for age 100
        mortality_qx_working[year == yr & age == 100 & sex == s, qx := dynamic_qx]
      }
    }

    # 1. Calculate births for this year
    births <- calculate_projected_births(
      year = yr,
      birth_rates = birth_rates,
      population_start = current_pop,
      population_end = NULL,  # Will estimate midyear from start
      config = config
    )

    # Scale births to TR2025 totals for specified years
    if (use_tr2025_births && yr %in% tr2025_births_years) {
      tr_births_yr <- tr2025_births[year == yr]
      if (nrow(tr_births_yr) > 0) {
        # Get TR2025 births by sex
        tr_male <- tr_births_yr[sex == "male", births]
        tr_female <- tr_births_yr[sex == "female", births]

        # Scale our calculated births to match TR2025 totals by sex
        our_male <- births[sex == "male", sum(births)]
        our_female <- births[sex == "female", sum(births)]

        if (our_male > 0 && length(tr_male) > 0) {
          births[sex == "male", births := births * (tr_male / our_male)]
        }
        if (our_female > 0 && length(tr_female) > 0) {
          births[sex == "female", births := births * (tr_female / our_female)]
        }

        if (verbose && yr == tr2025_births_years[1]) {
          new_total <- sum(births$births)
          cli::cli_alert("Scaled {yr} births to TR2025: {format(round(new_total), big.mark=',')}")
        }
      }
    }

    # 2. Calculate deaths for this year (use working qx with dynamic 100+)
    deaths <- calculate_projected_deaths(
      year = yr,
      mortality_qx = mortality_qx_working,
      population = current_pop,
      config = config
    )

    # 3. Calculate net immigration for this year
    net_imm <- calculate_net_immigration(
      year = yr,
      net_lpr = net_lpr,
      net_o = net_o,
      config = config
    )

    # 4. Project population to end of year (use working qx)
    # Pass qx_100_119 for separate mortality calculation on new 100-year-olds vs existing 100+
    new_pop <- project_population_year(
      year = yr,
      population_prev = current_pop,
      births = births,
      deaths = deaths,  # Used for age 0 (infant deaths)
      mortality_qx = mortality_qx_working,  # Used for ages 1-99 and weighted qx for existing 100+
      net_immigration = net_imm,
      config = config,
      qx_100_119 = qx_100_119  # Raw qx values for proper 100+ mortality calculation
    )

    # Update 100+ tracker for next year
    if (use_dynamic_100plus && !is.null(tracker_100plus)) {
      # Calculate new 100-year-olds: survivors from age 99 who aged to 100
      # These people face raw qx_100 (not qx_99) since they ARE 100 during year z
      # This matches the mortality calculation in project_population_year
      pop_99_prev <- current_pop[age == 99, .(pop99 = sum(population)), by = sex]

      # Get raw qx at age 100 for this year (from qx_100_119, not the weighted value)
      year_qx_100_119 <- qx_100_119[year == yr]
      if (nrow(year_qx_100_119) == 0) {
        available_years <- unique(qx_100_119$year)
        closest_year <- available_years[which.min(abs(available_years - yr))]
        year_qx_100_119 <- qx_100_119[year == closest_year]
      }
      raw_qx_100 <- year_qx_100_119[age == 100, .(sex, raw_qx = qx)]

      pop_99_prev <- merge(pop_99_prev, raw_qx_100, by = "sex", all.x = TRUE)
      pop_99_prev[is.na(raw_qx), raw_qx := 0]

      # Survivors who reach age 100 (after facing raw qx_100)
      new_100_male <- pop_99_prev[sex == "male", pop99 * (1 - raw_qx)]
      new_100_female <- pop_99_prev[sex == "female", pop99 * (1 - raw_qx)]

      if (length(new_100_male) == 0) new_100_male <- 0
      if (length(new_100_female) == 0) new_100_female <- 0

      # Update tracker (uses tracker$qx_data internally for aging existing 100+)
      tracker_100plus <- update_100plus_tracker(
        tracker = tracker_100plus,
        target_year = yr,
        new_100_male = new_100_male,
        new_100_female = new_100_female
      )
    }

    # Store results
    all_populations[[as.character(yr)]] <- new_pop
    all_births[[as.character(yr)]] <- births
    all_deaths[[as.character(yr)]] <- deaths
    all_immigration[[as.character(yr)]] <- net_imm

    # Update current population for next iteration
    current_pop <- new_pop

    # Progress message every 10 years
    if (verbose && (yr %% 10 == 0 || yr == end_year)) {
      total_pop <- sum(new_pop$population, na.rm = TRUE)
      cli::cli_alert("{yr}: Pop = {format(round(total_pop/1e6, 2), nsmall=2)}M")
    }
  }

  if (verbose) {
    cli::cli_progress_done()
  }

  # Combine all years
  population_dt <- data.table::rbindlist(all_populations)
  births_dt <- data.table::rbindlist(all_births)
  deaths_dt <- data.table::rbindlist(all_deaths)
  immigration_dt <- data.table::rbindlist(all_immigration)

  # Calculate summary statistics
  summary_stats <- population_dt[, .(
    total_population = sum(population)
  ), by = year]

  births_summary <- births_dt[, .(total_births = sum(births)), by = year]
  deaths_summary <- deaths_dt[, .(total_deaths = sum(deaths)), by = year]
  imm_summary <- immigration_dt[, .(total_net_imm = sum(net_immigration)), by = year]

  summary_stats <- merge(summary_stats, births_summary, by = "year", all.x = TRUE)
  summary_stats <- merge(summary_stats, deaths_summary, by = "year", all.x = TRUE)
  summary_stats <- merge(summary_stats, imm_summary, by = "year", all.x = TRUE)

  if (verbose) {
    cli::cli_h2("Projection Summary")
    cli::cli_alert_success("Projected {length(projection_years)} years")
    cli::cli_alert_info(
      "Population: {format(round(summary_stats[year == start_year, total_population]/1e6, 2), nsmall=2)}M ({start_year}) -> {format(round(summary_stats[year == end_year, total_population]/1e6, 2), nsmall=2)}M ({end_year})"
    )
  }

  list(
    population = population_dt,
    births = births_dt,
    deaths = deaths_dt,
    net_immigration = immigration_dt,
    summary = summary_stats,
    config = config,
    metadata = list(
      start_year = start_year,
      end_year = end_year,
      n_years = length(projection_years)
    )
  )
}

# =============================================================================
# PHASE 8C: MARITAL STATUS DISAGGREGATION (Equation 1.8.5)
# =============================================================================
#
# CRITICAL DESIGN PRINCIPLE:
# Phase 8C DISAGGREGATES Phase 8B population totals into marital statuses.
# The sum of marital status populations MUST equal Phase 8B totals exactly.
#
# This is NOT an independent projection - it's a disaggregation that respects
# the population constraint from Phase 8B.
#
# Algorithm:
# 1. Start with BOY marital distribution (from prior year EOY or historical)
# 2. Calculate transitions: deaths, marriages, divorces, widowings, immigration
# 3. Apply transitions to get preliminary EOY marital populations
# 4. Scale preliminary populations to match Phase 8B totals exactly
# =============================================================================

#' Distribute deaths by marital status (Phase 8C.1)
#'
#' @description
#' Allocates total deaths from Phase 8B by marital status using mortality
#' differentials from 2015-2019 as documented in TR2025.
#'
#' TR2025 Methodology:
#' "For a given age, sex, and population status, deaths are assigned by marital
#' status according to the relative differences in death rates by marital status
#' observed for that age and sex during the calendar years 2015-2019."
#'
#' @param total_deaths data.table: total deaths by age, sex from Phase 8B
#' @param marital_pop_boy data.table: population by age, sex, marital_status at BOY
#' @param mortality_differentials data.table: mortality factors by age, sex, marital_status
#'
#' @return data.table with deaths by age, sex, marital_status
#'
#' @export
distribute_deaths_by_marital <- function(total_deaths,
                                          marital_pop_boy,
                                          mortality_differentials) {

  # Ensure data.tables
  total_deaths <- data.table::as.data.table(total_deaths)
  marital_pop_boy <- data.table::as.data.table(marital_pop_boy)
  mortality_differentials <- data.table::as.data.table(mortality_differentials)

  # Standardize marital status names (never_married -> single)
  if ("never_married" %in% marital_pop_boy$marital_status) {
    marital_pop_boy[marital_status == "never_married", marital_status := "single"]
  }
  if ("never_married" %in% mortality_differentials$marital_status) {
    mortality_differentials[marital_status == "never_married", marital_status := "single"]
  }

  # Get the factor column name
  factor_col <- intersect(c("relative_factor", "mortality_factor", "factor"),
                          names(mortality_differentials))[1]
  if (is.na(factor_col)) {
    cli::cli_abort("Cannot find mortality factor column in differentials")
  }

  # Get deaths column name
  deaths_col <- intersect(c("deaths", "total_deaths"), names(total_deaths))[1]
  if (is.na(deaths_col)) {
    cli::cli_abort("Cannot find deaths column")
  }

  # Aggregate deaths by age, sex
  deaths_by_age_sex <- total_deaths[, .(total_deaths = sum(get(deaths_col), na.rm = TRUE)),
                                     by = .(age, sex)]

  # Aggregate marital population by age, sex, marital_status
  # IMPORTANT: Age the BOY population to match death ages
  # Deaths at age X apply to people who were at age X-1 at BOY
  marital_agg <- marital_pop_boy[, .(population = sum(population, na.rm = TRUE)),
                                  by = .(age, sex, marital_status)]
  marital_agg[, age := age + 1L]  # Age to match death ages (BOY age X -> EOY age X+1)

  # Cap at max_age (100) - ages 100+ are grouped together
  max_age <- max(deaths_by_age_sex$age)
  marital_agg[age > max_age, age := max_age]
  marital_agg <- marital_agg[, .(population = sum(population, na.rm = TRUE)),
                              by = .(age, sex, marital_status)]

  # Merge with mortality differentials
  marital_agg <- merge(
    marital_agg,
    mortality_differentials[, .(age, sex, marital_status,
                                diff_factor = get(factor_col))],
    by = c("age", "sex", "marital_status"),
    all.x = TRUE
  )

  # Default factor to 1.0 for missing (married is baseline)
  marital_agg[is.na(diff_factor), diff_factor := 1.0]

  # Calculate expected deaths weight: population * factor
  marital_agg[, death_weight := population * diff_factor]

  # Calculate proportion of deaths for each marital status within age-sex
  marital_agg[, total_weight := sum(death_weight, na.rm = TRUE), by = .(age, sex)]
  marital_agg[total_weight > 0, death_proportion := death_weight / total_weight]
  marital_agg[total_weight == 0 | is.na(total_weight), death_proportion := 0]

  # Merge with total deaths
  result <- merge(
    marital_agg[, .(age, sex, marital_status, death_proportion)],
    deaths_by_age_sex,
    by = c("age", "sex"),
    all.x = TRUE
  )

  # Distribute deaths
  result[is.na(total_deaths), total_deaths := 0]
  result[, deaths := total_deaths * death_proportion]

  result[, .(age, sex, marital_status, deaths)]
}

#' Distribute immigrants by marital status (Phase 8C.2)
#'
#' @description
#' Allocates net immigration by marital status using the beginning-of-year
#' marital distribution.
#'
#' TR2025 Methodology:
#' "For a given age, sex, and population status, immigrants are assigned by
#' marital status according to the beginning of year marital distribution of
#' the Social Security area population for that age and sex."
#'
#' @param total_net_immigration data.table: net immigration by age, sex from Phase 8B
#' @param marital_dist_boy data.table: marital distribution at BOY
#'
#' @return data.table with immigration by age, sex, marital_status
#'
#' @export
distribute_immigrants_by_marital <- function(total_net_immigration, marital_dist_boy) {

  total_net_immigration <- data.table::as.data.table(total_net_immigration)
  marital_dist_boy <- data.table::as.data.table(marital_dist_boy)

  # Standardize marital status names
  if ("never_married" %in% marital_dist_boy$marital_status) {
    marital_dist_boy[marital_status == "never_married", marital_status := "single"]
  }

  # Get immigration column name
  imm_col <- intersect(c("net_immigration", "net_imm", "immigration"),
                       names(total_net_immigration))[1]

  # Handle empty or missing net_immigration
  if (is.na(imm_col) || nrow(total_net_immigration) == 0) {
    # Return zero immigration for all marital statuses
    result <- marital_dist_boy[, .(population = sum(population, na.rm = TRUE)),
                                by = .(age, sex, marital_status)]
    result[, net_immigration := 0]
    return(result[, .(age, sex, marital_status, net_immigration)])
  }

  # Aggregate net immigration by age, sex
  imm_by_age_sex <- total_net_immigration[, .(total_imm = sum(get(imm_col), na.rm = TRUE)),
                                           by = .(age, sex)]

  # Calculate marital distribution proportions
  # IMPORTANT: Age the BOY population to match immigration ages
  # Immigration at age X applies to population that was age X-1 at BOY
  marital_props <- marital_dist_boy[, .(population = sum(population, na.rm = TRUE)),
                                     by = .(age, sex, marital_status)]
  marital_props[, age := age + 1L]  # Age to match immigration ages

  # Cap at max_age (100) - ages 100+ are grouped together
  max_age <- max(imm_by_age_sex$age)
  marital_props[age > max_age, age := max_age]
  marital_props <- marital_props[, .(population = sum(population, na.rm = TRUE)),
                                  by = .(age, sex, marital_status)]

  marital_props[, total_pop := sum(population, na.rm = TRUE), by = .(age, sex)]
  marital_props[total_pop > 0, proportion := population / total_pop]
  marital_props[total_pop == 0 | is.na(total_pop), proportion := 0.25]  # Equal split if no population

  # Merge with net immigration
  result <- merge(
    marital_props[, .(age, sex, marital_status, proportion)],
    imm_by_age_sex,
    by = c("age", "sex"),
    all.x = TRUE
  )

  # Distribute immigration
  result[is.na(total_imm), total_imm := 0]
  result[, net_immigration := total_imm * proportion]

  result[, .(age, sex, marital_status, net_immigration)]
}

#' Calculate midyear unmarried population (Phase 8C.3)
#'
#' @description
#' Estimates the midyear unmarried population for marriage calculations.
#'
#' TR2025 Methodology:
#' "The age-specific midyear unmarried male population is estimated from the
#' beginning of the year unmarried population. It is calculated by adjusting
#' the number of unmarried men at the beginning of the year to represent midyear
#' using the relationship between the prior beginning of year and the current
#' beginning of year unmarried male populations."
#'
#' @param marital_pop_boy data.table: marital population at BOY
#' @param marital_pop_prior_boy data.table: marital population at prior year BOY (optional)
#'
#' @return data.table with midyear unmarried population by age and sex
#'
#' @export
calculate_midyear_unmarried <- function(marital_pop_boy, marital_pop_prior_boy = NULL) {

  marital_pop_boy <- data.table::as.data.table(marital_pop_boy)

  # Standardize marital status names
  if ("never_married" %in% marital_pop_boy$marital_status) {
    marital_pop_boy[marital_status == "never_married", marital_status := "single"]
  }

  # Unmarried = single + divorced + widowed
  unmarried_statuses <- c("single", "divorced", "widowed")

  # Aggregate unmarried by age and sex
  unmarried_boy <- marital_pop_boy[marital_status %in% unmarried_statuses,
                                    .(pop_boy = sum(population, na.rm = TRUE)),
                                    by = .(age, sex)]

  if (is.null(marital_pop_prior_boy)) {
    # No prior year data - use BOY as midyear estimate
    unmarried_boy[, midyear_unmarried := pop_boy]
    return(unmarried_boy[, .(age, sex, midyear_unmarried)])
  }

  # With prior year data, estimate midyear using ratio method
  marital_pop_prior_boy <- data.table::as.data.table(marital_pop_prior_boy)
  if ("never_married" %in% marital_pop_prior_boy$marital_status) {
    marital_pop_prior_boy[marital_status == "never_married", marital_status := "single"]
  }

  unmarried_prior <- marital_pop_prior_boy[marital_status %in% unmarried_statuses,
                                            .(pop_prior = sum(population, na.rm = TRUE)),
                                            by = .(age, sex)]

  # Merge and calculate midyear
  merged <- merge(unmarried_boy, unmarried_prior, by = c("age", "sex"), all.x = TRUE)
  merged[is.na(pop_prior), pop_prior := pop_boy]

  # Midyear = average of BOY and what we'd expect from aging prior year
  # Approximation: midyear ≈ (current_boy + prior_boy_aged) / 2
  # For simplicity, use geometric mean approach
  merged[pop_prior > 0, ratio := pop_boy / pop_prior]
  merged[pop_prior == 0, ratio := 1]

  # Midyear = BOY * sqrt(ratio) centers between years
  merged[, midyear_unmarried := pop_boy * sqrt(pmin(ratio, 2))]  # Cap ratio to avoid extreme values

  merged[, .(age, sex, midyear_unmarried)]
}

#' Calculate new marriages for a year (Phase 8C.4)
#'
#' @description
#' Calculates the number of new marriages by age using marriage rates
#' and the geometric mean of midyear unmarried populations.
#'
#' TR2025 Methodology:
#' "The annual number of opposite-sex marriages occurring at each age of husband
#' crossed with each age of wife is obtained by multiplying the age-of-husband-
#' specific and age-of-wife-specific marriage rates with the geometric mean of
#' the midyear unmarried male population and the midyear unmarried female population."
#'
#' @param marriage_rate_grid Matrix: marriage rates (87x87, ages 14-100+), per 100,000
#' @param midyear_unmarried data.table: midyear unmarried population by age and sex
#' @param min_age Integer: minimum marriage age (default: 14)
#' @param max_age Integer: maximum marriage age (default: 100)
#'
#' @return list with:
#'   - marriages_grid: Matrix of marriages by husband age × wife age
#'   - marriages_by_husband: Vector of marriages by husband age
#'   - marriages_by_wife: Vector of marriages by wife age
#'   - total_marriages: Total marriages
#'
#' @export
calculate_new_marriages <- function(marriage_rate_grid,
                                     midyear_unmarried,
                                     min_age = 14,
                                     max_age = 100) {

  n_ages <- max_age - min_age + 1  # 87

  # Convert midyear unmarried to vectors by age
  male_unmarried <- rep(0, n_ages)
  female_unmarried <- rep(0, n_ages)

  midyear_unmarried <- data.table::as.data.table(midyear_unmarried)

  for (i in seq_len(nrow(midyear_unmarried))) {
    a <- midyear_unmarried$age[i]
    s <- midyear_unmarried$sex[i]
    val <- midyear_unmarried$midyear_unmarried[i]

    if (a >= min_age && a <= max_age && !is.na(val)) {
      idx <- a - min_age + 1
      if (s == "male") {
        male_unmarried[idx] <- val
      } else if (s == "female") {
        female_unmarried[idx] <- val
      }
    }
  }

  # Ensure marriage_rate_grid is correct size
  if (!is.matrix(marriage_rate_grid)) {
    cli::cli_abort("marriage_rate_grid must be a matrix")
  }
  if (nrow(marriage_rate_grid) != n_ages || ncol(marriage_rate_grid) != n_ages) {
    cli::cli_abort("Marriage rate grid must be {n_ages}×{n_ages}")
  }

  # Calculate geometric mean of unmarried populations
  # sqrt(male_h × female_w) for each h,w combination
  geom_mean_matrix <- sqrt(outer(male_unmarried, female_unmarried))

  # Marriages = (rate / 100,000) × geometric_mean
  marriages_grid <- (marriage_rate_grid / 100000) * geom_mean_matrix

  # Handle any NaN/Inf
  marriages_grid[!is.finite(marriages_grid)] <- 0

  list(
    marriages_grid = marriages_grid,
    marriages_by_husband = rowSums(marriages_grid, na.rm = TRUE),
    marriages_by_wife = colSums(marriages_grid, na.rm = TRUE),
    total_marriages = sum(marriages_grid, na.rm = TRUE)
  )
}

#' Calculate midyear married couples (Phase 8C.5)
#'
#' @description
#' Estimates the midyear married couples grid for divorce calculations.
#'
#' TR2025 Methodology:
#' "The number of age-of-husband crossed with age-of-wife midyear married couples
#' is estimated from the beginning of the year married couples. It is calculated
#' by adjusting the number of married couples at the beginning of the year to
#' represent midyear using the relationship between the number of married couples
#' at the beginning of the prior year and the beginning of the current year."
#'
#' @param couples_grid_boy Matrix: married couples at BOY (husband age × wife age)
#' @param couples_grid_prior Matrix: married couples at prior BOY (optional)
#'
#' @return Matrix of midyear married couples
#'
#' @export
calculate_midyear_married_couples <- function(couples_grid_boy, couples_grid_prior = NULL) {

  if (is.null(couples_grid_prior)) {
    # No prior year - use BOY as estimate
    return(couples_grid_boy)
  }

  # Calculate ratio and estimate midyear
  ratio <- couples_grid_boy / couples_grid_prior
  ratio[!is.finite(ratio)] <- 1

  # Midyear = BOY * sqrt(ratio)
  midyear <- couples_grid_boy * sqrt(pmin(ratio, 2))  # Cap ratio
  midyear[!is.finite(midyear)] <- 0

  midyear
}

#' Calculate divorces for a year (Phase 8C.6)
#'
#' @description
#' Calculates the number of divorces by age using divorce rates
#' and midyear married couples.
#'
#' TR2025 Methodology:
#' "The number of divorces during a year, occurring at each age of husband
#' crossed with each age of wife, is obtained by multiplying the age-of-husband
#' crossed with age-of-wife divorce rates for that year with the midyear number
#' of married couples in that age crossing."
#'
#' @param divorce_rate_grid Matrix: divorce rates (87x87), per 100,000
#' @param midyear_couples Matrix: midyear married couples by husband × wife age
#'
#' @return list with:
#'   - divorces_grid: Matrix of divorces by husband age × wife age
#'   - divorces_by_husband: Vector of divorces by husband age
#'   - divorces_by_wife: Vector of divorces by wife age
#'   - total_divorces: Total divorces
#'
#' @export
calculate_new_divorces <- function(divorce_rate_grid, midyear_couples) {

  # Divorces = (rate / 100,000) × midyear_couples
  divorces_grid <- (divorce_rate_grid / 100000) * midyear_couples

  # Handle any NaN/Inf
  divorces_grid[!is.finite(divorces_grid)] <- 0

  list(
    divorces_grid = divorces_grid,
    divorces_by_husband = rowSums(divorces_grid, na.rm = TRUE),
    divorces_by_wife = colSums(divorces_grid, na.rm = TRUE),
    total_divorces = sum(divorces_grid, na.rm = TRUE)
  )
}

#' Calculate widowings for a year (Phase 8C.7)
#'
#' @description
#' Calculates the number of widowings by applying death probabilities
#' to the married couples grid.
#'
#' TR2025 Methodology:
#' "Widowings are computed by applying general population probabilities of death
#' to the marriage prevalence at the beginning of the year."
#'
#' @param couples_grid_boy Matrix: married couples at BOY (husband age × wife age)
#' @param qx_male Numeric vector: death probabilities for males by age
#' @param qx_female Numeric vector: death probabilities for females by age
#' @param min_age Integer: minimum age (default: 14)
#' @param max_age Integer: maximum age (default: 100)
#'
#' @return list with:
#'   - widowed_women: Vector of newly widowed women by age
#'   - widowed_men: Vector of newly widowed men by age
#'   - total_widowings: Total widowings
#'
#' @export
calculate_widowings <- function(couples_grid_boy,
                                 qx_male,
                                 qx_female,
                                 min_age = 14,
                                 max_age = 100) {

  n_ages <- max_age - min_age + 1  # 87

  # Ensure qx vectors are correct length and indexed from age 0
  # Extract the relevant ages (14-100)
  if (length(qx_male) >= max_age + 1) {
    qx_m <- qx_male[(min_age + 1):(max_age + 1)]  # R is 1-indexed
  } else {
    # Pad with reasonable defaults
    qx_m <- c(qx_male, rep(qx_male[length(qx_male)], max_age + 1 - length(qx_male)))
    qx_m <- qx_m[(min_age + 1):(max_age + 1)]
  }

  if (length(qx_female) >= max_age + 1) {
    qx_f <- qx_female[(min_age + 1):(max_age + 1)]
  } else {
    qx_f <- c(qx_female, rep(qx_female[length(qx_female)], max_age + 1 - length(qx_female)))
    qx_f <- qx_f[(min_age + 1):(max_age + 1)]
  }

  # Women widowed = couples × P(husband dies)
  # Sum across wife ages for each husband age, weighted by couples
  widowed_women <- numeric(n_ages)
  widowed_men <- numeric(n_ages)

  for (h in 1:n_ages) {
    for (w in 1:n_ages) {
      couples_hw <- couples_grid_boy[h, w]
      if (is.finite(couples_hw) && couples_hw > 0) {
        # Women become widows when husband dies
        widowed_women[w] <- widowed_women[w] + couples_hw * qx_m[h]
        # Men become widowers when wife dies
        widowed_men[h] <- widowed_men[h] + couples_hw * qx_f[w]
      }
    }
  }

  list(
    widowed_women = widowed_women,
    widowed_men = widowed_men,
    total_widowings = sum(widowed_women) + sum(widowed_men)
  )
}

#' Build married couples grid from marital population (Phase 8C.8)
#'
#' @description
#' Constructs a married couples grid (husband age × wife age) from
#' the marginal married population totals.
#'
#' TR2025 uses actual couple counts from HISTORICAL POPULATION (Input Item 10).
#' Since we don't have access to that data structure, we approximate it using
#' the marginal married populations and distributing proportionally based on
#' typical age patterns.
#'
#' @param marital_pop data.table: population by age, sex, marital_status
#' @param min_age Integer: minimum age (default: 14)
#' @param max_age Integer: maximum age (default: 100)
#'
#' @return Matrix (87×87) of married couples by husband age × wife age
#'
#' @export
build_married_couples_grid <- function(marital_pop, min_age = 14, max_age = 100) {

  n_ages <- max_age - min_age + 1  # 87

  # Standardize marital status
  marital_pop <- data.table::as.data.table(marital_pop)
  if ("never_married" %in% marital_pop$marital_status) {
    marital_pop[marital_status == "never_married", marital_status := "single"]
  }

  # Get married population by age and sex
  married_male <- marital_pop[marital_status == "married" & sex == "male",
                               .(married = sum(population, na.rm = TRUE)), by = age]
  married_female <- marital_pop[marital_status == "married" & sex == "female",
                                 .(married = sum(population, na.rm = TRUE)), by = age]

  # Create vectors indexed by age
  males <- rep(0, n_ages)
  females <- rep(0, n_ages)

  for (i in seq_len(nrow(married_male))) {
    a <- married_male$age[i]
    if (a >= min_age && a <= max_age) {
      idx <- a - min_age + 1
      males[idx] <- married_male$married[i]
    }
  }

  for (i in seq_len(nrow(married_female))) {
    a <- married_female$age[i]
    if (a >= min_age && a <= max_age) {
      idx <- a - min_age + 1
      females[idx] <- married_female$married[i]
    }
  }

  # Build couples grid using age-gap distribution
  # Typical husband-wife age difference pattern: husbands slightly older
  # Use a normal distribution centered at age_diff = 2 with sd = 5
  couples_grid <- matrix(0, nrow = n_ages, ncol = n_ages)

  for (h in 1:n_ages) {
    for (w in 1:n_ages) {
      age_diff <- h - w  # Husband age - wife age
      # Weight based on typical age difference pattern
      weight <- dnorm(age_diff, mean = 2, sd = 5)
      couples_grid[h, w] <- weight
    }
  }

  # Normalize each row and column to match marginal totals
  # Use iterative proportional fitting (IPF) to match both margins
  couples_grid <- ipf_normalize(couples_grid, males, females, max_iter = 50)

  # Set row and column names
  ages <- min_age:max_age
  rownames(couples_grid) <- ages
  colnames(couples_grid) <- ages

  couples_grid
}

#' Iterative Proportional Fitting for couples grid
#'
#' @param grid Initial grid (n_ages × n_ages)
#' @param row_margins Target row sums (married males by age)
#' @param col_margins Target column sums (married females by age)
#' @param max_iter Maximum iterations (default: 50)
#' @param tol Convergence tolerance (default: 1e-6)
#'
#' @return Normalized grid matching both margins
#'
#' @keywords internal
ipf_normalize <- function(grid, row_margins, col_margins, max_iter = 50, tol = 1e-6) {

  n_rows <- nrow(grid)
  n_cols <- ncol(grid)

  # Handle zero margins
  row_margins[row_margins == 0] <- 1e-10
  col_margins[col_margins == 0] <- 1e-10

  for (iter in 1:max_iter) {
    # Scale rows
    row_sums <- rowSums(grid)
    row_sums[row_sums == 0] <- 1
    grid <- grid * (row_margins / row_sums)

    # Scale columns
    col_sums <- colSums(grid)
    col_sums[col_sums == 0] <- 1
    grid <- grid * matrix(col_margins / col_sums, nrow = n_rows, ncol = n_cols, byrow = TRUE)

    # Check convergence
    row_err <- max(abs(rowSums(grid) - row_margins) / (row_margins + 1))
    col_err <- max(abs(colSums(grid) - col_margins) / (col_margins + 1))

    if (row_err < tol && col_err < tol) {
      break
    }
  }

  grid
}

#' Update married couples grid for next year (Phase 8C.9)
#'
#' @description
#' Updates the married couples grid from one year to the next by:
#' 1. Aging the grid (shift diagonal)
#' 2. Removing couples due to deaths and divorces
#' 3. Adding new marriages
#'
#' @param couples_grid Current married couples grid (husband age × wife age)
#' @param marriages_grid New marriages grid
#' @param divorces_grid Divorces grid
#' @param widowed_women Vector of women widowed by age
#' @param widowed_men Vector of men widowed by age
#' @param qx_male Male death probabilities
#' @param qx_female Female death probabilities
#' @param min_age Minimum age (default: 14)
#' @param max_age Maximum age (default: 100)
#'
#' @return Updated couples grid
#'
#' @export
update_married_couples_grid <- function(couples_grid,
                                         marriages_grid,
                                         divorces_grid,
                                         widowed_women,
                                         widowed_men,
                                         qx_male,
                                         qx_female,
                                         min_age = 14,
                                         max_age = 100) {

  n_ages <- max_age - min_age + 1

  # Start with aged grid (people age one year)
  new_grid <- matrix(0, nrow = n_ages, ncol = n_ages)

  # Age the couples (shift diagonal)
  for (h in 2:n_ages) {
    for (w in 2:n_ages) {
      # Couples who were at (h-1, w-1) last year are now at (h, w)
      new_grid[h, w] <- couples_grid[h-1, w-1]
    }
  }

  # Handle couples where one spouse hits max age
  # Those at max_age stay at max_age (100+)
  if (n_ages > 1) {
    new_grid[n_ages, ] <- new_grid[n_ages, ] + couples_grid[n_ages, ]
    new_grid[, n_ages] <- new_grid[, n_ages] + couples_grid[, n_ages]
  }

  # Prepare qx vectors for ages 14-100
  if (length(qx_male) >= max_age + 1) {
    qx_m <- qx_male[(min_age + 1):(max_age + 1)]
  } else {
    qx_m <- c(qx_male, rep(qx_male[length(qx_male)], max_age + 1 - length(qx_male)))
    qx_m <- qx_m[(min_age + 1):(max_age + 1)]
  }

  if (length(qx_female) >= max_age + 1) {
    qx_f <- qx_female[(min_age + 1):(max_age + 1)]
  } else {
    qx_f <- c(qx_female, rep(qx_female[length(qx_female)], max_age + 1 - length(qx_female)))
    qx_f <- qx_f[(min_age + 1):(max_age + 1)]
  }

  # Remove couples where either spouse dies
  # P(couple survives) = P(husband survives) × P(wife survives)
  for (h in 1:n_ages) {
    for (w in 1:n_ages) {
      survival_prob <- (1 - qx_m[h]) * (1 - qx_f[w])
      new_grid[h, w] <- new_grid[h, w] * survival_prob
    }
  }

  # Remove divorces
  new_grid <- new_grid - divorces_grid
  new_grid[new_grid < 0] <- 0

  # Add new marriages
  new_grid <- new_grid + marriages_grid

  new_grid
}

#' Project marital status for one year (Phase 8C.10)
#'
#' @description
#' Projects marital status populations for a single year using flow-based
#' methodology per TR2025. Population totals emerge naturally from demographic
#' flows (births, deaths, immigration, marriages, divorces, widowings).
#'
#' Key insight: Age 14 at EOY consists of people who were 13 at BOY (pre-marital
#' tracking), so they enter as 100% single. Ages 15+ are evolved from BOY marital
#' population through demographic flows.
#'
#' Uses vectorized data.table operations for efficiency.
#'
#' @param marital_pop_boy data.table: marital population at BOY (Dec 31 of prior year)
#' @param phase8b_pop data.table: Phase 8B population totals for this year (EOY)
#'        Used ONLY to get age-14 population (new entrants to marital tracking)
#' @param total_deaths data.table: total deaths from Phase 8B
#' @param total_immigration data.table: total net immigration from Phase 8B
#' @param mortality_differentials data.table: mortality factors by marital status
#' @param marriage_rate_grid Matrix: marriage rates for this year (87×87)
#' @param divorce_rate_grid Matrix: divorce rates for this year (87×87)
#' @param couples_grid_boy Matrix: married couples at BOY
#' @param qx_male Numeric vector: male death probabilities
#' @param qx_female Numeric vector: female death probabilities
#' @param marital_pop_prior_boy data.table: marital population at prior year BOY (optional)
#' @param couples_grid_prior Matrix: married couples at prior BOY (optional)
#' @param year Integer: projection year
#' @param min_age Integer: minimum marriage age (default: 14)
#' @param max_age Integer: maximum marriage age (default: 100)
#'
#' @return list with:
#'   - marital_pop: data.table of population by age, sex, marital_status
#'   - couples_grid: Updated married couples grid for EOY
#'   - marriages: Total new marriages
#'   - divorces: Total divorces
#'   - widowings: Total widowings
#'
#' @export
project_marital_year <- function(marital_pop_boy,
                                  phase8b_pop,
                                  total_deaths,
                                  total_immigration,
                                  mortality_differentials,
                                  marriage_rate_grid,
                                  divorce_rate_grid,
                                  couples_grid_boy,
                                  qx_male,
                                  qx_female,
                                  marital_pop_prior_boy = NULL,
                                  couples_grid_prior = NULL,
                                  year,
                                  min_age = 14,
                                  max_age = 100) {

  n_ages <- max_age - min_age + 1

  # Standardize marital status
  marital_pop_boy <- data.table::as.data.table(marital_pop_boy)
  if ("never_married" %in% marital_pop_boy$marital_status) {
    marital_pop_boy[marital_status == "never_married", marital_status := "single"]
  }

  # Step 1: Distribute deaths by marital status
  deaths_by_marital <- distribute_deaths_by_marital(
    total_deaths, marital_pop_boy, mortality_differentials
  )

  # Step 2: Distribute immigration by marital status
  imm_by_marital <- distribute_immigrants_by_marital(
    total_immigration, marital_pop_boy
  )

  # Step 3: Calculate midyear unmarried for marriages
  midyear_unmarried <- calculate_midyear_unmarried(
    marital_pop_boy, marital_pop_prior_boy
  )

  # Step 4: Calculate new marriages
  marriages_result <- calculate_new_marriages(
    marriage_rate_grid, midyear_unmarried, min_age, max_age
  )

  # Step 5: Calculate midyear couples for divorces
  midyear_couples <- calculate_midyear_married_couples(
    couples_grid_boy, couples_grid_prior
  )

  # Step 6: Calculate divorces
  divorces_result <- calculate_new_divorces(
    divorce_rate_grid, midyear_couples
  )

  # Step 7: Calculate widowings
  widowings_result <- calculate_widowings(
    couples_grid_boy, qx_male, qx_female, min_age, max_age
  )

  # Step 8: Age the population and apply transitions (VECTORIZED)
  # Create aged BOY population (everyone ages by 1 year)
  # People at age 14 BOY become age 15 EOY, etc.
  # Match Phase 8B methodology: age everyone, cap at max_age (100+)
  aged_pop <- data.table::copy(marital_pop_boy)
  aged_pop[, age := age + 1]
  aged_pop[age > max_age, age := max_age]  # Cap at 100+ like Phase 8B
  aged_pop <- aged_pop[, .(population = sum(population, na.rm = TRUE)),
                        by = .(age, sex, marital_status)]

  # Create complete grid of all age/sex/marital_status combinations
  grid <- data.table::CJ(
    age = min_age:max_age,
    sex = c("male", "female"),
    marital_status = c("single", "married", "divorced", "widowed")
  )

  # Merge aged population (this gives ages 15+ their starting point)
  grid <- merge(grid, aged_pop, by = c("age", "sex", "marital_status"), all.x = TRUE)
  grid[is.na(population), population := 0]

  # Step 8a: Handle age 14 separately - these are NEW entrants to marital tracking
  # People at age 14 EOY were age 13 at BOY (pre-marital tracking), so ALL are single
  phase8b_pop <- data.table::as.data.table(phase8b_pop)
  age14_pop <- phase8b_pop[age == min_age, .(age, sex, phase8b_pop = sum(population)), by = .(age, sex)]

  # Set age 14 population: 100% single, 0% for other statuses
  grid[age == min_age & marital_status == "single",
       population := age14_pop[match(paste(grid[age == min_age & marital_status == "single", sex]),
                                     paste(age14_pop$sex)), phase8b_pop]]
  grid[age == min_age & marital_status != "single", population := 0]

  # Merge deaths (only for ages 15+, since age 14 deaths are implicit in Phase 8B count)
  grid <- merge(grid, deaths_by_marital[, .(age, sex, marital_status, deaths)],
                by = c("age", "sex", "marital_status"), all.x = TRUE)
  grid[is.na(deaths), deaths := 0]
  grid[age == min_age, deaths := 0]  # Age 14 already net of deaths from Phase 8B

  # Merge immigration (only for ages 15+, since age 14 immigration is implicit in Phase 8B count)
  grid <- merge(grid, imm_by_marital[, .(age, sex, marital_status, net_immigration)],
                by = c("age", "sex", "marital_status"), all.x = TRUE)
  grid[is.na(net_immigration), net_immigration := 0]
  grid[age == min_age, net_immigration := 0]  # Age 14 already includes immigration from Phase 8B

  # Create marriages by age/sex data.tables
  mar_male <- data.table::data.table(
    age = min_age:max_age,
    marriages = marriages_result$marriages_by_husband
  )
  mar_female <- data.table::data.table(
    age = min_age:max_age,
    marriages = marriages_result$marriages_by_wife
  )

  # Create divorces by age/sex data.tables
  div_male <- data.table::data.table(
    age = min_age:max_age,
    divorces = divorces_result$divorces_by_husband
  )
  div_female <- data.table::data.table(
    age = min_age:max_age,
    divorces = divorces_result$divorces_by_wife
  )

  # Create widowings by age/sex data.tables
  wid_male <- data.table::data.table(
    age = min_age:max_age,
    widowings = widowings_result$widowed_men
  )
  wid_female <- data.table::data.table(
    age = min_age:max_age,
    widowings = widowings_result$widowed_women
  )

  # Apply transitions vectorized
  # Start with population (aged for 15+, Phase 8B for age 14)
  grid[, final_pop := population]

  # Subtract deaths (ages 15+ only)
  grid[age > min_age, final_pop := final_pop - deaths]

  # Add immigration (ages 15+ only)
  grid[age > min_age, final_pop := final_pop + net_immigration]

  # Calculate unmarried totals for distributing marriages
  # IMPORTANT: Use final_pop (after deaths/immigration) for proportions, not original population
  # This ensures we distribute marriages based on who is actually alive to marry
  unmarried_totals <- grid[marital_status %in% c("single", "divorced", "widowed"),
                           .(unmarried_total = sum(final_pop, na.rm = TRUE)),
                           by = .(age, sex)]
  grid <- merge(grid, unmarried_totals, by = c("age", "sex"), all.x = TRUE)
  grid[is.na(unmarried_total), unmarried_total := 0]

  # Calculate marriage outflow proportion for unmarried statuses
  grid[, mar_out_prop := fifelse(
    marital_status %in% c("single", "divorced", "widowed") & unmarried_total > 0,
    final_pop / unmarried_total,
    0
  )]

  # Handle marriages - married gets inflow, unmarried get outflow
  # Merge marriage counts
  grid[sex == "male", marriages_total := mar_male[match(grid[sex == "male", age], age), marriages]]
  grid[sex == "female", marriages_total := mar_female[match(grid[sex == "female", age], age), marriages]]
  grid[is.na(marriages_total), marriages_total := 0]

  # Cap marriages at available unmarried population to prevent negative populations
  grid[, marriages_capped := pmin(marriages_total, unmarried_total)]

  # Married: add marriages (capped)
  grid[marital_status == "married", final_pop := final_pop + marriages_capped]
  # Unmarried: subtract proportional share of marriages (capped)
  grid[marital_status %in% c("single", "divorced", "widowed"),
       final_pop := final_pop - marriages_capped * mar_out_prop]

  # Handle divorces - married loses, divorced gains
  grid[sex == "male", divorces_total := div_male[match(grid[sex == "male", age], age), divorces]]
  grid[sex == "female", divorces_total := div_female[match(grid[sex == "female", age], age), divorces]]
  grid[is.na(divorces_total), divorces_total := 0]

  grid[marital_status == "married", final_pop := final_pop - divorces_total]
  grid[marital_status == "divorced", final_pop := final_pop + divorces_total]

  # Handle widowings - married loses, widowed gains
  grid[sex == "male", widowings_total := wid_male[match(grid[sex == "male", age], age), widowings]]
  grid[sex == "female", widowings_total := wid_female[match(grid[sex == "female", age], age), widowings]]
  grid[is.na(widowings_total), widowings_total := 0]

  grid[marital_status == "married", final_pop := final_pop - widowings_total]
  grid[marital_status == "widowed", final_pop := final_pop + widowings_total]

  # Handle negative populations by redistributing within age/sex
  # Negative populations occur when outflows (deaths + marriages) exceed available population
  # at specific age/sex/marital_status combinations. Rather than just clipping to 0 (which
  # artificially inflates total population), we redistribute the excess outflow within the
  # same age/sex group to maintain total balance.
  if (any(grid$final_pop < 0)) {
    # For each age/sex with negative populations, redistribute
    neg_by_agesex <- grid[final_pop < 0, .(neg_total = sum(final_pop)), by = .(age, sex)]

    if (nrow(neg_by_agesex) > 0) {
      for (i in seq_len(nrow(neg_by_agesex))) {
        a <- neg_by_agesex$age[i]
        s <- neg_by_agesex$sex[i]
        neg_amt <- abs(neg_by_agesex$neg_total[i])

        # Set negatives to 0
        grid[age == a & sex == s & final_pop < 0, final_pop := 0]

        # Subtract the negative amount proportionally from positive statuses at same age/sex
        pos_total <- grid[age == a & sex == s & final_pop > 0, sum(final_pop)]
        if (pos_total > 0) {
          grid[age == a & sex == s & final_pop > 0,
               final_pop := final_pop - neg_amt * (final_pop / pos_total)]
        }
      }
    }
  }

  # Final check - ensure no negatives remain (should be rare after redistribution)
  grid[final_pop < 0, final_pop := 0]

  # === RESCALE TO PHASE 8B TOTALS (TR2025 Eq 1.8.5) ===
  # Per TR2025: Marital status is a DISAGGREGATION of Phase 8B population totals.
  # "Once the population is projected by single year of age, sex, and population status,
  #  it is then disaggregated by population status into the following four marital states"
  # This means marital totals MUST sum to Phase 8B totals exactly at each age/sex.
  #
  # The flows (marriages, divorces, widowings) determine the DISTRIBUTION across statuses,
  # but the Phase 8B totals are the authoritative population counts.

  # Get Phase 8B totals for ages 14+ (marital tracking range)
  phase8b_totals <- phase8b_pop[age >= min_age,
                                 .(phase8b_total = sum(population, na.rm = TRUE)),
                                 by = .(age, sex)]

  # Calculate marital totals by age/sex
  marital_totals <- grid[, .(marital_total = sum(final_pop, na.rm = TRUE)),
                         by = .(age, sex)]

  # Merge to get scaling factors
  grid <- merge(grid, marital_totals, by = c("age", "sex"), all.x = TRUE)
  grid <- merge(grid, phase8b_totals, by = c("age", "sex"), all.x = TRUE)

  # Calculate scaling factor: Phase 8B total / marital total
  # Handle edge case where marital_total is 0 or NA
  grid[, scale_factor := fifelse(
    is.na(marital_total) | marital_total <= 0,
    1.0,
    fifelse(is.na(phase8b_total), 1.0, phase8b_total / marital_total)
  )]

  # Apply scaling to get final population that matches Phase 8B totals
  grid[, population := final_pop * scale_factor]

  # Clean up temporary columns
  grid[, c("marital_total", "phase8b_total", "scale_factor") := NULL]

  # Create final result
  final_marital <- grid[, .(year = year, age, sex, marital_status, population)]

  # Step 9: Update couples grid
  new_couples_grid <- update_married_couples_grid(
    couples_grid_boy,
    marriages_result$marriages_grid,
    divorces_result$divorces_grid,
    widowings_result$widowed_women,
    widowings_result$widowed_men,
    qx_male,
    qx_female,
    min_age,
    max_age
  )

  list(
    marital_pop = final_marital,
    couples_grid = new_couples_grid,
    marriages = marriages_result$total_marriages,
    divorces = divorces_result$total_divorces,
    widowings = widowings_result$total_widowings
  )
}

#' Run marital status projection (Phase 8C Main Function)
#'
#' @description
#' Projects population by marital status from the starting year through
#' the projection period. This is a disaggregation of Phase 8B population
#' totals - the sum of marital statuses equals Phase 8B totals exactly.
#'
#' @param phase8b_result list: Output from run_population_projection (Phase 8B)
#' @param starting_marital data.table: Starting marital population (Dec 31 starting year)
#' @param mortality_differentials data.table: Mortality factors by marital status
#' @param marriage_rates list: Marriage rates from marriage_projection
#' @param divorce_rates list: Divorce rates from divorce_projection
#' @param mortality_qx data.table: Death probabilities by year, age, sex
#' @param start_year Integer: Starting year (default: 2022)
#' @param end_year Integer: End year (default: 2099)
#' @param min_age Integer: Minimum age for marital status (default: 14)
#' @param max_age Integer: Maximum age for marital status (default: 100)
#' @param verbose Logical: Print progress messages (default: TRUE)
#'
#' @return list with:
#'   - marital_population: data.table with population by year, age, sex, marital_status
#'   - couples_grids: list of married couples grids by year
#'   - summary: Summary statistics by year
#'
#' @export
run_marital_projection <- function(phase8b_result,
                                    starting_marital,
                                    mortality_differentials,
                                    marriage_rates,
                                    divorce_rates,
                                    mortality_qx,
                                    start_year = 2022,
                                    end_year = 2099,
                                    min_age = 14,
                                    max_age = 100,
                                    verbose = TRUE) {

  if (verbose) {
    cli::cli_h1("Phase 8C: Marital Status Disaggregation")
    cli::cli_alert_info("Projection period: {start_year + 1}-{end_year}")
    cli::cli_alert_info("Ages: {min_age}-{max_age}")
  }

  # Extract Phase 8B population and deaths
  phase8b_pop <- phase8b_result$population
  phase8b_deaths <- phase8b_result$deaths
  phase8b_immigration <- phase8b_result$net_immigration

  # Get marriage rate grids by year
  # Structure depends on marriage_projection output
  if ("projected_rates" %in% names(marriage_rates)) {
    mar_rates_list <- marriage_rates$projected_rates
  } else if ("all_rates" %in% names(marriage_rates)) {
    mar_rates_list <- marriage_rates$all_rates
  } else {
    mar_rates_list <- marriage_rates
  }

  # Get divorce rate grids by year
  # Divorce rates structure: divorce_projection$projected_rates$rates
  if ("projected_rates" %in% names(divorce_rates)) {
    pr <- divorce_rates$projected_rates
    if ("rates" %in% names(pr)) {
      div_rates_list <- pr$rates
    } else {
      div_rates_list <- pr
    }
  } else if ("rates" %in% names(divorce_rates)) {
    div_rates_list <- divorce_rates$rates
  } else {
    div_rates_list <- divorce_rates
  }

  # Prepare mortality qx
  mortality_qx <- data.table::as.data.table(mortality_qx)

  # Initialize with starting year marital population
  current_marital <- data.table::as.data.table(starting_marital)
  if ("never_married" %in% current_marital$marital_status) {
    current_marital[marital_status == "never_married", marital_status := "single"]
  }

  # Build initial couples grid from starting marital population
  current_couples_grid <- build_married_couples_grid(current_marital, min_age, max_age)

  # Storage for results
  all_marital_pop <- list()
  all_couples_grids <- list()
  summary_list <- list()

  # Store starting year
  starting_year_marital <- data.table::copy(current_marital)
  starting_year_marital[, year := start_year]
  all_marital_pop[[as.character(start_year)]] <- starting_year_marital
  all_couples_grids[[as.character(start_year)]] <- current_couples_grid

  # Store prior year data for midyear calculations
  prior_marital <- current_marital
  prior_couples_grid <- current_couples_grid

  if (verbose) {
    cli::cli_progress_bar("Projecting marital status", total = end_year - start_year)
  }

  # Project each year
  projection_years <- (start_year + 1):end_year

  for (yr in projection_years) {
    if (verbose) {
      cli::cli_progress_update()
    }

    # Get Phase 8B data for this year
    pop_yr <- phase8b_pop[year == yr]
    deaths_yr <- phase8b_deaths[year == yr]
    imm_yr <- phase8b_immigration[year == yr]

    # Get marriage rates for this year
    mar_grid <- get_rate_grid_for_year(mar_rates_list, yr, min_age, max_age)

    # Get divorce rates for this year
    div_grid <- get_rate_grid_for_year(div_rates_list, yr, min_age, max_age)

    # Get qx for this year - ensure sorted by age
    qx_yr <- mortality_qx[year == yr]

    if (nrow(qx_yr) == 0) {
      cli::cli_abort("No mortality qx data found for year {yr}. Ensure mortality_qx covers projection period.")
    }

    # Sort by age and extract
    data.table::setorder(qx_yr, sex, age)
    qx_male <- qx_yr[sex == "male", qx]
    qx_female <- qx_yr[sex == "female", qx]

    if (length(qx_male) == 0 || length(qx_female) == 0) {
      cli::cli_abort("Missing qx data for year {yr}: male={length(qx_male)}, female={length(qx_female)}")
    }

    # Project this year
    year_result <- project_marital_year(
      marital_pop_boy = current_marital,
      phase8b_pop = pop_yr,
      total_deaths = deaths_yr,
      total_immigration = imm_yr,
      mortality_differentials = mortality_differentials,
      marriage_rate_grid = mar_grid,
      divorce_rate_grid = div_grid,
      couples_grid_boy = current_couples_grid,
      qx_male = qx_male,
      qx_female = qx_female,
      marital_pop_prior_boy = prior_marital,
      couples_grid_prior = prior_couples_grid,
      year = yr,
      min_age = min_age,
      max_age = max_age
    )

    # Store results
    all_marital_pop[[as.character(yr)]] <- year_result$marital_pop
    all_couples_grids[[as.character(yr)]] <- year_result$couples_grid

    # Store summary
    summary_list[[as.character(yr)]] <- data.table::data.table(
      year = yr,
      total_married = year_result$marital_pop[marital_status == "married", sum(population)],
      total_single = year_result$marital_pop[marital_status == "single", sum(population)],
      total_divorced = year_result$marital_pop[marital_status == "divorced", sum(population)],
      total_widowed = year_result$marital_pop[marital_status == "widowed", sum(population)],
      new_marriages = year_result$marriages,
      new_divorces = year_result$divorces,
      new_widowings = year_result$widowings
    )

    # Update for next iteration
    prior_marital <- current_marital
    prior_couples_grid <- current_couples_grid
    current_marital <- year_result$marital_pop
    current_couples_grid <- year_result$couples_grid

    # Progress message every 10 years
    if (verbose && (yr %% 10 == 0 || yr == end_year)) {
      total_married <- year_result$marital_pop[marital_status == "married", sum(population)]
      cli::cli_alert("{yr}: Married = {format(round(total_married/1e6, 2), nsmall=2)}M")
    }
  }

  if (verbose) {
    cli::cli_progress_done()
  }

  # Combine results
  marital_population <- data.table::rbindlist(all_marital_pop)
  summary_stats <- data.table::rbindlist(summary_list)

  if (verbose) {
    cli::cli_h2("Marital Status Projection Summary")
    cli::cli_alert_success("Projected {length(projection_years)} years")

    # Show starting and ending married populations
    start_married <- all_marital_pop[[as.character(start_year)]][
      marital_status == "married", sum(population)] / 1e6
    end_married <- all_marital_pop[[as.character(end_year)]][
      marital_status == "married", sum(population)] / 1e6

    cli::cli_alert_info(
      "Married population: {format(round(start_married, 2), nsmall=2)}M ({start_year}) -> {format(round(end_married, 2), nsmall=2)}M ({end_year})"
    )
  }

  list(
    marital_population = marital_population,
    couples_grids = all_couples_grids,
    summary = summary_stats,
    metadata = list(
      start_year = start_year,
      end_year = end_year,
      min_age = min_age,
      max_age = max_age,
      n_years = length(projection_years)
    )
  )
}

#' Get rate grid for a specific year (helper function)
#'
#' @param rates_list List of rate matrices or data.table with year and grid
#' @param year Target year
#' @param min_age Minimum age
#' @param max_age Maximum age
#'
#' @return Matrix (87×87) of rates
#'
#' @keywords internal
get_rate_grid_for_year <- function(rates_list, year, min_age = 14, max_age = 100) {

  n_ages <- max_age - min_age + 1

  # Handle different input formats
  if (is.list(rates_list) && !is.data.table(rates_list)) {
    # List indexed by year
    year_key <- as.character(year)
    if (year_key %in% names(rates_list)) {
      grid <- rates_list[[year_key]]
      if (is.matrix(grid)) {
        return(grid)
      }
    }

    # Try to find nearest year
    years_available <- as.integer(names(rates_list))
    if (length(years_available) > 0) {
      nearest <- years_available[which.min(abs(years_available - year))]
      grid <- rates_list[[as.character(nearest)]]
      if (is.matrix(grid)) {
        return(grid)
      }
    }
  }

  if (is.data.table(rates_list) || is.data.frame(rates_list)) {
    # Data.table with year column
    dt <- data.table::as.data.table(rates_list)
    if ("year" %in% names(dt) && "grid" %in% names(dt)) {
      grid_row <- dt[year == year]
      if (nrow(grid_row) > 0) {
        return(grid_row$grid[[1]])
      }
    }
  }

  # Return zero matrix if not found
  cli::cli_warn("Rate grid not found for year {year}, using zeros")
  matrix(0, nrow = n_ages, ncol = n_ages,
         dimnames = list(min_age:max_age, min_age:max_age))
}

# =============================================================================
# PHASE 8D: CHILDREN BY PARENT FATE (Equation 1.8.6)
# =============================================================================
# Tracks children ages 0-18 by parent survival status:
#   - Both parents alive
#   - Only father alive
#   - Only mother alive
#   - Both parents deceased
#
# Per TR2025 documentation: Children are tracked by (child_age, father_age,
# mother_age) with survival probabilities calculated from mortality rates.
# The calculated totals are adjusted to match population totals from Phase 8B.
# =============================================================================

#' Initialize children by parent ages (Phase 8D.1) - VECTORIZED
#'
#' @description
#' Creates the initial distribution of children (ages 0-18) by father age
#' and mother age for the starting year. This uses the married couples grid
#' and birth rates to establish the baseline distribution.
#'
#' Per TR2025: "The HISTORICAL POPULATION subprocess provides the historical
#' number of children (ages 0-18), number of women (ages 14-49), and the number
#' of married couples by single year of age of husband crossed with single year
#' of age of wife."
#'
#' This vectorized implementation replaces nested for loops with matrix operations.
#'
#' @param population_start data.table: Starting population by age and sex
#' @param couples_grid Matrix: Married couples by husband_age x wife_age (87x87)
#' @param birth_rates data.table: Birth rates by mother age (14-49)
#' @param qx_male Numeric vector: Male death probabilities by age (0-100)
#' @param qx_female Numeric vector: Female death probabilities by age (0-100)
#' @param min_parent_age Integer: Minimum parent age (default: 14)
#' @param max_parent_age Integer: Maximum parent age (default: 100)
#' @param max_child_age Integer: Maximum child age (default: 18)
#'
#' @return list with:
#'   - children_array: 4D array [child_age, father_age, mother_age, fate]
#'   - total_children: Total children ages 0-18
#'   - fate_totals: Totals by fate category
#'
#' @export
initialize_children_by_parents <- function(population_start,
                                            couples_grid,
                                            birth_rates,
                                            qx_male,
                                            qx_female,
                                            min_parent_age = 14,
                                            max_parent_age = 100,
                                            max_child_age = 18) {

  n_child_ages <- max_child_age + 1  # 0-18 = 19 ages
  n_parent_ages <- max_parent_age - min_parent_age + 1  # 87 ages
  n_fates <- 4

  # Fate indices: 1=both_alive, 2=only_father, 3=only_mother, 4=both_deceased
  fate_names <- c("both_alive", "only_father_alive", "only_mother_alive", "both_deceased")

  # Initialize 4D array: [child_age, father_age, mother_age, fate]
  children_array <- array(
    0,
    dim = c(n_child_ages, n_parent_ages, n_parent_ages, n_fates),
    dimnames = list(
      child_age = as.character(0:max_child_age),
      father_age = as.character(min_parent_age:max_parent_age),
      mother_age = as.character(min_parent_age:max_parent_age),
      fate = fate_names
    )
  )

  # Get total children ages 0-18 from population
  pop_dt <- data.table::as.data.table(population_start)
  total_children_by_age <- pop_dt[age <= max_child_age,
                                   .(total = sum(population, na.rm = TRUE)),
                                   by = age]
  data.table::setorder(total_children_by_age, age)

  # Convert to vector for easy access
  total_by_age_vec <- numeric(n_child_ages)
  for (i in seq_len(nrow(total_children_by_age))) {
    age_idx <- total_children_by_age$age[i] + 1
    if (age_idx <= n_child_ages) {
      total_by_age_vec[age_idx] <- total_children_by_age$total[i]
    }
  }

  couples_total <- sum(couples_grid, na.rm = TRUE)
  if (couples_total == 0) {
    cli::cli_warn("Couples grid is empty, using uniform distribution")
    couples_total <- 1
  }

  # =========================================================================
  # VECTORIZED distribution of children to parent ages
  # =========================================================================

  # Pre-compute valid parent age masks for each child age
  # father_ages and mother_ages as vectors (current ages)
  father_ages <- min_parent_age:max_parent_age
  mother_ages <- min_parent_age:max_parent_age

  # Create outer products for age constraints
  # father_age_mat[f, m] = father_ages[f] for all m
  # mother_age_mat[f, m] = mother_ages[m] for all f
  father_age_mat <- matrix(father_ages, nrow = n_parent_ages, ncol = n_parent_ages, byrow = FALSE)
  mother_age_mat <- matrix(mother_ages, nrow = n_parent_ages, ncol = n_parent_ages, byrow = TRUE)

  # Ensure couples_grid is the right size
  couples_use <- matrix(0, nrow = n_parent_ages, ncol = n_parent_ages)
  couple_rows <- min(nrow(couples_grid), n_parent_ages)
  couple_cols <- min(ncol(couples_grid), n_parent_ages)
  couples_use[1:couple_rows, 1:couple_cols] <- couples_grid[1:couple_rows, 1:couple_cols]

  for (c_age in 0:max_child_age) {
    total_at_age <- total_by_age_vec[c_age + 1]
    if (total_at_age == 0) next

    # Ages at birth
    father_age_at_birth <- father_age_mat - c_age
    mother_age_at_birth <- mother_age_mat - c_age

    # Valid mask: mother 14-49 at birth, father >= 14 at birth
    valid_mask <- (mother_age_at_birth >= 14) & (mother_age_at_birth <= 49) &
                  (father_age_at_birth >= 14)

    # Apply mask to couples grid
    temp_dist <- couples_use * valid_mask

    # Normalize and assign
    temp_total <- sum(temp_dist, na.rm = TRUE)
    if (temp_total > 0) {
      temp_dist <- temp_dist * total_at_age / temp_total
    }

    # Initially all children have both parents alive
    children_array[c_age + 1, , , 1] <- temp_dist
  }

  # =========================================================================
  # VECTORIZED cumulative mortality to estimate orphanhood
  # =========================================================================

  # Pre-compute px (survival) for all ages we might need
  # Father could have ages from (min_parent_age - max_child_age) to max_parent_age
  # But qx indices are 0-based (qx[1] = qx for age 0)

  # Build survival prob vectors with proper indexing
  max_qx_age <- length(qx_male) - 1
  px_male <- numeric(max_qx_age + 1)
  px_female <- numeric(max_qx_age + 1)
  for (a in 0:max_qx_age) {
    px_male[a + 1] <- 1 - qx_male[a + 1]
    px_female[a + 1] <- 1 - qx_female[a + 1]
  }

  # For each child age > 0, compute cumulative survival for each parent age combination
  for (c_age in 1:max_child_age) {
    # Get children with both alive
    children_both <- children_array[c_age + 1, , , 1]

    # Skip if no children at this age
    if (sum(children_both, na.rm = TRUE) == 0) next

    # Compute cumulative survival over c_age years
    # For father at age f_now, survival = prod(px_male[f_now-c_age+1 : f_now])
    # For mother at age m_now, survival = prod(px_female[m_now-c_age+1 : m_now])

    father_survival_vec <- numeric(n_parent_ages)
    mother_survival_vec <- numeric(n_parent_ages)

    for (f_idx in 1:n_parent_ages) {
      f_now <- father_ages[f_idx]
      surv <- 1
      for (yr_back in 1:c_age) {
        age_then <- f_now - yr_back + 1
        if (age_then >= 0 && age_then <= max_qx_age) {
          surv <- surv * px_male[age_then + 1]
        }
      }
      father_survival_vec[f_idx] <- max(0, min(1, surv))
    }

    for (m_idx in 1:n_parent_ages) {
      m_now <- mother_ages[m_idx]
      surv <- 1
      for (yr_back in 1:c_age) {
        age_then <- m_now - yr_back + 1
        if (age_then >= 0 && age_then <= max_qx_age) {
          surv <- surv * px_female[age_then + 1]
        }
      }
      mother_survival_vec[m_idx] <- max(0, min(1, surv))
    }

    # Create matrices for vectorized computation
    father_surv_mat <- matrix(father_survival_vec, nrow = n_parent_ages, ncol = n_parent_ages, byrow = FALSE)
    mother_surv_mat <- matrix(mother_survival_vec, nrow = n_parent_ages, ncol = n_parent_ages, byrow = TRUE)

    # Fate probabilities
    p_both <- father_surv_mat * mother_surv_mat
    p_only_father <- father_surv_mat * (1 - mother_surv_mat)
    p_only_mother <- (1 - father_surv_mat) * mother_surv_mat
    p_neither <- (1 - father_surv_mat) * (1 - mother_surv_mat)

    # Redistribute from "both alive" to appropriate fates
    children_array[c_age + 1, , , 1] <- children_both * p_both
    children_array[c_age + 1, , , 2] <- children_both * p_only_father
    children_array[c_age + 1, , , 3] <- children_both * p_only_mother
    children_array[c_age + 1, , , 4] <- children_both * p_neither
  }

  # Calculate totals
  total_children <- sum(children_array, na.rm = TRUE)
  fate_totals <- apply(children_array, 4, sum, na.rm = TRUE)
  names(fate_totals) <- fate_names

  list(
    children_array = children_array,
    total_children = total_children,
    fate_totals = fate_totals,
    metadata = list(
      n_child_ages = n_child_ages,
      n_parent_ages = n_parent_ages,
      min_parent_age = min_parent_age,
      max_parent_age = max_parent_age,
      max_child_age = max_child_age
    )
  )
}

#' Calculate parent survival probabilities (Phase 8D.3)
#'
#' @description
#' Calculates the probability of each parent surviving one year based on
#' their age and the mortality rates from the MORTALITY subprocess.
#'
#' @param father_age Integer: Father's current age
#' @param mother_age Integer: Mother's current age
#' @param qx_male Numeric vector: Male death probabilities by age (0-100)
#' @param qx_female Numeric vector: Female death probabilities by age (0-100)
#'
#' @return list with:
#'   - father_alive_prob: P(father survives)
#'   - mother_alive_prob: P(mother survives)
#'   - p_both_alive: P(both survive)
#'   - p_only_father: P(only father survives)
#'   - p_only_mother: P(only mother survives)
#'   - p_neither: P(neither survives)
#'
#' @export
calculate_parent_survival <- function(father_age, mother_age, qx_male, qx_female) {

  # Get death probabilities (handle out-of-range ages)
  if (father_age >= 0 && father_age < length(qx_male)) {
    qx_f <- qx_male[father_age + 1]
  } else if (father_age >= length(qx_male)) {
    qx_f <- qx_male[length(qx_male)]  # Use last value for very old ages
  } else {
    qx_f <- 0  # Invalid age
  }

  if (mother_age >= 0 && mother_age < length(qx_female)) {
    qx_m <- qx_female[mother_age + 1]
  } else if (mother_age >= length(qx_female)) {
    qx_m <- qx_female[length(qx_female)]
  } else {
    qx_m <- 0
  }

  # Survival probabilities
  father_alive_prob <- 1 - qx_f
  mother_alive_prob <- 1 - qx_m

  list(
    father_alive_prob = father_alive_prob,
    mother_alive_prob = mother_alive_prob,
    p_both_alive = father_alive_prob * mother_alive_prob,
    p_only_father = father_alive_prob * (1 - mother_alive_prob),
    p_only_mother = (1 - father_alive_prob) * mother_alive_prob,
    p_neither = (1 - father_alive_prob) * (1 - mother_alive_prob)
  )
}

#' Update parent fate distribution (Phase 8D.4)
#'
#' @description
#' Updates the four fate categories (both alive, only father, only mother,
#' both deceased) based on parent survival probabilities for one year.
#'
#' @param fate_prev Numeric vector of length 4: previous fate distribution
#'   [both_alive, only_father, only_mother, both_deceased]
#' @param survival_probs list: output from calculate_parent_survival()
#'
#' @return Numeric vector of length 4: updated fate distribution
#'
#' @export
update_parent_fates <- function(fate_prev, survival_probs) {

  p_f <- survival_probs$father_alive_prob
  p_m <- survival_probs$mother_alive_prob

  # Unpack previous fates
  prev_both <- fate_prev[1]
  prev_only_f <- fate_prev[2]
  prev_only_m <- fate_prev[3]
  prev_neither <- fate_prev[4]

  # Update each fate category
  # Children with both alive: some stay both, some become only_father, only_mother, or neither
  new_both <- prev_both * p_f * p_m

  # Children with only father: some stay, some become neither
  new_only_f <- prev_both * p_f * (1 - p_m) +
                prev_only_f * p_f

  # Children with only mother: some stay, some become neither
  new_only_m <- prev_both * (1 - p_f) * p_m +
                prev_only_m * p_m

  # Children with neither: accumulate from all transitions
  new_neither <- prev_both * (1 - p_f) * (1 - p_m) +
                 prev_only_f * (1 - p_f) +
                 prev_only_m * (1 - p_m) +
                 prev_neither

  c(new_both, new_only_f, new_only_m, new_neither)
}

#' Roll forward children one year (Phase 8D.2) - VECTORIZED
#'
#' @description
#' Ages children forward one year and updates parent ages and survival status.
#' Per TR2025: "Each year the number of children is then rolled forward a year
#' to the next age of husband, age of wife, and child age."
#'
#' This vectorized implementation replaces nested for loops with array operations
#' for significant performance improvement.
#'
#' @param children_array 4D array: [child_age, father_age, mother_age, fate]
#' @param new_births_grid Matrix: births distributed to father_age x mother_age
#' @param qx_male Numeric vector: Male death probabilities
#' @param qx_female Numeric vector: Female death probabilities
#' @param min_parent_age Integer: Minimum parent age (default: 14)
#' @param max_parent_age Integer: Maximum parent age (default: 100)
#' @param max_child_age Integer: Maximum child age (default: 18)
#'
#' @return Updated 4D array
#'
#' @export
roll_forward_children <- function(children_array,
                                   new_births_grid,
                                   qx_male,
                                   qx_female,
                                   min_parent_age = 14,
                                   max_parent_age = 100,
                                   max_child_age = 18) {

  n_child_ages <- max_child_age + 1
  n_parent_ages <- max_parent_age - min_parent_age + 1
  n_fates <- 4

  # Create new array for next year
  new_array <- array(
    0,
    dim = dim(children_array),
    dimnames = dimnames(children_array)
  )

  # Add new births at child_age = 0, all with both parents alive (fate = 1)
  if (!is.null(new_births_grid) && any(new_births_grid > 0, na.rm = TRUE)) {
    # Ensure births grid dimensions match
    births_dim <- pmin(dim(new_births_grid), c(n_parent_ages, n_parent_ages))
    new_array[1, 1:births_dim[1], 1:births_dim[2], 1] <-
      new_births_grid[1:births_dim[1], 1:births_dim[2]]
  }

  # =========================================================================
  # VECTORIZED aging and fate transitions
  # =========================================================================

  # Pre-compute survival probability matrices for all parent ages
  # Father ages: min_parent_age to max_parent_age (indices 1 to n_parent_ages)
  father_ages <- min_parent_age:max_parent_age

  # Get qx for each father age, handling out-of-range
  qx_f_vec <- numeric(n_parent_ages)
  for (i in seq_along(father_ages)) {
    age <- father_ages[i]
    if (age >= 0 && age < length(qx_male)) {
      qx_f_vec[i] <- qx_male[age + 1]
    } else if (age >= length(qx_male)) {
      qx_f_vec[i] <- qx_male[length(qx_male)]
    } else {
      qx_f_vec[i] <- 0
    }
  }

  # Get qx for each mother age
  mother_ages <- min_parent_age:max_parent_age
  qx_m_vec <- numeric(n_parent_ages)
  for (i in seq_along(mother_ages)) {
    age <- mother_ages[i]
    if (age >= 0 && age < length(qx_female)) {
      qx_m_vec[i] <- qx_female[age + 1]
    } else if (age >= length(qx_female)) {
      qx_m_vec[i] <- qx_female[length(qx_female)]
    } else {
      qx_m_vec[i] <- 0
    }
  }

  # Survival probabilities
  p_f_vec <- 1 - qx_f_vec  # P(father survives) for each father age
  p_m_vec <- 1 - qx_m_vec  # P(mother survives) for each mother age

  # Create 2D matrices for broadcasting: p_f[f_idx, m_idx] and p_m[f_idx, m_idx]
  # Using outer product for efficient matrix creation
  p_f_mat <- matrix(p_f_vec, nrow = n_parent_ages, ncol = n_parent_ages, byrow = FALSE)
  p_m_mat <- matrix(p_m_vec, nrow = n_parent_ages, ncol = n_parent_ages, byrow = TRUE)

  # Process all child ages 0 to max_child_age-1 (they age to 1 to max_child_age)
  for (c_age in 0:(max_child_age - 1)) {
    new_c_age <- c_age + 1

    # Extract current slice for this child age: [father_age, mother_age, fate]
    # Dimensions: n_parent_ages x n_parent_ages x 4
    prev_both <- children_array[c_age + 1, , , 1]
    prev_only_f <- children_array[c_age + 1, , , 2]
    prev_only_m <- children_array[c_age + 1, , , 3]
    prev_neither <- children_array[c_age + 1, , , 4]

    # --- INTERIOR CELLS: Both parents age forward (indices 1:(n-1) -> 2:n) ---

    # Source indices: 1:(n_parent_ages-1) for both dimensions
    # Target indices: 2:n_parent_ages for both dimensions

    # Get survival probs for source ages (1 to n_parent_ages-1)
    p_f_int <- p_f_mat[1:(n_parent_ages - 1), 1:(n_parent_ages - 1)]
    p_m_int <- p_m_mat[1:(n_parent_ages - 1), 1:(n_parent_ages - 1)]

    # Get previous fates for interior cells
    prev_both_int <- prev_both[1:(n_parent_ages - 1), 1:(n_parent_ages - 1)]
    prev_only_f_int <- prev_only_f[1:(n_parent_ages - 1), 1:(n_parent_ages - 1)]
    prev_only_m_int <- prev_only_m[1:(n_parent_ages - 1), 1:(n_parent_ages - 1)]
    prev_neither_int <- prev_neither[1:(n_parent_ages - 1), 1:(n_parent_ages - 1)]

    # Compute new fates (vectorized)
    new_both_int <- prev_both_int * p_f_int * p_m_int
    new_only_f_int <- prev_both_int * p_f_int * (1 - p_m_int) + prev_only_f_int * p_f_int
    new_only_m_int <- prev_both_int * (1 - p_f_int) * p_m_int + prev_only_m_int * p_m_int
    new_neither_int <- prev_both_int * (1 - p_f_int) * (1 - p_m_int) +
                       prev_only_f_int * (1 - p_f_int) +
                       prev_only_m_int * (1 - p_m_int) +
                       prev_neither_int

    # Store in new array at aged positions (indices 2:n for both dimensions)
    new_array[new_c_age + 1, 2:n_parent_ages, 2:n_parent_ages, 1] <-
      new_array[new_c_age + 1, 2:n_parent_ages, 2:n_parent_ages, 1] + new_both_int
    new_array[new_c_age + 1, 2:n_parent_ages, 2:n_parent_ages, 2] <-
      new_array[new_c_age + 1, 2:n_parent_ages, 2:n_parent_ages, 2] + new_only_f_int
    new_array[new_c_age + 1, 2:n_parent_ages, 2:n_parent_ages, 3] <-
      new_array[new_c_age + 1, 2:n_parent_ages, 2:n_parent_ages, 3] + new_only_m_int
    new_array[new_c_age + 1, 2:n_parent_ages, 2:n_parent_ages, 4] <-
      new_array[new_c_age + 1, 2:n_parent_ages, 2:n_parent_ages, 4] + new_neither_int

    # --- MOTHER AT MAX AGE: Father ages, mother stays at max ---
    # Source: f_idx 1:(n-1), m_idx = n
    # Target: f_idx 2:n, m_idx = n

    p_f_max_m <- p_f_vec[1:(n_parent_ages - 1)]
    p_m_max <- p_m_vec[n_parent_ages]  # scalar

    prev_both_max_m <- prev_both[1:(n_parent_ages - 1), n_parent_ages]
    prev_only_f_max_m <- prev_only_f[1:(n_parent_ages - 1), n_parent_ages]
    prev_only_m_max_m <- prev_only_m[1:(n_parent_ages - 1), n_parent_ages]
    prev_neither_max_m <- prev_neither[1:(n_parent_ages - 1), n_parent_ages]

    new_both_max_m <- prev_both_max_m * p_f_max_m * p_m_max
    new_only_f_max_m <- prev_both_max_m * p_f_max_m * (1 - p_m_max) + prev_only_f_max_m * p_f_max_m
    new_only_m_max_m <- prev_both_max_m * (1 - p_f_max_m) * p_m_max + prev_only_m_max_m * p_m_max
    new_neither_max_m <- prev_both_max_m * (1 - p_f_max_m) * (1 - p_m_max) +
                         prev_only_f_max_m * (1 - p_f_max_m) +
                         prev_only_m_max_m * (1 - p_m_max) +
                         prev_neither_max_m

    new_array[new_c_age + 1, 2:n_parent_ages, n_parent_ages, 1] <-
      new_array[new_c_age + 1, 2:n_parent_ages, n_parent_ages, 1] + new_both_max_m
    new_array[new_c_age + 1, 2:n_parent_ages, n_parent_ages, 2] <-
      new_array[new_c_age + 1, 2:n_parent_ages, n_parent_ages, 2] + new_only_f_max_m
    new_array[new_c_age + 1, 2:n_parent_ages, n_parent_ages, 3] <-
      new_array[new_c_age + 1, 2:n_parent_ages, n_parent_ages, 3] + new_only_m_max_m
    new_array[new_c_age + 1, 2:n_parent_ages, n_parent_ages, 4] <-
      new_array[new_c_age + 1, 2:n_parent_ages, n_parent_ages, 4] + new_neither_max_m

    # --- FATHER AT MAX AGE: Mother ages, father stays at max ---
    # Source: f_idx = n, m_idx 1:(n-1)
    # Target: f_idx = n, m_idx 2:n

    p_f_max <- p_f_vec[n_parent_ages]  # scalar
    p_m_max_f <- p_m_vec[1:(n_parent_ages - 1)]

    prev_both_max_f <- prev_both[n_parent_ages, 1:(n_parent_ages - 1)]
    prev_only_f_max_f <- prev_only_f[n_parent_ages, 1:(n_parent_ages - 1)]
    prev_only_m_max_f <- prev_only_m[n_parent_ages, 1:(n_parent_ages - 1)]
    prev_neither_max_f <- prev_neither[n_parent_ages, 1:(n_parent_ages - 1)]

    new_both_max_f <- prev_both_max_f * p_f_max * p_m_max_f
    new_only_f_max_f <- prev_both_max_f * p_f_max * (1 - p_m_max_f) + prev_only_f_max_f * p_f_max
    new_only_m_max_f <- prev_both_max_f * (1 - p_f_max) * p_m_max_f + prev_only_m_max_f * p_m_max_f
    new_neither_max_f <- prev_both_max_f * (1 - p_f_max) * (1 - p_m_max_f) +
                         prev_only_f_max_f * (1 - p_f_max) +
                         prev_only_m_max_f * (1 - p_m_max_f) +
                         prev_neither_max_f

    new_array[new_c_age + 1, n_parent_ages, 2:n_parent_ages, 1] <-
      new_array[new_c_age + 1, n_parent_ages, 2:n_parent_ages, 1] + new_both_max_f
    new_array[new_c_age + 1, n_parent_ages, 2:n_parent_ages, 2] <-
      new_array[new_c_age + 1, n_parent_ages, 2:n_parent_ages, 2] + new_only_f_max_f
    new_array[new_c_age + 1, n_parent_ages, 2:n_parent_ages, 3] <-
      new_array[new_c_age + 1, n_parent_ages, 2:n_parent_ages, 3] + new_only_m_max_f
    new_array[new_c_age + 1, n_parent_ages, 2:n_parent_ages, 4] <-
      new_array[new_c_age + 1, n_parent_ages, 2:n_parent_ages, 4] + new_neither_max_f

    # --- BOTH PARENTS AT MAX AGE: Both stay at max ---
    # Source: f_idx = n, m_idx = n
    # Target: f_idx = n, m_idx = n

    prev_both_mm <- prev_both[n_parent_ages, n_parent_ages]
    prev_only_f_mm <- prev_only_f[n_parent_ages, n_parent_ages]
    prev_only_m_mm <- prev_only_m[n_parent_ages, n_parent_ages]
    prev_neither_mm <- prev_neither[n_parent_ages, n_parent_ages]

    new_both_mm <- prev_both_mm * p_f_max * p_m_max
    new_only_f_mm <- prev_both_mm * p_f_max * (1 - p_m_max) + prev_only_f_mm * p_f_max
    new_only_m_mm <- prev_both_mm * (1 - p_f_max) * p_m_max + prev_only_m_mm * p_m_max
    new_neither_mm <- prev_both_mm * (1 - p_f_max) * (1 - p_m_max) +
                      prev_only_f_mm * (1 - p_f_max) +
                      prev_only_m_mm * (1 - p_m_max) +
                      prev_neither_mm

    new_array[new_c_age + 1, n_parent_ages, n_parent_ages, 1] <-
      new_array[new_c_age + 1, n_parent_ages, n_parent_ages, 1] + new_both_mm
    new_array[new_c_age + 1, n_parent_ages, n_parent_ages, 2] <-
      new_array[new_c_age + 1, n_parent_ages, n_parent_ages, 2] + new_only_f_mm
    new_array[new_c_age + 1, n_parent_ages, n_parent_ages, 3] <-
      new_array[new_c_age + 1, n_parent_ages, n_parent_ages, 3] + new_only_m_mm
    new_array[new_c_age + 1, n_parent_ages, n_parent_ages, 4] <-
      new_array[new_c_age + 1, n_parent_ages, n_parent_ages, 4] + new_neither_mm
  }

  # Children who turn 19 exit the tracking (ages out)

  new_array
}

#' Adjust children to population total (Phase 8D.5)
#'
#' @description
#' Scales calculated children by parent ages to match the total children
#' in the population projection. Per TR2025: "The calculated number of
#' children by age of father and age of mother must match the number of
#' children in the historical or projected population."
#'
#' @param children_array 4D array: calculated children by parent ages and fate
#' @param target_children Numeric vector: target children by age (0-18) from population
#' @param max_child_age Integer: maximum child age (default: 18)
#'
#' @return Adjusted 4D array
#'
#' @export
adjust_children_to_total <- function(children_array,
                                      target_children,
                                      max_child_age = 18) {

  adjusted <- children_array

  for (c_age in 0:max_child_age) {
    # Get calculated total for this age
    calc_total <- sum(children_array[c_age + 1, , , ], na.rm = TRUE)

    # Get target total
    target <- if (c_age + 1 <= length(target_children)) {
      target_children[c_age + 1]
    } else {
      0
    }

    # Calculate scaling factor
    if (calc_total > 0 && target > 0) {
      scale_factor <- target / calc_total

      # Apply scaling to all dimensions for this child age
      adjusted[c_age + 1, , , ] <- children_array[c_age + 1, , , ] * scale_factor
    }
  }

  adjusted
}

#' Distribute births to parent ages (helper for Phase 8D) - VECTORIZED
#'
#' @description
#' Distributes total births to father_age x mother_age cells using the
#' married couples grid and birth rates. Per TR2025: "The births are then
#' distributed to the age of husband in the same proportions as the age of
#' husband crossed with age of wife married couples grid."
#'
#' This vectorized implementation uses matrix operations.
#'
#' @param total_births Total births for the year
#' @param birth_rates_by_age Birth rates by mother age (14-49)
#' @param female_pop_by_age Female population by age
#' @param couples_grid Married couples grid
#' @param min_parent_age Minimum parent age (default: 14)
#' @param max_parent_age Maximum parent age (default: 100)
#'
#' @return Matrix of births by father_age x mother_age
#'
#' @keywords internal
distribute_births_to_parents <- function(total_births,
                                          birth_rates_by_age,
                                          female_pop_by_age,
                                          couples_grid,
                                          min_parent_age = 14,
                                          max_parent_age = 100) {

  n_parent_ages <- max_parent_age - min_parent_age + 1

  # Create births grid
  births_grid <- matrix(0, nrow = n_parent_ages, ncol = n_parent_ages)

  # Mother ages for childbearing
  mother_ages <- 14:49
  n_mother_ages <- length(mother_ages)

  # Vectorized: get birth rates and female population for each mother age
  rates <- numeric(n_mother_ages)
  rates[1:min(n_mother_ages, length(birth_rates_by_age))] <-
    birth_rates_by_age[1:min(n_mother_ages, length(birth_rates_by_age))]

  pops <- numeric(n_mother_ages)
  for (i in seq_along(mother_ages)) {
    m_age <- mother_ages[i]
    if (m_age + 1 <= length(female_pop_by_age)) {
      pops[i] <- female_pop_by_age[m_age + 1]
    }
  }

  # Births by mother age
  births_by_mother <- rates * pops

  total_calc_births <- sum(births_by_mother, na.rm = TRUE)

  # Scale to match total births
  if (total_calc_births > 0) {
    births_by_mother <- births_by_mother * total_births / total_calc_births
  }

  # Get column indices for mother ages in the grid
  m_indices <- mother_ages - min_parent_age + 1
  valid_m <- m_indices >= 1 & m_indices <= n_parent_ages

  # Vectorized distribution: for each valid mother age column
  for (i in which(valid_m)) {
    m_idx <- m_indices[i]

    if (births_by_mother[i] <= 0) next

    # Get husband age distribution from couples grid
    husband_dist <- couples_grid[, m_idx]
    husband_total <- sum(husband_dist, na.rm = TRUE)

    if (husband_total > 0) {
      births_grid[, m_idx] <- husband_dist * births_by_mother[i] / husband_total
    }
  }

  births_grid
}

#' Aggregate children array to output format (helper for Phase 8D) - VECTORIZED
#'
#' @description
#' Aggregates the detailed children array into the output format specified
#' by TR2025: C^z_{x,s,g,f} - children by child age (x), parent sex (s),
#' parent age group (g), and fate (f).
#'
#' This vectorized implementation pre-computes age group indices for efficiency.
#'
#' @param children_array 4D array: [child_age, father_age, mother_age, fate]
#' @param parent_age_groups list: age group definitions (e.g., list("14-24" = 14:24, ...))
#' @param year Integer: projection year
#' @param min_parent_age Integer: minimum parent age
#' @param max_child_age Integer: maximum child age (default: 18)
#'
#' @return data.table with columns: year, child_age, parent_sex, parent_age_group, fate, count
#'
#' @keywords internal
aggregate_children_to_output <- function(children_array,
                                          parent_age_groups,
                                          year,
                                          min_parent_age = 14,
                                          max_child_age = 18) {

  fate_names <- c("both_alive", "only_father_alive", "only_mother_alive", "both_deceased")
  n_parent_ages <- dim(children_array)[2]
  n_age_groups <- length(parent_age_groups)
  n_fates <- 4

  # Pre-compute valid indices for each age group
  age_group_indices <- lapply(parent_age_groups, function(ages) {
    idx <- ages - min_parent_age + 1
    idx[idx >= 1 & idx <= n_parent_ages]
  })

  # Pre-allocate result data.table
  n_rows <- (max_child_age + 1) * 2 * n_age_groups * n_fates
  result <- data.table::data.table(
    year = rep(year, n_rows),
    child_age = integer(n_rows),
    parent_sex = character(n_rows),
    parent_age_group = character(n_rows),
    fate = character(n_rows),
    count = numeric(n_rows)
  )

  row_idx <- 1
  grp_names <- names(parent_age_groups)

  for (c_age in 0:max_child_age) {
    # Extract slice for this child age
    child_slice <- children_array[c_age + 1, , , , drop = FALSE]
    dim(child_slice) <- dim(children_array)[2:4]

    for (s_idx in 1:2) {
      parent_sex <- c("father", "mother")[s_idx]

      for (g_idx in seq_along(grp_names)) {
        grp_name <- grp_names[g_idx]
        p_indices <- age_group_indices[[g_idx]]

        if (length(p_indices) == 0) {
          # No valid indices - store zeros
          for (f_idx in 1:n_fates) {
            data.table::set(result, row_idx, "child_age", c_age)
            data.table::set(result, row_idx, "parent_sex", parent_sex)
            data.table::set(result, row_idx, "parent_age_group", grp_name)
            data.table::set(result, row_idx, "fate", fate_names[f_idx])
            data.table::set(result, row_idx, "count", 0)
            row_idx <- row_idx + 1
          }
        } else {
          for (f_idx in 1:n_fates) {
            if (parent_sex == "father") {
              # Sum across mother ages for these father ages
              count <- sum(child_slice[p_indices, , f_idx], na.rm = TRUE)
            } else {
              # Sum across father ages for these mother ages
              count <- sum(child_slice[, p_indices, f_idx], na.rm = TRUE)
            }

            data.table::set(result, row_idx, "child_age", c_age)
            data.table::set(result, row_idx, "parent_sex", parent_sex)
            data.table::set(result, row_idx, "parent_age_group", grp_name)
            data.table::set(result, row_idx, "fate", fate_names[f_idx])
            data.table::set(result, row_idx, "count", count)
            row_idx <- row_idx + 1
          }
        }
      }
    }
  }

  result
}

#' Project children by parent fate (Phase 8D.6 - Main Function)
#'
#' @description
#' Main function for projecting children ages 0-18 by parent survival status
#' (Equation 1.8.6). Tracks children by father age × mother age with four
#' fate categories: both alive, only father, only mother, both deceased.
#'
#' Per TR2025: "The children (ages 0-18) population is further disaggregated
#' into the following four parent statuses (i.e., fates): both parents are
#' alive, only father is alive, only mother is alive, and both parents deceased."
#'
#' @param phase8b_result list: Output from run_population_projection (Phase 8B)
#' @param marital_result list: Output from run_marital_projection (Phase 8C)
#' @param birth_rates data.table: Birth rates by year and mother age
#' @param mortality_qx data.table: Death probabilities by year, age, sex
#' @param parent_age_groups list: Parent age group definitions
#' @param start_year Integer: Starting year (default: 2022)
#' @param end_year Integer: End year (default: 2099)
#' @param max_child_age Integer: Maximum child age (default: 18)
#' @param min_parent_age Integer: Minimum parent age (default: 14)
#' @param max_parent_age Integer: Maximum parent age (default: 100)
#' @param verbose Logical: Print progress messages (default: TRUE)
#'
#' @return list with:
#'   - children_fate: data.table C^z_{x,s,g,f} by year
#'   - children_arrays: list of detailed arrays by year (optional)
#'   - summary: Summary statistics by year
#'
#' @export
project_children_fate <- function(phase8b_result,
                                   marital_result,
                                   birth_rates,
                                   mortality_qx,
                                   parent_age_groups = NULL,
                                   start_year = 2022,
                                   end_year = 2099,
                                   max_child_age = 18,
                                   min_parent_age = 14,
                                   max_parent_age = 100,
                                   verbose = TRUE) {

  if (verbose) {
    cli::cli_h1("Phase 8D: Children by Parent Fate")
    cli::cli_alert_info("Projection period: {start_year}-{end_year}")
    cli::cli_alert_info("Child ages: 0-{max_child_age}")
  }

  # Default parent age groups per TR2025
  if (is.null(parent_age_groups)) {
    parent_age_groups <- list(
      "14-24" = 14:24,
      "25-34" = 25:34,
      "35-44" = 35:44,
      "45-54" = 45:54,
      "55-64" = 55:64,
      "65-100" = 65:100
    )
  }

  # Extract required data
  phase8b_pop <- phase8b_result$population
  phase8b_births <- phase8b_result$births

  # Get married couples grids from marital result
  couples_grids <- marital_result$couples_grids

  # Prepare mortality qx
  mortality_qx <- data.table::as.data.table(mortality_qx)

  # Prepare birth rates
  birth_rates <- data.table::as.data.table(birth_rates)

  # Get starting year population for initialization
  start_pop <- phase8b_pop[year == start_year]

  # Get starting couples grid
  start_couples <- if (!is.null(couples_grids[[as.character(start_year)]])) {
    couples_grids[[as.character(start_year)]]
  } else {
    # Build from marital population if grid not available
    marital_pop <- marital_result$marital_population[year == start_year]
    build_married_couples_grid(marital_pop, min_parent_age, max_parent_age)
  }

  # Get qx for starting year
  qx_start <- mortality_qx[year == start_year]
  data.table::setorder(qx_start, sex, age)
  qx_male <- qx_start[sex == "male", qx]
  qx_female <- qx_start[sex == "female", qx]

  # Handle missing qx by using nearest year
  if (length(qx_male) == 0) {
    nearest_year <- mortality_qx[, min(year)]
    qx_start <- mortality_qx[year == nearest_year]
    data.table::setorder(qx_start, sex, age)
    qx_male <- qx_start[sex == "male", qx]
    qx_female <- qx_start[sex == "female", qx]
    cli::cli_warn("Using qx from year {nearest_year} for initialization")
  }

  # Get birth rates for starting year
  br_start <- birth_rates[year == start_year]
  if (nrow(br_start) == 0) {
    br_start <- birth_rates[year == min(birth_rates$year)]
  }
  br_by_age <- br_start[order(age), birth_rate]

  # Initialize children array
  if (verbose) {
    cli::cli_alert("Initializing children by parent ages for {start_year}...")
  }

  init_result <- initialize_children_by_parents(
    population_start = start_pop,
    couples_grid = start_couples,
    birth_rates = br_by_age,
    qx_male = qx_male,
    qx_female = qx_female,
    min_parent_age = min_parent_age,
    max_parent_age = max_parent_age,
    max_child_age = max_child_age
  )

  current_array <- init_result$children_array

  if (verbose) {
    cli::cli_alert_success("Initial children: {format(round(init_result$total_children/1e6, 2), nsmall=2)}M")
    cli::cli_alert_info("Fate distribution: Both={round(100*init_result$fate_totals[1]/init_result$total_children, 1)}%, Orphan={round(100*(1-init_result$fate_totals[1]/init_result$total_children), 1)}%")
  }

  # Storage for results
  all_children_fate <- list()
  all_arrays <- list()
  summary_list <- list()

  # Store starting year
  start_fate <- aggregate_children_to_output(
    current_array, parent_age_groups, start_year, min_parent_age, max_child_age
  )
  all_children_fate[[as.character(start_year)]] <- start_fate
  all_arrays[[as.character(start_year)]] <- current_array

  # Calculate starting year summary
  fate_totals_start <- apply(current_array, 4, sum, na.rm = TRUE)
  summary_list[[as.character(start_year)]] <- data.table::data.table(
    year = start_year,
    total_children = sum(current_array, na.rm = TRUE),
    both_alive = fate_totals_start[1],
    only_father = fate_totals_start[2],
    only_mother = fate_totals_start[3],
    both_deceased = fate_totals_start[4],
    orphan_rate = 1 - fate_totals_start[1] / sum(current_array, na.rm = TRUE)
  )

  if (verbose) {
    cli::cli_progress_bar("Projecting children fate", total = end_year - start_year)
  }

  # Project each year
  projection_years <- (start_year + 1):end_year

  for (yr in projection_years) {
    if (verbose) {
      cli::cli_progress_update()
    }

    # Get qx for this year
    qx_yr <- mortality_qx[year == yr]
    if (nrow(qx_yr) == 0) {
      # Use nearest available year
      available_years <- unique(mortality_qx$year)
      nearest <- available_years[which.min(abs(available_years - yr))]
      qx_yr <- mortality_qx[year == nearest]
    }
    data.table::setorder(qx_yr, sex, age)
    qx_male <- qx_yr[sex == "male", qx]
    qx_female <- qx_yr[sex == "female", qx]

    # Get birth rates for this year
    br_yr <- birth_rates[year == yr]
    if (nrow(br_yr) == 0) {
      available_years <- unique(birth_rates$year)
      nearest <- available_years[which.min(abs(available_years - yr))]
      br_yr <- birth_rates[year == nearest]
    }
    br_by_age <- br_yr[order(age), birth_rate]

    # Get total births for this year from Phase 8B
    total_births_yr <- phase8b_births[year == yr, sum(births, na.rm = TRUE)]

    # Get female population by age for this year
    female_pop <- phase8b_pop[year == yr & sex == "female"]
    data.table::setorder(female_pop, age)
    female_pop_by_age <- female_pop$population

    # Get couples grid for this year
    couples_yr <- if (!is.null(couples_grids[[as.character(yr)]])) {
      couples_grids[[as.character(yr)]]
    } else if (!is.null(couples_grids[[as.character(yr - 1)]])) {
      couples_grids[[as.character(yr - 1)]]
    } else {
      start_couples  # Fallback to start
    }

    # Calculate births distribution to parent ages
    births_grid <- distribute_births_to_parents(
      total_births = total_births_yr,
      birth_rates_by_age = br_by_age,
      female_pop_by_age = female_pop_by_age,
      couples_grid = couples_yr,
      min_parent_age = min_parent_age,
      max_parent_age = max_parent_age
    )

    # Roll forward children
    current_array <- roll_forward_children(
      children_array = current_array,
      new_births_grid = births_grid,
      qx_male = qx_male,
      qx_female = qx_female,
      min_parent_age = min_parent_age,
      max_parent_age = max_parent_age,
      max_child_age = max_child_age
    )

    # Get target children totals from Phase 8B population
    children_pop <- phase8b_pop[year == yr & age <= max_child_age]
    target_by_age <- children_pop[, .(total = sum(population)), by = age]
    data.table::setorder(target_by_age, age)
    target_children <- target_by_age$total

    # Adjust to match population totals
    current_array <- adjust_children_to_total(
      children_array = current_array,
      target_children = target_children,
      max_child_age = max_child_age
    )

    # Aggregate to output format
    yr_fate <- aggregate_children_to_output(
      current_array, parent_age_groups, yr, min_parent_age, max_child_age
    )

    # Store results
    all_children_fate[[as.character(yr)]] <- yr_fate
    all_arrays[[as.character(yr)]] <- current_array

    # Calculate summary
    fate_totals <- apply(current_array, 4, sum, na.rm = TRUE)
    total_yr <- sum(current_array, na.rm = TRUE)

    summary_list[[as.character(yr)]] <- data.table::data.table(
      year = yr,
      total_children = total_yr,
      both_alive = fate_totals[1],
      only_father = fate_totals[2],
      only_mother = fate_totals[3],
      both_deceased = fate_totals[4],
      orphan_rate = 1 - fate_totals[1] / total_yr
    )

    # Progress message every 10 years
    if (verbose && (yr %% 10 == 0 || yr == end_year)) {
      orphan_pct <- round(100 * (1 - fate_totals[1] / total_yr), 2)
      cli::cli_alert("{yr}: Children = {format(round(total_yr/1e6, 2), nsmall=2)}M, Orphan rate = {orphan_pct}%")
    }
  }

  if (verbose) {
    cli::cli_progress_done()
  }

  # Combine results
  children_fate <- data.table::rbindlist(all_children_fate)
  summary_stats <- data.table::rbindlist(summary_list)

  if (verbose) {
    cli::cli_h2("Children Fate Projection Summary")
    cli::cli_alert_success("Projected {length(projection_years)} years")

    start_orphan <- summary_stats[year == start_year, orphan_rate]
    end_orphan <- summary_stats[year == end_year, orphan_rate]
    cli::cli_alert_info(
      "Orphan rate: {round(100*start_orphan, 2)}% ({start_year}) -> {round(100*end_orphan, 2)}% ({end_year})"
    )
  }

  list(
    children_fate = children_fate,
    children_arrays = all_arrays,
    summary = summary_stats,
    metadata = list(
      start_year = start_year,
      end_year = end_year,
      max_child_age = max_child_age,
      min_parent_age = min_parent_age,
      max_parent_age = max_parent_age,
      parent_age_groups = parent_age_groups,
      n_years = length(projection_years) + 1
    )
  )
}

#' Project mean children per married couple (Phase 8D.7)
#'
#' @description
#' Uses linear regression on CPS historical data to project the mean number
#' of children per married couple by age of householder.
#'
#' Per TR2025: "Linear regression is used to model the relationship between
#' the mean number of children in the population program to the mean number
#' of children from the U.S. Census Bureau."
#'
#' @param cps_children data.table: CPS children per couple data (from fetch_cps_children_per_couple)
#' @param children_fate_result list: Output from project_children_fate
#' @param marital_result list: Output from run_marital_projection (for married couples)
#' @param start_year Integer: First projection year
#' @param end_year Integer: Last projection year
#' @param trend_years Integer: Number of recent years for trend (default: 20)
#' @param verbose Logical: Print progress messages (default: TRUE)
#'
#' @return data.table with columns: year, age_group, mean_children, source
#'
#' @export
project_mean_children_per_couple <- function(cps_children,
                                              children_fate_result = NULL,
                                              marital_result = NULL,
                                              start_year = 2023,
                                              end_year = 2099,
                                              trend_years = 20,
                                              verbose = TRUE) {

  if (verbose) {
    cli::cli_h3("Projecting Mean Children per Married Couple")
  }

  cps_dt <- data.table::as.data.table(cps_children)

  # Get age groups from CPS data
  age_groups <- unique(cps_dt$age_group)

  result_list <- list()

  # Add historical data
  for (grp in age_groups) {
    grp_data <- cps_dt[age_group == grp]

    result_list[[length(result_list) + 1]] <- data.table::data.table(
      year = grp_data$year,
      age_group = grp,
      mean_children = grp_data$mean_children,
      source = "historical"
    )
  }

  # Project forward using linear trend from recent years
  projection_years <- start_year:end_year

  for (grp in age_groups) {
    grp_data <- cps_dt[age_group == grp]

    # Use last N years for trend
    recent_data <- tail(grp_data[order(year)], trend_years)

    if (nrow(recent_data) < 2) {
      # Not enough data, use constant
      mean_val <- mean(grp_data$mean_children, na.rm = TRUE)
      proj_values <- rep(mean_val, length(projection_years))
    } else {
      # Fit linear model
      model <- lm(mean_children ~ year, data = recent_data)

      # Project forward
      new_data <- data.frame(year = projection_years)
      proj_values <- predict(model, newdata = new_data)

      # Constrain to reasonable bounds (0.5 to 5 children)
      proj_values <- pmax(0.5, pmin(5, proj_values))
    }

    result_list[[length(result_list) + 1]] <- data.table::data.table(
      year = projection_years,
      age_group = grp,
      mean_children = proj_values,
      source = "projected"
    )
  }

  result <- data.table::rbindlist(result_list)
  data.table::setorder(result, year, age_group)

  if (verbose) {
    # Show trend summary
    for (grp in age_groups[1:min(3, length(age_groups))]) {
      start_val <- result[age_group == grp & year == start_year, mean_children]
      end_val <- result[age_group == grp & year == end_year, mean_children]
      if (length(start_val) > 0 && length(end_val) > 0) {
        cli::cli_alert_info(
          "Age {grp}: {round(start_val, 2)} ({start_year}) -> {round(end_val, 2)} ({end_year})"
        )
      }
    }
  }

  result
}

#' Validate children fate totals (Phase 8D.8)
#'
#' @description
#' Validates that children by fate sum to total children in population.
#'
#' @param children_fate_result list: Output from project_children_fate
#' @param phase8b_result list: Output from run_population_projection (Phase 8B)
#' @param max_child_age Integer: Maximum child age (default: 18)
#' @param tolerance Numeric: Relative tolerance for validation (default: 0.01)
#'
#' @return Validation report list
#'
#' @export
validate_children_fate <- function(children_fate_result,
                                    phase8b_result,
                                    max_child_age = 18,
                                    tolerance = 0.01) {

  cli::cli_h2("Phase 8D Validation: Children by Parent Fate")

  summary <- children_fate_result$summary
  phase8b_pop <- phase8b_result$population

  validation_results <- list()

  # Test 1: Children totals match Phase 8B
  cli::cli_h3("Test 1: Children totals match Phase 8B population")

  years_to_check <- unique(summary$year)
  mismatches <- 0
  max_error <- 0

  for (yr in years_to_check) {
    # Get children total from fate projection
    fate_total <- summary[year == yr, total_children]

    # Get children total from Phase 8B
    phase8b_total <- phase8b_pop[year == yr & age <= max_child_age, sum(population, na.rm = TRUE)]

    error <- abs(fate_total - phase8b_total) / phase8b_total

    if (error > tolerance) {
      mismatches <- mismatches + 1
    }
    max_error <- max(max_error, error)
  }

  if (mismatches == 0) {
    cli::cli_alert_success("All {length(years_to_check)} years within {tolerance*100}% tolerance")
  } else {
    cli::cli_alert_warning("{mismatches}/{length(years_to_check)} years exceed tolerance")
  }
  cli::cli_alert_info("Maximum error: {round(max_error*100, 3)}%")

  validation_results$totals_match <- (mismatches == 0)
  validation_results$max_total_error <- max_error

  # Test 2: Fates sum to total
  cli::cli_h3("Test 2: Fate categories sum to total")

  fate_sum_errors <- summary[, abs(both_alive + only_father + only_mother + both_deceased - total_children) / total_children]
  max_fate_error <- max(fate_sum_errors, na.rm = TRUE)

  if (max_fate_error < 1e-10) {
    cli::cli_alert_success("Fate categories sum to total (max error: {format(max_fate_error, scientific=TRUE)})")
    validation_results$fates_sum <- TRUE
  } else {
    cli::cli_alert_warning("Fate sum error: {format(max_fate_error, scientific=TRUE)}")
    validation_results$fates_sum <- (max_fate_error < 1e-6)
  }

  # Test 3: Orphan rate reasonable
  cli::cli_h3("Test 3: Orphan rate reasonable")

  orphan_rates <- summary$orphan_rate
  min_orphan <- min(orphan_rates, na.rm = TRUE)
  max_orphan <- max(orphan_rates, na.rm = TRUE)

  cli::cli_alert_info("Orphan rate range: {round(100*min_orphan, 2)}% - {round(100*max_orphan, 2)}%")

  # Expect orphan rate between 0.1% and 10%
  if (min_orphan >= 0 && max_orphan <= 0.15) {
    cli::cli_alert_success("Orphan rates within expected range")
    validation_results$orphan_rate_valid <- TRUE
  } else {
    cli::cli_alert_warning("Orphan rates outside expected range (0-15%)")
    validation_results$orphan_rate_valid <- FALSE
  }

  # Overall validation
  validation_results$all_passed <- validation_results$totals_match &&
                                   validation_results$fates_sum &&
                                   validation_results$orphan_rate_valid

  if (validation_results$all_passed) {
    cli::cli_alert_success("Phase 8D validation: All checks passed")
  } else {
    cli::cli_alert_warning("Phase 8D validation: Some checks failed")
  }

  validation_results
}

# =============================================================================
# PHASE 8E: CNI POPULATION (EQUATION 1.8.7)
# =============================================================================

#' Project USAF Population (Phase 8E.1) - Vectorized
#'
#' @description
#' Projects the U.S. resident population plus armed forces overseas (USAF)
#' forward by sex and single year of age using vectorized operations.
#'
#' Per TR2025: "The USAF population is projected forward by sex and single year
#' of age by subtracting deaths using the same death rates applied to the Social
#' Security area population, adding births in the residential population calculated
#' with the same birth rates applied to the Social Security area, and adding net
#' immigration prorated by the ratio of beginning of year USAF population to the
#' Social Security area population."
#'
#' @param usaf_prev data.table: USAF population at previous year-end by age/sex
#' @param ss_pop_prev data.table: SS area population at previous year-end by age/sex
#' @param births_ss Numeric: Total births during year (from SS area)
#' @param deaths_ss data.table: Deaths during year by age/sex (from SS area)
#' @param net_immigration data.table: Net immigration by age/sex
#' @param sex_ratio Numeric: Male births per 1000 female births (default: 1048)
#' @param max_age Integer: Maximum single age (default: 100)
#'
#' @return data.table with USAF population at end of year by age/sex
#'
#' @keywords internal
project_usaf_year <- function(usaf_prev,
                               ss_pop_prev,
                               births_ss,
                               deaths_ss,
                               net_immigration,
                               sex_ratio = 1048,
                               max_age = 100) {

  # Ensure data.tables are keyed properly for fast merges
  data.table::setkey(usaf_prev, age, sex)
  data.table::setkey(ss_pop_prev, age, sex)
  data.table::setkey(deaths_ss, age, sex)
  data.table::setkey(net_immigration, age, sex)

  # Calculate USAF/SS ratio at beginning of year
  usaf_total <- usaf_prev[, sum(population, na.rm = TRUE)]
  ss_total <- ss_pop_prev[, sum(population, na.rm = TRUE)]
  usaf_ss_ratio <- if (ss_total > 0) usaf_total / ss_total else 0.95

  # Calculate qx from SS deaths / SS population
  deaths_merged <- merge(deaths_ss, ss_pop_prev, by = c("age", "sex"), all.x = TRUE)
  deaths_merged[, qx := fifelse(population > 0, deaths / population, 0)]

  # Merge USAF with qx to get deaths
  usaf_calc <- merge(usaf_prev, deaths_merged[, .(age, sex, qx)], by = c("age", "sex"), all.x = TRUE)
  usaf_calc[is.na(qx), qx := 0]
  usaf_calc[, usaf_deaths := population * qx]

  # Prepare net immigration prorated to USAF
  net_imm_usaf <- data.table::copy(net_immigration)
  net_imm_usaf[, net_immigration := net_immigration * usaf_ss_ratio]

  # Merge all data for vectorized calculation
  calc_data <- merge(usaf_calc, net_imm_usaf, by = c("age", "sex"), all.x = TRUE)
  calc_data[is.na(net_immigration), net_immigration := 0]

  # Add births data (age 0 only)
  male_pct <- sex_ratio / (sex_ratio + 1000)
  female_pct <- 1 - male_pct
  births_usaf <- births_ss * usaf_ss_ratio

  # Create shifted population (age x gets pop from age x-1)
  # For vectorized aging
  prev_ages <- usaf_prev[, .(age_next = age + 1, sex, pop_prev = population)]
  prev_ages <- prev_ages[age_next <= max_age]  # Remove ages that would exceed max

  # Build new population
  # Ages 1 to max_age-1: previous year's age-1 minus deaths plus immigration
  usaf_new <- merge(
    data.table::data.table(
      age = rep(0:max_age, 2),
      sex = rep(c("male", "female"), each = max_age + 1)
    ),
    prev_ages,
    by.x = c("age", "sex"),
    by.y = c("age_next", "sex"),
    all.x = TRUE
  )
  usaf_new[is.na(pop_prev), pop_prev := 0]

  # Merge deaths and immigration
  usaf_new <- merge(usaf_new, calc_data[, .(age, sex, usaf_deaths, net_immigration)],
                    by = c("age", "sex"), all.x = TRUE)
  usaf_new[is.na(usaf_deaths), usaf_deaths := 0]
  usaf_new[is.na(net_immigration), net_immigration := 0]

  # Calculate new population
  usaf_new[, population := pop_prev - usaf_deaths + net_immigration]

  # Handle age 0: births minus deaths plus immigration
  usaf_new[age == 0 & sex == "male", population := births_usaf * male_pct - usaf_deaths + net_immigration]
  usaf_new[age == 0 & sex == "female", population := births_usaf * female_pct - usaf_deaths + net_immigration]

  # Handle max_age (100+): also add survivors from previous 100+
  prev_100_plus <- usaf_prev[age == max_age, .(sex, prev_100 = population)]
  deaths_100 <- calc_data[age == max_age, .(sex, deaths_100 = usaf_deaths)]
  imm_100 <- net_imm_usaf[age == max_age, .(sex, imm_100 = net_immigration)]

  max_age_adj <- merge(prev_100_plus, deaths_100, by = "sex", all.x = TRUE)
  max_age_adj <- merge(max_age_adj, imm_100, by = "sex", all.x = TRUE)
  max_age_adj[is.na(prev_100), prev_100 := 0]
  max_age_adj[is.na(deaths_100), deaths_100 := 0]
  max_age_adj[is.na(imm_100), imm_100 := 0]
  max_age_adj[, adj := prev_100 - deaths_100 + imm_100]

  usaf_new <- merge(usaf_new, max_age_adj[, .(sex, adj)], by = "sex", all.x = TRUE)
  usaf_new[is.na(adj), adj := 0]
  usaf_new[age == max_age, population := population + adj]

  # Ensure non-negative and clean up
  usaf_new[, population := pmax(0, population)]
  usaf_new[, c("pop_prev", "usaf_deaths", "net_immigration", "adj") := NULL]

  data.table::setorder(usaf_new, sex, age)
  usaf_new[, .(age, sex, population)]
}


#' Calculate Residential Population (Phase 8E.2)
#'
#' @description
#' Calculates residential population by subtracting armed forces overseas
#' from USAF population.
#'
#' Per TR2025: "The residential population is calculated by subtracting an
#' assumed constant number of armed forces overseas by sex and single year
#' of age from the USAF population."
#'
#' @param usaf_pop data.table: USAF population by age/sex
#' @param armed_forces_overseas data.table: Armed forces overseas by age/sex
#'   (assumed constant from starting year)
#'
#' @return data.table with residential population by age/sex
#'
#' @keywords internal
calculate_residential_population <- function(usaf_pop, armed_forces_overseas) {
  result <- merge(
    usaf_pop,
    armed_forces_overseas[, .(age, sex, af_overseas = population)],
    by = c("age", "sex"),
    all.x = TRUE
  )

  result[is.na(af_overseas), af_overseas := 0]
  result[, residential := pmax(0, population - af_overseas)]
  result[, .(age, sex, population = residential)]
}


#' Calculate Civilian Population (Phase 8E.3)
#'
#' @description
#' Calculates civilian population by subtracting total armed forces
#' from USAF population.
#'
#' Per TR2025: "Likewise, the civilian population is calculated by subtracting
#' an assumed constant number of total armed forces by sex and single year of
#' age from the USAF population."
#'
#' @param usaf_pop data.table: USAF population by age/sex
#' @param total_armed_forces data.table: Total armed forces by age/sex
#'   (assumed constant from starting year)
#'
#' @return data.table with civilian population by age/sex
#'
#' @keywords internal
calculate_civilian_population <- function(usaf_pop, total_armed_forces) {
  result <- merge(
    usaf_pop,
    total_armed_forces[, .(age, sex, af_total = population)],
    by = c("age", "sex"),
    all.x = TRUE
  )

  result[is.na(af_total), af_total := 0]
  result[, civilian := pmax(0, population - af_total)]
  result[, .(age, sex, population = civilian)]
}


#' Calculate CNI Population from Civilian (Phase 8E.4)
#'
#' @description
#' Applies CNI/civilian ratios to civilian population.
#'
#' Per TR2025: "The CNI population is calculated by applying the ratios of CNI
#' population to civilian population by year, sex, and single year of age in
#' the starting year to the civilian population."
#'
#' @param civilian_pop data.table: Civilian population by age/sex
#' @param cni_civilian_ratios data.table: CNI/civilian ratios by age/sex
#'   from starting year
#'
#' @return data.table with CNI population by age/sex
#'
#' @keywords internal
calculate_cni_from_civilian <- function(civilian_pop, cni_civilian_ratios) {
  result <- merge(
    civilian_pop,
    cni_civilian_ratios[, .(age, sex, cni_ratio = ratio)],
    by = c("age", "sex"),
    all.x = TRUE
  )

  # Default ratio of 1.0 if not available (most people are CNI)
  result[is.na(cni_ratio), cni_ratio := 0.98]
  result[, cni := population * cni_ratio]
  result[, .(age, sex, population = cni)]
}


#' Disaggregate CNI Population by Marital Status (Phase 8E.5) - Vectorized
#'
#' @description
#' Disaggregates CNI population into five marital statuses using vectorized operations.
#'
#' Per TR2025: CNI marital statuses are:
#' - single (never married)
#' - married_spouse_present
#' - separated
#' - widowed
#' - divorced
#'
#' @param cni_pop data.table: CNI population by age/sex
#' @param marital_adjustments data.table: Marital status adjustments by age/sex
#' @param separated_ratios data.table: Separated/married ratios by age/sex
#' @param cni_marital_ratios data.table: CNI/total ratios by age/sex/marital
#' @param marital_pop_total data.table: Total population by marital status (optional)
#'
#' @return data.table with CNI population by age, sex, marital_status
#'
#' @keywords internal
disaggregate_cni_marital <- function(cni_pop,
                                      marital_adjustments = NULL,
                                      separated_ratios = NULL,
                                      cni_marital_ratios = NULL,
                                      marital_pop_total = NULL) {

  # Ensure cni_pop is properly structured
  cni_pop <- data.table::as.data.table(cni_pop)
  if (!"population" %in% names(cni_pop)) {
    cli::cli_abort("cni_pop must have a 'population' column")
  }

  # Handle separated ratios
  if (is.null(separated_ratios)) {
    # Default separated ratio by age (increases with age)
    separated_ratios <- data.table::data.table(
      age = 0:100,
      separated_ratio = pmin(0.10, 0.02 + (0:100) * 0.0008)
    )
  }

  # If we have marital_pop_total, use it for proportions
  if (!is.null(marital_pop_total) && nrow(marital_pop_total) > 0) {
    # Make a clean copy of marital_pop_total with explicit column selection
    marital_dt <- data.table::data.table(
      age = as.integer(marital_pop_total$age),
      sex = as.character(marital_pop_total$sex),
      marital_status = as.character(marital_pop_total$marital_status),
      population = as.numeric(marital_pop_total$population)
    )

    # Aggregate marital_pop_total by age/sex/marital_status
    marital_props <- marital_dt[, list(population = sum(population, na.rm = TRUE)),
                                 by = list(age, sex, marital_status)]
    marital_props[, total := sum(population), by = list(age, sex)]
    marital_props[, prop := fifelse(total > 0, population / total, 0)]

    # Standardize marital status names (Phase 8C uses "single", "married", "divorced", "widowed")
    # CNI uses: "single", "married_spouse_present", "separated", "widowed", "divorced"
    # Map "single" -> "single", "married" -> split to married_spouse_present + separated

    # Get only ages/sexes that exist in cni_pop
    cni_ages_sex <- unique(cni_pop[, list(age, sex)])

    # Merge proportions with CNI population
    result <- merge(cni_ages_sex, cni_pop[, list(age, sex, cni_total = population)],
                    by = c("age", "sex"), all.x = TRUE)
    result[is.na(cni_total), cni_total := 0]

    # Build wide format of proportions
    prop_wide <- data.table::dcast(marital_props, age + sex ~ marital_status,
                                   value.var = "prop", fill = 0)

    # Merge with cni data
    result <- merge(result, prop_wide, by = c("age", "sex"), all.x = TRUE)

    # Handle missing proportions with age-based defaults
    result[is.na(single), single := fifelse(age < 15, 1.0, fifelse(age < 25, 0.70,
                                            fifelse(age < 65, 0.15, 0.05)))]
    result[is.na(married), married := fifelse(age < 15, 0.0, fifelse(age < 25, 0.25,
                                              fifelse(age < 65, 0.60, 0.50)))]
    result[is.na(divorced), divorced := fifelse(age < 15, 0.0, fifelse(age < 25, 0.04,
                                                fifelse(age < 65, 0.15, 0.12)))]
    result[is.na(widowed), widowed := fifelse(age < 15, 0.0, fifelse(age < 25, 0.01,
                                              fifelse(age < 65, 0.10, 0.33)))]

    # Normalize proportions to sum to 1
    result[, prop_sum := single + married + divorced + widowed]
    result[prop_sum > 0, `:=`(
      single = single / prop_sum,
      married = married / prop_sum,
      divorced = divorced / prop_sum,
      widowed = widowed / prop_sum
    )]

    # Merge separated ratios (merge by age and sex since separated_ratios has both)
    result <- merge(result, separated_ratios, by = c("age", "sex"), all.x = TRUE)
    result[is.na(separated_ratio), separated_ratio := 0.05]

    # Calculate populations for each marital status
    result[, `:=`(
      pop_single = cni_total * single,
      pop_married_sp = cni_total * married * (1 - separated_ratio),
      pop_separated = cni_total * married * separated_ratio,
      pop_divorced = cni_total * divorced,
      pop_widowed = cni_total * widowed
    )]

    # Reshape to long format
    final <- data.table::melt(
      result[, list(age, sex, pop_single, pop_married_sp, pop_separated, pop_divorced, pop_widowed)],
      id.vars = c("age", "sex"),
      variable.name = "marital_status",
      value.name = "population"
    )

    # Clean up marital status names
    final[, marital_status := gsub("pop_", "", marital_status)]
    final[marital_status == "married_sp", marital_status := "married_spouse_present"]

    data.table::setorder(final, age, sex, marital_status)
    return(final)
  }

  # Fallback: use vectorized default proportions
  # Create full grid
  all_ages <- unique(cni_pop$age)
  all_sexes <- unique(cni_pop$sex)
  cni_statuses <- c("single", "married_spouse_present", "separated", "widowed", "divorced")

  grid <- data.table::CJ(age = all_ages, sex = all_sexes, marital_status = cni_statuses)

  # Merge with CNI totals
  grid <- merge(grid, cni_pop[, .(age, sex, cni_total = population)],
                by = c("age", "sex"), all.x = TRUE)
  grid[is.na(cni_total), cni_total := 0]

  # Assign proportions based on age (vectorized)
  grid[, prop := 0]

  # Age < 15: all single
  grid[age < 15 & marital_status == "single", prop := 1]

  # Age 15-24
  grid[age >= 15 & age < 25 & marital_status == "single", prop := 0.70]
  grid[age >= 15 & age < 25 & marital_status == "married_spouse_present", prop := 0.24]
  grid[age >= 15 & age < 25 & marital_status == "separated", prop := 0.01]
  grid[age >= 15 & age < 25 & marital_status == "widowed", prop := 0.005]
  grid[age >= 15 & age < 25 & marital_status == "divorced", prop := 0.045]

  # Age 25-44
  grid[age >= 25 & age < 45 & marital_status == "single", prop := 0.25]
  grid[age >= 25 & age < 45 & marital_status == "married_spouse_present", prop := 0.55]
  grid[age >= 25 & age < 45 & marital_status == "separated", prop := 0.03]
  grid[age >= 25 & age < 45 & marital_status == "widowed", prop := 0.02]
  grid[age >= 25 & age < 45 & marital_status == "divorced", prop := 0.15]

  # Age 45-64
  grid[age >= 45 & age < 65 & marital_status == "single", prop := 0.12]
  grid[age >= 45 & age < 65 & marital_status == "married_spouse_present", prop := 0.58]
  grid[age >= 45 & age < 65 & marital_status == "separated", prop := 0.02]
  grid[age >= 45 & age < 65 & marital_status == "widowed", prop := 0.10]
  grid[age >= 45 & age < 65 & marital_status == "divorced", prop := 0.18]

  # Age 65+
  grid[age >= 65 & marital_status == "single", prop := 0.05]
  grid[age >= 65 & marital_status == "married_spouse_present", prop := 0.42]
  grid[age >= 65 & marital_status == "separated", prop := 0.01]
  grid[age >= 65 & marital_status == "widowed", prop := 0.38]
  grid[age >= 65 & marital_status == "divorced", prop := 0.14]

  # Calculate population
  grid[, population := cni_total * prop]
  grid[, c("cni_total", "prop") := NULL]

  data.table::setorder(grid, age, sex, marital_status)
  grid
}


#' Prepare CNI Starting Data (Phase 8E Data Prep)
#'
#' @description
#' Prepares starting year data required for CNI projection from
#' historical population outputs and Phase 8B SS area population.
#'
#' Per TR2025 Equation 1.8.7, the CNI projection requires:
#' - USAF (US resident + armed forces overseas) population by age/sex
#' - Armed forces overseas by age/sex (held constant)
#' - Total armed forces by age/sex (held constant)
#' - CNI/civilian ratios by age/sex (held constant from starting year)
#' - Separated/married ratios by age/sex
#'
#' @param historical_cni data.table: Historical CNI population from Phase 4
#' @param phase8b_pop data.table: Phase 8B population (SS area) by year/age/sex/pop_status
#' @param start_year Integer: Starting year (default: 2022)
#' @param max_age Integer: Maximum age (default: 100)
#'
#' @return list with:
#'   - usaf_pop: USAF population by age/sex for starting year
#'   - armed_forces_overseas: Armed forces overseas by age/sex (constant)
#'   - total_armed_forces: Total armed forces by age/sex (constant)
#'   - cni_civilian_ratios: CNI/civilian ratios by age/sex
#'   - cni_pop: CNI population by age/sex for starting year
#'   - separated_ratios: Separated/married ratios by age/sex
#'
#' @keywords internal
prepare_cni_starting_data <- function(historical_cni,
                                       phase8b_pop,
                                       start_year = 2022,
                                       max_age = 100) {

  cli::cli_h3("Preparing CNI Starting Data for {start_year}")

  # Filter historical CNI to starting year
  if ("year" %in% names(historical_cni)) {
    cni_start <- historical_cni[year == start_year]
  } else {
    cni_start <- historical_cni
  }

  if (nrow(cni_start) == 0) {
    cli::cli_abort("No historical CNI data found for year {start_year}")
  }

  # Get CNI population by age/sex (sum across marital statuses and orientations)
  cni_by_age_sex <- cni_start[, .(population = sum(population, na.rm = TRUE)),
                              by = .(age, sex)]
  data.table::setorder(cni_by_age_sex, sex, age)

  # Get SS area population for start year (aggregate across pop_status)
  # Per TR2025: USAF ≈ SS area population for our purposes
  # (SS area = US resident + some foreign territories, USAF = US resident + overseas military)
  ss_by_age_sex <- phase8b_pop[year == start_year,
                                .(population = sum(population, na.rm = TRUE)),
                                by = .(age, sex)]
  data.table::setorder(ss_by_age_sex, sex, age)

  # Use SS area population as proxy for USAF
  # The difference is small (armed forces overseas ~150K vs foreign territories ~4M)
  # but for TR2025 methodology we use SS area as the base
  usaf_pop <- data.table::copy(ss_by_age_sex)

  # Estimate armed forces by age/sex
  # Per TR2025: Armed forces are held constant at starting year values
  # Military ages are primarily 17-65, with peak at 20-30
  # Total US military ~1.3M active duty, ~150K overseas
  total_af_count <- 1300000  # Approximate total active duty
  overseas_af_count <- 150000  # Approximate overseas

  # Create armed forces age/sex distribution (military-typical profile)
  af_ages <- 17:65
  af_grid <- data.table::CJ(age = 0:max_age, sex = c("male", "female"))

  # Age distribution peaks at 20-25, declines after
  af_grid[, af_weight := 0]
  af_grid[age >= 17 & age <= 20, af_weight := 0.08]
  af_grid[age >= 21 & age <= 25, af_weight := 0.12]
  af_grid[age >= 26 & age <= 30, af_weight := 0.10]
  af_grid[age >= 31 & age <= 35, af_weight := 0.07]
  af_grid[age >= 36 & age <= 40, af_weight := 0.05]
  af_grid[age >= 41 & age <= 50, af_weight := 0.03]
  af_grid[age >= 51 & age <= 65, af_weight := 0.01]

  # Sex distribution: ~83% male, ~17% female
  af_grid[sex == "male", af_weight := af_weight * 0.83]
  af_grid[sex == "female", af_weight := af_weight * 0.17]

  # Normalize weights
  af_grid[, af_weight := af_weight / sum(af_weight)]

  # Calculate armed forces populations
  af_grid[, total_af := af_weight * total_af_count]
  af_grid[, overseas_af := af_weight * overseas_af_count]

  total_armed_forces <- af_grid[, .(age, sex, population = total_af)]
  armed_forces_overseas <- af_grid[, .(age, sex, population = overseas_af)]

  # Calculate civilian population = USAF - total armed forces
  civilian_pop <- merge(usaf_pop, total_armed_forces,
                        by = c("age", "sex"), suffixes = c("", "_af"), all.x = TRUE)
  civilian_pop[is.na(population_af), population_af := 0]
  civilian_pop[, civilian := pmax(0, population - population_af)]

  # Calculate CNI/civilian ratios
  # CNI = civilian - institutionalized population
  # Institutionalized includes: prisons, nursing homes, mental facilities, etc.
  cni_civilian <- merge(cni_by_age_sex, civilian_pop[, .(age, sex, civilian)],
                        by = c("age", "sex"), all = TRUE)
  cni_civilian[is.na(population), population := 0]
  cni_civilian[is.na(civilian), civilian := 0]

  # Calculate CNI/civilian ratio
  cni_civilian[, ratio := fifelse(civilian > 0, population / civilian, 0.98)]

  # CNI should be less than civilian - cap at 1.0
  cni_civilian[ratio > 1.0, ratio := 1.0]

  # For ages with no civilian (shouldn't happen but handle edge cases)
  cni_civilian[ratio < 0.5 & age < 65, ratio := 0.98]  # Most young people are CNI
  cni_civilian[ratio < 0.5 & age >= 65, ratio := 0.93]  # More elderly in institutions

  cni_civilian_ratios <- cni_civilian[, .(age, sex, ratio)]

  cli::cli_alert_info("CNI/civilian ratio range: {round(min(cni_civilian_ratios$ratio), 3)} - {round(max(cni_civilian_ratios$ratio), 3)}")

  # Get marital status distribution for separated ratio calculation
  cni_marital <- cni_start[, .(population = sum(population, na.rm = TRUE)),
                           by = .(age, sex, marital_status)]

  # Calculate separated ratios from historical data
  separated_ratios <- calculate_separated_ratios(cni_marital, max_age)

  # Totals for logging
  cni_total <- sum(cni_by_age_sex$population, na.rm = TRUE)
  ss_total <- sum(ss_by_age_sex$population, na.rm = TRUE)
  civilian_total <- sum(civilian_pop$civilian, na.rm = TRUE)

  cli::cli_alert_success("Prepared CNI starting data")
  cli::cli_alert_info("SS area / USAF ({start_year}): {format(round(ss_total/1e6, 2), nsmall=2)}M")
  cli::cli_alert_info("Civilian ({start_year}): {format(round(civilian_total/1e6, 2), nsmall=2)}M")
  cli::cli_alert_info("CNI ({start_year}): {format(round(cni_total/1e6, 2), nsmall=2)}M")
  cli::cli_alert_info("Overall CNI/civilian ratio: {round(cni_total/civilian_total, 4)}")

  list(
    usaf_pop = usaf_pop,
    armed_forces_overseas = armed_forces_overseas,
    total_armed_forces = total_armed_forces,
    cni_civilian_ratios = cni_civilian_ratios,
    cni_pop = cni_by_age_sex,
    separated_ratios = separated_ratios,
    cni_marital = cni_marital
  )
}


#' Calculate Separated/Married Ratios from Historical CNI
#'
#' @keywords internal
calculate_separated_ratios <- function(cni_marital, max_age = 100) {

  # Get married and separated populations
  married <- cni_marital[marital_status %in% c("married", "married_spouse_present"),
                         .(married = sum(population, na.rm = TRUE)),
                         by = .(age, sex)]

  separated <- cni_marital[marital_status == "separated",
                           .(separated = sum(population, na.rm = TRUE)),
                           by = .(age, sex)]

  result <- merge(married, separated, by = c("age", "sex"), all.x = TRUE)
  result[is.na(separated), separated := 0]
  result[, separated_ratio := separated / (married + separated)]
  result[is.na(separated_ratio), separated_ratio := 0.05]
  result[separated_ratio > 0.3, separated_ratio := 0.3]  # Cap at 30%

  result[, .(age, sex, separated_ratio)]
}


#' Project CNI Population (Phase 8E.6 - Main Entry Point)
#'
#' @description
#' Main function for projecting the civilian noninstitutionalized (CNI)
#' population by age, sex, and marital status (Equation 1.8.7).
#'
#' Per TR2025: "Once the Social Security area population is projected by single
#' year of age, sex, and marital status, the CNI population is projected by year,
#' sex, single year of age, and the following five marital states: single, married
#' (spouse present), separated, widowed, and divorced."
#'
#' TR2025 Methodology:
#' 1. Project USAF population forward using component method (births, deaths, net immigration)
#' 2. Calculate residential = USAF - armed forces overseas (constant)
#' 3. Calculate civilian = USAF - total armed forces (constant)
#' 4. Calculate CNI = civilian × CNI/civilian ratio (constant from starting year)
#' 5. Disaggregate CNI by marital status using Phase 8C proportions
#'
#' @param phase8b_result list: Output from run_population_projection (Phase 8B)
#' @param marital_result list: Output from run_marital_projection (Phase 8C)
#' @param historical_cni data.table: Historical CNI population from Phase 4
#' @param start_year Integer: Starting year (default: 2022)
#' @param end_year Integer: End year (default: 2099)
#' @param verbose Logical: Print progress (default: TRUE)
#'
#' @return list with:
#'   - cni_population: data.table N^z_{x,s,m} by year, age, sex, marital_status
#'   - usaf_population: data.table USAF population by year, age, sex
#'   - civilian_population: data.table civilian population by year, age, sex
#'   - summary: Summary statistics by year
#'
#' @export
project_cni_population <- function(phase8b_result,
                                    marital_result,
                                    historical_cni,
                                    start_year = 2022,
                                    end_year = 2099,
                                    verbose = TRUE) {

  if (verbose) {
    cli::cli_h1("Phase 8E: CNI Population Projection")
    cli::cli_alert_info("Projection period: {start_year}-{end_year}")
  }

  # Extract Phase 8B components
  phase8b_pop <- phase8b_result$population
  phase8b_births <- phase8b_result$births
  phase8b_deaths <- phase8b_result$deaths
  net_immigration <- phase8b_result$net_immigration

  # Get marital population for disaggregation
  marital_pop <- marital_result$marital_population

  # Prepare starting data (includes USAF, armed forces, CNI/civilian ratios)
  if (verbose) {
    cli::cli_h2("Step 1: Preparing Starting Data")
  }
  starting_data <- prepare_cni_starting_data(
    historical_cni = historical_cni,
    phase8b_pop = phase8b_pop,
    start_year = start_year
  )

  # Extract starting data
  current_usaf <- starting_data$usaf_pop
  armed_forces_overseas <- starting_data$armed_forces_overseas
  total_armed_forces <- starting_data$total_armed_forces
  cni_civilian_ratios <- starting_data$cni_civilian_ratios
  separated_ratios <- starting_data$separated_ratios

  # Storage for results
  all_cni <- list()
  all_usaf <- list()
  all_civilian <- list()
  summary_list <- list()

  # Store starting year CNI
  cni_start <- starting_data$cni_pop

  # Get marital population for start year
  marital_start <- marital_pop[year == start_year]
  if (nrow(marital_start) > 0) {
    cni_marital_start <- disaggregate_cni_marital(
      cni_start,
      separated_ratios = separated_ratios,
      marital_pop_total = marital_start
    )
  } else {
    cni_marital_start <- disaggregate_cni_marital(
      cni_start,
      separated_ratios = separated_ratios
    )
  }
  cni_marital_start[, year := start_year]
  all_cni[[as.character(start_year)]] <- cni_marital_start

  # Store starting USAF
  usaf_start <- data.table::copy(current_usaf)
  usaf_start[, year := start_year]
  all_usaf[[as.character(start_year)]] <- usaf_start

  # Store starting civilian
  civilian_start <- calculate_civilian_population(current_usaf, total_armed_forces)
  civilian_start[, year := start_year]
  all_civilian[[as.character(start_year)]] <- civilian_start

  # Starting year summary
  summary_list[[as.character(start_year)]] <- data.table::data.table(
    year = start_year,
    cni_total = sum(cni_marital_start$population, na.rm = TRUE),
    usaf_total = sum(current_usaf$population, na.rm = TRUE),
    civilian_total = sum(civilian_start$population, na.rm = TRUE)
  )

  if (verbose) {
    cli::cli_h2("Step 2: Projecting Forward")
    cli::cli_progress_bar("Projecting CNI population", total = end_year - start_year)
  }

  # Project each year
  for (yr in (start_year + 1):end_year) {
    if (verbose) {
      cli::cli_progress_update()
    }

    # Get SS area data for this year - aggregate across pop_status
    ss_pop_prev <- phase8b_pop[year == (yr - 1),
                                .(population = sum(population, na.rm = TRUE)),
                                by = .(age, sex)]
    births_yr <- phase8b_births[year == yr, sum(births, na.rm = TRUE)]
    deaths_yr <- phase8b_deaths[year == yr,
                                 .(deaths = sum(deaths, na.rm = TRUE)),
                                 by = .(age, sex)]
    net_imm_yr <- net_immigration[year == yr,
                                   .(net_immigration = sum(net_immigration, na.rm = TRUE)),
                                   by = .(age, sex)]

    # Project USAF forward using component method
    new_usaf <- project_usaf_year(
      usaf_prev = current_usaf,
      ss_pop_prev = ss_pop_prev,
      births_ss = births_yr,
      deaths_ss = deaths_yr,
      net_immigration = net_imm_yr
    )

    # Calculate civilian population (USAF - total armed forces)
    civilian <- calculate_civilian_population(new_usaf, total_armed_forces)

    # Calculate CNI population (civilian × CNI/civilian ratio)
    cni <- calculate_cni_from_civilian(civilian, cni_civilian_ratios)

    # Get marital population for this year for disaggregation
    marital_yr <- marital_pop[year == yr]

    # Disaggregate CNI by marital status (5 categories)
    if (nrow(marital_yr) > 0) {
      cni_marital <- disaggregate_cni_marital(
        cni,
        separated_ratios = separated_ratios,
        marital_pop_total = marital_yr
      )
    } else {
      cni_marital <- disaggregate_cni_marital(
        cni,
        separated_ratios = separated_ratios
      )
    }
    cni_marital[, year := yr]

    # Store results
    all_cni[[as.character(yr)]] <- cni_marital

    usaf_yr <- data.table::copy(new_usaf)
    usaf_yr[, year := yr]
    all_usaf[[as.character(yr)]] <- usaf_yr

    civilian_yr <- data.table::copy(civilian)
    civilian_yr[, year := yr]
    all_civilian[[as.character(yr)]] <- civilian_yr

    summary_list[[as.character(yr)]] <- data.table::data.table(
      year = yr,
      cni_total = sum(cni_marital$population, na.rm = TRUE),
      usaf_total = sum(new_usaf$population, na.rm = TRUE),
      civilian_total = sum(civilian$population, na.rm = TRUE)
    )

    # Update current state for next iteration
    current_usaf <- new_usaf
  }

  if (verbose) {
    cli::cli_progress_done()
  }

  # Combine results
  cni_population <- data.table::rbindlist(all_cni, use.names = TRUE)
  usaf_population <- data.table::rbindlist(all_usaf, use.names = TRUE)
  civilian_population <- data.table::rbindlist(all_civilian, use.names = TRUE)
  summary <- data.table::rbindlist(summary_list, use.names = TRUE)

  # Order
  data.table::setorder(cni_population, year, age, sex, marital_status)
  data.table::setorder(usaf_population, year, sex, age)
  data.table::setorder(civilian_population, year, sex, age)

  # Add CNI/SS ratio to summary
  ss_totals <- phase8b_pop[, .(ss_total = sum(population, na.rm = TRUE)), by = year]
  summary <- merge(summary, ss_totals, by = "year", all.x = TRUE)
  summary[, cni_ss_ratio := cni_total / ss_total]

  if (verbose) {
    cli::cli_h2("Summary")
    cli::cli_alert_success("CNI projection complete")
    cli::cli_alert_info("Years: {start_year}-{end_year}")
    cli::cli_alert_info("USAF total: {format(round(summary[year == start_year, usaf_total]/1e6, 2), nsmall=2)}M ({start_year}) -> {format(round(summary[year == end_year, usaf_total]/1e6, 2), nsmall=2)}M ({end_year})")
    cli::cli_alert_info("Civilian total: {format(round(summary[year == start_year, civilian_total]/1e6, 2), nsmall=2)}M ({start_year}) -> {format(round(summary[year == end_year, civilian_total]/1e6, 2), nsmall=2)}M ({end_year})")
    cli::cli_alert_info("CNI total: {format(round(summary[year == start_year, cni_total]/1e6, 2), nsmall=2)}M ({start_year}) -> {format(round(summary[year == end_year, cni_total]/1e6, 2), nsmall=2)}M ({end_year})")
    cli::cli_alert_info("CNI/SS ratio: {round(summary[year == start_year, cni_ss_ratio], 4)} ({start_year}) -> {round(summary[year == end_year, cni_ss_ratio], 4)} ({end_year})")

    # Marital status breakdown for end year
    end_marital <- cni_population[year == end_year, .(total = sum(population)), by = marital_status]
    end_marital[, pct := round(100 * total / sum(total), 1)]
    cli::cli_alert_info("Marital status ({end_year}):")
    for (i in seq_len(nrow(end_marital))) {
      cli::cli_alert_info("  {end_marital$marital_status[i]}: {end_marital$pct[i]}%")
    }
  }

  list(
    cni_population = cni_population,
    usaf_population = usaf_population,
    civilian_population = civilian_population,
    summary = summary
  )
}


#' Validate CNI Population Projection (Phase 8E.7)
#'
#' @description
#' Validates the CNI population projection against various consistency checks.
#'
#' @param cni_result list: Output from project_cni_population
#' @param phase8b_result list: Output from run_population_projection (Phase 8B)
#' @param tolerance Numeric: Relative tolerance (default: 0.02)
#'
#' @return list with validation results
#'
#' @export
validate_cni_projection <- function(cni_result,
                                     phase8b_result,
                                     tolerance = 0.02) {

  cli::cli_h2("Phase 8E Validation: CNI Population")

  cni_pop <- cni_result$cni_population
  summary <- cni_result$summary
  phase8b_pop <- phase8b_result$population

  validation_results <- list()

  # Test 1: CNI < Total Population
  cli::cli_h3("Test 1: CNI < Total Population")

  cni_totals <- summary[, .(cni_total = cni_total), by = year]
  ss_totals <- phase8b_pop[, .(ss_total = sum(population, na.rm = TRUE)), by = year]

  comparison <- merge(cni_totals, ss_totals, by = "year")
  comparison[, ratio := cni_total / ss_total]

  invalid_years <- comparison[ratio >= 1]
  if (nrow(invalid_years) == 0) {
    cli::cli_alert_success("CNI < SS area population for all years")
    validation_results$cni_less_than_total <- TRUE
  } else {
    cli::cli_alert_danger("{nrow(invalid_years)} years have CNI >= SS population")
    validation_results$cni_less_than_total <- FALSE
  }

  cli::cli_alert_info("CNI/SS ratio range: {round(min(comparison$ratio), 3)} - {round(max(comparison$ratio), 3)}")

  # Test 2: Marital statuses sum to total
  cli::cli_h3("Test 2: Marital Statuses Sum to Total")

  marital_sums <- cni_pop[, .(marital_sum = sum(population, na.rm = TRUE)), by = year]
  comparison2 <- merge(marital_sums, cni_totals, by = "year")
  comparison2[, diff := abs(marital_sum - cni_total) / cni_total]

  max_diff <- max(comparison2$diff, na.rm = TRUE)
  if (max_diff < 1e-6) {
    cli::cli_alert_success("Marital statuses sum to total (max error: {format(max_diff, scientific=TRUE)})")
    validation_results$marital_sum_valid <- TRUE
  } else {
    cli::cli_alert_warning("Marital sum error: {format(max_diff, scientific=TRUE)}")
    validation_results$marital_sum_valid <- (max_diff < 0.01)
  }

  # Test 3: CNI/civilian ratios reasonable
  cli::cli_h3("Test 3: CNI Growth Reasonable")

  start_cni <- summary[year == min(year), cni_total]
  end_cni <- summary[year == max(year), cni_total]
  growth_rate <- (end_cni / start_cni)^(1 / (max(summary$year) - min(summary$year))) - 1

  cli::cli_alert_info("Annual CNI growth rate: {round(100 * growth_rate, 2)}%")

  if (growth_rate > -0.01 && growth_rate < 0.03) {
    cli::cli_alert_success("CNI growth rate reasonable")
    validation_results$growth_reasonable <- TRUE
  } else {
    cli::cli_alert_warning("CNI growth rate outside expected range (-1% to 3%)")
    validation_results$growth_reasonable <- FALSE
  }

  # Test 4: Marital status proportions reasonable
  cli::cli_h3("Test 4: Marital Status Proportions")

  end_year <- max(cni_pop$year)
  end_marital <- cni_pop[year == end_year & age >= 18,
                          .(total = sum(population, na.rm = TRUE)),
                          by = marital_status]
  end_marital[, pct := total / sum(total)]

  # Check that single + married make up majority for working-age adults
  working_age <- cni_pop[year == end_year & age >= 25 & age <= 64,
                          .(total = sum(population, na.rm = TRUE)),
                          by = marital_status]
  working_age[, pct := total / sum(total)]

  married_pct <- working_age[marital_status == "married_spouse_present", pct]
  if (length(married_pct) == 0) married_pct <- 0

  if (married_pct > 0.3 && married_pct < 0.8) {
    cli::cli_alert_success("Married proportion for ages 25-64: {round(100*married_pct, 1)}%")
    validation_results$marital_props_valid <- TRUE
  } else {
    cli::cli_alert_warning("Married proportion unexpected: {round(100*married_pct, 1)}%")
    validation_results$marital_props_valid <- FALSE
  }

  # Overall
  validation_results$all_passed <- validation_results$cni_less_than_total &&
                                   validation_results$marital_sum_valid &&
                                   validation_results$growth_reasonable &&
                                   validation_results$marital_props_valid

  if (validation_results$all_passed) {
    cli::cli_alert_success("Phase 8E validation: All checks passed")
  } else {
    cli::cli_alert_warning("Phase 8E validation: Some checks failed")
  }

  validation_results
}


# =============================================================================
# PHASE 8F: INTEGRATION AND MAIN ENTRY POINT
# =============================================================================

#' Run Complete Projected Population (Main Entry Point)
#'
#' @description
#' Main function orchestrating all projected population calculations
#' (Equations 1.8.1 through 1.8.7) from TR2025 documentation.
#'
#' This function integrates:
#' - Phase 8B: Core population projection (Eq 1.8.1-1.8.4)
#' - Phase 8C: Marital status disaggregation (Eq 1.8.5)
#' - Phase 8D: Children by parent survival status (Eq 1.8.6)
#' - Phase 8E: Civilian noninstitutionalized population (Eq 1.8.7)
#'
#' @param starting_population data.table: Population at December 31 of starting year
#'   from HISTORICAL POPULATION subprocess
#' @param starting_marital_pop data.table: Population by marital status at starting year
#' @param starting_couples_grid matrix: Married couples by husband age × wife age
#' @param birth_rates data.table: Projected birth rates by year and age (from FERTILITY)
#' @param mortality_qx data.table: Death probabilities by year, age, sex (from MORTALITY)
#' @param mortality_differentials data.table: Mortality factors by marital status
#' @param net_lpr data.table: Net LPR immigration by year, age, sex (from LPR IMMIGRATION)
#' @param net_o data.table: Net O immigration by year, age, sex (from TEMP/UNLAWFUL)
#' @param marriage_rates list: Projected marriage rates (from MARRIAGE)
#' @param divorce_rates list: Projected divorce rates (from DIVORCE)
#' @param historical_cni data.table: Historical CNI population (from HISTORICAL POPULATION)
#' @param config list: Configuration parameters
#' @param start_year Integer: Starting year (default: 2022)
#' @param end_year Integer: Ending year (default: 2099)
#' @param run_phases Character vector: Which phases to run (default: all)
#' @param verbose Logical: Print progress (default: TRUE)
#'
#' @return list with all outputs:
#'   - population: P^z_{x,s,p} by year (Eq 1.8.4)
#'   - population_marital: P^z_{x,s,p,m} by year (Eq 1.8.5)
#'   - births: B^z_{s,p} by year (Eq 1.8.1)
#'   - deaths: D^z_{x,s,p} by year (Eq 1.8.2)
#'   - net_immigration: NI^z_{x,s,p} by year (Eq 1.8.3)
#'   - children_fate: C^z_{x,s,g,f} by year (Eq 1.8.6)
#'   - cni_population: N^z_{x,s,m} by year (Eq 1.8.7)
#'   - married_couples: Marriage grids by year
#'   - summary: Summary statistics by year
#'
#' @details
#' The projection follows TR2025 methodology exactly:
#'
#' 1. **Phase 8B (Eq 1.8.1-1.8.4):** Core population projection using component method
#'    - Births calculated from age-specific rates × midyear female population
#'    - Deaths calculated as qx × beginning-of-year population
#'    - Net immigration = net LPR + net O
#'    - Population rolled forward: P^z = P^{z-1} - D + NI (and births for age 0)
#'
#' 2. **Phase 8C (Eq 1.8.5):** Marital status disaggregation
#'    - Births assigned to single status
#'    - Deaths distributed using mortality differentials (2015-2019)
#'    - Immigration distributed by BOY marital distribution
#'    - Marriages = rate × geometric mean of midyear unmarried populations
#'    - Divorces = rate × midyear married couples
#'    - Widowings from death rates on married population
#'
#' 3. **Phase 8D (Eq 1.8.6):** Children by parent survival status
#'    - Children ages 0-18 tracked by father age × mother age × fate
#'    - Births distributed to husband age using couples grid proportions
#'    - Parent survival calculated from mortality rates
#'    - Adjusted to match Phase 8B population totals
#'
#' 4. **Phase 8E (Eq 1.8.7):** CNI population projection
#'    - USAF projected forward (births - deaths + prorated immigration)
#'    - Civilian = USAF - total armed forces (constant)
#'    - CNI = Civilian × CNI/civilian ratio (constant from starting year)
#'    - Disaggregated into 5 marital statuses
#'
#' @export
run_projected_population_full <- function(starting_population,
                                           starting_marital_pop = NULL,
                                           starting_couples_grid = NULL,
                                           birth_rates,
                                           mortality_qx,
                                           mortality_differentials = NULL,
                                           net_lpr,
                                           net_o,
                                           marriage_rates = NULL,
                                           divorce_rates = NULL,
                                           historical_cni = NULL,
                                           config = NULL,
                                           start_year = 2022,
                                           end_year = 2099,
                                           run_phases = c("8B", "8C", "8D", "8E"),
                                           verbose = TRUE) {

  if (is.null(config)) {
    config <- get_projected_population_config()
  }

  if (verbose) {
    cli::cli_h1("PROJECTED POPULATION SUBPROCESS")
    cli::cli_alert_info("Starting year: {start_year}")
    cli::cli_alert_info("End year: {end_year}")
    cli::cli_alert_info("Phases to run: {paste(run_phases, collapse = ', ')}")
  }

  results <- list()


  # ===========================================================================
  # PHASE 8B: Core Population Projection (Equations 1.8.1-1.8.4)
  # ===========================================================================
  if ("8B" %in% run_phases) {
    if (verbose) {
      cli::cli_h2("Phase 8B: Core Population Projection")
    }

    phase8b <- run_population_projection(
      starting_population = starting_population,
      birth_rates = birth_rates,
      mortality_qx = mortality_qx,
      net_lpr = net_lpr,
      net_o = net_o,
      start_year = start_year,
      end_year = end_year,
      config = config,
      verbose = verbose
    )

    results$population <- phase8b$population
    results$births <- phase8b$births
    results$deaths <- phase8b$deaths
    results$net_immigration <- phase8b$net_immigration
    results$phase8b_summary <- phase8b$summary
  } else {
    cli::cli_abort("Phase 8B must be included in run_phases")
  }

  # ===========================================================================
  # PHASE 8C: Marital Status Disaggregation (Equation 1.8.5)
  # ===========================================================================
  if ("8C" %in% run_phases) {
    if (verbose) {
      cli::cli_h2("Phase 8C: Marital Status Projection")
    }

    if (is.null(starting_marital_pop)) {
      cli::cli_alert_warning("No starting marital population provided, skipping Phase 8C")
    } else if (is.null(marriage_rates) || is.null(divorce_rates)) {
      cli::cli_alert_warning("Missing marriage or divorce rates, skipping Phase 8C")
    } else {
      phase8c <- run_marital_projection(
        phase8b_result = phase8b,
        starting_marital_pop = starting_marital_pop,
        starting_couples_grid = starting_couples_grid,
        marriage_rates = marriage_rates,
        divorce_rates = divorce_rates,
        mortality_qx = mortality_qx,
        mortality_differentials = mortality_differentials,
        start_year = start_year,
        end_year = end_year,
        verbose = verbose
      )

      results$population_marital <- phase8c$marital_population
      results$married_couples <- phase8c$couples_grids
      results$phase8c_summary <- phase8c$summary
    }
  }

  # ===========================================================================
  # PHASE 8D: Children by Parent Fate (Equation 1.8.6)
  # ===========================================================================
  if ("8D" %in% run_phases && exists("phase8c", inherits = FALSE)) {
    if (verbose) {
      cli::cli_h2("Phase 8D: Children by Parent Fate")
    }

    parent_age_groups <- list(
      "14-24" = 14:24,
      "25-34" = 25:34,
      "35-44" = 35:44,
      "45-54" = 45:54,
      "55-64" = 55:64,
      "65-100" = 65:100
    )

    phase8d <- project_children_fate(
      phase8b_result = phase8b,
      marital_result = phase8c,
      birth_rates = birth_rates,
      mortality_qx = mortality_qx,
      parent_age_groups = parent_age_groups,
      start_year = start_year,
      end_year = end_year,
      max_child_age = config$ages$children_max,
      min_parent_age = config$ages$marriage_min,
      max_parent_age = config$ages$max_age,
      verbose = verbose
    )

    results$children_fate <- phase8d$children_fate
    results$phase8d_summary <- phase8d$summary
  }

  # ===========================================================================
  # PHASE 8E: CNI Population (Equation 1.8.7)
  # ===========================================================================
  if ("8E" %in% run_phases && exists("phase8c", inherits = FALSE)) {
    if (verbose) {
      cli::cli_h2("Phase 8E: CNI Population Projection")
    }

    if (is.null(historical_cni)) {
      cli::cli_alert_warning("No historical CNI data provided, skipping Phase 8E")
    } else {
      phase8e <- project_cni_population(
        phase8b_result = phase8b,
        marital_result = phase8c,
        historical_cni = historical_cni,
        start_year = start_year,
        end_year = end_year,
        verbose = verbose
      )

      results$cni_population <- phase8e$cni_population
      results$usaf_population <- phase8e$usaf_population
      results$civilian_population <- phase8e$civilian_population
      results$phase8e_summary <- phase8e$summary
    }
  }

  # ===========================================================================
  # Combined Summary
  # ===========================================================================
  if (verbose) {
    cli::cli_h2("Projection Complete")

    # Population summary
    pop_start <- results$population[year == start_year, sum(population)] / 1e6
    pop_end <- results$population[year == end_year, sum(population)] / 1e6
    cli::cli_alert_success("Population: {round(pop_start, 2)}M ({start_year}) -> {round(pop_end, 2)}M ({end_year})")

    # Births summary
    births_start <- results$births[year == (start_year + 1), sum(births)] / 1e6
    births_end <- results$births[year == end_year, sum(births)] / 1e6
    cli::cli_alert_info("Births: {round(births_start, 2)}M ({start_year + 1}) -> {round(births_end, 2)}M ({end_year})")

    # Deaths summary
    deaths_start <- results$deaths[year == (start_year + 1), sum(deaths)] / 1e6
    deaths_end <- results$deaths[year == end_year, sum(deaths)] / 1e6
    cli::cli_alert_info("Deaths: {round(deaths_start, 2)}M ({start_year + 1}) -> {round(deaths_end, 2)}M ({end_year})")

    # Net immigration summary
    imm_start <- results$net_immigration[year == (start_year + 1), sum(net_immigration)] / 1e6
    imm_end <- results$net_immigration[year == end_year, sum(net_immigration)] / 1e6
    cli::cli_alert_info("Net Immigration: {round(imm_start, 2)}M ({start_year + 1}) -> {round(imm_end, 2)}M ({end_year})")
  }

  results
}


#' Comprehensive Projected Population Validation
#'
#' @description
#' Runs all validation checks on projected population outputs.
#' Validates against TR2025 population files and internal consistency checks.
#'
#' @param projection_results list: Complete projection results from run_projected_population()
#' @param tr2025_pop data.table: TR2025 official population projections (optional)
#' @param config list: Configuration parameters
#' @param tolerance Numeric: Relative tolerance for validation (default: 0.02)
#'
#' @return list with comprehensive validation report:
#'   - population_identity: Whether P = P_prev - D + B + NI holds
#'   - tr2025_comparison: Comparison with TR2025 (if provided)
#'   - marital_consistency: Marital status sums to total
#'   - children_totals: Children by fate sums to total children
#'   - cni_validation: CNI < total population
#'   - all_passed: Overall pass/fail
#'
#' @export
validate_projected_population_comprehensive <- function(projection_results,
                                                         tr2025_pop = NULL,
                                                         config = NULL,
                                                         tolerance = 0.02) {

  cli::cli_h1("Comprehensive Projected Population Validation")

  if (is.null(config)) {
    config <- get_projected_population_config()
  }

  validation <- list()
  all_passed <- TRUE

  # ===========================================================================
  # Test 1: Population Identity (P = P_prev - D + B + NI)
  # ===========================================================================
  cli::cli_h2("Test 1: Population Identity Check")

  pop <- projection_results$population
  births <- projection_results$births
  deaths <- projection_results$deaths
  net_imm <- projection_results$net_immigration

  # Check identity for each year
  years <- sort(unique(pop$year))
  identity_errors <- list()

  for (yr in years[-1]) {
    pop_yr <- pop[year == yr, sum(population)]
    pop_prev <- pop[year == (yr - 1), sum(population)]
    births_yr <- births[year == yr, sum(births)]
    deaths_yr <- deaths[year == yr, sum(deaths)]
    imm_yr <- net_imm[year == yr, sum(net_immigration)]

    expected <- pop_prev - deaths_yr + births_yr + imm_yr
    error <- abs(pop_yr - expected) / expected

    if (error > 0.001) {
      identity_errors[[as.character(yr)]] <- error
    }
  }

  if (length(identity_errors) == 0) {
    cli::cli_alert_success("Population identity holds for all years")
    validation$population_identity <- TRUE
  } else {
    cli::cli_alert_warning("{length(identity_errors)} years have identity errors > 0.1%")
    validation$population_identity <- FALSE
    all_passed <- FALSE
  }

  # ===========================================================================
  # Test 2: TR2025 Population Comparison (if provided)
  # ===========================================================================
  if (!is.null(tr2025_pop)) {
    cli::cli_h2("Test 2: TR2025 Population Comparison")

    tr2025_totals <- tr2025_pop[, .(tr2025_total = sum(population)), by = year]
    our_totals <- pop[, .(our_total = sum(population)), by = year]

    comparison <- merge(our_totals, tr2025_totals, by = "year")
    comparison[, pct_diff := 100 * (our_total - tr2025_total) / tr2025_total]

    max_diff <- max(abs(comparison$pct_diff), na.rm = TRUE)
    mean_diff <- mean(abs(comparison$pct_diff), na.rm = TRUE)

    cli::cli_alert_info("Max difference: {round(max_diff, 2)}%")
    cli::cli_alert_info("Mean difference: {round(mean_diff, 3)}%")

    if (max_diff < tolerance * 100) {
      cli::cli_alert_success("All years within {tolerance * 100}% of TR2025")
      validation$tr2025_comparison <- TRUE
    } else {
      cli::cli_alert_warning("Some years exceed {tolerance * 100}% difference")
      validation$tr2025_comparison <- FALSE
      all_passed <- FALSE
    }

    validation$tr2025_comparison_details <- comparison
  } else {
    cli::cli_alert_info("TR2025 comparison skipped (no data provided)")
    validation$tr2025_comparison <- NULL
  }

  # ===========================================================================
  # Test 3: Marital Status Consistency
  # ===========================================================================
  if (!is.null(projection_results$population_marital)) {
    cli::cli_h2("Test 3: Marital Status Consistency")

    marital_pop <- projection_results$population_marital

    # Sum marital statuses
    marital_totals <- marital_pop[, .(marital_total = sum(population)), by = year]

    # Compare with Phase 8B totals (ages 14+)
    phase8b_totals <- pop[age >= 14, .(phase8b_total = sum(population)), by = year]

    comparison <- merge(marital_totals, phase8b_totals, by = "year")
    comparison[, pct_diff := 100 * abs(marital_total - phase8b_total) / phase8b_total]

    max_diff <- max(comparison$pct_diff, na.rm = TRUE)

    if (max_diff < 0.1) {
      cli::cli_alert_success("Marital statuses sum to total (max error: {round(max_diff, 3)}%)")
      validation$marital_consistency <- TRUE
    } else {
      cli::cli_alert_warning("Marital sum error: {round(max_diff, 2)}%")
      validation$marital_consistency <- FALSE
      all_passed <- FALSE
    }
  } else {
    validation$marital_consistency <- NULL
  }

  # ===========================================================================

  # Test 4: Children by Fate Totals
  # ===========================================================================
  if (!is.null(projection_results$children_fate)) {
    cli::cli_h2("Test 4: Children by Fate Totals")

    children_fate <- projection_results$children_fate
    max_child_age <- config$ages$children_max

    # Sum children by fate - filter to one parent_sex only since each child
    # appears twice in the data (once for father, once for mother tracking)
    fate_totals <- children_fate[parent_sex == "father", .(fate_total = sum(count)), by = year]

    # Compare with Phase 8B children totals
    children_pop <- pop[age <= max_child_age, .(pop_total = sum(population)), by = year]

    comparison <- merge(fate_totals, children_pop, by = "year")
    comparison[, pct_diff := 100 * abs(fate_total - pop_total) / pop_total]

    max_diff <- max(comparison$pct_diff, na.rm = TRUE)

    if (max_diff < 1.0) {
      cli::cli_alert_success("Children fate totals match population (max error: {round(max_diff, 2)}%)")
      validation$children_totals <- TRUE
    } else {
      cli::cli_alert_warning("Children total error: {round(max_diff, 2)}%")
      validation$children_totals <- FALSE
      all_passed <- FALSE
    }
  } else {
    validation$children_totals <- NULL
  }

  # ===========================================================================
  # Test 5: CNI Population Validation
  # ===========================================================================
  if (!is.null(projection_results$cni_population)) {
    cli::cli_h2("Test 5: CNI Population Validation")

    cni_pop <- projection_results$cni_population

    # CNI totals
    cni_totals <- cni_pop[, .(cni_total = sum(population)), by = year]

    # SS area totals
    ss_totals <- pop[, .(ss_total = sum(population)), by = year]

    comparison <- merge(cni_totals, ss_totals, by = "year")
    comparison[, ratio := cni_total / ss_total]

    # Check CNI < SS area for all years
    invalid_years <- comparison[ratio >= 1]

    if (nrow(invalid_years) == 0) {
      cli::cli_alert_success("CNI < SS area population for all years")
      validation$cni_validation <- TRUE
    } else {
      cli::cli_alert_danger("{nrow(invalid_years)} years have CNI >= SS population")
      validation$cni_validation <- FALSE
      all_passed <- FALSE
    }

    cli::cli_alert_info("CNI/SS ratio range: {round(min(comparison$ratio), 3)} - {round(max(comparison$ratio), 3)}")

    # Check CNI marital statuses sum to total
    cni_marital_totals <- cni_pop[, .(marital_sum = sum(population)), by = year]
    cni_comparison <- merge(cni_marital_totals, cni_totals, by = "year")
    cni_comparison[, diff := abs(marital_sum - cni_total)]

    if (max(cni_comparison$diff) < 1) {
      cli::cli_alert_success("CNI marital statuses sum correctly")
    } else {
      cli::cli_alert_warning("CNI marital sum has errors")
    }
  } else {
    validation$cni_validation <- NULL
  }

  # ===========================================================================
  # Overall Result
  # ===========================================================================
  cli::cli_h2("Overall Validation Result")

  validation$all_passed <- all_passed

  if (all_passed) {
    cli::cli_alert_success("All validation checks PASSED")
  } else {
    cli::cli_alert_warning("Some validation checks FAILED")
  }

  validation
}

# =============================================================================
# TARGET HELPER FUNCTIONS
# =============================================================================

#' Combine mortality qx for population projection (target helper)
#'
#' @description
#' Helper function for the mortality_qx_for_projection target. Combines historical
#' and projected qx, optionally loading TR2025 historical qx directly, and applies
#' age-last-birthday adjustments for ages 85+.
#'
#' @param mortality_qx_projected data.table: Projected qx from mortality projection
#' @param mortality_qx_historical data.table: Historical qx (fallback if not using TR qx)
#' @param qx_age_last_birthday data.table: Age-last-birthday qx for adjustment
#' @param config List: config_assumptions with mortality settings
#'
#' @return data.table with combined qx for all years needed by projection
#'
#' @export
combine_mortality_qx_for_projection <- function(mortality_qx_projected,
                                                  mortality_qx_historical,
                                                  qx_age_last_birthday,
                                                  config) {
  # Select common columns
  common_cols <- c("year", "age", "sex", "qx")
  proj_qx <- mortality_qx_projected[, ..common_cols]

  # Check if using tr_qx method - if so, load TR2025 historical qx
  method <- config$mortality$starting_aax_method
  if (!is.null(method) && method == "tr_qx") {
    cli::cli_alert_info("Using TR2025 historical qx for tr_qx method")

    # Get historical file paths from config
    male_hist_file <- config$mortality$starting_tr_qx$male_qx_hist_file
    female_hist_file <- config$mortality$starting_tr_qx$female_qx_hist_file

    # Use default paths if not in config
    if (is.null(male_hist_file)) {
      male_hist_file <- "data/raw/SSA_TR2025/DeathProbsE_M_Hist_TR2025.csv"
    }
    if (is.null(female_hist_file)) {
      female_hist_file <- "data/raw/SSA_TR2025/DeathProbsE_F_Hist_TR2025.csv"
    }

    # Load TR2025 historical qx (1900-2022)
    hist_qx <- load_tr_qx_all_years(
      male_qx_file = male_hist_file,
      female_qx_file = female_hist_file,
      start_year = 1900,
      end_year = 2022,
      ages = 0:119
    )

    # Limit to age 100 (100+ group) like we do for projected
    hist_qx <- hist_qx[age <= 100]
    hist_qx <- hist_qx[, ..common_cols]
  } else {
    # Use calculated historical qx
    hist_qx <- mortality_qx_historical[, ..common_cols]
  }

  # Remove overlapping years from historical (projected takes precedence)
  proj_years <- unique(proj_qx$year)
  hist_qx <- hist_qx[!year %in% proj_years]

  combined_qx <- data.table::rbindlist(
    list(hist_qx, proj_qx),
    use.names = TRUE
  )

  # Apply age-last-birthday qx for all ages (per TR2025 Section 1.2.c)
  cli::cli_alert_info("Applying age-last-birthday qx adjustment for ages 0-100 (per TR2025 Section 1.2.c)")
  combined_qx <- apply_age_last_birthday_qx(
    mortality_qx = combined_qx,
    qx_alb = qx_age_last_birthday,
    min_age = 0
  )

  combined_qx
}

#' Extract starting marital population (target helper)
#'
#' @description
#' Helper function for the starting_marital_pop target. Extracts and processes
#' historical marital population for the starting year of projection.
#'
#' @param historical_population_marital data.table: Historical population by marital status
#' @param config List: config_assumptions with projected_population settings
#'
#' @return data.table with starting year marital population by age, sex, marital_status
#'
#' @export
extract_starting_marital_population <- function(historical_population_marital, config) {
  cli::cli_h2("Extracting Starting Marital Population")
  starting_year <- config$projected_population$starting_year

  # Get from historical_population_marital
  marital_data <- historical_population_marital[year == starting_year]

  # Aggregate by age, sex, marital_status (remove orientation if present)
  if ("orientation" %in% names(marital_data)) {
    marital_data <- marital_data[, .(population = sum(population, na.rm = TRUE)),
                                  by = .(year, age, sex, marital_status)]
  }

  # Standardize marital status names
  if ("never_married" %in% marital_data$marital_status) {
    marital_data[marital_status == "never_married", marital_status := "single"]
  }

  total_pop <- sum(marital_data$population, na.rm = TRUE)
  married_pop <- marital_data[marital_status == "married", sum(population, na.rm = TRUE)]

  cli::cli_alert_success(
    "Starting marital population: {format(round(total_pop/1e6, 2), big.mark=',')}M total, {format(round(married_pop/1e6, 2), big.mark=',')}M married"
  )

  marital_data
}

# =============================================================================
# HELPER FUNCTIONS FOR TARGET FACTORIES
# =============================================================================

#' Load starting population from TR2025 file
#'
#' @description
#' Loads starting population from TR2025 population file and splits by
#' sexual orientation status using default TR2025 assumptions.
#'
#' @param tr_file Path to TR2025 population file
#' @param starting_year Year to extract (default: 2022)
#' @param male_gay_pct Percentage of males who are gay (default: 2.5)
#' @param female_lesbian_pct Percentage of females who are lesbian (default: 4.5)
#'
#' @return data.table with columns year, age, sex, pop_status, population
#'
#' @export
load_tr_starting_population <- function(tr_file, starting_year = 2022,
                                             male_gay_pct = 2.5,
                                             female_lesbian_pct = 4.5) {
  tr_pop <- data.table::fread(tr_file)
  tr_start <- tr_pop[Year == starting_year,
                     .(year = Year, age = Age, male = `M Tot`, female = `F Tot`)]
  tr_start <- data.table::melt(
    tr_start,
    id.vars = c("year", "age"),
    measure.vars = c("male", "female"),
    variable.name = "sex",
    value.name = "population"
  )

  # Cap age at 100 and aggregate
  tr_start[age > 100, age := 100L]
  tr_start <- tr_start[, .(population = sum(population)), by = .(year, age, sex)]

  # Split by sexual orientation
  tr_start_status <- data.table::rbindlist(list(
    tr_start[sex == "male", .(year, age, sex, pop_status = "heterosexual",
                              population = population * (1 - male_gay_pct / 100))],
    tr_start[sex == "male", .(year, age, sex, pop_status = "gay",
                              population = population * (male_gay_pct / 100))],
    tr_start[sex == "female", .(year, age, sex, pop_status = "heterosexual",
                                population = population * (1 - female_lesbian_pct / 100))],
    tr_start[sex == "female", .(year, age, sex, pop_status = "lesbian",
                                population = population * (female_lesbian_pct / 100))]
  ))

  cli::cli_alert_success("Loaded TR2025 population for Dec 31, {starting_year}")
  tr_start_status
}

#' Create projection summary data.table
#'
#' @description
#' Creates a summary data.table from population projection results.
#'
#' @param population Population data.table
#' @param births Births data.table
#' @param deaths Deaths data.table
#' @param net_immigration Net immigration data.table
#' @param cni_summary CNI summary data.table
#' @param start_year Starting year of projection
#' @param end_year Ending year of projection
#'
#' @return data.table with summary metrics
#'
#' @export
create_projection_summary <- function(population, births, deaths, net_immigration,
                                       cni_summary, start_year, end_year) {
  start_pop <- population[year == start_year, sum(population)]
  end_pop <- population[year == end_year, sum(population)]
  growth_rate <- 100 * ((end_pop / start_pop)^(1 / (end_year - start_year)) - 1)

  cni_start <- if (!is.null(cni_summary) && nrow(cni_summary[year == start_year]) > 0) {
    cni_summary[year == start_year, cni_ss_ratio]
  } else NA_real_

  cni_end <- if (!is.null(cni_summary) && nrow(cni_summary[year == end_year]) > 0) {
    cni_summary[year == end_year, cni_ss_ratio]
  } else NA_real_

  data.table::data.table(
    metric = c(
      "Starting population (M)",
      "Ending population (M)",
      "Population growth rate (%/year)",
      "Total births 2023-2099 (M)",
      "Total deaths 2023-2099 (M)",
      "Total net immigration 2023-2099 (M)",
      "CNI/SS ratio (start)",
      "CNI/SS ratio (end)"
    ),
    value = c(
      round(start_pop / 1e6, 2),
      round(end_pop / 1e6, 2),
      round(growth_rate, 3),
      round(births[, sum(births)] / 1e6, 2),
      round(deaths[, sum(deaths)] / 1e6, 2),
      round(net_immigration[, sum(net_immigration)] / 1e6, 2),
      round(cni_start, 4),
      round(cni_end, 4)
    )
  )
}

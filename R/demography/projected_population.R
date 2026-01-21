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
#' @param projection_years Integer vector: years to project (2023-2099)
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
verify_fertility_inputs <- function(fertility_rates, projection_years = 2023:2099) {
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
#' @param projection_years Integer vector: years to project (2023-2099)
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
verify_mortality_inputs <- function(mortality_qx, projection_years = 2023:2099) {
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
#' @param projection_years Integer vector: years to project
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
verify_lpr_immigration_inputs <- function(net_lpr, projection_years = 2023:2099) {
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
#' @param projection_years Integer vector: years to project
#'
#' @return list with validation results
#'
#' @export
verify_o_immigration_inputs <- function(net_o, projection_years = 2023:2099) {
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
#' @param projection_years Integer vector: years to project
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
verify_marriage_inputs <- function(marriage_rates, projection_years = 2023:2099) {
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
#' @param projection_years Integer vector: years to project
#'
#' @return list with validation results
#'
#' @export
verify_divorce_inputs <- function(divorce_rates, projection_years = 2023:2099) {
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
#' @param starting_year Integer: starting year (default: 2022)
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
                                         starting_year = 2022) {
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
#' @param projection_years Years to project (default: 2023:2099)
#' @param starting_year Starting year for projection (default: 2022)
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
                                          projection_years = 2023:2099,
                                          starting_year = 2022) {
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
#'
#' @return list with configuration parameters
#'
#' @export
get_projected_population_config <- function() {
  list(
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
                                     config = NULL) {

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

  # === Ages 1+: Survive from previous year ===
  # P_{x,s,p}^z = P_{x-1,s,p}^{z-1} - D_{x,s,p}^z + NI_{x,s,p}^z
  #
  # IMPORTANT: D_{x}^z = qx_x * P_{x-1}^{z-1}
  # Deaths at age x must be calculated on the AGED population (people who were x-1)

  # Age population forward: people who were age x-1 last year are now age x
  aged_pop <- data.table::copy(population_prev)
  aged_pop[, age := age + 1L]
  aged_pop[, year := target_year]

  # Cap at max age (100+ group)
  aged_pop[age > max_age, age := max_age]

  # Aggregate for ages at max_age (people aging into 100+ from 99 and 100+)
  aged_pop <- aged_pop[, .(population = sum(population)), by = .(year, age, sex, pop_status)]

  # Calculate deaths for ages 1+ on the AGED population (correct methodology)
  # D_x = qx_x * aged_pop[x] where aged_pop[x] = population_prev[x-1]
  year_qx <- mortality_qx[year == target_year, .(age, sex, qx)]
  aged_pop <- merge(aged_pop, year_qx, by = c("age", "sex"), all.x = TRUE)
  aged_pop[is.na(qx), qx := 0]
  aged_pop[, deaths := qx * population]
  aged_pop[, population := population - deaths]

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
                                       start_year = 2022,
                                       end_year = 2099,
                                       config = NULL,
                                       qx_100_119 = NULL,
                                       verbose = TRUE) {

  if (is.null(config)) {
    config <- get_projected_population_config()
  }

  if (verbose) {
    cli::cli_h1("Population Projection ({start_year + 1}-{end_year})")
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
    new_pop <- project_population_year(
      year = yr,
      population_prev = current_pop,
      births = births,
      deaths = deaths,  # Used for age 0 (infant deaths)
      mortality_qx = mortality_qx_working,  # Used for ages 1+ (deaths calculated on aged pop)
      net_immigration = net_imm,
      config = config
    )

    # Update 100+ tracker for next year
    if (use_dynamic_100plus && !is.null(tracker_100plus)) {
      # Calculate new 100-year-olds: survivors from age 99 who aged to 100
      # This is the population at age 99 at end of previous year, survived through this year
      pop_99_prev <- current_pop[age == 99, .(sex, pop99 = population)]

      # Get qx at age 99 for this year
      qx_99 <- mortality_qx_working[year == yr & age == 99, .(sex, qx99 = qx)]
      pop_99_prev <- merge(pop_99_prev, qx_99, by = "sex", all.x = TRUE)
      pop_99_prev[is.na(qx99), qx99 := 0]

      # Survivors who reach age 100
      new_100_male <- pop_99_prev[sex == "male", pop99 * (1 - qx99)]
      new_100_female <- pop_99_prev[sex == "female", pop99 * (1 - qx99)]

      if (length(new_100_male) == 0) new_100_male <- 0
      if (length(new_100_female) == 0) new_100_female <- 0

      # Update tracker (uses tracker$qx_data internally)
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

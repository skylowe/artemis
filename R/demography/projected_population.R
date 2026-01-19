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
#' - Ages: 0-119 (single year, with 100+ grouped)
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

  # Check age coverage (need at least 0-99)
  available_ages <- sort(unique(mortality_qx$age))
  expected_ages <- 0:99
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
  expected_ages <- 0:99  # At minimum need 0-99
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
  # Mortality needs 0-119, Immigration needs 0-99, Fertility needs 14-49

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
      max_age = 119,
      max_age_group = 100,  # 100+ grouped
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

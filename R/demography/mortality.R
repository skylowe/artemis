#' Mortality Subprocess
#'
#' Functions for calculating historical death rates, projecting future mortality,
#' and computing life tables using the SSA methodology.
#'
#' @name mortality
NULL

# =============================================================================
# Phase 2B: Historical Mortality Calculations
# =============================================================================

#' Calculate central death rates (mx)
#'
#' @description
#' Computes central death rates from deaths and midyear population data.
#' Central death rate = Deaths / Midyear Population
#'
#' @param deaths data.table with columns: year, age, sex, cause, deaths
#' @param population data.table with columns: year, age, sex, population
#' @param by_cause Logical: calculate rates by cause of death (default: TRUE)
#'
#' @return data.table with columns: year, age, sex, [cause], deaths, population, mx
#'
#' @details
#' The central death rate mx is the fundamental mortality measure:
#' mx = Dx / Px
#'
#' Where:
#' - Dx = deaths to persons age x during year z
#' - Px = midyear population age x in year z
#'
#' When by_cause=TRUE, rates are computed separately for each cause.
#' Total mx can be obtained by summing across causes.
#'
#' @export
calculate_central_death_rates <- function(deaths, population, by_cause = TRUE) {
  # Validate inputs
  checkmate::assert_data_table(deaths)
  checkmate::assert_data_table(population)
  checkmate::assert_names(names(deaths), must.include = c("year", "age", "sex", "deaths"))
  checkmate::assert_names(names(population), must.include = c("year", "age", "sex", "population"))
  checkmate::assert_flag(by_cause)

  # Copy to avoid modifying originals
  d <- data.table::copy(deaths)
  p <- data.table::copy(population)

  # Determine grouping columns
  if (by_cause && "cause" %in% names(d)) {
    group_cols <- c("year", "age", "sex", "cause")
  } else {
    group_cols <- c("year", "age", "sex")
    # Aggregate deaths across causes if needed
    if ("cause" %in% names(d)) {
      d <- d[, .(deaths = sum(deaths)), by = .(year, age, sex)]
    }
  }

  # Aggregate deaths by grouping columns
  d <- d[, .(deaths = sum(deaths)), by = group_cols]

  # Ensure population is aggregated by year, age, sex
  p <- p[, .(population = sum(population)), by = .(year, age, sex)]

  # Join deaths and population
  dt <- merge(d, p, by = c("year", "age", "sex"), all.x = TRUE)

  # Calculate central death rate
  # Handle zero population (set mx to NA to avoid Inf)
  dt[, mx := data.table::fifelse(population > 0, deaths / population, NA_real_)]

  # Order results
  if (by_cause && "cause" %in% names(dt)) {
    data.table::setorder(dt, year, sex, age, cause)
  } else {
    data.table::setorder(dt, year, sex, age)
  }

  # Report summary
  years <- unique(dt$year)
  cli::cli_alert_success(
    "Calculated central death rates for {length(years)} years ({min(years)}-{max(years)})"
  )

  dt
}

#' Calculate total death rates from cause-specific rates
#'
#' @description
#' Sums cause-specific mx values to get total mx by age, sex, and year.
#'
#' @param mx_by_cause data.table with cause-specific mx values
#'
#' @return data.table with total mx (cause column removed or set to "total")
#'
#' @export
calculate_total_death_rates <- function(mx_by_cause) {
  checkmate::assert_data_table(mx_by_cause)
  checkmate::assert_names(names(mx_by_cause), must.include = c("year", "age", "sex", "cause", "mx"))

  # Sum mx across causes
  dt <- mx_by_cause[, .(
    deaths = sum(deaths, na.rm = TRUE),
    population = first(population),  # Same for all causes
    mx = sum(mx, na.rm = TRUE)
  ), by = .(year, age, sex)]

  dt[, cause := "total"]
  data.table::setcolorder(dt, c("year", "age", "sex", "cause", "deaths", "population", "mx"))
  data.table::setorder(dt, year, sex, age)

  dt
}

#' Calculate annual percentage reduction in death rates (AAx)
#'
#' @description
#' Computes AAx values using weighted log-linear regression over a historical
#' period. AAx represents the annual percentage change in mortality rates.
#'
#' @param mx data.table with historical death rates (year, age, sex, [cause], mx)
#' @param start_year Integer: first year of regression period (default: 2008)
#' @param end_year Integer: last year of regression period (default: 2019)
#' @param by_cause Logical: calculate AAx by cause of death (default: TRUE)
#'
#' @return data.table with columns: age, sex, [cause], aax, starting_aax, intercept, r_squared
#'
#' @details
#' The AAx is derived from weighted log-linear regression:
#' log(mx) = alpha + beta * year
#'
#' AAx = 1 - exp(beta) (positive AAx indicates mortality improvement)
#'
#' Per SSA methodology, the 12-year regression period (2008-2019) uses specific weights:
#' - Years 1-4 (2008-2011): 0.2, 0.4, 0.6, 0.8
#' - Years 5-10 (2012-2017): 1.0
#' - Years 11-12 (2018-2019): 2.0, 3.0
#'
#' Starting AAx rules:
#' - If calculated AAx >= 0: use as starting value
#' - If calculated AAx < 0: use 75% of the value
#'
#' @export
calculate_annual_reduction_rates <- function(mx,
                                              start_year = 2008,
                                              end_year = 2019,
                                              by_cause = TRUE) {
  checkmate::assert_data_table(mx)
  checkmate::assert_int(start_year, lower = 1900, upper = 2100)
  checkmate::assert_int(end_year, lower = start_year, upper = 2100)

  # Filter to regression period
  dt <- mx[year >= start_year & year <= end_year]

  if (nrow(dt) == 0) {
    cli::cli_abort("No data in regression period {start_year}-{end_year}")
  }

  # Filter out zero/NA mx values (can't take log)
  dt <- dt[!is.na(mx) & mx > 0]

  # Calculate log(mx)
  dt[, log_mx := log(mx)]

  # Determine grouping
  if (by_cause && "cause" %in% names(dt)) {
    group_cols <- c("age", "sex", "cause")
  } else {
    group_cols <- c("age", "sex")
  }

  # SSA-specified regression weights for 12-year period (2008-2019)
  # Years 1-4: 0.2, 0.4, 0.6, 0.8; Years 5-10: 1.0; Years 11-12: 2.0, 3.0
  ssa_weights <- c(0.2, 0.4, 0.6, 0.8, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 3.0)
  weight_years <- start_year:(start_year + length(ssa_weights) - 1)
  weight_map <- data.table::data.table(year = weight_years, ssa_weight = ssa_weights)

  # Merge weights into data
  dt <- merge(dt, weight_map, by = "year", all.x = TRUE)
  dt[is.na(ssa_weight), ssa_weight := 1.0]  # Default for years outside standard range

  # Perform weighted regression for each group
  # log(mx) = alpha + beta * year
  # AAx = 1 - exp(beta)
  result <- dt[, {
    if (.N >= 3 && sum(ssa_weight) > 0) {
      # Weighted linear regression with SSA weights
      fit <- tryCatch({
        lm(log_mx ~ year, weights = ssa_weight, data = .SD)
      }, error = function(e) NULL)

      if (!is.null(fit)) {
        coef_vals <- coef(fit)
        beta <- coef_vals["year"]

        # AAx = 1 - exp(beta) per SSA formula (Equation 1.2.2)
        calculated_aax <- 1 - exp(beta)

        # Apply 75% rule for negative AAx values
        starting_aax <- data.table::fifelse(
          calculated_aax >= 0,
          calculated_aax,
          0.75 * calculated_aax
        )

        list(
          aax = calculated_aax,
          starting_aax = starting_aax,
          intercept = coef_vals["(Intercept)"],
          r_squared = summary(fit)$r.squared,
          n_years = .N
        )
      } else {
        list(aax = NA_real_, starting_aax = NA_real_, intercept = NA_real_,
             r_squared = NA_real_, n_years = .N)
      }
    } else {
      list(aax = NA_real_, starting_aax = NA_real_, intercept = NA_real_,
           r_squared = NA_real_, n_years = .N)
    }
  }, by = group_cols]

  data.table::setorder(result, sex, age)

  cli::cli_alert_success(
    "Calculated AAx for {nrow(result)} age-sex{if(by_cause) '-cause' else ''} groups"
  )

  result
}

#' Whittaker-Henderson smoothing
#'
#' @description
#' Applies Whittaker-Henderson Type B smoothing to a series of values.
#' This is the standard actuarial smoothing method used by SSA.
#'
#' @param values Numeric vector of values to smooth
#' @param weights Numeric vector of weights (default: equal weights)
#' @param lambda Numeric: smoothing parameter (default: 0.01 per SSA methodology)
#' @param d Integer: order of differences/degree parameter (default: 2 per SSA methodology)
#'
#' @return Numeric vector of smoothed values
#'
#' @details
#' The Whittaker-Henderson method minimizes:
#' F = sum(w_i * (y_i - s_i)^2) + lambda * sum((delta^d s_i)^2)
#'
#' Where:
#' - y_i = original values
#' - s_i = smoothed values
#' - w_i = weights
#' - delta^d = d-th order differences
#' - lambda = smoothing parameter (higher = smoother)
#'
#' Per SSA 2025 Long-Range Model Documentation (Section 1.2 Mortality):
#' "degree parameter equal to 2 and smoothing parameter equal to 0.01"
#'
#' @export
whittaker_henderson_smooth <- function(values, weights = NULL, lambda = 0.01, d = 2) {
  n <- length(values)

  if (n < d + 1) {
    cli::cli_warn("Series too short for WH smoothing (n={n}, d={d}), returning original")
    return(values)
  }

  # Default equal weights
  if (is.null(weights)) {
    weights <- rep(1, n)
  }

  # Handle NA values - set weight to 0
  na_idx <- is.na(values)
  if (any(na_idx)) {
    weights[na_idx] <- 0
    values[na_idx] <- 0  # Will be overwritten by smoothing
  }

  # Create weight diagonal matrix
  W <- diag(weights)

  # Create difference matrix of order d
  D <- diff(diag(n), differences = d)

  # Solve the WH equation: (W + lambda * D'D) * s = W * y
  # This is a banded matrix system
  A <- W + lambda * crossprod(D)
  b <- W %*% values

  # Solve using standard linear algebra
  smoothed <- tryCatch({
    as.vector(solve(A, b))
  }, error = function(e) {
    cli::cli_warn("WH smoothing failed: {conditionMessage(e)}")
    values
  })

  # Restore NA positions if original had them
  if (any(na_idx)) {
    smoothed[na_idx] <- NA
  }

  smoothed
}

#' Smooth death rates using Whittaker-Henderson
#'
#' @description
#' Applies Whittaker-Henderson smoothing to death rates by age within
#' each year-sex-cause group.
#'
#' @param mx data.table with death rates
#' @param age_range Integer vector of length 2: age range to smooth (default: c(2, 99))
#' @param lambda Numeric: WH smoothing parameter (default: 0.01 per SSA methodology)
#' @param d Integer: WH difference order/degree (default: 2 per SSA methodology)
#'
#' @return data.table with smoothed mx column added
#'
#' @details
#' Ages 0 and 1 are excluded from smoothing due to special infant mortality
#' patterns. Ages 100+ are also excluded and handled separately.
#'
#' Per SSA 2025 Long-Range Model Documentation:
#' "degree parameter equal to 2 and smoothing parameter equal to 0.01"
#'
#' @export
smooth_death_rates <- function(mx, age_range = c(2, 99), lambda = 0.01, d = 2) {
  checkmate::assert_data_table(mx)
  checkmate::assert_names(names(mx), must.include = c("year", "age", "sex", "mx"))
  checkmate::assert_integerish(age_range, len = 2)

  dt <- data.table::copy(mx)

  # Determine grouping columns (exclude age)
  group_cols <- intersect(c("year", "sex", "cause"), names(dt))

  # Apply smoothing within each group

  dt[, mx_smooth := {
    # Get ages in range
    in_range <- age >= age_range[1] & age <= age_range[2]

    # Initialize with original values
    result <- mx

    if (sum(in_range) >= d + 1) {
      # Extract values to smooth
      vals <- mx[in_range]
      weights <- rep(1, length(vals))

      # Apply WH smoothing
      smoothed <- whittaker_henderson_smooth(vals, weights, lambda, d)
      result[in_range] <- smoothed
    }

    result
  }, by = group_cols]

  cli::cli_alert_success("Applied Whittaker-Henderson smoothing (lambda={lambda}, d={d})")

  dt
}

#' Calculate historical smoothed death rates
#'
#' @description
#' Main function to compute smoothed historical death rates for validation
#' and projection purposes.
#'
#' @param deaths data.table with death counts by year, age, sex, cause
#' @param population data.table with population by year, age, sex
#' @param years Integer vector of years to include (default: 1979:2022)
#' @param smooth Logical: apply WH smoothing (default: TRUE)
#' @param by_cause Logical: calculate by cause of death (default: TRUE)
#'
#' @return data.table with historical death rates
#'
#' @export
calculate_historical_mortality <- function(deaths,
                                            population,
                                            years = 1979:2022,
                                            smooth = TRUE,
                                            by_cause = TRUE) {
  # Filter to requested years
  d <- deaths[year %in% years]
  p <- population[year %in% years]

  if (nrow(d) == 0) {
    cli::cli_abort("No death data for years {min(years)}-{max(years)}")
  }
  if (nrow(p) == 0) {
    cli::cli_abort("No population data for years {min(years)}-{max(years)}")
  }

  # Calculate central death rates
  mx <- calculate_central_death_rates(d, p, by_cause = by_cause)

  # Apply smoothing if requested
  if (smooth) {
    mx <- smooth_death_rates(mx)
  }

  # Calculate total rates if by_cause
  if (by_cause && "cause" %in% names(mx)) {
    mx_total <- calculate_total_death_rates(mx)
    # Optionally bind both
    mx[, mx_smooth := NULL]  # Remove smooth column before merge if exists
    mx_total[, mx_smooth := NULL]
  }

  mx
}

# =============================================================================
# Phase 2C: Mortality Projection
# =============================================================================

#' Get ultimate AAx assumptions from TR2025 configuration
#'
#' @description
#' Returns ultimate annual percentage reduction assumptions by age group,
#' sex, and cause of death as specified by the Board of Trustees.
#'
#' @return data.table with columns: age_group, age_min, age_max, cause, ultimate_aax
#'
#' @details
#' Ultimate values from TR2025 Appendix 1.2-1. Same values for male and female.
#' Age groups: under 15, 15-49, 50-64, 65-84, 85+
#'
#' @export
get_ultimate_aax_assumptions <- function() {
  # Ultimate AAx values from TR2025 documentation (Appendix 1.2-1)
  # Values are percentages, converted to decimals
  ultimate <- data.table::data.table(
    age_group = rep(c("under_15", "15_49", "50_64", "65_84", "85_plus"), each = 6),
    age_min = rep(c(0, 15, 50, 65, 85), each = 6),
    age_max = rep(c(14, 49, 64, 84, 119), each = 6),
    cause = rep(c("CVD", "CAN", "ACV", "RES", "DEM", "OTH"), 5),
    ultimate_aax = c(
      # Under 15
      0.019, 0.015, 0.010, 0.020, 0.001, 0.017,
      # 15-49
      0.013, 0.015, 0.007, 0.005, 0.001, 0.008,
      # 50-64
      0.015, 0.015, 0.005, 0.007, 0.001, 0.006,
      # 65-84
      0.019, 0.009, 0.005, 0.003, 0.001, 0.003,
      # 85+
      0.015, 0.005, 0.003, 0.002, 0.001, 0.003
    )
  )

  ultimate
}

#' Map single ages to ultimate AAx age groups
#'
#' @param ages Integer vector of ages
#'
#' @return Character vector of age group names
#'
#' @keywords internal
map_age_to_ultimate_group <- function(ages) {
  data.table::fcase(
    ages < 15, "under_15",
    ages >= 15 & ages < 50, "15_49",
    ages >= 50 & ages < 65, "50_64",
    ages >= 65 & ages < 85, "65_84",
    ages >= 85, "85_plus"
  )
}

#' Calculate AAx transition trajectory
#'
#' @description
#' Calculates the AAx trajectory from starting values to ultimate values
#' over the transition period (24 years per SSA methodology).
#'
#' @param starting_aax data.table with starting AAx by age, sex, [cause]
#' @param ultimate_aax data.table from get_ultimate_aax_assumptions()
#' @param base_year Integer: last year of historical data (default: 2019)
#' @param projection_years Integer vector of years to project
#' @param transition_years Integer: years to reach ultimate (default: 24)
#' @param transition_rate Numeric: convergence rate per year (default: 0.80)
#'
#' @return data.table with AAx by year, age, sex, [cause]
#'
#' @details
#' Per SSA methodology:
#' - For each year after base_year: AA_x^z = ultimate + 0.8 * (AA_x^{z-1} - ultimate)
#' - At year 25 of projection, AAx equals ultimate values
#'
#' @export
calculate_aax_trajectory <- function(starting_aax,
                                      ultimate_aax = NULL,
                                      base_year = 2019,
                                      projection_years = 2020:2100,
                                      transition_years = 24,
                                      transition_rate = 0.80) {
  checkmate::assert_data_table(starting_aax)
  checkmate::assert_int(base_year)
  checkmate::assert_integerish(projection_years)

  if (is.null(ultimate_aax)) {
    ultimate_aax <- get_ultimate_aax_assumptions()
  }

  # Determine if we have cause-specific data
  has_cause <- "cause" %in% names(starting_aax)

  # Map starting AAx ages to ultimate age groups
  dt <- data.table::copy(starting_aax)
  dt[, age_group := map_age_to_ultimate_group(age)]

  # Merge with ultimate values
  if (has_cause) {
    dt <- merge(dt, ultimate_aax[, .(age_group, cause, ultimate_aax)],
                by = c("age_group", "cause"), all.x = TRUE)
  } else {
    # If no cause, use weighted average of ultimate AAx
    ultimate_avg <- ultimate_aax[, .(ultimate_aax = mean(ultimate_aax)), by = age_group]
    dt <- merge(dt, ultimate_avg, by = "age_group", all.x = TRUE)
  }

  # Handle any missing ultimate values
  dt[is.na(ultimate_aax), ultimate_aax := 0.005]  # Default small positive value

  # Build trajectory for each year
  result_list <- list()

  # Year 1 of projection (base_year + 1) uses starting_aax
  projection_start <- min(projection_years)
  ultimate_year <- base_year + transition_years + 1  # Year when ultimate is reached

  for (proj_year in projection_years) {
    year_dt <- data.table::copy(dt)
    year_dt[, year := proj_year]

    years_from_base <- proj_year - base_year

    if (years_from_base <= 0) {
      # Use starting values for base year and earlier
      year_dt[, aax_projected := starting_aax]
    } else if (proj_year >= ultimate_year) {
      # At or after ultimate year, use ultimate values
      year_dt[, aax_projected := ultimate_aax]
    } else {
      # Transition period: apply 80% convergence formula iteratively
      # AA_x^z = ultimate + 0.8^n * (starting - ultimate)
      # where n = years from base
      year_dt[, aax_projected := ultimate_aax + (transition_rate ^ years_from_base) * (starting_aax - ultimate_aax)]
    }

    result_list[[as.character(proj_year)]] <- year_dt
  }

  result <- data.table::rbindlist(result_list)

  # Select output columns
  if (has_cause) {
    out_cols <- c("year", "age", "sex", "cause", "aax_projected", "ultimate_aax")
  } else {
    out_cols <- c("year", "age", "sex", "aax_projected", "ultimate_aax")
  }
  out_cols <- intersect(out_cols, names(result))
  result <- result[, ..out_cols]

  data.table::setorder(result, year, sex, age)

  cli::cli_alert_success(

    "Calculated AAx trajectory for {length(projection_years)} years ({min(projection_years)}-{max(projection_years)})"
  )

  result
}

#' Project central death rates forward
#'
#' @description
#' Projects mx forward from starting values using AAx reduction rates.
#'
#' @param starting_mx data.table with starting mx by age, sex, [cause]
#' @param aax_trajectory data.table with AAx by year, age, sex, [cause]
#' @param actual_mx Optional data.table with actual mx for years to overwrite projections
#'   (e.g., 2020-2023 actual data)
#'
#' @return data.table with projected mx by year, age, sex, [cause]
#'
#' @details
#' Per SSA methodology:
#' - mx^z = mx^{z-1} * (1 - AAx^z)
#' - For 2020-2023, projected values are overwritten with actual data
#'
#' @export
project_death_rates <- function(starting_mx, aax_trajectory, actual_mx = NULL) {
  checkmate::assert_data_table(starting_mx)
  checkmate::assert_data_table(aax_trajectory)
  checkmate::assert_names(names(aax_trajectory), must.include = c("year", "age", "sex", "aax_projected"))

  # Determine grouping columns
  has_cause <- "cause" %in% names(starting_mx) && "cause" %in% names(aax_trajectory)
  if (has_cause) {
    group_cols <- c("age", "sex", "cause")
  } else {
    group_cols <- c("age", "sex")
  }

  # Get projection years from trajectory
  projection_years <- sort(unique(aax_trajectory$year))

  # Initialize with starting mx
  mx_col <- intersect(c("starting_mx", "mx"), names(starting_mx))[1]
  current_mx <- data.table::copy(starting_mx)
  if (mx_col != "mx") {
    data.table::setnames(current_mx, mx_col, "mx")
  }

  # Project year by year
  result_list <- list()

  for (proj_year in projection_years) {
    # Get AAx for this year
    aax_year <- aax_trajectory[year == proj_year]

    # Merge current mx with AAx
    proj_dt <- merge(current_mx[, c(group_cols, "mx"), with = FALSE],
                     aax_year[, c(group_cols, "aax_projected"), with = FALSE],
                     by = group_cols, all.x = TRUE)

    # Handle missing AAx (use 0 = no change)
    proj_dt[is.na(aax_projected), aax_projected := 0]

    # Apply reduction: mx^z = mx^{z-1} * (1 - AAx^z)
    proj_dt[, mx := mx * (1 - aax_projected)]
    proj_dt[, year := proj_year]

    # Store result
    result_list[[as.character(proj_year)]] <- proj_dt[, c("year", group_cols, "mx"), with = FALSE]

    # Update current_mx for next iteration
    current_mx <- proj_dt[, c(group_cols, "mx"), with = FALSE]
  }

  result <- data.table::rbindlist(result_list)

  # Overwrite with actual data if provided
  if (!is.null(actual_mx)) {
    checkmate::assert_data_table(actual_mx)
    actual_years <- unique(actual_mx$year)

    for (yr in actual_years) {
      if (yr %in% projection_years) {
        # Get actual mx for this year
        actual_yr <- actual_mx[year == yr]
        mx_actual_col <- intersect(c("mx_smooth", "mx"), names(actual_yr))[1]

        # Update projected values with actual
        result <- result[year != yr]
        actual_to_add <- actual_yr[, c("year", group_cols, mx_actual_col), with = FALSE]
        if (mx_actual_col != "mx") {
          data.table::setnames(actual_to_add, mx_actual_col, "mx")
        }
        result <- data.table::rbindlist(list(result, actual_to_add), fill = TRUE)
      }
    }

    data.table::setorder(result, year, sex, age)
    cli::cli_alert_info("Overwrote projected mx with actual data for years: {paste(actual_years[actual_years %in% projection_years], collapse=', ')}")
  }

  data.table::setorder(result, year, sex, age)

  cli::cli_alert_success(
    "Projected mx for {length(projection_years)} years ({min(projection_years)}-{max(projection_years)})"
  )

  result
}

#' Get COVID-19 adjustment factors
#'
#' @description
#' Returns COVID-19 adjustment factors for qx by year and age group.
#'
#' @return data.table with columns: year, age_min, age_max, covid_factor
#'
#' @details
#' Per SSA TR2025 documentation, COVID impacts death rates through 2025.
#'
#' @export
get_covid_adjustment_factors <- function() {
  data.table::data.table(
    year = c(rep(2024L, 5), rep(2025L, 5)),
    age_min = rep(c(0L, 1L, 15L, 65L, 85L), 2),
    age_max = rep(c(0L, 14L, 64L, 84L, 119L), 2),
    covid_factor = c(
      # 2024
      1.01, 1.17, 0.99, 1.02, 0.98,
      # 2025
      1.00, 1.09, 1.00, 1.00, 1.00
    )
  )
}

#' Apply COVID-19 adjustment factors to qx
#'
#' @description
#' Applies COVID-19 adjustment factors to death probabilities.
#'
#' @param qx data.table with qx values (must have year, age, sex, qx columns)
#' @param covid_factors data.table from get_covid_adjustment_factors()
#'
#' @return data.table with adjusted qx
#'
#' @export
apply_covid_adjustments <- function(qx, covid_factors = NULL) {
  checkmate::assert_data_table(qx)
  checkmate::assert_names(names(qx), must.include = c("year", "age", "qx"))

  if (is.null(covid_factors)) {
    covid_factors <- get_covid_adjustment_factors()
  }

  dt <- data.table::copy(qx)
  covid_years <- unique(covid_factors$year)

  # Only adjust years that have COVID factors
  years_to_adjust <- intersect(unique(dt$year), covid_years)

  if (length(years_to_adjust) == 0) {
    cli::cli_alert_info("No COVID adjustment years in data")
    return(dt)
  }

  # Apply factors by age group for each COVID year
  for (yr in years_to_adjust) {
    factors_yr <- covid_factors[year == yr]

    for (i in seq_len(nrow(factors_yr))) {
      age_lo <- factors_yr$age_min[i]
      age_hi <- factors_yr$age_max[i]
      factor_val <- factors_yr$covid_factor[i]

      dt[year == yr & age >= age_lo & age <= age_hi, qx := qx * factor_val]
    }
  }

  # Cap qx at 1.0
  dt[qx > 1, qx := 1.0]

  cli::cli_alert_success("Applied COVID-19 adjustments for years: {paste(years_to_adjust, collapse=', ')}")

  dt
}

#' Run complete mortality projection pipeline
#'
#' @description
#' Executes the full SSA mortality projection methodology:
#' 1. Calculate historical mx and AAx
#' 2. Determine starting mx from regression fitted values
#' 3. Calculate AAx trajectory to ultimate values
#' 4. Project mx forward
#' 5. Overwrite with actual data (2020-2023)
#' 6. Apply WH smoothing
#' 7. Convert to qx
#' 8. Apply COVID adjustments
#'
#' @param deaths data.table with historical deaths by year, age, sex, [cause]
#' @param population data.table with historical population by year, age, sex
#' @param base_year Integer: last year for regression (default: 2019)
#' @param projection_end Integer: last projection year (default: 2100)
#' @param by_cause Logical: project by cause of death (default: FALSE for total)
#'
#' @return list with projected_mx, projected_qx, aax_trajectory, and life_tables
#'
#' @export
run_mortality_projection <- function(deaths,
                                      population,
                                      base_year = 2019,
                                      projection_end = 2100,
                                      by_cause = FALSE) {
  cli::cli_h1("SSA Mortality Projection Pipeline")

  # Step 1: Calculate historical mx
  cli::cli_h2("Step 1: Calculate historical central death rates")
  historical_years <- unique(deaths$year)
  historical_years <- historical_years[historical_years <= base_year]

  mx_historical <- calculate_central_death_rates(deaths[year %in% historical_years],
                                                  population[year %in% historical_years],
                                                  by_cause = by_cause)

  # Step 2: Apply WH smoothing to historical mx
  cli::cli_h2("Step 2: Apply Whittaker-Henderson smoothing")
  mx_smooth <- smooth_death_rates(mx_historical)

  # Step 3: Calculate AAx from 2008-2019
  cli::cli_h2("Step 3: Calculate AAx (2008-2019 regression)")
  aax <- calculate_annual_reduction_rates(mx_smooth,
                                           start_year = 2008,
                                           end_year = base_year,
                                           by_cause = by_cause)

  # Step 4: Calculate starting mx from regression
  cli::cli_h2("Step 4: Calculate starting mx from regression fitted values")
  starting_mx <- calculate_starting_mx(aax, base_year = base_year)

  # Step 5: Calculate AAx trajectory
  cli::cli_h2("Step 5: Calculate AAx trajectory to ultimate")
  projection_years <- (base_year + 1):projection_end
  aax_trajectory <- calculate_aax_trajectory(
    starting_aax = aax[, .(age, sex, starting_aax)],
    base_year = base_year,
    projection_years = projection_years
  )

  # Step 6: Project mx forward
  cli::cli_h2("Step 6: Project mx forward")
  # Get actual mx for 2020-2023 to overwrite projections
  actual_years <- unique(deaths$year)
  actual_years <- actual_years[actual_years > base_year]

  actual_mx <- NULL
  if (length(actual_years) > 0) {
    mx_actual <- calculate_central_death_rates(deaths[year %in% actual_years],
                                                population[year %in% actual_years],
                                                by_cause = by_cause)
    actual_mx <- smooth_death_rates(mx_actual)
  }

  projected_mx <- project_death_rates(starting_mx, aax_trajectory, actual_mx)

  # Step 7: Apply WH smoothing to projected mx
  cli::cli_h2("Step 7: Apply WH smoothing to projections")
  projected_mx_smooth <- projected_mx[, {
    if (.N >= 3) {
      mx_vals <- mx[order(age)]
      ages <- age[order(age)]
      in_range <- ages >= 2 & ages <= 99

      if (sum(in_range) >= 3) {
        smoothed <- whittaker_henderson_smooth(mx_vals[in_range])
        mx_vals[in_range] <- smoothed
      }
      .(age = ages, mx_smooth = mx_vals)
    } else {
      .(age = age, mx_smooth = mx)
    }
  }, by = .(year, sex)]

  # Step 8: Convert to qx
  cli::cli_h2("Step 8: Convert mx to qx")
  # Process each year
  qx_list <- list()
  for (yr in unique(projected_mx_smooth$year)) {
    mx_yr <- projected_mx_smooth[year == yr, .(age, sex, mx = mx_smooth)]
    qx_yr <- convert_mx_to_qx(mx_yr)
    qx_yr[, year := yr]
    qx_list[[as.character(yr)]] <- qx_yr
  }
  projected_qx <- data.table::rbindlist(qx_list, fill = TRUE)

  # Step 9: Apply COVID adjustments
  cli::cli_h2("Step 9: Apply COVID-19 adjustments")
  projected_qx <- apply_covid_adjustments(projected_qx)

  cli::cli_h1("Projection complete")

  list(
    projected_mx = projected_mx_smooth,
    projected_qx = projected_qx,
    aax_trajectory = aax_trajectory,
    starting_mx = starting_mx,
    historical_aax = aax
  )
}

#' Calculate starting mx values from regression fitted values
#'
#' @description
#' Calculates starting mx values from weighted regression fitted values,
#' not actual historical values. Per SSA methodology, projections start
#' from the regression-fitted mx at the base year.
#'
#' @param aax_results data.table output from calculate_annual_reduction_rates
#'   (must include intercept column)
#' @param base_year Integer: base year for starting mx (default: 2019)
#'
#' @return data.table with starting mx by age, sex, [cause]
#'
#' @details
#' Starting mx = exp(intercept + beta * base_year)
#' where intercept and beta come from the weighted regression.
#'
#' Since AAx = 1 - exp(beta), we have beta = log(1 - AAx).
#' Therefore: starting_mx = exp(intercept + log(1 - AAx) * base_year)
#'
#' @export
calculate_starting_mx <- function(aax_results, base_year = 2019) {
  checkmate::assert_data_table(aax_results)
  checkmate::assert_names(names(aax_results), must.include = c("age", "sex", "aax", "intercept"))
  checkmate::assert_int(base_year, lower = 1900, upper = 2100)

  dt <- data.table::copy(aax_results)

  # Calculate beta from AAx: since AAx = 1 - exp(beta), beta = log(1 - AAx)
  dt[, beta := log(1 - aax)]

  # Starting mx = exp(intercept + beta * base_year)
  dt[, starting_mx := exp(intercept + beta * base_year)]

  # Clean up
  dt[, beta := NULL]

  # Select output columns
  group_cols <- intersect(c("age", "sex", "cause"), names(dt))
  result <- dt[, c(group_cols, "starting_mx", "starting_aax"), with = FALSE]

  cli::cli_alert_success(
    "Calculated starting mx for base year {base_year}"
  )

  result
}

#' Convert central death rates to death probabilities
#'
#' @description
#' Converts mx to qx using the standard actuarial formula.
#'
#' @param mx data.table with central death rates (columns: age, sex, mx, and optionally year, cause)
#' @param max_age Integer: maximum age to extrapolate to (default: 119)
#'
#' @return data.table with qx values added
#'
#' @details
#' Per SSA 2025 Long-Range Model Documentation (Section 1.2.2):
#'
#' **Ages 2-99:**
#' qx = mx / (1 + 0.5*mx)
#'
#' **Ages 100-104 (transition to ultimate growth):**
#' qx = q_{x-1} * (q99/q98 * (104-x)/5 + growth_rate * (x-99)/5)
#'
#' where growth_rate = 1.05 for males, 1.06 for females
#'
#' **Ages 105+:**
#' qx = growth_rate * q_{x-1}
#'
#' where growth_rate = 1.05 for males, 1.06 for females
#'
#' **Age 0:** Special calculation using detailed infant mortality data
#' (deaths by age in days/weeks/months). If infant_data not provided,
#' uses simplified formula qx = mx / (1 + 0.5*mx).
#'
#' **Age 1:** Calculated from 4m1 (death rate ages 1-4) using historical
#' ratio q1/4m1. If not available, uses simplified formula.
#'
#' Female qx is capped at male qx if crossover occurs at very old ages.
#'
#' @export
convert_mx_to_qx <- function(mx, max_age = 119) {
  checkmate::assert_data_table(mx)
  checkmate::assert_names(names(mx), must.include = c("age", "sex", "mx"))

  dt <- data.table::copy(mx)

  # For ages 0-99: standard formula qx = mx / (1 + 0.5*mx)
  dt[age <= 99, qx := mx / (1 + 0.5 * mx)]

  # Define growth rates by sex
  growth_rates <- c(male = 1.05, female = 1.06)

  # Process ages 100+ for each sex separately using extrapolation from q98/q99
  results_list <- list()

  for (sex_val in c("male", "female")) {
    growth_rate <- growth_rates[sex_val]

    # Get q98 and q99 for this sex (these should be valid from mx calculation)
    q98_val <- dt[sex == sex_val & age == 98, qx][1]
    q99_val <- dt[sex == sex_val & age == 99, qx][1]

    if (!is.na(q98_val) && !is.na(q99_val) && q98_val > 0) {
      ratio <- q99_val / q98_val

      # Build qx series for ages 100+
      qx_series <- numeric(max_age - 99)
      prev_qx <- q99_val

      for (i in seq_along(qx_series)) {
        age_x <- 99 + i

        if (age_x <= 104) {
          # Transition formula for ages 100-104
          multiplier <- ratio * (104 - age_x) / 5 + growth_rate * (age_x - 99) / 5
        } else {
          # Simple growth formula for ages 105+
          multiplier <- growth_rate
        }

        new_qx <- prev_qx * multiplier
        qx_series[i] <- pmin(new_qx, 1.0)
        prev_qx <- qx_series[i]
      }

      # Create data for ages 100+
      ages_100_plus <- data.table::data.table(
        age = 100:max_age,
        sex = sex_val,
        qx = qx_series
      )
      results_list[[sex_val]] <- ages_100_plus
    }
  }

  # Combine ages 100+ results
  if (length(results_list) > 0) {
    dt_100_plus <- data.table::rbindlist(results_list)

    # Remove any existing ages 100+ from dt (they have NA or bad mx)
    dt <- dt[age < 100]

    # Add the extrapolated ages 100+
    # First, get columns from dt to match
    common_cols <- intersect(names(dt), names(dt_100_plus))
    dt_100_plus <- dt_100_plus[, ..common_cols]

    # Add any missing columns with NA
    for (col in setdiff(names(dt), names(dt_100_plus))) {
      dt_100_plus[, (col) := NA]
    }

    dt <- data.table::rbindlist(list(dt, dt_100_plus), fill = TRUE)
  }

  # Female qx capped at male qx if crossover occurs at very old ages
  male_qx_lookup <- dt[sex == "male", .(age, male_qx = qx)]
  if (nrow(male_qx_lookup) > 0 && any(dt$sex == "female")) {
    dt <- merge(dt, male_qx_lookup, by = "age", all.x = TRUE)
    dt[sex == "female" & !is.na(male_qx) & !is.na(qx), qx := pmin(qx, male_qx)]
    dt[, male_qx := NULL]
  }

  data.table::setorder(dt, sex, age)

  cli::cli_alert_success("Converted mx to qx for {nrow(dt)} records (ages 0-{max_age})")

  dt
}

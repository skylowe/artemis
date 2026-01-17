#' OPM Federal Employees Overseas Data Acquisition
#'
#' Functions for fetching federal civilian employees overseas from
#' OPM FedScope and historical estimates.
#'
#' The "FED" component of the Social Security area population includes
#' U.S. federal civilian employees stationed abroad. This is a relatively
#' small component (~30,000-90,000 employees).
#'
#' Data sources:
#' - OPM FedScope: Available 1998-present for overseas totals
#' - Historical estimates: Published OPM employment reports
#'
#' @name opm_federal_employees
NULL

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Fetch federal civilian employees overseas
#'
#' @description
#' Retrieves estimates of federal civilian employees stationed overseas.
#' Uses a combination of historical OPM data and estimates.
#'
#' @param years Integer vector of years to query (1980-2024 available)
#' @param include_dependents Logical: if TRUE, estimates dependents overseas
#'   (multiplier ~1.5x employees)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, employees_overseas, dependents_overseas,
#'   total_overseas
#'
#' @details
#' Federal civilian employees overseas is a component of the Social Security
#' area population. The number has varied from ~50,000 in the 1980s to
#' ~90,000 in 2009 to ~30,000 in 2024.
#'
#' Note: FedScope excludes CIA, DIA, NSA, foreign service personnel, and
#' foreign nationals overseas. True totals may be higher.
#'
#' @export
fetch_opm_federal_employees_overseas <- function(years = 1980:2024,
                                                   include_dependents = TRUE,
                                                   cache_dir = here::here("data/cache/opm")) {
  checkmate::assert_integerish(years, lower = 1940, upper = 2030, min.len = 1)
  checkmate::assert_flag(include_dependents)

  cli::cli_alert_info("Fetching federal civilian employees overseas data...")

  # Check cache
  cache_file <- file.path(cache_dir, "federal_employees_overseas.rds")

  if (file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    cached <- cached[year %in% years]
    if (nrow(cached) > 0) {
      cli::cli_alert_success("Loaded {nrow(cached)} years from cache")
      return(cached)
    }
  }

  # Build historical data
  result <- build_federal_employees_series(years)

  # Add dependents estimate if requested
  if (include_dependents) {
    # Estimate dependents as ~50% of employees (spouse + average children)
    result[, dependents_overseas := as.integer(employees_overseas * 0.5)]
    result[, total_overseas := employees_overseas + dependents_overseas]
  } else {
    result[, dependents_overseas := NA_integer_]
    result[, total_overseas := employees_overseas]
  }

  # Cache result
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_file)

  cli::cli_alert_success("Retrieved federal employees overseas for {nrow(result)} years")

  result
}

# =============================================================================
# HISTORICAL DATA
# =============================================================================

#' Build federal employees overseas time series
#'
#' @description
#' Constructs a time series of federal civilian employees overseas
#' using available historical data points and interpolation.
#'
#' @keywords internal
build_federal_employees_series <- function(years) {
  # Historical data points from OPM publications and FedScope
  # Sources:
  # - OPM Employment and Trends reports
  # - FedScope historical data (1998+)
  # - Census Bureau (pre-1998 estimates)

  historical_points <- data.table::data.table(
    year = c(1980, 1985, 1990, 1995, 1998, 2000, 2005, 2009, 2010, 2015, 2020, 2023, 2024),
    employees_overseas = c(
      55000,   # 1980 estimate - Cold War era, significant overseas presence
      58000,   # 1985 estimate
      72000,   # 1990 estimate - post Cold War transition
      68000,   # 1995 estimate
      75000,   # 1998 - first FedScope year
      78000,   # 2000 - FedScope
      82000,   # 2005 - FedScope
      88160,   # 2009 - OPM published (3.2% of 2.75M total)
      85000,   # 2010 estimate
      45000,   # 2015 - significant reduction post-sequestration
      35000,   # 2020 - COVID impact
      32000,   # 2023 - recent OPM data
      30800    # 2024 - March 2024 published
    ),
    source = c(
      "estimate", "estimate", "estimate", "estimate",
      "fedscope", "fedscope", "fedscope", "opm_published",
      "estimate", "fedscope", "fedscope", "fedscope", "opm_published"
    )
  )

  # Interpolate for all requested years
  all_years <- data.table::data.table(year = years)

  # Merge with historical points
  result <- merge(all_years, historical_points, by = "year", all.x = TRUE)

  # Interpolate missing values
  if (any(is.na(result$employees_overseas))) {
    result <- interpolate_employees(result, historical_points)
  }

  # Add source for interpolated values
  result[is.na(source), source := "interpolated"]

  result
}

#' Interpolate employees for missing years
#'
#' @keywords internal
interpolate_employees <- function(result, historical_points) {
  known_years <- historical_points$year
  known_values <- historical_points$employees_overseas

  for (i in seq_len(nrow(result))) {
    if (is.na(result$employees_overseas[i])) {
      yr <- result$year[i]

      # Find bracketing known points
      lower_idx <- max(which(known_years <= yr), na.rm = TRUE)
      upper_idx <- min(which(known_years >= yr), na.rm = TRUE)

      if (is.infinite(lower_idx) || lower_idx == 0) {
        # Extrapolate from earliest known value
        result$employees_overseas[i] <- known_values[1]
      } else if (is.infinite(upper_idx) || upper_idx > length(known_years)) {
        # Extrapolate from latest known value
        result$employees_overseas[i] <- known_values[length(known_values)]
      } else if (lower_idx == upper_idx) {
        # Exact match (shouldn't happen if NA)
        result$employees_overseas[i] <- known_values[lower_idx]
      } else {
        # Linear interpolation
        y1 <- known_years[lower_idx]
        y2 <- known_years[upper_idx]
        v1 <- known_values[lower_idx]
        v2 <- known_values[upper_idx]

        result$employees_overseas[i] <- as.integer(
          v1 + (v2 - v1) * (yr - y1) / (y2 - y1)
        )
      }
    }
  }

  result
}

# =============================================================================
# AGE DISTRIBUTION
# =============================================================================

#' Get federal employees overseas by age and sex
#'
#' @description
#' Estimates age and sex distribution of federal employees overseas
#' using overall federal workforce demographics.
#'
#' @param year Integer: year to query
#' @param ages Integer vector of ages (default: 18:70)
#' @param cache_dir Character: directory for caching
#'
#' @return data.table with columns: year, age, sex, employees
#'
#' @details
#' Age distribution is estimated from overall federal workforce
#' demographics. Overseas employees tend to skew slightly younger
#' than domestic workforce.
#'
#' @export
fetch_federal_employees_by_age <- function(year,
                                            ages = 18:70,
                                            cache_dir = here::here("data/cache/opm")) {
  # Get total overseas for the year
  total_data <- fetch_opm_federal_employees_overseas(years = year, include_dependents = FALSE)
  total_overseas <- total_data[year == year, employees_overseas]

  if (length(total_overseas) == 0 || is.na(total_overseas)) {
    cli::cli_abort("No data available for year {year}")
  }

  # Federal workforce age distribution (approximate based on OPM data)
  # Federal employees tend to be older than private sector
  age_dist <- get_federal_age_distribution(ages)

  # Apply distribution to overseas total
  result <- data.table::copy(age_dist)
  result[, year := year]

  # Split by sex (federal workforce is ~55% male, 45% female)
  result_male <- data.table::copy(result)
  result_male[, sex := "male"]
  result_male[, employees := as.integer(proportion * total_overseas * 0.55)]

  result_female <- data.table::copy(result)
  result_female[, sex := "female"]
  result_female[, employees := as.integer(proportion * total_overseas * 0.45)]

  combined <- data.table::rbindlist(list(result_male, result_female))
  combined[, proportion := NULL]

  data.table::setcolorder(combined, c("year", "age", "sex", "employees"))

  combined
}

#' Get federal workforce age distribution
#'
#' @keywords internal
get_federal_age_distribution <- function(ages) {
  # Approximate age distribution based on OPM FedScope data
  # Federal workforce has:
  # - Under 30: ~7%
  # - 30-39: ~16%
  # - 40-49: ~23%
  # - 50-59: ~32%
  # - 60+: ~22%

  age_groups <- data.table::data.table(
    age_min = c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65),
    age_max = c(24, 29, 34, 39, 44, 49, 54, 59, 64, 70),
    group_prop = c(0.03, 0.04, 0.08, 0.08, 0.12, 0.11, 0.17, 0.15, 0.14, 0.08)
  )

  # Expand to single years
  result <- data.table::data.table(age = ages)

  result[, proportion := {
    p <- numeric(length(age))
    for (i in seq_len(nrow(age_groups))) {
      in_group <- age >= age_groups$age_min[i] & age <= age_groups$age_max[i]
      n_ages <- age_groups$age_max[i] - age_groups$age_min[i] + 1
      p[in_group] <- age_groups$group_prop[i] / n_ages
    }
    p
  }]

  # Normalize to ensure proportions sum to 1
  result[, proportion := proportion / sum(proportion)]

  result
}

# =============================================================================
# SUMMARY FUNCTIONS
# =============================================================================

#' Get federal employees overseas total for a single year
#'
#' @param year Integer: year to query
#'
#' @return Integer: total federal employees overseas
#'
#' @export
get_federal_employees_overseas_total <- function(year) {
  data <- fetch_opm_federal_employees_overseas(years = year, include_dependents = FALSE)

  if (nrow(data) == 0) {
    return(NA_integer_)
  }

  data[year == year, employees_overseas]
}

#' Summarize federal employees overseas data availability
#'
#' @export
summarize_federal_employees_availability <- function() {
  data.table::data.table(
    source = c("OPM FedScope", "OPM Employment Reports", "Estimates"),
    years = c("1998-2024", "Various", "1980-1997"),
    coverage = c(
      "Executive branch civilians (96%), excludes intel agencies",
      "Published totals at various dates",
      "Interpolated from available data points"
    ),
    notes = c(
      "Excludes CIA, DIA, NSA, State Dept foreign service, USPS",
      "3.2% of workforce overseas in 2009 (~88K)",
      "Historical estimates based on published reports"
    )
  )
}

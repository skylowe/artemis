#' Fertility Subprocess
#'
#' Functions for calculating historical birth rates and projecting future
#' age-specific fertility rates using the SSA methodology.
#'
#' @name fertility
NULL

#' Calculate historical age-specific birth rates
#'
#' @description
#' Computes age-specific birth rates from NCHS birth counts and Census
#' female population estimates. Rate = Births / Female Population.
#'
#' @param births data.table with columns: year, age, births
#' @param population data.table with columns: year, age, population
#' @param min_age Integer: minimum fertility age (default: 14)
#' @param max_age Integer: maximum fertility age (default: 49)
#'
#' @return data.table with columns: year, age, births, population, birth_rate
#'
#' @details
#' Birth rates are calculated as births per woman (not per 1,000).
#' Ages outside min_age:max_age are collapsed into the boundary ages
#' (births at age <14 counted at 14, births at age >49 counted at 49).
#'
#' @export
calculate_historical_birth_rates <- function(births, population,
                                              min_age = 14, max_age = 49) {
  # Validate inputs
  checkmate::assert_data_table(births)
  checkmate::assert_data_table(population)
  checkmate::assert_names(names(births), must.include = c("year", "age", "births"))
  checkmate::assert_names(names(population), must.include = c("year", "age", "population"))

  # Copy to avoid modifying originals
  b <- data.table::copy(births)
  p <- data.table::copy(population)

  # Collapse ages outside fertility range
  # Births at ages < min_age are attributed to min_age
  # Births at ages > max_age are attributed to max_age
  b[age < min_age, age := min_age]
  b[age > max_age, age := max_age]
  b <- b[, .(births = sum(births)), by = .(year, age)]

  # Filter population to fertility ages

  p <- p[age >= min_age & age <= max_age]

  # Join births and population
  dt <- merge(b, p, by = c("year", "age"), all = FALSE)

  # Calculate birth rate (births per woman, not per 1,000)
  dt[, birth_rate := births / population]

  # Order by year and age
  data.table::setorder(dt, year, age)

  cli::cli_alert_success("Calculated birth rates for {length(unique(dt$year))} years")

  dt
}

#' Calculate ratios to age 30 birth rate
#'
#' @description
#' Computes the ratio of each age's birth rate to the age 30 rate.
#' These ratios capture the shape of the fertility schedule.
#'
#' @param birth_rates data.table with columns: year, age, birth_rate
#'
#' @return data.table with columns: year, age, birth_rate, rate_age30, ratio_to_30
#'
#' @details
#' Formula: r_x^z = b_x^z / b_30^z
#'
#' Age 30 is used as the reference because it is typically near the peak
#' of the fertility schedule and relatively stable over time.
#'
#' @export
calculate_age30_ratios <- function(birth_rates) {
  checkmate::assert_data_table(birth_rates)
  checkmate::assert_names(names(birth_rates), must.include = c("year", "age", "birth_rate"))

  dt <- data.table::copy(birth_rates)

  # Extract age 30 rate for each year
  age30_rates <- dt[age == 30, .(year, rate_age30 = birth_rate)]

  # Merge back to full data
  dt <- merge(dt, age30_rates, by = "year", all.x = TRUE)

  # Calculate ratio
  dt[, ratio_to_30 := birth_rate / rate_age30]

  data.table::setorder(dt, year, age)

  dt
}

#' Calculate year-over-year trend factors
#'
#' @description
#' Computes how the age-to-age-30 ratios change from year to year.
#' These factors capture trends in the age pattern of fertility.
#'
#' @param ratios data.table with columns: year, age, ratio_to_30
#' @param exclude_years Integer vector of years to exclude (default: 1997)
#'
#' @return data.table with columns: age, avg_trend_factor (a_x)
#'
#' @details
#' Formula: p_x^z = r_x^z / r_x^{z-1}
#' The average a_x is computed over all years except excluded ones.
#'
#' 1997 is excluded because NCHS changed their age imputation method
#' for low and high maternal ages that year.
#'
#' @export
calculate_trend_factors <- function(ratios, exclude_years = 1997) {
  checkmate::assert_data_table(ratios)
  checkmate::assert_names(names(ratios), must.include = c("year", "age", "ratio_to_30"))

  dt <- data.table::copy(ratios)

  # Calculate year-over-year ratio change
  data.table::setorder(dt, age, year)
  dt[, prior_ratio := data.table::shift(ratio_to_30, 1), by = age]
  dt[, p_factor := ratio_to_30 / prior_ratio]

  # Remove first year (no prior) and excluded years
  dt <- dt[!is.na(prior_ratio) & !(year %in% exclude_years)]

  # Calculate average trend factor by age
  avg_factors <- dt[, .(avg_trend_factor = mean(p_factor, na.rm = TRUE)), by = age]

  data.table::setorder(avg_factors, age)

  avg_factors
}

#' Calculate ultimate years for each age
#'
#' @description
#' Determines when each age reaches its ultimate (constant) birth rate.
#' Younger ages reach ultimate faster; older ages take longer.
#'
#' @param min_age Integer: minimum fertility age (default: 14)
#' @param max_age Integer: maximum fertility age (default: 49)
#' @param base_year Integer: projection start year (default: 2025)
#' @param end_year Integer: year when max_age reaches ultimate (default: 2050)
#'
#' @return data.table with columns: age, ultimate_year
#'
#' @details
#' Formula: u_x = base_year + (x - min_age) * (end_year - base_year) / (max_age - min_age)
#'
#' This creates a linear interpolation where:
#' - Age 14 reaches ultimate in base_year (2025)
#' - Age 30 reaches ultimate in ~2036
#' - Age 49 reaches ultimate in end_year (2050)
#'
#' @export
calculate_ultimate_years <- function(min_age = 14, max_age = 49,
                                      base_year = 2025, end_year = 2050) {
  ages <- min_age:max_age

  ultimate_years <- base_year + (ages - min_age) * (end_year - base_year) / (max_age - min_age)
  ultimate_years <- round(ultimate_years)

  data.table::data.table(age = ages, ultimate_year = ultimate_years)
}

#' Calculate interpolation weights
#'
#' @description
#' Computes weights for blending current rates toward ultimate rates.
#' Uses a power function to create gradual transition.
#'
#' @param years Integer vector of projection years
#' @param base_year Integer: rate base year (default: 2024)
#' @param ultimate_year Integer: year age 30 reaches ultimate (default: 2036)
#' @param exponent Numeric: weight function exponent (default: 1.5)
#'
#' @return data.table with columns: year, weight
#'
#' @details
#' Formula: w^z = 1 - ((ultimate_year - z) / (ultimate_year - base_year))^exponent
#'
#' Exponent > 1 = front-loaded (faster initial change, slower approach to ultimate).
#' Exponent < 1 = back-loaded (slower initial change, faster near ultimate).
#' Weight = 0 at base year, weight = 1 at ultimate year.
#'
#' @export
calculate_interpolation_weights <- function(years,
                                             base_year = 2024,
                                             ultimate_year = 2036,
                                             exponent = 1.5) {
  weights <- 1 - ((ultimate_year - years) / (ultimate_year - base_year))^exponent
  weights <- pmax(0, pmin(1, weights))  # Clamp to [0, 1]

  data.table::data.table(year = years, weight = weights)
}

#' Solve for ultimate age 30 birth rate
#'
#' @description
#' Finds the age 30 birth rate in the ultimate year that produces
#' the target period TFR at the ultimate year (e.g., 2050).
#'
#' @param target_ctfr Numeric: target period TFR at ultimate year (default: 1.90)
#' @param base_age30_rate Numeric: age 30 rate in base year (2024)
#' @param base_ratios data.table with age-to-30 ratios in base year
#' @param trend_factors data.table with average trend factors by age
#' @param ultimate_years data.table with ultimate years by age
#' @param base_year Integer: base year (default: 2024)
#' @param overall_ultimate_year Integer: year when all ages have reached ultimate (default: 2050)
#'
#' @return Numeric: ultimate age 30 birth rate
#'
#' @details
#' At the overall ultimate year (2050), all ages have reached their ultimate rates
#' and the period TFR should equal the target (1.90).
#'
#' At ultimate year:
#'   TFR = sum_x(b_x) = sum_x(b_30 * r_x) = b_30 * sum_x(r_x)
#'
#' Therefore:
#'   b_30^{ultimate} = target_TFR / sum_x(r_x^{ultimate})
#'
#' Where r_x^{ultimate} is the ratio for each age at the overall ultimate year,
#' computed by projecting base ratios forward using trend factors.
#'
#' @export
solve_ultimate_age30_rate <- function(target_ctfr,
                                       base_age30_rate,
                                       base_ratios,
                                       trend_factors,
                                       ultimate_years,
                                       base_year = 2024,
                                       overall_ultimate_year = 2050) {
  # Merge base ratios with trend factors and ultimate years
  dt <- merge(base_ratios[, .(age, ratio_to_30)], trend_factors, by = "age")
  dt <- merge(dt, ultimate_years, by = "age")

  # Calculate years from base to overall ultimate year (2050)
  # Note: Each age's ratio evolves with trend factors until its individual ultimate year,

  # then stays constant. At the overall ultimate year (2050), all ages have stabilized.
  dt[, years_to_own_ultimate := ultimate_year - base_year]

  # Calculate projected ratio at overall ultimate year (2050)
  # The ratio evolves until each age's individual ultimate year, then is constant
  # So at 2050, each age's ratio is: r_x^{base} * a_x^{years_to_own_ultimate}
  dt[, ratio_at_ultimate := ratio_to_30 * (avg_trend_factor ^ years_to_own_ultimate)]

  # At overall ultimate year, period TFR = b_30 * sum(ratios)
  # So: b_30 = target_TFR / sum(ratios)
  sum_ratios <- sum(dt$ratio_at_ultimate)
  ultimate_age30_rate <- target_ctfr / sum_ratios

  cli::cli_alert_success("Solved ultimate age 30 rate: {round(ultimate_age30_rate, 6)}")

  ultimate_age30_rate
}

#' Project age 30 birth rates
#'
#' @description
#' Interpolates age 30 rates from base year to ultimate year.
#'
#' @param years Integer vector of projection years
#' @param base_rate Numeric: age 30 rate in base year
#' @param ultimate_rate Numeric: age 30 rate in ultimate year
#' @param weights data.table with interpolation weights by year
#'
#' @return data.table with columns: year, age30_rate
#'
#' @details
#' Formula: b_30^z = b_30^{base} * (1 - w^z) + b_30^{ultimate} * w^z
#'
#' @export
project_age30_rates <- function(years, base_rate, ultimate_rate, weights) {
  dt <- merge(data.table::data.table(year = years), weights, by = "year", all.x = TRUE)

  # Fill in weights for years beyond ultimate (weight = 1)
  dt[is.na(weight), weight := 1]

  dt[, age30_rate := base_rate * (1 - weight) + ultimate_rate * weight]

  dt[, .(year, age30_rate)]
}

#' Project birth rates for all ages
#'
#' @description
#' Computes projected birth rates for all ages using the projected
#' age 30 rates and evolving age ratios.
#'
#' @param years Integer vector of projection years
#' @param age30_rates data.table with projected age 30 rates by year
#' @param base_ratios data.table with age-to-30 ratios in base year
#' @param trend_factors data.table with trend factors by age
#' @param ultimate_years data.table with ultimate years by age
#'
#' @return data.table with columns: year, age, birth_rate
#'
#' @details
#' Method:
#' 1. Project r_x^z forward using trend factors until ultimate year
#' 2. After ultimate year, r_x^z stays constant
#' 3. Calculate: b_x^z = b_30^z * r_x^z
#'
#' @export
project_birth_rates <- function(years,
                                 age30_rates,
                                 base_ratios,
                                 trend_factors,
                                 ultimate_years) {
  # Get ages from base ratios
  ages <- sort(unique(base_ratios$age))
  base_year <- min(years) - 1  # Assume projection starts year after base

  # Create grid of all year-age combinations
  grid <- data.table::CJ(year = years, age = ages)

  # Merge in age30 rates
  grid <- merge(grid, age30_rates, by = "year")

  # Merge in base ratios and trend factors
  grid <- merge(grid, base_ratios[, .(age, base_ratio = ratio_to_30)], by = "age")
  grid <- merge(grid, trend_factors, by = "age")
  grid <- merge(grid, ultimate_years, by = "age")

  # Calculate projected ratio for each year-age
  # Years from base to current year (capped at ultimate)
  grid[, years_from_base := pmin(year - base_year, ultimate_year - base_year)]

  # Apply trend factor for those years
  grid[, ratio_to_30 := base_ratio * (avg_trend_factor^years_from_base)]

  # Calculate birth rate
  grid[, birth_rate := age30_rate * ratio_to_30]

  # Order and select columns
  data.table::setorder(grid, year, age)

  grid[, .(year, age, birth_rate)]
}

#' Calculate period and cohort total fertility rates
#'
#' @description
#' Computes TFR (period) and CTFR (cohort) from age-specific rates.
#'
#' @param birth_rates data.table with columns: year, age, birth_rate
#'
#' @return list with:
#'   - tfr: data.table with period TFR by year
#'   - ctfr: data.table with cohort TFR by birth year
#'
#' @export
calculate_fertility_totals <- function(birth_rates) {
  dt <- data.table::copy(birth_rates)

 # Period TFR: sum of rates within each year
  tfr <- dt[, .(tfr = sum(birth_rate)), by = year]

  # Cohort TFR: sum of rates across lifetime of each cohort
  # cohort birth year = calendar year - mother's age
  dt[, cohort_year := year - age]
  ctfr <- dt[, .(ctfr = sum(birth_rate)), by = cohort_year]
  data.table::setnames(ctfr, "cohort_year", "birth_year")

  list(tfr = tfr[order(year)], ctfr = ctfr[order(birth_year)])
}

#' Calculate TR2025 implied births from population and mortality data
#'
#' @description
#' Derives the number of births TR2025 assumed by working backwards from their
#' age 0 population (Dec 31) and infant mortality rates.
#'
#' Formula: implied_births = age_0_population / (1 - q0)
#'
#' This is needed because TR2025 assumed higher births for 2023-2024 than
#' actual NCHS data shows. The TR2025 was published before final NCHS data
#' was available.
#'
#' @param years Integer vector: years to calculate implied births for
#' @param tr_population_file Character: path to TR2025 SSPopDec file
#' @param tr_qx_male_file Character: path to TR2025 male death probabilities
#' @param tr_qx_female_file Character: path to TR2025 female death probabilities
#'
#' @return data.table with columns: year, sex, births
#'
#' @export
calculate_tr2025_implied_births <- function(
    years,
    tr_population_file = "data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv",
    tr_qx_male_file = "data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv",
    tr_qx_female_file = "data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv"
) {

  # Load TR2025 data
  tr_pop <- data.table::fread(tr_population_file)
  tr_qx_m <- data.table::fread(tr_qx_male_file)
  tr_qx_f <- data.table::fread(tr_qx_female_file)

  results <- data.table::rbindlist(lapply(years, function(yr) {
    # Get age 0 population by sex
    age0_m <- tr_pop[Year == yr & Age == 0, `M Tot`]
    age0_f <- tr_pop[Year == yr & Age == 0, `F Tot`]

    if (length(age0_m) == 0 || length(age0_f) == 0) {
      cli::cli_alert_warning("No TR2025 population data for year {yr}")
      return(NULL)
    }

    # Get infant mortality (q0) by sex
    q0_m <- tr_qx_m[Year == yr, `0`]
    q0_f <- tr_qx_f[Year == yr, `0`]

    if (length(q0_m) == 0 || length(q0_f) == 0) {
      cli::cli_alert_warning("No TR2025 mortality data for year {yr}")
      return(NULL)
    }

    # Calculate implied births: survivors = births * (1 - q0)
    # Therefore: births = survivors / (1 - q0)
    births_m <- age0_m / (1 - q0_m)
    births_f <- age0_f / (1 - q0_f)

    data.table::data.table(
      year = c(yr, yr),
      sex = c("male", "female"),
      births = c(births_m, births_f)
    )
  }))

  if (nrow(results) > 0) {
    cli::cli_alert_success(
      "Calculated TR2025 implied births for {length(years)} year(s): {paste(years, collapse=', ')}"
    )
  }

  results
}

#' Substitute NCHS births with TR2025 implied births for specified years
#'
#' @description
#' Replaces NCHS birth counts with TR2025's implied births for specified years.
#' This allows the model to match TR2025's population projections which were
#' based on birth assumptions that differed from actual NCHS data.
#'
#' @param nchs_births data.table: NCHS births by year and age (from nchs_births_raw)
#' @param tr2025_births data.table: TR2025 implied births by year and sex
#' @param substitute_years Integer vector: years to substitute (e.g., c(2023, 2024))
#'
#' @return data.table with substituted birth counts
#'
#' @details
#' The substitution preserves the age distribution from NCHS data but scales
#' the total to match TR2025. This is done because TR2025 only provides total
#' births (by sex), not births by mother's age.
#'
#' @export
substitute_tr2025_births <- function(nchs_births, tr2025_births, substitute_years) {

  if (is.null(substitute_years) || length(substitute_years) == 0) {
    cli::cli_alert_info("No birth substitution years specified - using NCHS data as-is")
    return(nchs_births)
  }

  dt <- data.table::copy(nchs_births)

  for (yr in substitute_years) {
    # Get TR2025 total births for this year
    tr_total <- tr2025_births[year == yr, sum(births)]

    if (length(tr_total) == 0 || is.na(tr_total)) {
      cli::cli_alert_warning("No TR2025 births for year {yr} - keeping NCHS data")
      next
    }

    # Get NCHS total births for this year
    nchs_total <- dt[year == yr, sum(births)]

    if (nchs_total == 0) {
      cli::cli_alert_warning("No NCHS births for year {yr} - cannot substitute")
      next
    }

    # Calculate scaling factor
    scale_factor <- tr_total / nchs_total

    # Apply scaling (preserves age distribution)
    dt[year == yr, births := births * scale_factor]

    cli::cli_alert_info(
      "Year {yr}: Substituted TR2025 births ({format(round(tr_total), big.mark=',')} vs NCHS {format(round(nchs_total), big.mark=',')}, scale={round(scale_factor, 4)})"
    )
  }

  dt
}

#' Constrain birth rates to target TFR for specified years
#'
#' @description
#' Scales all age-specific birth rates proportionally for specified years so that
#' the resulting TFR equals the target value. This matches TR2025's approach of
#' constraining recent years to match their published TFR values.
#'
#' @param birth_rates data.table with columns: year, age, birth_rate
#' @param constrain_tfr Named list or vector mapping years to target TFRs.
#'   Example: list("2023" = 1.62, "2024" = 1.62) or c("2023" = 1.62, "2024" = 1.62)
#'   Set to NULL to disable constraints.
#'
#' @return data.table with constrained birth rates for the specified years
#'
#' @details
#' The scaling preserves the relative age pattern of fertility while adjusting
#' the overall level to hit the target TFR for each constrained year.
#'
#' Formula: b_x^{constrained} = b_x^{calculated} * (target_TFR / calculated_TFR)
#'
#' Per TR2025 documentation (Section 1.1.c, Step 1):
#' "calculate estimated 2024 births rates... using the estimated total fertility
#' of 1.62 using selected state data births and residential populations"
#'
#' @export
constrain_tfr_for_years <- function(birth_rates, constrain_tfr) {

  if (is.null(constrain_tfr) || length(constrain_tfr) == 0) {
    cli::cli_alert_info("No TFR constraints specified - using calculated rates")
    return(birth_rates)
  }

  dt <- data.table::copy(birth_rates)

  # Convert to named list if needed
  if (!is.list(constrain_tfr)) {
    constrain_tfr <- as.list(constrain_tfr)
  }

  # Process each year

  for (yr_char in names(constrain_tfr)) {
    yr <- as.integer(yr_char)
    target_tfr <- constrain_tfr[[yr_char]]

    if (is.null(target_tfr) || is.na(target_tfr)) {
      next
    }

    # Calculate current TFR for this year
    year_rates <- dt[year == yr]
    if (nrow(year_rates) == 0) {
      cli::cli_alert_warning("No rates for year {yr} - cannot apply TFR constraint")
      next
    }

    calculated_tfr <- sum(year_rates$birth_rate, na.rm = TRUE)

    if (calculated_tfr == 0) {
      cli::cli_alert_warning("Calculated TFR is zero for year {yr} - cannot apply constraint")
      next
    }

    # Calculate scaling factor
    scale_factor <- target_tfr / calculated_tfr

    # Apply scaling to this year's rates
    dt[year == yr, birth_rate := birth_rate * scale_factor]

    # Verify the constraint was applied correctly
    new_tfr <- dt[year == yr, sum(birth_rate, na.rm = TRUE)]

    cli::cli_alert_success(
      "Constrained {yr} TFR: {round(calculated_tfr, 4)} -> {round(new_tfr, 4)} (target: {target_tfr}, scale: {round(scale_factor, 4)})"
    )
  }

  dt
}

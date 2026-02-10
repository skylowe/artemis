#' Mortality Subprocess
#'
#' Functions for calculating historical death rates, projecting future mortality,
#' and computing life tables using the SSA methodology.
#'
#' NOTE (Deviation #8): TR2025 Section 1.2.b lists extensive historical data
#' going back to 1900 (Items 2, 5, 12-16, 29). ARTEMIS only processes data
#' from 1968+ (deaths) and 1980+ (Census population). Pre-1968 data is only
#' used for historical life tables and has zero impact on the mortality
#' projection which uses 2008-2019 regression and projects from 2019 onward.
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
#' @param max_age Integer: maximum age to include. Ages above this are aggregated
#'   into max_age (e.g., ages 100+ -> age 100). Default: 100.
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
#' Ages above max_age are aggregated into max_age before calculating rates.
#' This is necessary because Census population data only provides ages 0-100
#' (with 100 representing 100+), while NCHS death data has individual ages
#' above 100.
#'
#' @export
calculate_central_death_rates <- function(deaths, population, by_cause = TRUE,
                                          max_age = 100) {
  # Validate inputs
  checkmate::assert_data_table(deaths)
  checkmate::assert_data_table(population)
  checkmate::assert_names(names(deaths), must.include = c("year", "age", "sex", "deaths"))
  checkmate::assert_names(names(population), must.include = c("year", "age", "sex", "population"))
  checkmate::assert_flag(by_cause)
  checkmate::assert_int(max_age, lower = 1, upper = 150)

  # Copy to avoid modifying originals
  d <- data.table::copy(deaths)
  p <- data.table::copy(population)

  # Aggregate ages above max_age into max_age (e.g., ages 100+ -> age 100)
  # This is necessary because Census population only has 100+ as a single category
  # while NCHS deaths has individual ages above 100
  d[age > max_age, age := max_age]
  p[age > max_age, age := max_age]

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

  # Aggregate deaths by grouping columns (this now includes 100+ aggregation)
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

# =============================================================================
# Starting AAx Method Functions
# =============================================================================

#' Apply capped starting AAx method
#'
#' @description
#' Caps starting AAx values at a configurable multiple of ultimate AAx.
#' This prevents unrealistically high starting improvement rates while
#' preserving the relative pattern across ages.
#'
#' @param starting_aax data.table with starting AAx by age, sex, [cause]
#' @param ultimate_aax data.table from get_ultimate_aax_assumptions()
#' @param cap_multiplier Numeric: cap starting AAx at this multiple of ultimate (default: 1.5)
#'
#' @return data.table with capped starting_aax values
#'
#' @details
#' For each age-sex combination:
#' - If starting_aax > ultimate_aax * cap_multiplier: cap at ultimate * multiplier
#' - If starting_aax <= 0: use ultimate_aax (no negative improvement)
#' - Otherwise: keep original starting_aax
#'
#' @export
apply_capped_starting_aax <- function(starting_aax,
                                       ultimate_aax = NULL,
                                       cap_multiplier = 1.5) {
  checkmate::assert_data_table(starting_aax)
  checkmate::assert_number(cap_multiplier, lower = 1.0, upper = 10.0)

  if (is.null(ultimate_aax)) {
    ultimate_aax <- get_ultimate_aax_assumptions()
  }

  dt <- data.table::copy(starting_aax)

  # Determine if we have cause-specific data
  has_cause <- "cause" %in% names(dt)

  # Map ages to ultimate age groups
  dt[, age_group := map_age_to_ultimate_group(age)]

  # Get cause-weighted ultimate AAx by age group and sex
  cause_props <- tryCatch(
    get_cause_of_death_proportions(),
    error = function(e) {
      cli::cli_alert_warning("Could not load cause-of-death proportions, using simple average")
      NULL
    }
  )

  if (!is.null(cause_props)) {
    # Merge proportions with ultimate AAx values
    weighted_data <- merge(
      ultimate_aax[, .(age_group, cause, ultimate_aax)],
      cause_props,
      by = c("age_group", "cause"),
      all.x = TRUE
    )
    weighted_data[is.na(proportion), proportion := 1/6]

    # Calculate weighted average by age_group and sex
    ultimate_avg <- weighted_data[, .(
      ultimate_aax_weighted = sum(ultimate_aax * proportion)
    ), by = .(age_group, sex)]
  } else {
    # Simple average fallback
    ultimate_avg <- ultimate_aax[, .(ultimate_aax_weighted = mean(ultimate_aax)), by = age_group]
  }

  # Merge ultimate values
  if ("sex" %in% names(ultimate_avg) && "sex" %in% names(dt)) {
    dt <- merge(dt, ultimate_avg, by = c("age_group", "sex"), all.x = TRUE)
  } else {
    dt <- merge(dt, ultimate_avg[, .(age_group, ultimate_aax_weighted)], by = "age_group", all.x = TRUE)
  }

  # Handle missing ultimate values
  dt[is.na(ultimate_aax_weighted), ultimate_aax_weighted := 0.005]

  # Calculate cap value
  dt[, cap_value := ultimate_aax_weighted * cap_multiplier]

  # Apply cap: limit starting_aax to cap_value
  # Also ensure starting_aax is at least 0 (no negative improvement allowed)
  original_aax <- dt$starting_aax
  dt[, starting_aax := pmax(0, pmin(starting_aax, cap_value))]

  # Count how many were capped
  n_capped <- sum(original_aax > dt$cap_value, na.rm = TRUE)
  n_negative <- sum(original_aax < 0, na.rm = TRUE)

  # Clean up temporary columns
  dt[, c("age_group", "ultimate_aax_weighted", "cap_value") := NULL]

  cli::cli_alert_success(
    "Applied capped starting AAx (multiplier={cap_multiplier}): {n_capped} capped, {n_negative} negative values zeroed"
  )

  dt
}

#' Load starting values from TR2025 death probabilities
#'
#' @description
#' Loads TR2025's qx values and converts to mx for use as starting values.
#' Also calculates implied AAx from year-over-year change in qx.
#' This ensures our mortality projection starts from and aligns with TR2025.
#'
#' @param male_qx_file Character: path to TR2025 male death probability file
#' @param female_qx_file Character: path to TR2025 female death probability file
#' @param base_year Integer: year to use for starting qx (default: 2024)
#' @param aax_reference_years Integer vector: years for calculating implied AAx (default: c(2024, 2025))
#' @param ages Integer vector: ages to include (default: 0:99)
#'
#' @return list with:
#'   - starting_mx: data.table with mx by age and sex
#'   - starting_aax: data.table with AAx by age and sex
#'
#' @details
#' TR2025 death probability files contain projected qx by year and age.
#' Starting mx is converted from qx using: mx = -log(1 - qx)
#' Implied AAx is calculated as: AAx = 1 - (qx_year2 / qx_year1)
#'
#' Note: TR2025 files have years in rows, ages in columns (0-119).
#'
#' @export
load_tr_starting_values <- function(
    male_qx_file = "data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv",
    female_qx_file = "data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv",
    base_year = 2024,
    aax_reference_years = c(2024, 2025),
    ages = 0:99
) {
  checkmate::assert_file_exists(male_qx_file)
  checkmate::assert_file_exists(female_qx_file)
  checkmate::assert_int(base_year)
  checkmate::assert_integerish(aax_reference_years, len = 2)
  checkmate::assert_integerish(ages)

  year1 <- min(aax_reference_years)
  year2 <- max(aax_reference_years)

  # Load TR2025 death probabilities
  tr_m <- data.table::fread(male_qx_file)
  tr_f <- data.table::fread(female_qx_file)

  # Rename first column to "year"
  data.table::setnames(tr_m, names(tr_m)[1], "year")
  data.table::setnames(tr_f, names(tr_f)[1], "year")

  # Melt to long format
  tr_m_long <- data.table::melt(tr_m, id.vars = "year",
                                 variable.name = "age", value.name = "qx")
  tr_m_long[, age := as.integer(as.character(age))]
  tr_m_long[, sex := "male"]

  tr_f_long <- data.table::melt(tr_f, id.vars = "year",
                                 variable.name = "age", value.name = "qx")
  tr_f_long[, age := as.integer(as.character(age))]
  tr_f_long[, sex := "female"]

  tr <- data.table::rbindlist(list(tr_m_long, tr_f_long))

  # Filter to relevant years and specified ages
  all_years <- unique(c(base_year, aax_reference_years))
  tr <- tr[year %in% all_years & age %in% ages]

  # --- Calculate starting mx from base year qx ---
  tr_base <- tr[year == base_year, .(age, sex, qx)]

  # Convert qx to mx: mx = -log(1 - qx)
  # For qx close to 1, cap at 0.999 to avoid infinite mx
  tr_base[, qx_capped := pmin(qx, 0.999)]
  tr_base[, starting_mx := -log(1 - qx_capped)]

  starting_mx <- tr_base[, .(age, sex, starting_mx)]
  data.table::setorder(starting_mx, sex, age)

  # --- Calculate implied AAx from year-over-year change ---
  tr_aax <- tr[year %in% aax_reference_years]
  tr_wide <- data.table::dcast(tr_aax, age + sex ~ year, value.var = "qx")
  col_names <- names(tr_wide)
  data.table::setnames(tr_wide, col_names[3:4], c("qx_year1", "qx_year2"))

  # Calculate implied AAx = 1 - (qx_year2 / qx_year1)
  tr_wide[, starting_aax := 1 - (qx_year2 / qx_year1)]

  # Handle edge cases (division by zero, negative qx, etc.)
  tr_wide[is.na(starting_aax) | !is.finite(starting_aax), starting_aax := 0]
  tr_wide[starting_aax < -0.1, starting_aax := -0.1]  # Cap extreme negative values
  tr_wide[starting_aax > 0.1, starting_aax := 0.1]   # Cap extreme positive values

  starting_aax <- tr_wide[, .(age, sex, starting_aax)]
  data.table::setorder(starting_aax, sex, age)

  cli::cli_alert_success(
    "Loaded TR2025 starting values: {nrow(starting_mx)} mx values (year {base_year}), {nrow(starting_aax)} AAx values ({year1}-{year2})"
  )

  list(
    starting_mx = starting_mx,
    starting_aax = starting_aax
  )
}

#' Load complete TR2025 death probabilities for all years
#'
#' @description
#' Loads TR2025's qx values for all projection years. This provides exact
#' alignment with TR2025 mortality projections by using their values directly
#' instead of our own AAx trajectory projection.
#'
#' @param male_qx_file Character: path to TR2025 male death probability file
#' @param female_qx_file Character: path to TR2025 female death probability file
#' @param start_year Integer: first year to include (default: 2024, or from config)
#' @param end_year Integer: last year to include (default: 2099, or from config)
#' @param ages Integer vector: ages to include (default: 0:119)
#' @param config List: optional configuration object to derive year parameters
#'
#' @return data.table with qx by year, age, and sex
#'
#' @details
#' TR2025 death probability files contain projected qx by year and age.
#' This function loads all years to enable direct use of TR2025 values.
#'
#' @export
load_tr_qx_all_years <- function(
    male_qx_file = "data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv",
    female_qx_file = "data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv",
    start_year = NULL,
    end_year = NULL,
    ages = 0:119,
    config = NULL
) {
  # Derive parameters from config if not provided
  if (!is.null(config)) {
    years <- get_projection_years(config, "mortality")
    if (is.null(start_year)) start_year <- years$projection_start
    if (is.null(end_year)) end_year <- years$projection_end
  } else {
    # Fallback defaults
    if (is.null(start_year)) start_year <- 2024
    if (is.null(end_year)) end_year <- 2099
  }
  checkmate::assert_file_exists(male_qx_file)
  checkmate::assert_file_exists(female_qx_file)
  checkmate::assert_int(start_year)
  checkmate::assert_int(end_year)
  checkmate::assert_integerish(ages)

  # Load TR2025 death probabilities
  tr_m <- data.table::fread(male_qx_file)
  tr_f <- data.table::fread(female_qx_file)

  # Rename first column to "year"
  data.table::setnames(tr_m, names(tr_m)[1], "year")
  data.table::setnames(tr_f, names(tr_f)[1], "year")

  # Melt to long format
  tr_m_long <- data.table::melt(tr_m, id.vars = "year",
                                 variable.name = "age", value.name = "qx")
  tr_m_long[, age := as.integer(as.character(age))]
  tr_m_long[, sex := "male"]

  tr_f_long <- data.table::melt(tr_f, id.vars = "year",
                                 variable.name = "age", value.name = "qx")
  tr_f_long[, age := as.integer(as.character(age))]
  tr_f_long[, sex := "female"]

  tr <- data.table::rbindlist(list(tr_m_long, tr_f_long))

  # Filter to requested years and ages
  tr <- tr[year >= start_year & year <= end_year & age %in% ages]
  data.table::setorder(tr, year, sex, age)

  cli::cli_alert_success(
    "Loaded TR2025 qx for {length(unique(tr$year))} years ({start_year}-{max(tr$year)}), ages {min(tr$age)}-{max(tr$age)}"
  )

  tr
}

#' Load TR2025 period life tables
#'
#' @description
#' Loads TR2025 period life tables which contain Lx and Tx values needed
#' to calculate age-last-birthday qx. TR2025's population projection uses
#' age-last-birthday qx, not exact-age qx.
#'
#' @param male_file Character: path to male period life table CSV
#' @param female_file Character: path to female period life table CSV
#' @param start_year Integer: first year to include (default: 1900)
#' @param end_year Integer: last year to include (default: 2099, or from config)
#' @param config List: optional configuration object to derive year parameters
#'
#' @return data.table with columns: year, age, sex, qx, lx, Lx, Tx, ex
#'
#' @details
#' The period life table files have a header that must be skipped.
#' Column names are: Year, x, q(x), l(x), d(x), L(x), T(x), e(x), ...
#'
#' @export
load_tr_period_life_tables <- function(
    male_file = "data/raw/SSA_TR2025/PerLifeTables_M_Alt2_TR2025.csv",
    female_file = "data/raw/SSA_TR2025/PerLifeTables_F_Alt2_TR2025.csv",
    start_year = NULL,
    end_year = NULL,
    config = NULL
) {
  # Derive parameters from config if not provided
  if (!is.null(config)) {
    years <- get_projection_years(config, "mortality")
    # start_year defaults to 1900 for historical coverage, not projection start
    if (is.null(start_year)) start_year <- 1900
    if (is.null(end_year)) end_year <- years$projection_end
  } else {
    # Fallback defaults
    if (is.null(start_year)) start_year <- 1900
    if (is.null(end_year)) end_year <- 2099
  }
  checkmate::assert_file_exists(male_file)
  checkmate::assert_file_exists(female_file)

  # Load male life table (skip header rows)
  lt_m <- data.table::fread(male_file, skip = 4, header = TRUE)
  data.table::setnames(lt_m, c("Year", "x", "qx", "lx", "dx", "Lx", "Tx", "ex",
                               "Dx", "Mx", "Ax", "Nx", "ax", "ax_12"))
  lt_m[, sex := "male"]

  # Load female life table
  lt_f <- data.table::fread(female_file, skip = 4, header = TRUE)
  data.table::setnames(lt_f, c("Year", "x", "qx", "lx", "dx", "Lx", "Tx", "ex",
                               "Dx", "Mx", "Ax", "Nx", "ax", "ax_12"))
  lt_f[, sex := "female"]

  # Combine and select relevant columns
  lt <- data.table::rbindlist(list(lt_m, lt_f), use.names = TRUE)
  lt <- lt[Year >= start_year & Year <= end_year,
           .(year = Year, age = x, sex, qx_exact = qx, lx, Lx, Tx, ex)]

  data.table::setorder(lt, year, sex, age)

  cli::cli_alert_success(
    "Loaded TR2025 period life tables: {length(unique(lt$year))} years, ages {min(lt$age)}-{max(lt$age)}"
  )

  lt
}

#' Calculate age-last-birthday qx from period life table
#'
#' @description
#' TR2025's population projection uses age-last-birthday qx, not exact-age qx.
#' This function converts using the formulas from TR2025 documentation (Section 1.2.c):
#'   - For ages 0-99: qx = 1 - L_{x+1}/L_x
#'   - For age 100+:  q100 = 1 - T_{101}/T_{100}
#'
#' @param period_life_table data.table: output from load_tr_period_life_tables()
#' @param min_age Integer: minimum age to convert (default: 0, per TR methodology)
#' @param max_age Integer: maximum single age before 100+ group (default: 99)
#'
#' @return data.table with columns: year, age, sex, qx_alb (age-last-birthday qx)
#'
#' @details
#' Per TR2025 Section 1.2.c: "The values of qx used in projecting the population
#' are based on age last birthday... values for qx representing age last birthday
#' are derived as follows: qx = 1 - Lx+1/Lx for ages 0 to 99"
#'
#' This conversion applies to all ages 0-99 and 100+, not just older ages.
#'
#' @export
calculate_age_last_birthday_qx <- function(
    period_life_table,
    min_age = 0,
    max_age = 99
) {
  checkmate::assert_data_table(period_life_table)
  checkmate::assert_int(min_age, lower = 0, upper = 99)
  checkmate::assert_int(max_age, lower = min_age, upper = 99)

  lt <- data.table::copy(period_life_table)

  # Calculate qx = 1 - L_{x+1}/L_x for ages min_age to max_age
  results <- list()

  for (yr in unique(lt$year)) {
    for (sx in c("male", "female")) {
      lt_sub <- lt[year == yr & sex == sx][order(age)]

      # For ages min_age to max_age: qx = 1 - L_{x+1}/L_x
      for (a in min_age:max_age) {
        Lx <- lt_sub[age == a, Lx]
        Lx_next <- lt_sub[age == a + 1, Lx]

        if (length(Lx) > 0 && length(Lx_next) > 0 && Lx > 0) {
          qx_alb <- 1 - Lx_next / Lx
          results[[length(results) + 1]] <- data.table::data.table(
            year = yr, age = a, sex = sx, qx_alb = qx_alb
          )
        }
      }

      # For age 100+: q100 = 1 - T_{101}/T_{100}
      T100 <- lt_sub[age == 100, Tx]
      T101 <- lt_sub[age == 101, Tx]

      if (length(T100) > 0 && length(T101) > 0 && T100 > 0) {
        q100_alb <- 1 - T101 / T100
        results[[length(results) + 1]] <- data.table::data.table(
          year = yr, age = 100L, sex = sx, qx_alb = q100_alb
        )
      }
    }
  }

  result <- data.table::rbindlist(results)
  data.table::setorder(result, year, sex, age)

  # Report the difference from exact-age qx
  comparison <- merge(
    result,
    lt[age >= min_age & age <= 100, .(year, age, sex, qx_exact)],
    by = c("year", "age", "sex")
  )
  comparison[, diff_pct := (qx_alb - qx_exact) / qx_exact * 100]

  # Calculate mean differences by age group
  mean_diff_0_84 <- comparison[age >= 0 & age <= 84, mean(diff_pct, na.rm = TRUE)]
  mean_diff_85_99 <- comparison[age >= 85 & age <= 99, mean(diff_pct, na.rm = TRUE)]
  mean_diff_100 <- comparison[age == 100, mean(diff_pct, na.rm = TRUE)]

  cli::cli_alert_success(
    "Calculated age-last-birthday qx for ages {min_age}-100 (per TR2025 Section 1.2.c)"
  )
  if (min_age == 0) {
    cli::cli_alert_info(
      "Mean diff from exact-age qx: ages 0-84 = {round(mean_diff_0_84, 1)}%, ages 85-99 = {round(mean_diff_85_99, 1)}%, age 100+ = {round(mean_diff_100, 1)}%"
    )
  } else {
    cli::cli_alert_info(
      "Mean diff from exact-age qx: ages 85-99 = {round(mean_diff_85_99, 1)}%, age 100+ = {round(mean_diff_100, 1)}%"
    )
  }

  result
}

#' Apply age-last-birthday qx adjustment to mortality data
#'
#' @description
#' Replaces exact-age qx values with age-last-birthday qx for specified ages.
#' This aligns our mortality data with TR2025's population projection methodology.
#'
#' Per TR2025 Section 1.2.c, all qx values used for population projection should
#' be age-last-birthday qx, converted using the formula qx = 1 - Lx+1/Lx.
#'
#' @param mortality_qx data.table: mortality qx with columns year, age, sex, qx
#' @param qx_alb data.table: age-last-birthday qx from calculate_age_last_birthday_qx()
#' @param min_age Integer: minimum age to replace (default: 0, per TR methodology)
#'
#' @return data.table with qx values replaced for ages >= min_age
#'
#' @export
apply_age_last_birthday_qx <- function(
    mortality_qx,
    qx_alb,
    min_age = 0
) {
  checkmate::assert_data_table(mortality_qx)
  checkmate::assert_data_table(qx_alb)
  checkmate::assert_int(min_age, lower = 0)

  dt <- data.table::copy(mortality_qx)

  # Merge with age-last-birthday qx
  dt <- merge(dt, qx_alb[, .(year, age, sex, qx_alb)],
              by = c("year", "age", "sex"), all.x = TRUE)

  # Replace qx with qx_alb for ages >= min_age where we have the alb value
  n_replaced <- dt[age >= min_age & !is.na(qx_alb), .N]
  dt[age >= min_age & !is.na(qx_alb), qx := qx_alb]
  dt[, qx_alb := NULL]

  cli::cli_alert_success(
    "Replaced {n_replaced} qx values with age-last-birthday qx (ages {min_age}+)"
  )

  dt
}

#' Apply configured starting AAx method
#'
#' @description
#' Applies the configured starting AAx method (regression, capped, or tr_qx).
#'
#' @param regression_aax data.table with regression-based AAx (from calculate_annual_reduction_rates)
#' @param method Character: method to apply ("regression", "capped", "tr_qx")
#' @param config List: mortality configuration (from load_mortality_config)
#'
#' @return For "regression" and "capped": data.table with starting_aax by age, sex
#'         For "tr_qx": list with starting_mx and starting_aax data.tables
#'
#' @export
apply_starting_aax_method <- function(regression_aax,
                                       method = "regression",
                                       config = NULL) {
  checkmate::assert_data_table(regression_aax)
  checkmate::assert_choice(method, c("regression", "capped", "tr_qx"))

  # Start with regression-based AAx
  dt <- data.table::copy(regression_aax)

  if (method == "regression") {
    # Original method: use regression-based starting_aax as-is
    cli::cli_alert_info("Using regression-based starting AAx (original method)")
    return(dt[, .(age, sex, starting_aax)])
  }

  if (method == "capped") {
    # Capped method: limit starting AAx to multiple of ultimate
    cap_multiplier <- 1.5
    if (!is.null(config) && !is.null(config$starting_aax_cap$multiplier)) {
      cap_multiplier <- config$starting_aax_cap$multiplier
    }

    result <- apply_capped_starting_aax(
      starting_aax = dt[, .(age, sex, starting_aax)],
      cap_multiplier = cap_multiplier
    )
    return(result)
  }

  if (method == "tr_qx") {
    # TR_QX method: use TR2025's actual qx as starting point
    # Returns both starting_mx AND starting_aax from TR2025 files
    male_file <- "data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv"
    female_file <- "data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv"
    base_year <- 2024
    aax_ref_years <- c(2024, 2025)

    if (!is.null(config) && !is.null(config$starting_tr_qx)) {
      if (!is.null(config$starting_tr_qx$male_qx_file)) {
        male_file <- config$starting_tr_qx$male_qx_file
      }
      if (!is.null(config$starting_tr_qx$female_qx_file)) {
        female_file <- config$starting_tr_qx$female_qx_file
      }
      if (!is.null(config$starting_tr_qx$base_year)) {
        base_year <- config$starting_tr_qx$base_year
      }
      if (!is.null(config$starting_tr_qx$aax_reference_years)) {
        aax_ref_years <- config$starting_tr_qx$aax_reference_years
      }
    }

    # Check if files exist
    if (!file.exists(male_file) || !file.exists(female_file)) {
      cli::cli_alert_warning("TR2025 qx files not found, falling back to capped method")
      return(apply_capped_starting_aax(
        starting_aax = dt[, .(age, sex, starting_aax)],
        cap_multiplier = 1.5
      ))
    }

    # Load both starting mx and starting aax from TR2025
    result <- load_tr_starting_values(
      male_qx_file = male_file,
      female_qx_file = female_file,
      base_year = base_year,
      aax_reference_years = aax_ref_years
    )
    return(result)
  }

  # Should never reach here
  cli::cli_abort("Unknown starting AAx method: {method}")
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

#' Load mortality configuration
#'
#' @description
#' Loads mortality configuration from a YAML file. If no config_path is provided,
#' loads from the default TR2025 configuration.
#'
#' @param config_path Character: path to YAML config file (default: tr2025.yaml)
#'
#' @return List with mortality configuration
#'
#' @export
load_mortality_config <- function(config_path = NULL) {
  if (is.null(config_path)) {
    config_path <- here::here("config/assumptions/tr2025.yaml")
  }

  if (!file.exists(config_path)) {
    cli::cli_abort(c(
      "Mortality configuration file not found",
      "x" = "Path: {config_path}",
      "i" = "Create a configuration file or use a different path"
    ))
  }

  config <- yaml::read_yaml(config_path)

  if (is.null(config$mortality)) {
    cli::cli_abort(c(
      "No mortality section in configuration file",
      "i" = "Config file: {config_path}"
    ))
  }

  config$mortality
}

#' Get ultimate AAx assumptions from configuration
#'
#' @description
#' Returns ultimate annual percentage reduction assumptions by age group,
#' sex, and cause of death. Reads from configuration file to allow user
#' customization of mortality improvement assumptions.
#'
#' @param config_path Character: path to YAML config file (default: tr2025.yaml)
#' @param config List: pre-loaded mortality config (overrides config_path)
#'
#' @return data.table with columns: age_group, age_min, age_max, cause, ultimate_aax
#'
#' @details
#' Default values are from TR2025 Appendix 1.2-1 (Intermediate Assumptions).
#' Same values for male and female.
#' Age groups: under 15, 15-49, 50-64, 65-84, 85+
#'
#' To customize assumptions, edit the mortality.ultimate_aax section in
#' config/assumptions/tr2025.yaml or create a new config file.
#'
#' @examples
#' \dontrun{
#' # Use default TR2025 assumptions
#' ultimate <- get_ultimate_aax_assumptions()
#'
#' # Use custom config file
#' ultimate <- get_ultimate_aax_assumptions("config/assumptions/custom.yaml")
#' }
#'
#' @export
get_ultimate_aax_assumptions <- function(config_path = NULL, config = NULL) {
  # Load config if not provided
  if (is.null(config)) {
    config <- tryCatch({
      load_mortality_config(config_path)
    }, error = function(e) {
      cli::cli_alert_warning("Could not load config, using hardcoded defaults: {conditionMessage(e)}")
      NULL
    })
  }

  # If config loaded successfully, build from config
  if (!is.null(config) && !is.null(config$ultimate_aax) && !is.null(config$age_groups)) {
    cli::cli_alert_info("Loading ultimate AAx assumptions from config file")

    results <- list()
    causes <- c("CVD", "CAN", "ACV", "RES", "DEM", "OTH")

    for (age_grp_name in names(config$age_groups)) {
      age_grp <- config$age_groups[[age_grp_name]]
      aax_grp <- config$ultimate_aax[[age_grp_name]]

      if (is.null(aax_grp)) {
        cli::cli_alert_warning("No AAx values for age group {age_grp_name}, using defaults")
        next
      }

      for (cause in causes) {
        # Get AAx value (as percentage), convert to decimal
        aax_pct <- aax_grp[[cause]]
        if (is.null(aax_pct)) {
          cli::cli_alert_warning("No AAx for {age_grp_name}/{cause}, using 0")
          aax_pct <- 0
        }

        results[[length(results) + 1]] <- data.table::data.table(
          age_group = age_grp_name,
          age_min = age_grp$min_age,
          age_max = age_grp$max_age,
          cause = cause,
          ultimate_aax = aax_pct / 100  # Convert percentage to decimal
        )
      }
    }

    if (length(results) > 0) {
      ultimate <- data.table::rbindlist(results)
      cli::cli_alert_success("Loaded {nrow(ultimate)} ultimate AAx assumptions from config")
      return(ultimate)
    }
  }


  # Fallback to hardcoded defaults (TR2025 Alternative II / Intermediate)
  cli::cli_alert_warning(
    "Falling back to hardcoded TR2025 Alt II (Intermediate) ultimate AAx assumptions"
  )
  cli::cli_alert_info(
    "To use custom assumptions, ensure config/assumptions/tr2025.yaml exists with mortality.ultimate_aax section"
  )
  ultimate <- data.table::data.table(
    age_group = rep(c("under_15", "age_15_49", "age_50_64", "age_65_84", "age_85_plus"), each = 6),
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

#' Get cause-of-death proportions by age group and sex
#'
#' @description
#' Calculates the proportion of deaths by cause for each age group and sex.
#' Used to weight cause-specific ultimate AAx values per TR2025 methodology.
#'
#' @param years Integer vector: years to use for proportions (default: 2015:2019)
#' @param cache_dir Character: path to cached NCHS death data
#'
#' @return data.table with columns: age_group, sex, cause, proportion
#'
#' @details
#' Per TR2025 mortality documentation, the "Resulting Total" AAx for each
#' age group is the weighted average of cause-specific AAx, where weights
#' are the proportion of deaths from each cause.
#'
#' Uses 2015-2019 by default (pre-COVID period matching TR2025).
#'
#' @export
get_cause_of_death_proportions <- function(years = 2015:2019,
                                           cache_dir = here::here("data/cache/nchs_deaths")) {
  # Load death data for specified years
  deaths_list <- list()

  for (yr in years) {
    cache_file <- file.path(cache_dir, sprintf("deaths_by_age_sex_cause_%d.rds", yr))
    if (file.exists(cache_file)) {
      deaths_list[[as.character(yr)]] <- readRDS(cache_file)
    } else {
      cli::cli_alert_warning("Cached death data not found for {yr}, skipping")
    }
  }

  if (length(deaths_list) == 0) {
    cli::cli_abort("No cached death data found for years {paste(years, collapse=', ')}")
  }

  deaths <- data.table::rbindlist(deaths_list)

  # Map ages to age groups
  deaths[, age_group := data.table::fcase(
    age < 15, "under_15",
    age >= 15 & age < 50, "age_15_49",
    age >= 50 & age < 65, "age_50_64",
    age >= 65 & age < 85, "age_65_84",
    age >= 85, "age_85_plus"
  )]

  # Calculate total deaths by age group, sex, cause
  by_cause <- deaths[, .(deaths = sum(deaths)), by = .(age_group, sex, cause)]

  # Calculate total deaths by age group, sex
  totals <- deaths[, .(total_deaths = sum(deaths)), by = .(age_group, sex)]

  # Merge and calculate proportions
  result <- merge(by_cause, totals, by = c("age_group", "sex"))
  result[, proportion := deaths / total_deaths]

  # Select output columns
  result <- result[, .(age_group, sex, cause, proportion)]
  data.table::setorder(result, age_group, sex, cause)

  cli::cli_alert_success(
    "Calculated cause-of-death proportions from {length(deaths_list)} years ({min(years)}-{max(years)})"
  )

  result
}

#' Map single ages to ultimate AAx age groups
#'
#' @param ages Integer vector of ages
#' @param config List: mortality config with age_groups section (optional)
#'
#' @return Character vector of age group names
#'
#' @keywords internal
map_age_to_ultimate_group <- function(ages, config = NULL) {
  # Use config age groups if available
  if (!is.null(config) && !is.null(config$age_groups)) {
    # Build mapping from config
    result <- rep(NA_character_, length(ages))
    for (grp_name in names(config$age_groups)) {
      grp <- config$age_groups[[grp_name]]
      result[ages >= grp$min_age & ages <= grp$max_age] <- grp_name
    }
    return(result)
  }

  # Default mapping (matches config/assumptions/tr2025.yaml)
  data.table::fcase(
    ages < 15, "under_15",
    ages >= 15 & ages < 50, "age_15_49",
    ages >= 50 & ages < 65, "age_50_64",
    ages >= 65 & ages < 85, "age_65_84",
    ages >= 85, "age_85_plus"
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
#' @param ultimate_year Integer: year when ultimate values are reached (default: 2043)
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
                                      ultimate_year = 2043,
                                      projection_years = 2020:2100,
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
  }

  if (!has_cause) {
    # Fallback: use cause-weighted average of ultimate AAx
    cause_props <- tryCatch(
      get_cause_of_death_proportions(),
      error = function(e) {
        cli::cli_alert_warning("Could not load cause-of-death proportions: {conditionMessage(e)}")
        cli::cli_alert_warning("Falling back to simple average of ultimate AAx")
        NULL
      }
    )

    if (!is.null(cause_props)) {
      # Merge proportions with ultimate AAx values
      weighted_data <- merge(
        ultimate_aax[, .(age_group, cause, ultimate_aax)],
        cause_props,
        by = c("age_group", "cause"),
        all.x = TRUE
      )
      # Handle any missing proportions (set to equal weight)
      weighted_data[is.na(proportion), proportion := 1/6]

      # Calculate weighted average by age_group and sex
      ultimate_avg <- weighted_data[, .(
        ultimate_aax = sum(ultimate_aax * proportion)
      ), by = .(age_group, sex)]

      cli::cli_alert_success("Using cause-weighted ultimate AAx")
    } else {
      # Fallback to simple average (not sex-specific)
      ultimate_avg <- ultimate_aax[, .(ultimate_aax = mean(ultimate_aax)), by = age_group]
    }

    # Merge with starting data (handle sex if present in ultimate_avg)
    if ("sex" %in% names(ultimate_avg) && "sex" %in% names(dt)) {
      dt <- merge(dt, ultimate_avg, by = c("age_group", "sex"), all.x = TRUE)
    } else {
      dt <- merge(dt, ultimate_avg[, .(age_group, ultimate_aax)], by = "age_group", all.x = TRUE)
    }
  }

  # Handle any missing ultimate values
  dt[is.na(ultimate_aax), ultimate_aax := 0.005]  # Default small positive value

  # Build trajectory for each year
  result_list <- list()

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
#' Reads from configuration file to allow user customization.
#'
#' @param config_path Character: path to YAML config file (default: tr2025.yaml)
#' @param config List: pre-loaded mortality config (overrides config_path)
#'
#' @return data.table with columns: year, age_min, age_max, covid_factor
#'
#' @details
#' Per SSA TR2025 documentation, COVID impacts death rates through 2025.
#'
#' @export
get_covid_adjustment_factors <- function(config_path = NULL, config = NULL) {
  # Load config if not provided
  if (is.null(config)) {
    config <- tryCatch({
      load_mortality_config(config_path)
    }, error = function(e) {
      NULL
    })
  }

  # If config loaded successfully, build from config
  if (!is.null(config) && !is.null(config$covid_adjustments)) {
    results <- list()

    # Age group mapping
    age_map <- list(
      age_0 = c(0L, 0L),
      age_1_14 = c(1L, 14L),
      age_15_64 = c(15L, 64L),
      age_65_84 = c(65L, 84L),
      age_85_plus = c(85L, 119L)
    )

    for (yr_name in names(config$covid_adjustments)) {
      yr <- as.integer(yr_name)
      yr_factors <- config$covid_adjustments[[yr_name]]

      for (age_name in names(yr_factors)) {
        if (age_name %in% names(age_map)) {
          results[[length(results) + 1]] <- data.table::data.table(
            year = yr,
            age_min = age_map[[age_name]][1],
            age_max = age_map[[age_name]][2],
            covid_factor = yr_factors[[age_name]]
          )
        }
      }
    }

    if (length(results) > 0) {
      return(data.table::rbindlist(results))
    }
  }

  # Fallback to hardcoded defaults (TR2025 Alternative II / Intermediate)
  cli::cli_alert_warning(
    "Falling back to hardcoded TR2025 Alt II (Intermediate) COVID adjustment factors"
  )
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
#' 2. Determine starting mx from regression fitted values (or TR2025 for tr_qx method)
#' 3. Apply configured starting AAx method
#' 4. Calculate AAx trajectory to ultimate values
#' 5. Project mx forward
#' 6. Overwrite with actual data (2020-2023)
#' 7. Apply WH smoothing
#' 8. Convert to qx
#' 9. Apply COVID adjustments
#'
#' @param deaths data.table with historical deaths by year, age, sex, [cause]
#' @param population data.table with historical population by year, age, sex
#' @param base_year Integer: last year for regression (default: 2019)
#' @param projection_end Integer: last projection year (default: 2100)
#' @param by_cause Logical: project by cause of death (default: FALSE for total)
#' @param starting_aax_method Character: method for starting values
#'   ("regression", "capped", "tr_qx"). Default: "regression"
#' @param mortality_config List: mortality configuration from config file (optional)
#'
#' @return list with projected_mx, projected_qx, aax_trajectory, and life_tables
#'
#' @export
run_mortality_projection <- function(deaths,
                                      population,
                                      base_year = 2019,
                                      projection_end = 2100,
                                      by_cause = FALSE,
                                      starting_aax_method = "regression",
                                      mortality_config = NULL) {
  cli::cli_h1("SSA Mortality Projection Pipeline")
  cli::cli_alert_info("Starting AAx method: {starting_aax_method}")

  # Step 1: Calculate historical mx
  cli::cli_h2("Step 1: Calculate historical central death rates")
  historical_years <- unique(deaths$year)
  historical_years <- historical_years[historical_years <= base_year]

  mx_historical <- calculate_central_death_rates(deaths[year %in% historical_years],
                                                  population[year %in% historical_years],
                                                  by_cause = by_cause)

  # Step 2: Apply WH smoothing to historical mx
  # NOTE (Deviation #10): Smoothing is applied BEFORE the AAx regression. The
  # TR2025 documentation (Section 1.2.c, Eq 1.2.1-1.2.2) describes smoothing
  # and regression separately without specifying their ordering. Smoothing first
  # is standard actuarial practice: it removes age-to-age noise from raw mx,
  # producing more stable AAx estimates from the subsequent regression.
  cli::cli_h2("Step 2: Apply Whittaker-Henderson smoothing")
  mx_smooth <- smooth_death_rates(mx_historical)

  # Step 3: Calculate AAx from 2008-2019 regression
  cli::cli_h2("Step 3: Calculate AAx (2008-2019 regression)")
  aax <- calculate_annual_reduction_rates(mx_smooth,
                                           start_year = 2008,
                                           end_year = base_year,
                                           by_cause = by_cause)

  # Step 4: Calculate starting mx from regression (may be overridden by tr_qx method)
  cli::cli_h2("Step 4: Calculate starting mx from regression fitted values")
  starting_mx <- calculate_starting_mx(aax, base_year = base_year)

  # Step 5: Apply configured starting AAx method
  cli::cli_h2(paste0("Step 5: Apply starting AAx method (", starting_aax_method, ")"))
  starting_values <- apply_starting_aax_method(
    regression_aax = aax,
    method = starting_aax_method,
    config = mortality_config
  )

  # Handle tr_qx method which returns both starting_mx and starting_aax
  # For other methods, starting_values is just starting_aax
  if (starting_aax_method == "tr_qx" && is.list(starting_values) && "starting_mx" %in% names(starting_values)) {
    # Use TR2025's starting mx and AAx
    starting_mx <- starting_values$starting_mx
    starting_aax <- starting_values$starting_aax

    # For tr_qx method, the effective base year is 2024 (TR2025's base year)
    # Adjust projection years accordingly
    tr_base_year <- 2024
    if (!is.null(mortality_config) && !is.null(mortality_config$starting_tr_qx$base_year)) {
      tr_base_year <- mortality_config$starting_tr_qx$base_year
    }
    cli::cli_alert_info("Using TR2025 starting values from year {tr_base_year}")
    effective_base_year <- tr_base_year
  } else {
    starting_aax <- starting_values
    effective_base_year <- base_year
  }

  # NOTE (Deviation #15): The "tr_qx" method intentionally bypasses the full
  # regression-based mortality projection (Steps 6-9) and loads TR2025's
  # pre-computed qx values directly. This ensures exact alignment with TR2025
  # mortality outputs. All other audit deviations from the TR2025 methodology
  # (e.g., cause-specific projection, CMS data, age-last-birthday conversion)
  # are only active when starting_aax_method = "regression" or "capped".
  if (starting_aax_method == "tr_qx") {
    cli::cli_h2("Step 6: Load TR2025 qx directly (skipping projection)")

    # Get config for file paths
    male_file <- "data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv"
    female_file <- "data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv"
    if (!is.null(mortality_config) && !is.null(mortality_config$starting_tr_qx)) {
      if (!is.null(mortality_config$starting_tr_qx$male_qx_file)) {
        male_file <- mortality_config$starting_tr_qx$male_qx_file
      }
      if (!is.null(mortality_config$starting_tr_qx$female_qx_file)) {
        female_file <- mortality_config$starting_tr_qx$female_qx_file
      }
    }

    # Load TR2025 qx for all years
    tr_qx <- load_tr_qx_all_years(
      male_qx_file = male_file,
      female_qx_file = female_file,
      start_year = effective_base_year,
      end_year = projection_end
    )

    # TR2025 files only go to age 119, no conversion needed
    projected_qx <- tr_qx

    # Create empty mx placeholder (we're using qx directly)
    projected_mx_smooth <- data.table::data.table(
      year = integer(),
      sex = character(),
      age = integer(),
      mx_smooth = numeric()
    )

    # Create empty AAx trajectory placeholder
    aax_trajectory <- data.table::data.table(
      year = integer(),
      age = integer(),
      sex = character(),
      aax_projected = numeric()
    )

    cli::cli_alert_success("Using TR2025 qx values directly (no projection)")
    cli::cli_h1("Projection complete")

    return(list(
      projected_mx = projected_mx_smooth,
      projected_qx = projected_qx,
      aax_trajectory = aax_trajectory,
      starting_mx = starting_mx,
      historical_aax = aax
    ))
  }

  # Standard projection path (regression or capped methods)
  # Step 6: Calculate AAx trajectory
  cli::cli_h2("Step 6: Calculate AAx trajectory to ultimate")
  projection_years <- (effective_base_year + 1):projection_end

  # Get ultimate_year from config, default to 2043
  config_ultimate_year <- if (!is.null(mortality_config$ultimate_year)) {
    mortality_config$ultimate_year
  } else {
    2043L
  }

  aax_trajectory <- calculate_aax_trajectory(
    starting_aax = starting_aax,
    base_year = effective_base_year,
    ultimate_year = config_ultimate_year,
    projection_years = projection_years
  )

  # Step 7: Project mx forward
  cli::cli_h2("Step 7: Project mx forward")
  # Get actual mx for years after effective base year to overwrite projections
  actual_years <- unique(deaths$year)
  actual_years <- actual_years[actual_years > effective_base_year]

  actual_mx <- NULL
  if (length(actual_years) > 0) {
    mx_actual <- calculate_central_death_rates(deaths[year %in% actual_years],
                                                population[year %in% actual_years],
                                                by_cause = by_cause)
    actual_mx <- smooth_death_rates(mx_actual)
  }

  projected_mx <- project_death_rates(starting_mx, aax_trajectory, actual_mx)

  # Step 8: Apply WH smoothing to projected mx
  cli::cli_h2("Step 8: Apply WH smoothing to projections")
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

  # Step 9: Convert to qx with TR q1 method
  cli::cli_h2("Step 9: Convert mx to qx (with TR q1 method)")

  # First, convert all ages using standard formula
  qx_list <- list()
  for (yr in unique(projected_mx_smooth$year)) {
    mx_yr <- projected_mx_smooth[year == yr, .(age, sex, mx = mx_smooth)]
    qx_yr <- convert_mx_to_qx(mx_yr)
    qx_yr[, year := yr]
    qx_list[[as.character(yr)]] <- qx_yr
  }
  projected_qx <- data.table::rbindlist(qx_list, fill = TRUE)

  # Apply TR q1 method: use 4m1 ratio for projected years
  # Per TR2025: "After the last historical year, each q1 is calculated from 4m1
  # assuming that the ratio of q1 to 4m1 measured for the last historical year
  # would remain constant thereafter."

  # Aggregate historical deaths by year/age/sex (remove cause if present)
  deaths_agg <- deaths[, .(deaths = sum(deaths, na.rm = TRUE)), by = .(year, age, sex)]
  last_hist_year <- effective_base_year

  # Calculate q1 using TR method (4m1 ratio for projected years)
  # The mx_data includes both historical fitted values and projected values
  # We need to reconstruct mx_data for q1 calculation

  # Create mx_data combining historical mx and projected mx
  projected_mx_for_q1 <- projected_mx_smooth[, .(year, age, sex, mx = mx_smooth)]

  # Get historical years that are also in projection
  actual_proj_years <- unique(projected_mx_for_q1$year)
  actual_proj_years <- actual_proj_years[actual_proj_years > last_hist_year]

  if (length(actual_proj_years) > 0) {
    q1_tr <- calculate_q1_tr_method(
      mx_data = projected_mx_for_q1,
      deaths = deaths_agg,
      population = population,
      last_historical_year = last_hist_year,
      projection_years = actual_proj_years
    )

    # Integrate the TR q1 values into projected_qx
    projected_qx <- integrate_q1(projected_qx, q1_tr)
  }

  # Step 10: Apply COVID adjustments
  cli::cli_h2("Step 10: Apply COVID-19 adjustments")
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
#' @param max_age Integer: maximum age in the data (default: 100). No extrapolation
#'   is performed - qx is calculated directly from mx for all ages present.
#'
#' @return data.table with qx values added
#'
#' @details
#' Per SSA 2025 Long-Range Model Documentation (Section 1.2.2):
#'
#' **All ages:**
#' qx = mx / (1 + 0.5*mx)
#'
#' This is the standard actuarial formula for converting central death rates
#' to death probabilities.
#'
#' **Age 100 (representing 100+):**
#' When the input mx includes age 100 representing the 100+ open-ended age group,
#' the same formula is applied. The mx for age 100 should be calculated from
#' aggregate deaths (100+) / aggregate population (100+).
#'
#' Female qx is capped at male qx if crossover occurs at very old ages.
#'
#' @export
convert_mx_to_qx <- function(mx, max_age = 100) {
  checkmate::assert_data_table(mx)
  checkmate::assert_names(names(mx), must.include = c("age", "sex", "mx"))

  dt <- data.table::copy(mx)
  has_year <- "year" %in% names(dt)

  # Standard formula for all ages: qx = mx / (1 + 0.5*mx)
  # This applies to ages 0-100 (with 100 representing 100+)
  dt[, qx := mx / (1 + 0.5 * mx)]

  # Cap qx at 1.0 for very old ages
  dt[qx > 1, qx := 1.0]

  # Female qx capped at male qx if crossover occurs
  # Only apply for ages < 85 where crossover would be truly anomalous

  # At ages 85+, small populations can cause random crossovers that should
  # be handled by HMD calibration instead of this simple cap
  # Must merge by age AND year (if present) to avoid cartesian join
  if (has_year) {
    male_qx_lookup <- dt[sex == "male", .(year, age, male_qx = qx)]
    merge_keys <- c("year", "age")
  } else {
    male_qx_lookup <- dt[sex == "male", .(age, male_qx = qx)]
    merge_keys <- "age"
  }

  if (nrow(male_qx_lookup) > 0 && any(dt$sex == "female")) {
    dt <- merge(dt, male_qx_lookup, by = merge_keys, all.x = TRUE)
    # Only cap female qx at male qx for ages < 85
    dt[sex == "female" & age < 85 & !is.na(male_qx) & !is.na(qx), qx := pmin(qx, male_qx)]
    dt[, male_qx := NULL]
  }

  if (has_year) {
    data.table::setorder(dt, year, sex, age)
  } else {
    data.table::setorder(dt, sex, age)
  }

  actual_max <- max(dt$age, na.rm = TRUE)
  cli::cli_alert_success("Converted mx to qx for {nrow(dt)} records (ages 0-{actual_max})")

  dt
}

# =============================================================================
# HMD-Based Calibration for Ages 85+
# =============================================================================

#' Get HMD elderly mortality ratios
#'
#' @description
#' Calculates qx(age)/qx(84) ratios from HMD for ages 85+.
#' These ratios represent the well-researched mortality progression
#' at oldest ages from the Human Mortality Database.
#'
#' @param max_age Integer: maximum age (default: 110, HMD's max)
#' @param reference_year Integer: year to use for ratios (default: 2019)
#'
#' @return data.table with columns: age, sex, qx_ratio (relative to age 84)
#'
#' @keywords internal
get_hmd_elderly_qx_ratios <- function(max_age = 110, reference_year = 2019) {
  # Load HMD life tables
  hmd_lt <- fetch_hmd_life_tables(sex = "both")

  # Use reference year, or most recent available
  available_years <- unique(hmd_lt$year)
  if (!reference_year %in% available_years) {
    reference_year <- max(available_years)
    cli::cli_alert_info("Using HMD reference year {reference_year}")
  }

  # Get qx for reference year
  hmd_qx <- hmd_lt[year == reference_year, .(age, sex, qx)]

  # Calculate ratios relative to age 84
  ratios <- hmd_qx[age >= 84 & age <= max_age]
  q84_lookup <- ratios[age == 84, .(sex, q84 = qx)]
  ratios <- merge(ratios, q84_lookup, by = "sex")
  ratios[, qx_ratio := qx / q84]
  ratios[, q84 := NULL]

  # Keep only ages 85+
  ratios <- ratios[age >= 85]

  cli::cli_alert_success(
    "Calculated HMD qx ratios for ages 85-{max_age} from year {reference_year}"
  )

  ratios[, .(age, sex, qx_ratio)]
}

#' Adjust qx for ages 85+ using HMD calibration
#'
#' @description
#' Adjusts death probabilities for ages 85+ using mortality ratios
#' derived from the Human Mortality Database. This corrects the
#' underestimation that occurs with simple extrapolation methods.
#'
#' @param qx data.table with qx values (year, age, sex, qx)
#' @param hmd_ratios data.table from get_hmd_elderly_qx_ratios(), or NULL to fetch
#' @param transition_age Integer: age at which to start HMD adjustment (default: 85)
#' @param max_age Integer: maximum age to calibrate (default: 100). For max_age = 100,
#'   only ages 85-99 are calibrated; age 100 (representing 100+) keeps its original
#'   data-driven value since HMD ratios are age-specific.
#'
#' @return data.table with adjusted qx values
#'
#' @details
#' The adjustment works by:
#' 1. For ages 0-84: keep our calculated qx unchanged
#' 2. For ages 85-99: use q84 * HMD_ratio(age)
#' 3. For age 100 (if max_age = 100): keep original qx (aggregate 100+ from data)
#'
#' This preserves our historical trends while using HMD's well-researched
#' mortality pattern at oldest ages.
#'
#' When max_age = 100, age 100 represents the open-ended 100+ age group and
#' is NOT calibrated with HMD ratios, since the original qx comes from
#' aggregate deaths/population data.
#'
#' @export
adjust_qx_with_hmd <- function(qx,
                                hmd_ratios = NULL,
                                transition_age = 85,
                                max_age = 100) {
  checkmate::assert_data_table(qx)
  checkmate::assert_int(transition_age, lower = 80, upper = 100)
  checkmate::assert_int(max_age, lower = 99, upper = 130)

  dt <- data.table::copy(qx)
  has_year <- "year" %in% names(dt)

  # Get HMD ratios if not provided
  if (is.null(hmd_ratios)) {
    hmd_ratios <- get_hmd_elderly_qx_ratios(max_age = min(max_age, 110))
  }

  # Get our q84 values for each year/sex
  ref_age <- transition_age - 1  # age 84
  if (has_year) {
    q_ref <- dt[age == ref_age, .(year, sex, q_ref = qx)]
  } else {
    q_ref <- dt[age == ref_age, .(sex, q_ref = qx)]
  }

  # Determine the highest age to calibrate with HMD
  # If max_age = 100, only calibrate 85-99 (age 100 is 100+ from data)
  # Otherwise calibrate up to max_age
  calibrate_max_age <- if (max_age == 100) 99 else max_age

  # Remove ages in the calibration range from our data (keep age 100 if present)
  dt_young <- dt[age < transition_age]
  dt_100plus <- if (max_age == 100) dt[age == 100] else data.table::data.table()

  # Create adjusted elderly qx using HMD ratios
  results_list <- list()

  if (has_year) {
    years <- unique(q_ref$year)
  } else {
    years <- NA
  }

  for (sex_val in c("male", "female")) {
    # Get HMD ratios for this sex
    sex_ratios <- hmd_ratios[sex == sex_val]

    # Extend ratios beyond HMD's max age if needed (only if calibrate_max_age > hmd_max)
    hmd_max <- max(sex_ratios$age)
    if (calibrate_max_age > hmd_max) {
      # Calculate growth rate from last few ages
      last_ratios <- sex_ratios[age >= (hmd_max - 5)]
      if (nrow(last_ratios) >= 2) {
        # Use geometric mean of growth rates
        growth_rates <- last_ratios$qx_ratio[-1] / last_ratios$qx_ratio[-nrow(last_ratios)]
        avg_growth <- exp(mean(log(growth_rates)))
      } else {
        avg_growth <- 1.05  # fallback
      }

      # Extend ratios
      last_ratio <- sex_ratios[age == hmd_max, qx_ratio]
      extended_ages <- (hmd_max + 1):calibrate_max_age
      extended_ratios <- data.table::data.table(
        age = extended_ages,
        sex = sex_val,
        qx_ratio = last_ratio * avg_growth^(seq_along(extended_ages))
      )
      sex_ratios <- data.table::rbindlist(list(sex_ratios, extended_ratios))
    }

    for (yr in years) {
      # Get our q84 for this year/sex
      if (has_year && !is.na(yr)) {
        q84_val <- q_ref[sex == sex_val & year == yr, q_ref]
      } else {
        q84_val <- q_ref[sex == sex_val, q_ref]
      }

      if (length(q84_val) == 0 || is.na(q84_val)) next

      # Calculate adjusted qx for ages in calibration range
      elderly_qx <- data.table::copy(sex_ratios[age >= transition_age & age <= calibrate_max_age])
      elderly_qx[, qx := pmin(q84_val * qx_ratio, 1.0)]
      elderly_qx[, qx_ratio := NULL]

      if (has_year && !is.na(yr)) {
        elderly_qx[, year := yr]
      }

      results_list[[length(results_list) + 1]] <- elderly_qx
    }
  }

  # Combine young ages with adjusted elderly
  # Note: dt_100plus (original 100+) is NOT used if we are recalculating it
  
  dt_elderly <- data.table::rbindlist(results_list, fill = TRUE)

  # Add any missing columns to elderly data
  for (col in setdiff(names(dt_young), names(dt_elderly))) {
    dt_elderly[, (col) := NA]
  }

  # If max_age != 100, we might need to handle ages > 99 differently, 
  # but per TR methodology we are focused on the max_age=100 case where 100 is aggregate.
  
  parts_to_combine <- list(dt_young, dt_elderly)
  
  # If we are NOT recalcing 100+ (e.g. max_age > 100), we append the rest of the original data
  if (max_age > 100) {
    dt_rest <- dt[age > calibrate_max_age]
     # Ensure dt_rest has same columns
    for (col in setdiff(names(dt_young), names(dt_rest))) {
      dt_rest[, (col) := NA]
    }
    parts_to_combine <- c(parts_to_combine, list(dt_rest))
  }
  
  result <- data.table::rbindlist(parts_to_combine, fill = TRUE)

  # Ensure female qx doesn't exceed male qx at younger ages (< 85)
  # At ages 85+, small populations can cause random crossovers that are
  # handled by HMD calibration for 85-99.
  if (has_year) {
    male_lookup <- result[sex == "male", .(year, age, male_qx = qx)]
    result <- merge(result, male_lookup, by = c("year", "age"), all.x = TRUE)
  } else {
    male_lookup <- result[sex == "male", .(age, male_qx = qx)]
    result <- merge(result, male_lookup, by = "age", all.x = TRUE)
  }
  # Only cap female qx at male qx for ages < 85
  result[sex == "female" & age < 85 & !is.na(male_qx), qx := pmin(qx, male_qx)]
  result[, male_qx := NULL]

  # Recalculate q100+ if max_age == 100
  # This ensures q100+ is consistent with the HMD-adjusted q98 and q99
  if (max_age == 100) {
    # Check if we have data to calculate 100+ (needs 98, 99)
    # We use the 'result' dt which now has adjusted 98/99
    q100_new <- calculate_tr_q100_plus(result)
    
    if (nrow(q100_new) > 0) {
      q100_new[, age := 100L]
      
      # Add missing columns from result to q100_new
      for (col in setdiff(names(result), names(q100_new))) {
        q100_new[, (col) := NA]
      }
      
      # Remove any existing age 100 from result (shouldn't be there given logic above, but safety)
      result <- result[age != 100]
      result <- data.table::rbindlist(list(result, q100_new), fill = TRUE)
    }
  }

  # Sort
  if (has_year) {
    data.table::setorder(result, year, sex, age)
  } else {
    data.table::setorder(result, sex, age)
  }

  n_adjusted <- nrow(result[age >= transition_age & age <= 99])
  cli::cli_alert_success(
    "Adjusted {n_adjusted} qx values for ages {transition_age}-99 using HMD calibration"
  )
  if (max_age == 100) {
    cli::cli_alert_success("Recalculated q100+ based on adjusted q98/q99")
  }

  result
}

# =============================================================================
# Phase 2D: Life Tables and Summary Statistics
# =============================================================================

#' Calculate complete period life table
#'
#' @description
#' Calculates a complete period life table from death probabilities (qx).
#' Returns all standard life table columns.
#'
#' @param qx data.table with death probabilities (must have age, sex, qx columns;
#'   optionally year for multiple years)
#' @param radix Integer: starting population for life table (default: 100,000)
#' @param max_age Integer: maximum age in life table (default: 100, representing 100+)
#'
#' @return data.table with life table columns:
#'   - age: exact age x
#'   - sex: male or female
#'   - year: calendar year (if present in input)
#'   - qx: probability of death between ages x and x+1
#'   - px: probability of survival (1 - qx)
#'   - lx: number surviving to exact age x
#'   - dx: number dying between ages x and x+1
#'   - Lx: person-years lived between ages x and x+1
#'   - Tx: total person-years lived above age x
#'   - ex: life expectancy at age x
#'
#' @details
#' Life table formulas:
#' - px = 1 - qx
#' - lx+1 = lx * px
#' - dx = lx * qx = lx - lx+1
#' - Lx = (lx + lx+1) / 2 for ages 1+; special formula for age 0
#' - Tx = sum of Lx from age x to omega
#' - ex = Tx / lx
#'
#' For age 0 (infant), Lx uses separation factor:
#' L0 = l1 + f0 * d0
#' where f0 is derived from q0 using the Coale-Demeny formula.
#'
#' @param f0 Optional numeric: infant separation factor. If NULL (default),
#'   derived from q0 using the Coale-Demeny formula by sex.
#'
#' @export
calculate_life_table <- function(qx, radix = 100000, max_age = 100, f0 = NULL) {
  checkmate::assert_data_table(qx)
  checkmate::assert_names(names(qx), must.include = c("age", "sex", "qx"))
  checkmate::assert_int(radix, lower = 1)
  checkmate::assert_int(max_age, lower = 1, upper = 150)

  dt <- data.table::copy(qx)

  # Determine grouping columns (year if present, plus sex)
  has_year <- "year" %in% names(dt)
  if (has_year) {
    group_cols <- c("year", "sex")
  } else {
    group_cols <- "sex"
  }

  # Calculate life table within each group
  result <- dt[, {
    # Ensure sorted by age and complete
    sd_sorted <- .SD[order(age)]
    ages <- sd_sorted$age
    qx_vals <- sd_sorted$qx

    # Fill any missing ages with qx = 0 (or interpolate)
    full_ages <- 0:max_age
    qx_full <- rep(NA_real_, length(full_ages))
    qx_full[match(ages, full_ages)] <- qx_vals

    # For missing qx, use linear interpolation or last value
    for (i in seq_along(qx_full)) {
      if (is.na(qx_full[i])) {
        if (i > 1 && !is.na(qx_full[i - 1])) {
          qx_full[i] <- qx_full[i - 1]
        } else {
          qx_full[i] <- 0
        }
      }
    }

    # Cap qx at 1.0
    qx_full <- pmin(qx_full, 1.0)

    n <- length(full_ages)

    # px: survival probability
    px <- 1 - qx_full

    # lx: survivors to exact age x (radix at age 0)
    lx <- numeric(n)
    lx[1] <- radix
    for (i in 2:n) {
      lx[i] <- lx[i - 1] * px[i - 1]
    }

    # dx: deaths between ages x and x+1
    dx <- lx * qx_full

    # Lx: person-years lived between ages x and x+1
    # Standard formula: Lx = (lx + lx+1) / 2
    # For last age (omega): L_omega = lx / mx, but since qx ≈ 1, use lx * 0.5
    Lx <- numeric(n)

    # Age 0: special separation factor for infant mortality
    # L0 = l1 + f0 * d0, where f0 is the fraction of the year lived by
    # those dying in their first year.
    # Coale-Demeny formula derives f0 from q0 by sex (Actuarial Study 120):
    #   Male:   f0 = 0.045 + 2.684 * q0  (if q0 < 0.107, else 0.330)
    #   Female: f0 = 0.053 + 2.800 * q0  (if q0 < 0.107, else 0.350)
    if (n >= 2) {
      if (!is.null(f0)) {
        f0_val <- f0  # Use explicitly provided value
      } else {
        # Derive from q0 using Coale-Demeny formula
        q0_val <- qx_full[1]
        current_sex <- .BY$sex
        if (current_sex == "male") {
          f0_val <- if (q0_val >= 0.107) 0.330 else 0.045 + 2.684 * q0_val
        } else {
          f0_val <- if (q0_val >= 0.107) 0.350 else 0.053 + 2.800 * q0_val
        }
      }
      Lx[1] <- lx[2] + f0_val * dx[1]
    } else {
      Lx[1] <- lx[1] * 0.5
    }

    # Ages 1 to second-to-last: average of lx and lx+1
    for (i in 2:(n - 1)) {
      Lx[i] <- (lx[i] + lx[i + 1]) / 2
    }

    # Last age (omega): L_omega = l_omega / m_omega
    # Derive mx from qx for the open-ended age interval. For qx close to 1,
    # use fallback to avoid division by zero.
    qx_omega <- qx_full[n]
    if (qx_omega >= 0.999) {
      Lx[n] <- lx[n] * 0.5  # Fallback when nearly everyone dies
    } else {
      mx_omega <- -log(1 - qx_omega)
      Lx[n] <- lx[n] / mx_omega
    }

    # Tx: total person-years lived above age x
    Tx <- numeric(n)
    Tx[n] <- Lx[n]
    for (i in (n - 1):1) {
      Tx[i] <- Tx[i + 1] + Lx[i]
    }

    # ex: life expectancy at age x
    ex <- Tx / lx
    # Handle division by zero for very old ages where lx ≈ 0
    ex[lx < 0.5] <- 0

    data.table::data.table(
      age = full_ages,
      qx = qx_full,
      px = px,
      lx = lx,
      dx = dx,
      Lx = Lx,
      Tx = Tx,
      ex = ex
    )
  }, by = group_cols]

  data.table::setorder(result, sex, age)
  if (has_year) {
    data.table::setorder(result, year, sex, age)
  }

  n_tables <- length(unique(result$sex))
  if (has_year) {
    n_tables <- n_tables * length(unique(result$year))
  }

  cli::cli_alert_success(
    "Calculated {n_tables} life table(s) with {max_age + 1} ages each"
  )

  result
}

#' Calculate life expectancy series
#'
#' @description
#' Extracts life expectancy at specified ages from life tables.
#' Commonly used ages: 0 (at birth), 65 (retirement age).
#'
#' @param life_table data.table from calculate_life_table()
#' @param at_ages Integer vector of ages for life expectancy (default: c(0, 65))
#'
#' @return data.table with year, sex, age, ex columns
#'
#' @export
calculate_life_expectancy <- function(life_table, at_ages = c(0, 65)) {
  checkmate::assert_data_table(life_table)
  checkmate::assert_names(names(life_table), must.include = c("age", "sex", "ex"))
  checkmate::assert_integerish(at_ages, lower = 0)

  # Filter to requested ages
  result <- life_table[age %in% at_ages, ]

  # Select relevant columns
  has_year <- "year" %in% names(result)
  if (has_year) {
    result <- result[, .(year, sex, age, ex)]
    data.table::setorder(result, year, sex, age)
  } else {
    result <- result[, .(sex, age, ex)]
    data.table::setorder(result, sex, age)
  }

  cli::cli_alert_success(
    "Extracted life expectancy at ages {paste(at_ages, collapse = ', ')}"
  )

  result
}

#' Calculate age-adjusted death rates
#'
#' @description
#' Calculates age-adjusted death rates (ADR by sex, ASDR for both sexes)
#' using the 2010 Census standard population as weights.
#'
#' @param mx data.table with central death rates (year, age, sex, mx)
#' @param standard_pop data.table with standard population (age, sex, population)
#'   If NULL, fetches 2010 Census standard population.
#'
#' @return data.table with columns:
#'   - year: calendar year
#'   - sex: "male", "female", or "both"
#'   - adr: age-adjusted death rate (deaths per 100,000)
#'
#' @details
#' Age-adjusted death rate formula:
#' ADR_s = sum(SP_x * mx_s) / sum(SP_x)
#'
#' Where SP_x is the standard population at age x.
#'
#' ASDR (both sexes) uses sex-specific standard population:
#' ASDR = sum(SP_x,s * mx_x,s) / sum(SP_x,s)
#'
#' Results are expressed per 100,000 population.
#'
#' @export
calculate_age_adjusted_death_rates <- function(mx, standard_pop = NULL) {
  checkmate::assert_data_table(mx)
  checkmate::assert_names(names(mx), must.include = c("age", "sex", "mx"))

  # Get standard population if not provided
  if (is.null(standard_pop)) {
    standard_pop <- get_standard_population_2010()
  }
  checkmate::assert_data_table(standard_pop)
  checkmate::assert_names(names(standard_pop), must.include = c("age", "sex", "population"))

  # Determine if mx has year column

  has_year <- "year" %in% names(mx)

  # Ensure we have matching ages in both datasets
  # Standard pop typically goes to age 100, mx may go higher
  max_std_age <- max(standard_pop$age)

  # Filter mx to ages with standard population
  mx_filtered <- mx[age <= max_std_age]

  # Calculate ADR by sex
  results_list <- list()

  # Get unique years if present
  if (has_year) {
    years <- unique(mx_filtered$year)
  } else {
    years <- NA
  }

  for (sex_val in c("male", "female")) {
    # Get standard population for this sex
    sp <- standard_pop[sex == sex_val, .(age, population)]
    total_sp <- sum(sp$population)

    for (yr in years) {
      if (is.na(yr)) {
        mx_subset <- mx_filtered[sex == sex_val]
      } else {
        mx_subset <- mx_filtered[sex == sex_val & year == yr]
      }

      if (nrow(mx_subset) == 0) next

      # Merge with standard population
      merged <- merge(mx_subset, sp, by = "age", all.x = TRUE, suffixes = c("", "_sp"))
      # Handle population column - use standard pop
      if ("population_sp" %in% names(merged)) {
        merged[, population := population_sp]
        merged[, population_sp := NULL]
      }
      merged[is.na(population), population := 0]

      # Calculate weighted death rate
      adr <- sum(merged$population * merged$mx, na.rm = TRUE) / total_sp

      # Convert to per 100,000
      adr <- adr * 100000

      if (is.na(yr)) {
        results_list[[length(results_list) + 1]] <- data.table::data.table(
          sex = sex_val,
          adr = adr
        )
      } else {
        results_list[[length(results_list) + 1]] <- data.table::data.table(
          year = yr,
          sex = sex_val,
          adr = adr
        )
      }
    }
  }

  # Calculate ASDR (both sexes combined)
  sp_both <- standard_pop[sex %in% c("male", "female")]
  total_sp_both <- sum(sp_both$population)

  for (yr in years) {
    if (is.na(yr)) {
      mx_subset <- mx_filtered[sex %in% c("male", "female")]
    } else {
      mx_subset <- mx_filtered[sex %in% c("male", "female") & year == yr]
    }

    if (nrow(mx_subset) == 0) next

    # Merge with standard population (by age and sex)
    merged <- merge(mx_subset, sp_both[, .(age, sex, population)],
                    by = c("age", "sex"), all.x = TRUE, suffixes = c("", "_sp"))
    # Handle population column - use standard pop
    if ("population_sp" %in% names(merged)) {
      merged[, population := population_sp]
      merged[, population_sp := NULL]
    }
    merged[is.na(population), population := 0]

    # Calculate weighted death rate
    asdr <- sum(merged$population * merged$mx, na.rm = TRUE) / total_sp_both

    # Convert to per 100,000
    asdr <- asdr * 100000

    if (is.na(yr)) {
      results_list[[length(results_list) + 1]] <- data.table::data.table(
        sex = "both",
        adr = asdr
      )
    } else {
      results_list[[length(results_list) + 1]] <- data.table::data.table(
        year = yr,
        sex = "both",
        adr = asdr
      )
    }
  }

  result <- data.table::rbindlist(results_list, fill = TRUE)

  if (has_year) {
    data.table::setorder(result, year, sex)
  } else {
    data.table::setorder(result, sex)
  }

  cli::cli_alert_success(
    "Calculated age-adjusted death rates for {nrow(result)} year-sex combinations"
  )

  result
}

#' Get 2010 Census standard population
#'
#' @description
#' Returns the 2010 Census standard population by single year of age and sex.
#' Used as weights for age-adjusted death rate calculations.
#'
#' @param cache_dir Directory for caching
#'
#' @return data.table with columns: age, sex, population
#'
#' @export
get_standard_population_2010 <- function(cache_dir = here::here("data/raw/census")) {
  # Check for cached file
  cache_file <- file.path(cache_dir, "standard_population_2010.rds")

  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  # Fetch actual single-year-of-age data from Census API for 2010
  cli::cli_alert_info("Fetching 2010 Census single-year-of-age population from API...")

  tryCatch({
    male_pop <- fetch_census_population(years = 2010, ages = 0:100, sex = "male")
    female_pop <- fetch_census_population(years = 2010, ages = 0:100, sex = "female")

    standard_pop <- data.table::rbindlist(list(male_pop, female_pop), use.names = TRUE)
    standard_pop <- standard_pop[, .(age, sex, population)]
    data.table::setorder(standard_pop, sex, age)

    # Cache the result
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(standard_pop, cache_file)

    cli::cli_alert_success("Fetched and cached 2010 standard population (single-year-of-age)")
    return(standard_pop)
  }, error = function(e) {
    cli::cli_alert_warning(
      "Census API unavailable for 2010 standard population: {conditionMessage(e)}"
    )
    cli::cli_alert_warning("Using fallback 5-year age group approximation")
  })

  # Fallback: 5-year age group approximation (only used if API fails)
  # Source: Census Bureau 2010 Demographic Profile (SF1)
  age_groups <- data.table::data.table(
    age_start = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85),
    age_end = c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 100),
    pop_male = c(10319427, 10389638, 10579862, 11303666, 11014176, 10635591,
                 9996500, 10042022, 10393977, 11209085, 10933274, 9523648,
                 7483818, 5765502, 4243972, 3182388, 2294374, 1273867),
    pop_female = c(9881935, 9959019, 10097332, 10736677, 10571823, 10466258,
                   10137620, 10154272, 10562525, 11468206, 11217456, 9970062,
                   8077500, 6582716, 5094129, 4135407, 3393811, 2723668)
  )

  results <- list()
  for (i in seq_len(nrow(age_groups))) {
    ages_in_group <- age_groups$age_start[i]:age_groups$age_end[i]
    n_ages <- length(ages_in_group)
    for (age in ages_in_group) {
      results[[length(results) + 1]] <- data.table::data.table(
        age = age, sex = "male",
        population = round(age_groups$pop_male[i] / n_ages)
      )
      results[[length(results) + 1]] <- data.table::data.table(
        age = age, sex = "female",
        population = round(age_groups$pop_female[i] / n_ages)
      )
    }
  }

  standard_pop <- data.table::rbindlist(results)
  data.table::setorder(standard_pop, sex, age)

  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(standard_pop, cache_file)

  cli::cli_alert_success("Created and cached 2010 standard population (fallback)")
  standard_pop
}

#' Calculate death probabilities by marital status
#'
#' @description
#' Applies marital status relative mortality factors to total qx values.
#' Per SSA methodology, all marital statuses converge to the same qx at age 95.
#'
#' @param qx_total data.table with total death probabilities (year, age, sex, qx)
#' @param marital_factors data.table with relative mortality by marital status
#'   If NULL, uses default factors based on SSA methodology.
#'
#' @return data.table with qx by year, age, sex, marital_status
#'
#' @details
#' Marital status categories:
#' - married: lowest mortality
#' - widowed: elevated mortality
#' - divorced: elevated mortality
#' - single: elevated mortality
#'
#' Relative factors from empirical studies (approximate):
#' - Married: 1.0 (reference)
#' - Widowed: 1.15-1.30 depending on age/sex
#' - Divorced: 1.20-1.40
#' - Never married: 1.25-1.50
#'
#' All converge to 1.0 at age 95.
#'
#' @export
calculate_qx_by_marital_status <- function(qx_total, marital_factors = NULL) {
  checkmate::assert_data_table(qx_total)
  checkmate::assert_names(names(qx_total), must.include = c("age", "sex", "qx"))

  # Default marital status relative mortality factors
  # Uses TR2025 methodology with real NCHS deaths and ACS population data
  if (is.null(marital_factors)) {
    marital_factors <- get_marital_mortality_factors()
  }

  dt <- data.table::copy(qx_total)
  has_year <- "year" %in% names(dt)

  # Expand to all marital statuses
  marital_statuses <- c("married", "widowed", "divorced", "single")
  result_list <- list()

  for (ms in marital_statuses) {
    dt_ms <- data.table::copy(dt)
    dt_ms[, marital_status := ms]

    # Get factors for this marital status
    factors_ms <- marital_factors[marital_status == ms]

    # Merge factors by age and sex
    dt_ms <- merge(dt_ms, factors_ms[, .(age, sex, relative_factor)],
                   by = c("age", "sex"), all.x = TRUE)

    # Fill missing factors with 1.0 (no adjustment)
    dt_ms[is.na(relative_factor), relative_factor := 1.0]

    # NOTE (Deviation #13): Convergence at ages 85-95 is already applied once
    # in calculate_marital_mortality_factors() Step 3 (blends rates toward total
    # rate). A second application here was over-suppressing the differential
    # (75% reduction at age 90 instead of the documented 50%). Removed per
    # TR2025 Section 1.2.c Step 3 which specifies a single convergence.

    # Apply factor to qx
    dt_ms[, qx := qx * relative_factor]

    # Cap at 1.0
    dt_ms[qx > 1, qx := 1.0]

    dt_ms[, relative_factor := NULL]
    result_list[[ms]] <- dt_ms
  }

  result <- data.table::rbindlist(result_list)

  if (has_year) {
    data.table::setorder(result, year, sex, marital_status, age)
  } else {
    data.table::setorder(result, sex, marital_status, age)
  }

  cli::cli_alert_success(
    "Calculated qx for {length(marital_statuses)} marital statuses"
  )

  result
}

# =============================================================================
# Marital Status Mortality Differentials - TR2025 Methodology
# =============================================================================

#' Apply Whittaker-Henderson smoothing
#'
#' @description
#' Applies Whittaker-Henderson smoothing to a vector of values.
#' Used per TR2025 methodology for smoothing death rates by single year of age.
#'
#' @param values Numeric vector of values to smooth
#' @param degree Degree parameter (default: 2)
#' @param smoothing Smoothing parameter (default: 0.01)
#'
#' @return Numeric vector of smoothed values
#'
#' @details
#' Whittaker-Henderson smoothing minimizes:
#'   sum(w * (y - z)^2) + smoothing * sum(diff(z, differences = degree)^2)
#' where y is the input, z is the smoothed output, and w are weights (assumed 1).
#'
#' For TR2025 mortality differentials, uses degree=2 and smoothing=0.01 for ages 15-94.
#'
#' @references
#' Whittaker, E. T. (1923). "On a New Method of Graduation".
#' Proceedings of the Edinburgh Mathematical Society, 41, 63-75.
#'
#' @keywords internal
wh_smooth_marital <- function(values, degree = 2, smoothing = 0.01) {
  n <- length(values)
  if (n < degree + 1) {
    return(values)  # Can't smooth if too few points
  }

  # Create identity matrix
  I <- diag(n)

  # Create difference matrix of specified degree
  D <- diff(I, differences = degree)

  # Solve the smoothing equation: (I + smoothing * D'D) * z = y
  # Where z is the smoothed values
  smoothing_matrix <- I + smoothing * crossprod(D)

  # Solve for smoothed values
  smoothed <- solve(smoothing_matrix, values)

  as.numeric(smoothed)
}


#' Calculate marital status mortality factors from data
#'
#' @description
#' Calculates relative mortality factors by marital status using NCHS deaths
#' and ACS population data per TR2025 methodology (Section 1.2.c).
#'
#' @param nchs_deaths data.table with deaths by year, age, sex, marital_status
#' @param acs_population data.table with population by year, age, sex, marital_status
#' @param reference_years Years to use for calculation (default: 2015:2019)
#'
#' @return data.table with columns: age, sex, marital_status, relative_factor
#'
#' @details
#' Implements the 7-step TR2025 methodology:
#' 1. Calculate preliminary death rates (deaths / population)
#' 2. Adjust older age rates for consistency
#' 3. Converge all marital statuses to same rate at age 95
#' 4. Ages under 15 use total death rates (no differential)
#' 5. Apply Whittaker-Henderson smoothing (degree=2, smoothing=0.5) for ages 15-94
#' 6. Adjust ages 15-20 non-single to match age 21 ratio
#' 7. Convert death rates to qx using qx = mx/(1+0.5*mx), then calculate
#'    relative factors from qx values (married = 1.0 reference)
#'
#' @export
calculate_marital_mortality_factors <- function(
    nchs_deaths,
    acs_population,
    reference_years = 2015:2019
) {
  checkmate::assert_data_table(nchs_deaths)
  checkmate::assert_data_table(acs_population)
  checkmate::assert_names(
    names(nchs_deaths),
    must.include = c("year", "age", "sex", "marital_status", "deaths")
  )
  checkmate::assert_names(
    names(acs_population),
    must.include = c("year", "age", "sex", "marital_status", "population")
  )

  cli::cli_alert_info("Calculating marital mortality factors from data...")

  # Filter to reference years
  deaths <- nchs_deaths[year %in% reference_years]
  pop <- acs_population[year %in% reference_years]

  # Handle separated marital status in ACS (combine with married)
  pop[marital_status == "separated", marital_status := "married"]

  # Step 1: Calculate preliminary death rates
  # Aggregate deaths and population across reference years
  deaths_agg <- deaths[, .(deaths = sum(deaths)), by = .(age, sex, marital_status)]
  pop_agg <- pop[, .(population = sum(population)), by = .(age, sex, marital_status)]

  # Merge deaths and population
  rates <- merge(deaths_agg, pop_agg, by = c("age", "sex", "marital_status"), all = TRUE)

  # Handle missing combinations
  rates[is.na(deaths), deaths := 0]
  rates[is.na(population) | population == 0, population := NA_real_]

  # Calculate death rate
  rates[, death_rate := deaths / population]

  # Step 2: Adjust older age populations and death rates for consistency
  # For ages 85+, use ratio-based adjustment to ensure smooth progression
  for (sx in c("male", "female")) {
    for (ms in unique(rates$marital_status)) {
      # Get rates for this sex/marital status
      idx <- rates$sex == sx & rates$marital_status == ms & rates$age >= 80 & rates$age <= 94
      subset_rates <- rates[idx]

      if (nrow(subset_rates) > 0) {
        # Fill in missing older age rates using extrapolation
        for (a in 85:94) {
          if (is.na(rates[age == a & sex == sx & marital_status == ms, death_rate]) ||
              rates[age == a & sex == sx & marital_status == ms, population] < 1000) {
            # Use rate progression from younger ages
            prev_rate <- rates[age == a - 1 & sex == sx & marital_status == ms, death_rate]
            if (!is.na(prev_rate) && prev_rate > 0) {
              # Assume ~8% increase per year for older ages
              rates[age == a & sex == sx & marital_status == ms, death_rate := prev_rate * 1.08]
            }
          }
        }
      }
    }
  }

  # Step 3: Convergence at age 95
  # Calculate total death rate (all marital statuses combined) at each age
  total_deaths <- deaths_agg[, .(total_deaths = sum(deaths)), by = .(age, sex)]
  total_pop <- pop_agg[, .(total_pop = sum(population)), by = .(age, sex)]
  total_rates <- merge(total_deaths, total_pop, by = c("age", "sex"))
  total_rates[, total_rate := total_deaths / total_pop]

  # Linear interpolation from age 85 to 95 toward total rate
  for (sx in c("male", "female")) {
    total_rate_95 <- total_rates[age == 94 & sex == sx, total_rate] * 1.08
    if (is.na(total_rate_95) || length(total_rate_95) == 0) {
      total_rate_95 <- 0.4  # Fallback for age 95
    }

    for (ms in unique(rates$marital_status)) {
      for (a in 85:94) {
        current_rate <- rates[age == a & sex == sx & marital_status == ms, death_rate]
        if (!is.na(current_rate)) {
          # Blend toward total rate: weight increases linearly from 0 at 85 to 1 at 95
          blend_weight <- (a - 85) / 10
          target_rate <- total_rates[age == a & sex == sx, total_rate]
          if (!is.na(target_rate)) {
            blended <- current_rate * (1 - blend_weight) + target_rate * blend_weight
            rates[age == a & sex == sx & marital_status == ms, death_rate := blended]
          }
        }
      }
      # At age 95+, use total rate
      rates[age >= 95 & sex == sx & marital_status == ms, death_rate := total_rate_95]
    }
  }

  # Step 4: Ages under 15 - assign total death rates (no marital differential)
  for (sx in c("male", "female")) {
    for (a in 0:14) {
      total_rate_a <- total_rates[age == a & sex == sx, total_rate]
      if (length(total_rate_a) == 0 || is.na(total_rate_a)) {
        total_rate_a <- 0.001  # Fallback
      }
      for (ms in unique(rates$marital_status)) {
        rates[age == a & sex == sx & marital_status == ms, death_rate := total_rate_a]
      }
    }
  }

  # Step 5: Whittaker-Henderson smoothing for ages 15-94
  for (sx in c("male", "female")) {
    for (ms in unique(rates$marital_status)) {
      # Get death rates for ages 15-94 in age order
      subset_rows <- rates[sex == sx & marital_status == ms & age >= 15 & age <= 94]
      setorder(subset_rows, age)

      dr_values <- subset_rows$death_rate

      # Handle any remaining NAs by interpolation
      if (any(is.na(dr_values))) {
        dr_values <- stats::approx(
          x = which(!is.na(dr_values)),
          y = dr_values[!is.na(dr_values)],
          xout = seq_along(dr_values),
          rule = 2
        )$y
      }

      # Apply smoothing
      smoothed <- wh_smooth_marital(dr_values, degree = 2, smoothing = 0.01)

      # Store back - update rates table by age (avoids data.table chained subset bug)
      ages_to_update <- subset_rows$age
      for (i in seq_along(ages_to_update)) {
        rates[sex == sx & marital_status == ms & age == ages_to_update[i],
              death_rate := smoothed[i]]
      }
    }
  }

  # Step 6: Adjust ages 15-20 non-single marital statuses
  # Use ratio relative to single at age 21
  for (sx in c("male", "female")) {
    single_21 <- rates[age == 21 & sex == sx & marital_status == "single", death_rate]
    if (is.na(single_21) || length(single_21) == 0) next

    for (ms in c("married", "widowed", "divorced")) {
      ratio_21 <- rates[age == 21 & sex == sx & marital_status == ms, death_rate] / single_21
      if (is.na(ratio_21) || length(ratio_21) == 0) next

      for (a in 15:20) {
        nm_rate <- rates[age == a & sex == sx & marital_status == "single", death_rate]
        if (!is.na(nm_rate)) {
          rates[age == a & sex == sx & marital_status == ms, death_rate := nm_rate * ratio_21]
        }
      }
    }
  }

  # Step 7: Convert death rates to qx, then calculate relative factors
 # Per TR2025 methodology: "Converting these death rates to probabilities of death"
  # Formula: qx = mx / (1 + 0.5*mx) for ages 2-99
  # Then calculate factors relative to married qx for each age/sex
  rates[, qx := death_rate / (1 + 0.5 * death_rate)]

  # Get married qx as reference
  married_qx <- rates[marital_status == "married", .(age, sex, married_qx = qx)]
  rates <- merge(rates, married_qx, by = c("age", "sex"), all.x = TRUE)

  # Calculate relative factor from qx (not mx)
  rates[, relative_factor := qx / married_qx]

  # Handle edge cases (NA/Inf from division issues)
  rates[is.na(relative_factor) | is.infinite(relative_factor), relative_factor := 1.0]
  rates[marital_status == "married", relative_factor := 1.0]

  # Note: No caps applied - TR2025 methodology relies on:
  # - Whittaker-Henderson smoothing (step 5) to handle noise
  # - Age 15-20 adjustment (step 6) to handle sparse young age data
  # - Convergence at age 95 (step 3) for oldest ages

  # Select output columns
  result <- rates[, .(age, sex, marital_status, relative_factor)]
  data.table::setorder(result, sex, marital_status, age)

  cli::cli_alert_success("Calculated marital mortality factors for {nrow(result)} age/sex/status combinations")

  result
}


#' Get marital status mortality factors
#'
#' @description
#' Returns relative mortality factors by marital status, age, and sex.
#' Calculated from NCHS deaths and ACS population data per TR2025 methodology.
#'
#' @param use_cache Logical: if TRUE, use cached factors if available
#' @param cache_dir Character: directory for cached factors
#'
#' @return data.table with columns: age, sex, marital_status, relative_factor
#'
#' @export
get_marital_mortality_factors <- function(
    use_cache = TRUE,
    cache_dir = here::here("data/cache/mortality")
) {
  cache_file <- file.path(cache_dir, "marital_mortality_factors.rds")

  if (use_cache && file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached marital mortality factors")
    return(readRDS(cache_file))
  }

  cli::cli_alert_info("Fetching data and calculating marital mortality factors...")

  # Fetch NCHS deaths by marital status
  nchs_deaths <- fetch_nchs_deaths_by_marital_status(years = 2015:2019)

  # Fetch ACS PUMS population by marital status
  acs_pop <- fetch_acs_pums_marital_status(years = 2015:2019, ages = 0:99)

  # Calculate factors
  factors <- calculate_marital_mortality_factors(
    nchs_deaths = nchs_deaths,
    acs_population = acs_pop,
    reference_years = 2015:2019
  )

  # Cache results
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(factors, cache_file)
  cli::cli_alert_success("Cached marital mortality factors to {cache_file}")

  factors
}


#' Get default marital status mortality factors (DEPRECATED)
#'
#' @description
#' Returns fabricated relative mortality factors by marital status, age, and sex.
#' DEPRECATED: Use `get_marital_mortality_factors()` for real data-based factors.
#'
#' @return data.table with columns: age, sex, marital_status, relative_factor
#'
#' @details
#' Factors represent the relative mortality risk compared to married persons.
#' - Married: 1.00 (reference)
#' - Widowed: 1.15-1.25 (higher for males, younger ages)
#' - Divorced: 1.20-1.35 (higher for males, younger ages)
#' - Never married: 1.25-1.45 (higher for males, younger ages)
#'
#' @keywords internal
get_default_marital_factors <- function() {
  # Create factors for ages 15-100 (marital status only relevant for adults)
  ages <- 15:100

  results <- list()

  for (sex_val in c("male", "female")) {
    for (age_val in ages) {
      # Base factors vary by sex (males have larger differentials)
      sex_mult <- if (sex_val == "male") 1.0 else 0.85

      # Age adjustment: differentials are larger at younger ages
      age_factor <- 1.0 + 0.3 * pmax(0, (65 - age_val)) / 50

      # Married: reference (1.0)
      results[[length(results) + 1]] <- data.table::data.table(
        age = age_val,
        sex = sex_val,
        marital_status = "married",
        relative_factor = 1.0
      )

      # Widowed
      widowed_base <- 1.15 + 0.10 * sex_mult
      results[[length(results) + 1]] <- data.table::data.table(
        age = age_val,
        sex = sex_val,
        marital_status = "widowed",
        relative_factor = widowed_base * age_factor
      )

      # Divorced
      divorced_base <- 1.20 + 0.15 * sex_mult
      results[[length(results) + 1]] <- data.table::data.table(
        age = age_val,
        sex = sex_val,
        marital_status = "divorced",
        relative_factor = divorced_base * age_factor
      )

      # Never married
      single_base <- 1.25 + 0.20 * sex_mult
      results[[length(results) + 1]] <- data.table::data.table(
        age = age_val,
        sex = sex_val,
        marital_status = "single",
        relative_factor = single_base * age_factor
      )
    }
  }

  data.table::rbindlist(results)
}

# =============================================================================
# Phase 2D: Infant Mortality (q0) Calculation
# =============================================================================

#' Calculate infant mortality rate (q0) using detailed age-at-death data
#'
#' @description
#' Computes the probability of death in the first year of life (q0) using
#' detailed infant death data by age (hours, days, months) and monthly birth
#' data to properly calculate exposure.
#'
#' @param infant_deaths data.table with columns: year, sex, age_unit, age_value, deaths
#'   - age_unit: "minutes", "hours", "days", or "months"
#'   - age_value: numeric age in that unit
#' @param monthly_births data.table with columns: year, month, births
#' @param year Integer: year for which to calculate q0
#' @param method Character: "separation_factor" (default) or "simple"
#' @param births_by_sex Optional data.table with columns: year, sex, births.
#'   If provided, uses actual sex-specific birth counts instead of a fixed
#'   51.2% male ratio.
#'
#' @return data.table with columns: year, sex, q0, deaths, births, exposure
#'
#' @details
#' The SSA methodology uses detailed infant death data to properly calculate
#' infant mortality. Deaths are categorized by age at death:
#' - Neonatal (0-27 days): mostly deaths to infants born in the same year
#' - Post-neonatal (28-364 days): mixed between current and previous year births
#'
#' The separation factor approach allocates deaths between birth cohorts based
#' on the monthly distribution of births and deaths.
#'
#' Formula: q0 = D0 / E0
#' Where:
#' - D0 = total infant deaths during year
#' - E0 = exposure (births adjusted for separation factor)
#'
#' NOTE (Deviation #16): TR2025 Item 7 provides starting qx values from
#' 1939-41 decennial life tables for infant/toddler age groups. These are only
#' used for the 1940-1967 period. ARTEMIS computes q0 from 1968 onward using
#' NCHS microdata, so the 1939-41 starting values are not needed.
#'
#' @export
calculate_infant_mortality <- function(infant_deaths, monthly_births, year,
                                       method = c("separation_factor", "simple"),
                                       births_by_sex = NULL) {
  method <- match.arg(method)

  checkmate::assert_data_table(infant_deaths)
  checkmate::assert_data_table(monthly_births)
  checkmate::assert_int(year, lower = 1900, upper = 2100)
  checkmate::assert_names(names(infant_deaths),
    must.include = c("year", "sex", "age_unit", "age_value", "deaths"))
  checkmate::assert_names(names(monthly_births),
    must.include = c("year", "month", "births"))

  # Filter to the specified year (use local variable to avoid data.table scoping issues)
  target_year <- year
  prev_year <- year - 1L
  deaths_yr <- infant_deaths[year == target_year]
  births_yr <- monthly_births[year == target_year]
  births_prev <- monthly_births[year == prev_year]

  if (nrow(deaths_yr) == 0) {
    cli::cli_abort("No infant deaths data for year {year}")
  }
  if (nrow(births_yr) == 0) {
    cli::cli_abort("No monthly births data for year {year}")
  }

  # Total births for current and previous year
  total_births <- sum(births_yr$births)
  total_births_prev <- if (nrow(births_prev) > 0) sum(births_prev$births) else total_births

  # Sex-specific birth counts: use actual data if available, else fixed ratio
  if (!is.null(births_by_sex)) {
    sex_births_yr <- births_by_sex[year == target_year]
    sex_births_prev <- births_by_sex[year == prev_year]
  } else {
    sex_births_yr <- NULL
    sex_births_prev <- NULL
  }

  results <- list()

  for (sex_val in c("male", "female")) {
    # Use actual sex-specific birth counts when available
    if (!is.null(sex_births_yr) && nrow(sex_births_yr[sex == sex_val]) > 0) {
      births_sex <- sex_births_yr[sex == sex_val, births]
    } else {
      # Fallback: fixed sex ratio (CDC ~51.2% male)
      sex_ratio <- if (sex_val == "male") 0.512 else 0.488
      births_sex <- as.integer(round(total_births * sex_ratio))
    }
    if (!is.null(sex_births_prev) && nrow(sex_births_prev[sex == sex_val]) > 0) {
      births_prev_sex <- sex_births_prev[sex == sex_val, births]
    } else {
      sex_ratio <- if (sex_val == "male") 0.512 else 0.488
      births_prev_sex <- as.integer(round(total_births_prev * sex_ratio))
    }

    # Get deaths for this sex
    deaths_sex <- deaths_yr[sex == sex_val]

    if (nrow(deaths_sex) == 0) {
      cli::cli_warn("No infant deaths for {sex_val} in {year}")
      next
    }

    # Convert deaths to SSA age groupings
    deaths_by_group <- convert_infant_deaths_to_ssa_groups(deaths_sex)

    # Total infant deaths for this sex
    total_deaths <- sum(deaths_by_group$deaths)

    if (method == "simple") {
      # Simple method: q0 = deaths / births (sex-specific)
      # Assumes all deaths are to current year births
      # This underestimates q0 slightly
      q0 <- total_deaths / births_sex

      results[[length(results) + 1]] <- data.table::data.table(
        year = year,
        sex = sex_val,
        q0 = q0,
        deaths = total_deaths,
        births = births_sex,
        exposure = births_sex,
        method = "simple"
      )
    } else {
      # Separation factor method
      # Calculate what fraction of deaths are to current year births

      # Get separation factors based on monthly birth distribution
      sep_factors <- calculate_ssa_separation_factors(
        monthly_births_current = births_yr,
        monthly_births_prev = births_prev
      )

      # Apply separation factors to deaths by age group
      deaths_with_sep <- merge(
        deaths_by_group,
        sep_factors,
        by = "ssa_age_group",
        all.x = TRUE
      )

      # Fill missing separation factors (default to 1.0 for neonatal)
      deaths_with_sep[is.na(sep_factor), sep_factor := 1.0]

      # Deaths attributed to current year births
      deaths_current <- sum(deaths_with_sep$deaths * deaths_with_sep$sep_factor)
      deaths_prev <- sum(deaths_with_sep$deaths * (1 - deaths_with_sep$sep_factor))

      # Calculate period q0 by summing partial probabilities
      # q0 = (deaths_current / births_current) + (deaths_prev / births_prev)
      term1 <- deaths_current / births_sex
      term2 <- if (births_prev_sex > 0) deaths_prev / births_prev_sex else 0
      
      q0 <- term1 + term2

      results[[length(results) + 1]] <- data.table::data.table(
        year = year,
        sex = sex_val,
        q0 = q0,
        deaths = total_deaths,
        deaths_current_cohort = deaths_current,
        deaths_prev_cohort = deaths_prev,
        births = births_sex,
        births_prev = births_prev_sex,
        method = "separation_factor"
      )
    }
  }

  result <- data.table::rbindlist(results, fill = TRUE)
  cli::cli_alert_success(
    "Calculated q0 for {year}: male={round(result[sex=='male', q0], 5)}, female={round(result[sex=='female', q0], 5)}"
  )

  result
}

#' Convert infant deaths to SSA age groups
#'
#' @description
#' Converts infant deaths from various age units (minutes, hours, days, months)
#' to the SSA-specified age groupings for infant mortality calculation.
#'
#' SSA age groups (from 2025 Long-Range Model Documentation):
#' - under 1 day (includes minutes and hours)
#' - 1-2 days
#' - 3-6 days
#' - 7-27 days
#' - 28 days - 1 month
#' - 2 months, 3 months, ..., 11 months
#'
#' @param deaths data.table with age_unit, age_value, deaths columns
#'
#' @return data.table with ssa_age_group, deaths columns
#'
#' @keywords internal
convert_infant_deaths_to_ssa_groups <- function(deaths) {
  dt <- data.table::copy(deaths)

  # Assign SSA age groups based on age_unit and age_value
  # SSA groups: under_1_day, 1-2_days, 3-6_days, 7-27_days, 1_month, 2_months, ..., 11_months
  dt[, ssa_age_group := data.table::fcase(
    # Under 1 day: minutes and hours, or days == 0
    age_unit == "minutes", "under_1_day",
    age_unit == "hours", "under_1_day",
    age_unit == "days" & age_value == 0, "under_1_day",
    # 1-2 days
    age_unit == "days" & age_value >= 1 & age_value <= 2, "1-2_days",
    # 3-6 days
    age_unit == "days" & age_value >= 3 & age_value <= 6, "3-6_days",
    # 7-27 days
    age_unit == "days" & age_value >= 7 & age_value <= 27, "7-27_days",
    # Months: map directly
    age_unit == "months" & age_value == 1, "1_month",
    age_unit == "months" & age_value == 2, "2_months",
    age_unit == "months" & age_value == 3, "3_months",
    age_unit == "months" & age_value == 4, "4_months",
    age_unit == "months" & age_value == 5, "5_months",
    age_unit == "months" & age_value == 6, "6_months",
    age_unit == "months" & age_value == 7, "7_months",
    age_unit == "months" & age_value == 8, "8_months",
    age_unit == "months" & age_value == 9, "9_months",
    age_unit == "months" & age_value == 10, "10_months",
    age_unit == "months" & age_value == 11, "11_months",
    default = "other"
  )]

  # Aggregate deaths by SSA age group
result <- dt[ssa_age_group != "other", .(deaths = sum(deaths)), by = .(ssa_age_group)]

  result
}

#' Calculate SSA separation factors for infant mortality
#'
#' @description
#' Calculates the fraction of infant deaths at each SSA age group that should be
#' attributed to the current year's birth cohort vs the previous year's.
#'
#' @param monthly_births_current data.table with year, month, births for current year
#' @param monthly_births_prev data.table with year, month, births for previous year
#'
#' @return data.table with ssa_age_group, sep_factor columns
#'
#' @details
#' The separation factor represents what fraction of deaths at a given age
#' occurred to infants born in the current calendar year.
#'
#' SSA age groups (per 2025 Long-Range Model Documentation):
#' - under 1 day, 1-2 days, 3-6 days, 7-27 days
#' - 1 month, 2 months, ..., 11 months
#'
#' For neonatal deaths (first 27 days), almost all deaths are to infants
#' born in the current year (separation factor close to 1.0).
#'
#' For post-neonatal deaths by month, the separation factor depends on
#' when during the year the death occurred. An infant dying at age X months
#' in month M was born in month (M - X). If M - X <= 0, they were born in
#' the previous year.
#'
#' @keywords internal
calculate_ssa_separation_factors <- function(monthly_births_current, monthly_births_prev) {

  # For neonatal deaths (under 1 day through 27 days), nearly all deaths
  # are to infants born in the current calendar year.
  # Only deaths in January to infants aged close to 1 month could be from
  # previous year births.

  # Neonatal separation factors (very high - most deaths same year as birth)
  sep_under_1_day <- 1.0    # All deaths same day as birth
  sep_1_2_days <- 1.0       # Nearly all same year
  sep_3_6_days <- 0.995     # Very few from previous year (only late Dec births dying in Jan)
  sep_7_27_days <- 0.98     # Slightly more from previous year

  # Post-neonatal separation factors by month of age
  # Logic: death at age M months in calendar month C means birth in month (C - M)
  # If C - M <= 0, birth was in previous year
  # Averaging across the year:
  # - 1 month old: births from (C-1), so Jan deaths = Dec prev year births
  #   Fraction from current year = 11/12 ≈ 0.917
  # - 2 months old: Jan-Feb deaths from prev year births = 2/12 from prev year
  #   Fraction from current year = 10/12 ≈ 0.833
  # And so on...

  sep_1_month <- 11/12   # ~0.917
  sep_2_months <- 10/12  # ~0.833
  sep_3_months <- 9/12   # 0.75
  sep_4_months <- 8/12   # ~0.667
  sep_5_months <- 7/12   # ~0.583
  sep_6_months <- 6/12   # 0.5
  sep_7_months <- 5/12   # ~0.417
  sep_8_months <- 4/12   # ~0.333
  sep_9_months <- 3/12   # 0.25
  sep_10_months <- 2/12  # ~0.167
  sep_11_months <- 1/12  # ~0.083

  data.table::data.table(
    ssa_age_group = c("under_1_day", "1-2_days", "3-6_days", "7-27_days",
                      "1_month", "2_months", "3_months", "4_months",
                      "5_months", "6_months", "7_months", "8_months",
                      "9_months", "10_months", "11_months"),
    sep_factor = c(sep_under_1_day, sep_1_2_days, sep_3_6_days, sep_7_27_days,
                   sep_1_month, sep_2_months, sep_3_months, sep_4_months,
                   sep_5_months, sep_6_months, sep_7_months, sep_8_months,
                   sep_9_months, sep_10_months, sep_11_months)
  )
}

#' Calculate infant mortality for multiple years
#'
#' @description
#' Batch calculation of infant mortality (q0) across multiple years.
#'
#' @param infant_deaths data.table with all years of infant death data
#' @param monthly_births data.table with all years of monthly birth data
#' @param years Integer vector of years to calculate
#' @param method Character: calculation method (default: "separation_factor")
#'
#' @return data.table with q0 for all years and sexes
#'
#' @export
calculate_infant_mortality_series <- function(infant_deaths, monthly_births, years,
                                               method = "separation_factor",
                                               births_by_sex = NULL) {
  checkmate::assert_data_table(infant_deaths)
  checkmate::assert_data_table(monthly_births)
  checkmate::assert_integerish(years, min.len = 1)

  results <- list()

  for (yr in years) {
    tryCatch({
      result <- calculate_infant_mortality(
        infant_deaths = infant_deaths,
        monthly_births = monthly_births,
        year = yr,
        method = method,
        births_by_sex = births_by_sex
      )
      results[[length(results) + 1]] <- result
    }, error = function(e) {
      cli::cli_warn("Failed to calculate q0 for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No q0 values could be calculated")
  }

  data.table::rbindlist(results, fill = TRUE)
}

#' Load infant deaths detail data
#'
#' @description
#' Loads cached infant deaths detail data for specified years.
#'
#' @param years Integer vector of years to load
#' @param cache_dir Character: directory containing cached RDS files
#'
#' @return data.table with combined infant deaths data
#'
#' @export
load_infant_deaths <- function(years, cache_dir = "data/cache/nchs_deaths") {
  checkmate::assert_integerish(years, min.len = 1)
  checkmate::assert_directory_exists(cache_dir)

  results <- list()

  for (yr in years) {
    file_path <- file.path(cache_dir, sprintf("infant_deaths_detail_%d.rds", yr))

    if (file.exists(file_path)) {
      dt <- readRDS(file_path)
      results[[length(results) + 1]] <- data.table::as.data.table(dt)
    } else {
      cli::cli_warn("Infant deaths file not found for {yr}: {file_path}")
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No infant deaths data files found")
  }

  data.table::rbindlist(results, fill = TRUE)
}

#' Load monthly births data
#'
#' @description
#' Loads cached monthly births data for specified years.
#'
#' @param years Integer vector of years to load
#' @param cache_dir Character: directory containing cached RDS files
#'
#' @return data.table with combined monthly births data
#'
#' @export
load_monthly_births <- function(years, cache_dir = "data/raw/nchs") {
  checkmate::assert_integerish(years, min.len = 1)
  checkmate::assert_directory_exists(cache_dir)

  results <- list()

  for (yr in years) {
    file_path <- file.path(cache_dir, sprintf("births_by_month_%d.rds", yr))

    if (file.exists(file_path)) {
      dt <- readRDS(file_path)
      results[[length(results) + 1]] <- data.table::as.data.table(dt)
    } else {
      cli::cli_warn("Monthly births file not found for {yr}: {file_path}")
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No monthly births data files found")
  }

  data.table::rbindlist(results, fill = TRUE)
}

#' Project infant mortality (q0) for future years
#'
#' @description
#' Projects q0 for years after the last historical data year using the
#' SSA methodology: q0 = m0 × (q0/m0 ratio from last historical year).
#'
#' Per SSA 2025 Long-Range Model Documentation:
#' "After the last historical year, q0 is calculated from m0, assuming that
#' the ratio of q0 to m0 measured for the last historical year would remain
#' constant thereafter."
#'
#' @param q0_historical data.table with historical q0 values (year, sex, q0)
#' @param m0_series data.table with m0 values for all years (year, sex, m0)
#' @param projection_years Integer vector of years to project
#'
#' @return data.table with projected q0 values
#'
#' @export
project_infant_mortality <- function(q0_historical, m0_series, projection_years) {
  checkmate::assert_data_table(q0_historical)
  checkmate::assert_data_table(m0_series)
  checkmate::assert_names(names(q0_historical), must.include = c("year", "sex", "q0"))
  checkmate::assert_names(names(m0_series), must.include = c("year", "sex"))
  checkmate::assert_integerish(projection_years, min.len = 1)

  # Find the last historical year
  last_hist_year <- max(q0_historical$year)

  # Get q0 and m0 for last historical year
  q0_last <- q0_historical[year == last_hist_year]

  # Determine m0 column name (could be m0 or mx with age==0)
  if ("m0" %in% names(m0_series)) {
    m0_col <- "m0"
  } else if ("mx" %in% names(m0_series) && "age" %in% names(m0_series)) {
    m0_series <- m0_series[age == 0]
    m0_series[, m0 := mx]
    m0_col <- "m0"
  } else {
    cli::cli_abort("m0_series must have either 'm0' column or 'mx' and 'age' columns")
  }

  m0_last <- m0_series[year == last_hist_year]

  results <- list()

  for (sex_val in c("male", "female")) {
    # Calculate q0/m0 ratio for last historical year
    q0_val <- q0_last[sex == sex_val, q0]
    m0_val <- m0_last[sex == sex_val, m0]

    if (length(q0_val) == 0 || length(m0_val) == 0) {
      cli::cli_warn("Missing q0 or m0 for {sex_val} in year {last_hist_year}")
      next
    }

    ratio <- q0_val / m0_val

    # Project q0 for each projection year
    for (proj_year in projection_years) {
      m0_proj <- m0_series[year == proj_year & sex == sex_val, m0]

      if (length(m0_proj) == 0) {
        cli::cli_warn("Missing m0 for {sex_val} in projection year {proj_year}")
        next
      }

      q0_proj <- m0_proj * ratio

      results[[length(results) + 1]] <- data.table::data.table(
        year = proj_year,
        sex = sex_val,
        q0 = q0_proj,
        method = "projected",
        q0_m0_ratio = ratio
      )
    }
  }

  result <- data.table::rbindlist(results)
  cli::cli_alert_success(
    "Projected q0 for {length(projection_years)} years using q0/m0 ratio from {last_hist_year}"
  )

  result
}

# =============================================================================
# q1 Calculation using TR Methodology (4m1 ratio method)
# =============================================================================

#' Calculate 4m1 - Central death rate for ages 1-4
#'
#' @description
#' Calculates the 4-year central death rate for ages 1-4, denoted as 4m1 in
#' actuarial notation. This is used in the TR methodology for calculating q1
#' in projected years.
#'
#' @param deaths data.table with columns: year, age, sex, deaths
#' @param population data.table with columns: year, age, sex, population
#' @param years Integer vector of years to calculate (default: all available)
#'
#' @return data.table with columns: year, sex, m4_1 (the 4m1 value)
#'
#' @details
#' The 4-year central death rate is calculated as:
#' 4m1 = (D1 + D2 + D3 + D4) / (P1 + P2 + P3 + P4)
#'
#' Where D is deaths and P is midyear population for each age.
#'
#' @export
calculate_4m1 <- function(deaths, population, years = NULL) {
  checkmate::assert_data_table(deaths)
  checkmate::assert_data_table(population)
  checkmate::assert_names(names(deaths), must.include = c("year", "age", "sex", "deaths"))
  checkmate::assert_names(names(population), must.include = c("year", "age", "sex", "population"))

  # Filter to ages 1-4
  d <- deaths[age >= 1 & age <= 4]
  p <- population[age >= 1 & age <= 4]

  if (is.null(years)) {
    years <- intersect(unique(d$year), unique(p$year))
  }

  d <- d[year %in% years]
  p <- p[year %in% years]

  # Aggregate deaths and population for ages 1-4 by year and sex
  d_agg <- d[, .(deaths_1_4 = sum(deaths, na.rm = TRUE)), by = .(year, sex)]
  p_agg <- p[, .(population_1_4 = sum(population, na.rm = TRUE)), by = .(year, sex)]

  # Merge and calculate 4m1
  result <- merge(d_agg, p_agg, by = c("year", "sex"))
  result[, m4_1 := deaths_1_4 / population_1_4]
  result[population_1_4 == 0, m4_1 := NA_real_]

  # Select output columns
  result <- result[, .(year, sex, m4_1)]
  data.table::setorder(result, year, sex)

  cli::cli_alert_success(
    "Calculated 4m1 for {length(unique(result$year))} years"
  )

  result
}

#' Calculate q1 using TR methodology
#'
#' @description
#' Calculates the probability of death at age 1 (q1) using the Trustees Report
#' methodology:
#' - For historical years: q1 is calculated from the standard mx-to-qx formula
#' - For projected years: q1 is calculated from 4m1 using a preserved ratio
#'
#' @param mx_data data.table with central death rates (year, age, sex, mx)
#' @param deaths data.table with death counts (year, age, sex, deaths)
#' @param population data.table with population (year, age, sex, population)
#' @param last_historical_year Integer: last year of historical data
#' @param projection_years Integer vector: years to project q1
#'
#' @return data.table with columns: year, sex, q1, method
#'
#' @details
#' Per TR2025 documentation (Section 1.2.c, Equation 1.2.3):
#' "For the period 1940 through the last year of historical data, probabilities
#' of death are calculated from tabulations of births by year and from deaths
#' at age 1. After the last historical year, each q1 is calculated from 4m1
#' assuming that the ratio of q1 to 4m1 measured for the last historical year
#' would remain constant thereafter."
#'
#' For projected years:
#' q1_projected = 4m1_projected × (q1 / 4m1)_last_historical
#'
#' @export
calculate_q1_tr_method <- function(mx_data,
                                   deaths,
                                   population,
                                   last_historical_year,
                                   projection_years = NULL) {
  checkmate::assert_data_table(mx_data)
  checkmate::assert_data_table(deaths)
  checkmate::assert_data_table(population)
  checkmate::assert_int(last_historical_year)

  # Get historical years from mx_data
  historical_years <- unique(mx_data$year)
  historical_years <- historical_years[historical_years <= last_historical_year]

  # Calculate q1 for historical years using standard formula
  # q1 = m1 / (1 + 0.5 * m1)
  mx_age1 <- mx_data[age == 1 & year %in% historical_years, .(year, sex, mx)]
  mx_age1[, q1 := mx / (1 + 0.5 * mx)]

  q1_historical <- mx_age1[, .(year, sex, q1, method = "historical")]

  # Calculate 4m1 for historical years
  m4_1_historical <- calculate_4m1(deaths, population, years = historical_years)

  # Calculate q1/4m1 ratio for last historical year
  q1_last <- q1_historical[year == last_historical_year, .(sex, q1_last = q1)]
  m4_1_last <- m4_1_historical[year == last_historical_year, .(sex, m4_1_last = m4_1)]
  ratio_data <- merge(q1_last, m4_1_last, by = "sex")
  ratio_data[, q1_m4_1_ratio := q1_last / m4_1_last]

  cli::cli_alert_info(
    "q1/4m1 ratio at {last_historical_year}: male={round(ratio_data[sex == 'male', q1_m4_1_ratio], 4)}, female={round(ratio_data[sex == 'female', q1_m4_1_ratio], 4)}"
  )

  # If no projection years specified, return only historical

  if (is.null(projection_years) || length(projection_years) == 0) {
    cli::cli_alert_success(
      "Calculated q1 for {nrow(q1_historical)} historical year-sex combinations"
    )
    return(q1_historical)
  }

  # Calculate 4m1 for projected years
  # Note: For projected years, we need projected mx for ages 1-4
  # If mx_data contains projected years, use them; otherwise use trend
  projected_mx_years <- unique(mx_data$year)
  projected_mx_years <- projected_mx_years[projected_mx_years > last_historical_year]

  if (length(projected_mx_years) > 0) {
    # Calculate 4m1 from projected mx
    mx_ages_1_4 <- mx_data[age >= 1 & age <= 4 & year %in% projection_years]

    # Calculate population-weighted 4m1 = sum(Dx) / sum(Px) ≈ sum(mx * Px) / sum(Px)
    # Use last historical year's population distribution for ages 1-4 as weights
    # (population distribution at these ages changes slowly over time)
    pop_weights <- population[age >= 1 & age <= 4 & year == last_historical_year,
                              .(age, sex, pop_weight = population)]
    mx_weighted <- merge(mx_ages_1_4, pop_weights, by = c("age", "sex"), all.x = TRUE)
    # Fallback to equal weights if population data is missing
    mx_weighted[is.na(pop_weight), pop_weight := 1]
    m4_1_projected <- mx_weighted[, .(
      m4_1 = sum(mx * pop_weight, na.rm = TRUE) / sum(pop_weight, na.rm = TRUE)
    ), by = .(year, sex)]
  } else {
    # Extrapolate 4m1 using AAx trajectory (simplified: use last value)
    cli::cli_alert_warning("No projected mx available, using last historical 4m1 with decay")
    m4_1_projected <- data.table::CJ(year = projection_years, sex = c("male", "female"))
    m4_1_projected <- merge(m4_1_projected, m4_1_last, by = "sex")
    data.table::setnames(m4_1_projected, "m4_1_last", "m4_1")
  }

  # Calculate projected q1 = 4m1 * ratio
  q1_projected <- merge(m4_1_projected, ratio_data[, .(sex, q1_m4_1_ratio)], by = "sex")
  q1_projected[, q1 := m4_1 * q1_m4_1_ratio]
  q1_projected[, method := "projected_4m1_ratio"]
  q1_projected <- q1_projected[, .(year, sex, q1, method)]

  # Combine historical and projected
  result <- data.table::rbindlist(list(q1_historical, q1_projected), use.names = TRUE)
  data.table::setorder(result, year, sex)

  cli::cli_alert_success(
    "Calculated q1 for {nrow(q1_historical)} historical + {nrow(q1_projected)} projected year-sex combinations"
  )
  cli::cli_alert_info(
    "- Historical: standard mx-to-qx formula"
  )
  cli::cli_alert_info(
    "- Projected: 4m1 ratio method (TR methodology)"
  )

  result
}

#' Integrate q1 into death probability series
#'
#' @description
#' Replaces the age-1 death probability (q1) in a qx series with values
#' calculated using the TR methodology (4m1 ratio method for projected years).
#'
#' @param qx data.table with death probabilities (must include age 1)
#' @param q1 data.table from calculate_q1_tr_method() with columns year, sex, q1
#'
#' @return data.table with updated q1 values
#'
#' @export
integrate_q1 <- function(qx, q1) {
  checkmate::assert_data_table(qx)
  checkmate::assert_data_table(q1)
  checkmate::assert_names(names(qx), must.include = c("year", "age", "sex", "qx"))
  checkmate::assert_names(names(q1), must.include = c("year", "sex", "q1"))

  dt <- data.table::copy(qx)

  # Merge q1 values
  q1_subset <- q1[, .(year, sex, q1_new = q1)]
  dt <- merge(dt, q1_subset, by = c("year", "sex"), all.x = TRUE)

  # Count replacements
  n_replaced <- dt[age == 1 & !is.na(q1_new), .N]

  # Replace qx at age 1 with calculated q1
  dt[age == 1 & !is.na(q1_new), qx := q1_new]
  dt[, q1_new := NULL]

  cli::cli_alert_success(
    "Integrated q1 values for {n_replaced} year-sex combinations"
  )

  dt
}

#' Integrate infant mortality into death probability series
#'
#' @description
#' Replaces the age-0 death probability (q0) in a qx series with the
#' properly calculated infant mortality rate.
#'
#' @param qx data.table with death probabilities (must include age 0)
#' @param q0 data.table from calculate_infant_mortality() or calculate_infant_mortality_series()
#'
#' @return data.table with updated q0 values
#'
#' @export
integrate_infant_mortality <- function(qx, q0) {
  checkmate::assert_data_table(qx)
  checkmate::assert_data_table(q0)
  checkmate::assert_names(names(qx), must.include = c("year", "age", "sex", "qx"))
  checkmate::assert_names(names(q0), must.include = c("year", "sex", "q0"))

  dt <- data.table::copy(qx)

  # Merge q0 values
  q0_subset <- q0[, .(year, sex, q0)]

  # Update age 0 qx with calculated q0
  dt <- merge(dt, q0_subset, by = c("year", "sex"), all.x = TRUE)

  # Replace qx at age 0 with q0
  dt[age == 0 & !is.na(q0), qx := q0]
  dt[, q0 := NULL]

  # Report changes
  years_updated <- unique(dt[age == 0 & year %in% q0$year, year])
  if (length(years_updated) > 0) {
    cli::cli_alert_success(
      "Updated q0 for {length(years_updated)} years: {min(years_updated)}-{max(years_updated)}"
    )
  }

  dt
}

#' Calculate weighted average qx for 100+ age group
#'
#' @description
#' Calculates a weighted average qx for the open-ended 100+ age group based on
#' the implied age distribution within the group. TR2025 has separate qx values
#' for ages 100-119, and using the weighted average produces more accurate
#' mortality for the 100+ group than using qx at age 100 alone.
#'
#' @param qx_data data.table: mortality qx with columns year, age, sex, qx
#'   Must include ages 100-119.
#' @param years Integer vector: years to calculate weighted qx for
#'
#' @return data.table with columns: year, sex, weighted_qx_100plus
#'
#' @details
#' The method:
#' 1. Calculate implied age distribution within 100+ using survival probabilities
#'    Starting with weight=1 at age 100, w(a+1) = w(a) * (1 - qx(a))
#' 2. Normalize weights to sum to 1
#' 3. Compute weighted average: sum(w(a) * qx(a)) for a = 100 to 119
#'
#' @export
calculate_weighted_qx_100plus <- function(qx_data, years = NULL) {
  qx_data <- data.table::as.data.table(qx_data)

  if (is.null(years)) {
    years <- unique(qx_data$year)
  }

  # Check that we have ages 100-119
  max_qx_age <- max(qx_data$age)
  if (max_qx_age < 119) {
    cli::cli_warn("qx_data only goes to age {max_qx_age}, need 119 for weighted average")
    return(NULL)
  }

  results <- list()

  for (yr in years) {
    for (s in c("male", "female")) {
      # Get qx values for ages 100-119
      qx_100_119 <- qx_data[year == yr & sex == s & age >= 100 & age <= 119,
                            .(age, qx)][order(age)]

      if (nrow(qx_100_119) < 20) {
        next  # Skip if missing data
      }

      # Calculate survival weights
      # w[1] = 1 (age 100), w[i+1] = w[i] * (1 - qx[i])
      weights <- numeric(20)
      weights[1] <- 1.0

      for (i in 2:20) {
        weights[i] <- weights[i-1] * (1 - qx_100_119$qx[i-1])
      }

      # Normalize to sum to 1
      weights <- weights / sum(weights)

      # Calculate weighted average qx
      weighted_qx <- sum(weights * qx_100_119$qx)

      results[[length(results) + 1]] <- data.table::data.table(
        year = yr,
        sex = s,
        weighted_qx_100plus = weighted_qx
      )
    }
  }

  data.table::rbindlist(results)
}

#' Apply weighted qx for age 100+ to mortality data
#'
#' @description
#' Replaces the qx at age 100 with the weighted average qx for the 100+ group.
#' This accounts for the higher mortality at ages 101+ within the open-ended group.
#'
#' @param qx_data data.table: mortality qx with columns year, age, sex, qx
#' @param qx_100_119 data.table: qx for ages 100-119 (from TR2025 projected files)
#'
#' @return data.table with qx at age 100 replaced by weighted average
#'
#' @export
apply_weighted_qx_100plus <- function(qx_data, qx_100_119) {
  qx_data <- data.table::copy(qx_data)

  # Calculate weighted qx for each year/sex
  weighted_qx <- calculate_weighted_qx_100plus(qx_100_119, years = unique(qx_data$year))

  if (is.null(weighted_qx) || nrow(weighted_qx) == 0) {
    cli::cli_warn("Could not calculate weighted qx for 100+, using original values")
    return(qx_data)
  }

  # Merge weighted qx
  qx_data <- merge(qx_data, weighted_qx, by = c("year", "sex"), all.x = TRUE)

  # Replace qx at age 100 with weighted value
  qx_data[age == 100 & !is.na(weighted_qx_100plus), qx := weighted_qx_100plus]
  qx_data[, weighted_qx_100plus := NULL]

  n_updated <- qx_data[age == 100 & year %in% weighted_qx$year, .N]
  cli::cli_alert_success("Applied weighted qx for 100+ group to {n_updated} year/sex combinations")

  qx_data
}

# =============================================================================
# DYNAMIC WEIGHTED QX FOR 100+ GROUP
# =============================================================================
# These functions track the internal age distribution within 100+ as it evolves
# over time, providing year-specific weighted qx values that reflect the actual
# composition of the 100+ population.

#' Initialize internal 100+ age distribution
#'
#' @description
#' Creates the initial internal age distribution within the 100+ group based on
#' the starting population. Uses survival probabilities from qx to estimate
#' how the 100+ population is distributed across ages 100-119.
#'
#' @param population_100plus Numeric: total population in the 100+ group
#' @param qx_100_119 data.table: qx values for ages 100-119 for a single year/sex
#' @param sex Character: "male" or "female"
#'
#' @return Named numeric vector of population at each age 100-119
#'
#' @export
initialize_100plus_distribution <- function(population_100plus, qx_100_119, sex) {
  # Sort qx by age
  qx_100_119 <- data.table::as.data.table(qx_100_119)[order(age)]

  # Calculate survival-based weights
  # w[1] = 1 (age 100), w[i+1] = w[i] * (1 - qx[i])
  weights <- numeric(20)
  weights[1] <- 1.0

  for (i in 2:20) {
    weights[i] <- weights[i-1] * (1 - qx_100_119$qx[i-1])
  }

  # Normalize to sum to 1
  weights <- weights / sum(weights)

  # Distribute population according to weights
  distribution <- population_100plus * weights
  names(distribution) <- as.character(100:119)

  distribution
}

#' Evolve 100+ internal distribution forward one year
#'
#' @description
#' Advances the internal 100+ age distribution by one year:
#' 1. Apply mortality at each age (using age-specific qx)
#' 2. Age survivors forward (100->101, 101->102, ..., 118->119)
#' 3. Add new 100-year-olds from age 99 survivors
#' 4. Terminal age 119 accumulates (no aging out)
#'
#' @param current_dist Named numeric vector: current population at ages 100-119
#' @param new_100_pop Numeric: new entrants to age 100 (survivors from age 99)
#' @param qx_100_119 data.table: qx values for ages 100-119 for this year/sex
#'
#' @return Named numeric vector: updated population at ages 100-119
#'
#' @export
evolve_100plus_distribution <- function(current_dist, new_100_pop, qx_100_119) {
  # Sort qx by age
  qx_100_119 <- data.table::as.data.table(qx_100_119)[order(age)]

  # Get qx values as vector (ages 100-119)
  qx_vec <- qx_100_119$qx

  # Current population at each age
  pop_vec <- as.numeric(current_dist)

  # Apply mortality: survivors = pop * (1 - qx)
  survivors <- pop_vec * (1 - qx_vec)

  # Age forward: people at age a move to age a+1
  # new_dist[1] = new 100-year-olds
  # new_dist[2] = survivors from age 100
  # ...
 # new_dist[20] = survivors from age 118 + survivors from age 119 (terminal)
  new_dist <- numeric(20)
  new_dist[1] <- new_100_pop
  new_dist[2:19] <- survivors[1:18]
  new_dist[20] <- survivors[19] + survivors[20]  # Age 119 is terminal

  names(new_dist) <- as.character(100:119)
  new_dist
}

#' Calculate dynamic weighted qx from current distribution
#'
#' @description
#' Computes the weighted average qx for the 100+ group based on the current
#' internal age distribution. This gives a year-specific mortality rate that
#' reflects the actual composition of the 100+ population.
#'
#' @param current_dist Named numeric vector: current population at ages 100-119
#' @param qx_100_119 data.table: qx values for ages 100-119 for this year/sex
#'
#' @return Numeric: weighted average qx for the 100+ group
#'
#' @export
calculate_dynamic_weighted_qx <- function(current_dist, qx_100_119) {
  # Sort qx by age
  qx_100_119 <- data.table::as.data.table(qx_100_119)[order(age)]

  # Get population weights (normalize to sum to 1)
  pop_vec <- as.numeric(current_dist)
  total_pop <- sum(pop_vec)

  if (total_pop <= 0) {
    # Fallback to qx at age 100 if no population
    return(qx_100_119$qx[1])
  }

  weights <- pop_vec / total_pop

  # Weighted average qx
  weighted_qx <- sum(weights * qx_100_119$qx)

  weighted_qx
}

#' Create 100+ distribution tracker for projection
#'
#' @description
#' Creates a tracker object that maintains the internal 100+ distribution
#' for both sexes throughout the projection. This is used by the projection
#' loop to get dynamic weighted qx values each year.
#'
#' @param starting_pop_100plus data.table: starting 100+ population by sex
#' @param qx_100_119 data.table: qx values for ages 100-119 (all years, both sexes)
#' @param start_year Integer: starting year of projection
#'
#' @return List with:
#'   - male: named numeric vector of male population at ages 100-119
#'   - female: named numeric vector of female population at ages 100-119
#'   - qx_data: reference to qx_100_119 data
#'   - current_year: current year in projection
#'
#' @export
create_100plus_tracker <- function(starting_pop_100plus, qx_100_119, start_year) {
  # Get starting population by sex
  male_pop <- starting_pop_100plus[sex == "male", sum(population, na.rm = TRUE)]
  female_pop <- starting_pop_100plus[sex == "female", sum(population, na.rm = TRUE)]

  # Get qx for start year (or closest available)
  available_years <- unique(qx_100_119$year)
  init_year <- if (start_year %in% available_years) start_year else min(available_years)

  male_qx <- qx_100_119[year == init_year & sex == "male", .(age, qx)]
  female_qx <- qx_100_119[year == init_year & sex == "female", .(age, qx)]

  # Initialize distributions
  male_dist <- initialize_100plus_distribution(male_pop, male_qx, "male")
  female_dist <- initialize_100plus_distribution(female_pop, female_qx, "female")

  list(
    male = male_dist,
    female = female_dist,
    qx_data = qx_100_119,
    current_year = start_year
  )
}

#' Get dynamic weighted qx for current year
#'
#' @description
#' Returns the weighted qx for the 100+ group based on the current internal
#' distribution. Call this during projection to get the year-specific qx.
#'
#' @param tracker List: 100+ distribution tracker from create_100plus_tracker
#' @param year Integer: current projection year
#' @param sex Character: "male" or "female"
#'
#' @return Numeric: weighted qx for the 100+ group
#'
#' @export
get_dynamic_weighted_qx <- function(tracker, target_year, target_sex) {
  # Get current distribution
  dist <- if (target_sex == "male") tracker$male else tracker$female

  # Get qx for this year (use local vars to avoid data.table scoping issues)
  qx_year <- tracker$qx_data[year == target_year & sex == target_sex, .(age, qx)]

  # If year not available, use closest
  if (nrow(qx_year) == 0) {
    available_years <- unique(tracker$qx_data$year)
    closest_year <- available_years[which.min(abs(available_years - target_year))]
    qx_year <- tracker$qx_data[year == closest_year & sex == target_sex, .(age, qx)]
  }

  calculate_dynamic_weighted_qx(dist, qx_year)
}

#' Update tracker after one projection year
#'
#' @description
#' Evolves the internal 100+ distribution forward by one year. Call this
#' at the end of each projection year after population has been updated.
#'
#' @param tracker List: 100+ distribution tracker
#' @param year Integer: year just completed
#' @param new_100_male Numeric: new male entrants to age 100 (from age 99 survivors)
#' @param new_100_female Numeric: new female entrants to age 100 (from age 99 survivors)
#'
#' @return Updated tracker list
#'
#' @export
update_100plus_tracker <- function(tracker, target_year, new_100_male, new_100_female) {
  # Get qx for this year (use local var to avoid data.table scoping issues)
  male_qx <- tracker$qx_data[year == target_year & sex == "male", .(age, qx)]
  female_qx <- tracker$qx_data[year == target_year & sex == "female", .(age, qx)]

  # Use closest year if not available
  if (nrow(male_qx) == 0) {
    available_years <- unique(tracker$qx_data$year)
    closest_year <- available_years[which.min(abs(available_years - target_year))]
    male_qx <- tracker$qx_data[year == closest_year & sex == "male", .(age, qx)]
    female_qx <- tracker$qx_data[year == closest_year & sex == "female", .(age, qx)]
  }

  # Evolve distributions
  tracker$male <- evolve_100plus_distribution(tracker$male, new_100_male, male_qx)
  tracker$female <- evolve_100plus_distribution(tracker$female, new_100_female, female_qx)
  tracker$current_year <- target_year

  tracker
}

# =============================================================================
# HELPER FUNCTIONS FOR TARGET FACTORIES
# =============================================================================

#' Calculate TR-method aggregate q100+
#'
#' @description
#' Calculates the aggregate probability of death for the open-ended 100+ age group
#' using the Trustees Report extrapolation methodology.
#'
#' @param qx_data data.table with columns year, age, sex, qx (must include ages 98, 99)
#'
#' @return data.table with columns year, sex, q100_plus
#'
#' @details
#' Per TR2025 methodology:
#' 1. Extrapolate qx for single ages 100-119 using growth formulas based on q98 and q99.
#' 2. Calculate life table functions (lx, Tx) for this extrapolated range.
#' 3. Calculate aggregate q100+ = 1 - T(101) / T(100).
#'
#' Extrapolation formulas:
#' For x = 100..104:
#' qx = q(x-1) * ( (q99/q98) * (104-x)/5 + k * (x-99)/5 )
#' Where k = 1.05 for males, 1.06 for females.
#'
#' For x = 105+:
#' qx = k * q(x-1) (capped at 1.0)
#'
#' @export
calculate_tr_q100_plus <- function(qx_data) {
  checkmate::assert_data_table(qx_data)
  checkmate::assert_names(names(qx_data), must.include = c("year", "age", "sex", "qx"))

  dt <- data.table::copy(qx_data)
  years <- unique(dt$year)

  results <- list()

  for (yr in years) {
    for (sx in c("male", "female")) {
      # Get q98 and q99
      q98 <- dt[year == yr & sex == sx & age == 98, qx]
      q99 <- dt[year == yr & sex == sx & age == 99, qx]

      if (length(q98) == 0 || length(q99) == 0) next

      # Set growth factor k
      k <- if (sx == "male") 1.05 else 1.06

      # Extrapolate ages 100-120
      q_extrap <- numeric(21) # 100 to 120
      prev_q <- q99

      for (i in 1:21) {
        age <- 99 + i

        if (age <= 104) {
          # Blended growth rate
          rate <- (q99/q98) * ((104 - age)/5) + k * ((age - 99)/5)
          q_val <- prev_q * rate
        } else {
          # Constant growth rate
          q_val <- prev_q * k
        }

        # Cap at 1.0
        q_val <- min(q_val, 1.0)
        q_extrap[i] <- q_val
        prev_q <- q_val
      }

      # Calculate life table functions for extrapolated ages
      # We only need relative lx, so start with l100 = 100000
      lx <- numeric(22) # 100 to 121
      lx[1] <- 100000

      # Calculate lx
      for (i in 1:21) {
        lx[i+1] <- lx[i] * (1 - q_extrap[i])
      }

      # Calculate Lx
      Lx <- numeric(21)
      for (i in 1:21) {
        # Standard approximation Lx = (lx + lx+1)/2
        Lx[i] <- (lx[i] + lx[i+1]) / 2
      }

      # Calculate Tx
      # T100 = sum(Lx[1:end])
      # T101 = sum(Lx[2:end])
      T100 <- sum(Lx)
      T101 <- sum(Lx[2:21])

      # Calculate q100+ = 1 - T101/T100
      if (T100 > 0) {
        q100_plus <- 1 - T101/T100
      } else {
        q100_plus <- 1.0
      }

      results[[length(results) + 1]] <- data.table::data.table(
        year = yr,
        sex = sx,
        qx = q100_plus
      )
    }
  }

  data.table::rbindlist(results)
}

#' Calculate qx with proper infant mortality (q0), q1 TR method, and TR 100+ method
#'
#' @description
#' Calculates age-specific death probabilities (qx) from central death rates (mx)
#' using the full TR methodology:
#' - q0: Separation factor method for infant mortality
#' - q1: 4m1 ratio method for projected years (TR2025 Section 1.2.c)
#' - q2-99: Standard qx = mx / (1 + 0.5*mx) formula
#' - q100+: TR extrapolation method
#'
#' @param mx_total data.table with columns year, age, sex, mx
#' @param infant_deaths_detailed data.table with detailed infant deaths (age units)
#' @param monthly_births data.table with monthly births
#' @param population data.table for filtering years
#' @param deaths_raw Optional data.table with raw death counts for 4m1 calculation
#'   (year, age, sex, deaths). If NULL, uses standard formula for q1.
#' @param last_historical_year Optional integer: last year of historical data.
#'   If provided with deaths_raw, uses 4m1 ratio method for q1 in projected years.
#' @param max_age Maximum age to include (default: 100)
#'
#' @return data.table with columns year, age, sex, qx
#'
#' @details
#' Per TR2025 documentation (Section 1.2.c):
#' - q0: "q0 is calculated directly from tabulations of births by month and from
#'   tabulations of deaths at ages 0, 1-2, 3-6, 7-28 days, 1 month, 2 months, ...,
#'   and 11 months"
#' - q1: "For the period 1940 through the last year of historical data, probabilities
#'   of death are calculated from tabulations of births by year and from deaths at
#'   age 1. After the last historical year, each q1 is calculated from 4m1 assuming
#'   that the ratio of q1 to 4m1 measured for the last historical year would remain
#'   constant thereafter."
#' - q2-99: "qx = mx / (1 + 0.5*mx)"
#' - q100+: Special extrapolation with k=1.05 (male) / k=1.06 (female)
#'
#' @export
calculate_qx_with_infant_mortality <- function(mx_total,
                                               infant_deaths_detailed,
                                               monthly_births,
                                               population,
                                               deaths_raw = NULL,
                                               last_historical_year = NULL,
                                               max_age = 100,
                                               births_by_sex = NULL) {
  # Get available years
  pop_years <- unique(population$year)
  years_to_calc <- intersect(unique(mx_total$year), pop_years)

  # 1. Calculate qx for ages 2-99 from mx (q1 will be handled separately)
  mx_filtered <- mx_total[year %in% years_to_calc]
  qx_from_mx <- convert_mx_to_qx(mx_filtered, max_age = max_age)
  qx_ages2_99 <- qx_from_mx[age >= 2 & age <= 99, .(year, age, sex, qx)]

  # 2. Calculate q0 using separation factor method
  # Filter data to relevant years
  infant_d <- infant_deaths_detailed[year %in% years_to_calc]
  births_m <- monthly_births[year %in% (years_to_calc - 1) | year %in% years_to_calc]

  q0_series <- calculate_infant_mortality_series(
    infant_deaths = infant_d,
    monthly_births = births_m,
    years = years_to_calc,
    method = "separation_factor",
    births_by_sex = births_by_sex
  )
  q0_formatted <- q0_series[, .(year, age = 0L, sex, qx = q0)]

  # 3. Calculate q1 using TR methodology (4m1 ratio method for projected years)
  use_tr_q1_method <- !is.null(deaths_raw) && !is.null(last_historical_year)

  if (use_tr_q1_method) {
    # Determine projection years (years after last_historical_year)
    projection_years <- years_to_calc[years_to_calc > last_historical_year]

    q1_series <- calculate_q1_tr_method(
      mx_data = mx_filtered,
      deaths = deaths_raw,
      population = population,
      last_historical_year = last_historical_year,
      projection_years = projection_years
    )
    q1_formatted <- q1_series[year %in% years_to_calc, .(year, age = 1L, sex, qx = q1)]
    q1_method_msg <- "4m1 ratio method (TR methodology)"
  } else {
    # Fallback: use standard mx-to-qx formula for q1
    q1_formatted <- qx_from_mx[age == 1, .(year, age, sex, qx)]
    q1_method_msg <- "standard mx-to-qx formula"
  }

  # 4. Calculate q100+ using TR extrapolation method
  # We need q98 and q99 from the mx-derived series
  q100_agg <- calculate_tr_q100_plus(qx_from_mx)
  q100_formatted <- q100_agg[, .(year, age = 100L, sex, qx)]

  # Combine all parts: q0, q1, q2-99, q100+
  result <- data.table::rbindlist(
    list(q0_formatted, q1_formatted, qx_ages2_99, q100_formatted),
    use.names = TRUE
  )
  data.table::setorder(result, year, sex, age)

  # Report status
  cli::cli_alert_success(
    "Calculated qx for {length(unique(result$year))} years"
  )
  cli::cli_alert_info(
    "- q0: separation factor method"
  )
  cli::cli_alert_info(
    "- q1: {q1_method_msg}"
  )
  cli::cli_alert_info(
    "- q2-99: standard qx = mx/(1+0.5*mx) formula"
  )
  cli::cli_alert_info(
    "- q100+: TR extrapolation method"
  )

  result
}

#' Combine life expectancy series
#'
#' @description
#' Combines historical and projected life expectancy series into a single data.table.
#'
#' @param historical data.table with historical life expectancy
#' @param projected data.table with projected life expectancy
#'
#' @return Combined and sorted data.table
#'
#' @export
combine_life_expectancy_series <- function(historical, projected) {
  combined <- data.table::rbindlist(
    list(historical, projected),
    use.names = TRUE
  )
  data.table::setorder(combined, year, sex, age)
  combined
}

#' Load total births for q0 calculation
#'
#' @description
#' Loads total births from cache files, trying monthly data first then
#' falling back to age-specific data. Used for q0 calculation.
#'
#' @param years Integer vector of years to load
#' @param monthly_pattern Cache file pattern for monthly data
#' @param age_pattern Cache file pattern for age-specific data (fallback)
#'
#' @return data.table with columns year, births
#'
#' @export
load_total_births_for_q0 <- function(years,
                                      monthly_pattern = "data/raw/nchs/births_by_month_%d.rds",
                                      age_pattern = "data/raw/nchs/births_by_age_%d.rds") {
  data.table::rbindlist(lapply(years, function(yr) {
    monthly_file <- sprintf(monthly_pattern, yr)
    if (file.exists(monthly_file)) {
      b <- readRDS(monthly_file)
      data.table::data.table(year = yr, births = sum(b$births))
    } else {
      age_file <- sprintf(age_pattern, yr)
      if (file.exists(age_file)) {
        b <- readRDS(age_file)
        data.table::data.table(year = yr, births = sum(b$births))
      } else {
        NULL
      }
    }
  }))
}

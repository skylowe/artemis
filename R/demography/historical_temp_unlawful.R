#' Historical Temporary or Unlawfully Present Population (Equation 1.4.3)
#'
#' @description
#' Functions for estimating the historical temporary or unlawfully present
#' immigrant population (O^z_{x,s}) from 1940 to 2022.
#'
#' This module implements Equation 1.4.3 from the TR2025 documentation
#' (Section 1.4.c):
#'   O^z_{x,s} = O stock estimated via residual method + stock modification
#'
#' @section Methodology (TR2025 Section 1.4.c):
#'
#' Per TR2025: "For each year, an initial net residual estimate by single year
#' of age and sex is backed out from estimates of beginning and end of year
#' populations, births, deaths, LPR immigrants, adjustments of status, and
#' legal emigrants. These residuals are then modified to ensure reasonableness.
#' Next, using these modified net residuals, along with deaths [...], an
#' initial temporary or unlawfully present immigrant stock is built. These
#' stocks are then modified to ensure reasonableness."
#'
#' Two methods are available (config: `historical_population.o_population.method`):
#'
#' **"residual" (default):** Follows the TR2025 4-step process:
#'
#'   1. Compute net residuals from the population accounting identity
#'   2. Modify residuals for reasonableness (rolling median smoothing + floor at 0)
#'   3. Build O stock from V.A2 net flows distributed by residual-derived age-sex
#'      proportions, with mortality/aging applied
#'   4. Modify stocks for reasonableness (DHS adjustment for 2000+, COVID averaging
#'      for 2020)
#'
#'   **Deviation from TR2025:** Step 3 uses V.A2 o_net totals for annual stock
#'   change levels rather than building stock directly from raw residuals.
#'   TR2025 builds stock from residuals and then modifies stock levels; we use
#'   V.A2 totals for levels and residuals only for age-sex distribution shape.
#'   This is necessary because our component inputs (ARTEMIS-derived immigration
#'   distributions, AOS ratios) differ from OCACT's internal data, causing
#'   residual-built stocks to diverge from plausible levels pre-2000. V.A2
#'   provides SSA's official O-immigration totals, so using them for levels
#'   is the most principled approach given public data limitations.
#'
#' **"va2_flows" (alternative):** Builds stock directly from V.A2 net flows
#'   using a static DHS-based age-sex distribution (config-driven). Simpler
#'   and does not require the total_pop dependency, but uses a fixed age-sex
#'   distribution for all years rather than data-derived year-varying shapes.
#'
#' @section Data Sources:
#' - TR2025 Table V.A2: Official OCACT immigration assumptions (1940-2100)
#' - DHS Unauthorized: Age-sex distribution and validation targets (2000+)
#' - Census/NCHS/etc: Population and component data for residual calculation
#' - Mortality (qx): For aging the O population stock
#'
#' @name historical_temp_unlawful
NULL

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Calculate Historical Temporary/Unlawfully Present Population (Eq 1.4.3)
#'
#' @description
#' Main entry point for calculating the temporary or unlawfully present
#' immigrant population (O^z_{x,s}) from 1940 to 2022.
#'
#' Supports two methods (configurable via `historical_population.o_population.method`):
#' - `"residual"` (default): Backs out O-population from the population accounting
#'   identity using total population, births, deaths, LPR immigration, emigration,
#'   and adjustments of status. Per TR2025 Section 1.4.c.
#' - `"va2_flows"`: Builds O stock directly from V.A2 net O flows.
#'
#' @param start_year Integer: First year (default: 1940)
#' @param end_year Integer: Last year (default: 2022)
#' @param ages Integer vector: Ages to include (default: 0:100)
#' @param config List: Configuration from tr2025.yaml
#' @param total_pop data.table: Total historical population (from historical_population target).
#'   Required for "residual" method.
#' @param lpr_assumptions data.table: LPR assumptions from V.A2 (from lpr_assumptions target).
#'   Required for "residual" method (provides immigration/emigration/AOS totals).
#' @param immigration_dist Immigration age-sex distribution (from lpr_distribution target).
#' @param emigration_dist Emigration age-sex distribution (from emigration_distribution target).
#' @param births_by_sex data.table: Births by sex (from nchs_births_by_sex target).
#' @param use_cache Logical: Use cached results if available
#' @param cache_dir Character: Directory for caching
#'
#' @return data.table with columns:
#'   - year: Calendar year (December 31 reference)
#'   - age: Single year of age (0-100)
#'   - sex: "male" or "female"
#'   - population: O population estimate
#'   - source: Data source indicator
#'
#' @export
calculate_historical_temp_unlawful <- function(start_year = 1940,
                                                end_year = 2022,
                                                ages = 0:100,
                                                config = NULL,
                                                total_pop = NULL,
                                                lpr_assumptions = NULL,
                                                immigration_dist = NULL,
                                                emigration_dist = NULL,
                                                births_by_sex = NULL,
                                                use_cache = TRUE,
                                                cache_dir = here::here("data/cache")) {
  cli::cli_h1("Calculating Temporary/Unlawfully Present Population (Eq 1.4.3)")

  # Read method from config
  method <- config$historical_population$o_population$method %||% "residual"
  if (!method %in% c("residual", "va2_flows")) {
    cli::cli_abort(c(
      "Invalid O-population method: {.val {method}}",
      "i" = "Must be one of: {.val residual}, {.val va2_flows}"
    ))
  }
  cli::cli_alert_info("O-population method: {.val {method}}")

  # Check cache
  cache_subdir <- file.path(cache_dir, "historical_population")
  if (!dir.exists(cache_subdir)) dir.create(cache_subdir, recursive = TRUE)

  cache_file <- file.path(
    cache_subdir,
    sprintf("o_population_%s_%d_%d.rds", method, start_year, end_year)
  )

  if (use_cache && file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached O population")
    cached <- readRDS(cache_file)
    return(cached)
  }

  years <- start_year:end_year

  if (method == "residual") {
    # =========================================================================
    # RESIDUAL METHOD (TR2025 default)
    # Uses V.A2 o_net flows for stock LEVELS (SSA's official O-immigration
    # totals) and the residual accounting identity only for the AGE-SEX SHAPE
    # of the distribution. This avoids stock accumulation issues from
    # component mismatch while providing data-driven age-sex patterns.
    # =========================================================================
    if (is.null(total_pop)) {
      cli::cli_abort(c(
        "total_pop is required for residual O-population method",
        "i" = "Pass historical_population target as total_pop parameter"
      ))
    }

    # Gather components needed for residual calculation
    cli::cli_h2("Step 1: Gathering Components for Residual Method")
    components <- gather_o_components(
      years = years, ages = ages, cache_dir = cache_dir, config = config,
      lpr_assumptions = lpr_assumptions,
      immigration_dist = immigration_dist,
      emigration_dist = emigration_dist,
      births_by_sex = births_by_sex
    )

    # Calculate residuals from population accounting identity
    cli::cli_h2("Step 2: Calculating Residuals from Accounting Identity")
    residuals <- calculate_other_residuals(
      total_pop = total_pop,
      components = components,
      years = years,
      ages = ages
    )
    cli::cli_alert_info("Computed residuals for {length(unique(residuals$year))} years")

    # Modify residuals for reasonableness (smooth + floor at 0)
    cli::cli_h2("Step 3: Modifying Residuals for Reasonableness")
    modified_residuals <- modify_residuals_for_reasonableness(residuals, config = config)

    # Extract per-year age-sex proportions from residuals
    cli::cli_h2("Step 4: Extracting Age-Sex Distribution from Residuals")
    static_dist <- get_o_age_sex_distribution(ages, config)
    residual_dist <- extract_residual_proportions(modified_residuals, static_dist)
    n_fallback <- residual_dist[, .(fb = all(source == "fallback")), by = year][fb == TRUE, .N]
    cli::cli_alert_info("Residual-derived proportions for {length(unique(residual_dist$year))} years ({n_fallback} using static fallback)")

    # Load V.A2 O flows for stock levels
    cli::cli_h2("Step 5: Loading V.A2 O Flows for Stock Levels")
    o_flows <- get_tr_historical_o_flows(years = years, convert_to_persons = TRUE)
    cli::cli_alert_info("Loaded V.A2 O flows for {nrow(o_flows)} years")

    # Build O stock using V.A2 levels + residual-derived age-sex distribution
    cli::cli_h2("Step 6: Building O Stock from V.A2 Flows + Residual Shape")
    mortality <- components$mortality
    o_stock <- build_o_stock_from_flows(
      o_flows = o_flows,
      age_sex_dist = residual_dist,
      mortality = mortality,
      years = years,
      ages = ages
    )
    cli::cli_alert_info("Built stock for {length(unique(o_stock$year))} years")

  } else {
    # =========================================================================
    # V.A2 FLOWS METHOD (alternative)
    # =========================================================================
    cli::cli_h2("Step 1: Loading TR2025 O Flows (Table V.A2)")
    o_flows <- get_tr_historical_o_flows(
      years = years,
      convert_to_persons = TRUE
    )
    cli::cli_alert_info("Loaded O flows for {nrow(o_flows)} years")

    # Show sample O net values
    sample_yrs <- c(1960, 1970, 1980, 1990, 2000, 2010, 2020)
    sample_yrs <- sample_yrs[sample_yrs %in% o_flows$year]
    for (yr in sample_yrs) {
      o_net <- o_flows[year == yr, o_net]
      cli::cli_alert_info("  {yr}: O net = {format(round(o_net), big.mark = ',')}")
    }

    cli::cli_h2("Step 2: Getting Age-Sex Distribution")
    age_sex_dist <- get_o_age_sex_distribution_by_year(ages, years, config)
    n_years_dist <- length(unique(age_sex_dist$year))
    cli::cli_alert_info("Distribution covers {nrow(age_sex_dist)} age-sex-year cells ({n_years_dist} years)")

    cli::cli_h2("Step 3: Loading Mortality Data")
    mortality <- load_mortality_for_o(years, config)
    cli::cli_alert_info("Loaded mortality for {length(unique(mortality$year))} years")

    cli::cli_h2("Step 4: Building O Stock from TR2025 Flows")
    o_stock <- build_o_stock_from_flows(
      o_flows = o_flows,
      age_sex_dist = age_sex_dist,
      mortality = mortality,
      years = years,
      ages = ages
    )
    cli::cli_alert_info("Built stock for {length(unique(o_stock$year))} years")
  }

  # =========================================================================
  # COVID-19 ADJUSTMENT: Replace 2020 with average of 2019 and 2021
  # =========================================================================
  covid_avg <- config$historical_population$o_population$covid_average_2020 %||% TRUE
  if (covid_avg && 2020 %in% o_stock$year && 2019 %in% o_stock$year && 2021 %in% o_stock$year) {
    cli::cli_h2("Applying 2020 COVID Averaging (TR2025 Footnote 9)")
    pop_2019 <- o_stock[year == 2019, .(age, sex, pop_2019 = population)]
    pop_2021 <- o_stock[year == 2021, .(age, sex, pop_2021 = population)]
    avg_2020 <- merge(pop_2019, pop_2021, by = c("age", "sex"))
    avg_2020[, population := (pop_2019 + pop_2021) / 2]

    o_stock <- o_stock[year != 2020]
    o_stock <- data.table::rbindlist(list(
      o_stock,
      avg_2020[, .(year = 2020L, age, sex, population, source = "covid_averaged")]
    ), fill = TRUE)
    data.table::setorder(o_stock, year, sex, age)

    old_total <- sum(pop_2019$pop_2019 + pop_2021$pop_2021) / 2
    cli::cli_alert_info("2020 O-stock replaced with avg(2019, 2021): {format(round(old_total), big.mark = ',')}")
  }

  # =========================================================================
  # DHS ADJUSTMENT (2000+)
  # =========================================================================
  cli::cli_h2("Adjusting to DHS Estimates (2000+)")
  dhs_estimates <- load_dhs_estimates_for_o(years)
  o_adjusted <- adjust_o_to_dhs(
    o_stock = o_stock,
    dhs_estimates = dhs_estimates,
    ages = ages
  )

  # Summary statistics
  cli::cli_h2("Summary")
  total_by_year <- o_adjusted[, .(total = sum(population)), by = year]
  cli::cli_alert_success("Calculated O population ({method} method) for {nrow(total_by_year)} years")

  sample_years <- c(1950, 1970, 1990, 2000, 2007, 2010, 2019, 2020, 2021, 2022)
  sample_years <- sample_years[sample_years %in% total_by_year$year]
  for (yr in sample_years) {
    total <- total_by_year[year == yr, total]
    cli::cli_alert_info("{yr}: {format(round(total), big.mark = ',')}")
  }

  # Save to cache — skip if cache disabled (e.g., scenario mode with read-only mount)
  if (use_cache) {
    saveRDS(o_adjusted, cache_file)
    cli::cli_alert_success("Saved to cache: {cache_file}")
  }

  o_adjusted
}

# =============================================================================
# AGE-SEX DISTRIBUTION FOR O POPULATION
# =============================================================================

#' Get Static Age-Sex Distribution for O Population
#'
#' @description
#' Returns a single static age-sex distribution from config. Used as a fallback
#' by the residual method (when all residuals are zero for a year) and as the
#' base distribution when era-based distributions are disabled.
#'
#' @param ages Integer vector: ages to include
#' @param config List: configuration from tr2025.yaml
#'
#' @return data.table with columns: age, sex, proportion (sums to 1)
#'
#' @keywords internal
get_o_age_sex_distribution <- function(ages = 0:100, config = NULL) {
  o_cfg <- config$historical_population$o_population
  if (is.null(o_cfg)) {
    cli::cli_abort("Config missing {.field historical_population.o_population}")
  }
  build_distribution_from_weights(
    age_dist = o_cfg$age_distribution,
    male_share = o_cfg$male_share,
    ages = ages
  )
}

#' Get Year-Varying Age-Sex Distribution for O Population
#'
#' @description
#' Returns a year-varying age-sex distribution for the va2_flows method.
#' When era_distributions are enabled in config, interpolates between era-specific
#' distributions. When disabled, returns the static distribution for all years.
#'
#' @param ages Integer vector: ages to include
#' @param years Integer vector: years to produce distributions for
#' @param config List: configuration from tr2025.yaml
#'
#' @return data.table with columns: year, age, sex, proportion
#'   Proportions sum to 1 within each year.
#'
#' @keywords internal
get_o_age_sex_distribution_by_year <- function(ages = 0:100, years = 1940:2022,
                                                config = NULL) {
  o_cfg <- config$historical_population$o_population
  if (is.null(o_cfg)) {
    cli::cli_abort("Config missing {.field historical_population.o_population}")
  }

  era_cfg <- o_cfg$era_distributions
  use_eras <- isTRUE(era_cfg$enabled) && !is.null(era_cfg$eras) && length(era_cfg$eras) > 0

  if (!use_eras) {
    # No era distributions — replicate static distribution for all years
    static <- get_o_age_sex_distribution(ages, config)
    result_list <- lapply(years, function(yr) {
      d <- data.table::copy(static)
      d[, year := yr]
      d
    })
    return(data.table::rbindlist(result_list, use.names = TRUE))
  }

  # Build distribution for each era
  eras <- era_cfg$eras
  era_dists <- list()
  era_bounds <- list()

  for (i in seq_along(eras)) {
    era <- eras[[i]]
    if (is.null(era$age_distribution) || is.null(era$male_share)) {
      cli::cli_abort("Era {i} missing age_distribution or male_share")
    }
    era_dists[[i]] <- build_distribution_from_weights(
      age_dist = era$age_distribution,
      male_share = era$male_share,
      ages = ages
    )
    era_bounds[[i]] <- list(start = era$start_year, end = era$end_year)
  }

  # For each year, find which era(s) apply and interpolate if between eras
  result_list <- list()

  for (yr in years) {
    # Find the era that contains this year
    in_era <- NULL
    for (i in seq_along(era_bounds)) {
      if (yr >= era_bounds[[i]]$start && yr <= era_bounds[[i]]$end) {
        in_era <- i
        break
      }
    }

    if (!is.null(in_era)) {
      # Year falls within an era — use that era's distribution
      d <- data.table::copy(era_dists[[in_era]])
      d[, year := yr]
      result_list[[length(result_list) + 1L]] <- d
    } else {
      # Year falls between eras — interpolate between adjacent eras
      prev_era <- NULL
      next_era <- NULL
      for (i in seq_along(era_bounds)) {
        if (era_bounds[[i]]$end < yr) prev_era <- i
        if (era_bounds[[i]]$start > yr && is.null(next_era)) next_era <- i
      }

      if (!is.null(prev_era) && !is.null(next_era)) {
        # Linear interpolation between adjacent era distributions
        gap_start <- era_bounds[[prev_era]]$end
        gap_end <- era_bounds[[next_era]]$start
        weight <- (yr - gap_start) / (gap_end - gap_start)

        d <- data.table::copy(era_dists[[prev_era]])
        d_next <- era_dists[[next_era]]
        d[, proportion := proportion * (1 - weight) + d_next$proportion * weight]
        # Re-normalize
        d[, proportion := proportion / sum(proportion)]
        d[, year := yr]
        result_list[[length(result_list) + 1L]] <- d
      } else if (!is.null(prev_era)) {
        # After all eras — use last era
        d <- data.table::copy(era_dists[[prev_era]])
        d[, year := yr]
        result_list[[length(result_list) + 1L]] <- d
      } else if (!is.null(next_era)) {
        # Before all eras — use first era
        d <- data.table::copy(era_dists[[next_era]])
        d[, year := yr]
        result_list[[length(result_list) + 1L]] <- d
      } else {
        # No eras defined — use static fallback
        d <- data.table::copy(get_o_age_sex_distribution(ages, config))
        d[, year := yr]
        result_list[[length(result_list) + 1L]] <- d
      }
    }
  }

  result <- data.table::rbindlist(result_list, use.names = TRUE)
  data.table::setorder(result, year, sex, age)
  result
}

#' Build Age-Sex Distribution from Config Weights
#'
#' @description
#' Helper that converts age-group weights and male_share into a normalized
#' single-year-of-age by sex distribution.
#'
#' @param age_dist Named list of age group weights (age_0_4, age_5_9, etc.)
#' @param male_share Numeric: proportion male (0-1)
#' @param ages Integer vector: ages to include
#'
#' @return data.table with columns: age, sex, proportion (sums to 1)
#'
#' @keywords internal
build_distribution_from_weights <- function(age_dist, male_share, ages = 0:100) {
  if (is.null(age_dist)) {
    cli::cli_abort("age_dist is required")
  }
  if (is.null(male_share)) {
    cli::cli_abort("male_share is required")
  }

  required_age_keys <- c("age_0_4", "age_5_9", "age_10_14", "age_15_17", "age_18_19",
    "age_20_24", "age_25_29", "age_30_34", "age_35_39", "age_40_44", "age_45_49",
    "age_50_54", "age_55_59", "age_60_64", "age_65_69", "age_70_79", "age_80_plus")
  missing <- setdiff(required_age_keys, names(age_dist))
  if (length(missing) > 0) {
    cli::cli_abort("age_distribution missing keys: {.field {missing}}")
  }

  dist <- data.table::data.table(age = ages)
  dist[, raw_prop := data.table::fcase(
    age < 5,  age_dist$age_0_4,
    age < 10, age_dist$age_5_9,
    age < 15, age_dist$age_10_14,
    age < 18, age_dist$age_15_17,
    age < 20, age_dist$age_18_19,
    age < 25, age_dist$age_20_24,
    age < 30, age_dist$age_25_29,
    age < 35, age_dist$age_30_34,
    age < 40, age_dist$age_35_39,
    age < 45, age_dist$age_40_44,
    age < 50, age_dist$age_45_49,
    age < 55, age_dist$age_50_54,
    age < 60, age_dist$age_55_59,
    age < 65, age_dist$age_60_64,
    age < 70, age_dist$age_65_69,
    age < 80, age_dist$age_70_79,
    default = age_dist$age_80_plus
  )]

  dist[, raw_prop := raw_prop / sum(raw_prop)]

  female_share <- 1 - male_share
  male_dist <- data.table::copy(dist)
  male_dist[, `:=`(sex = "male", proportion = raw_prop * male_share)]
  female_dist <- data.table::copy(dist)
  female_dist[, `:=`(sex = "female", proportion = raw_prop * female_share)]

  result <- data.table::rbindlist(list(male_dist, female_dist))
  result[, raw_prop := NULL]

  total <- sum(result$proportion)
  if (abs(total - 1) > 0.001) {
    result[, proportion := proportion / total]
  }

  data.table::setorder(result, sex, age)
  result
}

#' Build O Stock from TR2025 Net Flows
#'
#' @description
#' Builds up the O population stock from TR2025 V.A2 net flows.
#' Starting from 0 in 1940, accumulates annual net flows while applying
#' mortality and aging.
#'
#' @param o_flows data.table: TR2025 O flows with o_net column
#' @param age_sex_dist data.table: age-sex distribution proportions
#' @param mortality data.table: death probabilities by year, age, sex
#' @param years Integer vector: years to calculate
#' @param ages Integer vector: ages to include
#'
#' @return data.table with O stock by year, age, sex
#'
#' @export
build_o_stock_from_flows <- function(o_flows,
                                      age_sex_dist,
                                      mortality,
                                      years,
                                      ages) {
  first_year <- min(years)

  # Initialize stock at first year (0 in 1940)
  initial_stock <- data.table::data.table(
    year = first_year,
    age = rep(ages, 2),
    sex = c(rep("male", length(ages)), rep("female", length(ages))),
    population = 0,
    source = "initial"
  )

  result_list <- list(initial_stock)
  current_stock <- data.table::copy(initial_stock)

  # Build up stock year by year
  for (yr in years[-1]) {
    # Get net flow for this year
    yr_net <- o_flows[year == yr, o_net]
    if (length(yr_net) == 0 || is.na(yr_net)) {
      yr_net <- 0
    }

    # Distribute net flow by age-sex
    # age_sex_dist can be static (no year column) or year-varying (with year column)
    if ("year" %in% names(age_sex_dist)) {
      flow_by_age_sex <- age_sex_dist[year == yr, .(age, sex, proportion)]
      if (nrow(flow_by_age_sex) == 0) {
        # Year not found in year-varying distribution — use nearest available year
        available_years <- sort(unique(age_sex_dist$year))
        nearest_yr <- available_years[which.min(abs(available_years - yr))]
        flow_by_age_sex <- age_sex_dist[year == nearest_yr, .(age, sex, proportion)]
      }
    } else {
      flow_by_age_sex <- data.table::copy(age_sex_dist)
    }
    flow_by_age_sex[, net_flow := yr_net * proportion]

    # Get mortality rates
    if (is.null(mortality) || !yr %in% mortality$year) {
      cli::cli_abort(c(
        "Mortality data missing for year {yr} in O-population buildup",
        "i" = "Ensure TR2025 qx files are present in data/raw/SSA_TR2025/"
      ))
    }
    qx <- mortality[year == yr]

    # Calculate new stock for each age-sex
    new_pop_list <- list()

    max_age <- max(ages)

    for (s in c("male", "female")) {
      for (a in ages) {
        # Previous year's population at age a-1 (for aging)
        if (a == 0) {
          prev_pop <- 0  # O population at birth is 0
        } else if (a == max_age) {
          # Open-ended group (100+): survivors from age 99 aging in + survivors from 100+ staying
          prev_pop_from_below <- current_stock[age == (a - 1) & sex == s, population]
          prev_pop_staying <- current_stock[age == a & sex == s, population]
          if (length(prev_pop_from_below) == 0) prev_pop_from_below <- 0
          if (length(prev_pop_staying) == 0) prev_pop_staying <- 0
          prev_pop <- prev_pop_from_below + prev_pop_staying
        } else {
          prev_pop <- current_stock[age == (a - 1) & sex == s, population]
          if (length(prev_pop) == 0) {
            cli::cli_abort("Missing O-stock at year {yr - 1}, age {a - 1}, sex {s}")
          }
        }

        # Apply mortality to get survivors
        q <- qx[age == a & sex == s, qx]
        if (length(q) == 0) {
          cli::cli_abort("Missing qx at year {yr}, age {a}, sex {s}")
        }
        survivors <- prev_pop * (1 - q)

        # Add net flow for this age-sex
        net_in <- flow_by_age_sex[age == a & sex == s, net_flow]
        if (length(net_in) == 0) {
          cli::cli_abort("Missing O-flow distribution at age {a}, sex {s}")
        }

        # New stock = survivors + net inflow (floor at 0)
        new_pop <- max(0, survivors + net_in)

        new_pop_list[[length(new_pop_list) + 1]] <- data.table::data.table(
          year = yr,
          age = a,
          sex = s,
          population = new_pop,
          source = "buildup"
        )
      }
    }

    new_stock <- data.table::rbindlist(new_pop_list)
    current_stock <- data.table::copy(new_stock)
    result_list[[length(result_list) + 1]] <- new_stock
  }

  data.table::rbindlist(result_list, use.names = TRUE)
}

# =============================================================================
# COMPONENT DATA GATHERING
# =============================================================================

#' Gather Components for O Population Calculation
#'
#' @description
#' Builds all component data needed for the O-population residual calculation
#' from upstream target data. Uses the same LPR assumptions and distributions
#' as the historical population module.
#'
#' @param years Integer vector of years
#' @param ages Integer vector of ages
#' @param cache_dir Character: cache directory
#' @param config List: configuration
#' @param lpr_assumptions data.table: V.A2 assumptions (year, total_lpr, total_emigration, etc.)
#' @param immigration_dist Immigration age-sex distribution
#' @param emigration_dist Emigration age-sex distribution
#' @param births_by_sex data.table: births by sex from NCHS
#'
#' @keywords internal
gather_o_components <- function(years, ages, cache_dir, config = NULL,
                                 lpr_assumptions = NULL,
                                 immigration_dist = NULL,
                                 emigration_dist = NULL,
                                 births_by_sex = NULL) {
  components <- list()

  # 1. Mortality data (qx)
  cli::cli_alert("  Loading mortality data...")
  components$mortality <- load_mortality_for_o(years, config)

  # 2-3. LPR Immigration and Emigration from upstream targets
  cli::cli_alert("  Building LPR immigration/emigration from upstream data...")
  if (!is.null(lpr_assumptions) && !is.null(immigration_dist) && !is.null(emigration_dist)) {
    imm_emig <- build_historical_immigration_emigration(
      lpr_assumptions = lpr_assumptions,
      immigration_dist = immigration_dist,
      emigration_dist = emigration_dist,
      years = years
    )
    # Rename column for residual calculation compatibility
    components$lpr_immigration <- imm_emig$immigration
    data.table::setnames(components$lpr_immigration, "immigration", "immigration", skip_absent = TRUE)
    components$emigration <- imm_emig$emigration
    data.table::setnames(components$emigration, "emigration", "emigration", skip_absent = TRUE)
  } else {
    cli::cli_abort(c(
      "lpr_assumptions, immigration_dist, and emigration_dist are required for residual method",
      "i" = "Pass these from upstream targets (lpr_assumptions, lpr_distribution, emigration_distribution)"
    ))
  }

  # 4. Adjustments of Status (AOS)
  cli::cli_alert("  Building AOS estimates from LPR data...")
  if (is.null(config$historical_population$o_population$aos_ratio)) {
    cli::cli_abort("Config missing {.field historical_population.o_population.aos_ratio}")
  }
  aos_ratio <- config$historical_population$o_population$aos_ratio
  # AOS = aos_ratio * LPR immigration (by age/sex)
  components$aos <- data.table::copy(components$lpr_immigration)
  components$aos[, aos := immigration * aos_ratio]
  components$aos[, immigration := NULL]
  cli::cli_alert_info("  AOS ratio: {aos_ratio} applied to LPR immigration")

  # 5. Births from upstream NCHS data
  cli::cli_alert("  Loading births data...")
  if (!is.null(births_by_sex) && nrow(births_by_sex) > 0) {
    components$births <- build_historical_births(births_by_sex, years)
  } else {
    cli::cli_abort("births_by_sex is required for residual method")
  }

  cli::cli_alert_success("All O population component data gathered")

  components
}

#' Load Total Population for O Calculation
#'
#' @keywords internal
load_total_population <- function(start_year, end_year, cache_dir) {
  cache_file <- file.path(
    cache_dir, "historical_population",
    sprintf("ss_population_%d_%d.rds", start_year, end_year)
  )

  if (!file.exists(cache_file)) {
    return(NULL)
  }

  cached <- readRDS(cache_file)

  # Handle both old and new cache formats
  if (is.list(cached) && "population" %in% names(cached)) {
    return(cached$population)
  } else {
    return(cached)
  }
}

#' Load Mortality Data for O Calculation
#'
#' @description
#' Loads death probabilities (qx) for the O-population calculation.
#' Uses the same config-driven approach as the historical population module:
#' tries mortality subprocess cache first, then TR2025 DeathProbsE files.
#'
#' @param years Integer vector of years
#' @param config List: configuration with mortality file paths
#'
#' @keywords internal
load_mortality_for_o <- function(years, config = NULL) {
  # Use the shared load_mortality_data function from historical_population.R
  # which handles cache + config-driven TR file paths
  load_mortality_data(config)
}

# generate_simplified_mortality() — REMOVED
# Synthetic mortality rates were a silent fallback. Real qx data from TR2025
# files or the mortality subprocess is now required.

#' Load LPR Immigration for O Calculation
#'
#' @keywords internal
load_lpr_immigration_for_o <- function(years, ages) {
  # Try to load from LPR immigration subprocess
  lpr_file <- here::here("data/cache/immigration/lpr_immigration.rds")

  if (file.exists(lpr_file)) {
    lpr_data <- readRDS(lpr_file)
    lpr_data <- data.table::as.data.table(lpr_data)
    return(lpr_data[year %in% years & age %in% ages])
  }

  cli::cli_abort(c(
    "LPR immigration cache not found at {lpr_file}",
    "i" = "Run LPR immigration subprocess first: targets::tar_make(names = matches('lpr_immigration'))"
  ))
}

# generate_historical_lpr_estimates() — REMOVED
# Synthetic LPR estimates were a silent fallback. Real LPR data from the
# immigration subprocess is now required.

#' Load Emigration for O Calculation
#'
#' @param years Integer vector: years to load
#' @param ages Integer vector: ages to load
#' @param config List: configuration with emigration_ratio
#'
#' @keywords internal
load_emigration_for_o <- function(years, ages, config = NULL) {
  # Read emigration ratio from config
  if (is.null(config$historical_population$o_population$emigration_ratio)) {
    cli::cli_abort("Config missing {.field historical_population.o_population.emigration_ratio}")
  }
  emigration_ratio <- config$historical_population$o_population$emigration_ratio
  # Try to load from emigration subprocess
  emig_file <- here::here("data/cache/emigration/legal_emigration.rds")

  if (file.exists(emig_file)) {
    emig_data <- readRDS(emig_file)
    emig_data <- data.table::as.data.table(emig_data)
    return(emig_data[year %in% years & age %in% ages])
  }

  cli::cli_abort(c(
    "Emigration cache not found at {emig_file}",
    "i" = "Run legal emigration subprocess first: targets::tar_make(names = matches('legal_emigration'))"
  ))
}

#' Load AOS for O Calculation
#'
#' @keywords internal
load_aos_for_o <- function(years, ages, config = NULL) {
  # Try to load from LPR immigration subprocess
  aos_file <- here::here("data/cache/immigration/aos.rds")

  if (file.exists(aos_file)) {
    aos_data <- readRDS(aos_file)
    aos_data <- data.table::as.data.table(aos_data)
    return(aos_data[year %in% years & age %in% ages])
  }

  # Generate estimates using AOS ratio from config
  if (is.null(config$historical_population$o_population$aos_ratio)) {
    cli::cli_abort("Config missing {.field historical_population.o_population.aos_ratio}")
  }
  aos_ratio <- config$historical_population$o_population$aos_ratio
  cli::cli_alert_info("AOS cache not found, generating estimates (~{aos_ratio * 100}% of LPR)")
  lpr <- load_lpr_immigration_for_o(years, ages)
  aos <- data.table::copy(lpr)
  aos[, aos := immigration * aos_ratio]
  aos[, immigration := NULL]

  aos
}

#' Load Births for O Calculation
#'
#' @keywords internal
load_births_for_o <- function(years) {
  # Try to load from fertility subprocess
  births_file <- here::here("data/cache/fertility/births_by_sex.rds")

  if (file.exists(births_file)) {
    return(readRDS(births_file))
  }

  cli::cli_abort(c(
    "Births cache not found at {births_file}",
    "i" = "Run fertility subprocess first: targets::tar_make(names = matches('fertility'))"
  ))
}

#' Load DHS Estimates for O Calculation
#'
#' @keywords internal
load_dhs_estimates_for_o <- function(years) {
  # Filter to valid DHS years (1990-2022)
  dhs_years <- years[years >= 1990 & years <= 2022]

  if (length(dhs_years) == 0) {
    cli::cli_abort(c(
      "No valid DHS years in requested range",
      "i" = "DHS unauthorized estimates are available for 1990-2022",
      "i" = "Requested years: {paste(range(years), collapse = '-')}"
    ))
  }

  result <- fetch_dhs_unauthorized_estimates(years = dhs_years)
  if (is.null(result) || nrow(result) == 0) {
    cli::cli_abort(c(
      "DHS unauthorized estimates unavailable",
      "i" = "fetch_dhs_unauthorized_estimates() returned no data",
      "i" = "Check DHS data file availability"
    ))
  }
  result
}

# =============================================================================
# RESIDUAL CALCULATION
# =============================================================================

#' Calculate Net Residuals (Other In - Other Out)
#'
#' @description
#' For each year, backs out the net residual from the cohort-based
#' demographic accounting identity:
#'
#'   For age a > 0:
#'     other_net(a,s,Y) = P(a,s,Y) - P(a-1,s,Y-1)*(1-qx(a,s,Y))
#'                        - imm(a,s,Y) + emig(a,s,Y) - aos(a,s,Y)
#'
#'   For age 0:
#'     other_net(0,s,Y) = P(0,s,Y) - births(s,Y)*(1-q0(s,Y)/2)
#'                        - imm(0,s,Y) + emig(0,s,Y) - aos(0,s,Y)
#'
#' Per TR2025 Section 1.4.c: "an initial net residual estimate by single year
#' of age and sex is backed out from estimates of beginning and end of year
#' populations, births, deaths, LPR immigrants, adjustments of status, and
#' legal emigrants."
#'
#' @param total_pop data.table: Total population by year, age, sex
#' @param components list: All demographic components
#' @param years Integer vector: Years to calculate
#' @param ages Integer vector: Ages to include
#'
#' @return data.table with net residuals by year, age, sex
#'
#' @export
calculate_other_residuals <- function(total_pop,
                                       components,
                                       years,
                                       ages) {
  max_age <- max(ages)
  result_list <- list()

  for (yr in years[-1]) {  # Start from second year (need begin pop)
    prev_yr <- yr - 1

    # Get beginning (Dec 31 Y-1) and end (Dec 31 Y) populations
    begin_pop <- total_pop[year == prev_yr & age %in% ages]
    end_pop <- total_pop[year == yr & age %in% ages]

    if (nrow(begin_pop) == 0 || nrow(end_pop) == 0) {
      cli::cli_alert_warning("Missing population data for {yr}, skipping")
      next
    }

    # Get births (1940+ from extended nchs_births_by_sex target)
    # TR2025 Items 35/32: "births by month and sex of child, for years 1931-2023"
    births_data <- components$births
    if (is.null(births_data) || !yr %in% births_data$year) {
      cli::cli_abort(c(
        "Births data missing for year {yr} in residual calculation",
        "i" = "nchs_births_by_sex target should cover 1940+ (pre-1968 from CDC NVSR, post-1968 from NBER)"
      ))
    }
    births_male <- births_data[year == yr, male]
    births_female <- births_data[year == yr, female]
    if (length(births_male) == 0 || length(births_female) == 0) {
      cli::cli_abort("Births data incomplete for year {yr}: male={length(births_male)}, female={length(births_female)}")
    }

    # Get mortality
    mortality <- components$mortality
    if (is.null(mortality) || !yr %in% mortality$year) {
      cli::cli_abort(c(
        "Mortality data missing for year {yr} in residual calculation",
        "i" = "Ensure TR2025 DeathProbsE files are configured in config"
      ))
    }
    qx_yr <- mortality[year == yr]

    # Get LPR immigration
    lpr <- components$lpr_immigration
    if (is.null(lpr) || !yr %in% lpr$year) {
      cli::cli_abort("LPR immigration data missing for year {yr} in residual calculation")
    }
    lpr_yr <- lpr[year == yr]

    # Get emigration
    emig <- components$emigration
    if (is.null(emig) || !yr %in% emig$year) {
      cli::cli_abort("Emigration data missing for year {yr} in residual calculation")
    }
    emig_yr <- emig[year == yr]

    # Get AOS
    aos <- components$aos
    if (is.null(aos) || !yr %in% aos$year) {
      cli::cli_abort("AOS data missing for year {yr} in residual calculation")
    }
    aos_yr <- aos[year == yr]

    # Compute residual for each age-sex cell using cohort accounting
    yr_residuals <- list()

    for (s in c("male", "female")) {
      for (a in ages) {
        end_p <- end_pop[age == a & sex == s, population]
        if (length(end_p) == 0) end_p <- 0

        # Expected population from cohort aging
        if (a == 0) {
          # Age 0: expected = births surviving infant mortality
          birth_count <- if (s == "male") births_male else births_female
          q0 <- qx_yr[age == 0 & sex == s, qx]
          if (length(q0) == 0) q0 <- 0
          expected <- birth_count * (1 - q0 / 2)
        } else if (a == max_age) {
          # Open-ended group: survivors from age max_age-1 + survivors from max_age
          prev_below <- begin_pop[age == (a - 1) & sex == s, population]
          prev_stay <- begin_pop[age == a & sex == s, population]
          if (length(prev_below) == 0) prev_below <- 0
          if (length(prev_stay) == 0) prev_stay <- 0
          q_below <- qx_yr[age == (a - 1) & sex == s, qx]
          q_stay <- qx_yr[age == a & sex == s, qx]
          if (length(q_below) == 0) q_below <- 0
          if (length(q_stay) == 0) q_stay <- 0
          expected <- prev_below * (1 - q_below) + prev_stay * (1 - q_stay)
        } else {
          # Standard: survivors from age a-1 last year
          prev_a <- begin_pop[age == (a - 1) & sex == s, population]
          if (length(prev_a) == 0) prev_a <- 0
          q <- qx_yr[age == a & sex == s, qx]
          if (length(q) == 0) q <- 0
          expected <- prev_a * (1 - q)
        }

        # Add net LPR migration and AOS
        imm_val <- lpr_yr[age == a & sex == s, immigration]
        if (length(imm_val) == 0) imm_val <- 0
        emig_val <- emig_yr[age == a & sex == s, emigration]
        if (length(emig_val) == 0) emig_val <- 0
        aos_val <- aos_yr[age == a & sex == s, aos]
        if (length(aos_val) == 0) aos_val <- 0

        expected <- expected + imm_val - emig_val + aos_val

        # Residual = actual - expected = net other immigration
        residual <- end_p - expected

        yr_residuals[[length(yr_residuals) + 1L]] <- data.table::data.table(
          year = as.integer(yr), age = a, sex = s, residual = residual
        )
      }
    }

    result_list[[as.character(yr)]] <- data.table::rbindlist(yr_residuals)
  }

  data.table::rbindlist(result_list)
}

#' Modify Residuals for Reasonableness
#'
#' @description
#' Per TR2025 Section 1.4.c (Eq 1.4.3): "These residuals are then modified to
#' ensure reasonableness." For each (age, sex) time series:
#' 1. Apply rolling median smoothing to remove year-to-year noise
#' 2. Floor at zero — net O immigration should be non-negative (temporary and
#'    unlawful immigrants are net inflows to the Social Security area)
#'
#' Parameters are read from config: `historical_population.o_population.residual_smoothing`
#'
#' @param residuals data.table with columns: year, age, sex, residual
#' @param config List: configuration from tr2025.yaml
#'
#' @return data.table with modified residuals
#'
#' @keywords internal
modify_residuals_for_reasonableness <- function(residuals, config = NULL) {
  result <- data.table::copy(residuals)

  # Read smoothing parameters from config
  smooth_cfg <- config$historical_population$o_population$residual_smoothing
  if (is.null(smooth_cfg)) {
    cli::cli_abort("Config missing {.field historical_population.o_population.residual_smoothing}")
  }

  window <- smooth_cfg$rolling_median_window
  if (is.null(window)) {
    cli::cli_abort("Config missing {.field residual_smoothing.rolling_median_window}")
  }
  if (window %% 2 == 0) {
    cli::cli_abort("rolling_median_window must be odd, got {window}")
  }

  original_sum <- sum(result$residual, na.rm = TRUE)

  # Step 1: For each (age, sex) pair, smooth the time series across years
  # using rolling median to remove year-to-year noise while preserving trends
  data.table::setorder(result, sex, age, year)

  result[, residual := {
    if (.N >= window) {
      stats::runmed(residual, k = window, endrule = "median")
    } else {
      rep(stats::median(residual, na.rm = TRUE), .N)
    }
  }, by = .(age, sex)]

  smoothed_sum <- sum(result$residual, na.rm = TRUE)

  # Step 2: Floor residuals at 0 — net O immigration should be non-negative.
  # Negative residuals indicate ages where actual population is less than
  # what the accounting identity predicts. These ages don't contribute to
  # O-immigration inflows and should not reduce the distribution.
  # The stock builder uses V.A2 o_net for annual LEVELS; these residuals
  # are only used to derive age-sex PROPORTIONS.
  # TR2025 Section 1.4.c: "modified so that the time series for each year
  # of age and sex has an appropriate level, and does not change sign."
  n_neg <- sum(result$residual < 0)
  result[residual < 0, residual := 0]

  floored_sum <- sum(result$residual, na.rm = TRUE)
  cli::cli_alert_info("Residual modification: original={format(round(original_sum), big.mark=',')} -> smoothed={format(round(smoothed_sum), big.mark=',')} -> floored={format(round(floored_sum), big.mark=',')}")
  cli::cli_alert_info("Floored {format(n_neg, big.mark=',')} negative residuals to 0")

  data.table::setorder(result, year, sex, age)
  result
}

#' Extract Per-Year Age-Sex Proportions from Modified Residuals
#'
#' @description
#' Converts modified residuals (floored at 0) into per-year age-sex proportions.
#' For years where all residuals are 0 (e.g., pre-1980 when O-immigration was
#' minimal), falls back to the static DHS-based distribution.
#'
#' @param modified_residuals data.table with columns: year, age, sex, residual
#' @param fallback_dist data.table with columns: age, sex, proportion (static distribution)
#'
#' @return data.table with columns: year, age, sex, proportion, source
#'
#' @keywords internal
extract_residual_proportions <- function(modified_residuals, fallback_dist) {
  result_list <- list()

  for (yr in sort(unique(modified_residuals$year))) {
    yr_data <- modified_residuals[year == yr]
    total_residual <- sum(yr_data$residual, na.rm = TRUE)

    if (total_residual > 0) {
      # Derive proportions from positive residuals
      yr_props <- yr_data[, .(year = yr, age, sex, proportion = residual / total_residual,
                               source = "residual")]
    } else {
      # All residuals are 0 for this year — use static distribution
      yr_props <- data.table::copy(fallback_dist)
      yr_props[, `:=`(year = yr, source = "fallback")]
    }

    result_list[[length(result_list) + 1L]] <- yr_props
  }

  result <- data.table::rbindlist(result_list, use.names = TRUE, fill = TRUE)
  data.table::setorder(result, year, sex, age)
  result
}

# =============================================================================
# STOCK BUILD-UP
# =============================================================================

#' Build Temporary/Unlawfully Present Stock from Flows
#'
#' @description
#' Builds up the O population stock from flows using:
#' - Modified net residuals (other in - other out)
#' - Deaths among O population (using total population mortality rates)
#' - Deaths among AOS (adjustment of status) immigrants
#'
#' @param residuals data.table: Modified residuals by year, age, sex
#' @param mortality data.table: Death probabilities by year, age, sex
#' @param aos data.table: Adjustments of status by year, age, sex
#' @param years Integer vector: Years to calculate
#' @param ages Integer vector: Ages to include
#'
#' @return data.table with O stock by year, age, sex
#'
#' @export
build_temp_unlawful_stock <- function(residuals,
                                       mortality,
                                       aos,
                                       years,
                                       ages) {
  # Initialize stock at first year
  # Start with estimated stock in 1940 (very small unauthorized population)
  first_year <- min(years)

  # Initial stock estimate
  # In 1940, unauthorized immigration was minimal
  initial_stock <- data.table::data.table(
    year = first_year,
    age = rep(ages, 2),
    sex = c(rep("male", length(ages)), rep("female", length(ages))),
    population = 0,
    source = "initial"
  )

  result_list <- list(initial_stock)

  # Store current stock separately for tracking
  current_stock <- data.table::copy(initial_stock)

  # Build up stock year by year
  for (yr in years[-1]) {
    prev_yr <- yr - 1

    # Get mortality rates
    if (is.null(mortality) || !yr %in% mortality$year) {
      cli::cli_abort(c(
        "Mortality data missing for year {yr} in O-stock buildup",
        "i" = "Ensure TR2025 qx files are present in data/raw/SSA_TR2025/"
      ))
    }
    qx <- mortality[year == yr]

    # Get residuals for this year
    yr_residuals <- residuals[year == yr]
    if (nrow(yr_residuals) == 0) {
      cli::cli_alert_warning("No residuals for year {yr}, using zero residuals")
      yr_residuals <- data.table::data.table(
        age = rep(ages, 2),
        sex = c(rep("male", length(ages)), rep("female", length(ages))),
        residual = 0
      )
    }

    # Calculate new stock
    # 1. Age forward previous stock (age x -> age x+1)
    # 2. Apply mortality
    # 3. Add net residual (inflows - outflows)

    new_pop_list <- list()
    max_age <- max(ages)

    for (s in c("male", "female")) {
      for (a in ages) {
        # Previous year's population at age a-1 (or 0 for age 0)
        if (a == 0) {
          prev_pop <- 0  # O population at birth is 0 (births are US citizens)
        } else if (a == max_age) {
          # Open-ended group (100+): survivors from age 99 aging in + survivors from 100+ staying
          prev_pop_from_below <- current_stock[age == (a - 1) & sex == s, population]
          prev_pop_staying <- current_stock[age == a & sex == s, population]
          if (length(prev_pop_from_below) == 0) prev_pop_from_below <- 0
          if (length(prev_pop_staying) == 0) prev_pop_staying <- 0
          prev_pop <- prev_pop_from_below + prev_pop_staying
        } else {
          prev_pop <- current_stock[age == (a - 1) & sex == s, population]
          if (length(prev_pop) == 0) {
            cli::cli_abort("Missing O-stock at year {yr - 1}, age {a - 1}, sex {s}")
          }
        }

        # Apply mortality to get survivors
        q <- qx[age == a & sex == s, qx]
        if (length(q) == 0) {
          cli::cli_abort("Missing qx at year {yr}, age {a}, sex {s}")
        }
        survivors <- prev_pop * (1 - q)

        # Add net residual for this age-sex
        net_inflow <- yr_residuals[age == a & sex == s, residual]
        if (length(net_inflow) == 0) {
          cli::cli_abort("Missing residual at year {yr}, age {a}, sex {s}")
        }

        # New stock = survivors + net inflow (but floor at 0)
        new_pop <- max(0, survivors + net_inflow)

        new_pop_list[[length(new_pop_list) + 1]] <- data.table::data.table(
          year = yr,
          age = a,
          sex = s,
          population = new_pop,
          source = "buildup"
        )
      }
    }

    new_stock <- data.table::rbindlist(new_pop_list)

    # Update current stock for next iteration
    current_stock <- data.table::copy(new_stock)

    result_list[[length(result_list) + 1]] <- new_stock
  }

  data.table::rbindlist(result_list, use.names = TRUE)
}

# =============================================================================
# DHS ADJUSTMENT
# =============================================================================

#' Adjust O Population to Match DHS Estimates
#'
#' @description
#' Forces the O population totals to match DHS unauthorized immigrant estimates:
#' - 2001-2004: Linear interpolation from Jan 2000 to Jan 2005 totals
#' - 2005-2023: Force totals to match DHS estimates
#'
#' @param o_stock data.table: Built-up O population stock
#' @param dhs_estimates data.table: DHS unauthorized estimates by year
#' @param ages Integer vector: Ages to include
#'
#' @return data.table with adjusted O population
#'
#' @export
adjust_o_to_dhs <- function(o_stock,
                             dhs_estimates,
                             ages = 0:99) {
  result <- data.table::copy(o_stock)

  # Get total O by year
  o_totals <- o_stock[, .(buildup_total = sum(population)), by = year]

  # Merge with DHS estimates
  if ("unauthorized_population" %in% names(dhs_estimates)) {
    dhs_col <- "unauthorized_population"
  } else {
    dhs_col <- names(dhs_estimates)[2]  # Assume second column is the estimate
  }

  o_totals <- merge(
    o_totals,
    dhs_estimates[, .(year, dhs_total = get(dhs_col))],
    by = "year",
    all.x = TRUE
  )

  # Handle 2001-2004: Linear interpolation
  # From January 2000 to January 2005
  if (2000 %in% o_totals$year && 2005 %in% o_totals$year) {
    total_2000 <- o_totals[year == 2000, dhs_total]
    total_2005 <- o_totals[year == 2005, dhs_total]

    if (is.na(total_2000)) total_2000 <- o_totals[year == 2000, buildup_total]
    if (is.na(total_2005)) total_2005 <- dhs_estimates[year == 2005, get(dhs_col)]

    if (!is.na(total_2000) && !is.na(total_2005)) {
      for (yr in 2001:2004) {
        weight <- (yr - 2000) / (2005 - 2000)
        interpolated <- total_2000 + weight * (total_2005 - total_2000)
        o_totals[year == yr, dhs_total := interpolated]
      }
    }
  }

  # Calculate adjustment ratios
  o_totals[, ratio := dhs_total / buildup_total]
  o_totals[is.na(ratio) | is.infinite(ratio), ratio := 1]
  o_totals[ratio < 0, ratio := 1]  # Handle negative buildup (shouldn't happen)

  # Apply ratios to adjust populations
  result <- merge(result, o_totals[, .(year, ratio)], by = "year", all.x = TRUE)
  result[is.na(ratio), ratio := 1]

  # Only adjust years 2000+
  result[year >= 2000, population := population * ratio]

  # Log adjustment summary
  adjusted_years <- sum(result$ratio != 1) / (length(ages) * 2)  # approx unique years adjusted
  cli::cli_alert_info("Adjusted {round(adjusted_years)} years to DHS estimates")

  # Verify totals for key years
  adjusted_totals <- result[, .(adjusted_total = sum(population)), by = year]
  check_years <- c(2005, 2010, 2020, 2022)
  check_years <- check_years[check_years %in% adjusted_totals$year & check_years %in% dhs_estimates$year]

  for (yr in check_years) {
    adj_total <- adjusted_totals[year == yr, adjusted_total]
    dhs_total <- dhs_estimates[year == yr, get(dhs_col)]
    diff_pct <- (adj_total - dhs_total) / dhs_total * 100
    cli::cli_alert_info("  {yr}: Adjusted={format(round(adj_total), big.mark=',')}, DHS={format(round(dhs_total), big.mark=',')}, Diff={round(diff_pct, 1)}%")
  }

  result[, ratio := NULL]
  result
}

# =============================================================================
# VALIDATION
# =============================================================================

#' Validate O Population Against DHS Estimates
#'
#' @description
#' Compares calculated O population against DHS unauthorized estimates.
#'
#' @param o_pop data.table: Calculated O population
#' @param tolerance Numeric: Acceptable relative difference (default: 0.05 = 5%)
#'
#' @return data.table with validation results
#'
#' @export
validate_o_population <- function(o_pop, tolerance = 0.05) {
  cli::cli_h1("Validating O Population Against DHS Estimates")

  # Load DHS estimates
  dhs <- tryCatch({
    fetch_dhs_unauthorized_estimates(years = 2000:2022)
  }, error = function(e) {
    cli::cli_alert_warning("Could not load DHS estimates: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(dhs)) {
    return(NULL)
  }

  # Calculate totals by year
  o_totals <- o_pop[, .(calculated = sum(population)), by = year]
  o_totals <- merge(
    o_totals,
    dhs[, .(year, dhs = unauthorized_population)],
    by = "year"
  )

  o_totals[, diff_pct := (calculated - dhs) / dhs * 100]
  o_totals[, pass := abs(diff_pct) <= tolerance * 100]

  # Summary
  n_pass <- sum(o_totals$pass)
  n_total <- nrow(o_totals)

  cli::cli_alert_info("Validation: {n_pass}/{n_total} years within {tolerance * 100}% tolerance")

  # Show worst deviations
  worst <- o_totals[order(-abs(diff_pct))][1:5]
  cli::cli_alert("Largest deviations:")
  for (i in seq_len(min(5, nrow(worst)))) {
    cli::cli_alert("  {worst$year[i]}: {round(worst$diff_pct[i], 2)}%")
  }

  o_totals
}

# =============================================================================
# SUMMARY FUNCTIONS
# =============================================================================

#' Summarize O Population by Year
#'
#' @export
summarize_o_population <- function(o_pop) {
  cli::cli_h2("Temporary/Unlawfully Present Population Summary")

  # Total by year
  totals <- o_pop[, .(total = sum(population)), by = year]

  # Sample years
  sample_years <- c(1940, 1960, 1980, 1990, 2000, 2007, 2010, 2020, 2022)
  sample_years <- sample_years[sample_years %in% totals$year]

  cli::cli_alert("O Population totals:")
  for (yr in sample_years) {
    total <- totals[year == yr, total]
    cli::cli_alert("  {yr}: {format(round(total), big.mark = ',')}")
  }

  # By sex for most recent year
  latest_year <- max(o_pop$year)
  by_sex <- o_pop[year == latest_year, .(total = sum(population)), by = sex]

  cli::cli_alert("By sex ({latest_year}):")
  for (i in seq_len(nrow(by_sex))) {
    cli::cli_alert("  {by_sex$sex[i]}: {format(round(by_sex$total[i]), big.mark = ',')}")
  }

  # Age distribution for most recent year
  by_age_group <- o_pop[year == latest_year, .(
    age_group = data.table::fcase(
      age < 18, "Under 18",
      age >= 18 & age < 25, "18-24",
      age >= 25 & age < 45, "25-44",
      age >= 45 & age < 55, "45-54",
      age >= 55 & age < 65, "55-64",
      age >= 65, "65+"
    ),
    population = population
  )][, .(total = sum(population)), by = age_group]

  total_latest <- sum(by_age_group$total)
  by_age_group[, pct := round(total / total_latest * 100, 1)]

  cli::cli_alert("Age distribution ({latest_year}):")
  for (i in seq_len(nrow(by_age_group))) {
    cli::cli_alert("  {by_age_group$age_group[i]}: {format(round(by_age_group$total[i]), big.mark = ',')} ({by_age_group$pct[i]}%)")
  }

  invisible(totals)
}

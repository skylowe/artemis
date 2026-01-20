# ARTEMIS: OASDI Projection Model
# Targets pipeline definition
# =============================================================================

library(targets)
library(tarchetypes)

# Source all R files in R/ subdirectories
tar_source()
tar_source("R/utils")
tar_source("R/data_acquisition")
tar_source("R/demography")
tar_source("R/validation")

# Set global options for targets
tar_option_set(
  packages = c(
    "data.table",
    "dplyr",
    "tidyr",
    "httr2",
    "jsonlite",
    "yaml",
    "readxl",
    "readr",
    "haven",
    "here",
    "glue",
    "cli",
    "checkmate"
  ),
  format = "rds",  # Standard R serialization format
  error = "continue",
  memory = "transient",
  garbage_collection = TRUE
)

# =============================================================================
# PIPELINE DEFINITION
# =============================================================================

list(
  # ===========================================================================
  # CONFIGURATION TARGETS
  # ===========================================================================

  tar_target(
    config_assumptions,
    load_assumptions(here::here("config/assumptions/tr2025.yaml")),
    cue = tar_cue(mode = "thorough")
  ),

  tar_target(
    config_api,
    load_api_config(here::here("config/api_endpoints.yaml")),
    cue = tar_cue(mode = "thorough")
  ),

  # ===========================================================================
  # DATA ACQUISITION TARGETS - FERTILITY
  # ===========================================================================

  # Birth data from NCHS via NBER Stata files (1980-2024)
  # Downloads and caches birth counts by single year of mother's age
  tar_target(
    nchs_births_raw,
    {
      years <- config_assumptions$data_sources$historical_birth_data$start_year:2024
      all_births <- data.table::rbindlist(lapply(years, function(yr) {
        dt <- fetch_nchs_births_by_age(yr)
        dt[, year := yr]
        dt
      }))
      all_births
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Census Bureau female population estimates (1980-2024)
  # Uses file downloads for 1980-1989, API for 1990-2024
  tar_target(
    census_female_pop,
    {
      # Set census vintage from config (required)
      vintage <- config_assumptions$data_sources$census_vintage
      if (is.null(vintage)) {
        stop("census_vintage not set in config - please specify in config/assumptions/tr2025.yaml")
      }
      options(artemis.census_vintage = vintage)

      fetch_census_population_all(
        years = config_assumptions$data_sources$population_estimates$start_year:
                config_assumptions$data_sources$population_estimates$end_year,
        ages = 10:54,  # Wider range to capture all fertility ages after collapse
        sex = "female"
      )
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Historical birth rates (1917-1979) - DEFERRED
  # Only needed for historical output series, not projection methodology
  # tar_target(
  #   historical_birth_rates,
  #   load_historical_birth_rates(
  #     start_year = config_assumptions$data_sources$historical_rate_data$start_year,
  #     end_year = config_assumptions$data_sources$historical_rate_data$end_year
  #   )
  # ),

  # Provisional birth data - DEFERRED
  # Not needed since we have final 2024 data from NBER
  # May be needed in future when running projections before NBER publishes latest year
  # tar_target(
  #   provisional_births_2024,
  #   fetch_provisional_births(year = 2024, config = config_api)
  # ),

  # ===========================================================================
  # FERTILITY SUBPROCESS TARGETS
  # ===========================================================================

  # TR2025 implied births (derived from their age 0 population + infant mortality)
  # Used for birth substitution when configured
  tar_target(
    tr2025_implied_births,
    {
      substitute_years <- config_assumptions$fertility$use_tr2025_births_for_years
      if (is.null(substitute_years) || length(substitute_years) == 0) {
        # Return empty data.table if no substitution configured
        return(data.table::data.table(year = integer(), sex = character(), births = numeric()))
      }
      calculate_tr2025_implied_births(years = substitute_years)
    }
  ),

  # NCHS births with optional TR2025 substitution for specified years
  # Preserves NCHS age distribution but scales totals to match TR2025
  tar_target(
    nchs_births_adjusted,
    {
      substitute_years <- config_assumptions$fertility$use_tr2025_births_for_years
      substitute_tr2025_births(
        nchs_births = nchs_births_raw,
        tr2025_births = tr2025_implied_births,
        substitute_years = substitute_years
      )
    }
  ),

  # Step 1: Calculate historical birth rates (1980-2024)
  tar_target(
    fertility_rates_historical,
    calculate_historical_birth_rates(
      births = nchs_births_adjusted,  # Use adjusted births (with TR2025 substitution if configured)
      population = census_female_pop,
      min_age = config_assumptions$fertility$min_fertility_age,
      max_age = config_assumptions$fertility$max_fertility_age
    )
  ),

  # Step 1b: Apply TFR constraints for specified years if configured
  # TR2025 publishes TFR=1.62 for 2023 and constrains 2024 to TFR=1.62.
  # This scaling is applied before calculating ratios and projections.
  tar_target(
    fertility_rates_for_projection,
    constrain_tfr_for_years(
      birth_rates = fertility_rates_historical,
      constrain_tfr = config_assumptions$fertility$constrain_tfr
    )
  ),

  # Step 2: Calculate age-to-30 ratios (using constrained rates for base year)
  tar_target(
    fertility_age30_ratios,
    calculate_age30_ratios(fertility_rates_for_projection)
  ),

  # Step 3: Calculate trend factors (excluding 1997 due to NCHS method change)
  tar_target(
    fertility_trend_factors,
    calculate_trend_factors(
      ratios = fertility_age30_ratios,
      exclude_years = config_assumptions$fertility$exclude_years
    )
  ),

  # Step 4: Calculate ultimate years by age
  tar_target(
    fertility_ultimate_years,
    calculate_ultimate_years(
      min_age = config_assumptions$fertility$min_fertility_age,
      max_age = config_assumptions$fertility$max_fertility_age,
      base_year = config_assumptions$fertility$projection_start_year,
      end_year = config_assumptions$fertility$ultimate_year
    )
  ),

  # Step 5: Calculate interpolation weights for projection years
  tar_target(
    fertility_weights,
    calculate_interpolation_weights(
      years = config_assumptions$metadata$projection_period$start_year:
              config_assumptions$metadata$projection_period$end_year,
      base_year = config_assumptions$fertility$rate_base_year,
      ultimate_year = config_assumptions$fertility$age30_ultimate_year,
      exponent = config_assumptions$fertility$weight_exponent
    )
  ),

  # Step 6: Solve for ultimate age-30 rate to achieve target CTFR
  tar_target(
    fertility_ultimate_age30,
    solve_ultimate_age30_rate(
      target_ctfr = config_assumptions$fertility$ultimate_ctfr,
      base_age30_rate = fertility_rates_for_projection[year == config_assumptions$fertility$rate_base_year & age == 30, birth_rate],
      base_ratios = fertility_age30_ratios[year == config_assumptions$fertility$rate_base_year],
      trend_factors = fertility_trend_factors,
      ultimate_years = fertility_ultimate_years,
      base_year = config_assumptions$fertility$rate_base_year
    )
  ),

  # Step 7: Project age-30 rates from base to ultimate
  tar_target(
    fertility_age30_projected,
    project_age30_rates(
      years = config_assumptions$metadata$projection_period$start_year:
              config_assumptions$metadata$projection_period$end_year,
      base_rate = fertility_rates_for_projection[year == config_assumptions$fertility$rate_base_year & age == 30, birth_rate],
      ultimate_rate = fertility_ultimate_age30,
      weights = fertility_weights
    )
  ),

  # Step 8: Project all age-specific rates
  tar_target(
    fertility_rates_projected,
    project_birth_rates(
      years = config_assumptions$metadata$projection_period$start_year:
              config_assumptions$metadata$projection_period$end_year,
      age30_rates = fertility_age30_projected,
      base_ratios = fertility_age30_ratios[year == config_assumptions$fertility$rate_base_year],
      trend_factors = fertility_trend_factors,
      ultimate_years = fertility_ultimate_years,
      base_year = config_assumptions$fertility$rate_base_year
    )
  ),

  # Combined historical and projected rates (1980-2099)
  # Uses constrained rates for base year (2024) to match TR2025 methodology
  # Note: Exclude base year from projected to avoid duplicate
  tar_target(
    fertility_rates_complete,
    rbind(
      fertility_rates_for_projection[, .(year, age, birth_rate)],
      fertility_rates_projected[year > config_assumptions$fertility$rate_base_year]
    )
  ),

  # Step 9-10: Calculate TFR (period) and CTFR (cohort) series
  tar_target(
    fertility_totals,
    calculate_fertility_totals(fertility_rates_complete)
  ),

  # ===========================================================================
  # VALIDATION TARGETS
  # ===========================================================================

  # Load official TR tables for comparison
  # tar_target(
  #   tr_tables_official,
  #   load_tr_tables(tr_version = "TR2025")
  # ),

  # Validate fertility outputs
  # tar_target(
  #   fertility_validation,
  #   validate_fertility_outputs(
  #     rates = fertility_rates_complete,
  #     totals = fertility_totals,
  #     official_data = tr_tables_official,
  #     tolerance = 0.01
  #   )
  # )

  # ===========================================================================
  # DATA ACQUISITION TARGETS - MORTALITY
  # ===========================================================================

  # NCHS Deaths data (1968-2023) - downloads and caches by age, sex, cause
  tar_target(
    nchs_deaths_raw,
    {
      years <- 1968:2023
      all_deaths <- data.table::rbindlist(lapply(years, function(yr) {
        cache_file <- sprintf("data/cache/nchs_deaths/deaths_by_age_sex_cause_%d.rds", yr)
        if (file.exists(cache_file)) {
          dt <- readRDS(cache_file)
          dt[, year := yr]
          dt
        } else {
          dt <- fetch_nchs_deaths_by_age(yr)
          dt[, year := yr]
          dt
        }
      }))
      all_deaths
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Census population for mortality calculations (male and female separately, ages 0-100)
  tar_target(
    census_population_both,
    {
      # Get census vintage from config (required)
      vintage <- config_assumptions$data_sources$census_vintage
      if (is.null(vintage)) {
        stop("census_vintage not set in config - please specify in config/assumptions/tr2025.yaml")
      }
      options(artemis.census_vintage = vintage)

      cache_file <- sprintf("data/cache/census/population_by_age_sex_1980_2023_v%d_bysex.rds", vintage)
      if (file.exists(cache_file)) {
        cli::cli_alert_success("Loading cached Census Vintage {vintage} population data (by sex)")
        readRDS(cache_file)
      } else {
        cli::cli_alert_info("Fetching Census Vintage {vintage} population data (male and female)")
        # Fetch male and female separately to get sex-specific data
        male_pop <- fetch_census_population_all(
          years = 1980:2023,
          ages = 0:100,
          sex = "male"
        )
        female_pop <- fetch_census_population_all(
          years = 1980:2023,
          ages = 0:100,
          sex = "female"
        )
        result <- data.table::rbindlist(list(male_pop, female_pop), use.names = TRUE)
        # Cache the result
        dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
        saveRDS(result, cache_file)
        result
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Total births by year for q0 calculation (legacy - kept for compatibility)
  tar_target(
    nchs_births_total,
    {
      years <- 1968:2024
      data.table::rbindlist(lapply(years, function(yr) {
        births_file <- sprintf("data/raw/nchs/births_by_month_%d.rds", yr)
        if (file.exists(births_file)) {
          b <- readRDS(births_file)
          data.table::data.table(year = yr, births = sum(b$births))
        } else {
          # Fall back to births_by_age if available
          age_file <- sprintf("data/raw/nchs/births_by_age_%d.rds", yr)
          if (file.exists(age_file)) {
            b <- readRDS(age_file)
            data.table::data.table(year = yr, births = sum(b$births))
          } else {
            NULL
          }
        }
      }))
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Sex-specific births by year for accurate q0 calculation
  tar_target(
    nchs_births_by_sex,
    {
      years <- 1968:2024
      data.table::rbindlist(lapply(years, function(yr) {
        births_file <- sprintf("data/raw/nchs/births_by_month_sex_%d.rds", yr)
        if (file.exists(births_file)) {
          b <- readRDS(births_file)
          # Aggregate monthly data to annual by sex
          b[, .(births = sum(births)), by = .(year, sex)]
        } else {
          NULL
        }
      }))
    },
    cue = tar_cue(mode = "thorough")
  ),

  # ===========================================================================
  # MORTALITY SUBPROCESS TARGETS
  # ===========================================================================

  # Step 1: Calculate historical central death rates (mx) by age, sex, cause
  tar_target(
    mortality_mx_historical,
    calculate_central_death_rates(
      deaths = nchs_deaths_raw,
      population = census_population_both,
      by_cause = TRUE
    )
  ),

  # Step 2: Calculate total mx (sum across causes)
  tar_target(
    mortality_mx_total,
    calculate_total_death_rates(mortality_mx_historical)
  ),

  # Step 3: Calculate historical AAx (annual percentage reduction)
  # Uses weighted regression over 2008-2019 period (weights are hardcoded per SSA spec)
  tar_target(
    mortality_aax_historical,
    calculate_annual_reduction_rates(
      mx = mortality_mx_historical,
      start_year = config_assumptions$mortality$regression_start_year,
      end_year = config_assumptions$mortality$regression_end_year,
      by_cause = TRUE
    )
  ),

  # Step 4: Get starting mx values (fitted values from regression at base year)
  tar_target(
    mortality_mx_starting,
    calculate_starting_mx(
      aax_results = mortality_aax_historical,
      base_year = config_assumptions$mortality$base_year
    )
  ),

  # Step 5: Run full mortality projection
  # Note: starting_aax_method controls how starting AAx values are determined:
  #   "regression" - Use historical regression (original method)
  #   "capped"     - Cap starting AAx at multiple of ultimate (recommended)
  #   "tr2025"     - Derive from TR2025 death probabilities
  tar_target(
    mortality_mx_projected,
    run_mortality_projection(
      deaths = nchs_deaths_raw,
      population = census_population_both,
      base_year = config_assumptions$mortality$base_year,
      projection_end = config_assumptions$metadata$projection_period$end_year,
      by_cause = FALSE,
      starting_aax_method = if (is.null(config_assumptions$mortality$starting_aax_method)) "regression" else config_assumptions$mortality$starting_aax_method,
      mortality_config = config_assumptions$mortality
    )
  ),

  # Step 6: Calculate qx from mx (including q0 using births)
  # Note: Limited to 1980+ because Census population data starts in 1980
  tar_target(
    mortality_qx_unadjusted,
    {
      # Get years with population data (1980+)
      pop_years <- unique(census_population_both$year)
      mx_filtered <- mortality_mx_total[year %in% pop_years]

      # Calculate qx for ages 1+ from mx (ages 0-100, with 100 representing 100+)
      qx_from_mx <- convert_mx_to_qx(mx_filtered, max_age = 100)

      # Calculate q0 using deaths/births with actual sex-specific births
      infant_deaths <- nchs_deaths_raw[age == 0 & year %in% pop_years,
                                       .(deaths = sum(deaths)), by = .(year, sex)]

      # Use actual sex-specific births data
      q0 <- merge(infant_deaths, nchs_births_by_sex, by = c("year", "sex"), all.x = TRUE)
      q0 <- q0[!is.na(births)]  # Only years with births data
      q0[, qx := deaths / births]
      q0 <- q0[, .(year, age = 0L, sex, qx)]

      # Combine q0 with qx for ages 1+
      qx_ages1plus <- qx_from_mx[age >= 1, .(year, age, sex, qx)]
      result <- data.table::rbindlist(list(q0, qx_ages1plus), use.names = TRUE)
      data.table::setorder(result, year, sex, age)
      result
    }
  ),

  # Step 6b: Adjust qx for ages 85+ using HMD calibration
  # HMD provides well-researched mortality patterns at oldest ages
  tar_target(
    mortality_qx_historical,
    adjust_qx_with_hmd(
      qx = mortality_qx_unadjusted,
      transition_age = 85,
      max_age = 100  # 100 represents 100+ (open-ended age group)
    )
  ),

  # Step 7: Calculate life tables from qx
  tar_target(
    mortality_life_tables,
    calculate_life_table(
      qx = mortality_qx_historical,
      radix = 100000,
      max_age = 100  # 100 represents 100+ (open-ended age group)
    )
  ),

  # Step 8: Calculate life expectancy at key ages (historical)
  tar_target(
    mortality_life_expectancy_hist,
    calculate_life_expectancy(
      life_table = mortality_life_tables,
      at_ages = c(0, 65)
    )
  ),

  # ===========================================================================
  # PROJECTED MORTALITY TARGETS
  # ===========================================================================

  # Step 9: Extract and adjust projected qx (2024-2099)
  # The projection already has qx, but we need to apply HMD calibration for ages 85+
  # EXCEPTION: Skip HMD calibration for tr_qx method since TR2025 already has calibrated qx
  tar_target(
    mortality_qx_projected,
    {
      # Get projected qx from the projection output
      qx_proj <- mortality_mx_projected$projected_qx

      # Filter to projection years only (2024+, since 2020-2023 are in historical)
      qx_proj <- qx_proj[year >= 2024]

      # Check if using tr_qx method - if so, skip HMD calibration
      # TR2025 qx values are already properly calibrated for all ages
      method <- config_assumptions$mortality$starting_aax_method
      if (!is.null(method) && method == "tr_qx") {
        cli::cli_alert_info("Skipping HMD calibration for tr_qx method (using TR2025 qx directly)")
        # For tr_qx, aggregate ages 100+ into age 100 to match our 100+ approach
        # TR2025 files have ages 0-119, but we model 100+ as single open-ended group
        qx_proj[age > 100, age := 100L]
        qx_proj <- qx_proj[, .(qx = mean(qx, na.rm = TRUE)), by = .(year, age, sex)]
        return(qx_proj)
      }

      # Apply HMD calibration for ages 85-99 (for regression/capped methods)
      # Age 100 (100+) keeps its data-driven value
      adjust_qx_with_hmd(
        qx = qx_proj,
        transition_age = 85,
        max_age = 100  # 100 represents 100+ (open-ended age group)
      )
    }
  ),

  # Step 10: Calculate projected life tables
  tar_target(
    mortality_life_tables_projected,
    calculate_life_table(
      qx = mortality_qx_projected,
      radix = 100000,
      max_age = 100  # 100 represents 100+ (open-ended age group)
    )
  ),

  # Step 11: Calculate projected life expectancy
  tar_target(
    mortality_life_expectancy_proj,
    calculate_life_expectancy(
      life_table = mortality_life_tables_projected,
      at_ages = c(0, 65)
    )
  ),

  # Step 12: Combine historical and projected life expectancy
  tar_target(
    mortality_life_expectancy,
    {
      combined <- data.table::rbindlist(
        list(mortality_life_expectancy_hist, mortality_life_expectancy_proj),
        use.names = TRUE
      )
      data.table::setorder(combined, year, sex, age)
      combined
    }
  ),

  # ===========================================================================
  # MORTALITY VALIDATION TARGETS
  # ===========================================================================

  # Load TR2025 historical qx for validation
  tar_target(
    tr2025_qx_historical,
    load_tr2025_death_probs_hist(sex = "both")
  ),

  # Load TR2025 life tables for validation
  tar_target(
    tr2025_life_tables,
    load_tr2025_life_tables_hist(sex = "both")
  ),

  # Validate qx against TR2025
  tar_target(
    mortality_qx_validation,
    validate_qx_against_tr2025(
      qx_calculated = mortality_qx_historical,
      years = 2010:2019,
      tolerance = 0.05
    )
  ),

  # Validate life expectancy against TR2025
  tar_target(
    mortality_ex_validation,
    validate_life_expectancy_against_tr2025(
      life_table = mortality_life_tables,
      at_ages = c(0, 65),
      tolerance = 0.5  # 0.5 year tolerance
    )
  ),

  # ===========================================================================
  # MORTALITY MARITAL STATUS DIFFERENTIALS (TR2025 Section 1.2.c)
  # ===========================================================================

  # NCHS deaths by marital status (2015-2019)
  tar_target(
    nchs_deaths_by_marital,
    fetch_nchs_deaths_by_marital_status(years = 2015:2019),
    cue = tar_cue(mode = "thorough")
  ),

  # ACS PUMS population by marital status (2015-2019)
  tar_target(
    acs_pop_by_marital,
    fetch_acs_pums_marital_status(years = 2015:2019, ages = 0:99),
    cue = tar_cue(mode = "thorough")
  ),

  # Calculate marital mortality factors per TR2025 methodology
  tar_target(
    mortality_marital_factors,
    calculate_marital_mortality_factors(
      nchs_deaths = nchs_deaths_by_marital,
      acs_population = acs_pop_by_marital,
      reference_years = 2015:2019
    )
  ),

  # ===========================================================================
  # DATA ACQUISITION TARGETS - IMMIGRATION
  # ===========================================================================

  # DHS LPR immigration data (2006-2023)
  # Age-sex distributions for new arrivals and adjustments of status
  tar_target(
    dhs_lpr_data,
    {
      cache_file <- "data/cache/dhs_immigration/dhs_lpr_all_years.rds"
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        cli::cli_abort("DHS data not cached - run fetch_dhs_lpr_data_multi() first")
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # ===========================================================================
  # LPR IMMIGRATION SUBPROCESS TARGETS
  # ===========================================================================
  # Uses DHS data for age-sex distributions and NEW/AOS ratio, TR2025 for totals
  # Produces all 5 required outputs: L (LPR), NEW, AOS, E (Emigration), NL (Net LPR)

  # Step 1: Calculate LPR immigration distribution
  # Method controlled by config: "dhs" (default) or "tr2025_derived"
  tar_target(
    lpr_distribution,
    {
      method <- config_assumptions$immigration$lpr$distribution_method
      if (is.null(method)) method <- "dhs"

      if (method == "tr2025_derived") {
        # Derive distribution from TR2025 population projections
        tr_config <- config_assumptions$immigration$lpr$tr2025_derived_distribution
        get_immigration_distribution(
          method = "tr2025_derived",
          tr_config = tr_config
        )
      } else {
        # Use DHS-derived distribution (default)
        dhs_years <- config_assumptions$immigration$lpr$dhs_distribution$distribution_years
        if (is.null(dhs_years)) {
          dhs_years <- config_assumptions$immigration$lpr$distribution_years
        }
        get_immigration_distribution(
          method = "dhs",
          dhs_data = load_dhs_lpr_data(),
          dhs_years = dhs_years
        )
      }
    }
  ),

  # Step 2: Calculate emigration distribution from DHS data
  tar_target(
    emigration_distribution,
    calculate_emigration_distribution_dhs(
      dhs_data = load_dhs_lpr_data(),
      reference_years = config_assumptions$immigration$emigration$distribution_years
    )
  ),

  # Step 3: Calculate NEW/AOS ratio from DHS data
  tar_target(
    new_aos_ratio,
    calculate_new_aos_ratio(
      dhs_data = load_dhs_lpr_data(),
      reference_years = config_assumptions$immigration$lpr$new_aos_ratio_years
    )
  ),

  # Step 4: Get TR2025 LPR immigration assumptions
  tar_target(
    lpr_assumptions,
    get_tr2025_lpr_assumptions(
      years = config_assumptions$metadata$projection_period$start_year:
              config_assumptions$metadata$projection_period$end_year
    )
  ),

  # Step 5: Project total LPR immigration (L) by age and sex
  tar_target(
    lpr_immigration_projected,
    project_lpr_immigration(
      assumptions = lpr_assumptions,
      distribution = lpr_distribution
    )
  ),

  # Step 6: Split LPR into NEW and AOS
  # Method controlled by config: "assumption" (default, uses TR2025 V.A2 values)
  # or "ratio" (uses historical DHS ratio)
  tar_target(
    lpr_new_aos_split,
    {
      method <- config_assumptions$immigration$lpr$new_aos_split_method
      if (is.null(method)) method <- "assumption"
      if (method == "assumption") {
        split_lpr_new_aos(
          lpr_immigration = lpr_immigration_projected,
          assumptions = lpr_assumptions,
          method = "assumption"
        )
      } else {
        split_lpr_new_aos(
          lpr_immigration = lpr_immigration_projected,
          new_ratio = new_aos_ratio$new_ratio,
          method = "ratio"
        )
      }
    }
  ),

  # Extract NEW arrivals
  tar_target(
    new_arrivals_projected,
    lpr_new_aos_split$new_arrivals
  ),

  # Extract AOS
  tar_target(
    aos_projected,
    lpr_new_aos_split$aos
  ),

  # Step 7: Project legal emigration (E) by age and sex
  tar_target(
    legal_emigration_projected,
    project_legal_emigration(
      assumptions = lpr_assumptions,
      distribution = emigration_distribution
    )
  ),

  # Step 8: Calculate net LPR immigration (NL = L - E)
  tar_target(
    net_lpr_immigration,
    calculate_net_lpr(
      lpr_immigration = lpr_immigration_projected,
      emigration = legal_emigration_projected
    )
  ),

  # ===========================================================================
  # IMMIGRATION VALIDATION TARGETS
  # ===========================================================================

  # Comprehensive validation of all 5 LPR outputs
  tar_target(
    lpr_immigration_validation,
    {
      # Build projection result list for validation
      projection_result <- list(
        lpr_immigration = lpr_immigration_projected,
        new_arrivals = new_arrivals_projected,
        aos = aos_projected,
        emigration = legal_emigration_projected,
        net_lpr = net_lpr_immigration,
        distributions = list(
          lpr = lpr_distribution,
          emigration = emigration_distribution
        ),
        new_aos_ratio = new_aos_ratio,
        assumptions = lpr_assumptions
      )
      validate_lpr_outputs(projection_result, tolerance = 0.001)
    }
  ),

  # ===========================================================================
  # HISTORICAL POPULATION SUBPROCESS TARGETS (Phase 4)
  # ===========================================================================
  # Implements Equations 1.4.1 - 1.4.4 from TR2025 Documentation
  # Produces: P_x_s, P_x_s_m, O_x_s, C_x_s_m

  # Step 1: Calculate Historical Population by Age and Sex (Eq 1.4.1)
  # P^z_{x,s} = USAF + UC + TERR + FED + DEP + BEN + OTH
  tar_target(
    historical_population,
    calculate_historical_population(
      start_year = 1940,
      end_year = 2022,
      ages = 0:100,
      config = config_assumptions,
      use_cache = TRUE
    )
  ),

  # Step 2: Calculate Historical Population by Marital Status (Eq 1.4.2)
  # P^z_{x,s,m} = P^z_{x,s} × MaritalPct^z_{x,s,m}
  tar_target(
    historical_population_marital,
    calculate_historical_population_marital(
      total_pop = historical_population,
      start_year = 1940,
      end_year = 2022,
      ages = 14:100,
      use_cache = TRUE,
      include_same_sex = TRUE
    )
  ),

  # Step 3: Calculate Temporary/Unlawfully Present Population (Eq 1.4.3)
  # O^z_{x,s} from TR2025 V.A2 flows
  tar_target(
    historical_temp_unlawful,
    calculate_historical_temp_unlawful(
      start_year = 1940,
      end_year = 2022,
      ages = 0:99,
      use_cache = TRUE
    )
  ),

  # Step 4: Calculate Civilian Noninstitutionalized Population (Eq 1.4.4)
  # C^z_{x,s,m} = CivNonInst^z_{x,s} × MaritalPct^z_{x,s,m}
  tar_target(
    historical_civilian_noninst,
    calculate_historical_civilian_noninst(
      start_year = 2010,
      end_year = 2022,
      ages = 0:99,
      include_orientation = TRUE,
      use_cache = TRUE
    )
  ),

  # ===========================================================================
  # HISTORICAL POPULATION VALIDATION TARGETS
  # ===========================================================================

  # Load TR2025 December 31 population for validation
  tar_target(
    tr2025_population_dec,
    {
      file_path <- here::here("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")
      if (file.exists(file_path)) {
        data.table::fread(file_path)
      } else {
        cli::cli_warn("TR2025 population file not found: {file_path}")
        NULL
      }
    }
  ),

  # Validate historical population against TR2025
  tar_target(
    historical_population_validation,
    {
      if (is.null(tr2025_population_dec)) {
        cli::cli_warn("Skipping validation - TR2025 data not available")
        return(list(validated = FALSE, reason = "TR2025 data not available"))
      }

      # Calculate totals by year from our data
      calc_totals <- historical_population[, .(calculated = sum(population)), by = year]

      # Calculate totals from TR2025
      tr_totals <- tr2025_population_dec[, .(tr2025 = sum(Total)), by = Year]
      data.table::setnames(tr_totals, "Year", "year")

      # Merge and compare
      comparison <- merge(calc_totals, tr_totals, by = "year", all.x = TRUE)
      comparison[, diff_pct := (calculated - tr2025) / tr2025 * 100]

      # Summary statistics
      valid_rows <- comparison[!is.na(tr2025)]
      list(
        validated = TRUE,
        mean_abs_error = mean(abs(valid_rows$diff_pct)),
        max_abs_error = max(abs(valid_rows$diff_pct)),
        within_1pct = sum(abs(valid_rows$diff_pct) < 1),
        within_2pct = sum(abs(valid_rows$diff_pct) < 2),
        total_years = nrow(valid_rows),
        comparison = comparison
      )
    }
  ),

  # ===========================================================================
  # DATA ACQUISITION TARGETS - O IMMIGRATION (Phase 5)
  # ===========================================================================
  # Data for temporary/unlawfully present immigration projection

  # DHS Nonimmigrant stock data (3 reference dates)
  tar_target(
    dhs_nonimmigrant_stock,
    {
      cache_file <- "data/cache/dhs/nonimmigrant_stock_age_sex.rds"
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_dhs_nonimmigrant_stock()
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # DHS Beginning-of-year nonimmigrant totals
  tar_target(
    dhs_boy_nonimmigrants,
    {
      cache_file <- "data/cache/dhs/boy_nonimmigrants.rds"
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_dhs_boy_nonimmigrants()
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # DHS DACA grants and stock
  tar_target(
    dhs_daca_data,
    {
      grants_file <- "data/cache/dhs/daca_grants.rds"
      stock_file <- "data/cache/dhs/daca_stock.rds"
      if (file.exists(grants_file) && file.exists(stock_file)) {
        list(
          grants = readRDS(grants_file),
          stock = readRDS(stock_file)
        )
      } else {
        list(
          grants = fetch_dhs_daca_grants(),
          stock = fetch_dhs_daca_stock()
        )
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # ACS Foreign-born new arrivals (2006-2023)
  # Load cached foreign-born flows data
  tar_target(
    acs_foreign_born_flows,
    {
      cache_dir <- "data/cache/acs_pums"
      years <- c(2006:2019, 2021:2023)  # Skip 2020 (COVID data issues)

      data.table::rbindlist(lapply(years, function(yr) {
        cache_file <- file.path(cache_dir, sprintf("foreign_born_flows_%d.rds", yr))
        if (file.exists(cache_file)) {
          readRDS(cache_file)
        } else {
          NULL
        }
      }), fill = TRUE)
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Calculate new arrivals from foreign-born flows
  tar_target(
    acs_foreign_born_arrivals,
    {
      arrivals <- calculate_acs_new_arrivals(
        foreign_born_flows = acs_foreign_born_flows,
        entry_window = 1
      )
      # Aggregate to remove is_cuban dimension (sum Cuban and non-Cuban)
      arrivals[, .(population = sum(population)), by = .(year, age, sex)]
    }
  ),

  # ACS undercount factors for O immigration
  tar_target(
    acs_undercount_factors,
    calculate_acs_undercount_factors(
      years = 2017:2019,
      use_fallback = TRUE  # Use DHS hardcoded factors
    )
  ),

  # ===========================================================================
  # O IMMIGRATION SUBPROCESS TARGETS (Phase 5)
  # ===========================================================================
  # Implements Equations 1.5.1 - 1.5.4 from TR2025 Documentation
  # Produces: OI (O Immigration), OE (O Emigration), NO (Net O), OP (O Population)

  # Run full O immigration projection (Equations 1.5.1-1.5.4)
  # This orchestrates OI, OE, NO, and OP calculations
  tar_target(
    o_immigration_projection,
    run_full_o_projection(
      historical_o_pop = historical_temp_unlawful,
      acs_new_arrivals = acs_foreign_born_arrivals,
      lpr_new_arrivals = new_arrivals_projected,
      lpr_aos = aos_projected,
      mortality_qx = mortality_qx_historical,
      undercount_factors = acs_undercount_factors,
      projection_years = config_assumptions$metadata$projection_period$start_year:
                         config_assumptions$metadata$projection_period$end_year,
      config = config_assumptions
    )
  ),

  # Extract individual outputs from projection
  tar_target(
    o_immigration_oi,
    o_immigration_projection$o_immigration
  ),

  tar_target(
    o_emigration_oe,
    o_immigration_projection$o_emigration
  ),

  tar_target(
    net_o_immigration,
    o_immigration_projection$net_o
  ),

  tar_target(
    o_population_stock,
    o_immigration_projection$o_population
  ),

  # ===========================================================================
  # DACA PROJECTION TARGETS (Phase 5F)
  # ===========================================================================

  # Run DACA population projection
  tar_target(
    daca_projection,
    run_daca_projection(
      acs_2012_data = NULL,  # Use internal estimates
      dhs_stock = dhs_daca_data$stock,
      dhs_grants = dhs_daca_data$grants,
      projection_years = config_assumptions$metadata$projection_period$start_year:
                         config_assumptions$metadata$projection_period$end_year,
      config = config_assumptions
    )
  ),

  # Extract DACA population
  tar_target(
    daca_population_projected,
    daca_projection$daca_population
  ),

  # ===========================================================================
  # O IMMIGRATION VALIDATION TARGETS (Phase 5G)
  # ===========================================================================

  # Comprehensive validation of O immigration outputs
  tar_target(
    o_immigration_validation,
    validate_o_immigration_comprehensive(
      projection = o_immigration_projection,
      daca_projection = daca_projection,
      tolerance_strict = 0.001,
      tolerance_relaxed = 0.15
    )
  ),

  # ===========================================================================
  # DATA ACQUISITION TARGETS - MARRIAGE (Phase 6)
  # ===========================================================================

  # NCHS MRA marriages 1978-1988 (for base MarGrid)
  tar_target(
    nchs_mra_marriages_1978_1988,
    {
      cache_file <- here::here("data/cache/nber_marriage/nchs_mra_marriages_1978_1988.rds")
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_nchs_mra_marriages_1978_1988()
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # CPS unmarried population (1962-1995)
  tar_target(
    cps_unmarried_population,
    {
      cache_file <- here::here("data/cache/ipums_cps/cps_unmarried_1957_1995.rds")
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        cli::cli_abort("CPS unmarried data not cached - run fetch_cps_unmarried_population() first")
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # NCHS MRA subset marriages 1989-1995
  tar_target(
    nchs_mra_subset_1989_1995,
    {
      cache_file <- here::here("data/cache/nber_marriage/nchs_mra_marriages_1989_1995.rds")
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_nchs_mra_marriages_1989_1995()
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # NCHS U.S. total marriages 1989-2022
  tar_target(
    nchs_us_total_marriages,
    fetch_nchs_us_total_marriages(years = 1989:2022)
  ),

  # ACS new marriage grids 2007-2022 (aligned to MarGrid dimensions)
  tar_target(
    acs_marriage_grids,
    {
      cache_dir <- here::here("data/cache/acs_marriage")
      acs_files <- list.files(cache_dir, pattern = "^new_marriages_20[0-9]{2}\\.rds$", full.names = TRUE)

      if (length(acs_files) == 0) {
        cli::cli_abort("ACS marriage grids not cached - run fetch_acs_new_marriages() first")
      }

      aligned_grids <- list()
      for (f in acs_files) {
        yr <- as.integer(gsub(".*new_marriages_([0-9]{4})\\.rds", "\\1", f))
        data <- readRDS(f)
        acs_grid <- data$grid
        acs_ages <- as.integer(rownames(acs_grid))

        # Align to MarGrid dimensions (87×87, ages 14-100)
        aligned <- matrix(0, nrow = 87, ncol = 87,
                          dimnames = list(14:100, 14:100))

        for (i in seq_along(acs_ages)) {
          h_age <- acs_ages[i]
          if (h_age < 14 || h_age > 100) next
          h_idx <- h_age - 14 + 1

          for (j in seq_along(acs_ages)) {
            w_age <- acs_ages[j]
            if (w_age < 14 || w_age > 100) next
            w_idx <- w_age - 14 + 1
            aligned[h_idx, w_idx] <- acs_grid[i, j]
          }
        }
        aligned_grids[[as.character(yr)]] <- aligned
      }
      aligned_grids
    },
    cue = tar_cue(mode = "thorough")
  ),

  # 2010 standard population by age group
  tar_target(
    standard_pop_2010,
    {
      cache_file <- here::here("data/cache/acs_pums/marital_all_years.rds")
      if (file.exists(cache_file)) {
        get_2010_standard_population(cache_file = cache_file, by_age_group = TRUE)
      } else {
        cli::cli_abort("Standard population cache not found - run Phase 4 data acquisition first")
      }
    }
  ),

  # NCHS marriages by prior status 1978-1988
  tar_target(
    nchs_prior_status_1978_1988,
    {
      cache_file <- here::here("data/cache/nber_marriage/nchs_marriages_by_prior_status_1978_1988.rds")
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_nchs_marriages_by_prior_status_1978_1988()
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # ACS same-sex marriage grids 2015-2022
  tar_target(
    acs_same_sex_grids,
    fetch_acs_same_sex_grids(years = 2015:2022),
    cue = tar_cue(mode = "thorough")
  ),

  # Calculate same-sex fraction from ACS data
  tar_target(
    same_sex_fraction,
    {
      ss_result <- calculate_same_sex_fraction(
        same_sex_grids = acs_same_sex_grids,
        opposite_sex_grids = acs_marriage_grids,
        years = 2015:2019
      )
      ss_result$overall_fraction
    }
  ),

  # ===========================================================================
  # MARRIAGE SUBPROCESS TARGETS (Phase 6)
  # ===========================================================================
  # Implements Equations 1.6.1 - 1.6.2 from TR2025 Documentation
  # Produces: m̂_{x,y}^z (age-specific rates), AMR^z (age-adjusted rate)

  # Run complete marriage projection
  tar_target(
    marriage_projection,
    run_marriage_projection(
      nchs_marriages_1978_1988 = nchs_mra_marriages_1978_1988,
      cps_unmarried = cps_unmarried_population,
      nchs_subset = nchs_mra_subset_1989_1995,
      acs_grids = acs_marriage_grids,
      nchs_us_totals = nchs_us_total_marriages,
      standard_pop_by_group = standard_pop_2010,
      prior_status_data = nchs_prior_status_1978_1988,
      same_sex_data = acs_same_sex_grids,
      same_sex_fraction = same_sex_fraction,
      ultimate_amr = config_assumptions$marriage$ultimate_amr,
      ultimate_year = config_assumptions$marriage$ultimate_year,
      end_year = config_assumptions$metadata$projection_period$end_year,
      include_same_sex = TRUE,
      include_prior_status = TRUE,
      force_recompute = FALSE
    )
  ),

  # Extract marriage rates (all years)
  tar_target(
    marriage_rates_all,
    marriage_projection$all_rates
  ),

  # Extract historical AMR
  tar_target(
    marriage_amr_historical,
    marriage_projection$amr_historical
  ),

  # Extract projected AMR
  tar_target(
    marriage_amr_projected,
    marriage_projection$amr_projected
  ),

  # Extract opposite-sex rates
  tar_target(
    marriage_rates_opposite_sex,
    marriage_projection$opposite_sex_rates
  ),

  # Extract same-sex rates
  tar_target(
    marriage_rates_same_sex,
    marriage_projection$same_sex_rates
  ),

  # Extract prior status differentials
  tar_target(
    marriage_status_differentials,
    marriage_projection$status_differentials
  ),

  # Extract base MarGrid
  tar_target(
    marriage_margrid,
    marriage_projection$margrid
  ),

  # ===========================================================================
  # MARRIAGE VALIDATION TARGETS (Phase 6H)
  # ===========================================================================

  # Comprehensive marriage projection validation
  tar_target(
    marriage_validation,
    validate_marriage_comprehensive(
      projection = marriage_projection,
      tolerance_amr = 0.001,
      tolerance_totals = 0.05
    )
  ),

  # Quick marriage validation (for faster iteration)
  tar_target(
    marriage_validation_quick,
    validate_marriage_quick(projection = marriage_projection)
  ),

  # ===========================================================================
  # DIVORCE SUBPROCESS TARGETS (Phase 7)
  # ===========================================================================
  # Implements Equations 1.7.1 - 1.7.2 from TR2025 Documentation
  # Produces: d̂_{x,y}^z (age-specific rates), ADR^z (age-adjusted rate)

  # Run complete divorce projection
  tar_target(
    divorce_projection,
    run_divorce_projection(
      cache_dir = here::here("data/cache"),
      ultimate_adr = config_assumptions$divorce$ultimate_adr,
      ultimate_year = config_assumptions$divorce$ultimate_year,
      end_year = config_assumptions$metadata$projection_period$end_year,
      force = FALSE
    )
  ),

  # Extract projected ADR series (2023-2099)
  tar_target(
    divorce_adr_projected,
    divorce_projection$projected_adr
  ),

  # Extract historical ADR series (1989-2022)
  tar_target(
    divorce_adr_historical,
    divorce_projection$historical$adr_series
  ),

  # Extract complete ADR series (historical + projected)
  tar_target(
    divorce_adr_complete,
    divorce_projection$complete_adr
  ),

  # Extract projected divorce rates (DivGrid by year)
  tar_target(
    divorce_rates_projected,
    divorce_projection$projected_rates
  ),

  # Extract base DivGrid (1979-1988 average)
  tar_target(
    divorce_divgrid_base,
    divorce_projection$historical$base_result$divgrid
  ),

  # Extract adjusted DivGrid (ACS-adjusted)
  tar_target(
    divorce_divgrid_adjusted,
    divorce_projection$historical$adjusted_result$adjusted_divgrid
  ),

  # Extract standard married population (July 1, 2010)
  tar_target(
    divorce_standard_pop,
    divorce_projection$standard_pop
  ),

  # ===========================================================================
  # DIVORCE VALIDATION TARGETS (Phase 7H)
  # ===========================================================================

  # Comprehensive divorce projection validation
  tar_target(
    divorce_validation,
    validate_divorce_comprehensive(
      projection = divorce_projection,
      tolerance_adr = 0.001
    )
  ),

  # Quick divorce validation (for faster iteration)
  tar_target(
    divorce_validation_quick,
    validate_divorce_quick(projection = divorce_projection)
  ),

  # ===========================================================================
  # PROJECTED POPULATION SUBPROCESS - PHASE 8A (Data Assembly)
  # ===========================================================================
  # Implements Section 1.8 from TR2025 Documentation
  # Phase 8A: Verify all inputs and extract starting population

  # CPS children per couple data (Item 29)
  # Note: This requires an IPUMS extract. Run submit_cps_children_extract() first
  # and check status with get_cps_children_extract_status()
  tar_target(
    cps_children_per_couple,
    {
      cache_file <- here::here("data/cache/ipums_cps/cps_children_per_couple_1962_2022.rds")
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        cli::cli_alert_warning("CPS children data not cached")
        cli::cli_alert_info("Run submit_cps_children_extract() to download data")
        NULL
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Verify all projection inputs (Phase 8A.9)
  tar_target(
    projection_inputs_verification,
    {
      # Combine historical and projected qx for mortality input
      # Select only common columns to ensure rbindlist works
      common_cols <- c("year", "age", "sex", "qx")
      hist_qx <- mortality_qx_historical[, ..common_cols]

      # Handle case where projected qx may not exist yet
      if (exists("mortality_qx_projected") && !is.null(mortality_qx_projected)) {
        proj_qx <- mortality_qx_projected[, ..common_cols]
        mortality_qx_combined <- data.table::rbindlist(
          list(hist_qx, proj_qx),
          use.names = TRUE
        )
      } else {
        # Use only historical for now
        mortality_qx_combined <- hist_qx
      }

      verify_all_projection_inputs(
        fertility_rates = fertility_rates_complete,
        mortality_qx = mortality_qx_combined,
        mortality_differentials = mortality_marital_factors,
        net_lpr = net_lpr_immigration,
        net_o = net_o_immigration,
        marriage_rates = marriage_projection,
        divorce_rates = divorce_projection,
        historical_population = historical_population,
        historical_marital = historical_population_marital,
        projection_years = config_assumptions$metadata$projection_period$start_year:
                           config_assumptions$metadata$projection_period$end_year,
        starting_year = config_assumptions$projected_population$starting_year
      )
    }
  ),

  # Extract starting population (Phase 8A.7)
  tar_target(
    starting_population,
    {
      extract_starting_population(
        historical_population = historical_population,
        historical_marital = historical_population_marital,
        starting_year = config_assumptions$projected_population$starting_year
      )
    }
  ),

  # ===========================================================================
  # PHASE 8B: CORE POPULATION PROJECTION (Equations 1.8.1-1.8.4)
  # ===========================================================================

  # Combined mortality qx (historical + projected) for population projection
  tar_target(
    mortality_qx_for_projection,
    {
      # Select common columns
      common_cols <- c("year", "age", "sex", "qx")
      hist_qx <- mortality_qx_historical[, ..common_cols]
      proj_qx <- mortality_qx_projected[, ..common_cols]

      data.table::rbindlist(
        list(hist_qx, proj_qx),
        use.names = TRUE
      )
    }
  ),

  # Net O immigration data (extracted from projection result)
  tar_target(
    net_o_for_projection,
    {
      o_immigration_projection$net_o_immigration
    }
  ),

  # Run population projection (Equations 1.8.1-1.8.4)
  tar_target(
    population_projection,
    {
      run_population_projection(
        starting_population = starting_population$population,
        birth_rates = fertility_rates_complete,
        mortality_qx = mortality_qx_for_projection,
        net_lpr = net_lpr_immigration,
        net_o = net_o_for_projection,
        start_year = config_assumptions$projected_population$starting_year,
        end_year = config_assumptions$projected_population$projection_end,
        config = list(
          sex_ratio_at_birth = config_assumptions$projected_population$sex_ratio_at_birth,
          population_status = config_assumptions$projected_population$population_status,
          ages = config_assumptions$projected_population$ages
        ),
        verbose = TRUE
      )
    }
  ),

  # Extract projected population by year for downstream use
  tar_target(
    projected_population,
    {
      population_projection$population
    }
  ),

  # Projected births
  tar_target(
    projected_births,
    {
      population_projection$births
    }
  ),

  # Projected deaths
  tar_target(
    projected_deaths,
    {
      population_projection$deaths
    }
  ),

  # Projected net immigration
  tar_target(
    projected_net_immigration,
    {
      population_projection$net_immigration
    }
  ),

  # Population projection summary
  tar_target(
    population_projection_summary,
    {
      population_projection$summary
    }
  ),

  # ===========================================================================
  # PHASE 8C: MARITAL STATUS DISAGGREGATION (Equation 1.8.5)
  # ===========================================================================
  # Disaggregates Phase 8B population totals into marital statuses (single,
  # married, divorced, widowed). The sum of marital status populations equals
  # Phase 8B totals exactly - this is a disaggregation, not independent projection.

  # Extract starting marital population (Dec 31 of starting year)
  tar_target(
    starting_marital_pop,
    {
      cli::cli_h2("Extracting Starting Marital Population")
      starting_year <- config_assumptions$projected_population$starting_year

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
  ),

  # Run marital status projection (Phase 8C main)
  tar_target(
    marital_projection,
    {
      run_marital_projection(
        phase8b_result = population_projection,
        starting_marital = starting_marital_pop,
        mortality_differentials = mortality_marital_factors,
        marriage_rates = marriage_projection,
        divorce_rates = divorce_projection,
        mortality_qx = mortality_qx_for_projection,
        start_year = config_assumptions$projected_population$starting_year,
        end_year = config_assumptions$projected_population$projection_end,
        min_age = config_assumptions$projected_population$ages$marriage_min,
        max_age = config_assumptions$projected_population$ages$max_age_group,
        verbose = TRUE
      )
    }
  ),

  # Extract projected marital population
  tar_target(
    projected_marital_population,
    {
      marital_projection$marital_population
    }
  ),

  # Extract marital projection summary
  tar_target(
    marital_projection_summary,
    {
      marital_projection$summary
    }
  ),

  # Phase 8C validation: Verify sum of marital statuses equals Phase 8B totals
  tar_target(
    marital_validation,
    {
      cli::cli_h2("Phase 8C Validation: Marital Status Totals")

      marital_pop <- marital_projection$marital_population
      phase8b_pop <- population_projection$population

      # Aggregate marital by year, age, sex
      marital_totals <- marital_pop[, .(marital_total = sum(population, na.rm = TRUE)),
                                     by = .(year, age, sex)]

      # Aggregate Phase 8B by year, age, sex (ages 14-100)
      min_age <- config_assumptions$projected_population$ages$marriage_min
      max_age <- config_assumptions$projected_population$ages$max_age_group

      phase8b_totals <- phase8b_pop[age >= min_age & age <= max_age,
                                     .(phase8b_total = sum(population, na.rm = TRUE)),
                                     by = .(year, age, sex)]

      # Merge and compare
      comparison <- merge(marital_totals, phase8b_totals,
                          by = c("year", "age", "sex"), all = TRUE)

      comparison[is.na(marital_total), marital_total := 0]
      comparison[is.na(phase8b_total), phase8b_total := 0]

      comparison[, diff := abs(marital_total - phase8b_total)]
      comparison[phase8b_total > 0, pct_diff := diff / phase8b_total * 100]

      # Summary statistics
      max_pct_diff <- comparison[, max(pct_diff, na.rm = TRUE)]
      mean_pct_diff <- comparison[, mean(pct_diff, na.rm = TRUE)]
      n_large_diff <- comparison[pct_diff > 1, .N]

      cli::cli_alert_info("Max percentage difference: {round(max_pct_diff, 4)}%")
      cli::cli_alert_info("Mean percentage difference: {round(mean_pct_diff, 6)}%")

      # Validation passes if difference is essentially zero (within floating point tolerance)
      tolerance <- 0.01  # 0.01% tolerance

      if (max_pct_diff < tolerance) {
        cli::cli_alert_success("PASS: Marital totals match Phase 8B totals (max diff < {tolerance}%)")
        valid <- TRUE
      } else {
        cli::cli_alert_warning("WARNING: Some differences > {tolerance}% ({n_large_diff} cells)")
        valid <- FALSE
      }

      # Check married population trajectory
      married_summary <- marital_pop[marital_status == "married",
                                      .(married = sum(population, na.rm = TRUE) / 1e6),
                                      by = year]

      years <- c(min(married_summary$year), 2050, max(married_summary$year))
      for (yr in years) {
        married_val <- married_summary[year == yr, married]
        if (length(married_val) > 0) {
          cli::cli_alert_info("Married population {yr}: {format(round(married_val, 2), nsmall=2)}M")
        }
      }

      list(
        valid = valid,
        max_pct_diff = max_pct_diff,
        mean_pct_diff = mean_pct_diff,
        comparison = comparison,
        married_trajectory = married_summary
      )
    }
  ),

  # ===========================================================================
  # PLACEHOLDER: Future process targets will be added here
  # ===========================================================================
  # - Projected population children targets (Phase 8D)
  # - Projected population CNI targets (Phase 8E)
  # - Economics process targets
  # - Beneficiaries process targets
  # - Trust fund operations targets

  NULL
)

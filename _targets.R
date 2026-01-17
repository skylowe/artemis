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
    fetch_census_population_all(
      years = config_assumptions$data_sources$population_estimates$start_year:
              config_assumptions$data_sources$population_estimates$end_year,
      ages = 10:54,  # Wider range to capture all fertility ages after collapse
      sex = "female"
    ),
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

  # Step 1: Calculate historical birth rates (1980-2024)
  tar_target(
    fertility_rates_historical,
    calculate_historical_birth_rates(
      births = nchs_births_raw,
      population = census_female_pop,
      min_age = config_assumptions$fertility$min_fertility_age,
      max_age = config_assumptions$fertility$max_fertility_age
    )
  ),

  # Step 2: Calculate age-to-30 ratios
  tar_target(
    fertility_age30_ratios,
    calculate_age30_ratios(fertility_rates_historical)
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
      base_age30_rate = fertility_rates_historical[year == config_assumptions$fertility$rate_base_year & age == 30, birth_rate],
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
      base_rate = fertility_rates_historical[year == config_assumptions$fertility$rate_base_year & age == 30, birth_rate],
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
      ultimate_years = fertility_ultimate_years
    )
  ),

  # Combined historical and projected rates (1980-2099)
  tar_target(
    fertility_rates_complete,
    rbind(
      fertility_rates_historical[, .(year, age, birth_rate)],
      fertility_rates_projected
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

  # Census population for mortality calculations (both sexes, ages 0-100)
  tar_target(
    census_population_both,
    {
      cache_file <- "data/cache/census/population_by_age_sex_1980_2023.rds"
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_census_population_all(
          years = 1980:2023,
          ages = 0:100,
          sex = "both"
        )
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Total births by year for q0 calculation
  tar_target(
    nchs_births_total,
    {
      years <- 1968:2023
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
  tar_target(
    mortality_mx_projected,
    run_mortality_projection(
      deaths = nchs_deaths_raw,
      population = census_population_both,
      base_year = config_assumptions$mortality$base_year,
      projection_end = config_assumptions$metadata$projection_period$end_year,
      by_cause = FALSE
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

      # Calculate qx for ages 1+ from mx
      qx_from_mx <- convert_mx_to_qx(mx_filtered, max_age = 119)

      # Calculate q0 using deaths/births (also filter to pop_years)
      infant_deaths <- nchs_deaths_raw[age == 0 & year %in% pop_years,
                                       .(deaths = sum(deaths)), by = .(year, sex)]
      q0 <- merge(infant_deaths, nchs_births_total, by = "year", all.x = TRUE)
      q0 <- q0[!is.na(births)]  # Only years with births data
      # Use 51.2% male assumption until sex-specific births available
      q0[sex == "male", births_sex := births * 0.512]
      q0[sex == "female", births_sex := births * 0.488]
      q0[, qx := deaths / births_sex]
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
      max_age = 119
    )
  ),

  # Step 7: Calculate life tables from qx
  tar_target(
    mortality_life_tables,
    calculate_life_table(
      qx = mortality_qx_historical,
      radix = 100000,
      max_age = 119
    )
  ),

  # Step 8: Calculate life expectancy at key ages
  tar_target(
    mortality_life_expectancy,
    calculate_life_expectancy(
      life_table = mortality_life_tables,
      at_ages = c(0, 65)
    )
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
  # PLACEHOLDER: Future process targets will be added here
  # ===========================================================================
  # - Immigration subprocess targets
  # - Historical population targets
  # - Marriage/Divorce targets
  # - Projected population targets
  # - Economics process targets
  # - Beneficiaries process targets
  # - Trust fund operations targets

  NULL
)

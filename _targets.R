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
  # PLACEHOLDER: Future process targets will be added here
  # ===========================================================================
  # - Mortality subprocess targets
  # - Immigration subprocess targets
  # - Historical population targets
  # - Marriage/Divorce targets
  # - Projected population targets
  # - Economics process targets
  # - Beneficiaries process targets
  # - Trust fund operations targets

  NULL
)

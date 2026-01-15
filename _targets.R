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
    "here",
    "glue",
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

  # Birth data from NCHS/CDC (1980-2023)
  # tar_target(
  #   nchs_births_raw,
  #   fetch_nchs_births(
  #     years = config_assumptions$data_sources$historical_birth_data$start_year:
  #             config_assumptions$data_sources$historical_birth_data$end_year,
  #     config = config_api

  #   ),
  #   cue = tar_cue(mode = "thorough")
  # ),

  # Census Bureau female population estimates (1980-2024)
  # tar_target(
  #   census_female_pop,
  #   fetch_census_population(
  #     years = config_assumptions$data_sources$population_estimates$start_year:
  #             config_assumptions$data_sources$population_estimates$end_year,
  #     ages = config_assumptions$fertility$min_fertility_age:
  #            config_assumptions$fertility$max_fertility_age,
  #     sex = "female",
  #     config = config_api
  #   ),
  #   cue = tar_cue(mode = "thorough")
  # ),

  # Historical birth rates (1917-1979)
  # tar_target(
  #   historical_birth_rates,
  #   load_historical_birth_rates(
  #     start_year = config_assumptions$data_sources$historical_rate_data$start_year,
  #     end_year = config_assumptions$data_sources$historical_rate_data$end_year
  #   )
  # ),

  # Provisional birth data (2024)
  # tar_target(
  #   provisional_births_2024,
  #   fetch_provisional_births(year = 2024, config = config_api)
  # ),

  # ===========================================================================
  # FERTILITY SUBPROCESS TARGETS
  # ===========================================================================

  # Historical birth rates calculation (1941-2023)
  # tar_target(
  #   fertility_rates_historical,
  #   calculate_historical_birth_rates(
  #     births = nchs_births_raw,
  #     population = census_female_pop,
  #     historical_rates = historical_birth_rates
  #   )
  # ),

  # Estimated 2024 rates
  # tar_target(
  #   fertility_rates_2024,
  #   estimate_current_year_rates(
  #     provisional_rates = provisional_births_2024,
  #     prior_year_rates = fertility_rates_historical[year == 2023],
  #     tfr_estimate = config_assumptions$fertility$tfr_estimate_2024
  #   )
  # ),

  # Combined historical rates through present
  # tar_target(
  #   fertility_rates_to_present,
  #   rbind(fertility_rates_historical, fertility_rates_2024)
  # ),

  # Age-30 ratios
  # tar_target(
  #   fertility_age30_ratios,
  #   calculate_age30_ratios(fertility_rates_to_present)
  # ),

  # Trend factors
  # tar_target(
  #   fertility_trend_factors,
  #   calculate_trend_factors(
  #     ratios = fertility_age30_ratios,
  #     exclude_years = config_assumptions$fertility$exclude_years
  #   )
  # ),

  # Ultimate years by age
  # tar_target(
  #   fertility_ultimate_years,
  #   calculate_ultimate_years(config_assumptions$fertility)
  # ),

  # Interpolation weights
  # tar_target(
  #   fertility_weights,
  #   calculate_interpolation_weights(
  #     years = config_assumptions$metadata$projection_period$start_year:
  #             config_assumptions$metadata$projection_period$end_year,
  #     config = config_assumptions$fertility
  #   )
  # ),

  # Solve for ultimate age-30 rate
  # tar_target(
  #   fertility_ultimate_age30,
  #   solve_ultimate_age30_rate(
  #     target_ctfr = config_assumptions$fertility$ultimate_ctfr,
  #     base_age30_rate = fertility_rates_to_present[year == 2024 & age == 30, birth_rate],
  #     trend_factors = fertility_trend_factors,
  #     ultimate_years = fertility_ultimate_years,
  #     weights = fertility_weights
  #   )
  # ),

  # Project age-30 rates
  # tar_target(
  #   fertility_age30_projected,
  #   project_age30_rates(
  #     years = config_assumptions$metadata$projection_period$start_year:
  #             config_assumptions$metadata$projection_period$end_year,
  #     base_rate = fertility_rates_to_present[year == 2024 & age == 30, birth_rate],
  #     ultimate_rate = fertility_ultimate_age30,
  #     weights = fertility_weights
  #   )
  # ),

  # Project all age rates
  # tar_target(
  #   fertility_rates_projected,
  #   project_birth_rates(
  #     years = config_assumptions$metadata$projection_period$start_year:
  #             config_assumptions$metadata$projection_period$end_year,
  #     age30_rates = fertility_age30_projected,
  #     base_ratios = fertility_age30_ratios[year == 2024],
  #     trend_factors = fertility_trend_factors,
  #     ultimate_years = fertility_ultimate_years
  #   )
  # ),

  # Combined historical and projected rates
  # tar_target(
  #   fertility_rates_complete,
  #   rbind(fertility_rates_to_present, fertility_rates_projected)
  # ),

  # TFR and CTFR series
  # tar_target(
  #   fertility_totals,
  #   calculate_fertility_totals(fertility_rates_complete)
  # ),

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

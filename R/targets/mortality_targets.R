#' Mortality Targets
#'
#' @description
#' Factory function that creates targets for the mortality subprocess (Section 1.2).
#'
#' @name mortality_targets
NULL

#' Create mortality targets
#'
#' @description
#' Creates targets for mortality data acquisition, projection, and validation.
#' Includes qx calculation, life tables, and marital mortality differentials.
#'
#' @return List of targets for mortality subprocess
#'
#' @export
create_mortality_targets <- function() {
  list(
    # ==========================================================================
    # DATA ACQUISITION TARGETS
    # ==========================================================================

    # NCHS Deaths data (1968-2023)
    targets::tar_target(
      nchs_deaths_raw,
      fetch_nchs_deaths_multi(years = 1968:2023),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Census population for mortality calculations
    targets::tar_target(
      census_population_both,
      {
        vintage <- set_census_vintage_option(config_assumptions)
        cache_file <- sprintf("data/cache/census/population_by_age_sex_1980_2023_v%d_bysex.rds", vintage)
        load_or_fetch(
          cache_file = cache_file,
          fetch_fn = function() {
            male_pop <- fetch_census_population_all(years = 1980:2023, ages = 0:100, sex = "male")
            female_pop <- fetch_census_population_all(years = 1980:2023, ages = 0:100, sex = "female")
            data.table::rbindlist(list(male_pop, female_pop), use.names = TRUE)
          },
          save_cache = TRUE
        )
      },
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Total births by year for q0 calculation
    targets::tar_target(
      nchs_births_total,
      load_total_births_for_q0(years = 1968:2024),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Sex-specific births by year
    targets::tar_target(
      nchs_births_by_sex,
      {
        raw_data <- load_cached_multi_year(
          years = 1968:2024,
          cache_pattern = "data/raw/nchs/births_by_month_sex_%d.rds",
          on_missing = "skip"
        )
        raw_data[, .(births = sum(births)), by = .(year, sex)]
      },
      cue = targets::tar_cue(mode = "thorough")
    ),

    # ==========================================================================
    # MORTALITY SUBPROCESS TARGETS
    # ==========================================================================

    # Step 1: Calculate historical mx
    targets::tar_target(
      mortality_mx_historical,
      calculate_central_death_rates(
        deaths = nchs_deaths_raw,
        population = census_population_both,
        by_cause = TRUE
      )
    ),

    # Step 2: Calculate total mx
    targets::tar_target(
      mortality_mx_total,
      calculate_total_death_rates(mortality_mx_historical)
    ),

    # Step 3: Calculate historical AAx
    targets::tar_target(
      mortality_aax_historical,
      calculate_annual_reduction_rates(
        mx = mortality_mx_historical,
        start_year = config_assumptions$mortality$regression_start_year,
        end_year = config_assumptions$mortality$regression_end_year,
        by_cause = TRUE
      )
    ),

    # Step 4: Get starting mx values
    targets::tar_target(
      mortality_mx_starting,
      calculate_starting_mx(
        aax_results = mortality_aax_historical,
        base_year = config_assumptions$mortality$base_year
      )
    ),

    # Step 5: Run full mortality projection
    targets::tar_target(
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

    # Step 6: Calculate qx from mx
    targets::tar_target(
      mortality_qx_unadjusted,
      calculate_qx_with_infant_mortality(
        mx_total = mortality_mx_total,
        deaths_raw = nchs_deaths_raw,
        births_by_sex = nchs_births_by_sex,
        population = census_population_both,
        max_age = 100
      )
    ),

    # Step 6b: Adjust qx for ages 85+ using HMD
    targets::tar_target(
      mortality_qx_historical,
      adjust_qx_with_hmd(
        qx = mortality_qx_unadjusted,
        transition_age = 85,
        max_age = 100
      )
    ),

    # Step 7: Calculate life tables
    targets::tar_target(
      mortality_life_tables,
      calculate_life_table(
        qx = mortality_qx_historical,
        radix = 100000,
        max_age = 100
      )
    ),

    # Step 8: Calculate historical life expectancy
    targets::tar_target(
      mortality_life_expectancy_hist,
      calculate_life_expectancy(
        life_table = mortality_life_tables,
        at_ages = c(0, 65)
      )
    ),

    # ==========================================================================
    # PROJECTED MORTALITY TARGETS
    # ==========================================================================

    # Step 9: Extract projected qx
    targets::tar_target(
      mortality_qx_projected,
      {
        qx_proj <- mortality_mx_projected$projected_qx
        method <- get_config_with_default(
          config_assumptions, "mortality", "starting_aax_method",
          default = "regression"
        )
        if (method == "tr_qx") {
          cli::cli_alert_info("Skipping HMD calibration for tr_qx method")
          qx_proj <- qx_proj[age <= 100]
          return(qx_proj)
        }
        adjust_qx_with_hmd(qx = qx_proj, transition_age = 85, max_age = 100)
      }
    ),

    # Step 10: Calculate projected life tables
    targets::tar_target(
      mortality_life_tables_projected,
      calculate_life_table(
        qx = mortality_qx_projected,
        radix = 100000,
        max_age = 100
      )
    ),

    # Step 11: Calculate projected life expectancy
    targets::tar_target(
      mortality_life_expectancy_proj,
      calculate_life_expectancy(
        life_table = mortality_life_tables_projected,
        at_ages = c(0, 65)
      )
    ),

    # Step 12: Combine life expectancy
    targets::tar_target(
      mortality_life_expectancy,
      combine_life_expectancy_series(
        historical = mortality_life_expectancy_hist,
        projected = mortality_life_expectancy_proj
      )
    ),

    # ==========================================================================
    # VALIDATION TARGETS
    # ==========================================================================

    # TR2025 historical qx for validation
    targets::tar_target(
      tr2025_qx_historical,
      load_tr_death_probs_hist(sex = "both")
    ),

    # TR2025 life tables for validation
    targets::tar_target(
      tr2025_life_tables,
      load_tr_life_tables_hist(sex = "both")
    ),

    # Validate qx against TR2025
    targets::tar_target(
      mortality_qx_validation,
      validate_qx_against_tr(
        qx_calculated = mortality_qx_historical,
        years = 2010:2019,
        tolerance = 0.05
      )
    ),

    # Validate life expectancy against TR2025
    targets::tar_target(
      mortality_ex_validation,
      validate_life_expectancy_against_tr(
        life_table = mortality_life_tables,
        at_ages = c(0, 65),
        tolerance = 0.5
      )
    ),

    # ==========================================================================
    # MARITAL STATUS DIFFERENTIALS
    # ==========================================================================

    # NCHS deaths by marital status
    targets::tar_target(
      nchs_deaths_by_marital,
      fetch_nchs_deaths_by_marital_status(years = 2015:2019),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # ACS PUMS population by marital status
    targets::tar_target(
      acs_pop_by_marital,
      fetch_acs_pums_marital_status(years = 2015:2019, ages = 0:99),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Calculate marital mortality factors
    targets::tar_target(
      mortality_marital_factors,
      calculate_marital_mortality_factors(
        nchs_deaths = nchs_deaths_by_marital,
        acs_population = acs_pop_by_marital,
        reference_years = 2015:2019
      )
    )
  )
}

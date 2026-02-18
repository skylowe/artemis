#' Fertility Targets
#'
#' @description
#' Factory function that creates targets for the fertility subprocess (Section 1.1).
#'
#' @name fertility_targets
NULL

#' Create fertility targets
#'
#' @description
#' Creates targets for fertility data acquisition and projection (Section 1.1).
#' Includes historical birth rates, TFR projections, and cohort fertility.
#'
#' @return List of targets for fertility subprocess
#'
#' @export
create_fertility_targets <- function() {
  list(
    # ==========================================================================
    # DATA ACQUISITION TARGETS
    # ==========================================================================

    # Birth data from NCHS via NBER Stata files (1980-2024)
    targets::tar_target(
      nchs_births_raw,
      fetch_nchs_births_by_age_multi(
        years = config_data_sources$historical_birth_data$start_year:config_data_sources$historical_birth_data$end_year
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Census Bureau female population estimates (1980-2024)
    targets::tar_target(
      census_female_pop,
      {
        set_census_vintage_option(list(data_sources = config_data_sources))
        fetch_census_population_all(
          years = config_data_sources$population_estimates$start_year:
                  config_data_sources$population_estimates$end_year,
          ages = 10:54,
          sex = "female"
        )
      },
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Female population for fertility - always Census per TR2025 Section 1.1.b Input #3:
    # "From the U.S. Census Bureau, estimates of the July 1st female resident population"
    targets::tar_target(
      female_pop_for_fertility,
      census_female_pop
    ),

    # ==========================================================================
    # FERTILITY SUBPROCESS TARGETS
    # ==========================================================================

    # Step 1: Calculate historical birth rates from NCHS births / Census population
    targets::tar_target(
      fertility_rates_historical,
      calculate_historical_birth_rates(
        births = nchs_births_raw,
        population = female_pop_for_fertility,
        min_age = config_fertility$min_fertility_age,
        max_age = config_fertility$max_fertility_age
      )
    ),

    # Step 1b: Apply custom TFR overrides for recent years (if configured)
    targets::tar_target(
      fertility_rates_for_projection,
      apply_custom_recent_tfr(
        birth_rates = fertility_rates_historical,
        custom_recent_tfr = config_fertility$custom_recent_tfr
      )
    ),

    # Step 2: Calculate age-to-30 ratios
    targets::tar_target(
      fertility_age30_ratios,
      calculate_age30_ratios(fertility_rates_for_projection)
    ),

    # Step 3: Calculate trend factors
    targets::tar_target(
      fertility_trend_factors,
      calculate_trend_factors(
        ratios = fertility_age30_ratios,
        exclude_years = config_fertility$exclude_years
      )
    ),

    # Step 4: Calculate ultimate years by age
    targets::tar_target(
      fertility_ultimate_years,
      calculate_ultimate_years(
        min_age = config_fertility$min_fertility_age,
        max_age = config_fertility$max_fertility_age,
        base_year = config_fertility$projection_start_year,
        end_year = config_fertility$ultimate_year
      )
    ),

    # Step 5: Calculate interpolation weights
    # Note: ultimate_year (for age 30) is derived from fertility_ultimate_years table
    targets::tar_target(
      fertility_weights,
      calculate_interpolation_weights(
        years = config_metadata$projection_period$start_year:
                config_metadata$projection_period$end_year,
        base_year = config_fertility$rate_base_year,
        ultimate_year = fertility_ultimate_years[age == config_fertility$reference_age, ultimate_year],
        exponent = config_fertility$weight_exponent
      )
    ),

    # Step 6: Solve for ultimate age-30 rate
    # Note: age30_ultimate_year is derived from fertility_ultimate_years table
    targets::tar_target(
      fertility_ultimate_age30,
      solve_ultimate_age30_rate(
        target_tfr = config_fertility$ultimate_ctfr,
        base_age30_rate = fertility_rates_for_projection[year == config_fertility$rate_base_year & age == 30, birth_rate],
        base_ratios = fertility_age30_ratios[year == config_fertility$rate_base_year],
        trend_factors = fertility_trend_factors,
        ultimate_years = fertility_ultimate_years,
        base_year = config_fertility$rate_base_year,
        age30_ultimate_year = fertility_ultimate_years[age == config_fertility$reference_age, ultimate_year],
        weight_exponent = config_fertility$weight_exponent
      )
    ),

    # Step 7: Project age-30 rates
    targets::tar_target(
      fertility_age30_projected,
      project_age30_rates(
        years = config_metadata$projection_period$start_year:
                config_metadata$projection_period$end_year,
        base_rate = fertility_rates_for_projection[year == config_fertility$rate_base_year & age == 30, birth_rate],
        ultimate_rate = fertility_ultimate_age30,
        weights = fertility_weights
      )
    ),

    # Step 8: Project all age-specific rates
    targets::tar_target(
      fertility_rates_projected,
      project_birth_rates(
        years = config_metadata$projection_period$start_year:
                config_metadata$projection_period$end_year,
        age30_rates = fertility_age30_projected,
        base_ratios = fertility_age30_ratios[year == config_fertility$rate_base_year],
        trend_factors = fertility_trend_factors,
        ultimate_years = fertility_ultimate_years,
        base_year = config_fertility$rate_base_year
      )
    ),

    # Combined historical and projected rates
    targets::tar_target(
      fertility_rates_complete,
      rbind(
        fertility_rates_for_projection[, .(year, age, birth_rate)],
        fertility_rates_projected[year > config_fertility$rate_base_year]
      )
    ),

    # Step 9-10: Calculate TFR and CTFR series
    targets::tar_target(
      fertility_totals,
      calculate_fertility_totals(fertility_rates_complete)
    )
  )
}

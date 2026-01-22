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
      {
        years <- config_assumptions$data_sources$historical_birth_data$start_year:2024
        all_births <- data.table::rbindlist(lapply(years, function(yr) {
          dt <- fetch_nchs_births_by_age(yr)
          dt[, year := yr]
          dt
        }))
        all_births
      },
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Census Bureau female population estimates (1980-2024)
    targets::tar_target(
      census_female_pop,
      {
        vintage <- config_assumptions$data_sources$census_vintage
        if (is.null(vintage)) {
          stop("census_vintage not set in config - please specify in config/assumptions/tr2025.yaml")
        }
        options(artemis.census_vintage = vintage)

        fetch_census_population_all(
          years = config_assumptions$data_sources$population_estimates$start_year:
                  config_assumptions$data_sources$population_estimates$end_year,
          ages = 10:54,
          sex = "female"
        )
      },
      cue = targets::tar_cue(mode = "thorough")
    ),

    # TR2025 female population for fertility rate calculations
    targets::tar_target(
      tr2025_female_pop,
      {
        tr_file <- config_assumptions$projected_population$tr_historical_population_file
        if (is.null(tr_file) || !file.exists(tr_file)) {
          cli::cli_alert_warning("TR2025 population file not found, returning empty data")
          return(data.table::data.table(year = integer(), age = integer(), population = numeric()))
        }

        tr_pop <- data.table::fread(tr_file)
        years <- config_assumptions$data_sources$population_estimates$start_year:
                 config_assumptions$data_sources$population_estimates$end_year
        result <- tr_pop[Year %in% years & Age >= 10 & Age <= 54,
                         .(year = Year, age = Age, population = `F Tot`)]
        data.table::setorder(result, year, age)
        cli::cli_alert_success("Loaded TR2025 female population for {length(unique(result$year))} years")
        result
      }
    ),

    # Female population for fertility - uses TR2025 or Census based on config
    targets::tar_target(
      female_pop_for_fertility,
      {
        use_tr <- config_assumptions$projected_population$use_tr_historical_population
        if (isTRUE(use_tr)) {
          cli::cli_alert_info("Using TR2025 population for fertility rate calculations")
          tr2025_female_pop
        } else {
          cli::cli_alert_info("Using Census population for fertility rate calculations")
          census_female_pop
        }
      }
    ),

    # ==========================================================================
    # FERTILITY SUBPROCESS TARGETS
    # ==========================================================================

    # TR2025 implied births
    targets::tar_target(
      tr2025_implied_births,
      {
        substitute_years <- config_assumptions$fertility$use_tr2025_births_for_years
        if (is.null(substitute_years) || length(substitute_years) == 0) {
          return(data.table::data.table(year = integer(), sex = character(), births = numeric()))
        }
        calculate_tr2025_implied_births(years = substitute_years)
      }
    ),

    # NCHS births with optional TR2025 substitution
    targets::tar_target(
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

    # Step 1: Calculate historical birth rates
    targets::tar_target(
      fertility_rates_historical,
      calculate_historical_birth_rates(
        births = nchs_births_adjusted,
        population = female_pop_for_fertility,
        min_age = config_assumptions$fertility$min_fertility_age,
        max_age = config_assumptions$fertility$max_fertility_age
      )
    ),

    # Step 1b: Apply TFR constraints for specified years
    targets::tar_target(
      fertility_rates_for_projection,
      constrain_tfr_for_years(
        birth_rates = fertility_rates_historical,
        constrain_tfr = config_assumptions$fertility$constrain_tfr
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
        exclude_years = config_assumptions$fertility$exclude_years
      )
    ),

    # Step 4: Calculate ultimate years by age
    targets::tar_target(
      fertility_ultimate_years,
      calculate_ultimate_years(
        min_age = config_assumptions$fertility$min_fertility_age,
        max_age = config_assumptions$fertility$max_fertility_age,
        base_year = config_assumptions$fertility$projection_start_year,
        age30_ultimate_year = config_assumptions$fertility$age30_ultimate_year,
        end_year = config_assumptions$fertility$ultimate_year
      )
    ),

    # Step 5: Calculate interpolation weights
    targets::tar_target(
      fertility_weights,
      calculate_interpolation_weights(
        years = config_assumptions$metadata$projection_period$start_year:
                config_assumptions$metadata$projection_period$end_year,
        base_year = config_assumptions$fertility$rate_base_year,
        ultimate_year = config_assumptions$fertility$age30_ultimate_year,
        exponent = config_assumptions$fertility$weight_exponent
      )
    ),

    # Step 6: Solve for ultimate age-30 rate
    targets::tar_target(
      fertility_ultimate_age30,
      solve_ultimate_age30_rate(
        target_tfr = config_assumptions$fertility$ultimate_ctfr,
        base_age30_rate = fertility_rates_for_projection[year == config_assumptions$fertility$rate_base_year & age == 30, birth_rate],
        base_ratios = fertility_age30_ratios[year == config_assumptions$fertility$rate_base_year],
        trend_factors = fertility_trend_factors,
        ultimate_years = fertility_ultimate_years,
        base_year = config_assumptions$fertility$rate_base_year,
        age30_ultimate_year = config_assumptions$fertility$age30_ultimate_year,
        weight_exponent = config_assumptions$fertility$weight_exponent
      )
    ),

    # Step 7: Project age-30 rates
    targets::tar_target(
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
    targets::tar_target(
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

    # Combined historical and projected rates
    targets::tar_target(
      fertility_rates_complete,
      rbind(
        fertility_rates_for_projection[, .(year, age, birth_rate)],
        fertility_rates_projected[year > config_assumptions$fertility$rate_base_year]
      )
    ),

    # Step 9-10: Calculate TFR and CTFR series
    targets::tar_target(
      fertility_totals,
      calculate_fertility_totals(fertility_rates_complete)
    )
  )
}

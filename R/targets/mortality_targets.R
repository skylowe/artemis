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

    # NCHS Deaths data (1968 to TR year - 2)
    # Start year 1968: first year of NCHS microdata with single-year-of-age detail
    # End year derived from config: provisional year = trustees_report_year - 2
    targets::tar_target(
      nchs_deaths_raw,
      {
        tr_year <- config_assumptions$metadata$trustees_report_year
        last_death_year <- tr_year - 2L
        fetch_nchs_deaths_multi(years = 1968:last_death_year, config = config_assumptions)
      },
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Census population for mortality calculations
    # End year matches death data (population denominators for mx calculation)
    targets::tar_target(
      census_population_both,
      {
        tr_year <- config_assumptions$metadata$trustees_report_year
        last_death_year <- tr_year - 2L
        vintage <- set_census_vintage_option(config_assumptions)
        cache_file <- sprintf("data/cache/census/population_by_age_sex_1980_%d_v%d_bysex.rds", last_death_year, vintage)
        load_or_fetch(
          cache_file = cache_file,
          fetch_fn = function() {
            male_pop <- fetch_census_population_all(years = 1980:last_death_year, ages = 0:100, sex = "male")
            female_pop <- fetch_census_population_all(years = 1980:last_death_year, ages = 0:100, sex = "female")
            data.table::rbindlist(list(male_pop, female_pop), use.names = TRUE)
          },
          save_cache = TRUE
        )
      },
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Total births by year for q0 calculation
    # Birth data available one year ahead of death data (preliminary release)
    targets::tar_target(
      nchs_births_total,
      {
        tr_year <- config_assumptions$metadata$trustees_report_year
        last_birth_year <- tr_year - 1L
        load_total_births_for_q0(years = 1968:last_birth_year)
      },
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Sex-specific births by year (1940+)
    # Pre-1968: CDC NCHS NVSR Vol. 53 No. 20, Table 1 (published totals)
    # Post-1968: NBER microdata aggregated from monthly records
    # TR2025 Items 35 (Section 1.4.b) and 32 (Section 1.5.b): "births by
    # month and sex of child, for years 1931-2023"
    targets::tar_target(
      nchs_births_by_sex,
      {
        tr_year <- config_assumptions$metadata$trustees_report_year
        last_birth_year <- tr_year - 1L

        # Post-1968: NBER microdata (existing cache)
        post_1968 <- load_cached_multi_year(
          years = 1968:last_birth_year,
          cache_pattern = "data/raw/nchs/births_by_month_sex_%d.rds",
          on_missing = "skip"
        )
        post_1968 <- post_1968[, .(births = sum(births)), by = .(year, sex)]

        # Pre-1968: CDC NVSR published totals
        pre_1968 <- load_historical_births_by_sex()

        # Combine (pre-1968 rows won't overlap with post-1968)
        data.table::rbindlist(list(pre_1968, post_1968), use.names = TRUE)
      },
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Detailed infant deaths for q0 calculation
    targets::tar_target(
      nchs_infant_deaths_detailed,
      {
        tr_year <- config_assumptions$metadata$trustees_report_year
        last_death_year <- tr_year - 2L
        load_infant_deaths(years = 1968:last_death_year)
      },
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Monthly births for q0 calculation
    # Starts 1 year before death data for birth cohort alignment
    targets::tar_target(
      nchs_births_monthly,
      {
        tr_year <- config_assumptions$metadata$trustees_report_year
        last_birth_year <- tr_year - 1L
        load_monthly_births(years = 1967:last_birth_year)
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
    # Default: regression method with by_cause=TRUE (TR2025 official methodology).
    # Set starting_aax_method = "tr_qx" for exact TR2025 qx alignment.
    targets::tar_target(
      mortality_mx_projected,
      run_mortality_projection(
        deaths = nchs_deaths_raw,
        population = census_population_both,
        base_year = config_assumptions$mortality$base_year,
        projection_end = config_assumptions$metadata$projection_period$end_year,
        by_cause = if (is.null(config_assumptions$mortality$by_cause)) FALSE else config_assumptions$mortality$by_cause,
        starting_aax_method = if (is.null(config_assumptions$mortality$starting_aax_method)) "regression" else config_assumptions$mortality$starting_aax_method,
        mortality_config = config_assumptions$mortality
      )
    ),

    # Step 6: Calculate qx from mx using full TR methodology
    # - q0: separation factor method
    # - q1: 4m1 ratio method for projected years (TR2025 Section 1.2.c)
    # - q2-99: standard qx = mx/(1+0.5*mx)
    # - q100+: TR extrapolation method
    targets::tar_target(
      mortality_qx_unadjusted,
      {
        # Aggregate deaths by year/age/sex (removing cause) for 4m1 calculation
        deaths_agg <- nchs_deaths_raw[, .(deaths = sum(deaths, na.rm = TRUE)),
                                      by = .(year, age, sex)]
        # Get last historical year from data (max year in deaths data)
        last_hist_year <- max(deaths_agg$year)

        calculate_qx_with_infant_mortality(
          mx_total = mortality_mx_total,
          infant_deaths_detailed = nchs_infant_deaths_detailed,
          monthly_births = nchs_births_monthly,
          population = census_population_both,
          deaths_raw = deaths_agg,
          last_historical_year = last_hist_year,
          max_age = 100,
          births_by_sex = nchs_births_by_sex
        )
      }
    ),

    # Step 6b: Adjust qx for ages 85+ using HMD (configurable)
    targets::tar_target(
      mortality_qx_historical,
      {
        hmd_cfg <- config_assumptions$mortality$hmd_calibration
        use_hmd <- hmd_cfg$enabled %||% TRUE
        if (use_hmd) {
          adjust_qx_with_hmd(
            qx = mortality_qx_unadjusted,
            transition_age = hmd_cfg$transition_age %||% 85L,
            max_age = 100
          )
        } else {
          cli::cli_alert_info("HMD calibration disabled — using raw qx for all ages")
          mortality_qx_unadjusted
        }
      }
    ),

    # Step 6c: Compute infant separation factor (f0) from actual death timing
    targets::tar_target(
      mortality_infant_f0,
      calculate_infant_separation_factor(nchs_infant_deaths_detailed)
    ),

    # Step 7: Calculate life tables
    targets::tar_target(
      mortality_life_tables,
      calculate_life_table(
        qx = mortality_qx_historical,
        radix = 100000,
        max_age = 100,
        f0 = mortality_infant_f0
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
    # For regression method: apply HMD calibration + age-last-birthday conversion
    # For tr_qx method: TR files already contain age-last-birthday qx
    targets::tar_target(
      mortality_qx_projected,
      {
        qx_proj <- mortality_mx_projected$projected_qx
        method <- get_config_with_default(
          config_assumptions, "mortality", "starting_aax_method",
          default = "regression"
        )
        if (method == "tr_qx") {
          cli::cli_alert_info("Skipping HMD calibration and ALB conversion for tr_qx method")
          qx_proj <- qx_proj[age <= 100]
          return(qx_proj)
        }
        hmd_cfg <- config_assumptions$mortality$hmd_calibration
        use_hmd <- hmd_cfg$enabled %||% TRUE
        if (use_hmd) {
          qx_adjusted <- adjust_qx_with_hmd(
            qx = qx_proj,
            transition_age = hmd_cfg$transition_age %||% 85L,
            max_age = 100
          )
        } else {
          cli::cli_alert_info("HMD calibration disabled for projected qx — using raw values")
          qx_adjusted <- qx_proj
        }

        # Extend f0 to cover projected years using last historical value
        proj_years <- sort(unique(qx_adjusted$year))
        f0_proj <- extend_f0_to_projected_years(mortality_infant_f0, proj_years)

        # Convert to age-last-birthday qx (TR2025 Section 1.2.c):
        # qx_alb = 1 - L_{x+1}/L_x for ages 0-99, q100 = 1 - T101/T100
        cli::cli_alert_info("Applying age-last-birthday qx conversion (regression mode)")
        lt_temp <- calculate_life_table(qx = qx_adjusted, radix = 100000, max_age = 100,
                                        f0 = f0_proj)
        # Add qx_exact column for ALB function compatibility
        lt_temp[, qx_exact := qx]
        qx_alb <- calculate_age_last_birthday_qx(lt_temp, min_age = 0, max_age = 99)
        result <- apply_age_last_birthday_qx(qx_adjusted, qx_alb, min_age = 0)
        # Clamp qx to [0, 1] — ALB formula can produce negative values at the
        # HMD calibration boundary (age 99) where the survival curve has a kink
        n_clamped <- result[qx < 0 | qx > 1, .N]
        if (n_clamped > 0) {
          cli::cli_alert_warning("Clamped {n_clamped} out-of-range ALB qx values to [0, 1]")
          result[qx < 0, qx := 0]
          result[qx > 1, qx := 1]
        }
        result
      }
    ),

    # Step 10: Calculate projected life tables
    targets::tar_target(
      mortality_life_tables_projected,
      {
        proj_years <- sort(unique(mortality_qx_projected$year))
        f0_proj <- extend_f0_to_projected_years(mortality_infant_f0, proj_years)
        calculate_life_table(
          qx = mortality_qx_projected,
          radix = 100000,
          max_age = 100,
          f0 = f0_proj
        )
      }
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
      fetch_nchs_deaths_by_marital_status(
        years = config_assumptions$mortality$marital_reference_years
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # ACS PUMS population by marital status
    targets::tar_target(
      acs_pop_by_marital,
      fetch_acs_pums_marital_status(
        years = config_assumptions$mortality$marital_reference_years,
        ages = 0:99
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Calculate marital mortality factors
    targets::tar_target(
      mortality_marital_factors,
      calculate_marital_mortality_factors(
        nchs_deaths = nchs_deaths_by_marital,
        acs_population = acs_pop_by_marital,
        reference_years = config_assumptions$mortality$marital_reference_years,
        smoothing_parameter = config_assumptions$mortality$marital_smoothing_parameter
      )
    )
  )
}

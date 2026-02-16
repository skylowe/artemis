#' Marriage and Divorce Targets
#'
#' @description
#' Factory function that creates targets for marriage and divorce subprocesses.
#'
#' @name marriage_divorce_targets
NULL

#' Create marriage and divorce targets
#'
#' @description
#' Creates targets for marriage (Section 1.6) and divorce (Section 1.7) projections.
#' Includes data acquisition, rate calculations, and validation.
#'
#' Note: O immigration targets (Phase 5) are in create_immigration_targets().
#'
#' @return List of targets for marriage and divorce subprocesses
#'
#' @export
create_marriage_divorce_targets <- function() {
  list(
    # ==========================================================================
    # DATA ACQUISITION - MARRIAGE
    # ==========================================================================

    targets::tar_target(
      nchs_mra_marriages_1978_1988,
      load_or_fetch(
        cache_file = here::here("data/cache/nber_marriage/nchs_mra_marriages_1978_1988.rds"),
        fetch_fn = fetch_nchs_mra_marriages_1978_1988
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    targets::tar_target(
      cps_unmarried_population,
      load_cached_rds(
        cache_file = here::here("data/cache/ipums_cps/cps_unmarried_1957_1995.rds"),
        on_missing = "abort",
        abort_message = "CPS data not cached"
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    targets::tar_target(
      nchs_mra_subset_1989_1995,
      load_or_fetch(
        cache_file = here::here("data/cache/nber_marriage/nchs_mra_marriages_1989_1995.rds"),
        fetch_fn = fetch_nchs_mra_marriages_1989_1995
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    targets::tar_target(
      nchs_us_total_marriages,
      fetch_nchs_us_total_marriages(years = 1989:2022)
    ),

    targets::tar_target(
      acs_marriage_grids,
      load_aligned_acs_marriage_grids(cache_dir = here::here("data/cache/acs_marriage")),
      cue = targets::tar_cue(mode = "thorough")
    ),

    targets::tar_target(
      standard_pop_2010,
      {
        cache_file <- here::here("data/cache/acs_pums/marital_all_years.rds")
        if (file.exists(cache_file)) get_2010_standard_population(cache_file = cache_file, by_age_group = TRUE)
        else cli::cli_abort("Standard population cache not found")
      }
    ),

    targets::tar_target(
      nchs_prior_status_1978_1988,
      load_or_fetch(
        cache_file = here::here("data/cache/nber_marriage/nchs_marriages_by_prior_status_1978_1988.rds"),
        fetch_fn = fetch_nchs_marriages_by_prior_status_1978_1988
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    targets::tar_target(
      cps_unmarried_by_prior_status,
      extract_cps_unmarried_by_prior_status(
        years = config_assumptions$marriage$prior_status$years
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    targets::tar_target(
      acs_same_sex_grids,
      fetch_acs_same_sex_grids(years = 2015:2022),
      cue = targets::tar_cue(mode = "thorough")
    ),

    targets::tar_target(
      same_sex_fraction,
      {
        ss_result <- calculate_same_sex_fraction(
          same_sex_grids = acs_same_sex_grids,
          opposite_sex_grids = acs_marriage_grids,
          years = config_assumptions$marriage$same_sex$reference_years
        )
        ss_result$overall_fraction
      }
    ),

    # ==========================================================================
    # MARRIAGE SUBPROCESS (Phase 6)
    # ==========================================================================

    targets::tar_target(
      marriage_projection,
      run_marriage_projection(
        nchs_marriages_1978_1988 = nchs_mra_marriages_1978_1988,
        cps_unmarried = cps_unmarried_population,
        nchs_subset = nchs_mra_subset_1989_1995,
        acs_grids = acs_marriage_grids,
        nchs_us_totals = nchs_us_total_marriages,
        standard_pop_by_group = standard_pop_2010,
        prior_status_data = nchs_prior_status_1978_1988,
        unmarried_pop_by_status = cps_unmarried_by_prior_status,
        same_sex_data = acs_same_sex_grids,
        same_sex_fraction = same_sex_fraction,
        config = config_assumptions,
        include_same_sex = TRUE,
        include_prior_status = TRUE
      )
    ),

    targets::tar_target(marriage_rates_all, marriage_projection$all_rates),
    targets::tar_target(marriage_amr_historical, marriage_projection$amr_historical),
    targets::tar_target(marriage_amr_projected, marriage_projection$amr_projected),
    targets::tar_target(marriage_rates_opposite_sex, marriage_projection$opposite_sex_rates),
    targets::tar_target(marriage_rates_same_sex, marriage_projection$same_sex_rates),
    targets::tar_target(marriage_status_differentials, marriage_projection$status_differentials),
    targets::tar_target(marriage_margrid, marriage_projection$margrid),

    targets::tar_target(
      marriage_validation,
      validate_marriage_comprehensive(projection = marriage_projection, tolerance_amr = 0.001, tolerance_totals = 0.05)
    ),

    targets::tar_target(
      marriage_validation_quick,
      validate_marriage_quick(projection = marriage_projection)
    ),

    # ==========================================================================
    # DIVORCE SUBPROCESS (Phase 7)
    # ==========================================================================

    targets::tar_target(
      divorce_projection,
      run_divorce_projection(
        cache_dir = here::here("data/cache"),
        config = config_assumptions
      )
    ),

    # Extract divorce outputs
    targets::tar_target(divorce_adr_projected, divorce_projection$projected_adr),
    targets::tar_target(divorce_adr_historical, divorce_projection$historical_adr),
    targets::tar_target(divorce_adr_complete, divorce_projection$adr_complete),
    targets::tar_target(divorce_rates_projected, divorce_projection$projected_rates),
    targets::tar_target(divorce_divgrid_base, divorce_projection$divgrid_base),
    targets::tar_target(divorce_divgrid_adjusted, divorce_projection$divgrid_adjusted),
    targets::tar_target(divorce_standard_pop, divorce_projection$standard_pop),
    targets::tar_target(divorce_all_rates, divorce_projection$all_rates),

    # Divorce validation
    targets::tar_target(divorce_validation, validate_divorce_comprehensive(projection = divorce_projection)),
    targets::tar_target(divorce_validation_quick, validate_divorce_quick(projection = divorce_projection))
  )
}

#' Immigration Targets
#'
#' @description
#' Factory function that creates targets for the LPR and O immigration subprocesses.
#'
#' @name immigration_targets
NULL

#' Create immigration targets
#'
#' @description
#' Creates targets for LPR immigration (Section 1.3) and O immigration (Section 1.5).
#' Includes DHS data loading, distribution calculations, and projections.
#'
#' @return List of targets for immigration subprocess
#'
#' @export
create_immigration_targets <- function() {
  list(
    # ==========================================================================
    # DHS DATA ACQUISITION
    # ==========================================================================

    # DHS LPR immigration data (2006-2023)
    targets::tar_target(
      dhs_lpr_data,
      load_cached_rds(
        cache_file = "data/cache/dhs_immigration/dhs_lpr_all_years.rds",
        on_missing = "abort",
        abort_message = "DHS data not cached - run fetch_dhs_lpr_data_multi() first"
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # ==========================================================================
    # TR2025 DATA FOR IMMIGRATION DISTRIBUTION
    # ==========================================================================

    # TR2025 population in long format
    targets::tar_target(
      tr2025_population_long,
      load_tr_population_long(
        file_path = here::here("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")
      )
    ),

    # TR2025 qx in long format
    targets::tar_target(
      tr2025_qx_long,
      load_tr_qx_long(
        male_file = here::here("data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv"),
        female_file = here::here("data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv")
      )
    ),

    # TR2025-derived immigration distribution
    targets::tar_target(
      tr_derived_immigration_dist,
      calculate_tr_derived_distribution(
        tr_population = tr2025_population_long,
        tr_qx = tr2025_qx_long,
        years = 2023:2030
      )
    ),

    # ==========================================================================
    # LPR IMMIGRATION SUBPROCESS
    # ==========================================================================

    # Step 1: LPR distribution
    targets::tar_target(
      lpr_distribution,
      get_lpr_distribution_for_projection(
        config = config_assumptions,
        tr_derived_dist = tr_derived_immigration_dist
      )
    ),

    # Step 2: Emigration distribution
    targets::tar_target(
      emigration_distribution,
      calculate_emigration_distribution_dhs(
        dhs_data = load_dhs_lpr_data(),
        reference_years = config_assumptions$immigration$emigration$distribution_years
      )
    ),

    # Step 3: NEW/AOS ratio
    targets::tar_target(
      new_aos_ratio,
      calculate_new_aos_ratio(
        dhs_data = load_dhs_lpr_data(),
        reference_years = config_assumptions$immigration$lpr$new_aos_ratio_years
      )
    ),

    # Step 4: LPR assumptions
    targets::tar_target(
      lpr_assumptions,
      get_tr_lpr_assumptions(
        years = config_assumptions$metadata$projection_period$start_year:
                config_assumptions$metadata$projection_period$end_year
      )
    ),

    # Step 5: Project LPR immigration
    targets::tar_target(
      lpr_immigration_projected,
      project_lpr_immigration(
        assumptions = lpr_assumptions,
        distribution = lpr_distribution
      )
    ),

    # Step 6: Split LPR into NEW and AOS
    targets::tar_target(
      lpr_new_aos_split,
      {
        method <- get_config_with_default(
          config_assumptions, "immigration", "lpr", "new_aos_split_method",
          default = "assumption"
        )
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
    targets::tar_target(
      new_arrivals_projected,
      lpr_new_aos_split$new_arrivals
    ),

    # Extract AOS
    targets::tar_target(
      aos_projected,
      lpr_new_aos_split$aos
    ),

    # Step 7: Project emigration
    targets::tar_target(
      legal_emigration_projected,
      project_legal_emigration(
        assumptions = lpr_assumptions,
        distribution = emigration_distribution
      )
    ),

    # Step 8: Calculate net LPR
    targets::tar_target(
      net_lpr_immigration,
      calculate_net_lpr(
        lpr_immigration = lpr_immigration_projected,
        emigration = legal_emigration_projected
      )
    ),

    # ==========================================================================
    # O IMMIGRATION DATA ACQUISITION
    # ==========================================================================

    # DHS Nonimmigrant stock data
    targets::tar_target(
      dhs_nonimmigrant_stock,
      load_or_fetch(
        cache_file = "data/cache/dhs/nonimmigrant_stock_age_sex.rds",
        fetch_fn = fetch_dhs_nonimmigrant_stock
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # DHS BOY nonimmigrants
    targets::tar_target(
      dhs_boy_nonimmigrants,
      load_or_fetch(
        cache_file = "data/cache/dhs/boy_nonimmigrants.rds",
        fetch_fn = fetch_dhs_boy_nonimmigrants
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # DHS DACA data
    targets::tar_target(
      dhs_daca_data,
      load_cached_dhs_daca_data(
        fetch_grants_fn = fetch_dhs_daca_grants,
        fetch_stock_fn = fetch_dhs_daca_stock
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # ACS foreign-born flows
    targets::tar_target(
      acs_foreign_born_flows,
      load_cached_multi_year(
        years = c(2006:2019, 2021:2023),
        cache_pattern = "data/cache/acs_pums/foreign_born_flows_%d.rds",
        on_missing = "skip"
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # ACS foreign-born arrivals
    targets::tar_target(
      acs_foreign_born_arrivals,
      {
        arrivals <- calculate_acs_new_arrivals(foreign_born_flows = acs_foreign_born_flows, entry_window = 1)
        arrivals[, .(population = sum(population)), by = .(year, age, sex)]
      }
    ),

    # ACS undercount factors
    targets::tar_target(
      acs_undercount_factors,
      calculate_acs_undercount_factors(years = 2017:2019, use_fallback = TRUE)
    ),

    # ==========================================================================
    # O IMMIGRATION SUBPROCESS
    # ==========================================================================

    # V.A2 net immigration data
    targets::tar_target(
      va2_net_immigration,
      {
        alternative <- get_config_with_default(
          config_assumptions, "immigration", "va2_alternative",
          default = "intermediate"
        )
        va2_file <- get_config_with_default(config_assumptions, "immigration", "va2_file", default = "")
        data_dir <- if (va2_file == "") "data/raw/SSA_TR2025" else dirname(va2_file)
        if (data_dir == ".") data_dir <- "data/raw/SSA_TR2025"
        get_tr_va2_net_immigration(
          years = config_assumptions$metadata$projection_period$start_year:
                  config_assumptions$metadata$projection_period$end_year,
          alternative = alternative,
          data_dir = data_dir,
          convert_to_persons = TRUE
        )
      }
    ),

    # Net O for projection
    targets::tar_target(
      net_o_for_projection,
      calculate_net_o_for_projection(
        va2_net_immigration = va2_net_immigration,
        tr_derived_dist = tr_derived_immigration_dist,
        calibration_add = 0
      )
    ),

    # Full O immigration projection
    targets::tar_target(
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

    # Extract O immigration outputs
    targets::tar_target(o_immigration_oi, o_immigration_projection$o_immigration),
    targets::tar_target(o_emigration_oe, o_immigration_projection$o_emigration),
    targets::tar_target(net_o_immigration, o_immigration_projection$net_o),
    targets::tar_target(o_population_stock, o_immigration_projection$o_population),

    # DACA projection
    targets::tar_target(
      daca_projection,
      run_daca_projection(
        acs_2012_data = NULL,
        dhs_stock = dhs_daca_data$stock,
        dhs_grants = dhs_daca_data$grants,
        projection_years = config_assumptions$metadata$projection_period$start_year:
                           config_assumptions$metadata$projection_period$end_year,
        config = config_assumptions
      )
    ),

    # Extract DACA population
    targets::tar_target(daca_population_projected, daca_projection$daca_population),

    # ==========================================================================
    # IMMIGRATION VALIDATION
    # ==========================================================================

    # LPR validation
    targets::tar_target(
      lpr_immigration_validation,
      {
        projection_result <- list(
          lpr_immigration = lpr_immigration_projected,
          new_arrivals = new_arrivals_projected,
          aos = aos_projected,
          emigration = legal_emigration_projected,
          net_lpr = net_lpr_immigration,
          distributions = list(lpr = lpr_distribution, emigration = emigration_distribution),
          new_aos_ratio = new_aos_ratio,
          assumptions = lpr_assumptions
        )
        validate_lpr_outputs(projection_result, tolerance = 0.001)
      }
    ),

    # O immigration validation
    targets::tar_target(
      o_immigration_validation,
      validate_o_immigration_comprehensive(
        projection = o_immigration_projection,
        daca_projection = daca_projection,
        tolerance_strict = 0.001,
        tolerance_relaxed = 0.15
      )
    )
  )
}

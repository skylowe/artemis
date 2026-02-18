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
        file_path = here::here(resolve_tr_file(list(metadata = config_metadata), "population_dec"))
      )
    ),

    # TR2025 qx in long format
    targets::tar_target(
      tr2025_qx_long,
      load_tr_qx_long(
        male_file = here::here(resolve_tr_file(list(metadata = config_metadata), "death_probs_proj", sex = "male")),
        female_file = here::here(resolve_tr_file(list(metadata = config_metadata), "death_probs_proj", sex = "female"))
      )
    ),

    # TR2025-derived immigration distribution
    targets::tar_target(
      tr_derived_immigration_dist,
      calculate_tr_derived_distribution(
        tr_population = tr2025_population_long,
        tr_qx = tr2025_qx_long,
        years = config_lpr_immigration$lpr$tr_derived_years
      )
    ),

    # ==========================================================================
    # LPR IMMIGRATION SUBPROCESS
    # ==========================================================================

    # Step 1: LPR distribution(s)
    # Returns either:
    #   - list(new_distribution, aos_distribution, combined_distribution) for separate mode
    #   - data.table(age, sex, distribution) for combined/tr_derived mode
    targets::tar_target(
      lpr_distribution,
      get_lpr_distribution_for_projection(
        config = list(immigration = list(
          lpr = config_lpr_immigration$lpr,
          ultimate_net_lpr_immigration = config_lpr_immigration$ultimate_net_lpr_immigration,
          o_immigration = list(ultimate_gross_o = config_o_immigration$ultimate_gross_o)
        )),
        tr_derived_dist = tr_derived_immigration_dist
      )
    ),

    # Step 2: Emigration distribution
    # Source determined by config: "cbo" (default) or "dhs" (legacy)
    targets::tar_target(
      emigration_distribution,
      {
        source <- config_lpr_immigration$emigration$distribution_source
        if (source == "cbo") {
          cbo_data <- load_cbo_migration(
            file_path = here::here(config_lpr_immigration$cbo_file)
          )
          calculate_cbo_emigration_distribution(
            cbo_data,
            config_lpr_immigration$emigration$cbo_reference_years
          )
        } else {
          calculate_emigration_distribution_dhs(
            dhs_data = load_dhs_lpr_data(),
            reference_years = config_lpr_immigration$emigration$distribution_years
          )
        }
      }
    ),

    # Step 3: NEW/AOS ratio (used only for combined mode / ratio split)
    targets::tar_target(
      new_aos_ratio,
      calculate_new_aos_ratio(
        dhs_data = load_dhs_lpr_data(),
        reference_years = config_lpr_immigration$lpr$new_aos_ratio_years
      )
    ),

    # Step 4: LPR assumptions (V.A2 or CBO, based on config)
    # Includes both historical (for historical_population component method)
    # and projected years (for LPR projection)
    targets::tar_target(
      lpr_assumptions,
      {
        source <- config_lpr_immigration$lpr$assumptions_source
        hist_start <- config_historical_pop$start_year
        proj_end <- config_metadata$projection_period$end_year
        all_years <- hist_start:proj_end

        if (source == "cbo") {
          # CBO only has projected years; historical comes from V.A2
          proj_start <- config_metadata$projection_period$start_year
          proj_years <- proj_start:proj_end
          cbo_file <- here::here(config_lpr_immigration$cbo_file)
          cbo_result <- get_cbo_lpr_assumptions(
            years = proj_years,
            cbo_file = cbo_file,
            new_aos_ratio = new_aos_ratio$new_ratio
          )

          # Get historical from V.A2
          emig_ratio <- config_lpr_immigration$emigration$ratio
          alternative <- config_lpr_immigration$va2_alternative
          data_dir <- get_tr_data_dir(list(metadata = config_metadata))

          hist_years <- hist_start:(proj_start - 1L)
          va2_hist <- get_tr_lpr_assumptions(
            years = hist_years,
            alternative = alternative,
            data_dir = data_dir,
            emigration_ratio = emig_ratio
          )
          data.table::rbindlist(list(va2_hist, cbo_result), fill = TRUE)
        } else {
          emig_ratio <- config_lpr_immigration$emigration$ratio
          alternative <- config_lpr_immigration$va2_alternative
          data_dir <- get_tr_data_dir(list(metadata = config_metadata))

          get_tr_lpr_assumptions(
            years = all_years,
            alternative = alternative,
            data_dir = data_dir,
            emigration_ratio = emig_ratio
          )
        }
      }
    ),

    # Step 5: Project LPR immigration
    # Conditional: uses separate distributions (TR2025) or combined (legacy)
    targets::tar_target(
      lpr_projection_result,
      {
        dist_method <- config_lpr_immigration$lpr$distribution_method
        dhs_mode <- config_lpr_immigration$lpr$dhs_distribution_mode

        use_separate <- (dist_method == "dhs" && dhs_mode == "separate" &&
                          is.list(lpr_distribution) && "new_distribution" %in% names(lpr_distribution))

        if (use_separate) {
          # TR2025 Section 1.3: Separate NEW/AOS distributions
          cli::cli_alert_info("Projecting LPR with separate NEW/AOS distributions")
          result <- project_lpr_immigration_separate(
            assumptions = lpr_assumptions,
            new_distribution = lpr_distribution$new_distribution,
            aos_distribution = lpr_distribution$aos_distribution
          )
          result
        } else {
          # Combined distribution mode (legacy or tr_derived)
          dist <- if (is.list(lpr_distribution) && "combined_distribution" %in% names(lpr_distribution)) {
            lpr_distribution$combined_distribution
          } else {
            lpr_distribution
          }

          lpr_imm <- project_lpr_immigration(
            assumptions = lpr_assumptions,
            distribution = dist
          )

          # Split into NEW/AOS using assumptions or ratio
          split_method <- config_lpr_immigration$lpr$new_aos_split_method
          if (split_method == "assumption") {
            split_result <- split_lpr_new_aos(
              lpr_immigration = lpr_imm,
              assumptions = lpr_assumptions,
              method = "assumption"
            )
          } else {
            split_result <- split_lpr_new_aos(
              lpr_immigration = lpr_imm,
              new_ratio = new_aos_ratio$new_ratio,
              method = "ratio"
            )
          }

          list(
            lpr_immigration = lpr_imm,
            new_arrivals = split_result$new_arrivals,
            aos = split_result$aos
          )
        }
      }
    ),

    # Extract targets from projection result (preserves downstream names)
    targets::tar_target(
      lpr_immigration_projected,
      lpr_projection_result$lpr_immigration
    ),

    targets::tar_target(
      new_arrivals_projected,
      lpr_projection_result$new_arrivals
    ),

    targets::tar_target(
      aos_projected,
      lpr_projection_result$aos
    ),

    # Step 6: Project emigration
    targets::tar_target(
      legal_emigration_projected,
      project_legal_emigration(
        assumptions = lpr_assumptions,
        distribution = emigration_distribution
      )
    ),

    # Step 7: Calculate net LPR
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
        years = config_data_sources$acs_foreign_born_years,
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
      calculate_acs_undercount_factors(
        years = config_data_sources$acs_undercount_years,
        use_fallback = TRUE
      )
    ),

    # ==========================================================================
    # O IMMIGRATION STATIC DATA (CSV)
    # ==========================================================================

    # Overstay percentages by age (from Warren & Kerwin 2017, DHS reports)
    targets::tar_target(
      o_overstay_percentages,
      load_overstay_percentages()
    ),

    # Nonimmigrant age distribution (from DHS Yearbook visa composition)
    targets::tar_target(
      o_ni_age_distribution,
      load_nonimmigrant_age_distribution()
    ),

    # DHS anchor point totals for type interpolation (2010, 2015)
    targets::tar_target(
      o_dhs_anchor_points,
      load_dhs_anchor_points()
    ),

    # Base departure rates by age/type/sex
    targets::tar_target(
      o_base_departure_rates,
      load_base_departure_rates()
    ),

    # DACA historical stock (2013-2022 USCIS data)
    targets::tar_target(
      daca_historical_stock,
      load_daca_historical_stock()
    ),

    # ==========================================================================
    # O IMMIGRATION SUBPROCESS
    # ==========================================================================

    # V.A2 net immigration data
    targets::tar_target(
      va2_net_immigration,
      {
        data_dir <- get_tr_data_dir(list(metadata = config_metadata))
        get_tr_va2_net_immigration(
          years = config_metadata$projection_period$start_year:
                  config_metadata$projection_period$end_year,
          alternative = config_lpr_immigration$va2_alternative,
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
        projection_years = config_metadata$projection_period$start_year:
                           config_metadata$projection_period$end_year,
        config = list(immigration = list(o_immigration = config_o_immigration)),
        dhs_ni_stock = dhs_nonimmigrant_stock
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
        projection_years = config_metadata$projection_period$start_year:
                           config_metadata$projection_period$end_year,
        config = list(immigration = list(o_immigration = config_o_immigration))
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
        # Build distributions list based on mode (separate vs combined)
        if (is.list(lpr_distribution) && "new_distribution" %in% names(lpr_distribution)) {
          dist_list <- list(
            lpr = lpr_distribution$combined_distribution,
            new = lpr_distribution$new_distribution,
            aos = lpr_distribution$aos_distribution,
            emigration = emigration_distribution
          )
        } else {
          lpr_dist_for_val <- if (is.list(lpr_distribution) && "combined_distribution" %in% names(lpr_distribution)) {
            lpr_distribution$combined_distribution
          } else {
            lpr_distribution
          }
          dist_list <- list(lpr = lpr_dist_for_val, emigration = emigration_distribution)
        }

        projection_result <- list(
          lpr_immigration = lpr_immigration_projected,
          new_arrivals = new_arrivals_projected,
          aos = aos_projected,
          emigration = legal_emigration_projected,
          net_lpr = net_lpr_immigration,
          distributions = dist_list,
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

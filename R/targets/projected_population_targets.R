#' Projected Population Targets
#'
#' @description
#' Factory function that creates targets for the projected population subprocess (Section 1.8).
#'
#' @name projected_population_targets
NULL

#' Create projected population targets
#'
#' @description
#' Creates targets for projected population (Section 1.8).
#' Includes Phase 8A-8F: data assembly, core projection, marital status,
#' children fate, CNI population, and validation.
#'
#' @return List of targets for projected population subprocess
#'
#' @export
create_projected_population_targets <- function() {
  list(
    # ==========================================================================
    # PHASE 8A: DATA ASSEMBLY
    # ==========================================================================

    # CPS children per couple data
    targets::tar_target(
      cps_children_per_couple,
      load_cached_rds(
        cache_file = here::here("data/cache/ipums_cps/cps_children_per_couple_1962_2022.rds"),
        on_missing = "warn"
      ),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Verify projection inputs
    targets::tar_target(
      projection_inputs_verification,
      {
        common_cols <- c("year", "age", "sex", "qx")
        hist_qx <- mortality_qx_historical[, ..common_cols]
        if (exists("mortality_qx_projected") && !is.null(mortality_qx_projected)) {
          proj_qx <- mortality_qx_projected[, ..common_cols]
          mortality_qx_combined <- data.table::rbindlist(list(hist_qx, proj_qx), use.names = TRUE)
        } else {
          mortality_qx_combined <- hist_qx
        }
        verify_all_projection_inputs(
          fertility_rates = fertility_rates_complete,
          mortality_qx = mortality_qx_combined,
          mortality_differentials = mortality_marital_factors,
          net_lpr = net_lpr_immigration,
          net_o = net_o_immigration,
          marriage_rates = marriage_projection,
          divorce_rates = divorce_projection,
          historical_population = historical_population,
          historical_marital = historical_population_marital,
          projection_years = config_assumptions$metadata$projection_period$start_year:
                             config_assumptions$metadata$projection_period$end_year,
          starting_year = config_assumptions$projected_population$starting_year
        )
      }
    ),

    # Extract starting population
    targets::tar_target(
      starting_population,
      {
        use_tr <- config_assumptions$projected_population$use_tr_historical_population
        starting_year <- config_assumptions$projected_population$starting_year
        if (isTRUE(use_tr)) {
          cli::cli_alert_info("Using TR2025 historical population for starting population")
          tr_file <- get_config_with_default(
            config_assumptions, "projected_population", "tr_historical_population_file",
            default = "data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv"
          )
          tr_start_status <- load_tr_starting_population(
            tr_file = tr_file,
            starting_year = starting_year
          )
          list(valid = TRUE, population = tr_start_status, message = "TR2025 historical population loaded")
        } else {
          extract_starting_population(
            historical_population = historical_population,
            historical_marital = historical_population_marital,
            starting_year = starting_year
          )
        }
      }
    ),

    # ==========================================================================
    # PHASE 8B: CORE POPULATION PROJECTION
    # ==========================================================================

    # Period life tables for age-last-birthday qx
    targets::tar_target(
      tr2025_period_life_tables,
      load_tr_period_life_tables(
        male_file = "data/raw/SSA_TR2025/PerLifeTables_M_Alt2_TR2025.csv",
        female_file = "data/raw/SSA_TR2025/PerLifeTables_F_Alt2_TR2025.csv",
        start_year = 1900, end_year = 2099
      )
    ),

    # Age-last-birthday qx
    targets::tar_target(
      qx_age_last_birthday,
      calculate_age_last_birthday_qx(period_life_table = tr2025_period_life_tables, min_age = 85, max_age = 99)
    ),

    # Combined mortality qx
    targets::tar_target(
      mortality_qx_for_projection,
      combine_mortality_qx_for_projection(
        mortality_qx_projected = mortality_qx_projected,
        mortality_qx_historical = mortality_qx_historical,
        qx_age_last_birthday = qx_age_last_birthday,
        config = config_assumptions
      )
    ),

    # qx for ages 100-119
    targets::tar_target(
      qx_100_119,
      {
        male_proj_file <- get_config_with_default(
          config_assumptions, "mortality", "starting_tr_qx", "male_qx_proj_file",
          default = "data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv"
        )
        female_proj_file <- get_config_with_default(
          config_assumptions, "mortality", "starting_tr_qx", "female_qx_proj_file",
          default = "data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv"
        )
        load_tr_qx_all_years(
          male_qx_file = male_proj_file,
          female_qx_file = female_proj_file,
          start_year = 2023, end_year = 2100, ages = 100:119
        )
      }
    ),

    # Core population projection
    # Note: va2_net_immigration and net_o_for_projection are defined in immigration_targets.R
    targets::tar_target(
      population_projection,
      run_population_projection(
        starting_population = starting_population$population,
        birth_rates = fertility_rates_complete,
        mortality_qx = mortality_qx_for_projection,
        net_lpr = net_lpr_immigration,
        net_o = net_o_for_projection,
        start_year = config_assumptions$projected_population$starting_year,
        end_year = config_assumptions$projected_population$projection_end,
        config = list(sex_ratio_at_birth = config_assumptions$projected_population$sex_ratio_at_birth,
                      population_status = config_assumptions$projected_population$population_status,
                      ages = config_assumptions$projected_population$ages),
        qx_100_119 = qx_100_119,
        tr2025_births = tr2025_implied_births,
        tr2025_births_years = config_assumptions$fertility$use_tr2025_births_for_years,
        verbose = TRUE
      )
    ),

    targets::tar_target(projected_population, population_projection$population),
    targets::tar_target(projected_births, population_projection$births),
    targets::tar_target(projected_deaths, population_projection$deaths),
    targets::tar_target(projected_net_immigration, population_projection$net_immigration),
    targets::tar_target(population_projection_summary, population_projection$summary),

    # ==========================================================================
    # PHASE 8C: MARITAL STATUS
    # ==========================================================================

    targets::tar_target(
      starting_marital_pop,
      extract_starting_marital_population(historical_population_marital = historical_population_marital,
                                           config = config_assumptions)
    ),

    targets::tar_target(
      marital_projection,
      run_marital_projection(
        phase8b_result = population_projection,
        starting_marital = starting_marital_pop,
        mortality_differentials = mortality_marital_factors,
        marriage_rates = marriage_projection,
        divorce_rates = divorce_projection,
        mortality_qx = mortality_qx_for_projection,
        start_year = config_assumptions$projected_population$starting_year,
        end_year = config_assumptions$projected_population$projection_end,
        min_age = config_assumptions$projected_population$ages$marriage_min,
        max_age = config_assumptions$projected_population$ages$max_age_group,
        verbose = TRUE
      )
    ),

    targets::tar_target(projected_marital_population, marital_projection$marital_population),
    targets::tar_target(marital_projection_summary, marital_projection$summary),

    targets::tar_target(
      marital_validation,
      {
        cli::cli_h2("Phase 8C Validation: Marital Status Totals")
        marital_pop <- marital_projection$marital_population
        phase8b_pop <- population_projection$population
        start_year <- config_assumptions$projected_population$starting_year
        first_proj_year <- start_year + 1
        marital_totals <- marital_pop[year >= first_proj_year,
                                       .(marital_total = sum(population, na.rm = TRUE)),
                                       by = .(year, age, sex)]
        min_age <- config_assumptions$projected_population$ages$marriage_min
        max_age <- config_assumptions$projected_population$ages$max_age_group
        phase8b_totals <- phase8b_pop[year >= first_proj_year & age >= min_age & age <= max_age,
                                       .(phase8b_total = sum(population, na.rm = TRUE)),
                                       by = .(year, age, sex)]
        comparison <- merge(marital_totals, phase8b_totals, by = c("year", "age", "sex"), all = TRUE)
        comparison[is.na(marital_total), marital_total := 0]
        comparison[is.na(phase8b_total), phase8b_total := 0]
        comparison[, diff := abs(marital_total - phase8b_total)]
        comparison[phase8b_total > 0, pct_diff := diff / phase8b_total * 100]
        max_pct_diff <- comparison[, max(pct_diff, na.rm = TRUE)]
        tolerance <- 0.01
        valid <- max_pct_diff < tolerance
        if (valid) cli::cli_alert_success("PASS: Marital totals match Phase 8B")
        else cli::cli_alert_warning("WARNING: Some differences > {tolerance}%")
        list(valid = valid, max_pct_diff = max_pct_diff, comparison = comparison)
      }
    ),

    # ==========================================================================
    # PHASE 8D: CHILDREN BY PARENT FATE
    # ==========================================================================

    targets::tar_target(
      children_fate_projection,
      {
        parent_age_groups <- list("14-24" = 14:24, "25-34" = 25:34, "35-44" = 35:44,
                                   "45-54" = 45:54, "55-64" = 55:64, "65-100" = 65:100)
        project_children_fate(
          phase8b_result = population_projection,
          marital_result = marital_projection,
          birth_rates = fertility_rates_for_projection,
          mortality_qx = mortality_qx_for_projection,
          parent_age_groups = parent_age_groups,
          start_year = config_assumptions$projected_population$starting_year,
          end_year = config_assumptions$projected_population$projection_end,
          max_child_age = config_assumptions$projected_population$ages$children_max,
          min_parent_age = config_assumptions$projected_population$ages$marriage_min,
          max_parent_age = config_assumptions$projected_population$ages$max_age,
          verbose = TRUE
        )
      }
    ),

    targets::tar_target(projected_children_fate, children_fate_projection$children_fate),
    targets::tar_target(children_fate_summary, children_fate_projection$summary),
    targets::tar_target(
      children_fate_validation,
      validate_children_fate(children_fate_result = children_fate_projection,
                              phase8b_result = population_projection,
                              max_child_age = config_assumptions$projected_population$ages$children_max,
                              tolerance = 0.01)
    ),

    # ==========================================================================
    # PHASE 8E: CNI POPULATION
    # ==========================================================================

    targets::tar_target(
      cni_projection,
      project_cni_population(
        phase8b_result = population_projection,
        marital_result = marital_projection,
        historical_cni = historical_civilian_noninst,
        start_year = config_assumptions$projected_population$starting_year,
        end_year = config_assumptions$projected_population$projection_end,
        verbose = TRUE
      )
    ),

    targets::tar_target(projected_cni_population, cni_projection$cni_population),
    targets::tar_target(cni_summary, cni_projection$summary),
    targets::tar_target(
      cni_validation,
      validate_cni_projection(cni_result = cni_projection, phase8b_result = population_projection, tolerance = 0.02)
    ),

    # ==========================================================================
    # PHASE 8F: INTEGRATION AND VALIDATION
    # ==========================================================================

    targets::tar_target(
      projected_population_validation,
      {
        projection_results <- list(
          population = population_projection$population,
          births = population_projection$births,
          deaths = population_projection$deaths,
          net_immigration = population_projection$net_immigration,
          population_marital = marital_projection$marital_population,
          children_fate = children_fate_projection$children_fate,
          cni_population = cni_projection$cni_population
        )
        tr2025_pop <- tryCatch({
          tr_pop_file <- here::here(config_assumptions$projected_population$tr_historical_population_file)
          if (file.exists(tr_pop_file)) {
            tr_pop <- data.table::fread(tr_pop_file)
            data.table::setnames(tr_pop, tolower(names(tr_pop)))
            if ("total" %in% names(tr_pop)) data.table::setnames(tr_pop, "total", "population")
            tr_pop
          } else NULL
        }, error = function(e) NULL)
        validate_projected_population_comprehensive(projection_results = projection_results,
                                                      tr2025_pop = tr2025_pop, tolerance = 0.02)
      }
    ),

    targets::tar_target(
      projected_population_summary,
      create_projection_summary(
        population = population_projection$population,
        births = population_projection$births,
        deaths = population_projection$deaths,
        net_immigration = population_projection$net_immigration,
        cni_summary = cni_projection$summary,
        start_year = config_assumptions$projected_population$starting_year,
        end_year = config_assumptions$projected_population$projection_end
      )
    )
  )
}

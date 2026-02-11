#' Historical Population Targets
#'
#' @description
#' Factory function that creates targets for the historical population subprocess (Section 1.4).
#'
#' @name historical_population_targets
NULL

#' Create historical population targets
#'
#' @description
#' Creates targets for historical population estimation (Section 1.4).
#' Implements Equations 1.4.1-1.4.4 for SS area population.
#'
#' @return List of targets for historical population subprocess
#'
#' @export
create_historical_population_targets <- function() {
  list(
    # ==========================================================================
    # HISTORICAL POPULATION (Equations 1.4.1-1.4.4)
    # ==========================================================================

    # Step 1: Historical Population by Age and Sex (Eq 1.4.1)
    targets::tar_target(
      historical_population,
      calculate_historical_population(
        start_year = config_assumptions$historical_population$start_year,
        end_year = config_assumptions$historical_population$end_year,
        ages = 0:config_assumptions$historical_population$max_age,
        config = config_assumptions,
        lpr_assumptions = lpr_assumptions,
        immigration_dist = lpr_distribution,
        emigration_dist = emigration_distribution,
        births_by_sex = nchs_births_by_sex,
        use_cache = TRUE
      )
    ),

    # Step 2: Historical Population by Marital Status (Eq 1.4.2)
    targets::tar_target(
      historical_population_marital,
      calculate_historical_population_marital(
        total_pop = historical_population,
        start_year = config_assumptions$historical_population$start_year,
        end_year = config_assumptions$historical_population$end_year,
        ages = 14:config_assumptions$historical_population$max_age,
        config = config_assumptions,
        use_cache = TRUE,
        include_same_sex = TRUE
      )
    ),

    # Step 3: Temporary/Unlawfully Present Population (Eq 1.4.3)
    targets::tar_target(
      historical_temp_unlawful,
      calculate_historical_temp_unlawful(
        start_year = config_assumptions$historical_population$start_year,
        end_year = config_assumptions$historical_population$end_year,
        ages = 0:config_assumptions$historical_population$max_age,
        config = config_assumptions,
        use_cache = TRUE
      )
    ),

    # Step 4: Civilian Noninstitutionalized Population (Eq 1.4.4)
    targets::tar_target(
      historical_civilian_noninst,
      calculate_historical_civilian_noninst(
        start_year = config_assumptions$historical_population$cni_start_year,
        end_year = config_assumptions$historical_population$end_year,
        ages = 0:config_assumptions$historical_population$max_age,
        config = config_assumptions,
        include_orientation = TRUE,
        use_cache = TRUE
      )
    ),

    # ==========================================================================
    # VALIDATION TARGETS
    # ==========================================================================

    # Age-sex population vs SSPopDec
    targets::tar_target(
      historical_age_sex_validation,
      validate_age_sex_vs_tr(
        population = historical_population,
        config = config_assumptions,
        tolerance = 0.02
      )
    ),

    # Ages 85+ detail vs SSPopDec
    targets::tar_target(
      historical_85plus_validation,
      validate_85plus_vs_tr(
        population = historical_population,
        config = config_assumptions
      )
    ),

    # Marital proportions vs SSPopDec
    targets::tar_target(
      historical_marital_validation,
      validate_marital_proportions_vs_tr(
        marital_pop = historical_population_marital,
        config = config_assumptions
      )
    ),

    # Population component accounting (year-over-year plausibility)
    targets::tar_target(
      historical_component_validation,
      validate_population_components(
        population = historical_population,
        config = config_assumptions
      )
    ),

    # Sex ratio plausibility
    targets::tar_target(
      historical_sex_ratio_validation,
      validate_sex_ratios(
        population = historical_population
      )
    ),

    # O-population distribution validation
    targets::tar_target(
      historical_o_pop_validation,
      validate_o_age_distribution(
        o_pop = historical_temp_unlawful,
        config = config_assumptions
      )
    ),

    # Combined validation summary
    targets::tar_target(
      historical_population_validation,
      {
        results <- list(
          age_sex = historical_age_sex_validation,
          plus_85 = historical_85plus_validation,
          marital = historical_marital_validation,
          components = historical_component_validation,
          sex_ratios = historical_sex_ratio_validation,
          o_pop = historical_o_pop_validation
        )

        all_valid <- all(vapply(results, function(r) isTRUE(r$valid), logical(1)))

        cli::cli_h1("Historical Population Validation Summary")
        for (nm in names(results)) {
          status <- if (isTRUE(results[[nm]]$valid)) "PASS" else if (is.na(results[[nm]]$valid)) "SKIP" else "FAIL"
          icon <- if (status == "PASS") cli::symbol$tick else if (status == "SKIP") "-" else cli::symbol$cross
          cli::cli_alert("{icon} {nm}: {status}")
        }

        list(all_valid = all_valid, results = results)
      }
    )
  )
}

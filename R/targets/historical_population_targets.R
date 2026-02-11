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
        start_year = 1940,
        end_year = 2022,
        ages = 0:100,
        config = config_assumptions,
        use_cache = TRUE
      )
    ),

    # Step 2: Historical Population by Marital Status (Eq 1.4.2)
    targets::tar_target(
      historical_population_marital,
      calculate_historical_population_marital(
        total_pop = historical_population,
        start_year = 1940,
        end_year = 2022,
        ages = 14:100,
        config = config_assumptions,
        use_cache = TRUE,
        include_same_sex = TRUE
      )
    ),

    # Step 3: Temporary/Unlawfully Present Population (Eq 1.4.3)
    targets::tar_target(
      historical_temp_unlawful,
      calculate_historical_temp_unlawful(
        start_year = 1940,
        end_year = 2022,
        ages = 0:100,
        config = config_assumptions,
        use_cache = TRUE
      )
    ),

    # Step 4: Civilian Noninstitutionalized Population (Eq 1.4.4)
    targets::tar_target(
      historical_civilian_noninst,
      calculate_historical_civilian_noninst(
        start_year = 2010,
        end_year = 2022,
        ages = 0:100,
        config = config_assumptions,
        include_orientation = TRUE,
        use_cache = TRUE
      )
    ),

    # ==========================================================================
    # VALIDATION TARGETS
    # ==========================================================================

    # TR2025 December 31 population
    targets::tar_target(
      tr2025_population_dec,
      {
        file_path <- here::here("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")
        if (file.exists(file_path)) {
          data.table::fread(file_path)
        } else {
          cli::cli_warn("TR2025 population file not found: {file_path}")
          NULL
        }
      }
    ),

    # Validate historical population against TR2025
    targets::tar_target(
      historical_population_validation,
      validate_year_totals_vs_tr(
        population = historical_population,
        tr2025_pop = tr2025_population_dec,
        tolerance = 0.02
      )
    )
  )
}

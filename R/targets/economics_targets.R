#' Economics Targets
#'
#' @description
#' Factory function that creates targets for the Economics process.
#' Currently implements Subprocess 2.1: U.S. Employment (USEMP).
#'
#' @name economics_targets
NULL

#' Create economics targets
#'
#' @description
#' Creates targets for the U.S. Employment subprocess (USEMP),
#' which projects unemployment rates, labor force participation rates,
#' labor force, employment, and employed OP.
#'
#' @return List of targets for the economics pipeline
#'
#' @export
create_economics_targets <- function() {
  list(
    # ══════════════════════════════════════════════════════════════════
    # DATA ACQUISITION
    # ══════════════════════════════════════════════════════════════════

    # TR2025 projected economic assumptions (V.B1, V.B2)
    targets::tar_target(
      tr2025_economic_assumptions,
      load_tr2025_economic_assumptions(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # TR2025 DI prevalence (V.C5)
    targets::tar_target(
      tr2025_di_prevalence,
      load_tr2025_di_prevalence(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # TR2025 benefit parameters (V.C7)
    targets::tar_target(
      tr2025_benefit_params,
      load_tr2025_benefit_params(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Historical economic data from APIs (BLS, BEA, FRED)
    targets::tar_target(
      historical_economic_assumptions,
      fetch_historical_economic_assumptions(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # BLS CPS labor force data by age/sex
    targets::tar_target(
      bls_cps_labor_force,
      fetch_cps_labor_force_by_age_sex(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # CPS educational attainment
    targets::tar_target(
      cps_educational_attainment,
      fetch_cps_educational_attainment(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Load coefficient files
    targets::tar_target(
      unemployment_coefficients,
      yaml::read_yaml(here::here("config/coefficients/unemployment_coefficients.yaml"))
    ),

    targets::tar_target(
      lfpr_coefficients,
      yaml::read_yaml(here::here("config/coefficients/lfpr_coefficients.yaml"))
    ),

    targets::tar_target(
      eo_parameters,
      yaml::read_yaml(here::here("config/coefficients/eo_parameters.yaml"))
    ),

    # ══════════════════════════════════════════════════════════════════
    # INPUT VARIABLE CONSTRUCTION
    # ══════════════════════════════════════════════════════════════════

    # Build all employment input variables
    targets::tar_target(
      employment_inputs,
      build_employment_inputs(
        projected_population = projected_population,
        projected_cni_population = projected_cni_population,
        projected_marital_population = projected_marital_population,
        o_population_stock = o_population_stock,
        military_population = armed_forces_for_projection,
        tr2025_economic_assumptions = tr2025_economic_assumptions,
        tr2025_di_prevalence = tr2025_di_prevalence,
        tr2025_benefit_params = tr2025_benefit_params,
        cps_education = cps_educational_attainment,
        config_employment = config_economics$employment
      )
    ),

    # ══════════════════════════════════════════════════════════════════
    # CORE PROJECTIONS (Eqs 2.1.1-2.1.6)
    # ══════════════════════════════════════════════════════════════════

    # Unemployment rate projection (Eq 2.1.3)
    targets::tar_target(
      unemployment_projection,
      project_unemployment_rates(
        rtp_quarterly = employment_inputs$rtp_quarterly,
        base_year_labor_force = bls_cps_labor_force[
          year == config_economics$employment$base_year & concept == "labor_force",
          .(age_group, sex, labor_force = value)
        ],
        historical_ru = bls_cps_labor_force[
          concept == "unemployment_rate",
          .(year, quarter = 1L, age_group, sex, rate = value)
        ],
        target_ru = tr2025_economic_assumptions[
          variable == "unemployment_rate",
          .(year, quarter = 1L, rate = value)
        ],
        ru_coefficients = unemployment_coefficients,
        config_employment = config_economics$employment
      )
    ),

    # LFPR projection (Eq 2.1.4)
    targets::tar_target(
      lfpr_projection,
      project_lfpr(
        unemployment_rates = unemployment_projection,
        cni_population = employment_inputs$quarterly_cni_pop,
        employment_inputs = employment_inputs,
        lfpr_coefficients = lfpr_coefficients,
        historical_lfpr = bls_cps_labor_force[concept == "lfpr"],
        config_employment = config_economics$employment
      )
    ),

    # Labor force and employment projection (Eqs 2.1.5-2.1.6)
    targets::tar_target(
      labor_force_employment,
      project_labor_force_employment(
        lfpr_projection = lfpr_projection,
        unemployment_projection = unemployment_projection,
        quarterly_cni_pop = employment_inputs$quarterly_cni_pop
      )
    ),

    # ══════════════════════════════════════════════════════════════════
    # EMPLOYED OP (Eqs 2.1.7-2.1.19)
    # ══════════════════════════════════════════════════════════════════

    # EO by visa status (Eqs 2.1.7-2.1.10)
    targets::tar_target(
      employed_op_projection,
      project_employed_op(
        quarterly_op = employment_inputs$quarterly_op,
        employment_projection = labor_force_employment$employment,
        quarterly_cni_pop = employment_inputs$quarterly_cni_pop,
        eo_params = eo_parameters,
        config_employment = config_economics$employment
      )
    ),

    # EO by earnings recording status (Eqs 2.1.11-2.1.14)
    targets::tar_target(
      eo_earnings_recording,
      split_eo_by_earnings_recording(
        eo_projection = employed_op_projection,
        eo_params = eo_parameters
      )
    ),

    # At-any-time employment TEO (Eqs 2.1.15-2.1.19)
    targets::tar_target(
      teo_projection,
      compute_teo(
        eo_by_recording = eo_earnings_recording,
        eo_params = eo_parameters
      )
    ),

    # ══════════════════════════════════════════════════════════════════
    # VALIDATION
    # ══════════════════════════════════════════════════════════════════

    targets::tar_target(
      employment_validation,
      validate_employment_projections(
        unemployment_projection = unemployment_projection,
        lfpr_projection = lfpr_projection,
        labor_force_employment = labor_force_employment,
        employed_op = employed_op_projection,
        teo = teo_projection,
        tr2025_assumptions = tr2025_economic_assumptions,
        config_employment = config_economics$employment
      )
    )
  )
}

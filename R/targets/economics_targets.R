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

    # TR projected economic assumptions (V.B1, V.B2)
    # Depends on config_economics gate so economic_alternative changes trigger rebuild
    targets::tar_target(
      tr_economic_assumptions,
      {
        force(config_economics)
        load_tr_economic_assumptions(config_assumptions)
      },
      cue = targets::tar_cue(mode = "thorough")
    ),

    # TR DI prevalence (V.C5)
    targets::tar_target(
      tr_di_prevalence,
      load_tr_di_prevalence(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # TR benefit parameters (V.C7)
    targets::tar_target(
      tr_benefit_params,
      load_tr_benefit_params(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # TR economic levels (VI.G6 — AWI, CPI, GDP, interest)
    targets::tar_target(
      tr_economic_levels,
      load_tr_vig6(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Historical economic data from APIs (BLS, BEA, FRED)
    targets::tar_target(
      historical_economic_assumptions,
      fetch_historical_economic_assumptions(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # CPS ASEC labor force data via IPUMS (Inputs #24, #25, #26/#27, #29)
    # Provides: unemployment rates, LFPRs, labor force levels by
    # 14 age groups × sex, single-year-of-age 55-79, marital status,
    # children under 6, and educational attainment — all from one extract
    targets::tar_target(
      cps_labor_force,
      fetch_cps_labor_force(config_assumptions),
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

    # DI age-sex profile from published SSA data (DI ASR + Supplement)
    targets::tar_target(
      di_age_profile,
      load_di_age_profile(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # Historical quarterly RTP from FRED (GDPC1/GDPPOT)
    # Provides D(RTP) lags for Q1 of first projection year
    targets::tar_target(
      historical_rtp,
      fetch_fred_quarterly_rtp(config_assumptions),
      cue = targets::tar_cue(mode = "thorough")
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
        projected_mothers_child_under6 = projected_mothers_child_under6,
        o_population_stock = o_population_stock,
        military_population = armed_forces_for_projection,
        tr_economic_assumptions = tr_economic_assumptions,
        tr_economic_levels = tr_economic_levels,
        tr_di_prevalence = tr_di_prevalence,
        tr_benefit_params = tr_benefit_params,
        di_age_profile = di_age_profile,
        cps_labor_data = cps_labor_force,
        historical_rtp = historical_rtp,
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
        base_year_labor_force = cps_labor_force[
          year == config_economics$employment$base_year &
            concept == "labor_force" &
            marital_status == "all" &
            is.na(age),
          .(age_group, sex, labor_force = value)
        ],
        historical_ru = cps_labor_force[
          concept == "unemployment_rate" & marital_status == "all" &
            is.na(age),  # Exclude single-year-of-age rows
          .(year, quarter = 1L, age_group, sex, rate = value)
        ],
        target_ru = {
          ru_annual <- unique(tr_economic_assumptions[
            variable == "unemployment_rate", .(year, rate = value)])
          # Apply user's ultimate UR override (if different from V.B2 terminal)
          ru_annual <- override_unemployment_path(ru_annual, config_economics$employment)
          # Extend past TR data endpoint if projection end year exceeds it
          proj_end <- config_economics$employment$end_year
          max_ru_year <- max(ru_annual$year)
          if (!is.null(proj_end) && proj_end > max_ru_year) {
            last_rate <- ru_annual[year == max_ru_year, rate]
            extension <- data.table::data.table(
              year = (max_ru_year + 1L):proj_end, rate = last_rate
            )
            ru_annual <- data.table::rbindlist(list(ru_annual, extension))
          }
          data.table::rbindlist(lapply(1:4, function(q) {
            data.table::copy(ru_annual)[, quarter := q]
          }))
        },
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
        historical_lfpr = cps_labor_force[concept == "lfpr"],
        historical_ru = cps_labor_force[
          concept == "unemployment_rate" & marital_status == "all" &
            is.na(age),
          .(year, age_group, sex, rate = value)
        ],
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
        tr_assumptions = tr_economic_assumptions,
        config_employment = config_economics$employment
      )
    )
  )
}

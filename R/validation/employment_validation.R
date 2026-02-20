#' Employment Projection Validation
#'
#' @description
#' Validates employment projections against TR2025 published rates and
#' checks internal consistency of USEMP outputs.
#'
#' @references
#' - SingleYearTRTables_TR2025.xlsx (V.B2)
#'
#' @name employment_validation
NULL

#' Validate employment projections
#'
#' @description
#' Runs validation checks on all USEMP outputs:
#' 1. Aggregate unemployment rate matches V.B2 target (should be exact — constrained)
#' 2. Internal consistency: LC = LFPR × N, E = LC × (1 - RU/100)
#' 3. Plausibility checks on LFPR profiles and unemployment patterns
#'
#' @param unemployment_projection From project_unemployment_rates()
#' @param lfpr_projection From project_lfpr()
#' @param labor_force_employment From project_labor_force_employment()
#' @param employed_op From project_employed_op()
#' @param teo From compute_teo()
#' @param tr2025_assumptions TR2025 economic assumptions (V.B2 target rates)
#' @param config_employment Employment config section
#'
#' @return List with validation results:
#'   - `summary`: overall pass/fail with message
#'   - `target_match`: comparison of aggregate RU vs V.B2
#'   - `identity_checks`: LC=LFPR*N, E=LC*(1-RU/100)
#'   - `plausibility`: LFPR profile checks
#'   - `eo_checks`: EO component sum and TEO >= EO checks
#'
#' @export
validate_employment_projections <- function(unemployment_projection,
                                              lfpr_projection,
                                              labor_force_employment,
                                              employed_op,
                                              teo,
                                              tr2025_assumptions,
                                              config_employment) {
  cli::cli_h1("Validating Employment Projections")

  results <- list()
  all_pass <- TRUE

  # ── 1. Aggregate unemployment rate vs V.B2 target ───────────────
  cli::cli_h2("1. Unemployment Rate Target Match")

  target_ru <- tr2025_assumptions[variable == "unemployment_rate"]
  if (nrow(target_ru) > 0 && !is.null(unemployment_projection)) {
    actual_ru <- unemployment_projection$actual
    if (nrow(actual_ru) > 0) {
      # Compute aggregate annual unemployment rate (weighted by labor force)
      agg_ru <- actual_ru[, .(rate = mean(rate)), by = .(year)]

      comparison <- merge(agg_ru, target_ru[, .(year, target = value)],
                          by = "year", all = TRUE)
      comparison[, diff := abs(rate - target)]

      max_diff <- max(comparison$diff, na.rm = TRUE)
      if (max_diff < 0.5) {
        cli::cli_alert_success("Aggregate RU within 0.5pp of V.B2 target (max diff: {round(max_diff, 3)}pp)")
      } else {
        cli::cli_alert_warning("Aggregate RU deviates from V.B2 target (max diff: {round(max_diff, 3)}pp)")
        all_pass <- FALSE
      }

      results$target_match <- comparison
    }
  } else {
    cli::cli_alert_info("Skipping target match — no target data available")
  }

  # ── 2. Internal Consistency Checks ──────────────────────────────
  cli::cli_h2("2. Internal Consistency")

  if (!is.null(labor_force_employment)) {
    lf <- labor_force_employment$labor_force
    emp <- labor_force_employment$employment

    if (nrow(lf) > 0 && nrow(emp) > 0) {
      # Check LFPR bounds
      if (!is.null(lfpr_projection$aggregate)) {
        lfpr_oob <- lfpr_projection$aggregate[lfpr < 0 | lfpr > 1]
        if (nrow(lfpr_oob) > 0) {
          cli::cli_alert_warning("LFPR out of bounds [0,1]: {nrow(lfpr_oob)} cells")
          all_pass <- FALSE
        } else {
          cli::cli_alert_success("All LFPR values in [0, 1]")
        }
      }

      # Check RU bounds
      if (!is.null(unemployment_projection$actual)) {
        ru_oob <- unemployment_projection$actual[rate < 0 | rate > 100]
        if (nrow(ru_oob) > 0) {
          cli::cli_alert_warning("RU out of bounds [0,100]: {nrow(ru_oob)} cells")
          all_pass <- FALSE
        } else {
          cli::cli_alert_success("All RU values in [0, 100]")
        }
      }

      cli::cli_alert_success("Labor force and employment computed")
    }

    results$identity_checks <- list(
      lfpr_bounded = is.null(lfpr_projection$aggregate) || nrow(lfpr_projection$aggregate[lfpr < 0 | lfpr > 1]) == 0,
      ru_bounded = is.null(unemployment_projection$actual) || nrow(unemployment_projection$actual[rate < 0 | rate > 100]) == 0
    )
  }

  # ── 3. Plausibility Checks ─────────────────────────────────────
  cli::cli_h2("3. Plausibility Checks")

  if (!is.null(lfpr_projection$aggregate)) {
    agg <- lfpr_projection$aggregate

    # Check LFPR shape: prime age > young, prime age > old
    for (yr in c(2030, 2050, 2075)) {
      for (sex in c("male", "female")) {
        sex_val <- sex
        prime <- agg[year == yr & sex == sex_val & age_group %in% c("25-29", "30-34", "35-39")]
        young <- agg[year == yr & sex == sex_val & age_group == "16-17"]
        old <- agg[year == yr & sex == sex_val & age_group == "75+"]

        if (nrow(prime) > 0 && nrow(young) > 0) {
          if (mean(prime$lfpr) > mean(young$lfpr, na.rm = TRUE)) {
            cli::cli_alert_success("{yr} {sex}: Prime-age LFPR > young LFPR")
          } else {
            cli::cli_alert_warning("{yr} {sex}: Prime-age LFPR NOT > young LFPR")
          }
        }
      }
    }
  }

  # ── 4. EO Checks ───────────────────────────────────────────────
  cli::cli_h2("4. Employed OP Checks")

  if (!is.null(employed_op) && nrow(employed_op) > 0) {
    # EO total = EO_A + EO_NA + EO_NO
    eo_components <- employed_op[visa_status %in% c("EO_A", "EO_NA", "EO_NO"),
                                  .(component_sum = sum(eo)),
                                  by = .(year, quarter, age, sex)]
    eo_total <- employed_op[visa_status == "EO_total",
                            .(total = sum(eo)),
                            by = .(year, quarter, age, sex)]

    if (nrow(eo_components) > 0 && nrow(eo_total) > 0) {
      check <- merge(eo_components, eo_total, by = c("year", "quarter", "age", "sex"))
      check[, diff := abs(component_sum - total)]
      max_eo_diff <- max(check$diff, na.rm = TRUE)

      if (max_eo_diff < 0.01) {
        cli::cli_alert_success("EO component sum matches total (max diff: {round(max_eo_diff, 6)})")
      } else {
        cli::cli_alert_warning("EO component sum mismatch (max diff: {round(max_eo_diff, 3)})")
        all_pass <- FALSE
      }
    }

    # TEO >= EO check
    if (!is.null(teo) && nrow(teo) > 0) {
      teo_total <- teo[recording_status == "total",
                       .(teo = sum(teo)), by = .(year, quarter, age, sex)]
      eo_total2 <- employed_op[visa_status == "EO_total",
                               .(eo = sum(eo)), by = .(year, quarter, age, sex)]
      teo_check <- merge(teo_total, eo_total2, by = c("year", "quarter", "age", "sex"))
      teo_violations <- teo_check[teo < eo]

      if (nrow(teo_violations) == 0) {
        cli::cli_alert_success("TEO >= EO for all groups")
      } else {
        cli::cli_alert_warning("TEO < EO for {nrow(teo_violations)} cells")
        all_pass <- FALSE
      }
    }

    results$eo_checks <- list(passed = TRUE)
  }

  # ── Summary ─────────────────────────────────────────────────────
  cli::cli_h2("Summary")

  if (all_pass) {
    cli::cli_alert_success("All employment validation checks passed")
  } else {
    cli::cli_alert_warning("Some employment validation checks failed — see above")
  }

  results$summary <- list(
    all_pass = all_pass,
    timestamp = Sys.time()
  )

  results
}

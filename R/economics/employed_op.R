#' Employed OP (EO) and At-Any-Time Employed OP (TEO) Projection
#'
#' @description
#' Projects employment for the temporary/unlawfully present population (OP),
#' disaggregated by visa status and earnings recording status.
#' Implements Equations 2.1.7–2.1.19 from the USEMP documentation.
#'
#' @references
#' - 2025_LR_Model_Documentation_Economics_1_USEmployment.md (Eqs 2.1.7-2.1.19)
#'
#' @name employed_op
NULL

# =============================================================================
# EO Components (Eqs 2.1.7-2.1.10)
# =============================================================================

#' Project employed OP by visa status
#'
#' @description
#' Computes employed OP (EO) for each visa status component:
#' - EO_A: Authorized temporary (Eq 2.1.7) — visa-specific employment ratios
#' - EO_NA: Overstayed authorization (Eq 2.1.8) — uses civilian E/N ratio
#' - EO_NO: Never authorized (Eq 2.1.9) — uses civilian E/N ratio
#' - EO: Total (Eq 2.1.10) — sum of components
#'
#' @param quarterly_op Quarterly OP population by age, sex, visa_status
#' @param employment_projection Civilian employment by age group and sex
#' @param quarterly_cni_pop Civilian noninstitutional population
#' @param eo_params EO parameters from eo_parameters.yaml
#' @param config_employment Employment config section
#'
#' @return data.table with columns: year, quarter, age, sex, visa_status, eo
#'
#' @references Eqs 2.1.7-2.1.10
#' @export
project_employed_op <- function(quarterly_op,
                                 employment_projection,
                                 quarterly_cni_pop,
                                 eo_params,
                                 config_employment) {
  checkmate::assert_data_table(quarterly_op)
  checkmate::assert_data_table(employment_projection)
  checkmate::assert_data_table(quarterly_cni_pop)
  checkmate::assert_list(eo_params)

  if (!isTRUE(config_employment$employed_op$enabled)) {
    cli::cli_alert_warning("Employed OP projection disabled in config")
    return(data.table::data.table(
      year = integer(), quarter = integer(), age = integer(),
      sex = character(), visa_status = character(), eo = numeric()
    ))
  }

  cli::cli_alert_info("Projecting employed OP (EO)")

  # Compute civilian employment-to-population ratio (E/N) by age group and sex
  en_ratio <- merge(
    employment_projection[, .(year, quarter, age_group, sex, employment)],
    quarterly_cni_pop[, .(year, quarter, age_group = as.character(age), sex, population)],
    by = c("year", "quarter", "age_group", "sex"),
    all.x = TRUE
  )
  en_ratio[, e_n_ratio := fifelse(population > 0, employment / population, 0)]

  results <- list()

  # EO_A: Authorized temporary — apply visa-specific employment rates
  op_a <- quarterly_op[visa_status == "OP_A"]
  if (nrow(op_a) > 0) {
    # Use weighted average of visa subcategory employment rates
    eo_a_ratios <- eo_params$eo_a_ratios
    avg_emp_rate <- mean(vapply(eo_a_ratios, function(x) x$employment_rate, numeric(1)))
    op_a[, eo := population * avg_emp_rate]
    op_a[, visa_status := "EO_A"]
    results$eo_a <- op_a[, .(year, quarter, age, sex, visa_status, eo)]
  }

  # EO_NA: Overstayed — same E/N ratio as civilian (Eq 2.1.8)
  op_na <- quarterly_op[visa_status == "OP_NA"]
  if (nrow(op_na) > 0) {
    op_na <- merge(op_na, en_ratio[, .(year, quarter, age_group, sex, e_n_ratio)],
                   by.x = c("year", "quarter", "age", "sex"),
                   by.y = c("year", "quarter", "age_group", "sex"),
                   all.x = TRUE)
    op_na[is.na(e_n_ratio), e_n_ratio := 0]
    op_na[, eo := population * e_n_ratio]
    op_na[, visa_status := "EO_NA"]
    results$eo_na <- op_na[, .(year, quarter, age, sex, visa_status, eo)]
  }

  # EO_NO: Never authorized — same E/N ratio as civilian (Eq 2.1.9)
  op_no <- quarterly_op[visa_status == "OP_NO"]
  if (nrow(op_no) > 0) {
    op_no <- merge(op_no, en_ratio[, .(year, quarter, age_group, sex, e_n_ratio)],
                   by.x = c("year", "quarter", "age", "sex"),
                   by.y = c("year", "quarter", "age_group", "sex"),
                   all.x = TRUE)
    op_no[is.na(e_n_ratio), e_n_ratio := 0]
    op_no[, eo := population * e_n_ratio]
    op_no[, visa_status := "EO_NO"]
    results$eo_no <- op_no[, .(year, quarter, age, sex, visa_status, eo)]
  }

  eo_all <- data.table::rbindlist(results, fill = TRUE)

  # EO total (Eq 2.1.10)
  eo_total <- eo_all[, .(eo = sum(eo)), by = .(year, quarter, age, sex)]
  eo_total[, visa_status := "EO_total"]
  eo_all <- data.table::rbindlist(list(eo_all, eo_total), fill = TRUE)

  cli::cli_alert_success("Projected EO: {nrow(eo_all)} rows")

  eo_all
}

# =============================================================================
# Earnings Recording Status (Eqs 2.1.11-2.1.14)
# =============================================================================

#' Split EO by earnings recording status
#'
#' @description
#' Splits employed OP into earnings recording categories:
#' - EO_MEF: Earnings posted to Master Earnings File (Eq 2.1.11)
#' - EO_MEFC: MEF earnings that are OASDI covered (Eq 2.1.12)
#' - EO_ESF: Earnings posted to Earnings Suspense File (Eq 2.1.13)
#' - EO_UND: Underground economy (Eq 2.1.14)
#'
#' @param eo_projection EO by visa status from project_employed_op()
#' @param eo_params EO parameters from eo_parameters.yaml
#'
#' @return data.table with year, quarter, age, sex, visa_status, recording_status, eo
#'
#' @references Eqs 2.1.11-2.1.14
#' @export
split_eo_by_earnings_recording <- function(eo_projection, eo_params) {
  checkmate::assert_data_table(eo_projection)
  checkmate::assert_list(eo_params)

  cli::cli_alert_info("Splitting EO by earnings recording status")

  results <- list()

  # EO_A: Use visa-specific MEF posting rates
  eo_a <- eo_projection[visa_status == "EO_A"]
  if (nrow(eo_a) > 0) {
    eo_a_ratios <- eo_params$eo_a_ratios
    avg_mef_rate <- mean(vapply(eo_a_ratios, function(x) x$mef_posting_rate, numeric(1)))
    avg_coverage <- mean(vapply(eo_a_ratios, function(x) x$oasdi_coverage, numeric(1)))

    eo_a_mef <- data.table::copy(eo_a)
    eo_a_mef[, `:=`(recording_status = "MEF", eo = eo * avg_mef_rate)]
    eo_a_mefc <- data.table::copy(eo_a)
    eo_a_mefc[, `:=`(recording_status = "MEFC", eo = eo * avg_mef_rate * avg_coverage)]
    # ESF and UND minimal for authorized workers
    eo_a_esf <- data.table::copy(eo_a)
    eo_a_esf[, `:=`(recording_status = "ESF", eo = eo * 0.02)]
    eo_a_und <- data.table::copy(eo_a)
    eo_a_und[, `:=`(recording_status = "UND", eo = eo * (1 - avg_mef_rate - 0.02))]

    results <- c(results, list(eo_a_mef, eo_a_mefc, eo_a_esf, eo_a_und))
  }

  # EO_NA: Use eo_na_splits
  eo_na <- eo_projection[visa_status == "EO_NA"]
  if (nrow(eo_na) > 0) {
    na_splits <- eo_params$eo_na_splits
    for (status in c("MEF", "ESF")) {
      rate_key <- tolower(paste0(status, "_posting_rate"))
      dt <- data.table::copy(eo_na)
      dt[, `:=`(recording_status = status, eo = eo * na_splits[[rate_key]])]
      results <- c(results, list(dt))
    }
    # MEFC
    dt_mefc <- data.table::copy(eo_na)
    dt_mefc[, `:=`(recording_status = "MEFC",
                   eo = eo * na_splits$mef_posting_rate * na_splits$oasdi_coverage_of_mef)]
    results <- c(results, list(dt_mefc))
    # UND
    dt_und <- data.table::copy(eo_na)
    dt_und[, `:=`(recording_status = "UND",
                  eo = eo * (1 - na_splits$mef_posting_rate - na_splits$esf_posting_rate))]
    results <- c(results, list(dt_und))
  }

  # EO_NO: Use eo_no_splits (distinguish pre-2002 vs post-2001)
  eo_no <- eo_projection[visa_status == "EO_NO"]
  if (nrow(eo_no) > 0) {
    no_splits <- eo_params$eo_no_splits
    # Use post_2001 rates as default (most workers by projection period)
    splits <- no_splits$post_2001
    for (status in c("MEF", "ESF")) {
      rate_key <- tolower(paste0(status, "_posting_rate"))
      dt <- data.table::copy(eo_no)
      dt[, `:=`(recording_status = status, eo = eo * splits[[rate_key]])]
      results <- c(results, list(dt))
    }
    dt_mefc <- data.table::copy(eo_no)
    dt_mefc[, `:=`(recording_status = "MEFC",
                   eo = eo * splits$mef_posting_rate * splits$oasdi_coverage_of_mef)]
    results <- c(results, list(dt_mefc))
    dt_und <- data.table::copy(eo_no)
    dt_und[, `:=`(recording_status = "UND",
                  eo = eo * (1 - splits$mef_posting_rate - splits$esf_posting_rate))]
    results <- c(results, list(dt_und))
  }

  result <- data.table::rbindlist(results, fill = TRUE)

  cli::cli_alert_success("Split EO into {length(unique(result$recording_status))} recording statuses: {nrow(result)} rows")

  result
}

# =============================================================================
# At-Any-Time Employment TEO (Eqs 2.1.15-2.1.19)
# =============================================================================

#' Convert EO to at-any-time employment TEO
#'
#' @description
#' Converts average weekly employment (EO) to at-any-time employment (TEO)
#' using age-sex conversion weights.
#'
#' @param eo_by_recording EO by recording status from split_eo_by_earnings_recording()
#' @param eo_params EO parameters with conversion weights
#'
#' @return data.table with year, quarter, age, sex, visa_status, recording_status, teo
#'
#' @references Eqs 2.1.15-2.1.19
#' @export
compute_teo <- function(eo_by_recording, eo_params) {
  checkmate::assert_data_table(eo_by_recording)
  checkmate::assert_list(eo_params)

  cli::cli_alert_info("Computing at-any-time employment (TEO)")

  weights <- eo_params$teo_conversion_weights
  dt <- data.table::copy(eo_by_recording)

  # Map age to weight bracket
  dt[, age_num := as.integer(age)]
  dt[, weight_group := fcase(
    age_num <= 19, "16-19",
    age_num <= 24, "20-24",
    age_num <= 54, "25-54",
    age_num <= 64, "55-64",
    default = "65+"
  )]

  # Apply conversion weights by sex and age group
  for (s in c("male", "female")) {
    sex_weights <- weights[[s]]
    for (wg in names(sex_weights)) {
      w <- sex_weights[[wg]]
      dt[sex == s & weight_group == wg, teo := eo * w]
    }
  }

  # For authorized temporary workers, use special factors
  auth_factors <- weights$authorized_temporary
  if (!is.null(auth_factors)) {
    dt[visa_status == "EO_A", teo := eo * auth_factors$full_year_factor]
  }

  dt[is.na(teo), teo := eo * 1.25]  # Default fallback

  result <- dt[, .(year, quarter, age, sex, visa_status, recording_status, teo)]

  # TEO total (Eq 2.1.19): sum across MEF, ESF, UND
  teo_total <- result[recording_status %in% c("MEF", "ESF", "UND"),
                      .(teo = sum(teo)), by = .(year, quarter, age, sex, visa_status)]
  teo_total[, recording_status := "total"]
  result <- data.table::rbindlist(list(result, teo_total), fill = TRUE)

  cli::cli_alert_success("Computed TEO: {nrow(result)} rows")

  result
}

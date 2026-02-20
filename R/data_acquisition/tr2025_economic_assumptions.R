#' TR2025 Economic Assumptions Loader
#'
#' @description
#' Loads projected economic assumptions from TR2025 SingleYear tables.
#' Provides data from sheets V.B1, V.B2, V.C5, V.C7, and VI.G6.
#'
#' When `assumptions_data_source = "tr2025"`, also provides historical data
#' from these tables, bypassing API fetching entirely.
#'
#' @references
#' - SingleYearTRTables_TR2025.xlsx (V.B1, V.B2, V.C5, V.C7, VI.G6)
#'
#' @name tr2025_economic_assumptions
NULL

# =============================================================================
# V.B1: Economic Assumptions — Growth Rates
# =============================================================================

#' Load TR2025 Table V.B1 (Economic Assumptions)
#'
#' @description
#' Loads productivity growth, GDP deflator, average hours, earnings/compensation
#' ratio, and CPI from the SingleYearTRTables V.B1 sheet.
#'
#' @param config List with metadata section (for TR data directory)
#' @param years_filter Optional integer vector to filter specific years
#'
#' @return data.table with columns: year, variable, value
#'   Variables: productivity, gdp_deflator, avg_hours, earnings_compensation_ratio,
#'             real_wage, nominal_earnings, cpi
#'
#' @export
load_tr2025_vb1 <- function(config, years_filter = NULL) {
  tr_dir <- get_tr_data_dir(config)
  file_path <- file.path(tr_dir, "SingleYearTRTables_TR2025.xlsx")

  if (!file.exists(file_path)) {
    cli::cli_abort(c(
      "TR2025 SingleYear tables not found",
      "x" = "Expected: {.file {file_path}}",
      "i" = "Place TR2025 data files in {.file {tr_dir}}"
    ))
  }

  cli::cli_alert_info("Loading TR2025 V.B1 (economic assumptions) from {.file {file_path}}")

  # Read the V.B1 sheet
  raw <- readxl::read_excel(file_path, sheet = "V.B1", skip = 2)

  # The sheet has columns: Year, then various economic variables

  # Column mapping depends on exact sheet layout — handle flexibly
  dt <- data.table::as.data.table(raw)

  # Identify the year column (first column)
  year_col <- names(dt)[1]
  data.table::setnames(dt, year_col, "year")

  # Clean year column
  dt[, year := suppressWarnings(as.integer(year))]
  dt <- dt[!is.na(year)]

  if (!is.null(years_filter)) {
    dt <- dt[year %in% years_filter]
  }

  # Map remaining columns to standard variable names
  # V.B1 typically has these columns in order after Year:
  # Productivity, Real wage, Average hours, Earnings/compensation ratio,
  # GDP deflator, CPI
  var_cols <- names(dt)[names(dt) != "year"]

  # Melt to long format
  result <- data.table::melt(dt, id.vars = "year",
                              measure.vars = var_cols,
                              variable.name = "variable_raw",
                              value.name = "value")

  # Clean variable names
  result[, value := suppressWarnings(as.numeric(value))]
  result <- result[!is.na(value)]

  # Standardize variable names based on position
  var_mapping <- data.table::data.table(
    variable_raw = var_cols,
    variable = c("productivity", "real_wage", "avg_hours",
                 "earnings_compensation_ratio", "gdp_deflator", "cpi",
                 "nominal_earnings")[seq_along(var_cols)]
  )
  result <- merge(result, var_mapping, by = "variable_raw", all.x = TRUE)
  result[is.na(variable), variable := as.character(variable_raw)]
  result[, variable_raw := NULL]

  cli::cli_alert_success("Loaded V.B1: {nrow(result)} rows, years {min(result$year)}-{max(result$year)}")

  result[, .(year, variable, value)]
}

# =============================================================================
# V.B2: Economic Assumptions — Unemployment, Labor Force, GDP
# =============================================================================

#' Load TR2025 Table V.B2 (Employment/GDP Assumptions)
#'
#' @description
#' Loads unemployment rate, labor force change, employment change, real GDP change,
#' and interest rates from the SingleYearTRTables V.B2 sheet.
#'
#' @param config List with metadata section
#' @param years_filter Optional integer vector
#'
#' @return data.table with columns: year, variable, value
#'   Variables: unemployment_rate, labor_force_change, employment_change,
#'             real_gdp_change, nominal_interest_rate, real_interest_rate
#'
#' @export
load_tr2025_vb2 <- function(config, years_filter = NULL) {
  tr_dir <- get_tr_data_dir(config)
  file_path <- file.path(tr_dir, "SingleYearTRTables_TR2025.xlsx")

  if (!file.exists(file_path)) {
    cli::cli_abort(c(
      "TR2025 SingleYear tables not found",
      "x" = "Expected: {.file {file_path}}"
    ))
  }

  cli::cli_alert_info("Loading TR2025 V.B2 (employment/GDP assumptions)")

  raw <- readxl::read_excel(file_path, sheet = "V.B2", skip = 2)
  dt <- data.table::as.data.table(raw)

  year_col <- names(dt)[1]
  data.table::setnames(dt, year_col, "year")
  dt[, year := suppressWarnings(as.integer(year))]
  dt <- dt[!is.na(year)]

  if (!is.null(years_filter)) {
    dt <- dt[year %in% years_filter]
  }

  var_cols <- names(dt)[names(dt) != "year"]

  result <- data.table::melt(dt, id.vars = "year",
                              measure.vars = var_cols,
                              variable.name = "variable_raw",
                              value.name = "value")

  result[, value := suppressWarnings(as.numeric(value))]
  result <- result[!is.na(value)]

  # Map columns
  var_mapping <- data.table::data.table(
    variable_raw = var_cols,
    variable = c("unemployment_rate", "labor_force_change", "employment_change",
                 "real_gdp_change", "nominal_interest_rate", "real_interest_rate")[seq_along(var_cols)]
  )
  result <- merge(result, var_mapping, by = "variable_raw", all.x = TRUE)
  result[is.na(variable), variable := as.character(variable_raw)]
  result[, variable_raw := NULL]

  cli::cli_alert_success("Loaded V.B2: {nrow(result)} rows, years {min(result$year)}-{max(result$year)}")

  result[, .(year, variable, value)]
}

# =============================================================================
# V.C5: Disability Prevalence
# =============================================================================

#' Load TR2025 Table V.C5 (DI Prevalence)
#'
#' @description
#' Loads disability beneficiary counts, insured population, and prevalence rates.
#'
#' @param config List with metadata section
#'
#' @return data.table with columns: year, variable, value
#'
#' @export
load_tr2025_vc5 <- function(config) {
  tr_dir <- get_tr_data_dir(config)
  file_path <- file.path(tr_dir, "SingleYearTRTables_TR2025.xlsx")

  if (!file.exists(file_path)) {
    cli::cli_abort("TR2025 SingleYear tables not found: {.file {file_path}}")
  }

  cli::cli_alert_info("Loading TR2025 V.C5 (DI prevalence)")

  raw <- readxl::read_excel(file_path, sheet = "V.C5", skip = 2)
  dt <- data.table::as.data.table(raw)

  year_col <- names(dt)[1]
  data.table::setnames(dt, year_col, "year")
  dt[, year := suppressWarnings(as.integer(year))]
  dt <- dt[!is.na(year)]

  var_cols <- names(dt)[names(dt) != "year"]

  result <- data.table::melt(dt, id.vars = "year",
                              measure.vars = var_cols,
                              variable.name = "variable",
                              value.name = "value")
  result[, value := suppressWarnings(as.numeric(value))]
  result <- result[!is.na(value)]

  cli::cli_alert_success("Loaded V.C5: {nrow(result)} rows")

  result
}

# =============================================================================
# V.C7: Benefit Amounts
# =============================================================================

#' Load TR2025 Table V.C7 (Benefit Amounts)
#'
#' @description
#' Loads PIA by earnings level (scaled workers) for constructing RRADJ.
#'
#' @param config List with metadata section
#'
#' @return data.table with columns: year, variable, value
#'
#' @export
load_tr2025_vc7 <- function(config) {
  tr_dir <- get_tr_data_dir(config)
  file_path <- file.path(tr_dir, "SingleYearTRTables_TR2025.xlsx")

  if (!file.exists(file_path)) {
    cli::cli_abort("TR2025 SingleYear tables not found: {.file {file_path}}")
  }

  cli::cli_alert_info("Loading TR2025 V.C7 (benefit amounts)")

  raw <- readxl::read_excel(file_path, sheet = "V.C7", skip = 2)
  dt <- data.table::as.data.table(raw)

  year_col <- names(dt)[1]
  data.table::setnames(dt, year_col, "year")
  dt[, year := suppressWarnings(as.integer(year))]
  dt <- dt[!is.na(year)]

  var_cols <- names(dt)[names(dt) != "year"]

  result <- data.table::melt(dt, id.vars = "year",
                              measure.vars = var_cols,
                              variable.name = "variable",
                              value.name = "value")
  result[, value := suppressWarnings(as.numeric(value))]
  result <- result[!is.na(value)]

  cli::cli_alert_success("Loaded V.C7: {nrow(result)} rows")

  result
}

# =============================================================================
# VI.G6: Economic Levels (AWI, CPI, GDP)
# =============================================================================

#' Load TR2025 Table VI.G6 (Economic Levels)
#'
#' @description
#' Loads CPI level, AWI level, taxable payroll, and GDP level.
#'
#' @param config List with metadata section
#'
#' @return data.table with columns: year, variable, value
#'
#' @export
load_tr2025_vig6 <- function(config) {
  tr_dir <- get_tr_data_dir(config)
  file_path <- file.path(tr_dir, "SingleYearTRTables_TR2025.xlsx")

  if (!file.exists(file_path)) {
    cli::cli_abort("TR2025 SingleYear tables not found: {.file {file_path}}")
  }

  cli::cli_alert_info("Loading TR2025 VI.G6 (economic levels)")

  raw <- readxl::read_excel(file_path, sheet = "VI.G6", skip = 2)
  dt <- data.table::as.data.table(raw)

  year_col <- names(dt)[1]
  data.table::setnames(dt, year_col, "year")
  dt[, year := suppressWarnings(as.integer(year))]
  dt <- dt[!is.na(year)]

  var_cols <- names(dt)[names(dt) != "year"]

  result <- data.table::melt(dt, id.vars = "year",
                              measure.vars = var_cols,
                              variable.name = "variable",
                              value.name = "value")
  result[, value := suppressWarnings(as.numeric(value))]
  result <- result[!is.na(value)]

  cli::cli_alert_success("Loaded VI.G6: {nrow(result)} rows")

  result
}

# =============================================================================
# Combined Loader
# =============================================================================

#' Load all TR2025 economic assumptions
#'
#' @description
#' Loads and combines all economic assumption tables from TR2025 SingleYear tables.
#' When `assumptions_data_source = "tr2025"`, returns full historical + projected data.
#' When `assumptions_data_source = "api"`, returns only projected values (for merging
#' with API-fetched historical data).
#'
#' @param config Full ARTEMIS config (needs metadata and economics sections)
#'
#' @return data.table with columns: year, variable, value, source
#'
#' @export
load_tr2025_economic_assumptions <- function(config) {
  cli::cli_h1("Loading TR2025 Economic Assumptions")

  data_source <- config$economics$employment$assumptions_data_source %||% "api"
  base_year <- config$economics$employment$base_year %||% 2024

  vb1 <- load_tr2025_vb1(config)
  vb1[, source := "V.B1"]


  vb2 <- load_tr2025_vb2(config)
  vb2[, source := "V.B2"]

  combined <- data.table::rbindlist(list(vb1, vb2))

  if (data_source == "api") {
    # Only return projected values (after base year)
    combined <- combined[year > base_year]
    cli::cli_alert_info("API mode: returning projected values only (year > {base_year})")
  } else {
    cli::cli_alert_info("TR2025 mode: returning full historical + projected data")
  }

  cli::cli_alert_success("Loaded {nrow(combined)} economic assumption rows")

  combined
}

#' Load TR2025 DI prevalence data
#'
#' @param config Full ARTEMIS config
#' @return data.table
#' @export
load_tr2025_di_prevalence <- function(config) {
  load_tr2025_vc5(config)
}

#' Load TR2025 benefit parameters
#'
#' @param config Full ARTEMIS config
#' @return data.table
#' @export
load_tr2025_benefit_params <- function(config) {
  vc7 <- load_tr2025_vc7(config)
  vc7[, source := "V.C7"]
  vc7
}

#' Load AWI historical data
#'
#' @param config Full ARTEMIS config
#' @return data.table with year and awi columns
#' @export
load_awi_historical <- function(config) {
  tr_dir <- get_tr_data_dir(config)
  file_path <- file.path(tr_dir, "AWI_hist.csv")

  if (!file.exists(file_path)) {
    cli::cli_abort(c(
      "AWI historical file not found",
      "x" = "Expected: {.file {file_path}}"
    ))
  }

  dt <- data.table::fread(file_path)
  cli::cli_alert_success("Loaded AWI historical: {nrow(dt)} rows ({min(dt$year, na.rm = TRUE)}-{max(dt$year, na.rm = TRUE)})")

  dt
}

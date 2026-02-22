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
#' @name tr_economic_assumptions
NULL

# =============================================================================
# Shared Helper: Parse Sectioned TR2025 Sheet
# =============================================================================

#' Parse a TR2025 SingleYear sheet with Historical/Intermediate/Low/High sections
#'
#' @param file_path Path to SingleYearTRTables_TR{year}.xlsx
#' @param sheet Sheet name (e.g., "V.B1", "V.B2")
#' @param skip_rows Number of header rows to skip before data
#' @param col_names Character vector of column names (first must be year)
#' @param alternative Which alternative to use for projected data
#'
#' @return data.table with historical + selected alternative, named columns
#' @keywords internal
parse_tr_sectioned_sheet <- function(file_path, sheet, skip_rows,
                                          col_names, alternative = "intermediate") {
  raw <- readxl::read_excel(file_path, sheet = sheet, skip = skip_rows, col_names = FALSE)
  dt <- data.table::as.data.table(raw)

  # Assign column names
  if (ncol(dt) >= length(col_names)) {
    data.table::setnames(dt, names(dt)[seq_along(col_names)], col_names)
  }

  # Parse year (first column) — handle "2024 a" style annotations
  dt[, year := suppressWarnings(as.integer(gsub("\\s.*", "", as.character(year))))]

  # Find section boundaries
  year_raw <- as.character(raw[[1]])
  section_rows <- which(is.na(suppressWarnings(as.integer(gsub("\\s.*", "", year_raw)))) &
                         !is.na(year_raw) & nchar(trimws(year_raw)) > 0)
  section_labels <- trimws(year_raw[section_rows])

  # Historical: rows before first section header
  first_section <- if (length(section_rows) > 0) section_rows[1] else nrow(dt) + 1
  historical <- dt[1:(first_section - 1)][!is.na(year)]

  # Find the requested alternative
  alt_pattern <- switch(alternative,
    intermediate = "Intermediate",
    low = "Low",
    high = "High",
    "Intermediate"
  )
  section_idx <- grep(alt_pattern, section_labels, ignore.case = TRUE)

  projected <- data.table::data.table()
  if (length(section_idx) > 0) {
    start_row <- section_rows[section_idx[1]] + 1
    end_row <- if (section_idx[1] < length(section_rows)) {
      section_rows[section_idx[1] + 1] - 1
    } else {
      nrow(dt)
    }
    projected <- dt[start_row:end_row][!is.na(year)]
  }

  result <- data.table::rbindlist(list(historical, projected))

  # Convert value columns to numeric
  value_cols <- setdiff(col_names, "year")
  for (col in value_cols) {
    if (col %in% names(result)) {
      result[, (col) := suppressWarnings(as.numeric(get(col)))]
    }
  }

  result[!is.na(year)]
}

# =============================================================================
# V.B1: Economic Assumptions — Growth Rates
# =============================================================================

#' Load TR2025 Table V.B1 (Economic Assumptions)
#'
#' @description
#' Loads productivity growth, GDP deflator, average hours, earnings/compensation
#' ratio, and CPI from the SingleYearTRTables V.B1 sheet.
#' Parses section headers to return only historical + selected alternative.
#'
#' @param config List with metadata section (for TR data directory)
#' @param alternative Character: "intermediate" (default), "low", or "high"
#'
#' @return data.table with columns: year, variable, value
#'
#' @export
load_tr_vb1 <- function(config, alternative = "intermediate") {
  file_path <- resolve_tr_file(config, "single_year_tables")
  tr_year <- config$metadata$trustees_report_year

  if (!file.exists(file_path)) {
    cli::cli_abort(c(
      "TR SingleYear tables not found",
      "x" = "Expected: {.file {file_path}}",
      "i" = "Place TR data files in the TR data directory"
    ))
  }

  cli::cli_alert_info("Loading TR{tr_year} V.B1 (economic assumptions, {alternative})")

  # V.B1: skip 10 header rows, 8 data columns
  col_names <- c("year", "productivity", "gdp_deflator", "avg_hours",
                 "earnings_compensation_ratio", "nominal_earnings",
                 "real_wage", "cpi")

  dt <- parse_tr_sectioned_sheet(file_path, "V.B1", skip_rows = 10,
                                      col_names = col_names, alternative = alternative)

  # Melt to long format
  value_cols <- setdiff(col_names, "year")
  result <- data.table::melt(dt, id.vars = "year", measure.vars = value_cols,
                              variable.name = "variable", value.name = "value")
  result <- result[!is.na(value)]

  cli::cli_alert_success("Loaded V.B1: {nrow(result)} rows ({min(result$year)}-{max(result$year)}), {alternative}")

  result[, .(year, variable, value)]
}

# =============================================================================
# V.B2: Economic Assumptions — Unemployment, Labor Force, GDP
# =============================================================================

#' Load TR2025 Table V.B2 (Employment/GDP Assumptions)
#'
#' @description
#' Loads unemployment rate, labor force change, employment change, real GDP change,
#' and interest rates. Parses section headers for alternative selection.
#'
#' @param config List with metadata section
#' @param alternative Character: "intermediate" (default), "low", or "high"
#'
#' @return data.table with columns: year, variable, value
#'
#' @export
load_tr_vb2 <- function(config, alternative = "intermediate") {
  file_path <- resolve_tr_file(config, "single_year_tables")
  tr_year <- config$metadata$trustees_report_year

  if (!file.exists(file_path)) {
    cli::cli_abort(c(
      "TR SingleYear tables not found",
      "x" = "Expected: {.file {file_path}}"
    ))
  }

  cli::cli_alert_info("Loading TR{tr_year} V.B2 (employment/GDP assumptions, {alternative})")

  # V.B2: skip 9 header rows, 7 data columns
  col_names <- c("year", "unemployment_rate", "labor_force_change",
                 "employment_change", "real_gdp_change",
                 "nominal_interest_rate", "real_interest_rate")

  dt <- parse_tr_sectioned_sheet(file_path, "V.B2", skip_rows = 9,
                                      col_names = col_names, alternative = alternative)

  value_cols <- setdiff(col_names, "year")
  result <- data.table::melt(dt, id.vars = "year", measure.vars = value_cols,
                              variable.name = "variable", value.name = "value")
  result <- result[!is.na(value)]

  cli::cli_alert_success("Loaded V.B2: {nrow(result)} rows ({min(result$year)}-{max(result$year)}), {alternative}")

  result[, .(year, variable, value)]
}

# =============================================================================
# V.C5: Disability Prevalence
# =============================================================================

#' Load TR2025 Table V.C5 (DI Prevalence)
#'
#' @description
#' Loads disability beneficiary counts and prevalence rates from V.C5.
#' The sheet has 7 columns after the year:
#'   1. Disabled-worker beneficiaries (thousands)
#'   2. Auxiliary: Spouse (thousands)
#'   3. Auxiliary: Child (thousands)
#'   4. Total beneficiaries (thousands)
#'   5. Gross prevalence rate (per 1,000 insured)
#'   6. Age-sex adjusted prevalence rate (per 1,000 insured)
#'
#' Contains historical (1975-2024) then three alternatives (intermediate,
#' low-cost, high-cost) each projecting 2025-2100.
#'
#' @param config List with metadata section
#' @param alternative Character: "intermediate" (default), "low", or "high"
#'
#' @return data.table with columns: year, dw_beneficiaries, spouse, child,
#'   total_beneficiaries, gross_prevalence_rate, age_sex_adj_prevalence_rate
#'
#' @export
load_tr_vc5 <- function(config, alternative = "intermediate") {
  file_path <- resolve_tr_file(config, "single_year_tables")
  tr_year <- config$metadata$trustees_report_year

  if (!file.exists(file_path)) {
    cli::cli_abort(c("TR SingleYear tables not found", "x" = "Expected: {.file {file_path}}"))
  }

  cli::cli_alert_info("Loading TR{tr_year} V.C5 (DI prevalence, {alternative})")

  # Read raw data — skip 9 header rows, no column names
  raw <- readxl::read_excel(file_path, sheet = "V.C5", skip = 9, col_names = FALSE)
  dt <- data.table::as.data.table(raw)

  # Assign proper column names
  col_names <- c("year_raw", "dw_beneficiaries", "spouse", "child",
                 "total_beneficiaries", "gross_prevalence_rate",
                 "age_sex_adj_prevalence_rate")
  if (ncol(dt) >= 7) {
    data.table::setnames(dt, names(dt)[1:7], col_names)
  } else {
    cli::cli_abort("V.C5 has {ncol(dt)} columns, expected at least 7")
  }

  # Parse year and find section boundaries
  dt[, year := suppressWarnings(as.integer(year_raw))]

  # Identify section boundaries from non-numeric year_raw values
  section_rows <- which(is.na(dt$year) & !is.na(dt$year_raw))
  section_labels <- dt$year_raw[section_rows]

  # Historical data: rows before first section header
  first_section <- if (length(section_rows) > 0) section_rows[1] else nrow(dt) + 1
  historical <- dt[1:(first_section - 1)][!is.na(year)]

  # Find the requested alternative section
  if (alternative == "intermediate") {
    section_idx <- grep("Intermediate", section_labels, ignore.case = TRUE)
  } else if (alternative == "low") {
    section_idx <- grep("Low", section_labels, ignore.case = TRUE)
  } else if (alternative == "high") {
    section_idx <- grep("High", section_labels, ignore.case = TRUE)
  } else {
    cli::cli_abort("Invalid alternative: {alternative}. Use 'intermediate', 'low', or 'high'.")
  }

  projected <- data.table::data.table()
  if (length(section_idx) > 0) {
    start_row <- section_rows[section_idx[1]] + 1
    end_row <- if (section_idx[1] < length(section_rows)) {
      section_rows[section_idx[1] + 1] - 1
    } else {
      nrow(dt)
    }
    projected <- dt[start_row:end_row][!is.na(year)]
  }

  result <- data.table::rbindlist(list(historical, projected))
  result[, year_raw := NULL]

  # Convert all value columns to numeric
  value_cols <- setdiff(names(result), "year")
  for (col in value_cols) {
    result[, (col) := suppressWarnings(as.numeric(get(col)))]
  }
  result <- result[!is.na(year)]

  cli::cli_alert_success(
    "Loaded V.C5: {nrow(result)} rows ({min(result$year)}-{max(result$year)}), {alternative} alternative"
  )

  result
}

# =============================================================================
# V.C7: Benefit Amounts
# =============================================================================

#' Load TR2025 Table V.C7 (Benefit Amounts)
#'
#' @description
#' Loads scheduled benefit amounts for retired workers by earnings pattern.
#' V.C7 has 5 sections by earnings level: very_low, low, medium, high, maximum.
#' Each section has 7 columns: year_attaining_65, retirement_age_nra,
#' cpi_indexed_benefit_nra, pct_earnings_nra, retirement_age_65,
#' cpi_indexed_benefit_65, pct_earnings_65.
#'
#' @param config List with metadata section
#' @param earnings_level Which earnings pattern: "very_low", "low", "medium"
#'   (default), "high", or "maximum". Use "all" for all levels.
#'
#' @return data.table with columns: year, earnings_level, nra_age,
#'   benefit_at_nra, pct_earnings_nra, benefit_at_65, pct_earnings_65
#'
#' @export
load_tr_vc7 <- function(config, earnings_level = "all") {
  file_path <- resolve_tr_file(config, "single_year_tables")
  tr_year <- config$metadata$trustees_report_year

  if (!file.exists(file_path)) {
    cli::cli_abort(c("TR SingleYear tables not found", "x" = "Expected: {.file {file_path}}"))
  }

  cli::cli_alert_info("Loading TR{tr_year} V.C7 (benefit amounts)")

  # Read raw without column names — 7 columns, skip first 10 header rows
  raw <- readxl::read_excel(file_path, sheet = "V.C7", skip = 10, col_names = FALSE)
  dt <- data.table::as.data.table(raw)

  col_names <- c("year_raw", "nra_age", "benefit_at_nra", "pct_earnings_nra",
                  "age_65", "benefit_at_65", "pct_earnings_65")
  if (ncol(dt) >= 7) {
    data.table::setnames(dt, names(dt)[1:7], col_names)
  }

  # Identify section boundaries by earnings pattern labels
  section_labels <- c("Scaled very low earnings", "Scaled low earnings",
                       "Scaled medium earnings", "Scaled high earnings",
                       "Steady maximum earnings")
  section_names <- c("very_low", "low", "medium", "high", "maximum")

  section_rows <- integer(0)
  for (lbl in section_labels) {
    idx <- grep(lbl, as.character(dt$year_raw), ignore.case = TRUE)
    section_rows <- c(section_rows, idx)
  }
  section_rows <- sort(section_rows)

  # Parse each section
  all_sections <- list()
  for (i in seq_along(section_rows)) {
    start_row <- section_rows[i] + 1
    end_row <- if (i < length(section_rows)) section_rows[i + 1] - 1 else nrow(dt)

    section <- dt[start_row:end_row]
    section[, year := suppressWarnings(as.integer(gsub("\\s.*", "", as.character(year_raw))))]
    section <- section[!is.na(year)]

    # Parse NRA age (format "65:0", "66:2", "67:0")
    section[, nra_age := as.character(nra_age)]

    # Convert value columns to numeric
    for (col in c("benefit_at_nra", "pct_earnings_nra", "benefit_at_65", "pct_earnings_65")) {
      section[, (col) := suppressWarnings(as.numeric(get(col)))]
    }

    section[, earnings_level := section_names[i]]
    section[, c("year_raw", "age_65") := NULL]

    all_sections[[i]] <- section
  }

  result <- data.table::rbindlist(all_sections, fill = TRUE)

  # Filter by requested earnings level
  if (earnings_level != "all" && earnings_level %in% section_names) {
    result <- result[earnings_level == earnings_level]
  }

  data.table::setcolorder(result, c("year", "earnings_level", "nra_age",
                                     "benefit_at_nra", "pct_earnings_nra",
                                     "benefit_at_65", "pct_earnings_65"))

  cli::cli_alert_success(
    "Loaded V.C7: {nrow(result)} rows, {length(unique(result$earnings_level))} earnings levels ({min(result$year)}-{max(result$year)})"
  )

  result
}

# =============================================================================
# VI.G6: Economic Levels (AWI, CPI, GDP)
# =============================================================================

#' Load TR2025 Table VI.G6 (Economic Levels)
#'
#' @description
#' Loads CPI level, AWI level, taxable payroll, GDP level, and compound
#' effective trust fund interest factor. Uses the standard sectioned parser
#' for Historical/Intermediate/Low/High structure.
#'
#' @param config List with metadata section
#' @param alternative Character: "intermediate" (default), "low", or "high"
#'
#' @return data.table with columns: year, variable, value
#'
#' @export
load_tr_vig6 <- function(config, alternative = "intermediate") {
  file_path <- resolve_tr_file(config, "single_year_tables")
  tr_year <- config$metadata$trustees_report_year

  if (!file.exists(file_path)) {
    cli::cli_abort(c("TR SingleYear tables not found", "x" = "Expected: {.file {file_path}}"))
  }

  cli::cli_alert_info("Loading TR{tr_year} VI.G6 (economic levels, {alternative})")

  # VI.G6: skip 11 rows (10 header + "Historical data:" label), 6 columns
  col_names <- c("year", "adjusted_cpi", "awi", "taxable_payroll",
                  "gdp", "trust_fund_interest_factor")

  dt <- parse_tr_sectioned_sheet(file_path, "VI.G6", skip_rows = 11,
                                      col_names = col_names, alternative = alternative)

  # Melt to long format
  value_cols <- setdiff(col_names, "year")
  result <- data.table::melt(dt, id.vars = "year", measure.vars = value_cols,
                              variable.name = "variable", value.name = "value")
  result <- result[!is.na(value)]

  cli::cli_alert_success(
    "Loaded VI.G6: {nrow(result)} rows ({min(result$year)}-{max(result$year)}), {alternative}"
  )

  result[, .(year, variable, value)]
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
load_tr_economic_assumptions <- function(config) {
  tr_year <- config$metadata$trustees_report_year
  cli::cli_h1("Loading TR{tr_year} Economic Assumptions")

  data_source <- config$economics$employment$assumptions_data_source %||% "api"
  base_year <- config$economics$employment$base_year %||% 2024
  alternative <- config$economics$employment$economic_alternative %||% "intermediate"

  vb1 <- load_tr_vb1(config, alternative = alternative)
  vb1[, source := "V.B1"]

  vb2 <- load_tr_vb2(config, alternative = alternative)
  vb2[, source := "V.B2"]

  combined <- data.table::rbindlist(list(vb1, vb2))

  if (data_source == "api") {
    # Only return projected values (after base year)
    combined <- combined[year > base_year]
    cli::cli_alert_info("API mode: returning projected values only (year > {base_year})")
  } else {
    cli::cli_alert_info("TR table mode: returning full historical + projected data")
  }

  cli::cli_alert_success("Loaded {nrow(combined)} economic assumption rows")

  combined
}

#' Load TR2025 DI prevalence data
#'
#' @param config Full ARTEMIS config
#' @return data.table
#' @export
load_tr_di_prevalence <- function(config) {
  load_tr_vc5(config)
}

#' Load TR2025 benefit parameters
#'
#' @param config Full ARTEMIS config
#' @return data.table with structured V.C7 data (year, earnings_level,
#'   nra_age, benefit_at_nra, pct_earnings_nra, benefit_at_65, pct_earnings_65)
#' @export
load_tr_benefit_params <- function(config) {
  vc7 <- load_tr_vc7(config, earnings_level = "all")
  vc7[, source := "V.C7"]
  vc7
}

# =============================================================================
# DI Age Profile from Published SSA Data
# =============================================================================

#' Load disability prevalence age profile from published SSA data
#'
#' @description
#' Computes age-sex-specific disability prevalence rates from:
#' - DI Annual Statistical Report Table 4 (disabled worker beneficiaries by age and sex)
#' - SSA Annual Statistical Supplement Table 4.C5 (SS area population and % fully insured)
#'
#' The resulting rates serve as the age shape for distributing V.C5's aggregate
#' prevalence rate across LFPR age groups. This will be replaced when the
#' Beneficiaries subprocess computes age-sex-specific DI prevalence directly.
#'
#' @param config Full ARTEMIS config (uses trustees_report_year to find files)
#'
#' @return data.table with columns: age_group, sex, prevalence_rate (per 1,000 insured)
#'
#' @export
load_di_age_profile <- function(config) {
  tr_year <- config$metadata$trustees_report_year
  yy <- sprintf("%02d", (tr_year - 1) %% 100)

  supplemental_dir <- here::here("data/raw/SSA_supplemental")
  di_file <- file.path(supplemental_dir, paste0("di_asr", yy, ".xlsx"))
  supp_file <- file.path(supplemental_dir, paste0("supplement", yy, ".xlsx"))

  if (!file.exists(di_file)) {
    cli::cli_abort(c(
      "DI Annual Statistical Report not found",
      "x" = "Expected: {.file {di_file}}",
      "i" = "Place SSA supplemental data files in {.path {supplemental_dir}}"
    ))
  }
  if (!file.exists(supp_file)) {
    cli::cli_abort(c(
      "SSA Annual Statistical Supplement not found",
      "x" = "Expected: {.file {supp_file}}",
      "i" = "Place SSA supplemental data files in {.path {supplemental_dir}}"
    ))
  }

  cli::cli_alert_info(
    "Loading DI age profile from di_asr{yy}.xlsx Table 4 and supplement{yy}.xlsx Table 4.C5"
  )

  # Helper: find the row where a label appears (scanning all columns)
  find_section_row <- function(dt, label) {
    for (j in seq_len(ncol(dt))) {
      matches <- which(trimws(as.character(dt[[j]])) == label)
      if (length(matches) > 0) return(matches[1])
    }
    cli::cli_abort("Could not find '{label}' section label in Excel sheet")
  }

  # ── 1. Parse disabled worker beneficiaries from DI ASR Table 4 ─────
  # Structure (skip=4): section label → subtotal → 10 age groups
  # Col 1 = age label, Col 6 = disabled workers count
  raw_di <- data.table::as.data.table(
    readxl::read_excel(di_file, sheet = "Table 4", skip = 4, col_names = FALSE)
  )

  men_row <- find_section_row(raw_di, "Men")
  women_row <- find_section_row(raw_di, "Women")

  parse_di_data <- function(dt, start, n = 10) {
    rows <- dt[(start + 2):(start + 1 + n)]
    data.table::data.table(
      age_label = gsub("\u2013", "-", trimws(as.character(rows[[1]]))),
      workers = suppressWarnings(as.numeric(rows[[6]]))
    )
  }

  men_di <- parse_di_data(raw_di, men_row)[, sex := "male"]
  women_di <- parse_di_data(raw_di, women_row)[, sex := "female"]
  di_data <- data.table::rbindlist(list(men_di, women_di))

  # Remove footnote markers (e.g., "65-FRA a" → "65-FRA")
  di_data[, age_label := gsub("\\s+[a-z]$", "", age_label)]

  # Drop 65-FRA: ages 62+ use cohort RD at age 61, not current rates
  di_data <- di_data[!grepl("65", age_label)]

  # ── 2. Parse SS area population + % insured from Supplement 4.C5 ───
  # Structure (skip=4): section label → subtotal → 14 age groups
  # Paired columns per year: (population in thousands, % fully insured)
  raw_supp <- data.table::as.data.table(
    readxl::read_excel(supp_file, sheet = "4.C5", skip = 4, col_names = FALSE)
  )

  # Read header row to find year columns dynamically
  header_row <- data.table::as.data.table(
    readxl::read_excel(supp_file, sheet = "4.C5", skip = 2, n_max = 1, col_names = FALSE)
  )
  header_years <- suppressWarnings(as.integer(as.character(header_row[1, ])))
  year_cols <- which(!is.na(header_years))

  if (length(year_cols) == 0) {
    cli::cli_abort("Could not find year columns in Supplement Table 4.C5 header")
  }

  # Use most recent year
  latest_idx <- which.max(header_years[year_cols])
  pop_col <- year_cols[latest_idx]
  pct_col <- year_cols[latest_idx] + 1
  data_year <- header_years[pop_col]

  cli::cli_alert_info("Using {data_year} population data from Table 4.C5")

  male_row <- find_section_row(raw_supp, "Male")
  female_row <- find_section_row(raw_supp, "Female")

  parse_supp_data <- function(dt, start, n = 14) {
    rows <- dt[(start + 2):(start + 1 + n)]
    pct_raw <- as.character(rows[[pct_col]])
    pct_clean <- gsub("^a\\s+", "", pct_raw)
    pct_clean <- gsub("\\(L\\)", "0", pct_clean)

    data.table::data.table(
      age_label = gsub("\u2013", "-", trimws(as.character(rows[[1]]))),
      population = suppressWarnings(as.numeric(rows[[pop_col]])) * 1000,
      pct_insured = suppressWarnings(as.numeric(pct_clean))
    )
  }

  male_pop <- parse_supp_data(raw_supp, male_row)[, sex := "male"]
  female_pop <- parse_supp_data(raw_supp, female_row)[, sex := "female"]
  pop_data <- data.table::rbindlist(list(male_pop, female_pop))

  # ── 3. Align supplement age groups to DI age groups ────────────────
  # DI "Under 25" = Supplement "15-19" + "20-24" (Under 15 not DI-eligible)
  # DI "25-29" through "60-64" = direct match
  pop_data[, di_age_group := data.table::fcase(
    age_label %in% c("15-19", "20-24"), "Under 25",
    age_label %in% c("25-29", "30-34", "35-39", "40-44",
                      "45-49", "50-54", "55-59", "60-64"), age_label,
    default = NA_character_
  )]

  # Validate parsed data before aggregation
  matched_pop <- pop_data[!is.na(di_age_group)]
  na_pop <- sum(is.na(matched_pop$population))
  na_pct <- sum(is.na(matched_pop$pct_insured))
  if (na_pop > 0 || na_pct > 0) {
    cli::cli_abort(c(
      "Missing values in Supplement Table 4.C5 data",
      "x" = "{na_pop} NA population values, {na_pct} NA insured-percentage values",
      "i" = "Check that {.file {supp_file}} Table 4.C5 has complete data for ages 15-64"
    ))
  }

  na_workers <- sum(is.na(di_data$workers))
  if (na_workers > 0) {
    cli::cli_abort(c(
      "Missing values in DI ASR Table 4 worker counts",
      "x" = "{na_workers} age groups have NA disabled-worker counts",
      "i" = "Check that {.file {di_file}} Table 4 has complete data"
    ))
  }

  pop_agg <- matched_pop[, .(
    insured_pop = sum(population * pct_insured / 100)
  ), by = .(di_age_group, sex)]

  # ── 4. Compute prevalence rates and map to LFPR age groups ────────
  di_data[, di_age_group := age_label]
  rates <- merge(di_data, pop_agg, by = c("di_age_group", "sex"))
  rates[, prevalence_rate := workers / insured_pop * 1000]

  # Map DI groups to LFPR equation age groups
  # Under 25 applies equally to 16-17, 18-19, and 20-24
  lfpr_map <- data.table::data.table(
    di_age_group = c(rep("Under 25", 3), "25-29", "30-34", "35-39",
                      "40-44", "45-49", "50-54", "55-59", "60-64"),
    age_group = c("16-17", "18-19", "20-24", "25-29", "30-34", "35-39",
                   "40-44", "45-49", "50-54", "55-59", "60-61")
  )

  result <- merge(lfpr_map, rates[, .(di_age_group, sex, prevalence_rate)],
                   by = "di_age_group", allow.cartesian = TRUE)
  result[, di_age_group := NULL]
  data.table::setorder(result, sex, age_group)

  cli::cli_alert_success(
    "Computed DI age profile: {nrow(result)} groups, rates {round(min(result$prevalence_rate), 1)}-{round(max(result$prevalence_rate), 1)} per 1,000 insured"
  )

  result[, .(age_group, sex, prevalence_rate)]
}

#' Load AWI historical data
#'
#' @description
#' Loads Average Wage Index from SSA CSV file. Parses comma-formatted
#' numbers and percentage strings.
#'
#' @param config Full ARTEMIS config
#' @return data.table with columns: year (integer), awi (numeric)
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
  data.table::setnames(dt, tolower(gsub(" ", "_", names(dt))))

  if (!"year" %in% names(dt)) {
    cli::cli_abort(c(
      "AWI CSV missing 'Year' column",
      "i" = "Found columns: {.val {names(dt)}}"
    ))
  }
  if (!"awi" %in% names(dt)) {
    cli::cli_abort(c(
      "AWI CSV missing 'AWI' column",
      "i" = "Found columns: {.val {names(dt)}}"
    ))
  }

  # Parse comma-formatted numbers (e.g., "2,799.16" -> 2799.16)
  dt[, awi := as.numeric(gsub(",", "", awi))]

  if (any(is.na(dt$awi))) {
    n_na <- sum(is.na(dt$awi))
    cli::cli_abort("AWI CSV has {n_na} unparseable values in AWI column")
  }

  # Keep only year and awi columns
  dt <- dt[, .(year, awi)]

  cli::cli_alert_success(
    "Loaded AWI historical: {nrow(dt)} rows ({min(dt$year)}-{max(dt$year)})"
  )

  dt
}

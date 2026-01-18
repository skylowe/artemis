#' TR2025 Data Acquisition
#'
#' Functions for loading SSA Trustees Report 2025 data files.
#' These files provide validation targets and historical baselines.
#'
#' @name tr2025_data
NULL

#' Load TR2025 historical life tables
#'
#' @description
#' Loads period life tables from TR2025 historical data files.
#' Data available from 1900-2022.
#'
#' @param sex Character: "male", "female", or "both"
#' @param data_dir Character: directory containing TR2025 files
#'
#' @return data.table with columns: year, age, sex, qx, lx, dx, Lx, Tx, ex
#'
#' @export
load_tr2025_life_tables_hist <- function(sex = c("both", "male", "female"),
                                          data_dir = "data/raw/SSA_TR2025") {
  sex <- match.arg(sex)

  read_life_table <- function(file_path, sex_label) {
    # Skip 4 header lines, read CSV
    dt <- data.table::fread(
      file_path,
      skip = 4,
      select = c(1:8),  # Year, x, q(x), l(x), d(x), L(x), T(x), e(x)
      col.names = c("year", "age", "qx", "lx", "dx", "Lx", "Tx", "ex")
    )
    dt[, sex := sex_label]
    dt
  }

  results <- list()

  if (sex %in% c("both", "male")) {
    male_file <- file.path(data_dir, "PerLifeTables_M_Hist_TR2025.csv")
    if (!file.exists(male_file)) {
      cli::cli_abort("Male life table file not found: {male_file}")
    }
    results$male <- read_life_table(male_file, "male")
  }

  if (sex %in% c("both", "female")) {
    female_file <- file.path(data_dir, "PerLifeTables_F_Hist_TR2025.csv")
    if (!file.exists(female_file)) {
      cli::cli_abort("Female life table file not found: {female_file}")
    }
    results$female <- read_life_table(female_file, "female")
  }

  dt <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setcolorder(dt, c("year", "age", "sex", "qx", "lx", "dx", "Lx", "Tx", "ex"))
  data.table::setorder(dt, year, sex, age)

  dt
}

#' Get 1939-41 baseline death probabilities
#'
#' @description
#' Extracts death probabilities (qx) from 1939-1941 period life tables.
#' These serve as historical baseline for very long-range projections.
#'
#' @param sex Character: "male", "female", or "both"
#' @param average Logical: if TRUE, return average of 1939-1941; if FALSE, return all 3 years
#' @param data_dir Character: directory containing TR2025 files
#'
#' @return data.table with columns: age, sex, qx (and year if average=FALSE)
#'
#' @export
get_baseline_qx_1939_41 <- function(sex = c("both", "male", "female"),
                                     average = TRUE,
                                     data_dir = "data/raw/SSA_TR2025") {
  sex <- match.arg(sex)

  # Load full historical life tables
  lt <- load_tr2025_life_tables_hist(sex = sex, data_dir = data_dir)


  # Filter to 1939-1941
  baseline <- lt[year %in% 1939:1941, .(year, age, sex, qx)]

  if (nrow(baseline) == 0) {
    cli::cli_abort("No data found for years 1939-1941")
  }

  if (average) {
    # Average qx across the 3 years
    result <- baseline[, .(qx = mean(qx)), by = .(age, sex)]
    data.table::setorder(result, sex, age)
    cli::cli_alert_success("Extracted average qx for 1939-1941 ({nrow(result)} rows)")
  } else {
    result <- baseline
    data.table::setorder(result, year, sex, age)
    cli::cli_alert_success("Extracted qx for 1939-1941 ({nrow(result)} rows)")
  }

  result
}

#' Load TR2025 death probabilities (projected)
#'
#' @description
#' Loads projected death probabilities from TR2025 for validation.
#'
#' @param sex Character: "male", "female", or "both"
#' @param alternative Character: "Alt1", "Alt2", or "Alt3" (default: Alt2 = intermediate)
#' @param data_dir Character: directory containing TR2025 files
#'
#' @return data.table with columns: year, age, sex, qx
#'
#' @export
load_tr2025_death_probs <- function(sex = c("both", "male", "female"),
                                     alternative = "Alt2",
                                     data_dir = "data/raw/SSA_TR2025") {
  sex <- match.arg(sex)

  read_death_probs <- function(file_path, sex_label) {
    dt <- data.table::fread(file_path, skip = 4)
    # File format: Year in first column, then qx for ages 0-119 in columns 2-121
    year_col <- names(dt)[1]
    age_cols <- names(dt)[2:ncol(dt)]

    # Melt to long format
    dt_long <- data.table::melt(
      dt,
      id.vars = year_col,
      variable.name = "age_col",
      value.name = "qx"
    )
    data.table::setnames(dt_long, year_col, "year")

    # Extract age from column name (assumes columns are in order 0-119)
    dt_long[, age := as.integer(gsub("V", "", age_col)) - 2L]  # V2 = age 0, V3 = age 1, etc.
    dt_long[, age_col := NULL]
    dt_long[, sex := sex_label]

    dt_long[!is.na(qx)]
  }

  results <- list()

  if (sex %in% c("both", "male")) {
    male_file <- file.path(data_dir, paste0("DeathProbsE_M_", alternative, "_TR2025.csv"))
    if (!file.exists(male_file)) {
      cli::cli_abort("Male death probs file not found: {male_file}")
    }
    results$male <- read_death_probs(male_file, "male")
  }

  if (sex %in% c("both", "female")) {
    female_file <- file.path(data_dir, paste0("DeathProbsE_F_", alternative, "_TR2025.csv"))
    if (!file.exists(female_file)) {
      cli::cli_abort("Female death probs file not found: {female_file}")
    }
    results$female <- read_death_probs(female_file, "female")
  }

  dt <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setcolorder(dt, c("year", "age", "sex", "qx"))
  data.table::setorder(dt, year, sex, age)

  dt
}

#' Load TR2025 historical death probabilities
#'
#' @description
#' Loads historical death probabilities from TR2025.
#'
#' @param sex Character: "male", "female", or "both"
#' @param data_dir Character: directory containing TR2025 files
#'
#' @return data.table with columns: year, age, sex, qx
#'
#' @export
load_tr2025_death_probs_hist <- function(sex = c("both", "male", "female"),
                                          data_dir = "data/raw/SSA_TR2025") {
  sex <- match.arg(sex)

  read_death_probs_hist <- function(file_path, sex_label) {
    dt <- data.table::fread(file_path, skip = 4)
    year_col <- names(dt)[1]

    # Melt to long format
    dt_long <- data.table::melt(
      dt,
      id.vars = year_col,
      variable.name = "age_col",
      value.name = "qx"
    )
    data.table::setnames(dt_long, year_col, "year")

    # Extract age from column position
    dt_long[, age := as.integer(gsub("V", "", age_col)) - 2L]
    dt_long[, age_col := NULL]
    dt_long[, sex := sex_label]

    dt_long[!is.na(qx)]
  }

  results <- list()

  if (sex %in% c("both", "male")) {
    male_file <- file.path(data_dir, "DeathProbsE_M_Hist_TR2025.csv")
    if (!file.exists(male_file)) {
      cli::cli_abort("Male historical death probs file not found: {male_file}")
    }
    results$male <- read_death_probs_hist(male_file, "male")
  }

  if (sex %in% c("both", "female")) {
    female_file <- file.path(data_dir, "DeathProbsE_F_Hist_TR2025.csv")
    if (!file.exists(female_file)) {
      cli::cli_abort("Female historical death probs file not found: {female_file}")
    }
    results$female <- read_death_probs_hist(female_file, "female")
  }

  dt <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setcolorder(dt, c("year", "age", "sex", "qx"))
  data.table::setorder(dt, year, sex, age)

  dt
}

# =============================================================================
# IMMIGRATION ASSUMPTIONS (Table V.A2)
# =============================================================================

#' Load TR2025 Immigration Assumptions (Table V.A2)
#'
#' @description
#' Loads immigration assumptions from TR2025 Table V.A2, which contains
#' historical (1940-2023) and projected (2024-2100) immigration flows for:
#' - LPR (Lawful Permanent Resident) immigration
#' - LPR emigration
#' - Adjustments of Status
#' - Temporary/Unlawfully present (O) population flows
#'
#' @param data_dir Character: directory containing TR2025 files
#' @param cache Logical: whether to cache the parsed result (default: TRUE)
#' @param cache_dir Character: directory for caching
#'
#' @return data.table with columns:
#'   - year: Calendar year
#'   - lpr_inflow: LPR immigration (thousands)
#'   - lpr_outflow: LPR emigration (thousands)
#'   - lpr_aos: LPR adjustments of status (thousands)
#'   - lpr_net: Net LPR change (thousands)
#'   - o_inflow: Temp/unlawful immigration (thousands)
#'   - o_outflow: Temp/unlawful emigration (thousands)
#'   - o_aos: Temp/unlawful adjustments to LPR (thousands)
#'   - o_net: Net temp/unlawful change (thousands)
#'   - total_net: Total net immigration change (thousands)
#'   - data_type: "historical", "estimated", or "projected"
#'
#' @details
#' Data source: SSA Office of the Chief Actuary, 2025 Trustees Report
#' Table V.A2 - Immigration Assumptions, Calendar Years 1940-2100
#'
#' The table distinguishes between:
#' - Historical data: actual recorded values
#' - Estimated data: marked with footnotes (f, g) for recent years
#' - Projected data: intermediate assumptions for future years
#'
#' All values are in thousands. Multiply by 1000 for actual counts.
#'
#' @section Column Definitions:
#' - **LPR Inflow**: New lawful permanent residents admitted
#' - **LPR Outflow**: Legal emigrants (former LPRs who left permanently)
#' - **LPR AOS**: Adjustments of status (already in US, status changed to LPR)
#' - **O Inflow**: Temporary visitors + unauthorized immigrants entering
#' - **O Outflow**: Temporary visitors + unauthorized immigrants leaving
#' - **O AOS**: Persons converting from O status to LPR status
#'
#' @export
load_tr2025_immigration_assumptions <- function(data_dir = "data/raw/SSA_TR2025",
                                                 cache = TRUE,
                                                 cache_dir = "data/cache/tr2025") {
  # Check for cached data
  if (cache) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    cache_file <- file.path(cache_dir, "immigration_assumptions_va2.rds")
    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached TR2025 immigration assumptions")
      return(readRDS(cache_file))
    }
  }

  # Find the Excel file
  excel_file <- file.path(data_dir, "SingleYearTRTables_TR2025.xlsx")
  if (!file.exists(excel_file)) {
    cli::cli_abort("TR2025 tables file not found: {excel_file}")
  }

  cli::cli_alert_info("Loading TR2025 Table V.A2 (Immigration Assumptions)...")

  # Read the V.A2 sheet
  raw <- readxl::read_excel(excel_file, sheet = "V.A2", col_names = FALSE)
  dt <- data.table::as.data.table(raw)

  # Find the data start row (look for "1940" in first column)
  data_start <- NULL
  for (i in 1:nrow(dt)) {
    val <- as.character(dt[[1]][i])
    if (!is.na(val) && grepl("^1940$", trimws(val))) {
      data_start <- i
      break
    }
  }

  if (is.null(data_start)) {
    cli::cli_abort("Could not find data start row (year 1940) in V.A2")
  }

  # Parse the data rows
  result_list <- list()

  for (i in data_start:nrow(dt)) {
    row_vals <- as.character(unlist(dt[i, ]))

    # Get year - skip section headers like "Intermediate:"
    year_str <- trimws(row_vals[1])
    if (is.na(year_str) || year_str == "" || grepl("^[A-Za-z]", year_str)) {
      next
    }

    # Clean year (remove footnote markers like "g")
    year_clean <- gsub("[^0-9]", "", year_str)
    if (nchar(year_clean) != 4) next

    year <- as.integer(year_clean)
    if (is.na(year) || year < 1940 || year > 2100) next

    # Parse numeric values - handle "—", footnotes, commas
    parse_val <- function(x) {
      if (is.na(x)) return(NA_real_)
      x <- trimws(as.character(x))
      if (x == "" || x == "—" || x == "-") return(NA_real_)
      # Remove footnote markers (f, g) and commas
      x <- gsub("[fgFG,]", "", x)
      x <- trimws(x)
      if (x == "") return(NA_real_)
      suppressWarnings(as.numeric(x))
    }

    # Extract columns (positions based on V.A2 structure)
    # Col 1: Year
    # Col 2: LPR Inflow
    # Col 3: LPR Outflow
    # Col 4: LPR AOS
    # Col 5: LPR Net
    # Col 6: O Inflow
    # Col 7: O Outflow
    # Col 8: O AOS
    # Col 9: O Net
    # Col 10: Total Net

    result_list[[length(result_list) + 1]] <- data.table::data.table(
      year = year,
      lpr_inflow = parse_val(row_vals[2]),
      lpr_outflow = parse_val(row_vals[3]),
      lpr_aos = parse_val(row_vals[4]),
      lpr_net = parse_val(row_vals[5]),
      o_inflow = parse_val(row_vals[6]),
      o_outflow = parse_val(row_vals[7]),
      o_aos = parse_val(row_vals[8]),
      o_net = parse_val(row_vals[9]),
      total_net = parse_val(row_vals[10])
    )
  }

  result <- data.table::rbindlist(result_list)

  # Add data type classification
  result[, data_type := data.table::fcase(
    year <= 2022, "historical",
    year <= 2024, "estimated",
    default = "projected"
  )]

  # Order by year
  data.table::setorder(result, year)

  # Report summary
  n_hist <- sum(result$data_type == "historical")
  n_proj <- sum(result$data_type == "projected")
  cli::cli_alert_success("Loaded {nrow(result)} years: {n_hist} historical, {n_proj} projected")

  # Sample validation
  if (1940 %in% result$year) {
    lpr_1940 <- result[year == 1940, lpr_inflow]
    cli::cli_alert_info("1940 LPR inflow: {lpr_1940} thousand")
  }
  if (2000 %in% result$year) {
    lpr_2000 <- result[year == 2000, lpr_inflow]
    o_2000 <- result[year == 2000, o_inflow]
    cli::cli_alert_info("2000 LPR inflow: {lpr_2000} thousand, O inflow: {o_2000} thousand")
  }

  # Cache result
  if (cache) {
    saveRDS(result, cache_file)
    cli::cli_alert_success("Cached to {cache_file}")
  }

  result
}

#' Get TR2025 Historical LPR Immigration
#'
#' @description
#' Extracts historical LPR immigration data from V.A2 for use in
#' population calculations.
#'
#' @param years Integer vector: years to extract (default: 1940:2024)
#' @param convert_to_persons Logical: if TRUE, multiply by 1000 (default: TRUE)
#' @param data_dir Character: directory containing TR2025 files
#'
#' @return data.table with columns: year, lpr_inflow, lpr_outflow, lpr_aos, lpr_net
#'
#' @export
get_tr2025_historical_lpr <- function(years = 1940:2024,
                                       convert_to_persons = TRUE,
                                       data_dir = "data/raw/SSA_TR2025") {
  # Load full immigration assumptions
  imm <- load_tr2025_immigration_assumptions(data_dir = data_dir)

  # Filter to requested years and LPR columns
  result <- imm[year %in% years, .(year, lpr_inflow, lpr_outflow, lpr_aos, lpr_net, data_type)]

  if (nrow(result) == 0) {
    cli::cli_abort("No data found for years {min(years)}-{max(years)}")
  }

  # Convert to persons if requested
  if (convert_to_persons) {
    result[, lpr_inflow := lpr_inflow * 1000]
    result[, lpr_outflow := lpr_outflow * 1000]
    result[, lpr_aos := lpr_aos * 1000]
    result[, lpr_net := lpr_net * 1000]
  }

  # Handle NAs (early years may have missing AOS data)
  result[is.na(lpr_aos), lpr_aos := 0]

  cli::cli_alert_success("Extracted LPR data for {nrow(result)} years ({min(result$year)}-{max(result$year)})")

  result
}

#' Get TR2025 Historical O (Temporary/Unlawful) Flows
#'
#' @description
#' Extracts historical temporary/unlawfully present population flows
#' from V.A2 for use in O population calculations (Eq 1.4.3).
#'
#' @param years Integer vector: years to extract (default: 1940:2024)
#' @param convert_to_persons Logical: if TRUE, multiply by 1000 (default: TRUE)
#' @param data_dir Character: directory containing TR2025 files
#'
#' @return data.table with columns: year, o_inflow, o_outflow, o_aos, o_net
#'
#' @export
get_tr2025_historical_o_flows <- function(years = 1940:2024,
                                           convert_to_persons = TRUE,
                                           data_dir = "data/raw/SSA_TR2025") {
  # Load full immigration assumptions
  imm <- load_tr2025_immigration_assumptions(data_dir = data_dir)

  # Filter to requested years and O columns
  result <- imm[year %in% years, .(year, o_inflow, o_outflow, o_aos, o_net, data_type)]

  if (nrow(result) == 0) {
    cli::cli_abort("No data found for years {min(years)}-{max(years)}")
  }

  # Convert to persons if requested
  if (convert_to_persons) {
    result[, o_inflow := o_inflow * 1000]
    result[, o_outflow := o_outflow * 1000]
    result[, o_aos := o_aos * 1000]
    result[, o_net := o_net * 1000]
  }

  # Handle NAs (early years have no O data - set to 0)
  result[is.na(o_inflow), o_inflow := 0]
  result[is.na(o_outflow), o_outflow := 0]
  result[is.na(o_aos), o_aos := 0]
  result[is.na(o_net), o_net := 0]

  cli::cli_alert_success("Extracted O flow data for {nrow(result)} years ({min(result$year)}-{max(result$year)})")

  result
}

#' Get TR2025 Combined Immigration Totals
#'
#' @description
#' Returns combined LPR and O immigration totals for validation
#' against total population changes.
#'
#' @param years Integer vector: years to extract
#' @param convert_to_persons Logical: if TRUE, multiply by 1000 (default: TRUE)
#' @param data_dir Character: directory containing TR2025 files
#'
#' @return data.table with year and total_net columns
#'
#' @export
get_tr2025_total_net_immigration <- function(years = 1940:2024,
                                              convert_to_persons = TRUE,
                                              data_dir = "data/raw/SSA_TR2025") {
  imm <- load_tr2025_immigration_assumptions(data_dir = data_dir)

  result <- imm[year %in% years, .(year, total_net, data_type)]

  if (convert_to_persons) {
    result[, total_net := total_net * 1000]
  }

  result[is.na(total_net), total_net := 0]

  result
}

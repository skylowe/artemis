#' TR Data Acquisition
#'
#' Functions for loading SSA Trustees Report data files.
#' These files provide validation targets and historical baselines.
#' Functions are generic and work with any TR year specified in config.
#'
#' @name tr_data
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
load_tr_life_tables_hist <- function(sex = c("both", "male", "female"),
                                          data_dir = NULL) {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
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
                                     data_dir = NULL) {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
  sex <- match.arg(sex)

  # Load full historical life tables
  lt <- load_tr_life_tables_hist(sex = sex, data_dir = data_dir)


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
load_tr_death_probs <- function(sex = c("both", "male", "female"),
                                     alternative = "Alt2",
                                     data_dir = NULL) {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
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
load_tr_death_probs_hist <- function(sex = c("both", "male", "female"),
                                          data_dir = NULL) {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
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
#' @param alternative Character: which alternative to load ("intermediate", "low", "high")
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
#' The table has three sections for Low, Intermediate, and High alternatives.
#' This function loads only the specified alternative.
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
load_tr_immigration_assumptions <- function(data_dir = NULL,
                                                 alternative = "intermediate",
                                                 cache = TRUE,
                                                 cache_dir = "data/cache/tr2025") {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
  alternative <- tolower(alternative)
  if (!alternative %in% c("intermediate", "low", "high")) {
    cli::cli_abort("alternative must be one of: intermediate, low, high")
  }

  # Check for cached data (cache per alternative)
  if (cache) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    cache_file <- file.path(cache_dir, sprintf("immigration_assumptions_va2_%s.rds", alternative))
    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached TR2025 immigration assumptions ({alternative})")
      return(readRDS(cache_file))
    }
  }

  # Find the Excel file
  excel_file <- file.path(data_dir, "SingleYearTRTables_TR2025.xlsx")
  if (!file.exists(excel_file)) {
    cli::cli_abort("TR2025 tables file not found: {excel_file}")
  }

  cli::cli_alert_info("Loading TR2025 Table V.A2 ({alternative} alternative)...")

  # Read the V.A2 sheet
  raw <- readxl::read_excel(excel_file, sheet = "V.A2", col_names = FALSE)
  dt <- data.table::as.data.table(raw)

  # Find section boundaries by looking for headers
  # V.A2 structure:
  #   - "Historical data:" header (common to all alternatives)
  #   - Historical years 1940-2024
  #   - "Intermediate:" header
  #   - Intermediate projected years 2025+
  #   - "Low-cost:" header
  #   - Low projected years 2025+
  #   - "High-cost:" header
  #   - High projected years 2025+

  section_markers <- list(
    historical = "^Historical",
    intermediate = "^Intermediate:?$",
    low = "^Low[- ]?cost:?$",
    high = "^High[- ]?cost:?$"
  )

  # Find the FIRST occurrence of each section header
  section_rows <- list()
  for (i in 1:nrow(dt)) {
    val <- as.character(dt[[1]][i])
    if (is.na(val)) next
    val <- trimws(val)
    for (sect_name in names(section_markers)) {
      # Only record if we haven't found this section yet
      if (is.null(section_rows[[sect_name]]) && grepl(section_markers[[sect_name]], val, ignore.case = TRUE)) {
        section_rows[[sect_name]] <- i
      }
    }
  }

  # Determine rows to parse
  # For any alternative, we need: historical data (1940-2024) + alternative-specific projected data
  if (!alternative %in% names(section_rows)) {
    cli::cli_abort("Could not find {alternative} section in V.A2")
  }

  # Historical section: from "Historical data:" to first alternative header
  hist_start <- section_rows[["historical"]]
  hist_end <- section_rows[["intermediate"]] - 1  # Historical ends before Intermediate

  # Alternative-specific projected section
  proj_start <- section_rows[[alternative]]

  # Find the next section to determine projected end
  alt_order <- c("intermediate", "low", "high")
  current_idx <- which(alt_order == alternative)
  if (current_idx < length(alt_order)) {
    next_alt <- alt_order[current_idx + 1]
    if (next_alt %in% names(section_rows)) {
      proj_end <- section_rows[[next_alt]] - 1
    } else {
      proj_end <- nrow(dt)
    }
  } else {
    proj_end <- nrow(dt)
  }

  cli::cli_alert_info("Parsing historical rows {hist_start} to {hist_end}")
  cli::cli_alert_info("Parsing {alternative} projected rows {proj_start} to {proj_end}")

  # Helper function to parse numeric values - handle "—", footnotes, commas
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

  # Parse the data rows from both historical and projected sections
  result_list <- list()

  # Combine row ranges: historical + projected
  rows_to_parse <- c((hist_start + 1):hist_end, (proj_start + 1):proj_end)

  for (i in rows_to_parse) {
    if (i > nrow(dt)) next

    row_vals <- as.character(unlist(dt[i, ]))

    # Get year - skip section headers and empty rows
    year_str <- trimws(row_vals[1])
    if (is.na(year_str) || year_str == "" || grepl("^[A-Za-z]", year_str)) {
      next
    }

    # Clean year (remove footnote markers like "g", "f")
    year_clean <- gsub("[^0-9]", "", year_str)
    if (nchar(year_clean) != 4) next

    year <- as.integer(year_clean)
    if (is.na(year) || year < 1940 || year > 2100) next

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

  if (length(result_list) == 0) {
    cli::cli_abort("No data rows found in {alternative} section")
  }

  result <- data.table::rbindlist(result_list)

  # Add data type classification
  result[, data_type := data.table::fcase(
    year <= 2022, "historical",
    year <= 2024, "estimated",
    default = "projected"
  )]

  # Add alternative indicator
  result[, alternative := alternative]

  # Order by year
  data.table::setorder(result, year)

  # Report summary
  n_hist <- sum(result$data_type == "historical")
  n_proj <- sum(result$data_type == "projected")
  cli::cli_alert_success("Loaded {nrow(result)} years: {n_hist} historical, {n_proj} projected ({alternative})")

  # Sample validation for intermediate
  if (alternative == "intermediate") {
    if (2027 %in% result$year) {
      lpr_net_2027 <- result[year == 2027, lpr_net]
      o_net_2027 <- result[year == 2027, o_net]
      cli::cli_alert_info("2027 Net LPR: {lpr_net_2027}K, Net O: {o_net_2027}K")
    }
    if (2099 %in% result$year) {
      o_net_2099 <- result[year == 2099, o_net]
      cli::cli_alert_info("2099 Net O: {o_net_2099}K (decreases over time)")
    }
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
#' @param alternative Character: which alternative to load (default: "intermediate")
#'
#' @return data.table with columns: year, lpr_inflow, lpr_outflow, lpr_aos, lpr_net
#'
#' @export
get_tr_historical_lpr <- function(years = 1940:2024,
                                       convert_to_persons = TRUE,
                                       data_dir = NULL,
                                       alternative = "intermediate") {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
  # Load full immigration assumptions
  imm <- load_tr_immigration_assumptions(data_dir = data_dir, alternative = alternative)

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
#' @param alternative Character: which alternative to load (default: "intermediate")
#'
#' @return data.table with columns: year, o_inflow, o_outflow, o_aos, o_net
#'
#' @export
get_tr_historical_o_flows <- function(years = 1940:2024,
                                           convert_to_persons = TRUE,
                                           data_dir = NULL,
                                           alternative = "intermediate") {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
  # Load full immigration assumptions
  imm <- load_tr_immigration_assumptions(data_dir = data_dir, alternative = alternative)

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
#' @param alternative Character: which alternative to load (default: "intermediate")
#'
#' @return data.table with year and total_net columns
#'
#' @export
get_tr_total_net_immigration <- function(years = 1940:2024,
                                              convert_to_persons = TRUE,
                                              data_dir = NULL,
                                              alternative = "intermediate") {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
  imm <- load_tr_immigration_assumptions(data_dir = data_dir, alternative = alternative)

  result <- imm[year %in% years, .(year, total_net, data_type)]

  if (convert_to_persons) {
    result[, total_net := total_net * 1000]
  }

  result[is.na(total_net), total_net := 0]

  result
}

#' Get TR2025 V.A2 Net Immigration for Projection
#'
#' @description
#' Returns net LPR and net O immigration values directly from TR2025 Table V.A2
#' for use in population projections. This ensures exact alignment with
#' Trustees Report assumptions.
#'
#' @param years Integer vector: years to extract (default: 2023:2099)
#' @param alternative Character: which alternative to load ("intermediate", "low", "high")
#' @param data_dir Character: directory containing TR2025 files
#' @param convert_to_persons Logical: if TRUE, multiply by 1000 (default: TRUE)
#'
#' @return data.table with columns: year, lpr_net, o_net, total_net
#'
#' @details
#' Key differences from previous methodology:
#' - Net O immigration varies over time (not constant)
#' - 2025: Net O = 1,192K (high due to policy assumptions)
#' - 2026: Net O = 517K
#' - 2027-2099: Net O declines from 513K to 448K as emigration grows with O stock
#'
#' @section TR2025 Intermediate Values:
#' - Net LPR: 910K (2025-26), 788K (2027+)
#' - Net O: 1,192K (2025), 517K (2026), 513K→448K (2027-2099)
#' - Total Net: Varies from ~2,100K (2025) to ~1,236K (2099)
#'
#' @export
get_tr_va2_net_immigration <- function(years = 2023:2099,
                                            alternative = "intermediate",
                                            data_dir = NULL,
                                            convert_to_persons = TRUE) {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
  # Load V.A2 data for specified alternative
  imm <- load_tr_immigration_assumptions(
    data_dir = data_dir,
    alternative = alternative,
    cache = TRUE
  )

  # Filter to requested years
  result <- imm[year %in% years, .(year, lpr_net, o_net, total_net, data_type)]

  if (nrow(result) == 0) {
    cli::cli_abort("No V.A2 data found for years {min(years)}-{max(years)}")
  }

  # Convert to persons if requested
  if (convert_to_persons) {
    result[, lpr_net := lpr_net * 1000]
    result[, o_net := o_net * 1000]
    result[, total_net := total_net * 1000]
  }

  # Handle NAs
  result[is.na(lpr_net), lpr_net := 0]
  result[is.na(o_net), o_net := 0]
  result[is.na(total_net), total_net := 0]

  # Extend last year's values for years beyond V.A2 data range
  max_data_year <- max(result$year)
  max_requested_year <- max(years)
  if (max_requested_year > max_data_year) {
    extend_years <- (max_data_year + 1L):max_requested_year
    last_row <- result[year == max_data_year]
    extended <- data.table::rbindlist(lapply(extend_years, function(yr) {
      row <- data.table::copy(last_row)
      row[, year := yr]
      row[, data_type := "extended"]
      row
    }))
    result <- data.table::rbindlist(list(result, extended), use.names = TRUE)
    cli::cli_alert_warning(
      "V.A2 data ends at {max_data_year}; extending last-year values through {max_requested_year} ({length(extend_years)} additional years)"
    )
  }

  # Report sample values
  if (2027 %in% result$year) {
    lpr_2027 <- format(result[year == 2027, lpr_net], big.mark = ",")
    o_2027 <- format(result[year == 2027, o_net], big.mark = ",")
    cli::cli_alert_info("2027 V.A2 values: Net LPR = {lpr_2027}, Net O = {o_2027}")
  }
  if (2099 %in% result$year) {
    o_2099 <- format(result[year == 2099, o_net], big.mark = ",")
    cli::cli_alert_info("2099 V.A2 Net O = {o_2099} (decreases from 2027)")
  }

  cli::cli_alert_success(
    "Loaded V.A2 net immigration for {nrow(result)} years ({alternative} alternative)"
  )

  result
}

#' Get TR2025 V.A2 Net O Immigration by Year
#'
#' @description
#' Returns net O immigration values from V.A2 formatted for the projection pipeline.
#' The O emigration is implicit in V.A2's net values - no dynamic calculation needed.
#'
#' @param years Integer vector: years to extract
#' @param alternative Character: which alternative to load
#' @param data_dir Character: directory containing TR2025 files
#' @param distribution data.table: age-sex distribution to apply (optional)
#'
#' @return data.table with net O immigration by year (and age/sex if distribution provided)
#'
#' @export
get_tr_va2_net_o <- function(years = 2023:2099,
                                  alternative = "intermediate",
                                  data_dir = NULL,
                                  distribution = NULL) {
  if (is.null(data_dir)) cli::cli_abort("data_dir is required — pass the TR data directory from config")
  # Get V.A2 totals
  va2 <- get_tr_va2_net_immigration(
    years = years,
    alternative = alternative,
    data_dir = data_dir,
    convert_to_persons = TRUE
  )

  # If no distribution provided, return totals only
  if (is.null(distribution)) {
    return(va2[, .(year, net_o = o_net)])
  }

  # Apply distribution to get age-sex breakdown
  # Distribution should have columns: age, sex, and proportion (summing to 1)
  result_list <- list()

  for (yr in years) {
    yr_total <- va2[year == yr, o_net]
    if (length(yr_total) == 0 || is.na(yr_total)) {
      cli::cli_alert_warning("No V.A2 data for year {yr}")
      next
    }

    # Get proportion column (may be named differently)
    prop_col <- intersect(c("proportion", "prop", "dist", "odist", "implied_dist"), names(distribution))
    if (length(prop_col) == 0) {
      # Calculate proportion if not present
      if ("net_o" %in% names(distribution)) {
        total_dist <- sum(distribution$net_o, na.rm = TRUE)
        distribution[, proportion := net_o / total_dist]
        prop_col <- "proportion"
      } else {
        cli::cli_abort("Distribution must have a proportion column")
      }
    } else {
      prop_col <- prop_col[1]
    }

    yr_dist <- data.table::copy(distribution)
    yr_dist[, year := yr]
    yr_dist[, net_o := yr_total * get(prop_col)]

    result_list[[as.character(yr)]] <- yr_dist[, .(year, age, sex, net_o)]
  }

  result <- data.table::rbindlist(result_list)
  data.table::setorder(result, year, sex, age)

  cli::cli_alert_success("Applied V.A2 net O to age-sex distribution for {length(years)} years")

  result
}

# =============================================================================
# TR2025 POPULATION AND QX LOADING (for target modularization)
# =============================================================================

#' Load TR2025 Population in Long Format
#'
#' @description
#' Loads TR2025 December 31 population data and converts to long format
#' with columns: year, age, sex, population.
#'
#' @param file_path Character: path to SSPopDec file (default: Alt2)
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @export
load_tr_population_long <- function(file_path = NULL) {
  if (is.null(file_path)) cli::cli_abort("file_path is required — pass the TR population file path from config")
  if (!file.exists(file_path)) {
    cli::cli_abort("TR2025 population file not found: {file_path}")
  }

  tr_pop <- data.table::fread(file_path)

  # Convert to long format: year, age, sex, population
  tr_pop_long <- data.table::rbindlist(list(
    tr_pop[, .(year = Year, age = Age, sex = "male", population = `M Tot`)],
    tr_pop[, .(year = Year, age = Age, sex = "female", population = `F Tot`)]
  ))

  cli::cli_alert_success("Loaded TR2025 population: {nrow(tr_pop_long)} rows")
  tr_pop_long
}

#' Load TR2025 Death Probabilities (qx) in Long Format
#'
#' @description
#' Loads TR2025 death probability files and converts to long format
#' with columns: year, age, sex, qx.
#'
#' @param male_file Character: path to male qx file
#' @param female_file Character: path to female qx file
#'
#' @return data.table with columns: year, age, sex, qx
#'
#' @export
load_tr_qx_long <- function(male_file = NULL,
                                 female_file = NULL) {
  if (is.null(male_file) || is.null(female_file)) cli::cli_abort("male_file and female_file are required — pass TR qx file paths from config")
  if (!file.exists(male_file) || !file.exists(female_file)) {
    cli::cli_abort("TR2025 qx files not found")
  }

  # Load and melt male qx
  tr_qx_male <- data.table::fread(male_file, skip = 1)
  tr_qx_m <- data.table::melt(tr_qx_male, id.vars = "Year",
                               variable.name = "age_chr", value.name = "qx")
  tr_qx_m[, age := as.integer(as.character(age_chr))]
  tr_qx_m[, sex := "male"]

  # Load and melt female qx
  tr_qx_female <- data.table::fread(female_file, skip = 1)
  tr_qx_f <- data.table::melt(tr_qx_female, id.vars = "Year",
                               variable.name = "age_chr", value.name = "qx")
  tr_qx_f[, age := as.integer(as.character(age_chr))]
  tr_qx_f[, sex := "female"]

  # Combine
  tr_qx <- data.table::rbindlist(list(tr_qx_m, tr_qx_f))
  tr_qx <- tr_qx[, .(year = Year, age, sex, qx)]

  cli::cli_alert_success("Loaded TR2025 qx: {nrow(tr_qx)} rows")
  tr_qx
}

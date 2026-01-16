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

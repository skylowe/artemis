#' Configuration Loading Functions
#'
#' Functions for loading and validating ARTEMIS configuration files.
#'
#' @name config
NULL

#' Load assumptions configuration
#'
#' @description
#' Loads and validates TR assumptions from a YAML configuration file.
#'
#' @param config_path Character: path to YAML configuration file
#'
#' @return List with validated assumptions
#'
#' @export
load_assumptions <- function(config_path) {
  checkmate::assert_file_exists(config_path)

  config <- yaml::read_yaml(config_path)
  validate_assumptions(config)

  cli::cli_alert_success("Loaded assumptions from {.file {basename(config_path)}}")
  config
}

#' Validate assumptions configuration
#'
#' @description
#' Checks that required fields exist and values are within acceptable ranges.
#' Ensures all TR-specific parameters are explicitly provided.
#'
#' @param config List: parsed configuration
#'
#' @return Invisible TRUE if valid, otherwise throws error
#'
#' @keywords internal
validate_assumptions <- function(config) {
  # Check required top-level sections
  required_sections <- c("metadata", "fertility", "data_sources")
  missing <- setdiff(required_sections, names(config))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required config sections: {.val {missing}}")
  }

  # Validate metadata section - REQUIRED for TR configurability
  metadata <- config$metadata
  if (is.null(metadata$trustees_report_year)) {
    cli::cli_abort(c(
      "Config must specify {.field metadata.trustees_report_year}",
      "i" = "Example: {.code trustees_report_year: 2025}"
    ))
  }
  if (is.null(metadata$alternative_number)) {
    cli::cli_abort(c(
      "Config must specify {.field metadata.alternative_number}",
      "i" = "Values: 1 = Low Cost, 2 = Intermediate, 3 = High Cost"
    ))
  }
  checkmate::assert_integerish(
    metadata$trustees_report_year,
    lower = 2020,
    upper = 2100,
    .var.name = "trustees_report_year"
  )
  checkmate::assert_integerish(
    metadata$alternative_number,
    lower = 1,
    upper = 3,
    .var.name = "alternative_number"
  )

  # Log which TR year and alternative are being used
  alt_name <- switch(as.character(metadata$alternative_number),
                     "1" = "Low Cost",
                     "2" = "Intermediate",
                     "3" = "High Cost")
  cli::cli_alert_info("Config: TR{metadata$trustees_report_year} {alt_name} assumptions")

  # Validate fertility section
  fertility <- config$fertility
  if (!is.null(fertility)) {
    # Check ultimate CTFR is reasonable
    if (!is.null(fertility$ultimate_ctfr)) {
      checkmate::assert_number(
        fertility$ultimate_ctfr,
        lower = 0.5,
        upper = 5.0,
        .var.name = "ultimate_ctfr"
      )
    }

    # Check age range
    if (!is.null(fertility$min_fertility_age) && !is.null(fertility$max_fertility_age)) {
      checkmate::assert_integerish(
        fertility$min_fertility_age,
        lower = 10,
        upper = 20,
        .var.name = "min_fertility_age"
      )
      checkmate::assert_integerish(
        fertility$max_fertility_age,
        lower = 40,
        upper = 55,
        .var.name = "max_fertility_age"
      )
    }
  }

  invisible(TRUE)
}

#' Load API endpoint configuration
#'
#' @description
#' Loads API endpoint configuration from a YAML file.
#'
#' @param config_path Character: path to API config YAML
#'
#' @return List with API configurations
#'
#' @export
load_api_config <- function(config_path) {
  checkmate::assert_file_exists(config_path)

  config <- yaml::read_yaml(config_path)

  cli::cli_alert_success("Loaded API config from {.file {basename(config_path)}}")
  config
}

#' Get config value with default
#'
#' @description
#' Safely retrieve a config value, returning a default if not found.
#'
#' @param config List: configuration object
#' @param ... Character: path components to traverse (e.g., "mortality", "method")
#' @param default Default value if path not found
#'
#' @return Config value or default
#' @export
get_config_with_default <- function(config, ..., default = NULL) {
  path <- c(...)
  result <- config
  for (key in path) {
    if (!is.list(result) || !key %in% names(result)) {
      return(default)
    }
    result <- result[[key]]
  }
  if (is.null(result)) default else result
}

#' Require config value
#'
#' @description
#' Retrieve a required config value, aborting if not found.
#'
#' @param config List: configuration object
#' @param ... Character: path components to traverse
#' @param name Character: descriptive name for error message
#'
#' @return Config value
#' @export
require_config_value <- function(config, ..., name = NULL) {
  path <- c(...)
  result <- config
  for (key in path) {
    if (!is.list(result) || !key %in% names(result)) {
      path_str <- paste(path, collapse = "$")
      name_str <- if (!is.null(name)) paste0(" (", name, ")") else ""
      cli::cli_abort("Required config value not found: {.field {path_str}}{name_str}")
    }
    result <- result[[key]]
  }
  if (is.null(result)) {
    path_str <- paste(path, collapse = "$")
    name_str <- if (!is.null(name)) paste0(" (", name, ")") else ""
    cli::cli_abort("Required config value is NULL: {.field {path_str}}{name_str}")
  }
  result
}

#' Set Census vintage option
#'
#' @description
#' Validate and set the Census vintage option from config.
#'
#' @param config List: configuration object with data_sources$census_vintage
#'
#' @return The vintage value (invisibly)
#' @export
set_census_vintage_option <- function(config) {
  vintage <- config$data_sources$census_vintage
  if (is.null(vintage)) {
    cli::cli_abort("census_vintage not set in config - please specify in your config file (e.g., config/assumptions/tr2025.yaml)")
  }
  options(artemis.census_vintage = vintage)
  invisible(vintage)
}

#' Get TR data directory from config
#'
#' @description
#' Returns the data directory for Trustees Report files based on the
#' trustees_report_year in config. This function does NOT provide a fallback -
#' the TR year must be explicitly specified in config.
#'
#' @param config List: configuration object with metadata$trustees_report_year
#'
#' @return Character: path to TR data directory (e.g., "data/raw/SSA_TR2025")
#'
#' @export
get_tr_data_dir <- function(config) {
  year <- require_config_value(config, "metadata", "trustees_report_year",
                                name = "Trustees Report year")
  paste0("data/raw/SSA_TR", year)
}

#' Resolve TR file path with pattern substitution
#'
#' @description
#' Resolves a Trustees Report file path using config values and pattern substitution.
#' No fallback defaults - all TR-specific parameters come from config.
#'
#' @param config List: configuration object
#' @param file_type Character: type of file (see Details for supported types)
#' @param sex Character: "male" or "female" (optional, for sex-specific files)
#'
#' @return Character: full path to the TR file
#'
#' @details
#' Supported file types:
#' - death_probs_proj: Projected death probabilities (DeathProbsE_{sex}_Alt{alt}_TR{year}.csv)
#' - death_probs_hist: Historical death probabilities (DeathProbsE_{sex}_Hist_TR{year}.csv)
#' - life_tables_proj: Projected period life tables (PerLifeTables_{sex}_Alt{alt}_TR{year}.csv)
#' - life_tables_hist: Historical period life tables (PerLifeTables_{sex}_Hist_TR{year}.csv)
#' - population_dec: December 31 population (SSPopDec_Alt{alt}_TR{year}.csv)
#' - population_jan: January 1 population (SSPopJan_Alt{alt}_TR{year}.csv)
#' - population_jul: July 1 population (SSPopJul_Alt{alt}_TR{year}.csv)
#' - qx_proj_m: Projected male qx (qxprdM_Alt{alt}_TR{year}.csv)
#' - qx_proj_f: Projected female qx (qxprdF_Alt{alt}_TR{year}.csv)
#' - single_year_tables: Single year tables Excel file (SingleYearTRTables_TR{year}.xlsx)
#'
#' @export
resolve_tr_file <- function(config, file_type, sex = NULL) {
  year <- require_config_value(config, "metadata", "trustees_report_year")
  alt_num <- require_config_value(config, "metadata", "alternative_number")
  data_dir <- get_tr_data_dir(config)

  patterns <- list(
    death_probs_proj = "DeathProbsE_{sex}_Alt{alt}_TR{year}.csv",
    death_probs_hist = "DeathProbsE_{sex}_Hist_TR{year}.csv",
    life_tables_proj = "PerLifeTables_{sex}_Alt{alt}_TR{year}.csv",
    life_tables_hist = "PerLifeTables_{sex}_Hist_TR{year}.csv",
    population_dec = "SSPopDec_Alt{alt}_TR{year}.csv",
    population_jan = "SSPopJan_Alt{alt}_TR{year}.csv",
    population_jul = "SSPopJul_Alt{alt}_TR{year}.csv",
    qx_proj_m = "qxprdM_Alt{alt}_TR{year}.csv",
    qx_proj_f = "qxprdF_Alt{alt}_TR{year}.csv",
    single_year_tables = "SingleYearTRTables_TR{year}.xlsx"
  )

  pattern <- patterns[[file_type]]
  if (is.null(pattern)) {
    cli::cli_abort("Unknown TR file type: {.val {file_type}}. Valid types: {.val {names(patterns)}}")
  }

  # Substitute pattern variables
  filename <- gsub("\\{year\\}", year, pattern)
  filename <- gsub("\\{alt\\}", alt_num, filename)
  if (!is.null(sex)) {
    sex_code <- if (tolower(sex) == "male") "M" else "F"
    filename <- gsub("\\{sex\\}", sex_code, filename)
  }

  file.path(data_dir, filename)
}

#' Resolve TR file path (legacy)
#'
#' @description
#' Resolve a TR file path from config, with fallback to default location.
#' This is a legacy function - prefer using resolve_tr_file() for new code.
#'
#' @param config List: configuration object
#' @param config_path Character vector: path in config to file location
#' @param default_filename Character: default filename if not in config
#' @param data_dir Character: default data directory (derived from config if NULL)
#'
#' @return Resolved file path
#' @export
resolve_tr_file_path <- function(config, config_path, default_filename,
                                 data_dir = NULL) {
  # Try to get from config path first
  file_path <- get_config_with_default(config, config_path, default = NULL)
  if (!is.null(file_path) && file_path != "") {
    return(file_path)
  }

  # Fall back to data_dir + default_filename
  if (is.null(data_dir)) {
    # Try to derive from config
    data_dir <- tryCatch(
      get_tr_data_dir(config),
      error = function(e) "data/raw/SSA_TR2025"  # Legacy fallback
    )
  }
  file.path(data_dir, default_filename)
}

#' Select population source based on config
#'
#' @description
#' Helper to select between TR2025 and Census population based on config setting.
#'
#' @param tr_pop data.table: TR2025 population data
#' @param census_pop data.table: Census population data
#' @param use_tr Logical: whether to use TR2025 (from config)
#'
#' @return Selected population data.table
#' @export
select_population_source <- function(tr_pop, census_pop, use_tr) {
  if (isTRUE(use_tr)) {
    cli::cli_alert_info("Using TR2025 population for calculations")
    tr_pop
  } else {
    cli::cli_alert_info("Using Census population for calculations")
    census_pop
  }
}

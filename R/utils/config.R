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
    cli::cli_abort("census_vintage not set in config - please specify in config/assumptions/tr2025.yaml")
  }
  options(artemis.census_vintage = vintage)
  invisible(vintage)
}

#' Resolve TR2025 file path
#'
#' @description
#' Resolve a TR2025 file path from config, with fallback to default location.
#'
#' @param config List: configuration object
#' @param config_path Character vector: path in config to file location
#' @param default_filename Character: default filename if not in config
#' @param data_dir Character: default data directory
#'
#' @return Resolved file path
#' @export
resolve_tr2025_file_path <- function(config, config_path, default_filename,
                                      data_dir = "data/raw/SSA_TR2025") {
  file_path <- get_config_with_default(config, config_path, default = NULL)
  if (is.null(file_path) || file_path == "") {
    file.path(data_dir, default_filename)
  } else {
    file_path
  }
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

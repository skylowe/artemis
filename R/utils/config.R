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

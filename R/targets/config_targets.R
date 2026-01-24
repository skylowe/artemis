#' Configuration Targets
#'
#' @description
#' Factory function that creates targets for loading configuration files.
#'
#' @name config_targets
NULL

#' Get assumptions config file path
#'
#' @description
#' Returns the path to the assumptions config file. Uses the ARTEMIS_CONFIG
#' environment variable if set, otherwise falls back to the default TR2025 config.
#'
#' @return Character: path to config file
#'
#' @details
#' To switch between Trustees Report years:
#' ```
#' # Use TR2025 (default)
#' export ARTEMIS_CONFIG=config/assumptions/tr2025.yaml
#'
#' # Use TR2026
#' export ARTEMIS_CONFIG=config/assumptions/tr2026.yaml
#' ```
#'
#' @keywords internal
get_config_file <- function() {
  config_file <- Sys.getenv("ARTEMIS_CONFIG", "")
  if (config_file == "") {
    config_file <- "config/assumptions/tr2025.yaml"
    cli::cli_alert_info("Using default config: {.file {config_file}}")
    cli::cli_alert_info("Set {.envvar ARTEMIS_CONFIG} to use a different config file")
    # Only use here::here for relative paths
    return(here::here(config_file))
  } else {
    cli::cli_alert_success("Using config from {.envvar ARTEMIS_CONFIG}: {.file {config_file}}")
  }
  # If path is absolute (starts with / or drive letter), return as-is
  # Otherwise use here::here to resolve relative to project root
  if (startsWith(config_file, "/") || grepl("^[A-Za-z]:", config_file)) {
    config_file
  } else {
    here::here(config_file)
  }
}

#' Create configuration targets
#'
#' @description
#' Creates targets for loading configuration files that drive the pipeline.
#' The assumptions config file can be overridden via the ARTEMIS_CONFIG
#' environment variable.
#'
#' @return List of targets for loading configuration
#'
#' @export
create_config_targets <- function() {
  list(
    targets::tar_target(
      config_assumptions,
      load_assumptions(get_config_file()),
      cue = targets::tar_cue(mode = "thorough")
    ),
    targets::tar_target(
      config_api,
      load_api_config(here::here("config/api_endpoints.yaml")),
      cue = targets::tar_cue(mode = "thorough")
    )
  )
}

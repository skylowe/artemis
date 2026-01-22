#' Configuration Targets
#'
#' @description
#' Factory function that creates targets for loading configuration files.
#'
#' @name config_targets
NULL

#' Create configuration targets
#'
#' @description
#' Creates targets for loading configuration files that drive the pipeline.
#'
#' @return List of targets for loading configuration
#'
#' @export
create_config_targets <- function() {
  list(
    targets::tar_target(
      config_assumptions,
      load_assumptions(here::here("config/assumptions/tr2025.yaml")),
      cue = targets::tar_cue(mode = "thorough")
    ),
    targets::tar_target(
      config_api,
      load_api_config(here::here("config/api_endpoints.yaml")),
      cue = targets::tar_cue(mode = "thorough")
    )
  )
}

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
  # Version-stable RDS format — deterministic across R patch versions.
  # saveRDS() embeds the writer's R version in the file header (e.g., 4.5.0
  # vs 4.5.2), so identical objects produce different file hashes when built
  # on the host (R 4.5.2) vs in the container (R 4.5.0). This defeats
  # {targets} early cutoff for unchanged config sections.
  # Fix: write uncompressed RDS, then zero out the 4-byte R version field
  # (header bytes 7-10). readRDS() ignores these bytes, so the files remain
  # fully readable.
  format_config <- targets::tar_format(
    read = function(path) readRDS(path),
    write = function(object, path) {
      saveRDS(object, path, compress = FALSE)
      raw <- readBin(path, "raw", file.info(path)$size)
      raw[7:10] <- as.raw(0)
      writeBin(raw, path)
    }
  )

  list(
    # Track the YAML file itself — format="file" hashes file CONTENT,
    # mode="always" re-checks on every tar_make() so env var changes
    # (ARTEMIS_CONFIG) are detected automatically.
    targets::tar_target(
      config_file,
      get_config_file(),
      format = "file",
      cue = targets::tar_cue(mode = "always")
    ),

    # Full config — rebuilds when file content changes
    targets::tar_target(
      config_assumptions,
      load_assumptions(config_file)
    ),

    # Keep config_api unchanged
    targets::tar_target(
      config_api,
      load_api_config(here::here("config/api_endpoints.yaml")),
      cue = targets::tar_cue(mode = "thorough")
    ),

    # ── Domain config gates (one per demography subprocess) ──────────
    # Each extracts its subprocess's config section.
    # Early cutoff: if the section didn't change, downstream targets skip.
    #
    # Uses uncompressed RDS (format_config) instead of the default gzip-
    # compressed RDS. Gzip output varies across R sessions/environments,
    # so the same R object can produce different file hashes — defeating
    # early cutoff when host and container serialize separately.
    # Uncompressed RDS is deterministic: identical objects always produce
    # identical bytes on disk.

    # Subprocess 1: Fertility
    targets::tar_target(config_fertility, config_assumptions$fertility,
                        format = format_config),

    # Subprocess 2: Mortality
    targets::tar_target(config_mortality, config_assumptions$mortality,
                        format = format_config),

    # Subprocess 3: LPR Immigration (lpr, emigration, va2, cbo)
    targets::tar_target(config_lpr_immigration, config_assumptions$immigration[c(
      "lpr", "emigration", "va2_alternative", "va2_file", "cbo_file",
      "ultimate_net_lpr_immigration"
    )], format = format_config),

    # Subprocess 5: Temp/Unlawful Immigration (o_immigration, daca)
    targets::tar_target(config_o_immigration, config_assumptions$immigration$o_immigration,
                        format = format_config),

    # Subprocess 4: Historical Population
    targets::tar_target(config_historical_pop, config_assumptions$historical_population,
                        format = format_config),

    # Subprocess 6: Marriage
    targets::tar_target(config_marriage, config_assumptions$marriage,
                        format = format_config),

    # Subprocess 7: Divorce
    targets::tar_target(config_divorce, config_assumptions$divorce,
                        format = format_config),

    # Subprocess 8: Projected Population
    targets::tar_target(config_projected_pop, config_assumptions$projected_population,
                        format = format_config),

    # Narrow gate: population_status only (gay/lesbian %, etc.)
    # Prevents net_o_source changes from invalidating historical pop targets
    # that only need orientation percentages.
    targets::tar_target(config_population_status,
                        config_assumptions$projected_population$population_status,
                        format = format_config),

    # ── Economics config gate ──────────────────────────────────────
    # Subprocess 2.1: US Employment
    targets::tar_target(config_economics, config_assumptions$economics,
                        format = format_config),

    # Cross-cutting sections
    targets::tar_target(config_metadata, config_assumptions$metadata,
                        format = format_config),
    targets::tar_target(config_data_sources, config_assumptions$data_sources,
                        format = format_config),
    targets::tar_target(config_runtime, config_assumptions$runtime,
                        format = format_config)
  )
}

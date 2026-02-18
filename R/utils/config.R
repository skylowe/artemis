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
#' @param config_path Character: path to YAML or RDS configuration file
#'
#' @return List with validated assumptions
#'
#' @export
load_assumptions <- function(config_path) {
  checkmate::assert_file_exists(config_path)

  # Support RDS format (used by scenario engine to avoid lossy YAML round-trip)
  if (grepl("\\.rds$", config_path, ignore.case = TRUE)) {
    config <- readRDS(config_path)
  } else {
    config <- yaml::read_yaml(config_path)
  }

  # Fill in defaults derived from trustees_report_year and end_year

  config <- derive_config_defaults(config)

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

    # Check reference_age - TR2025 methodology uses age 30
    if (!is.null(fertility$reference_age)) {
      min_age <- if (!is.null(fertility$min_fertility_age)) fertility$min_fertility_age else 14
      max_age <- if (!is.null(fertility$max_fertility_age)) fertility$max_fertility_age else 49
      checkmate::assert_integerish(
        fertility$reference_age,
        lower = min_age,
        upper = max_age,
        .var.name = "reference_age"
      )
      if (fertility$reference_age != 30) {
        cli::cli_warn(c(
          "Non-standard {.field reference_age}: {fertility$reference_age}",
          "i" = "TR methodology uses age 30 as the reference age",
          "i" = "Changing this value deviates from standard methodology"
        ))
      }
    }
  }

  # Warn about inconsistent derived values (non-fatal)
  warn_config_inconsistencies(config)

  invisible(TRUE)
}

#' Derive config defaults from trustees_report_year
#'
#' @description
#' Fills in NULL/missing config values using derivation rules based on
#' `metadata$trustees_report_year` and `metadata$projection_period$end_year`.
#' Explicit values in the config are never overwritten (null-coalescing logic).
#'
#' Called automatically by `load_assumptions()` before validation.
#'
#' @param config List: parsed configuration (from YAML or RDS)
#'
#' @return Config list with derived defaults filled in
#'
#' @keywords internal
derive_config_defaults <- function(config) {
  tr_year <- config$metadata$trustees_report_year
  if (is.null(tr_year)) return(config)

  alt_num <- config$metadata$alternative_number %||% 2L
  tr_data_dir <- paste0("data/raw/SSA_TR", tr_year)

  # Master projection period
  proj_start <- config$metadata$projection_period$start_year %||% (tr_year - 2L)
  proj_end <- config$metadata$projection_period$end_year %||% (tr_year + 74L)
  config$metadata$projection_period$start_year <- proj_start
  config$metadata$projection_period$end_year <- proj_end

  # Fertility
  fert <- config$fertility
  if (!is.null(fert)) {
    config$fertility$projection_start_year <- fert$projection_start_year %||% tr_year
    config$fertility$ultimate_year <- fert$ultimate_year %||% (tr_year + 25L)
    config$fertility$rate_base_year <- fert$rate_base_year %||% (tr_year - 1L)
  }

  # Mortality starting qx
  if (!is.null(config$mortality$starting_tr_qx)) {
    config$mortality$starting_tr_qx$base_year <-
      config$mortality$starting_tr_qx$base_year %||% proj_start
  }

  # Marriage
  if (!is.null(config$marriage)) {
    config$marriage$ultimate_year <- config$marriage$ultimate_year %||% (tr_year + 24L)
    config$marriage$acs_end <- config$marriage$acs_end %||% (tr_year - 3L)
  }

  # Divorce
  if (!is.null(config$divorce)) {
    config$divorce$ultimate_year <- config$divorce$ultimate_year %||% (tr_year + 24L)
    config$divorce$historical_end_year <- config$divorce$historical_end_year %||% (tr_year - 3L)
  }

  # Historical population
  if (!is.null(config$historical_population)) {
    config$historical_population$end_year <-
      config$historical_population$end_year %||% (tr_year - 3L)
    hist_end <- config$historical_population$end_year
    if (is.null(config$historical_population$tab_years$final)) {
      config$historical_population$tab_years$final <- list(hist_end)
    }
  }

  # Projected population
  pp <- config$projected_population
  if (!is.null(pp)) {
    config$projected_population$starting_year <- pp$starting_year %||% (proj_start - 1L)
    config$projected_population$projection_start <- pp$projection_start %||% proj_start
    config$projected_population$projection_end <- pp$projection_end %||% proj_end
  }

  # Data sources
  ds <- config$data_sources
  if (!is.null(ds)) {
    config$data_sources$historical_birth_data$end_year <-
      ds$historical_birth_data$end_year %||% (tr_year - 1L)
    config$data_sources$population_estimates$end_year <-
      ds$population_estimates$end_year %||% (tr_year - 1L)
  }

  # TR file paths
  if (!is.null(config$projected_population)) {
    config$projected_population$tr_historical_population_file <-
      config$projected_population$tr_historical_population_file %||%
      file.path(tr_data_dir, sprintf("SSPopDec_Alt%d_TR%d.csv", alt_num, tr_year))
  }
  if (!is.null(config$immigration)) {
    config$immigration$va2_file <-
      config$immigration$va2_file %||%
      file.path(tr_data_dir, sprintf("SingleYearTRTables_TR%d.xlsx", tr_year))
  }

  # Mortality TR qx file paths
  if (!is.null(config$mortality$starting_tr_qx)) {
    stqx <- config$mortality$starting_tr_qx
    config$mortality$starting_tr_qx$male_qx_file <-
      stqx$male_qx_file %||%
      file.path(tr_data_dir, sprintf("DeathProbsE_M_Alt%d_TR%d.csv", alt_num, tr_year))
    config$mortality$starting_tr_qx$female_qx_file <-
      stqx$female_qx_file %||%
      file.path(tr_data_dir, sprintf("DeathProbsE_F_Alt%d_TR%d.csv", alt_num, tr_year))
    config$mortality$starting_tr_qx$male_qx_hist_file <-
      stqx$male_qx_hist_file %||%
      file.path(tr_data_dir, sprintf("DeathProbsE_M_Hist_TR%d.csv", tr_year))
    config$mortality$starting_tr_qx$female_qx_hist_file <-
      stqx$female_qx_hist_file %||%
      file.path(tr_data_dir, sprintf("DeathProbsE_F_Hist_TR%d.csv", tr_year))
  }

  config
}

#' Warn about inconsistent derived config values
#'
#' @description
#' Issues warnings (not errors) when derived values seem inconsistent
#' with the trustees_report_year derivation rules.
#'
#' @param config List: configuration after derive_config_defaults()
#'
#' @keywords internal
warn_config_inconsistencies <- function(config) {
  tr_year <- config$metadata$trustees_report_year
  if (is.null(tr_year)) return(invisible())

  proj_start <- config$metadata$projection_period$start_year

  # Fertility projection_start_year should equal TR_YEAR
  if (!is.null(config$fertility$projection_start_year) &&
      config$fertility$projection_start_year != tr_year) {
    cli::cli_warn(c(
      "fertility.projection_start_year ({config$fertility$projection_start_year}) != trustees_report_year ({tr_year})",
      "i" = "Default derivation: TR_YEAR = {tr_year}"
    ))
  }

  # projected_population.starting_year should equal projection_period.start_year - 1
  if (!is.null(config$projected_population$starting_year) &&
      config$projected_population$starting_year != (proj_start - 1L)) {
    cli::cli_warn(c(
      "projected_population.starting_year ({config$projected_population$starting_year}) != projection_period.start_year - 1 ({proj_start - 1L})",
      "i" = "Default derivation: proj_start - 1 = {proj_start - 1L}"
    ))
  }

  proj_end <- config$metadata$projection_period$end_year

  # Marriage/divorce ultimate_year should be within projection range
  for (domain in c("marriage", "divorce")) {
    uy <- config[[domain]]$ultimate_year
    if (!is.null(uy) && (uy < proj_start || uy > proj_end)) {
      cli::cli_warn(c(
        "{domain}.ultimate_year ({uy}) is outside projection range ({proj_start}-{proj_end})"
      ))
    }
  }

  # projected_population.projection_end should equal projection_period.end_year
  pp_end <- config$projected_population$projection_end
  if (!is.null(pp_end) && !is.null(proj_end) && pp_end != proj_end) {
    cli::cli_warn(c(
      "projected_population.projection_end ({pp_end}) != metadata.projection_period.end_year ({proj_end})",
      "i" = "Remove the explicit projection_end from config to auto-derive from the master end_year"
    ))
  }

  # historical_population.end_year should be < projection_period.start_year
  hist_end <- config$historical_population$end_year
  if (!is.null(hist_end) && hist_end >= proj_start) {
    cli::cli_warn(c(
      "historical_population.end_year ({hist_end}) >= projection_period.start_year ({proj_start})",
      "i" = "Historical end should be before projection start"
    ))
  }

  invisible()
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
      error = function(e) {
        # Derive from TR year if available, else error
        yr <- get_config_with_default(config, "metadata", "trustees_report_year", default = NULL)
        if (!is.null(yr)) paste0("data/raw/SSA_TR", yr)
        else cli::cli_abort("Cannot resolve TR file path: no data_dir, va2_file, or trustees_report_year in config")
      }
    )
  }
  file.path(data_dir, default_filename)
}

#' Select population source based on config
#' Get projection year parameters from config
#'
#' @description
#' Extracts projection year parameters from config, with fallback defaults
#' for backward compatibility.
#'
#' @param config List: configuration object
#' @param component Character: "population", "marriage", "divorce", or "mortality"
#'
#' @return List with starting_year, projection_start, projection_end, ultimate_year
#'
#' @export
get_projection_years <- function(config, component = "population") {
  # Get global projection period from metadata

  # Derive fallback from TR year if available
  tr_year <- get_config_with_default(config, "metadata", "trustees_report_year", default = 2025L)
  default_proj_start <- tr_year - 2L
  default_proj_end <- tr_year + 74L

  proj_period <- get_config_with_default(
    config, "metadata", "projection_period",
    default = list(start_year = default_proj_start, end_year = default_proj_end)
  )

  # Get component-specific config
  comp_config <- switch(component,
    "population" = config$projected_population,
    "marriage" = config$marriage,
    "divorce" = config$divorce,
    "mortality" = config$mortality,
    NULL
  )

  list(
    starting_year = get_config_with_default(
      comp_config, "starting_year",
      default = proj_period$start_year - 1
    ),
    projection_start = proj_period$start_year,
    projection_end = proj_period$end_year,
    ultimate_year = get_config_with_default(
      comp_config, "ultimate_year",
      default = tr_year + 24L
    )
  )
}

#' Generate cache filename with year range
#'
#' @description
#' Creates a standardized cache filename that includes the year range.
#' Useful for ensuring cache files are invalidated when year ranges change.
#'
#' @param base_name Character: base name (e.g., "projected_rates")
#' @param start_year Integer: start year
#' @param end_year Integer: end year
#' @param ext Character: file extension (default: "rds")
#'
#' @return Character: filename like "projected_rates_2023_2099.rds"
#'
#' @export
get_cache_filename <- function(base_name, start_year, end_year, ext = "rds") {
  sprintf("%s_%d_%d.%s", base_name, start_year, end_year, ext)
}

# Configuration Utilities
# =============================================================================
# Helper functions for configuration manipulation

#' Deep merge two configuration lists
#'
#' @param base Base configuration list
#' @param override Override values (takes precedence)
#' @return Merged configuration
merge_configs <- function(base, override) {
  if (is.null(override)) return(base)
  if (is.null(base)) return(override)

  for (key in names(override)) {
    if (is.list(override[[key]]) && is.list(base[[key]])) {
      base[[key]] <- merge_configs(base[[key]], override[[key]])
    } else {
      base[[key]] <- override[[key]]
    }
  }

  base
}

#' Get nested value from config
#'
#' @param config Configuration list
#' @param path Character vector path (e.g., c("fertility", "ultimate_ctfr"))
#' @param default Default value if not found
#' @return Value at path or default
get_nested <- function(config, path, default = NULL) {
  result <- config
  for (key in path) {
    if (!is.list(result) || !key %in% names(result)) {
      return(default)
    }
    result <- result[[key]]
  }
  if (is.null(result)) default else result
}

#' Set nested value in config
#'
#' @param config Configuration list
#' @param path Character vector path
#' @param value Value to set
#' @return Modified configuration
set_nested <- function(config, path, value) {
  if (length(path) == 0) return(config)

  if (length(path) == 1) {
    config[[path]] <- value
    return(config)
  }

  if (is.null(config[[path[1]]])) {
    config[[path[1]]] <- list()
  }

  config[[path[1]]] <- set_nested(config[[path[1]]], path[-1], value)
  config
}

#' Validate configuration values
#'
#' @param config Configuration list
#' @return List with valid (logical) and errors (character vector)
validate_config <- function(config) {
  errors <- character()

  # Check required sections
  required <- c("metadata", "fertility", "mortality", "immigration")
  missing <- setdiff(required, names(config))
  if (length(missing) > 0) {
    errors <- c(errors, paste("Missing sections:", paste(missing, collapse = ", ")))
  }

  # Validate fertility
  if (!is.null(config$fertility)) {
    tfr <- config$fertility$ultimate_ctfr
    if (!is.null(tfr) && (tfr < 0.5 || tfr > 5.0)) {
      errors <- c(errors, "Ultimate TFR must be between 0.5 and 5.0")
    }
  }

  # Validate mortality
  if (!is.null(config$mortality)) {
    method <- config$mortality$starting_aax_method
    if (!is.null(method) && !method %in% c("regression", "tr_qx")) {
      errors <- c(errors, paste("Invalid starting_aax_method:", method))
    }
  }

  # Validate immigration
  if (!is.null(config$immigration)) {
    alt <- config$immigration$va2_alternative
    if (!is.null(alt) && !alt %in% c("low", "intermediate", "high")) {
      errors <- c(errors, paste("Invalid va2_alternative:", alt))
    }

    ratio <- config$immigration$emigration$ratio
    if (!is.null(ratio) && (ratio < 0 || ratio > 1)) {
      errors <- c(errors, "Emigration ratio must be between 0 and 1")
    }
  }

  # Validate marriage
  if (!is.null(config$marriage)) {
    amr <- config$marriage$ultimate_amr
    if (!is.null(amr) && (amr < 1000 || amr > 10000)) {
      errors <- c(errors, "Ultimate AMR must be between 1000 and 10000")
    }
  }

  # Validate divorce
  if (!is.null(config$divorce)) {
    adr <- config$divorce$ultimate_adr
    if (!is.null(adr) && (adr < 500 || adr > 5000)) {
      errors <- c(errors, "Ultimate ADR must be between 500 and 5000")
    }
  }

  list(
    valid = length(errors) == 0,
    errors = errors
  )
}

#' Create config diff between two configurations
#'
#' @param base Base configuration
#' @param modified Modified configuration
#' @param path Current path (for recursion)
#' @return data.frame with parameter, base_value, new_value
diff_configs <- function(base, modified, path = character()) {
  diffs <- list()

  all_keys <- union(names(base), names(modified))

  for (key in all_keys) {
    current_path <- c(path, key)
    path_str <- paste(current_path, collapse = "$")

    base_val <- base[[key]]
    mod_val <- modified[[key]]

    if (is.list(base_val) && is.list(mod_val)) {
      # Recurse into nested lists
      nested_diffs <- diff_configs(base_val, mod_val, current_path)
      diffs <- c(diffs, list(nested_diffs))
    } else if (!identical(base_val, mod_val)) {
      diffs <- c(diffs, list(data.frame(
        parameter = path_str,
        base_value = toString(base_val),
        new_value = toString(mod_val),
        stringsAsFactors = FALSE
      )))
    }
  }

  if (length(diffs) > 0) {
    do.call(rbind, diffs)
  } else {
    data.frame(
      parameter = character(),
      base_value = character(),
      new_value = character(),
      stringsAsFactors = FALSE
    )
  }
}

#' Format config for display
#'
#' @param config Configuration list
#' @return Character string (formatted YAML)
format_config <- function(config) {
  yaml::as.yaml(config)
}

#' Summarize key configuration parameters
#'
#' @param config Configuration list
#' @return data.frame with key parameters
summarize_config <- function(config) {
  params <- list(
    list("TR Year", config$metadata$trustees_report_year),
    list("Alternative", config$metadata$alternative),
    list("Ultimate TFR", config$fertility$ultimate_ctfr),
    list("TFR Ultimate Year", config$fertility$ultimate_year),
    list("Mortality Ultimate Year", config$mortality$ultimate_year),
    list("Immigration Scenario", config$immigration$va2_alternative),
    list("Ultimate AMR", config$marriage$ultimate_amr),
    list("Ultimate ADR", config$divorce$ultimate_adr),
    list("Use TR2025 Population", config$projected_population$use_tr_historical_population)
  )

  data.frame(
    Parameter = sapply(params, `[[`, 1),
    Value = sapply(params, function(x) toString(x[[2]])),
    stringsAsFactors = FALSE
  )
}

#' Create a modified config for sensitivity analysis
#'
#' @param base_config Base configuration
#' @param param_path Path to parameter
#' @param values Vector of values to test
#' @return List of modified configs
create_sensitivity_configs <- function(base_config, param_path, values) {
  configs <- lapply(values, function(val) {
    config <- base_config
    config <- set_nested(config, param_path, val)
    attr(config, "sensitivity_value") <- val
    config
  })

  names(configs) <- paste(paste(param_path, collapse = "_"), values, sep = "_")
  configs
}

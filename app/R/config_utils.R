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

  # Validate projection end year
  end_year <- config$metadata$projection_period$end_year
  if (!is.null(end_year)) {
    start_year <- config$metadata$projection_period$start_year %||% 2023
    if (end_year < start_year + 10) {
      errors <- c(errors, paste("Projection end year must be at least", start_year + 10))
    }
    if (end_year > 2200) {
      errors <- c(errors, "Projection end year must be at most 2200")
    }
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

    hmd_cfg <- config$mortality$hmd_calibration
    if (!is.null(hmd_cfg$enabled) && !is.logical(hmd_cfg$enabled)) {
      errors <- c(errors, "hmd_calibration.enabled must be TRUE or FALSE")
    }

    aax_cap <- config$mortality$elderly_aax_cap
    if (!is.null(aax_cap)) {
      if (!is.null(aax_cap$enabled) && !is.logical(aax_cap$enabled)) {
        errors <- c(errors, "elderly_aax_cap.enabled must be TRUE or FALSE")
      }
      if (!is.null(aax_cap$max_aax) && (aax_cap$max_aax < 0.001 || aax_cap$max_aax > 0.02)) {
        errors <- c(errors, "elderly_aax_cap.max_aax must be between 0.001 and 0.02")
      }
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

  # Validate O-immigration
  if (!is.null(config$immigration$o_immigration)) {
    o_imm <- config$immigration$o_immigration
    if (!is.null(o_imm$ultimate_gross_o) && (o_imm$ultimate_gross_o < 0 || o_imm$ultimate_gross_o > 5000000)) {
      errors <- c(errors, "Ultimate Gross O must be between 0 and 5,000,000")
    }
    if (!is.null(o_imm$transition_gross_o) && (o_imm$transition_gross_o < 0 || o_imm$transition_gross_o > 5000000)) {
      errors <- c(errors, "Transition Gross O must be between 0 and 5,000,000")
    }
    dr <- o_imm$departure_rates
    if (!is.null(dr)) {
      dr_fields <- c("n_pre_2015_multiplier", "n_post_2015_multiplier", "n_recent_arrival_multiplier",
                      "ni_initial_multiplier", "v_pre_2015_multiplier", "v_post_2015_multiplier")
      for (f in dr_fields) {
        val <- dr[[f]]
        if (!is.null(val) && (val < 0 || val > 5.0)) {
          errors <- c(errors, paste("Departure rate", f, "must be between 0 and 5.0"))
        }
      }
      if (!is.null(dr$daca_rate_reduction) && (dr$daca_rate_reduction < 0 || dr$daca_rate_reduction > 1)) {
        errors <- c(errors, "DACA rate reduction must be between 0 and 1")
      }
    }
  }

  # Validate COVID factors
  covid <- config$mortality$covid_adjustments
  if (!is.null(covid)) {
    for (yr in c("2024", "2025")) {
      if (!is.null(covid[[yr]])) {
        for (ag in names(covid[[yr]])) {
          val <- covid[[yr]][[ag]]
          if (!is.null(val) && (val < 0.5 || val > 2.0)) {
            errors <- c(errors, paste0("COVID factor ", yr, " ", ag, " must be between 0.5 and 2.0"))
          }
        }
      }
    }
  }

  # Validate projected population
  if (!is.null(config$projected_population)) {
    srb <- config$projected_population$sex_ratio_at_birth
    if (!is.null(srb) && (srb < 1000 || srb > 1100)) {
      errors <- c(errors, "Sex ratio at birth must be between 1000 and 1100")
    }
    ps <- config$projected_population$population_status
    if (!is.null(ps)) {
      if (!is.null(ps$gay_percent) && (ps$gay_percent < 0 || ps$gay_percent > 0.20)) {
        errors <- c(errors, "Gay population percent must be between 0 and 0.20 (20%)")
      }
      if (!is.null(ps$lesbian_percent) && (ps$lesbian_percent < 0 || ps$lesbian_percent > 0.20)) {
        errors <- c(errors, "Lesbian population percent must be between 0 and 0.20 (20%)")
      }
    }
  }

  # Validate same-sex marriage fraction
  ss_frac <- config$marriage$same_sex$default_fraction
  if (!is.null(ss_frac) && (ss_frac < 0 || ss_frac > 0.30)) {
    errors <- c(errors, "Same-sex marriage fraction must be between 0 and 0.30 (30%)")
  }

  # Validate elderly override
  eld <- config$immigration$lpr$elderly_override_tr_derived
  if (!is.null(eld)) {
    for (grp in c("ages_65_84", "ages_85_99", "age_100_plus")) {
      if (!is.null(eld[[grp]])) {
        at <- eld[[grp]]$annual_total
        if (!is.null(at) && at < 0) {
          errors <- c(errors, paste("Elderly override", grp, "annual_total must be >= 0"))
        }
        fs <- eld[[grp]]$female_share
        if (!is.null(fs) && (fs < 0 || fs > 1)) {
          errors <- c(errors, paste("Elderly override", grp, "female_share must be between 0 and 1"))
        }
      }
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
    list("End Year", config$metadata$projection_period$end_year),
    list("Ultimate TFR", config$fertility$ultimate_ctfr),
    list("TFR Ultimate Year", config$fertility$ultimate_year),
    list("Mortality Ultimate Year", config$mortality$ultimate_year),
    list("Immigration Scenario", config$immigration$va2_alternative),
    list("Ultimate AMR", config$marriage$ultimate_amr),
    list("Ultimate ADR", config$divorce$ultimate_adr),
    list("Population Source", config$historical_population$population_source %||% "hybrid"),
    list("O-Pop Method", config$historical_population$o_population$method %||% "residual"),
    list("Ultimate Gross O", format(config$immigration$o_immigration$ultimate_gross_o %||% 1350000, big.mark = ",")),
    list("Sex Ratio at Birth", config$projected_population$sex_ratio_at_birth %||% 1048)
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

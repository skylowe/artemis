# Scenario Engine
# =============================================================================
# Executes projection scenarios using the ARTEMIS targets pipeline

#' Run a scenario projection
#'
#' @param config Modified configuration list
#' @param artemis_root Path to ARTEMIS project root
#' @param progress_callback Function(pct, msg) for progress updates
#' @return List with success status and projection data
run_scenario_projection <- function(config, artemis_root, progress_callback = NULL) {

  # Helper to report progress
  report_progress <- function(pct, msg) {
    if (!is.null(progress_callback)) {
      progress_callback(pct, msg)
    }
  }

  report_progress(5, "Validating configuration...")

  # Create temporary config file
  temp_config <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_config), add = TRUE)

  yaml::write_yaml(config, temp_config)

  report_progress(10, "Writing temporary config...")

  # Set environment variable for targets
  old_config <- Sys.getenv("ARTEMIS_CONFIG")
  Sys.setenv(ARTEMIS_CONFIG = temp_config)
  on.exit(Sys.setenv(ARTEMIS_CONFIG = old_config), add = TRUE)

  report_progress(15, "Running projection pipeline...")

  # Change to ARTEMIS root
  old_wd <- getwd()
  setwd(artemis_root)
  on.exit(setwd(old_wd), add = TRUE)

  # Define key targets to run
  targets_to_run <- c(
    "config_assumptions",
    "projected_population",
    "projected_births",
    "projected_deaths",
    "projected_net_immigration",
    "projected_marital_population",
    "population_projection_summary"
  )

  # Run pipeline with progress updates
  result <- tryCatch({
    # Check if targets package is available
    if (!requireNamespace("targets", quietly = TRUE)) {
      stop("targets package not available")
    }

    report_progress(20, "Loading pipeline...")

    # Get the store path
    store <- file.path(artemis_root, "_targets")

    # Run incremental make
    targets::tar_make(
      names = tidyselect::any_of(targets_to_run),
      store = store,
      callr_function = NULL  # Run in current session
    )

    report_progress(80, "Loading results...")

    # Load results
    data <- list()
    for (target in targets_to_run) {
      tryCatch({
        data[[target]] <- targets::tar_read_raw(target, store = store)
      }, error = function(e) {
        # Target not available
      })
    }

    report_progress(95, "Calculating summary statistics...")

    # Get summary stats
    pop_2050 <- if (!is.null(data$projected_population)) {
      sum(data$projected_population[year == 2050]$population, na.rm = TRUE)
    } else NA

    pop_2099 <- if (!is.null(data$projected_population)) {
      sum(data$projected_population[year == 2099]$population, na.rm = TRUE)
    } else NA

    report_progress(100, "Complete")

    list(
      success = TRUE,
      data = data,
      pop_2050 = pop_2050,
      pop_2099 = pop_2099
    )

  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      data = NULL
    )
  })

  result
}

#' Run projection with cached results
#'
#' @param config Configuration list
#' @param cache_dir Directory for cached results
#' @param force_rerun Force rerun even if cached
#' @return List with projection results
run_cached_projection <- function(config, cache_dir = SCENARIOS_DIR,
                                   force_rerun = FALSE) {

  # Compute config hash
  config_hash <- digest::digest(config)
  cache_file <- file.path(cache_dir, paste0("cache_", config_hash, ".rds"))

  # Check cache
  if (!force_rerun && file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    if (!is.null(cached$data)) {
      cli::cli_alert_success("Using cached projection results")
      return(cached)
    }
  }

  # Run projection
  result <- run_scenario_projection(config, ARTEMIS_ROOT)

  # Cache if successful
  if (result$success) {
    saveRDS(result, cache_file)
  }

  result
}

#' Quick sensitivity analysis
#'
#' @param base_config Base configuration
#' @param param_path Path to parameter (e.g., c("fertility", "ultimate_ctfr"))
#' @param values Vector of parameter values to test
#' @return List of results for each value
run_sensitivity <- function(base_config, param_path, values) {
  results <- list()

  for (val in values) {
    # Modify config
    config <- base_config
    config <- set_nested_value(config, param_path, val)

    # Run projection (use cache)
    result <- run_cached_projection(config)

    results[[as.character(val)]] <- list(
      value = val,
      success = result$success,
      pop_2050 = result$pop_2050,
      pop_2099 = result$pop_2099
    )
  }

  results
}

#' Set nested value in list
#' @param x List
#' @param path Character vector path
#' @param value Value to set
#' @return Modified list
set_nested_value <- function(x, path, value) {
  if (length(path) == 1) {
    x[[path]] <- value
  } else {
    if (is.null(x[[path[1]]])) {
      x[[path[1]]] <- list()
    }
    x[[path[1]]] <- set_nested_value(x[[path[1]]], path[-1], value)
  }
  x
}

#' Compare multiple scenarios
#'
#' @param scenarios List of scenario objects
#' @param metrics Character vector of metrics to compare
#' @return data.table with comparison
compare_scenarios <- function(scenarios, metrics = c("population", "births", "deaths")) {

  results <- list()

  for (name in names(scenarios)) {
    scenario <- scenarios[[name]]
    data <- scenario$results

    if (is.null(data)) next

    for (metric in metrics) {
      target_name <- switch(metric,
        "population" = "projected_population",
        "births" = "projected_births",
        "deaths" = "projected_deaths",
        "immigration" = "projected_net_immigration",
        NULL
      )

      if (!is.null(target_name) && !is.null(data[[target_name]])) {
        dt <- data[[target_name]]
        value_col <- intersect(names(dt), c("population", "births", "deaths", "net_immigration"))

        if (length(value_col) > 0) {
          agg <- dt[, .(value = sum(get(value_col[1]), na.rm = TRUE)), by = year]
          agg[, `:=`(scenario = name, metric = metric)]
          results[[paste(name, metric, sep = "_")]] <- agg
        }
      }
    }
  }

  if (length(results) > 0) {
    rbindlist(results, use.names = TRUE, fill = TRUE)
  } else {
    data.table(year = integer(), value = numeric(),
               scenario = character(), metric = character())
  }
}

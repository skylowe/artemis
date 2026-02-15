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

  # Inject runtime section so targets know they're in scenario mode.
  # This enables writable temp cache dirs and forces recomputation of
  # targets that normally use cached results from data/cache/ (read-only
  # in Docker containers).
  scenario_cache_dir <- file.path(tempdir(), "artemis_cache")
  dir.create(scenario_cache_dir, showWarnings = FALSE, recursive = TRUE)
  config$runtime <- list(
    scenario_mode = TRUE,
    cache_dir = scenario_cache_dir
  )

  # Create temporary config file (must happen after runtime injection)
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

  # Targets to build via tar_make(shortcut = TRUE).
  # shortcut = TRUE tells targets to skip upstream dependency checking, so
  # cached data targets are loaded as-is without attempting to re-validate
  # or re-run them. Pure data-loading targets (NCHS, Census, ACS, DHS, TR
  # files) are loaded from the cached baseline store automatically.
  # Computation targets listed here are rebuilt with the user's modified config.
  targets_to_make <- c(
    # Config: re-reads from the temporary YAML with user's modified params
    "config_assumptions",

    # Fertility chain (pure computation — no file I/O)
    "fertility_ultimate_years",
    "fertility_ultimate_age30",
    "fertility_age30_projected",
    "fertility_rates_projected",
    "fertility_rates_complete",

    # Mortality chain
    "nchs_deaths_raw",             # For use_wonder_provisional (reads from existing cache)
    "mortality_mx_projected",      # For starting_aax_method, ultimate_year, elderly_aax_cap, apply_covid
    "mortality_qx_historical",     # For hmd_calibration changes on historical qx
    "mortality_qx_projected",
    "mortality_qx_for_projection",

    # Immigration chain (reads V.A2 from data/raw/)
    "lpr_distribution",            # For distribution_method
    "va2_net_immigration",         # For immigration_scenario
    "lpr_assumptions",             # For lpr_assumptions_source, emigration_ratio
    "lpr_projection_result",       # Uses updated lpr_distribution + lpr_assumptions
    "lpr_immigration_projected",   # Extraction
    "new_arrivals_projected",      # Extraction
    "aos_projected",               # Extraction
    "legal_emigration_projected",  # Uses updated lpr_assumptions
    "net_lpr_immigration",         # Uses updated immigration chain
    "net_o_for_projection",        # Uses updated va2_net_immigration

    # O-immigration (ARTEMIS-computed, for net_o_source = "artemis")
    "historical_temp_unlawful",    # For o_population_method (with runtime cache)
    "o_immigration_projection",    # Pure computation, no cache writes
    "net_o_immigration",           # Extraction

    # Marriage/divorce (full pipeline with writable temp cache)
    "marriage_projection",         # Force recompute with runtime cache dir
    "divorce_projection",          # Force recompute with runtime cache dir
    "marriage_amr_projected",
    "divorce_adr_projected",

    # Starting population (reads from data/raw/ or cached baseline)
    "starting_population",         # For historical_pop_source
    "starting_marital_pop",        # Depends on historical_population_marital (cached)

    # Core population projection (pure computation)
    "population_projection",
    "marital_projection",
    "cni_projection",

    # Final output extractions
    "projected_population",
    "projected_births",
    "projected_deaths",
    "projected_net_immigration",
    "projected_marital_population",
    "projected_cni_population",
    "population_projection_summary"
  )

  # Targets to load results from after tar_make
  targets_to_read <- c(
    "config_assumptions",
    "projected_population",
    "projected_births",
    "projected_deaths",
    "projected_net_immigration",
    "projected_marital_population",
    "projected_cni_population",
    "fertility_rates_complete",
    "mortality_qx_projected",
    "marriage_amr_projected",
    "divorce_adr_projected",
    "marriage_projection",
    "divorce_projection",
    "net_lpr_immigration",
    "population_projection_summary"
  )

  # Run pipeline with progress updates — separate tar_make from data reading
  # so that partial results are returned even when some targets error.

  # Phase 1: Setup and run tar_make
  pipeline_error <- NULL
  store <- NULL

  setup_result <- tryCatch({
    if (!requireNamespace("targets", quietly = TRUE)) {
      stop("targets package not available")
    }

    report_progress(15, "Loading pipeline...")

    # Copy _targets store to a temp directory so scenario runs never corrupt
    # the baseline store. The temp copy is cleaned up on exit.
    main_store <- file.path(artemis_root, "_targets")
    if (!dir.exists(main_store)) {
      stop("Targets store not found at: ", main_store)
    }
    store <<- file.path(tempdir(), paste0("artemis_scenario_", format(Sys.time(), "%Y%m%d%H%M%S")))
    on.exit(unlink(store, recursive = TRUE), add = TRUE)

    report_progress(18, "Copying targets store...")
    copy_ok <- file.copy(main_store, dirname(store), recursive = TRUE)
    if (!copy_ok) {
      stop("Failed to copy _targets store to temp directory")
    }
    file.rename(file.path(dirname(store), basename(main_store)), store)

    # Invalidate ONLY the targets we intend to re-run (targets_to_make).
    # IMPORTANT: Do NOT invalidate targets outside this list — tar_invalidate
    # removes metadata, and shortcut = TRUE needs metadata intact to load
    # upstream dependencies from cache.
    report_progress(25, "Invalidating cached targets...")
    tryCatch({
      targets::tar_invalidate(
        names = tidyselect::any_of(targets_to_make),
        store = store
      )
    }, error = function(e) {
      # Ignore errors if targets don't exist yet
    })

    list(success = TRUE)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })

  if (!setup_result$success) {
    return(list(success = FALSE, error = setup_result$error, data = NULL))
  }

  # Phase 2: Run tar_make — some targets may error (e.g., divorce_projection in
  # scenario mode), but other targets (population_projection) may succeed.
  # tar_make raises a condition when any target errors, but successfully built
  # targets are still readable from the store.
  report_progress(30, "Running projection pipeline...")
  tryCatch({
    targets::tar_make(
      names = tidyselect::any_of(targets_to_make),
      shortcut = TRUE,
      store = store,
      callr_function = NULL  # Run in current session
    )
  }, error = function(e) {
    pipeline_error <<- e$message
    cli::cli_alert_warning("Some targets errored: {e$message}")
  })

  # Phase 3: Load results — read whatever targets succeeded, regardless of
  # whether tar_make raised a condition.
  report_progress(80, "Loading results...")

  data <- list()
  failed_targets <- character()
  for (target in targets_to_read) {
    tryCatch({
      data[[target]] <- targets::tar_read_raw(target, store = store)
    }, error = function(e) {
      failed_targets <<- c(failed_targets, target)
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

  # Return results — success if we got core projection data, even if some
  # secondary targets (divorce, marriage) failed.
  has_core_data <- !is.null(data$projected_population) && !is.null(data$population_projection_summary)

  result <- list(
    success = has_core_data,
    data = data,
    pop_2050 = pop_2050,
    pop_2099 = pop_2099
  )

  if (!is.null(pipeline_error)) {
    result$warning <- pipeline_error
    result$failed_targets <- failed_targets
  }

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

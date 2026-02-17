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
  # Targets check runtime$scenario_mode to bypass internal RDS caching
  # (use_cache = !scenario_mode), since data/cache/ is read-only in Docker.
  # Results are persisted by the {targets} store instead.
  config$runtime <- list(
    scenario_mode = TRUE
  )

  # Create temporary config file (must happen after runtime injection)
  # Use RDS instead of YAML to avoid lossy round-trip (integer/double coercion,
  # NULL handling, etc.) that causes all domain config hashes to differ from
  # baseline, defeating early cutoff on the first scenario run.
  temp_config <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_config), add = TRUE)

  saveRDS(config, temp_config)

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
    # Config: re-reads from the temporary RDS with user's modified params
    "config_file",
    "config_assumptions",

    # Domain config gates (early cutoff skips unchanged domains)
    "config_fertility",
    "config_mortality",
    "config_lpr_immigration",
    "config_o_immigration",
    "config_historical_pop",
    "config_marriage",
    "config_divorce",
    "config_projected_pop",
    "config_metadata",
    "config_data_sources",
    "config_runtime",

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

    # Marriage/divorce (pure computation — no internal caching)
    "marriage_projection",
    "divorce_projection",
    "marriage_amr_projected",
    "divorce_adr_projected",

    # Historical population (recomputed when population_source changes)
    "historical_population",       # For historical_pop_source (census/hybrid/ssa)
    "historical_population_marital", # Depends on historical_population

    # Starting population (reads from data/raw/ or cached historical)
    "starting_population",         # For use_tr_historical_population
    "starting_marital_pop",        # Depends on historical_population_marital

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

  setup_result <- tryCatch({
    if (!requireNamespace("targets", quietly = TRUE)) {
      stop("targets package not available")
    }

    report_progress(15, "Loading pipeline...")

    # Use a persistent scenario store so subsequent runs reuse cached targets.
    # The baseline store is never modified — scenario runs write to a separate
    # persistent copy that survives across runs within a session.
    main_store <- file.path(artemis_root, "_targets")
    if (!dir.exists(main_store)) {
      stop("Targets store not found at: ", main_store)
    }

    # Persistent scenario store in persist volume (survives container restarts)
    persist_dir <- Sys.getenv("ARTEMIS_PERSIST_DIR", "/home/artemis/persist")
    store_path <- file.path(persist_dir, "_targets_scenario")

    # Resolve the baseline store path (main_store may be a symlink)
    baseline_store <- normalizePath(main_store, mustWork = TRUE)

    if (!dir.exists(file.path(store_path, "meta"))) {
      report_progress(18, "First run: copying baseline targets store...")
      if (dir.exists(store_path)) unlink(store_path, recursive = TRUE)
      # Use system cp to avoid R's file.copy symlink issues
      exit_code <- system2("cp", c("-r", baseline_store, store_path))
      if (exit_code != 0) {
        stop("Failed to copy _targets store to scenario directory")
      }
    } else {
      report_progress(18, "Using cached scenario store...")
    }

    # No manual invalidation needed — config_file target has
    # cue = tar_cue(mode = "always") and format = "file", so it
    # automatically detects when the YAML content changes. Domain
    # config gates provide early cutoff for unchanged subprocesses.
    report_progress(25, "Configuration ready...")

    list(success = TRUE, store = store_path)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })

  # Extract store path from result
  store <- setup_result$store

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

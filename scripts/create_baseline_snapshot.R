#' Create Baseline Snapshot for TR2025 Output Verification
#'
#' @description
#' This script captures the current TR2025 pipeline outputs as a baseline
#' for verifying that refactoring changes don't alter results.
#'
#' Run BEFORE making any code changes to establish the baseline.
#'
#' @usage
#' Rscript scripts/create_baseline_snapshot.R
#'
#' Or in R:
#' source("scripts/create_baseline_snapshot.R")

library(targets)
library(data.table)
library(digest)
library(cli)
library(here)

# =============================================================================
# CONFIGURATION
# =============================================================================

BASELINE_DIR <- here::here("data/baseline/tr2025")
HASH_FILE <- file.path(BASELINE_DIR, "output_hashes.rds")

# Key targets to snapshot for verification
# These represent the main outputs of each subprocess
TARGETS_TO_SNAPSHOT <- c(
  # Population projection outputs (Phase 8)
  "population_projection",
  "projected_population",
  "projected_births",
  "projected_deaths",
  "projected_net_immigration",

  # Mortality outputs
  "mortality_qx_projected",
  "mortality_qx_historical",
  "mortality_life_expectancy",

  # Fertility outputs
  "fertility_rates_complete",

  # Marital outputs
  "marital_projection",
  "projected_marital_population",

  # CNI outputs
  "cni_projection",
  "projected_cni_population",

  # Immigration outputs
  "net_lpr_immigration",
  "net_o_for_projection",

  # Marriage/Divorce outputs
  "marriage_projection",
  "divorce_projection"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Calculate hash of an R object
#'
#' @param obj Any R object
#' @return Character: SHA256 hash
calculate_hash <- function(obj) {
  # Serialize and hash
  digest::digest(obj, algo = "sha256")
}

#' Normalize a data.table for consistent hashing
#'
#' @param dt data.table to normalize
#' @return Normalized data.table
normalize_dt <- function(dt) {
  if (!data.table::is.data.table(dt)) {
    return(dt)
  }

  # Make a copy to avoid modifying original
  dt <- data.table::copy(dt)

  # Sort by all columns to ensure consistent ordering
  key_cols <- names(dt)
  data.table::setorderv(dt, key_cols)

  # Reset row names and keys
  data.table::setkey(dt, NULL)

  dt
}

#' Normalize complex objects (lists containing data.tables)
#'
#' @param obj Object to normalize
#' @return Normalized object
normalize_object <- function(obj) {
  if (data.table::is.data.table(obj)) {
    return(normalize_dt(obj))
  }

  if (is.list(obj) && !is.data.frame(obj)) {
    return(lapply(obj, normalize_object))
  }

  obj
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

cli::cli_h1("Creating TR2025 Baseline Snapshot")

# Ensure baseline directory exists
if (!dir.exists(BASELINE_DIR)) {
  dir.create(BASELINE_DIR, recursive = TRUE)
  cli::cli_alert_success("Created baseline directory: {.path {BASELINE_DIR}}")
}

# Check if pipeline has been run
if (!file.exists(here::here("_targets"))) {
  cli::cli_abort(c(
    "No _targets directory found",
    "i" = "Run {.code targets::tar_make()} first to build the pipeline"
  ))
}

# Load each target and save snapshot
cli::cli_h2("Saving Target Snapshots")

snapshots <- list()
hashes <- list()
failed_targets <- character()

for (target_name in TARGETS_TO_SNAPSHOT) {
  cli::cli_progress_step("Processing {.field {target_name}}")

  tryCatch({
    # Load the target
    obj <- targets::tar_read_raw(target_name)

    # Normalize for consistent hashing
    normalized_obj <- normalize_object(obj)

    # Calculate hash
    hash <- calculate_hash(normalized_obj)
    hashes[[target_name]] <- hash

    # Save snapshot
    snapshot_file <- file.path(BASELINE_DIR, paste0(target_name, ".rds"))
    saveRDS(normalized_obj, snapshot_file)

    snapshots[[target_name]] <- list(
      hash = hash,
      class = class(obj),
      size = object.size(obj),
      file = snapshot_file
    )

    cli::cli_alert_success("{target_name}: {substr(hash, 1, 16)}...")

  }, error = function(e) {
    cli::cli_alert_warning("Failed to snapshot {target_name}: {e$message}")
    failed_targets <<- c(failed_targets, target_name)
  })
}

# Save hash index
saveRDS(hashes, HASH_FILE)
cli::cli_alert_success("Saved hash index to {.path {HASH_FILE}}")

# Save metadata
metadata <- list(
  created_at = Sys.time(),
  r_version = R.version.string,
  targets_version = as.character(packageVersion("targets")),
  data.table_version = as.character(packageVersion("data.table")),
  targets_snapshotted = names(snapshots),
  targets_failed = failed_targets,
  snapshots = snapshots
)
saveRDS(metadata, file.path(BASELINE_DIR, "metadata.rds"))

# Summary
cli::cli_h2("Summary")
cli::cli_alert_success("Successfully snapshotted {length(snapshots)} targets")
if (length(failed_targets) > 0) {
  cli::cli_alert_warning("Failed to snapshot {length(failed_targets)} targets: {paste(failed_targets, collapse = ', ')}")
}

cli::cli_h3("Snapshot Files")
for (name in names(snapshots)) {
  file_size <- format(snapshots[[name]]$size, units = "auto")
  cli::cli_bullets(c("*" = "{name}: {file_size}"))
}

cli::cli_alert_info("Baseline created at: {.path {BASELINE_DIR}}")
cli::cli_alert_info("Run {.code source('scripts/verify_baseline.R')} after refactoring to verify outputs match")

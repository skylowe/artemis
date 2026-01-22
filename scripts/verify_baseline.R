#' Verify Pipeline Output Against TR2025 Baseline
#'
#' @description
#' This script compares current pipeline outputs against the baseline snapshot
#' created before refactoring. Use this to verify that changes don't alter results.
#'
#' Run AFTER making code changes to verify outputs match.
#'
#' @usage
#' Rscript scripts/verify_baseline.R
#'
#' Or in R:
#' source("scripts/verify_baseline.R")

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

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Calculate hash of an R object
#'
#' @param obj Any R object
#' @return Character: SHA256 hash
calculate_hash <- function(obj) {
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

  dt <- data.table::copy(dt)
  key_cols <- names(dt)
  data.table::setorderv(dt, key_cols)
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

#' Compare two objects and report differences
#'
#' @param baseline Baseline object
#' @param current Current object
#' @param name Target name for reporting
#' @return List with comparison results
compare_objects <- function(baseline, current, name) {
  result <- list(
    name = name,
    match = FALSE,
    baseline_hash = calculate_hash(normalize_object(baseline)),
    current_hash = calculate_hash(normalize_object(current)),
    details = NULL
  )

  result$match <- result$baseline_hash == result$current_hash

  if (!result$match) {
    # Try to provide more details about differences
    if (data.table::is.data.table(baseline) && data.table::is.data.table(current)) {
      result$details <- compare_data_tables(baseline, current)
    } else if (is.list(baseline) && is.list(current)) {
      result$details <- compare_lists(baseline, current)
    }
  }

  result
}

#' Compare two data.tables and report differences
#'
#' @param dt1 First data.table
#' @param dt2 Second data.table
#' @return Character description of differences
compare_data_tables <- function(dt1, dt2) {
  diffs <- character()

  # Check dimensions
  if (nrow(dt1) != nrow(dt2)) {
    diffs <- c(diffs, sprintf("Row count: baseline=%d, current=%d", nrow(dt1), nrow(dt2)))
  }

  if (ncol(dt1) != ncol(dt2)) {
    diffs <- c(diffs, sprintf("Column count: baseline=%d, current=%d", ncol(dt1), ncol(dt2)))
  }

  # Check column names
  if (!identical(names(dt1), names(dt2))) {
    missing_in_current <- setdiff(names(dt1), names(dt2))
    extra_in_current <- setdiff(names(dt2), names(dt1))
    if (length(missing_in_current) > 0) {
      diffs <- c(diffs, sprintf("Missing columns: %s", paste(missing_in_current, collapse = ", ")))
    }
    if (length(extra_in_current) > 0) {
      diffs <- c(diffs, sprintf("Extra columns: %s", paste(extra_in_current, collapse = ", ")))
    }
  }

  # Check numeric columns for max differences
  common_cols <- intersect(names(dt1), names(dt2))
  numeric_cols <- common_cols[sapply(common_cols, function(col) is.numeric(dt1[[col]]))]

  for (col in numeric_cols) {
    if (length(dt1[[col]]) == length(dt2[[col]])) {
      diff <- abs(dt1[[col]] - dt2[[col]])
      max_diff <- max(diff, na.rm = TRUE)
      if (max_diff > 1e-10) {
        mean_diff <- mean(diff, na.rm = TRUE)
        diffs <- c(diffs, sprintf("Column '%s': max_diff=%.6e, mean_diff=%.6e", col, max_diff, mean_diff))
      }
    }
  }

  if (length(diffs) == 0) {
    diffs <- "Unknown difference (structure appears same but hash differs)"
  }

  paste(diffs, collapse = "\n")
}

#' Compare two lists and report differences
#'
#' @param list1 First list
#' @param list2 Second list
#' @return Character description of differences
compare_lists <- function(list1, list2) {
  diffs <- character()

  # Check element names
  if (!identical(names(list1), names(list2))) {
    missing <- setdiff(names(list1), names(list2))
    extra <- setdiff(names(list2), names(list1))
    if (length(missing) > 0) diffs <- c(diffs, sprintf("Missing elements: %s", paste(missing, collapse = ", ")))
    if (length(extra) > 0) diffs <- c(diffs, sprintf("Extra elements: %s", paste(extra, collapse = ", ")))
  }

  # Check each element
  common_elements <- intersect(names(list1), names(list2))
  for (elem in common_elements) {
    hash1 <- calculate_hash(normalize_object(list1[[elem]]))
    hash2 <- calculate_hash(normalize_object(list2[[elem]]))
    if (hash1 != hash2) {
      diffs <- c(diffs, sprintf("Element '%s' differs", elem))
    }
  }

  if (length(diffs) == 0) {
    diffs <- "Unknown difference in list structure"
  }

  paste(diffs, collapse = "\n")
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

cli::cli_h1("Verifying Pipeline Output Against TR2025 Baseline")

# Check baseline exists
if (!file.exists(HASH_FILE)) {
  cli::cli_abort(c(
    "Baseline hash file not found: {.path {HASH_FILE}}",
    "i" = "Run {.code source('scripts/create_baseline_snapshot.R')} first to create baseline"
  ))
}

# Load baseline hashes
baseline_hashes <- readRDS(HASH_FILE)
cli::cli_alert_info("Loaded baseline with {length(baseline_hashes)} targets")

# Load metadata
metadata_file <- file.path(BASELINE_DIR, "metadata.rds")
if (file.exists(metadata_file)) {
  metadata <- readRDS(metadata_file)
  cli::cli_alert_info("Baseline created: {metadata$created_at}")
}

# Verify each target
cli::cli_h2("Comparing Targets")

results <- list()
matches <- 0
mismatches <- 0
errors <- 0

for (target_name in names(baseline_hashes)) {
  cli::cli_progress_step("Verifying {.field {target_name}}")

  tryCatch({
    # Load current target
    current_obj <- targets::tar_read_raw(target_name)
    current_normalized <- normalize_object(current_obj)
    current_hash <- calculate_hash(current_normalized)

    # Load baseline
    baseline_file <- file.path(BASELINE_DIR, paste0(target_name, ".rds"))
    if (file.exists(baseline_file)) {
      baseline_obj <- readRDS(baseline_file)
      comparison <- compare_objects(baseline_obj, current_normalized, target_name)
    } else {
      # Fall back to hash comparison only
      comparison <- list(
        name = target_name,
        match = current_hash == baseline_hashes[[target_name]],
        baseline_hash = baseline_hashes[[target_name]],
        current_hash = current_hash,
        details = if (current_hash != baseline_hashes[[target_name]]) "Baseline file not found for detailed comparison" else NULL
      )
    }

    results[[target_name]] <- comparison

    if (comparison$match) {
      matches <- matches + 1
      cli::cli_alert_success("{target_name}: MATCH")
    } else {
      mismatches <- mismatches + 1
      cli::cli_alert_danger("{target_name}: MISMATCH")
      if (!is.null(comparison$details)) {
        cli::cli_bullets(c("!" = comparison$details))
      }
    }

  }, error = function(e) {
    errors <<- errors + 1
    results[[target_name]] <<- list(
      name = target_name,
      match = FALSE,
      error = e$message
    )
    cli::cli_alert_warning("{target_name}: ERROR - {e$message}")
  })
}

# Summary
cli::cli_h2("Verification Summary")

total <- matches + mismatches + errors
cli::cli_bullets(c(
  "*" = "Total targets: {total}",
  "v" = "Matches: {matches}",
  "x" = "Mismatches: {mismatches}",
  "!" = "Errors: {errors}"
))

if (mismatches == 0 && errors == 0) {
  cli::cli_alert_success("All targets match baseline - refactoring is safe!")
  quit(status = 0)
} else {
  cli::cli_alert_danger("Some targets do not match baseline!")

  # List mismatches
  if (mismatches > 0) {
    cli::cli_h3("Mismatched Targets")
    for (r in results) {
      if (!is.null(r$match) && !r$match && is.null(r$error)) {
        cli::cli_bullets(c("x" = "{r$name}"))
        if (!is.null(r$details)) {
          cat("  Details:", r$details, "\n")
        }
      }
    }
  }

  # List errors
  if (errors > 0) {
    cli::cli_h3("Targets with Errors")
    for (r in results) {
      if (!is.null(r$error)) {
        cli::cli_bullets(c("!" = "{r$name}: {r$error}"))
      }
    }
  }

  quit(status = 1)
}

#!/usr/bin/env Rscript
#' Verify ARTEMIS Setup
#'
#' Comprehensive check that the project is ready to run
#'
#' Usage: Rscript scripts/verify_setup.R

cat("\n")
cat("========================================\n")
cat("ARTEMIS Setup Verification\n")
cat("========================================\n\n")

# Track overall status
all_checks_passed <- TRUE

# Check 1: API Keys
cat("1. Checking API Keys...\n")
expected_keys <- c("CENSUS_KEY", "BEA_API_KEY", "BLS_API_KEY", "FRED_API_KEY")
missing_keys <- character(0)
for (key in expected_keys) {
  value <- Sys.getenv(key)
  if (value == "") {
    missing_keys <- c(missing_keys, key)
    cat("   ✗", key, "- NOT SET\n")
    all_checks_passed <- FALSE
  } else {
    cat("   ✓", key, "\n")
  }
}
if (length(missing_keys) == 0) {
  cat("   ✓ All API keys configured\n")
}
cat("\n")

# Check 2: Data Files
cat("2. Checking Data Files...\n")
data_dirs <- c(
  "data/raw/SSA_TR2025",
  "data/cache",
  "data/baseline",
  "data/outputs"
)
for (dir in data_dirs) {
  if (dir.exists(dir)) {
    file_count <- length(list.files(dir, recursive = TRUE))
    cat("   ✓", dir, sprintf("(%d files)\n", file_count))
  } else {
    cat("   ✗", dir, "- MISSING\n")
    all_checks_passed <- FALSE
  }
}
cat("\n")

# Check 3: Key R Packages
cat("3. Checking Key R Packages...\n")
key_packages <- c("targets", "renv", "data.table", "dplyr", "ggplot2",
                  "shiny", "yaml", "httr2", "here", "checkmate")
for (pkg in key_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("   ✓", pkg, "\n")
  } else {
    cat("   ✗", pkg, "- NOT INSTALLED\n")
    all_checks_passed <- FALSE
  }
}
cat("\n")

# Check 4: Project Structure
cat("4. Checking Project Structure...\n")
required_dirs <- c("R", "config", "data", "app", "scripts", "plans")
for (dir in required_dirs) {
  if (dir.exists(dir)) {
    cat("   ✓", dir, "/\n")
  } else {
    cat("   ✗", dir, "/ - MISSING\n")
    all_checks_passed <- FALSE
  }
}
cat("\n")

# Check 5: Configuration Files
cat("5. Checking Configuration Files...\n")
config_files <- c(
  "config/assumptions/tr2025.yaml",
  "_targets.R",
  "renv.lock",
  ".Rprofile"
)
for (file in config_files) {
  if (file.exists(file)) {
    cat("   ✓", file, "\n")
  } else {
    cat("   ✗", file, "- MISSING\n")
    all_checks_passed <- FALSE
  }
}
cat("\n")

# Check 6: Load Configuration
cat("6. Testing Configuration Loading...\n")
tryCatch({
  config <- yaml::read_yaml("config/assumptions/tr2025.yaml")
  tr_year <- config$metadata$trustees_report_year
  cat("   ✓ Configuration loaded successfully\n")
  cat("   ✓ Trustees Report Year:", tr_year, "\n")
}, error = function(e) {
  cat("   ✗ Failed to load configuration:", conditionMessage(e), "\n")
  all_checks_passed <<- FALSE
})
cat("\n")

# Check 7: Test Loading ARTEMIS Functions
cat("7. Testing ARTEMIS Function Loading...\n")
tryCatch({
  # Source a utility file to test
  source("R/utils/api_helpers.R")
  cat("   ✓ Successfully loaded R/utils/api_helpers.R\n")

  # Test that functions are available
  if (exists("get_api_key")) {
    cat("   ✓ get_api_key() function available\n")
  }
}, error = function(e) {
  cat("   ✗ Failed to load ARTEMIS functions:", conditionMessage(e), "\n")
  all_checks_passed <<- FALSE
})
cat("\n")

# Final Summary
cat("========================================\n")
if (all_checks_passed) {
  cat("✓ ALL CHECKS PASSED\n")
  cat("========================================\n\n")
  cat("The ARTEMIS project is ready to run!\n\n")
  cat("Next steps:\n")
  cat("  - Run the pipeline: Rscript -e \"targets::tar_make()\"\n")
  cat("  - Launch the app: Rscript run_app.R\n")
  cat("\n")
  quit(status = 0)
} else {
  cat("✗ SOME CHECKS FAILED\n")
  cat("========================================\n\n")
  cat("Please address the issues above before running the project.\n\n")
  quit(status = 1)
}

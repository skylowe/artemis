#!/usr/bin/env Rscript
#' Test API Key Configuration
#'
#' Verifies that API keys are properly loaded from ~/.Renviron
#'
#' Usage: Rscript scripts/test_api_keys.R

# Expected API keys based on CLAUDE.md documentation
expected_keys <- c(
  "CENSUS_KEY",
  "BEA_API_KEY",
  "BLS_API_KEY",
  "FRED_API_KEY",
  "EIA_API_KEY",
  "NASSQS_TOKEN",
  "SP_USER",
  "SP_PASS",
  "MOODYS_ACCESS"
)

cat("Testing API Key Configuration\n")
cat("================================\n\n")

# Check if home .Renviron exists
home_renviron <- path.expand("~/.Renviron")
if (!file.exists(home_renviron)) {
  cat("ERROR: ~/.Renviron file not found\n")
  quit(status = 1)
}

cat("✓ Found ~/.Renviron file\n\n")

# Test each key
missing_keys <- character(0)
found_keys <- character(0)

for (key_name in expected_keys) {
  value <- Sys.getenv(key_name, unset = NA)

  if (is.na(value) || value == "") {
    missing_keys <- c(missing_keys, key_name)
    cat("✗", key_name, "- NOT SET\n")
  } else {
    found_keys <- c(found_keys, key_name)
    # Show first 8 chars only for security
    preview <- paste0(substr(value, 1, 8), "...")
    cat("✓", key_name, "=", preview, "\n")
  }
}

cat("\n================================\n")
cat("Summary:\n")
cat("  Found:", length(found_keys), "keys\n")
cat("  Missing:", length(missing_keys), "keys\n")

if (length(missing_keys) > 0) {
  cat("\nMissing keys:\n")
  cat(paste0("  - ", missing_keys, "\n"))
  cat("\nNote: Some keys may be optional depending on data sources used.\n")
}

if (length(found_keys) == length(expected_keys)) {
  cat("\n✓ All API keys are configured!\n")
  quit(status = 0)
} else if (length(found_keys) > 0) {
  cat("\n⚠ Some API keys are configured, but some are missing.\n")
  quit(status = 0)
} else {
  cat("\n✗ No API keys found. Check ~/.Renviron configuration.\n")
  quit(status = 1)
}

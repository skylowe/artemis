# ARTEMIS Visualization Tool - Dependency Installation
# =============================================================================
# Run this script to install required packages for the visualization tool.
#
# Usage: Rscript app/install_dependencies.R
#
# =============================================================================

# Packages required for the visualization app
required_packages <- c(
  # Core Shiny
  "shiny",
  "bslib",
  "shinyWidgets",
  "shinyjs",

  # Visualization
  "ggplot2",
  "plotly",
  "scales",
  "viridis",
  "patchwork",

  # Tables
  "DT",
  "rhandsontable",

  # Data manipulation
  "data.table",
  "dplyr",
  "tidyr",

  # Utilities
  "yaml",
  "digest",
  "here",
  "cli",
  "waiter",
  "R6"
)

# Check which packages are missing
installed <- installed.packages()[, "Package"]
missing <- setdiff(required_packages, installed)

if (length(missing) == 0) {
  message("All required packages are already installed.")
} else {
  message("Installing ", length(missing), " missing packages:")
  message("  ", paste(missing, collapse = ", "))
  message("")

  # Install missing packages
  install.packages(missing, repos = "https://cloud.r-project.org")

  # Verify installation
  still_missing <- setdiff(missing, installed.packages()[, "Package"])

  if (length(still_missing) == 0) {
    message("\nAll packages installed successfully!")
  } else {
    warning("\nFailed to install: ", paste(still_missing, collapse = ", "))
    message("Try installing manually with: install.packages(c('",
            paste(still_missing, collapse = "', '"), "'))")
  }
}

# If using renv, snapshot the packages
if (file.exists(here::here("renv.lock"))) {
  message("\nUpdating renv.lock...")
  tryCatch({
    renv::snapshot(prompt = FALSE)
    message("renv.lock updated successfully!")
  }, error = function(e) {
    message("Note: Could not update renv.lock automatically.")
    message("Run 'renv::snapshot()' manually if needed.")
  })
}

message("\nDone! You can now run the app with: shiny::runApp('app')")

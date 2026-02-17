# ARTEMIS Visualization Tool - Global Configuration
# =============================================================================
# Loads packages, baseline data, and ARTEMIS integration

# Check for required packages
required_packages <- c(
  "shiny", "bslib", "shinyWidgets", "shinyjs",
  "ggplot2", "plotly", "scales", "viridis", "patchwork",
  "DT", "rhandsontable",
  "data.table", "dplyr", "tidyr",
  "yaml", "digest", "here", "cli", "waiter", "R6"
)

missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  message("Missing packages: ", paste(missing_packages, collapse = ", "))
  message("Run 'Rscript app/install_dependencies.R' to install them.")
  message("Or install manually with: install.packages(c('",
          paste(missing_packages, collapse = "', '"), "'))")
}

# Package loading with graceful fallback
suppressPackageStartupMessages({
  # Core Shiny
  library(shiny)
  library(bslib)

  # Optional packages - load if available
  if (requireNamespace("shinyWidgets", quietly = TRUE)) library(shinyWidgets)
  if (requireNamespace("shinyjs", quietly = TRUE)) library(shinyjs)

  # Visualization
  library(ggplot2)
  if (requireNamespace("plotly", quietly = TRUE)) library(plotly)
  library(scales)
  if (requireNamespace("viridis", quietly = TRUE)) library(viridis)
  if (requireNamespace("patchwork", quietly = TRUE)) library(patchwork)

  # Tables
  library(DT)
  if (requireNamespace("rhandsontable", quietly = TRUE)) library(rhandsontable)

  # Data manipulation
  library(data.table)
  library(dplyr)
  library(tidyr)

  # Utilities
  library(yaml)
  if (requireNamespace("digest", quietly = TRUE)) library(digest)
  library(here)
  library(cli)
  if (requireNamespace("waiter", quietly = TRUE)) library(waiter)
})

# =============================================================================
# ARTEMIS Integration
# =============================================================================

# Set project root (parent of app/)
ARTEMIS_ROOT <- normalizePath(file.path(dirname(getwd()), "."), winslash = "/")
if (!file.exists(file.path(ARTEMIS_ROOT, "_targets.R"))) {
  # Try alternative paths for different launch contexts
  ARTEMIS_ROOT <- normalizePath(here::here(), winslash = "/")
}

# Default config path
DEFAULT_CONFIG_PATH <- file.path(ARTEMIS_ROOT, "config/assumptions/tr2025.yaml")

# Source ARTEMIS utilities (needed for config loading)
source_artemis_files <- function() {
  utils_dir <- file.path(ARTEMIS_ROOT, "R/utils")
  if (dir.exists(utils_dir)) {
    utils_files <- list.files(utils_dir, pattern = "\\.R$", full.names = TRUE)
    for (f in utils_files) {
      tryCatch(source(f), error = function(e) {
        message("Warning: Could not source ", basename(f), ": ", e$message)
      })
    }
  }
}

# Try to source ARTEMIS utilities
tryCatch(source_artemis_files(), error = function(e) {
  message("Note: ARTEMIS utilities not loaded: ", e$message)
})

# =============================================================================
# Load Baseline Data
# =============================================================================

#' Load baseline TR2025 results from targets store
#' @return List with baseline projection results
load_baseline_data <- function() {
  snapshot_file <- file.path(BASELINE_DIR, "baseline_data.rds")

  # If a baseline snapshot exists, load from it (immune to _targets/ overwrites)
  if (file.exists(snapshot_file)) {
    cli::cli_alert_info("Loading baseline from saved snapshot")
    baseline <- readRDS(snapshot_file)
    cli::cli_alert_success("Loaded {length(baseline)} baseline targets from snapshot")
    return(baseline)
  }

  # First load: read from _targets/ store
  targets_store <- file.path(ARTEMIS_ROOT, "_targets")

  baseline <- list()

  # Check if targets store exists
  if (!dir.exists(targets_store)) {
    cli::cli_alert_warning("Targets store not found at {.path {targets_store}}")
    cli::cli_alert_info("Run {.code targets::tar_make()} to generate baseline data")
    return(baseline)
  }

  # Load available targets
  target_names <- c(
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
    "population_projection_summary"
  )

  for (target in target_names) {
    tryCatch({
      baseline[[target]] <- targets::tar_read_raw(target, store = targets_store)
    }, error = function(e) {
      # Target not available
    })
  }

  if (length(baseline) > 0) {
    cli::cli_alert_success("Loaded {length(baseline)} baseline targets")
    # Save snapshot so future loads are immune to _targets/ overwrites
    dir.create(dirname(snapshot_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(baseline, snapshot_file)
    cli::cli_alert_info("Saved baseline snapshot to {.path {snapshot_file}}")
  } else {
    cli::cli_alert_warning("No baseline data loaded - run pipeline first")
  }

  baseline
}

#' Load TR2025 assumptions config
#' @param config_path Path to YAML config file
#' @return List with configuration
load_config <- function(config_path = DEFAULT_CONFIG_PATH) {
  if (!file.exists(config_path)) {
    cli::cli_abort("Config file not found: {.path {config_path}}")
  }
  yaml::read_yaml(config_path)
}

# =============================================================================
# Application Theme
# =============================================================================

#' Create ARTEMIS theme
artemis_theme <- function() {
  bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#95A5A6",
    success = "#18BC9C",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("Fira Mono")
  )
}

# =============================================================================
# Data Directories
# =============================================================================

APP_DATA_DIR <- file.path(dirname(sys.frame(1)$ofile %||% getwd()), "data")
BASELINE_DIR <- file.path(APP_DATA_DIR, "baseline")
SCENARIOS_DIR <- file.path(APP_DATA_DIR, "scenarios")

# Create directories if they don't exist
dir.create(BASELINE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SCENARIOS_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# Constants
# =============================================================================

# Projection years
MIN_YEAR <- 2022
MAX_YEAR <- 2099

# Age limits
MIN_AGE <- 0
MAX_AGE <- 100

# Color palettes
SEX_COLORS <- c("male" = "#3498DB", "female" = "#E74C3C")
MARITAL_COLORS <- c(
  "single" = "#9B59B6",
  "married" = "#27AE60",
  "divorced" = "#E67E22",
  "widowed" = "#7F8C8D"
)
SCENARIO_COLORS <- if (requireNamespace("viridis", quietly = TRUE)) {
  viridis::viridis(8)
} else {
  c("#440154", "#46337E", "#365C8D", "#277F8E", "#1FA187", "#4AC16D", "#9FDA3A", "#FDE725")
}

# =============================================================================
# Global Options
# =============================================================================

# ggplot2 theme for all plots
theme_set(theme_minimal(base_size = 12) +
            theme(
              plot.title = element_text(face = "bold"),
              legend.position = "bottom",
              panel.grid.minor = element_blank()
            ))

# plotly options
options(shiny.usecairo = TRUE)

# =============================================================================
# Source App Modules and Utilities
# =============================================================================

# Get app directory - try multiple approaches
get_app_dir <- function() {
  # Approach 1: Check if we're in the app directory
  if (dir.exists("modules") && dir.exists("R")) {
    return(".")
  }

  # Approach 2: Check if app/ subdirectory exists
  if (dir.exists("app") && dir.exists("app/modules")) {
    return("app")
  }

  # Approach 3: Use here package
  app_path <- here::here("app")
  if (dir.exists(app_path) && dir.exists(file.path(app_path, "modules"))) {
    return(app_path)
  }

  # Approach 4: Check relative to ARTEMIS_ROOT
  if (exists("ARTEMIS_ROOT")) {
    app_path <- file.path(ARTEMIS_ROOT, "app")
    if (dir.exists(app_path)) {
      return(app_path)
    }
  }

  # Fallback
  "app"
}

APP_DIR <- get_app_dir()

# Source R utilities
r_dir <- file.path(APP_DIR, "R")
if (dir.exists(r_dir)) {
  r_files <- list.files(path = r_dir, pattern = "\\.R$", full.names = TRUE)
  for (f in r_files) {
    tryCatch(source(f, local = FALSE), error = function(e) {
      message("Warning: Could not source ", basename(f), ": ", e$message)
    })
  }
}

# Register www/ directory for static file serving
www_dir <- file.path(APP_DIR, "www")
if (dir.exists(www_dir)) {
  addResourcePath("www", www_dir)
}

# Source modules
modules_dir <- file.path(APP_DIR, "modules")
if (dir.exists(modules_dir)) {
  module_files <- list.files(path = modules_dir, pattern = "^mod_.*\\.R$", full.names = TRUE)
  for (f in module_files) {
    tryCatch(source(f, local = FALSE), error = function(e) {
      message("Warning: Could not source ", basename(f), ": ", e$message)
    })
  }
}

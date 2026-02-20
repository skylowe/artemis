# ARTEMIS: OASDI Projection Model
# Targets pipeline definition
# =============================================================================
#
# This file defines the targets pipeline for the ARTEMIS OASDI projection model.
# Target definitions have been modularized into factory functions in R/targets/.
#
# Factory functions:
#   - create_config_targets()               Configuration loading
#   - create_fertility_targets()            Fertility subprocess (Section 1.1)
#   - create_mortality_targets()            Mortality subprocess (Section 1.2)
#   - create_immigration_targets()          Immigration subprocesses (Sections 1.3, 1.5)
#   - create_historical_population_targets() Historical population (Section 1.4)
#   - create_marriage_divorce_targets()     Marriage/Divorce (Sections 1.6, 1.7)
#   - create_projected_population_targets() Projected population (Section 1.8)
#   - create_economics_targets()            US Employment subprocess (Section 2.1)
#   - create_validation_targets()           Cross-cutting validation
#
# =============================================================================

library(targets)
library(tarchetypes)
library(crew)

# Isolated environment for pipeline functions.
# Ensures import graph is identical whether tar_make runs via callr::r
# (host, clean globalenv) or callr_function=NULL (Docker, globalenv
# contains Shiny app objects). Only R/ functions appear as imports.
pipeline_env <- new.env(parent = globalenv())
tar_option_set(envir = pipeline_env)

# Source all R files in R/ subdirectories
tar_source()
tar_source("R/utils")
tar_source("R/data_acquisition")
tar_source("R/demography")
tar_source("R/economics")
tar_source("R/validation")
tar_source("R/targets")

# Set global options for targets
tar_option_set(
  packages = c(
    "data.table",
    "dplyr",
    "tidyr",
    "httr2",
    "jsonlite",
    "yaml",
    "readxl",
    "readr",
    "haven",
    "here",
    "glue",
    "cli",
    "checkmate"
  ),
  format = "rds",
  error = "continue",
  memory = "transient",
  garbage_collection = TRUE,
  # Parallel execution via crew: independent targets run concurrently.
  # The pipeline has 15 sequential levels with up to 27 targets per level.
  # 8 workers balances parallelism against memory (~2GB per worker).
  controller = crew_controller_local(
    workers = 8,
    seconds_idle = 10
  )
)

# =============================================================================
# PIPELINE DEFINITION
# =============================================================================

evalq(c(
  # Configuration
  create_config_targets(),

  # Demography subprocesses
  create_fertility_targets(),
  create_mortality_targets(),
  create_immigration_targets(),
  create_historical_population_targets(),
  create_marriage_divorce_targets(),

  # Population projection (Phase 8)
  create_projected_population_targets(),

  # Economics subprocesses
  create_economics_targets(),

  # Cross-cutting validation
  create_validation_targets()
), envir = pipeline_env)

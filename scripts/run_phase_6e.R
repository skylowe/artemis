#!/usr/bin/env Rscript
# Run Phase 6E: AMR Projection (2023-2099)

library(data.table)
library(here)

source(here::here("R/demography/marriage.R"))
source(here::here("R/data_acquisition/nchs_marriage.R"))
source(here::here("R/data_acquisition/acs_marriage.R"))

cat("=== Phase 6E: AMR Projection (2023-2099) ===\n\n")

# Step 1: Load historical data from Phase 6D
cat("Step 1: Loading historical rates from Phase 6D...\n")
cache_file <- here::here("data/cache/marriage/historical_rates_1989_2022.rds")

if (!file.exists(cache_file)) {
  cat("  ERROR: Historical rates not found. Run Phase 6D first.\n")
  stop("Run scripts/run_phase_6d.R first")
}

historical_data <- readRDS(cache_file)
historical_amr <- historical_data$amr
cat("  Loaded", nrow(historical_amr), "years of historical AMR\n")
cat("  AMR range:", round(min(historical_amr$amr)), "-", round(max(historical_amr$amr)), "\n")

# Step 2: Build standard population grid
cat("\nStep 2: Building 2010 standard population grid...\n")
std_pop_file <- here::here("data/cache/acs_pums/marital_all_years.rds")
std_pop_by_group <- get_2010_standard_population(cache_file = std_pop_file, by_age_group = TRUE)
std_pop_by_group[, year := 2010]

unmarried_pop_grid <- build_standard_population_grid(
  unmarried_pop = std_pop_by_group,
  std_year = 2010,
  min_age = MARGRID_MIN_AGE,
  max_age = MARGRID_MAX_AGE
)
cat("  Standard pop grid:", nrow(unmarried_pop_grid), "x", ncol(unmarried_pop_grid), "\n")

# Step 3: Get base MarGrid from the most recent historical year
cat("\nStep 3: Getting base MarGrid from 2022...\n")
base_margrid <- historical_data$rates[["2022"]]
if (is.null(base_margrid)) {
  # Fallback to 2019 if 2022 not available
  base_margrid <- historical_data$rates[["2019"]]
  cat("  Using 2019 MarGrid (2022 not available)\n")
} else {
  cat("  Using 2022 MarGrid\n")
}
cat("  Base MarGrid:", nrow(base_margrid), "x", ncol(base_margrid), "\n")

# Step 4: Test starting AMR calculation
cat("\n=== Testing Starting AMR Calculation ===\n")
starting_amr <- calculate_starting_amr(historical_amr, n_years = 5)
cat("  Starting AMR:", round(starting_amr, 1), "\n")

# Show the years used
recent_years <- historical_amr[order(-year)][1:5]
cat("  Years used:\n")
print(recent_years[, .(year, amr = round(amr, 1))])

# Step 5: Test AMR projection
cat("\n=== Testing AMR Projection ===\n")
amr_projection <- project_amr(
  starting_amr = starting_amr,
  ultimate_amr = 4000,
  start_year = 2023,
  ultimate_year = 2047,
  end_year = 2099
)

cat("\nProjected AMR at key years:\n")
key_years <- c(2023, 2025, 2030, 2035, 2040, 2045, 2047, 2050, 2060, 2070, 2080, 2090, 2099)
print(amr_projection[year %in% key_years, .(
  year,
  projected_amr = round(projected_amr, 1),
  annual_change = round(amr_change, 1)
)])

# Step 6: Verify rate of change decreases
cat("\n=== Verifying Rate of Change Decreases ===\n")
amr_projection[, abs_change := abs(amr_change)]
early_change <- amr_projection[year %in% 2024:2030, mean(abs_change, na.rm = TRUE)]
mid_change <- amr_projection[year %in% 2035:2040, mean(abs_change, na.rm = TRUE)]
late_change <- amr_projection[year %in% 2043:2047, mean(abs_change, na.rm = TRUE)]

cat("  Early period (2024-2030) mean change:", round(early_change, 2), "\n")
cat("  Mid period (2035-2040) mean change:", round(mid_change, 2), "\n")
cat("  Late period (2043-2047) mean change:", round(late_change, 2), "\n")

if (early_change > mid_change && mid_change > late_change) {
  cat("  ✓ PASS: Rate of change decreases as ultimate year approaches\n")
} else {
  cat("  ✗ FAIL: Rate of change does not decrease properly\n")
}

# Step 7: Run full projection
cat("\n=== Running Full Projection ===\n")
projected <- project_marriage_rates(
  base_margrid = base_margrid,
  historical_amr = historical_amr,
  standard_pop_grid = unmarried_pop_grid,
  ultimate_amr = 4000,
  start_year = 2023,
  ultimate_year = 2047,
  end_year = 2099,
  force_recompute = TRUE
)

cat("\n=== RESULTS ===\n")
cat("\nProjected AMR at key years:\n")
print(projected$amr[year %in% key_years, .(
  year,
  projected_amr = round(projected_amr, 1)
)])

# Verify AMR from scaled grids
cat("\n=== Verifying Scaled Grid AMR ===\n")
check_years <- c(2023, 2047, 2099)
for (yr in check_years) {
  grid <- projected$rates[[as.character(yr)]]
  if (!is.null(grid)) {
    actual_amr <- calculate_amr_from_matrix(grid, unmarried_pop_grid)
    target_amr <- projected$amr[year == yr, projected_amr]
    cat("  ", yr, ": Target =", round(target_amr, 1),
        ", Actual =", round(actual_amr, 1),
        ", Diff =", round(actual_amr - target_amr, 2), "\n")
  }
}

cat("\n=== Phase 6E Complete ===\n")
cat("Starting AMR:", round(projected$starting_amr, 1), "\n")
cat("Ultimate AMR:", projected$ultimate_amr, "(by", projected$ultimate_year, ")\n")
cat("Total projection years:", length(projected$rates), "\n")

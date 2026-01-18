#!/usr/bin/env Rscript
# Test Phase 6D: Historical Period (1989-2022)

library(data.table)
library(here)

# Source required files
source(here::here("R/demography/marriage.R"))
source(here::here("R/data_acquisition/nchs_marriage.R"))
source(here::here("R/data_acquisition/acs_marriage.R"))

cat("=== Phase 6D Test: Historical Period (1989-2022) ===\n\n")

# =========================================================================
# STEP 1: Load base MarGrid from 1978-1988
# =========================================================================
cat("Step 1: Building base MarGrid from 1978-1988...\n")

# Load NCHS 1978-1988 data
nchs_1978_1988 <- fetch_nchs_mra_marriages_1978_1988()
cat("  NCHS 1978-1988: ", nrow(nchs_1978_1988), " rows\n", sep = "")

# Load CPS unmarried population
cps_file <- here::here("data/cache/ipums_cps/cps_unmarried_1957_1995.rds")
if (file.exists(cps_file)) {
  cps_unmarried <- readRDS(cps_file)
  cat("  CPS unmarried: ", nrow(cps_unmarried), " rows\n", sep = "")
} else {
  stop("CPS unmarried population file not found: ", cps_file)
}

# Build base MarGrid
margrid_result <- build_base_margrid(
  nchs_marriages = nchs_1978_1988,
  cps_unmarried = cps_unmarried,
  years = 1978:1988,
  smooth = TRUE
)

base_margrid <- margrid_result$margrid
cat("  Base MarGrid: ", nrow(base_margrid), " x ", ncol(base_margrid), "\n", sep = "")
cat("  Peak rate: ", round(max(base_margrid, na.rm = TRUE), 1),
    " at age pair (", which(base_margrid == max(base_margrid), arr.ind = TRUE)[1] + 13, ", ",
    which(base_margrid == max(base_margrid), arr.ind = TRUE)[2] + 13, ")\n", sep = "")

# =========================================================================
# STEP 2: Load standard population grid (2010)
# =========================================================================
cat("\nStep 2: Building 2010 standard population grid...\n")

std_pop_file <- here::here("data/cache/acs_pums/marital_all_years.rds")
if (!file.exists(std_pop_file)) {
  stop("Standard population file not found: ", std_pop_file)
}

# Get standard population by age group (for AMR)
std_pop_by_group <- get_2010_standard_population(cache_file = std_pop_file, by_age_group = TRUE)
cat("  Standard population by group: ", nrow(std_pop_by_group), " rows\n", sep = "")

# Add year column for build_standard_population_grid
std_pop_by_group[, year := 2010]

# Build the grid
unmarried_pop_grid <- build_standard_population_grid(
  unmarried_pop = std_pop_by_group,
  std_year = 2010,
  min_age = MARGRID_MIN_AGE,
  max_age = MARGRID_MAX_AGE
)
cat("  Unmarried pop grid: ", nrow(unmarried_pop_grid), " x ", ncol(unmarried_pop_grid), "\n", sep = "")
cat("  Total geometric mean: ", format(sum(unmarried_pop_grid), big.mark = ","), "\n", sep = "")

# =========================================================================
# STEP 3: Load NCHS subset data (1989-1995)
# =========================================================================
cat("\nStep 3: Loading NCHS subset data (1989-1995)...\n")

nchs_subset <- fetch_nchs_mra_marriages_1989_1995()
cat("  NCHS subset 1989-1995: ", nrow(nchs_subset), " rows\n", sep = "")
cat("  Years: ", paste(unique(nchs_subset$year), collapse = ", "), "\n", sep = "")
cat("  Total marriages by year:\n")
print(nchs_subset[, .(total = sum(marriages)), by = year])

# =========================================================================
# STEP 4: Load NCHS U.S. totals
# =========================================================================
cat("\nStep 4: Loading NCHS U.S. total marriages...\n")

nchs_us_totals <- fetch_nchs_us_total_marriages(years = 1989:2022)
cat("  NCHS U.S. totals: ", nrow(nchs_us_totals), " years\n", sep = "")
cat("  Sample totals:\n")
print(head(nchs_us_totals, 5))

# =========================================================================
# STEP 5: Load ACS marriage grids (2008-2022)
# =========================================================================
cat("\nStep 5: Loading ACS marriage grids (2008-2022)...\n")

acs_cache_dir <- here::here("data/cache/acs_marriage")
acs_files <- list.files(acs_cache_dir, pattern = "^new_marriages_20[0-9]{2}\\.rds$", full.names = TRUE)
cat("  Found ", length(acs_files), " ACS marriage grid files\n", sep = "")

acs_grids <- list()
for (f in acs_files) {
  yr <- as.integer(gsub(".*new_marriages_([0-9]{4})\\.rds", "\\1", f))
  data <- readRDS(f)
  acs_grids[[as.character(yr)]] <- data$grid
  cat("    ", yr, ": ", format(round(sum(data$grid)), big.mark = ","), " marriages\n", sep = "")
}

# =========================================================================
# STEP 6: Test 1989-1995 calculation
# =========================================================================
cat("\n=== Testing 1989-1995 calculation ===\n")

result_1989_1995 <- calculate_historical_rates_1989_1995(
  base_margrid = base_margrid,
  nchs_subset = nchs_subset,
  nchs_us_totals = nchs_us_totals,
  unmarried_pop_grid = unmarried_pop_grid,
  ss_area_factor = 1.003,
  smooth = TRUE
)

cat("\n1989-1995 AMR Results:\n")
print(result_1989_1995$amr)

# =========================================================================
# STEP 7: Test interpolation (1996-2007)
# =========================================================================
cat("\n=== Testing 1996-2007 interpolation ===\n")

grid_1995 <- result_1989_1995$rates[["1995"]]
grid_2008 <- acs_grids[["2008"]]

if (is.null(grid_1995)) {
  cat("ERROR: 1995 grid not available\n")
} else if (is.null(grid_2008)) {
  cat("ERROR: 2008 grid not available\n")
} else {
  # Need to align dimensions - ACS grid may be different size than MarGrid
  cat("  1995 grid: ", nrow(grid_1995), " x ", ncol(grid_1995), "\n", sep = "")
  cat("  2008 ACS grid: ", nrow(grid_2008), " x ", ncol(grid_2008), "\n", sep = "")

  # Convert ACS grid to rates using same scale as MarGrid
  # For now, skip if dimensions don't match
  if (nrow(grid_1995) == nrow(grid_2008) && ncol(grid_1995) == ncol(grid_2008)) {
    result_interp <- interpolate_marriage_grids(
      grid_start = grid_1995,
      grid_end = grid_2008,
      year_start = 1995,
      year_end = 2008,
      years = 1996:2007,
      nchs_us_totals = nchs_us_totals,
      unmarried_pop_grid = unmarried_pop_grid
    )
    cat("\nInterpolated AMR Results:\n")
    print(result_interp$amr)
  } else {
    cat("  Dimensions don't match - need to align ACS to MarGrid size\n")
    cat("  This is expected - ACS uses ages 15-99, MarGrid uses 14-100\n")
  }
}

# =========================================================================
# STEP 8: Test 2008-2022 ACS calculation
# =========================================================================
cat("\n=== Testing 2008-2022 ACS calculation ===\n")

# First, we need to align ACS grids to MarGrid dimensions
# ACS is 15-99, MarGrid is 14-100
# Create aligned ACS grids
aligned_acs_grids <- list()
for (yr_char in names(acs_grids)) {
  acs_grid <- acs_grids[[yr_char]]
  acs_ages <- as.integer(rownames(acs_grid))

  # Create empty MarGrid-sized matrix
  aligned <- matrix(0, nrow = MARGRID_SIZE, ncol = MARGRID_SIZE,
                    dimnames = list(MARGRID_MIN_AGE:MARGRID_MAX_AGE,
                                   MARGRID_MIN_AGE:MARGRID_MAX_AGE))

  # Copy values from ACS grid into aligned positions
  for (i in seq_along(acs_ages)) {
    h_age <- acs_ages[i]
    if (h_age < MARGRID_MIN_AGE || h_age > MARGRID_MAX_AGE) next
    h_idx <- h_age - MARGRID_MIN_AGE + 1

    for (j in seq_along(acs_ages)) {
      w_age <- acs_ages[j]
      if (w_age < MARGRID_MIN_AGE || w_age > MARGRID_MAX_AGE) next
      w_idx <- w_age - MARGRID_MIN_AGE + 1

      aligned[h_idx, w_idx] <- acs_grid[i, j]
    }
  }

  aligned_acs_grids[[yr_char]] <- aligned
}
cat("  Aligned ", length(aligned_acs_grids), " ACS grids to MarGrid dimensions\n", sep = "")

result_2008_2022 <- calculate_historical_rates_2008_2022(
  base_margrid = base_margrid,
  acs_grids = aligned_acs_grids,
  nchs_us_totals = nchs_us_totals,
  unmarried_pop_grid = unmarried_pop_grid,
  ss_area_factor = 1.003,
  smooth = TRUE
)

cat("\n2008-2022 AMR Results:\n")
print(result_2008_2022$amr)

# =========================================================================
# STEP 9: Run complete historical period
# =========================================================================
cat("\n=== Running complete historical period calculation ===\n")

result_full <- calculate_historical_period(
  base_margrid = base_margrid,
  nchs_subset = nchs_subset,
  acs_grids = aligned_acs_grids,
  nchs_us_totals = nchs_us_totals,
  unmarried_pop_grid = unmarried_pop_grid,
  ss_area_factor = 1.003,
  smooth = TRUE
)

cat("\n=== FINAL RESULTS ===\n")
cat("\nAll years AMR:\n")
print(result_full$amr)

cat("\nSummary Statistics:\n")
print(result_full$summary)

cat("\n=== Phase 6D Test Complete ===\n")

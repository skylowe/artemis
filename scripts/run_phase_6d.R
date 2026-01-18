#!/usr/bin/env Rscript
# Run Phase 6D calculation and cache results

library(data.table)
library(here)

source(here::here("R/demography/marriage.R"))
source(here::here("R/data_acquisition/nchs_marriage.R"))
source(here::here("R/data_acquisition/acs_marriage.R"))

cat("=== Phase 6D: Building and Caching Historical Rates ===\n\n")

# Step 1: Build base MarGrid
cat("Step 1: Building base MarGrid...\n")
nchs_1978_1988 <- fetch_nchs_mra_marriages_1978_1988()
cps_unmarried <- readRDS(here::here("data/cache/ipums_cps/cps_unmarried_1957_1995.rds"))

margrid_result <- build_base_margrid(
  nchs_marriages = nchs_1978_1988,
  cps_unmarried = cps_unmarried,
  years = 1978:1988,
  smooth = TRUE
)
base_margrid <- margrid_result$margrid
cat("  Base MarGrid built: ", nrow(base_margrid), "x", ncol(base_margrid), "\n")

# Step 2: Build standard population grid
cat("\nStep 2: Building standard population grid...\n")
std_pop_file <- here::here("data/cache/acs_pums/marital_all_years.rds")
std_pop_by_group <- get_2010_standard_population(cache_file = std_pop_file, by_age_group = TRUE)
std_pop_by_group[, year := 2010]

unmarried_pop_grid <- build_standard_population_grid(
  unmarried_pop = std_pop_by_group,
  std_year = 2010,
  min_age = MARGRID_MIN_AGE,
  max_age = MARGRID_MAX_AGE
)
cat("  Standard pop grid built\n")
cat("  Total male unmarried: ", format(attr(unmarried_pop_grid, "total_male"), big.mark = ","), "\n")
cat("  Total female unmarried: ", format(attr(unmarried_pop_grid, "total_female"), big.mark = ","), "\n")

# Step 3: Load input data
cat("\nStep 3: Loading input data...\n")
nchs_subset <- fetch_nchs_mra_marriages_1989_1995()
nchs_us_totals <- fetch_nchs_us_total_marriages(years = 1989:2022)

# Load and align ACS grids
acs_cache_dir <- here::here("data/cache/acs_marriage")
acs_files <- list.files(acs_cache_dir, pattern = "^new_marriages_20[0-9]{2}\\.rds$", full.names = TRUE)

aligned_acs_grids <- list()
for (f in acs_files) {
  yr <- as.integer(gsub(".*new_marriages_([0-9]{4})\\.rds", "\\1", f))
  data <- readRDS(f)
  acs_grid <- data$grid
  acs_ages <- as.integer(rownames(acs_grid))

  # Create MarGrid-sized matrix
  aligned <- matrix(0, nrow = MARGRID_SIZE, ncol = MARGRID_SIZE,
                    dimnames = list(MARGRID_MIN_AGE:MARGRID_MAX_AGE,
                                   MARGRID_MIN_AGE:MARGRID_MAX_AGE))

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
  aligned_acs_grids[[as.character(yr)]] <- aligned
}
cat("  Loaded ", length(aligned_acs_grids), " ACS grids\n")

# Step 4: Run full historical period calculation
cat("\nStep 4: Running historical period calculation (this will take a few minutes)...\n")

result <- calculate_historical_period(
  base_margrid = base_margrid,
  nchs_subset = nchs_subset,
  acs_grids = aligned_acs_grids,
  nchs_us_totals = nchs_us_totals,
  unmarried_pop_grid = unmarried_pop_grid,
  ss_area_factor = 1.003,
  smooth = TRUE,
  force_recompute = TRUE  # Force fresh calculation
)

cat("\n=== RESULTS ===\n")
cat("\nHistorical AMR by year:\n")
print(result$amr)

cat("\nSummary:\n")
print(result$summary)

cat("\n=== Phase 6D Complete - Results Cached ===\n")

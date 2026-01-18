#!/usr/bin/env Rscript
# Run Phase 6F: Marriage Rate Projection with Prior Status Differentials

library(data.table)
library(here)

source(here::here("R/utils/api_helpers.R"))
source(here::here("R/demography/marriage.R"))
source(here::here("R/data_acquisition/nchs_marriage.R"))
source(here::here("R/data_acquisition/acs_marriage.R"))
source(here::here("R/data_acquisition/ipums_cps.R"))

cat("=== Phase 6F: Marriage Rate Projection ===\n\n")

# =============================================================================
# STEP 1: Load all required data
# =============================================================================
cat("Step 1: Loading input data...\n")

# 1a. NCHS marriages 1978-1988
cat("  Loading NCHS marriages (1978-1988)...\n")
nchs_1978_1988 <- fetch_nchs_mra_marriages_1978_1988()
cat("    Rows:", nrow(nchs_1978_1988), "\n")

# 1b. CPS unmarried population
cat("  Loading CPS unmarried population (1978-1995)...\n")
cps_unmarried <- readRDS(here::here("data/cache/ipums_cps/cps_unmarried_1957_1995.rds"))
cat("    Rows:", nrow(cps_unmarried), "\n")

# 1c. NCHS subset 1989-1995
cat("  Loading NCHS subset (1989-1995)...\n")
nchs_subset <- fetch_nchs_mra_marriages_1989_1995()
cat("    Rows:", nrow(nchs_subset), "\n")

# 1d. NCHS U.S. totals
cat("  Loading NCHS U.S. totals...\n")
nchs_us_totals <- fetch_nchs_us_total_marriages(years = 1989:2022)
cat("    Rows:", nrow(nchs_us_totals), "\n")

# 1e. ACS grids
cat("  Loading ACS marriage grids...\n")
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
cat("    ACS grids:", length(aligned_acs_grids), "\n")

# 1f. 2010 standard population
cat("  Loading 2010 standard population...\n")
std_pop_file <- here::here("data/cache/acs_pums/marital_all_years.rds")
std_pop_by_group <- get_2010_standard_population(cache_file = std_pop_file, by_age_group = TRUE)
cat("    Rows:", nrow(std_pop_by_group), "\n")

# 1g. Prior marital status data (1979, 1981-88)
cat("  Loading prior marital status data (1979, 1981-88)...\n")
prior_status_data <- fetch_nchs_marriages_by_prior_status_1978_1988()
cat("    Rows:", nrow(prior_status_data), "\n")

# 1h. ACS same-sex marriage grids (2015-2022)
cat("  Loading ACS same-sex marriage grids (2015-2022)...\n")
same_sex_data <- fetch_acs_same_sex_grids(years = 2015:2022)
cat("    Years:", length(same_sex_data$years), "\n")
cat("    Male-male share:", round(100 * sum(same_sex_data$totals[sex_combo == "male_male", marriages]) /
                                   sum(same_sex_data$totals$marriages), 1), "%\n")

# 1i. Calculate same-sex fraction from ACS data
cat("  Calculating same-sex fraction from ACS data...\n")
ss_fraction_data <- calculate_same_sex_fraction(
  same_sex_grids = same_sex_data,
  opposite_sex_grids = aligned_acs_grids,
  years = 2015:2019  # Use overlapping years
)
same_sex_fraction <- ss_fraction_data$overall_fraction
cat("    Same-sex fraction:", round(same_sex_fraction * 100, 2), "%\n")

# =============================================================================
# STEP 2: Test prior status differentials calculation
# =============================================================================
cat("\n=== Testing Prior Status Differentials ===\n")

status_diffs <- calculate_prior_status_differentials(
  marriages_by_status = prior_status_data,
  years = c(1979, 1981:1988)
)

cat("\nDifferentials by status and age group (females):\n")
print(status_diffs[sex == "female", .(age_group, prior_status,
                                       relative_rate = round(relative_rate, 3),
                                       avg_proportion = round(avg_proportion, 3))])

cat("\nKey observations:\n")
cat("  - Single relative rate ranges:", round(range(status_diffs[prior_status == "single"]$relative_rate), 3), "\n")
cat("  - Divorced relative rate ranges:", round(range(status_diffs[prior_status == "divorced"]$relative_rate), 3), "\n")
cat("  - Widowed relative rate ranges:", round(range(status_diffs[prior_status == "widowed"]$relative_rate), 3), "\n")

# =============================================================================
# STEP 3: Run complete marriage projection
# =============================================================================
cat("\n=== Running Complete Marriage Projection ===\n")

result <- run_marriage_projection(
  nchs_marriages_1978_1988 = nchs_1978_1988,
  cps_unmarried = cps_unmarried,
  nchs_subset = nchs_subset,
  acs_grids = aligned_acs_grids,
  nchs_us_totals = nchs_us_totals,
  standard_pop_by_group = std_pop_by_group,
  prior_status_data = prior_status_data,
  same_sex_data = same_sex_data,
  same_sex_fraction = same_sex_fraction,
  ultimate_amr = 4000,
  ultimate_year = 2047,
  end_year = 2099,
  include_same_sex = TRUE,
  include_prior_status = TRUE,
  force_recompute = TRUE
)

# =============================================================================
# STEP 4: Validate results
# =============================================================================
cat("\n=== Validation Results ===\n")

cat("\nHistorical AMR (sample years):\n")
historical_years <- c(1989, 1995, 2000, 2010, 2019, 2022)
for (yr in historical_years) {
  if (yr %in% result$amr_historical$year) {
    amr <- result$amr_historical[year == yr, amr]
    cat("  ", yr, ":", round(amr, 1), "\n")
  }
}

cat("\nProjected AMR (sample years):\n")
proj_years <- c(2023, 2030, 2040, 2047, 2050, 2075, 2099)
for (yr in proj_years) {
  if (yr %in% result$amr_projected$year) {
    amr <- result$amr_projected[year == yr, projected_amr]
    cat("  ", yr, ":", round(amr, 1), "\n")
  }
}

cat("\nPrior status rate factors (at age 25-29):\n")
if (!is.null(result$status_differentials)) {
  factors <- result$status_differentials[age_group == "25-29"]
  for (i in seq_len(nrow(factors))) {
    cat("  ", factors$sex[i], "-", factors$prior_status[i], ":",
        round(factors$relative_rate[i], 3), "\n")
  }
}

cat("\nSame-sex/opposite-sex split:\n")
if (!is.null(result$opposite_sex_rates)) {
  yr <- "2050"
  opp_total <- sum(result$opposite_sex_rates[[yr]], na.rm = TRUE)
  same_total <- sum(result$same_sex_rates[[yr]], na.rm = TRUE)
  cat("  2050 opposite-sex total:", round(opp_total), "\n")
  cat("  2050 same-sex total:", round(same_total), "\n")
  cat("  Same-sex percentage:", round(100 * same_total / (opp_total + same_total), 1), "%\n")

  if (!is.null(result$male_male_rates)) {
    mm_total <- sum(result$male_male_rates[[yr]], na.rm = TRUE)
    ff_total <- sum(result$female_female_rates[[yr]], na.rm = TRUE)
    cat("    Male-male:", round(mm_total), "(", round(100 * mm_total / same_total, 1), "% of same-sex)\n")
    cat("    Female-female:", round(ff_total), "(", round(100 * ff_total / same_total, 1), "% of same-sex)\n")
  }
}

cat("\n=== Phase 6F Complete ===\n")
cat("Total years:", length(result$all_rates), "\n")
cat("Historical years:", length(result$historical_rates), "\n")
cat("Projected years:", length(result$projected_rates), "\n")
cat("Prior status included:", !is.null(result$rates_by_status), "\n")
cat("Same-sex separation included:", !is.null(result$opposite_sex_rates), "\n")
cat("Male-male/female-female split:", !is.null(result$male_male_rates), "\n")

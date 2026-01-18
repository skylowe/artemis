#!/usr/bin/env Rscript
# Run Phase 6H: Marriage Validation
# Tests the complete marriage projection and validation pipeline

library(data.table)
library(here)

# Source required files
source(here::here("R/utils/api_helpers.R"))
source(here::here("R/demography/marriage.R"))
source(here::here("R/data_acquisition/nchs_marriage.R"))
source(here::here("R/data_acquisition/acs_marriage.R"))
source(here::here("R/data_acquisition/ipums_cps.R"))
source(here::here("R/validation/validate_marriage.R"))

cat("=== Phase 6H: Marriage Projection Validation ===\n\n")

# =============================================================================
# STEP 1: Load cached projection or run if needed
# =============================================================================
cat("Step 1: Loading marriage projection...\n")

cache_file <- here::here("data/cache/marriage/marriage_projection_complete.rds")

if (file.exists(cache_file)) {
  cat("  Loading from cache...\n")
  projection <- readRDS(cache_file)
  cat("  Loaded projection with", length(projection$all_rates), "years\n")
} else {
  cat("  Running full projection (no cache found)...\n")

  # Load all required data
  cat("  Loading NCHS marriages (1978-1988)...\n")
  nchs_1978_1988 <- fetch_nchs_mra_marriages_1978_1988()

  cat("  Loading CPS unmarried population...\n")
  cps_unmarried <- readRDS(here::here("data/cache/ipums_cps/cps_unmarried_1957_1995.rds"))

  cat("  Loading NCHS subset (1989-1995)...\n")
  nchs_subset <- fetch_nchs_mra_marriages_1989_1995()

  cat("  Loading NCHS U.S. totals...\n")
  nchs_us_totals <- fetch_nchs_us_total_marriages(years = 1989:2022)

  cat("  Loading ACS grids...\n")
  acs_cache_dir <- here::here("data/cache/acs_marriage")
  acs_files <- list.files(acs_cache_dir, pattern = "^new_marriages_20[0-9]{2}\\.rds$", full.names = TRUE)

  aligned_acs_grids <- list()
  for (f in acs_files) {
    yr <- as.integer(gsub(".*new_marriages_([0-9]{4})\\.rds", "\\1", f))
    data <- readRDS(f)
    acs_grid <- data$grid
    acs_ages <- as.integer(rownames(acs_grid))

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

  cat("  Loading standard population...\n")
  std_pop_file <- here::here("data/cache/acs_pums/marital_all_years.rds")
  std_pop_by_group <- get_2010_standard_population(cache_file = std_pop_file, by_age_group = TRUE)

  cat("  Loading prior status data...\n")
  prior_status_data <- fetch_nchs_marriages_by_prior_status_1978_1988()

  cat("  Loading same-sex grids...\n")
  same_sex_data <- fetch_acs_same_sex_grids(years = 2015:2022)

  cat("  Calculating same-sex fraction...\n")
  ss_fraction_data <- calculate_same_sex_fraction(
    same_sex_grids = same_sex_data,
    opposite_sex_grids = aligned_acs_grids,
    years = 2015:2019
  )
  same_sex_fraction <- ss_fraction_data$overall_fraction

  cat("  Running projection...\n")
  projection <- run_marriage_projection(
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
}

# =============================================================================
# STEP 2: Run comprehensive validation
# =============================================================================
cat("\n=== Running Comprehensive Validation ===\n\n")

validation_result <- validate_marriage_comprehensive(
  projection = projection,
  tolerance_amr = 0.001,
  tolerance_totals = 0.05
)

# =============================================================================
# STEP 3: Display summary
# =============================================================================
cat("\n=== Validation Summary ===\n\n")

cat("Overall Result: ", ifelse(validation_result$passed, "PASSED", "FAILED"), "\n")
cat("Checks Passed: ", validation_result$n_passed, "/", validation_result$n_checks, "\n\n")

# Display detailed results
cat("Detailed Results:\n")
for (i in seq_len(nrow(validation_result$summary))) {
  row <- validation_result$summary[i]
  status <- ifelse(row$passed == "PASS", "✓", ifelse(row$passed == "FAIL", "✗", "?"))
  cat(sprintf("  %s %s: %s\n", status, row$check, row$message))
}

# =============================================================================
# STEP 4: Display key metrics
# =============================================================================
cat("\n=== Key Metrics ===\n\n")

cat("Historical AMR Range:\n")
if (!is.null(projection$amr_historical)) {
  hist_amr <- projection$amr_historical
  cat("  Years:", min(hist_amr$year), "-", max(hist_amr$year), "\n")
  cat("  AMR Range:", round(min(hist_amr$amr), 1), "-", round(max(hist_amr$amr), 1), "\n")
}

cat("\nProjected AMR:\n")
if (!is.null(projection$amr_projected)) {
  proj_amr <- projection$amr_projected
  sample_years <- c(2025, 2030, 2040, 2047, 2050, 2099)
  for (yr in sample_years[sample_years %in% proj_amr$year]) {
    amr <- proj_amr[year == yr, projected_amr]
    cat("  ", yr, ":", round(amr, 1), "\n")
  }
}

cat("\nMarGrid Properties:\n")
if (!is.null(projection$margrid)) {
  margrid <- projection$margrid
  cat("  Dimensions:", nrow(margrid), "×", ncol(margrid), "\n")
  cat("  Peak Rate:", round(max(margrid, na.rm = TRUE), 1), "\n")
  max_idx <- which(margrid == max(margrid, na.rm = TRUE), arr.ind = TRUE)[1,]
  cat("  Peak Ages: H=", max_idx[1] + 13, ", W=", max_idx[2] + 13, "\n")
}

cat("\nSame-Sex Separation:\n")
if (!is.null(projection$opposite_sex_rates)) {
  yr <- "2050"
  opp_total <- sum(projection$opposite_sex_rates[[yr]], na.rm = TRUE)
  ss_total <- sum(projection$same_sex_rates[[yr]], na.rm = TRUE)
  cat("  2050 Opposite-Sex:", round(opp_total), "\n")
  cat("  2050 Same-Sex:", round(ss_total), "\n")
  cat("  Same-Sex %:", round(100 * ss_total / (opp_total + ss_total), 2), "%\n")

  if (!is.null(projection$male_male_rates)) {
    mm <- sum(projection$male_male_rates[[yr]], na.rm = TRUE)
    ff <- sum(projection$female_female_rates[[yr]], na.rm = TRUE)
    cat("  Male-Male %:", round(100 * mm / ss_total, 1), "%\n")
    cat("  Female-Female %:", round(100 * ff / ss_total, 1), "%\n")
  }
}

cat("\nPrior Status Differentials:\n")
if (!is.null(projection$status_differentials)) {
  diff <- projection$status_differentials
  cat("  Rows:", nrow(diff), "\n")
  cat("  Statuses:", paste(unique(diff$prior_status), collapse = ", "), "\n")
  cat("  Rate Range:", round(min(diff$relative_rate), 3), "-",
      round(max(diff$relative_rate), 3), "\n")
}

cat("\n=== Phase 6H Validation Complete ===\n")
cat("Total years in projection:", length(projection$all_rates), "\n")
cat("Historical years:", length(projection$historical_years), "\n")
cat("Projected years:", length(projection$projection_years), "\n")

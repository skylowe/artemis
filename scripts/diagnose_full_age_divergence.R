#!/usr/bin/env Rscript
#' Diagnose Full Age Distribution Divergence
#'
#' Analyzes population divergence across ALL ages to understand the impact
#' of the elderly override on younger ages

library(data.table)
library(targets)

cat("=================================================================\n")
cat("FULL AGE DISTRIBUTION DIVERGENCE ANALYSIS\n")
cat("=================================================================\n\n")

# =============================================================================
# Load Data
# =============================================================================

cat("--- Loading Data ---\n\n")

# TR2025 population
tr_pop <- fread("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")
tr_pop_long <- rbindlist(list(
  tr_pop[, .(year = Year, age = Age, sex = "male", population = `M Tot`)],
  tr_pop[, .(year = Year, age = Age, sex = "female", population = `F Tot`)]
))

# Our projected population
our_pop <- tar_read(projected_population)

# TR2025 implied immigration distribution
implied <- fread("data/processed/tr2025_implied_immigration_distribution.csv")

# Our immigration distribution
our_dist <- tar_read(lpr_distribution)

cat("Data loaded.\n")

# =============================================================================
# Compare Immigration Distribution: Ours vs TR2025 Implied
# =============================================================================

cat("\n--- Immigration Distribution Comparison ---\n\n")

# Merge for comparison
dist_comp <- merge(
  implied[, .(age, sex, tr_pct = implied_dist * 100)],
  our_dist[, .(age, sex, our_pct = distribution * 100)],
  by = c("age", "sex")
)
dist_comp[, diff := our_pct - tr_pct]

# Aggregate by age groups
age_groups <- list(
  "0-17" = 0:17,
  "18-24" = 18:24,
  "25-44" = 25:44,
  "45-64" = 45:64,
  "65-84" = 65:84,
  "85-99" = 85:99,
  "100+" = 100
)

cat("Immigration distribution by age group (% of total):\n")
cat(sprintf("%-10s %10s %10s %10s\n", "Age Group", "TR2025", "Ours", "Diff"))
cat(sprintf("%-10s %10s %10s %10s\n", "---------", "------", "----", "----"))
for (grp in names(age_groups)) {
  ages <- age_groups[[grp]]
  tr_val <- dist_comp[age %in% ages, sum(tr_pct)]
  our_val <- dist_comp[age %in% ages, sum(our_pct)]
  diff_val <- our_val - tr_val
  cat(sprintf("%-10s %10.2f %10.2f %+10.2f\n", grp, tr_val, our_val, diff_val))
}

# =============================================================================
# Population Divergence by Age Group Over Time
# =============================================================================

cat("\n--- Population Divergence by Age Group ---\n\n")

# Merge populations
pop_comp <- merge(
  tr_pop_long[year >= 2023, .(year, age, sex, tr_pop = population)],
  our_pop[, .(year, age, sex, our_pop = population)],
  by = c("year", "age", "sex")
)
pop_comp[, error := (our_pop - tr_pop) / tr_pop * 100]

# Summarize by age group and year
years_to_show <- c(2030, 2050, 2075, 2099)

cat("Population error by age group (%):\n\n")
for (yr in years_to_show) {
  cat(sprintf("Year %d:\n", yr))
  for (grp in names(age_groups)) {
    ages <- age_groups[[grp]]
    tr_val <- pop_comp[year == yr & age %in% ages, sum(tr_pop)]
    our_val <- pop_comp[year == yr & age %in% ages, sum(our_pop)]
    err <- (our_val - tr_val) / tr_val * 100
    cat(sprintf("  %-10s: %+6.2f%% (TR: %10.0f, Ours: %10.0f)\n", grp, err, tr_val, our_val))
  }
  total_tr <- pop_comp[year == yr, sum(tr_pop)]
  total_our <- pop_comp[year == yr, sum(our_pop)]
  total_err <- (total_our - total_tr) / total_tr * 100
  cat(sprintf("  %-10s: %+6.2f%% (TR: %10.0f, Ours: %10.0f)\n", "TOTAL", total_err, total_tr, total_our))
  cat("\n")
}

# =============================================================================
# Analyze Total Net Immigration
# =============================================================================

cat("\n--- Total Net Immigration Analysis ---\n\n")

# Get our net immigration totals
net_lpr <- tar_read(net_lpr_immigration)
net_o <- tar_read(net_o_for_projection)

# Find the correct column names
lpr_col <- intersect(names(net_lpr), c("net_lpr_immigration", "net_immigration", "total"))
o_col <- intersect(names(net_o), c("net_o_immigration", "net_immigration", "total"))

if (length(lpr_col) > 0 && length(o_col) > 0) {
  our_total_ni <- merge(
    net_lpr[, .(year, net_lpr = get(lpr_col[1]))],
    net_o[, .(year, net_o = get(o_col[1]))],
    by = "year"
  )
  our_total_ni[, total_ni := net_lpr + net_o]

  cat("Our total net immigration by year:\n")
  print(our_total_ni[year %in% c(2025, 2030, 2050, 2099)])
}

# Compare with TR2025 implied total
tr_total_implied <- implied[, sum(avg_implied)]
cat(sprintf("\nTR2025 implied total net immigration (averaged): %.0f\n", tr_total_implied))

# =============================================================================
# Key Finding: Distribution Shift Impact
# =============================================================================

cat("\n=================================================================\n")
cat("KEY FINDINGS\n")
cat("=================================================================\n\n")

cat("1. IMMIGRATION DISTRIBUTION SHIFT:\n")
cat("   When we apply elderly override (85+ gets 3.5% of total),\n")
cat("   we scale down ages 0-84. This reduces immigration to younger ages.\n\n")

cat("2. ROOT CAUSE HYPOTHESIS:\n")
cat("   TR2025's total net immigration may be HIGHER than ours.\n")
cat("   If TR2025 has higher total immigration, then even with the same\n")
cat("   percentage at ages 0-84, the absolute numbers would be higher.\n\n")

cat("3. ALTERNATIVE EXPLANATION:\n")
cat("   TR2025's immigration distribution at younger ages might be\n")
cat("   systematically different from DHS data (which we use as base).\n\n")

# =============================================================================
# Calculate Required Adjustment
# =============================================================================

cat("\n--- Calculating Required Adjustment ---\n\n")

# What would total immigration need to be to match TR2025 at all ages?
# Look at a specific year (2050) and calculate implied total

yr_test <- 2050

# TR2025 implied immigration (back-calculated from population)
# We have this from the earlier analysis
tr_implied_total <- implied[, sum(avg_implied)]

# Our current total
if (exists("our_total_ni")) {
  our_total <- our_total_ni[year == yr_test, total_ni]
  cat(sprintf("Our total net immigration in %d: %.0f\n", yr_test, our_total))
  cat(sprintf("TR2025 implied annual average: %.0f\n", tr_implied_total))
  cat(sprintf("Ratio (TR/Ours): %.2f\n", tr_implied_total / our_total))
}

# =============================================================================
# Test: What if we DON'T scale down 0-84?
# =============================================================================

cat("\n--- Alternative Approach: Additive Override ---\n\n")

cat("Current approach: Scale down 0-84 when adding 85+ override\n")
cat("Alternative: Add 85+ override WITHOUT scaling down 0-84\n")
cat("  (This effectively increases total immigration)\n\n")

# Calculate what total would be with additive approach
if (exists("our_total_ni")) {
  elderly_addition <- 68500 - 4000  # 85-99 minus 100+ outflow
  new_total <- our_total + elderly_addition
  cat(sprintf("If additive: New total = %.0f + %.0f = %.0f\n",
              our_total, elderly_addition, new_total))
  cat(sprintf("Compared to TR implied %.0f: %.1f%%\n",
              tr_implied_total, (new_total - tr_implied_total) / tr_implied_total * 100))
}

cat("\n=== DONE ===\n")

#!/usr/bin/env Rscript
#' Explore Options for Fixing 85+ Immigration Distribution
#'
#' Analyzes different approaches to match TR2025's implied immigration at ages 85+

library(data.table)
library(targets)

cat("=================================================================\n")
cat("EXPLORING OPTIONS FOR 85+ IMMIGRATION DISTRIBUTION\n")
cat("=================================================================\n\n")

# =============================================================================
# Load Current Data
# =============================================================================

cat("--- Loading Current Data ---\n\n")

# TR2025 population
tr_pop <- fread("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")
tr_pop_long <- rbindlist(list(
  tr_pop[, .(year = Year, age = Age, sex = "male", population = `M Tot`)],
  tr_pop[, .(year = Year, age = Age, sex = "female", population = `F Tot`)]
))

# TR2025 qx
tr_qx_male <- fread("data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv", skip = 1)
tr_qx_female <- fread("data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv", skip = 1)
tr_qx_m <- melt(tr_qx_male, id.vars = "Year", variable.name = "age_chr", value.name = "qx")
tr_qx_m[, age := as.integer(as.character(age_chr))][, sex := "male"]
tr_qx_f <- melt(tr_qx_female, id.vars = "Year", variable.name = "age_chr", value.name = "qx")
tr_qx_f[, age := as.integer(as.character(age_chr))][, sex := "female"]
tr_qx <- rbindlist(list(tr_qx_m, tr_qx_f))[, .(year = Year, age, sex, qx)]

# Our current distributions
lpr_dist <- tar_read(lpr_distribution)
calibrated_file <- "data/processed/calibrated_lpr_distribution.csv"
if (file.exists(calibrated_file)) {
  calibrated_dist <- fread(calibrated_file)
}

# Current net immigration
net_lpr <- tar_read(net_lpr_immigration)
net_o <- tar_read(net_o_for_projection)

cat("Data loaded.\n")

# =============================================================================
# Calculate TR2025's Implied Net Immigration by Age
# =============================================================================

cat("\n--- Calculating TR2025 Implied Immigration Distribution ---\n\n")

# Calculate implied immigration for years 2023-2030
years_to_calc <- 2023:2030

implied_list <- list()
for (yr in years_to_calc) {
  for (a in 1:100) {
    for (s in c("male", "female")) {
      pop_prev <- tr_pop_long[year == yr - 1 & age == a - 1 & sex == s, population]
      pop_now <- tr_pop_long[year == yr & age == a & sex == s, population]
      qx_val <- tr_qx[year == yr & age == a & sex == s, qx]

      if (length(pop_prev) == 1 && length(pop_now) == 1 && length(qx_val) == 1) {
        survivors <- pop_prev * (1 - qx_val)
        implied <- pop_now - survivors
        implied_list[[length(implied_list) + 1]] <- data.table(
          year = yr, age = a, sex = s, implied_ni = implied
        )
      }
    }
  }
}
tr_implied <- rbindlist(implied_list)

# Average implied immigration by age/sex
tr_implied_avg <- tr_implied[, .(avg_implied = mean(implied_ni)), by = .(age, sex)]
tr_implied_avg[, total_implied := sum(avg_implied)]
tr_implied_avg[, implied_dist := avg_implied / total_implied]

cat("TR2025 Implied Immigration Distribution (ages 80-100):\n")
print(tr_implied_avg[age >= 80, .(age, sex, avg_implied = round(avg_implied),
                                   implied_pct = round(implied_dist * 100, 3))])

# Total at ages 85+
tr_85plus <- tr_implied_avg[age >= 85 & age < 100, sum(avg_implied)]
tr_85plus_pct <- tr_implied_avg[age >= 85 & age < 100, sum(implied_dist)] * 100
cat(sprintf("\nTR2025 implied at ages 85-99: %.0f (%.2f%% of total)\n", tr_85plus, tr_85plus_pct))

# =============================================================================
# OPTION 1: Adjust Calibrated Distribution at Ages 85+
# =============================================================================

cat("\n=================================================================\n")
cat("OPTION 1: Scale Up Calibrated Distribution at Ages 85+\n")
cat("=================================================================\n\n")

# Current calibrated distribution
current_85plus <- calibrated_dist[age >= 85 & age < 100, sum(distribution)]
current_85plus_pct <- current_85plus * 100

# Target: match TR2025's implied ~3.7%
target_85plus_pct <- tr_85plus_pct

# Required scale factor
scale_factor <- target_85plus_pct / current_85plus_pct

cat(sprintf("Current distribution at 85-99: %.4f%%\n", current_85plus_pct))
cat(sprintf("Target distribution at 85-99: %.2f%%\n", target_85plus_pct))
cat(sprintf("Required scale factor: %.1fx\n", scale_factor))

# Create scaled distribution
option1_dist <- copy(calibrated_dist)
option1_dist[age >= 85 & age < 100, distribution := distribution * scale_factor]
# Renormalize
option1_dist[, distribution := distribution / sum(distribution)]

cat("\nOption 1 Distribution at ages 85-99:\n")
cat(sprintf("  Before: %.4f%%\n", current_85plus_pct))
cat(sprintf("  After: %.4f%%\n", option1_dist[age >= 85 & age < 100, sum(distribution)] * 100))

# Impact on other ages
cat("\nImpact on other age groups:\n")
age_groups <- list(
  "0-17" = 0:17, "18-24" = 18:24, "25-44" = 25:44,
  "45-64" = 45:64, "65-84" = 65:84, "85-99" = 85:99
)
for (grp in names(age_groups)) {
  ages <- age_groups[[grp]]
  before <- calibrated_dist[age %in% ages, sum(distribution)] * 100
  after <- option1_dist[age %in% ages, sum(distribution)] * 100
  cat(sprintf("  %s: %.2f%% -> %.2f%% (change: %+.2f%%)\n", grp, before, after, after - before))
}

# =============================================================================
# OPTION 2: Use TR2025's Implied Distribution Directly
# =============================================================================

cat("\n=================================================================\n")
cat("OPTION 2: Use TR2025's Implied Distribution Directly\n")
cat("=================================================================\n\n")

# Normalize TR implied to create a distribution
option2_dist <- tr_implied_avg[age >= 0 & age <= 100, .(age, sex, distribution = implied_dist)]
# Handle negative implied (emigration > immigration) - set floor at 0
option2_dist[distribution < 0, distribution := 0]
option2_dist[, distribution := distribution / sum(distribution)]

cat("TR2025-derived distribution at key ages:\n")
for (grp in names(age_groups)) {
  ages <- age_groups[[grp]]
  pct <- option2_dist[age %in% ages, sum(distribution)] * 100
  cat(sprintf("  %s: %.2f%%\n", grp, pct))
}

cat("\nPros:\n")
cat("  - Directly matches TR2025's implied pattern\n")
cat("  - Should minimize population divergence\n")
cat("Cons:\n")
cat("  - Not based on actual immigration data (DHS/CBO)\n")
cat("  - Derived distribution may include methodology artifacts\n")

# =============================================================================
# OPTION 3: Hybrid - Keep DHS base, Override 85+
# =============================================================================

cat("\n=================================================================\n")
cat("OPTION 3: Hybrid - DHS Base with 85+ Override\n")
cat("=================================================================\n\n")

# Keep calibrated for ages 0-84, use TR implied for 85+
option3_dist <- copy(calibrated_dist)

# Get TR implied proportion for 85-99
tr_85_99_prop <- tr_implied_avg[age >= 85 & age < 100]
tr_85_99_total <- tr_85_99_prop[, sum(avg_implied)]
tr_total <- tr_implied_avg[, sum(avg_implied)]
tr_85_99_share <- tr_85_99_total / tr_total

# Scale down ages 0-84 to make room for 85-99
current_0_84 <- calibrated_dist[age < 85, sum(distribution)]
target_0_84 <- 1 - tr_85_99_share

scale_0_84 <- target_0_84 / current_0_84
option3_dist[age < 85, distribution := distribution * scale_0_84]

# Set 85-99 to TR implied proportions (relative within group)
tr_85_99_rel <- tr_85_99_prop[, .(age, sex, rel_prop = avg_implied / tr_85_99_total)]
option3_dist <- merge(option3_dist, tr_85_99_rel, by = c("age", "sex"), all.x = TRUE)
option3_dist[age >= 85 & age < 100, distribution := rel_prop * tr_85_99_share]
option3_dist[, rel_prop := NULL]

# Renormalize
option3_dist[, distribution := distribution / sum(distribution)]

cat("Hybrid distribution at key ages:\n")
for (grp in names(age_groups)) {
  ages <- age_groups[[grp]]
  before <- calibrated_dist[age %in% ages, sum(distribution)] * 100
  after <- option3_dist[age %in% ages, sum(distribution)] * 100
  cat(sprintf("  %s: %.2f%% -> %.2f%%\n", grp, before, after))
}

cat("\nPros:\n")
cat("  - Preserves DHS-based distribution for working ages\n")
cat("  - Matches TR2025 at ages 85+\n")
cat("Cons:\n")
cat("  - Mix of data sources\n")
cat("  - May have discontinuities at age 85 boundary\n")

# =============================================================================
# OPTION 4: Configuration Parameter for 85+ Immigration
# =============================================================================

cat("\n=================================================================\n")
cat("OPTION 4: Add Configuration Parameter for 85+ Immigration\n")
cat("=================================================================\n\n")

cat("Add to config/assumptions/tr2025.yaml:\n")
cat("```yaml\n")
cat("immigration:\n")
cat("  lpr:\n")
cat("    elderly_immigration_override:\n")
cat("      enabled: true\n")
cat("      # Override net immigration at ages 85+ (annual total)\n")
cat("      ages_85_99_annual: 70000  # ~3.7% of total\n")
cat("      age_100_plus_annual: -4000  # Negative (net outflow)\n")
cat("      # Distribute by sex (based on TR2025 pattern)\n")
cat("      female_share: 0.60\n")
cat("```\n")

cat("\nImplementation:\n")
cat("  - After standard distribution, replace ages 85+ with override values\n")
cat("  - Adjust ages 0-84 to maintain total\n")
cat("\nPros:\n")
cat("  - Explicit, transparent configuration\n")
cat("  - Easy to adjust for sensitivity analysis\n")
cat("  - Clear documentation of deviation from DHS data\n")
cat("Cons:\n")
cat("  - Another assumption to maintain\n")
cat("  - Values must be manually updated if TR2025 changes\n")

# =============================================================================
# OPTION 5: Re-calibrate with 85+ Constraints
# =============================================================================

cat("\n=================================================================\n")
cat("OPTION 5: Re-calibrate Optimizer with 85+ Constraints\n")
cat("=================================================================\n\n")

cat("Modify scripts/calibrate_lpr_dist.R:\n")
cat("  - Add heavy weight to 85-99 age group error\n")
cat("  - Or add hard constraint: sum(dist[85:99]) >= 0.03\n")

cat("\nExample weighted error function:\n")
cat("```r\n")
cat("calc_error <- function(scale_vector) {\n")
cat("  # ... existing code ...\n")
cat("  comp[, weight := fifelse(age >= 85 & age <= 99, 10.0,  # Heavy weight for 85-99\n")
cat("                   fifelse(age >= 20 & age <= 64, 2.0,\n")
cat("                   fifelse(age >= 65 & age <= 84, 1.5, 1.0)))]\n")
cat("  # ...\n")
cat("}\n")
cat("```\n")

cat("\nPros:\n")
cat("  - Uses optimization framework already in place\n")
cat("  - Balances all age groups simultaneously\n")
cat("Cons:\n")
cat("  - May not converge if 85+ target conflicts with other ages\n")
cat("  - Optimizer may find local minimum\n")

# =============================================================================
# Summary and Recommendation
# =============================================================================

cat("\n=================================================================\n")
cat("SUMMARY AND RECOMMENDATION\n")
cat("=================================================================\n\n")

cat("Current State:\n")
cat(sprintf("  - Our 85-99 immigration: %.2f%% of total\n", current_85plus_pct))
cat(sprintf("  - TR2025 implied 85-99: %.2f%% of total\n", target_85plus_pct))
cat(sprintf("  - Gap: ~%.0f people/year\n", tr_85plus))

cat("\nRecommendation: OPTION 4 (Configuration Parameter)\n")
cat("\nRationale:\n")
cat("  1. Most transparent - explicit override clearly documented\n")
cat("  2. Easy to enable/disable for comparison\n")
cat("  3. Preserves DHS-based distribution for working ages\n")
cat("  4. Allows sensitivity analysis by adjusting values\n")
cat("  5. Can be refined as we learn more about TR2025's methodology\n")

cat("\nImplementation Steps:\n")
cat("  1. Add elderly_immigration_override config section\n")
cat("  2. Modify get_immigration_distribution() to apply override\n")
cat("  3. Test with override enabled vs disabled\n")
cat("  4. Document methodology deviation in CLAUDE.md\n")

# =============================================================================
# Save TR2025 Implied Distribution for Reference
# =============================================================================

cat("\n--- Saving TR2025 Implied Distribution ---\n")
fwrite(tr_implied_avg[order(sex, age)], "data/processed/tr2025_implied_immigration_distribution.csv")
cat("Saved to: data/processed/tr2025_implied_immigration_distribution.csv\n")

cat("\n=== DONE ===\n")

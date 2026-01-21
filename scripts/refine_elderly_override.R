#!/usr/bin/env Rscript
#' Refine Elderly Immigration Override Values
#'
#' Analyzes TR2025 implied distribution to create better override parameters

library(data.table)
library(targets)

cat("=================================================================\n")
cat("REFINING ELDERLY IMMIGRATION OVERRIDE\n")
cat("=================================================================\n\n")

# =============================================================================
# Load TR2025 Implied Distribution
# =============================================================================

cat("--- Loading TR2025 Implied Distribution ---\n\n")

implied <- fread("data/processed/tr2025_implied_immigration_distribution.csv")

# =============================================================================
# Analyze Ages 85-99 Distribution Pattern
# =============================================================================

cat("--- Ages 85-99 Distribution Analysis ---\n\n")

# Filter to 85-99
dist_85_99 <- implied[age >= 85 & age < 100]
total_85_99 <- dist_85_99[, sum(avg_implied)]

cat(sprintf("Total implied immigration at ages 85-99: %.0f\n\n", total_85_99))

# Show by age
cat("Distribution by age (both sexes combined):\n")
by_age <- dist_85_99[, .(
  implied = sum(avg_implied),
  pct_of_85_99 = sum(avg_implied) / total_85_99 * 100
), by = age][order(age)]
print(by_age)

# Sex split
cat("\nSex split at ages 85-99:\n")
sex_split <- dist_85_99[, .(total = sum(avg_implied)), by = sex]
sex_split[, pct := total / sum(total) * 100]
print(sex_split)

# Female share
female_share_85_99 <- sex_split[sex == "female", total] / sex_split[, sum(total)]
cat(sprintf("\nFemale share at 85-99: %.1f%%\n", female_share_85_99 * 100))

# =============================================================================
# Analyze 100+ - This is the problematic age group
# =============================================================================

cat("\n--- Age 100+ Analysis ---\n\n")

dist_100 <- implied[age >= 100]
cat("Implied distribution at age 100+:\n")
print(dist_100[, .(age, sex, avg_implied, implied_pct = implied_dist * 100)])

cat("\nNote: Large positive values at 100+ are likely calculation artifacts.\n")
cat("The back-calculation formula NI = P(z) - P(z-1)*(1-qx) doesn't work for open-ended groups.\n")

# =============================================================================
# Compare with Current Override Values
# =============================================================================

cat("\n--- Current Override vs TR2025 Implied ---\n\n")

# Current override values (from config)
current_85_99 <- 68500
current_100 <- -4000

cat(sprintf("Current override 85-99: %d\n", current_85_99))
cat(sprintf("Current override 100+: %d\n", current_100))
cat(sprintf("TR2025 implied 85-99: %.0f\n", total_85_99))
cat(sprintf("TR2025 implied 100+: %.0f (likely artifact)\n", dist_100[, sum(avg_implied)]))

# =============================================================================
# Check Actual Population Divergence to Calibrate Override
# =============================================================================

cat("\n--- Loading Population Data to Calibrate Override ---\n\n")

# Load TR2025 population
tr_pop <- fread("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")

# Get ages 85-99 and 100+ over time
tr_pop_elderly <- tr_pop[Age >= 85, .(
  pop_85_99 = sum((`M Tot` + `F Tot`)[Age < 100]),
  pop_100 = sum((`M Tot` + `F Tot`)[Age >= 100])
), by = Year]

cat("TR2025 Population at elderly ages (selected years):\n")
print(tr_pop_elderly[Year %in% c(2023, 2030, 2050, 2075, 2099)])

# Load our projected population
our_pop <- tar_read(projected_population)
if (!is.null(our_pop)) {
  our_elderly <- our_pop[age >= 85, .(
    our_85_99 = sum(population[age < 100]),
    our_100 = sum(population[age >= 100])
  ), by = year]

  # Compare
  comp <- merge(
    tr_pop_elderly[, .(year = Year, tr_85_99 = pop_85_99, tr_100 = pop_100)],
    our_elderly,
    by = "year"
  )
  comp[, error_85_99 := (our_85_99 - tr_85_99) / tr_85_99 * 100]
  comp[, error_100 := (our_100 - tr_100) / tr_100 * 100]

  cat("\nPopulation comparison at elderly ages:\n")
  print(comp[year %in% c(2023, 2030, 2040, 2050, 2075, 2099),
             .(year, tr_85_99 = round(tr_85_99), our_85_99 = round(our_85_99),
               error_85_99 = round(error_85_99, 1),
               tr_100 = round(tr_100), our_100 = round(our_100),
               error_100 = round(error_100, 1))])
}

# =============================================================================
# Analyze Within-Age Distribution Pattern
# =============================================================================

cat("\n--- Within-Age Distribution Pattern (85-99) ---\n\n")

# Calculate relative weights within 85-99
weights_85_99 <- dist_85_99[, .(weight = sum(avg_implied)), by = age]
weights_85_99[, rel_weight := weight / sum(weight)]

cat("Relative weights by age (sums to 1.0):\n")
print(weights_85_99[order(age), .(age, annual_implied = round(weight), rel_weight = round(rel_weight, 4))])

# Fit a simple model to describe the pattern
# It looks like exponential decay with age
cat("\nPattern appears to be declining with age.\n")
cat("Age 85 weight:", round(weights_85_99[age == 85, rel_weight], 4), "\n")
cat("Age 99 weight:", round(weights_85_99[age == 99, rel_weight], 4), "\n")
cat("Ratio (85/99):", round(weights_85_99[age == 85, rel_weight] / weights_85_99[age == 99, rel_weight], 2), "\n")

# =============================================================================
# Calculate Refined Override Values
# =============================================================================

cat("\n=================================================================\n")
cat("REFINED OVERRIDE RECOMMENDATION\n")
cat("=================================================================\n\n")

# For ages 85-99, use the TR2025 implied total (already validated)
refined_85_99_total <- round(total_85_99)
refined_85_99_female_share <- round(female_share_85_99, 2)

cat("Ages 85-99:\n")
cat(sprintf("  annual_total: %d (was %d)\n", refined_85_99_total, current_85_99))
cat(sprintf("  female_share: %.2f (unchanged)\n", refined_85_99_female_share))

# For 100+, we need to investigate more carefully
# The implied distribution shows ~57K which seems unrealistic
# Let's back-calculate what's needed to match TR2025 population

cat("\nAge 100+ Investigation:\n")
cat("  TR2025 implied: ~57,000 (likely artifact from open-ended age group calculation)\n")

# Try to estimate 100+ net immigration from population changes
if (!is.null(our_pop)) {
  # Look at how 100+ population grows in TR2025
  tr_100_change <- tr_pop_elderly[Year >= 2023 & Year <= 2030,
                                   .(year = Year, pop_100, change = pop_100 - shift(pop_100))]
  cat("  TR2025 100+ population changes:\n")
  print(tr_100_change[!is.na(change)])

  # Average annual change
  avg_change <- tr_100_change[!is.na(change), mean(change)]
  cat(sprintf("  Average annual population change at 100+: %.0f\n", avg_change))

  # This change includes: survivors from 99 + survivors within 100+ + net immigration - deaths
  # Hard to isolate immigration from this
}

cat("\n")
cat("Recommendation for 100+:\n")
cat("  The current -4,000 override is causing over-projection.\n")
cat("  Options:\n")
cat("    1. Try -8,000 to -10,000 (more aggressive outflow)\n")
cat("    2. Set to 0 (neutral)\n")
cat("    3. Use small positive value if TR2025 implies net inflow\n")
cat("\n")

# =============================================================================
# Generate Updated Config
# =============================================================================

cat("=================================================================\n")
cat("RECOMMENDED CONFIG UPDATE\n")
cat("=================================================================\n\n")

cat("elderly_override:\n")
cat("  enabled: true\n")
cat("  # Ages 85-99: Use TR2025 implied values with age-declining pattern\n")
cat("  ages_85_99:\n")
cat(sprintf("    annual_total: %d  # TR2025 implied (was 68500)\n", refined_85_99_total))
cat(sprintf("    female_share: %.2f\n", refined_85_99_female_share))
cat("    # Use declining distribution by age (not uniform)\n")
cat("    use_declining_distribution: true\n")
cat("  # Age 100+: Test with different values\n")
cat("  age_100_plus:\n")
cat("    annual_total: 0  # Try neutral first (was -4000)\n")
cat("    female_share: 0.65\n")

# =============================================================================
# Save Detailed Weights for Implementation
# =============================================================================

cat("\n--- Saving Age-Specific Weights ---\n")

# Expand to include sex
weights_detailed <- dist_85_99[, .(
  age, sex,
  annual_implied = avg_implied,
  weight_within_85_99 = avg_implied / total_85_99
)]

fwrite(weights_detailed[order(age, sex)],
       "data/processed/tr2025_implied_85_99_weights.csv")
cat("Saved to: data/processed/tr2025_implied_85_99_weights.csv\n")

cat("\n=== DONE ===\n")

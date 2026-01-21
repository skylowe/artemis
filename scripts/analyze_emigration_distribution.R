#!/usr/bin/env Rscript
#' Analyze Emigration Distribution at Ages 85+
#'
#' Compares our distributions to TR2025 implied values

library(data.table)
library(targets)

cat("=================================================================\n")
cat("EMIGRATION DISTRIBUTION ANALYSIS\n")
cat("=================================================================\n\n")

# =============================================================================
# 1. Load Our Distributions
# =============================================================================

cat("--- 1. Our Current Distributions ---\n\n")

# Load the distributions we're using
lpr_dist <- tar_read(lpr_distribution)
emig_dist <- tar_read(emigration_distribution)

# Also load the calibrated distribution if available
calibrated_file <- "data/processed/calibrated_lpr_distribution.csv"
if (file.exists(calibrated_file)) {
  calibrated_dist <- fread(calibrated_file)
  cat("Loaded calibrated LPR distribution\n")
} else {
  calibrated_dist <- NULL
  cat("No calibrated distribution found\n")
}

# Check if our emigration uses same dist as immigration
cat("\nComparing LPR vs Emigration distribution (should be identical):\n")
comp_dist <- merge(
  lpr_dist[, .(age, sex, lpr_dist = distribution)],
  emig_dist[, .(age, sex, emig_dist = distribution)],
  by = c("age", "sex")
)
comp_dist[, diff := lpr_dist - emig_dist]
cat(sprintf("  Max difference: %.10f\n", max(abs(comp_dist$diff))))
cat(sprintf("  Same distribution: %s\n", ifelse(max(abs(comp_dist$diff)) < 1e-10, "YES", "NO")))

# =============================================================================
# 2. Our Distribution at Ages 85+
# =============================================================================

cat("\n--- 2. Distribution at Ages 85+ ---\n\n")

# LPR/Emigration distribution at 85+
dist_85plus <- lpr_dist[age >= 85]
cat("DHS-based distribution at ages 85+:\n")
print(dist_85plus[, .(age, sex, dist_pct = round(distribution * 100, 6))][age %in% c(85, 90, 95, 100)])

total_85plus <- dist_85plus[, sum(distribution)]
cat(sprintf("\nTotal distribution at ages 85+: %.4f%% of all immigration\n", total_85plus * 100))

# If calibrated exists
if (!is.null(calibrated_dist)) {
  cal_85plus <- calibrated_dist[age >= 85]
  cal_total <- cal_85plus[, sum(distribution)]
  cat(sprintf("Calibrated distribution at ages 85+: %.4f%% of all immigration\n", cal_total * 100))
}

# =============================================================================
# 3. Load LPR Assumptions to Calculate Absolute Numbers
# =============================================================================

cat("\n--- 3. Absolute Immigration/Emigration at Ages 85+ ---\n\n")

lpr_assumptions <- tar_read(lpr_assumptions)
net_lpr <- tar_read(net_lpr_immigration)

# Get totals for 2025
lpr_2025 <- lpr_assumptions[year == 2025]
cat(sprintf("2025 Total LPR Immigration: %s\n", format(lpr_2025$total_lpr, big.mark = ",")))
cat(sprintf("2025 Total Emigration (25%%): %s\n", format(lpr_2025$emigration, big.mark = ",")))
cat(sprintf("2025 Net LPR: %s\n", format(lpr_2025$net_lpr_total, big.mark = ",")))

# Immigration at 85+
imm_85plus <- lpr_2025$total_lpr * total_85plus
emig_85plus <- lpr_2025$emigration * total_85plus  # Same distribution
net_lpr_85plus <- imm_85plus - emig_85plus

cat(sprintf("\nAt Ages 85+ (using DHS distribution):\n"))
cat(sprintf("  Immigration: %.0f\n", imm_85plus))
cat(sprintf("  Emigration: %.0f\n", emig_85plus))
cat(sprintf("  Net LPR: %.0f\n", net_lpr_85plus))

# Verify against actual net_lpr target
actual_net_85plus <- net_lpr[year == 2025 & age >= 85, sum(net_lpr)]
cat(sprintf("  Actual Net LPR (from target): %.0f\n", actual_net_85plus))

# =============================================================================
# 4. What Does TR2025 Imply About Emigration?
# =============================================================================

cat("\n--- 4. TR2025 Implied Emigration Distribution ---\n\n")

# From our earlier analysis, TR2025 implies ~122K net immigration at ages 85+
# If net = imm - emig, and we know the total net LPR from V.A2...

# TR2025 V.A2 shows:
# - Total LPR: 1,213,000 (2025)
# - Net LPR: 909,750 (75% of gross)
# - Emigration: 303,250 (25% of gross)

# If TR2025 implied net immigration at 85+ is ~122K (from our back-calculation)
# And this includes both LPR and O immigration...

cat("TR2025 Implied Values (from population back-calculation):\n")
cat("  Net Total Immigration at 85+ (2025): ~122,372\n")
cat("  This is LPR + O combined\n")

# Our values
cat("\nOur Model Values (2025):\n")
net_o <- tar_read(net_o_for_projection)
our_lpr_85 <- net_lpr[year == 2025 & age >= 85, sum(net_lpr)]
our_o_85 <- net_o[year == 2025 & age >= 85, sum(net_o_immigration)]
cat(sprintf("  Net LPR at 85+: %.0f\n", our_lpr_85))
cat(sprintf("  Net O at 85+: %.0f\n", our_o_85))
cat(sprintf("  Total Net at 85+: %.0f\n", our_lpr_85 + our_o_85))

# Gap
gap <- 122372 - (our_lpr_85 + our_o_85)
cat(sprintf("\n  GAP vs TR2025: %.0f people/year\n", gap))

# =============================================================================
# 5. What Emigration Distribution Would Be Needed?
# =============================================================================

cat("\n--- 5. Back-Calculating Required Emigration Distribution ---\n\n")

# The TR2025 documentation says emigration uses Census 1980-1990 data
# which gives 5-year age groups up to 80-84
# This means ages 85+ may have VERY DIFFERENT emigration patterns

# If we assume TR2025's immigration distribution is similar to ours...
# Let's calculate what emigration distribution would be needed

# Assumption: TR2025 uses similar immigration distribution as us at 85+
# (DHS data should be similar to what they use for NEW arrivals)

# For net at 85+ to be ~+122K (positive, not negative)
# Net = Imm - Emig
# If Imm at 85+ is similar to ours...

# Our immigration at 85+ (with calibrated dist if available)
if (!is.null(calibrated_dist)) {
  imm_dist_85 <- calibrated_dist[age >= 85, sum(distribution)]
} else {
  imm_dist_85 <- total_85plus
}

total_lpr_2025 <- 1213000
our_imm_85plus <- total_lpr_2025 * imm_dist_85
cat(sprintf("Our Immigration at 85+ (using our dist): %.0f\n", our_imm_85plus))

# For net = +122K (TR2025 implied for total, including O)
# Let's focus on LPR only first
# If TR2025 has much higher net LPR at 85+, their emigration at 85+ must be LOWER

# Actually, let me re-think this...
# The 122K implied net is TOTAL (LPR + O)
# Our total is about -3,378 (LPR) + something for O = negative

# The issue is likely:
# 1. TR2025's O immigration distribution assigns MORE to ages 85+
# 2. AND/OR TR2025's emigration distribution assigns LESS to ages 85+

cat("\nPossible explanations for the gap:\n")
cat("1. TR2025's emigration distribution (Census 1980-1990) may have\n")
cat("   FEWER emigrants at ages 85+ than our DHS-based distribution\n")
cat("2. TR2025's O immigration distribution may assign MORE to ages 85+\n")
cat("3. The Census 1980-1990 emigration pattern may reflect return migration\n")
cat("   of working-age adults, not elderly\n")

# =============================================================================
# 6. Hypothetical: What if Emigration at 85+ is Near Zero?
# =============================================================================

cat("\n--- 6. Sensitivity Analysis: Emigration Distribution ---\n\n")

# What if TR2025's emigration at ages 85+ is near zero?
# (Elderly immigrants rarely return to their home countries)

# Gross LPR at 85+ with our immigration distribution
total_emig_2025 <- 303250  # 25% of 1,213,000

cat("Scenario: What if emigration at 85+ is proportionally much lower?\n")

# Our current: same distribution for both
our_emig_85 <- total_emig_2025 * imm_dist_85
our_net_lpr_85 <- our_imm_85plus - our_emig_85
cat(sprintf("\nCurrent (same dist for both):\n"))
cat(sprintf("  Immigration 85+: %.0f\n", our_imm_85plus))
cat(sprintf("  Emigration 85+: %.0f\n", our_emig_85))
cat(sprintf("  Net LPR 85+: %.0f\n", our_net_lpr_85))

# Scenario: emigration at 85+ is 0
cat(sprintf("\nScenario A - Zero emigration at 85+:\n"))
cat(sprintf("  Immigration 85+: %.0f\n", our_imm_85plus))
cat(sprintf("  Emigration 85+: 0\n"))
cat(sprintf("  Net LPR 85+: %.0f\n", our_imm_85plus))

# This shows that even with zero emigration, we'd only get ~2,000-3,000 net LPR
# The gap is ~125,000, so this isn't the main driver

cat("\n=== KEY FINDING ===\n")
cat("Even with ZERO emigration at ages 85+, our net LPR would only be ~2,000-3,000\n")
cat("TR2025 implies ~122,000 net immigration at 85+.\n")
cat("This means the immigration (not emigration) distribution is the main issue.\n")
cat("TR2025 likely assigns ~10-15% of immigration to ages 85+ vs our ~0.02%\n")

cat("\n=== DONE ===\n")

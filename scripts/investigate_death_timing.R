#!/usr/bin/env Rscript
#' Investigate Death Timing Methodology
#'
#' Check if differences in how/when deaths are applied could explain divergence

library(data.table)
library(targets)

cat("=================================================================\n")
cat("DEATH TIMING METHODOLOGY INVESTIGATION\n")
cat("=================================================================\n\n")

# Load data
tr_pop <- fread("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")
tr_qx_male <- fread("data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv", skip = 1)
tr_qx_female <- fread("data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv", skip = 1)

mortality_qx <- tar_read(mortality_qx_for_projection)
pop_proj <- tar_read(projected_population)

# Reshape TR qx
tr_qx_m <- melt(tr_qx_male, id.vars = "Year", variable.name = "age_chr", value.name = "qx")
tr_qx_m[, age := as.integer(as.character(age_chr))]
tr_qx_m[, sex := "male"]
tr_qx_f <- melt(tr_qx_female, id.vars = "Year", variable.name = "age_chr", value.name = "qx")
tr_qx_f[, age := as.integer(as.character(age_chr))]
tr_qx_f[, sex := "female"]
tr_qx <- rbindlist(list(tr_qx_m, tr_qx_f))[, .(year = Year, age, sex, qx)]

# =============================================================================
# 1. Check Our Death Calculation
# =============================================================================

cat("--- 1. Our Death Calculation Methodology ---\n\n")

cat("Our model applies deaths as:\n")
cat("  D_{x}^z = qx_{x}^z * P_{x-1}^{z-1}\n")
cat("  (deaths at age x = qx at age x applied to population aged from x-1)\n\n")

cat("This is the standard cohort-component method where:\n")
cat("  - P_{x}^{z} = P_{x-1}^{z-1} * (1 - qx_{x}) + NI_{x}^{z}\n\n")

# =============================================================================
# 2. Verify Our Implementation Matches This Formula
# =============================================================================

cat("--- 2. Verify Our Implementation ---\n\n")

# Get our population for 2022 and 2023
our_2022 <- pop_proj[year == 2022, .(pop_2022 = sum(population)), by = .(age, sex)]
our_2023 <- pop_proj[year == 2023, .(pop_2023 = sum(population)), by = .(age, sex)]
our_qx_2023 <- mortality_qx[year == 2023, .(age, sex, qx)]

# Merge
verify <- merge(our_2022, our_2023, by = c("age", "sex"), all = TRUE)
verify <- merge(verify, our_qx_2023, by = c("age", "sex"), all.x = TRUE)

# For ages 1-99, calculate what pop_2023 should be (ignoring immigration)
verify[, prev_age_pop := shift(pop_2022, 1), by = sex]  # P_{x-1}^{z-1}
verify[, expected_survivors := prev_age_pop * (1 - qx)]
verify[, implied_immigration := pop_2023 - expected_survivors]

cat("Sample verification for ages 84-90, males:\n")
print(verify[sex == "male" & age >= 84 & age <= 90,
             .(age, pop_2022, pop_2023, qx = round(qx, 5),
               prev_age_pop, expected_surv = round(expected_survivors),
               implied_imm = round(implied_immigration))])

# =============================================================================
# 3. Do Same Calculation for TR2025
# =============================================================================

cat("\n--- 3. TR2025 Population Balance Check ---\n\n")

tr_pop_long <- rbindlist(list(
  tr_pop[, .(year = Year, age = Age, sex = "male", population = `M Tot`)],
  tr_pop[, .(year = Year, age = Age, sex = "female", population = `F Tot`)]
))

tr_2022 <- tr_pop_long[year == 2022, .(age, sex, pop_2022 = population)]
tr_2023 <- tr_pop_long[year == 2023, .(age, sex, pop_2023 = population)]
tr_qx_2023 <- tr_qx[year == 2023, .(age, sex, qx)]

tr_verify <- merge(tr_2022, tr_2023, by = c("age", "sex"), all = TRUE)
tr_verify <- merge(tr_verify, tr_qx_2023, by = c("age", "sex"), all.x = TRUE)

tr_verify[, prev_age_pop := shift(pop_2022, 1), by = sex]
tr_verify[, expected_survivors := prev_age_pop * (1 - qx)]
tr_verify[, implied_immigration := pop_2023 - expected_survivors]

cat("TR2025 balance check for ages 84-90, males:\n")
print(tr_verify[sex == "male" & age >= 84 & age <= 90,
                .(age, pop_2022, pop_2023, qx = round(qx, 5),
                  prev_age_pop, expected_surv = round(expected_survivors),
                  implied_imm = round(implied_immigration))])

# =============================================================================
# 4. Compare Implied Immigration
# =============================================================================

cat("\n--- 4. Implied Immigration Comparison ---\n\n")

# Merge our and TR implied immigration
comp_imm <- merge(
  verify[, .(age, sex, our_implied = implied_immigration)],
  tr_verify[, .(age, sex, tr_implied = implied_immigration)],
  by = c("age", "sex")
)

cat("Implied immigration at ages 85-100 (both sexes combined):\n")
comp_85plus <- comp_imm[age >= 85]
comp_85plus_agg <- comp_85plus[, .(our = sum(our_implied, na.rm = TRUE),
                                    tr = sum(tr_implied, na.rm = TRUE)), by = age]
print(comp_85plus_agg[, .(age, Our = round(our), TR2025 = round(tr), Diff = round(tr - our))])

cat(sprintf("\nTotal implied immigration at 85+ (2023):\n"))
cat(sprintf("  Our model: %.0f\n", comp_85plus[, sum(our_implied, na.rm = TRUE)]))
cat(sprintf("  TR2025: %.0f\n", comp_85plus[, sum(tr_implied, na.rm = TRUE)]))

# =============================================================================
# 5. Check What's Happening at Total Population Level
# =============================================================================

cat("\n--- 5. Total Population Comparison (Ages 85-99) ---\n\n")

# Year 2022 starting point
our_85_2022 <- pop_proj[year == 2022 & age >= 85 & age < 100, sum(population)]
tr_85_2022 <- tr_pop_long[year == 2022 & age >= 85 & age < 100, sum(population)]

# Year 2023
our_85_2023 <- pop_proj[year == 2023 & age >= 85 & age < 100, sum(population)]
tr_85_2023 <- tr_pop_long[year == 2023 & age >= 85 & age < 100, sum(population)]

cat(sprintf("Population 85-99:\n"))
cat(sprintf("  2022: Ours = %s, TR2025 = %s, Diff = %.2f%%\n",
            format(round(our_85_2022), big.mark = ","),
            format(round(tr_85_2022), big.mark = ","),
            (our_85_2022 - tr_85_2022) / tr_85_2022 * 100))
cat(sprintf("  2023: Ours = %s, TR2025 = %s, Diff = %.2f%%\n",
            format(round(our_85_2023), big.mark = ","),
            format(round(tr_85_2023), big.mark = ","),
            (our_85_2023 - tr_85_2023) / tr_85_2023 * 100))

# Change
our_change <- our_85_2023 - our_85_2022
tr_change <- tr_85_2023 - tr_85_2022

cat(sprintf("\n  Change 2022->2023: Ours = %s, TR2025 = %s\n",
            format(round(our_change), big.mark = ","),
            format(round(tr_change), big.mark = ",")))

# =============================================================================
# 6. Detailed Age-by-Age Flow Analysis
# =============================================================================

cat("\n--- 6. Age-by-Age Flow Analysis (2022->2023) ---\n\n")

cat("For a cohort to go from age x-1 (Dec 31, 2022) to age x (Dec 31, 2023):\n")
cat("  Pop_x_2023 = Pop_{x-1}_2022 * (1 - qx_x) + Immigration_x - Emigration_x\n\n")

# Compare flows for specific ages
flow_check <- function(age_target, sex_target) {
  # Our data
  our_pop_prev <- pop_proj[year == 2022 & age == age_target - 1 & sex == sex_target, sum(population)]
  our_pop_now <- pop_proj[year == 2023 & age == age_target & sex == sex_target, sum(population)]
  our_qx <- mortality_qx[year == 2023 & age == age_target & sex == sex_target, qx]

  # TR data
  tr_pop_prev <- tr_pop_long[year == 2022 & age == age_target - 1 & sex == sex_target, population]
  tr_pop_now <- tr_pop_long[year == 2023 & age == age_target & sex == sex_target, population]
  tr_qx_val <- tr_qx[year == 2023 & age == age_target & sex == sex_target, qx]

  our_surv <- our_pop_prev * (1 - our_qx)
  tr_surv <- tr_pop_prev * (1 - tr_qx_val)

  our_implied <- our_pop_now - our_surv
  tr_implied <- tr_pop_now - tr_surv

  data.table(
    age = age_target,
    sex = sex_target,
    our_pop_prev = our_pop_prev,
    tr_pop_prev = tr_pop_prev,
    our_qx = our_qx,
    tr_qx = tr_qx_val,
    our_survivors = our_surv,
    tr_survivors = tr_surv,
    our_pop_now = our_pop_now,
    tr_pop_now = tr_pop_now,
    our_implied_imm = our_implied,
    tr_implied_imm = tr_implied
  )
}

ages_to_check <- c(85, 90, 95)
flows <- rbindlist(lapply(ages_to_check, function(a) {
  rbindlist(list(flow_check(a, "male"), flow_check(a, "female")))
}))

cat("Age-by-Age Flow Analysis:\n")
for (a in ages_to_check) {
  cat(sprintf("\nAge %d:\n", a))
  age_data <- flows[age == a]
  for (s in c("male", "female")) {
    row <- age_data[sex == s]
    cat(sprintf("  %s:\n", s))
    cat(sprintf("    Pop prev (age %d, 2022): Ours=%s, TR=%s\n",
                a-1, format(round(row$our_pop_prev), big.mark=","),
                format(round(row$tr_pop_prev), big.mark=",")))
    cat(sprintf("    qx: Ours=%.6f, TR=%.6f (diff=%.8f)\n",
                row$our_qx, row$tr_qx, row$our_qx - row$tr_qx))
    cat(sprintf("    Survivors: Ours=%s, TR=%s\n",
                format(round(row$our_survivors), big.mark=","),
                format(round(row$tr_survivors), big.mark=",")))
    cat(sprintf("    Pop now (age %d, 2023): Ours=%s, TR=%s\n",
                a, format(round(row$our_pop_now), big.mark=","),
                format(round(row$tr_pop_now), big.mark=",")))
    cat(sprintf("    Implied imm: Ours=%s, TR=%s\n",
                format(round(row$our_implied_imm), big.mark=","),
                format(round(row$tr_implied_imm), big.mark=",")))
  }
}

cat("\n=== DONE ===\n")

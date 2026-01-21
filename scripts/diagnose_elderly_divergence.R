#!/usr/bin/env Rscript
#' Diagnose Population Divergence for Ages 85-99 and 100+
#'
#' Investigates the root causes of over-projection at elderly ages

library(data.table)
library(targets)

cat("=================================================================\n")
cat("DIAGNOSING ELDERLY POPULATION DIVERGENCE (Ages 85+)\n")
cat("=================================================================\n\n")

# =============================================================================
# 1. Load Data
# =============================================================================

cat("--- 1. Loading Data ---\n")

# Load our projections and inputs
pop_proj <- tar_read(projected_population)
starting_pop <- tar_read(starting_population)
mortality_qx <- tar_read(mortality_qx_for_projection)
net_lpr <- tar_read(net_lpr_immigration)
net_o <- tar_read(net_o_for_projection)

# Load TR2025 population
tr_pop <- fread("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")
tr_pop_long <- rbindlist(list(
  tr_pop[, .(year = Year, age = Age, sex = "male", population = `M Tot`)],
  tr_pop[, .(year = Year, age = Age, sex = "female", population = `F Tot`)]
))
setkey(tr_pop_long, year, age, sex)

# Load TR2025 death probabilities for comparison
# Skip the header description row
tr_qx_male <- fread("data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv", skip = 1)
tr_qx_female <- fread("data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv", skip = 1)

cat("Data loaded successfully.\n")

# =============================================================================
# 2. Compare Starting Population (Dec 31, 2022)
# =============================================================================

cat("\n--- 2. Starting Population Comparison (Dec 31, 2022) ---\n")

start_pop_dt <- as.data.table(starting_pop$population)
start_pop_elderly <- start_pop_dt[age >= 85, .(ours = sum(population)), by = .(age, sex)]

tr_start <- tr_pop_long[year == 2022 & age >= 85]
setnames(tr_start, "population", "tr2025")
tr_start[, year := NULL]

comp_start <- merge(start_pop_elderly, tr_start, by = c("age", "sex"))
comp_start[, diff_pct := round((ours - tr2025) / tr2025 * 100, 2)]

cat("\nStarting Population at Ages 85+ (Dec 31, 2022):\n")
comp_start_wide <- dcast(comp_start, age ~ sex, value.var = c("ours", "tr2025", "diff_pct"))
print(comp_start_wide[age %in% c(85, 90, 95, 99, 100)], row.names = FALSE)

cat("\nAge Group Totals:\n")
cat(sprintf("  85-99: Ours = %s, TR2025 = %s, Diff = %.2f%%\n",
            format(comp_start[age >= 85 & age < 100, sum(ours)], big.mark = ","),
            format(comp_start[age >= 85 & age < 100, sum(tr2025)], big.mark = ","),
            comp_start[age >= 85 & age < 100, (sum(ours) - sum(tr2025)) / sum(tr2025) * 100]))
cat(sprintf("  100+:  Ours = %s, TR2025 = %s, Diff = %.2f%%\n",
            format(comp_start[age >= 100, sum(ours)], big.mark = ","),
            format(comp_start[age >= 100, sum(tr2025)], big.mark = ","),
            comp_start[age >= 100, (sum(ours) - sum(tr2025)) / sum(tr2025) * 100]))

# =============================================================================
# 3. Compare Death Probabilities (qx) for ages 85-100
# =============================================================================

cat("\n--- 3. Death Probability (qx) Comparison ---\n")

# Our qx
our_qx <- mortality_qx[age >= 85 & year %in% c(2023, 2030, 2050)]
our_qx <- our_qx[, .(year, age, sex, qx_ours = qx)]

# TR2025 qx - reshape (columns are ages 0-119, rows are years)
tr_qx_m <- melt(tr_qx_male, id.vars = "Year", variable.name = "age_chr", value.name = "qx")
tr_qx_m[, age := as.integer(as.character(age_chr))]
tr_qx_m[, sex := "male"]
tr_qx_m <- tr_qx_m[, .(year = Year, age, sex, qx_tr = qx)]

tr_qx_f <- melt(tr_qx_female, id.vars = "Year", variable.name = "age_chr", value.name = "qx")
tr_qx_f[, age := as.integer(as.character(age_chr))]
tr_qx_f[, sex := "female"]
tr_qx_f <- tr_qx_f[, .(year = Year, age, sex, qx_tr = qx)]

tr_qx_all <- rbindlist(list(tr_qx_m, tr_qx_f))
tr_qx_elderly <- tr_qx_all[age >= 85 & year %in% c(2023, 2030, 2050)]

# Merge for comparison
qx_comp <- merge(our_qx, tr_qx_elderly, by = c("year", "age", "sex"))
qx_comp[, diff_pct := round((qx_ours - qx_tr) / qx_tr * 100, 4)]

cat("\nDeath Probability (qx) at Key Ages (years 2023, 2030, 2050):\n")
cat("Showing ages 85, 90, 95, 100 for males:\n")
print(qx_comp[sex == "male" & age %in% c(85, 90, 95, 100),
              .(year, age, qx_ours = round(qx_ours, 6), qx_tr = round(qx_tr, 6), diff_pct)],
      row.names = FALSE)

cat("\nMax qx difference at ages 85+:\n")
print(qx_comp[, .(max_diff_pct = max(abs(diff_pct))), by = .(year, sex)], row.names = FALSE)

# =============================================================================
# 4. Compare Immigration at Ages 85+
# =============================================================================

cat("\n--- 4. Immigration at Ages 85+ ---\n")

# Net LPR at ages 85+
lpr_cols <- names(net_lpr)
lpr_val_col <- intersect(c("net_lpr_immigration", "net_lpr", "value"), lpr_cols)[1]
lpr_elderly <- net_lpr[age >= 85 & year %in% c(2025, 2030, 2050)]
lpr_summary <- lpr_elderly[, .(net_lpr = sum(get(lpr_val_col))), by = .(year, sex)]

# Net O at ages 85+
o_cols <- names(net_o)
o_val_col <- intersect(c("net_o_immigration", "net_o", "value"), o_cols)[1]
net_o_elderly <- net_o[age >= 85 & year %in% c(2025, 2030, 2050)]
o_summary <- net_o_elderly[, .(net_o = sum(get(o_val_col))), by = .(year, sex)]

imm_summary <- merge(lpr_summary, o_summary, by = c("year", "sex"), all = TRUE)
imm_summary[is.na(net_lpr), net_lpr := 0]
imm_summary[is.na(net_o), net_o := 0]
imm_summary[, total := net_lpr + net_o]

cat("\nNet Immigration at Ages 85+ by Year:\n")
print(imm_summary, row.names = FALSE)

# =============================================================================
# 5. Year-by-Year Population Accumulation Analysis
# =============================================================================

cat("\n--- 5. Year-by-Year Accumulation (Ages 85-99) ---\n")

# Our projection
our_8599 <- pop_proj[age >= 85 & age < 100, .(ours = sum(population)), by = year]

# TR2025
tr_8599 <- tr_pop_long[age >= 85 & age < 100, .(tr2025 = sum(population)), by = year]

comp_8599 <- merge(our_8599, tr_8599, by = "year")
comp_8599[, diff := ours - tr2025]
comp_8599[, diff_pct := round((ours - tr2025) / tr2025 * 100, 2)]
comp_8599[, yoy_diff_growth := c(NA, diff(diff))]

cat("\nPopulation 85-99 Comparison Over Time:\n")
print(comp_8599[year %in% seq(2022, 2099, 5)], row.names = FALSE)

cat("\nCumulative excess at 85-99:\n")
cat(sprintf("  2030: +%s people (%.1f%%)\n",
            format(comp_8599[year == 2030, diff], big.mark = ","),
            comp_8599[year == 2030, diff_pct]))
cat(sprintf("  2050: +%s people (%.1f%%)\n",
            format(comp_8599[year == 2050, diff], big.mark = ","),
            comp_8599[year == 2050, diff_pct]))
cat(sprintf("  2099: +%s people (%.1f%%)\n",
            format(comp_8599[year == 2099, diff], big.mark = ","),
            comp_8599[year == 2099, diff_pct]))

# =============================================================================
# 6. Analyze 100+ Population Dynamics
# =============================================================================

cat("\n--- 6. Age 100+ Population Analysis ---\n")

our_100 <- pop_proj[age >= 100, .(ours = sum(population)), by = year]
tr_100 <- tr_pop_long[age >= 100, .(tr2025 = sum(population)), by = year]

comp_100 <- merge(our_100, tr_100, by = "year")
comp_100[, diff_pct := round((ours - tr2025) / tr2025 * 100, 2)]

cat("\nAge 100+ Population (thousands):\n")
print(comp_100[year %in% c(2022, 2025, 2030, 2040, 2050, 2070, 2099),
               .(year, ours_k = round(ours/1000, 1), tr_k = round(tr2025/1000, 1), diff_pct)],
      row.names = FALSE)

# =============================================================================
# 7. Trace Single-Age Population Flow
# =============================================================================

cat("\n--- 7. Tracing Population Flow (Age 84 -> 85 -> 86...) ---\n")

# Trace the 2022 cohort that was age 84
cat("\nCohort born ~1938 (age 84 in 2022):\n")
cat("Tracing through projection...\n\n")

# Our model
our_cohort <- pop_proj[year - age == 2022 - 84 & year >= 2022 & year <= 2030 & age >= 84]
our_cohort_agg <- our_cohort[, .(ours = sum(population)), by = .(year, age)]

# TR2025
tr_cohort <- tr_pop_long[year - age == 2022 - 84 & year >= 2022 & year <= 2030 & age >= 84]
tr_cohort_agg <- tr_cohort[, .(tr2025 = sum(population)), by = .(year, age)]

cohort_comp <- merge(our_cohort_agg, tr_cohort_agg, by = c("year", "age"))
cohort_comp[, diff_pct := round((ours - tr2025) / tr2025 * 100, 2)]
cohort_comp[, our_surv := c(NA, ours[-1] / ours[-.N] * 100)]
cohort_comp[, tr_surv := c(NA, tr2025[-1] / tr2025[-.N] * 100)]

cat("Cohort tracking (both sexes combined):\n")
print(cohort_comp[, .(year, age, ours = round(ours), tr2025 = round(tr2025),
                      diff_pct, our_surv = round(our_surv, 1), tr_surv = round(tr_surv, 1))],
      row.names = FALSE)

# =============================================================================
# 8. Check qx at age 100 (the 100+ aggregate qx)
# =============================================================================

cat("\n--- 8. Age 100+ Death Probability Analysis ---\n")

# Our qx at age 100
our_q100 <- mortality_qx[age == 100, .(year, sex, qx_ours = qx)]

# TR2025 qx at age 100
tr_q100 <- tr_qx_all[age == 100, .(year, sex, qx_tr = qx)]

q100_comp <- merge(our_q100, tr_q100, by = c("year", "sex"))
q100_comp[, diff_pct := round((qx_ours - qx_tr) / qx_tr * 100, 4)]

cat("\nAge 100 qx Comparison (key years):\n")
print(q100_comp[year %in% c(2023, 2030, 2050, 2070, 2099) & sex == "male",
                .(year, qx_ours = round(qx_ours, 5), qx_tr = round(qx_tr, 5), diff_pct)],
      row.names = FALSE)

# What should qx be for an aggregated 100+ group?
cat("\nNote: Our model uses qx at age 100 for entire 100+ group.\n")
cat("TR2025 provides qx for ages 100-119 separately.\n")

# Calculate what an aggregate 100+ qx would look like
cat("\nTR2025 qx for ages 100-110 (2050, male):\n")
tr_supercentenarian <- tr_qx_all[year == 2050 & sex == "male" & age >= 100 & age <= 110]
print(tr_supercentenarian[, .(age, qx = round(qx_tr, 4))], row.names = FALSE)

# =============================================================================
# 9. Summary and Conclusions
# =============================================================================

cat("\n=================================================================\n")
cat("SUMMARY OF FINDINGS\n")
cat("=================================================================\n")

# Starting population match
start_diff <- comp_start[age >= 85 & age < 100, (sum(ours) - sum(tr2025)) / sum(tr2025) * 100]
cat(sprintf("\n1. Starting Population (85-99): %.2f%% difference\n", start_diff))

# qx match
max_qx_diff <- qx_comp[, max(abs(diff_pct))]
cat(sprintf("2. Death Probabilities (qx): Max %.4f%% difference\n", max_qx_diff))

# End projection difference
end_diff <- comp_8599[year == 2099, diff_pct]
cat(sprintf("3. End Population (85-99, 2099): %.2f%% difference\n", end_diff))

# Accumulation pattern
cat("\n4. Accumulation Pattern:\n")
cat("   If starting pop matches and qx matches, but end pop differs,\n")
cat("   the issue is likely:\n")
cat("   - Immigration distribution at ages 85+\n")
cat("   - Boundary handling at age 100\n")
cat("   - Rounding/aggregation in year-by-year projection\n")

cat("\n=== DONE ===\n")

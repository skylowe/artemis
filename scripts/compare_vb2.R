#!/usr/bin/env Rscript
# ARTEMIS vs TR2025 V.B2 Employment Comparison
library(data.table)

# --- Load ARTEMIS data ---
lfpr_proj <- targets::tar_read(lfpr_projection)
unemp_proj <- targets::tar_read(unemployment_projection)
emp_inputs <- targets::tar_read(employment_inputs)
lf_emp <- targets::tar_read(labor_force_employment)
cni <- emp_inputs$quarterly_cni_pop

# --- Load TR data ---
tr <- targets::tar_read(tr_economic_assumptions)

# --- Derive base year from config (resolves null → TR_year - 1) ---
config <- yaml::read_yaml(Sys.getenv("ARTEMIS_CONFIG",
  unset = "config/assumptions/tr2025.yaml"))
base_year <- config$economics$employment$base_year
if (is.null(base_year)) {
  base_year <- config$metadata$trustees_report_year - 1L
}
cps <- targets::tar_read(cps_labor_force)
base_lf <- cps[year == base_year & concept == "labor_force" &
                  marital_status == "all" & is.na(age),
                .(age_group, sex, base_labor_force = value)]

# === 1. Compute ARTEMIS aggregate labor force (annual average) ===

lfpr_agg <- lfpr_proj$aggregate

# Map single-year ages to LFPR age groups
age_map <- data.table(age = 0:100)
age_map[, age_group := fcase(
  age < 16, "under16",
  age <= 17, "16-17",
  age <= 19, "18-19",
  age <= 24, "20-24",
  age <= 29, "25-29",
  age <= 34, "30-34",
  age <= 39, "35-39",
  age <= 44, "40-44",
  age <= 49, "45-49",
  age <= 54, "50-54",
  age <= 79, as.character(age),
  age <= 100, "80+"
)]

cni_grp <- merge(cni, age_map, by = "age")
cni_grp <- cni_grp[age_group != "under16"]

# Sum ages within group per quarter, then average across quarters for annual
pop_quarterly <- cni_grp[, .(population = sum(population)), by = .(year, quarter, age_group, sex)]
pop_annual <- pop_quarterly[, .(population = mean(population)), by = .(year, age_group, sex)]

# Merge with LFPR
lf_detail <- merge(pop_annual, lfpr_agg, by = c("year", "age_group", "sex"))
lf_detail[, labor_force := population * lfpr]

# Aggregate
artemis_lf <- lf_detail[, .(
  labor_force = sum(labor_force),
  cni_pop_16plus = sum(population),
  agg_lfpr = sum(labor_force) / sum(population)
), by = year]
artemis_lf[, lf_change_pct := (labor_force / shift(labor_force) - 1) * 100]

# === 2. Compute ARTEMIS aggregate unemployment rate ===

# 2a. Current-year labor force weights (standard approach)
# LF uses single-year ages for 55+; map to UR 5-year groups before merging
unemp_actual <- unemp_proj$actual
lf_for_ur <- data.table::copy(lf_detail[, .(year, age_group, sex, labor_force)])
lf_for_ur[, ur_age_group := age_group]
lf_for_ur[age_group %in% as.character(55:59), ur_age_group := "55-59"]
lf_for_ur[age_group %in% as.character(60:64), ur_age_group := "60-64"]
lf_for_ur[age_group %in% as.character(65:69), ur_age_group := "65-69"]
lf_for_ur[age_group %in% as.character(70:74), ur_age_group := "70-74"]
lf_for_ur[age_group %in% c(as.character(75:100), "80+"), ur_age_group := "75+"]
lf_by_ur_group <- lf_for_ur[, .(labor_force = sum(labor_force)),
                              by = .(year, ur_age_group, sex)]
unemp_lf <- merge(unemp_actual, lf_by_ur_group,
                   by.x = c("year", "age_group", "sex"),
                   by.y = c("year", "ur_age_group", "sex"), all.x = TRUE)
artemis_ur <- unemp_lf[!is.na(labor_force), .(
  ur_current_wt = weighted.mean(rate, labor_force, na.rm = TRUE)
), by = year]

# 2b. Base-year labor force weights (TR2025 methodology for aggregate constraint)
# Map single-year LFPR ages to UR 5-year groups for matching
ur_age_map <- data.table(
  age_group = c("16-17", "18-19", "20-24", "25-29", "30-34", "35-39",
                "40-44", "45-49", "50-54",
                as.character(55:79), "80+"),
  ur_age_group = c("16-17", "18-19", "20-24", "25-29", "30-34", "35-39",
                   "40-44", "45-49", "50-54",
                   rep("55-59", 5), rep("60-64", 5), rep("65-69", 5),
                   rep("70-74", 5), rep("75+", 5), "75+")
)
unemp_base <- merge(unemp_actual, ur_age_map, by = "age_group", all.x = TRUE)
unemp_base[is.na(ur_age_group), ur_age_group := age_group]
unemp_base <- merge(unemp_base, base_lf,
                     by.x = c("ur_age_group", "sex"),
                     by.y = c("age_group", "sex"),
                     all.x = TRUE)
artemis_ur_base <- unemp_base[!is.na(base_labor_force), .(
  ur_base_wt = weighted.mean(rate, base_labor_force, na.rm = TRUE)
), by = year]

artemis_ur <- merge(artemis_ur, artemis_ur_base, by = "year", all.x = TRUE)

# === 3. Check employment for NAs ===

emp <- lf_emp$employment
emp_na <- emp[is.na(employment)]
if (nrow(emp_na) > 0) {
  cat(sprintf("\n  WARNING: %d employment rows with NA values\n", nrow(emp_na)))
  print(unique(emp_na[, .(age_group, sex)]))
} else {
  cat("\n  Employment: no NA values (all age groups mapped correctly)\n")
}

# === 4. TR2025 V.B2 values ===

tr_ur <- tr[variable == "unemployment_rate", .(year, tr_unemployment_rate = value)]
tr_lf <- tr[variable == "labor_force_change", .(year, tr_lf_change_pct = value)]

# === 5. Build comparison ===

comp <- merge(artemis_lf, artemis_ur, by = "year", all.x = TRUE)
comp <- merge(comp, tr_ur, by = "year", all.x = TRUE)
comp <- merge(comp, tr_lf, by = "year", all.x = TRUE)
comp[, labor_force_M := labor_force / 1e6]
comp[, cni_pop_M := cni_pop_16plus / 1e6]

# === PRINT ===

cat("\n")
cat("================================================================================\n")
cat("  ARTEMIS vs TR2025 V.B2 - US Employment Comparison\n")
cat("================================================================================\n\n")

# --- Near-term ---
cat("-- Near-Term (2025-2035) -------------------------------------------------------\n\n")
cat(sprintf("  %-6s | %8s %8s %8s | %8s %8s %8s | %8s %8s\n",
    "Year", "CNI(M)", "LF(M)", "LFPR%",
    "UR(cur)", "UR(base)", "V.B2 UR",
    "LF chg%", "V.B2 LF%"))
cat("  -------|--------------------------|--------------------------|-------------------\n")

for (y in 2025:2035) {
  r <- comp[year == y]
  if (nrow(r) == 0) next
  cat(sprintf("  %-6d | %8.1f %8.1f %8.1f | %8.1f %8.1f %8.1f | %8.2f %8.1f\n",
      y, r$cni_pop_M, r$labor_force_M, r$agg_lfpr * 100,
      ifelse(is.na(r$ur_current_wt), NA, r$ur_current_wt),
      ifelse(is.na(r$ur_base_wt), NA, r$ur_base_wt),
      ifelse(is.na(r$tr_unemployment_rate), NA, r$tr_unemployment_rate),
      ifelse(is.na(r$lf_change_pct), NA, r$lf_change_pct),
      ifelse(is.na(r$tr_lf_change_pct), NA, r$tr_lf_change_pct)))
}

# --- Long-term ---
cat("\n-- Long-Term (every 10 years) --------------------------------------------------\n\n")
cat(sprintf("  %-6s | %8s %8s %8s | %8s %8s %8s | %8s %8s\n",
    "Year", "CNI(M)", "LF(M)", "LFPR%",
    "UR(cur)", "UR(base)", "V.B2 UR",
    "LF chg%", "V.B2 LF%"))
cat("  -------|--------------------------|--------------------------|-------------------\n")

for (y in seq(2040, 2100, by = 10)) {
  r <- comp[year == y]
  if (nrow(r) == 0) next
  cat(sprintf("  %-6d | %8.1f %8.1f %8.1f | %8.1f %8.1f %8.1f | %8.2f %8.1f\n",
      y, r$cni_pop_M, r$labor_force_M, r$agg_lfpr * 100,
      ifelse(is.na(r$ur_current_wt), NA, r$ur_current_wt),
      ifelse(is.na(r$ur_base_wt), NA, r$ur_base_wt),
      ifelse(is.na(r$tr_unemployment_rate), NA, r$tr_unemployment_rate),
      ifelse(is.na(r$lf_change_pct), NA, r$lf_change_pct),
      ifelse(is.na(r$tr_lf_change_pct), NA, r$tr_lf_change_pct)))
}

# --- LFPR by age group ---
cat("\n-- LFPR by Age Group (2025) ----------------------------------------------------\n\n")
cat(sprintf("  %-10s | %10s %12s | %12s %12s\n",
    "Age Group", "Male LFPR%", "Female LFPR%", "Male Pop(K)", "Female Pop(K)"))
cat("  -----------|-------------------------|---------------------------\n")

detail_2025 <- lf_detail[year == 2025]
for (ag in c("16-17", "18-19", "20-24", "25-29", "30-34", "35-39", "40-44",
             "45-49", "50-54", "55", "60", "62", "65", "67", "70", "75", "80+")) {
  m <- detail_2025[age_group == ag & sex == "male"]
  f <- detail_2025[age_group == ag & sex == "female"]
  if (nrow(m) > 0 && nrow(f) > 0) {
    cat(sprintf("  %-10s | %10.1f %12.1f | %12.0f %12.0f\n",
        ag, m$lfpr * 100, f$lfpr * 100, m$population / 1e3, f$population / 1e3))
  }
}

# --- Reality check ---
cat("\n-- Reality Check ---------------------------------------------------------------\n\n")
cat("  Actual US (BLS 2024): Labor force ~168M, LFPR ~62.6%, UR ~4.0%\n\n")
cat(sprintf("  ARTEMIS 2025: LF = %.1fM, LFPR = %.1f%%, UR(cur) = %.1f%%, UR(base) = %.1f%%\n",
    comp[year == 2025]$labor_force_M,
    comp[year == 2025]$agg_lfpr * 100,
    comp[year == 2025]$ur_current_wt,
    comp[year == 2025]$ur_base_wt))
cat(sprintf("  TR V.B2 2025: UR = %.1f%%, LF change = +%.1f%%\n",
    comp[year == 2025]$tr_unemployment_rate,
    comp[year == 2025]$tr_lf_change_pct))

cat("\n  Note: V.B2 publishes only % changes, not absolute levels.\n")
cat("  If BLS 2024 LF = 168M and V.B2 2025 LF change = +1.3%, implied 2025 LF = 170.2M\n")
cat("\n  UR(cur): aggregate UR weighted by current-year labor force\n")
cat("  UR(base): aggregate UR weighted by base-year labor force (TR2025 methodology)\n")
cat("================================================================================\n")

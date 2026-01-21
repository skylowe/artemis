#!/usr/bin/env Rscript
#' Derive Age-Sex Net Immigration from TR2025 Population and Mortality Data
#' Using December (End of Year) Population - VECTORIZED VERSION
#'
#' Formula: NI(z,x) = P(Dec z, x) - P(Dec z-1, x-1) * (1 - qx)

library(data.table)

cat("=================================================================\n")
cat("Deriving TR2025 Net Immigration by Age and Sex (December Population)\n")
cat("=================================================================\n\n")

# =============================================================================
# 1. Load Data
# =============================================================================

cat("--- 1. Loading Data ---\n")

tr_pop <- fread("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")
qx_m_hist <- fread("data/raw/SSA_TR2025/DeathProbsE_M_Hist_TR2025.csv", skip = 1)
qx_f_hist <- fread("data/raw/SSA_TR2025/DeathProbsE_F_Hist_TR2025.csv", skip = 1)
qx_m_proj <- fread("data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv", skip = 1)
qx_f_proj <- fread("data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv", skip = 1)

cat(sprintf("Population: years %d-%d, ages %d-%d\n",
            min(tr_pop$Year), max(tr_pop$Year), min(tr_pop$Age), max(tr_pop$Age)))

# =============================================================================
# 2. Reshape to Long Format (Vectorized)
# =============================================================================

cat("\n--- 2. Reshaping Data ---\n")

# Combine and reshape qx
qx_m <- rbindlist(list(qx_m_hist, qx_m_proj))[!duplicated(Year, fromLast = TRUE)]
qx_f <- rbindlist(list(qx_f_hist, qx_f_proj))[!duplicated(Year, fromLast = TRUE)]

qx_long <- rbindlist(list(
  melt(qx_m, id.vars = "Year", variable.name = "age", value.name = "qx")[, sex := "male"],
  melt(qx_f, id.vars = "Year", variable.name = "age", value.name = "qx")[, sex := "female"]
))
qx_long[, age := as.integer(as.character(age))]
setnames(qx_long, "Year", "year")
setkey(qx_long, year, age, sex)

# Reshape population to long
pop_long <- rbindlist(list(
  tr_pop[, .(year = Year, age = Age, sex = "male", population = `M Tot`)],
  tr_pop[, .(year = Year, age = Age, sex = "female", population = `F Tot`)]
))
setkey(pop_long, year, age, sex)

cat(sprintf("Population rows: %d, qx rows: %d\n", nrow(pop_long), nrow(qx_long)))

# =============================================================================
# 3. Vectorized NI Derivation for Ages 1-99
# =============================================================================

cat("\n--- 3. Deriving Net Immigration (Vectorized) ---\n")

max_age <- max(pop_long$age)

# Create current year population
pop_curr <- copy(pop_long)
setnames(pop_curr, "population", "p_curr")

# Create previous year population (shifted: year-1, age-1)
pop_prev <- copy(pop_long)
pop_prev[, year := year + 1]
pop_prev[, age := age + 1]
setnames(pop_prev, "population", "p_prev")
setkey(pop_prev, year, age, sex)

# Merge current with previous
ni_data <- merge(pop_curr, pop_prev, by = c("year", "age", "sex"), all.x = TRUE)

# For qx, we need qx(z, x-1)
qx_shifted <- copy(qx_long)
qx_shifted[, age := age + 1]
setkey(qx_shifted, year, age, sex)

# Merge qx
ni_data <- merge(ni_data, qx_shifted, by = c("year", "age", "sex"), all.x = TRUE)

# Calculate survivors and NI for ages 1-99
ni_data[age >= 1 & age < max_age, survivors := p_prev * (1 - qx)]
ni_data[age >= 1 & age < max_age, net_immigration := p_curr - survivors]

cat(sprintf("Calculated NI for %d rows (ages 1-%d)\n",
            nrow(ni_data[age >= 1 & age < max_age & !is.na(net_immigration)]), max_age - 1))

# =============================================================================
# 4. Handle Age 100+ (Open-Ended Group)
# =============================================================================

cat("\n--- 4. Handling Age 100+ ---\n")

pop_99_prev <- pop_long[age == 99, .(year = year + 1, sex, p_99_prev = population)]
pop_100_prev <- pop_long[age == 100, .(year = year + 1, sex, p_100_prev = population)]
qx_99 <- qx_long[age == 99, .(year, sex, q99 = qx)]
qx_100 <- qx_long[age == 100, .(year, sex, q100 = qx)]

age100_data <- ni_data[age == 100, .(year, sex, p_curr)]
age100_data <- merge(age100_data, pop_99_prev, by = c("year", "sex"), all.x = TRUE)
age100_data <- merge(age100_data, pop_100_prev, by = c("year", "sex"), all.x = TRUE)
age100_data <- merge(age100_data, qx_99, by = c("year", "sex"), all.x = TRUE)
age100_data <- merge(age100_data, qx_100, by = c("year", "sex"), all.x = TRUE)

age100_data[, surv_from_99 := p_99_prev * (1 - q99)]
age100_data[, surv_from_100 := p_100_prev * (1 - q100)]
age100_data[, survivors_100 := surv_from_99 + surv_from_100]
age100_data[, ni_100 := p_curr - survivors_100]

ni_data <- merge(ni_data, age100_data[, .(year, sex, survivors_100, ni_100)],
                  by = c("year", "sex"), all.x = TRUE)
ni_data[age == 100, survivors := survivors_100]
ni_data[age == 100, net_immigration := ni_100]
ni_data[, c("survivors_100", "ni_100") := NULL]

cat(sprintf("Updated %d rows for age 100+\n", nrow(ni_data[age == 100 & !is.na(net_immigration)])))

# =============================================================================
# 5. Handle Age 0 (Estimate Proportionally)
# =============================================================================

cat("\n--- 5. Estimating Age 0 ---\n")

ni_1_plus <- ni_data[age >= 1, .(ni_ages_1_plus = sum(net_immigration, na.rm = TRUE)), by = year]

age0_share <- ni_data[, .(
  age0_pop = sum(p_curr[age == 0], na.rm = TRUE),
  total_pop = sum(p_curr, na.rm = TRUE)
), by = year]
age0_share[, share := age0_pop / total_pop]

ni_1_plus <- merge(ni_1_plus, age0_share[, .(year, share)], by = "year")
ni_1_plus[, est_age0_ni := ni_ages_1_plus * share / (1 - share)]

ni_data <- merge(ni_data, ni_1_plus[, .(year, est_age0_ni)], by = "year", all.x = TRUE)
ni_data[age == 0, net_immigration := est_age0_ni]
ni_data[age == 0, survivors := NA_real_]
ni_data[, est_age0_ni := NULL]

cat("Done.\n")

# =============================================================================
# 6. Calculate Distributions
# =============================================================================

cat("\n--- 6. Calculating Distributions ---\n")

total_ni <- ni_data[, .(total_ni = sum(net_immigration, na.rm = TRUE)), by = year]
ni_data <- merge(ni_data, total_ni, by = "year")
ni_data[, distribution := net_immigration / total_ni]

ni_data <- ni_data[!is.na(year) & !is.na(age)]
setkey(ni_data, year, age, sex)

cat(sprintf("Final dataset: %d rows\n", nrow(ni_data)))

# =============================================================================
# 7. Summary Statistics
# =============================================================================

cat("\n--- 7. Summary Statistics ---\n")

cat("\nTotal Net Immigration by Year:\n")
print(total_ni[year %in% c(1941, 1950, 1970, 1990, 2000, 2010, 2020, 2023, 2030, 2050, 2099),
                .(year, total_ni = format(round(total_ni), big.mark = ","))])

cat("\n\nNet Immigration by Age Group (2030):\n")
ni_2030 <- ni_data[year == 2030]
ni_2030[, age_group := cut(age, breaks = c(-1, 0, 18, 45, 65, 85, Inf),
                            labels = c("0", "1-17", "18-44", "45-64", "65-84", "85+"))]
by_group <- ni_2030[, .(ni = sum(net_immigration, na.rm = TRUE),
                         dist = sum(distribution, na.rm = TRUE)), by = age_group]
print(by_group[, .(age_group, ni = format(round(ni), big.mark = ","),
                   dist_pct = sprintf("%.1f%%", dist * 100))])

cat("\n\nNet Immigration by Sex (2030):\n")
by_sex <- ni_2030[, .(ni = sum(net_immigration, na.rm = TRUE)), by = sex]
by_sex[, pct := ni / sum(ni) * 100]
print(by_sex[, .(sex, ni = format(round(ni), big.mark = ","), pct = sprintf("%.1f%%", pct))])

# =============================================================================
# 8. Check Negative Values
# =============================================================================

cat("\n--- 8. Negative Values Analysis ---\n")

negative_ni <- ni_data[net_immigration < 0]
cat(sprintf("Cells with negative NI: %d out of %d (%.2f%%)\n",
            nrow(negative_ni), nrow(ni_data), nrow(negative_ni) / nrow(ni_data) * 100))

cat("\nNegative NI by age group:\n")
negative_ni[, age_group := cut(age, breaks = c(-1, 18, 45, 65, 85, Inf),
                                labels = c("0-17", "18-44", "45-64", "65-84", "85+"))]
print(negative_ni[, .N, by = age_group])

# =============================================================================
# 9. Save Results
# =============================================================================

cat("\n--- 9. Saving Results ---\n")

output <- ni_data[, .(year, age, sex, population = p_curr, survivors, net_immigration, distribution)]
fwrite(output, "data/processed/tr2025_derived_immigration.csv")
cat("Saved to: data/processed/tr2025_derived_immigration.csv\n")

# Save summary
summary_by_year <- ni_data[, .(
  total_ni = sum(net_immigration, na.rm = TRUE),
  male_ni = sum(net_immigration[sex == "male"], na.rm = TRUE),
  female_ni = sum(net_immigration[sex == "female"], na.rm = TRUE),
  negative_cells = sum(net_immigration < 0, na.rm = TRUE)
), by = year]
fwrite(summary_by_year, "data/processed/tr2025_derived_immigration_summary.csv")
cat("Saved summary to: data/processed/tr2025_derived_immigration_summary.csv\n")

cat("\n=== DONE ===\n")

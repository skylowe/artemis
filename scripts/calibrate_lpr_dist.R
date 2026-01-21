#!/usr/bin/env Rscript
#' Calibrate LPR Distribution to Match TR2025 Population Projection
#' FAST VERSION - Optimizes by age group (14 params) instead of single ages (202 params)
#'
#' Finds scaling factors for each age group that minimize population error vs TR2025

library(data.table)
library(targets)

cat("=================================================================\n")
cat("Calibrating LPR Distribution to Match TR2025 Population\n")
cat("=================================================================\n\n")

# =============================================================================
# 1. Load Data
# =============================================================================

cat("--- 1. Loading Data ---\n")

# Load TR2025 target population
tr_pop <- fread("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")
tr_pop_long <- rbindlist(list(
  tr_pop[, .(year = Year, age = Age, sex = "male", population = `M Tot`)],
  tr_pop[, .(year = Year, age = Age, sex = "female", population = `F Tot`)]
))
tr_pop_long[, sex := as.character(sex)]
setkey(tr_pop_long, year, age, sex)

# Load model outputs
tar_load(net_o_for_projection)
tar_load(lpr_assumptions)
tar_load(mortality_qx_projected)
tar_load(starting_population)
tar_load(lpr_distribution)

proj_years <- 2023:2050
cat(sprintf("Calibration years: %d-%d\n", min(proj_years), max(proj_years)))

# =============================================================================
# 2. Prepare Fixed Components
# =============================================================================

cat("\n--- 2. Preparing Fixed Components ---\n")

start_pop <- as.data.table(starting_population$population)
start_pop <- start_pop[, .(population = sum(population)), by = .(year, age, sex)]
start_pop[, sex := as.character(sex)]
setkey(start_pop, year, age, sex)

net_o <- as.data.table(net_o_for_projection)
net_o <- net_o[year %in% proj_years, .(net_o = sum(net_o_immigration)), by = .(year, age, sex)]
setkey(net_o, year, age, sex)

qx <- as.data.table(mortality_qx_projected)
qx[, sex := as.character(sex)]
qx <- qx[year %in% proj_years]
setkey(qx, year, age, sex)

lpr_totals <- as.data.table(lpr_assumptions)[year %in% proj_years, .(year, net_lpr_total)]
setkey(lpr_totals, year)

tr_target <- tr_pop_long[year %in% proj_years]
setkey(tr_target, year, age, sex)

dhs_dist <- as.data.table(lpr_distribution)
dhs_dist[, sex := as.character(sex)]
setorder(dhs_dist, sex, age)

cat(sprintf("Starting pop: %.2fM\n", sum(start_pop$population) / 1e6))

# =============================================================================
# 3. Define Age Groups
# =============================================================================

cat("\n--- 3. Setting Up Age Group Optimization ---\n")

# Define age groups
age_groups <- list(
  "0-17" = 0:17,
  "18-24" = 18:24,
  "25-44" = 25:44,
  "45-64" = 45:64,
  "65-84" = 65:84,
  "85-99" = 85:99,
  "100+" = 100
)

# Add age group to DHS distribution
dhs_dist[, age_group := fcase(
  age <= 17, "0-17",
  age <= 24, "18-24",
  age <= 44, "25-44",
  age <= 64, "45-64",
  age <= 84, "65-84",
  age <= 99, "85-99",
  default = "100+"
)]

cat(sprintf("Optimizing %d parameters (7 age groups × 2 sexes)\n", 14))

# =============================================================================
# 4. Vectorized Projection Function
# =============================================================================

#' Apply scaling factors to distribution and project
project_with_scales <- function(scale_vector) {
  scale_dt <- data.table(
    sex = rep(c("male", "female"), each = 7),
    age_group = rep(names(age_groups), 2),
    scale = pmax(0.01, scale_vector)
  )

  lpr_dist <- copy(dhs_dist)
  lpr_dist <- merge(lpr_dist, scale_dt, by = c("sex", "age_group"), all.x = TRUE)
  lpr_dist[, distribution := distribution * scale]
  lpr_dist[, distribution := distribution / sum(distribution)]
  setkey(lpr_dist, age, sex)

  results <- vector("list", length(proj_years) + 1)
  results[[1]] <- copy(start_pop)

  for (i in seq_along(proj_years)) {
    yr <- proj_years[i]
    prev_pop <- results[[i]]

    new_pop <- CJ(year = yr, age = 0:100, sex = c("male", "female"))

    lpr_total <- lpr_totals[year == yr, net_lpr_total]
    net_lpr <- lpr_dist[, .(age, sex, net_lpr = distribution * lpr_total)]

    net_o_yr <- net_o[year == yr, .(age, sex, net_o)]
    qx_yr <- qx[year == yr, .(age, sex, qx)]

    prev_shifted <- copy(prev_pop)
    prev_shifted[, age := age + 1]
    prev_shifted[, year := yr]
    setnames(prev_shifted, "population", "prev_pop")

    qx_shifted <- copy(qx_yr)
    qx_shifted[, age := age + 1]

    new_pop <- merge(new_pop, prev_shifted[, .(year, age, sex, prev_pop)],
                     by = c("year", "age", "sex"), all.x = TRUE)
    new_pop <- merge(new_pop, qx_shifted[, .(age, sex, qx)],
                     by = c("age", "sex"), all.x = TRUE)
    new_pop <- merge(new_pop, net_lpr, by = c("age", "sex"), all.x = TRUE)
    new_pop <- merge(new_pop, net_o_yr, by = c("age", "sex"), all.x = TRUE)

    new_pop[is.na(prev_pop), prev_pop := 0]
    new_pop[is.na(qx), qx := 0]
    new_pop[is.na(net_lpr), net_lpr := 0]
    new_pop[is.na(net_o), net_o := 0]

    new_pop[age >= 1 & age <= 99,
            population := prev_pop * (1 - qx) + net_lpr + net_o]

    age0_tr <- tr_target[year == yr & age == 0, .(sex, population)]
    new_pop <- merge(new_pop, age0_tr, by = "sex", all.x = TRUE, suffixes = c("", "_tr"))
    new_pop[age == 0, population := population_tr]
    new_pop[, population_tr := NULL]

    prev_99 <- prev_pop[age == 99, .(sex, p99 = population)]
    prev_100 <- prev_pop[age == 100, .(sex, p100 = population)]
    qx_99 <- qx_yr[age == 99, .(sex, q99 = qx)]
    qx_100 <- qx_yr[age == 100, .(sex, q100 = qx)]

    age100_calc <- merge(prev_99, prev_100, by = "sex", all = TRUE)
    age100_calc <- merge(age100_calc, qx_99, by = "sex", all.x = TRUE)
    age100_calc <- merge(age100_calc, qx_100, by = "sex", all.x = TRUE)
    age100_calc[is.na(p99), p99 := 0]
    age100_calc[is.na(p100), p100 := 0]
    age100_calc[is.na(q99), q99 := 0.3]
    age100_calc[is.na(q100), q100 := 0.4]
    age100_calc[, surv_100 := p99 * (1 - q99) + p100 * (1 - q100)]

    new_pop <- merge(new_pop, age100_calc[, .(sex, surv_100)], by = "sex", all.x = TRUE)
    ni_100 <- new_pop[age == 100, .(sex, ni_100 = net_lpr + net_o)]
    new_pop <- merge(new_pop, ni_100, by = "sex", all.x = TRUE)
    new_pop[age == 100, population := surv_100 + ni_100]
    new_pop[, c("surv_100", "ni_100") := NULL]

    new_pop <- new_pop[, .(year, age, sex, population)]
    setkey(new_pop, year, age, sex)
    results[[i + 1]] <- new_pop
  }

  rbindlist(results)
}

#' Calculate weighted error
calc_error <- function(scale_vector) {
  proj <- project_with_scales(scale_vector)
  proj <- proj[year %in% proj_years]

  comp <- merge(proj, tr_target, by = c("year", "age", "sex"),
                suffixes = c("_ours", "_tr"))

  comp[, weight := fifelse(age >= 20 & age <= 64, 2.0,
                   fifelse(age >= 65 & age <= 84, 1.5,
                   fifelse(age == 100, 0.5, 1.0)))]

  comp[, sq_err := weight * ((population_ours - population_tr) / 1e6)^2]

  sum(comp$sq_err, na.rm = TRUE)
}

# =============================================================================
# 5. Test and Optimize
# =============================================================================

cat("\n--- 4. Testing Initial (DHS) ---\n")

init_scales <- rep(1.0, 14)

t1 <- Sys.time()
init_error <- calc_error(init_scales)
t2 <- Sys.time()
cat(sprintf("Initial error: %.4f\n", init_error))
cat(sprintf("Projection time: %.2f seconds\n", as.numeric(t2 - t1, units = "secs")))

cat("\n--- 5. Running Optimization ---\n")

result <- optim(
  par = init_scales,
  fn = calc_error,
  method = "L-BFGS-B",
  lower = rep(0.1, 14),
  upper = rep(5.0, 14),
  control = list(maxit = 100, trace = 1, REPORT = 5)
)

cat(sprintf("\nOptimization complete.\n"))
cat(sprintf("Convergence: %d\n", result$convergence))
cat(sprintf("Final error: %.4f (initial: %.4f)\n", result$value, init_error))
cat(sprintf("Improvement: %.1f%%\n", (1 - result$value / init_error) * 100))

# =============================================================================
# 6. Display Scaling Factors
# =============================================================================

cat("\n--- 6. Scaling Factors by Age Group ---\n")

scale_results <- data.table(
  sex = rep(c("male", "female"), each = 7),
  age_group = rep(names(age_groups), 2),
  scale = result$par
)

cat("\nOptimal scaling factors (multiply DHS distribution by these):\n")
scale_wide <- dcast(scale_results, age_group ~ sex, value.var = "scale")
print(scale_wide[, .(age_group, male = round(male, 3), female = round(female, 3))])

# =============================================================================
# 7. Create and Save Calibrated Distribution
# =============================================================================

cat("\n--- 7. Saving Calibrated Distribution ---\n")

calibrated_dist <- copy(dhs_dist)
calibrated_dist <- merge(calibrated_dist, scale_results, by = c("sex", "age_group"), all.x = TRUE)
calibrated_dist[, distribution := distribution * scale]
calibrated_dist[, distribution := distribution / sum(distribution)]
calibrated_dist <- calibrated_dist[, .(age, sex, distribution)]
setorder(calibrated_dist, sex, age)

fwrite(calibrated_dist, "data/processed/calibrated_lpr_distribution.csv")
cat("Saved to: data/processed/calibrated_lpr_distribution.csv\n")

# =============================================================================
# 8. Compare Distributions
# =============================================================================

cat("\n--- 8. Distribution Comparison ---\n")

comp_dist <- merge(
  dhs_dist[, .(age, sex, age_group, dhs = distribution)],
  calibrated_dist[, .(age, sex, calibrated = distribution)],
  by = c("age", "sex")
)

by_group <- comp_dist[, .(
  dhs = sum(dhs) * 100,
  calibrated = sum(calibrated) * 100
), by = age_group]
by_group[, change := calibrated - dhs]

cat("\nDistribution by Age Group (%):\n")
print(by_group[, .(age_group, DHS = round(dhs, 2), Calibrated = round(calibrated, 2), Change = round(change, 2))])

# =============================================================================
# 9. Validation
# =============================================================================

cat("\n--- 9. Validation vs TR2025 ---\n")

proj_final <- project_with_scales(result$par)
proj_final <- proj_final[year %in% proj_years]

proj_final[, age_group := fcase(
  age <= 17, "0-17", age <= 24, "18-24", age <= 44, "25-44",
  age <= 64, "45-64", age <= 84, "65-84", age <= 99, "85-99", default = "100+"
)]

ours <- proj_final[, .(ours = sum(population)), by = .(year, age_group)]
tr <- tr_target[, .(tr = sum(population)), by = .(year, age_group)]

val <- merge(ours, tr, by = c("year", "age_group"))
val[, pct_diff := (ours - tr) / tr * 100]

val_wide <- dcast(val, year ~ age_group, value.var = "pct_diff")
cat("\nPercent Difference (Calibrated vs TR2025):\n")
print(val_wide[year %in% c(2025, 2030, 2035, 2040, 2045, 2050),
               .(`year`, `0-17`, `18-24`, `25-44`, `45-64`, `65-84`, `85-99`, `100+`)], digits = 2)

cat("\n=== DONE ===\n")

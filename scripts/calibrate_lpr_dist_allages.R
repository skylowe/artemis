#!/usr/bin/env Rscript
#' Calibrate LPR Distribution to Match TR2025 Population Projection
#' VECTORIZED VERSION
#'
#' Finds the LPR age-sex distribution that minimizes population error vs TR2025

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

# Load our model outputs from targets
tar_load(net_o_for_projection)
tar_load(lpr_assumptions)
tar_load(mortality_qx_projected)
tar_load(starting_population)
tar_load(lpr_distribution)

# Projection years for calibration
proj_years <- 2023:2050
cat(sprintf("Calibration years: %d-%d\n", min(proj_years), max(proj_years)))

# =============================================================================
# 2. Prepare Fixed Components (Vectorized)
# =============================================================================

cat("\n--- 2. Preparing Fixed Components ---\n")

# Starting population (extract from list and aggregate across pop_status)
start_pop <- as.data.table(starting_population$population)
start_pop <- start_pop[, .(population = sum(population)), by = .(year, age, sex)]
start_pop[, sex := as.character(sex)]
setkey(start_pop, year, age, sex)

# Net O immigration (aggregate across type)
net_o <- as.data.table(net_o_for_projection)
net_o <- net_o[year %in% proj_years, .(net_o = sum(net_o_immigration)), by = .(year, age, sex)]
setkey(net_o, year, age, sex)

# Mortality qx - need qx for age-1 to calculate survivors
qx <- as.data.table(mortality_qx_projected)
qx[, sex := as.character(sex)]
qx <- qx[year %in% proj_years]
setkey(qx, year, age, sex)

# LPR totals
lpr_totals <- as.data.table(lpr_assumptions)[year %in% proj_years, .(year, net_lpr_total)]
setkey(lpr_totals, year)

# TR2025 target
tr_target <- tr_pop_long[year %in% proj_years]
setkey(tr_target, year, age, sex)

cat(sprintf("Starting pop: %.2fM\n", sum(start_pop$population) / 1e6))
cat(sprintf("Net O rows: %d\n", nrow(net_o)))
cat(sprintf("Years: %d\n", length(proj_years)))

# =============================================================================
# 3. Vectorized Projection Function
# =============================================================================

cat("\n--- 3. Setting Up Vectorized Projection ---\n")

#' Vectorized population projection
#'
#' @param dist_vector Distribution as vector (male 0-100, female 0-100)
#' @return data.table with projected population
project_vec <- function(dist_vector) {
  # Create distribution table
  lpr_dist <- data.table(
    age = rep(0:100, 2),
    sex = rep(c("male", "female"), each = 101),
    distribution = dist_vector / sum(dist_vector)
  )
  setkey(lpr_dist, age, sex)

  # Initialize results with starting population
  results <- vector("list", length(proj_years) + 1)
  results[[1]] <- copy(start_pop)

  for (i in seq_along(proj_years)) {
    yr <- proj_years[i]
    prev_yr <- yr - 1

    prev_pop <- results[[i]]

    # Create population for current year
    new_pop <- CJ(year = yr, age = 0:100, sex = c("male", "female"))

    # Get net LPR for this year
    lpr_total <- lpr_totals[year == yr, net_lpr_total]
    net_lpr <- copy(lpr_dist)
    net_lpr[, net_lpr := distribution * lpr_total]

    # Get net O for this year
    net_o_yr <- net_o[year == yr, .(age, sex, net_o)]

    # Get qx for previous year (survival from age-1 to age)
    qx_yr <- qx[year == yr, .(age, sex, qx)]

    # Shift prev_pop: age+1 for next year
    prev_shifted <- copy(prev_pop)
    prev_shifted[, age := age + 1]
    prev_shifted[, year := yr]
    setnames(prev_shifted, "population", "prev_pop")

    # Shift qx: we need qx at age-1 for survival to age
    qx_shifted <- copy(qx_yr)
    qx_shifted[, age := age + 1]

    # Merge all components
    new_pop <- merge(new_pop, prev_shifted[, .(year, age, sex, prev_pop)],
                     by = c("year", "age", "sex"), all.x = TRUE)
    new_pop <- merge(new_pop, qx_shifted[, .(age, sex, qx)],
                     by = c("age", "sex"), all.x = TRUE)
    new_pop <- merge(new_pop, net_lpr[, .(age, sex, net_lpr)],
                     by = c("age", "sex"), all.x = TRUE)
    new_pop <- merge(new_pop, net_o_yr,
                     by = c("age", "sex"), all.x = TRUE)

    # Fill NAs
    new_pop[is.na(prev_pop), prev_pop := 0]
    new_pop[is.na(qx), qx := 0]
    new_pop[is.na(net_lpr), net_lpr := 0]
    new_pop[is.na(net_o), net_o := 0]

    # Calculate survivors and new population for ages 1-99
    new_pop[age >= 1 & age <= 99,
            population := prev_pop * (1 - qx) + net_lpr + net_o]

    # Age 0: use TR2025 births (fixed)
    age0_tr <- tr_target[year == yr & age == 0, .(sex, population)]
    new_pop <- merge(new_pop, age0_tr, by = "sex", all.x = TRUE, suffixes = c("", "_tr"))
    new_pop[age == 0, population := population_tr]
    new_pop[, population_tr := NULL]

    # Age 100+: survivors from 99 and 100+
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

    # Clean up and store
    new_pop <- new_pop[, .(year, age, sex, population)]
    setkey(new_pop, year, age, sex)
    results[[i + 1]] <- new_pop
  }

  rbindlist(results)
}

#' Calculate weighted squared error vs TR2025
calc_error <- function(dist_vector) {
  proj <- project_vec(dist_vector)
  proj <- proj[year %in% proj_years]

  comp <- merge(proj, tr_target, by = c("year", "age", "sex"),
                suffixes = c("_ours", "_tr"))

  # Weight: more weight on working ages, less on 100+
  comp[, weight := fifelse(age >= 20 & age <= 64, 2.0,
                   fifelse(age >= 65 & age <= 84, 1.5,
                   fifelse(age == 100, 0.5, 1.0)))]

  comp[, sq_err := weight * ((population_ours - population_tr) / 1e6)^2]

  sum(comp$sq_err, na.rm = TRUE)
}

# =============================================================================
# 4. Initialize and Test
# =============================================================================

cat("\n--- 4. Testing Initial Distribution ---\n")

# Current DHS distribution as starting point
dhs_dist <- as.data.table(lpr_distribution)
dhs_dist[, sex := as.character(sex)]
setorder(dhs_dist, sex, age)

init_vector <- c(
  dhs_dist[sex == "male", distribution],
  dhs_dist[sex == "female", distribution]
)

cat(sprintf("Distribution elements: %d\n", length(init_vector)))

# Test projection speed
t1 <- Sys.time()
init_error <- calc_error(init_vector)
t2 <- Sys.time()
cat(sprintf("Initial error (DHS): %.4f\n", init_error))
cat(sprintf("Projection time: %.2f seconds\n", as.numeric(t2 - t1, units = "secs")))

# =============================================================================
# 5. Run Optimization
# =============================================================================

cat("\n--- 5. Running Optimization ---\n")

result <- optim(
  par = init_vector,
  fn = calc_error,
  method = "L-BFGS-B",
  lower = rep(1e-8, length(init_vector)),  # Small positive lower bound
  upper = rep(0.1, length(init_vector)),   # No single age > 10%
  control = list(
    maxit = 200,
    trace = 1,
    REPORT = 10
  )
)

cat(sprintf("\nOptimization complete.\n"))
cat(sprintf("Convergence: %d\n", result$convergence))
cat(sprintf("Final error: %.4f (initial: %.4f)\n", result$value, init_error))
cat(sprintf("Improvement: %.1f%%\n", (1 - result$value / init_error) * 100))

# =============================================================================
# 6. Save Calibrated Distribution
# =============================================================================

cat("\n--- 6. Saving Results ---\n")

calibrated_dist <- data.table(
  age = rep(0:100, 2),
  sex = rep(c("male", "female"), each = 101),
  distribution = result$par / sum(result$par)
)
setorder(calibrated_dist, sex, age)

fwrite(calibrated_dist, "data/processed/calibrated_lpr_distribution.csv")
cat("Saved to: data/processed/calibrated_lpr_distribution.csv\n")

# =============================================================================
# 7. Compare Distributions
# =============================================================================

cat("\n--- 7. Distribution Comparison ---\n")

comp_dist <- merge(
  dhs_dist[, .(age, sex, dhs = distribution)],
  calibrated_dist[, .(age, sex, calibrated = distribution)],
  by = c("age", "sex")
)

comp_dist[, age_group := fcase(
  age <= 17, "0-17",
  age <= 24, "18-24",
  age <= 44, "25-44",
  age <= 64, "45-64",
  age <= 84, "65-84",
  age <= 99, "85-99",
  default = "100+"
)]

by_group <- comp_dist[, .(
  dhs = sum(dhs) * 100,
  calibrated = sum(calibrated) * 100
), by = age_group]
by_group[, diff := calibrated - dhs]

cat("\nDistribution by Age Group (%):\n")
print(by_group[, .(age_group,
                   DHS = round(dhs, 2),
                   Calibrated = round(calibrated, 2),
                   Change = round(diff, 2))])

by_sex <- comp_dist[, .(
  dhs = sum(dhs) * 100,
  calibrated = sum(calibrated) * 100
), by = sex]
cat("\nDistribution by Sex (%):\n")
print(by_sex)

# =============================================================================
# 8. Validation
# =============================================================================

cat("\n--- 8. Validation vs TR2025 ---\n")

proj_final <- project_vec(result$par)
proj_final <- proj_final[year %in% proj_years]

proj_final[, age_group := fcase(
  age <= 17, "0-17",
  age <= 24, "18-24",
  age <= 44, "25-44",
  age <= 64, "45-64",
  age <= 84, "65-84",
  age <= 99, "85-99",
  default = "100+"
)]

tr_target[, age_group := fcase(
  age <= 17, "0-17",
  age <= 24, "18-24",
  age <= 44, "25-44",
  age <= 64, "45-64",
  age <= 84, "65-84",
  age <= 99, "85-99",
  default = "100+"
)]

ours <- proj_final[, .(ours = sum(population)), by = .(year, age_group)]
tr <- tr_target[, .(tr = sum(population)), by = .(year, age_group)]

val <- merge(ours, tr, by = c("year", "age_group"))
val[, pct_diff := (ours - tr) / tr * 100]

val_wide <- dcast(val, year ~ age_group, value.var = "pct_diff")
cat("\nPercent Difference (Calibrated vs TR2025):\n")
print(val_wide[year %in% c(2025, 2030, 2035, 2040, 2045, 2050),
               .(`year`, `0-17`, `18-24`, `25-44`, `45-64`, `65-84`, `85-99`, `100+`)],
      digits = 2)

cat("\n=== DONE ===\n")

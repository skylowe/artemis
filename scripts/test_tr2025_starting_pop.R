#!/usr/bin/env Rscript
#' Test Population Projection with TR2025 Starting Population
#'
#' This script tests our projection methodology by using TR2025's historical
#' population as the starting point, isolating differences due to projection
#' methodology vs differences in historical population estimation.
#'
#' Usage: Rscript scripts/test_tr2025_starting_pop.R

library(data.table)
library(targets)

cat("=================================================================\n")
cat("Testing Population Projection with TR2025 Starting Population\n")
cat("=================================================================\n\n")

# Load required inputs from targets
cat("Loading projection inputs from targets...\n")
birth_rates <- tar_read(fertility_rates_complete)
mortality_qx <- tar_read(mortality_qx_for_projection)
net_lpr <- tar_read(net_lpr_immigration)
net_o <- tar_read(net_o_for_projection)
config <- tar_read(config_assumptions)

# Source the projection function
source("R/demography/projected_population.R")

# Debug: Check input data formats
cat("\n--- Debug: Input Data Formats ---\n")
cat("Birth rates years available: ", paste(sort(unique(birth_rates$year))[1:5], collapse=", "), "...\n")
cat("Birth rates columns: ", paste(names(birth_rates), collapse=", "), "\n")
cat("Net LPR years available: ", paste(sort(unique(net_lpr$year))[1:5], collapse=", "), "...\n")
cat("Net O years available: ", paste(sort(unique(net_o$year))[1:5], collapse=", "), "...\n")

# Load TR2025 historical population
cat("Loading TR2025 December 2022 population...\n")
tr_pop <- fread("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")

# Extract 2022 as starting population
tr_2022 <- tr_pop[Year == 2022, .(
  year = Year,
  age = Age,
  male = `M Tot`,
  female = `F Tot`
)]

# Convert to long format
tr_start <- melt(
  tr_2022,
  id.vars = c("year", "age"),
  measure.vars = c("male", "female"),
  variable.name = "sex",
  value.name = "population"
)

# Cap age at 100 (aggregate 100+ into single group)
tr_start[age > 100, age := 100L]
tr_start <- tr_start[, .(population = sum(population)), by = .(year, age, sex)]

# Add population status (2.5% gay for males, 4.5% lesbian for females)
tr_start_status <- rbindlist(list(
  tr_start[sex == "male", .(year, age, sex, pop_status = "heterosexual",
                             population = population * 0.975)],
  tr_start[sex == "male", .(year, age, sex, pop_status = "gay",
                             population = population * 0.025)],
  tr_start[sex == "female", .(year, age, sex, pop_status = "heterosexual",
                               population = population * 0.955)],
  tr_start[sex == "female", .(year, age, sex, pop_status = "lesbian",
                               population = population * 0.045)]
))

# Also load our starting population for comparison
our_start <- tar_read(starting_population)$population

# Compare starting populations
cat("\n--- Starting Population Comparison (Dec 31, 2022) ---\n")
our_totals <- our_start[, .(ours = sum(population)), by = age]
tr_totals <- tr_start_status[, .(tr = sum(population)), by = age]
start_comp <- merge(our_totals, tr_totals, by = "age")
start_comp[, diff_pct := (ours - tr) / tr * 100]

cat("Total population:\n")
cat(sprintf("  Our model: %s\n", format(sum(start_comp$ours), big.mark = ",")))
cat(sprintf("  TR2025:    %s\n", format(sum(start_comp$tr), big.mark = ",")))
cat(sprintf("  Diff:      %.2f%%\n\n", (sum(start_comp$ours) - sum(start_comp$tr)) / sum(start_comp$tr) * 100))

cat("Ages with largest differences:\n")
print(head(start_comp[order(-abs(diff_pct))], 10))

cat("\n--- Running Population Projection with TR2025 Starting Pop ---\n")

# Run projection with TR2025 starting population
result_list <- run_population_projection(
  starting_population = tr_start_status,
  birth_rates = birth_rates,
  mortality_qx = mortality_qx,
  net_lpr = net_lpr,
  net_o = net_o,
  start_year = 2022,
  end_year = 2099,
  config = config
)

# Extract population data.table from result
result_tr <- as.data.table(result_list$population)

# Show diagnostic summary
cat("\n--- Diagnostic Summary ---\n")
summary_dt <- result_list$summary
cat("Year  |  Births   |  Deaths   |  Net Imm  |  Population\n")
cat("------+----------+-----------+-----------+-------------\n")
for (yr in c(2023, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2099)) {
  row <- summary_dt[year == yr]
  cat(sprintf("%d | %9.0f | %9.0f | %9.0f | %12.0f\n",
              yr, row$total_births, row$total_deaths, row$total_net_imm, row$total_population))
}

# Load TR2025 projections for comparison
cat("\n--- Comparing Projections ---\n")

# Create age groups
age_breaks <- c(0, 18, 25, 45, 65, 85, 100, Inf)
age_labels <- c("0-17", "18-24", "25-44", "45-64", "65-84", "85-99", "100+")

# Aggregate our TR-start projection
result_tr[, age_group := cut(age, breaks = age_breaks, labels = age_labels, right = FALSE)]
our_agg <- result_tr[, .(pop_ours = sum(population, na.rm = TRUE)), by = .(year, age_group)]

# Aggregate TR2025 projection
tr_pop[, age_group := cut(Age, breaks = age_breaks, labels = age_labels, right = FALSE)]
tr_agg <- tr_pop[, .(pop_tr = sum(`M Tot`, na.rm = TRUE) + sum(`F Tot`, na.rm = TRUE)), by = .(year = Year, age_group)]

# Merge
comp <- merge(our_agg, tr_agg, by = c("year", "age_group"), all.x = TRUE)
comp[, diff_pct := round((pop_ours - pop_tr) / pop_tr * 100, 2)]

# Wide format
wide <- dcast(comp[year >= 2023], year ~ age_group, value.var = "diff_pct")

# Add total column
totals <- comp[year >= 2023, .(total_ours = sum(pop_ours), total_tr = sum(pop_tr)), by = year]
totals[, Total := round((total_ours - total_tr) / total_tr * 100, 2)]
wide <- merge(wide, totals[, .(year, Total)], by = "year")

# Print key years
cat("\nPercent Difference by Age Group (Using TR2025 Starting Pop):\n")
cat("=============================================================\n")
key_years <- wide[year %in% c(2023, 2025, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2099)]
print(key_years, row.names = FALSE)

# 100+ detail
cat("\n\n100+ Population Detail:\n")
cat("=======================\n")
our_100 <- result_tr[age >= 100, .(pop_ours = sum(population)), by = year]
tr_100 <- tr_pop[Age >= 100, .(pop_tr = sum(`M Tot`) + sum(`F Tot`)), by = .(year = Year)]
comp_100 <- merge(our_100, tr_100, by = "year")
comp_100[, diff_pct := round((pop_ours - pop_tr) / pop_tr * 100, 1)]
result_100 <- comp_100[year >= 2023, .(year,
                                        ours_k = round(pop_ours / 1000, 1),
                                        tr_k = round(pop_tr / 1000, 1),
                                        diff_pct)]
print(result_100[year %in% c(2023, 2025, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2099)], row.names = FALSE)

cat("\n\nDone.\n")

#!/usr/bin/env Rscript
#' Compare Population Projection by Age Group with TR2025
#'
#' This script compares our population projection against TR2025's
#' intermediate assumption, broken down by age group.
#'
#' Usage: Rscript scripts/compare_population_projection.R

library(data.table)
library(targets)

# Load our population projection
pop_proj <- tar_read(projected_population)

# Load TR2025 population
tr_pop <- fread("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")

# Create age groups with 85-99 and 100+ split
age_breaks <- c(0, 18, 25, 45, 65, 85, 100, Inf)
age_labels <- c("0-17", "18-24", "25-44", "45-64", "65-84", "85-99", "100+")

# Aggregate our projection by year and age group
pop_proj[, age_group := cut(age, breaks = age_breaks, labels = age_labels, right = FALSE)]
our_agg <- pop_proj[, .(pop_ours = sum(population, na.rm = TRUE)), by = .(year, age_group)]

# Aggregate TR2025 by year and age group
tr_pop[, age_group := cut(Age, breaks = age_breaks, labels = age_labels, right = FALSE)]
tr_agg <- tr_pop[, .(pop_tr = sum(`M Tot`, na.rm = TRUE) + sum(`F Tot`, na.rm = TRUE)), by = .(year = Year, age_group)]

# Merge
comp <- merge(our_agg, tr_agg, by = c("year", "age_group"), all.x = TRUE)
comp[, diff_pct := round((pop_ours - pop_tr) / pop_tr * 100, 2)]

# Wide format - difference by age group
wide <- dcast(comp[year >= 2023], year ~ age_group, value.var = "diff_pct")

# Add total column
totals <- comp[year >= 2023, .(total_ours = sum(pop_ours), total_tr = sum(pop_tr)), by = year]
totals[, Total := round((total_ours - total_tr) / total_tr * 100, 2)]

wide <- merge(wide, totals[, .(year, Total)], by = "year")

cat("Percent Difference by Age Group (Our Model vs TR2025):\n")
cat("=======================================================\n")
print(wide, row.names = FALSE)

# Also show 100+ population counts for context
cat("\n\n100+ Population (thousands):\n")
cat("============================\n")
our_100 <- pop_proj[age >= 100, .(pop_ours = sum(population)), by = year]
tr_100 <- tr_pop[Age >= 100, .(pop_tr = sum(`M Tot`) + sum(`F Tot`)), by = .(year = Year)]
comp_100 <- merge(our_100, tr_100, by = "year")
comp_100[, diff_pct := round((pop_ours - pop_tr) / pop_tr * 100, 1)]
result_100 <- comp_100[year >= 2023, .(year,
                                        ours_k = round(pop_ours / 1000, 1),
                                        tr_k = round(pop_tr / 1000, 1),
                                        diff_pct)]
print(result_100[year %in% c(2023, 2025, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2099)], row.names = FALSE)

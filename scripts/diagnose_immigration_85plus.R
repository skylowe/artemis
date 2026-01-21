#!/usr/bin/env Rscript
#' Diagnose Immigration at Ages 85+ by Deriving TR2025's Implied Immigration
#'
#' Uses cohort survival to back-calculate what net immigration TR2025 must have

library(data.table)
library(targets)

cat("=================================================================\n")
cat("DERIVING TR2025 IMPLIED IMMIGRATION AT AGES 85+\n")
cat("=================================================================\n\n")

# Load data
tr_pop <- fread("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")
tr_qx_male <- fread("data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv", skip = 1)
tr_qx_female <- fread("data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv", skip = 1)

# Reshape population
tr_pop_long <- rbindlist(list(
  tr_pop[, .(year = Year, age = Age, sex = "male", population = `M Tot`)],
  tr_pop[, .(year = Year, age = Age, sex = "female", population = `F Tot`)]
))
setkey(tr_pop_long, year, age, sex)

# Reshape qx
tr_qx_m <- melt(tr_qx_male, id.vars = "Year", variable.name = "age_chr", value.name = "qx")
tr_qx_m[, age := as.integer(as.character(age_chr))]
tr_qx_m[, sex := "male"]

tr_qx_f <- melt(tr_qx_female, id.vars = "Year", variable.name = "age_chr", value.name = "qx")
tr_qx_f[, age := as.integer(as.character(age_chr))]
tr_qx_f[, sex := "female"]

tr_qx <- rbindlist(list(tr_qx_m, tr_qx_f))
tr_qx <- tr_qx[, .(year = Year, age, sex, qx)]
setkey(tr_qx, year, age, sex)

# Load our net immigration
net_lpr <- tar_read(net_lpr_immigration)
net_o <- tar_read(net_o_for_projection)

# Get our total net immigration at each age
our_imm <- merge(
  net_lpr[, .(year, age, sex, net_lpr = net_lpr)],
  net_o[, .(year, age, sex, net_o = net_o_immigration)],
  by = c("year", "age", "sex"),
  all = TRUE
)
our_imm[is.na(net_lpr), net_lpr := 0]
our_imm[is.na(net_o), net_o := 0]
our_imm[, total_imm := net_lpr + net_o]

# =============================================================================
# Back-calculate TR2025's implied immigration
# Formula: P_{x}^{z} = P_{x-1}^{z-1} * (1 - qx_{x}) + NI_{x}^{z}
# Therefore: NI_{x}^{z} = P_{x}^{z} - P_{x-1}^{z-1} * (1 - qx_{x})
# =============================================================================

cat("--- Computing TR2025 Implied Net Immigration ---\n\n")

# For ages 1-100, calculate implied immigration
ages_to_calc <- 1:100
years_to_calc <- 2023:2030

implied_ni <- list()

for (yr in years_to_calc) {
  for (a in ages_to_calc) {
    for (sx in c("male", "female")) {
      # Population this year at age a
      pop_now <- tr_pop_long[year == yr & age == a & sex == sx, population]

      # Population last year at age a-1
      pop_prev <- tr_pop_long[year == yr - 1 & age == a - 1 & sex == sx, population]

      # qx at age a for year z
      qx_val <- tr_qx[year == yr & age == a & sex == sx, qx]

      if (length(pop_now) == 1 && length(pop_prev) == 1 && length(qx_val) == 1) {
        # Survivors = pop_prev * (1 - qx)
        survivors <- pop_prev * (1 - qx_val)

        # Implied immigration = current pop - survivors
        implied_immigration <- pop_now - survivors

        implied_ni[[length(implied_ni) + 1]] <- data.table(
          year = yr,
          age = a,
          sex = sx,
          pop_prev = pop_prev,
          pop_now = pop_now,
          qx = qx_val,
          survivors = survivors,
          implied_ni = implied_immigration
        )
      }
    }
  }
}

implied_ni_dt <- rbindlist(implied_ni)

# Compare at ages 85-100
cat("TR2025 Implied Net Immigration at Ages 85-100:\n")
cat("==============================================\n\n")

implied_85plus <- implied_ni_dt[age >= 85 & year %in% c(2023, 2025, 2030)]
implied_summary <- implied_85plus[, .(
  implied_ni_total = sum(implied_ni)
), by = .(year, sex)]

# Our immigration at same ages
our_85plus <- our_imm[age >= 85 & year %in% c(2023, 2025, 2030)]
our_summary <- our_85plus[, .(
  our_ni_total = sum(total_imm)
), by = .(year, sex)]

# Compare
comp <- merge(implied_summary, our_summary, by = c("year", "sex"))
comp[, diff := our_ni_total - implied_ni_total]

cat("Net Immigration at Ages 85+ (Total by Year and Sex):\n")
print(comp[, .(year, sex,
               TR2025_implied = round(implied_ni_total),
               Ours = round(our_ni_total),
               Difference = round(diff))], row.names = FALSE)

# Show by specific age for one year
cat("\n\nNet Immigration at Each Age 85-100 (2025, Both Sexes):\n")
implied_2025 <- implied_ni_dt[age >= 85 & year == 2025]
implied_by_age <- implied_2025[, .(implied_ni = sum(implied_ni)), by = age]

our_2025 <- our_imm[age >= 85 & year == 2025]
our_by_age <- our_2025[, .(our_ni = sum(total_imm)), by = age]

comp_by_age <- merge(implied_by_age, our_by_age, by = "age", all = TRUE)
comp_by_age[, diff := our_ni - implied_ni]

print(comp_by_age[, .(age,
                      TR2025_implied = round(implied_ni),
                      Ours = round(our_ni),
                      Difference = round(diff))], row.names = FALSE)

# Calculate cumulative effect
cat("\n\nCumulative Immigration Difference (85+, 2023-2030, Both Sexes):\n")
all_years_comp <- implied_ni_dt[age >= 85, .(tr_implied = sum(implied_ni)), by = year]
our_years <- our_imm[age >= 85, .(ours = sum(total_imm)), by = year]
yearly_comp <- merge(all_years_comp, our_years, by = "year")
yearly_comp[, diff := ours - tr_implied]
yearly_comp[, cum_diff := cumsum(diff)]

print(yearly_comp[year <= 2030, .(year,
                                   TR2025_implied = round(tr_implied),
                                   Ours = round(ours),
                                   Difference = round(diff),
                                   Cumulative_Diff = round(cum_diff))], row.names = FALSE)

# Special analysis for age 100+ (the open-ended group)
cat("\n\n=== Age 100+ Analysis ===\n")
cat("Note: Age 100 is an open-ended group (100+).\n")
cat("Formula: P_{100}^z = [P_{99}^{z-1} * (1-q99) + P_{100}^{z-1} * (1-q100)] + NI_{100}^z\n\n")

for (yr in c(2023, 2025, 2030)) {
  # TR2025 values
  p99_prev <- tr_pop_long[year == yr - 1 & age == 99, sum(population)]
  p100_prev <- tr_pop_long[year == yr - 1 & age == 100, sum(population)]
  p100_now <- tr_pop_long[year == yr & age == 100, sum(population)]

  q99_m <- tr_qx[year == yr & age == 99 & sex == "male", qx]
  q99_f <- tr_qx[year == yr & age == 99 & sex == "female", qx]
  q100_m <- tr_qx[year == yr & age == 100 & sex == "male", qx]
  q100_f <- tr_qx[year == yr & age == 100 & sex == "female", qx]

  # Get sex breakdown
  p99_m <- tr_pop_long[year == yr - 1 & age == 99 & sex == "male", population]
  p99_f <- tr_pop_long[year == yr - 1 & age == 99 & sex == "female", population]
  p100_m <- tr_pop_long[year == yr - 1 & age == 100 & sex == "male", population]
  p100_f <- tr_pop_long[year == yr - 1 & age == 100 & sex == "female", population]

  # Survivors by sex
  surv_from_99 <- p99_m * (1 - q99_m) + p99_f * (1 - q99_f)
  surv_at_100 <- p100_m * (1 - q100_m) + p100_f * (1 - q100_f)
  total_surv <- surv_from_99 + surv_at_100

  implied_ni_100 <- p100_now - total_surv

  # Our immigration at 100
  our_ni_100 <- our_imm[age == 100 & year == yr, sum(total_imm)]

  cat(sprintf("Year %d:\n", yr))
  cat(sprintf("  P(99, %d): %s | q99: M=%.4f, F=%.4f\n", yr-1, format(p99_prev, big.mark=","), q99_m, q99_f))
  cat(sprintf("  P(100+, %d): %s | q100: M=%.4f, F=%.4f\n", yr-1, format(p100_prev, big.mark=","), q100_m, q100_f))
  cat(sprintf("  Survivors from 99->100: %s\n", format(round(surv_from_99), big.mark=",")))
  cat(sprintf("  Survivors staying at 100+: %s\n", format(round(surv_at_100), big.mark=",")))
  cat(sprintf("  Total survivors: %s\n", format(round(total_surv), big.mark=",")))
  cat(sprintf("  P(100+, %d): %s\n", yr, format(p100_now, big.mark=",")))
  cat(sprintf("  TR2025 implied NI(100): %s\n", format(round(implied_ni_100), big.mark=",")))
  cat(sprintf("  Our NI(100): %s\n", format(round(our_ni_100), big.mark=",")))
  cat(sprintf("  Difference: %s\n\n", format(round(our_ni_100 - implied_ni_100), big.mark=",")))
}

cat("\n=== DONE ===\n")

#' Derive TR2025 Implied Immigration Age Distribution
#'
#' This script calculates the implied net immigration age distribution from
#' TR2025 population projections by analyzing year-over-year population changes.
#'
#' Method:
#' For ages 1-99, net immigration is derived as:
#'   NI(age, year) = P(age, year) - P(age-1, year-1) * (1 - q_{age-1})
#'
#' Where:
#'   P(age, year) = population at age in year (from SSPopDec)
#'   q_{age-1} = probability of death at age-1 (from DeathProbsE)
#'
#' This reverse-engineers what net immigration TR2025 must have assumed by
#' comparing actual population to expected survivors from the prior year.
#'
#' Output:
#'   - data/cache/tr2025_implied_immigration_distribution.csv
#'
#' @author ARTEMIS Project
#' @date 2026-01-19

library(data.table)

#' Calculate implied net immigration for a single year
#'
#' @param year Integer: the year to calculate immigration for
#' @param tr_pop data.table: TR2025 population data (SSPopDec)
#' @param tr_qx_m data.table: TR2025 male death probabilities
#' @param tr_qx_f data.table: TR2025 female death probabilities
#'
#' @return data.table with columns: year, age, sex, net_immigration
calculate_implied_net_immigration <- function(year, tr_pop, tr_qx_m, tr_qx_f) {
  # Get population for current and previous year

tr_prev <- tr_pop[Year == year - 1]
  tr_curr <- tr_pop[Year == year]

  # Get mortality rates for previous year (applied to survivors)
  qx_m <- tr_qx_m[Year == year - 1]
  qx_f <- tr_qx_f[Year == year - 1]

  # Calculate implied net immigration for ages 1-99
  # (Age 0 requires birth data which we handle separately)
  results <- rbindlist(lapply(1:99, function(a) {
    # Current year population at age a
    curr_m <- tr_curr[Age == a, `M Tot`]
    curr_f <- tr_curr[Age == a, `F Tot`]

    # Previous year population at age a-1
    prev_m <- tr_prev[Age == a - 1, `M Tot`]
    prev_f <- tr_prev[Age == a - 1, `F Tot`]

    # Survival probability (1 - death probability at age a-1)
    surv_m <- 1 - qx_m[1, get(as.character(a - 1))]
    surv_f <- 1 - qx_f[1, get(as.character(a - 1))]

    # Expected survivors without any immigration
    expected_m <- prev_m * surv_m
    expected_f <- prev_f * surv_f

    # Implied net immigration = actual - expected
    ni_m <- curr_m - expected_m
    ni_f <- curr_f - expected_f

    data.table(
      year = year,
      age = a,
      male = ni_m,
      female = ni_f,
      total = ni_m + ni_f
    )
  }))

  results
}


#' Derive TR2025 implied immigration distribution
#'
#' Calculates the average implied net immigration distribution across
#' multiple projection years to get a stable estimate.
#'
#' @param years Integer vector: years to analyze (default: 2025:2050)
#' @param tr_pop_file Character: path to TR2025 population file
#' @param tr_qx_male_file Character: path to TR2025 male mortality file
#' @param tr_qx_female_file Character: path to TR2025 female mortality file
#'
#' @return data.table with columns: age, male, female, total, male_pct, female_pct, total_pct
derive_tr2025_immigration_distribution <- function(
    years = 2025:2050,
    tr_pop_file = "data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv",
    tr_qx_male_file = "data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv",
    tr_qx_female_file = "data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv"
) {
  # Load TR2025 data
  cli::cli_alert_info("Loading TR2025 data files...")
  tr_pop <- fread(tr_pop_file)
  tr_qx_m <- fread(tr_qx_male_file)
  tr_qx_f <- fread(tr_qx_female_file)

  # Calculate implied net immigration for each year
  cli::cli_alert_info("Calculating implied net immigration for {length(years)} years...")
  all_ni <- rbindlist(lapply(years, function(y) {
    calculate_implied_net_immigration(y, tr_pop, tr_qx_m, tr_qx_f)
  }))

  # Average across years to get stable distribution
  avg_dist <- all_ni[, .(
    male = mean(male),
    female = mean(female),
    total = mean(total)
  ), by = age]

  # Calculate percentages
  avg_dist[, male_pct := male / sum(male) * 100]
  avg_dist[, female_pct := female / sum(female) * 100]
  avg_dist[, total_pct := total / sum(total) * 100]

  # Order by age
setorder(avg_dist, age)

  cli::cli_alert_success("Derived immigration distribution for ages 1-99")

  avg_dist
}


#' Print summary of immigration distribution
#'
#' @param dist data.table: output from derive_tr2025_immigration_distribution
print_distribution_summary <- function(dist) {
  # Add age groups
  dist_copy <- copy(dist)
  dist_copy[, age_group := cut(age,
    breaks = c(0, 4, 9, 14, 17, 24, 34, 44, 54, 64, 72, 99),
    labels = c("1-4", "5-9", "10-14", "15-17", "18-24", "25-34",
               "35-44", "45-54", "55-64", "65-72", "73-99")
  )]

  summary <- dist_copy[, .(
    total = sum(total),
    male = sum(male),
    female = sum(female)
  ), by = age_group]

  summary[, total_pct := round(total / sum(total) * 100, 2)]
  summary[, male_pct := round(male / sum(male) * 100, 2)]
  summary[, female_pct := round(female / sum(female) * 100, 2)]

  cli::cli_h2("TR2025 Implied Net Immigration by Age Group")
  print(summary[, .(age_group, total = round(total), total_pct, male_pct, female_pct)])

  # Key statistics
  cli::cli_h2("Key Statistics")
  cli::cli_alert_info("Total implied net immigration (annual avg): {format(round(sum(dist$total)), big.mark=',')}")
  cli::cli_alert_info("Male share: {round(sum(dist$male) / sum(dist$total) * 100, 1)}%")
  cli::cli_alert_info("Female share: {round(sum(dist$female) / sum(dist$total) * 100, 1)}%")

  # Emigration ages
  emig_total <- sum(dist[total < 0, total])
  emig_ages <- dist[total < 0, range(age)]
  cli::cli_alert_warning("Net EMIGRATION at ages {emig_ages[1]}-{emig_ages[2]}: {format(round(emig_total), big.mark=',')} per year")

  # Peak ages
  setorder(dist_copy, -total)
  peak_ages <- head(dist_copy$age, 5)
  cli::cli_alert_info("Peak immigration ages: {paste(peak_ages, collapse=', ')}")

  invisible(summary)
}


# Main execution
if (sys.nframe() == 0) {
  # Running as script

  cli::cli_h1("Deriving TR2025 Implied Immigration Age Distribution")

  # Derive distribution
  dist <- derive_tr2025_immigration_distribution()

  # Print summary
  print_distribution_summary(dist)

# Save to file
  output_file <- "data/cache/tr2025_implied_immigration_distribution.csv"
  fwrite(dist, output_file)
  cli::cli_alert_success("Saved distribution to {output_file}")

  # Also save age group summary
  dist[, age_group := cut(age,
    breaks = c(0, 4, 9, 14, 17, 24, 34, 44, 54, 64, 72, 99),
    labels = c("1-4", "5-9", "10-14", "15-17", "18-24", "25-34",
               "35-44", "45-54", "55-64", "65-72", "73-99")
  )]

  summary <- dist[, .(
    total = sum(total),
    male = sum(male),
    female = sum(female),
    total_pct = sum(total_pct),
    male_pct = sum(male_pct),
    female_pct = sum(female_pct)
  ), by = age_group]

  summary_file <- "data/cache/tr2025_implied_immigration_distribution_by_age_group.csv"
  fwrite(summary, summary_file)
  cli::cli_alert_success("Saved age group summary to {summary_file}")
}

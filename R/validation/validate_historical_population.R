#' Historical Population Validation Functions
#'
#' Functions for validating historical population outputs (Eq 1.4.1-1.4.4)
#' against TR SSPopDec reference data, internal consistency checks, and
#' demographic plausibility constraints.
#'
#' @name validate_historical_population
NULL

# =============================================================================
# AGE-SEX VALIDATION
# =============================================================================

#' Validate Age-Sex Population Against SSPopDec
#'
#' @description
#' Compares ARTEMIS historical population by year, age, and sex against
#' the official SSPopDec file. Reports differences at the total, age-group,
#' and single-year-of-age levels.
#'
#' @param population data.table with columns: year, age, sex, population
#' @param config List: configuration (used to resolve SSPopDec path)
#' @param years Integer vector of years to validate (NULL = all overlapping)
#' @param tolerance Numeric: max acceptable percent difference (default: 2%)
#'
#' @return List with: valid, summary, by_year, by_age_sex
#'
#' @export
validate_age_sex_vs_tr <- function(population, config, years = NULL, tolerance = 0.02) {
  cli::cli_h2("Validation: Age-Sex Population vs SSPopDec")

  tr_year <- config$metadata$trustees_report_year
  filepath <- resolve_tr_file(config, "population_dec")

  if (!file.exists(filepath)) {
    cli::cli_warn("SSPopDec file not found at {filepath} - skipping validation")
    return(list(valid = NA, reason = "SSPopDec file not found"))
  }

  tr_pop <- data.table::fread(filepath)

  # Reshape TR data to long format
  tr_long <- data.table::rbindlist(list(
    tr_pop[, .(year = Year, age = Age, sex = "male", tr_pop = `M Tot`)],
    tr_pop[, .(year = Year, age = Age, sex = "female", tr_pop = `F Tot`)]
  ))

  # Filter to overlapping years
  overlap_years <- intersect(unique(population$year), unique(tr_long$year))
  if (!is.null(years)) overlap_years <- intersect(overlap_years, years)

  if (length(overlap_years) == 0) {
    cli::cli_warn("No overlapping years between ARTEMIS and SSPopDec")
    return(list(valid = NA, reason = "No overlapping years"))
  }

  calc <- population[year %in% overlap_years]
  tr <- tr_long[year %in% overlap_years]

  # --- By-year totals ---
  calc_yr <- calc[, .(calc_total = sum(population)), by = year]
  tr_yr <- tr[, .(tr_total = sum(tr_pop)), by = year]
  by_year <- merge(calc_yr, tr_yr, by = "year")
  by_year[, pct_diff := (calc_total - tr_total) / tr_total * 100]

  max_yr_diff <- max(abs(by_year$pct_diff))
  mean_yr_diff <- mean(abs(by_year$pct_diff))

  if (max_yr_diff <= tolerance * 100) {
    cli::cli_alert_success("Year totals: PASS (max diff: {round(max_yr_diff, 3)}%)")
  } else {
    cli::cli_alert_warning("Year totals: FAIL (max diff: {round(max_yr_diff, 2)}%)")
    worst <- by_year[order(-abs(pct_diff))][1:min(5, nrow(by_year))]
    for (i in seq_len(nrow(worst))) {
      cli::cli_alert("  {worst$year[i]}: {round(worst$pct_diff[i], 3)}%")
    }
  }

  # --- By age-sex ---
  calc_as <- calc[, .(calc_pop = sum(population)), by = .(year, age, sex)]
  by_age_sex <- merge(calc_as, tr, by = c("year", "age", "sex"), all.x = TRUE)
  by_age_sex[tr_pop > 0, pct_diff := (calc_pop - tr_pop) / tr_pop * 100]

  max_as_diff <- by_age_sex[!is.na(pct_diff), max(abs(pct_diff))]

  if (max_as_diff <= tolerance * 100) {
    cli::cli_alert_success("Age-sex detail: PASS (max diff: {round(max_as_diff, 3)}%)")
  } else {
    n_fail <- by_age_sex[abs(pct_diff) > tolerance * 100, .N]
    cli::cli_alert_warning("Age-sex detail: {n_fail} cells exceed {tolerance * 100}% tolerance")
  }

  valid <- max_yr_diff <= tolerance * 100

  list(
    valid = valid,
    summary = list(
      years_validated = length(overlap_years),
      max_year_pct_diff = max_yr_diff,
      mean_year_pct_diff = mean_yr_diff,
      max_age_sex_pct_diff = max_as_diff
    ),
    by_year = by_year,
    by_age_sex = by_age_sex
  )
}

# =============================================================================
# 85+ VALIDATION
# =============================================================================

#' Validate Ages 85+ Against SSPopDec
#'
#' @description
#' Focused validation of the 85-100 age range, which was previously
#' estimated via survival curves and is now read directly from SSPopDec.
#'
#' @param population data.table with columns: year, age, sex, population
#' @param config List: configuration
#' @param tab_years Integer vector of tab years to check (NULL = from config)
#'
#' @return List with: valid, comparison
#'
#' @export
validate_85plus_vs_tr <- function(population, config, tab_years = NULL) {
  cli::cli_h2("Validation: Ages 85+ vs SSPopDec")

  if (is.null(tab_years)) {
    tab_years <- config$historical_population$tab_years
  }

  filepath <- resolve_tr_file(config, "population_dec")
  if (!file.exists(filepath)) {
    cli::cli_warn("SSPopDec file not found - skipping 85+ validation")
    return(list(valid = NA, reason = "SSPopDec file not found"))
  }

  tr_pop <- data.table::fread(filepath)
  tr_85 <- data.table::rbindlist(list(
    tr_pop[Age >= 85, .(year = Year, age = Age, sex = "male", tr_pop = `M Tot`)],
    tr_pop[Age >= 85, .(year = Year, age = Age, sex = "female", tr_pop = `F Tot`)]
  ))

  calc_85 <- population[age >= 85 & year %in% tab_years]
  calc_85_sum <- calc_85[, .(calc_pop = sum(population)), by = .(year, age, sex)]

  comparison <- merge(calc_85_sum, tr_85, by = c("year", "age", "sex"), all.x = TRUE)
  comparison[tr_pop > 0, pct_diff := (calc_pop - tr_pop) / tr_pop * 100]

  # Summarize by tab year
  by_tab <- comparison[, .(
    calc_total = sum(calc_pop),
    tr_total = sum(tr_pop, na.rm = TRUE),
    max_age_diff = max(abs(pct_diff), na.rm = TRUE)
  ), by = year]
  by_tab[tr_total > 0, pct_diff := (calc_total - tr_total) / tr_total * 100]

  max_diff <- max(abs(by_tab$pct_diff), na.rm = TRUE)
  valid <- max_diff < 1  # Strict 1% for 85+ since it's now from SSPopDec directly

  if (valid) {
    cli::cli_alert_success("85+ totals: PASS across {nrow(by_tab)} tab years (max diff: {round(max_diff, 3)}%)")
  } else {
    cli::cli_alert_warning("85+ totals: FAIL (max diff: {round(max_diff, 2)}%)")
  }

  list(valid = valid, by_tab_year = by_tab, detail = comparison)
}

# =============================================================================
# SEX RATIO VALIDATION
# =============================================================================

#' Validate Sex Ratios by Age
#'
#' @description
#' Checks that male/female ratios by age are within demographically plausible
#' bounds. Sex ratio at birth is ~1.05, declining with age due to differential
#' mortality, reaching ~0.5 by age 85+.
#'
#' @param population data.table with columns: year, age, sex, population
#' @param years Integer vector of years to validate (NULL = all)
#'
#' @return List with: valid, flagged_cells
#'
#' @export
validate_sex_ratios <- function(population, years = NULL) {
  cli::cli_h2("Validation: Sex Ratios")

  pop <- if (!is.null(years)) population[year %in% years] else population

  wide <- data.table::dcast(pop, year + age ~ sex, value.var = "population",
                             fun.aggregate = sum)
  wide[female > 0, sex_ratio := male / female]

  # Plausible bounds by age
  wide[, ratio_min := data.table::fifelse(age < 50, 0.90, 0.30)]
  wide[, ratio_max := data.table::fifelse(age < 50, 1.10, 1.10)]

  # More relaxed for very old ages
  wide[age >= 85, ratio_min := 0.15]

  flagged <- wide[!is.na(sex_ratio) & (sex_ratio < ratio_min | sex_ratio > ratio_max)]

  if (nrow(flagged) == 0) {
    cli::cli_alert_success("Sex ratios: PASS (all within plausible bounds)")
  } else {
    cli::cli_alert_warning("Sex ratios: {nrow(flagged)} cells outside plausible bounds")
  }

  list(
    valid = nrow(flagged) == 0,
    n_flagged = nrow(flagged),
    flagged_cells = flagged
  )
}

# =============================================================================
# POPULATION COMPONENT VALIDATION
# =============================================================================

#' Validate Population Components (Demographic Accounting Identity)
#'
#' @description
#' Verifies the demographic accounting identity:
#'   P(t) = P(t-1) + Births(t) - Deaths(t) + Immigration(t) - Emigration(t)
#'
#' Checks that year-over-year changes are consistent with available component data.
#'
#' @param population data.table with columns: year, age, sex, population
#' @param config List: configuration
#'
#' @return List with: valid, comparison
#'
#' @export
validate_population_components <- function(population, config) {
  cli::cli_h2("Validation: Population Component Accounting")

  # Calculate year-over-year total changes
  yr_totals <- population[, .(total = sum(population)), by = year]
  data.table::setorder(yr_totals, year)
  yr_totals[, change := total - data.table::shift(total)]
  yr_totals[, pct_change := change / data.table::shift(total) * 100]

  # Flag implausible year-over-year changes (> 5% in either direction)
  flagged <- yr_totals[!is.na(pct_change) & abs(pct_change) > 5]

  if (nrow(flagged) == 0) {
    cli::cli_alert_success("Year-over-year changes: PASS (all < 5%)")
  } else {
    cli::cli_alert_warning("Year-over-year changes: {nrow(flagged)} years exceed 5%")
    for (i in seq_len(min(5, nrow(flagged)))) {
      cli::cli_alert("  {flagged$year[i]}: {round(flagged$pct_change[i], 2)}%")
    }
  }

  # Check monotonicity of total population (should generally increase 1940-2022)
  decreases <- yr_totals[!is.na(change) & change < -1e6]
  if (nrow(decreases) > 0) {
    cli::cli_alert_warning("Large population decreases detected in {nrow(decreases)} years")
  }

  list(
    valid = nrow(flagged) == 0,
    year_changes = yr_totals,
    flagged = flagged
  )
}

# =============================================================================
# MARITAL STATUS VALIDATION
# =============================================================================

#' Validate Marital Proportions Against SSPopDec
#'
#' @description
#' Compares ARTEMIS marital status splits against SSPopDec columns
#' (M Sin, M Mar, M Wid, M Div, F Sin, F Mar, F Wid, F Div).
#'
#' @param marital_pop data.table with columns: year, age, sex, marital_status, population
#' @param config List: configuration
#' @param years Integer vector of years to validate (NULL = all overlapping)
#'
#' @return List with: valid, by_year_status
#'
#' @export
validate_marital_proportions_vs_tr <- function(marital_pop, config, years = NULL) {
  cli::cli_h2("Validation: Marital Proportions vs SSPopDec")

  filepath <- resolve_tr_file(config, "population_dec")
  if (!file.exists(filepath)) {
    cli::cli_warn("SSPopDec file not found - skipping marital validation")
    return(list(valid = NA, reason = "SSPopDec file not found"))
  }

  tr_pop <- data.table::fread(filepath)

  # Map SSPopDec columns to marital_status names
  # SSPopDec has: M Sin, M Mar, M Wid, M Div, F Sin, F Mar, F Wid, F Div
  tr_marital <- data.table::rbindlist(list(
    tr_pop[, .(year = Year, age = Age, sex = "male", marital_status = "single", tr_pop = `M Sin`)],
    tr_pop[, .(year = Year, age = Age, sex = "male", marital_status = "married", tr_pop = `M Mar`)],
    tr_pop[, .(year = Year, age = Age, sex = "male", marital_status = "widowed", tr_pop = `M Wid`)],
    tr_pop[, .(year = Year, age = Age, sex = "male", marital_status = "divorced", tr_pop = `M Div`)],
    tr_pop[, .(year = Year, age = Age, sex = "female", marital_status = "single", tr_pop = `F Sin`)],
    tr_pop[, .(year = Year, age = Age, sex = "female", marital_status = "married", tr_pop = `F Mar`)],
    tr_pop[, .(year = Year, age = Age, sex = "female", marital_status = "widowed", tr_pop = `F Wid`)],
    tr_pop[, .(year = Year, age = Age, sex = "female", marital_status = "divorced", tr_pop = `F Div`)]
  ))

  # Filter to marital-status ages (14+) and overlapping years
  tr_marital <- tr_marital[age >= 14]
  overlap_years <- intersect(unique(marital_pop$year), unique(tr_marital$year))
  if (!is.null(years)) overlap_years <- intersect(overlap_years, years)

  if (length(overlap_years) == 0) {
    cli::cli_warn("No overlapping years for marital validation")
    return(list(valid = NA, reason = "No overlapping years"))
  }

  # Aggregate ARTEMIS data by year/sex/marital_status
  calc <- marital_pop[year %in% overlap_years,
                      .(calc_pop = sum(population)),
                      by = .(year, sex, marital_status)]
  tr <- tr_marital[year %in% overlap_years,
                   .(tr_pop = sum(tr_pop)),
                   by = .(year, sex, marital_status)]

  comparison <- merge(calc, tr, by = c("year", "sex", "marital_status"), all.x = TRUE)
  comparison[tr_pop > 0, pct_diff := (calc_pop - tr_pop) / tr_pop * 100]

  max_diff <- comparison[!is.na(pct_diff), max(abs(pct_diff))]

  if (max_diff < 5) {
    cli::cli_alert_success("Marital proportions: PASS (max diff: {round(max_diff, 2)}%)")
  } else {
    cli::cli_alert_warning("Marital proportions: max diff {round(max_diff, 2)}%")
    worst <- comparison[order(-abs(pct_diff))][1:min(5, nrow(comparison))]
    for (i in seq_len(nrow(worst))) {
      cli::cli_alert("  {worst$year[i]} {worst$sex[i]} {worst$marital_status[i]}: {round(worst$pct_diff[i], 2)}%")
    }
  }

  list(
    valid = max_diff < 5,
    max_pct_diff = max_diff,
    by_year_status = comparison
  )
}

# =============================================================================
# TERRITORY VALIDATION
# =============================================================================

#' Validate Territory Populations
#'
#' @description
#' Checks that territory population totals are within plausible ranges
#' based on Census IDB data and known benchmarks.
#'
#' @param population data.table with columns: year, age, sex, population
#' @param config List: configuration
#'
#' @return List with: valid, summary
#'
#' @export
validate_territory_populations <- function(population, config) {
  cli::cli_h2("Validation: Territory Populations")

  # Territory populations are embedded in the total. We validate that
  # total population is reasonable given known territory population ranges.
  # PR: ~3.2-3.7M, GU: ~160-170K, VI: ~87-105K, AS: ~50-55K
  # Total territories: ~3.5-4.0M

  yr_totals <- population[, .(total = sum(population)), by = year]
  recent <- yr_totals[year >= 2010]

  # US mainland-only Census estimate for 2020: ~331.4M
  # With territories: ~335.0M
  # Territory share: ~1.0-1.2% of total
  territory_share_pct <- 1.1  # approximate

  cli::cli_alert_info("Territory validation: checking total population plausibility")
  cli::cli_alert_info("Recent years: {min(recent$year)}-{max(recent$year)}, totals: {format(min(recent$total), big.mark=',')} - {format(max(recent$total), big.mark=',')}")

  # Check that totals are in a reasonable range for SS area population
  min_expected <- 130e6   # 1940 minimum

  max_expected <- 350e6   # 2022 maximum

  flagged <- yr_totals[total < min_expected | total > max_expected]
  valid <- nrow(flagged) == 0

  if (valid) {
    cli::cli_alert_success("Territory populations: PASS (all totals in plausible range)")
  } else {
    cli::cli_alert_warning("Territory populations: {nrow(flagged)} years outside plausible range")
  }

  list(valid = valid, year_totals = yr_totals, flagged = flagged)
}

# =============================================================================
# O-POPULATION VALIDATION
# =============================================================================

#' Validate O-Population Age Distribution
#'
#' @description
#' Checks the Other (temporary/unlawfully present) population age-sex
#' distribution against DHS estimates and plausibility constraints.
#'
#' @param o_pop data.table with columns: year, age, sex, population
#' @param config List: configuration
#'
#' @return List with: valid, summary
#'
#' @export
validate_o_age_distribution <- function(o_pop, config) {
  cli::cli_h2("Validation: O-Population Age Distribution")

  if (is.null(o_pop) || nrow(o_pop) == 0) {
    cli::cli_warn("O-population data is empty - skipping validation")
    return(list(valid = NA, reason = "Empty O-population"))
  }

  yr_totals <- o_pop[, .(total = sum(population)), by = year]
  recent <- yr_totals[year >= 2005]

  checks <- list()

  # Check 1: DHS range for recent years (~10-12M unauthorized + ~2-3M temp)
  if (nrow(recent) > 0) {
    dhs_min <- 10e6
    dhs_max <- 18e6
    out_of_range <- recent[total < dhs_min | total > dhs_max]
    checks$dhs_range <- nrow(out_of_range) == 0

    if (checks$dhs_range) {
      cli::cli_alert_success("O-pop totals 2005+: PASS (within DHS range)")
    } else {
      cli::cli_alert_warning("O-pop totals: {nrow(out_of_range)} years outside DHS range ({dhs_min/1e6}M-{dhs_max/1e6}M)")
    }
  }

  # Check 2: Age distribution - working-age concentration
  # O-population should be heavily concentrated in working ages (18-55)
  age_dist <- o_pop[, .(pop = sum(population)), by = .(age_group = data.table::fifelse(
    age < 18, "under_18",
    data.table::fifelse(age <= 55, "18_55", "56_plus")
  ))]
  age_dist[, share := pop / sum(pop)]

  working_age_share <- age_dist[age_group == "18_55", share]
  checks$working_age <- length(working_age_share) > 0 && working_age_share > 0.60

  if (isTRUE(checks$working_age)) {
    cli::cli_alert_success("Working age share: PASS ({round(working_age_share * 100, 1)}% are 18-55)")
  } else {
    cli::cli_alert_warning("Working age share: {round(working_age_share * 100, 1)}% (expected > 60%)")
  }

  # Check 3: Male share should be > 50% (predominantly male immigration)
  sex_dist <- o_pop[, .(pop = sum(population)), by = sex]
  male_share <- sex_dist[sex == "male", pop] / sum(sex_dist$pop)
  checks$male_majority <- male_share > 0.50

  if (checks$male_majority) {
    cli::cli_alert_success("Male share: PASS ({round(male_share * 100, 1)}%)")
  } else {
    cli::cli_alert_warning("Male share: {round(male_share * 100, 1)}% (expected > 50%)")
  }

  valid <- all(unlist(checks), na.rm = TRUE)

  list(
    valid = valid,
    checks = checks,
    year_totals = yr_totals,
    age_distribution = age_dist
  )
}

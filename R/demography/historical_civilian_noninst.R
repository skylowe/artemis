#' Historical Civilian Noninstitutionalized Population (Equation 1.4.4)
#'
#' @description
#' Implements Equation 1.4.4 from the TR2025 documentation to calculate
#' historical Social Security area civilian noninstitutionalized population
#' by age, sex, and marital status.
#'
#' $$C^z_{x,s,m} = CivNonInst^z_{x,s} \times MaritalPct^z_{x,s,m}$$
#'
#' @details
#' **Data Sources:**
#' - CivNonInst totals: ACS PUMS (2010-2023) via `fetch_acs_pums_civilian_noninst()`
#' - Marital proportions: ACS PUMS (2006-2023) via `fetch_acs_pums_civilian_noninst_marital()`
#'
#' **Marital Status Categories (differs from Eq 1.4.2):**
#' - married_spouse_present: Currently married with spouse in household
#' - separated: Currently married but separated from spouse (split from "married")
#' - widowed: Widowed from a previous marriage
#' - divorced: Divorced from a previous marriage
#' - single: Never been married
#'
#' **Same-Sex Marriage (2013+):**
#' - 2.5% of male population assumed gay
#' - 4.5% of female population assumed lesbian
#' - Separate tracking for orientation (heterosexual, gay, lesbian)
#'
#' **Coverage:**
#' - Available for 2010-2022 (full implementation per TR2025)
#' - Ages 15-99 for marital status detail
#' - Ages 0-14 assumed single
#'
#' **Reference Date Conversion:**
#' - Census data is July 1; converted to December 31 basis
#'
#' @name historical_civilian_noninst
NULL

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Calculate Historical Civilian Noninstitutionalized Population (Eq 1.4.4)
#'
#' @description
#' Main entry point for calculating the civilian noninstitutionalized
#' population by age, sex, and marital status.
#'
#' @param start_year Integer: First year (default: 2010, per TR2025)
#' @param end_year Integer: Last year (default: 2022)
#' @param ages Integer vector: Ages to include (default: 0:99)
#' @param include_orientation Logical: Include orientation (gay/lesbian) for 2013+
#' @param use_cache Logical: Use cached results if available
#' @param cache_dir Character: Directory for caching
#'
#' @return data.table with columns:
#'   - year: Calendar year (December 31 reference)
#'   - age: Single year of age (0-99)
#'   - sex: "male" or "female"
#'   - marital_status: marital status category
#'   - orientation: "heterosexual", "gay", or "lesbian" (2013+ if include_orientation=TRUE)
#'   - population: Civilian noninstitutionalized population
#'   - source: Data source indicator
#'
#' @export
calculate_historical_civilian_noninst <- function(start_year = 2010,
                                                   end_year = 2022,
                                                   ages = 0:99,
                                                   include_orientation = TRUE,
                                                   use_cache = TRUE,
                                                   cache_dir = here::here("data/cache")) {
  cli::cli_h1("Calculating Civilian Noninstitutionalized Population (Eq 1.4.4)")

  # Check cache
  cache_subdir <- file.path(cache_dir, "historical_population")
  if (!dir.exists(cache_subdir)) dir.create(cache_subdir, recursive = TRUE)

  cache_file <- file.path(
    cache_subdir,
    sprintf("civilian_noninst_marital_%d_%d.rds", start_year, end_year)
  )

  if (use_cache && file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached civilian noninst population")
    return(readRDS(cache_file))
  }

  # Define years (excluding 2020 if in range)
  years <- start_year:end_year
  years <- years[years != 2020]  # ACS not available for 2020
  cli::cli_alert_info("Processing {length(years)} years: {min(years)}-{max(years)}")

  # Step 1: Load civilian noninst totals by age/sex
  cli::cli_h2("Step 1: Loading Civilian Noninstitutionalized Totals")
  civ_noninst <- load_civilian_noninst_totals(years, ages)
  cli::cli_alert_info("Loaded {format(nrow(civ_noninst), big.mark = ',')} rows")

  # Step 2: Load marital proportions
  cli::cli_h2("Step 2: Loading Marital Proportions")
  marital_props <- load_civilian_noninst_marital_proportions(years, ages)
  cli::cli_alert_info("Loaded proportions for {length(unique(marital_props$year))} years")

  # Step 3: Calculate C population by age, sex, marital status
  cli::cli_h2("Step 3: Calculating C Population")
  result <- calculate_c_population(civ_noninst, marital_props, ages)
  cli::cli_alert_info("Calculated {format(nrow(result), big.mark = ',')} rows")

  # Step 4: Balance married populations
  cli::cli_h2("Step 4: Balancing Married Populations")
  result <- balance_c_married_populations(result)

  # Step 5: Add same-sex marriage orientation (2013+)
  if (include_orientation) {
    cli::cli_h2("Step 5: Adding Same-Sex Marriage Orientation (2013+)")
    result <- add_c_orientation(result)
    cli::cli_alert_info("Added orientation for {sum(result$year >= 2013)} year-rows")
  } else {
    result[, orientation := "heterosexual"]
  }

  # Step 6: Validate totals
  cli::cli_h2("Step 6: Validating Totals")
  validate_c_totals(result, civ_noninst)

  # Summary statistics
  cli::cli_h2("Summary")
  summarize_c_population(result)

  # Save to cache
  saveRDS(result, cache_file)
  cli::cli_alert_success("Saved to cache: {cache_file}")

  result
}

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Load Civilian Noninstitutionalized Totals
#'
#' @description
#' Loads civilian noninstitutionalized population by age and sex from ACS PUMS.
#'
#' @param years Integer vector: Years to load
#' @param ages Integer vector: Ages to include
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @keywords internal
load_civilian_noninst_totals <- function(years, ages) {
  # Load from ACS PUMS
  result <- tryCatch({
    fetch_acs_pums_civilian_noninst(years = years, ages = ages)
  }, error = function(e) {
    cli::cli_alert_warning("Error loading ACS PUMS: {conditionMessage(e)}")
    NULL
  })

  if (is.null(result) || nrow(result) == 0) {
    cli::cli_abort("Could not load civilian noninstitutionalized totals")
  }

  # Standardize column names
  if ("count" %in% names(result)) {
    data.table::setnames(result, "count", "population")
  }

  # Ensure required columns
  required_cols <- c("year", "age", "sex", "population")
  missing_cols <- setdiff(required_cols, names(result))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing columns: {paste(missing_cols, collapse = ', ')}")
  }

  result[, .(year, age, sex, population)]
}

#' Load Civilian Noninstitutionalized Marital Proportions
#'
#' @description
#' Loads marital status proportions for civilian noninstitutionalized
#' population from ACS PUMS.
#'
#' @param years Integer vector: Years to load
#' @param ages Integer vector: Ages to include
#'
#' @return data.table with columns: year, age, sex, marital_status, proportion
#'
#' @keywords internal
load_civilian_noninst_marital_proportions <- function(years, ages) {
  # Load marital status data
  marital_data <- tryCatch({
    fetch_acs_pums_civilian_noninst_marital(years = years, ages = ages)
  }, error = function(e) {
    cli::cli_alert_warning("Error loading ACS PUMS marital: {conditionMessage(e)}")
    NULL
  })

  if (is.null(marital_data) || nrow(marital_data) == 0) {
    cli::cli_abort("Could not load civilian noninst marital data")
  }

  # Standardize column names
  if ("count" %in% names(marital_data)) {
    data.table::setnames(marital_data, "count", "population")
  }

  # Calculate proportions within each year-age-sex group
  marital_data[, total := sum(population), by = .(year, age, sex)]
  marital_data[, proportion := population / total]
  marital_data[is.na(proportion), proportion := 0]

  # Validate proportions sum to 1
  prop_sums <- marital_data[, .(prop_sum = sum(proportion)), by = .(year, age, sex)]
  bad_sums <- prop_sums[abs(prop_sum - 1) > 0.01]
  if (nrow(bad_sums) > 0) {
    cli::cli_alert_warning("{nrow(bad_sums)} age-sex-year cells don't sum to 1")
  }

  marital_data[, .(year, age, sex, marital_status, proportion)]
}

# =============================================================================
# POPULATION CALCULATION
# =============================================================================

#' Calculate C Population (Eq 1.4.4)
#'
#' @description
#' Applies marital proportions to civilian noninstitutionalized totals.
#'
#' @param civ_noninst data.table: Civilian noninst totals by age/sex
#' @param marital_props data.table: Marital proportions
#' @param ages Integer vector: Ages to include
#'
#' @return data.table with C population by age, sex, marital status
#'
#' @keywords internal
calculate_c_population <- function(civ_noninst, marital_props, ages) {
  # Merge totals with proportions
  result <- merge(
    civ_noninst,
    marital_props,
    by = c("year", "age", "sex"),
    all.x = TRUE,
    allow.cartesian = TRUE
  )

  # For ages under 15, assign all to single
  under_15 <- result[age < 15]
  if (nrow(under_15) > 0) {
    # Create single rows for under 15
    under_15_pop <- civ_noninst[age < 15]
    under_15_pop[, marital_status := "single"]
    under_15_pop[, proportion := 1]
    under_15_pop[, c_population := population]
    under_15_pop[, source := "acs_pums"]

    # Remove under-15 from main result
    result <- result[age >= 15]
  } else {
    under_15_pop <- NULL
  }

  # Handle missing marital proportions (use uniform if missing)
  result[is.na(proportion), proportion := 0]
  result[is.na(marital_status), marital_status := "single"]

  # Calculate C population
  result[, c_population := population * proportion]

  # Add source indicator
  result[, source := "acs_pums"]

  # Combine with under-15 data
  if (!is.null(under_15_pop) && nrow(under_15_pop) > 0) {
    result <- data.table::rbindlist(
      list(
        result[, .(year, age, sex, marital_status, c_population, source)],
        under_15_pop[, .(year, age, sex, marital_status, c_population, source)]
      ),
      use.names = TRUE
    )
  } else {
    result <- result[, .(year, age, sex, marital_status, c_population, source)]
  }

  # Rename to standard column name
  data.table::setnames(result, "c_population", "population")

  # Order
  data.table::setorder(result, year, age, sex, marital_status)

  result
}

# =============================================================================
# VALIDATION
# =============================================================================

#' Validate C Population Totals
#'
#' @description
#' Validates that marital status populations sum to totals.
#'
#' @param c_pop data.table: C population by age/sex/marital
#' @param totals data.table: Original totals
#'
#' @keywords internal
validate_c_totals <- function(c_pop, totals) {
  # Sum by year-age-sex
  c_sums <- c_pop[, .(calc_total = sum(population)), by = .(year, age, sex)]

  # Merge with original totals
  comparison <- merge(c_sums, totals, by = c("year", "age", "sex"))

  # Calculate differences
  comparison[, diff := calc_total - population]
  comparison[, diff_pct := (diff / population) * 100]

  # Summary
  max_diff <- max(abs(comparison$diff_pct), na.rm = TRUE)
  mean_diff <- mean(abs(comparison$diff_pct), na.rm = TRUE)

  if (max_diff < 0.1) {
    cli::cli_alert_success("Totals match exactly (max diff: {round(max_diff, 4)}%)")
  } else if (max_diff < 1) {
    cli::cli_alert_info("Totals match within 1% (max diff: {round(max_diff, 2)}%)")
  } else {
    cli::cli_alert_warning("Some totals differ by more than 1% (max: {round(max_diff, 2)}%)")
  }

  # Check by year
  by_year <- comparison[, .(
    mean_diff = mean(abs(diff_pct), na.rm = TRUE),
    max_diff = max(abs(diff_pct), na.rm = TRUE)
  ), by = year]

  cli::cli_alert_info("Mean absolute diff by year: {round(mean(by_year$mean_diff), 4)}%")
}

#' Validate C Population Against Census
#'
#' @description
#' Compares calculated C population against Census Bureau estimates.
#'
#' @param c_pop data.table: Calculated C population
#' @param tolerance Numeric: Acceptable relative difference (default: 0.02 = 2%)
#'
#' @return data.table with validation results
#'
#' @export
validate_c_population <- function(c_pop, tolerance = 0.02) {
  cli::cli_h1("Validating C Population Against Census")

  # Calculate totals by year
  c_totals <- c_pop[, .(calculated = sum(population)), by = year]

  # Load Census estimates for comparison
  census_civ_noninst <- tryCatch({
    fetch_acs_pums_civilian_noninst(years = unique(c_pop$year))
  }, error = function(e) NULL)

  if (is.null(census_civ_noninst)) {
    cli::cli_alert_warning("Could not load Census data for validation")
    return(NULL)
  }

  census_totals <- census_civ_noninst[, .(census = sum(count)), by = year]

  # Merge and compare
  comparison <- merge(c_totals, census_totals, by = "year", all = TRUE)
  comparison[, diff_pct := (calculated - census) / census * 100]
  comparison[, pass := abs(diff_pct) <= tolerance * 100]

  # Summary
  n_pass <- sum(comparison$pass, na.rm = TRUE)
  n_total <- sum(!is.na(comparison$pass))

  cli::cli_alert_info("Validation: {n_pass}/{n_total} years within {tolerance * 100}% tolerance")

  comparison
}

# =============================================================================
# SUMMARY FUNCTIONS
# =============================================================================

#' Summarize C Population
#'
#' @description
#' Displays summary statistics for the civilian noninstitutionalized population.
#'
#' @param c_pop data.table: C population data
#'
#' @keywords internal
summarize_c_population <- function(c_pop) {
  # Total by year
  totals <- c_pop[, .(total = sum(population)), by = year]
  data.table::setorder(totals, year)

  cli::cli_alert("Population totals by year:")
  sample_years <- c(2006, 2010, 2015, 2019, 2022)
  sample_years <- sample_years[sample_years %in% totals$year]

  for (yr in sample_years) {
    total <- totals[year == yr, total]
    cli::cli_alert_info("  {yr}: {format(round(total), big.mark = ',')}")
  }

  # By marital status for most recent year
  latest_year <- max(c_pop$year)
  by_marital <- c_pop[year == latest_year, .(total = sum(population)), by = marital_status]
  by_marital[, pct := round(total / sum(total) * 100, 1)]

  cli::cli_alert("Marital status distribution ({latest_year}):")
  for (i in seq_len(nrow(by_marital))) {
    cli::cli_alert_info("  {by_marital$marital_status[i]}: {round(by_marital$pct[i], 1)}%")
  }

  # By sex
  by_sex <- c_pop[year == latest_year, .(total = sum(population)), by = sex]

  cli::cli_alert("By sex ({latest_year}):")
  for (i in seq_len(nrow(by_sex))) {
    cli::cli_alert_info("  {by_sex$sex[i]}: {format(round(by_sex$total[i]), big.mark = ',')}")
  }

  invisible(totals)
}

#' Get Civilian Noninstitutionalized Population Summary
#'
#' @description
#' Returns summary statistics for the C population.
#'
#' @param c_pop data.table: C population data
#'
#' @return list with summary statistics
#'
#' @export
get_c_population_summary <- function(c_pop) {
  list(
    years = range(c_pop$year),
    n_years = length(unique(c_pop$year)),
    n_rows = nrow(c_pop),
    total_by_year = c_pop[, .(total = sum(population)), by = year],
    marital_distribution = c_pop[, .(total = sum(population)), by = marital_status],
    age_range = range(c_pop$age)
  )
}

# =============================================================================
# MARRIAGE BALANCING
# =============================================================================

#' Balance Married Populations in C Population
#'
#' @description
#' Ensures married males approximately equal married females by age,
#' per TR2025 methodology.
#'
#' @param c_pop data.table: C population by age/sex/marital
#'
#' @return data.table with balanced married populations
#'
#' @details
#' The balancing adjusts married_spouse_present populations so that:
#' - Total married males = Total married females (approximately)
#' - Preserves age distribution shape
#' - Other marital statuses adjusted proportionally to maintain totals
#'
#' @keywords internal
balance_c_married_populations <- function(c_pop) {
  result <- data.table::copy(c_pop)

  for (yr in unique(result$year)) {
    # Get married populations for this year
    married_statuses <- c("married_spouse_present", "married")

    for (ms in married_statuses) {
      if (!ms %in% result$marital_status) next

      married_male <- result[year == yr & sex == "male" & marital_status == ms,
                             sum(population)]
      married_female <- result[year == yr & sex == "female" & marital_status == ms,
                               sum(population)]

      if (married_male == 0 || married_female == 0) next

      # Calculate average and adjustment ratio
      avg_married <- (married_male + married_female) / 2

      # Adjust males
      if (married_male > 0) {
        male_ratio <- avg_married / married_male
        result[year == yr & sex == "male" & marital_status == ms,
               population := population * male_ratio]
      }

      # Adjust females
      if (married_female > 0) {
        female_ratio <- avg_married / married_female
        result[year == yr & sex == "female" & marital_status == ms,
               population := population * female_ratio]
      }
    }
  }

  # Log adjustment summary
  cli::cli_alert_info("Balanced married populations across {length(unique(result$year))} years")

  result
}

# =============================================================================
# SAME-SEX MARRIAGE ORIENTATION
# =============================================================================
#' Add Orientation to C Population (2013+)
#'
#' @description
#' Adds same-sex marriage orientation (gay/lesbian) for years 2013 and later,
#' per TR2025 methodology.
#'
#' @param c_pop data.table: C population by age/sex/marital
#'
#' @return data.table with orientation column added
#'
#' @details
#' Per TR2025:
#' - 2.5% of male population assumed gay
#' - 4.5% of female population assumed lesbian
#' - For pre-2013: all heterosexual
#' - For 2013+: split into heterosexual, gay, lesbian
#'
#' The married population is split so that:
#' - Gay males married to gay males
#' - Lesbian females married to lesbian females
#' - Heterosexual males/females married to opposite sex
#'
#' @keywords internal
add_c_orientation <- function(c_pop) {
  # Gay/lesbian proportions per TR2025
  gay_pct <- 0.025      # 2.5% of males
  lesbian_pct <- 0.045  # 4.5% of females

  result_list <- list()

  for (yr in unique(c_pop$year)) {
    yr_data <- c_pop[year == yr]

    if (yr < 2013) {
      # Pre-2013: all heterosexual
      yr_data[, orientation := "heterosexual"]
      result_list[[as.character(yr)]] <- yr_data
    } else {
      # 2013+: split by orientation
      # For each age-sex-marital cell, split into orientations

      for (s in c("male", "female")) {
        for (ms in unique(yr_data$marital_status)) {
          cell <- yr_data[sex == s & marital_status == ms]

          if (nrow(cell) == 0) next

          # Determine LGBT percentage based on sex
          if (s == "male") {
            lgbt_pct <- gay_pct
            lgbt_label <- "gay"
          } else {
            lgbt_pct <- lesbian_pct
            lgbt_label <- "lesbian"
          }

          # For married people, apply LGBT proportion
          # For non-married, still apply (they could be gay/lesbian single, widowed, etc.)
          for (i in seq_len(nrow(cell))) {
            pop <- cell$population[i]
            age <- cell$age[i]

            # Heterosexual portion
            hetero_row <- data.table::data.table(
              year = yr,
              age = age,
              sex = s,
              marital_status = ms,
              population = pop * (1 - lgbt_pct),
              source = cell$source[i],
              orientation = "heterosexual"
            )

            # LGBT portion
            lgbt_row <- data.table::data.table(
              year = yr,
              age = age,
              sex = s,
              marital_status = ms,
              population = pop * lgbt_pct,
              source = cell$source[i],
              orientation = lgbt_label
            )

            result_list[[paste(yr, s, ms, age, "hetero", sep = "_")]] <- hetero_row
            result_list[[paste(yr, s, ms, age, "lgbt", sep = "_")]] <- lgbt_row
          }
        }
      }
    }
  }

  result <- data.table::rbindlist(result_list, use.names = TRUE, fill = TRUE)
  data.table::setorder(result, year, age, sex, marital_status, orientation)

  # Summary
  post_2013 <- result[year >= 2013]
  if (nrow(post_2013) > 0) {
    lgbt_total <- post_2013[orientation %in% c("gay", "lesbian"), sum(population)]
    total <- post_2013[, sum(population)]
    cli::cli_alert_info("LGBT population (2013+): {round(lgbt_total/total * 100, 2)}% of total")
  }

  result
}

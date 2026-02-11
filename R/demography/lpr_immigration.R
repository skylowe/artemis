#' LPR Immigration Projection
#'
#' Functions for projecting LPR immigration by age and sex using:
#' - DHS data for age-sex distributions (with Beers interpolation to single years)
#' - Or TR2025-derived distribution (back-calculated from population projections)
#' - DHS data for NEW/AOS split ratio
#' - TR2025 assumptions for aggregate totals
#'
#' Required Outputs (per TR2025 Section 1.3):
#' - L: Total LPR Immigration
#' - NEW: New Arrivals
#' - AOS: Adjustments of Status
#' - E: Legal Emigration
#' - NL: Net LPR Immigration
#'
#' @name lpr_immigration
#'
#' @section Known Methodology Deviations from TR2025 Section 1.3:
#'
#' **Deviation 2 (Unfixable): Refugee/Asylee Reclassification**
#' TR2025 Step 1 reclassifies refugee and asylee LPR admissions from AOS to
#' NEW arrivals. DHS public Tables 8-11 provide only AOS vs NEW totals by age
#' group; refugee/asylee class-of-admission detail by age is not available in
#' public data. DHS provides this to SSA via non-public single-year-of-age data.
#' Impact is minor: refugees are a small and declining fraction of AOS.
#'
#' **Deviation 7 (Unfixable): SS Area Conversion Factors for Emigration**
#' TR2025 Section 1.3.c applies internal OCACT conversion factors to Census
#' emigration estimates so they correspond to people leaving the Social Security
#' area (not just the US). These factors are unpublished and unavailable.
#' CBO emigration data (our Phase 4 fix) is a reasonable approximation since
#' CBO's estimates are calibrated to residual demographic accounting.
#'
#' **Deviation 9 (Unfixable): DHS Single-Year-of-Age Data**
#' DHS provides SSA with non-public single-year-of-age LPR data. Public DHS
#' Tables 8-11 use 5-year age groups. We apply H.S. Beers ordinary interpolation
#' to convert to single years, which is the standard demographic solution and
#' the same method TR2025 uses for emigration distribution.
#'
#' **Deviation 10 (ARTEMIS-Specific): Elderly Override System**
#' The elderly override (ages 65+/85+/100+) is an ARTEMIS addition that
#' compensates for data limitations at advanced ages. It adjusts distribution
#' values for older ages where back-calculated or DHS-derived values are
#' unreliable. This is not part of TR2025 methodology.
NULL

# ===========================================================================
# IMMIGRATION AGE-SEX DISTRIBUTION
# ===========================================================================

#' Get immigration age-sex distribution
#'
#' @description
#' Returns the immigration age-sex distribution using one of two methods:
#'
#' 1. "dhs" - DHS expanded tables with Beers interpolation (actual LPR admissions)
#' 2. "tr_derived" - Distribution back-calculated from TR2025 population projections
#'
#' The TR-derived method ensures population projections align with TR2025.
#' The DHS method reflects actual admission patterns but differs from TR2025 implied.
#'
#' @param method Character: "dhs" or "tr_derived"
#' @param dhs_data data.table: DHS data from load_dhs_lpr_data() (for "dhs" method)
#' @param dhs_years Integer vector: years for DHS averaging (for "dhs" method)
#' @param tr_derived_data data.table: Pre-computed TR-derived distribution from
#'   calculate_tr_derived_distribution() (for "tr_derived" method, preferred)
#' @param tr_derived_file Character: path to TR2025-derived distribution CSV
#'   (for "tr_derived" method, fallback if tr_derived_data is NULL)
#' @param elderly_override List: override configuration for ages 85+ (for "dhs" method)
#' @param elderly_override_tr_derived List: override configuration for ages 85+ (for "tr_derived" method)
#' @param total_net_immigration Numeric: total annual net immigration (for overrides)
#'
#' @return data.table with columns: age, sex, distribution
#'
#' @export
get_immigration_distribution <- function(
    method = "dhs",
    dhs_data = NULL,
    dhs_years = 2016:2020,
    tr_derived_data = NULL,
    tr_derived_file = "data/processed/tr2025_implied_immigration_distribution.csv",
    elderly_override = NULL,
    elderly_override_tr_derived = NULL,
    total_net_immigration = NULL
) {

  if (method == "dhs") {
    # DHS-based distribution with optional elderly override
    if (is.null(dhs_data)) {
      cli::cli_abort("dhs_data required for DHS distribution method")
    }

    cli::cli_alert_info("Using DHS-derived immigration distribution")
    dist <- calculate_lpr_distribution_dhs(dhs_data, dhs_years)

    # Apply elderly override if enabled (for DHS method, this adjusts 85+)
    if (!is.null(elderly_override) && isTRUE(elderly_override$enabled)) {
      if (is.null(total_net_immigration)) {
        cli::cli_abort("total_net_immigration required when elderly_override is enabled")
      }
      dist <- apply_elderly_override(dist, elderly_override, total_net_immigration)
    }

  } else if (method == "tr_derived") {
    # TR2025-derived distribution
    cli::cli_alert_info("Using TR2025-derived immigration distribution")
    dist <- load_tr_derived_distribution(
      tr_derived_data = tr_derived_data,
      file_path = tr_derived_file
    )

    # Apply elderly override for ages 85+ (TR-derived has inflated values at older ages)
    if (!is.null(elderly_override_tr_derived) && isTRUE(elderly_override_tr_derived$enabled)) {
      if (is.null(total_net_immigration)) {
        cli::cli_abort("total_net_immigration required when elderly_override_tr_derived is enabled")
      }
      dist <- apply_elderly_override(dist, elderly_override_tr_derived, total_net_immigration)
    }

  } else {
    cli::cli_abort("Unknown distribution method: {method}. Use 'dhs' or 'tr_derived'.")
  }

  dist
}

#' Calculate TR2025-derived immigration distribution
#'
#' @description
#' Back-calculates the implied immigration distribution from TR2025 population
#' projections. Uses the cohort-component identity:
#'
#'   implied_NI = P(x,z) - P(x-1,z-1) × (1 - qx)
#'
#' Where:
#' - P(x,z) = population at age x in year z
#' - qx = probability of death at age x
#' - implied_NI = net immigration implied by population change
#'
#' @param tr_population data.table: TR2025 population with columns year, age, sex, population
#' @param tr_qx data.table: TR2025 death probabilities with columns year, age, sex, qx
#' @param years Integer vector: years to calculate (default 2023:2030)
#'
#' @return data.table with columns: age, sex, avg_implied, total_implied, implied_dist
#'
#' @export
calculate_tr_derived_distribution <- function(
    tr_population,
    tr_qx,
    years = 2023:2030
) {
  cli::cli_alert_info("Calculating TR2025-derived immigration distribution")

  # Ensure data.tables

tr_population <- data.table::as.data.table(tr_population)
  tr_qx <- data.table::as.data.table(tr_qx)

  # Calculate implied immigration for each year, age, sex
  implied_list <- list()

  for (yr in years) {
    for (a in 1:100) {
      for (s in c("male", "female")) {
        # Previous year's population at age-1
        pop_prev <- tr_population[year == yr - 1 & age == a - 1 & sex == s, population]
        # Current year's population at age
        pop_now <- tr_population[year == yr & age == a & sex == s, population]
        # Death probability at current age/year
        qx_val <- tr_qx[year == yr & age == a & sex == s, qx]

        if (length(pop_prev) == 1 && length(pop_now) == 1 && length(qx_val) == 1) {
          # Expected survivors from cohort aging
          survivors <- pop_prev * (1 - qx_val)
          # Implied net immigration = actual - expected
          implied <- pop_now - survivors

          implied_list[[length(implied_list) + 1]] <- data.table::data.table(
            year = yr,
            age = a,
            sex = s,
            implied_ni = implied
          )
        }
      }
    }
  }

  tr_implied <- data.table::rbindlist(implied_list)

  # Average implied immigration by age/sex across years
  tr_implied_avg <- tr_implied[, .(avg_implied = mean(implied_ni)), by = .(age, sex)]
  tr_implied_avg[, total_implied := sum(avg_implied)]
  tr_implied_avg[, implied_dist := avg_implied / total_implied]

  cli::cli_alert_success(
    "Calculated implied distribution for ages 1-100 (averaged over {length(years)} years)"
  )

  # Report key statistics
  total_implied <- tr_implied_avg[, sum(avg_implied)]
  pct_85_99 <- tr_implied_avg[age >= 85 & age < 100, sum(implied_dist)] * 100
  cli::cli_alert_info("Total implied annual immigration: {format(round(total_implied), big.mark=',')}")
  cli::cli_alert_info("Ages 85-99: {round(pct_85_99, 1)}% of total")

  tr_implied_avg[order(sex, age)]
}

#' Load TR2025-derived immigration distribution
#'
#' @description
#' Processes the TR2025-derived distribution (either from a pre-computed data.table
#' or from a CSV file) into the format needed for immigration projection.
#'
#' @param tr_derived_data data.table: Pre-computed TR-derived distribution from
#'   calculate_tr_derived_distribution(), or NULL to load from file
#' @param file_path Path to CSV file (used if tr_derived_data is NULL)
#'
#' @return data.table with columns: age, sex, distribution
#'
#' @keywords internal
load_tr_derived_distribution <- function(
    tr_derived_data = NULL,
    file_path = "data/processed/tr2025_implied_immigration_distribution.csv"
) {
  # Get the implied distribution data
if (!is.null(tr_derived_data)) {
    implied <- data.table::copy(tr_derived_data)
    cli::cli_alert_info("Using pre-computed TR-derived distribution")
  } else if (file.exists(file_path)) {
    implied <- data.table::fread(file_path)
    cli::cli_alert_info("Loaded TR-derived distribution from file")
  } else {
    cli::cli_abort("TR2025-derived distribution not available. Either provide tr_derived_data or ensure file exists: {file_path}")
  }

  # The data has: age, sex, avg_implied, total_implied, implied_dist
  # We need to transform this to: age, sex, distribution

  # Note: The data has ages 1-100, missing age 0
  # Also, age 100 values are artifacts from the back-calculation

  # Create distribution for ages 0-99 (exclude 100+ artifact)
  dist_0_99 <- implied[age < 100, .(age, sex, avg_implied)]

  # Handle missing age 0 by using same distribution as age 1
  age_1_vals <- dist_0_99[age == 1]
  age_0_vals <- data.table::data.table(
    age = 0L,
    sex = age_1_vals$sex,
    avg_implied = age_1_vals$avg_implied
  )
  dist_0_99 <- rbind(age_0_vals, dist_0_99)

  # Create placeholder for age 100+ (will be overridden)
  age_100_vals <- data.table::data.table(
    age = 100L,
    sex = c("male", "female"),
    avg_implied = c(0, 0)  # Placeholder, will be set by override
  )
  dist_full <- rbind(dist_0_99, age_100_vals)

  # Normalize to distribution (sum to 1) for ages 0-99
  total_0_99 <- dist_full[age < 100, sum(avg_implied)]
  dist_full[age < 100, distribution := avg_implied / total_0_99]
  dist_full[age >= 100, distribution := 0]  # Will be set by override

  cli::cli_alert_success("TR-derived distribution ready (ages 0-99, 100+ placeholder)")

  dist_full[, .(age, sex, distribution)]
}

#' Apply age 100+ override to distribution
#'
#' @description
#' Sets the age 100+ distribution values and renormalizes ages 0-99.
#' This is used for TR-derived distribution where 100+ values are artifacts.
#'
#' @param dist data.table: distribution with columns age, sex, distribution
#' @param override List: override configuration with annual_total and female_share
#' @param total_net_immigration Numeric: total annual net immigration
#'
#' @return data.table: adjusted distribution
#'
#' @keywords internal
apply_age_100_override <- function(dist, override, total_net_immigration) {
  dist <- data.table::copy(dist)

  # Calculate 100+ proportions
  target_100_total <- override$annual_total
  target_100_female <- target_100_total * override$female_share
  target_100_male <- target_100_total * (1 - override$female_share)

  prop_100_female <- target_100_female / total_net_immigration
  prop_100_male <- target_100_male / total_net_immigration

  # Scale ages 0-99 to make room for 100+
  current_0_99 <- dist[age < 100, sum(distribution)]
  target_0_99 <- 1 - (prop_100_female + prop_100_male)

  if (target_0_99 <= 0) {
    cli::cli_abort("Age 100+ override values exceed total immigration")
  }

  scale_0_99 <- target_0_99 / current_0_99

  cli::cli_alert_info(
    "Applying 100+ override: {round(target_100_total)} (scaling 0-99 by {round(scale_0_99, 4)})"
  )

  # Apply scale to ages 0-99
  dist[age < 100, distribution := distribution * scale_0_99]

  # Set age 100+
  dist[age >= 100 & sex == "female", distribution := prop_100_female]
  dist[age >= 100 & sex == "male", distribution := prop_100_male]

  # Verify distribution sums to 1

  total <- dist[, sum(distribution)]
  if (abs(total - 1.0) > 0.001) {
    cli::cli_warn("Distribution sums to {round(total, 6)} after 100+ override")
  }

  dist
}

#' Apply elderly immigration override to distribution
#'
#' @description
#' Adjusts the immigration distribution for ages 65+ based on TR2025 implied values.
#' The TR-derived distribution overestimates elderly immigration significantly:
#' - Ages 65-84: 2-3x higher than TR2025 implied
#' - Ages 85-99: ~7x higher than TR2025 implied
#' - Ages 100+: artifacts from open-ended age group
#'
#' @param dist data.table: base distribution with columns age, sex, distribution
#' @param override List: override configuration with ages_65_84, ages_85_99, age_100_plus
#' @param total_net_immigration Numeric: total annual net immigration
#'
#' @return data.table: adjusted distribution
#'
#' @keywords internal
apply_elderly_override <- function(dist, override, total_net_immigration) {
  dist <- data.table::copy(dist)

  # Extract override values
  ages_65_84 <- override$ages_65_84
  ages_85_99 <- override$ages_85_99
  age_100_plus <- override$age_100_plus

  if (is.null(ages_85_99) || is.null(age_100_plus)) {
    cli::cli_warn("Elderly override enabled but missing 85-99/100+ configuration, skipping")
    return(dist)
  }

  # Calculate target proportions for each elderly age group
  # ages_65_84 (optional - if not provided, scale with rest of 0-64)
  if (!is.null(ages_65_84)) {
    target_65_84_total <- ages_65_84$annual_total
    target_65_84_female <- target_65_84_total * ages_65_84$female_share
    target_65_84_male <- target_65_84_total * (1 - ages_65_84$female_share)
    prop_65_84_female <- target_65_84_female / total_net_immigration
    prop_65_84_male <- target_65_84_male / total_net_immigration
  } else {
    prop_65_84_female <- 0
    prop_65_84_male <- 0
    target_65_84_total <- 0
  }

  # ages_85_99
  target_85_99_total <- ages_85_99$annual_total
  target_85_99_female <- target_85_99_total * ages_85_99$female_share
  target_85_99_male <- target_85_99_total * (1 - ages_85_99$female_share)
  prop_85_99_female <- target_85_99_female / total_net_immigration
  prop_85_99_male <- target_85_99_male / total_net_immigration

  # age_100_plus
  target_100_total <- age_100_plus$annual_total
  target_100_female <- target_100_total * age_100_plus$female_share
  target_100_male <- target_100_total * (1 - age_100_plus$female_share)
  prop_100_female <- target_100_female / total_net_immigration
  prop_100_male <- target_100_male / total_net_immigration

  # Calculate scaling based on whether ages_65_84 is specified
  if (!is.null(ages_65_84)) {
    # Scale ages 0-64 to make room for all elderly overrides
    current_0_64 <- dist[age < 65, sum(distribution)]
    target_0_64 <- 1 - (prop_65_84_female + prop_65_84_male +
                         prop_85_99_female + prop_85_99_male +
                         prop_100_female + prop_100_male)

    if (target_0_64 <= 0) {
      cli::cli_abort("Elderly override values exceed total immigration")
    }

    scale_0_64 <- target_0_64 / current_0_64

    cli::cli_alert_info(
      "Applying elderly override: 65-84={round(target_65_84_total)}, 85-99={round(target_85_99_total)}, 100+={round(target_100_total)}"
    )
    cli::cli_alert_info(
      "Scaling ages 0-64 by {round(scale_0_64, 4)} ({round(current_0_64*100, 2)}% -> {round(target_0_64*100, 2)}%)"
    )

    # Apply scale to ages 0-64
    dist[age < 65, distribution := distribution * scale_0_64]

    # Distribute 65-84 uniformly (20 ages)
    n_ages_65_84 <- 20
    dist[age >= 65 & age < 85 & sex == "female", distribution := prop_65_84_female / n_ages_65_84]
    dist[age >= 65 & age < 85 & sex == "male", distribution := prop_65_84_male / n_ages_65_84]

  } else {
    # Original behavior: scale ages 0-84
    current_0_84 <- dist[age < 85, sum(distribution)]
    target_0_84 <- 1 - (prop_85_99_female + prop_85_99_male + prop_100_female + prop_100_male)

    if (target_0_84 <= 0) {
      cli::cli_abort("Elderly override values exceed total immigration")
    }

    scale_0_84 <- target_0_84 / current_0_84

    cli::cli_alert_info(
      "Applying elderly override: 85-99={round(target_85_99_total)}, 100+={round(target_100_total)}"
    )
    cli::cli_alert_info(
      "Scaling ages 0-84 by {round(scale_0_84, 4)} ({round(current_0_84*100, 2)}% -> {round(target_0_84*100, 2)}%)"
    )

    # Apply scale to ages 0-84
    dist[age < 85, distribution := distribution * scale_0_84]
  }

  # Distribute 85-99 uniformly (15 ages)
  n_ages_85_99 <- 15
  dist[age >= 85 & age < 100 & sex == "female", distribution := prop_85_99_female / n_ages_85_99]
  dist[age >= 85 & age < 100 & sex == "male", distribution := prop_85_99_male / n_ages_85_99]

  # Set age 100+
  dist[age >= 100 & sex == "female", distribution := prop_100_female]
  dist[age >= 100 & sex == "male", distribution := prop_100_male]

  # Verify distribution sums to 1
  total <- dist[, sum(distribution)]
  if (abs(total - 1.0) > 0.01) {
    cli::cli_warn("Distribution sums to {round(total, 4)} after elderly override")
  }

  # Report final distribution
  age_65_84_pct <- dist[age >= 65 & age < 85, sum(distribution)] * 100
  age_85_99_pct <- dist[age >= 85 & age < 100, sum(distribution)] * 100
  age100_pct <- dist[age >= 100, sum(distribution)] * 100
  cli::cli_alert_success(
    "Final distribution: 65-84={round(age_65_84_pct, 2)}%, 85-99={round(age_85_99_pct, 2)}%, 100+={round(age100_pct, 2)}%"
  )

  dist
}

# ===========================================================================
# DHS DATA LOADING AND DISTRIBUTION FUNCTIONS
# ===========================================================================

#' Load DHS LPR data
#'
#' @param file_path Path to DHS LPR data cache
#' @return data.table with DHS age-sex data
#' @export
load_dhs_lpr_data <- function(file_path = "data/cache/dhs_immigration/dhs_lpr_all_years.rds") {
  if (!file.exists(file_path)) {
    cli::cli_abort("DHS LPR data not found: {file_path}")
  }

  dhs_data <- readRDS(file_path)

  # Extract age_sex component if list
  if (is.list(dhs_data) && "age_sex" %in% names(dhs_data)) {
    dhs_data <- dhs_data$age_sex
  }

  data.table::as.data.table(dhs_data)
}

#' Calculate LPR immigration distribution from DHS data
#'
#' @description
#' Computes age-sex distribution for LPR immigration using DHS expanded tables.
#' DHS provides 5-year age groups which are interpolated to single years using
#' the Beers ordinary formula.
#'
#' @param dhs_data data.table from load_dhs_lpr_data()
#' @param reference_years Years to average for distribution (default: 2018:2023)
#' @return data.table with columns: age, sex, distribution
#' @export
calculate_lpr_distribution_dhs <- function(dhs_data,
                                           reference_years = 2016:2020,
                                           interpolation_method = "uniform") {
  dhs_data <- data.table::as.data.table(dhs_data)

  # Filter to reference years (combine AOS and new arrivals for total LPR)
  ref_data <- dhs_data[fiscal_year %in% reference_years]

  if (nrow(ref_data) == 0) {
    cli::cli_abort("No DHS data found for years {paste(reference_years, collapse=', ')}")
  }

  # Aggregate across years by age group and sex
  dist_5yr <- ref_data[, .(count = sum(count, na.rm = TRUE)), by = .(age_min, age_max, sex)]

  # Calculate distribution (still in 5-year groups)
  total <- sum(dist_5yr$count)
  dist_5yr[, distribution := count / total]

  # Report 5-year distribution summary
  by_sex <- dist_5yr[, .(pct = sum(distribution) * 100), by = sex]
  cli::cli_alert_info(
    "DHS 5-year distribution ({min(reference_years)}-{max(reference_years)}): {round(by_sex[sex=='female', pct], 1)}% female, {round(by_sex[sex=='male', pct], 1)}% male"
  )

  # Interpolate to single years
  interpolate_fn <- if (interpolation_method == "beers") {
    beers_interpolate_immigration
  } else {
    uniform_interpolate_dhs
  }
  dist_single <- interpolate_fn(dist_5yr)

  # Report single-year summary
  by_sex_single <- dist_single[, .(pct = sum(distribution) * 100), by = sex]
  cli::cli_alert_info(
    "LPR distribution (single-year, {interpolation_method}): {round(by_sex_single[sex=='female', pct], 1)}% female, {round(by_sex_single[sex=='male', pct], 1)}% male"
  )

  dist_single
}

#' Calculate separate NEW and AOS distributions from DHS data
#'
#' @description
#' Per TR2025 Section 1.3, creates separate age-sex distributions for
#' new arrivals and adjustments of status. NEW and AOS have meaningfully
#' different age-sex profiles (AOS skews older since they're already in the US).
#'
#' Steps (per Section 1.3.c):
#' 1. Optionally convert FY to CY
#' 2. Filter DHS data by admission_type, aggregate by age/sex
#' 3. Interpolate each to single years (Beers or uniform)
#' 4. Normalize each to sum to 1.0
#'
#' @param dhs_data data.table from load_dhs_lpr_data() with admission_type column
#' @param reference_years Integer vector: years for averaging (default: 2016:2020)
#' @param convert_fy_cy Logical: apply FY-to-CY conversion (default: FALSE)
#' @param interpolation_method Character: "beers" or "uniform" (default: "uniform")
#'
#' @return list with:
#'   - new_distribution: data.table with age, sex, distribution (sums to 1.0)
#'   - aos_distribution: data.table with age, sex, distribution (sums to 1.0)
#'   - combined_distribution: data.table with age, sex, distribution (for backward compat)
#'
#' @export
calculate_lpr_distributions_separate <- function(dhs_data,
                                                  reference_years = 2016:2020,
                                                  convert_fy_cy = FALSE,
                                                  interpolation_method = "uniform") {
  dhs_data <- data.table::as.data.table(dhs_data)

  # Step 1: Optionally convert FY to CY
  if (convert_fy_cy) {
    dhs_data <- convert_fy_to_cy(dhs_data)
  }

  # Determine year column name (fiscal_year or calendar_year after conversion)
  year_col <- if ("calendar_year" %in% names(dhs_data)) "calendar_year" else "fiscal_year"

  # Filter to reference years
  ref_data <- dhs_data[get(year_col) %in% reference_years]
  if (nrow(ref_data) == 0) {
    cli::cli_abort("No DHS data found for {year_col} {paste(reference_years, collapse=', ')}")
  }

  # Verify admission_type column exists
  if (!"admission_type" %in% names(ref_data)) {
    cli::cli_abort("DHS data must have 'admission_type' column with 'new_arrival' and 'aos' values")
  }

  # Choose interpolation function
  interpolate_fn <- if (interpolation_method == "beers") {
    beers_interpolate_immigration
  } else {
    uniform_interpolate_dhs
  }

  # Step 2: Calculate NEW distribution
  new_data <- ref_data[admission_type == "new_arrival"]
  new_5yr <- new_data[, .(count = sum(count, na.rm = TRUE)), by = .(age_min, age_max, sex)]
  new_total <- sum(new_5yr$count)
  new_5yr[, distribution := count / new_total]

  new_dist <- interpolate_fn(new_5yr)

  cli::cli_alert_info(
    "NEW distribution ({min(reference_years)}-{max(reference_years)}): {format(round(new_total), big.mark=',')} total admissions"
  )

  # Step 3: Calculate AOS distribution
  aos_data <- ref_data[admission_type == "aos"]
  aos_5yr <- aos_data[, .(count = sum(count, na.rm = TRUE)), by = .(age_min, age_max, sex)]
  aos_total <- sum(aos_5yr$count)
  aos_5yr[, distribution := count / aos_total]

  aos_dist <- interpolate_fn(aos_5yr)

  cli::cli_alert_info(
    "AOS distribution ({min(reference_years)}-{max(reference_years)}): {format(round(aos_total), big.mark=',')} total admissions"
  )

  # Also compute combined distribution for backward compatibility
  combined_5yr <- ref_data[, .(count = sum(count, na.rm = TRUE)), by = .(age_min, age_max, sex)]
  combined_total <- sum(combined_5yr$count)
  combined_5yr[, distribution := count / combined_total]
  combined_dist <- interpolate_fn(combined_5yr)

  # Report age profile differences
  new_25_44 <- new_dist[age >= 25 & age <= 44, sum(distribution)] * 100
  aos_25_44 <- aos_dist[age >= 25 & age <= 44, sum(distribution)] * 100
  new_45_64 <- new_dist[age >= 45 & age <= 64, sum(distribution)] * 100
  aos_45_64 <- aos_dist[age >= 45 & age <= 64, sum(distribution)] * 100

  cli::cli_alert_info(
    "Age 25-44: NEW={round(new_25_44, 1)}% vs AOS={round(aos_25_44, 1)}%"
  )
  cli::cli_alert_info(
    "Age 45-64: NEW={round(new_45_64, 1)}% vs AOS={round(aos_45_64, 1)}%"
  )

  list(
    new_distribution = new_dist,
    aos_distribution = aos_dist,
    combined_distribution = combined_dist
  )
}

#' Calculate emigration distribution from DHS data
#'
#' @description
#' Uses the same age-sex distribution as immigration since DHS does not
#' provide emigration-specific data. This assumes emigrants have a similar
#' age-sex profile to immigrants. This is a legacy fallback; prefer CBO
#' emigration distribution (see config immigration.emigration.distribution_source).
#'
#' @param dhs_data data.table from load_dhs_lpr_data()
#' @param reference_years Years to average for distribution (default: 2016:2020)
#' @return data.table with columns: age, sex, distribution
#' @export
calculate_emigration_distribution_dhs <- function(dhs_data,
                                                   reference_years = 2016:2020) {
  cli::cli_alert_info("Using DHS immigration distribution for emigration (legacy fallback)")
  calculate_lpr_distribution_dhs(dhs_data, reference_years)
}

# ===========================================================================
# FISCAL-YEAR TO CALENDAR-YEAR CONVERSION
# ===========================================================================

#' Convert DHS fiscal-year data to calendar-year data
#'
#' @description
#' Per TR2025 Section 1.3, DHS data reported by federal fiscal year
#' (Oct 1 - Sep 30) is converted to calendar year:
#'
#'   CY_z = 0.75 * FY_z + 0.25 * FY_{z+1}
#'
#' This reflects that 75% of a fiscal year (Oct-Jun) falls in the calendar
#' year, and 25% (Jul-Sep of FY_{z+1}) also falls in the calendar year.
#'
#' @param dhs_data data.table with fiscal_year, age_min, age_max, sex,
#'   admission_type, count columns
#'
#' @return data.table with calendar_year replacing fiscal_year,
#'   counts weighted by FY-CY overlap. Last available CY = max(FY) - 1.
#'
#' @export
convert_fy_to_cy <- function(dhs_data) {
  dhs_data <- data.table::as.data.table(dhs_data)

  if (!"fiscal_year" %in% names(dhs_data)) {
    cli::cli_abort("dhs_data must have 'fiscal_year' column")
  }

  fy_range <- range(dhs_data$fiscal_year)
  cli::cli_alert_info("Converting FY{fy_range[1]}-FY{fy_range[2]} to calendar years")

  # Group columns for merging
  group_cols <- intersect(
    c("age_min", "age_max", "sex", "admission_type", "age_group"),
    names(dhs_data)
  )

  # For each CY_z, we need FY_z and FY_{z+1}
  # CY_z = 0.75 * FY_z + 0.25 * FY_{z+1}
  # Available CY range: min(FY) to max(FY) - 1
  cy_range <- fy_range[1]:(fy_range[2] - 1)

  result_list <- list()

  for (cy in cy_range) {
    fy_current <- dhs_data[fiscal_year == cy]
    fy_next <- dhs_data[fiscal_year == cy + 1]

    if (nrow(fy_current) == 0 || nrow(fy_next) == 0) next

    # Merge the two fiscal years on group columns
    merged <- merge(
      fy_current[, c(group_cols, "count"), with = FALSE],
      fy_next[, c(group_cols, "count"), with = FALSE],
      by = group_cols,
      suffixes = c("_fy", "_fy_next"),
      all = TRUE
    )

    # Fill NAs with 0
    merged[is.na(count_fy), count_fy := 0]
    merged[is.na(count_fy_next), count_fy_next := 0]

    # Apply CY conversion formula
    merged[, count := 0.75 * count_fy + 0.25 * count_fy_next]
    merged[, calendar_year := cy]
    merged[, c("count_fy", "count_fy_next") := NULL]

    result_list[[length(result_list) + 1]] <- merged
  }

  result <- data.table::rbindlist(result_list, use.names = TRUE)

  cli::cli_alert_success(
    "Converted to CY{min(cy_range)}-CY{max(cy_range)} ({length(cy_range)} calendar years)"
  )

  result
}

# ===========================================================================
# H.S. BEERS INTERPOLATION FOR IMMIGRATION
# ===========================================================================

#' Apply H.S. Beers ordinary interpolation to immigration age groups
#'
#' @description
#' Converts DHS 5-year age group distributions to single-year-of-age using
#' the H.S. Beers ordinary interpolation method. This is the standard
#' demographic technique referenced in TR2025 Section 1.3 for emigration
#' and used by SSA for immigration data processing.
#'
#' Handles DHS-specific irregular age groups:
#' - "Under 1" (1 year) + "1-4" (4 years) combined into "0-4" (5 years)
#' - Standard 5-year groups: 5-9, 10-14, ..., 80-84
#' - Open-ended "85+" distributed uniformly (Beers can't extrapolate)
#'
#' Reuses \code{\link[=get_beers_coefficients]{get_beers_coefficients()}} and
#' \code{\link[=beers_interpolate]{beers_interpolate()}} from historical_marital_status.R.
#'
#' @param dist data.table with columns: age_min, age_max, sex, distribution
#' @param max_age Integer: maximum single-year age (default: 100, representing 100+)
#'
#' @return data.table with columns: age (0 to max_age), sex, distribution
#'   normalized to sum to 1.0
#'
#' @export
beers_interpolate_immigration <- function(dist, max_age = 100) {
  results <- list()

  for (s in c("male", "female")) {
    sex_dist <- dist[tolower(sex) == s]
    data.table::setorder(sex_dist, age_min)

    # Step 1: Combine irregular first groups into standard 5-year groups
    # DHS has: Under 1 (0-0), 1-4, 5-9, 10-14, ..., 80-84, 85+
    # Need: 0-4, 5-9, 10-14, ..., 80-84 (standard 5-year), plus 85+ separate

    # Separate open-ended group (85+) from regular groups
    open_ended <- sex_dist[age_max >= 85 | age_min >= 85]
    regular <- sex_dist[age_max < 85]

    # Combine "Under 1" (0-0) and "1-4" into "0-4"
    group_0 <- regular[age_min == 0 & age_max == 0]
    group_1_4 <- regular[age_min == 1 & age_max == 4]

    if (nrow(group_0) > 0 && nrow(group_1_4) > 0) {
      # Combine into 0-4 (include count if present in input)
      combined_0_4 <- data.table::data.table(
        age_min = 0L, age_max = 4L, sex = s,
        distribution = group_0$distribution + group_1_4$distribution
      )
      if ("count" %in% names(regular)) {
        combined_0_4[, count := group_0$count + group_1_4$count]
      }
      # Replace the two groups
      regular <- regular[!(age_min == 0 & age_max == 0) & !(age_min == 1 & age_max == 4)]
      regular <- rbind(combined_0_4, regular, use.names = TRUE)
      data.table::setorder(regular, age_min)
    } else if (nrow(group_0) == 0 && nrow(group_1_4) == 0) {
      # Already has 0-4 combined, or starts at 0-4
      # Check if first group is 0-4
      if (nrow(regular) > 0 && regular$age_min[1] == 0 && regular$age_max[1] == 4) {
        # Already standard format
      }
    }

    # Verify we have standard 5-year groups
    n_regular <- nrow(regular)
    if (n_regular < 6) {
      cli::cli_alert_warning(
        "Only {n_regular} regular age groups for {s}, falling back to uniform interpolation"
      )
      # Fall back to uniform
      return(uniform_interpolate_dhs(dist, max_age))
    }

    # Step 2: Apply Beers interpolation to standard groups (0-84)
    grouped_values <- regular$distribution
    n_groups <- length(grouped_values)

    # Use beers_interpolate from historical_marital_status.R
    single_year_values <- beers_interpolate(
      grouped_values = grouped_values,
      min_age = 0,
      max_age = n_groups * 5 - 1
    )

    # Create data.table for regular ages (0-84)
    regular_ages <- as.integer(names(single_year_values))
    regular_result <- data.table::data.table(
      age = regular_ages,
      distribution = as.numeric(single_year_values)
    )

    # Step 3: Handle open-ended 85+ group (distribute uniformly)
    open_ended_total <- sum(open_ended$distribution)
    if (open_ended_total > 0 && max_age >= 85) {
      n_open_ages <- max_age - 85 + 1  # ages 85 to max_age
      per_age <- open_ended_total / n_open_ages

      open_result <- data.table::data.table(
        age = 85:max_age,
        distribution = per_age
      )

      regular_result <- rbind(regular_result, open_result)
    }

    # Pad any missing ages with 0
    all_ages <- 0:max_age
    missing_ages <- setdiff(all_ages, regular_result$age)
    if (length(missing_ages) > 0) {
      regular_result <- rbind(regular_result, data.table::data.table(
        age = missing_ages,
        distribution = 0
      ))
    }

    regular_result[, sex := s]
    data.table::setorder(regular_result, age)

    results[[s]] <- regular_result
  }

  result <- data.table::rbindlist(results)

  # Ensure non-negative
  result[distribution < 0, distribution := 0]

  # Normalize to sum to 1
  result[, distribution := distribution / sum(distribution)]

  data.table::setorder(result, sex, age)

  result
}

#' Interpolate age groups to single years (uniform method)
#'
#' @description
#' Converts 5-year age group distributions to single-year-of-age distributions
#' using uniform distribution within each group. This preserves the total
#' distribution for each age group while providing single-year granularity.
#'
#' Note: Despite the original name "beers_interpolate_dhs", this function
#' uses uniform distribution, not H.S. Beers interpolation. Renamed to
#' accurately reflect the method. For real Beers interpolation, see
#' \code{\link{beers_interpolate_immigration}}.
#'
#' @param dist data.table with columns: age_min, age_max, sex, distribution
#' @param max_age Integer: maximum single-year age (default: 100, representing 100+)
#' @return data.table with columns: age (0 to max_age), sex, distribution
#' @keywords internal
uniform_interpolate_dhs <- function(dist, max_age = 100) {
  results <- list()

  for (s in c("male", "female")) {
    sex_dist <- dist[tolower(sex) == s]
    data.table::setorder(sex_dist, age_min)

    # Build single-year distribution (ages 0 to max_age)
    single_ages <- numeric(max_age + 1)  # ages 0-100

    for (i in seq_len(nrow(sex_dist))) {
      age_start <- sex_dist$age_min[i]
      age_end <- sex_dist$age_max[i]
      group_value <- sex_dist$distribution[i]

      # If age group extends beyond max_age, cap it
      # All ages >= max_age go into the max_age bucket (100+ -> 100)
      if (age_end > max_age) {
        age_end <- max_age
      }
      if (age_start > max_age) {
        age_start <- max_age
      }

      group_width <- age_end - age_start + 1
      if (group_width < 1) group_width <- 1

      # Distribute evenly within the age group
      per_year <- group_value / group_width
      for (age in age_start:age_end) {
        if (age >= 0 && age <= max_age) {
          single_ages[age + 1] <- single_ages[age + 1] + per_year  # +1 for R indexing
        }
      }
    }

    results[[s]] <- data.table::data.table(
      age = 0:max_age,
      sex = s,
      distribution = single_ages
    )
  }

  result <- data.table::rbindlist(results)

  # Normalize to sum to 1
  result[, distribution := distribution / sum(distribution)]

  data.table::setorder(result, sex, age)

  result
}

#' @rdname uniform_interpolate_dhs
#' @keywords internal
beers_interpolate_dhs <- uniform_interpolate_dhs

# ===========================================================================
# NEW/AOS RATIO CALCULATION
# ===========================================================================

#' Calculate NEW/AOS ratio from DHS data
#'
#' @param dhs_data data.table from load_dhs_lpr_data()
#' @param reference_years Years to calculate ratio (default: 2016:2019)
#' @return list with new_ratio, aos_ratio, and details
#' @export
calculate_new_aos_ratio <- function(dhs_data,
                                     reference_years = 2016:2020) {
  dhs_data <- data.table::as.data.table(dhs_data)

  # Filter to reference years
  ref_data <- dhs_data[fiscal_year %in% reference_years]

  if (nrow(ref_data) == 0) {
    cli::cli_abort("No DHS data found for years {paste(reference_years, collapse=', ')}")
  }

  # Calculate totals by admission type
  totals <- ref_data[, .(total = sum(count, na.rm = TRUE)), by = admission_type]
  overall_total <- sum(totals$total)

  # Extract ratios
  new_total <- totals[admission_type == "new_arrival", total]
  aos_total <- totals[admission_type == "aos", total]

  new_ratio <- new_total / overall_total
  aos_ratio <- aos_total / overall_total

  # Calculate by-year details
  by_year <- ref_data[, .(total = sum(count, na.rm = TRUE)), by = .(fiscal_year, admission_type)]
  by_year_wide <- data.table::dcast(by_year, fiscal_year ~ admission_type, value.var = "total")
  by_year_wide[, lpr_total := aos + new_arrival]
  by_year_wide[, new_pct := round(new_arrival / lpr_total * 100, 1)]
  by_year_wide[, aos_pct := round(aos / lpr_total * 100, 1)]

  cli::cli_alert_info(
    "NEW/AOS ratio from DHS {min(reference_years)}-{max(reference_years)}: NEW={round(new_ratio*100, 1)}%, AOS={round(aos_ratio*100, 1)}%"
  )

  list(
    new_ratio = new_ratio,
    aos_ratio = aos_ratio,
    reference_years = reference_years,
    by_year = by_year_wide,
    totals = list(
      new_arrivals = new_total,
      aos = aos_total,
      total = overall_total
    )
  )
}

# ===========================================================================
# TR2025 ASSUMPTIONS
# ===========================================================================

#' Get TR2025 LPR immigration assumptions from Table V.A2
#'
#' @description
#' Loads LPR immigration assumptions from TR2025 Table V.A2. This provides
#' official OCACT immigration assumptions for both historical (1940-2024)
#' and projected (2025-2100) years.
#'
#' @param years Integer vector: years to get assumptions for (default: 2025:2099)
#' @param alternative Character: which alternative to use for projections
#'   ("intermediate", "low-cost", or "high-cost"). Default: "intermediate"
#' @param data_dir Character: directory containing TR2025 files
#'
#' @return data.table with columns: year, total_lpr, total_emigration, net_lpr_total,
#'   lpr_aos (if available), data_type
#'
#' @details
#' For historical years (1940-2024), returns actual recorded values.
#' For projected years (2025+), returns the selected alternative assumptions.
#'
#' V.A2 provides:
#' - LPR Inflow: Total LPR immigration
#' - LPR Outflow: Legal emigration
#' - LPR AOS: Adjustments of status (included in inflow)
#' - LPR Net: Net LPR change
#'
#' @param emigration_ratio Numeric: fraction of LPR immigration that emigrates.
#'   TR2025 Section 1.3.c Eq 1.3.4: E = ratio * L. Required, no default.
#'
#' @export
get_tr_lpr_assumptions <- function(years,
                                        alternative,
                                        data_dir,
                                        emigration_ratio) {
  alternative <- match.arg(alternative, c("intermediate", "low-cost", "high-cost"))

 # Try to load from V.A2
  va2_available <- tryCatch({
    # Check if the loader function exists
    if (!exists("load_tr_immigration_assumptions", mode = "function")) {
      source(here::here("R/data_acquisition/tr_data.R"))
    }
    TRUE
  }, error = function(e) FALSE)

  if (!va2_available) {
    cli::cli_abort(c(
      "V.A2 data is required for LPR assumptions",
      "i" = "Ensure {.file {file.path(data_dir, 'SingleYearTRTables_TR2025.xlsx')}} exists",
      "i" = "All immigration totals must come from official V.A2 data, not hardcoded values"
    ))
  }

  result <- get_tr_lpr_assumptions_va2(years, alternative, data_dir, emigration_ratio)
  result
}

#' Get LPR assumptions from V.A2 data
#'
#' @param years Integer vector: years to retrieve
#' @param alternative Character: V.A2 alternative scenario
#' @param data_dir Character: directory containing TR2025 files
#' @param emigration_ratio Numeric: fraction of LPR immigration that emigrates
#'   (TR2025 Section 1.3.c Eq 1.3.4: E = ratio * L). Required, no default.
#'
#' @keywords internal
get_tr_lpr_assumptions_va2 <- function(years,
                                            alternative,
                                            data_dir,
                                            emigration_ratio) {
  # Load V.A2 immigration assumptions
  imm <- load_tr_immigration_assumptions(data_dir = data_dir, cache = TRUE)

  # For projected years, V.A2 has duplicates for different alternatives

  # Historical years (data_type == "historical") are unique
  # Projected years appear 3 times (Intermediate, Low-cost, High-cost)

  # Filter to requested years
  result <- imm[year %in% years]

  if (nrow(result) == 0) {
    cli::cli_abort("No V.A2 data found for years {min(years)}-{max(years)}")
  }

  # Handle duplicates for projected years by keeping first occurrence (Intermediate)
  # V.A2 structure: Historical first, then Intermediate, then Low-cost, then High-cost
  # For now, we take the first occurrence which is Intermediate for projected years
  if (alternative == "intermediate") {
    result <- result[!duplicated(year)]
  } else {
    # For other alternatives, would need to parse the section headers
    # For now, use the default (Intermediate)
    cli::cli_alert_warning("Alternative '{alternative}' not yet implemented, using intermediate")
    result <- result[!duplicated(year)]
  }

  # Rename columns to match expected format
  # NOTE: In V.A2, LPR Inflow = NEW arrivals only, LPR AOS = Adjustments of Status
  # Total LPR Immigration = NEW + AOS (per TR2025 methodology)
  # Emigration computed as total_lpr * emigration_ratio (TR2025 Eq 1.3.4)
  result <- result[, .(
    year = year,
    total_lpr = (lpr_inflow + lpr_aos) * 1000,  # Total = NEW + AOS, convert to persons
    lpr_new = lpr_inflow * 1000,                 # NEW arrivals only
    lpr_aos = lpr_aos * 1000,                    # AOS only
    total_emigration = (lpr_inflow + lpr_aos) * 1000 * emigration_ratio,
    net_lpr_total = (lpr_inflow + lpr_aos) * 1000 * (1 - emigration_ratio),
    data_type = data_type
  )]

  # Handle NAs (early years may have missing data)
  result[is.na(lpr_new), lpr_new := 0]
  result[is.na(lpr_aos), lpr_aos := 0]
  result[is.na(total_lpr), total_lpr := lpr_new + lpr_aos]
  result[is.na(total_emigration), total_emigration := 0]
  result[is.na(net_lpr_total), net_lpr_total := total_lpr - total_emigration]

  # Report what we loaded
  n_hist <- sum(result$data_type == "historical")
  n_proj <- sum(result$data_type %in% c("projected", "estimated"))
  cli::cli_alert_success("Loaded LPR assumptions from V.A2: {n_hist} historical, {n_proj} projected years")

  # Sample validation
  if (2025 %in% result$year) {
    lpr_2025 <- result[year == 2025, total_lpr]
    cli::cli_alert_info("2025 LPR total: {format(lpr_2025, big.mark = ',')}")
  }
  if (2027 %in% result$year) {
    lpr_2027 <- result[year == 2027, total_lpr]
    cli::cli_alert_info("2027 LPR total: {format(lpr_2027, big.mark = ',')}")
  }

  data.table::setorder(result, year)
  result
}

# NOTE: get_tr_lpr_assumptions_hardcoded() was removed. All LPR immigration
# totals must come from official V.A2 data or CBO data.

#' Get LPR assumptions from CBO Demographic Outlook data
#'
#' @description
#' Loads LPR+ immigration and emigration totals from CBO migration data.
#' CBO provides both immigration and emigration by year, age, and sex.
#' Emigration totals come directly from CBO (no ratio applied).
#'
#' CBO has no NEW/AOS split, so the DHS historical ratio is used to
#' derive lpr_new and lpr_aos from total_lpr.
#'
#' @param years Integer vector: years to get assumptions for. Required, no default.
#' @param cbo_file Character: path to CBO migration CSV file. Required, no default.
#' @param new_aos_ratio Numeric: fraction of total LPR that are new arrivals,
#'   from DHS historical data. Required, no default.
#'
#' @return data.table with columns: year, total_lpr, lpr_new, lpr_aos,
#'   total_emigration, net_lpr_total, data_type
#'
#' @export
get_cbo_lpr_assumptions <- function(years, cbo_file, new_aos_ratio) {
  # Load CBO data
  cbo_data <- load_cbo_migration(file_path = cbo_file)

  # Get LPR+ immigration by year
  lpr_imm <- get_cbo_lpr_immigration(cbo_data, years = years)
  if (nrow(lpr_imm) == 0) {
    cli::cli_abort("No CBO LPR+ immigration data found for years {min(years)}-{max(years)}")
  }
  imm_by_year <- lpr_imm[, .(total_lpr = sum(immigration)), by = year]

  # Get LPR+ emigration by year (direct from CBO, no ratio)
  lpr_emig <- get_cbo_lpr_emigration(cbo_data, years = years)
  if (nrow(lpr_emig) == 0) {
    cli::cli_abort("No CBO LPR+ emigration data found for years {min(years)}-{max(years)}")
  }
  emig_by_year <- lpr_emig[, .(total_emigration = sum(emigration)), by = year]

  # Merge immigration and emigration
  result <- merge(imm_by_year, emig_by_year, by = "year", all = TRUE)

  # Compute net LPR
  result[, net_lpr_total := total_lpr - total_emigration]

  # Split into NEW/AOS using DHS historical ratio
  result[, lpr_new := total_lpr * new_aos_ratio]
  result[, lpr_aos := total_lpr * (1 - new_aos_ratio)]

  # Set data type
  result[, data_type := "cbo"]

  # Ensure column order matches V.A2 schema
  result <- result[, .(year, total_lpr, lpr_new, lpr_aos,
                        total_emigration, net_lpr_total, data_type)]

  # Filter to requested years (CBO may have more/fewer years)
  result <- result[year %in% years]

  # Report
  n_years <- nrow(result)
  cli::cli_alert_success("Loaded CBO LPR+ assumptions: {n_years} years ({min(result$year)}-{max(result$year)})")

  if (nrow(result) > 0) {
    sample_yr <- if (2027 %in% result$year) 2027 else min(result$year)
    sample <- result[year == sample_yr]
    cli::cli_alert_info(
      "{sample_yr} CBO: LPR={format(round(sample$total_lpr), big.mark=',')} Emig={format(round(sample$total_emigration), big.mark=',')} Net={format(round(sample$net_lpr_total), big.mark=',')}"
    )
    cli::cli_alert_info(
      "NEW/AOS split using DHS ratio: NEW={round(new_aos_ratio * 100, 1)}%, AOS={round((1 - new_aos_ratio) * 100, 1)}%"
    )
  }

  data.table::setorder(result, year)
  result
}

# ===========================================================================
# PROJECTION FUNCTIONS
# ===========================================================================

#' Project LPR immigration by age and sex
#'
#' @param assumptions TR2025 assumptions data.table
#' @param distribution Age-sex distribution from DHS
#' @return data.table with columns: year, age, sex, immigration
#' @export
project_lpr_immigration <- function(assumptions, distribution) {
  assumptions <- data.table::as.data.table(assumptions)
  distribution <- data.table::as.data.table(distribution)

  # Validate distribution sums to 1
  dist_sum <- sum(distribution$distribution)
  if (abs(dist_sum - 1.0) > 0.001) {
    cli::cli_alert_warning("Distribution sums to {round(dist_sum, 6)}, normalizing")
    distribution[, distribution := distribution / dist_sum]
  }

  # Cross join years with distribution
  result <- data.table::CJ(
    year = assumptions$year,
    age = unique(distribution$age),
    sex = unique(distribution$sex),
    sorted = FALSE
  )

  # Merge distribution
  result <- merge(result, distribution[, .(age, sex, distribution)], by = c("age", "sex"), all.x = TRUE)

  # Merge total LPR
  result <- merge(result, assumptions[, .(year, total_lpr)], by = "year", all.x = TRUE)

  # Calculate immigration by age-sex
  result[, immigration := total_lpr * distribution]

  # Clean up
  result[, c("distribution", "total_lpr") := NULL]
  data.table::setcolorder(result, c("year", "age", "sex", "immigration"))
  data.table::setorder(result, year, age, sex)

  result
}

#' Project LPR immigration with separate NEW/AOS distributions
#'
#' @description
#' Per TR2025 Eqs 1.3.1-1.3.3, applies separate age-sex distributions
#' to NEW arrivals and AOS totals, then sums to get total LPR immigration.
#'
#'   NEW_x,s = lpr_new_total * new_dist_x,s
#'   AOS_x,s = lpr_aos_total * aos_dist_x,s
#'   L_x,s   = NEW_x,s + AOS_x,s
#'
#' @param assumptions data.table with columns: year, total_lpr, lpr_new, lpr_aos
#' @param new_distribution data.table with columns: age, sex, distribution (sums to 1.0)
#' @param aos_distribution data.table with columns: age, sex, distribution (sums to 1.0)
#'
#' @return list with:
#'   - lpr_immigration: data.table (year, age, sex, immigration) - total L
#'   - new_arrivals: data.table (year, age, sex, new_arrivals) - NEW component
#'   - aos: data.table (year, age, sex, aos) - AOS component
#'
#' @export
project_lpr_immigration_separate <- function(assumptions, new_distribution, aos_distribution) {
  assumptions <- data.table::as.data.table(assumptions)
  new_distribution <- data.table::as.data.table(new_distribution)
  aos_distribution <- data.table::as.data.table(aos_distribution)

  # Validate distributions sum to 1
  for (dist_info in list(
    list(dist = new_distribution, name = "NEW"),
    list(dist = aos_distribution, name = "AOS")
  )) {
    dist_sum <- sum(dist_info$dist$distribution)
    if (abs(dist_sum - 1.0) > 0.001) {
      cli::cli_alert_warning("{dist_info$name} distribution sums to {round(dist_sum, 6)}, normalizing")
      dist_info$dist[, distribution := distribution / dist_sum]
    }
  }

  # Validate assumptions have lpr_new and lpr_aos columns
  if (!all(c("lpr_new", "lpr_aos") %in% names(assumptions))) {
    cli::cli_abort("assumptions must have 'lpr_new' and 'lpr_aos' columns (from V.A2)")
  }

  ages <- unique(new_distribution$age)
  sexes <- unique(new_distribution$sex)

  # Project NEW arrivals: NEW_x,s = lpr_new * new_dist_x,s
  new_result <- data.table::CJ(year = assumptions$year, age = ages, sex = sexes, sorted = FALSE)
  new_result <- merge(new_result, new_distribution[, .(age, sex, distribution)], by = c("age", "sex"), all.x = TRUE)
  new_result <- merge(new_result, assumptions[, .(year, lpr_new)], by = "year", all.x = TRUE)
  new_result[, new_arrivals := lpr_new * distribution]
  new_result[, c("distribution", "lpr_new") := NULL]
  data.table::setorder(new_result, year, age, sex)

  # Project AOS: AOS_x,s = lpr_aos * aos_dist_x,s
  aos_result <- data.table::CJ(year = assumptions$year, age = ages, sex = sexes, sorted = FALSE)
  aos_result <- merge(aos_result, aos_distribution[, .(age, sex, distribution)], by = c("age", "sex"), all.x = TRUE)
  aos_result <- merge(aos_result, assumptions[, .(year, lpr_aos)], by = "year", all.x = TRUE)
  aos_result[, aos := lpr_aos * distribution]
  aos_result[, c("distribution", "lpr_aos") := NULL]
  data.table::setorder(aos_result, year, age, sex)

  # Total LPR: L_x,s = NEW_x,s + AOS_x,s (Eq 1.3.3)
  lpr_result <- merge(
    new_result[, .(year, age, sex, new_arrivals)],
    aos_result[, .(year, age, sex, aos)],
    by = c("year", "age", "sex")
  )
  lpr_result[, immigration := new_arrivals + aos]
  lpr_immigration <- lpr_result[, .(year, age, sex, immigration)]
  data.table::setorder(lpr_immigration, year, age, sex)

  # Verify totals match assumptions
  check <- lpr_immigration[, .(projected = sum(immigration)), by = year]
  check <- merge(check, assumptions[, .(year, expected = total_lpr)], by = "year")
  max_diff <- max(abs(check$projected - check$expected))
  if (max_diff > 1) {
    cli::cli_alert_warning("Total LPR differs from assumptions by up to {round(max_diff)} (rounding)")
  } else {
    cli::cli_alert_success("NEW + AOS = L verified for all years")
  }

  # Report sample year
  sample_yr <- if (2030 %in% assumptions$year) 2030 else min(assumptions$year)
  new_total <- new_result[year == sample_yr, sum(new_arrivals)]
  aos_total <- aos_result[year == sample_yr, sum(aos)]
  cli::cli_alert_info(
    "Sample {sample_yr}: NEW={format(round(new_total), big.mark=',')} + AOS={format(round(aos_total), big.mark=',')} = L={format(round(new_total + aos_total), big.mark=',')}"
  )

  list(
    lpr_immigration = lpr_immigration,
    new_arrivals = new_result[, .(year, age, sex, new_arrivals)],
    aos = aos_result[, .(year, age, sex, aos)]
  )
}

#' Project legal emigration by age and sex
#'
#' @param assumptions TR2025 assumptions data.table
#' @param distribution Age-sex distribution from DHS
#' @return data.table with columns: year, age, sex, emigration
#' @export
project_legal_emigration <- function(assumptions, distribution) {
  assumptions <- data.table::as.data.table(assumptions)
  distribution <- data.table::as.data.table(distribution)

  # Validate distribution sums to 1
  dist_sum <- sum(distribution$distribution)
  if (abs(dist_sum - 1.0) > 0.001) {
    cli::cli_alert_warning("Distribution sums to {round(dist_sum, 6)}, normalizing")
    distribution[, distribution := distribution / dist_sum]
  }

  # Cross join years with distribution
  result <- data.table::CJ(
    year = assumptions$year,
    age = unique(distribution$age),
    sex = unique(distribution$sex),
    sorted = FALSE
  )

  # Merge distribution
  result <- merge(result, distribution[, .(age, sex, distribution)], by = c("age", "sex"), all.x = TRUE)

  # Merge total emigration
  result <- merge(result, assumptions[, .(year, total_emigration)], by = "year", all.x = TRUE)

  # Calculate emigration by age-sex
  result[, emigration := total_emigration * distribution]

  # Clean up
  result[, c("distribution", "total_emigration") := NULL]
  data.table::setcolorder(result, c("year", "age", "sex", "emigration"))
  data.table::setorder(result, year, age, sex)

  result
}

#' Split LPR immigration into NEW and AOS
#'
#' Splits total LPR immigration into NEW arrivals and Adjustments of Status (AOS).
#' AOS represents people transitioning from temporary/unlawful (O) status to LPR.
#'
#' @param lpr_immigration Projected LPR by year, age, sex
#' @param assumptions LPR assumptions with year-specific lpr_new and lpr_aos columns
#'   (required when method = "assumption")
#' @param new_ratio Ratio of new arrivals from DHS historical data
#'   (required when method = "ratio")
#' @param method Split method: "assumption" uses TR2025 V.A2 year-specific values,
#'   "ratio" uses single historical ratio for all years. Default: "assumption"
#' @return list with new_arrivals and aos data.tables
#' @export
split_lpr_new_aos <- function(lpr_immigration,
                              assumptions = NULL,
                              new_ratio = NULL,
                              method = c("assumption", "ratio")) {
  method <- match.arg(method)
  lpr_immigration <- data.table::as.data.table(lpr_immigration)

  if (method == "assumption") {
    # Use TR2025 V.A2 year-specific values
    if (is.null(assumptions)) {
      cli::cli_abort("assumptions required when method = 'assumption'")
    }

    assumptions <- data.table::as.data.table(assumptions)

    # Calculate year-specific ratios from assumptions
    year_ratios <- assumptions[, .(
      year = year,
      new_ratio = lpr_new / total_lpr,
      aos_ratio = lpr_aos / total_lpr
    )]

    # Merge ratios into immigration data
    lpr_with_ratios <- merge(
      lpr_immigration,
      year_ratios,
      by = "year",
      all.x = TRUE
    )

    # Check for missing years
    missing_years <- lpr_with_ratios[is.na(new_ratio), unique(year)]
    if (length(missing_years) > 0) {
      cli::cli_abort("No assumptions found for years: {paste(missing_years, collapse = ', ')}")
    }

    # Create NEW arrivals
    new_arrivals <- data.table::copy(lpr_with_ratios)
    new_arrivals[, new_arrivals := immigration * new_ratio]
    new_arrivals[, c("immigration", "new_ratio", "aos_ratio") := NULL]

    # Create AOS
    aos <- data.table::copy(lpr_with_ratios)
    aos[, aos := immigration * aos_ratio]
    aos[, c("immigration", "new_ratio", "aos_ratio") := NULL]

    # Report totals
    total_new <- sum(new_arrivals$new_arrivals)
    total_aos <- sum(aos$aos)
    min_year <- min(lpr_immigration$year)
    max_year <- max(lpr_immigration$year)
    cli::cli_alert_info("Split LPR using TR2025 assumptions ({min_year}-{max_year})")
    cli::cli_alert_info("Total NEW: {format(round(total_new), big.mark = ',')} | Total AOS: {format(round(total_aos), big.mark = ',')}")

    # Sample year validation
    if (2025 %in% assumptions$year) {
      aos_2025 <- assumptions[year == 2025, lpr_aos]
      cli::cli_alert_info("2025 AOS target: {format(aos_2025, big.mark = ',')}")
    }

  } else {
    # Use single historical ratio for all years (original method)
    if (is.null(new_ratio)) {
      cli::cli_abort("new_ratio required when method = 'ratio'")
    }

    aos_ratio <- 1 - new_ratio

    # Create NEW arrivals
    new_arrivals <- data.table::copy(lpr_immigration)
    new_arrivals[, new_arrivals := immigration * new_ratio]
    new_arrivals[, immigration := NULL]

    # Create AOS
    aos <- data.table::copy(lpr_immigration)
    aos[, aos := immigration * aos_ratio]
    aos[, immigration := NULL]

    cli::cli_alert_info("Split LPR using historical ratio: NEW={round(new_ratio*100, 1)}%, AOS={round(aos_ratio*100, 1)}%")
  }

  list(
    new_arrivals = new_arrivals,
    aos = aos
  )
}

#' Calculate net LPR immigration
#'
#' @param lpr_immigration data.table with immigration by year, age, sex
#' @param emigration data.table with emigration by year, age, sex
#' @return data.table with columns: year, age, sex, immigration, emigration, net_lpr
#' @export
calculate_net_lpr <- function(lpr_immigration, emigration) {
  lpr_immigration <- data.table::as.data.table(lpr_immigration)
  emigration <- data.table::as.data.table(emigration)

  # Merge immigration and emigration
  result <- merge(
    lpr_immigration,
    emigration,
    by = c("year", "age", "sex"),
    all = TRUE
  )

  # Calculate net
  result[, net_lpr := immigration - emigration]

  data.table::setorder(result, year, age, sex)

  result
}

# ===========================================================================
# TARGET HELPER FUNCTIONS
# ===========================================================================

#' Get LPR distribution for projection (target helper)
#'
#' @description
#' Helper function for the lpr_distribution target. Selects the appropriate
#' distribution method based on config and applies elderly overrides.
#'
#' When distribution_method = "dhs" and dhs_distribution_mode = "separate",
#' returns a list with separate NEW and AOS distributions per TR2025 Section 1.3.
#' Otherwise returns a single combined distribution.
#'
#' @param config List: config_assumptions containing immigration settings
#' @param tr_derived_dist data.table: TR-derived distribution from
#'   calculate_tr_derived_distribution() (for "tr_derived" method)
#'
#' @return Either:
#'   - list with new_distribution, aos_distribution, combined_distribution
#'     (when dhs + separate mode)
#'   - data.table with columns: age, sex, distribution (otherwise)
#'
#' @export
get_lpr_distribution_for_projection <- function(config, tr_derived_dist = NULL) {
  method <- config$immigration$lpr$distribution_method
  if (is.null(method)) method <- "dhs"

  # Calculate approximate total net immigration for override proportions
  # Using ultimate values: net LPR (~788K) + net O (~750K) = ~1.54M
  total_net_imm <- config$immigration$ultimate_net_lpr_immigration +
                   (config$immigration$o_immigration$ultimate_gross_o * 0.5)

  if (method == "dhs") {
    dhs_mode <- config$immigration$lpr$dhs_distribution_mode
    if (is.null(dhs_mode)) dhs_mode <- "separate"

    ref_years <- config$immigration$lpr$distribution_years
    interp_method <- config$immigration$lpr$interpolation_method
    if (is.null(interp_method)) interp_method <- "uniform"
    convert_fy_cy <- isTRUE(config$immigration$lpr$convert_fy_to_cy)

    dhs_data <- load_dhs_lpr_data()

    if (dhs_mode == "separate") {
      # TR2025 Section 1.3: Separate NEW/AOS distributions
      cli::cli_alert_info("Using separate NEW/AOS distributions (TR2025 Section 1.3)")
      result <- calculate_lpr_distributions_separate(
        dhs_data = dhs_data,
        reference_years = ref_years,
        convert_fy_cy = convert_fy_cy,
        interpolation_method = interp_method
      )

      # Apply elderly override to combined distribution (for backward compat targets)
      elderly_override <- config$immigration$lpr$elderly_override
      if (!is.null(elderly_override) && isTRUE(elderly_override$enabled)) {
        result$combined_distribution <- apply_elderly_override(
          result$combined_distribution, elderly_override, total_net_imm
        )
      }

      result
    } else {
      # Legacy combined mode
      dist <- get_immigration_distribution(
        method = "dhs",
        dhs_data = dhs_data,
        dhs_years = ref_years,
        elderly_override = config$immigration$lpr$elderly_override,
        total_net_immigration = total_net_imm
      )
      dist
    }
  } else {
    # TR2025-derived distribution with elderly override for ages 85+
    get_immigration_distribution(
      method = "tr_derived",
      tr_derived_data = tr_derived_dist,
      elderly_override_tr_derived = config$immigration$lpr$elderly_override_tr_derived,
      total_net_immigration = total_net_imm
    )
  }
}

# ===========================================================================
# MAIN ENTRY POINT
# ===========================================================================

#' Run complete LPR immigration projection
#'
#' @param dhs_data DHS expanded tables data (or NULL to load from file)
#' @param projection_years Years to project (default: 2025:2099)
#' @param distribution_years Years for DHS age-sex distribution (default: 2018:2023)
#' @param new_aos_ratio_years Years for DHS NEW/AOS ratio (default: 2016:2019)
#' @return list with all 5 outputs plus distributions, ratios, assumptions
#' @export
run_lpr_projection <- function(dhs_data = NULL,
                                projection_years = 2025:2099,
                                distribution_years = 2016:2020,
                                new_aos_ratio_years = 2016:2020) {
  cli::cli_h2("LPR Immigration Projection")

  # Load DHS data if not provided
  if (is.null(dhs_data)) {
    cli::cli_h3("Loading DHS LPR data")
    dhs_data <- load_dhs_lpr_data()
  }

  # Step 1: Calculate age-sex distributions from DHS
  cli::cli_h3("Step 1: Calculate age-sex distributions from DHS")
  lpr_distribution <- calculate_lpr_distribution_dhs(dhs_data, distribution_years)
  emigration_distribution <- calculate_emigration_distribution_dhs(dhs_data, distribution_years)

  # Step 2: Calculate NEW/AOS ratio from DHS
  cli::cli_h3("Step 2: Calculate NEW/AOS ratio from DHS")
  new_aos_ratio <- calculate_new_aos_ratio(dhs_data, new_aos_ratio_years)

  # Step 3: Get TR2025 assumptions
  cli::cli_h3("Step 3: Load TR2025 assumptions")
  assumptions <- get_tr_lpr_assumptions(
    years = projection_years,
    alternative = "intermediate",
    data_dir = "data/raw/SSA_TR2025",
    emigration_ratio = 0.25
  )
  cli::cli_alert_info("Projection years: {min(projection_years)}-{max(projection_years)}")
  cli::cli_alert_info("2025 LPR: {format(assumptions[year==2025, total_lpr], big.mark=',')}")
  cli::cli_alert_info("Ultimate LPR: {format(assumptions[year==max(projection_years), total_lpr], big.mark=',')}")

  # Step 4: Project total LPR immigration (L)
  cli::cli_h3("Step 4: Project total LPR immigration (L)")
  lpr_immigration <- project_lpr_immigration(assumptions, lpr_distribution)

  # Verify totals
  lpr_totals <- lpr_immigration[, .(projected = sum(immigration)), by = year]
  lpr_totals <- merge(lpr_totals, assumptions[, .(year, expected = total_lpr)], by = "year")
  lpr_totals[, diff := abs(projected - expected)]
  max_diff <- max(lpr_totals$diff)
  if (max_diff > 1) {
    cli::cli_alert_warning("LPR totals differ from assumptions by up to {round(max_diff)}")
  } else {
    cli::cli_alert_success("LPR totals match TR2025 assumptions")
  }

  # Step 5: Split into NEW and AOS
  cli::cli_h3("Step 5: Split into NEW and AOS")
  new_aos_split <- split_lpr_new_aos(
    lpr_immigration = lpr_immigration,
    assumptions = assumptions,
    method = "assumption"
  )

  # Step 6: Project emigration (E)
  cli::cli_h3("Step 6: Project legal emigration (E)")
  emigration <- project_legal_emigration(assumptions, emigration_distribution)

  # Verify emigration totals
  emig_totals <- emigration[, .(projected = sum(emigration)), by = year]
  emig_totals <- merge(emig_totals, assumptions[, .(year, expected = total_emigration)], by = "year")
  emig_totals[, diff := abs(projected - expected)]
  max_diff <- max(emig_totals$diff)
  if (max_diff > 1) {
    cli::cli_alert_warning("Emigration totals differ from assumptions by up to {round(max_diff)}")
  } else {
    cli::cli_alert_success("Emigration totals match TR2025 (25% of LPR)")
  }

  # Step 7: Calculate net LPR (NL = L - E)
  cli::cli_h3("Step 7: Calculate net LPR (NL = L - E)")
  net_lpr <- calculate_net_lpr(lpr_immigration, emigration)

  # Verify net totals
  net_totals <- net_lpr[, .(projected = sum(net_lpr)), by = year]
  net_totals <- merge(net_totals, assumptions[, .(year, expected = net_lpr_total)], by = "year")
  net_totals[, diff := abs(projected - expected)]
  max_diff <- max(net_totals$diff)
  if (max_diff > 1) {
    cli::cli_alert_warning("Net LPR totals differ from expected by up to {round(max_diff)}")
  } else {
    cli::cli_alert_success("Net LPR totals = 75% of gross LPR (L - E)")
  }

  # Summary
  cli::cli_h3("Summary")
  sample_year <- 2030
  if (sample_year %in% projection_years) {
    l_total <- lpr_immigration[year == sample_year, sum(immigration)]
    new_total <- new_aos_split$new_arrivals[year == sample_year, sum(new_arrivals)]
    aos_total <- new_aos_split$aos[year == sample_year, sum(aos)]
    e_total <- emigration[year == sample_year, sum(emigration)]
    nl_total <- net_lpr[year == sample_year, sum(net_lpr)]

    cli::cli_alert_info("Sample year {sample_year}:")
    cli::cli_alert_info("  L (Total LPR):   {format(round(l_total), big.mark=',')}")
    cli::cli_alert_info("  NEW (New arr.):  {format(round(new_total), big.mark=',')} ({round(new_total/l_total*100, 1)}%)")
    cli::cli_alert_info("  AOS (Adj.stat.): {format(round(aos_total), big.mark=',')} ({round(aos_total/l_total*100, 1)}%)")
    cli::cli_alert_info("  E (Emigration):  {format(round(e_total), big.mark=',')} (25% of L)")
    cli::cli_alert_info("  NL (Net LPR):    {format(round(nl_total), big.mark=',')} (L - E)")
  }

  list(
    # 5 required outputs
    lpr_immigration = lpr_immigration,
    new_arrivals = new_aos_split$new_arrivals,
    aos = new_aos_split$aos,
    emigration = emigration,
    net_lpr = net_lpr,
    # Supporting data
    distributions = list(
      lpr = lpr_distribution,
      emigration = emigration_distribution
    ),
    new_aos_ratio = new_aos_ratio,
    assumptions = assumptions
  )
}

#' LPR Immigration Projection
#'
#' Functions for projecting LPR immigration by age and sex using:
#' - DHS data for age-sex distributions (with Beers interpolation to single years)
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
NULL

# ===========================================================================
# TR2025-DERIVED IMMIGRATION DISTRIBUTION
# ===========================================================================

#' Derive TR2025 implied immigration age-sex distribution
#'
#' @description
#' Calculates the implied net immigration age-sex distribution from TR2025
#' population projections by analyzing year-over-year population changes.
#'
#' Method:
#' For ages 1-99, net immigration is derived as:
#'   NI(age, year) = P(age, year) - P(age-1, year-1) * (1 - q_{age-1})
#'
#' This reverse-engineers what net immigration TR2025 must have assumed.
#'
#' @param years Integer vector: years to analyze (default: 2025:2050)
#' @param tr_pop_file Character: path to TR2025 population file
#' @param tr_qx_male_file Character: path to TR2025 male mortality file
#' @param tr_qx_female_file Character: path to TR2025 female mortality file
#'
#' @return data.table with columns: age, sex, distribution (sums to 1)
#'
#' @export
derive_tr2025_immigration_distribution <- function(
    years = 2025:2050,
    tr_pop_file = "data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv",
    tr_qx_male_file = "data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv",
    tr_qx_female_file = "data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv"
) {
  # Load TR2025 data
  cli::cli_alert_info("Loading TR2025 data for immigration distribution derivation...")
  tr_pop <- data.table::fread(tr_pop_file)
  tr_qx_m <- data.table::fread(tr_qx_male_file)
  tr_qx_f <- data.table::fread(tr_qx_female_file)

  # Calculate implied net immigration for each year
  all_ni <- data.table::rbindlist(lapply(years, function(year) {
    calculate_implied_ni_for_year(year, tr_pop, tr_qx_m, tr_qx_f)
  }))

  # Average across years to get stable distribution
  avg_dist <- all_ni[, .(
    net_immigration = mean(net_immigration)
  ), by = .(age, sex)]

  # Calculate distribution (normalize to sum to 1)
  # Note: Some ages may have negative values (net emigration)
  # We keep the shape but normalize so total sums to 1
  total <- sum(avg_dist$net_immigration)
  avg_dist[, distribution := net_immigration / total]

  # Report summary
  pos_ages <- avg_dist[distribution > 0, .N]
  neg_ages <- avg_dist[distribution < 0, .N]
  cli::cli_alert_success(
    "Derived TR2025 immigration distribution: {pos_ages} ages with immigration, {neg_ages} ages with emigration"
  )

  # Report age groups
  avg_dist[, age_group := data.table::fcase(
    age <= 24, "0-24",
    age <= 44, "25-44",
    age <= 64, "45-64",
    default = "65+"
  )]
  by_group <- avg_dist[, .(pct = sum(distribution) * 100), by = age_group]
  cli::cli_alert_info("Distribution: 0-24: {round(by_group[age_group=='0-24', pct], 1)}%, 25-44: {round(by_group[age_group=='25-44', pct], 1)}%, 45-64: {round(by_group[age_group=='45-64', pct], 1)}%, 65+: {round(by_group[age_group=='65+', pct], 1)}%")

  # Return in standard format
  data.table::setorder(avg_dist, sex, age)
  avg_dist[, .(age, sex, distribution)]
}

#' Calculate implied net immigration for a single year
#'
#' @param year Integer: the year to calculate immigration for
#' @param tr_pop data.table: TR2025 population data
#' @param tr_qx_m data.table: TR2025 male death probabilities
#' @param tr_qx_f data.table: TR2025 female death probabilities
#'
#' @return data.table with columns: year, age, sex, net_immigration
#'
#' @keywords internal
calculate_implied_ni_for_year <- function(year, tr_pop, tr_qx_m, tr_qx_f) {
  tr_prev <- tr_pop[Year == year - 1]
  tr_curr <- tr_pop[Year == year]
  qx_m <- tr_qx_m[Year == year - 1]
  qx_f <- tr_qx_f[Year == year - 1]

  # Calculate for ages 1-99
  results <- data.table::rbindlist(lapply(1:99, function(a) {
    # Current year population at age a
    curr_m <- tr_curr[Age == a, `M Tot`]
    curr_f <- tr_curr[Age == a, `F Tot`]

    # Previous year population at age a-1
    prev_m <- tr_prev[Age == a - 1, `M Tot`]
    prev_f <- tr_prev[Age == a - 1, `F Tot`]

    # Survival probability
    surv_m <- 1 - qx_m[1, get(as.character(a - 1))]
    surv_f <- 1 - qx_f[1, get(as.character(a - 1))]

    # Implied net immigration
    ni_m <- curr_m - prev_m * surv_m
    ni_f <- curr_f - prev_f * surv_f

    data.table::data.table(
      year = year,
      age = a,
      sex = c("male", "female"),
      net_immigration = c(ni_m, ni_f)
    )
  }))

  # Add age 0 with zero (we can't derive it without birth data)
  age0 <- data.table::data.table(
    year = year,
    age = 0L,
    sex = c("male", "female"),
    net_immigration = c(0, 0)
  )

  rbind(age0, results)
}

#' Get immigration distribution based on config method
#'
#' @description
#' Returns the immigration age-sex distribution based on the configured method.
#' Supports "dhs" (current DHS-based) or "tr2025_derived" (from TR2025 population).
#'
#' @param method Character: "dhs" or "tr2025_derived"
#' @param dhs_data data.table: DHS data (required if method = "dhs")
#' @param dhs_years Integer vector: years for DHS averaging
#' @param tr_config List: TR2025 file paths (required if method = "tr2025_derived")
#'
#' @return data.table with columns: age, sex, distribution
#'
#' @export
get_immigration_distribution <- function(
    method = c("dhs", "tr2025_derived"),
    dhs_data = NULL,
    dhs_years = 2018:2023,
    tr_config = NULL
) {
  method <- match.arg(method)

  if (method == "dhs") {
    if (is.null(dhs_data)) {
      cli::cli_abort("dhs_data required when method = 'dhs'")
    }
    cli::cli_alert_info("Using DHS-derived immigration distribution")
    return(calculate_lpr_distribution_dhs(dhs_data, dhs_years))
  }

  if (method == "tr2025_derived") {
    cli::cli_alert_info("Using TR2025-derived immigration distribution")

    # Get file paths from config or use defaults
    tr_pop_file <- if (!is.null(tr_config$population_file)) {
      tr_config$population_file
    } else {
      "data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv"
    }
    tr_qx_male_file <- if (!is.null(tr_config$qx_male_file)) {
      tr_config$qx_male_file
    } else {
      "data/raw/SSA_TR2025/DeathProbsE_M_Alt2_TR2025.csv"
    }
    tr_qx_female_file <- if (!is.null(tr_config$qx_female_file)) {
      tr_config$qx_female_file
    } else {
      "data/raw/SSA_TR2025/DeathProbsE_F_Alt2_TR2025.csv"
    }
    tr_years <- if (!is.null(tr_config$derivation_years)) {
      tr_config$derivation_years
    } else {
      2025:2050
    }

    return(derive_tr2025_immigration_distribution(
      years = tr_years,
      tr_pop_file = tr_pop_file,
      tr_qx_male_file = tr_qx_male_file,
      tr_qx_female_file = tr_qx_female_file
    ))
  }
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
                                           reference_years = 2018:2023) {
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

  # Interpolate to single years using Beers
  dist_single <- beers_interpolate_dhs(dist_5yr)

  # Report single-year summary
  by_sex_single <- dist_single[, .(pct = sum(distribution) * 100), by = sex]
  cli::cli_alert_info(
    "LPR distribution (single-year): {round(by_sex_single[sex=='female', pct], 1)}% female, {round(by_sex_single[sex=='male', pct], 1)}% male"
  )

  dist_single
}

#' Calculate emigration distribution from DHS data
#'
#' @description
#' Uses the same age-sex distribution as immigration since DHS does not
#' provide emigration-specific data. This assumes emigrants have a similar
#' age-sex profile to immigrants.
#'
#' @param dhs_data data.table from load_dhs_lpr_data()
#' @param reference_years Years to average for distribution (default: 2018:2023)
#' @return data.table with columns: age, sex, distribution
#' @export
calculate_emigration_distribution_dhs <- function(dhs_data,
                                                   reference_years = 2018:2023) {
  # Use same distribution as immigration (DHS doesn't have emigration data)
  cli::cli_alert_info("Using DHS immigration distribution for emigration (no emigration-specific data)")
  calculate_lpr_distribution_dhs(dhs_data, reference_years)
}

#' Interpolate age groups to single years
#'
#' @description
#' Converts 5-year age group distributions to single-year-of-age distributions
#' using uniform distribution within each group. This preserves the total
#' distribution for each age group while providing single-year granularity.
#'
#' @param dist data.table with columns: age_min, age_max, sex, distribution
#' @param max_age Integer: maximum single-year age (default: 100, representing 100+)
#' @return data.table with columns: age (0 to max_age), sex, distribution
#' @keywords internal
beers_interpolate_dhs <- function(dist, max_age = 100) {
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
                                     reference_years = 2016:2019) {
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
#' @export
get_tr2025_lpr_assumptions <- function(years = 2025:2099,
                                        alternative = c("intermediate", "low-cost", "high-cost"),
                                        data_dir = "data/raw/SSA_TR2025") {
  alternative <- match.arg(alternative)

 # Try to load from V.A2
  va2_available <- tryCatch({
    # Check if the loader function exists
    if (!exists("load_tr2025_immigration_assumptions", mode = "function")) {
      source(here::here("R/data_acquisition/tr2025_data.R"))
    }
    TRUE
  }, error = function(e) FALSE)

  if (va2_available) {
    result <- get_tr2025_lpr_assumptions_va2(years, alternative, data_dir)
  } else {
    # Fallback to hardcoded values
    cli::cli_alert_warning("V.A2 data not available, using hardcoded assumptions")
    result <- get_tr2025_lpr_assumptions_hardcoded(years)
  }

  result
}

#' Get LPR assumptions from V.A2 data
#'
#' @keywords internal
get_tr2025_lpr_assumptions_va2 <- function(years,
                                            alternative = "intermediate",
                                            data_dir = "data/raw/SSA_TR2025") {
  # Load V.A2 immigration assumptions
  imm <- load_tr2025_immigration_assumptions(data_dir = data_dir, cache = TRUE)

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
  result <- result[, .(
    year = year,
    total_lpr = (lpr_inflow + lpr_aos) * 1000,  # Total = NEW + AOS, convert to persons
    lpr_new = lpr_inflow * 1000,                 # NEW arrivals only
    lpr_aos = lpr_aos * 1000,                    # AOS only
    total_emigration = lpr_outflow * 1000,
    net_lpr_total = lpr_net * 1000,
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

#' Fallback hardcoded LPR assumptions
#'
#' @keywords internal
get_tr2025_lpr_assumptions_hardcoded <- function(years) {
  result <- data.table::data.table(year = years)

  # Set LPR immigration levels per TR2025 intermediate assumptions
  # Total LPR = NEW + AOS
  result[, total_lpr := data.table::fcase(
    year == 2024, 1263000L,
    year %in% 2025:2026, 1213000L,
    year >= 2027, 1050000L,
    default = 1050000L
  )]

  # Approximate NEW/AOS split (~60% NEW, ~40% AOS based on historical patterns)
  result[, lpr_new := round(total_lpr * 0.60)]
  result[, lpr_aos := total_lpr - lpr_new]

  # Emigration = 25% of LPR
  result[, total_emigration := round(total_lpr * 0.25)]

  # Net LPR
  result[, net_lpr_total := total_lpr - total_emigration]

  # Mark as hardcoded
  result[, data_type := "hardcoded"]

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
                                distribution_years = 2018:2023,
                                new_aos_ratio_years = 2016:2019) {
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
  assumptions <- get_tr2025_lpr_assumptions(projection_years)
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

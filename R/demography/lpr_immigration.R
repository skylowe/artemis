#' LPR Immigration Projection - Hybrid B+C Approach
#'
#' Functions for projecting LPR immigration by age and sex using:
#' - CBO data for single-year-of-age distributions (immigration and emigration)
#' - DHS data for aggregate NEW/AOS split ratio
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
# CBO DATA LOADING AND DISTRIBUTION FUNCTIONS
# ===========================================================================

#' Load and prepare CBO migration data
#'
#' @param file_path Path to CBO gross migration CSV
#' @return data.table with cleaned CBO data
#' @export
load_cbo_migration <- function(file_path = "data/raw/cbo/grossMigration_byYearAgeSexStatusFlow.csv") {
  if (!file.exists(file_path)) {
    cli::cli_abort("CBO migration file not found: {file_path}")
  }

  cbo_data <- data.table::fread(file_path)

  # Clean column names
  data.table::setnames(cbo_data,
    old = c("immigration_status", "migration_flow", "number_of_people"),
    new = c("status", "flow", "count"),
    skip_absent = TRUE
  )

  # Convert age to integer, handling "100+" and "-1"
  cbo_data[, age := gsub("\\+", "", age)]  # Remove +
  cbo_data[, age := as.integer(age)]

  # Filter out invalid ages (e.g., -1)
  cbo_data <- cbo_data[age >= 0 & age <= 100]

  # Standardize sex
  cbo_data[, sex := tolower(sex)]

  cli::cli_alert_info("Loaded CBO data: {format(nrow(cbo_data), big.mark=',')} rows, years {min(cbo_data$year)}-{max(cbo_data$year)}")

  cbo_data
}

#' Calculate LPR immigration distribution from CBO data
#'
#' @param cbo_data data.table from load_cbo_migration()
#' @param reference_years Years to average for distribution (default: 2021:2024)
#' @return data.table with columns: age, sex, distribution
#' @export
calculate_lpr_distribution_cbo <- function(cbo_data,
                                           reference_years = 2021:2024) {
  cbo_data <- data.table::as.data.table(cbo_data)

  # Filter to LPR+ immigration in reference years
  ref_data <- cbo_data[
    status == "LPR+" &
    flow == "immigration" &
    year %in% reference_years &
    age >= 0 & age <= 99
  ]

  if (nrow(ref_data) == 0) {
    cli::cli_abort("No LPR+ immigration data found for years {paste(reference_years, collapse=', ')}")
  }

  # Aggregate across years
  dist <- ref_data[, .(count = sum(count, na.rm = TRUE)), by = .(age, sex)]

  # Calculate distribution
  total <- sum(dist$count)
  dist[, distribution := count / total]

  # Report summary
  by_sex <- dist[, .(pct = sum(distribution) * 100), by = sex]
  cli::cli_alert_info(
    "LPR distribution from CBO {min(reference_years)}-{max(reference_years)}: {round(by_sex[sex=='female', pct], 1)}% female, {round(by_sex[sex=='male', pct], 1)}% male"
  )

  # Verify all ages 0-99 are present
  expected_ages <- 0:99
  for (s in c("male", "female")) {
    present <- dist[sex == s, age]
    missing <- setdiff(expected_ages, present)
    if (length(missing) > 0) {
      # Add missing ages with 0 count
      missing_rows <- data.table::data.table(
        age = missing,
        sex = s,
        count = 0,
        distribution = 0
      )
      dist <- data.table::rbindlist(list(dist, missing_rows))
    }
  }

  # Reorder
  data.table::setorder(dist, sex, age)

  dist[, .(age, sex, distribution)]
}

#' Calculate emigration distribution from CBO data
#'
#' @param cbo_data data.table from load_cbo_migration()
#' @param reference_years Years to average for distribution (default: 2021:2024)
#' @return data.table with columns: age, sex, distribution
#' @export
calculate_emigration_distribution_cbo <- function(cbo_data,
                                                   reference_years = 2021:2024) {
  cbo_data <- data.table::as.data.table(cbo_data)

  # Filter to LPR+ emigration in reference years
  ref_data <- cbo_data[
    status == "LPR+" &
    flow == "emigration" &
    year %in% reference_years &
    age >= 0 & age <= 99
  ]

  if (nrow(ref_data) == 0) {
    cli::cli_abort("No LPR+ emigration data found for years {paste(reference_years, collapse=', ')}")
  }

  # Aggregate across years
  dist <- ref_data[, .(count = sum(count, na.rm = TRUE)), by = .(age, sex)]

  # Calculate distribution
  total <- sum(dist$count)
  dist[, distribution := count / total]

  # Report summary
  by_sex <- dist[, .(pct = sum(distribution) * 100), by = sex]
  cli::cli_alert_info(
    "Emigration distribution from CBO {min(reference_years)}-{max(reference_years)}: {round(by_sex[sex=='female', pct], 1)}% female, {round(by_sex[sex=='male', pct], 1)}% male"
  )

  # Verify all ages 0-99 are present
  expected_ages <- 0:99
  for (s in c("male", "female")) {
    present <- dist[sex == s, age]
    missing <- setdiff(expected_ages, present)
    if (length(missing) > 0) {
      missing_rows <- data.table::data.table(
        age = missing,
        sex = s,
        count = 0,
        distribution = 0
      )
      dist <- data.table::rbindlist(list(dist, missing_rows))
    }
  }

  # Reorder
  data.table::setorder(dist, sex, age)

  dist[, .(age, sex, distribution)]
}

# ===========================================================================
# DHS DATA LOADING AND NEW/AOS RATIO
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

#' Get TR2025 LPR immigration assumptions
#'
#' @param years Integer vector: years to get assumptions for (default: 2025:2099)
#' @return data.table with columns: year, total_lpr, total_emigration
#' @export
get_tr2025_lpr_assumptions <- function(years = 2025:2099) {
  result <- data.table::data.table(year = years)

  # Set LPR immigration levels per TR2025 intermediate assumptions
  result[, total_lpr := data.table::fcase(
    year == 2024, 1263000L,
    year %in% 2025:2026, 1213000L,
    year >= 2027, 1050000L,
    default = 1050000L
  )]

  # Emigration = 25% of LPR
  result[, total_emigration := round(total_lpr * 0.25)]

  # Net LPR
  result[, net_lpr_total := total_lpr - total_emigration]

  result
}

# ===========================================================================
# PROJECTION FUNCTIONS
# ===========================================================================

#' Project LPR immigration by age and sex
#'
#' @param assumptions TR2025 assumptions data.table
#' @param distribution Age-sex distribution from CBO
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
#' @param distribution Age-sex distribution from CBO
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
#' @param lpr_immigration Projected LPR by year, age, sex
#' @param new_ratio Ratio of new arrivals (from DHS)
#' @return list with new_arrivals and aos data.tables
#' @export
split_lpr_new_aos <- function(lpr_immigration, new_ratio) {
  lpr_immigration <- data.table::as.data.table(lpr_immigration)

  aos_ratio <- 1 - new_ratio

  # Create NEW arrivals
  new_arrivals <- data.table::copy(lpr_immigration)
  new_arrivals[, new_arrivals := immigration * new_ratio]
  new_arrivals[, immigration := NULL]

  # Create AOS
  aos <- data.table::copy(lpr_immigration)
  aos[, aos := immigration * aos_ratio]
  aos[, immigration := NULL]

  cli::cli_alert_info("Split LPR: NEW={round(new_ratio*100, 1)}%, AOS={round(aos_ratio*100, 1)}%")

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

#' Run complete LPR immigration projection (Hybrid B+C)
#'
#' @param cbo_data CBO migration data (or NULL to load from file)
#' @param dhs_data DHS expanded tables data (or NULL to load from file)
#' @param projection_years Years to project (default: 2025:2099)
#' @param distribution_years Years for CBO distribution (default: 2021:2024)
#' @param new_aos_ratio_years Years for DHS NEW/AOS ratio (default: 2016:2019)
#' @return list with all 5 outputs plus distributions, ratios, assumptions
#' @export
run_lpr_projection <- function(cbo_data = NULL,
                                dhs_data = NULL,
                                projection_years = 2025:2099,
                                distribution_years = 2021:2024,
                                new_aos_ratio_years = 2016:2019) {
  cli::cli_h2("LPR Immigration Projection (Hybrid B+C)")

  # Load data if not provided
  if (is.null(cbo_data)) {
    cli::cli_h3("Loading CBO migration data")
    cbo_data <- load_cbo_migration()
  }

  if (is.null(dhs_data)) {
    cli::cli_h3("Loading DHS LPR data")
    dhs_data <- load_dhs_lpr_data()
  }

  # Step 1: Calculate distributions from CBO
  cli::cli_h3("Step 1: Calculate age-sex distributions from CBO")
  lpr_distribution <- calculate_lpr_distribution_cbo(cbo_data, distribution_years)
  emigration_distribution <- calculate_emigration_distribution_cbo(cbo_data, distribution_years)

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
  new_aos_split <- split_lpr_new_aos(lpr_immigration, new_aos_ratio$new_ratio)

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


# ===========================================================================
# LEGACY FUNCTIONS (kept for compatibility)
# ===========================================================================

#' Calculate LPR immigration age-sex distribution (legacy - DHS based)
#'
#' @description
#' DEPRECATED: Use calculate_lpr_distribution_cbo() instead.
#' This function is kept for backward compatibility.
#'
#' @param dhs_age_sex data.table with DHS age-sex data
#' @param reference_years Integer vector: years to use for distribution
#' @param admission_type Character: "new_arrival", "aos", or "total"
#' @return data.table with columns: age_min, age_max, sex, distribution
#' @export
calculate_lpr_age_sex_distribution <- function(dhs_age_sex,
                                                reference_years = 2016:2020,
                                                admission_type = "total") {
  cli::cli_alert_warning("calculate_lpr_age_sex_distribution() is deprecated. Use calculate_lpr_distribution_cbo()")

  dhs_age_sex <- data.table::as.data.table(dhs_age_sex)

  # Filter to reference years
  ref_data <- dhs_age_sex[fiscal_year %in% reference_years]

  if (nrow(ref_data) == 0) {
    cli::cli_abort("No data found for reference years {paste(reference_years, collapse=', ')}")
  }

  # Filter by admission type if specified
  if (admission_type == "new_arrival") {
    ref_data <- ref_data[admission_type == "new_arrival"]
  } else if (admission_type == "aos") {
    ref_data <- ref_data[admission_type == "aos"]
  }

  # Aggregate across years by age group and sex
  dist <- ref_data[, .(count = sum(count, na.rm = TRUE)), by = .(age_min, age_max, sex)]

  # Calculate distribution
  total <- sum(dist$count)
  dist[, distribution := count / total]

  dist[, .(age_min, age_max, sex, distribution)]
}

#' Interpolate 5-year age groups to single years using Beers formula
#'
#' @description
#' DEPRECATED: Not needed when using CBO single-year data.
#'
#' @param dist data.table with columns: age_min, age_max, sex, distribution
#' @return data.table with columns: age, sex, distribution
#' @export
beers_interpolate <- function(dist) {
  cli::cli_alert_warning("beers_interpolate() is deprecated. CBO data provides single-year ages.")

  # Beers ordinary coefficients for interior groups
  beers_coef <- matrix(c(
     0.0016, -0.0832,  0.6096,  0.5936, -0.1216,
    -0.0336,  0.0848,  0.3744,  0.6784, -0.1040,
    -0.0416,  0.1136,  0.2464,  0.7216, -0.0400,
    -0.0240,  0.0816,  0.1840,  0.7200,  0.0384,
     0.0016,  0.0128,  0.1744,  0.6816,  0.1296
  ), nrow = 5, ncol = 5, byrow = TRUE)

  results <- list()

  for (s in c("male", "female")) {
    sex_dist <- dist[sex == s]
    data.table::setorder(sex_dist, age_min)

    groups <- sex_dist$distribution
    n_groups <- length(groups)

    single_ages <- numeric()

    for (g in 1:n_groups) {
      age_start <- sex_dist$age_min[g]

      if (g < n_groups) {
        n_years <- sex_dist$age_min[g + 1] - age_start
      } else {
        n_years <- 25
      }

      if (n_years == 5) {
        g_vals <- numeric(5)
        for (k in 1:5) {
          idx <- g + k - 3
          if (idx < 1) idx <- 1
          if (idx > n_groups) idx <- n_groups
          g_vals[k] <- groups[idx]
        }

        for (j in 1:5) {
          single_ages <- c(single_ages, sum(beers_coef[j, ] * g_vals))
        }
      } else if (n_years == 1) {
        single_ages <- c(single_ages, groups[g])
      } else if (n_years == 4) {
        single_ages <- c(single_ages, rep(groups[g] / 4, 4))
      } else {
        single_ages <- c(single_ages, rep(groups[g] / n_years, n_years))
      }
    }

    n_ages <- length(single_ages)
    ages <- 0:(n_ages - 1)

    results[[s]] <- data.table::data.table(
      age = ages,
      sex = s,
      distribution = single_ages
    )
  }

  result <- data.table::rbindlist(results)
  result[, distribution := distribution / sum(distribution)]

  result
}

#' Convert fiscal year to calendar year (legacy)
#'
#' @param data data.table with fiscal_year column
#' @param method Character: "weighted" or "simple"
#' @return data.table with calendar_year column added
#' @export
convert_fiscal_to_calendar <- function(data, method = "simple") {
  data <- data.table::copy(data)

  if (method == "simple") {
    data[, calendar_year := fiscal_year]
  } else if (method == "weighted") {
    cli::cli_alert_warning("Weighted method requires quarterly data; using simple method")
    data[, calendar_year := fiscal_year]
  }

  data
}

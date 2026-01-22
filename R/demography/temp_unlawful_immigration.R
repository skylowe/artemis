#' Temporary or Unlawfully Present Immigration Projection
#'
#' Functions for projecting temporary or unlawfully present (O) immigration
#' by age, sex, and type following TR2025 Section 1.5 methodology.
#'
#' O immigrants include:
#' - Never-authorized (N): Entered without authorization
#' - Nonimmigrants (I): Temporary legal residents (workers, students)
#' - Visa-overstayers (V): Nonimmigrants who overstayed
#'
#' Primary equations:
#' - OI^z_{x,s,t} = TO^z × ODIST_{x,s,t} (Eq 1.5.1)
#' - OE^z_{x,s,t} = ORate × OP^{z-1} (Eq 1.5.2)
#' - NO^z_{x,s,t} = OI - OE - AOS (Eq 1.5.3)
#' - OP^z_{x,s,t} = OP^{z-1} + OI - OE - AOS - OD (Eq 1.5.4)
#'
#' @name temp_unlawful_immigration
NULL

# =============================================================================
# ODIST CALCULATION (EQUATION 1.5.1)
# =============================================================================

#' Calculate O immigration from ACS and LPR data
#'
#' @description
#' Calculates historical O immigration by subtracting LPR NEW arrivals from
#' ACS foreign-born new arrivals. This follows TR2025 methodology:
#' O Immigration = ACS foreign-born (with undercount) - LPR NEW arrivals
#'
#' @param acs_new_arrivals data.table with ACS new arrivals by year, age, sex
#' @param lpr_new_arrivals data.table with LPR NEW arrivals by year, age, sex
#' @param undercount_factors data.table with undercount factors by age group
#' @param years Integer vector of years to process (default: 2015:2019)
#'
#' @return data.table with O immigration by year, age, sex
#'
#' @details
#' Per TR2025 Section 1.5.c:
#' "The estimated temporary or unlawfully present immigration is calculated
#' by taking the foreign born from the ACS (after smoothing and applying
#' the undercount factors) and subtracting the LPR new arrivals."
#'
#' @export
calculate_o_immigration <- function(acs_new_arrivals,
                                     lpr_new_arrivals,
                                     undercount_factors,
                                     years = 2015:2019) {
  checkmate::assert_data_table(acs_new_arrivals)
  checkmate::assert_data_table(lpr_new_arrivals)
  checkmate::assert_data_table(undercount_factors)

  # Apply undercount correction to ACS data
  acs_corrected <- apply_undercount_to_acs(acs_new_arrivals, undercount_factors)

  # Ensure LPR data has single-year ages (may need interpolation)
  lpr_single_age <- convert_lpr_to_single_age(lpr_new_arrivals, years)

  # Filter to requested years
  acs_filtered <- acs_corrected[year %in% years]
  lpr_filtered <- lpr_single_age[year %in% years]

  # Determine the LPR count column name (could be 'count', 'new_arrivals', or 'immigration')
  lpr_count_col <- intersect(c("new_arrivals", "count", "immigration"), names(lpr_filtered))[1]
  if (is.na(lpr_count_col)) {
    cli::cli_abort("LPR data must have column 'new_arrivals', 'count', or 'immigration'")
  }

  # Merge and calculate O immigration
  lpr_for_merge <- lpr_filtered[, .(year, age, sex, lpr_new = get(lpr_count_col))]
  merged <- merge(
    acs_filtered[, .(year, age, sex, acs_arrivals = adjusted_population)],
    lpr_for_merge,
    by = c("year", "age", "sex"),
    all.x = TRUE
  )

  # Fill missing LPR values with 0
  merged[is.na(lpr_new), lpr_new := 0]

  # Calculate O immigration (ensure non-negative)
  merged[, o_immigration := pmax(0, acs_arrivals - lpr_new)]

  # Report summary
  cli::cli_alert_info(
    "O immigration ({min(years)}-{max(years)}): {format(sum(merged$o_immigration), big.mark = ',')} total"
  )

  merged[, .(year, age, sex, o_immigration)]
}

#' Apply undercount correction to ACS new arrivals
#'
#' @keywords internal
apply_undercount_to_acs <- function(acs_data, undercount_factors) {
  dt <- data.table::copy(acs_data)

  # Assign age groups matching undercount factors
  dt[, age_group := data.table::fcase(
    age < 18, "0-17",
    age >= 18 & age < 35, "18-34",
    age >= 35 & age < 50, "35-49",
    age >= 50 & age < 65, "50-64",
    age >= 65, "65+"
  )]

  # Merge undercount factors
  dt <- merge(dt, undercount_factors[, .(age_group, undercount_factor)],
              by = "age_group", all.x = TRUE)

  # Default factor of 1.0 if missing
  dt[is.na(undercount_factor), undercount_factor := 1.0]

  # Apply correction
  dt[, adjusted_population := population * undercount_factor]

  dt
}

#' Convert LPR NEW arrivals from age groups to single years
#'
#' @description
#' Converts DHS age group data to single years using linear interpolation
#' within age groups.
#'
#' @keywords internal
convert_lpr_to_single_age <- function(lpr_data, years) {
  # Check if already single-year ages
  if ("age" %in% names(lpr_data) && max(lpr_data$age, na.rm = TRUE) > 20) {
    return(lpr_data)
  }

  results <- list()

  for (yr in years) {
    for (sx in c("male", "female")) {
      yr_sex_data <- lpr_data[fiscal_year == yr & sex == sx]

      if (nrow(yr_sex_data) == 0) next

      # Prepare for interpolation
      age_groups <- data.table::data.table(
        age_group = yr_sex_data$age_group,
        age_min = yr_sex_data$age_min,
        age_max = yr_sex_data$age_max,
        count = yr_sex_data$count
      )

      # Use simple linear interpolation within age groups
      single_ages <- interpolate_age_groups(age_groups)
      single_ages[, year := yr]
      single_ages[, sex := sx]

      results[[paste(yr, sx)]] <- single_ages
    }
  }

  data.table::rbindlist(results)
}

#' Interpolate age groups to single years (simple method)
#'
#' @keywords internal
interpolate_age_groups <- function(age_groups) {
  results <- list()

  for (i in seq_len(nrow(age_groups))) {
    grp <- age_groups[i]
    age_min <- grp$age_min
    age_max <- min(grp$age_max, 100)  # Cap at 99

    # Calculate number of single years
    n_ages <- age_max - age_min + 1

    # Distribute count evenly across ages
    count_per_age <- grp$count / n_ages

    results[[i]] <- data.table::data.table(
      age = age_min:age_max,
      count = count_per_age
    )
  }

  data.table::rbindlist(results)
}

#' Calculate ODIST - Age-Sex-Type Distribution
#'
#' @description
#' Calculates the ODIST distribution used to project O immigration.
#' Per TR2025: "This age-sex-type distribution is developed by using
#' average historical estimates of temporary or unlawfully present
#' immigrants entering the country from 2015 through 2019."
#'
#' @param o_immigration data.table from calculate_o_immigration()
#' @param type_splits data.table with type proportions by age/sex
#' @param reference_years Integer vector (default: 2015:2019)
#'
#' @return data.table with ODIST by age, sex, type
#'
#' @details
#' ODIST_{x,s,t} = AvgOI_{x,s,t} / sum(AvgOI)
#'
#' @export
calculate_odist <- function(o_immigration,
                            type_splits = NULL,
                            reference_years = 2015:2019) {
  checkmate::assert_data_table(o_immigration)

  # Filter to reference years
  ref_data <- o_immigration[year %in% reference_years]

  if (nrow(ref_data) == 0) {
    cli::cli_abort("No O immigration data for reference years {reference_years}")
  }

  # Calculate average across years by age and sex
  avg_oi <- ref_data[, .(avg_o_immigration = mean(o_immigration)),
                     by = .(age, sex)]

  # Get type splits (default to no split if not provided)
  if (is.null(type_splits)) {
    type_splits <- get_default_type_splits()
  }

  # Apply type splits to create age-sex-type distribution
  odist <- apply_type_splits(avg_oi, type_splits)

  # Calculate distribution (normalize to sum to 1)
  total <- sum(odist$avg_o_immigration)
  odist[, odist := avg_o_immigration / total]

  cli::cli_alert_success(
    "Calculated ODIST: {nrow(odist)} age-sex-type combinations"
  )

  # Verify sums to 1
  if (abs(sum(odist$odist) - 1) > 0.001) {
    cli::cli_alert_warning("ODIST does not sum to 1.0")
  }

  odist
}

# =============================================================================
# HISTORICAL TYPE INTERPOLATION (TR2025 Methodology)
# =============================================================================
# Per TR2025: "It is assumed that all temporary or unlawfully present immigrants
# were nonimmigrants as of December 31, 1963. Between December 31, 1963, and
# December 31, 2010, the percentage of total temporary or unlawfully present
# immigrants by age and sex in each type is linearly interpolated from the
# percentages at those two points in time. Between December 31, 2010, and
# December 31, 2015, a similar interpolation is done from the percentages at
# those two points in time."
# =============================================================================

#' Get type splits using historical interpolation (TR2025 methodology)
#'
#' @description
#' Calculates type proportions (N/I/V) using the TR2025 historical interpolation
#' methodology with anchor points at 1963, 2010, and 2015.
#'
#' @param year Integer: year for type splits
#' @param age Integer or vector: age(s) for type splits
#' @param sex Character: "male" or "female"
#' @param anchor_points List with type proportions at anchor years (optional)
#'
#' @return data.table with type_n, type_i, type_v proportions
#'
#' @details
#' TR2025 specifies three anchor points:
#' - Dec 31, 1963: 100% nonimmigrant (I = 1.0, N = 0, V = 0)
#' - Dec 31, 2010: Estimated from DHS unauthorized + nonimmigrant stock
#' - Dec 31, 2015: Current proportions from DHS data
#'
#' Linear interpolation is used between anchor points:
#' - 1963-2010: Gradual shift from all-NI to mixed population
#' - 2010-2015: Adjustment based on updated DHS estimates
#' - 2015+: Use 2015 proportions (or slight extrapolation)
#'
#' @export
get_type_splits_interpolated <- function(year,
                                          age = 0:99,
                                          sex = c("male", "female"),
                                          anchor_points = NULL) {
  # Get anchor point proportions
  if (is.null(anchor_points)) {
    anchor_points <- get_type_anchor_points()
  }

  # Create age-sex grid
  result <- data.table::CJ(age = age, sex = sex)

  # Get anchor proportions for each age-sex combination
  result <- merge(result, anchor_points$y1963, by = c("age", "sex"),
                  all.x = TRUE, suffixes = c("", "_1963"))
  result <- merge(result, anchor_points$y2010, by = c("age", "sex"),
                  all.x = TRUE, suffixes = c("", "_2010"))
  result <- merge(result, anchor_points$y2015, by = c("age", "sex"),
                  all.x = TRUE, suffixes = c("", "_2015"))

  # Rename columns for clarity
  data.table::setnames(result,
    c("type_n", "type_i", "type_v"),
    c("type_n_1963", "type_i_1963", "type_v_1963"),
    skip_absent = TRUE
  )

  # Interpolate based on year
  if (year <= 1963) {
    # Before 1963: all nonimmigrant
    result[, type_n := 0]
    result[, type_i := 1]
    result[, type_v := 0]
  } else if (year <= 2010) {
    # 1963-2010: Linear interpolation
    progress <- (year - 1963) / (2010 - 1963)
    result[, type_n := type_n_1963 + progress * (type_n_2010 - type_n_1963)]
    result[, type_i := type_i_1963 + progress * (type_i_2010 - type_i_1963)]
    result[, type_v := type_v_1963 + progress * (type_v_2010 - type_v_1963)]
  } else if (year <= 2015) {
    # 2010-2015: Linear interpolation
    progress <- (year - 2010) / (2015 - 2010)
    result[, type_n := type_n_2010 + progress * (type_n_2015 - type_n_2010)]
    result[, type_i := type_i_2010 + progress * (type_i_2015 - type_i_2010)]
    result[, type_v := type_v_2010 + progress * (type_v_2015 - type_v_2010)]
  } else {
    # 2015+: Use 2015 proportions (stable)
    result[, type_n := type_n_2015]
    result[, type_i := type_i_2015]
    result[, type_v := type_v_2015]
  }

  # Ensure non-negative and normalize
  result[type_n < 0, type_n := 0]
  result[type_i < 0, type_i := 0]
  result[type_v < 0, type_v := 0]

  result[, total := type_n + type_i + type_v]
  result[total > 0, `:=`(
    type_n = type_n / total,
    type_i = type_i / total,
    type_v = type_v / total
  )]

  # Select only needed columns
  result[, .(age, sex, type_n, type_i, type_v)]
}

#' Get type proportion anchor points for historical interpolation
#'
#' @description
#' Returns type proportions at the three TR2025 anchor points:
#' - 1963: 100% nonimmigrant
#' - 2010: Based on DHS unauthorized and nonimmigrant stock estimates
#' - 2015: Current proportions from DHS data
#'
#' @param config Optional configuration to override anchor values
#'
#' @return List with data.tables for y1963, y2010, y2015
#'
#' @details
#' The 2010 and 2015 anchor points are derived from:
#' - DHS nonimmigrant stock (Apr 2008, Dec 2010, Apr 2016)
#' - DHS unauthorized immigrant population estimates
#' - Overstay percentage estimates (Input #25)
#'
#' @export
get_type_anchor_points <- function(config = NULL) {
  ages <- 0:100
  sexes <- c("male", "female")

  # =========================================================================
  # ANCHOR POINT 1: December 31, 1963
  # =========================================================================
  # Per TR2025: "all temporary or unlawfully present immigrants were
  # nonimmigrants as of December 31, 1963"
  # =========================================================================
  y1963 <- data.table::CJ(age = ages, sex = sexes)
  y1963[, type_n := 0]
  y1963[, type_i := 1]  # 100% nonimmigrant

  y1963[, type_v := 0]

  # =========================================================================
  # ANCHOR POINT 2: December 31, 2010
  # =========================================================================
  # Based on DHS estimates for this period:
  # - Total unauthorized: ~10.8M (DHS 2010 estimate)
  # - Nonimmigrant stock: ~1.9M (DHS Dec 2010)
  # - Total O: ~12.7M
  # - Overstay proportion of unauthorized: ~40% (Warren & Kerwin)
  #
  # Type proportions:
  # - I (nonimmigrant): 1.9M / 12.7M ≈ 15%
  # - V (overstayer): 10.8M × 0.40 / 12.7M ≈ 34%
  # - N (never-auth): remainder ≈ 51%
  # =========================================================================
  y2010 <- calculate_anchor_from_dhs(
    reference_year = 2010,
    total_unauthorized = 10800000,  # DHS 2010 estimate
    total_nonimmigrant = 1900000,   # DHS Dec 2010 stock
    overstay_pct_overall = 0.40,    # Warren & Kerwin estimate
    config = config
  )

  # =========================================================================
  # ANCHOR POINT 3: December 31, 2015
  # =========================================================================
  # Based on more recent DHS estimates:
  # - Total unauthorized: ~10.7M (DHS 2015 estimate)
  # - Nonimmigrant stock: ~2.1M (DHS Apr 2016, closest available)
  # - Total O: ~12.8M
  # - Overstay proportion: ~42% (updated Warren & Kerwin)
  #
  # Type proportions:
  # - I (nonimmigrant): 2.1M / 12.8M ≈ 16%
  # - V (overstayer): 10.7M × 0.42 / 12.8M ≈ 35%
  # - N (never-auth): remainder ≈ 49%
  # =========================================================================
  y2015 <- calculate_anchor_from_dhs(
    reference_year = 2015,
    total_unauthorized = 10700000,  # DHS 2015 estimate
    total_nonimmigrant = 2100000,   # DHS Apr 2016 stock
    overstay_pct_overall = 0.42,    # Updated estimate
    config = config
  )

  list(
    y1963 = y1963,
    y2010 = y2010,
    y2015 = y2015
  )
}

#' Calculate anchor point type proportions from DHS data
#'
#' @description
#' Calculates age-sex specific type proportions for a given anchor year
#' using DHS unauthorized and nonimmigrant estimates.
#'
#' @param reference_year Reference year for the anchor
#' @param total_unauthorized Total unauthorized population
#' @param total_nonimmigrant Total nonimmigrant stock
#' @param overstay_pct_overall Overall overstay percentage of unauthorized
#' @param config Optional configuration
#'
#' @return data.table with type proportions by age and sex
#'
#' @keywords internal
calculate_anchor_from_dhs <- function(reference_year,
                                       total_unauthorized,
                                       total_nonimmigrant,
                                       overstay_pct_overall,
                                       config = NULL) {
  ages <- 0:100
  sexes <- c("male", "female")

  result <- data.table::CJ(age = ages, sex = sexes)

  # Total O population

  total_o <- total_unauthorized + total_nonimmigrant

  # Get age-specific overstay percentages
  overstay_by_age <- get_overstay_percentages(config)

  # Get age distribution of nonimmigrants (working-age concentrated)
  ni_age_dist <- get_nonimmigrant_age_distribution()

  # Merge distributions
  result <- merge(result, overstay_by_age[, .(age, overstay_pct)],
                  by = "age", all.x = TRUE)
  result <- merge(result, ni_age_dist[, .(age, sex, ni_age_pct)],
                  by = c("age", "sex"), all.x = TRUE)

  # Fill missing values
  result[is.na(overstay_pct), overstay_pct := overstay_pct_overall]
  result[is.na(ni_age_pct), ni_age_pct := 1 / (length(ages) * length(sexes))]

  # Calculate type proportions
  # Type I: Nonimmigrant proportion based on stock
  result[, type_i := (total_nonimmigrant / total_o) * ni_age_pct * length(ages) * length(sexes)]
  result[type_i > 0.50, type_i := 0.50]  # Cap at 50%

  # Type V: Overstayer proportion of non-nonimmigrant
  result[, type_v := (1 - type_i) * overstay_pct]

  # Type N: Never-authorized (remainder)
  result[, type_n := 1 - type_i - type_v]
  result[type_n < 0, type_n := 0]

  # Normalize
  result[, total := type_n + type_i + type_v]
  result[total > 0, `:=`(
    type_n = type_n / total,
    type_i = type_i / total,
    type_v = type_v / total
  )]

  result[, .(age, sex, type_n, type_i, type_v)]
}

#' Get nonimmigrant age distribution
#'
#' @description
#' Returns the relative concentration of nonimmigrants by age and sex.
#' Nonimmigrants are concentrated in working ages (students, workers).
#'
#' @return data.table with ni_age_pct by age and sex
#'
#' @keywords internal
get_nonimmigrant_age_distribution <- function() {
  ages <- 0:100
  sexes <- c("male", "female")

  result <- data.table::CJ(age = ages, sex = sexes)

  # ===========================================================================
  # Age distribution based on visa categories
  # ===========================================================================
  # - F-1 students: concentrated ages 18-30
  # - H-1B workers: concentrated ages 25-45
  # - L-1 transfers: ages 30-50
  # - Other workers: various
  # ===========================================================================
  result[, ni_age_pct := data.table::fcase(
    age < 5, 0.02,
    age >= 5 & age < 18, 0.05,
    age >= 18 & age < 22, 0.15,   # College students
    age >= 22 & age < 25, 0.18,   # Grad students, early workers
    age >= 25 & age < 30, 0.20,   # Peak H-1B
    age >= 30 & age < 35, 0.15,
    age >= 35 & age < 40, 0.10,
    age >= 40 & age < 50, 0.08,
    age >= 50 & age < 60, 0.04,
    age >= 60, 0.02
  )]

  # Slight male skew in worker categories
  result[sex == "male" & age >= 25 & age < 50, ni_age_pct := ni_age_pct * 1.1]
  result[sex == "female" & age >= 25 & age < 50, ni_age_pct := ni_age_pct * 0.9]

  # Normalize to sum to 1
  result[, ni_age_pct := ni_age_pct / sum(ni_age_pct)]

  result
}

#' Calculate ODIST with historical type interpolation
#'
#' @description
#' Calculates ODIST using the TR2025 historical interpolation methodology.
#' Type proportions for each year in the reference period (2015-2019) are
#' calculated using interpolated anchor points.
#'
#' @param o_immigration data.table with O immigration by year, age, sex
#' @param reference_years Years for ODIST average (default: 2015:2019)
#' @param use_interpolation Logical: use historical interpolation (default: TRUE)
#'
#' @return data.table with ODIST by age, sex, type
#'
#' @export
calculate_odist_with_interpolation <- function(o_immigration,
                                                reference_years = 2015:2019,
                                                use_interpolation = TRUE) {
  checkmate::assert_data_table(o_immigration)

  cli::cli_h3("Calculating ODIST with historical type interpolation")

  # Filter to reference years
  ref_data <- o_immigration[year %in% reference_years]

  if (nrow(ref_data) == 0) {
    cli::cli_abort("No O immigration data for reference years {reference_years}")
  }

  # Get anchor points for interpolation
  anchor_points <- get_type_anchor_points()

  # For each year, get interpolated type splits and apply to O immigration
  results <- list()

  for (yr in reference_years) {
    yr_data <- ref_data[year == yr]

    if (nrow(yr_data) == 0) {
      cli::cli_alert_warning("No data for year {yr}, skipping")
      next
    }

    # Get interpolated type splits for this year
    type_splits <- get_type_splits_interpolated(
      year = yr,
      age = unique(yr_data$age),
      sex = unique(yr_data$sex),
      anchor_points = anchor_points
    )

    # Merge and apply type splits
    yr_typed <- merge(yr_data, type_splits, by = c("age", "sex"), all.x = TRUE)

    # Fill any missing type splits
    yr_typed[is.na(type_n), type_n := 0.50]
    yr_typed[is.na(type_i), type_i := 0.15]
    yr_typed[is.na(type_v), type_v := 0.35]

    # Create long format with type dimension
    yr_result <- data.table::rbindlist(list(
      yr_typed[, .(year, age, sex, type = "N",
                   o_immigration = o_immigration * type_n)],
      yr_typed[, .(year, age, sex, type = "I",
                   o_immigration = o_immigration * type_i)],
      yr_typed[, .(year, age, sex, type = "V",
                   o_immigration = o_immigration * type_v)]
    ))

    results[[as.character(yr)]] <- yr_result

    cli::cli_alert("Year {yr}: N={round(mean(type_splits$type_n)*100)}%, I={round(mean(type_splits$type_i)*100)}%, V={round(mean(type_splits$type_v)*100)}%")
  }

  # Combine all years
  all_years <- data.table::rbindlist(results)

  # Calculate average across years
  avg_oi <- all_years[, .(avg_o_immigration = mean(o_immigration)),
                      by = .(age, sex, type)]

  # Calculate distribution (normalize to sum to 1)
  total <- sum(avg_oi$avg_o_immigration)
  avg_oi[, odist := avg_o_immigration / total]

  cli::cli_alert_success("Calculated ODIST with {length(reference_years)} years of interpolated types")

  # Report summary
  type_totals <- avg_oi[, .(total = sum(odist)), by = type]
  cli::cli_alert_info(
    "Type proportions: N={round(type_totals[type=='N', total]*100, 1)}%, I={round(type_totals[type=='I', total]*100, 1)}%, V={round(type_totals[type=='V', total]*100, 1)}%"
  )

  avg_oi
}

# =============================================================================
# TYPE SPLIT FUNCTIONS (Legacy - kept for backward compatibility)
# =============================================================================

#' Get type split proportions by age and sex (Legacy)
#'
#' @description
#' Returns the proportion of O immigrants in each type category.
#' NOTE: For TR2025-compliant implementation, use get_type_splits_interpolated().
#'
#' @param reference_year Integer: year for type splits (default: 2019)
#' @param nonimmigrant_stock data.table with DHS nonimmigrant stock by age/sex
#' @param unauthorized_total Numeric: total unauthorized population
#'
#' @return data.table with type proportions by age and sex
#'
#' @export
calculate_type_splits <- function(reference_year = 2019,
                                   nonimmigrant_stock = NULL,
                                   unauthorized_total = NULL) {
  # Get age-specific overstay percentages
  overstay_pct <- get_overstay_percentages()

  # Load nonimmigrant stock if not provided
  if (is.null(nonimmigrant_stock)) {
    nonimmigrant_stock <- get_nonimmigrant_stock_distribution()
  }

  # Get unauthorized population distribution
  unauth_dist <- get_unauthorized_distribution()

  # Calculate type splits based on relative populations
  # This is an approximation of the TR methodology

  # Create age-sex grid
  ages <- 0:100
  sexes <- c("male", "female")

  result <- data.table::CJ(age = ages, sex = sexes)

  # Merge nonimmigrant proportions
  result <- merge(result, nonimmigrant_stock, by = c("age", "sex"), all.x = TRUE)
  result[is.na(ni_pct), ni_pct := 0]

  # Merge unauthorized proportions
  result <- merge(result, unauth_dist, by = c("age", "sex"), all.x = TRUE)

  result[is.na(unauth_pct), unauth_pct := 0]

  # Merge overstay percentages (by age only)
  result <- merge(result, overstay_pct[, .(age, overstay_pct)],
                  by = "age", all.x = TRUE)
  result[is.na(overstay_pct), overstay_pct := 0]

  # Calculate type proportions
  # I = Nonimmigrant proportion
  # V = Overstay proportion (subset of non-nonimmigrant)
  # N = Never-authorized (remainder)

  # Normalize to ensure they sum to 1
  result[, type_i := ni_pct]
  result[, type_v := (1 - ni_pct) * overstay_pct]
  result[, type_n := 1 - type_i - type_v]

  # Ensure non-negative
  result[type_n < 0, type_n := 0]

  # Renormalize
  result[, total := type_n + type_i + type_v]
  result[total > 0, `:=`(
    type_n = type_n / total,
    type_i = type_i / total,
    type_v = type_v / total
  )]

  result[, .(age, sex, type_n, type_i, type_v)]
}

#' Get default type splits
#'
#' @description
#' Returns simplified type splits when detailed data is not available.
#'
#' **HARDCODED VALUES**: These proportions are approximations based on
#' DHS population estimates. See source documentation below.
#'
#' @param config Optional list to override default values
#'
#' @section Sources:
#' - DHS (2022) "Estimates of the Unauthorized Immigrant Population": ~11M total
#' - DHS nonimmigrant stock estimates: ~2M
#' - Warren & Kerwin (2017) "The 2,000 Mile Wall in Search of a Purpose":
#'   Estimates ~40-45% of unauthorized are visa overstayers
#'
#' @keywords internal
get_default_type_splits <- function() {
  # Approximate type proportions from DHS data:
  # - Unauthorized: ~11M (includes never-authorized + overstayers)
  # - Nonimmigrants: ~2M
  # - Total O: ~13M
  #
  # Of unauthorized (~11M):
  # - Never-authorized: ~60%
  # - Visa-overstayers: ~40%

  ages <- 0:100
  sexes <- c("male", "female")

  result <- data.table::CJ(age = ages, sex = sexes)

  # Age-varying type proportions
  # Working-age adults have higher nonimmigrant share
  result[, type_i := data.table::fcase(
    age < 18, 0.05,      # Few child nonimmigrants
    age >= 18 & age < 25, 0.25,  # Students
    age >= 25 & age < 35, 0.20,  # Workers
    age >= 35 & age < 50, 0.15,
    age >= 50 & age < 65, 0.10,
    age >= 65, 0.05
  )]

  # Overstayer proportion (of non-nonimmigrant)
  overstay_pct <- get_overstay_percentages()
  result <- merge(result, overstay_pct[, .(age, overstay_pct)],
                  by = "age", all.x = TRUE)
  result[is.na(overstay_pct), overstay_pct := 0.40]

  # Calculate N and V
  result[, type_v := (1 - type_i) * overstay_pct]
  result[, type_n := 1 - type_i - type_v]

  result[, .(age, sex, type_n, type_i, type_v)]
}

#' Get overstay percentages by age
#'
#' @description
#' Returns age-specific overstay percentages. These can be provided via
#' configuration or default values are used.
#'
#' **HARDCODED DEFAULT VALUES**: The default percentages are approximations
#' based on published research since SSA's internal values are not public.
#'
#' @param config Optional list with custom overstay percentages by age group.
#'   If NULL, uses default values based on published research.
#'   Expected format: list with age_group and overstay_pct columns.
#'
#' @return data.table with overstay_pct by single year of age (0-100)
#'
#' @details
#' TR2025 Input #25 states: "Internally developed overstay percentages by age.
#' These data are based off a RAND Corporation document using data from the
#' 1980s, and are adjusted based on insights from the DHS."
#'
#' Since the SSA internal values are not published, defaults are derived from:
#'
#' **Sources for default values:**
#' 1. Warren & Kerwin (2017) "The 2,000 Mile Wall in Search of a Purpose"
#'    Center for Migration Studies. Estimates 42% of unauthorized population
#'    (as of 2014) are visa overstayers, varying by age.
#'
#' 2. DHS Entry/Exit Overstay Reports (2016-2022): Show overstay rates by

#'    visa category. Student (F-1) and exchange visitor (J-1) visas have
#'    higher overstay rates among young adults.
#'
#' 3. Passel & Cohn (2019) Pew Research Center: Age distribution analysis
#'    shows younger unauthorized immigrants more likely to have entered legally.
#'
#' **Age pattern rationale:**
#' - Children (0-17): Lower rates (accompany parents, inherit status)
#' - Young adults (18-30): Highest rates (students, tourists who overstay)
#' - Middle age (30-50): Medium rates (mixed entry modes)
#' - Older adults (50+): Lower rates (long-term EWI residents)
#'
#' @section Configuration:
#' To override defaults, pass a config list:
#' ```
#' config <- list(
#'   overstay_by_age_group = data.table(
#'     age_min = c(0, 18, 35, 50, 65),
#'     age_max = c(17, 34, 49, 64, 100),
#'     overstay_pct = c(0.25, 0.50, 0.40, 0.25, 0.15)
#'   )
#' )
#' overstay <- get_overstay_percentages(config)
#' ```
#'
#' @export
get_overstay_percentages <- function(config = NULL) {
  ages <- 0:100
  overstay <- data.table::data.table(age = ages)


  # Check for user-provided configuration
  if (!is.null(config) && !is.null(config$overstay_by_age_group)) {
    cli::cli_alert_info("Using user-provided overstay percentages")
    user_config <- config$overstay_by_age_group

    # Expand age groups to single years
    overstay[, overstay_pct := NA_real_]
    for (i in seq_len(nrow(user_config))) {
      overstay[age >= user_config$age_min[i] & age <= user_config$age_max[i],
               overstay_pct := user_config$overstay_pct[i]]
    }

    return(overstay)
  }

  # =========================================================================
  # HARDCODED DEFAULT VALUES
  # =========================================================================
  # These are approximations based on published research.
  # SSA's internal values (TR2025 Input #25) are not publicly available.
  #
  # Sources:
  # - Warren & Kerwin (2017): Overall ~42% overstay rate

  # - DHS Entry/Exit Overstay Reports: Higher rates for student visas
  # - Age pattern: Young adults peak, elderly lowest
  # =========================================================================

  cli::cli_alert_info("Using default overstay percentages (HARDCODED - see documentation)")

  overstay[, overstay_pct := data.table::fcase(
    # Children: Lower rates (usually accompany unauthorized parents)
    age < 5, 0.20,
    age >= 5 & age < 18, 0.30,

    # Young adults: Highest rates (students, tourists, temporary workers)
    # Source: DHS shows F-1/J-1 visas have elevated overstay rates
    age >= 18 & age < 25, 0.55,
    age >= 25 & age < 30, 0.50,

    # Middle age: Moderate rates (mixed entry modes)
    age >= 30 & age < 35, 0.45,
    age >= 35 & age < 40, 0.40,
    age >= 40 & age < 45, 0.35,
    age >= 45 & age < 50, 0.30,

    # Older adults: Lower rates (long-term EWI residents)
    # Source: Warren & Kerwin show older cohorts more likely EWI
    age >= 50 & age < 55, 0.25,
    age >= 55 & age < 60, 0.22,
    age >= 60 & age < 65, 0.20,
    age >= 65, 0.15
  )]

  overstay
}

#' Get overstay percentage sources
#'
#' @description
#' Returns documentation of sources for overstay percentage estimates.
#'
#' @return List with source citations and notes
#'
#' @export
get_overstay_percentage_sources <- function() {
  list(
    tr2025_reference = paste0(
      "TR2025 Input #25: 'Internally developed overstay percentages by age. ",
      "These data are based off a RAND Corporation document using data from ",
      "the 1980s, and are adjusted based on insights from the DHS.'"
    ),
    sources_used = list(
      warren_kerwin_2017 = list(
        citation = paste0(
          "Warren, R. & Kerwin, D. (2017). 'The 2,000 Mile Wall in Search of a ",
          "Purpose: Since 2007 Visa Overstays have Outnumbered Undocumented Border ",
          "Crossers by a Half Million.' Center for Migration Studies."
        ),
        url = "https://cmsny.org/publications/jmhs-visa-overstays-border-wall/",
        finding = "42% of unauthorized population (as of 2014) are visa overstayers"
      ),
      dhs_overstay_reports = list(
        citation = "DHS Entry/Exit Overstay Reports (2016-2022)",
        url = "https://www.dhs.gov/immigration-statistics/overstay",
        finding = paste0(
          "Student (F-1) and exchange visitor (J-1) visas show elevated overstay ",
          "rates, particularly among young adults ages 18-30"
        )
      ),
      passel_cohn_2019 = list(
        citation = paste0(
          "Passel, J.S. & Cohn, D. (2019). 'Mexicans decline to less than half ",
          "the U.S. unauthorized immigrant population for the first time.' ",
          "Pew Research Center."
        ),
        url = "https://www.pewresearch.org/fact-tank/2019/06/12/",
        finding = "Age distribution analysis of unauthorized population"
      )
    ),
    note = paste0(
      "The actual SSA values from the RAND Corporation document are not publicly ",
      "available. Default values are approximations based on the pattern described ",
      "in published research. Users can override these values via configuration."
    )
  )
}

#' Apply type splits to O immigration
#'
#' @description
#' Applies type splits (N/I/V) to average O immigration by age and sex.
#'
#' **HARDCODED FALLBACKS**: Contains hardcoded fallback values for missing type splits.
#'
#' @keywords internal
apply_type_splits <- function(avg_oi, type_splits) {
  # Merge type splits with O immigration
  merged <- merge(avg_oi, type_splits, by = c("age", "sex"), all.x = TRUE)

  # Fill missing type splits with defaults
  # HARDCODED FALLBACKS: Used when type splits are missing for some age-sex combinations
  # Based on overall TR2025 proportions: ~50% never-authorized, ~15% nonimmigrant, ~35% overstay
  merged[is.na(type_n), type_n := 0.50]  # HARDCODED fallback
  merged[is.na(type_i), type_i := 0.15]  # HARDCODED fallback
  merged[is.na(type_v), type_v := 0.35]  # HARDCODED fallback

  # Create long format with type dimension
  result <- data.table::rbindlist(list(
    merged[, .(age, sex, type = "N",
               avg_o_immigration = avg_o_immigration * type_n)],
    merged[, .(age, sex, type = "I",
               avg_o_immigration = avg_o_immigration * type_i)],
    merged[, .(age, sex, type = "V",
               avg_o_immigration = avg_o_immigration * type_v)]
  ))

  data.table::setorder(result, type, sex, age)

  result
}

# =============================================================================
# NONIMMIGRANT AND UNAUTHORIZED DISTRIBUTIONS
# =============================================================================

#' Get nonimmigrant stock distribution by age and sex
#'
#' @description
#' Returns the proportion of O population that are nonimmigrants
#' by age and sex, based on DHS nonimmigrant stock estimates.
#'
#' **HARDCODED VALUES**: Uses hardcoded total O population estimate (~13M) when
#' calculating proportions. This is based on DHS unauthorized + nonimmigrant estimates.
#'
#' @param config Optional list to override default total O population
#'
#' @section Sources:
#' - DHS (2022) "Estimates of the Unauthorized Immigrant Population": ~11.4M unauthorized
#' - DHS Nonimmigrant Stock estimates: ~2.1M
#' - Total O population: ~13M (unauthorized + nonimmigrant stock)
#'
#' @keywords internal
get_nonimmigrant_stock_distribution <- function(config = NULL) {
  # Load DHS nonimmigrant stock (from Phase 5A)
  ni_stock <- tryCatch({
    source(here::here("R/data_acquisition/dhs_nonimmigrant.R"), local = TRUE)
    fetch_dhs_nonimmigrant_stock()
  }, error = function(e) {
    cli::cli_alert_warning("Could not load nonimmigrant stock: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(ni_stock)) {
    # Return default distribution
    return(get_default_ni_distribution())
  }

  # Use 2016 estimate as reference
  ni_2016 <- ni_stock[reference_date == "2016-04-01"]

  # Calculate total by age group and sex
  total_ni <- sum(ni_2016$nonimmigrant_stock)

  # Convert to single-year ages using simple interpolation
  result <- interpolate_ni_to_single_age(ni_2016)

  # ===========================================================================
  # HARDCODED: Total O population estimate
  # ===========================================================================
  # Source: DHS unauthorized (~11M) + nonimmigrant stock (~2M) = ~13M
  # This can be overridden via config$total_o_population
  # ===========================================================================
  if (!is.null(config) && !is.null(config$total_o_population)) {
    total_o <- config$total_o_population
    cli::cli_alert_info("Using user-provided total O population: {format(total_o, big.mark = ',')}")
  } else {
    total_o <- 13000000  # HARDCODED: Approximate total O population
  }

  result[, ni_pct := count / total_o]

  # Cap at reasonable values
  result[, ni_pct := pmin(ni_pct, 0.50)]

  result[, .(age, sex, ni_pct)]
}

#' Interpolate nonimmigrant stock to single ages
#'
#' @keywords internal
interpolate_ni_to_single_age <- function(ni_data) {
  results <- list()

  for (sx in c("male", "female")) {
    sx_data <- ni_data[sex == sx]

    for (i in seq_len(nrow(sx_data))) {
      grp <- sx_data[i]

      # Map DHS age groups to standard
      age_min <- grp$age_min
      age_max <- min(grp$age_max, 100)

      # Distribute evenly
      n_ages <- age_max - age_min + 1
      count_per_age <- grp$nonimmigrant_stock / n_ages

      results[[paste(sx, i)]] <- data.table::data.table(
        age = age_min:age_max,
        sex = sx,
        count = count_per_age
      )
    }
  }

  data.table::rbindlist(results)
}

#' Get default nonimmigrant distribution
#'
#' @description
#' Returns fallback nonimmigrant distribution when DHS data unavailable.
#'
#' **HARDCODED VALUES**: These are approximations based on DHS aggregate reports.
#'
#' @section Sources:
#' - DHS Yearbook of Immigration Statistics (2019): Nonimmigrant Admissions
#' - Total nonimmigrant stock estimated at ~2.1M based on DHS reports
#' - Age pattern reflects visa categories: F-1 students, H-1B workers
#'
#' @keywords internal
get_default_ni_distribution <- function() {
  # =========================================================================
  # HARDCODED FALLBACK VALUES
  # =========================================================================
  # These are used when DHS nonimmigrant stock data cannot be loaded.
  # Source: DHS Yearbook of Immigration Statistics + professional judgment
  # Total ~2.1M nonimmigrants, concentrated in working ages
  # =========================================================================

  cli::cli_alert_warning("Using default NI distribution (HARDCODED fallback)")

  ages <- 0:100
  sexes <- c("male", "female")

  result <- data.table::CJ(age = ages, sex = sexes)

  # HARDCODED: Working-age concentration based on visa types
  result[, ni_pct := data.table::fcase(
    age < 5, 0.01,               # HARDCODED: Few child nonimmigrants
    age >= 5 & age < 18, 0.02,   # HARDCODED: Some F-2 dependents
    age >= 18 & age < 25, 0.25,  # HARDCODED: F-1 students peak
    age >= 25 & age < 35, 0.20,  # HARDCODED: H-1B workers peak
    age >= 35 & age < 50, 0.15,  # HARDCODED: L-1, H-1B workers
    age >= 50 & age < 65, 0.08,  # HARDCODED: Declining work visas
    age >= 65, 0.02              # HARDCODED: Few elderly nonimmigrants
  )]

  result
}

#' Get unauthorized population distribution
#'
#' @description
#' Returns unauthorized population distribution by age and sex.
#'
#' **HARDCODED VALUES**: Contains fallback values and population ratio estimates.
#'
#' @section Sources:
#' - DHS (2022) "Estimates of the Unauthorized Immigrant Population"
#' - Total unauthorized: ~11M; Total O population: ~13M (ratio = 0.846)
#'
#' @keywords internal
get_unauthorized_distribution <- function() {
  # Load DHS unauthorized age distribution (from Phase 5B)
  unauth_dist <- tryCatch({
    source(here::here("R/data_acquisition/acs_foreign_born.R"), local = TRUE)
    get_dhs_unauthorized_age_distribution()
  }, error = function(e) {
    cli::cli_alert_warning("Could not load unauthorized distribution")
    return(NULL)
  })

  if (is.null(unauth_dist)) {
    # =========================================================================
    # HARDCODED FALLBACK VALUES
    # =========================================================================
    # Used when DHS unauthorized data cannot be loaded.
    # 85% unauthorized is a rough approximation (11M of 13M total O)
    # =========================================================================
    cli::cli_alert_warning("Using default unauthorized distribution (HARDCODED fallback)")
    ages <- 0:100
    sexes <- c("male", "female")
    result <- data.table::CJ(age = ages, sex = sexes)
    result[, unauth_pct := 0.85]  # HARDCODED: Default to 85% unauthorized
    return(result)
  }

  # Expand age groups to single ages
  ages <- 0:100
  sexes <- c("male", "female")
  result <- data.table::CJ(age = ages, sex = sexes)

  # Map age groups
  result[, age_group := data.table::fcase(
    age < 18, "0-17",
    age >= 18 & age < 35, "18-34",
    age >= 35 & age < 50, "35-49",
    age >= 50 & age < 65, "50-64",
    age >= 65, "65+"
  )]

  # Merge
  result <- merge(result, unauth_dist[, .(age_group, unauthorized_pct)],
                  by = "age_group", all.x = TRUE)
  result[is.na(unauthorized_pct), unauthorized_pct := 0.25]  # HARDCODED fallback

  # ===========================================================================
  # HARDCODED: Population ratio conversion
  # ===========================================================================
  # unauthorized_pct here is share of total unauthorized, not share of O
  # Convert to approximate share of O population
  # Source: DHS unauthorized ~11M, nonimmigrant ~2M, Total O ~13M
  # ===========================================================================
  unauth_total <- 11000000  # HARDCODED: DHS unauthorized estimate
  total_o <- 13000000       # HARDCODED: Total O population estimate
  result[, unauth_pct := unauthorized_pct * (unauth_total / total_o)]

  result[, .(age, sex, unauth_pct)]
}

# =============================================================================
# PROJECTION FUNCTIONS
# =============================================================================

#' Project O immigration using ODIST
#'
#' @description
#' Projects O immigration for a given year using the ODIST distribution.
#' Per TR2025 Equation 1.5.1: OI^z_{x,s,t} = TO^z × ODIST_{x,s,t}
#'
#' @param total_o Numeric: Total O immigration for the year (TO^z)
#' @param odist data.table: ODIST distribution from calculate_odist()
#'
#' @return data.table with projected O immigration by age, sex, type
#'
#' @export
project_o_immigration <- function(total_o, odist) {
  checkmate::assert_number(total_o, lower = 0)
  checkmate::assert_data_table(odist)

  result <- data.table::copy(odist)

  # Apply Equation 1.5.1: OI = TO × ODIST
  result[, o_immigration := total_o * odist]

  result[, .(age, sex, type, o_immigration)]
}

#' Get TR2025 O immigration assumptions
#'
#' @description
#' Returns the Trustees' assumed total O immigration by year.
#'
#' **HARDCODED VALUES**: These are directly from TR2025 documentation.
#'
#' @param years Integer vector of years
#' @param config Optional list to override default values. If NULL, uses TR2025 values.
#'
#' @return data.table with total_o by year
#'
#' @section TR2025 Source:
#' Per TR2025 Section 1.5: "The ultimate annual level is 1,350,000 for each year
#' beginning in 2026. The level is estimated to be 2,200,000, 2,700,000,
#' 2,600,000, and 2,000,000 for years 2022-25, respectively."
#'
#' @section Configuration:
#' To override defaults, pass a config list:
#' ```
#' config <- list(
#'   o_immigration_by_year = data.table(
#'     year = 2022:2030,
#'     total_o = c(2200000, 2700000, 2600000, 2000000, rep(1350000, 5))
#'   )
#' )
#' assumptions <- get_tr2025_o_immigration_assumptions(2022:2030, config)
#' ```
#'
#' @export
get_tr2025_o_immigration_assumptions <- function(years = 2022:2099, config = NULL) {
  # Check for user-provided configuration
  if (!is.null(config) && !is.null(config$o_immigration_by_year)) {
    cli::cli_alert_info("Using user-provided O immigration assumptions")
    user_config <- config$o_immigration_by_year
    result <- user_config[year %in% years]
    return(result)
  }

  # =========================================================================
  # HARDCODED VALUES FROM TR2025
  # =========================================================================
  # Source: TR2025 Section 1.5 - Temporary or Unlawfully Present Immigration
  # These values represent Trustees' assumptions for total O immigration.
  # =========================================================================

  cli::cli_alert_info("Using TR2025 O immigration assumptions (HARDCODED)")

  result <- data.table::data.table(year = years)

  result[, total_o := data.table::fcase(
    year == 2022, 2200000,   # HARDCODED: TR2025 estimate
    year == 2023, 2700000,   # HARDCODED: TR2025 estimate
    year == 2024, 2600000,   # HARDCODED: TR2025 estimate
    year == 2025, 2000000,   # HARDCODED: TR2025 estimate
    year >= 2026, 1350000    # HARDCODED: TR2025 ultimate level
  )]

  result
}

#' Run full O immigration projection
#'
#' @description
#' Runs the complete O immigration projection following TR2025 methodology.
#'
#' @param acs_new_arrivals ACS foreign-born new arrivals data
#' @param lpr_new_arrivals LPR NEW arrivals data
#' @param undercount_factors Undercount factors by age group
#' @param projection_years Years to project (default: 2023:2099)
#' @param reference_years Years for ODIST calculation (default: 2015:2019)
#'
#' @return List with:
#'   - odist: ODIST distribution
#'   - projections: Projected O immigration by year, age, sex, type
#'
#' @export
run_o_immigration_projection <- function(acs_new_arrivals,
                                          lpr_new_arrivals,
                                          undercount_factors,
                                          projection_years = 2023:2099,
                                          reference_years = 2015:2019) {
  cli::cli_h2("Running O Immigration Projection")

  # Step 1: Calculate historical O immigration
  cli::cli_alert("Step 1: Calculating O immigration ({min(reference_years)}-{max(reference_years)})...")
  o_imm <- calculate_o_immigration(
    acs_new_arrivals,
    lpr_new_arrivals,
    undercount_factors,
    years = reference_years
  )

  # Step 2: Calculate type splits
  cli::cli_alert("Step 2: Calculating type splits...")
  type_splits <- get_default_type_splits()

  # Step 3: Calculate ODIST
  cli::cli_alert("Step 3: Calculating ODIST...")
  odist <- calculate_odist(o_imm, type_splits, reference_years)

  # Step 4: Get TR assumptions
  cli::cli_alert("Step 4: Getting TR2025 assumptions...")
  assumptions <- get_tr2025_o_immigration_assumptions(projection_years)

  # Step 5: Project O immigration
  cli::cli_alert("Step 5: Projecting O immigration...")
  projections <- list()

  for (yr in projection_years) {
    total_o <- assumptions[year == yr, total_o]
    proj_yr <- project_o_immigration(total_o, odist)
    proj_yr[, year := yr]
    projections[[as.character(yr)]] <- proj_yr
  }

  projections_dt <- data.table::rbindlist(projections)

  cli::cli_alert_success(
    "Projected O immigration for {length(projection_years)} years"
  )

  list(
    odist = odist,
    projections = projections_dt,
    assumptions = assumptions
  )
}

# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

#' Validate ODIST against expectations
#'
#' @description
#' Validates the calculated ODIST against expected properties.
#'
#' @param odist data.table from calculate_odist()
#'
#' @return Logical: TRUE if validation passes
#'
#' @export
validate_odist <- function(odist) {
  checks_passed <- TRUE

  # Check 1: ODIST sums to 1
  total <- sum(odist$odist)
  if (abs(total - 1) > 0.001) {
    cli::cli_alert_danger("ODIST sum: {round(total, 4)} (expected: 1.0)")
    checks_passed <- FALSE
  } else {
    cli::cli_alert_success("ODIST sums to 1.0")
  }

  # Check 2: All values non-negative
  if (any(odist$odist < 0)) {
    cli::cli_alert_danger("Negative ODIST values found")
    checks_passed <- FALSE
  } else {
    cli::cli_alert_success("All ODIST values non-negative")
  }

  # Check 3: Type proportions reasonable
  type_totals <- odist[, .(total = sum(odist)), by = type]
  cli::cli_alert_info("Type proportions: N={round(type_totals[type=='N', total]*100, 1)}%, I={round(type_totals[type=='I', total]*100, 1)}%, V={round(type_totals[type=='V', total]*100, 1)}%")

  # Check 4: Sex proportions reasonable (should be roughly balanced)
  sex_totals <- odist[, .(total = sum(odist)), by = sex]
  cli::cli_alert_info("Sex proportions: Female={round(sex_totals[sex=='female', total]*100, 1)}%, Male={round(sex_totals[sex=='male', total]*100, 1)}%")

  checks_passed
}

# =============================================================================
# TARGET HELPER FUNCTIONS
# =============================================================================

#' Calculate Net O immigration for population projection (target helper)
#'
#' @description
#' Helper function for the net_o_for_projection target. Takes V.A2 net O totals
#' and applies the TR-derived age-sex distribution to get net O by age and sex.
#'
#' @param va2_net_immigration data.table with V.A2 net immigration values
#' @param tr_derived_dist data.table: TR-derived immigration distribution from
#'   calculate_tr_derived_distribution()
#' @param calibration_add Numeric: additional immigration to add (default: 0)
#'
#' @return data.table with columns: year, age, sex, net_o
#'
#' @export
calculate_net_o_for_projection <- function(va2_net_immigration,
                                            tr_derived_dist,
                                            calibration_add = 0) {
  # Get V.A2 totals
  va2_totals <- va2_net_immigration[, .(year, o_net)]

  # Add calibration
  va2_totals[, net_o_total := o_net + calibration_add]

  # Get the TR-derived distribution for applying to totals
  # IMPORTANT: Age 100 has inflated values due to 100+ open-ended group artifact
  # in the back-calculation. TR2025 assumes zero immigration at 100+, so we
  # exclude age 100 and renormalize the distribution for ages 0-99.
  dist <- tr_derived_dist[, .(age, sex, implied_dist)]

  # Exclude age 100 and renormalize across BOTH sexes
  dist_0_99 <- dist[age < 100]
  total_dist <- sum(dist_0_99$implied_dist)
  dist_0_99[, implied_dist := implied_dist / total_dist]

  # Add age 100 with 0 distribution
  dist_100 <- data.table::data.table(
    age = 100L,
    sex = c("male", "female"),
    implied_dist = 0
  )
  dist_clean <- rbind(dist_0_99, dist_100)

  # Apply distribution to V.A2 totals
  result_list <- lapply(unique(va2_totals$year), function(yr) {
    yr_total <- va2_totals[year == yr, net_o_total]
    if (length(yr_total) == 0 || is.na(yr_total)) return(NULL)

    yr_dist <- data.table::copy(dist_clean)
    yr_dist[, year := yr]
    yr_dist[, net_o := yr_total * implied_dist]
    yr_dist[, .(year, age, sex, net_o)]
  })

  result <- data.table::rbindlist(result_list)
  data.table::setorder(result, year, sex, age)

  # Log sample values
  if (2099 %in% result$year) {
    total <- result[year == 2099, sum(net_o)]
    base <- va2_totals[year == 2099, o_net]
    cli::cli_alert_info(
      "2099 Net O: {format(round(total), big.mark=',')} (Base {format(base, big.mark=',')} + {format(calibration_add, big.mark=',')})"
    )
  }

  result
}

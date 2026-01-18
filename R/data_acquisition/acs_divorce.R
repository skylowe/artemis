#' ACS Divorce Data Acquisition
#'
#' @description
#' Functions for fetching American Community Survey (ACS) data on recent divorces.
#' Starting in 2008, ACS includes MARHD (divorced in past 12 months) which enables
#' construction of divorce age distributions.
#'
#' This data is used in the DIVORCE subprocess (Section 1.7) for:
#' - Adjusting the base DivGrid (1979-1988) with more recent patterns
#' - Alternative to 18-state data that TR2025 uses (2009-2012)
#'
#' Per TR2025 methodology, the adjustment uses ratio-based scaling:
#' "This state data single year of age grid is derived by ratioing the 1988
#' DivGrid cells using the original state age-group data."
#'
#' @name acs_divorce
NULL

# =============================================================================
# CONSTANTS AND CONFIGURATION
# =============================================================================

#' Age groups for ACS divorce data (matching TR2025)
#' @keywords internal
ACS_DIVORCE_AGE_GROUPS <- list(
  group_15_19 = 15:19,
  group_20_24 = 20:24,
  group_25_29 = 25:29,
  group_30_34 = 30:34,
  group_35_44 = 35:44,
  group_45_54 = 45:54,
  group_55_64 = 55:64,
  group_65_plus = 65:100
)

# =============================================================================
# MAIN FETCHING FUNCTIONS
# =============================================================================

#' Fetch ACS divorces by age and sex
#'
#' @description
#' Downloads ACS PUMS data for persons divorced in the last 12 months (MARHD=1).
#' Returns age distributions for divorced males and females separately.
#'
#' Since ACS doesn't allow linking ex-spouses (they're typically in different
#' households post-divorce), we obtain marginal distributions by age for each sex.
#' These are used to adjust the base DivGrid using ratio-based scaling.
#'
#' @param years Integer vector of ACS years (2008-2023; 2020 skipped)
#' @param min_age Minimum age to include (default: 15)
#' @param max_age Maximum age to include (default: 99, with 100+ grouped)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return list with:
#'   - by_year: List of data.tables by year with divorces by age/sex
#'   - summary: data.table with summary statistics by year
#'   - male_marginal: Average male age distribution (normalized)
#'   - female_marginal: Average female age distribution (normalized)
#'   - combined: Combined data.table for all years
#'
#' @export
fetch_acs_divorces <- function(years = 2008:2022,
                                min_age = 15,
                                max_age = 99,
                                cache_dir = here::here("data/cache/acs_divorce")) {

  # Validate inputs
  checkmate::assert_integerish(years, lower = 2008, upper = 2024, min.len = 1)
  checkmate::assert_int(min_age, lower = 14, upper = 99)
  checkmate::assert_int(max_age, lower = min_age, upper = 100)

  # Create cache directory
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Get API key
  api_key <- get_api_key("CENSUS_KEY")

  by_year <- list()
  summaries <- list()

  cli::cli_h2("Fetching ACS Divorce Data")

  for (yr in years) {
    # Skip 2020 - ACS 1-year not released due to COVID
    if (yr == 2020) {
      cli::cli_alert_warning("Skipping 2020 - ACS 1-year not released due to COVID")
      next
    }

    cache_file <- file.path(cache_dir, sprintf("divorces_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached divorces for {yr}")
      cached <- readRDS(cache_file)
      by_year[[as.character(yr)]] <- cached$data
      summaries[[as.character(yr)]] <- cached$summary
      next
    }

    cli::cli_alert("Fetching ACS divorces for {yr}...")

    tryCatch({
      result <- fetch_acs_divorces_year(yr, min_age, max_age, api_key)

      if (!is.null(result$data) && nrow(result$data) > 0) {
        # Cache the result
        saveRDS(result, cache_file)
        total_div <- sum(result$data$divorces, na.rm = TRUE)
        cli::cli_alert_success("Cached divorces for {yr} (total: {format(round(total_div), big.mark=',')})")
        by_year[[as.character(yr)]] <- result$data
        summaries[[as.character(yr)]] <- result$summary
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(by_year) == 0) {
    cli::cli_abort("No ACS divorce data retrieved")
  }

  # Combine summaries
  summary_dt <- data.table::rbindlist(summaries, use.names = TRUE)
  data.table::setorder(summary_dt, year)

  # Combine all year data
  combined <- data.table::rbindlist(by_year, use.names = TRUE)

  # Calculate average marginal distributions (normalized to sum to 1)
  male_marginal <- calculate_marginal_distribution(combined, "male", min_age, max_age)
  female_marginal <- calculate_marginal_distribution(combined, "female", min_age, max_age)

  cli::cli_alert_success(
    "Retrieved divorce data for {length(by_year)} years"
  )
  cli::cli_alert_info(
    "Total divorces: {format(round(sum(combined$divorces)), big.mark=',')}"
  )

  list(
    by_year = by_year,
    summary = summary_dt,
    male_marginal = male_marginal,
    female_marginal = female_marginal,
    combined = combined,
    years = as.integer(names(by_year)),
    min_age = min_age,
    max_age = max_age
  )
}


#' Fetch ACS divorces for a single year
#'
#' @description
#' Downloads ACS PUMS data for persons who were divorced in the past 12 months.
#'
#' @param year Integer: year to fetch
#' @param min_age Integer: minimum age
#' @param max_age Integer: maximum age
#' @param api_key Character: Census API key
#'
#' @return list with data and summary statistics
#'
#' @keywords internal
fetch_acs_divorces_year <- function(year, min_age, max_age, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  # Variables needed:
  # - AGEP: Age
  # - SEX: Sex (1=Male, 2=Female)
  # - MARHD: Divorced in past 12 months (1=Yes, 2=No)
  # - PWGTP: Person weight

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = "AGEP,SEX,MARHD,PWGTP",
      # Filter to those divorced in past 12 months
      MARHD = "1",
      key = api_key
    ) |>
    httr2::req_timeout(300) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    cli::cli_alert_warning("No data returned for {year}")
    return(list(data = NULL, summary = NULL))
  }

  # Convert to data.table
  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  # Parse variables
  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, weight := as.numeric(PWGTP)]

  # Map sex codes
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Filter to valid ages and cap at max_age
  dt <- dt[age >= min_age & !is.na(sex)]
  dt[age > max_age, age := max_age]

  if (nrow(dt) == 0) {
    cli::cli_alert_warning("No valid records for {year}")
    return(list(data = NULL, summary = NULL))
  }

  # Aggregate by age and sex
  result <- dt[, .(divorces = sum(weight)), by = .(age, sex)]
  result[, year := year]
  data.table::setcolorder(result, c("year", "age", "sex", "divorces"))
  data.table::setorder(result, age, sex)

  # Calculate summary statistics
  summary_stats <- calculate_divorce_summary(result, year)

  list(data = result, summary = summary_stats)
}


#' Calculate marginal distribution for one sex
#'
#' @description
#' Calculates the normalized age distribution for divorces by sex.
#' Used for ratio-based DivGrid adjustment.
#'
#' @param combined data.table with all divorce data
#' @param sex_filter Character: "male" or "female"
#' @param min_age Integer: minimum age
#' @param max_age Integer: maximum age
#'
#' @return Named numeric vector with normalized distribution by age
#'
#' @keywords internal
calculate_marginal_distribution <- function(combined, sex_filter, min_age, max_age) {
  # Sum across all years for this sex
  marginal <- combined[sex == sex_filter, .(divorces = sum(divorces)), by = age]

  # Ensure all ages are present
  ages <- min_age:max_age
  full_marginal <- data.table::data.table(age = ages)
  full_marginal <- merge(full_marginal, marginal, by = "age", all.x = TRUE)
  full_marginal[is.na(divorces), divorces := 0]

  # Normalize to sum to 1
  total <- sum(full_marginal$divorces)
  full_marginal[, proportion := divorces / total]

  # Return as named vector
  props <- full_marginal$proportion
  names(props) <- full_marginal$age

  props
}


#' Calculate divorce summary statistics
#'
#' @param data data.table with divorce data for one year
#' @param year Integer: survey year
#'
#' @return data.table with summary statistics
#'
#' @keywords internal
calculate_divorce_summary <- function(data, year) {
  total <- sum(data$divorces)

  if (total == 0) {
    return(data.table::data.table(
      year = year,
      total_divorces = 0,
      total_male = NA_real_,
      total_female = NA_real_,
      avg_male_age = NA_real_,
      avg_female_age = NA_real_
    ))
  }

  # By sex totals
  male_data <- data[sex == "male"]
  female_data <- data[sex == "female"]

  total_male <- sum(male_data$divorces)
  total_female <- sum(female_data$divorces)

  # Average ages (weighted)
  avg_male_age <- if (total_male > 0) {
    sum(male_data$age * male_data$divorces) / total_male
  } else NA_real_

  avg_female_age <- if (total_female > 0) {
    sum(female_data$age * female_data$divorces) / total_female
  } else NA_real_

  data.table::data.table(
    year = year,
    total_divorces = round(total),
    total_male = round(total_male),
    total_female = round(total_female),
    avg_male_age = round(avg_male_age, 1),
    avg_female_age = round(avg_female_age, 1)
  )
}


# =============================================================================
# AGE GROUP AGGREGATION
# =============================================================================

#' Calculate divorces by age group
#'
#' @description
#' Aggregates single-year divorce data into age groups for comparison
#' and for ratio-based DivGrid adjustment.
#'
#' @param acs_divorce_data Result from fetch_acs_divorces()
#' @param age_groups List of age group vectors (default: ACS_DIVORCE_AGE_GROUPS)
#'
#' @return data.table with divorces by age group and sex
#'
#' @export
calculate_divorces_by_age_group <- function(acs_divorce_data,
                                             age_groups = ACS_DIVORCE_AGE_GROUPS) {
  combined <- acs_divorce_data$combined

  results <- list()

  for (grp_name in names(age_groups)) {
    ages_in_group <- age_groups[[grp_name]]

    for (sx in c("male", "female")) {
      grp_data <- combined[sex == sx & age %in% ages_in_group,
                           .(divorces = sum(divorces)),
                           by = year]

      if (nrow(grp_data) > 0) {
        grp_data[, sex := sx]
        grp_data[, age_group := gsub("group_", "", grp_name)]
        results[[length(results) + 1]] <- grp_data
      }
    }
  }

  if (length(results) == 0) {
    return(data.table::data.table())
  }

  result <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setcolorder(result, c("year", "sex", "age_group", "divorces"))
  data.table::setorder(result, year, sex, age_group)

  result
}


#' Calculate average age group distribution
#'
#' @description
#' Calculates the average age group distribution across multiple years.
#' Used as the reference pattern for DivGrid adjustment.
#'
#' @param acs_divorce_data Result from fetch_acs_divorces()
#' @param years_to_use Integer vector of years to include (default: all)
#' @param age_groups List of age group vectors
#'
#' @return data.table with sex, age_group, and proportion columns
#'
#' @export
calculate_average_age_group_distribution <- function(acs_divorce_data,
                                                      years_to_use = NULL,
                                                      age_groups = ACS_DIVORCE_AGE_GROUPS) {

  by_age_group <- calculate_divorces_by_age_group(acs_divorce_data, age_groups)

  if (!is.null(years_to_use)) {
    by_age_group <- by_age_group[year %in% years_to_use]
  }

  # Sum across years
  totals <- by_age_group[, .(divorces = sum(divorces)), by = .(sex, age_group)]

  # Calculate proportions within each sex
  totals[, total_sex := sum(divorces), by = sex]
  totals[, proportion := divorces / total_sex]

  totals[, .(sex, age_group, divorces, proportion)]
}


# =============================================================================
# PUERTO RICO DIVORCE DATA (Item 11 from TR2025)
# =============================================================================

#' Fetch ACS divorces for Puerto Rico
#'
#' @description
#' Downloads ACS PUMS data for divorces in Puerto Rico.
#' Per TR2025 Input #11: "The number of divorces for years 2008-2022 in Puerto Rico,
#' estimated using the 2008-2022 (excluding 2020) American Community Survey (ACS)
#' public use microdata sample (PUMS) files."
#'
#' @param years Integer vector of ACS years (2008-2022, excluding 2020)
#' @param cache_dir Character: directory for caching
#'
#' @return data.table with columns: year, total_divorces
#'
#' @export
fetch_acs_pr_divorces <- function(years = setdiff(2008:2022, 2020),
                                   cache_dir = here::here("data/cache/acs_divorce")) {

  checkmate::assert_integerish(years, lower = 2008, upper = 2024, min.len = 1)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  api_key <- get_api_key("CENSUS_KEY")

  results <- list()

  cli::cli_h2("Fetching ACS Puerto Rico Divorce Data")

  for (yr in years) {
    if (yr == 2020) {
      next
    }

    cache_file <- file.path(cache_dir, sprintf("pr_divorces_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached PR divorces for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS PR divorces for {yr}...")

    tryCatch({
      total <- fetch_acs_pr_divorces_year(yr, api_key)

      if (!is.null(total)) {
        result <- data.table::data.table(year = yr, total_divorces = total)
        saveRDS(result, cache_file)
        cli::cli_alert_success("Cached PR divorces for {yr}: {format(round(total), big.mark=',')}")
        results[[as.character(yr)]] <- result
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_warn("No Puerto Rico divorce data retrieved")
    return(data.table::data.table(year = integer(), total_divorces = numeric()))
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year)

  cli::cli_alert_success("Retrieved PR divorce data for {nrow(combined)} years")

  combined
}


#' Fetch ACS PR divorces for a single year
#'
#' @keywords internal
fetch_acs_pr_divorces_year <- function(year, api_key) {
  # Puerto Rico uses the PRCS (Puerto Rico Community Survey) via ACS
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = "PWGTP",
      MARHD = "1",
      ST = "72",  # Puerto Rico FIPS code
      key = api_key
    ) |>
    httr2::req_timeout(120) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- tryCatch({
    api_request_with_retry(req, max_retries = 3)
  }, error = function(e) {
    # Try alternative approach - fetch all and filter
    return(NULL)
  })

  if (is.null(resp)) {
    # Alternative: fetch without ST filter and filter locally
    # This is slower but works if the direct query fails
    req2 <- httr2::request(base_url) |>
      httr2::req_url_query(
        get = "ST,PWGTP",
        MARHD = "1",
        key = api_key
      ) |>
      httr2::req_timeout(300) |>
      httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

    resp <- api_request_with_retry(req2, max_retries = 3)
    check_api_response(resp, sprintf("ACS PUMS API PR (%d)", year))

    json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    if (length(json_data) < 2) {
      return(NULL)
    }

    headers <- json_data[1, ]
    data_rows <- json_data[-1, , drop = FALSE]
    dt <- data.table::as.data.table(data_rows)
    data.table::setnames(dt, headers)

    # Filter to Puerto Rico
    dt <- dt[ST == "72"]
    dt[, weight := as.numeric(PWGTP)]

    return(sum(dt$weight, na.rm = TRUE))
  }

  check_api_response(resp, sprintf("ACS PUMS API PR (%d)", year))

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    return(NULL)
  }

  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  dt[, weight := as.numeric(PWGTP)]

  sum(dt$weight, na.rm = TRUE)
}


# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

#' Validate ACS divorce data
#'
#' @description
#' Performs validation checks on fetched ACS divorce data.
#'
#' @param acs_divorce_data Result from fetch_acs_divorces()
#'
#' @return list with validation results
#'
#' @export
validate_acs_divorce_data <- function(acs_divorce_data) {
  results <- list(
    checks = list(),
    passed = 0,
    failed = 0
  )

  cli::cli_h2("ACS Divorce Data Validation")

  # Check 1: Data available for multiple years
  n_years <- length(acs_divorce_data$years)
  check1_passed <- n_years >= 5
  results$checks$years_available <- list(
    passed = check1_passed,
    message = sprintf("Years available: %d (need >= 5)", n_years)
  )
  if (check1_passed) {
    cli::cli_alert_success(results$checks$years_available$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$years_available$message)
    results$failed <- results$failed + 1
  }

  # Check 2: Total divorces reasonable (expect 700K-1.5M per year)
  summary <- acs_divorce_data$summary
  avg_divorces <- mean(summary$total_divorces, na.rm = TRUE)
  check2_passed <- avg_divorces >= 500000 && avg_divorces <= 2000000
  results$checks$total_reasonable <- list(
    passed = check2_passed,
    message = sprintf("Average annual divorces: %s (expect 500K-2M)",
                      format(round(avg_divorces), big.mark = ","))
  )
  if (check2_passed) {
    cli::cli_alert_success(results$checks$total_reasonable$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$total_reasonable$message)
    results$failed <- results$failed + 1
  }

  # Check 3: Male/female ratio reasonable (expect 45-55% each)
  male_pct <- mean(summary$total_male / summary$total_divorces, na.rm = TRUE) * 100
  check3_passed <- male_pct >= 45 && male_pct <= 55
  results$checks$sex_balance <- list(
    passed = check3_passed,
    message = sprintf("Male percentage: %.1f%% (expect 45-55%%)", male_pct)
  )
  if (check3_passed) {
    cli::cli_alert_success(results$checks$sex_balance$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$sex_balance$message)
    results$failed <- results$failed + 1
  }

  # Check 4: Average ages reasonable (expect 35-50)
  avg_male_age <- mean(summary$avg_male_age, na.rm = TRUE)
  avg_female_age <- mean(summary$avg_female_age, na.rm = TRUE)
  check4_passed <- avg_male_age >= 35 && avg_male_age <= 55 &&
    avg_female_age >= 35 && avg_female_age <= 55
  results$checks$age_reasonable <- list(
    passed = check4_passed,
    message = sprintf("Average ages - Male: %.1f, Female: %.1f (expect 35-55)",
                      avg_male_age, avg_female_age)
  )
  if (check4_passed) {
    cli::cli_alert_success(results$checks$age_reasonable$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$age_reasonable$message)
    results$failed <- results$failed + 1
  }

  # Check 5: Marginal distributions sum to 1
  male_sum <- sum(acs_divorce_data$male_marginal)
  female_sum <- sum(acs_divorce_data$female_marginal)
  check5_passed <- abs(male_sum - 1) < 0.001 && abs(female_sum - 1) < 0.001
  results$checks$distributions_normalized <- list(
    passed = check5_passed,
    message = sprintf("Marginal distributions sum - Male: %.4f, Female: %.4f",
                      male_sum, female_sum)
  )
  if (check5_passed) {
    cli::cli_alert_success(results$checks$distributions_normalized$message)
    results$passed <- results$passed + 1
  } else {
    cli::cli_alert_warning(results$checks$distributions_normalized$message)
    results$failed <- results$failed + 1
  }

  cli::cli_alert_info("Passed: {results$passed}/{results$passed + results$failed}")

  invisible(results)
}


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

#' Fetch or load cached ACS divorce data
#'
#' @description
#' Main entry point for ACS divorce data. Loads from cache if available,
#' otherwise fetches from API.
#'
#' @param years Integer vector of years
#' @param cache_dir Character: cache directory
#' @param force Logical: force refresh from API (default: FALSE)
#'
#' @return Result from fetch_acs_divorces()
#'
#' @export
get_acs_divorce_data <- function(years = 2008:2022,
                                  cache_dir = here::here("data/cache/acs_divorce"),
                                  force = FALSE) {

  combined_cache <- file.path(cache_dir, "acs_divorce_combined.rds")

  if (file.exists(combined_cache) && !force) {
    cli::cli_alert_success("Loading cached ACS divorce data")
    cached <- readRDS(combined_cache)

    # Check if all requested years are present
    if (all(setdiff(years, 2020) %in% cached$years)) {
      return(cached)
    }
    cli::cli_alert_info("Cached data missing some years, fetching...")
  }

  # Fetch data
  result <- fetch_acs_divorces(years, cache_dir = cache_dir)

  # Save combined result
  saveRDS(result, combined_cache)
  cli::cli_alert_success("Cached combined ACS divorce data")

  result
}

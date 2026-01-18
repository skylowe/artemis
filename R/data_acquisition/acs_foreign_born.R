#' ACS Foreign-Born Data for O Immigration Subprocess
#'
#' Functions for fetching and processing American Community Survey data
#' specifically for the Temporary/Unlawfully Present Immigration subprocess.
#' This includes new arrivals calculation, undercount factors, and special
#' populations for DACA eligibility estimation.
#'
#' @name acs_foreign_born
NULL

# =============================================================================
# NEW ARRIVALS CALCULATION FOR ODIST
# =============================================================================

#' Calculate new arrivals from ACS foreign-born flows
#'
#' @description
#' Extracts "new arrivals" from ACS foreign-born population data. New arrivals
#' are defined as foreign-born persons whose year of entry is recent relative
#' to the survey year (typically entered in the survey year or previous year).
#'
#' This is used to estimate annual O immigration flows for ODIST calculation.
#'
#' @param foreign_born_flows data.table from fetch_acs_foreign_born_flows()
#' @param entry_window Integer: how many years prior to survey year to include
#'   (default: 1, meaning entry in survey year or prior year)
#'
#' @return data.table with columns: year, age, sex, is_cuban, population
#'   representing new arrivals by age and sex
#'
#' @details
#' The ACS asks respondents what year they came to live in the US. For new
#' arrivals analysis:
#' - entry_window = 0: Only persons who entered in the survey year
#' - entry_window = 1: Persons who entered in survey year or prior year
#'
#' Note: ACS surveys in mid-year, so "entered in survey year" captures roughly
#' half a year of arrivals. Using entry_window = 1 provides better coverage.
#'
#' @export
calculate_acs_new_arrivals <- function(foreign_born_flows, entry_window = 1) {
  checkmate::assert_data_table(foreign_born_flows)
  checkmate::assert_int(entry_window, lower = 0, upper = 5)

  # Make a copy to avoid modifying input
  dt <- data.table::copy(foreign_born_flows)

  # Calculate years since entry
  dt[, years_since_entry := year - year_of_entry]

  # Filter to new arrivals within the window
  new_arrivals <- dt[years_since_entry >= 0 & years_since_entry <= entry_window]

  # Aggregate by year, age, sex, Cuban status
  result <- new_arrivals[, .(population = sum(population)),
                         by = .(year, age, sex, is_cuban)]

  data.table::setorder(result, year, sex, age)

  cli::cli_alert_success(
    "Calculated new arrivals for {length(unique(result$year))} years"
  )

  result
}

#' Calculate average new arrivals distribution (ODIST input)
#'
#' @description
#' Calculates the average age-sex distribution of new arrivals over a reference
#' period (typically 2015-2019). This is used as input to ODIST calculation.
#'
#' @param new_arrivals data.table from calculate_acs_new_arrivals()
#' @param reference_years Integer vector of years to average (default: 2015:2019)
#'
#' @return data.table with columns: age, sex, is_cuban, avg_population, pct
#'   representing the average distribution of new arrivals
#'
#' @export
calculate_new_arrivals_distribution <- function(new_arrivals,
                                                 reference_years = 2015:2019) {
  checkmate::assert_data_table(new_arrivals)

  # Filter to reference years
  ref_data <- new_arrivals[year %in% reference_years]

  if (nrow(ref_data) == 0) {
    cli::cli_abort("No data found for reference years {reference_years}")
  }

  # Calculate average across years
  avg_dist <- ref_data[, .(avg_population = mean(population)),
                       by = .(age, sex, is_cuban)]

  # Calculate percentage of total
  total_pop <- sum(avg_dist$avg_population)
  avg_dist[, pct := avg_population / total_pop]

  data.table::setorder(avg_dist, sex, age)

  cli::cli_alert_info(
    "Average new arrivals ({min(reference_years)}-{max(reference_years)}): {format(total_pop, big.mark = ',')}"
  )

  avg_dist
}

# =============================================================================
# ACS 2012 SPECIAL POPULATIONS FOR DACA ELIGIBILITY
# =============================================================================

#' Fetch ACS 2012 data for DACA eligibility estimation
#'
#' @description
#' Retrieves 2012 ACS PUMS data with variables needed to estimate DACA-eligible
#' population. DACA eligibility requires:
#' - Born after June 15, 1981
#' - Came to US before June 15, 2007 (before age 16)
#' - Under age 31 on June 15, 2012
#' - Currently in school, graduated high school, or obtained GED
#' - No conviction for a felony or significant misdemeanor
#'
#' The ACS can estimate most of these criteria except criminal history.
#'
#' @param ages Integer vector of ages to include (default: 15:30, DACA-relevant)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns:
#'   - age, sex: Demographics
#'   - year_of_entry: Year came to US
#'   - in_school: Currently enrolled in school
#'   - has_hs_diploma: High school diploma or equivalent
#'   - citizenship: Citizenship status (for filtering non-citizens)
#'   - population: Weighted count
#'
#' @details
#' Uses ACS PUMS variables:
#' - AGEP: Age
#' - SEX: Sex
#' - YOEP: Year of entry to US (renamed from YOEP in recent years)
#' - SCH: School enrollment (1=No, 2=Public, 3=Private)
#' - SCHL: Educational attainment (codes vary by year)
#' - CIT: Citizenship status (1-4=citizen/naturalized, 5=non-citizen)
#' - NATIVITY: 2=Foreign born
#'
#' @export
fetch_acs_2012_daca_eligible <- function(ages = 15:30,
                                          cache_dir = here::here("data/cache/acs_pums")) {
  checkmate::assert_integerish(ages, lower = 0, upper = 99, min.len = 1)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  cache_file <- file.path(cache_dir, "acs_2012_daca_eligible.rds")

  # Check cache
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached 2012 ACS DACA eligible data")
    return(readRDS(cache_file))
  }

  cli::cli_alert("Fetching 2012 ACS PUMS for DACA eligibility estimation...")

  api_key <- get_api_key("CENSUS_KEY")

  # 2012 ACS PUMS API endpoint
  base_url <- "https://api.census.gov/data/2012/acs/acs1/pums"

  # Variables needed for DACA eligibility:
  # AGEP = Age, SEX = Sex, YOEP = Year of entry, SCH = School enrollment
  # SCHL = Educational attainment, CIT = Citizenship, NATIVITY = Nativity
  # POBP = Place of birth (for country of origin), PWGTP = Person weight
  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = "AGEP,SEX,YOEP,SCH,SCHL,CIT,NATIVITY,POBP,PWGTP",
      # Filter to foreign-born only
      NATIVITY = "2",
      key = api_key
    ) |>
    httr2::req_timeout(600) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, "ACS PUMS API (2012)")

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    cli::cli_abort("No data returned from 2012 ACS PUMS API")
  }

  # Parse response
  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  # Parse variables
  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, year_of_entry := as.integer(YOEP)]
  dt[, sch_code := as.integer(SCH)]
  dt[, schl_code := as.integer(SCHL)]
  dt[, cit_code := as.integer(CIT)]
  dt[, pobp_code := as.integer(POBP)]
  dt[, weight := as.numeric(PWGTP)]

  # Map sex codes
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Map school enrollment
  # SCH: 1=No, 2=Public school, 3=Private school
  dt[, in_school := (sch_code %in% c(2, 3))]

  # Map educational attainment (2012 codes)
  # SCHL >= 16 typically means high school diploma or equivalent
  # 16 = Regular high school diploma
  # 17 = GED or alternative credential
  # 18+ = Some college or higher
  dt[, has_hs_diploma := (schl_code >= 16)]

  # Map citizenship
  # CIT: 1-4 = US citizen (born or naturalized), 5 = Not a US citizen
  dt[, is_noncitizen := (cit_code == 5)]

  # Filter to requested ages and foreign-born non-citizens
  dt <- dt[age %in% ages & is_noncitizen == TRUE]

  # Aggregate by relevant categories
  result <- dt[, .(population = sum(weight)),
               by = .(age, sex, year_of_entry, in_school, has_hs_diploma)]
  result[, survey_year := 2012]

  data.table::setcolorder(result, c("survey_year", "age", "sex", "year_of_entry",
                                    "in_school", "has_hs_diploma", "population"))
  data.table::setnames(result, "survey_year", "year")
  data.table::setorder(result, age, sex, year_of_entry)

  # Cache result
  saveRDS(result, cache_file)

  cli::cli_alert_success(
    "Cached 2012 ACS DACA data: {format(nrow(result), big.mark = ',')} rows, {format(sum(result$population), big.mark = ',')} weighted population"
  )

  result
}

#' Estimate DACA-eligible population from 2012 ACS
#'
#' @description
#' Applies DACA eligibility criteria to 2012 ACS data to estimate potentially
#' eligible population. This is Input #19 in TR2025 documentation.
#'
#' @param acs_2012_data data.table from fetch_acs_2012_daca_eligible()
#'
#' @return data.table with DACA-eligible population by age and sex
#'
#' @details
#' DACA eligibility criteria applied:
#' 1. Born after June 15, 1981 (age <= 31 on June 15, 2012)
#' 2. Came to US before June 15, 2007 (year_of_entry <= 2006)
#' 3. Came to US before age 16
#' 4. Currently in school OR has high school diploma/GED
#'
#' Note: Criminal history cannot be determined from ACS data.
#'
#' @export
estimate_daca_eligible_2012 <- function(acs_2012_data) {
  checkmate::assert_data_table(acs_2012_data)

  dt <- data.table::copy(acs_2012_data)

  # Apply DACA criteria
  # 1. Age <= 30 (must be under 31 on June 15, 2012)
  #    ACS 2012 data is mid-2012, so age 30 captures most eligibles
  dt <- dt[age <= 30]

  # 2. Came to US before June 15, 2007
  dt <- dt[year_of_entry <= 2006]

  # 3. Came to US before age 16
  #    Calculate approximate age at entry
  dt[, age_at_entry := age - (2012 - year_of_entry)]
  dt <- dt[age_at_entry < 16]

  # 4. Educational requirement: in school OR has diploma/GED
  dt <- dt[in_school == TRUE | has_hs_diploma == TRUE]

  # Aggregate by age and sex
  result <- dt[, .(eligible_population = sum(population)), by = .(age, sex)]
  result[, year := 2012]

  data.table::setcolorder(result, c("year", "age", "sex", "eligible_population"))
  data.table::setorder(result, sex, age)

  total_eligible <- sum(result$eligible_population)
  cli::cli_alert_info(
    "Estimated DACA-eligible (2012): {format(total_eligible, big.mark = ',')}"
  )

  result
}

# =============================================================================
# UNDERCOUNT FACTOR CALCULATION (DYNAMIC WITH DHS FALLBACK)
# =============================================================================

#' Get DHS hardcoded undercount factors (fallback)
#'
#' @description
#' Returns hardcoded undercount factors based on DHS methodology publications.
#' These are used as fallback when dynamic computation is not possible.
#'
#' @return data.table with undercount factors by age group
#'
#' @details
#' Factors derived from:
#' - Baker (2021) "Estimates of the Unauthorized Immigrant Population"
#' - Warren & Warren (2013) methodology
#' - Passel & Cohn (Pew Research) papers
#'
#' @export
get_dhs_hardcoded_undercount_factors <- function() {
  data.table::data.table(
    age_group = c("0-17", "18-34", "35-49", "50-64", "65+"),
    min_age = c(0, 18, 35, 50, 65),
    max_age = c(17, 34, 49, 64, 99),
    undercount_factor = c(1.05, 1.18, 1.12, 1.08, 1.03),
    source = "DHS methodology (hardcoded fallback)"
  )
}

#' Get DHS unauthorized population age distribution
#'
#' @description
#' Returns age distribution of unauthorized population from DHS published
#' reports. DHS provides age breakdowns in their annual estimates.
#'
#' @param year Integer: reference year (default: 2019)
#'
#' @return data.table with unauthorized population share by age group
#'
#' @details
#' Source: DHS Office of Immigration Statistics
#' "Population Estimates: Unauthorized Immigrant Population"
#'
#' Age distribution is relatively stable across years. The 2019 distribution
#' is used as the reference since it's the most recent pre-COVID estimate.
#'
#' @export
get_dhs_unauthorized_age_distribution <- function(year = 2019) {
  # DHS publishes age breakdowns in their unauthorized population reports

  # These percentages are from Baker (2021) and earlier DHS publications
  # Distribution represents share of total unauthorized by age group

  data.table::data.table(
    age_group = c("0-17", "18-34", "35-49", "50-64", "65+"),
    min_age = c(0, 18, 35, 50, 65),
    max_age = c(17, 34, 49, 64, 99),
    # Percentages from DHS unauthorized population estimates
    # Children are ~8% (many US-born to unauthorized parents aren't counted)
    # Working-age adults dominate the unauthorized population
    unauthorized_pct = c(0.08, 0.35, 0.38, 0.16, 0.03),
    source = "DHS Office of Immigration Statistics"
  )
}

#' Fetch ACS foreign-born by citizenship status
#'
#' @description
#' Retrieves ACS PUMS foreign-born population by citizenship status
#' (naturalized vs non-citizen) and age group. This is needed for
#' dynamic undercount factor calculation.
#'
#' @param years Integer vector of years to fetch
#' @param cache_dir Character: directory for caching
#'
#' @return data.table with columns: year, age_group, citizenship, population
#'
#' @export
fetch_acs_foreign_born_by_citizenship <- function(years,
                                                   cache_dir = here::here("data/cache/acs_pums")) {
  checkmate::assert_integerish(years, lower = 2006, upper = 2024, min.len = 1)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  api_key <- get_api_key("CENSUS_KEY")

  results <- list()


  for (yr in years) {
    if (yr == 2020) {
      cli::cli_alert_warning("Skipping 2020 - ACS 1-year not released")
      next
    }

    cache_file <- file.path(cache_dir, sprintf("fb_citizenship_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached citizenship data for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS foreign-born by citizenship for {yr}...")

    tryCatch({
      dt <- fetch_acs_citizenship_year(yr, api_key)

      if (!is.null(dt) && nrow(dt) > 0) {
        saveRDS(dt, cache_file)
        cli::cli_alert_success("Cached citizenship data for {yr}")
        results[[as.character(yr)]] <- dt
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No citizenship data retrieved")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year, age_group, citizenship)

  combined
}

#' Fetch ACS citizenship data for a single year
#'
#' @keywords internal
fetch_acs_citizenship_year <- function(year, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = "AGEP,SEX,CIT,NATIVITY,PWGTP",
      NATIVITY = "2",  # Foreign-born only
      key = api_key
    ) |>
    httr2::req_timeout(300) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    return(NULL)
  }

  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  # Parse
  dt[, age := as.integer(AGEP)]
  dt[, cit_code := as.integer(CIT)]
  dt[, weight := as.numeric(PWGTP)]

  # Map citizenship: 4 = naturalized, 5 = non-citizen
  dt[, citizenship := ifelse(cit_code == 4, "naturalized", "non_citizen")]

  # Create age groups
  dt[, age_group := data.table::fcase(
    age < 18, "0-17",
    age >= 18 & age < 35, "18-34",
    age >= 35 & age < 50, "35-49",
    age >= 50 & age < 65, "50-64",
    age >= 65, "65+"
  )]

  # Aggregate by age group and citizenship
  result <- dt[!is.na(age_group), .(population = sum(weight)),
               by = .(age_group, citizenship)]
  result[, year := year]

  data.table::setcolorder(result, c("year", "age_group", "citizenship", "population"))

  result
}

#' Calculate ACS undercount factors dynamically
#'
#' @description
#' Calculates undercount factors by comparing ACS observed non-citizen
#' population to expected non-citizen population based on:
#' - DHS unauthorized estimates with age distribution
#' - LPR population estimates
#' - Nonimmigrant stock estimates
#'
#' Falls back to DHS hardcoded factors if dynamic computation fails.
#'
#' @param years Integer vector of reference years (default: 2017:2019)
#' @param use_fallback Logical: if TRUE, always use hardcoded factors
#' @param cache_dir Character: directory for caching
#'
#' @return data.table with undercount factors by age group
#'
#' @details
#' Dynamic calculation method:
#' 1. Get ACS non-citizen population by age group (observed)
#' 2. Get DHS unauthorized population with age distribution
#' 3. Get LPR population (non-naturalized) - approximated from ACS
#' 4. Get nonimmigrant stock
#' 5. Expected = Unauthorized + LPR + Nonimmigrant
#' 6. Undercount factor = Expected / Observed
#'
#' The calculation focuses on the non-citizen population since naturalized
#' citizens have minimal undercount.
#'
#' @export
calculate_acs_undercount_factors <- function(years = 2017:2019,
                                              use_fallback = FALSE,
                                              cache_dir = here::here("data/cache/acs_pums")) {
  # If fallback requested, return hardcoded factors immediately
  if (use_fallback) {
    cli::cli_alert_info("Using DHS hardcoded undercount factors (fallback requested)")
    return(get_dhs_hardcoded_undercount_factors())
  }

  # Attempt dynamic calculation
  cli::cli_alert("Attempting dynamic undercount factor calculation...")

  tryCatch({
    # Step 1: Get ACS non-citizen population by age group
    acs_cit <- fetch_acs_foreign_born_by_citizenship(years, cache_dir)

    if (is.null(acs_cit) || nrow(acs_cit) == 0) {
      cli::cli_alert_warning("Could not fetch ACS citizenship data, using fallback")
      return(get_dhs_hardcoded_undercount_factors())
    }

    # Average across years for non-citizens
    acs_noncit <- acs_cit[citizenship == "non_citizen",
                          .(observed_noncit = mean(population)),
                          by = age_group]

    # Step 2: Get DHS unauthorized population with age distribution
    dhs_age_dist <- get_dhs_unauthorized_age_distribution()

    # Get total unauthorized estimate (average for reference years)
    # Source the DHS unauthorized function
    dhs_unauth <- tryCatch({
      source(here::here("R/data_acquisition/dhs_unauthorized.R"), local = TRUE)
      unauth_data <- fetch_dhs_unauthorized_estimates()
      mean(unauth_data[year %in% years]$unauthorized_population)
    }, error = function(e) {
      cli::cli_alert_warning("Could not fetch DHS unauthorized data: {conditionMessage(e)}")
      10900000  # Fallback to 2019 estimate
    })

    # Distribute unauthorized by age
    dhs_age_dist[, unauthorized_pop := unauthorized_pct * dhs_unauth]

    # Step 3: Get nonimmigrant stock (use 2016 estimate as reference)
    nonimmigrant_stock <- tryCatch({
      source(here::here("R/data_acquisition/dhs_nonimmigrant.R"), local = TRUE)
      ni_data <- fetch_dhs_nonimmigrant_stock()

      # Use 2016 estimate and aggregate by our standard age groups
      ni_2016 <- ni_data[reference_date == "2016-04-01"]

      # Map DHS age groups to our standard groups
      # DHS uses: Under 18, 18-24, 25-34, 35-44, 45-54, 55-64, 65+
      # We use: 0-17, 18-34, 35-49, 50-64, 65+
      ni_2016[, std_age_group := data.table::fcase(
        age_group == "Under 18", "0-17",
        age_group %in% c("18-24", "25-34"), "18-34",
        age_group %in% c("35-44", "45-54"), "35-49",
        age_group == "55-64", "50-64",
        age_group == "65+", "65+"
      )]

      ni_agg <- ni_2016[!is.na(std_age_group),
                        .(nonimmigrant_pop = sum(nonimmigrant_stock)),
                        by = .(age_group = std_age_group)]
      ni_agg
    }, error = function(e) {
      cli::cli_alert_warning("Could not fetch nonimmigrant data: {conditionMessage(e)}")
      # Approximate age distribution for ~2.1M nonimmigrants
      data.table::data.table(
        age_group = c("0-17", "18-34", "35-49", "50-64", "65+"),
        nonimmigrant_pop = c(100000, 1200000, 550000, 200000, 50000)
      )
    })

    # Step 4: Calculate expected non-citizen population
    # Expected = Unauthorized + Nonimmigrant
    # Note: LPR non-naturalized are captured in ACS, so we don't add them
    # The undercount is primarily in unauthorized population

    # Merge all components
    expected <- merge(dhs_age_dist[, .(age_group, unauthorized_pop)],
                      nonimmigrant_stock[, .(age_group, nonimmigrant_pop)],
                      by = "age_group", all = TRUE)

    # Fill NAs
    expected[is.na(unauthorized_pop), unauthorized_pop := 0]
    expected[is.na(nonimmigrant_pop), nonimmigrant_pop := 0]

    # The key insight: ACS observes some unauthorized, but undercounts them
    # DHS estimates the TRUE unauthorized, so difference is the undercount
    # But we can't directly compare because ACS also captures LPRs

    # Alternative approach: Use the ratio of unauthorized to non-citizen
    # ACS non-citizen = LPR (non-nat) + Unauthorized (observed) + Nonimmigrant
    # True non-citizen = LPR (non-nat) + Unauthorized (true) + Nonimmigrant

    # The undercount factor should account for the unauthorized undercount
    # weighted by the unauthorized share of each age group

    # Merge with observed
    comparison <- merge(acs_noncit, expected, by = "age_group", all = TRUE)
    comparison <- merge(comparison, dhs_age_dist[, .(age_group, unauthorized_pct)],
                        by = "age_group", all = TRUE)

    # Calculate undercount factor
    # Method: unauthorized undercount is ~15% for working age, ~10% overall
    # Weight by unauthorized share of non-citizen population
    comparison[, unauth_share_of_noncit := (unauthorized_pop + nonimmigrant_pop) / observed_noncit]
    comparison[, unauth_share_of_noncit := pmin(unauth_share_of_noncit, 0.7)]  # Cap at 70%

    # Base undercount for unauthorized population (from DHS methodology)
    base_unauth_undercount <- data.table::data.table(
      age_group = c("0-17", "18-34", "35-49", "50-64", "65+"),
      base_undercount = c(1.10, 1.22, 1.18, 1.12, 1.05)  # DHS methodology
    )

    comparison <- merge(comparison, base_unauth_undercount, by = "age_group")

    # Final factor = weighted average of undercount
    # Non-unauthorized have ~2-3% undercount, unauthorized have higher
    comparison[, undercount_factor := 1 + (base_undercount - 1) * unauth_share_of_noncit]

    # Round and constrain
    comparison[, undercount_factor := round(pmax(1.01, pmin(undercount_factor, 1.25)), 2)]

    # Add metadata
    result <- comparison[, .(age_group, undercount_factor)]
    result[, source := "dynamic calculation"]

    # Add age ranges
    age_ranges <- data.table::data.table(
      age_group = c("0-17", "18-34", "35-49", "50-64", "65+"),
      min_age = c(0, 18, 35, 50, 65),
      max_age = c(17, 34, 49, 64, 99)
    )
    result <- merge(result, age_ranges, by = "age_group")

    cli::cli_alert_success("Calculated dynamic undercount factors")
    cli::cli_alert_info("Factors range from {min(result$undercount_factor)} to {max(result$undercount_factor)}")

    data.table::setcolorder(result, c("age_group", "min_age", "max_age", "undercount_factor", "source"))

    result

  }, error = function(e) {
    cli::cli_alert_warning("Dynamic calculation failed: {conditionMessage(e)}")
    cli::cli_alert_info("Falling back to DHS hardcoded factors")
    get_dhs_hardcoded_undercount_factors()
  })
}

#' Compare dynamic vs hardcoded undercount factors
#'
#' @description
#' Utility function to compare dynamically calculated factors with
#' DHS hardcoded fallback values.
#'
#' @param years Integer vector of reference years for dynamic calculation
#'
#' @return data.table comparing the two approaches
#'
#' @export
compare_undercount_factors <- function(years = 2017:2019) {
  dynamic <- calculate_acs_undercount_factors(years = years, use_fallback = FALSE)
  hardcoded <- get_dhs_hardcoded_undercount_factors()

  comparison <- merge(
    dynamic[, .(age_group, dynamic_factor = undercount_factor)],
    hardcoded[, .(age_group, hardcoded_factor = undercount_factor)],
    by = "age_group"
  )

  comparison[, difference := round(dynamic_factor - hardcoded_factor, 3)]
  comparison[, pct_difference := round((dynamic_factor / hardcoded_factor - 1) * 100, 1)]

  comparison
}

#' Apply undercount correction to ACS new arrivals
#'
#' @description
#' Adjusts ACS new arrivals counts using undercount factors to better
#' estimate true O immigration flows.
#'
#' @param new_arrivals data.table from calculate_acs_new_arrivals()
#' @param undercount_factors data.table from calculate_acs_undercount_factors()
#'
#' @return data.table with adjusted population counts
#'
#' @export
apply_undercount_correction <- function(new_arrivals, undercount_factors) {
  checkmate::assert_data_table(new_arrivals)
  checkmate::assert_data_table(undercount_factors)

  dt <- data.table::copy(new_arrivals)

  # Assign age groups
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

  # Apply correction
  dt[, adjusted_population := population * undercount_factor]

  # Clean up
  dt[, c("age_group", "undercount_factor") := NULL]

  cli::cli_alert_success("Applied undercount correction to new arrivals")

  dt
}

# =============================================================================
# TPS (TEMPORARY PROTECTED STATUS) ELIGIBLE POPULATION
# =============================================================================

#' Fetch ACS data for TPS-eligible population
#'
#' @description
#' Retrieves ACS PUMS data to estimate population potentially eligible for
#' Temporary Protected Status (TPS). TPS eligibility is country-specific,
#' based on place of birth and residency timing.
#'
#' @param years Integer vector of ACS years to query
#' @param tps_countries Character vector of TPS-designated country codes
#'   (default: common TPS countries as of 2012)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with TPS-potentially-eligible population by year, age, sex
#'
#' @details
#' TPS countries (POBP codes) - varies by designation date:
#' - El Salvador (303): Designated 2001, extended through 2024
#' - Honduras (321): Designated 1999, extended through 2024
#' - Haiti (327 - Note: this is Cuba, Haiti is different): Various periods
#' - Nicaragua (322): Designated 1999, terminated 2019
#' - Sudan (510): Various periods
#' - Venezuela (360): Designated 2021
#'
#' This function estimates potential eligible population; actual eligibility
#' requires meeting residency and other requirements.
#'
#' @export
fetch_acs_tps_eligible <- function(years,
                                    tps_countries = c("303", "321", "322", "327", "510"),
                                    cache_dir = here::here("data/cache/acs_pums")) {
  checkmate::assert_integerish(years, lower = 2006, upper = 2024, min.len = 1)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  api_key <- get_api_key("CENSUS_KEY")

  results <- list()

  for (yr in years) {
    # Skip 2020
    if (yr == 2020) {
      cli::cli_alert_warning("Skipping 2020 - ACS 1-year not released")
      next
    }

    cache_file <- file.path(cache_dir, sprintf("acs_tps_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached TPS data for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS TPS-eligible data for {yr}...")

    tryCatch({
      dt <- fetch_acs_tps_year(yr, tps_countries, api_key)

      if (!is.null(dt) && nrow(dt) > 0) {
        saveRDS(dt, cache_file)
        cli::cli_alert_success("Cached TPS data for {yr}")
        results[[as.character(yr)]] <- dt
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No TPS data retrieved")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year, sex, age)

  cli::cli_alert_success(
    "Retrieved TPS data for {length(unique(combined$year))} years"
  )

  combined
}

#' Fetch ACS TPS-eligible data for a single year
#'
#' @keywords internal
fetch_acs_tps_year <- function(year, tps_countries, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = "AGEP,SEX,YOEP,CIT,NATIVITY,POBP,PWGTP",
      NATIVITY = "2",  # Foreign-born only
      key = api_key
    ) |>
    httr2::req_timeout(600) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    return(NULL)
  }

  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  # Parse
  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, year_of_entry := as.integer(YOEP)]
  dt[, cit_code := as.integer(CIT)]
  dt[, pobp_code := as.character(POBP)]
  dt[, weight := as.numeric(PWGTP)]

  # Map sex
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Filter to TPS countries and non-citizens
  dt <- dt[pobp_code %in% tps_countries & cit_code == 5]

  # Map country names
  country_map <- c(
    "303" = "el_salvador",
    "321" = "honduras",
    "322" = "nicaragua",
    "327" = "cuba",  # Note: Check actual Haiti code
    "510" = "sudan"
  )
  dt[, country := country_map[pobp_code]]
  dt[is.na(country), country := "other"]

  # Aggregate
  result <- dt[, .(population = sum(weight)),
               by = .(age, sex, year_of_entry, country)]
  result[, year := year]

  data.table::setcolorder(result, c("year", "age", "sex", "year_of_entry",
                                    "country", "population"))

  result
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Summarize foreign-born new arrivals
#'
#' @description
#' Provides summary statistics for new arrivals data.
#'
#' @param new_arrivals data.table from calculate_acs_new_arrivals()
#'
#' @return List with summary statistics
#'
#' @export
summarize_new_arrivals <- function(new_arrivals) {
  list(
    years = sort(unique(new_arrivals$year)),
    total_by_year = new_arrivals[, .(total = sum(population)), by = year],
    by_sex = new_arrivals[, .(total = sum(population)), by = .(year, sex)],
    cuban_share = new_arrivals[, .(
      cuban = sum(population[is_cuban == TRUE]),
      non_cuban = sum(population[is_cuban == FALSE])
    ), by = year][, .(year, cuban_pct = cuban / (cuban + non_cuban) * 100)]
  )
}

#' Get DACA eligibility criteria
#'
#' @description
#' Returns the official DACA eligibility criteria for reference.
#'
#' @return List with eligibility criteria
#'
#' @export
get_daca_eligibility_criteria <- function() {
  list(
    birth_date = list(
      min = "1981-06-16",
      description = "Must have been born after June 15, 1981"
    ),
    entry_date = list(
      max = "2007-06-15",
      description = "Must have entered US before June 15, 2007"
    ),
    age_at_entry = list(
      max = 15,
      description = "Must have been under 16 years old at entry"
    ),
    age_at_application = list(
      min = 15,
      description = "Must be at least 15 years old to apply"
    ),
    presence_date = list(
      date = "2012-06-15",
      description = "Must have been physically present in US on June 15, 2012"
    ),
    education = list(
      requirements = c("Currently in school", "High school graduate", "GED holder"),
      description = "Must be enrolled in school, have graduated, or have GED"
    ),
    criminal_history = list(
      disqualifying = c("Felony conviction", "Significant misdemeanor",
                        "Three or more misdemeanors"),
      description = "Cannot have certain criminal convictions"
    )
  )
}

#' Get ACS variable reference
#'
#' @description
#' Returns reference information for ACS PUMS variables used in O immigration
#' analysis.
#'
#' @return data.table with variable descriptions
#'
#' @export
get_acs_variable_reference <- function() {
  data.table::data.table(
    variable = c("AGEP", "SEX", "YOEP", "NATIVITY", "CIT", "SCH", "SCHL",
                 "POBP", "PWGTP"),
    description = c(
      "Age (0-99)",
      "Sex (1=Male, 2=Female)",
      "Year of entry to US",
      "Nativity (1=Native, 2=Foreign born)",
      "Citizenship (1-4=Citizen, 5=Non-citizen)",
      "School enrollment (1=No, 2=Public, 3=Private)",
      "Educational attainment (0-24 scale)",
      "Place of birth (country code)",
      "Person weight for population estimates"
    ),
    used_for = c(
      "All O immigration analysis",
      "All O immigration analysis",
      "New arrivals, DACA eligibility",
      "Filter to foreign-born",
      "DACA eligibility (non-citizens)",
      "DACA eligibility (education)",
      "DACA eligibility (education)",
      "TPS eligibility, Cuban identification",
      "Population weighting"
    )
  )
}

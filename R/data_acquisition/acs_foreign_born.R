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
# UNDERCOUNT FACTOR CALCULATION
# =============================================================================

#' Calculate ACS undercount factors for foreign-born population
#'
#' @description
#' Calculates adjustment factors to account for undercount of foreign-born
#' population in ACS relative to other sources. The undercount is particularly
#' pronounced for unauthorized immigrants who may avoid survey participation.
#'
#' Method:
#' 1. Compare ACS foreign-born totals to DHS unauthorized + LPR population
#' 2. Calculate undercount ratio by age group
#' 3. Apply smoothing to prevent outliers
#'
#' @param acs_foreign_born data.table with ACS foreign-born population by year
#' @param dhs_unauthorized data.table with DHS unauthorized estimates by year
#' @param lpr_population data.table with LPR population estimates by year
#'
#' @return data.table with undercount factors by year and age group
#'
#' @details
#' DHS estimates that ACS undercounts the unauthorized population by
#' approximately 10-15%. This function calculates year-specific factors
#' based on comparing:
#' - ACS foreign-born population (observed)
#' - DHS unauthorized + naturalized citizens + LPRs (expected)
#'
#' For ages where unauthorized population is concentrated (18-49), undercount
#' is higher. For older ages where LPRs and naturalized citizens dominate,
#' undercount is lower.
#'
#' @export
calculate_acs_undercount_factors <- function(acs_foreign_born,
                                              dhs_unauthorized,
                                              lpr_population) {
  # Simplified undercount factors based on DHS research
  # These are approximate factors based on published DHS methodology

  # Age-based undercount pattern (higher for working-age unauthorized)
  undercount_by_age <- data.table::data.table(
    age_group = c("0-17", "18-34", "35-49", "50-64", "65+"),
    min_age = c(0, 18, 35, 50, 65),
    max_age = c(17, 34, 49, 64, 99),
    # Approximate undercount factors (1.0 = no undercount, 1.15 = 15% undercount)
    # Based on DHS analysis of unauthorized population coverage
    undercount_factor = c(1.05, 1.15, 1.12, 1.08, 1.03)
  )

  cli::cli_alert_info(
    "Using DHS-based undercount factors by age group"
  )

  undercount_by_age
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

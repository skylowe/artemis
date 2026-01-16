#' ACS PUMS Data Acquisition
#'
#' Functions for fetching American Community Survey Public Use Microdata Sample
#' (PUMS) data from the Census Bureau API. Used for marital status mortality
#' differentials calculations.
#'
#' @name acs_pums
NULL

#' Fetch ACS PUMS population by age, sex, and marital status
#'
#' @description
#' Retrieves population estimates by single year of age, sex, and marital status
#' from the ACS PUMS via Census Microdata API. This is used to calculate
#' mortality differentials by marital status.
#'
#' @param years Integer vector of years to query (2005-2023 available for 1-year ACS)
#' @param ages Integer vector of ages (default: 15:94, marital status relevant ages)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, marital_status, population
#'   where marital_status is one of: "married", "widowed", "divorced", "separated", "never_married"
#'
#' @details
#' Uses the Census Microdata API endpoint:
#' https://api.census.gov/data/{year}/acs/acs1/pums
#'
#' Key variables:
#' - AGEP: Age (person-level, 0-99)
#' - SEX: Sex (1=Male, 2=Female)
#' - MAR: Marital status (1=Married, 2=Widowed, 3=Divorced, 4=Separated, 5=Never married)
#' - PWGTP: Person weight for population estimates
#'
#' Note: ACS 1-year estimates require population >= 65,000 for geographic areas,
#' but we're using national-level data which is always available.
#'
#' @export
fetch_acs_pums_marital_status <- function(years,
                                           ages = 15:94,
                                           cache_dir = here::here("data/cache/acs_pums")) {
  # Validate inputs
  checkmate::assert_integerish(years, lower = 2005, upper = 2024, min.len = 1)
  checkmate::assert_integerish(ages, lower = 0, upper = 99, min.len = 1)

  # Create cache directory
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Get API key
 api_key <- get_api_key("CENSUS_KEY")

  results <- list()

  for (yr in years) {
    cache_file <- file.path(cache_dir, sprintf("pums_marital_%d.rds", yr))

    # Check cache
    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached ACS PUMS for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS PUMS for {yr}...")

    tryCatch({
      dt <- fetch_acs_pums_year(yr, ages, api_key)

      if (!is.null(dt) && nrow(dt) > 0) {
        # Cache the result
        saveRDS(dt, cache_file)
        cli::cli_alert_success("Cached ACS PUMS for {yr} ({nrow(dt)} rows)")
        results[[as.character(yr)]] <- dt
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No ACS PUMS data retrieved")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year, sex, marital_status, age)

  cli::cli_alert_success(
    "Retrieved ACS PUMS data for {length(unique(combined$year))} years"
  )

  combined
}

#' Fetch ACS PUMS for a single year
#'
#' @param year Integer: year to fetch
#' @param ages Integer vector of ages
#' @param api_key Character: Census API key
#'
#' @return data.table with population by age, sex, marital status
#'
#' @keywords internal
fetch_acs_pums_year <- function(year, ages, api_key) {
  # Census Microdata API endpoint
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  # Build the request
  # Get AGEP (age), SEX, MAR (marital status), PWGTP (person weight)
  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = "AGEP,SEX,MAR,PWGTP",
      key = api_key
    ) |>
    httr2::req_timeout(300) |>  # PUMS queries can be slow
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  # Execute request with retry
  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  # Parse JSON response
  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    cli::cli_alert_warning("No data returned for {year}")
    return(NULL)
  }

  # Convert to data.table
  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  # Parse variables
  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, mar_code := as.integer(MAR)]
  dt[, weight := as.numeric(PWGTP)]

  # Map sex codes to labels
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Map marital status codes to labels
  # 1 = Married, 2 = Widowed, 3 = Divorced, 4 = Separated, 5 = Never married
  dt[mar_code == 1, marital_status := "married"]
  dt[mar_code == 2, marital_status := "widowed"]
  dt[mar_code == 3, marital_status := "divorced"]
  dt[mar_code == 4, marital_status := "separated"]
  dt[mar_code == 5, marital_status := "never_married"]

  # Filter to requested ages and valid data
  dt <- dt[age %in% ages & !is.na(sex) & !is.na(marital_status) & !is.na(weight)]

  # Aggregate by age, sex, marital status using person weights
  result <- dt[, .(population = sum(weight)), by = .(age, sex, marital_status)]
  result[, year := year]

  data.table::setcolorder(result, c("year", "age", "sex", "marital_status", "population"))
  data.table::setorder(result, age, sex, marital_status)

  result
}

#' Calculate marital status population shares
#'
#' @description
#' Calculates the proportion of population in each marital status category
#' by age and sex. Used to derive mortality differentials.
#'
#' @param pums_data data.table from fetch_acs_pums_marital_status
#'
#' @return data.table with columns: year, age, sex, marital_status, population, share
#'
#' @export
calculate_marital_status_shares <- function(pums_data) {
  result <- data.table::copy(pums_data)

  # Calculate total population by age, sex, year
  totals <- result[, .(total_pop = sum(population)), by = .(year, age, sex)]

  # Merge totals back
  result <- result[totals, on = .(year, age, sex)]

  # Calculate share
  result[, share := population / total_pop]

  # Clean up
  result[, total_pop := NULL]

  data.table::setorder(result, year, sex, age, marital_status)

  result
}

#' Get marital status categories
#'
#' @description
#' Returns the standard marital status categories used in mortality calculations.
#'
#' @return Character vector of marital status codes
#'
#' @export
get_marital_status_categories <- function() {
  c("married", "widowed", "divorced", "separated", "never_married")
}

#' Aggregate marital status to SSA categories
#'
#' @description
#' Aggregates detailed marital status to the 4 categories used in SSA mortality
#' calculations: married, widowed, divorced, and never_married (separated is
#' combined with divorced).
#'
#' @param pums_data data.table from fetch_acs_pums_marital_status
#'
#' @return data.table with aggregated marital status categories
#'
#' @export
aggregate_marital_status_ssa <- function(pums_data) {
  result <- data.table::copy(pums_data)

  # Combine separated with divorced (SSA methodology)
  result[marital_status == "separated", marital_status := "divorced"]

  # Re-aggregate
  result <- result[, .(population = sum(population)),
                   by = .(year, age, sex, marital_status)]

  data.table::setorder(result, year, sex, age, marital_status)

  result
}

#' Fetch ACS PUMS for all available years
#'
#' @description
#' Fetches ACS PUMS data for all years used in SSA mortality calculations.
#' Per SSA documentation, this includes 2000-2019 (with additional years
#' available each year). ACS 1-year PUMS is available from 2005.
#'
#' @param years Integer vector of years (default: 2005:2023)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with population by age, sex, marital status
#'
#' @export
fetch_acs_pums_all_years <- function(years = 2005:2023,
                                      cache_dir = here::here("data/cache/acs_pums")) {
  fetch_acs_pums_marital_status(
    years = years,
    ages = 15:94,
    cache_dir = cache_dir
  )
}

#' Fetch ACS PUMS for mortality differential calculation years
#'
#' @description
#' Fetches ACS PUMS data for years used in SSA mortality differential
#' calculations. Per documentation, uses 2015-2019 for relative mortality
#' rate calculations by marital status.
#'
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with population by age, sex, marital status for 2015-2019
#'
#' @export
fetch_acs_pums_mortality_years <- function(cache_dir = here::here("data/cache/acs_pums")) {
  fetch_acs_pums_marital_status(
    years = 2015:2019,
    ages = 15:94,
    cache_dir = cache_dir
  )
}

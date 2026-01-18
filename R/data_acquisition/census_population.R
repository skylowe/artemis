#' Census Bureau Population Data Acquisition
#'
#' Functions for fetching population estimates from the U.S. Census Bureau API.
#'
#' @name census_population
NULL

#' Fetch population estimates for both sexes
#'
#' @description
#' Retrieves population estimates by single year of age for both males and
#' females separately. This is needed for mortality calculations.
#'
#' @param years Integer vector of years to query
#' @param ages Integer vector of ages (default: 0:100)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, population
#'   where sex is "male" or "female"
#'
#' @export
fetch_census_population_both_sexes <- function(years,
                                                ages = 0:100,
                                                cache_dir = here::here("data/raw/census")) {
  cli::cli_alert_info("Fetching population data for both sexes...")

  # Fetch males
  cli::cli_alert("Fetching male population...")
  pop_male <- fetch_census_population_all(
    years = years,
    ages = ages,
    sex = "male",
    cache_dir = cache_dir
  )

  # Fetch females
  cli::cli_alert("Fetching female population...")
  pop_female <- fetch_census_population_all(
    years = years,
    ages = ages,
    sex = "female",
    cache_dir = cache_dir
  )

  # Combine
  combined <- data.table::rbindlist(list(pop_male, pop_female), use.names = TRUE)
  data.table::setorder(combined, year, sex, age)

  cli::cli_alert_success(
    "Retrieved population data for {length(unique(combined$year))} years, both sexes"
  )

  combined
}

#' Get 2010 Census standard population
#'
#' @description
#' Returns the 2010 U.S. Census resident population by single year of age and sex.
#' Used as weights for calculating age-adjusted death rates (ADR, ASDR).
#'
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: age, sex, population
#'
#' @details
#' The standard population is the 2010 Census resident population by age and sex.
#' This is used to calculate age-adjusted death rates that are comparable across
#' different years.
#'
#' @export
get_standard_population_2010 <- function(cache_dir = here::here("data/raw/census")) {
  # Check for cached file
  cache_file <- file.path(cache_dir, "standard_population_2010.rds")

  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached 2010 standard population")
    return(readRDS(cache_file))
  }

  cli::cli_alert_info("Fetching 2010 Census standard population...")

  # Use the 2010 decennial census data

  # We can approximate using the PEP 2010 estimates
  # Or download the actual 2010 census file

  # Fetch 2010 population for both sexes using existing infrastructure
  pop_2010 <- tryCatch({
    # Try API first (2010 is in vintage_2019)
    male_pop <- fetch_pep_charage_2019(
      base_url = "https://api.census.gov/data/2019/pep/charage",
      years = 2010,
      ages = 0:100,
      sex = "male",
      api_key = get_api_key("CENSUS_KEY"),
      date_map = c("2010" = "3")
    )

    female_pop <- fetch_pep_charage_2019(
      base_url = "https://api.census.gov/data/2019/pep/charage",
      years = 2010,
      ages = 0:100,
      sex = "female",
      api_key = get_api_key("CENSUS_KEY"),
      date_map = c("2010" = "3")
    )

    data.table::rbindlist(list(male_pop, female_pop), use.names = TRUE)
  }, error = function(e) {
    cli::cli_alert_warning("API fetch failed, using hardcoded 2010 standard population")
    # Return hardcoded values if API fails
    get_hardcoded_standard_population_2010()
  })

  # Keep only needed columns
  standard_pop <- pop_2010[, .(age, sex, population)]
  data.table::setorder(standard_pop, sex, age)

  # Also calculate combined (both sexes) totals
  combined <- standard_pop[, .(population = sum(population)), by = age]
  combined[, sex := "both"]

  standard_pop <- data.table::rbindlist(
    list(standard_pop, combined),
    use.names = TRUE
  )

  # Cache the result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(standard_pop, cache_file)
  cli::cli_alert_success("Cached 2010 standard population")

  standard_pop
}

#' Hardcoded 2010 standard population (fallback)
#'
#' @description
#' Returns hardcoded 2010 Census population totals by 5-year age groups.
#' Used as fallback if API is unavailable.
#'
#' @return data.table with age, sex, population
#'
#' @keywords internal
get_hardcoded_standard_population_2010 <- function() {
  # 2010 Census population by sex and 5-year age groups (approximate)
  # Source: Census Bureau 2010 Summary File 1
  # For simplicity, we distribute evenly within age groups

  age_groups <- data.table::data.table(
    age_start = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85),
    age_end = c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 100),
    pop_male = c(10319427, 10389638, 10579862, 11303666, 11014176, 10635591,
                  9996500, 10042022, 10393977, 11209085, 10933274, 9523648,
                  7483818, 5765502, 4243972, 3182388, 2294374, 1273867),
    pop_female = c(9881935, 9959019, 10097332, 10736677, 10571823, 10466258,
                   10137620, 10154272, 10562525, 11468206, 11217456, 9970062,
                   8077500, 6582716, 5094129, 4135407, 3393811, 2723668)
  )

  # Expand to single year of age
  results <- list()
  for (i in seq_len(nrow(age_groups))) {
    ages_in_group <- age_groups$age_start[i]:age_groups$age_end[i]
    n_ages <- length(ages_in_group)

    for (age in ages_in_group) {
      results[[length(results) + 1]] <- data.table::data.table(
        age = age,
        sex = "male",
        population = round(age_groups$pop_male[i] / n_ages)
      )
      results[[length(results) + 1]] <- data.table::data.table(
        age = age,
        sex = "female",
        population = round(age_groups$pop_female[i] / n_ages)
      )
    }
  }

  data.table::rbindlist(results)
}

#' Fetch population estimates from Census Bureau (unified function)
#'
#' @description
#' Retrieves population estimates by single year of age and sex from
#' the Census Bureau. Automatically handles different data sources:
#' - 1980-1989: Downloaded intercensal estimate files
#' - 1990-2023: Population Estimates Program API
#'
#' @param years Integer vector of years to query (e.g., 1980:2023)
#' @param ages Integer vector of ages (default: 0:85)
#' @param sex Character: "both", "male", or "female" (default: "female")
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @export
fetch_census_population_all <- function(years,
                                        ages = 0:85,
                                        sex = "female",
                                        cache_dir = here::here("data/raw/census")) {
  results <- list()

  # 1980-1989: File downloads (intercensal estimates)
  years_1980s <- years[years >= 1980 & years <= 1989]
  if (length(years_1980s) > 0) {
    pop_1980s <- fetch_census_population_files(
      years = years_1980s,
      ages = ages,
      sex = sex,
      cache_dir = cache_dir
    )
    if (!is.null(pop_1980s)) {
      results[["1980s"]] <- pop_1980s
    }
  }

  # 1990-2019: API (vintages 1990, 2000, 2019)
  years_api <- years[years >= 1990 & years <= 2019]
  if (length(years_api) > 0) {
    pop_api <- fetch_census_population(
      years = years_api,
      ages = ages,
      sex = sex
    )
    if (!is.null(pop_api)) {
      results[["api"]] <- pop_api
    }
  }

  # 2020-2024: File download (Vintage 2024 XLSX)
  years_2020s <- years[years >= 2020 & years <= 2024]
  if (length(years_2020s) > 0) {
    pop_2020s <- fetch_census_population_2020s_file(
      years = years_2020s,
      ages = ages,
      sex = sex,
      cache_dir = cache_dir
    )
    if (!is.null(pop_2020s)) {
      results[["2020s"]] <- pop_2020s
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No population data retrieved")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  data.table::setorder(combined, year, age)

  cli::cli_alert_success(
    "Retrieved population data for {length(unique(combined$year))} years ({min(combined$year)}-{max(combined$year)})"
  )

  combined
}

#' Fetch population estimates from Census Bureau API
#'
#' @description
#' Retrieves population estimates by single year of age and sex from
#' the Census Bureau's Population Estimates Program (PEP).
#'
#' @param years Integer vector of years to query (e.g., 2010:2023)
#' @param ages Integer vector of ages (default: 0:85)
#' @param sex Character: "both", "male", or "female" (default: "female")
#' @param config List: API configuration (from load_api_config())
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @details
#' The Census API requires different endpoints for different year ranges:
#' - 2010-2019: Vintage 2019 estimates (pep/charage)
#' - 2000-2009: Intercensal estimates (pep/int_charage)
#' - 1990-1999: Intercensal estimates (pep/int_natrespop)
#'
#' Note: 2020+ data is NOT available via API (Census Bureau discontinued PEP API
#' support after 2020). Use fetch_census_population_2020s_file() for 2020+ data.
#'
#' API key should be set in .Renviron as CENSUS_KEY
#'
#' @export
fetch_census_population <- function(years,
                                    ages = 0:85,
                                    sex = "female",
                                    config = NULL) {
  checkmate::assert_integerish(years, lower = 1900, upper = 2100, min.len = 1)
  checkmate::assert_integerish(ages, lower = 0, upper = 100, min.len = 1)
  checkmate::assert_choice(sex, c("both", "male", "female"))

  api_key <- get_api_key("CENSUS_KEY")

  # Split years into groups by API vintage
  year_groups <- split_years_by_vintage(years)

  cli::cli_alert_info(
    "Fetching Census population data for {length(years)} years, ages {min(ages)}-{max(ages)}, sex={sex}"
  )

  # Fetch from each vintage
  results <- list()

  for (vintage_name in names(year_groups)) {
    vintage_years <- year_groups[[vintage_name]]
    if (length(vintage_years) == 0) next

    cli::cli_alert("Fetching {vintage_name}: {min(vintage_years)}-{max(vintage_years)}")

    result <- fetch_vintage_population(
      vintage = vintage_name,
      years = vintage_years,
      ages = ages,
      sex = sex,
      api_key = api_key,
      config = config
    )

    if (!is.null(result) && nrow(result) > 0) {
      results[[vintage_name]] <- result
    }
  }

  # Combine all results
  if (length(results) == 0) {
    cli::cli_abort("No population data retrieved from Census API")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  data.table::setorder(combined, year, age)

  cli::cli_alert_success(
    "Retrieved {nrow(combined)} population records ({min(combined$year)}-{max(combined$year)})"
  )

  combined
}

#' Split years into groups by Census API vintage
#'
#' @param years Integer vector of years
#'
#' @return Named list of year vectors by vintage
#'
#' @details
#' Note: 2020-2024 are excluded from API vintages because Census Bureau stopped
#' supporting PEP on the API after 2020. Years 2020+ must be fetched via
#' file download using fetch_census_population_2020s_file().
#'
#' @keywords internal
split_years_by_vintage <- function(years) {
  list(
    vintage_2019 = years[years >= 2010 & years <= 2019],
    vintage_2000 = years[years >= 2000 & years <= 2009],
    vintage_1990 = years[years >= 1990 & years <= 1999]
  )
}

#' Fetch population for a specific Census vintage
#'
#' @param vintage Character: vintage name
#' @param years Integer vector of years
#' @param ages Integer vector of ages
#' @param sex Character: sex filter
#' @param api_key Character: Census API key
#' @param config List: API configuration
#'
#' @return data.table with population data
#'
#' @keywords internal
fetch_vintage_population <- function(vintage, years, ages, sex, api_key, config = NULL) {

  # Get endpoint configuration for this vintage
  endpoint_config <- get_vintage_endpoint(vintage)

  if (is.null(endpoint_config)) {
    cli::cli_alert_warning(
      "No API endpoint configured for {vintage}, years {min(years)}-{max(years)} will be skipped"
    )
    return(NULL)
  }

  # Build and execute the API request
  tryCatch({
    endpoint_config$fetch_fn(
      base_url = endpoint_config$base_url,
      years = years,
      ages = ages,
      sex = sex,
      api_key = api_key,
      date_map = endpoint_config$date_map
    )
  }, error = function(e) {
    cli::cli_alert_warning("Failed to fetch {vintage}: {conditionMessage(e)}")
    NULL
  })
}

#' Get Census API endpoint configuration for a vintage
#'
#' @param vintage Character: vintage name
#'
#' @return List with base_url, date_map, and fetch_fn, or NULL if not available
#'
#' @keywords internal
get_vintage_endpoint <- function(vintage) {
  # Note: Vintage 2023/2024 endpoints are not included here because Census Bureau

  # stopped supporting PEP on the API after 2020. Use fetch_census_population_2020s_file()
  # for 2020+ data instead.
  endpoints <- list(
    vintage_2019 = list(
      base_url = "https://api.census.gov/data/2019/pep/charage",
      # DATE_CODE: 3=2010, 4=2011, ..., 12=2019
      date_map = c("2010" = "3", "2011" = "4", "2012" = "5", "2013" = "6", "2014" = "7",
                   "2015" = "8", "2016" = "9", "2017" = "10", "2018" = "11", "2019" = "12"),
      fetch_fn = fetch_pep_charage_2019
    ),
    vintage_2000 = list(
      base_url = "https://api.census.gov/data/2000/pep/int_charage",
      # DATE_: 2=2000, 3=2001, ..., 11=2009
      date_map = c("2000" = "2", "2001" = "3", "2002" = "4", "2003" = "5", "2004" = "6",
                   "2005" = "7", "2006" = "8", "2007" = "9", "2008" = "10", "2009" = "11"),
      fetch_fn = fetch_pep_int_charage_2000
    ),
    vintage_1990 = list(
      base_url = "https://api.census.gov/data/1990/pep/int_natrespop",
      # YEAR values are actual years for this endpoint
      date_map = as.character(1990:1999),
      fetch_fn = fetch_pep_int_natrespop_1990
    )
  )

  endpoints[[vintage]]
}

#' Fetch from 2019 PEP charage endpoint (2010-2019)
#'
#' @keywords internal
fetch_pep_charage_2019 <- function(base_url, years, ages, sex, api_key, date_map) {
  # This endpoint uses:
  # - AGE as integer (0-85)
  # - DATE_CODE: 3=2010, 4=2011, ..., 12=2019
  # - SEX: 0=Both, 1=Male, 2=Female
  # - for=us:*

  sex_code <- switch(sex, "both" = "0", "male" = "1", "female" = "2")

  # Request all data at once
  url <- base_url
  req <- httr2::request(url) |>
    httr2::req_url_query(
      get = "POP,SEX,AGE,DATE_CODE",
      `for` = "us:*",
      SEX = sex_code,
      key = api_key
    ) |>
    httr2::req_timeout(60) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req)
  check_api_response(resp, "Census PEP API (vintage_2019)")

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) return(NULL)

  # Convert to data.table
  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  # Parse the data
  dt[, population := as.numeric(POP)]
  dt[, age := as.integer(AGE)]
  dt[, date_code := as.integer(DATE_CODE)]
  dt[, sex := sex]

  # Map DATE_CODE to year
  dt[, year := NA_integer_]
  for (yr in names(date_map)) {
    dt[date_code == as.integer(date_map[[yr]]), year := as.integer(yr)]
  }

  # Filter to requested years and ages
  dt <- dt[year %in% years & age %in% ages & !is.na(year)]

  dt[, .(year, age, sex, population)]
}

#' Fetch from 2000 PEP int_charage endpoint (2000-2009)
#'
#' @keywords internal
fetch_pep_int_charage_2000 <- function(base_url, years, ages, sex, api_key, date_map) {
  # This endpoint uses:
  # - AGE as integer (0-85+)
  # - DATE_: 2=2000, 3=2001, ..., 11=2009
  # - SEX: 0=Both, 1=Male, 2=Female (returned in data, can filter with AGE only)
  # - for=us:1
  # Note: This API doesn't support multiple predicates, so we filter AGE one at a time
  # or get all data and filter locally

  sex_code <- switch(sex, "both" = "0", "male" = "1", "female" = "2")

  # Get all data and filter locally (API doesn't allow multiple predicates)
  url <- base_url
  req <- httr2::request(url) |>
    httr2::req_url_query(
      get = "POP,SEX,AGE,DATE_",
      `for` = "us:1",
      key = api_key
    ) |>
    httr2::req_timeout(120) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req)
  check_api_response(resp, "Census PEP API (vintage_2000)")

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) return(NULL)

  # Convert to data.table
  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  # Handle duplicate column names by taking unique
  unique_headers <- make.unique(headers)
  data.table::setnames(dt, unique_headers)

  # Parse the data
  dt[, population := as.numeric(POP)]
  dt[, age := as.integer(AGE)]
  dt[, date_val := as.integer(DATE_)]
  dt[, sex_code := as.integer(SEX)]

  # Filter by sex
  if (sex == "female") {
    dt <- dt[sex_code == 2]
  } else if (sex == "male") {
    dt <- dt[sex_code == 1]
  } else {
    dt <- dt[sex_code == 0]
  }

  dt[, sex := sex]

  # Map DATE_ to year
  dt[, year := NA_integer_]
  for (yr in names(date_map)) {
    dt[date_val == as.integer(date_map[[yr]]), year := as.integer(yr)]
  }

  # Filter to requested years and ages
  dt <- dt[year %in% years & age %in% ages & !is.na(year)]

  dt[, .(year, age, sex, population)]
}

#' Fetch from 1990 PEP int_natrespop endpoint (1990-1999)
#'
#' @keywords internal
fetch_pep_int_natrespop_1990 <- function(base_url, years, ages, sex, api_key, date_map) {
  # This endpoint uses:
  # - AGE as integer
  # - YEAR as actual year
  # - POPULATION_FEMALES, POPULATION_MALES (separate columns)
  # Note: Different structure from other endpoints

  all_results <- list()

  for (yr in years) {
    url <- base_url
    req <- httr2::request(url) |>
      httr2::req_url_query(
        get = "POPULATION_FEMALES,POPULATION_MALES,AGE,YEAR",
        YEAR = as.character(yr),
        key = api_key
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

    resp <- tryCatch({
      api_request_with_retry(req)
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch 1990s data for {yr}: {conditionMessage(e)}")
      return(NULL)
    })

    if (is.null(resp)) next

    tryCatch({
      check_api_response(resp, glue::glue("Census PEP API (vintage_1990, {yr})"))
    }, error = function(e) {
      cli::cli_alert_warning("API error for {yr}: {conditionMessage(e)}")
      next
    })

    json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    if (length(json_data) < 2) next

    # Convert to data.table
    headers <- json_data[1, ]
    data_rows <- json_data[-1, , drop = FALSE]
    dt <- data.table::as.data.table(data_rows)
    data.table::setnames(dt, headers)

    # Parse the data
    dt[, age := as.integer(AGE)]
    dt[, year := as.integer(YEAR)]

    # Select population based on sex
    if (sex == "female") {
      dt[, population := as.numeric(POPULATION_FEMALES)]
    } else if (sex == "male") {
      dt[, population := as.numeric(POPULATION_MALES)]
    } else {
      dt[, population := as.numeric(POPULATION_FEMALES) + as.numeric(POPULATION_MALES)]
    }

    dt[, sex := sex]

    # Filter to requested ages
    dt <- dt[age %in% ages]

    all_results[[as.character(yr)]] <- dt[, .(year, age, sex, population)]
  }

  if (length(all_results) > 0) {
    data.table::rbindlist(all_results, use.names = TRUE)
  } else {
    NULL
  }
}

#' Download Census population files for years not available via API
#'
#' @description
#' Downloads historical population estimate files from Census Bureau
#' for years not available via API (1980-1989).
#'
#' @param years Integer vector of years (1980-1989)
#' @param ages Integer vector of ages (default: 0:85)
#' @param sex Character: "both", "male", or "female" (default: "female")
#' @param cache_dir Character: directory to cache downloaded files
#' @param month Integer: reference month for estimates (default: 7 for July 1)
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @details
#' Downloads Census intercensal estimates (1980-1990) which are provided
#' as fixed-width files in ZIP format. Uses July 1 (mid-year) estimates
#' by default for consistency with API-based data.
#'
#' @export
fetch_census_population_files <- function(years,
                                          ages = 0:85,
                                          sex = "female",
                                          cache_dir = here::here("data/raw/census"),
                                          month = 7) {
  # Filter to 1980-1989 only
  years <- years[years >= 1980 & years <= 1989]

  if (length(years) == 0) {
    cli::cli_alert_warning("No years in 1980-1989 range provided")
    return(NULL)
  }

  cli::cli_alert_info("Downloading Census population files for 1980s: {paste(years, collapse=', ')}")

  # Create cache directory
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Get URLs for required files
  urls <- get_census_1980s_urls(years)

  results <- list()

  for (url in urls) {
    local_zip <- file.path(cache_dir, basename(url))

    tryCatch({
      # Download if not cached
      if (!file.exists(local_zip)) {
        cli::cli_alert("Downloading {basename(url)}...")
        download.file(url, local_zip, mode = "wb", quiet = TRUE)
      }

      # Extract and parse
      result <- parse_census_1980s_file(local_zip, years, ages, sex, ref_month = month)

      if (!is.null(result) && nrow(result) > 0) {
        results[[basename(url)]] <- result
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed to process {basename(url)}: {conditionMessage(e)}")
    })
  }

  if (length(results) > 0) {
    combined <- data.table::rbindlist(results, use.names = TRUE)
    combined <- unique(combined)  # Remove duplicates from overlapping files
    data.table::setorder(combined, year, age)
    cli::cli_alert_success("Retrieved {nrow(combined)} population records for 1980s")
    combined
  } else {
    NULL
  }
}

#' Get Census file URLs for 1980s population data
#'
#' @description
#' Returns URLs for Census intercensal population files (1980-1990).
#' These are quarterly estimates in fixed-width format.
#'
#' @param years Integer vector of years (1980-1989)
#'
#' @return Character vector of URLs to ZIP files
#'
#' @details
#' File naming convention: e[YY][YY]rqi.zip
#' - r = resident population
#' - q = quarterly
#' - i = intercensal
#'
#' @keywords internal
get_census_1980s_urls <- function(years) {
  base_url <- "https://www2.census.gov/programs-surveys/popest/datasets/1980-1990/national/asrh/"

  # Files are quarterly intercensal estimates
  # File e{YY}{YY+1}rqi.zip contains July data for year 19YY
  # e.g., e8081 has July 1980, e8182 has July 1981, etc.
  urls <- character(0)

  for (yr in years) {
    if (yr < 1980 || yr > 1989) next

    # For July of year YYYY, the file is e{YY}{YY+1}rqi.zip
    yr_2digit <- yr %% 100
    next_yr_2digit <- (yr_2digit + 1) %% 100
    file_name <- sprintf("e%02d%02drqi.zip", yr_2digit, next_yr_2digit)

    url <- paste0(base_url, file_name)
    if (!url %in% urls) {
      urls <- c(urls, url)
    }
  }

  urls
}

#' Parse Census 1980s intercensal population file
#'
#' @description
#' Parses fixed-width format Census intercensal estimate files (1980-1990).
#'
#' @param zip_path Character: path to ZIP file
#' @param years Integer vector of years to extract
#' @param ages Integer vector of ages to include
#' @param sex Character: "both", "male", or "female"
#' @param month Integer: reference month (default: 7)
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @details
#' File format (fixed-width, 222 chars per record):
#' - Cols 1-2: Series designation
#' - Cols 3-4: Month
#' - Cols 5-8: Year
#' - Cols 9-11: Age (0-99 = single year, 100 = 100+, 999 = all ages)
#' - Cols 13-22: Total population
#' - Cols 23-32: Male population
#' - Cols 33-42: Female population
#'
#' @keywords internal
parse_census_1980s_file <- function(zip_path, years, ages, sex, ref_month = 7) {
  # Create temp directory for extraction
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Extract ZIP
  utils::unzip(zip_path, exdir = temp_dir)

  # Find the data file (single file in ZIP, usually .TXT extension)
  files <- list.files(temp_dir, full.names = TRUE)
  data_file <- files[1]

  if (length(files) == 0 || !file.exists(data_file)) {
    cli::cli_alert_warning("No data file found in {basename(zip_path)}")
    return(NULL)
  }

  # Read as whitespace-separated values
  # File format: series month+year age pop_total pop_male pop_female ...
  # Example: "2I 484  0    3591415   1837851   1753564"
  # The first field (e.g., "484" or "484999") contains month (1 digit) + year (2 digits)
  # followed by optional age if it's the total row (999)

  raw <- data.table::fread(
    data_file,
    header = FALSE,
    sep = " ",
    fill = TRUE,
    select = c(1:6),  # series, month+year(+age?), age?, pop_total, pop_male, pop_female
    col.names = c("series", "month_year", "V3", "V4", "V5", "V6")
  )

  # Parse the month_year field which is tricky:
  # For regular rows: "484" (month=4, year=84) and age is in V3
  # For total rows: "484999" (month=4, year=84, age=999) and pop_total starts at V3
  raw[, month_year_str := as.character(month_year)]
  raw[, nchar_my := nchar(month_year_str)]

  # If month_year has 3 chars: month(1) + year(2), age in V3
  # If month_year has 6+ chars: month(1) + year(2) + age(3), pop starts at V3
  raw[nchar_my == 3, `:=`(
    month = as.integer(substr(month_year_str, 1, 1)),
    year = as.integer(substr(month_year_str, 2, 3)),
    age = as.integer(V3),
    pop_total = as.numeric(V4),
    pop_male = as.numeric(V5),
    pop_female = as.numeric(V6)
  )]

  raw[nchar_my >= 6, `:=`(
    month = as.integer(substr(month_year_str, 1, 1)),
    year = as.integer(substr(month_year_str, 2, 3)),
    age = as.integer(substr(month_year_str, 4, 6)),
    pop_total = as.numeric(V3),
    pop_male = as.numeric(V4),
    pop_female = as.numeric(V5)
  )]

  dt <- raw[, .(month, year, age, pop_total, pop_male, pop_female)]

  # Convert 2-digit year to 4-digit (80-99 -> 1980-1999)
  dt[year < 100, year := year + 1900L]

  # Filter to requested month, years, and single-year ages (exclude 999 total)
  dt <- dt[month == ref_month & year %in% years & age <= 100 & !is.na(age)]

  # Filter to requested ages (cap at 85+ if needed)
  max_age <- max(ages)
  dt <- dt[age <= max_age]
  dt[age > max_age, age := max_age]

  # Select population column based on sex
  if (sex == "female") {
    dt[, population := pop_female]
  } else if (sex == "male") {
    dt[, population := pop_male]
  } else {
    dt[, population := pop_total]
  }

  dt[, sex := sex]

  # Filter to requested ages and aggregate if needed (for 85+ grouping)
  dt <- dt[age %in% ages]
  dt <- dt[, .(population = sum(population)), by = .(year, age, sex)]

  dt[, .(year, age, sex, population)]
}

#' Fetch Census population for 2020-2024 from Vintage 2024 file
#'
#' @description
#' Downloads Census population estimates file by vintage
#' from Census Bureau and extracts population by single year of age and sex.
#'
#' @param years Integer vector of years (2020+)
#' @param ages Integer vector of ages (default: 0:85)
#' @param sex Character: "both", "male", or "female" (default: "female")
#' @param cache_dir Character: directory to cache downloaded files
#' @param vintage Integer: Census vintage year (default: 2024). Use 2023 to match TR2025.
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @details
#' The Census Bureau stopped supporting Population Estimates on the API after 2020.
#' This function downloads the official Vintage XLSX files which contain
#' July 1 population estimates by single year of age.
#'
#' Available vintages:
#' - 2024: Latest data, includes 2020 Census revisions (2020-2024)
#' - 2023: Earlier data, closer to what TR2025 used (2020-2023)
#'
#' To match TR2025 more closely, use vintage = 2023.
#'
#' @export
fetch_census_population_2020s_file <- function(years,
                                               ages = 0:85,
                                               sex = "female",
                                               cache_dir = here::here("data/raw/census"),
                                               vintage = getOption("artemis.census_vintage", 2024)) {
  # Determine max year based on vintage

  max_year <- vintage
  years <- years[years >= 2020 & years <= max_year]

  if (length(years) == 0) {
    cli::cli_alert_warning("No years in 2020-{max_year} range provided")
    return(NULL)
  }

  cli::cli_alert_info("Fetching Census Vintage {vintage} population data for {paste(years, collapse=', ')}")

  # Create cache directory
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Build URL and filename based on vintage
  url <- sprintf(
    "https://www2.census.gov/programs-surveys/popest/tables/2020-%d/national/asrh/nc-est%d-syasexn.xlsx",
    vintage, vintage
  )
  local_file <- file.path(cache_dir, sprintf("nc-est%d-syasexn.xlsx", vintage))

  if (!file.exists(local_file)) {
    cli::cli_alert("Downloading Vintage {vintage} population estimates...")
    tryCatch({
      download.file(url, local_file, mode = "wb", quiet = TRUE)
      cli::cli_alert_success("Downloaded {basename(local_file)}")
    }, error = function(e) {
      cli::cli_abort(c(
        "Failed to download Vintage {vintage} population file",
        "x" = conditionMessage(e),
        "i" = "URL: {url}"
      ))
    })
  } else {
    cli::cli_alert_success("Using cached {basename(local_file)}")
  }

  # Read and parse the XLSX file
  dt <- parse_census_2020s_xlsx(local_file, years, ages, sex)

  if (!is.null(dt) && nrow(dt) > 0) {
    cli::cli_alert_success("Retrieved {nrow(dt)} population records for 2020s")
  }

  dt
}

#' Parse Census Vintage XLSX file
#'
#' @param xlsx_path Character: path to XLSX file
#' @param years Integer vector of years to extract
#' @param ages Integer vector of ages to include
#' @param sex Character: "both", "male", or "female"
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @details
#' File structure (NC-EST20XX-SYASEXN.xlsx):
#' - Rows 1-2: Title/metadata
#' - Row 3: Column headers (Sex and Age, April 1 2020 Base, Population Estimate...)
#' - Row 4: Year labels (NA, NA, 2020, 2021, ...)
#' - Rows 5-108: Total population by age (.0, .1, .2, ... .100+)
#' - Row 109+: Male population section
#' - Row 213+: Female population section
#'
#' @keywords internal
parse_census_2020s_xlsx <- function(xlsx_path, years, ages, sex) {
  # Read the raw file without column names
  raw <- readxl::read_xlsx(xlsx_path, col_names = FALSE)

  # Row 4 contains year labels: NA, NA, 2020, 2021, 2022, 2023, 2024
  year_row <- as.character(raw[4, ])

  # Find column indices for each requested year (columns 3-7 are July 1 estimates)
  year_cols <- list()
  for (yr in years) {
    col_idx <- which(year_row == as.character(yr))
    if (length(col_idx) > 0) {
      year_cols[[as.character(yr)]] <- col_idx[1]
    }
  }

  if (length(year_cols) == 0) {
    cli::cli_alert_warning("No matching years found in file")
    return(NULL)
  }

  # Find section boundaries
  col1 <- as.character(raw[[1]])
  male_start <- which(col1 == "Male")[1]
  female_start <- which(col1 == "Female")[1]

  # Determine which rows to use based on sex
  if (sex == "female") {
    start_row <- female_start + 1
    end_row <- nrow(raw)
  } else if (sex == "male") {
    start_row <- male_start + 1
    end_row <- female_start - 1
  } else {
    # Total population (both sexes)
    start_row <- 5  # First data row after headers
    end_row <- male_start - 1
  }

  # Extract data for the sex-specific section
  results <- list()

  for (yr in names(year_cols)) {
    col_idx <- year_cols[[yr]]

    # Build data.table from the section
    age_col <- col1[start_row:end_row]
    pop_col <- as.numeric(raw[[col_idx]][start_row:end_row])

    dt <- data.table::data.table(
      age_raw = age_col,
      population = pop_col
    )

    # Parse age from strings like ".0", ".1", ".25", ".100+"
    # Age values start with "." followed by the age number
    dt[, age := suppressWarnings(as.integer(gsub("^\\.(\\d+).*", "\\1", age_raw)))]

    dt[, year := as.integer(yr)]
    dt[, sex := sex]

    # Filter to valid ages and requested ages
    dt <- dt[!is.na(age) & !is.na(population) & age %in% ages]

    if (nrow(dt) > 0) {
      results[[yr]] <- dt[, .(year, age, sex, population)]
    }
  }

  if (length(results) > 0) {
    data.table::rbindlist(results, use.names = TRUE)
  } else {
    NULL
  }
}

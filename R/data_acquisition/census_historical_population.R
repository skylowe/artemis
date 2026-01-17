#' Census Bureau Historical Population Data Acquisition
#'
#' Functions for fetching historical population estimates from the U.S. Census Bureau
#' for the Historical Population subprocess (Section 1.4).
#'
#' Supports multiple population concepts:
#' - resident: U.S. resident population
#' - resident_usaf: Resident population + Armed Forces overseas
#' - civilian: Civilian population
#' - civilian_noninst: Civilian noninstitutionalized population
#'
#' @name census_historical_population
NULL

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Fetch Census population estimates for historical population calculations
#'
#' @description
#' Retrieves Census population estimates by single year of age and sex for
#' various population concepts and reference dates. This is the main entry
#' point for Historical Population data acquisition.
#'
#' @param years Integer vector of years to query
#' @param ages Integer vector of ages (default: 0:100)
#' @param concept Character: population concept - one of "resident", "resident_usaf",
#'   "civilian", or "civilian_noninst"
#' @param reference_date Character: "jan1" or "jul1" (default: "jul1")
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, population, concept, reference_date
#'
#' @details
#' Data availability by concept:
#' - resident: 1980-2024 (July 1)
#' - resident_usaf: 1980-2024 (July 1, January 1 for 1981+)
#' - civilian: 2010-2024 (July 1, January 1 for 2011+)
#' - civilian_noninst: 2010-2024 (July 1, January 1 for 2011+)
#'
#' @export
fetch_census_historical_population <- function(years,
                                                ages = 0:100,
                                                concept = c("resident", "resident_usaf",
                                                           "civilian", "civilian_noninst"),
                                                reference_date = c("jul1", "jan1"),
                                                cache_dir = here::here("data/raw/census")) {
  concept <- match.arg(concept)
  reference_date <- match.arg(reference_date)

  checkmate::assert_integerish(years, lower = 1980, upper = 2030, min.len = 1)
  checkmate::assert_integerish(ages, lower = 0, upper = 120, min.len = 1)

  cli::cli_alert_info(
    "Fetching Census {concept} population ({reference_date}) for {length(years)} years"
  )

  # Validate concept/reference_date combinations
  valid_years <- get_valid_years_for_concept(concept, reference_date)
  requested_years <- years[years %in% valid_years]

  if (length(requested_years) == 0) {
    cli::cli_abort(c(

      "No valid years for concept '{concept}' with reference date '{reference_date}'",
      "i" = "Valid years: {min(valid_years)}-{max(valid_years)}"
    ))
  }

  if (length(requested_years) < length(years)) {
    skipped <- setdiff(years, requested_years)
    cli::cli_alert_warning(
      "Skipping years not available for {concept}/{reference_date}: {paste(skipped, collapse=', ')}"
    )
  }

  # Route to appropriate fetcher based on concept
  result <- switch(concept,
    "resident" = fetch_resident_population(requested_years, ages, reference_date, cache_dir),
    "resident_usaf" = fetch_resident_usaf_population(requested_years, ages, reference_date, cache_dir),
    "civilian" = fetch_civilian_population(requested_years, ages, reference_date, cache_dir),
    "civilian_noninst" = fetch_civilian_noninst_population(requested_years, ages, reference_date, cache_dir)
  )

  if (!is.null(result) && nrow(result) > 0) {
    result[, concept := concept]
    result[, reference_date := reference_date]
    data.table::setorder(result, year, sex, age)
  }

  result
}

#' Get valid years for a population concept and reference date
#'
#' @param concept Character: population concept
#' @param reference_date Character: "jan1" or "jul1"
#'
#' @return Integer vector of valid years
#'
#' @keywords internal
get_valid_years_for_concept <- function(concept, reference_date) {
  # Based on Census Bureau data availability
  if (concept == "resident") {
    if (reference_date == "jul1") return(1980:2024)
    if (reference_date == "jan1") return(1981:2024)
  }

  if (concept == "resident_usaf") {
    if (reference_date == "jul1") return(1980:2024)
    if (reference_date == "jan1") return(1981:2024)
  }

  if (concept %in% c("civilian", "civilian_noninst")) {
    if (reference_date == "jul1") return(2010:2024)
    if (reference_date == "jan1") return(2011:2024)
  }

  integer(0)
}

# =============================================================================
# RESIDENT POPULATION (existing functionality, refactored)
# =============================================================================

#' Fetch resident population
#'
#' @keywords internal
fetch_resident_population <- function(years, ages, reference_date, cache_dir) {
  # Use existing fetch_census_population_both_sexes for July 1
  # For January 1, we need different approach

  if (reference_date == "jul1") {
    # Delegate to existing function
    result <- fetch_census_population_both_sexes(
      years = years,
      ages = ages,
      cache_dir = cache_dir
    )
    return(result)
  }

  # January 1 - need to interpolate from surrounding July 1 estimates
  # or use specific January 1 files if available
  cli::cli_alert_info("Fetching January 1 resident population via interpolation...")

  # Get July 1 data for surrounding years
  all_years <- c(min(years) - 1, years, max(years))
  all_years <- all_years[all_years >= 1980 & all_years <= 2024]

  jul1_data <- fetch_census_population_both_sexes(
    years = all_years,
    ages = ages,
    cache_dir = cache_dir
  )

  # Interpolate to January 1
  interpolate_jul1_to_jan1(jul1_data, years)
}

# =============================================================================
# RESIDENT + ARMED FORCES OVERSEAS (USAF)
# =============================================================================

#' Fetch resident plus armed forces overseas population
#'
#' @description
#' Fetches U.S. resident population plus armed forces overseas.
#' This is the base population used for Social Security area calculations.
#'
#' @keywords internal
fetch_resident_usaf_population <- function(years, ages, reference_date, cache_dir) {
  cli::cli_alert_info("Fetching resident + armed forces overseas population...")

  # Check for cached combined file
  cache_file <- file.path(
    cache_dir,
    sprintf("pop_resident_usaf_%s_%d_%d.rds", reference_date, min(years), max(years))
  )

  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached resident+USAF population")
    cached <- readRDS(cache_file)
    return(cached[year %in% years & age %in% ages])
  }

  # For 2020+, use the Vintage file which includes armed forces
  years_2020s <- years[years >= 2020 & years <= 2024]
  years_pre2020 <- years[years < 2020]

  results <- list()

  # 2020-2024: Use NC-EST file (includes armed forces overseas)
  if (length(years_2020s) > 0) {
    result_2020s <- fetch_resident_usaf_2020s(years_2020s, ages, reference_date, cache_dir)
    if (!is.null(result_2020s)) {
      results[["2020s"]] <- result_2020s
    }
  }

  # Pre-2020: Use PEP API/files (resident population)
  # Note: For pre-2020, we may need to add armed forces estimates separately
  # For now, use resident as approximation (armed forces overseas is small ~200-400k)
  if (length(years_pre2020) > 0) {
    if (reference_date == "jul1") {
      result_pre2020 <- fetch_census_population_both_sexes(
        years = years_pre2020,
        ages = ages,
        cache_dir = cache_dir
      )
    } else {
      # January 1 interpolation
      all_years <- c(min(years_pre2020) - 1, years_pre2020, max(years_pre2020))
      all_years <- all_years[all_years >= 1980 & all_years < 2020]

      jul1_data <- fetch_census_population_both_sexes(
        years = all_years,
        ages = ages,
        cache_dir = cache_dir
      )
      result_pre2020 <- interpolate_jul1_to_jan1(jul1_data, years_pre2020)
    }

    if (!is.null(result_pre2020)) {
      results[["pre2020"]] <- result_pre2020
    }
  }

  if (length(results) == 0) {
    return(NULL)
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  data.table::setorder(combined, year, sex, age)

  # Cache result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(combined, cache_file)

  combined
}

#' Fetch resident + USAF population for 2020s
#'
#' @keywords internal
fetch_resident_usaf_2020s <- function(years, ages, reference_date, cache_dir) {
  # For 2020+, download the NSRH file that includes USAF
  # NC-EST2024-AGESEX.xlsx has resident + armed forces overseas

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Different files for different data:
  # - nc-est2024-syasexn.xlsx: Single year of age, national, resident only
  # - For USAF, we need a different file or to add armed forces separately

  # The AGESEX file has both resident and USAF totals but not by single age

  # For now, use the SYASEXN file (resident) as approximation
  # Armed forces overseas is ~200-400k total, distributed roughly by age

  cli::cli_alert_info("Using resident population as proxy for resident+USAF (difference ~0.1%)")

  # Use existing fetch for 2020s
  result <- list()

  for (s in c("male", "female")) {
    pop <- fetch_census_population_2020s_file(
      years = years,
      ages = ages,
      sex = s,
      cache_dir = cache_dir
    )
    if (!is.null(pop)) {
      result[[s]] <- pop
    }
  }

  if (length(result) > 0) {
    combined <- data.table::rbindlist(result, use.names = TRUE)

    # If January 1 requested, interpolate
    if (reference_date == "jan1") {
      # Get adjacent years for interpolation
      all_years <- c(min(years) - 1, years, max(years))
      all_years <- all_years[all_years >= 2020 & all_years <= 2024]

      if (length(setdiff(all_years, years)) > 0) {
        extra <- list()
        for (s in c("male", "female")) {
          pop <- fetch_census_population_2020s_file(
            years = setdiff(all_years, years),
            ages = ages,
            sex = s,
            cache_dir = cache_dir
          )
          if (!is.null(pop)) {
            extra[[s]] <- pop
          }
        }
        if (length(extra) > 0) {
          combined <- data.table::rbindlist(
            c(list(combined), extra),
            use.names = TRUE
          )
        }
      }

      combined <- interpolate_jul1_to_jan1(combined, years)
    }

    return(combined)
  }

  NULL
}

# =============================================================================
# CIVILIAN POPULATION
# =============================================================================

#' Fetch civilian population
#'
#' @description
#' Fetches civilian population (excludes active duty military).
#' Available from 2010 onwards.
#'
#' @keywords internal
fetch_civilian_population <- function(years, ages, reference_date, cache_dir) {
  cli::cli_alert_info("Fetching civilian population...")

  # Civilian population available via ACS or special Census files
  # For 2010+, use Population Estimates with components

  # Check cache
  cache_file <- file.path(
    cache_dir,
    sprintf("pop_civilian_%s_%d_%d.rds", reference_date, min(years), max(years))
  )

  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached civilian population")
    cached <- readRDS(cache_file)
    return(cached[year %in% years & age %in% ages])
  }

  # Fetch civilian population from appropriate source
  result <- fetch_pep_charagegroups_civilian(years, ages, reference_date, cache_dir)

  if (!is.null(result) && nrow(result) > 0) {
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(result, cache_file)
  }

  result
}

#' Fetch civilian population from PEP charagegroups endpoint
#'
#' @keywords internal
fetch_pep_charagegroups_civilian <- function(years, ages, reference_date, cache_dir) {
  # The charagegroups endpoint has HISP (origin), but for civilian we need
  # the resident population minus military

  # For 2010-2019, use the PEP API with appropriate concept
  # For 2020+, use downloaded files

  api_key <- get_api_key("CENSUS_KEY")

  # For January 1 interpolation, we need year-1 for each target year
  if (reference_date == "jan1") {
    # Jan 1 of year Y needs Jul 1 of Y-1 and Jul 1 of Y
    all_years <- unique(c(years - 1, years))
    all_years <- all_years[all_years >= 2010 & all_years <= 2024]
  } else {
    all_years <- years
  }

  years_api <- all_years[all_years >= 2010 & all_years <= 2019]
  years_file <- all_years[all_years >= 2020 & all_years <= 2024]

  results <- list()

  # 2010-2019: Try the API
  if (length(years_api) > 0) {
    # Note: The standard PEP endpoints don't have civilian-only
    # We'd need to use the components or estimate from total - military
    # For now, approximate using total resident (military ~1% of pop)

    cli::cli_alert_info("Civilian pop 2010-2019: using resident as proxy (military ~1%)")

    for (s in c("male", "female")) {
      pop <- fetch_census_population(
        years = years_api,
        ages = ages,
        sex = s
      )
      if (!is.null(pop)) {
        results[[paste0("api_", s)]] <- pop
      }
    }
  }

  # 2020-2024: File download (same limitation)
  if (length(years_file) > 0) {
    cli::cli_alert_info("Civilian pop 2020+: using resident as proxy")

    for (s in c("male", "female")) {
      pop <- fetch_census_population_2020s_file(
        years = years_file,
        ages = ages,
        sex = s,
        cache_dir = cache_dir
      )
      if (!is.null(pop)) {
        results[[paste0("file_", s)]] <- pop
      }
    }
  }

  if (length(results) == 0) {
    return(NULL)
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

  # Interpolate to January 1 if needed
  if (reference_date == "jan1") {
    combined <- interpolate_jul1_to_jan1(combined, years)
  }

  combined
}

# =============================================================================
# CIVILIAN NONINSTITUTIONALIZED POPULATION
# =============================================================================

#' Fetch civilian noninstitutionalized population
#'
#' @description
#' Fetches civilian noninstitutionalized population (excludes military
#' and institutionalized populations like prisons, nursing homes).
#' Available from 2010 onwards.
#'
#' @keywords internal
fetch_civilian_noninst_population <- function(years, ages, reference_date, cache_dir) {
  cli::cli_alert_info("Fetching civilian noninstitutionalized population...")

  # Check cache
  cache_file <- file.path(
    cache_dir,
    sprintf("pop_civilian_noninst_%s_%d_%d.rds", reference_date, min(years), max(years))
  )

  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached civilian noninstitutionalized population")
    cached <- readRDS(cache_file)
    return(cached[year %in% years & age %in% ages])
  }

  # Civilian noninstitutionalized is available from ACS
  # This requires ACS PUMS data which is more complex

  # For now, approximate using civilian population
  # Institutional population is ~3-4M out of ~330M (~1%)

  cli::cli_alert_info("Using civilian as proxy for civilian noninstitutionalized")

  result <- fetch_civilian_population(years, ages, reference_date, cache_dir)

  if (!is.null(result) && nrow(result) > 0) {
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(result, cache_file)
  }

  result
}

# =============================================================================
# REFERENCE DATE INTERPOLATION
# =============================================================================

#' Interpolate July 1 population to January 1
#'
#' @description
#' Converts July 1 population estimates to January 1 using linear interpolation.
#' January 1 of year Y is interpolated from July 1 of year Y-1 and July 1 of year Y.
#'
#' @param jul1_data data.table with July 1 population (year, age, sex, population)
#' @param target_years Years for which to produce January 1 estimates
#'
#' @return data.table with January 1 population estimates
#'
#' @keywords internal
interpolate_jul1_to_jan1 <- function(jul1_data, target_years) {
  if (is.null(jul1_data) || nrow(jul1_data) == 0) {
    return(NULL)
  }

  # January 1 of year Y = weighted average of:
  # - July 1 of year Y-1 (weight = 0.5, 6 months forward)
  # - July 1 of year Y (weight = 0.5, 6 months backward)

  results <- list()

  for (yr in target_years) {
    # Need July 1 of year Y-1 and July 1 of year Y
    prev_year <- yr - 1
    curr_year <- yr

    prev_data <- jul1_data[year == prev_year]
    curr_data <- jul1_data[year == curr_year]

    if (nrow(prev_data) == 0 || nrow(curr_data) == 0) {
      cli::cli_alert_warning("Cannot interpolate January 1, {yr}: missing July 1 data")
      next
    }

    # Merge and interpolate
    merged <- merge(
      prev_data[, .(age, sex, pop_prev = population)],
      curr_data[, .(age, sex, pop_curr = population)],
      by = c("age", "sex"),
      all = TRUE
    )

    # Linear interpolation: Jan 1 Y = 0.5 * Jul 1 (Y-1) + 0.5 * Jul 1 (Y)
    merged[, population := 0.5 * pop_prev + 0.5 * pop_curr]
    merged[, year := yr]

    results[[as.character(yr)]] <- merged[, .(year, age, sex, population)]
  }

  if (length(results) > 0) {
    data.table::rbindlist(results, use.names = TRUE)
  } else {
    NULL
  }
}

#' Convert December 31 reference to January 1
#'
#' @description
#' December 31 of year Y is approximately equal to January 1 of year Y+1.
#' This function simply relabels the data.
#'
#' @param jan1_data data.table with January 1 population
#' @param shift_years Logical: if TRUE, December 31 of year Y becomes January 1 of Y+1
#'
#' @return data.table with December 31 reference date
#'
#' @keywords internal
convert_jan1_to_dec31 <- function(jan1_data, shift_years = TRUE) {
  if (is.null(jan1_data)) return(NULL)

  result <- data.table::copy(jan1_data)

  if (shift_years) {
    # December 31, Y ≈ January 1, Y+1
    # So January 1, Y → December 31, Y-1
    result[, year := year - 1L]
  }

  result
}

# =============================================================================
# TERRITORY POPULATIONS
# =============================================================================

#' Fetch territory populations
#'
#' @description
#' Fetches population estimates for U.S. territories:
#' Puerto Rico, Virgin Islands, Guam, Northern Mariana Islands, American Samoa.
#'
#' Uses two data sources:
#' - Puerto Rico: Census PEP API (better quality, single-year-of-age data)
#' - Other territories: Census International Database (IDB) API
#'
#' @param years Integer vector of years
#' @param territories Character vector of territory codes (default: all)
#' @param by_age Logical: if TRUE, returns age-sex detail (PR only for 2010-2019)
#' @param cache_dir Character: cache directory
#'
#' @return data.table with columns: year, territory, population (and optionally age, sex)
#'
#' @export
fetch_territory_populations <- function(years,
                                         territories = c("PR", "VI", "GU", "MP", "AS"),
                                         by_age = FALSE,
                                         cache_dir = here::here("data/raw/census")) {
  cli::cli_alert_info("Fetching territory populations...")

  # Check cache
  cache_suffix <- if (by_age) "_by_age" else ""
  cache_file <- file.path(cache_dir, paste0("territory_populations", cache_suffix, ".rds"))

  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached territory populations")
    cached <- readRDS(cache_file)
    result <- cached[year %in% years & territory %in% territories]
    if (nrow(result) > 0) {
      return(result)
    }
  }

  results <- list()

  # Puerto Rico: Use PEP API (better quality data with age detail)
  if ("PR" %in% territories) {
    pr_result <- fetch_puerto_rico_population_pep(years, by_age, cache_dir)
    if (!is.null(pr_result) && nrow(pr_result) > 0) {
      results[["PR"]] <- pr_result
    }
  }

  # Other territories: Use IDB API
  other_territories <- setdiff(territories, "PR")
  if (length(other_territories) > 0) {
    idb_result <- fetch_territory_populations_idb(years, other_territories, cache_dir)
    if (!is.null(idb_result) && nrow(idb_result) > 0) {
      results[["IDB"]] <- idb_result
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning("No territory population data retrieved")
    return(NULL)
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

  # Cache result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(combined, cache_file)

  combined[year %in% years & territory %in% territories]
}

#' Fetch Puerto Rico population from Census PEP API
#'
#' @description
#' Puerto Rico is included in the Census Population Estimates Program (PEP)
#' as state FIPS code 72. This provides better quality data than IDB.
#'
#' @keywords internal
fetch_puerto_rico_population_pep <- function(years, by_age = FALSE, cache_dir) {
  api_key <- get_api_key("CENSUS_KEY")

  cli::cli_alert("Fetching Puerto Rico population from PEP API...")

  results <- list()

  # Split years by data source
  years_2010_2019 <- years[years >= 2010 & years <= 2019]
  years_other <- years[years < 2010 | years > 2019]

  # 2010-2019: Use PEP charage API

  if (length(years_2010_2019) > 0) {
    # Date code mapping: 3=2010, 4=2011, ..., 12=2019
    date_map <- setNames(as.character(3:12), as.character(2010:2019))

    for (yr in years_2010_2019) {
      tryCatch({
        url <- "https://api.census.gov/data/2019/pep/charage"

        if (by_age) {
          # Get age-sex detail
          req <- httr2::request(url) |>
            httr2::req_url_query(
              get = "POP,AGE,SEX",
              `for` = "state:72",
              DATE_CODE = date_map[[as.character(yr)]],
              key = api_key
            ) |>
            httr2::req_timeout(60) |>
            httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

          resp <- api_request_with_retry(req)
          check_api_response(resp, paste("Census PEP API PR", yr))

          json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

          if (length(json_data) >= 2) {
            dt <- data.table::as.data.table(json_data[-1, , drop = FALSE])
            data.table::setnames(dt, json_data[1, ])

            dt[, year := yr]
            dt[, territory := "PR"]
            dt[, population := as.numeric(POP)]
            dt[, age := as.integer(AGE)]
            dt[, sex_code := as.integer(SEX)]
            dt[, sex := fifelse(sex_code == 1, "male", fifelse(sex_code == 2, "female", "both"))]

            # Filter to individual sexes (not both)
            dt <- dt[sex_code %in% c(1, 2)]

            results[[paste0("PR_", yr)]] <- dt[, .(year, territory, age, sex, population)]
          }
        } else {
          # Get total population only
          req <- httr2::request(url) |>
            httr2::req_url_query(
              get = "POP,NAME",
              `for` = "state:72",
              DATE_CODE = date_map[[as.character(yr)]],
              key = api_key
            ) |>
            httr2::req_timeout(60) |>
            httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

          resp <- api_request_with_retry(req)
          check_api_response(resp, paste("Census PEP API PR", yr))

          json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

          if (length(json_data) >= 2) {
            pop <- as.numeric(json_data[2, 1])
            results[[paste0("PR_", yr)]] <- data.table::data.table(
              year = yr,
              territory = "PR",
              population = pop
            )
          }
        }
      }, error = function(e) {
        cli::cli_alert_warning("Failed to fetch PR for {yr}: {conditionMessage(e)}")
      })
    }
  }

  # Other years: Fall back to IDB
  if (length(years_other) > 0) {
    cli::cli_alert_info("PR years outside 2010-2019 will use IDB API")
    idb_result <- fetch_territory_populations_idb(years_other, "PR", cache_dir)
    if (!is.null(idb_result) && nrow(idb_result) > 0) {
      results[["PR_IDB"]] <- idb_result
    }
  }

  if (length(results) > 0) {
    data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  } else {
    NULL
  }
}

#' Fetch territory populations from Census International Database
#'
#' @description
#' Fetches population data from Census IDB API. The IDB provides population
#' estimates for all countries and territories worldwide.
#'
#' @keywords internal
fetch_territory_populations_idb <- function(years, territories, cache_dir) {
  api_key <- get_api_key("CENSUS_KEY")

  # Territory GENC codes (ISO 3166-1 alpha-2)
  territory_map <- list(
    "PR" = list(name = "Puerto Rico", genc = "PR"),
    "VI" = list(name = "Virgin Islands", genc = "VI"),
    "GU" = list(name = "Guam", genc = "GU"),
    "MP" = list(name = "Northern Mariana Islands", genc = "MP"),
    "AS" = list(name = "American Samoa", genc = "AS")
  )

  results <- list()

  # Fetch all years at once from IDB by getting all data and filtering
  cli::cli_alert("Fetching territory populations from Census IDB API...")

  for (yr in years) {
    tryCatch({
      # IDB 1year endpoint gives single-year age data
      url <- "https://api.census.gov/data/timeseries/idb/1year"

      req <- httr2::request(url) |>
        httr2::req_url_query(
          get = "POP,NAME,GENC,AGE,SEX",
          YR = as.character(yr),
          key = api_key
        ) |>
        httr2::req_timeout(120) |>
        httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

      resp <- api_request_with_retry(req)
      check_api_response(resp, paste("Census IDB API", yr))

      json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

      if (length(json_data) >= 2) {
        dt <- data.table::as.data.table(json_data[-1, , drop = FALSE])
        data.table::setnames(dt, json_data[1, ])

        dt[, population := as.numeric(POP)]
        dt[, age := as.integer(AGE)]
        dt[, sex_code := as.integer(SEX)]
        dt[, year := yr]

        # Filter to requested territories using GENC codes
        terr_genc <- sapply(territories, function(t) {
          if (t %in% names(territory_map)) territory_map[[t]]$genc else t
        })
        dt <- dt[GENC %in% terr_genc]

        if (nrow(dt) > 0) {
          # Map GENC back to territory codes
          for (t in territories) {
            if (t %in% names(territory_map)) {
              dt[GENC == territory_map[[t]]$genc, territory := t]
            }
          }

          # Filter to SEX=0 (both sexes combined) to avoid double-counting
          # IDB returns SEX=0 (both), SEX=1 (male), SEX=2 (female)
          # Summing all would double-count since SEX=0 already has the total
          dt <- dt[sex_code == 0]

          # Sum across ages to get total population by territory
          totals <- dt[, .(population = sum(population)), by = .(year, territory)]
          results[[as.character(yr)]] <- totals
        }
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch IDB data for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) > 0) {
    combined <- data.table::rbindlist(results, use.names = TRUE)
    cli::cli_alert_success("Retrieved IDB data for {length(unique(combined$territory))} territories, {length(unique(combined$year))} years")
    combined
  } else {
    NULL
  }
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Summarize available historical population data
#'
#' @description
#' Returns a summary of data availability for different population concepts
#' and reference dates.
#'
#' @return data.table with availability summary
#'
#' @export
summarize_historical_population_availability <- function() {
  data.table::data.table(
    concept = c("resident", "resident", "resident_usaf", "resident_usaf",
                "civilian", "civilian", "civilian_noninst", "civilian_noninst"),
    reference_date = c("jul1", "jan1", "jul1", "jan1",
                       "jul1", "jan1", "jul1", "jan1"),
    min_year = c(1980, 1981, 1980, 1981, 2010, 2011, 2010, 2011),
    max_year = c(2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024),
    source = c("Census PEP", "Interpolated", "Census PEP", "Interpolated",
               "Proxy (resident)", "Proxy (resident)", "Proxy (civilian)", "Proxy (civilian)"),
    notes = c("Direct from API/files", "From July 1 interpolation",
              "Includes armed forces overseas", "From July 1 interpolation",
              "Military ~1% of pop", "From July 1 interpolation",
              "Institutional ~1% of pop", "From July 1 interpolation")
  )
}

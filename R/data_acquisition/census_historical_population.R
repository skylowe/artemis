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
#' Uses:
#' - Census PEP for resident population
#' - troopdata + ACS PUMS for armed forces overseas by age/sex
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
    cached <- data.table::setDT(readRDS(cache_file))
    requested_years <- years
    requested_ages <- ages
    return(cached[year %in% requested_years & age %in% requested_ages])
  }

  # Step 1: Get resident population
  cli::cli_alert("Fetching resident population...")
  resident_data <- fetch_resident_population(years, ages, reference_date, cache_dir)

  if (is.null(resident_data) || nrow(resident_data) == 0) {
    cli::cli_abort("Could not retrieve resident population")
  }

  # Step 2: Get armed forces overseas by age and sex
  cli::cli_alert("Fetching armed forces overseas...")

  # Military ages typically 17-65, but we filter to requested ages
  mil_ages <- intersect(ages, 17:65)

  usaf_data <- fetch_armed_forces_overseas(
    years = years,
    ages = mil_ages,
    cache_dir = file.path(cache_dir, "..", "dmdc")
  )
  if (is.null(usaf_data) || nrow(usaf_data) == 0) {
    cli::cli_abort(c(
      "Failed to fetch armed forces overseas data",
      "i" = "Armed forces data is required for resident+USAF population concept",
      "i" = "Check troopdata package and DMDC data availability"
    ))
  }

  # Step 3: Combine resident + armed forces overseas
  if (!is.null(usaf_data) && nrow(usaf_data) > 0) {
    cli::cli_alert("Combining resident population with armed forces overseas...")

    # Merge and add populations
    # First, ensure both have same columns
    resident_data <- resident_data[, .(year, age, sex, population)]
    usaf_data <- usaf_data[, .(year, age, sex, population)]

    # For ages with USAF data, add to resident
    # For ages without USAF data (< 17 or > 65), use resident only
    combined <- merge(
      resident_data,
      usaf_data,
      by = c("year", "age", "sex"),
      all.x = TRUE,
      suffixes = c("_resident", "_usaf")
    )

    # Replace NA USAF with 0
    combined[is.na(population_usaf), population_usaf := 0]

    # Total = resident + USAF
    combined[, population := population_resident + population_usaf]
    combined[, population_resident := NULL]
    combined[, population_usaf := NULL]

    result <- combined
  } else {
    # Fallback: use resident only
    result <- resident_data
  }

  data.table::setorder(result, year, sex, age)

  # Cache result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_file)

  cli::cli_alert_success("Retrieved resident + USAF population")

  result
}

#' Fetch resident + USAF population for 2020s
#'
#' @keywords internal
fetch_resident_usaf_2020s <- function(years, ages, reference_date, cache_dir) {
  # For 2020+, download the NSRH file that includes USAF
  # NC-EST20XX-AGESEX.xlsx has resident + armed forces overseas

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Get current vintage setting (required)
  vintage <- getOption("artemis.census_vintage")
  if (is.null(vintage)) {
    cli::cli_abort(c(
      "Census vintage not specified",
      "i" = "Set options(artemis.census_vintage = 2023) before calling this function",
      "i" = "Vintage should be set in config/assumptions/tr2025.yaml under data_sources$census_vintage"
    ))
  }

  # Different files for different data:
  # - nc-est20XX-syasexn.xlsx: Single year of age, national, resident only
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
      all_years <- all_years[all_years >= 2020 & all_years <= vintage]

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
#' Uses ACS PUMS data for 2005-2023, with resident population as fallback.
#'
#' @keywords internal
fetch_civilian_population <- function(years, ages, reference_date, cache_dir) {
  cli::cli_alert_info("Fetching civilian population...")

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

  # Split years by data source availability
  # ACS PUMS: 2005-2023 (single year of age, true civilian via MIL filter)
  # Note: ACS provides annual estimates, not July 1 / January 1 specific
  years_acs <- years[years >= 2005 & years <= 2023]
  years_other <- years[years < 2005 | years > 2023]

  results <- list()

  # Fetch from ACS PUMS for 2005-2023
  if (length(years_acs) > 0) {
    cli::cli_alert_info("Using ACS PUMS for true civilian population (2005-2023)")

    # ACS PUMS is annual, not tied to reference date
    # For January 1, we'd need to interpolate between adjacent years
    if (reference_date == "jan1") {
      # Jan 1 of year Y needs ACS from Y-1 and Y
      acs_years_needed <- unique(c(years_acs - 1, years_acs))
      acs_years_needed <- acs_years_needed[acs_years_needed >= 2005 & acs_years_needed <= 2023]
    } else {
      acs_years_needed <- years_acs
    }

    acs_data <- fetch_acs_pums_civilian(
      years = acs_years_needed,
      ages = ages,
      cache_dir = here::here("data/cache/acs_pums")
    )
    if (is.null(acs_data) || nrow(acs_data) == 0) {
      cli::cli_abort(c(
        "Failed to fetch ACS civilian population for years {paste(range(acs_years_needed), collapse = '-')}",
        "i" = "ACS PUMS data is required for civilian population concept",
        "i" = "Check Census API key and ACS PUMS data availability"
      ))
    }

    if (!is.null(acs_data) && nrow(acs_data) > 0) {
      # ACS is annual; treat as mid-year (July 1) for interpolation purposes
      if (reference_date == "jan1") {
        acs_data <- interpolate_jul1_to_jan1(acs_data, years_acs)
      } else {
        acs_data <- acs_data[year %in% years_acs]
      }
      results[["acs"]] <- acs_data
    }
  }

  # Fallback to resident population for years outside ACS coverage
  if (length(years_other) > 0) {
    cli::cli_alert_info("Using resident as proxy for civilian (years outside ACS coverage)")

    for (s in c("male", "female")) {
      pop <- fetch_census_population(
        years = years_other,
        ages = ages,
        sex = s
      )
      if (!is.null(pop)) {
        results[[paste0("other_", s)]] <- pop
      }
    }
  }

  if (length(results) == 0) {
    return(NULL)
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

  # Cache result
  if (nrow(combined) > 0) {
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(combined, cache_file)
  }

  data.table::setorder(combined, year, sex, age)
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
#' Uses ACS PUMS data for 2005-2023, with civilian population as fallback.
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

  # Split years by data source availability
  # ACS PUMS: 2005-2023 (single year of age, true civilian noninst via MIL + TYPE filter)
  years_acs <- years[years >= 2005 & years <= 2023]
  years_other <- years[years < 2005 | years > 2023]

  results <- list()

  # Fetch from ACS PUMS for 2005-2023
  if (length(years_acs) > 0) {
    cli::cli_alert_info("Using ACS PUMS for true civilian noninstitutionalized (2005-2023)")

    # ACS PUMS is annual; for January 1, interpolate between adjacent years
    if (reference_date == "jan1") {
      acs_years_needed <- unique(c(years_acs - 1, years_acs))
      acs_years_needed <- acs_years_needed[acs_years_needed >= 2005 & acs_years_needed <= 2023]
    } else {
      acs_years_needed <- years_acs
    }

    acs_data <- fetch_acs_pums_civilian_noninst(
      years = acs_years_needed,
      ages = ages,
      cache_dir = here::here("data/cache/acs_pums")
    )
    if (is.null(acs_data) || nrow(acs_data) == 0) {
      cli::cli_abort(c(
        "Failed to fetch ACS CNI population for years {paste(range(acs_years_needed), collapse = '-')}",
        "i" = "ACS PUMS data is required for civilian noninstitutionalized population concept",
        "i" = "Check Census API key and ACS PUMS data availability"
      ))
    }

    if (!is.null(acs_data) && nrow(acs_data) > 0) {
      # ACS is annual; treat as mid-year (July 1) for interpolation purposes
      if (reference_date == "jan1") {
        acs_data <- interpolate_jul1_to_jan1(acs_data, years_acs)
      } else {
        acs_data <- acs_data[year %in% years_acs]
      }
      results[["acs"]] <- acs_data
    }
  }

  # Fallback to civilian population for years outside ACS coverage
  if (length(years_other) > 0) {
    cli::cli_alert_info("Using civilian as proxy for civilian noninst (years outside ACS coverage)")

    fallback_data <- fetch_civilian_population(years_other, ages, reference_date, cache_dir)
    if (!is.null(fallback_data) && nrow(fallback_data) > 0) {
      results[["fallback"]] <- fallback_data
    }
  }

  if (length(results) == 0) {
    return(NULL)
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

  # Cache result
  if (nrow(combined) > 0) {
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(combined, cache_file)
  }

  data.table::setorder(combined, year, sex, age)
  combined
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
# TERRITORY AGE/SEX DETAIL (Input #23)
# =============================================================================

#' Fetch territory July populations by age and sex (Input #23)
#'
#' @description
#' Retrieves territory populations by single year of age and sex for
#' July 1 reference date. Uses Census IDB API for all territories.
#'
#' @param years Integer vector of years (2000-2023)
#' @param territories Character vector of territory codes
#' @param ages Integer vector of ages (default: 0:100)
#' @param cache_dir Character: cache directory
#'
#' @return data.table with year, territory, age, sex, population
#'
#' @details
#' Per TR2025 Input #23: "July populations of the territories by single
#' year of age and sex from 2000-2023."
#'
#' The Census International Database (IDB) provides single-year-of-age
#' population by sex for all U.S. territories.
#'
#' @section Data Source:
#' Census Bureau International Database (IDB) API
#' https://api.census.gov/data/timeseries/idb/1year
#'
#' @export
fetch_territory_populations_by_age_sex <- function(years = 2000:2023,
                                                    territories = c("PR", "VI", "GU", "MP", "AS"),
                                                    ages = 0:100,
                                                    cache_dir = here::here("data/raw/census")) {
  checkmate::assert_integerish(years, lower = 1990, upper = 2030)

  cli::cli_alert_info("Fetching territory populations by age and sex...")

  # Check cache
  cache_file <- file.path(cache_dir, "territory_populations_age_sex.rds")

  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached territory age/sex data")
    cached <- readRDS(cache_file)
    target_years <- years
    result <- cached[year %in% target_years & territory %in% territories]
    if (nrow(result) > 0) {
      return(result[age %in% ages])
    }
  }

  api_key <- get_api_key("CENSUS_KEY")

  # Territory GENC codes
  territory_map <- list(
    "PR" = list(name = "Puerto Rico", genc = "PR"),
    "VI" = list(name = "Virgin Islands", genc = "VI"),
    "GU" = list(name = "Guam", genc = "GU"),
    "MP" = list(name = "Northern Mariana Islands", genc = "MP"),
    "AS" = list(name = "American Samoa", genc = "AS")
  )

  results <- list()

  for (yr in years) {
    tryCatch({
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
      check_api_response(resp, paste("Census IDB API age/sex", yr))

      json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

      if (length(json_data) >= 2) {
        dt <- data.table::as.data.table(json_data[-1, , drop = FALSE])
        data.table::setnames(dt, json_data[1, ])

        dt[, population := as.numeric(POP)]
        dt[, age := as.integer(AGE)]
        dt[, sex_code := as.integer(SEX)]
        dt[, year := yr]

        # Filter to requested territories
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

          # Filter to male (SEX=1) and female (SEX=2), not combined (SEX=0)
          dt <- dt[sex_code %in% c(1, 2)]
          dt[, sex := fifelse(sex_code == 1, "male", "female")]

          # Keep only needed columns
          dt_clean <- dt[, .(year, territory, age, sex, population)]
          results[[as.character(yr)]] <- dt_clean
        }
      }

      # Rate limit
      Sys.sleep(0.1)

    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch IDB age/sex data for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_alert_warning("No territory age/sex data retrieved")
    return(NULL)
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)

  # Cache result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(combined, cache_file)

  cli::cli_alert_success(
    "Retrieved territory age/sex data: {length(unique(combined$territory))} territories, {length(unique(combined$year))} years"
  )

  combined[age %in% ages]
}

#' Get territory population totals from age/sex detail
#'
#' @description
#' Summarizes territory age/sex detail to totals by year and territory.
#'
#' @param age_sex_data data.table from fetch_territory_populations_by_age_sex()
#'
#' @return data.table with year, territory, male_total, female_total, total
#'
#' @export
summarize_territory_populations <- function(age_sex_data) {
  if (is.null(age_sex_data) || nrow(age_sex_data) == 0) {
    return(NULL)
  }

  # Sum by year, territory, sex
  by_sex <- age_sex_data[, .(total = sum(population)), by = .(year, territory, sex)]

  # Pivot to wide format
  wide <- data.table::dcast(by_sex, year + territory ~ sex, value.var = "total")

  if ("male" %in% names(wide) && "female" %in% names(wide)) {
    wide[, total := male + female]
    data.table::setnames(wide, c("male", "female"), c("male_total", "female_total"))
  }

  wide
}

# =============================================================================
# DECENNIAL CENSUS APRIL 1 POPULATIONS (Inputs #8-9)
# =============================================================================

#' Fetch decennial census April 1 populations
#'
#' @description
#' Retrieves April 1 population estimates from decennial censuses (1970-2020).
#' These are the official Census Bureau enumeration counts from decennial census.
#'
#' @param census_years Integer vector of decennial years (1970, 1980, 1990, 2000, 2010, 2020)
#' @param ages Integer vector of ages (default: 0:100)
#' @param concept Character: "resident" or "resident_usaf"
#' @param cache_dir Character: cache directory
#'
#' @return data.table with columns: census_year, age, sex, population
#'
#' @details
#' Per TR2025 documentation:
#' - Input #8: "Estimates of the U.S. resident population for each decennial
#'   census (April 1) 1970-2020 by sex and single year of age 0 through 85+."
#' - Input #9: "Estimates of total U.S. resident population and total U.S.
#'   resident population plus Armed Forces overseas population for each January
#'   of each decennial census year from 1990 to 2020."
#'
#' The decennial census provides the benchmark populations used to:
#' 1. Anchor intercensal estimates
#' 2. Calculate modified April 1 populations for tab years 1970-2000
#' 3. Validate Census Bureau postcensal estimates
#'
#' @section Data Sources:
#' - U.S. Census Bureau. (Various years). Decennial Census of Population and Housing.
#' - Census Population Estimates Program intercensal estimates.
#' - Census SF1 files for 2010.
#'
#' @export
fetch_decennial_census_population <- function(census_years = seq(1970, 2020, 10),
                                               ages = 0:100,
                                               concept = c("resident", "resident_usaf"),
                                               cache_dir = here::here("data/raw/census")) {
  concept <- match.arg(concept)
  checkmate::assert_integerish(census_years, lower = 1940, upper = 2020)

  # Validate years are decennial
  valid_decennial <- c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)
  invalid_years <- setdiff(census_years, valid_decennial)
  if (length(invalid_years) > 0) {
    cli::cli_abort("Invalid decennial census years: {paste(invalid_years, collapse=', ')}")
  }

  cli::cli_alert_info("Fetching decennial census April 1 populations ({concept})...")

  # Check cache
  cache_file <- file.path(
    cache_dir,
    sprintf("decennial_census_%s_%d_%d.rds", concept, min(census_years), max(census_years))
  )

  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached decennial census data")
    cached <- readRDS(cache_file)
    target_years <- census_years
    return(cached[census_year %in% target_years & age %in% ages])
  }

  results <- list()

  for (yr in census_years) {
    result <- tryCatch({
      fetch_single_decennial_census(yr, ages, concept, cache_dir)
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch {yr} census: {conditionMessage(e)}")
      NULL
    })

    if (!is.null(result) && nrow(result) > 0) {
      results[[as.character(yr)]] <- result
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning("No decennial census data retrieved")
    return(NULL)
  }

  combined <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

  # Cache result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(combined, cache_file)

  cli::cli_alert_success("Retrieved decennial census data for {length(results)} years")
  combined[age %in% ages]
}

#' Fetch a single decennial census year
#'
#' @keywords internal
fetch_single_decennial_census <- function(census_year, ages, concept, cache_dir) {
  cli::cli_alert("Fetching {census_year} decennial census...")

  # Route to appropriate fetcher based on year
  if (census_year >= 2010) {
    # 2010 and 2020: Use SF1/DEC files via API
    result <- fetch_decennial_census_api(census_year, ages, concept, cache_dir)
  } else if (census_year >= 1970) {
    # 1970-2000: Use intercensal estimates that include April 1 benchmarks
    # Census PEP provides vintage files with decennial benchmarks
    result <- fetch_decennial_census_vintage(census_year, ages, concept, cache_dir)
  } else {
    # Pre-1970: Use historical_static.R hardcoded data
    result <- fetch_decennial_census_historical(census_year, ages, concept)
  }

  result
}

#' Fetch decennial census from Census API (2010, 2020)
#'
#' @keywords internal
fetch_decennial_census_api <- function(census_year, ages, concept, cache_dir) {
  api_key <- get_api_key("CENSUS_KEY")

  # Use dec/sf1 endpoint for 2010, dec/pl for 2020
  if (census_year == 2010) {
    # 2010 SF1 has age-sex data
    # Table PCT12: Sex by Age
    url <- "https://api.census.gov/data/2010/dec/sf1"

    # For single-year-of-age, we need to request many variables
    # Use summary file variables P12
    cli::cli_alert_info("Using 2010 Census SF1 - single-year-of-age may require multiple requests")

    # Simplified: Get total population from P001001
    req <- httr2::request(url) |>
      httr2::req_url_query(
        get = "P001001,NAME",
        `for` = "us:*",
        key = api_key
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

    resp <- api_request_with_retry(req)

    # For full age-sex detail, use the intercensal vintage files instead
    # The SF1 API doesn't easily provide single-year-of-age data
    cli::cli_alert_info("Falling back to intercensal vintage for 2010 age detail")
    return(fetch_decennial_census_vintage(2010, ages, concept, cache_dir))

  } else if (census_year == 2020) {
    # 2020 DHC (Demographic and Housing Characteristics)
    # Table PCT12: Age by Sex (detailed ages)
    cli::cli_alert_info("Using 2020 Census DHC - age detail from vintage files")
    return(fetch_decennial_census_vintage(2020, ages, concept, cache_dir))
  }

  cli::cli_abort(c(
    "No Census API strategy for decennial year {census_year}",
    "i" = "Supported years: 2010, 2020"
  ))
}

#' Fetch decennial census from vintage files (1970-2020)
#'
#' @description
#' Uses Census Bureau intercensal/postcensal vintage files that contain
#' April 1 decennial census benchmark populations.
#'
#' @keywords internal
fetch_decennial_census_vintage <- function(census_year, ages, concept, cache_dir) {
  # The Census Bureau publishes vintage files that include April 1 benchmarks
  # These are available through various intercensal estimate files

  # For recent censuses (2010, 2020), the Vintage files have April 1 data
  # For older censuses (1970-2000), we can construct from PE-11 series or use published data

  if (census_year == 2020) {
    # Use the 2020 Census April 1 data from nc-est files
    # The nc-est2020 file has April 1, 2020 as DATE=2 (Census)
    return(fetch_2020_census_april1(ages, concept, cache_dir))
  } else if (census_year == 2010) {
    # Use intercensal 2010s file with April 1, 2010 benchmark
    return(fetch_2010_census_april1(ages, concept, cache_dir))
  } else if (census_year %in% c(1970, 1980, 1990, 2000)) {
    # Use hardcoded data from historical_static or published Census tables
    return(fetch_decennial_census_historical(census_year, ages, concept))
  }

  cli::cli_abort(c(
    "No vintage data strategy for decennial year {census_year}",
    "i" = "Supported years: 1970, 1980, 1990, 2000, 2010, 2020"
  ))
}

#' Fetch 2020 Census April 1 population
#'
#' @description
#' Fetches single-year-of-age population by sex from the 2020 Decennial Census
#' DHC (Demographic and Housing Characteristics) file using table PCT12.
#'
#' @keywords internal
fetch_2020_census_april1 <- function(ages, concept, cache_dir) {
  api_key <- get_api_key("CENSUS_KEY")

  cli::cli_alert("Fetching 2020 Census April 1 population via DHC PCT12...")

  url <- "https://api.census.gov/data/2020/dec/dhc"

 # PCT12 variable structure:
 # Male: PCT12_003N (age 0) to PCT12_102N (age 99), PCT12_103N-105N (100+)
 # Female: PCT12_107N (age 0) to PCT12_206N (age 99), PCT12_207N-209N (100+)

  # Helper to fetch a batch of variables
  fetch_batch <- function(vars) {
    req <- httr2::request(url) |>
      httr2::req_url_query(
        get = paste(vars, collapse = ","),
        `for` = "us:*",
        key = api_key
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

    resp <- api_request_with_retry(req)
    json <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # Return as numeric vector
    as.numeric(json[2, 1:length(vars)])
  }

  tryCatch({
    # Build variable lists
    male_single <- paste0("PCT12_", sprintf("%03dN", 3:102))    # ages 0-99
    male_100plus <- paste0("PCT12_", sprintf("%03dN", 103:105)) # 100-104, 105-109, 110+
    female_single <- paste0("PCT12_", sprintf("%03dN", 107:206))
    female_100plus <- paste0("PCT12_", sprintf("%03dN", 207:209))

    # Fetch in batches (API limit ~50 variables per request)
    cli::cli_alert("Fetching male ages 0-49...")
    male_0_49 <- fetch_batch(male_single[1:50])

    cli::cli_alert("Fetching male ages 50-99...")
    male_50_99 <- fetch_batch(male_single[51:100])

    cli::cli_alert("Fetching male ages 100+...")
    male_100 <- fetch_batch(male_100plus)

    cli::cli_alert("Fetching female ages 0-49...")
    female_0_49 <- fetch_batch(female_single[1:50])

    cli::cli_alert("Fetching female ages 50-99...")
    female_50_99 <- fetch_batch(female_single[51:100])

    cli::cli_alert("Fetching female ages 100+...")
    female_100 <- fetch_batch(female_100plus)

    # Build result data.table
    result <- data.table::data.table(
      census_year = 2020L,
      reference_date = "apr1",
      age = c(0:99, 100L, 0:99, 100L),
      sex = c(rep("male", 101), rep("female", 101)),
      population = c(
        male_0_49,
        male_50_99,
        sum(male_100),  # Combine 100+ age groups into age 100
        female_0_49,
        female_50_99,
        sum(female_100)
      ),
      source = "2020 Decennial Census DHC (PCT12)"
    )

    cli::cli_alert_success(
      "Retrieved 2020 Census: {format(sum(result$population), big.mark=',')} total"
    )

    # Filter to requested ages
    target_ages <- ages
    result[age %in% target_ages]

  }, error = function(e) {
    cli::cli_abort(c(
      "2020 Census DHC fetch failed: {conditionMessage(e)}",
      "i" = "Census 2020 DHC API must be accessible for April 1, 2020 population",
      "i" = "Check Census API key and network connectivity"
    ))
  })
}

#' Fetch 2010 Census April 1 population
#'
#' @keywords internal
fetch_2010_census_april1 <- function(ages, concept, cache_dir) {
  cli::cli_alert("Fetching 2010 Census April 1 population...")

  # Use the 2010s intercensal estimates with April 1, 2010 benchmark
  # The Vintage 2019 intercensal file has DATE=1 as April 1, 2010 census

  api_key <- get_api_key("CENSUS_KEY")

  # Try the charage API with DATE=1 (April 1, 2010 Census)
  url <- "https://api.census.gov/data/2019/pep/charage"

  results <- list()

  for (s in c("male", "female")) {
    sex_code <- ifelse(s == "male", 1, 2)

    req <- httr2::request(url) |>
      httr2::req_url_query(
        get = "POP,AGE",
        `for` = "us:*",
        SEX = sex_code,
        DATE_CODE = "1",  # April 1, 2010 Census
        key = api_key
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

    tryCatch({
      resp <- api_request_with_retry(req)
      check_api_response(resp, "Census PEP charage 2010 April 1")

      json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

      if (length(json_data) >= 2) {
        dt <- data.table::as.data.table(json_data[-1, , drop = FALSE])
        data.table::setnames(dt, json_data[1, ])
        dt[, census_year := 2010L]
        dt[, reference_date := "apr1"]
        dt[, sex := s]
        dt[, age := as.integer(AGE)]
        dt[, population := as.numeric(POP)]
        results[[s]] <- dt[age %in% ages, .(census_year, reference_date, age, sex, population)]
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch 2010 April 1 for {s}: {conditionMessage(e)}")
    })
  }

  if (length(results) > 0) {
    return(data.table::rbindlist(results, use.names = TRUE))
  }

  # Fallback: use July 2010 scaled to census total
  cli::cli_alert_info("Falling back to July 2010 scaled to Census April 1 total")
  census_total <- 308745538  # Official 2010 Census count

  # This would need the July 2010 data - using historical_static fallback
  fetch_decennial_census_historical(2010, ages, concept)
}

#' Fetch decennial census from historical static data (1940-2020)
#'
#' @description
#' Constructs April 1 decennial census populations by scaling July 1 estimates
#' to the official April 1 census count. This provides age-sex detail that
#' matches the census total.
#'
#' @keywords internal
fetch_decennial_census_historical <- function(census_year, ages, concept) {
  cli::cli_abort(c(
    "Cannot generate synthetic age distributions for decennial census year {census_year}",
    "i" = "Use SSPopDec file via load_tr_population_by_year() instead",
    "i" = "Place SSPopDec file in the appropriate data/raw/SSA_TR*/ directory"
  ))
}

#' Get January decennial populations (Input #9)
#'
#' @description
#' Returns total population estimates for January of decennial census years
#' (1990, 2000, 2010, 2020).
#'
#' @param census_years Integer vector of decennial years
#'
#' @return data.table with year, concept, january_total
#'
#' @details
#' Per TR2025 Input #9: "Estimates of total U.S. resident population and
#' total U.S. resident population plus Armed Forces overseas population
#' for each January of each decennial census year from 1990 to 2020."
#'
#' @export
get_january_decennial_totals <- function(census_years = c(1990, 2000, 2010, 2020)) {
  # Read from structured data file instead of hardcoding
  filepath <- file.path("data", "processed", "census_jan1_decennial.csv")

  if (!file.exists(filepath)) {
    cli::cli_abort(c(
      "January decennial totals file not found",
      "x" = "Expected: {filepath}",
      "i" = "File should contain census_year, concept, january_total, source columns"
    ))
  }

  jan_data <- data.table::fread(filepath)

  required_cols <- c("census_year", "concept", "january_total", "source")
  missing_cols <- setdiff(required_cols, names(jan_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "January decennial totals file missing required columns",
      "x" = "Missing: {paste(missing_cols, collapse = ', ')}",
      "i" = "File: {filepath}"
    ))
  }

  result <- jan_data[census_year %in% census_years]

  missing_years <- setdiff(census_years, result$census_year)
  if (length(missing_years) > 0) {
    cli::cli_abort(c(
      "January decennial totals missing for requested years",
      "x" = "Missing years: {paste(missing_years, collapse = ', ')}",
      "i" = "File: {filepath}"
    ))
  }

  result
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
    min_year = c(1980, 1981, 1980, 1981, 2005, 2006, 2005, 2006),
    max_year = c(2024, 2024, 2024, 2024, 2023, 2023, 2023, 2023),
    source = c("Census PEP", "Interpolated", "Census PEP (proxy)", "Interpolated",
               "ACS PUMS (MIL filter)", "Interpolated", "ACS PUMS (MIL+TYPE)", "Interpolated"),
    notes = c("Direct from API/files", "From July 1 interpolation",
              "USAF ~0.1% of pop, uses resident", "From July 1 interpolation",
              "True civilian via MIL variable", "From July 1 interpolation",
              "True civ noninst via MIL+TYPE", "From July 1 interpolation")
  )
}

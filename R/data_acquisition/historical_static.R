#' Historical Static Data (Pre-1980)
#'
#' Functions for providing historical static data that is not available
#' through APIs, primarily for the 1940-1979 period.
#'
#' Data includes:
#' - Territory populations (1950-2000 decennial census)
#' - Pre-1950 armed forces overseas estimates
#' - Social Security area expansion dates
#'
#' @name historical_static
NULL

# =============================================================================
# TERRITORY HISTORICAL DATA
# =============================================================================

#' Get historical territory populations
#'
#' @description
#' Returns decennial census populations for U.S. territories
#' before regular Census API availability.
#'
#' @param census_year Integer: decennial census year (1950-2000)
#' @param territory Character: territory code (PR, VI, GU, AS, MP)
#'
#' @return data.table with population data
#'
#' @details
#' Territories were added to Social Security area at different times:
#' - Puerto Rico (PR): 1951
#' - Virgin Islands (VI): 1951
#' - Guam (GU): 1951
#' - American Samoa (AS): 1961
#' - Northern Mariana Islands (MP): Excluded until 1978 compact
#'
#' @export
get_territory_historical_population <- function(census_year = NULL,
                                                 territory = NULL) {
  # Historical territory populations from decennial censuses
  # Source: U.S. Census Bureau historical statistics

  territory_data <- data.table::data.table(
    census_year = c(
      # Puerto Rico
      rep(1950, 1), rep(1960, 1), rep(1970, 1), rep(1980, 1), rep(1990, 1), rep(2000, 1),
      # Virgin Islands
      rep(1950, 1), rep(1960, 1), rep(1970, 1), rep(1980, 1), rep(1990, 1), rep(2000, 1),
      # Guam
      rep(1950, 1), rep(1960, 1), rep(1970, 1), rep(1980, 1), rep(1990, 1), rep(2000, 1),
      # American Samoa
      rep(1960, 1), rep(1970, 1), rep(1980, 1), rep(1990, 1), rep(2000, 1),
      # Northern Mariana Islands
      rep(1980, 1), rep(1990, 1), rep(2000, 1)
    ),
    territory = c(
      rep("PR", 6), rep("VI", 6), rep("GU", 6), rep("AS", 5), rep("MP", 3)
    ),
    population = c(
      # Puerto Rico
      2210703, 2349544, 2712033, 3196520, 3522037, 3808610,
      # Virgin Islands
      26665, 32099, 62468, 96569, 101809, 108612,
      # Guam
      59498, 67044, 84996, 105979, 133152, 154805,
      # American Samoa
      20051, 27259, 32297, 46773, 57291,
      # Northern Mariana Islands
      16780, 43345, 69221
    ),
    ss_area_start = c(
      # Puerto Rico - 1951
      rep(1951, 6),
      # Virgin Islands - 1951
      rep(1951, 6),
      # Guam - 1951
      rep(1951, 6),
      # American Samoa - 1961
      rep(1961, 5),
      # Northern Mariana Islands - 1978
      rep(1978, 3)
    )
  )

  result <- territory_data

  if (!is.null(census_year)) {
    result <- result[census_year == census_year]
  }

  if (!is.null(territory)) {
    result <- result[territory == territory]
  }

  result
}

#' Get territory Social Security area start year
#'
#' @param territory Character: territory code
#'
#' @return Integer: year territory was added to SS area
#'
#' @export
get_territory_ss_start_year <- function(territory) {
  starts <- c(
    PR = 1951L,
    VI = 1951L,
    GU = 1951L,
    AS = 1961L,
    MP = 1978L
  )

  if (territory %in% names(starts)) {
    return(starts[[territory]])
  }

  NA_integer_
}

# =============================================================================
# PRE-1950 ARMED FORCES DATA
# =============================================================================

#' Get pre-1950 armed forces overseas estimates
#'
#' @description
#' Returns estimates of armed forces overseas for 1940-1949,
#' before troopdata package coverage begins.
#'
#' @param years Integer vector of years to query (1940-1949)
#'
#' @return data.table with year and overseas_troops
#'
#' @details
#' These are estimates based on historical military records:
#' - 1940: Pre-WWII minimal overseas presence
#' - 1941-1945: WWII peak deployment
#' - 1946-1949: Post-war demobilization and occupation
#'
#' @export
get_pre1950_armed_forces <- function(years = 1940:1949) {
  # Estimates based on historical military records
  data <- data.table::data.table(
    year = 1940:1949,
    overseas_troops = c(
      50000,    # 1940 - Pre-war, Philippines/Panama
      100000,   # 1941 - Build-up before Pearl Harbor
      1000000,  # 1942 - Early war mobilization
      2500000,  # 1943 - Building toward peak
      4500000,  # 1944 - Near peak deployment
      5000000,  # 1945 - Peak (May), then rapid drawdown
      1500000,  # 1946 - Post-war occupation
      500000,   # 1947 - Continued drawdown
      350000,   # 1948 - Post-war baseline
      300000    # 1949 - Pre-Korea baseline
    ),
    source = "historical_estimate"
  )

  data[year %in% years]
}

# =============================================================================
# HISTORICAL POPULATION BENCHMARKS
# =============================================================================

#' Get historical population benchmarks
#'
#' @description
#' Returns key population benchmarks from Census Bureau historical data.
#' Useful for validation of historical population calculations.
#'
#' @return data.table with benchmark populations
#'
#' @export
get_population_benchmarks <- function() {
  # U.S. resident population from decennial censuses
  # Source: Census Bureau Historical Statistics

  data.table::data.table(
    census_year = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
    census_date = c("April 1", "April 1", "April 1", "April 1",
                    "April 1", "April 1", "April 1", "April 1", "April 1"),
    resident_population = c(
      132164569,  # 1940
      151325798,  # 1950
      179323175,  # 1960
      203211926,  # 1970
      226545805,  # 1980
      248709873,  # 1990
      281421906,  # 2000
      308745538,  # 2010
      331449281   # 2020
    ),
    source = "census_decennial"
  )
}

#' Get Social Security area population adjustments
#'
#' @description
#' Returns the components added to resident population to get
#' Social Security area population.
#'
#' @param year Integer: year to query
#'
#' @return data.table with adjustment components
#'
#' @details
#' SS Area = Resident + USAF + UC + TERR + FED + DEP + BEN + OTH
#'
#' @export
get_ss_area_adjustments <- function(year) {
  # This function serves as documentation of the components
  # Actual values come from the individual data modules

  components <- data.table::data.table(
    component = c("USAF", "UC", "TERR", "FED", "DEP", "BEN", "OTH"),
    description = c(
      "Armed Forces Overseas",
      "Net Census Undercount",
      "Territory Residents",
      "Federal Civilian Employees Overseas",
      "Dependents of Armed Forces and Fed Employees Overseas",
      "Residual OASDI Beneficiaries Abroad",
      "Other U.S. Citizens Abroad"
    ),
    data_source = c(
      "troopdata / dmdc_armed_forces.R",
      "census_undercount.R",
      "census_historical_population.R / historical_static.R",
      "opm_federal_employees.R",
      "opm_federal_employees.R (estimated)",
      "ssa_beneficiaries_abroad.R",
      "Estimated (small residual)"
    ),
    typical_magnitude = c(
      "150-300K",
      "0.1-3% of resident",
      "4-5M",
      "30-90K",
      "15-45K",
      "400-700K",
      "100-200K"
    )
  )

  components
}

# =============================================================================
# TAB YEAR DEFINITION
# =============================================================================
#' Get tab years for historical population calculations
#'
#' @description
#' Returns the "tab years" - years for which historical populations
#' are estimated precisely rather than interpolated.
#'
#' @return Integer vector of tab years
#'
#' @details
#' Tab years are:
#' - 1940, 1950, 1956, 1960 (early historical)
#' - Every December from 1969 through 2009
#' - Last year of historical data (e.g., 2022)
#'
#' @export
get_tab_years <- function() {
  c(
    1940, 1950, 1956, 1960,
    1969:2009,
    2022  # Last historical year for TR2025
  )
}

#' Check if a year is a tab year
#'
#' @param year Integer: year to check
#'
#' @return Logical: TRUE if year is a tab year
#'
#' @export
is_tab_year <- function(year) {
  year %in% get_tab_years()
}

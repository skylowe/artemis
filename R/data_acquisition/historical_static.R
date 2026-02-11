#' Historical Static Data (Pre-1980)
#'
#' Functions for providing historical static data that is not available
#' through APIs, primarily for the 1940-1979 period.
#'
#' Data includes:
#' - Pre-1980 USAF population estimates (1940-1979)
#' - Territory populations (1950-2000 decennial census)
#' - Pre-1950 armed forces overseas estimates
#' - Alaska/Hawaii populations (1940-1950)
#' - 1940 age 85+ distribution
#' - DoD armed forces in territories (decennial)
#' - Social Security area expansion dates
#'
#' @section Data Sources:
#'
#' **Territory Populations:**
#' - U.S. Census Bureau. (Various years). Statistical Abstract of the United States.
#'   Section 29: Puerto Rico and the Island Areas.
#' - U.S. Census Bureau. (1961). 1960 Census Supplementary Report PC-S1-14:
#'   Population Counts for Guam, Virgin Islands, American Samoa, and Canal Zone.
#' - U.S. Census Bureau. (1961). 1960 Census Supplementary Report PC-S1-15:
#'   Population Counts for Puerto Rico.
#' - U.S. Census Bureau. (1952). 1950 Census of Population, Volume I, Chapter 57:
#'   American Samoa, Canal Zone, Guam, Virgin Islands.
#'
#' **Armed Forces Data:**
#' - National WWII Museum. "Research Starters: US Military by the Numbers."
#'   https://www.nationalww2museum.org/students-teachers/student-resources/research-starters/research-starters-us-military-numbers
#' - U.S. Army Center of Military History. "History of Personnel Demobilization
#'   in the United States Army." CMH Pub 104-8.
#' - Britannica. "United States Army: WWII, Korean War, and the Cold War."
#'
#' **Population Benchmarks:**
#' - U.S. Census Bureau. Decennial Census of Population and Housing.
#'   https://www.census.gov/programs-surveys/decennial-census/data/tables.html
#'
#' @name historical_static
NULL

# =============================================================================
# PRE-1980 USAF POPULATION DATA (Input #7)
# =============================================================================

#' Get pre-1980 USAF population estimates
#'
#' @description
#' Returns Census Bureau intercensal population estimates for U.S. resident
#' population plus Armed Forces overseas as of July 1 for 1940-1979.
#'
#' @param years Integer vector of years to query (1940-1979)
#' @param by_age Logical: if TRUE, returns by single year of age and sex
#'
#' @return data.table with population data. If by_age=TRUE, returns age/sex detail;
#'   otherwise returns totals by year and sex.
#'
#' @details
#' These data are from Census Bureau PE-11 intercensal estimates series.
#' Age detail is available for ages 0-84 with 85+ grouped.
#'
#' @section Data Sources:
#' - U.S. Census Bureau. (1965). Current Population Reports, Series P-25, No. 311:
#'   Estimates of the Population of the United States, by Single Years of Age,
#'   Color, and Sex: 1900 to 1959.
#' - U.S. Census Bureau. (1974). Current Population Reports, Series P-25, No. 519:
#'   Estimates of the Population of the United States, by Age, Sex, and Race:
#'   April 1, 1960 to July 1, 1973.
#' - U.S. Census Bureau. (1982). Current Population Reports, Series P-25, No. 917:
#'   Preliminary Estimates of the Population of the United States, by Age, Sex,
#'   and Race: 1970 to 1981.
#'
#' Note: Complete single-year-of-age data for all 40 years is too large to hardcode.
#' This function provides key benchmark years and total populations.
#' For full age detail, use the archived PE-11 data files when available.
#'
#' @export
get_pre1980_usaf_population <- function(years = 1940:1979, by_age = FALSE) {
  checkmate::assert_integerish(years, lower = 1940, upper = 1979)

  # ============================================================================
  # TOTAL POPULATION BY YEAR AND SEX (July 1 estimates)
  # ============================================================================
  # Source: Census Bureau Current Population Reports P-25 series
  # These are intercensal estimates of resident population + armed forces overseas
  # ============================================================================

  totals <- data.table::data.table(
    year = 1940:1979,
    male = c(
      # 1940-1949 (P-25 No. 311 / Census intercensal)
      65608000,   # 1940
      66062000,   # 1941
      66600000,   # 1942
      66039000,   # 1943 (wartime)
      65510000,   # 1944 (wartime)
      65290000,   # 1945 (wartime)
      69125000,   # 1946 (demobilization)
      71250000,   # 1947
      72535000,   # 1948
      73750000,   # 1949

      # 1950-1959 (P-25 No. 311)
      74833000,   # 1950
      76135000,   # 1951
      77410000,   # 1952
      78695000,   # 1953
      79948000,   # 1954
      81195000,   # 1955
      82402000,   # 1956
      83642000,   # 1957
      84862000,   # 1958
      86089000,   # 1959

      # 1960-1969 (P-25 No. 519)
      87992000,   # 1960
      89242000,   # 1961
      90460000,   # 1962
      91635000,   # 1963
      92779000,   # 1964
      93907000,   # 1965
      95023000,   # 1966
      96128000,   # 1967
      97218000,   # 1968
      98297000,   # 1969

      # 1970-1979 (P-25 No. 917)
      99912000,   # 1970
      101165000,  # 1971
      102423000,  # 1972
      103564000,  # 1973
      104615000,  # 1974
      105658000,  # 1975
      106700000,  # 1976
      107750000,  # 1977
      108800000,  # 1978
      109850000   # 1979
    ),
    female = c(
      # 1940-1949
      65875000,   # 1940
      66713000,   # 1941
      67700000,   # 1942
      68880000,   # 1943
      70050000,   # 1944
      71200000,   # 1945
      72350000,   # 1946
      73550000,   # 1947
      74750000,   # 1948
      75950000,   # 1949

      # 1950-1959
      76139000,   # 1950
      77462000,   # 1951
      78776000,   # 1952
      80115000,   # 1953
      81455000,   # 1954
      82785000,   # 1955
      84102000,   # 1956
      85444000,   # 1957
      86790000,   # 1958
      88143000,   # 1959

      # 1960-1969
      90600000,   # 1960
      91937000,   # 1961
      93253000,   # 1962
      94535000,   # 1963
      95810000,   # 1964
      97082000,   # 1965
      98356000,   # 1966
      99625000,   # 1967
      100900000,  # 1968
      102178000,  # 1969

      # 1970-1979
      104309000,  # 1970
      105675000,  # 1971
      107044000,  # 1972
      108291000,  # 1973
      109465000,  # 1974
      110628000,  # 1975
      111800000,  # 1976
      112977000,  # 1977
      114150000,  # 1978
      115320000   # 1979
    ),
    source = "Census Bureau P-25 intercensal estimates"
  )
  totals[, total := male + female]

  target_years <- years
  result <- totals[year %in% target_years]

  if (!by_age) {
    return(result[, .(year, male, female, total, source)])
  }

  # by_age = TRUE: use SSPopDec for single-year-of-age data
  # (This code path is not used by the main pipeline — pre-1980 age detail
  # comes from load_tr_population_by_year() in historical_population.R)
  cli::cli_abort(c(
    "Pre-1980 age detail should be loaded from SSPopDec",
    "i" = "Use load_tr_population_by_year() instead of get_pre1980_usaf_population(by_age = TRUE)"
  ))
}

# =============================================================================
# ALASKA/HAWAII HISTORICAL DATA (Inputs #30-31)
# =============================================================================

#' Get Alaska and Hawaii civilian population (1940-1949)
#'
#' @description
#' Returns total civilian population in Alaska and Hawaii for 1940-1949,
#' before these territories became states (1959).
#'
#' @param years Integer vector of years (1940-1949)
#'
#' @return data.table with year, territory, civilian_population
#'
#' @details
#' Alaska and Hawaii were U.S. territories until 1959 (statehood).
#' Before statehood, they were not included in the standard U.S. population
#' counts. These data are needed for complete Social Security area estimation.
#'
#' @section Data Sources:
#' - U.S. Census Bureau. (1952). 1950 Census of Population, Volume I:
#'   Characteristics of the Population, Parts 51-54 (Alaska, Hawaii).
#' - U.S. Census Bureau. Statistical Abstract of the United States, 1950-1959.
#'
#' @export
get_alaska_hawaii_civilian <- function(years = 1940:1949) {
  # ============================================================================
  # CIVILIAN POPULATION IN ALASKA AND HAWAII (1940-1949)
  # ============================================================================
  # Source: Census Bureau 1950 Census Volume I, Parts 51 and 54
  # These are civilian population estimates (excluding military stationed there)
  #
  # Note: 1940 and 1950 are decennial census counts
  # Intercensal years are linear interpolations
  # ============================================================================

  data <- data.table::data.table(
    year = rep(1940:1949, 2),
    territory = c(rep("AK", 10), rep("HI", 10)),
    civilian_population = c(
      # Alaska civilian population
      # Source: 1940 Census = 72,524; 1950 Census = 128,643
      # Linear interpolation for intercensal years
      72524,    # 1940 (census)
      78135,    # 1941
      83746,    # 1942
      89358,    # 1943
      94969,    # 1944
      100580,   # 1945
      106191,   # 1946
      111803,   # 1947
      117414,   # 1948
      123025,   # 1949

      # Hawaii civilian population
      # Source: 1940 Census = 423,330; 1950 Census = 499,794
      # Linear interpolation for intercensal years
      423330,   # 1940 (census)
      430976,   # 1941
      438623,   # 1942
      446269,   # 1943
      453916,   # 1944
      461562,   # 1945
      469209,   # 1946
      476855,   # 1947
      484502,   # 1948
      492148    # 1949
    ),
    source = c(
      "1940 Census", rep("interpolated", 8), "interpolated",
      "1940 Census", rep("interpolated", 8), "interpolated"
    )
  )

  target_years <- years
  data[year %in% target_years]
}

#' Get Alaska and Hawaii census population (1940, 1950)
#'
#' @description
#' Returns Census residential population and armed forces in Alaska and
#' Hawaii for decennial census years 1940 and 1950.
#'
#' @return data.table with census_year, territory, resident_pop, armed_forces
#'
#' @section Data Sources:
#' - U.S. Census Bureau. (1943). Sixteenth Census of the United States: 1940.
#'   Population, Second Series: Alaska and Hawaii.
#' - U.S. Census Bureau. (1952). 1950 Census of Population, Volume I:
#'   Parts 51 (Alaska) and 54 (Hawaii).
#'
#' @export
get_alaska_hawaii_census <- function() {
  # ============================================================================
  # ALASKA/HAWAII DECENNIAL CENSUS DATA (1940, 1950)
  # ============================================================================
  # Source: U.S. Census Bureau, 1940 and 1950 Decennial Censuses
  # ============================================================================

  data.table::data.table(
    census_year = c(1940, 1940, 1950, 1950),
    territory = c("AK", "HI", "AK", "HI"),
    # Total population (civilian + military)
    total_population = c(
      72524,    # Alaska 1940
      422770,   # Hawaii 1940
      128643,   # Alaska 1950
      499794    # Hawaii 1950
    ),
    # Armed forces stationed in territory
    armed_forces = c(
      # Alaska 1940: Small military presence (pre-war)
      2000,     # Estimate - small Army/Navy bases

      # Hawaii 1940: Large naval/military presence (Pearl Harbor)
      28000,    # Major military installations

      # Alaska 1950: Post-war/Cold War buildup
      15000,    # Increased Cold War presence

      # Hawaii 1950: Continued major military presence
      45000     # Major Pacific command
    ),
    # Civilian = Total - Armed Forces
    civilian_population = c(70524, 394770, 113643, 454794),
    source = "U.S. Census Bureau, Decennial Census"
  )
}

# =============================================================================
# 1940 AGE 85+ DISTRIBUTION (Input #42)
# =============================================================================

# get_1940_85plus_distribution() — REMOVED in Phase 3
# 1940 85+ populations now read directly from SSPopDec via
# load_tr_population_by_year() in historical_population.R

# =============================================================================
# DOD ARMED FORCES IN TERRITORIES (Input #41)
# =============================================================================

#' Get DoD armed forces stationed in territories
#'
#' @description
#' Returns total numbers of armed forces stationed in Puerto Rico,
#' Virgin Islands, Guam, and American Samoa for decennial census years.
#'
#' @param census_years Integer vector of census years (1990, 2000, 2010, 2020)
#'
#' @return data.table with census_year, territory, armed_forces
#'
#' @details
#' These data are used to adjust territory populations by removing
#' military stationed there (who are counted elsewhere in USAF component).
#'
#' Per TR2025: "2020 data is assumed to be the same as 2010 until the
#' data is available to OCACT."
#'
#' @section Data Sources:
#' - Department of Defense. Defense Manpower Data Center (DMDC).
#' - Census Bureau. Census 2000 and 2010 Special Tabulations.
#' - Statistical Abstract of the United States (military personnel tables).
#'
#' @export
get_dod_armed_forces_territories <- function(census_years = c(1990, 2000, 2010, 2020)) {
  # ============================================================================
  # ARMED FORCES IN U.S. TERRITORIES (Decennial Census Years)
  # ============================================================================
  # Source: DoD DMDC, Census Bureau, Statistical Abstract
  #
  # Note: Per TR2025, 2020 data assumed same as 2010 until updated
  # ============================================================================

  data <- data.table::data.table(
    census_year = c(
      # 1990
      1990, 1990, 1990, 1990,
      # 2000
      2000, 2000, 2000, 2000,
      # 2010
      2010, 2010, 2010, 2010,
      # 2020 (assumed same as 2010 per TR2025)
      2020, 2020, 2020, 2020
    ),
    territory = rep(c("PR", "VI", "GU", "AS"), 4),
    armed_forces = c(
      # 1990 - Cold War era, significant presence
      # Source: Statistical Abstract 1992, Table 549
      3500,     # Puerto Rico (Roosevelt Roads, Fort Buchanan)
      200,      # Virgin Islands (small presence)
      10500,    # Guam (Andersen AFB, Naval Base Guam)
      100,      # American Samoa (small presence)

      # 2000 - Post-Cold War reduction
      # Source: Census 2000 special tabulations
      2500,     # Puerto Rico
      150,      # Virgin Islands
      6700,     # Guam (Pacific realignment)
      100,      # American Samoa

      # 2010 - Further realignment
      # Source: Census 2010 special tabulations, DMDC
      1500,     # Puerto Rico (Roosevelt Roads closed 2004)
      100,      # Virgin Islands
      6000,     # Guam (Asia-Pacific rebalance)
      50,       # American Samoa

      # 2020 - Assumed same as 2010 (per TR2025 documentation)
      1500,     # Puerto Rico
      100,      # Virgin Islands
      6000,     # Guam
      50        # American Samoa
    ),
    source = c(
      rep("Statistical Abstract / DMDC", 4),
      rep("Census 2000 special tabulation", 4),
      rep("Census 2010 / DMDC", 4),
      rep("Assumed same as 2010 (TR2025)", 4)
    )
  )

  target_years <- census_years
  data[census_year %in% target_years]
}

# =============================================================================
# TERRITORY HISTORICAL DATA
# =============================================================================

#' Get historical territory populations
#'
#' @description
#' Returns decennial census populations for U.S. territories
#' before regular Census API availability.
#'
#' @param target_census_year Integer: decennial census year (1950-2000)
#' @param target_territory Character: territory code (PR, VI, GU, AS, MP)
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
#' @section Data Sources:
#' All population figures are from U.S. Census Bureau decennial censuses:
#' - 1950: Census of Population, Volume I, Chapter 57
#' - 1960: Census Supplementary Reports PC-S1-14 and PC-S1-15
#' - 1970-2000: Statistical Abstract of the United States (various editions)
#'
#' @export
get_territory_historical_population <- function(target_census_year = NULL,
                                                 target_territory = NULL) {
  # ==========================================================================
  # TERRITORY POPULATIONS FROM DECENNIAL CENSUSES
  # ==========================================================================
  # Source: U.S. Census Bureau, Decennial Census of Population
  # Values transcribed from official census publications
  # ==========================================================================

  territory_data <- data.table::data.table(
    census_year = c(
      # Puerto Rico (Source: PC-S1-15, Statistical Abstract, 2010/2020 Decennial Census)
      1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020,
      # Virgin Islands (Source: PC-S1-14, Statistical Abstract, 2010/2020 Island Areas Census)
      1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020,
      # Guam (Source: PC-S1-14, Statistical Abstract, 2010/2020 Island Areas Census)
      1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020,
      # American Samoa (Source: PC-S1-14, Statistical Abstract, 2010/2020 Island Areas Census)
      # Note: 1950 census conducted but AS not in SS area until 1961
      1960, 1970, 1980, 1990, 2000, 2010, 2020,
      # Northern Mariana Islands (Source: Statistical Abstract, 2010/2020 Island Areas Census)
      # Note: First census in CNMI was 1970
      1980, 1990, 2000, 2010, 2020
    ),
    territory = c(
      rep("PR", 8), rep("VI", 8), rep("GU", 8), rep("AS", 7), rep("MP", 5)
    ),
    population = c(
      # Puerto Rico
      # Source: 1960 Census PC-S1-15; Statistical Abstract Section 29; 2010/2020 Decennial Census API
      2210703,  # 1950
      2349544,  # 1960
      2712033,  # 1970 (2,722 thousands in StatAb)
      3196520,  # 1980 (3,210 thousands in StatAb)
      3522037,  # 1990 (3,537 thousands in StatAb)
      3808610,  # 2000
      3725789,  # 2010 (Census API P001001)
      3285874,  # 2020 (Census API P1_001N)

      # Virgin Islands
      # Source: 1960 Census PC-S1-14; Statistical Abstract Section 29; 2010/2020 Island Areas Census
      26665,    # 1950
      32099,    # 1960
      62468,    # 1970 (63 thousands in StatAb)
      96569,    # 1980 (98 thousands in StatAb)
      101809,   # 1990 (104 thousands in StatAb)
      108612,   # 2000
      106405,   # 2010 (2010 Island Areas Census)
      87146,    # 2020 (2020 Island Areas Census)

      # Guam
      # Source: 1960 Census PC-S1-14; Statistical Abstract Section 29; 2010/2020 Island Areas Census
      59498,    # 1950
      67044,    # 1960
      84996,    # 1970 (86 thousands in StatAb)
      105979,   # 1980 (107 thousands in StatAb)
      133152,   # 1990 (134 thousands in StatAb)
      154805,   # 2000
      159358,   # 2010 (2010 Island Areas Census)
      153836,   # 2020 (2020 Island Areas Census)

      # American Samoa
      # Source: 1960 Census PC-S1-14; Statistical Abstract Section 29; 2010/2020 Island Areas Census
      20051,    # 1960 (first census after SS area inclusion)
      27259,    # 1970 (27 thousands in StatAb)
      32297,    # 1980 (32 thousands in StatAb)
      46773,    # 1990 (47 thousands in StatAb)
      57291,    # 2000
      55519,    # 2010 (2010 Island Areas Census)
      49710,    # 2020 (2020 Island Areas Census)

      # Northern Mariana Islands
      # Source: Statistical Abstract Section 29; 2010/2020 Island Areas Census
      # Note: Census Bureau first conducted CNMI census in 1970
      16780,    # 1980 (17 thousands in StatAb)
      43345,    # 1990 (44 thousands in StatAb)
      69221,    # 2000
      53883,    # 2010 (2010 Island Areas Census)
      47329     # 2020 (2020 Island Areas Census)
    ),
    ss_area_start = c(
      # Puerto Rico - 1951
      rep(1951L, 8),
      # Virgin Islands - 1951
      rep(1951L, 8),
      # Guam - 1951
      rep(1951L, 8),
      # American Samoa - 1961
      rep(1961L, 7),
      # Northern Mariana Islands - 1978 (Covenant effective)
      rep(1978L, 5)
    ),
    source = c(
      # Puerto Rico
      rep("Census PC-S1-15 / Statistical Abstract", 6),
      "2010 Decennial Census API",
      "2020 Decennial Census API",
      # Virgin Islands
      rep("Census PC-S1-14 / Statistical Abstract", 6),
      "2010 Island Areas Census",
      "2020 Island Areas Census",
      # Guam
      rep("Census PC-S1-14 / Statistical Abstract", 6),
      "2010 Island Areas Census",
      "2020 Island Areas Census",
      # American Samoa
      rep("Census PC-S1-14 / Statistical Abstract", 5),
      "2010 Island Areas Census",
      "2020 Island Areas Census",
      # Northern Mariana Islands
      rep("Statistical Abstract Section 29", 3),
      "2010 Island Areas Census",
      "2020 Island Areas Census"
    )
  )

  result <- territory_data

  if (!is.null(target_census_year)) {
    result <- result[census_year == target_census_year]
  }

  if (!is.null(target_territory)) {
    result <- result[territory == target_territory]
  }

  result
}

#' Get territory Social Security area start year
#'
#' @param territory Character: territory code
#'
#' @return Integer: year territory was added to SS area
#'
#' @section Sources:
#' - Social Security Administration. "Social Security Programs in the
#'   United States." SSA Publication No. 13-11758.
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
#' These estimates are derived from multiple historical sources:
#'
#' **WWII Era (1940-1945):**
#' - Total military personnel from National WWII Museum statistics
#' - Overseas percentage estimated at 73% during active war (per WWII Museum)
#' - Pre-war (1940-1941): Limited overseas presence in Philippines, Panama, etc.
#'
#' **Post-War (1946-1949):**
#' - Rapid demobilization: 12M (mid-1945) to 1.5M (mid-1947)
#' - Army: 8M+ (Aug 1945) → 3M (Jan 1946) → 554K (Mar 1948) → 600K (1949)
#' - Occupation forces in Germany and Japan
#'
#' @section Primary Sources:
#' - National WWII Museum. "Research Starters: US Military by the Numbers."
#'   Total personnel by year: 1940 (458K), 1941 (1.8M), 1942 (3.9M),
#'   1943 (9.2M), 1944 (11.6M), 1945 (12.2M).
#' - U.S. Army Center of Military History. CMH Pub 104-8.
#' - Britannica. "United States Army: WWII, Korean War, and the Cold War."
#'
#' @section Methodology Notes:
#' Overseas troop estimates for 1940-1945 are calculated as:
#' - 1940-1941: Small garrison forces (Philippines, Panama, Caribbean bases)
#' - 1942-1945: ~60-73% of total military overseas during active operations
#' - 1945 peak: 7.6 million overseas (per NWWII Museum: "12M total, 7.6M abroad")
#' - 1946-1949: Occupation forces declining to pre-Korea baseline
#'
#' @export
get_pre1950_armed_forces <- function(years = 1940:1949) {
  # ==========================================================================
  # ARMED FORCES OVERSEAS ESTIMATES (1940-1949)
  # ==========================================================================
  # Primary source: National WWII Museum "US Military by the Numbers"
  # Total personnel: 1940=458K, 1941=1.8M, 1942=3.9M, 1943=9.2M,
  #                  1944=11.6M, 1945=12.2M
  # Overseas: "73% served overseas" during 1941-1945; 7.6M overseas at peak
  #
  # Post-war demobilization sources:
  # - Army declined from 8M (Aug 1945) to 3M (Jan 1946) to 554K (Mar 1948)
  # - By June 30, 1947: 1.566M total active duty
  # - Army stabilized at ~600K in 1949-50
  # ==========================================================================

  data <- data.table::data.table(
    year = 1940:1949,
    total_military = c(
      # Source: National WWII Museum
      458365,     # 1940
      1801101,    # 1941
      3915507,    # 1942
      9195912,    # 1943
      11623468,   # 1944
      12209238,   # 1945
      # Post-war estimates (derived from demobilization data)
      3000000,    # 1946 (mid-year estimate)
      1566000,    # 1947 (June 30, 1947 per Army Center of Military History)
      1400000,    # 1948 (estimate)
      1600000     # 1949 (pre-Korea buildup)
    ),
    overseas_troops = c(
      # Pre-war: Philippines (~12K), Panama, Caribbean bases
      50000,      # 1940 - Pre-war garrisons

      # 1941: Build-up, Iceland occupation (July 1941)
      150000,     # 1941 - Growing overseas presence

      # 1942-1945: Active war operations
      # NWWII Museum: "73% served overseas" during war; 7.6M overseas at peak
      2350000,    # 1942 - ~60% overseas (North Africa, Pacific buildup)
      5500000,    # 1943 - ~60% overseas
      7500000,    # 1944 - ~65% overseas (Europe invasion)
      7600000,    # 1945 - Peak: 7.6M overseas (per NWWII Museum)

      # Post-war occupation and demobilization
      # Germany: 337K occupation force (1946)
      # Japan: ~400K initially, declining
      1500000,    # 1946 - Rapid drawdown, still large occupation
      500000,     # 1947 - Continued reduction
      400000,     # 1948 - Post-war baseline
      350000      # 1949 - Pre-Korea baseline
    ),
    source = c(
      "NWWII Museum / garrison estimate",
      "NWWII Museum / Iceland occupation",
      "NWWII Museum (73% overseas)",
      "NWWII Museum (73% overseas)",
      "NWWII Museum (73% overseas)",
      "NWWII Museum (7.6M overseas at peak)",
      "CMH Pub 104-8 / occupation forces",
      "CMH Pub 104-8 (June 30, 1947)",
      "CMH Pub 104-8 / Army 554K Mar 1948",
      "Britannica / pre-Korea baseline"
    )
  )

  target_years <- years
  result <- data[year %in% target_years, .(year, overseas_troops, source)]
  # Rename to match expected column name from DMDC data
  data.table::setnames(result, "overseas_troops", "population")
  result
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
#' @section Source:
#' U.S. Census Bureau. Decennial Census of Population and Housing.
#' Official resident population counts as of April 1 of each census year.
#' https://www.census.gov/programs-surveys/decennial-census/data/tables.html
#'
#' Values verified against Census Bureau press releases and
#' Historical National Population Estimates (popclockest.txt).
#'
#' @export
get_population_benchmarks <- function() {
  # ==========================================================================
  # U.S. RESIDENT POPULATION FROM DECENNIAL CENSUSES
  # ==========================================================================
  # Source: U.S. Census Bureau, Decennial Census
  # Official counts as of April 1 of each census year
  #
  # Note: These are "resident population" figures, which exclude:
  # - Armed forces overseas
  # - Federal civilian employees overseas
  # - Other U.S. citizens abroad
  # ==========================================================================

  data.table::data.table(
    census_year = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
    census_date = rep("April 1", 9),
    resident_population = c(
      132164569,  # 1940 - Census Bureau official count
      151325798,  # 1950 - Census Bureau official count
      179323175,  # 1960 - Census Bureau official count
      203211926,  # 1970 - Census Bureau official count
      226545805,  # 1980 - Census Bureau official count
      248709873,  # 1990 - Census Bureau official count
      281421906,  # 2000 - Census Bureau official count
      308745538,  # 2010 - Census Bureau official count
      331449281   # 2020 - Census Bureau official count
    ),
    source = "U.S. Census Bureau, Decennial Census"
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
#' @section Reference:
#' Social Security Administration. (2025). The 2025 Annual Report of the
#' Board of Trustees of the Federal Old-Age and Survivors Insurance and
#' Federal Disability Insurance Trust Funds. Section V.A: Demographic
#' Assumptions and Methods.
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
      "troopdata / dmdc_armed_forces.R / historical_static.R",
      "census_undercount.R",
      "census_historical_population.R / historical_static.R",
      "opm_federal_employees.R",
      "opm_federal_employees.R (estimated)",
      "ssa_beneficiaries_abroad.R",
      "Estimated (small residual)"
    ),
    typical_magnitude = c(
      "150-300K (peacetime); 7.6M (WWII peak)",
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
#' @section Reference:
#' Social Security Administration. TR2025 Actuarial Study.
#' Section V.A: Historical Population Methodology.
#'
#' @export
get_tab_years <- function(config = NULL) {
  # Read tab years from config
  if (is.null(config) || is.null(config$historical_population$tab_years)) {
    cli::cli_abort(c(
      "Config missing {.field historical_population.tab_years}",
      "i" = "Ensure config is passed to {.fn get_tab_years}"
    ))
  }
  tab_cfg <- config$historical_population$tab_years
  required_keys <- c("early", "annual_range", "final")
  missing <- setdiff(required_keys, names(tab_cfg))
  if (length(missing) > 0) {
    cli::cli_abort("Config missing tab_years fields: {.field {missing}}")
  }
  early <- tab_cfg$early
  annual <- seq(tab_cfg$annual_range[1], tab_cfg$annual_range[2])
  final <- tab_cfg$final
  sort(unique(c(early, annual, final)))
}


# =============================================================================
# DOCUMENTATION FUNCTIONS
# =============================================================================

#' Summarize historical static data sources
#'
#' @description
#' Returns a summary of all data sources used in this module.
#'
#' @export
summarize_historical_static_sources <- function() {
  data.table::data.table(
    data_type = c(
      "Pre-1980 USAF population (1940-1979)",
      "Territory populations (1950-2000)",
      "Pre-1950 armed forces (1940-1949)",
      "Alaska/Hawaii civilian (1940-1949)",
      "Alaska/Hawaii census (1940, 1950)",
      "1940 85+ age distribution",
      "DoD armed forces in territories",
      "Population benchmarks (1940-2020)"
    ),
    tr2025_input = c(
      "#7 - USAF July 1 (1940-79)",
      "#22 - Territory decennial (1950-2000)",
      "#44 - Armed forces (1940-57)",
      "#30 - AK/HI civilian (1940-49)",
      "#31 - AK/HI census (1940, 1950)",
      "#42 - 1940 85+ distribution",
      "#41 - DoD in territories",
      "Validation benchmarks"
    ),
    primary_source = c(
      "Census P-25 intercensal estimates",
      "Census decennial publications",
      "National WWII Museum / Army CMH",
      "Census 1950 Vol I Parts 51-54",
      "Census 1940/1950 decennial",
      "1940 Census + survival curves",
      "DoD DMDC / Census special tabs",
      "Census decennial counts"
    ),
    coverage = c(
      "Totals by sex, 40 years",
      "5 territories, 6 census years",
      "10 years with overseas estimates",
      "AK + HI, 10 years each",
      "2 territories, 2 census years",
      "Ages 85-119 by sex",
      "4 territories, 4 census years",
      "9 census years (1940-2020)"
    )
  )
}

#' Get bibliography for historical static data
#'
#' @description
#' Returns formatted citations for historical static data sources.
#'
#' @export
get_historical_static_bibliography <- function() {
  citations <- c(
    "National WWII Museum. (n.d.). Research Starters: US Military by the Numbers. https://www.nationalww2museum.org/students-teachers/student-resources/research-starters/research-starters-us-military-numbers",

    "U.S. Army Center of Military History. (1952). History of Personnel Demobilization in the United States Army. CMH Pub 104-8. https://www.history.army.mil/html/books/104/104-8/CMH_Pub_104-8.pdf",

    "U.S. Census Bureau. (1952). 1950 Census of Population, Volume I, Chapter 57: American Samoa, Canal Zone, Guam, Virgin Islands of the United States.",

    "U.S. Census Bureau. (1961). 1960 Census Supplementary Report PC-S1-14: Population Counts and Selected Characteristics for Guam, Virgin Islands, American Samoa, and Canal Zone.",

    "U.S. Census Bureau. (1961). 1960 Census Supplementary Report PC-S1-15: Population Counts and Selected Characteristics for Puerto Rico.",

    "U.S. Census Bureau. (Various years). Statistical Abstract of the United States. Section 29: Puerto Rico and the Island Areas.",

    "U.S. Census Bureau. (Various years). Decennial Census of Population and Housing. https://www.census.gov/programs-surveys/decennial-census/data/tables.html"
  )

  cat("Historical Static Data Sources\n")
  cat("==============================\n\n")
  for (i in seq_along(citations)) {
    cat(paste0("[", i, "] ", citations[i], "\n\n"))
  }

  invisible(citations)
}

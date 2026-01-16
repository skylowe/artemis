# Test script for mortality data acquisition
# Run this to verify the NCHS death data download and processing works

library(data.table)
library(cli)
library(haven)
library(httr2)

# Source the data acquisition functions
source(here::here("R/data_acquisition/nchs_deaths.R"))
source(here::here("R/data_acquisition/census_population.R"))
source(here::here("R/utils/api_helpers.R"))

# ============================================================================
# Test 1: ICD-10 cause mapping
# ============================================================================
cli_h1("Test 1: ICD-10 Cause Mapping")

test_codes <- c("I25.1", "C34", "V89", "J18", "G30", "A01", "F01", "X70")
expected <- c("CVD", "CAN", "ACV", "RES", "DEM", "OTH", "DEM", "ACV")

result <- map_icd10_to_cause(test_codes)
cli_alert_info("Test ICD-10 codes: {paste(test_codes, collapse=', ')}")
cli_alert_info("Expected causes: {paste(expected, collapse=', ')}")
cli_alert_info("Mapped causes:   {paste(result, collapse=', ')}")

if (all(result == expected)) {
  cli_alert_success("ICD-10 mapping test PASSED")
} else {
  cli_alert_danger("ICD-10 mapping test FAILED")
  mismatches <- which(result != expected)
  for (i in mismatches) {
    cli_alert_warning("  {test_codes[i]}: expected {expected[i]}, got {result[i]}")
  }
}

# ============================================================================
# Test 2: Check CDC NCHS mortality file availability
# ============================================================================
cli_h1("Test 2: Check CDC NCHS Mortality File Availability")

test_year <- 2019
url <- sprintf("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/mort%dus.zip", test_year)
cli_alert_info("Testing URL: {url}")

tryCatch({
  req <- request(url) |>
    req_method("HEAD") |>
    req_timeout(30)
  resp <- req_perform(req)

  if (resp_status(resp) == 200) {
    cli_alert_success("File exists at CDC NCHS for year {test_year}")
  } else {
    cli_alert_warning("File returned status {resp_status(resp)}")
  }
}, error = function(e) {
  cli_alert_danger("Failed to check file: {conditionMessage(e)}")
})

# ============================================================================
# Test 3: Download and process a single year of mortality data
# ============================================================================
cli_h1("Test 3: Download Single Year Mortality Data")

# Use a recent year (2019 - last pre-COVID year)
cli_alert_info("Downloading mortality data for {test_year}...")
cli_alert_warning("This will download a large file (~300-500 MB). Continue? [Set test_download=TRUE]")

test_download <- FALSE  # Set to TRUE to actually download

if (test_download) {
  deaths <- tryCatch({
    fetch_nchs_deaths_by_age(
      year = test_year,
      cache_dir = here::here("data/cache/nchs_deaths"),
      force_download = FALSE
    )
  }, error = function(e) {
    cli_alert_danger("Download failed: {conditionMessage(e)}")
    NULL
  })

  if (!is.null(deaths)) {
    cli_alert_success("Downloaded mortality data for {test_year}")

    # Summary statistics
    cli_h2("Data Summary")
    cli_alert_info("Total records: {nrow(deaths)}")
    cli_alert_info("Total deaths: {format(sum(deaths$deaths), big.mark=',')}")
    cli_alert_info("Age range: {min(deaths$age)} - {max(deaths$age)}")
    cli_alert_info("Sexes: {paste(unique(deaths$sex), collapse=', ')}")
    cli_alert_info("Causes: {paste(unique(deaths$cause), collapse=', ')}")

    # Deaths by cause
    cli_h2("Deaths by Cause")
    by_cause <- deaths[, .(deaths = sum(deaths)), by = cause]
    setorder(by_cause, -deaths)
    print(by_cause)

    # Deaths by age group
    cli_h2("Deaths by Age Group")
    deaths[, age_group := cut(age, breaks = c(0, 1, 15, 50, 65, 85, Inf),
                               labels = c("0", "1-14", "15-49", "50-64", "65-84", "85+"),
                               right = FALSE)]
    by_age <- deaths[, .(deaths = sum(deaths)), by = age_group]
    print(by_age)
  }
} else {
  cli_alert_info("Skipping download test. Set test_download=TRUE to run.")
}

# ============================================================================
# Test 4: Check cached birth data availability for comparison
# ============================================================================
cli_h1("Test 4: Check Existing Birth Data Cache")

birth_cache_dir <- here::here("data/raw/nchs")
if (dir.exists(birth_cache_dir)) {
  cached_files <- list.files(birth_cache_dir, pattern = "births_by_age_.*\\.rds")
  cli_alert_info("Found {length(cached_files)} cached birth data files")
  if (length(cached_files) > 0) {
    years_cached <- gsub("births_by_age_(\\d+)\\.rds", "\\1", cached_files)
    cli_alert_info("Years: {paste(range(as.integer(years_cached)), collapse='-')}")
  }
} else {
  cli_alert_info("No birth data cache directory found")
}

# ============================================================================
# Test 5: Test 2010 standard population
# ============================================================================
cli_h1("Test 5: 2010 Standard Population")

cli_alert_info("Attempting to fetch 2010 standard population...")

standard_pop <- tryCatch({
  get_standard_population_2010(cache_dir = here::here("data/raw/census"))
}, error = function(e) {
  cli_alert_warning("API fetch failed, checking fallback...")
  get_hardcoded_standard_population_2010()
})

if (!is.null(standard_pop)) {
  cli_alert_success("Retrieved 2010 standard population")
  cli_alert_info("Total records: {nrow(standard_pop)}")
  cli_alert_info("Sexes: {paste(unique(standard_pop$sex), collapse=', ')}")
  cli_alert_info("Total population (both): {format(sum(standard_pop[sex=='both', population]), big.mark=',')}")
}

# ============================================================================
# Summary
# ============================================================================
cli_h1("Test Summary")
cli_alert_success("Mortality data acquisition infrastructure is set up")
cli_alert_info("Next steps:")
cli_alert_info("  1. Run with test_download=TRUE to fetch actual data")
cli_alert_info("  2. Download multiple years: 1979-2022")
cli_alert_info("  3. Validate against TR2025 death probabilities")

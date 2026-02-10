#' NCHS Mortality Data Acquisition
#'
#' Functions for downloading and processing death data from NCHS via NBER.
#' Processes mortality microdata to aggregate deaths by age, sex, and cause.
#'
#' NOTE: TR2025 Section 1.2.b data scope vs ARTEMIS implementation:
#'
#' Intentionally omitted (zero impact on projections):
#' - Items 2, 5, 12-16: Pre-1968 historical data (deaths 1900-67, populations
#'   1900-79, Death Registration area states). Only used for historical life
#'   tables prior to the regression period (2008-2019).
#' - Items 17-18: USAF overseas population and 5-year age group splitting for
#'   pre-1968 ages 85+. Only affects pre-1968 elderly population estimates.
#' - Item 7: Starting qx from 1939-41 decennial life tables for infant/toddler
#'   age groups. Only used for 1940-1967 period q0 calculation.
#'
#' @name nchs_deaths
NULL

#' ICD-10 to broad cause mapping
#'
#' @description
#' Maps ICD-10 cause of death codes to the 6 broad categories used in
#' SSA mortality projections.
#'
#' @return data.table with columns: cause_code, cause_name, icd10_pattern
#'
#' @details
#' The six causes are:
#' - CVD: Cardiovascular Disease (I00-I78, N02-N03, N05-N07, N26)
#' - CAN: Cancer (C00-C97)
#' - ACV: Accidents and Violence (U01-U03, V01-Y35, Y40-Y87.2, Y88, Y89.0, Y89.9)
#' - RES: Respiratory Disease (J00-J06, J09-J18, J20-J22, J30-J47, J60-J98, U04)
#' - DEM: Dementia (F01, F03, G30, G31)
#' - OTH: All Other
#'
#' @export
get_icd10_cause_mapping <- function() {
  data.table::data.table(
    cause_code = c("CVD", "CAN", "ACV", "RES", "DEM", "OTH"),
    cause_name = c(
      "Cardiovascular Disease",
      "Cancer",
      "Accidents and Violence",
      "Respiratory Disease",
      "Dementia",
      "Other"
    )
  )
}

#' Map ICD-10 code to broad cause category
#'
#' @description
#' Determines which of the 6 broad cause categories an ICD-10 code belongs to.
#'
#' @param icd10_code Character vector of ICD-10 codes
#'
#' @return Character vector of cause codes (CVD, CAN, ACV, RES, DEM, OTH)
#'
#' @export
map_icd10_to_cause <- function(icd10_code) {
  # Clean the code - remove dots and convert to uppercase

  code <- toupper(gsub("\\.", "", icd10_code))

  # Initialize result as "OTH" (Other)
  result <- rep("OTH", length(code))

  # Cardiovascular Disease: I00-I78, N02-N03, N05-N07, N26
  cvd_pattern <- "^I[0-6]|^I7[0-8]|^N0[2-357]|^N26"
  result[grepl(cvd_pattern, code)] <- "CVD"


  # Cancer: C00-C97
  can_pattern <- "^C[0-9]"
  result[grepl(can_pattern, code)] <- "CAN"

  # Accidents and Violence: U01-U03, V01-Y35, Y40-Y87, Y88, Y89.0, Y89.9
  # Note: Complex pattern covering external causes
  acv_pattern <- "^U0[1-3]|^V|^W|^X|^Y[0-3]|^Y4|^Y5|^Y6|^Y7|^Y8[0-7]|^Y88|^Y890|^Y899"
  result[grepl(acv_pattern, code)] <- "ACV"

  # Respiratory Disease: J00-J06, J09-J18, J20-J22, J30-J47, J60-J98, U04
  res_pattern <- "^J0[0-6]|^J09|^J1[0-8]|^J2[0-2]|^J3|^J4[0-7]|^J[6-9]|^U04"
  result[grepl(res_pattern, code)] <- "RES"

  # Dementia: F01, F03, G30, G31
  dem_pattern <- "^F01|^F03|^G30|^G31"
  result[grepl(dem_pattern, code)] <- "DEM"

  result
}

#' Map ICD-9 code to broad cause category
#'
#' @description
#' Maps ICD-9 codes (used 1979-1998) to the 6 broad cause categories.
#' Uses approximate mapping based on ICD-9 to ICD-10 equivalencies.
#'
#' @param icd9_code Character vector of ICD-9 codes
#'
#' @return Character vector of cause codes (CVD, CAN, ACV, RES, DEM, OTH)
#'
#' @details
#' ICD-9 to broad cause approximate mappings:
#' - CVD: 390-459 (Diseases of circulatory system)
#' - CAN: 140-239 (Neoplasms)
#' - ACV: E800-E999 (External causes), 800-999 (Injury and poisoning)
#' - RES: 460-519 (Diseases of respiratory system)
#' - DEM: 290, 331 (Dementia, Alzheimer's)
#'
#' @export
map_icd9_to_cause <- function(icd9_code) {
  # Clean the code
  code <- toupper(gsub("\\.", "", icd9_code))

  # Initialize result as "OTH"
  result <- rep("OTH", length(code))

  # For numeric codes, extract the base number
  numeric_code <- suppressWarnings(as.integer(substr(gsub("^[A-Z]", "", code), 1, 3)))

  # Cardiovascular Disease: 390-459
  result[!is.na(numeric_code) & numeric_code >= 390 & numeric_code <= 459] <- "CVD"

  # Cancer: 140-239
  result[!is.na(numeric_code) & numeric_code >= 140 & numeric_code <= 239] <- "CAN"

  # Accidents and Violence: E-codes (E800-E999) and injury codes (800-999)
  result[grepl("^E[89]", code)] <- "ACV"
  result[!is.na(numeric_code) & numeric_code >= 800 & numeric_code <= 999] <- "ACV"

  # Respiratory Disease: 460-519
  result[!is.na(numeric_code) & numeric_code >= 460 & numeric_code <= 519] <- "RES"

  # Dementia: 290, 331
  result[!is.na(numeric_code) & (numeric_code == 290 | numeric_code == 331)] <- "DEM"

  result
}

#' Map ICD-8 code to broad cause category
#'
#' @description
#' Maps ICD-8 codes (used 1968-1978) to the 6 broad cause categories.
#' ICD-8 code structure is similar to ICD-9.
#'
#' @param icd8_code Character vector of ICD-8 codes
#'
#' @return Character vector of cause codes (CVD, CAN, ACV, RES, DEM, OTH)
#'
#' @details
#' ICD-8 to broad cause approximate mappings:
#' - CVD: 390-458 (Diseases of circulatory system)
#' - CAN: 140-239 (Neoplasms)
#' - ACV: E800-E999 (External causes), 800-999 (Injury and poisoning)
#' - RES: 460-519 (Diseases of respiratory system)
#' - DEM: 290 (Senile and presenile dementia)
#'
#' Note: ICD-8 did not have a specific Alzheimer's code (G30 in ICD-10, 331 in ICD-9)
#'
#' @export
map_icd8_to_cause <- function(icd8_code) {
  # Clean the code - remove leading/trailing whitespace
  code <- toupper(trimws(icd8_code))

  # Initialize result as "OTH"
  result <- rep("OTH", length(code))

  # For numeric codes, extract the base number
  # ICD-8 codes are typically 3-4 digits
  numeric_code <- suppressWarnings(as.integer(substr(gsub("^[A-Z]", "", code), 1, 3)))

  # Cardiovascular Disease: 390-458 (ICD-8 range slightly different from ICD-9)
  result[!is.na(numeric_code) & numeric_code >= 390 & numeric_code <= 458] <- "CVD"

  # Cancer: 140-239
  result[!is.na(numeric_code) & numeric_code >= 140 & numeric_code <= 239] <- "CAN"

  # Accidents and Violence: E-codes (E800-E999) and injury codes (800-999)
  result[grepl("^E[89]", code)] <- "ACV"
  result[!is.na(numeric_code) & numeric_code >= 800 & numeric_code <= 999] <- "ACV"

  # Respiratory Disease: 460-519
  result[!is.na(numeric_code) & numeric_code >= 460 & numeric_code <= 519] <- "RES"

  # Dementia: 290 (Senile and presenile dementia in ICD-8)
  result[!is.na(numeric_code) & numeric_code == 290] <- "DEM"

  result
}

#' Fetch deaths by age, sex, and cause from NCHS
#'
#' @description
#' Downloads NCHS mortality microdata from CDC and aggregates to death counts
#' by single year of age, sex, and cause of death. Results are cached locally.
#'
#' @param year Integer: year to fetch (1968-2023 for single-year-of-age data)
#' @param cache_dir Character: directory to cache downloaded/processed data
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, age, sex, cause, deaths
#'
#' @details
#' Data source: CDC NCHS Vital Statistics Multiple Cause of Death files
#' URL: https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/
#' File pattern: mort{year}us.zip
#'
#' Key variables (vary by year):
#' - detail_age/age: Age at death
#' - sex: Sex (M/F or 1/2)
#' - ucod/icd10: Underlying cause of death (ICD code)
#'
#' ICD versions:
#' - 1999+: ICD-10
#' - 1979-1998: ICD-9
#' - Pre-1979: ICD-8 or earlier (limited cause mapping)
#'
#' Files are fixed-width format requiring layout documentation.
#'
#' @export
fetch_nchs_deaths_by_age <- function(year, cache_dir = "data/cache/nchs_deaths",
                                      force_download = FALSE) {
  # Validate year
  if (year < 1968 || year > 2023) {
    cli::cli_abort("Year must be between 1968 and 2023")
  }

  # Ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Check for cached aggregated results
  cache_file <- file.path(cache_dir, sprintf("deaths_by_age_sex_cause_%d.rds", year))
  if (file.exists(cache_file) && !force_download) {
    cli::cli_alert_success("Loading cached death data for {year}")
    return(readRDS(cache_file))
  }

  cli::cli_alert_info("Downloading NCHS mortality data for {year}...")
  cli::cli_alert_warning("This may take several minutes (files are 40-160 MB)")

  # Construct URL - CDC NCHS FTP site
  # Format: https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/mort{year}us.zip
  url <- sprintf(
    "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/mort%dus.zip",
    year
  )

  # Download to temp file
 temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(c(temp_zip, temp_dir), recursive = TRUE), add = TRUE)

  # Set longer timeout for large files
  old_timeout <- getOption("timeout")
  options(timeout = 1800)  # 30 minutes
  on.exit(options(timeout = old_timeout), add = TRUE)

  tryCatch({
    download.file(url, temp_zip, mode = "wb", quiet = FALSE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download NCHS mortality data for {year}",
      "x" = conditionMessage(e),
      "i" = "URL: {url}"
    ))
  })

  # Extract ZIP file
  # Note: R's built-in unzip() can fail on very large files (>2GB)
  # Use system unzip as fallback for reliability
  cli::cli_alert("Extracting ZIP file...")
  unzip_result <- tryCatch({
    utils::unzip(temp_zip, exdir = temp_dir)
    TRUE
  }, warning = function(w) {
    # Warnings like "zip file is corrupt" often mean the file is too large
    FALSE
  }, error = function(e) {
    FALSE
  })

  # If R's unzip failed, try system unzip
  if (!unzip_result || length(list.files(temp_dir)) == 0) {
    cli::cli_alert("Using system unzip for large file...")
    system2("unzip", args = c("-o", "-q", temp_zip, "-d", temp_dir),
            stdout = FALSE, stderr = FALSE)
  }

  # Find the data file
  # File naming varies by year:
  #   2019: VS19MORT.DUSMCPUB_r20210304 (no extension)
  #   2018: Mort2018US.PubUse.txt
  #   older: various patterns
  data_files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
  # Exclude documentation files (pdf, doc, docx) but keep txt data files
  data_files <- data_files[!grepl("\\.(pdf|docx?)$", data_files, ignore.case = TRUE)]

  # Filter to files that look like data files (large size or specific naming)
  # Data files are typically > 50MB
  file_sizes <- file.size(data_files)
  large_files <- data_files[file_sizes > 50 * 1024^2]  # > 50MB
  if (length(large_files) > 0) {
    data_files <- large_files
  }

  if (length(data_files) == 0) {
    cli::cli_abort("No data file found in ZIP archive for {year}")
  }

  data_file <- data_files[1]
  cli::cli_alert("Reading and processing mortality data from {basename(data_file)}...")

  # Get file layout for this year
  layout <- get_mortality_file_layout(year)

  # Read fixed-width file
  dt <- read_mortality_fixed_width(data_file, layout, year)

  # Process the data
  result <- process_mortality_data(dt, year)

  # Cache the result
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached results to {cache_file}")

  total_deaths <- sum(result$deaths)
  cli::cli_alert_success("Retrieved {format(total_deaths, big.mark = ',')} total deaths for {year}")

  result
}

#' Get file layout for mortality fixed-width files
#'
#' @description
#' Returns column positions and names for NCHS mortality fixed-width files.
#' Layouts vary by year.
#'
#' @param year Integer: data year
#'
#' @return List with column specifications
#'
#' @keywords internal
get_mortality_file_layout <- function(year) {
  # Layouts from official CDC NCHS mortality file documentation
  # See data/raw/nchs/mortality_documentation/ for source documents
  #
  # Key format changes:
  # - 1968-1978 (ICD-8): Sex=35, Age=39-41 (3 char), UCOD=60-63

  # - 1979-2002 (ICD-9 and early ICD-10): Sex=59, Age=64-66 (3 char), UCOD=142-145
  # - 2003-2019 (ICD-10): Sex=69, Age=70-73 (4 char), UCOD=146-149
  # - 2020+ (ICD-10): Same as 2003-2019

  if (year >= 2020) {
    # 2020+ layout (similar to 2003-2004)
    # 2020+ files use the longer format (800+ chars per line)
    # Empirically verified from 2020 file
    list(
      detail_age = c(start = 70, end = 73),   # Detail age (4 chars)
      sex = c(start = 69, end = 69),          # Sex (M/F)
      ucod = c(start = 146, end = 149),       # Underlying cause ICD-10
      marital_status = c(start = 84, end = 84),  # S=single, M=married, W=widowed, D=divorced, U=unknown
      record_type = c(start = 1, end = 1)
    )
  } else if (year >= 2003) {
    # 2003-2019 layout (ICD-10, 4-char detail_age format)
    # Verified from Record_Layout_2003.md, Record_Layout_2005.md documentation:
    #   Sex: position 69 (M/F)
    #   Detail Age: positions 70-73 (4 chars: unit + 3-digit value)
    #   Underlying Cause: positions 146-149
    #   Marital Status: position 84 (S/M/W/D/U)
    list(
      detail_age = c(start = 70, end = 73),   # Detail age (4 chars)
      sex = c(start = 69, end = 69),          # Sex (M/F)
      ucod = c(start = 146, end = 149),       # Underlying cause ICD-10
      marital_status = c(start = 84, end = 84),  # S=single, M=married, W=widowed, D=divorced, U=unknown
      record_type = c(start = 1, end = 1)
    )
  } else if (year >= 1979) {
    # 1979-2002 layout (ICD-9 era and ICD-10 transition years 1999-2002)
    # All years 1979-2002 use same field positions per documentation:
    # - Mort1995_ICD9.md, Mort2000_Interim.md, Mort2001_Interim.md, Mort2002_Interim.md
    # From documentation:
    #   Sex: position 59 (1=Male, 2=Female)
    #   Detail Age: positions 64-66 (3 chars)
    #     - Position 64: unit (0=years<100, 1=years 100+, 2=months, 3=weeks, 4=days, 5=hours, 6=minutes, 9=not stated)
    #     - Positions 65-66: number of units
    #   Underlying Cause ICD-9: positions 142-145
    #   Marital Status: position 77 (1=single, 2=married, 3=widowed, 4=divorced, 8/9=unknown)
    list(
      detail_age = c(start = 64, end = 66),  # 3 chars for ICD-9 era
      sex = c(start = 59, end = 59),
      ucod = c(start = 142, end = 145),  # ICD-9 code (4 chars)
      marital_status = c(start = 77, end = 77),  # 1=single, 2=married, 3=widowed, 4=divorced, 8/9=unknown
      record_type = c(start = 20, end = 20)
    )
  } else {
    # 1968-1978 layout (ICD-8 era)
    # From dt78icd8.pdf documentation:
    #   Sex: Location 35 (1=Male, 2=Female)
    #   Age: Location 39-41 (39=unit, 40-41=value)
    #     - Unit: 0=years<1, 1=years, 2=months, 3=weeks, 4=days, 5=hours, 6=minutes, 9=not stated
    #   Underlying Cause: Location 60-63 (ICD-8 code)
    # Note: Marital status NOT available for 1968-1978
    list(
      detail_age = c(start = 39, end = 41),  # 3 chars: unit + 2-digit value
      sex = c(start = 35, end = 35),
      ucod = c(start = 60, end = 63),  # ICD-8 code (4 chars)
      marital_status = NULL,  # Not available for ICD-8 era
      record_type = c(start = 11, end = 11)
    )
  }
}

#' Read mortality fixed-width file
#'
#' @param file_path Path to data file
#' @param layout Column layout specification
#' @param year Data year
#'
#' @return data.table with extracted columns
#'
#' @keywords internal
read_mortality_fixed_width <- function(file_path, layout, year) {
  # Read the file as raw text
  cli::cli_alert("Reading file (this may take a moment for large files)...")

  # Use data.table::fread for speed, reading as single column
  # IMPORTANT: strip.white = FALSE to preserve leading whitespace (field positions are fixed)
  # encoding = "Latin-1" handles some older files with non-UTF-8 characters
  raw <- data.table::fread(
    file_path,
    header = FALSE,
    sep = "\n",
    col.names = "line",
    colClasses = "character",
    strip.white = FALSE,
    encoding = "Latin-1"
  )

  cli::cli_alert_info("Read {format(nrow(raw), big.mark=',')} records")

  # Special handling for 1979 which has mixed record formats
  # 440-char records: standard ICD-9 positions (sex=59, age=64-66, ucod=142-145)
  # 439-char records: 1-position offset (sex=58, age=63-65, ucod=141-144)
  if (year == 1979) {
    raw[, line_len := nchar(line)]

    # For 440-char records: use standard layout
    raw[line_len == 440, detail_age := substr(line, layout$detail_age[1], layout$detail_age[2])]
    raw[line_len == 440, sex := substr(line, layout$sex[1], layout$sex[2])]
    raw[line_len == 440, ucod := trimws(substr(line, layout$ucod[1], layout$ucod[2]))]
    if (!is.null(layout$marital_status)) {
      raw[line_len == 440, marital_status := substr(line, layout$marital_status[1], layout$marital_status[2])]
    }

    # For 439-char records: use -1 offset
    raw[line_len == 439, detail_age := substr(line, layout$detail_age[1] - 1, layout$detail_age[2] - 1)]
    raw[line_len == 439, sex := substr(line, layout$sex[1] - 1, layout$sex[2] - 1)]
    raw[line_len == 439, ucod := trimws(substr(line, layout$ucod[1] - 1, layout$ucod[2] - 1))]
    if (!is.null(layout$marital_status)) {
      raw[line_len == 439, marital_status := substr(line, layout$marital_status[1] - 1, layout$marital_status[2] - 1)]
    }

    raw[, line_len := NULL]
  } else {
    # Extract columns using substr
    # All years (1968+) now use detail_age format
    raw[, detail_age := substr(line, layout$detail_age[1], layout$detail_age[2])]
    raw[, sex := substr(line, layout$sex[1], layout$sex[2])]
    raw[, ucod := trimws(substr(line, layout$ucod[1], layout$ucod[2]))]

    # Extract marital status if available (1979+)
    if (!is.null(layout$marital_status)) {
      raw[, marital_status := substr(line, layout$marital_status[1], layout$marital_status[2])]
    }
  }

  # Remove the raw line column to save memory
  raw[, line := NULL]

  raw
}

#' Process mortality data after reading
#'
#' @param dt data.table from read_mortality_fixed_width
#' @param year Data year
#'
#' @return data.table with year, age, sex, cause, deaths
#'
#' @keywords internal
process_mortality_data <- function(dt, year) {
  cli::cli_alert("Processing mortality data...")

  # Process age - format varies by year
  if ("detail_age" %in% names(dt)) {
    if (year >= 2003) {
      # ICD-10 era (2003+): 4 chars (both 2003-2004 and 2005+ use 4-char detail_age)
      # Format: first char = unit (1=years, 2=months, 4=days, 5=hours, 6=minutes, 9=not stated)
      #         chars 2-4 = value (001-135 for years, 001-011 for months, etc.)
      dt[, detail_age_num := suppressWarnings(as.integer(detail_age))]

      # Years (1001-1199 typically, but can go up to 1135)
      dt[detail_age_num >= 1000 & detail_age_num < 2000, age := detail_age_num - 1000L]

      # Months, days, hours, minutes = age 0
      dt[detail_age_num >= 2000 & detail_age_num < 7000, age := 0L]

      # Unknown/not stated
      dt[detail_age_num >= 9000 | is.na(detail_age_num), age := NA_integer_]
    } else {
      # ICD-8/ICD-9 era (1968-2002): 3 chars
      # Format: first char = unit, chars 2-3 = value (01-99)
      # Unit codes are the SAME for both ICD-8 and ICD-9:
      #   0 = years (under 100)
      #   1 = years 100+
      #   2 = months (infant)
      #   3 = weeks (infant)
      #   4 = days (infant)
      #   5 = hours (infant)
      #   6 = minutes (infant)
      #   9 = not stated
      dt[, age_unit := substr(detail_age, 1, 1)]
      dt[, age_value := suppressWarnings(as.integer(substr(detail_age, 2, 3)))]

      # Years less than 100 (unit = 0)
      dt[age_unit == "0" & !is.na(age_value), age := age_value]

      # Years 100 or more (unit = 1)
      dt[age_unit == "1" & !is.na(age_value), age := 100L + age_value]

      # Months, weeks, days, hours, minutes = age 0 (infants)
      dt[age_unit %in% c("2", "3", "4", "5", "6"), age := 0L]

      # Unknown/not stated (unit = 9)
      dt[age_unit == "9" | is.na(age_value), age := NA_integer_]

      # Clean up temporary columns
      dt[, c("age_unit", "age_value") := NULL]
    }
  } else if ("age" %in% names(dt)) {
    # Simple age format (fallback)
    dt[, age := suppressWarnings(as.integer(age))]
  }

  # Process sex
  dt[, sex := data.table::fifelse(sex %in% c("M", "1"), "male",
                       data.table::fifelse(sex %in% c("F", "2"), "female", NA_character_))]

  # Map cause of death
  if (year >= 1999) {
    dt[, cause := map_icd10_to_cause(ucod)]
  } else if (year >= 1979) {
    dt[, cause := map_icd9_to_cause(ucod)]
  } else {
    # 1968-1978: ICD-8 era
    dt[, cause := map_icd8_to_cause(ucod)]
  }

  # Filter to valid data
  dt <- dt[!is.na(age) & !is.na(sex) & age >= 0 & age <= 119]

  # Aggregate
  result <- dt[, .(deaths = .N), by = .(age, sex, cause)]
  result[, year := year]
  data.table::setcolorder(result, c("year", "age", "sex", "cause", "deaths"))
  data.table::setorder(result, age, sex, cause)

  result
}


#' Fetch deaths for multiple years
#'
#' @description
#' Downloads and aggregates death data for multiple years.
#'
#' @param years Integer vector: years to fetch
#' @param cache_dir Character: directory to cache data
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, age, sex, cause, deaths
#'
#' @export
fetch_nchs_deaths_multi <- function(years, cache_dir = "data/cache/nchs_deaths",
                                     force_download = FALSE) {
  results <- list()

  for (yr in years) {
    cli::cli_alert("Processing mortality year {yr}...")
    tryCatch({
      results[[as.character(yr)]] <- fetch_nchs_deaths_by_age(
        year = yr,
        cache_dir = cache_dir,
        force_download = force_download
      )
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No mortality data retrieved")
  }

  data.table::rbindlist(results, use.names = TRUE)
}

#' Calculate total deaths (all causes combined)
#'
#' @description
#' Aggregates cause-specific deaths to total deaths by age and sex.
#'
#' @param deaths_by_cause data.table with cause-specific deaths
#'
#' @return data.table with columns: year, age, sex, deaths
#'
#' @export
aggregate_deaths_to_total <- function(deaths_by_cause) {
  deaths_by_cause[, .(deaths = sum(deaths)), by = .(year, age, sex)]
}

#' Get available mortality years
#'
#' @description
#' Returns the range of years available from NCHS mortality data with
#' single-year-of-age detail.
#'
#' @return Integer vector of available years
#'
#' @details
#' Note: 1972 data contains only a 50% sample of death records per CDC.
#' This is a known data limitation documented in the NCHS ICD-8 documentation.
#'
#' @export
get_nchs_mortality_years <- function() {
  1968:2023
}

#' Get mortality years with cause of death (ICD-8/9/10)
#'
#' @description
#' Returns years with reliable cause of death coding (all ICD versions).
#'
#' @return Integer vector of years
#'
#' @export
get_mortality_years_with_cause <- function() {
  1968:2023
}

#' Adjust death counts for known sampling limitations
#'
#' @description
#' Adjusts death counts for years with known sampling limitations.
#' Currently only 1972 is affected (50% sample per CDC documentation).
#'
#' @param deaths data.table with columns including year and deaths
#' @param method Character: "weight" (multiply by 2) or "interpolate" (use 1971/1973 average)
#'
#' @return data.table with adjusted death counts for affected years
#'
#' @details
#' The 1972 NCHS mortality file contains only a 50% sample of death records.
#' This is documented in the official NCHS ICD-8 documentation (dt78icd8.pdf).
#'
#' For mortality rate calculations and projections using 2008-2019 data,
#' this adjustment is not needed. Only use this function when analyzing
#' complete historical death counts or long-term trends including 1972.
#'
#' @export
adjust_for_sampling <- function(deaths, method = c("weight", "interpolate")) {
  method <- match.arg(method)

  result <- data.table::copy(deaths)


  if (method == "weight") {
    # Simply double 1972 counts
    result[year == 1972, deaths := deaths * 2L]
  } else if (method == "interpolate") {
    # Use average of 1971 and 1973 for each age/sex/cause combination
    if (!all(c(1971, 1973) %in% result$year)) {
      cli::cli_abort("Interpolation requires 1971 and 1973 data to be present")
    }

    # Get grouping columns (everything except year and deaths)
    group_cols <- setdiff(names(result), c("year", "deaths"))

    # Calculate interpolated values
    interp <- result[year %in% c(1971, 1973),
                     .(deaths = as.integer(mean(deaths))),
                     by = c(group_cols)]
    interp[, year := 1972L]

    # Replace 1972 data
    result <- result[year != 1972]
    result <- data.table::rbindlist(list(result, interp), use.names = TRUE)
    data.table::setorder(result, year)
  }

  result
}

#' Fetch infant deaths with age detail
#'
#' @description
#' Downloads and processes NCHS mortality data to extract infant deaths
#' (age < 1 year) with detailed age information (days, weeks, months).
#' This is needed for accurate q0 (infant mortality) calculation per SSA methodology.
#'
#' @param year Integer: year to fetch (1968-2023)
#' @param cache_dir Character: directory for cached files
#' @param force_download Logical: if TRUE, re-process even if cached
#'
#' @return data.table with columns: year, sex, age_unit, age_value, cause, deaths
#'   where age_unit is one of: "days", "weeks", "months", "hours", "minutes"
#'   and age_value is the numeric value (e.g., 1-364 for days, 1-51 for weeks, 1-11 for months)
#'
#' @details
#' The SSA methodology uses detailed infant death timing for q0 calculation.
#' Deaths are categorized by:
#' - Days (0-27 days, neonatal)
#' - Weeks (not commonly used but available)
#' - Months (1-11 months, post-neonatal)
#'
#' Age unit codes in NCHS files:
#' - ICD-10 era (1999+): 2xxx=months, 4xxx=days, 5xxx=hours, 6xxx=minutes
#' - ICD-8/9 era (1968-1998): 2=months, 3=weeks, 4=days, 5=hours, 6=minutes
#'
#' @export
fetch_nchs_infant_deaths_detail <- function(year, cache_dir = "data/cache/nchs_deaths",
                                             force_download = FALSE) {
  # Validate year
  if (year < 1968 || year > 2023) {
    cli::cli_abort("Year must be between 1968 and 2023")
  }

  # Check for cached data
  cache_file <- file.path(cache_dir, sprintf("infant_deaths_detail_%d.rds", year))
  if (file.exists(cache_file) && !force_download) {
    cli::cli_alert_success("Loading cached infant death detail for {year}")
    return(readRDS(cache_file))
  }

  # Need to read raw mortality file and process differently
  # First, get the file (may already be cached from regular deaths fetch)
  mort_file <- get_cached_mortality_file(year, cache_dir)

  if (is.null(mort_file)) {
    # Need to download
    cli::cli_alert_info("Downloading NCHS mortality data for {year}...")
    mort_file <- download_mortality_file(year, cache_dir)
  }

  # Read the fixed-width data
  cli::cli_alert("Reading mortality file for infant death detail...")
  layout <- get_mortality_file_layout(year)
  raw <- read_mortality_fixed_width(mort_file, layout, year)

  # Process infant deaths with detail
  result <- process_infant_deaths_detail(raw, year)

  # Cache and return
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached infant death detail to {cache_file}")
  cli::cli_alert_success("Retrieved {sum(result$deaths)} infant deaths for {year}")

  result
}

#' Process infant deaths with age detail
#'
#' @param dt data.table from read_mortality_fixed_width
#' @param year Data year
#'
#' @return data.table with year, sex, age_unit, age_value, cause, deaths
#'
#' @keywords internal
process_infant_deaths_detail <- function(dt, year) {
  cli::cli_alert("Processing infant death detail...")

  # Extract age unit and value, keeping only infant deaths
  if (year >= 2003) {
    # ICD-10 era: 4-char detail_age format
    # 1xxx = years, 2xxx = months, 4xxx = days, 5xxx = hours, 6xxx = minutes
    dt[, detail_age_num := suppressWarnings(as.integer(detail_age))]

    # Filter to infants only (not years)
    infants <- dt[detail_age_num >= 2000 & detail_age_num < 7000]

    # Extract unit and value
    infants[detail_age_num >= 2000 & detail_age_num < 3000, `:=`(
      age_unit = "months",
      age_value = detail_age_num - 2000L
    )]
    infants[detail_age_num >= 4000 & detail_age_num < 5000, `:=`(
      age_unit = "days",
      age_value = detail_age_num - 4000L
    )]
    infants[detail_age_num >= 5000 & detail_age_num < 6000, `:=`(
      age_unit = "hours",
      age_value = detail_age_num - 5000L
    )]
    infants[detail_age_num >= 6000 & detail_age_num < 7000, `:=`(
      age_unit = "minutes",
      age_value = detail_age_num - 6000L
    )]

  } else {
    # ICD-8/9 era: 3-char format
    # Unit: 2=months, 3=weeks, 4=days, 5=hours, 6=minutes
    dt[, age_unit_code := substr(detail_age, 1, 1)]
    dt[, age_value := suppressWarnings(as.integer(substr(detail_age, 2, 3)))]

    # Filter to infants only
    infants <- dt[age_unit_code %in% c("2", "3", "4", "5", "6")]

    # Map unit codes to labels
    infants[age_unit_code == "2", age_unit := "months"]
    infants[age_unit_code == "3", age_unit := "weeks"]
    infants[age_unit_code == "4", age_unit := "days"]
    infants[age_unit_code == "5", age_unit := "hours"]
    infants[age_unit_code == "6", age_unit := "minutes"]
  }

  # Process sex
  infants[, sex := data.table::fifelse(sex %in% c("M", "1"), "male",
                         data.table::fifelse(sex %in% c("F", "2"), "female", NA_character_))]

  # Map cause of death
  if (year >= 1999) {
    infants[, cause := map_icd10_to_cause(ucod)]
  } else if (year >= 1979) {
    infants[, cause := map_icd9_to_cause(ucod)]
  } else {
    infants[, cause := map_icd8_to_cause(ucod)]
  }

  # Filter valid data and aggregate
  infants <- infants[!is.na(sex) & !is.na(age_unit) & !is.na(age_value)]

  result <- infants[, .(deaths = .N), by = .(sex, age_unit, age_value, cause)]
  result[, year := year]
  data.table::setcolorder(result, c("year", "sex", "age_unit", "age_value", "cause", "deaths"))
  data.table::setorder(result, sex, age_unit, age_value, cause)

  result
}

#' Get cached mortality file path
#'
#' @param year Year
#' @param cache_dir Cache directory
#'
#' @return File path if cached, NULL otherwise
#'
#' @keywords internal
get_cached_mortality_file <- function(year, cache_dir) {
  # Check for extracted mortality file in extract subdirectory
  extract_dir <- file.path(cache_dir, sprintf("mort%d_extract", year))

  if (dir.exists(extract_dir)) {
    # Find the data file in extract directory
    data_files <- list.files(extract_dir, full.names = TRUE, recursive = TRUE)
    data_files <- data_files[!grepl("\\.(pdf|docx?|rds)$", data_files, ignore.case = TRUE)]

    # Filter to large files (actual data files)
    file_sizes <- file.size(data_files)
    large_files <- data_files[file_sizes > 50 * 1024^2]
    if (length(large_files) > 0) {
      return(large_files[1])
    }
    if (length(data_files) > 0) {
      return(data_files[1])
    }
  }

  NULL
}

#' Download mortality file
#'
#' @param year Year to download
#' @param cache_dir Cache directory
#'
#' @return Path to downloaded/extracted file
#'
#' @keywords internal
download_mortality_file <- function(year, cache_dir) {
  # Mirror the extraction logic from fetch_nchs_deaths_by_age

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Build URL based on year
  if (year >= 1968 && year <= 2023) {
    url <- sprintf("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/mort%dus.zip", year)
  } else {
    cli::cli_abort("Year {year} not supported")
  }

  # Download ZIP file
  temp_zip <- tempfile(fileext = ".zip")

  old_timeout <- getOption("timeout")
  options(timeout = 1800)  # 30 minutes
  on.exit(options(timeout = old_timeout), add = TRUE)

  tryCatch({
    download.file(url, temp_zip, mode = "wb", quiet = FALSE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download NCHS mortality data for {year}",
      "x" = conditionMessage(e)
    ))
  })

  # Extract to persistent directory (not temp)
  extract_dir <- file.path(cache_dir, sprintf("mort%d_extract", year))
  if (!dir.exists(extract_dir)) {
    dir.create(extract_dir, recursive = TRUE)
  }

  cli::cli_alert("Extracting ZIP file...")

  # Try R's unzip first, fall back to system unzip for large files
  unzip_result <- tryCatch({
    utils::unzip(temp_zip, exdir = extract_dir)
    TRUE
  }, warning = function(w) {
    FALSE
  }, error = function(e) {
    FALSE
  })

  # If R's unzip failed, try system unzip
  if (!unzip_result || length(list.files(extract_dir)) == 0) {
    cli::cli_alert("Using system unzip for large file...")
    system2("unzip", args = c("-o", "-q", temp_zip, "-d", extract_dir),
            stdout = FALSE, stderr = FALSE)
  }

  # Clean up ZIP file
  unlink(temp_zip)

  # Find the data file
  data_files <- list.files(extract_dir, full.names = TRUE, recursive = TRUE)
  # Exclude documentation files
  data_files <- data_files[!grepl("\\.(pdf|docx?)$", data_files, ignore.case = TRUE)]

  # Filter to large files (actual data files are > 50MB)
  file_sizes <- file.size(data_files)
  large_files <- data_files[file_sizes > 50 * 1024^2]
  if (length(large_files) > 0) {
    data_files <- large_files
  }

  if (length(data_files) == 0) {
    cli::cli_abort("No data file found in ZIP for year {year}")
  }

  data_files[1]
}

#' Fetch infant deaths detail for multiple years
#'
#' @param years Integer vector of years
#' @param cache_dir Cache directory
#' @param force_download Force re-processing
#'
#' @return data.table with infant death detail for all years
#'
#' @export
fetch_nchs_infant_deaths_detail_multi <- function(years, cache_dir = "data/cache/nchs_deaths",
                                                   force_download = FALSE) {
  results <- list()

  for (yr in years) {
    cli::cli_alert("Processing infant deaths for {yr}...")
    tryCatch({
      results[[as.character(yr)]] <- fetch_nchs_infant_deaths_detail(
        year = yr,
        cache_dir = cache_dir,
        force_download = force_download
      )
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No infant death data retrieved")
  }

  data.table::rbindlist(results, use.names = TRUE)
}

#' Aggregate infant deaths to standard age groups
#'
#' @description
#' Aggregates detailed infant deaths to standard age groups used in mortality calculations:
#' - Neonatal (0-27 days)
#' - Post-neonatal (28-364 days, or 1-11 months)
#'
#' @param infant_detail data.table from fetch_nchs_infant_deaths_detail
#'
#' @return data.table with columns: year, sex, age_group, cause, deaths
#'
#' @export
aggregate_infant_deaths_to_groups <- function(infant_detail) {
  result <- data.table::copy(infant_detail)

  # Convert all to days for grouping
  result[age_unit == "minutes", age_days := 0L]
  result[age_unit == "hours", age_days := 0L]
  result[age_unit == "days", age_days := age_value]
  result[age_unit == "weeks", age_days := age_value * 7L]
  result[age_unit == "months", age_days := age_value * 30L]  # Approximate

  # Classify into neonatal/post-neonatal
  result[age_days <= 27, age_group := "neonatal"]
  result[age_days > 27, age_group := "post_neonatal"]

  # Aggregate
  agg <- result[, .(deaths = sum(deaths)), by = .(year, sex, age_group, cause)]
  data.table::setorder(agg, year, sex, age_group, cause)

  agg
}


#' Fetch NCHS deaths by marital status
#'
#' @description
#' Downloads and processes NCHS mortality microdata to extract deaths
#' by single year of age, sex, and marital status. Used for calculating
#' mortality differentials per TR2025 methodology.
#'
#' @param years Integer vector of years (1979-2022). Marital status not available before 1979.
#' @param cache_dir Character: cache directory
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, age, sex, marital_status, deaths
#'
#' @details
#' Marital status codes in NCHS files:
#' - 2003-2022: S=Never married, M=Married, W=Widowed, D=Divorced, U=Unknown
#' - 1979-2002: 1=Never married, 2=Married, 3=Widowed, 4=Divorced, 8/9=Unknown
#'
#' Deaths with unknown/not stated marital status are excluded from the output.
#'
#' @export
fetch_nchs_deaths_by_marital_status <- function(
    years = 2015:2019,
    cache_dir = here::here("data/cache/nchs_deaths"),
    force_download = FALSE
) {
  # Validate years - marital status only available from 1979
  if (any(years < 1979)) {
    cli::cli_abort("Marital status not available before 1979. Years must be >= 1979.")
  }
  if (any(years > 2022)) {
    cli::cli_abort("Marital status data currently available through 2022.")
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  results <- list()

  for (yr in years) {
    # Check for cached results
    cache_file <- file.path(cache_dir, sprintf("deaths_by_marital_%d.rds", yr))

    if (file.exists(cache_file) && !force_download) {
      cli::cli_alert_success("Loading cached marital status deaths for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert_info("Processing deaths by marital status for {yr}...")

    # Get or download the mortality file
    mort_file <- get_cached_mortality_file(yr, cache_dir)

    if (is.null(mort_file)) {
      cli::cli_alert_info("Downloading NCHS mortality data for {yr}...")
      mort_file <- download_mortality_file(yr, cache_dir)
    }

    # Read fixed-width data with marital status
    layout <- get_mortality_file_layout(yr)
    raw <- read_mortality_fixed_width(mort_file, layout, yr)

    # Process the data
    result <- process_mortality_data_by_marital(raw, yr)

    # Cache and store
    saveRDS(result, cache_file)
    cli::cli_alert_success("Cached marital status deaths for {yr}: {format(sum(result$deaths), big.mark=',')} deaths")
    results[[as.character(yr)]] <- result
  }

  if (length(results) == 0) {
    cli::cli_abort("No mortality data by marital status retrieved")
  }

  data.table::rbindlist(results, use.names = TRUE)
}


#' Process mortality data by marital status
#'
#' @param dt data.table from read_mortality_fixed_width
#' @param year Data year
#'
#' @return data.table with year, age, sex, marital_status, deaths
#'
#' @keywords internal
process_mortality_data_by_marital <- function(dt, year) {
  cli::cli_alert("Processing mortality data by marital status...")

  # Check that marital_status column exists
  if (!"marital_status" %in% names(dt)) {
    cli::cli_abort("Marital status column not found. Year {year} may not support marital status.")
  }

  # Process age - same as process_mortality_data
  if (year >= 2003) {
    dt[, detail_age_num := suppressWarnings(as.integer(detail_age))]
    dt[detail_age_num >= 1000 & detail_age_num < 2000, age := detail_age_num - 1000L]
    dt[detail_age_num >= 2000 & detail_age_num < 7000, age := 0L]
    dt[detail_age_num >= 9000 | is.na(detail_age_num), age := NA_integer_]
  } else {
    dt[, age_unit := substr(detail_age, 1, 1)]
    dt[, age_value := suppressWarnings(as.integer(substr(detail_age, 2, 3)))]
    dt[age_unit == "0" & !is.na(age_value), age := age_value]
    dt[age_unit == "1" & !is.na(age_value), age := 100L + age_value]
    dt[age_unit %in% c("2", "3", "4", "5", "6"), age := 0L]
    dt[age_unit == "9" | is.na(age_value), age := NA_integer_]
    dt[, c("age_unit", "age_value") := NULL]
  }

  # Process sex
  dt[, sex := data.table::fifelse(sex %in% c("M", "1"), "male",
                       data.table::fifelse(sex %in% c("F", "2"), "female", NA_character_))]

  # Process marital status
  # Map codes to standard labels
  if (year >= 2003) {
    # 2003+: letter codes
    dt[, marital_status_mapped := data.table::fcase(
      marital_status == "S", "single",
      marital_status == "M", "married",
      marital_status == "W", "widowed",
      marital_status == "D", "divorced",
      default = NA_character_
    )]
  } else {
    # 1979-2002: numeric codes
    dt[, marital_status_mapped := data.table::fcase(
      marital_status == "1", "single",
      marital_status == "2", "married",
      marital_status == "3", "widowed",
      marital_status == "4", "divorced",
      default = NA_character_
    )]
  }

  # Filter to valid data (excluding unknown marital status)
  dt <- dt[!is.na(age) & !is.na(sex) & !is.na(marital_status_mapped) & age >= 0 & age <= 119]

  # Aggregate
  result <- dt[, .(deaths = .N), by = .(age, sex, marital_status = marital_status_mapped)]
  result[, year := year]
  data.table::setcolorder(result, c("year", "age", "sex", "marital_status", "deaths"))
  data.table::setorder(result, age, sex, marital_status)

  result
}

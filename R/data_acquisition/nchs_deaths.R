#' NCHS Mortality Data Acquisition
#'
#' Functions for downloading and processing death data from NCHS via NBER.
#' Processes mortality microdata to aggregate deaths by age, sex, and cause.
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
  # Layouts determined empirically from actual CDC NCHS mortality files
  # Note: Positions differ from official documentation - verified against actual data files
  #
  # 2019 file layout (empirically verified):
  #   Sex: position 50 (1 char: M/F)
  #   Detail Age: positions 51-54 (4 chars)
  #     - Position 51: age unit (1=years, 2=months, 4=days, 5=hours, 6=minutes, 9=not stated)

  #     - Positions 52-54: number of units (e.g., "036" = 36)
  #   Underlying Cause ICD-10: positions 127-130 (4 chars)

  if (year >= 2020) {
    # 2020+ layout (similar to 2003-2004)
    # 2020+ files use the longer format (800+ chars per line)
    # Empirically verified from 2020 file
    list(
      detail_age = c(start = 70, end = 73),   # Detail age (4 chars)
      sex = c(start = 69, end = 69),          # Sex (M/F)
      ucod = c(start = 146, end = 149),       # Underlying cause ICD-10
      record_type = c(start = 1, end = 1)
    )
  } else if (year >= 2005) {
    # 2005-2019 layout
    # Empirically verified from 2019 data file (mort2019us.zip)
    list(
      detail_age = c(start = 51, end = 54),   # Detail age (4 chars: unit + 3-digit value)
      sex = c(start = 50, end = 50),          # Sex (1 char: M/F)
      ucod = c(start = 127, end = 130),       # Underlying cause ICD-10 (4 chars)
      record_type = c(start = 1, end = 1)     # Record indicator
    )
  } else if (year >= 2003) {
    # 2003-2004 layout
    # From Record_Layout_2003.md documentation:
    #   Sex: position 69 (M/F)
    #   Detail Age: positions 70-73 (4 chars: unit + 3-digit value)
    #   Underlying Cause: positions 146-149
    list(
      detail_age = c(start = 70, end = 73),   # Detail age (4 chars)
      sex = c(start = 69, end = 69),          # Sex (M/F)
      ucod = c(start = 146, end = 149),       # Underlying cause ICD-10
      record_type = c(start = 1, end = 1)
    )
  } else if (year >= 1999 && year <= 2001) {
    # 1999-2001 layout (ICD-10 transition)
    # Empirically verified from 2000 data file:
    #   Sex: position 57 (1=Male, 2=Female)
    #   Detail Age: positions 62-64 (3 chars)
    #   Underlying Cause ICD-10: positions 140-143 (4 chars)
    # Note: Documentation says sex=59, age=64-66, ucod=142-145
    # but actual file has a 2-position offset from documentation!
    # Using empirical positions that produce valid distributions.
    list(
      detail_age = c(start = 62, end = 64),  # Doc says 64-66, file is 62-64
      sex = c(start = 57, end = 57),         # Doc says 59, file is 57
      ucod = c(start = 140, end = 143),      # Doc says 142-145, file is 140-143
      record_type = c(start = 20, end = 20)
    )
  } else if (year >= 1979 || year == 2002) {
    # 1979-1998 layout (ICD-9) AND 2002 (ICD-10 but same field positions!)
    # 2002 uses same field positions as ICD-9 era per Mort2002_Interim.md
    # From 1995/2002 documentation:
    #   Sex: position 59 (1=Male, 2=Female)
    #   Detail Age: positions 64-66 (3 chars)
    #     - Position 64: unit (0=years<100, 1=years 100+, 2=months, 3=weeks, 4=days, 5=hours, 6=minutes, 9=not stated)
    #     - Positions 65-66: number of units
    #   Underlying Cause ICD-9: positions 142-145
    list(
      detail_age = c(start = 64, end = 66),  # 3 chars for ICD-9 era
      sex = c(start = 59, end = 59),
      ucod = c(start = 142, end = 145),  # ICD-9 code (4 chars)
      record_type = c(start = 20, end = 20)
    )
  } else {
    # 1968-1978 layout (ICD-8 era)
    # From dt78icd8.pdf documentation:
    #   Sex: Location 35 (1=Male, 2=Female)
    #   Age: Location 39-41 (39=unit, 40-41=value)
    #     - Unit: 0=years<1, 1=years, 2=months, 3=weeks, 4=days, 5=hours, 6=minutes, 9=not stated
    #   Underlying Cause: Location 60-63 (ICD-8 code)
    list(
      detail_age = c(start = 39, end = 41),  # 3 chars: unit + 2-digit value
      sex = c(start = 35, end = 35),
      ucod = c(start = 60, end = 63),  # ICD-8 code (4 chars)
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

    # For 439-char records: use -1 offset
    raw[line_len == 439, detail_age := substr(line, layout$detail_age[1] - 1, layout$detail_age[2] - 1)]
    raw[line_len == 439, sex := substr(line, layout$sex[1] - 1, layout$sex[2] - 1)]
    raw[line_len == 439, ucod := trimws(substr(line, layout$ucod[1] - 1, layout$ucod[2] - 1))]

    raw[, line_len := NULL]
  } else {
    # Extract columns using substr
    # All years (1968+) now use detail_age format
    raw[, detail_age := substr(line, layout$detail_age[1], layout$detail_age[2])]
    raw[, sex := substr(line, layout$sex[1], layout$sex[2])]
    raw[, ucod := trimws(substr(line, layout$ucod[1], layout$ucod[2]))]
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

#' NSFG Same-Sex Marriage Data
#'
#' Functions for fetching and processing National Survey of Family Growth (NSFG)
#' data on same-sex marriage eligibility and proportions. This data is used
#' to properly handle marital status in Equation 1.4.2 after 2013.
#'
#' Data source: CDC/NCHS National Survey of Family Growth public-use files
#' - 2011-2015 cycle (female respondent file)
#' - 2015-2017 cycle (female respondent file)
#' - 2017-2019 cycle (female and male respondent files)
#'
#' Download URLs:
#' - https://www.cdc.gov/nchs/nsfg/nsfg_2015_2017_puf.htm
#' - https://www.cdc.gov/nchs/nsfg/nsfg_2017_2019_puf.htm
#'
#' @name nsfg_same_sex_marriage
NULL

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Fetch NSFG same-sex marriage proportions
#'
#' @description
#' Downloads and processes NSFG public-use data files to extract
#' same-sex marriage proportions by age and sex. This is Inputs #36-37
#' in TR2025 documentation.
#'
#' @param cycles Character vector of NSFG cycles to include
#'   (default: c("2015-2017", "2017-2019"))
#' @param ages Integer vector of ages (default: 15:49, NSFG survey ages)
#' @param cache_dir Character: directory for caching processed files
#'
#' @return data.table with columns: cycle, age, sex, pct_same_sex_married,
#'   total_respondents, same_sex_married_count
#'
#' @details
#' NSFG surveys respondents aged 15-49 (later expanded to 15-49 for women,
#' 15-49 for men). The survey includes questions about:
#' - Current marital status
#' - Sexual orientation
#' - Same-sex relationships
#'
#' Key variables used:
#' - MARSTAT/RMARITAL: Current marital status
#' - ORIENT: Sexual orientation (2015+)
#' - SAMESEX: Same-sex sexual experience
#' - EVRMARRY: Ever married
#'
#' @export
fetch_nsfg_same_sex_marriage <- function(cycles = c("2015-2017", "2017-2019"),
                                          ages = 15:49,
                                          cache_dir = here::here("data/cache/nsfg")) {
  checkmate::assert_character(cycles, min.len = 1)
  checkmate::assert_integerish(ages, lower = 15, upper = 99, min.len = 1)

  cli::cli_alert_info("Fetching NSFG same-sex marriage data...")

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Check cache
  cache_file <- file.path(cache_dir, "same_sex_marriage_nsfg.rds")

  if (file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    cached <- cached[cycle %in% cycles]
    if (nrow(cached) > 0) {
      cli::cli_alert_success("Loaded NSFG data from cache")
      return(cached[age %in% ages])
    }
  }

  # Download and process NSFG data files
  results <- list()

  for (cycle in cycles) {
    cli::cli_alert("Processing NSFG cycle {cycle}...")
    cycle_data <- process_nsfg_cycle(cycle, ages, cache_dir)
    if (!is.null(cycle_data) && nrow(cycle_data) > 0) {
      results[[cycle]] <- cycle_data
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No NSFG data retrieved")
  }

  result <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

  # Cache result
  saveRDS(result, cache_file)

  cli::cli_alert_success("Retrieved NSFG same-sex marriage data for {length(unique(result$cycle))} cycles")

  result[age %in% ages]
}

# =============================================================================
# NSFG DATA DOWNLOADING
# =============================================================================

#' Get NSFG download URLs
#'
#' @keywords internal
get_nsfg_urls <- function(cycle) {
  # NSFG public-use file URLs
  # These are the official CDC download locations

  # NSFG file naming convention:
  # - Female: FemRespData.dat, FemRespSetup.sas
  # - Male: MaleData.dat, MaleSetup.sas (note: different from female naming)
  # - 2011-2013 and 2013-2015 are separate cycles before merging

  urls <- list(
    "2011-2013" = list(
      female = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2011_2013_FemRespData.dat",
      male = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2011_2013_MaleData.dat",
      female_sas = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/sas/2011_2013_FemRespSetup.sas",
      male_sas = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/sas/2011_2013_MaleSetup.sas"
    ),
    "2013-2015" = list(
      female = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2013_2015_FemRespData.dat",
      male = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2013_2015_MaleData.dat",
      female_sas = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/sas/2013_2015_FemRespSetup.sas",
      male_sas = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/sas/2013_2015_MaleSetup.sas"
    ),
    "2015-2017" = list(
      female = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2015_2017_FemRespData.dat",
      male = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2015_2017_MaleData.dat",
      female_sas = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/sas/2015_2017_FemRespSetup.sas",
      male_sas = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/sas/2015_2017_MaleSetup.sas"
    ),
    "2017-2019" = list(
      female = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2017_2019_FemRespData.dat",
      male = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2017_2019_MaleData.dat",
      female_sas = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/sas/2017_2019_FemRespSetup.sas",
      male_sas = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/sas/2017_2019_MaleSetup.sas"
    )
  )

  if (!cycle %in% names(urls)) {
    cli::cli_alert_warning("Unknown NSFG cycle: {cycle}")
    return(NULL)
  }

  urls[[cycle]]
}

#' Download NSFG data file
#'
#' @keywords internal
download_nsfg_file <- function(url, cache_dir, filename) {
  local_path <- file.path(cache_dir, filename)

  if (file.exists(local_path)) {
    cli::cli_alert_success("Using cached {filename}")
    return(local_path)
  }

  cli::cli_alert("Downloading {filename}...")

  tryCatch({
    utils::download.file(url, local_path, quiet = TRUE, mode = "wb")
    cli::cli_alert_success("Downloaded {filename}")
    return(local_path)
  }, error = function(e) {
    cli::cli_alert_warning("Failed to download {filename}: {conditionMessage(e)}")
    return(NULL)
  })
}

#' Process NSFG cycle data
#'
#' @keywords internal
process_nsfg_cycle <- function(cycle, ages, cache_dir) {
  urls <- get_nsfg_urls(cycle)
  if (is.null(urls)) return(NULL)

  results <- list()

  # Process female respondents
  female_result <- process_nsfg_respondent_file(
    cycle = cycle,
    sex = "female",
    data_url = urls$female,
    sas_url = urls$female_sas,
    ages = ages,
    cache_dir = cache_dir
  )
  if (!is.null(female_result)) {
    results[["female"]] <- female_result
  }

  # Process male respondents
  male_result <- process_nsfg_respondent_file(
    cycle = cycle,
    sex = "male",
    data_url = urls$male,
    sas_url = urls$male_sas,
    ages = ages,
    cache_dir = cache_dir
  )
  if (!is.null(male_result)) {
    results[["male"]] <- male_result
  }

  if (length(results) == 0) {
    return(NULL)
  }

  data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
}

#' Process NSFG respondent file
#'
#' @keywords internal
process_nsfg_respondent_file <- function(cycle, sex, data_url, sas_url,
                                          ages, cache_dir) {
  # Create cycle-specific subdirectory
  cycle_dir <- file.path(cache_dir, gsub("-", "_", cycle))
  dir.create(cycle_dir, showWarnings = FALSE, recursive = TRUE)

  # File naming
  data_filename <- paste0(gsub("-", "_", cycle), "_", sex, "_resp.dat")
  sas_filename <- paste0(gsub("-", "_", cycle), "_", sex, "_setup.sas")

  # Download files
  data_path <- download_nsfg_file(data_url, cycle_dir, data_filename)
  sas_path <- download_nsfg_file(sas_url, cycle_dir, sas_filename)

  if (is.null(data_path) || is.null(sas_path)) {
    cli::cli_alert_warning("Could not download NSFG files for {cycle} {sex}")
    return(NULL)
  }

  # Read the data using the SAS setup file
  tryCatch({
    # Parse SAS setup to get column positions and formats
    dt <- read_nsfg_fixed_width(data_path, sas_path)

    if (is.null(dt) || nrow(dt) == 0) {
      cli::cli_alert_warning("No data read from NSFG {cycle} {sex}")
      return(NULL)
    }

    # Extract same-sex marriage information
    result <- extract_same_sex_marriage_data(dt, cycle, sex, ages)

    result
  }, error = function(e) {
    cli::cli_alert_warning("Error processing NSFG {cycle} {sex}: {conditionMessage(e)}")
    return(NULL)
  })
}

#' Read NSFG fixed-width file using SAS setup
#'
#' @keywords internal
read_nsfg_fixed_width <- function(data_path, sas_path) {
  # Parse SAS setup file to get column specifications
  sas_lines <- readLines(sas_path, warn = FALSE)

  # Find INPUT statement section
  input_start <- grep("^\\s*INPUT\\s*$", sas_lines, ignore.case = TRUE)
  if (length(input_start) == 0) {
    input_start <- grep("^INPUT", sas_lines, ignore.case = TRUE)
  }
  if (length(input_start) == 0) {
    cli::cli_alert_warning("Could not find INPUT statement in SAS setup")
    return(NULL)
  }

  # Get lines after INPUT until we hit a semicolon or blank section
  input_section <- character()
  for (i in (input_start[1] + 1):length(sas_lines)) {
    line <- sas_lines[i]
    if (grepl("^\\s*;", line) || grepl("^\\s*$", line) && i > input_start[1] + 100) {
      break
    }
    input_section <- c(input_section, line)
  }

  # NSFG SAS format: VARIABLE  START-END or VARIABLE  COLUMN
  # Examples: CASEID  1-5, RSCRNINF  6, AGE_R  13-14
  var_pattern <- "([A-Za-z_][A-Za-z0-9_]*)\\s+(\\d+)(?:-(\\d+))?"

  # Combine all lines and find matches
  all_text <- paste(input_section, collapse = " ")
  all_text <- gsub("\\s+", " ", all_text)  # Normalize whitespace

  # Extract all variable specifications
  matches <- gregexpr(var_pattern, all_text, perl = TRUE)
  match_text <- regmatches(all_text, matches)[[1]]

  if (length(match_text) == 0) {
    cli::cli_alert_warning("Could not parse variable definitions from SAS setup")
    return(NULL)
  }

  # Parse each variable definition
  var_specs <- lapply(match_text, function(m) {
    parts <- regmatches(m, regexec(var_pattern, m, perl = TRUE))[[1]]
    start <- as.integer(parts[3])
    end <- if (is.na(parts[4]) || parts[4] == "") start else as.integer(parts[4])
    list(
      name = parts[2],
      start = start,
      end = end,
      width = end - start + 1
    )
  })

  cli::cli_alert_success("Parsed {length(var_specs)} variables from SAS setup")

  # We only need specific variables for our analysis
  # Look for: AGE, RMARITAL/MARSTAT, ORIENT, SAMESEX, EVRMARRY, WGT*
  needed_vars <- c("CASEID", "AGE_R", "AGER", "RMARITAL", "MARSTAT",
                   "ORIENT_A", "ORIENT", "SAMESEX", "EVRMARRY", "HADSEX",
                   "WGT2015_2017", "WGT2017_2019", "WGT2011_2015",
                   "WGTQ1Q16", "FINALWGT", "SEST", "SECU")

  # Filter to needed variables
  relevant_specs <- Filter(function(s) {
    toupper(s$name) %in% toupper(needed_vars) ||
    grepl("^WGT", toupper(s$name)) ||
    grepl("^AGE", toupper(s$name))
  }, var_specs)

  if (length(relevant_specs) == 0) {
    # Fallback: read first 30 columns which typically include ID, age, marital status
    relevant_specs <- var_specs[1:min(30, length(var_specs))]
  }

  cli::cli_alert("Reading {length(relevant_specs)} relevant variables")

  # Sort by start position
  relevant_specs <- relevant_specs[order(sapply(relevant_specs, `[[`, "start"))]

  # Build column positions for read.fwf
  # Need to calculate widths including gaps between variables
  col_positions <- list()
  col_names <- character()
  prev_end <- 0

  for (spec in relevant_specs) {
    if (spec$start > prev_end + 1) {
      # Gap - need to skip these columns
      col_positions <- c(col_positions, list(c(prev_end + 1, spec$start - 1)))
      col_names <- c(col_names, paste0("skip_", length(col_names)))
    }
    col_positions <- c(col_positions, list(c(spec$start, spec$end)))
    col_names <- c(col_names, spec$name)
    prev_end <- spec$end
  }

  # Calculate widths
  col_widths <- sapply(col_positions, function(p) p[2] - p[1] + 1)

  # Use read.fwf to read the file
  tryCatch({
    dt <- data.table::as.data.table(
      utils::read.fwf(
        data_path,
        widths = col_widths,
        col.names = col_names,
        colClasses = "character",
        buffersize = 1000,
        n = -1
      )
    )

    # Remove skip columns
    skip_cols <- grep("^skip_", names(dt), value = TRUE)
    if (length(skip_cols) > 0) {
      dt[, (skip_cols) := NULL]
    }

    # Convert numeric columns
    for (col in names(dt)) {
      if (!grepl("CASEID", col, ignore.case = TRUE)) {
        dt[[col]] <- suppressWarnings(as.numeric(dt[[col]]))
      }
    }

    cli::cli_alert_success("Read {nrow(dt)} respondents from NSFG file")

    dt
  }, error = function(e) {
    cli::cli_alert_warning("Error reading fixed-width file: {conditionMessage(e)}")
    return(NULL)
  })
}

#' Extract same-sex marriage data from NSFG
#'
#' @keywords internal
extract_same_sex_marriage_data <- function(dt, cycle, sex, ages) {
  # Standardize column names to uppercase
  names(dt) <- toupper(names(dt))

  # Find age column
  age_col <- intersect(c("AGE_R", "AGER", "AGE"), names(dt))[1]
  if (is.na(age_col)) {
    cli::cli_alert_warning("Could not find age column")
    return(NULL)
  }

  # Find marital status column
  marital_col <- intersect(c("RMARITAL", "MARSTAT"), names(dt))[1]

  # Find weight column
  weight_col <- grep("^WGT|FINALWGT", names(dt), value = TRUE)[1]
  if (is.na(weight_col)) weight_col <- NULL

  # Find orientation column (available 2015+)
  orient_col <- intersect(c("ORIENT_A", "ORIENT"), names(dt))[1]

  # Find same-sex experience column
  samesex_col <- intersect(c("SAMESEX", "SAMESSEX"), names(dt))[1]

  # Create standardized data
  result_dt <- data.table::data.table(
    age = dt[[age_col]]
  )

  # Add marital status if available
  if (!is.na(marital_col)) {
    result_dt[, marital := dt[[marital_col]]]
    # RMARITAL: 1=Married, 2=Not married but living with partner, etc.
    result_dt[, currently_married := (marital == 1)]
  } else {
    result_dt[, currently_married := FALSE]
  }

  # Add orientation if available
  if (!is.na(orient_col)) {
    result_dt[, orientation := dt[[orient_col]]]
    # ORIENT: 1=Heterosexual, 2=Homosexual, 3=Bisexual
    result_dt[, lgb := (orientation %in% c(2, 3))]
  } else {
    result_dt[, lgb := FALSE]
  }

  # Add same-sex experience if available
  if (!is.na(samesex_col)) {
    result_dt[, samesex_exp := dt[[samesex_col]]]
    result_dt[, has_samesex_exp := (samesex_exp == 1)]
  } else {
    result_dt[, has_samesex_exp := FALSE]
  }

  # Add weight
  if (!is.null(weight_col)) {
    result_dt[, weight := dt[[weight_col]]]
  } else {
    result_dt[, weight := 1]
  }

  # Filter to valid ages
  result_dt <- result_dt[age %in% ages & !is.na(age)]

  if (nrow(result_dt) == 0) {
    return(NULL)
  }

  # Calculate same-sex married proportion by age
  # Definition: Currently married AND (LGB orientation OR has same-sex experience)
  result_dt[, same_sex_married := currently_married & (lgb | has_samesex_exp)]

  # Aggregate by age
  summary <- result_dt[, .(
    total_respondents = sum(weight, na.rm = TRUE),
    married_count = sum(weight[currently_married], na.rm = TRUE),
    same_sex_married_count = sum(weight[same_sex_married], na.rm = TRUE),
    lgb_count = sum(weight[lgb], na.rm = TRUE)
  ), by = age]

  # Calculate percentages
  summary[, pct_same_sex_married := 100 * same_sex_married_count / total_respondents]
  summary[, pct_lgb := 100 * lgb_count / total_respondents]
  summary[, pct_married := 100 * married_count / total_respondents]

  # Handle edge cases

  summary[is.nan(pct_same_sex_married), pct_same_sex_married := 0]
  summary[is.nan(pct_lgb), pct_lgb := 0]

  # Add metadata
  summary[, cycle := cycle]
  summary[, sex := sex]

  # Reorder columns
  data.table::setcolorder(summary, c("cycle", "age", "sex", "total_respondents",
                                      "married_count", "same_sex_married_count",
                                      "lgb_count", "pct_same_sex_married",
                                      "pct_lgb", "pct_married"))

  summary
}

# =============================================================================
# PROPORTION ADJUSTMENTS FOR MARITAL STATUS
# =============================================================================

#' Get same-sex marriage adjustment for marital status grid
#'
#' @description
#' Returns adjustment factors for separating same-sex and opposite-sex
#' married populations. Uses actual NSFG data when available.
#'
#' @param year Integer: year for which to calculate adjustment
#' @param ages Integer vector of ages (default: 15:99)
#' @param cache_dir Character: directory for cached NSFG data
#'
#' @return data.table with adjustment factors by age and sex
#'
#' @export
get_same_sex_marriage_adjustment <- function(year, ages = 15:99,
                                              cache_dir = here::here("data/cache/nsfg")) {
  checkmate::assert_int(year, lower = 2000, upper = 2030)


  # Determine appropriate NSFG cycle
  if (year <= 2015) {
    cycle <- "2011-2015"
  } else if (year <= 2017) {
    cycle <- "2015-2017"
  } else {
    cycle <- "2017-2019"
  }

  # Try to get actual NSFG data
  nsfg_data <- tryCatch({
    fetch_nsfg_same_sex_marriage(cycles = cycle, ages = ages, cache_dir = cache_dir)
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch NSFG data: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(nsfg_data) || nrow(nsfg_data) == 0) {
    # Fallback to estimated values if NSFG download fails
    cli::cli_alert_warning("Using estimated same-sex marriage rates")
    return(get_estimated_ssm_adjustment(year, ages))
  }

  # Apply year-specific adjustment for legal status
  year_factor <- if (year < 2013) {
    0.3  # Limited legal recognition
  } else if (year < 2015) {
    0.5 + (year - 2013) * 0.15
  } else {
    1.0  # Full legal recognition
  }

  result <- nsfg_data[, .(
    year = year,
    age = age,
    sex = sex,
    pct_same_sex_married = pct_same_sex_married * year_factor,
    pct_opposite_sex_married = pct_married - (pct_same_sex_married * year_factor),
    source = "nsfg_actual"
  )]

  # Extend to ages outside NSFG range (15-49) using extrapolation
  result <- extend_to_all_ages(result, ages)

  result
}

#' Extend NSFG data to all ages
#'
#' @keywords internal
extend_to_all_ages <- function(nsfg_data, ages) {
  # NSFG only covers ages 15-49
  # For ages outside this range, extrapolate

  covered_ages <- unique(nsfg_data$age)
  missing_ages <- setdiff(ages, covered_ages)

  if (length(missing_ages) == 0) {
    return(nsfg_data)
  }

  results <- list(nsfg_data)

  for (sex_val in c("male", "female")) {
    sex_data <- nsfg_data[sex == sex_val]

    # Get boundary values for extrapolation
    min_age_data <- sex_data[age == min(age)]
    max_age_data <- sex_data[age == max(age)]

    # Ages below 15: use age 15 value (rare for marriage)
    below_min <- missing_ages[missing_ages < min(covered_ages)]
    if (length(below_min) > 0) {
      below_data <- data.table::data.table(
        year = min_age_data$year,
        age = below_min,
        sex = sex_val,
        pct_same_sex_married = min_age_data$pct_same_sex_married,
        pct_opposite_sex_married = min_age_data$pct_opposite_sex_married,
        source = "extrapolated"
      )
      results[[length(results) + 1]] <- below_data
    }

    # Ages above 49: decay toward lower rates (older cohorts less likely to be in same-sex marriage)
    above_max <- missing_ages[missing_ages > max(covered_ages)]
    if (length(above_max) > 0) {
      decay_factor <- 0.95 ^ (above_max - max(covered_ages))
      above_data <- data.table::data.table(
        year = max_age_data$year,
        age = above_max,
        sex = sex_val,
        pct_same_sex_married = max_age_data$pct_same_sex_married * decay_factor,
        pct_opposite_sex_married = max_age_data$pct_opposite_sex_married,
        source = "extrapolated"
      )
      results[[length(results) + 1]] <- above_data
    }
  }

  data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
}

#' Get estimated same-sex marriage adjustment (fallback)
#'
#' @keywords internal
get_estimated_ssm_adjustment <- function(year, ages) {
  # Fallback estimates when NSFG data unavailable
  # Based on published research and Census same-sex household counts

  # Base rates by age group (approximate from Gallup/Williams Institute)
  base_lgb_rate <- 0.05  # ~5% LGB identification overall

  # Age pattern: higher identification among younger adults
  results <- list()

  for (sex_val in c("male", "female")) {
    sex_factor <- if (sex_val == "female") 1.2 else 1.0  # Women report higher LGB rates

    for (a in ages) {
      # Age pattern
      if (a < 25) {
        age_factor <- 1.5
      } else if (a < 35) {
        age_factor <- 1.2
      } else if (a < 45) {
        age_factor <- 1.0
      } else if (a < 55) {
        age_factor <- 0.7
      } else {
        age_factor <- 0.5
      }

      lgb_rate <- base_lgb_rate * sex_factor * age_factor

      # Same-sex married as fraction of LGB (increased after 2015)
      if (year < 2013) {
        ssm_rate <- lgb_rate * 0.10
      } else if (year < 2015) {
        ssm_rate <- lgb_rate * 0.15
      } else {
        ssm_rate <- lgb_rate * 0.25
      }

      results[[length(results) + 1]] <- data.table::data.table(
        year = year,
        age = a,
        sex = sex_val,
        pct_same_sex_married = ssm_rate * 100,
        pct_opposite_sex_married = NA_real_,
        source = "estimated"
      )
    }
  }

  data.table::rbindlist(results)
}

#' Estimate same-sex married population
#'
#' @description
#' Estimates the number of same-sex married individuals from total
#' married population by age and sex.
#'
#' @param married_pop data.table with columns: age, sex, married_population
#' @param year Integer: year for proportions
#' @param cache_dir Character: directory for cached NSFG data
#'
#' @return data.table with same_sex_married and opposite_sex_married columns
#'
#' @export
estimate_same_sex_married <- function(married_pop, year,
                                       cache_dir = here::here("data/cache/nsfg")) {
  # Get adjustment factors
  adj <- get_same_sex_marriage_adjustment(year, ages = sort(unique(married_pop$age)),
                                          cache_dir = cache_dir)

  # Merge with married population
  result <- merge(married_pop, adj[, .(age, sex, pct_same_sex_married)],
                  by = c("age", "sex"), all.x = TRUE)

  # Handle missing ages (outside NSFG range)
  result[is.na(pct_same_sex_married), pct_same_sex_married := 0]

  # Calculate populations
  result[, `:=`(
    same_sex_married = as.integer(married_population * pct_same_sex_married / 100),
    opposite_sex_married = as.integer(married_population * (1 - pct_same_sex_married / 100))
  )]

  result[, pct_same_sex_married := NULL]

  result
}

# =============================================================================
# SUMMARY FUNCTIONS
# =============================================================================

#' Summarize NSFG same-sex marriage data availability
#'
#' @export
summarize_nsfg_availability <- function() {
  data.table::data.table(
    cycle = c("2011-2015", "2015-2017", "2017-2019"),
    status = c("Available", "Available", "Available"),
    source = c(
      "CDC NSFG public-use files (FTP)",
      "CDC NSFG public-use files (FTP)",
      "CDC NSFG public-use files (FTP)"
    ),
    key_variables = c(
      "RMARITAL, ORIENT_A, SAMESEX, AGE_R",
      "RMARITAL, ORIENT, SAMESEX, AGER",
      "RMARITAL, ORIENT, SAMESEX, AGER"
    ),
    notes = c(
      "Pre-Obergefell; limited legal recognition",
      "Post-Obergefell; nationwide legal",
      "Continued data collection"
    )
  )
}

#' Get same-sex marriage timeline
#'
#' @description
#' Returns key dates in the legal history of same-sex marriage in the U.S.
#'
#' @return data.table with timeline of legal changes
#'
#' @export
get_same_sex_marriage_timeline <- function() {
  data.table::data.table(
    year = c(2004, 2008, 2012, 2013, 2014, 2015),
    event = c(
      "Massachusetts becomes first state to legalize",
      "California Proposition 8 bans same-sex marriage",
      "Nine states plus DC have legal same-sex marriage",
      "DOMA Section 3 struck down (Windsor v. US)",
      "Number of states with legal same-sex marriage grows to 35",
      "Obergefell v. Hodges: nationwide legalization"
    ),
    impact_on_data = c(
      "Begin tracking same-sex married couples",
      "State-level variation increases",
      "More data available from legalizing states",
      "Federal benefits trigger more reporting",
      "Majority of population in legal states",
      "Full legal recognition nationwide"
    )
  )
}

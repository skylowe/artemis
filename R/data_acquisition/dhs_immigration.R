#' DHS Immigration Data Acquisition
#'
#' Functions for downloading and processing LPR immigration data from DHS.
#' Uses the expanded Tables 8-11 which distinguish between new arrivals
#' and adjustments of status.
#'
#' @name dhs_immigration
NULL

#' Get DHS expanded tables URL for a fiscal year
#'
#' @description
#' Returns the download URL for the expanded Tables 8-11 Excel file
#' for a given fiscal year. These files distinguish between new arrivals
#' and adjustments of status.
#'
#' @param fiscal_year Integer: fiscal year (2006-2023 available)
#'
#' @return Character: full URL to the Excel file
#'
#' @keywords internal
get_dhs_expanded_tables_url <- function(fiscal_year) {
  base_url <- "https://ohss.dhs.gov"

  # URL patterns vary by year
  urls <- list(
    "2023" = "/system/files/2025-09/2025_0828_ohss_tables8-11newadj_fy2023.xlsx",
    "2022" = "/sites/default/files/2023-12/plcy_tables8-11newadj_fy2022_d.xlsx",
    "2021" = "/sites/default/files/2023-12/2202_0405_plcy_tables8-11newadj_fy2021_d.xlsx",
    "2020" = "/sites/default/files/2023-12/2021_0920_plcy_tables8-11newadj_fy2020_d.xlsx",
    "2019" = "/sites/default/files/2023-12/fy2019_tables8-11_newadj_d.xlsx",
    "2018" = "/sites/default/files/2023-12/fy2018_tables8-11newadj_d.xlsx",
    "2017" = "/sites/default/files/2023-12/fy2017_tables8-11newadj_d.xlsx",
    "2016" = "/sites/default/files/2023-12/fy2016_tables8-11newadj_d.xlsx",
    "2015" = "/sites/default/files/2023-12/fy2015_tables8-11newadj_d_0.xlsx",
    "2014" = "/sites/default/files/2023-12/fy2014_tables8-11newadj_d_0.xlsx",
    "2013" = "/sites/default/files/2023-12/fy2013_tables8-11newadj_d.xlsx",
    "2012" = "/sites/default/files/2023-12/fy2012_tables8-11newadj_d.xlsx",
    "2011" = "/sites/default/files/2023-12/fy2011_tables8-11newadj_d.xlsx",
    "2010" = "/sites/default/files/2023-12/fy2010_tables8-11newadj_d.xlsx",
    "2009" = "/sites/default/files/2023-12/fy2009_tables8-11newadj_d.xlsx",
    "2008" = "/sites/default/files/2023-12/fy2008_tables8-11newadj_d.xlsx",
    "2007" = "/sites/default/files/2023-12/fy2007_tables8-11newadj_d.xlsx",
    "2006" = "/sites/default/files/2023-12/fy2006_tables8-11newadj_d.xlsx"
  )

  year_str <- as.character(fiscal_year)
  if (!year_str %in% names(urls)) {
    cli::cli_abort("Expanded tables not available for fiscal year {fiscal_year}. Available: 2006-2023")
  }

  paste0(base_url, urls[[year_str]])
}

#' Download DHS expanded tables Excel file
#'
#' @description
#' Downloads the expanded Tables 8-11 Excel file for a fiscal year.
#' Uses wget with referer header since DHS site blocks automated downloads.
#'
#' @param fiscal_year Integer: fiscal year (2006-2023)
#' @param cache_dir Character: directory to cache downloaded files
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return Character: path to the downloaded file
#'
#' @keywords internal
download_dhs_expanded_tables <- function(fiscal_year,
                                          cache_dir = "data/cache/dhs_immigration",
                                          force_download = FALSE) {
  # Ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Check for cached file
  cache_file <- file.path(cache_dir, sprintf("dhs_tables8-11_fy%d.xlsx", fiscal_year))
  if (file.exists(cache_file) && !force_download) {
    cli::cli_alert_success("Using cached DHS file for FY{fiscal_year}")
    return(cache_file)
  }

  # Download using wget with referer (DHS blocks requests without proper headers)
  url <- get_dhs_expanded_tables_url(fiscal_year)
  referer <- "https://ohss.dhs.gov/topics/immigration/lawful-permanent-residents/lpr-yearbook-tables-8-11-expanded"
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"

  cli::cli_alert_info("Downloading DHS Tables 8-11 for FY{fiscal_year}...")

  # Try wget first (works better with DHS CDN)
  wget_cmd <- sprintf(
    'wget --user-agent="%s" --referer="%s" -q -O "%s" "%s"',
    user_agent, referer, cache_file, url
  )

  result <- tryCatch({
    system(wget_cmd, intern = FALSE, ignore.stderr = TRUE)
  }, error = function(e) {
    # If wget fails, try with curl
    curl_cmd <- sprintf(
      'curl -L -A "%s" -e "%s" -o "%s" "%s"',
      user_agent, referer, cache_file, url
    )
    system(curl_cmd, intern = FALSE, ignore.stderr = TRUE)
  })

  # Verify download succeeded
  if (!file.exists(cache_file) || file.size(cache_file) < 1000) {
    if (file.exists(cache_file)) unlink(cache_file)
    cli::cli_abort(c(
      "Failed to download DHS data for FY{fiscal_year}",
      "i" = "URL: {url}",
      "i" = "DHS may be blocking automated downloads. Try downloading manually."
    ))
  }

  cli::cli_alert_success("Downloaded DHS Tables 8-11 for FY{fiscal_year}")
  cache_file
}

#' Parse DHS Table 8 (demographics by admission type)
#'
#' @description
#' Parses Table 8 from the DHS expanded tables Excel file.
#' Table 8 contains: Age, Marital Status, Occupation
#' broken down by Adjustments of Status and New Arrivals, each with sex breakdown.
#'
#' @param excel_path Character: path to the Excel file
#' @param fiscal_year Integer: fiscal year (for labeling)
#'
#' @return data.table with columns: fiscal_year, category, value,
#'   aos_total, aos_female, aos_male, new_total, new_female, new_male
#'
#' @details
#' Table 8 has two possible structures:
#'
#' **Format 1 (2006-2022):** Single sheet with 9 columns
#' - Column 1: Label/Category
#' - Columns 2-5: Adjustments of Status (Total, Female, Male, Unknown)
#' - Columns 6-9: New Arrivals (Total, Female, Male, Unknown)
#'
#' **Format 2 (2023+):** Separate sheets for New Arrivals and Adjustments
#' - Each sheet has 5 columns: Label, Total, Female, Male, Unknown
#'
#' @keywords internal
parse_dhs_table8 <- function(excel_path, fiscal_year) {
  sheets <- readxl::excel_sheets(excel_path)

  # Check if this is Format 2 (separate sheets)
  has_separate_sheets <- any(grepl("Table 8 New|Table 8 Adjust", sheets, ignore.case = TRUE))

  if (has_separate_sheets) {
    return(parse_dhs_table8_format2(excel_path, fiscal_year))
  }

  # Format 1: Combined sheet
  table8_sheet <- sheets[grepl("Table 8|Table8", sheets, ignore.case = TRUE)]
  if (length(table8_sheet) == 0) {
    table8_sheet <- sheets[1]
    cli::cli_alert_warning("Could not find Table 8 sheet, using: {table8_sheet}")
  } else {
    table8_sheet <- table8_sheet[1]
  }

  # Read the sheet
  raw <- readxl::read_excel(excel_path, sheet = table8_sheet, col_names = FALSE)
  dt <- data.table::as.data.table(raw)

  # Find data start - look for "AGE" row marker
  data_start <- NULL
  for (i in 1:min(20, nrow(dt))) {
    val <- as.character(dt[[1]][i])
    if (!is.na(val) && grepl("^AGE$", val, ignore.case = TRUE)) {
      data_start <- i + 1  # Start after AGE header
      break
    }
  }

  if (is.null(data_start)) {
    cli::cli_abort("Could not find AGE section in Table 8 for FY{fiscal_year}")
  }

  # Parse all data rows
  result <- data.table::data.table(
    fiscal_year = integer(),
    category = character(),
    value = character(),
    aos_total = numeric(),
    aos_female = numeric(),
    aos_male = numeric(),
    new_total = numeric(),
    new_female = numeric(),
    new_male = numeric()
  )

  current_category <- "AGE"
  category_markers <- c("AGE", "BROAD AGE GROUPS", "MARITAL STATUS", "OCCUPATION")

  for (i in data_start:nrow(dt)) {
    # Get values from row
    label <- as.character(dt[[1]][i])
    if (is.na(label) || label == "") next

    # Check if this is a category marker
    label_upper <- toupper(trimws(label))
    if (label_upper %in% category_markers) {
      current_category <- label_upper
      next
    }

    # Skip "Total" rows (summary rows within categories)
    if (grepl("^Total$", label, ignore.case = TRUE)) next

    # Parse numeric values - handle "-" as 0
    parse_val <- function(x) {
      if (is.na(x) || x == "-" || x == "" || x == "D" || x == "X") return(0)
      suppressWarnings(as.numeric(gsub(",", "", x)))
    }

    aos_total <- parse_val(dt[[2]][i])
    aos_female <- parse_val(dt[[3]][i])
    aos_male <- parse_val(dt[[4]][i])
    new_total <- parse_val(dt[[6]][i])
    new_female <- parse_val(dt[[7]][i])
    new_male <- parse_val(dt[[8]][i])

    # Only add if we have at least some numeric data
    if (!is.na(aos_total) || !is.na(new_total)) {
      result <- rbind(result, data.table::data.table(
        fiscal_year = fiscal_year,
        category = current_category,
        value = trimws(label),
        aos_total = aos_total,
        aos_female = aos_female,
        aos_male = aos_male,
        new_total = new_total,
        new_female = new_female,
        new_male = new_male
      ))
    }
  }

  result
}

#' Parse DHS Table 8 Format 2 (separate sheets)
#'
#' @description
#' Parses Table 8 when data is in separate sheets for New Arrivals
#' and Adjustments of Status (used in 2023+).
#'
#' @param excel_path Character: path to the Excel file
#' @param fiscal_year Integer: fiscal year (for labeling)
#'
#' @return data.table with same structure as parse_dhs_table8
#'
#' @keywords internal
parse_dhs_table8_format2 <- function(excel_path, fiscal_year) {
  sheets <- readxl::excel_sheets(excel_path)

  # Helper to parse a single sheet (New Arrivals or Adjustments)
  parse_single_sheet <- function(sheet_name, prefix) {
    raw <- readxl::read_excel(excel_path, sheet = sheet_name, col_names = FALSE)
    dt <- data.table::as.data.table(raw)

    # Find data start - look for "AGE" row marker
    data_start <- NULL
    for (i in 1:min(20, nrow(dt))) {
      val <- as.character(dt[[1]][i])
      if (!is.na(val) && grepl("^AGE$", val, ignore.case = TRUE)) {
        data_start <- i + 1
        break
      }
    }

    if (is.null(data_start)) {
      cli::cli_alert_warning("Could not find AGE section in {sheet_name}")
      return(data.table::data.table())
    }

    # Parse data
    result <- data.table::data.table(
      category = character(),
      value = character(),
      total = numeric(),
      female = numeric(),
      male = numeric()
    )

    current_category <- "AGE"
    category_markers <- c("AGE", "BROAD AGE GROUPS", "MARITAL STATUS", "OCCUPATION")

    parse_val <- function(x) {
      if (is.na(x) || x == "-" || x == "" || x == "D" || x == "X") return(0)
      suppressWarnings(as.numeric(gsub(",", "", x)))
    }

    for (i in data_start:nrow(dt)) {
      label <- as.character(dt[[1]][i])
      if (is.na(label) || label == "") next

      label_upper <- toupper(trimws(label))
      if (label_upper %in% category_markers) {
        current_category <- label_upper
        next
      }

      if (grepl("^Total$", label, ignore.case = TRUE)) next

      total_val <- parse_val(dt[[2]][i])
      female_val <- parse_val(dt[[3]][i])
      male_val <- parse_val(dt[[4]][i])

      if (!is.na(total_val)) {
        result <- rbind(result, data.table::data.table(
          category = current_category,
          value = trimws(label),
          total = total_val,
          female = female_val,
          male = male_val
        ))
      }
    }

    result
  }

  # Find the sheet names
  new_sheet <- sheets[grepl("Table 8 New", sheets, ignore.case = TRUE)][1]
  aos_sheet <- sheets[grepl("Table 8 Adjust", sheets, ignore.case = TRUE)][1]

  # Parse both sheets
  new_data <- parse_single_sheet(new_sheet, "new")
  aos_data <- parse_single_sheet(aos_sheet, "aos")

  # Merge the data
  if (nrow(new_data) == 0 || nrow(aos_data) == 0) {
    cli::cli_abort("Failed to parse data from separate sheets for FY{fiscal_year}")
  }

  # Combine by category/value
  combined <- merge(
    aos_data,
    new_data,
    by = c("category", "value"),
    all = TRUE,
    suffixes = c("_aos", "_new")
  )

  # Rename columns
  data.table::setnames(combined,
    c("total_aos", "female_aos", "male_aos", "total_new", "female_new", "male_new"),
    c("aos_total", "aos_female", "aos_male", "new_total", "new_female", "new_male"),
    skip_absent = TRUE
  )

  # Add fiscal year
  combined[, fiscal_year := fiscal_year]

  # Reorder columns
  data.table::setcolorder(combined, c(
    "fiscal_year", "category", "value",
    "aos_total", "aos_female", "aos_male",
    "new_total", "new_female", "new_male"
  ))

  combined
}

#' Extract age-sex data from parsed Table 8
#'
#' @description
#' Extracts age breakdowns with sex from parsed Table 8 data
#' and converts to a standardized long format.
#'
#' @param table8_data data.table from parse_dhs_table8()
#'
#' @return data.table with columns: fiscal_year, age_group, age_min, age_max,
#'   sex, admission_type, count
#'
#' @keywords internal
extract_age_sex_from_table8 <- function(table8_data) {
  # Extract age data (not broad age groups)
  age_data <- table8_data[category == "AGE"]

  if (nrow(age_data) == 0) {
    cli::cli_alert_warning("No age data found in Table 8")
    return(data.table::data.table())
  }

  # Parse age groups to get min/max
  age_ranges <- parse_age_group(age_data$value)
  age_data[, age_group := value]
  age_data[, age_min := age_ranges$age_min]
  age_data[, age_max := age_ranges$age_max]

  # Melt to long format - we have 6 columns of data (aos/new by sex)
  # First melt by admission type and sex
  result_list <- list()

  # AOS by sex
  aos_female <- age_data[, .(
    fiscal_year, age_group, age_min, age_max,
    sex = "female", admission_type = "aos", count = aos_female
  )]
  aos_male <- age_data[, .(
    fiscal_year, age_group, age_min, age_max,
    sex = "male", admission_type = "aos", count = aos_male
  )]

  # New arrivals by sex
  new_female <- age_data[, .(
    fiscal_year, age_group, age_min, age_max,
    sex = "female", admission_type = "new_arrival", count = new_female
  )]
  new_male <- age_data[, .(
    fiscal_year, age_group, age_min, age_max,
    sex = "male", admission_type = "new_arrival", count = new_male
  )]

  result <- data.table::rbindlist(list(aos_female, aos_male, new_female, new_male))

  # Remove rows with unknown ages
  result <- result[!is.na(age_min)]

  result
}

#' Parse age group text to numeric range
#'
#' @description
#' Converts DHS age group text to min/max ages.
#'
#' @param age_text Character vector of age group descriptions
#'
#' @return list with age_min and age_max vectors
#'
#' @keywords internal
parse_age_group <- function(age_text) {
  n <- length(age_text)
  age_min <- rep(NA_integer_, n)
  age_max <- rep(NA_integer_, n)

  for (i in seq_along(age_text)) {
    txt <- age_text[i]
    if (is.na(txt)) next

    # Pattern: "Under X years" or "Under X"
    if (grepl("Under", txt, ignore.case = TRUE)) {
      num <- as.integer(gsub(".*Under\\s+(\\d+).*", "\\1", txt, ignore.case = TRUE))
      if (!is.na(num)) {
        age_min[i] <- 0L
        age_max[i] <- num - 1L
      }
    }
    # Pattern: "X to Y years" or "X-Y"
    else if (grepl("\\d+\\s*(to|-|-)\\s*\\d+", txt)) {
      nums <- as.integer(unlist(regmatches(txt, gregexpr("\\d+", txt))))
      if (length(nums) >= 2) {
        age_min[i] <- nums[1]
        age_max[i] <- nums[2]
      }
    }
    # Pattern: "X years and over" or "X+"
    else if (grepl("and over|\\+|plus", txt, ignore.case = TRUE)) {
      num <- as.integer(gsub(".*?(\\d+).*", "\\1", txt))
      if (!is.na(num)) {
        age_min[i] <- num
        age_max[i] <- 99L
      }
    }
    # Pattern: Single number "X years"
    else if (grepl("^\\d+\\s*(years?)?$", txt, ignore.case = TRUE)) {
      num <- as.integer(gsub("\\D", "", txt))
      if (!is.na(num)) {
        age_min[i] <- num
        age_max[i] <- num
      }
    }
    # Pattern: "Unknown"
    else if (grepl("Unknown|Not stated", txt, ignore.case = TRUE)) {
      age_min[i] <- NA_integer_
      age_max[i] <- NA_integer_
    }
  }

  list(age_min = age_min, age_max = age_max)
}

#' Fetch DHS LPR immigration data for a fiscal year
#'
#' @description
#' Downloads and parses DHS LPR immigration data from the expanded
#' Tables 8-11 for a given fiscal year.
#'
#' @param fiscal_year Integer: fiscal year (2006-2023 available)
#' @param cache_dir Character: directory to cache data
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return list with:
#'   - raw: data.table with all parsed Table 8 data (categories: AGE, MARITAL STATUS, OCCUPATION)
#'   - age_sex: data.table with age-sex breakdown by admission type
#'
#' @details
#' Data source: DHS Office of Homeland Security Statistics
#' URL: https://ohss.dhs.gov/topics/immigration/lawful-permanent-residents/lpr-yearbook-tables-8-11-expanded
#'
#' The expanded tables distinguish between:
#' - New Arrivals: persons living abroad granted LPR visa
#' - Adjustments of Status: persons already in US adjusting to LPR status
#'
#' Note: Refugees and asylees are classified as AOS in DHS data but should
#' be reclassified as new arrivals per OCACT methodology. This reclassification
#' is done in the lpr_immigration.R projection functions.
#'
#' @export
fetch_dhs_lpr_data <- function(fiscal_year,
                               cache_dir = "data/cache/dhs_immigration",
                               force_download = FALSE) {
  # Validate year
  if (fiscal_year < 2006 || fiscal_year > 2023) {
    cli::cli_abort("Expanded tables available for fiscal years 2006-2023")
  }

  # Check for cached processed data
  processed_cache <- file.path(cache_dir, sprintf("dhs_lpr_processed_fy%d.rds", fiscal_year))
  if (file.exists(processed_cache) && !force_download) {
    cli::cli_alert_success("Loading cached processed DHS data for FY{fiscal_year}")
    return(readRDS(processed_cache))
  }

  # Download Excel file
  excel_path <- download_dhs_expanded_tables(fiscal_year, cache_dir, force_download)

  # Parse Table 8
  cli::cli_alert_info("Parsing Table 8 for FY{fiscal_year}...")
  table8_raw <- tryCatch({
    parse_dhs_table8(excel_path, fiscal_year)
  }, error = function(e) {
    cli::cli_alert_warning("Failed to parse Table 8: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(table8_raw) || nrow(table8_raw) == 0) {
    cli::cli_abort("Failed to parse any data from Table 8 for FY{fiscal_year}")
  }

  # Extract age-sex data
  age_sex <- extract_age_sex_from_table8(table8_raw)

  result <- list(
    raw = table8_raw,
    age_sex = age_sex
  )

  # Cache processed data
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  saveRDS(result, processed_cache)
  cli::cli_alert_success("Cached processed data for FY{fiscal_year}")

  # Report summary
  if (nrow(age_sex) > 0) {
    totals <- age_sex[, .(total = sum(count, na.rm = TRUE)), by = admission_type]
    cli::cli_alert_info("FY{fiscal_year}: AOS={totals[admission_type=='aos', total]}, New={totals[admission_type=='new_arrival', total]}")
  }

  result
}

#' Fetch DHS LPR immigration data for multiple years
#'
#' @description
#' Downloads and parses DHS LPR immigration data for multiple fiscal years.
#'
#' @param fiscal_years Integer vector: fiscal years to fetch (2006-2023)
#' @param cache_dir Character: directory to cache data
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return list with:
#'   - age_sex: data.table with age-sex breakdown by admission type (all years)
#'   - by_year: list of individual year results
#'
#' @export
fetch_dhs_lpr_data_multi <- function(fiscal_years = 2006:2023,
                                     cache_dir = "data/cache/dhs_immigration",
                                     force_download = FALSE) {
  results <- list()
  age_sex_list <- list()

  for (fy in fiscal_years) {
    cli::cli_alert("Processing FY{fy}...")
    tryCatch({
      result <- fetch_dhs_lpr_data(fy, cache_dir, force_download)
      results[[as.character(fy)]] <- result
      if (!is.null(result$age_sex) && nrow(result$age_sex) > 0) {
        age_sex_list[[as.character(fy)]] <- result$age_sex
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for FY{fy}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No DHS data retrieved")
  }

  combined_age_sex <- data.table::rbindlist(age_sex_list, use.names = TRUE, fill = TRUE)

  # Report summary
  if (nrow(combined_age_sex) > 0) {
    totals <- combined_age_sex[, .(total = sum(count, na.rm = TRUE)), by = .(fiscal_year, admission_type)]
    cli::cli_alert_success("Retrieved data for {length(results)} fiscal years")
    cli::cli_alert_info("Total records: {nrow(combined_age_sex)}")
  }

  list(
    age_sex = combined_age_sex,
    by_year = results
  )
}

#' Get available DHS LPR data years
#'
#' @description
#' Returns the range of fiscal years available from DHS expanded tables.
#'
#' @return Integer vector of available fiscal years
#'
#' @export
get_dhs_lpr_available_years <- function() {
  2006:2023
}

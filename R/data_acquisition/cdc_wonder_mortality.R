#' CDC WONDER Provisional Mortality Data
#'
#' Functions for fetching provisional death data from CDC WONDER.
#' Used for the most recent year where final NCHS microdata is not yet available.
#'
#' Per TR2025 Item 9: the most recent year uses WONDER provisional data for
#' total deaths, with cause proportions carried forward from the prior NCHS
#' final year.
#'
#' @name cdc_wonder_mortality
NULL

#' Fetch provisional deaths from CDC WONDER
#'
#' @description
#' Queries CDC WONDER Provisional Mortality Statistics (database D176) for
#' total deaths by single year of age and sex. Returns total deaths only
#' (no cause breakdown — cause is applied separately via prior year proportions).
#'
#' @param year Integer: provisional year to fetch
#' @param cache_dir Character: directory to cache results
#' @param force_download Logical: if TRUE, re-fetch even if cached
#'
#' @return data.table with columns: year, age, sex, deaths
#'
#' @details
#' CDC WONDER's Provisional Mortality Statistics database (D176) provides
#' near-real-time death counts. The request uses form-encoded POST parameters
#' to group by single year of age and sex.
#'
#' @export
fetch_wonder_provisional_deaths <- function(year,
                                             cache_dir = "data/cache/wonder",
                                             force_download = FALSE) {
  checkmate::assert_integerish(year, lower = 2018, upper = 2099, len = 1)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_file <- file.path(cache_dir, sprintf("provisional_deaths_%d.rds", year))
  if (file.exists(cache_file) && !force_download) {
    cli::cli_alert_success("Loading cached WONDER provisional deaths for {year}")
    return(readRDS(cache_file))
  }

  cli::cli_alert_info("Fetching provisional deaths from CDC WONDER for {year}...")

  # Build WONDER D176 POST request
  base_url <- "https://wonder.cdc.gov/controller/datarequest/D176"

  # WONDER form parameters for D176 (Provisional Mortality Statistics)
  # Group by: Single Year of Age (D176.V5), Sex (D176.V7)
  # Filter by: Year (D176.V1)
  params <- list(
    "B_1" = "D176.V5",       # Group by single year of age
    "B_2" = "D176.V7",       # Group by sex
    "F_D176.V1" = as.character(year),  # Filter year
    "O_V5_fmode" = "freg",   # Age: regular
    "O_V7_fmode" = "freg",   # Sex: regular
    "action" = "Send",
    "M_1" = "D176.M1",       # Measure: Deaths
    "O_title" = "",
    "O_timeout" = "600",
    "O_location" = "D176.V9",
    "VM_I" = "*All*"         # All ICD codes
  )

  req <- create_api_request(base_url, timeout_seconds = 120) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(!!!params)

  resp <- api_request_with_retry(req, max_retries = 3)

  status <- httr2::resp_status(resp)
  if (status != 200) {
    cli::cli_abort(c(
      "WONDER API returned status {status} for provisional deaths",
      "i" = "Year: {year}",
      "i" = "The WONDER provisional database may not have data for this year"
    ))
  }

  # Parse response — WONDER returns tab-delimited text
  body <- httr2::resp_body_string(resp)
  result <- parse_wonder_mortality_response(body, year)

  saveRDS(result, cache_file)
  cli::cli_alert_success(
    "Cached WONDER provisional deaths for {year}: {format(sum(result$deaths), big.mark=',')} total"
  )

  result
}

#' Parse WONDER mortality response
#'
#' @param body Character: response body from WONDER API
#' @param year Integer: year for the data
#'
#' @return data.table with columns: year, age, sex, deaths
#'
#' @keywords internal
parse_wonder_mortality_response <- function(body, year) {
  # WONDER returns tab-delimited data with header rows
  # Read as data.table, skipping metadata rows
  lines <- strsplit(body, "\n")[[1]]

  # Find the header line (contains "Deaths")
  header_idx <- grep("Deaths", lines)[1]
  if (is.na(header_idx)) {
    cli::cli_abort("Could not parse WONDER response: no 'Deaths' header found")
  }

  # Read from header line onward
  dt <- data.table::fread(
    text = paste(lines[header_idx:length(lines)], collapse = "\n"),
    sep = "\t",
    header = TRUE,
    fill = TRUE,
    na.strings = c("", "Suppressed", "Not Applicable", "Unreliable")
  )

  # Standardize column names (WONDER uses variable labels)
  names(dt) <- tolower(gsub("[^a-zA-Z0-9]", "_", names(dt)))

  # Extract age and sex columns
  # WONDER D176 returns "Single Year Ages" and "Sex" columns
  age_col <- grep("age|year", names(dt), value = TRUE)[1]
  sex_col <- grep("sex", names(dt), value = TRUE)[1]
  deaths_col <- grep("deaths", names(dt), value = TRUE)[1]

  if (is.na(age_col) || is.na(sex_col) || is.na(deaths_col)) {
    cli::cli_abort(c(
      "Could not identify required columns in WONDER response",
      "i" = "Found columns: {paste(names(dt), collapse = ', ')}"
    ))
  }

  result <- dt[, .SD, .SDcols = c(age_col, sex_col, deaths_col)]
  data.table::setnames(result, c("age_raw", "sex_raw", "deaths"))

  # Parse age: WONDER returns age as text (e.g., "0", "1", "< 1 year", "100+")
  result[, age := suppressWarnings(as.integer(gsub("[^0-9]", "", age_raw)))]
  # Handle "< 1" or "Under 1" as age 0
  result[grepl("< ?1|under|less", age_raw, ignore.case = TRUE), age := 0L]
  # Handle 100+ as age 100
  result[grepl("100\\+|100 and over", age_raw, ignore.case = TRUE), age := 100L]

  # Parse sex
  result[, sex := data.table::fcase(
    grepl("^[Mm]", sex_raw), "male",
    grepl("^[Ff]", sex_raw), "female",
    default = NA_character_
  )]

  # Parse deaths (may have commas)
  result[, deaths := suppressWarnings(
    as.integer(gsub(",", "", as.character(deaths)))
  )]

  # Filter valid rows
  result <- result[!is.na(age) & !is.na(sex) & !is.na(deaths) & age >= 0 & age <= 119]

  # Format output
  result[, year := year]
  result <- result[, .(year, age, sex, deaths)]
  data.table::setorder(result, age, sex)

  result
}

#' Apply prior year cause proportions to WONDER total deaths
#'
#' @description
#' Distributes WONDER total deaths across the 6 cause categories using
#' cause proportions from the prior NCHS final year. Per TR2025 Item 9,
#' the most recent year's cause distribution is assumed to match the
#' prior year's proportions.
#'
#' @param wonder_deaths data.table with columns: year, age, sex, deaths
#' @param nchs_prior_year data.table with columns: year, age, sex, cause, deaths
#'   (from the prior year's final NCHS data)
#'
#' @return data.table with columns: year, age, sex, cause, deaths
#'
#' @export
apply_prior_year_cause_proportions <- function(wonder_deaths, nchs_prior_year) {
  checkmate::assert_data_table(wonder_deaths)
  checkmate::assert_data_table(nchs_prior_year)

  # Calculate cause proportions from prior year
  prior_totals <- nchs_prior_year[, .(total = sum(deaths)), by = .(age, sex)]
  proportions <- merge(nchs_prior_year, prior_totals, by = c("age", "sex"))
  proportions[, prop := deaths / total]
  proportions[is.nan(prop), prop := 0]
  proportions <- proportions[, .(age, sex, cause, prop)]

  # Expand WONDER deaths to cause-specific using prior year proportions
  prov_year <- unique(wonder_deaths$year)
  expanded <- merge(
    wonder_deaths[, .(age, sex, total_deaths = deaths)],
    proportions,
    by = c("age", "sex"),
    allow.cartesian = TRUE
  )
  expanded[, deaths := as.integer(round(total_deaths * prop))]

  # Set OTH as residual to preserve total deaths (handle rounding)
  non_oth <- expanded[cause != "OTH", .(non_oth = sum(deaths)), by = .(age, sex)]
  expanded <- merge(expanded, non_oth, by = c("age", "sex"), all.x = TRUE)
  expanded[is.na(non_oth), non_oth := 0L]
  expanded[cause == "OTH", deaths := pmax(0L, as.integer(total_deaths - non_oth))]
  expanded[, c("total_deaths", "prop", "non_oth") := NULL]

  expanded[, year := prov_year]
  result <- expanded[, .(year, age, sex, cause, deaths)]
  data.table::setorder(result, age, sex, cause)

  cli::cli_alert_success(
    "Applied prior year cause proportions to {format(sum(result$deaths), big.mark=',')} provisional deaths"
  )

  result
}

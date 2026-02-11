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
#' @param year Integer: provisional year to fetch (2018+)
#' @param cache_dir Character: directory to cache results
#' @param force_download Logical: if TRUE, re-fetch even if cached
#'
#' @return data.table with columns: year, age, sex, deaths
#'
#' @details
#' CDC WONDER's Provisional Mortality Statistics database (D176) provides
#' near-real-time death counts. The API accepts XML request documents via POST
#' and returns XML responses. Results are grouped by single year of age
#' (D176.V52) and sex (D176.V7).
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

  # Build WONDER D176 XML request
  xml_request <- build_wonder_d176_request(year)
  base_url <- "https://wonder.cdc.gov/controller/datarequest/D176"

  req <- httr2::request(base_url) |>
    httr2::req_timeout(180) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(
      request_xml = xml_request,
      accept_datause_restrictions = "true"
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  body <- httr2::resp_body_string(resp)

  if (status != 200) {
    # Extract error messages from XML response
    error_msgs <- extract_wonder_error_messages(body)
    cli::cli_abort(c(
      "WONDER API returned status {status} for provisional deaths",
      "i" = "Year: {year}",
      error_msgs
    ))
  }

  result <- parse_wonder_xml_response(body, year)

  if (nrow(result) == 0) {
    cli::cli_abort(c(
      "WONDER returned no data rows for year {year}",
      "i" = "The provisional database may not have data for this year yet"
    ))
  }

  saveRDS(result, cache_file)
  cli::cli_alert_success(
    "Cached WONDER provisional deaths for {year}: {format(sum(result$deaths), big.mark=',')} total"
  )

  result
}

#' Build WONDER D176 XML request
#'
#' @description
#' Constructs the XML request document for the WONDER D176 (Provisional
#' Mortality Statistics) API. Groups by single year of age and sex, filtered
#' to a specific year.
#'
#' @param year Integer: year to query
#'
#' @return Character string containing the XML request
#'
#' @details
#' The XML format follows the WONDER API specification. Key variable codes:
#' - D176.V52: Single-Year Age Groups (B_1 group-by)
#' - D176.V7: Sex (B_2 group-by)
#' - D176.V1: Year (F_ filter)
#' - D176.M1: Deaths measure
#'
#' The template includes all required filter, option, and variable parameters
#' based on the D176 database defaults.
#'
#' @keywords internal
build_wonder_d176_request <- function(year) {
  year_str <- as.character(year)

  paste0(
    "<request-parameters>",
    "<parameter><name>accept_datause_restrictions</name><value>true</value></parameter>",
    # Group by single year of age and sex
    "<parameter><name>B_1</name><value>D176.V52</value></parameter>",
    "<parameter><name>B_2</name><value>D176.V7</value></parameter>",
    "<parameter><name>B_3</name><value>*None*</value></parameter>",
    "<parameter><name>B_4</name><value>*None*</value></parameter>",
    "<parameter><name>B_5</name><value>*None*</value></parameter>",
    # Finder values - year filter and locations
    "<parameter><name>F_D176.V1</name><value>", year_str, "</value></parameter>",
    "<parameter><name>F_D176.V10</name><value>*All*</value></parameter>",
    "<parameter><name>F_D176.V100</name><value>*All*</value></parameter>",
    "<parameter><name>F_D176.V13</name><value>*All*</value></parameter>",
    "<parameter><name>F_D176.V2</name><value>*All*</value></parameter>",
    "<parameter><name>F_D176.V25</name><value>*All*</value></parameter>",
    "<parameter><name>F_D176.V26</name><value>*All*</value></parameter>",
    "<parameter><name>F_D176.V27</name><value>*All*</value></parameter>",
    "<parameter><name>F_D176.V77</name><value>*All*</value></parameter>",
    "<parameter><name>F_D176.V79</name><value>*All*</value></parameter>",
    "<parameter><name>F_D176.V80</name><value>*All*</value></parameter>",
    "<parameter><name>F_D176.V9</name><value>*All*</value></parameter>",
    # Index/label values
    "<parameter><name>I_D176.V1</name><value>", year_str, "</value></parameter>",
    "<parameter><name>I_D176.V10</name><value>*All* (The United States)</value></parameter>",
    "<parameter><name>I_D176.V100</name><value>*All* (All Dates)</value></parameter>",
    "<parameter><name>I_D176.V2</name><value>*All*</value></parameter>",
    "<parameter><name>I_D176.V25</name><value>All Causes of Death</value></parameter>",
    "<parameter><name>I_D176.V27</name><value>*All* (The United States)</value></parameter>",
    "<parameter><name>I_D176.V77</name><value>*All* (The United States)</value></parameter>",
    "<parameter><name>I_D176.V79</name><value>*All* (The United States)</value></parameter>",
    "<parameter><name>I_D176.V80</name><value>*All* (The United States)</value></parameter>",
    "<parameter><name>I_D176.V9</name><value>*All* (The United States)</value></parameter>",
    # Measures - deaths, population, crude rate
    "<parameter><name>M_1</name><value>D176.M1</value></parameter>",
    "<parameter><name>M_2</name><value>D176.M2</value></parameter>",
    "<parameter><name>M_3</name><value>D176.M3</value></parameter>",
    # Options
    "<parameter><name>O_MMWR</name><value>false</value></parameter>",
    "<parameter><name>O_V100_fmode</name><value>freg</value></parameter>",
    "<parameter><name>O_V10_fmode</name><value>freg</value></parameter>",
    "<parameter><name>O_V13_fmode</name><value>fadv</value></parameter>",
    "<parameter><name>O_V15_fmode</name><value>fadv</value></parameter>",
    "<parameter><name>O_V16_fmode</name><value>fadv</value></parameter>",
    "<parameter><name>O_V1_fmode</name><value>freg</value></parameter>",
    "<parameter><name>O_V25_fmode</name><value>freg</value></parameter>",
    "<parameter><name>O_V26_fmode</name><value>fadv</value></parameter>",
    "<parameter><name>O_V27_fmode</name><value>freg</value></parameter>",
    "<parameter><name>O_V2_fmode</name><value>freg</value></parameter>",
    "<parameter><name>O_V77_fmode</name><value>freg</value></parameter>",
    "<parameter><name>O_V79_fmode</name><value>freg</value></parameter>",
    "<parameter><name>O_V80_fmode</name><value>freg</value></parameter>",
    "<parameter><name>O_V9_fmode</name><value>freg</value></parameter>",
    "<parameter><name>O_aar</name><value>aar_none</value></parameter>",
    "<parameter><name>O_aar_pop</name><value>0000</value></parameter>",
    "<parameter><name>O_age</name><value>D176.V52</value></parameter>",
    "<parameter><name>O_dates</name><value>YEAR</value></parameter>",
    "<parameter><name>O_death_location</name><value>D176.V79</value></parameter>",
    "<parameter><name>O_death_urban</name><value>D176.V89</value></parameter>",
    "<parameter><name>O_javascript</name><value>on</value></parameter>",
    "<parameter><name>O_location</name><value>D176.V9</value></parameter>",
    "<parameter><name>O_mcd</name><value>D176.V13</value></parameter>",
    "<parameter><name>O_precision</name><value>1</value></parameter>",
    "<parameter><name>O_race</name><value>D176.V42</value></parameter>",
    "<parameter><name>O_rate_per</name><value>100000</value></parameter>",
    "<parameter><name>O_show_totals</name><value>false</value></parameter>",
    "<parameter><name>O_timeout</name><value>600</value></parameter>",
    "<parameter><name>O_title</name><value></value></parameter>",
    "<parameter><name>O_ucd</name><value>D176.V2</value></parameter>",
    "<parameter><name>O_urban</name><value>D176.V19</value></parameter>",
    # Variable filter values
    "<parameter><name>V_D176.V1</name><value></value></parameter>",
    "<parameter><name>V_D176.V10</name><value></value></parameter>",
    "<parameter><name>V_D176.V100</name><value></value></parameter>",
    "<parameter><name>V_D176.V11</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V12</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V13</name><value></value></parameter>",
    "<parameter><name>V_D176.V13_AND</name><value></value></parameter>",
    "<parameter><name>V_D176.V15</name><value></value></parameter>",
    "<parameter><name>V_D176.V15_AND</name><value></value></parameter>",
    "<parameter><name>V_D176.V16</name><value></value></parameter>",
    "<parameter><name>V_D176.V16_AND</name><value></value></parameter>",
    "<parameter><name>V_D176.V17</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V19</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V2</name><value></value></parameter>",
    "<parameter><name>V_D176.V20</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V21</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V22</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V23</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V25</name><value></value></parameter>",
    "<parameter><name>V_D176.V26</name><value></value></parameter>",
    "<parameter><name>V_D176.V26_AND</name><value></value></parameter>",
    "<parameter><name>V_D176.V27</name><value></value></parameter>",
    "<parameter><name>V_D176.V4</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V42</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V43</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V44</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V5</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V51</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V52</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V6</name><value>00</value></parameter>",
    "<parameter><name>V_D176.V7</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V77</name><value></value></parameter>",
    "<parameter><name>V_D176.V79</name><value></value></parameter>",
    "<parameter><name>V_D176.V80</name><value></value></parameter>",
    "<parameter><name>V_D176.V81</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V89</name><value>*All*</value></parameter>",
    "<parameter><name>V_D176.V9</name><value></value></parameter>",
    # Metadata
    "<parameter><name>action-Send</name><value>Send</value></parameter>",
    "<parameter><name>dataset_code</name><value>D176</value></parameter>",
    "<parameter><name>finder-stage-D176.V1</name><value>codeset</value></parameter>",
    "<parameter><name>finder-stage-D176.V10</name><value>codeset</value></parameter>",
    "<parameter><name>finder-stage-D176.V100</name><value>codeset</value></parameter>",
    "<parameter><name>finder-stage-D176.V13</name><value>codeset</value></parameter>",
    "<parameter><name>finder-stage-D176.V2</name><value>codeset</value></parameter>",
    "<parameter><name>finder-stage-D176.V25</name><value>codeset</value></parameter>",
    "<parameter><name>finder-stage-D176.V26</name><value>codeset</value></parameter>",
    "<parameter><name>finder-stage-D176.V27</name><value>codeset</value></parameter>",
    "<parameter><name>finder-stage-D176.V77</name><value>codeset</value></parameter>",
    "<parameter><name>finder-stage-D176.V79</name><value>codeset</value></parameter>",
    "<parameter><name>finder-stage-D176.V80</name><value>codeset</value></parameter>",
    "<parameter><name>finder-stage-D176.V9</name><value>codeset</value></parameter>",
    "<parameter><name>saved_id</name><value></value></parameter>",
    "<parameter><name>stage</name><value>request</value></parameter>",
    "</request-parameters>"
  )
}

#' Parse WONDER XML response
#'
#' @description
#' Parses the XML response from the WONDER D176 API into a data.table.
#' The response contains `<r>` (row) elements with `<c>` (cell) children.
#' When grouped by age and sex, rows with a new age have 5 cells
#' (age, sex, deaths, population, rate) while continuation rows for the
#' same age have 4 cells (sex, deaths, population, rate).
#'
#' @param body Character: XML response body from WONDER API
#' @param year Integer: year for the data
#'
#' @return data.table with columns: year, age, sex, deaths
#'
#' @keywords internal
parse_wonder_xml_response <- function(body, year) {
  doc <- xml2::read_xml(body)
  rows <- xml2::xml_find_all(doc, ".//r")

  if (length(rows) == 0) {
    return(data.table::data.table(year = integer(), age = integer(),
                                   sex = character(), deaths = integer()))
  }

  # Parse rows with carry-forward for age labels.
  # Rows with a new age group have 5 cells; continuation rows have 4.
  results <- list()
  current_age <- NA_character_

  for (row in rows) {
    cells <- xml2::xml_find_all(row, ".//c")
    n <- length(cells)

    if (n == 5L) {
      current_age <- xml2::xml_attr(cells[[1]], "l")
      sex_val <- xml2::xml_attr(cells[[2]], "l")
      deaths_val <- xml2::xml_attr(cells[[3]], "v")
    } else if (n == 4L) {
      sex_val <- xml2::xml_attr(cells[[1]], "l")
      deaths_val <- xml2::xml_attr(cells[[2]], "v")
    } else {
      next
    }

    if (!is.na(current_age) && !is.na(sex_val) && !is.na(deaths_val)) {
      results[[length(results) + 1L]] <- list(
        age_raw = current_age,
        sex_raw = sex_val,
        deaths_raw = deaths_val
      )
    }
  }

  if (length(results) == 0L) {
    return(data.table::data.table(year = integer(), age = integer(),
                                   sex = character(), deaths = integer()))
  }

  dt <- data.table::rbindlist(results)

  # Parse age: "< 1 year" -> 0, "1 year" -> 1, "100+" -> 100, "Not Stated" -> NA
  dt[, age := suppressWarnings(as.integer(gsub("[^0-9]", "", age_raw)))]
  dt[grepl("< ?1|under", age_raw, ignore.case = TRUE), age := 0L]
  dt[grepl("100\\+|100 and over", age_raw, ignore.case = TRUE), age := 100L]

  # Parse sex
  dt[, sex := data.table::fcase(
    grepl("^[Ff]", sex_raw), "female",
    grepl("^[Mm]", sex_raw), "male",
    default = NA_character_
  )]

  # Parse deaths (comma-formatted integers)
  dt[, deaths := suppressWarnings(as.integer(gsub(",", "", deaths_raw)))]

  # Filter to valid rows
  dt <- dt[!is.na(age) & !is.na(sex) & !is.na(deaths) & age >= 0L & age <= 100L]

  # Format output
  dt[, year := as.integer(year)]
  result <- dt[, .(year, age, sex, deaths)]
  data.table::setorder(result, age, sex)

  result
}

#' Extract error messages from WONDER XML response
#'
#' @param body Character: XML response body
#' @return Character vector of error messages (prefixed with "x" for cli)
#' @keywords internal
extract_wonder_error_messages <- function(body) {
  msgs <- regmatches(body, gregexpr("<message>[^<]+</message>", body))[[1]]
  msgs <- gsub("</?message>", "", msgs)
  if (length(msgs) == 0L) return(character())
  stats::setNames(msgs, rep("x", length(msgs)))
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

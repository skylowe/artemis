#' BLS CPS Labor Force Data Acquisition
#'
#' @description
#' Fetches historical CPS labor force data from BLS API v2 for USEMP calibration.
#' Uses the BLS LN (Labor Force Statistics) series catalog to identify exact
#' series IDs by age group, sex, marital status, and other disaggregations.
#'
#' @references
#' - BLS API v2: https://www.bls.gov/developers/
#' - data/raw/bls_ln.series.txt (67,251 series catalog)
#' - Input #26 (Monthly CPS), Input #27 (CPS Annual Averages)
#'
#' @name bls_cps_labor
NULL

# =============================================================================
# BLS Series ID Lookup
# =============================================================================

#' Load BLS LN series catalog
#'
#' @description
#' Loads the BLS Labor Force Statistics series catalog from the local file.
#' This file contains 67,251 series with metadata columns for filtering.
#'
#' @param catalog_path Path to bls_ln.series.txt
#'
#' @return data.table with series metadata
#'
#' @export
load_bls_series_catalog <- function(catalog_path = here::here("data/raw/bls_ln.series.txt")) {
  if (!file.exists(catalog_path)) {
    cli::cli_abort(c(
      "BLS series catalog not found",
      "x" = "Expected: {.file {catalog_path}}",
      "i" = "Download from https://download.bls.gov/pub/time.series/ln/"
    ))
  }

  dt <- data.table::fread(catalog_path, sep = "\t", strip.white = TRUE)
  cli::cli_alert_success("Loaded BLS series catalog: {nrow(dt)} series")
  dt
}

#' Look up BLS series ID by criteria
#'
#' @description
#' Queries the BLS LN series catalog to find the exact series ID matching
#' the specified age group, sex, concept, periodicity, and seasonal adjustment.
#'
#' @param catalog data.table from load_bls_series_catalog()
#' @param lfst_code Labor force status code (10=CLF level, 12=emp level,
#'   13=unemp level, 14=unemp rate, 20=LFPR)
#' @param ages_code Age group code (e.g., "07"=16-17, "13"=18-19, "20"=20-24)
#' @param sexs_code Sex code (0=both, 1=male, 2=female)
#' @param mari_code Marital status code (00=all, 02=married, 06=never married)
#' @param seasonal Seasonal adjustment (S=adjusted, U=unadjusted)
#'
#' @return Character: the matching series_id, or error if not found
#'
#' @export
lookup_bls_series_id <- function(catalog, lfst_code, ages_code,
                                  sexs_code = "0", mari_code = "00",
                                  seasonal = "S") {
  checkmate::assert_data_table(catalog)

  matches <- catalog[
    lfst_code == (!!lfst_code) &
    ages_code == (!!ages_code) &
    sexs_code == (!!sexs_code) &
    seasonal == (!!seasonal)
  ]

  if ("mari_code" %in% names(catalog) && mari_code != "00") {
    matches <- matches[mari_code == (!!mari_code)]
  }

  if (nrow(matches) == 0) {
    cli::cli_abort(c(
      "No BLS series found matching criteria",
      "i" = "lfst_code={lfst_code}, ages_code={ages_code}, sexs_code={sexs_code}, mari_code={mari_code}"
    ))
  }

  if (nrow(matches) > 1) {
    cli::cli_alert_warning("Multiple series match ({nrow(matches)}), using first: {matches$series_id[1]}")
  }

  trimws(matches$series_id[1])
}

# =============================================================================
# BLS API v2 Data Fetching
# =============================================================================

#' Fetch data from BLS API v2
#'
#' @description
#' Fetches time series data from BLS API v2 for one or more series IDs.
#' Requires BLS_API_KEY in .Renviron for API v2 access (500 queries/day).
#'
#' @param series_ids Character vector of BLS series IDs
#' @param start_year Integer: first year to fetch
#' @param end_year Integer: last year to fetch
#' @param api_key Character: BLS API key (default: from .Renviron)
#' @param cache_dir Character: cache directory (default: data/cache/bls/)
#'
#' @return data.table with columns: series_id, year, period, value
#'
#' @export
fetch_bls_series <- function(series_ids,
                              start_year,
                              end_year,
                              api_key = Sys.getenv("BLS_API_KEY"),
                              cache_dir = here::here("data/cache/bls")) {
  if (api_key == "") {
    cli::cli_abort(c(
      "BLS_API_KEY not found in .Renviron",
      "i" = "Required for fetching CPS labor force data",
      "i" = "Register at https://data.bls.gov/registrationEngine/"
    ))
  }

  # Create cache directory
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  # BLS API v2 allows max 50 series per request, 20 years per request
  max_series <- 50
  max_years <- 20

  all_results <- list()

  # Split series into batches
  series_batches <- split(series_ids, ceiling(seq_along(series_ids) / max_series))

  # Split year ranges
  year_ranges <- list()
  yr <- start_year
  while (yr <= end_year) {
    yr_end <- min(yr + max_years - 1, end_year)
    year_ranges[[length(year_ranges) + 1]] <- c(yr, yr_end)
    yr <- yr_end + 1
  }

  for (batch in series_batches) {
    for (yr_range in year_ranges) {
      cache_key <- paste0("bls_",
                          digest::digest(paste(sort(batch), yr_range[1], yr_range[2], sep = "_")),
                          ".rds")
      cache_file <- file.path(cache_dir, cache_key)

      if (file.exists(cache_file)) {
        all_results[[length(all_results) + 1]] <- readRDS(cache_file)
        next
      }

      cli::cli_alert_info("Fetching BLS data: {length(batch)} series, {yr_range[1]}-{yr_range[2]}")

      body <- list(
        seriesid = batch,
        startyear = as.character(yr_range[1]),
        endyear = as.character(yr_range[2]),
        registrationkey = api_key
      )

      resp <- httr2::request("https://api.bls.gov/publicAPI/v2/timeseries/data/") |>
        httr2::req_body_json(body) |>
        httr2::req_timeout(60) |>
        httr2::req_retry(max_tries = 3, backoff = ~ 2) |>
        httr2::req_perform()

      json <- httr2::resp_body_json(resp)

      if (json$status != "REQUEST_SUCCEEDED") {
        cli::cli_abort("BLS API request failed: {json$message[[1]]}")
      }

      batch_results <- list()
      for (series in json$Results$series) {
        sid <- series$seriesID
        for (obs in series$data) {
          batch_results[[length(batch_results) + 1]] <- data.table::data.table(
            series_id = sid,
            year = as.integer(obs$year),
            period = obs$period,
            value = suppressWarnings(as.numeric(obs$value))
          )
        }
      }

      batch_dt <- data.table::rbindlist(batch_results)

      # Cache the result
      saveRDS(batch_dt, cache_file)
      all_results[[length(all_results) + 1]] <- batch_dt
    }
  }

  result <- data.table::rbindlist(all_results)
  cli::cli_alert_success("Fetched {nrow(result)} BLS data points")

  result
}

# =============================================================================
# CPS Labor Force Data by Age/Sex
# =============================================================================

#' USEMP age group to BLS ages_code mapping
#' @keywords internal
USEMP_BLS_AGE_CODES <- list(
  "16-17" = "07",
  "18-19" = "13",
  "20-24" = "20",
  "25-29" = "25",
  "30-34" = "30",
  "35-39" = "35",
  "40-44" = "40",
  "45-49" = "45",
  "50-54" = "50",
  "55-59" = "55",
  "60-64" = "60",
  "65-69" = "65",
  "70-74" = "70",
  "75+"   = "75"
)

#' Fetch CPS labor force data by age group and sex
#'
#' @description
#' Fetches unemployment rates, LFPR, labor force, and employment levels
#' for USEMP's 14 age groups and 2 sexes from BLS CPS data.
#'
#' @param config Full ARTEMIS config
#' @param start_year First year (default: from config)
#' @param end_year Last year (default: base_year from config)
#'
#' @return data.table with columns: year, age_group, sex, concept, value
#'   concept values: unemployment_rate, lfpr, labor_force, employment, population
#'
#' @export
fetch_cps_labor_force_by_age_sex <- function(config,
                                              start_year = NULL,
                                              end_year = NULL) {
  emp_config <- config$economics$employment
  if (is.null(start_year)) start_year <- emp_config$historical_start_year %||% 1968
  if (is.null(end_year)) end_year <- emp_config$base_year %||% 2024

  cli::cli_alert_info("Fetching CPS labor force data by age/sex ({start_year}-{end_year})")

  # Load series catalog
  catalog <- load_bls_series_catalog()

  # Build list of series IDs needed
  series_list <- list()
  concepts <- list(
    unemployment_rate = "14",
    lfpr = "20",
    labor_force = "10",
    employment = "12"
  )

  for (sex_name in c("male", "female")) {
    sexs_code <- if (sex_name == "male") "1" else "2"

    for (ag in names(USEMP_BLS_AGE_CODES)) {
      ages_code <- USEMP_BLS_AGE_CODES[[ag]]

      for (concept_name in names(concepts)) {
        lfst_code <- concepts[[concept_name]]

        tryCatch({
          sid <- lookup_bls_series_id(catalog, lfst_code, ages_code, sexs_code)
          series_list[[paste(sex_name, ag, concept_name)]] <- list(
            series_id = sid,
            sex = sex_name,
            age_group = ag,
            concept = concept_name
          )
        }, error = function(e) {
          cli::cli_alert_warning("Series not found: {sex_name} {ag} {concept_name}")
        })
      }
    }
  }

  if (length(series_list) == 0) {
    cli::cli_abort("No BLS series found for CPS labor force data")
  }

  # Extract unique series IDs
  all_series_ids <- vapply(series_list, function(x) x$series_id, character(1))
  unique_ids <- unique(all_series_ids)

  cli::cli_alert_info("Fetching {length(unique_ids)} unique BLS series")

  # Fetch data
  raw_data <- fetch_bls_series(unique_ids, start_year, end_year)

  # Map series IDs back to age/sex/concept
  series_meta <- data.table::rbindlist(lapply(series_list, function(x) {
    data.table::data.table(series_id = x$series_id, sex = x$sex,
                           age_group = x$age_group, concept = x$concept)
  }))

  result <- merge(raw_data, series_meta, by = "series_id", all.x = TRUE)

  # Filter to annual averages (M13 period in BLS data)
  result_annual <- result[period == "M13"]

  if (nrow(result_annual) == 0) {
    # Some series may use annual average differently
    # Fall back to computing annual average from monthly data
    result_monthly <- result[grepl("^M(0[1-9]|1[0-2])$", period)]
    result_annual <- result_monthly[, .(value = mean(value, na.rm = TRUE)),
                                     by = .(year, age_group, sex, concept)]
  } else {
    result_annual <- result_annual[, .(year, age_group, sex, concept, value)]
  }

  data.table::setorder(result_annual, year, sex, age_group, concept)

  cli::cli_alert_success(
    "Fetched CPS data: {nrow(result_annual)} annual rows, {length(unique(result_annual$year))} years"
  )

  result_annual
}

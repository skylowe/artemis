#' Fetch Historical Economic Assumptions from APIs
#'
#' @description
#' Fetches historical economic data from BLS, BEA, and FRED APIs.
#' Used when `economics.employment.assumptions_data_source = "api"`.
#'
#' @references
#' - BLS API v2: Productivity, CPI, unemployment
#' - BEA NIPA API: GDP deflator, compensation ratios, GDP levels
#' - FRED API: Interest rates
#' - AWI historical from data/raw/SSA_TR2025/AWI_hist.csv
#'
#' @name fetch_economic_assumptions
NULL

# =============================================================================
# BLS Economic Series
# =============================================================================

#' Fetch BLS economic series
#'
#' @description
#' Fetches productivity, CPI, and average hours from BLS API v2.
#'
#' @param config ARTEMIS config
#' @param start_year First year
#' @param end_year Last year
#'
#' @return data.table with columns: year, variable, value
#'
#' @export
fetch_bls_economic_series <- function(config,
                                       start_year = 1948,
                                       end_year = NULL) {
  emp_config <- config$economics$employment
  if (is.null(end_year)) end_year <- emp_config$base_year %||% 2024

  api_key <- Sys.getenv("BLS_API_KEY")
  if (api_key == "") {
    cli::cli_abort(c(
      "BLS_API_KEY not found in .Renviron",
      "i" = "Required for fetching historical economic data"
    ))
  }

  cli::cli_alert_info("Fetching BLS economic series ({start_year}-{end_year})")

  # Key BLS series
  series_map <- list(
    productivity = "PRS85006092",  # Output per hour, nonfarm business
    cpi_w = "CWUR0000SA0",         # CPI-W, all items
    avg_weekly_hours = "CES0500000005",  # Avg weekly hours, total private
    unemployment_rate = "LNS14000000"     # Unemployment rate, seas adj
  )

  all_ids <- unlist(series_map)
  raw <- fetch_bls_series(all_ids, start_year, end_year,
                          api_key = api_key)

  # Filter to annual averages
  annual <- raw[period == "M13" | period == "Q05"]
  if (nrow(annual) == 0) {
    # Compute from monthly
    monthly <- raw[grepl("^M(0[1-9]|1[0-2])$", period)]
    annual <- monthly[, .(value = mean(value, na.rm = TRUE)),
                      by = .(series_id, year)]
  } else {
    annual <- annual[, .(series_id, year, value)]
  }

  # Map series IDs back to variable names
  id_to_var <- data.table::data.table(
    series_id = unlist(series_map),
    variable = names(series_map)
  )
  result <- merge(annual, id_to_var, by = "series_id")
  result <- result[, .(year, variable, value)]

  cli::cli_alert_success("Fetched {nrow(result)} BLS economic data points")

  result
}

# =============================================================================
# BEA NIPA Tables
# =============================================================================

#' Fetch BEA NIPA table data
#'
#' @description
#' Fetches GDP deflator, compensation ratios, and GDP levels from BEA NIPA API.
#'
#' @param config ARTEMIS config
#' @param start_year First year
#' @param end_year Last year
#'
#' @return data.table with columns: year, variable, value
#'
#' @export
fetch_bea_nipa_tables <- function(config,
                                    start_year = 1929,
                                    end_year = NULL) {
  emp_config <- config$economics$employment
  if (is.null(end_year)) end_year <- emp_config$base_year %||% 2024

  bea_key <- Sys.getenv("BEA_API_KEY")
  if (bea_key == "") {
    cli::cli_abort(c(
      "BEA_API_KEY not found in .Renviron",
      "i" = "Required for fetching NIPA data",
      "i" = "Register at https://apps.bea.gov/API/signup/"
    ))
  }

  cli::cli_alert_info("Fetching BEA NIPA tables ({start_year}-{end_year})")

  results <- list()

  # Table 1.1.4 Line 1: GDP Implicit Price Deflator
  tryCatch({
    resp <- httr2::request("https://apps.bea.gov/api/data") |>
      httr2::req_url_query(
        UserID = bea_key,
        method = "GetData",
        DataSetName = "NIPA",
        TableName = "T10104",
        Frequency = "A",
        Year = paste(start_year:end_year, collapse = ","),
        ResultFormat = "JSON"
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()

    json <- httr2::resp_body_json(resp)
    data_items <- json$BEAAPI$Results$Data

    for (item in data_items) {
      if (item$LineNumber == "1") {
        results[[length(results) + 1]] <- data.table::data.table(
          year = as.integer(item$TimePeriod),
          variable = "gdp_deflator",
          value = as.numeric(item$DataValue)
        )
      }
    }
  }, error = function(e) {
    cli::cli_alert_warning("Failed to fetch GDP deflator: {e$message}")
  })

  # Table 1.1.1 Line 1: Real GDP
  tryCatch({
    resp <- httr2::request("https://apps.bea.gov/api/data") |>
      httr2::req_url_query(
        UserID = bea_key,
        method = "GetData",
        DataSetName = "NIPA",
        TableName = "T10101",
        Frequency = "A",
        Year = paste(start_year:end_year, collapse = ","),
        ResultFormat = "JSON"
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()

    json <- httr2::resp_body_json(resp)
    data_items <- json$BEAAPI$Results$Data

    for (item in data_items) {
      if (item$LineNumber == "1") {
        results[[length(results) + 1]] <- data.table::data.table(
          year = as.integer(item$TimePeriod),
          variable = "real_gdp_change",
          value = as.numeric(item$DataValue)
        )
      }
    }
  }, error = function(e) {
    cli::cli_alert_warning("Failed to fetch real GDP: {e$message}")
  })

  if (length(results) > 0) {
    result <- data.table::rbindlist(results)
    cli::cli_alert_success("Fetched {nrow(result)} BEA NIPA data points")
    return(result)
  }

  cli::cli_alert_warning("No BEA data fetched — returning empty table")
  data.table::data.table(year = integer(), variable = character(), value = numeric())
}

# =============================================================================
# FRED Interest Rates
# =============================================================================

#' Fetch FRED interest rate data
#'
#' @description
#' Fetches 10-year Treasury constant maturity rate from FRED API.
#'
#' @param config ARTEMIS config
#' @param start_year First year
#' @param end_year Last year
#'
#' @return data.table with columns: year, variable, value
#'
#' @export
fetch_fred_interest_rates <- function(config,
                                       start_year = 1953,
                                       end_year = NULL) {
  emp_config <- config$economics$employment
  if (is.null(end_year)) end_year <- emp_config$base_year %||% 2024

  fred_key <- Sys.getenv("FRED_API_KEY")
  if (fred_key == "") {
    cli::cli_abort(c(
      "FRED_API_KEY not found in .Renviron",
      "i" = "Required for fetching interest rate data",
      "i" = "Register at https://fred.stlouisfed.org/docs/api/api_key.html"
    ))
  }

  cli::cli_alert_info("Fetching FRED interest rates ({start_year}-{end_year})")

  cache_dir <- here::here("data/cache/fred")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  cache_file <- file.path(cache_dir, sprintf("gs10_%d_%d.rds", start_year, end_year))
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached FRED data")
    return(readRDS(cache_file))
  }

  resp <- httr2::request("https://api.stlouisfed.org/fred/series/observations") |>
    httr2::req_url_query(
      series_id = "GS10",
      api_key = fred_key,
      file_type = "json",
      observation_start = paste0(start_year, "-01-01"),
      observation_end = paste0(end_year, "-12-31"),
      frequency = "a",
      aggregation_method = "avg"
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()

  json <- httr2::resp_body_json(resp)

  results <- list()
  for (obs in json$observations) {
    val <- suppressWarnings(as.numeric(obs$value))
    if (!is.na(val)) {
      results[[length(results) + 1]] <- data.table::data.table(
        year = as.integer(substr(obs$date, 1, 4)),
        variable = "nominal_interest_rate",
        value = val
      )
    }
  }

  result <- data.table::rbindlist(results)
  saveRDS(result, cache_file)

  cli::cli_alert_success("Fetched {nrow(result)} FRED interest rate data points")

  result
}

# =============================================================================
# Combined Historical Economic Data
# =============================================================================

#' Fetch all historical economic assumptions from APIs
#'
#' @description
#' Master function that combines data from BLS, BEA, FRED, and AWI sources.
#' Returns a unified data.table of historical economic variables.
#'
#' @param config Full ARTEMIS config
#'
#' @return data.table with columns: year, variable, value, source
#'
#' @export
fetch_historical_economic_assumptions <- function(config) {
  data_source <- config$economics$employment$assumptions_data_source %||% "api"

  if (data_source != "api") {
    cli::cli_alert_info("Skipping API fetch — using TR2025 tables for historical data")
    return(data.table::data.table(
      year = integer(), variable = character(), value = numeric(), source = character()
    ))
  }

  cli::cli_h1("Fetching Historical Economic Assumptions from APIs")

  results <- list()

  # BLS series
  tryCatch({
    bls <- fetch_bls_economic_series(config)
    bls[, source := "BLS"]
    results$bls <- bls
  }, error = function(e) {
    cli::cli_alert_warning("BLS fetch failed: {e$message}")
  })

  # BEA NIPA
  tryCatch({
    bea <- fetch_bea_nipa_tables(config)
    bea[, source := "BEA"]
    results$bea <- bea
  }, error = function(e) {
    cli::cli_alert_warning("BEA fetch failed: {e$message}")
  })

  # FRED interest rates
  tryCatch({
    fred <- fetch_fred_interest_rates(config)
    fred[, source := "FRED"]
    results$fred <- fred
  }, error = function(e) {
    cli::cli_alert_warning("FRED fetch failed: {e$message}")
  })

  # AWI
  tryCatch({
    awi <- load_awi_historical(config)
    if ("awi" %in% names(awi) && "year" %in% names(awi)) {
      awi_dt <- data.table::data.table(
        year = awi$year,
        variable = "awi",
        value = awi$awi,
        source = "SSA"
      )
      results$awi <- awi_dt
    }
  }, error = function(e) {
    cli::cli_alert_warning("AWI load failed: {e$message}")
  })

  combined <- data.table::rbindlist(results, fill = TRUE)

  cli::cli_alert_success("Fetched {nrow(combined)} historical economic data points from {length(results)} sources")

  combined
}

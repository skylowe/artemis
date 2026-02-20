#' Fetch Historical Economic Assumptions from APIs
#'
#' @description
#' Fetches historical economic data from BLS, BEA, and FRED APIs.
#' Used when `economics.employment.assumptions_data_source = "api"`.
#'
#' @references
#' - BLS API v2: Productivity, CPI, unemployment
#' - BEA NIPA API: GDP deflator, GDP levels
#' - FRED API: Interest rates
#' - AWI historical from data/raw/SSA_TR{year}/AWI_hist.csv
#'
#' @name fetch_economic_assumptions
NULL

# =============================================================================
# BLS API v2 Wrapper
# =============================================================================

#' Fetch series data from BLS API v2
#'
#' @description
#' Low-level wrapper for BLS API v2 POST endpoint. Handles the 20-year span
#' limit by chunking requests automatically.
#'
#' @param series_ids Character vector of BLS series IDs
#' @param start_year First year
#' @param end_year Last year
#' @param api_key BLS API key
#'
#' @return data.table with columns: series_id, year, period, value
#'
#' @keywords internal
fetch_bls_series <- function(series_ids, start_year, end_year, api_key) {
  checkmate::assert_character(series_ids, min.len = 1)
  checkmate::assert_integerish(start_year, len = 1)
  checkmate::assert_integerish(end_year, len = 1)
  checkmate::assert_string(api_key, min.chars = 1)

  # BLS API v2 limits to 20-year spans per request
  max_span <- 20L
  results <- list()

  year_starts <- seq(start_year, end_year, by = max_span)

  for (chunk_start in year_starts) {
    chunk_end <- min(chunk_start + max_span - 1L, end_year)

    resp <- httr2::request("https://api.bls.gov/publicAPI/v2/timeseries/data/") |>
      httr2::req_body_json(list(
        seriesid = as.list(unname(series_ids)),
        startyear = as.character(chunk_start),
        endyear = as.character(chunk_end),
        registrationkey = api_key
      )) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()

    json <- httr2::resp_body_json(resp)

    if (json$status != "REQUEST_SUCCEEDED") {
      cli::cli_abort(c(
        "BLS API request failed for years {chunk_start}-{chunk_end}",
        "i" = "Status: {json$status}",
        "i" = "Message: {paste(json$message, collapse = '; ')}"
      ))
    }

    for (series in json$Results$series) {
      sid <- series$seriesID
      for (d in series$data) {
        val <- suppressWarnings(as.numeric(d$value))
        if (!is.na(val)) {
          results[[length(results) + 1L]] <- data.table::data.table(
            series_id = sid,
            year = as.integer(d$year),
            period = d$period,
            value = val
          )
        }
      }
    }
  }

  if (length(results) == 0L) {
    cli::cli_abort("BLS API returned no data for series {.val {series_ids}} ({start_year}-{end_year})")
  }

  data.table::rbindlist(results)
}

# =============================================================================
# BLS Economic Series
# =============================================================================

#' Fetch BLS economic series
#'
#' @description
#' Fetches productivity, CPI, average hours, and unemployment from BLS API v2.
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
  if (is.null(emp_config)) {
    cli::cli_abort("config$economics$employment is missing")
  }
  if (is.null(end_year)) end_year <- emp_config$base_year

  api_key <- Sys.getenv("BLS_API_KEY")
  if (api_key == "") {
    cli::cli_abort(c(
      "BLS_API_KEY not found in .Renviron",
      "i" = "Required for fetching historical economic data from BLS",
      "i" = "Register at https://www.bls.gov/developers/home.htm"
    ))
  }

  cli::cli_alert_info("Fetching BLS economic series ({start_year}-{end_year})")

  # BLS series IDs — verified against BLS API v2
  series_map <- list(
    productivity = "PRS85006092",       # Output per hour, nonfarm business (quarterly)
    cpi_w = "CWUR0000SA0",              # CPI-W, all items (monthly)
    avg_weekly_hours = "CES0500000002", # Avg weekly hours, all employees, total private (monthly)
    unemployment_rate = "LNS14000000"   # Unemployment rate, seasonally adjusted (monthly)
  )

  all_ids <- unlist(series_map)
  raw <- fetch_bls_series(all_ids, start_year, end_year, api_key = api_key)

  # Compute annual averages from monthly/quarterly data
  # Monthly: M01-M12, Quarterly: Q01-Q04
  monthly <- raw[grepl("^M(0[1-9]|1[0-2])$", period)]
  quarterly <- raw[grepl("^Q0[1-4]$", period)]
  periodic <- data.table::rbindlist(list(monthly, quarterly))

  if (nrow(periodic) == 0L) {
    cli::cli_abort("BLS API returned no monthly or quarterly data for series {.val {all_ids}}")
  }

  annual <- periodic[, .(value = mean(value, na.rm = TRUE)),
                     by = .(series_id, year)]

  # Map series IDs back to variable names
  id_to_var <- data.table::data.table(
    series_id = unlist(series_map),
    variable = names(series_map)
  )
  result <- merge(annual, id_to_var, by = "series_id")
  result <- result[, .(year, variable, value)]

  # Verify we got data for each series
  fetched_vars <- unique(result$variable)
  missing_vars <- setdiff(names(series_map), fetched_vars)
  if (length(missing_vars) > 0L) {
    cli::cli_abort(c(
      "BLS API returned no data for {length(missing_vars)} series",
      "i" = "Missing: {.val {missing_vars}}",
      "i" = "Series IDs: {.val {series_map[missing_vars]}}"
    ))
  }

  cli::cli_alert_success("Fetched {nrow(result)} BLS economic data points ({length(fetched_vars)} variables)")

  result
}

# =============================================================================
# BEA NIPA Tables
# =============================================================================

#' Fetch BEA NIPA table data
#'
#' @description
#' Fetches GDP deflator and real GDP from BEA NIPA API.
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
  if (is.null(emp_config)) {
    cli::cli_abort("config$economics$employment is missing")
  }
  if (is.null(end_year)) end_year <- emp_config$base_year

  bea_key <- Sys.getenv("BEA_API_KEY")
  if (bea_key == "") {
    cli::cli_abort(c(
      "BEA_API_KEY not found in .Renviron",
      "i" = "Required for fetching NIPA data from BEA",
      "i" = "Register at https://apps.bea.gov/API/signup/"
    ))
  }

  cli::cli_alert_info("Fetching BEA NIPA tables ({start_year}-{end_year})")

  # Fetch a single NIPA table and extract Line 1
  fetch_nipa_line1 <- function(table_name, variable_name) {
    resp <- httr2::request("https://apps.bea.gov/api/data") |>
      httr2::req_url_query(
        UserID = bea_key,
        method = "GetData",
        DataSetName = "NIPA",
        TableName = table_name,
        Frequency = "A",
        Year = paste(start_year:end_year, collapse = ","),
        ResultFormat = "JSON"
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()

    json <- httr2::resp_body_json(resp)
    data_items <- json$BEAAPI$Results$Data

    rows <- list()
    for (item in data_items) {
      if (item$LineNumber == "1") {
        rows[[length(rows) + 1L]] <- data.table::data.table(
          year = as.integer(item$TimePeriod),
          variable = variable_name,
          value = as.numeric(gsub(",", "", item$DataValue))
        )
      }
    }

    if (length(rows) == 0L) {
      cli::cli_abort("BEA API returned no Line 1 data for table {.val {table_name}}")
    }

    data.table::rbindlist(rows)
  }

  # Table 1.1.4 Line 1: GDP Implicit Price Deflator
  gdp_deflator <- fetch_nipa_line1("T10104", "gdp_deflator")

  # Table 1.1.1 Line 1: Real GDP (percent change)
  real_gdp <- fetch_nipa_line1("T10101", "real_gdp_change")

  result <- data.table::rbindlist(list(gdp_deflator, real_gdp))

  cli::cli_alert_success("Fetched {nrow(result)} BEA NIPA data points")

  result
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
  if (is.null(emp_config)) {
    cli::cli_abort("config$economics$employment is missing")
  }
  if (is.null(end_year)) end_year <- emp_config$base_year

  fred_key <- Sys.getenv("FRED_API_KEY")
  if (fred_key == "") {
    cli::cli_abort(c(
      "FRED_API_KEY not found in .Renviron",
      "i" = "Required for fetching interest rate data from FRED",
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
      results[[length(results) + 1L]] <- data.table::data.table(
        year = as.integer(substr(obs$date, 1, 4)),
        variable = "nominal_interest_rate",
        value = val
      )
    }
  }

  if (length(results) == 0L) {
    cli::cli_abort("FRED API returned no data for GS10 series ({start_year}-{end_year})")
  }

  result <- data.table::rbindlist(results)
  saveRDS(result, cache_file)

  cli::cli_alert_success("Fetched {nrow(result)} FRED interest rate data points")

  result
}

# =============================================================================
# FRED Quarterly GDP (for RTP)
# =============================================================================

#' Fetch quarterly RTP (Real GDP / Potential GDP) from FRED
#'
#' @description
#' Fetches quarterly Real GDP (GDPC1) and Real Potential GDP (GDPPOT) from FRED,
#' then computes RTP = GDPC1 / GDPPOT. This provides historical quarterly RTP
#' needed for the D(RTP) distributed lags in the unemployment rate equations.
#'
#' @param config Full ARTEMIS config
#' @param start_year First year (default 2015, only need a few years of lags)
#' @param end_year Last year (default: base_year from config)
#'
#' @return data.table with columns: year, quarter, rtp
#'
#' @export
fetch_fred_quarterly_rtp <- function(config, start_year = 2015, end_year = NULL) {
  emp_config <- config$economics$employment
  if (is.null(emp_config)) {
    cli::cli_abort("config$economics$employment is missing")
  }
  if (is.null(end_year)) end_year <- emp_config$base_year

  fred_key <- Sys.getenv("FRED_API_KEY")
  if (fred_key == "") {
    cli::cli_abort(c(
      "FRED_API_KEY not found in .Renviron",
      "i" = "Required for fetching GDP data from FRED",
      "i" = "Register at https://fred.stlouisfed.org/docs/api/api_key.html"
    ))
  }

  cli::cli_alert_info("Fetching FRED quarterly GDP for RTP ({start_year}-{end_year})")

  cache_dir <- here::here("data/cache/fred")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  cache_file <- file.path(cache_dir, sprintf("rtp_quarterly_%d_%d.rds", start_year, end_year))
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached FRED quarterly RTP")
    return(readRDS(cache_file))
  }

  # Helper to fetch a single FRED quarterly series
  fetch_fred_quarterly <- function(series_id) {
    resp <- httr2::request("https://api.stlouisfed.org/fred/series/observations") |>
      httr2::req_url_query(
        series_id = series_id,
        api_key = fred_key,
        file_type = "json",
        observation_start = paste0(start_year, "-01-01"),
        observation_end = paste0(end_year, "-12-31"),
        frequency = "q"
      ) |>
      httr2::req_timeout(30) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()

    json <- httr2::resp_body_json(resp)

    rows <- list()
    for (obs in json$observations) {
      val <- suppressWarnings(as.numeric(obs$value))
      if (!is.na(val)) {
        date <- as.Date(obs$date)
        rows[[length(rows) + 1L]] <- data.table::data.table(
          year = as.integer(format(date, "%Y")),
          quarter = as.integer((as.integer(format(date, "%m")) - 1L) %/% 3L + 1L),
          value = val
        )
      }
    }

    if (length(rows) == 0L) {
      cli::cli_abort("FRED API returned no data for {series_id} ({start_year}-{end_year})")
    }

    data.table::rbindlist(rows)
  }

  # Fetch both series
  gdpc1 <- fetch_fred_quarterly("GDPC1")    # Real GDP
  gdppot <- fetch_fred_quarterly("GDPPOT")  # Potential GDP

  data.table::setnames(gdpc1, "value", "real_gdp")
  data.table::setnames(gdppot, "value", "potential_gdp")

  # Merge and compute RTP
  rtp_dt <- merge(gdpc1, gdppot, by = c("year", "quarter"))

  if (nrow(rtp_dt) == 0L) {
    cli::cli_abort("No overlapping quarters between GDPC1 and GDPPOT")
  }

  rtp_dt[, rtp := real_gdp / potential_gdp]
  result <- rtp_dt[, .(year, quarter, rtp)]
  data.table::setorder(result, year, quarter)

  saveRDS(result, cache_file)

  cli::cli_alert_success(
    "Fetched quarterly RTP from FRED: {nrow(result)} quarters ({min(result$year)}Q{min(result[year == min(year)]$quarter)}-{max(result$year)}Q{max(result[year == max(year)]$quarter)})"
  )

  result
}

# =============================================================================
# Verification: API vs TR2025 Tables
# =============================================================================

#' Verify API-fetched historical data against TR2025 table values
#'
#' @description
#' Loads full TR2025 V.B1/V.B2 data (including historical years) and compares
#' against API-fetched values for overlapping years and variables. Reports
#' discrepancies that may indicate wrong API series or data transformations.
#'
#' @param api_data data.table from fetch_historical_economic_assumptions()
#' @param config Full ARTEMIS config
#' @param tolerance Maximum acceptable absolute difference (default 0.5)
#'
#' @return list with: valid (logical), discrepancies (data.table), summary (character)
#'
#' @export
verify_historical_vs_tr2025 <- function(api_data, config, tolerance = 0.5) {
  checkmate::assert_data_table(api_data)
  checkmate::assert_list(config)

  cli::cli_h2("Verifying API data against TR table values")

  # Load full TR data (all years including historical)
  saved_source <- config$economics$employment$assumptions_data_source
  config$economics$employment$assumptions_data_source <- "tr2025"
  tr_data <- load_tr_economic_assumptions(config)
  config$economics$employment$assumptions_data_source <- saved_source

  # Standardize column names
  api_compare <- api_data[, .(year, variable, api_value = value)]
  tr_compare <- tr_data[, .(year, variable, tr_value = value)]

  # Find overlapping years and variables
  comparison <- merge(api_compare, tr_compare, by = c("year", "variable"))

  if (nrow(comparison) == 0L) {
    cli::cli_alert_warning("No overlapping year/variable combinations for verification")
    return(list(valid = TRUE, discrepancies = data.table::data.table(), summary = "No overlap"))
  }

  comparison[, abs_diff := abs(api_value - tr_value)]
  comparison[tr_value != 0, pct_diff := abs_diff / abs(tr_value) * 100]

  discrepancies <- comparison[abs_diff > tolerance]

  if (nrow(discrepancies) > 0L) {
    cli::cli_alert_warning("{nrow(discrepancies)} discrepancies exceed tolerance of {tolerance}")
    # Summarize by variable
    disc_summary <- discrepancies[, .(
      n_years = .N,
      max_abs_diff = max(abs_diff),
      max_pct_diff = max(pct_diff, na.rm = TRUE)
    ), by = variable]
    print(disc_summary)
  } else {
    cli::cli_alert_success("All {nrow(comparison)} API-vs-TR comparisons within tolerance ({tolerance})")
  }

  list(
    valid = nrow(discrepancies) == 0L,
    n_compared = nrow(comparison),
    discrepancies = discrepancies,
    summary = comparison
  )
}

# =============================================================================
# Combined Historical Economic Data
# =============================================================================

#' Fetch all historical economic assumptions from APIs
#'
#' @description
#' Master function that combines data from BLS, BEA, FRED, and AWI sources.
#' Returns a unified data.table of historical economic variables.
#' Errors if any API source fails (no silent fallbacks).
#'
#' @param config Full ARTEMIS config
#'
#' @return data.table with columns: year, variable, value, source
#'
#' @export
fetch_historical_economic_assumptions <- function(config) {
  data_source <- config$economics$employment$assumptions_data_source
  if (is.null(data_source)) {
    cli::cli_abort(c(
      "config$economics$employment$assumptions_data_source is not set",
      "i" = "Set to {.val api} (fetch from BLS/BEA/FRED) or {.val tr2025} (use TR tables only)"
    ))
  }

  if (data_source != "api") {
    cli::cli_alert_info("Skipping API fetch (assumptions_data_source = {.val {data_source}})")
    return(data.table::data.table(
      year = integer(), variable = character(), value = numeric(), source = character()
    ))
  }

  cli::cli_h1("Fetching Historical Economic Assumptions from APIs")

  # BLS series (productivity, CPI, average hours, unemployment)
  bls <- fetch_bls_economic_series(config)
  bls[, source := "BLS"]

  # BEA NIPA (GDP deflator, real GDP change)
  bea <- fetch_bea_nipa_tables(config)
  bea[, source := "BEA"]

  # FRED (10-year Treasury rate)
  fred <- fetch_fred_interest_rates(config)
  fred[, source := "FRED"]

  # AWI (Average Wage Index from SSA file)
  awi_raw <- load_awi_historical(config)
  awi <- data.table::data.table(
    year = awi_raw$year,
    variable = "awi",
    value = awi_raw$awi,
    source = "SSA"
  )

  combined <- data.table::rbindlist(list(bls, bea, fred, awi), fill = TRUE)

  # Verify against TR table values
  verification <- verify_historical_vs_tr2025(combined, config)
  if (!verification$valid) {
    cli::cli_alert_warning(
      "API-vs-TR verification found {nrow(verification$discrepancies)} discrepancies (see above)"
    )
  }

  cli::cli_alert_success(
    "Fetched {nrow(combined)} historical economic data points from 4 sources (BLS, BEA, FRED, SSA)"
  )

  combined
}

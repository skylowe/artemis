#' Load cached RDS file
#'
#' @description
#' Load data from a cached RDS file. Optionally abort or warn if missing.
#'
#' @param cache_file Path to cache file (.rds)
#' @param on_missing Action when file missing: "abort" (default), "warn", or "null"
#' @param abort_message Custom abort message (uses default if NULL)
#' @param verbose Whether to print status messages (default: TRUE)
#'
#' @return Data from cache, or NULL if on_missing = "null"/"warn"
#' @export
load_cached_rds <- function(cache_file, on_missing = "abort",
                            abort_message = NULL, verbose = TRUE) {
  checkmate::assert_string(cache_file)
  checkmate::assert_choice(on_missing, c("abort", "warn", "null"))

  if (file.exists(cache_file)) {
    if (verbose) cli::cli_alert_success("Loading from cache: {.file {basename(cache_file)}}")
    return(readRDS(cache_file))
  }

  # File doesn't exist
  if (on_missing == "abort") {
    msg <- abort_message %||% "Cache file not found: {.file {cache_file}}"
    cli::cli_abort(msg)
  } else if (on_missing == "warn") {
    cli::cli_alert_warning("Cache file not found: {.file {basename(cache_file)}}")
    return(NULL)
  } else {
    return(NULL)
  }
}

#' Load cached data or fetch from source
#'
#' @param cache_file Path to cache file (.rds)
#' @param fetch_fn Function to call if cache doesn't exist
#' @param save_cache Whether to save fetched data to cache (default: FALSE)
#' @param verbose Whether to print status messages (default: TRUE)
#'
#' @return Data from cache or freshly fetched
#' @export
load_or_fetch <- function(cache_file, fetch_fn, save_cache = FALSE, verbose = TRUE) {
  checkmate::assert_string(cache_file)
  checkmate::assert_function(fetch_fn)

  if (file.exists(cache_file)) {
    if (verbose) cli::cli_alert_success("Loading from cache: {.file {basename(cache_file)}}")
    return(readRDS(cache_file))
  }

  if (verbose) cli::cli_alert_info("Cache miss, fetching: {.file {basename(cache_file)}}")
  result <- fetch_fn()

  if (save_cache) {
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(result, cache_file)
    if (verbose) cli::cli_alert_success("Saved to cache: {.file {basename(cache_file)}}")
  }

  result
}

#' Load cached data for multiple years
#'
#' @param years Integer vector of years to load
#' @param cache_pattern sprintf pattern with single %d for year
#' @param fetch_fn Function(year) to fetch data for a single year
#' @param year_col Column name to add for year (default: "year")
#' @param verbose Whether to print status messages
#'
#' @return Combined data.table with all years
#' @export
load_or_fetch_years <- function(years, cache_pattern, fetch_fn,
                                 year_col = "year", verbose = TRUE) {
  checkmate::assert_integerish(years, min.len = 1)
  checkmate::assert_string(cache_pattern)
  checkmate::assert_function(fetch_fn)

  data.table::rbindlist(lapply(years, function(yr) {
    cache_file <- sprintf(cache_pattern, yr)
    dt <- load_or_fetch(cache_file, function() fetch_fn(yr), verbose = verbose)
    if (!is.null(dt) && !year_col %in% names(dt)) {
      dt[[year_col]] <- yr
    }
    dt
  }), fill = TRUE)
}

#' Load cached data for multiple years (cache only)
#'
#' @description
#' Load RDS files for multiple years from cache. Unlike load_or_fetch_years,
#' this does not fetch missing data - it only loads from existing cache files.
#'
#' @param years Integer vector of years to load
#' @param cache_pattern sprintf pattern with single %d for year
#' @param year_col Column name to add for year (default: "year")
#' @param on_missing Action when file missing: "skip" (default), "warn", or "abort"
#' @param verbose Whether to print status messages
#'
#' @return Combined data.table with all available years
#' @export
load_cached_multi_year <- function(years, cache_pattern, year_col = "year",
                                    on_missing = "skip", verbose = TRUE) {
  checkmate::assert_integerish(years, min.len = 1)
  checkmate::assert_string(cache_pattern)
  checkmate::assert_choice(on_missing, c("skip", "warn", "abort"))

  results <- lapply(years, function(yr) {
    cache_file <- sprintf(cache_pattern, yr)
    if (file.exists(cache_file)) {
      dt <- readRDS(cache_file)
      if (!is.null(dt) && !year_col %in% names(dt)) {
        dt[[year_col]] <- yr
      }
      dt
    } else {
      if (on_missing == "abort") {
        cli::cli_abort("Cache file not found for year {yr}: {.file {cache_file}}")
      } else if (on_missing == "warn") {
        cli::cli_alert_warning("Cache file not found for year {yr}")
      }
      NULL
    }
  })

  result <- data.table::rbindlist(results[!sapply(results, is.null)], fill = TRUE)
  if (verbose && nrow(result) > 0) {
    cli::cli_alert_success("Loaded {length(unique(result[[year_col]]))} years from cache")
  }
  result
}

#' Load DACA data from cache
#'
#' @description
#' Load DHS DACA grants and stock data from cache files.
#'
#' @param grants_file Path to DACA grants cache file
#' @param stock_file Path to DACA stock cache file
#' @param fetch_grants_fn Function to fetch grants if not cached
#' @param fetch_stock_fn Function to fetch stock if not cached
#' @param verbose Whether to print status messages
#'
#' @return List with grants and stock data
#' @export
load_cached_dhs_daca_data <- function(grants_file = "data/cache/dhs/daca_grants.rds",
                                       stock_file = "data/cache/dhs/daca_stock.rds",
                                       fetch_grants_fn = NULL,
                                       fetch_stock_fn = NULL,
                                       verbose = TRUE) {
  grants <- if (file.exists(grants_file)) {
    if (verbose) cli::cli_alert_success("Loading DACA grants from cache")
    readRDS(grants_file)
  } else if (!is.null(fetch_grants_fn)) {
    if (verbose) cli::cli_alert_info("Fetching DACA grants")
    fetch_grants_fn()
  } else {
    cli::cli_abort("DACA grants cache not found and no fetch function provided")
  }

  stock <- if (file.exists(stock_file)) {
    if (verbose) cli::cli_alert_success("Loading DACA stock from cache")
    readRDS(stock_file)
  } else if (!is.null(fetch_stock_fn)) {
    if (verbose) cli::cli_alert_info("Fetching DACA stock")
    fetch_stock_fn()
  } else {
    cli::cli_abort("DACA stock cache not found and no fetch function provided")
  }

  list(grants = grants, stock = stock)
}

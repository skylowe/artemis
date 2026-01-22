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

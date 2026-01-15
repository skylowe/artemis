#' API Helper Functions
#'
#' Utility functions for making API requests with rate limiting,
#' retry logic, and error handling.
#'
#' @name api_helpers
NULL

#' Execute API request with retry logic
#'
#' @description
#' Performs an HTTP request with automatic retries on failure and
#' exponential backoff between attempts.
#'
#' @param request An httr2 request object
#' @param max_retries Integer: maximum number of retry attempts (default: 3)
#' @param backoff_factor Numeric: multiplier for exponential backoff (default: 2)
#' @param retry_on_status Integer vector: HTTP status codes that trigger retry
#'
#' @return httr2 response object
#'
#' @export
api_request_with_retry <- function(request,
                                   max_retries = 3,
                                   backoff_factor = 2,
                                   retry_on_status = c(429, 500, 502, 503, 504)) {
  checkmate::assert_class(request, "httr2_request")
  checkmate::assert_integerish(max_retries, lower = 1, upper = 10)
  checkmate::assert_number(backoff_factor, lower = 1, upper = 5)

  last_error <- NULL


  for (attempt in seq_len(max_retries + 1)) {
    tryCatch({
      resp <- httr2::req_perform(request)

      # Check if we got a retry-able status code
      status <- httr2::resp_status(resp)
      if (status %in% retry_on_status && attempt <= max_retries) {
        wait_time <- backoff_factor^(attempt - 1)
        cli::cli_alert_warning(
          "Got status {status}, retrying in {wait_time}s (attempt {attempt}/{max_retries})"
        )
        Sys.sleep(wait_time)
        next
      }

      return(resp)

    }, error = function(e) {
      last_error <<- e

      if (attempt <= max_retries) {
        wait_time <- backoff_factor^(attempt - 1)
        cli::cli_alert_warning(
          "Request failed: {conditionMessage(e)}. Retrying in {wait_time}s (attempt {attempt}/{max_retries})"
        )
        Sys.sleep(wait_time)
      }
    })
  }

  # If we get here, all retries failed
  cli::cli_abort(
    c("API request failed after {max_retries} retries",
      "x" = "Last error: {conditionMessage(last_error)}")
  )
}

#' Create a base API request with common settings
#'
#' @description
#' Creates an httr2 request object with standard headers and timeout settings.
#'
#' @param url Character: the URL to request
#' @param timeout_seconds Numeric: request timeout (default: 30)
#' @param user_agent Character: user agent string
#'
#' @return httr2 request object
#'
#' @export
create_api_request <- function(url,
                               timeout_seconds = 30,
                               user_agent = "ARTEMIS OASDI Projection Model (R)") {
  checkmate::assert_string(url)
  checkmate::assert_number(timeout_seconds, lower = 1, upper = 300)

  httr2::request(url) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_error(is_error = function(resp) FALSE)  # Handle errors manually
}

#' Rate-limited API request execution
#'
#' @description
#' Executes multiple API requests while respecting rate limits.
#' Introduces delays between requests as needed.
#'
#' @param requests List of httr2 request objects
#' @param rate_limit_per_minute Integer: maximum requests per minute
#' @param progress Logical: show progress bar (default: TRUE)
#'
#' @return List of httr2 response objects
#'
#' @export
execute_rate_limited <- function(requests,
                                 rate_limit_per_minute = 60,
                                 progress = TRUE) {
  checkmate::assert_list(requests, min.len = 1)
  checkmate::assert_integerish(rate_limit_per_minute, lower = 1)

  # Calculate delay between requests (in seconds)
  delay <- 60 / rate_limit_per_minute

  n_requests <- length(requests)
  responses <- vector("list", n_requests)

  if (progress && n_requests > 1) {
    cli::cli_progress_bar(
      "Fetching data",
      total = n_requests,
      format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )
  }

  for (i in seq_along(requests)) {
    responses[[i]] <- api_request_with_retry(requests[[i]])

    if (progress && n_requests > 1) {
      cli::cli_progress_update()
    }

    # Add delay between requests (except after the last one)
    if (i < n_requests && delay > 0) {
      Sys.sleep(delay)
    }
  }

  if (progress && n_requests > 1) {
    cli::cli_progress_done()
  }

  responses
}

#' Check API response for errors
#'
#' @description
#' Checks an httr2 response for HTTP errors and returns informative messages.
#'
#' @param resp httr2 response object
#' @param context Character: description of what was being fetched (for error messages)
#'
#' @return Invisible TRUE if successful, otherwise throws error
#'
#' @export
check_api_response <- function(resp, context = "API request") {
  checkmate::assert_class(resp, "httr2_response")

 status <- httr2::resp_status(resp)

  if (status >= 400) {
    status_desc <- httr2::resp_status_desc(resp)

    # Try to get error details from response body
    error_body <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) "(unable to read response body)"
    )

    cli::cli_abort(
      c("{context} failed with status {status}: {status_desc}",
        "i" = "Response: {substr(error_body, 1, 500)}")
    )
  }

  invisible(TRUE)
}

#' Get environment variable with informative error
#'
#' @description
#' Retrieves an environment variable, throwing an informative error if not set.
#'
#' @param var_name Character: name of environment variable
#' @param required Logical: if TRUE, throw error when not found (default: TRUE)
#'
#' @return Character: the environment variable value
#'
#' @export
get_api_key <- function(var_name, required = TRUE) {
  checkmate::assert_string(var_name)

  value <- Sys.getenv(var_name)

  if (value == "" && required) {
    cli::cli_abort(
      c("Required API key not found",
        "x" = "Environment variable {.envvar {var_name}} is not set",
        "i" = "Add it to your {.file .Renviron} file: {var_name}=your_key_here")
    )
  }

  value
}

#' Parse JSON response with error handling
#'
#' @description
#' Parses JSON from an httr2 response with informative error messages.
#'
#' @param resp httr2 response object
#' @param simplify_vector Logical: simplify JSON arrays to vectors (default: TRUE)
#'
#' @return Parsed JSON as R object
#'
#' @export
parse_json_response <- function(resp, simplify_vector = TRUE) {
  checkmate::assert_class(resp, "httr2_response")

  tryCatch({
    httr2::resp_body_json(resp, simplifyVector = simplify_vector)
  }, error = function(e) {
    # Try to get raw body for debugging
    raw_body <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e2) "(unable to read body)"
    )

    cli::cli_abort(
      c("Failed to parse JSON response",
        "x" = "Parse error: {conditionMessage(e)}",
        "i" = "Response starts with: {substr(raw_body, 1, 200)}")
    )
  })
}

#' Download file with progress
#'
#' @description
#' Downloads a file from a URL with progress indication and caching support.
#'
#' @param url Character: URL to download
#' @param dest_path Character: destination file path
#' @param overwrite Logical: overwrite existing file (default: FALSE)
#' @param quiet Logical: suppress progress messages (default: FALSE)
#'
#' @return Character: path to downloaded file
#'
#' @export
download_with_cache <- function(url,
                                dest_path,
                                overwrite = FALSE,
                                quiet = FALSE) {
  checkmate::assert_string(url)
  checkmate::assert_string(dest_path)
  checkmate::assert_path_for_output(dest_path, overwrite = overwrite)

  # Check if file already exists
  if (file.exists(dest_path) && !overwrite) {
    if (!quiet) {
      cli::cli_alert_info("Using cached file: {.file {basename(dest_path)}}")
    }
    return(dest_path)
  }

  # Create directory if needed
  dir.create(dirname(dest_path), showWarnings = FALSE, recursive = TRUE)

  if (!quiet) {
    cli::cli_alert_info("Downloading: {.url {url}}")
  }

  # Download the file
  tryCatch({
    req <- create_api_request(url, timeout_seconds = 120)
    resp <- api_request_with_retry(req)
    check_api_response(resp, "File download")

    # Write to file
    writeBin(httr2::resp_body_raw(resp), dest_path)

    if (!quiet) {
      file_size <- file.size(dest_path)
      cli::cli_alert_success(
        "Downloaded {.file {basename(dest_path)}} ({prettyunits::pretty_bytes(file_size)})"
      )
    }

    dest_path
  }, error = function(e) {
    # Clean up partial download
    if (file.exists(dest_path)) {
      unlink(dest_path)
    }
    cli::cli_abort(
      c("Failed to download file",
        "x" = "{conditionMessage(e)}",
        "i" = "URL: {url}")
    )
  })
}

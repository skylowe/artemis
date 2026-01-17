#' Human Mortality Database (HMD) Data Acquisition
#'
#' Functions for downloading and caching mortality data from the Human Mortality
#' Database (mortality.org). Requires free registration and credentials stored
#' in .Renviron as HMD_USERNAME and HMD_PASSWORD.
#'
#' @name hmd_data
NULL

#' Get HMD credentials from environment
#'
#' @description
#' Retrieves HMD username and password from environment variables.
#' Credentials should be stored in .Renviron as:
#'   HMD_USERNAME=your_email
#'   HMD_PASSWORD=your_password
#'
#' @return List with username and password
#'
#' @keywords internal
get_hmd_credentials <- function() {
  username <- Sys.getenv("HMD_USERNAME")
  password <- Sys.getenv("HMD_PASSWORD")

  if (username == "" || password == "") {
    cli::cli_abort(c(
      "HMD credentials not found in environment variables.",
      "i" = "Register for free at {.url https://www.mortality.org}",
      "i" = "Add to your .Renviron file:",
      " " = "HMD_USERNAME=your_email",
      " " = "HMD_PASSWORD=your_password"
    ))
  }

  list(username = username, password = password)
}

#' Check if HMDHFDplus package is available
#'
#' @keywords internal
check_hmd_package <- function() {
  if (!requireNamespace("HMDHFDplus", quietly = TRUE)) {
    cli::cli_abort(c(

"Package {.pkg HMDHFDplus} is required for HMD data access.",
      "i" = "Install with: {.code install.packages('HMDHFDplus')}"
    ))
  }
}

#' Fetch HMD life tables for USA
#'
#' @description
#' Downloads period life tables from the Human Mortality Database for the USA.
#' Data is cached locally to avoid repeated downloads.
#'
#' @param sex Character: "male", "female", or "both"
#' @param format Character: age x year format. Options include:
#'   - "1x1": single year of age, single calendar year (default)
#'   - "1x5": single year of age, 5-year periods
#'   - "5x1": 5-year age groups, single calendar year
#' @param cache_dir Character: directory for caching downloaded files
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, age, sex, mx, qx, ax, lx, dx, Lx, Tx, ex
#'
#' @details
#' HMD USA data covers 1933-2023 (as of 2024).
#'
#' The HMD applies demographic methods to improve data quality:
#' - Correction for age misreporting
#' - Smoothing at oldest ages
#' - Consistent methodology across years
#'
#' This makes HMD data particularly valuable for ages 65+ where
#' raw NCHS data may have quality issues.
#'
#' @export
fetch_hmd_life_tables <- function(sex = c("both", "male", "female"),
                                   format = "1x1",
                                   cache_dir = "data/cache/hmd",
                                   force_download = FALSE) {
  sex <- match.arg(sex)
  check_hmd_package()

  # Create cache directory if needed
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Define item codes for HMD
  item_codes <- list(
    male = paste0("mltper_", format),    # Male period life table
    female = paste0("fltper_", format)   # Female period life table
  )

  results <- list()

  sexes_to_fetch <- if (sex == "both") c("male", "female") else sex

  for (sex_val in sexes_to_fetch) {
    cache_file <- file.path(cache_dir, sprintf("usa_%s_lt_%s.rds", sex_val, format))

    if (file.exists(cache_file) && !force_download) {
      cli::cli_alert_success("Loading cached HMD {sex_val} life table")
      results[[sex_val]] <- readRDS(cache_file)
    } else {
      cli::cli_alert_info("Downloading HMD {sex_val} life table from mortality.org...")

      creds <- get_hmd_credentials()

      tryCatch({
        dt <- HMDHFDplus::readHMDweb(
          CNTRY = "USA",
          item = item_codes[[sex_val]],
          username = creds$username,
          password = creds$password,
          fixup = TRUE
        )

        # Convert to data.table and standardize column names
        dt <- data.table::as.data.table(dt)

        # Standardize column names (HMD uses Title Case)
        old_names <- c("Year", "Age", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
        new_names <- c("year", "age", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")

        # Only rename columns that exist
        for (i in seq_along(old_names)) {
          if (old_names[i] %in% names(dt)) {
            data.table::setnames(dt, old_names[i], new_names[i])
          }
        }

        # Add sex column
        dt[, sex := sex_val]

        # Remove any extra columns (like OpenInterval, AgeInterval)
        keep_cols <- intersect(c("year", "age", "sex", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex"),
                               names(dt))
        dt <- dt[, ..keep_cols]

        # Cache the result
        saveRDS(dt, cache_file)
        cli::cli_alert_success("Cached HMD {sex_val} life table to {cache_file}")

        results[[sex_val]] <- dt

      }, error = function(e) {
        cli::cli_abort(c(
          "Failed to download HMD data for {sex_val}",
          "x" = conditionMessage(e),
          "i" = "Check your credentials and internet connection"
        ))
      })
    }
  }

  # Combine results
  dt <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setcolorder(dt, c("year", "age", "sex"))
  data.table::setorder(dt, year, sex, age)

  cli::cli_alert_success(
    "Retrieved HMD life tables: {length(unique(dt$year))} years, ages {min(dt$age)}-{max(dt$age)}"
  )

  dt
}

#' Fetch HMD death rates for USA
#'
#' @description
#' Downloads central death rates (mx) from the Human Mortality Database.
#'
#' @param sex Character: "male", "female", or "both"
#' @param format Character: age x year format (default "1x1")
#' @param cache_dir Character: directory for caching
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, age, sex, mx
#'
#' @export
fetch_hmd_death_rates <- function(sex = c("both", "male", "female"),
                                   format = "1x1",
                                   cache_dir = "data/cache/hmd",
                                   force_download = FALSE) {
  sex <- match.arg(sex)
  check_hmd_package()

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_file <- file.path(cache_dir, sprintf("usa_mx_%s.rds", format))

  if (file.exists(cache_file) && !force_download) {
    cli::cli_alert_success("Loading cached HMD death rates")
    return(readRDS(cache_file))
  }

  cli::cli_alert_info("Downloading HMD death rates from mortality.org...")

  creds <- get_hmd_credentials()

  tryCatch({
    dt <- HMDHFDplus::readHMDweb(
      CNTRY = "USA",
      item = paste0("Mx_", format),
      username = creds$username,
      password = creds$password,
      fixup = TRUE
    )

    dt <- data.table::as.data.table(dt)

    # Standardize column names
    if ("Year" %in% names(dt)) data.table::setnames(dt, "Year", "year")
    if ("Age" %in% names(dt)) data.table::setnames(dt, "Age", "age")
    if ("Female" %in% names(dt)) data.table::setnames(dt, "Female", "female")
    if ("Male" %in% names(dt)) data.table::setnames(dt, "Male", "male")

    # Reshape from wide to long if needed
    if ("female" %in% names(dt) && "male" %in% names(dt)) {
      dt <- data.table::melt(
        dt,
        id.vars = c("year", "age"),
        measure.vars = c("male", "female"),
        variable.name = "sex",
        value.name = "mx"
      )
      dt[, sex := as.character(sex)]
    }

    # Filter by sex if requested
    if (sex != "both") {
      dt <- dt[sex == !!sex]
    }

    data.table::setorder(dt, year, sex, age)

    saveRDS(dt, cache_file)
    cli::cli_alert_success("Cached HMD death rates to {cache_file}")

    dt

  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download HMD death rates",
      "x" = conditionMessage(e)
    ))
  })
}

#' Fetch HMD population data for USA
#'
#' @description
#' Downloads population estimates from the Human Mortality Database.
#'
#' @param format Character: age x year format (default "1x1")
#' @param cache_dir Character: directory for caching
#' @param force_download Logical: if TRUE, re-download even if cached
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @export
fetch_hmd_population <- function(format = "1x1",
                                  cache_dir = "data/cache/hmd",
                                  force_download = FALSE) {
  check_hmd_package()

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_file <- file.path(cache_dir, sprintf("usa_pop_%s.rds", format))

  if (file.exists(cache_file) && !force_download) {
    cli::cli_alert_success("Loading cached HMD population data")
    return(readRDS(cache_file))
  }

  cli::cli_alert_info("Downloading HMD population data from mortality.org...")

  creds <- get_hmd_credentials()

  tryCatch({
    dt <- HMDHFDplus::readHMDweb(
      CNTRY = "USA",
      item = "Population",
      username = creds$username,
      password = creds$password,
      fixup = TRUE
    )

    dt <- data.table::as.data.table(dt)

    # Standardize names
    if ("Year" %in% names(dt)) data.table::setnames(dt, "Year", "year")
    if ("Age" %in% names(dt)) data.table::setnames(dt, "Age", "age")

    # HMD population has Female1, Female2, Male1, Male2 (Jan 1 and Dec 31)
    # Use January 1 estimates (Female1, Male1)
    if ("Female1" %in% names(dt)) {
      dt_long <- data.table::melt(
        dt[, .(year, age, male = Male1, female = Female1)],
        id.vars = c("year", "age"),
        measure.vars = c("male", "female"),
        variable.name = "sex",
        value.name = "population"
      )
      dt_long[, sex := as.character(sex)]
      dt <- dt_long
    }

    data.table::setorder(dt, year, sex, age)

    saveRDS(dt, cache_file)
    cli::cli_alert_success("Cached HMD population to {cache_file}")

    dt

  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download HMD population data",
      "x" = conditionMessage(e)
    ))
  })
}

#' Compare HMD and calculated qx values
#'
#' @description
#' Compares death probabilities from HMD with our calculated values
#' to identify discrepancies, particularly at older ages.
#'
#' @param qx_calculated data.table with our calculated qx (year, age, sex, qx)
#' @param years Integer vector of years to compare
#' @param ages Integer vector of ages to compare (default: all)
#'
#' @return data.table with comparison metrics
#'
#' @export
compare_qx_with_hmd <- function(qx_calculated,
                                 years = 2010:2019,
                                 ages = NULL) {
  # Fetch HMD data
  hmd_lt <- fetch_hmd_life_tables(sex = "both")

  # Filter to comparison years
  hmd_qx <- hmd_lt[year %in% years, .(year, age, sex, qx_hmd = qx)]

  # Filter our data
  our_qx <- qx_calculated[year %in% years, .(year, age, sex, qx_ours = qx)]

  # Merge
  comparison <- merge(our_qx, hmd_qx, by = c("year", "age", "sex"), all = FALSE)

  # Filter ages if specified
  if (!is.null(ages)) {
    comparison <- comparison[age %in% ages]
  }

  # Calculate differences
  comparison[, `:=`(
    abs_diff = qx_ours - qx_hmd,
    rel_diff = (qx_ours - qx_hmd) / qx_hmd,
    pct_diff = 100 * (qx_ours - qx_hmd) / qx_hmd
  )]

  # Summary by age group
  comparison[, age_group := data.table::fcase(
    age == 0, "0 (infant)",
    age == 1, "1",
    age >= 2 & age <= 14, "2-14",
    age >= 15 & age <= 49, "15-49",
    age >= 50 & age <= 64, "50-64",
    age >= 65 & age <= 84, "65-84",
    age >= 85 & age <= 99, "85-99",
    age >= 100, "100+"
  )]

  cli::cli_h2("qx Comparison: Our Calculation vs HMD")
  cli::cli_alert_info("Compared {nrow(comparison)} qx values for years {min(years)}-{max(years)}")

  # Summary table
  summary_dt <- comparison[, .(
    n_obs = .N,
    mean_pct_diff = mean(pct_diff, na.rm = TRUE),
    median_pct_diff = median(pct_diff, na.rm = TRUE),
    max_abs_pct_diff = max(abs(pct_diff), na.rm = TRUE),
    within_5pct = mean(abs(pct_diff) <= 5, na.rm = TRUE)
  ), by = .(age_group, sex)]

  data.table::setorder(summary_dt, sex, age_group)

  cli::cli_h3("Summary by Age Group")
  print(summary_dt)

  invisible(comparison)
}

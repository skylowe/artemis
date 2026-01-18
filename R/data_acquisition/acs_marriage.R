#' ACS Marriage Data Acquisition
#'
#' @description
#' Functions for fetching American Community Survey (ACS) data on new marriages.
#' Starting in 2008, ACS asks if a person was married in the last 12 months,
#' enabling construction of marriage grids by age-of-husband × age-of-wife.
#'
#' This data is used in the MARRIAGE subprocess (Section 1.6) for:
#' - Calculating age-specific marriage rates
#' - Updating MarGrid distributions for 2008-2022
#' - Validating against NCHS total marriage counts
#'
#' @name acs_marriage
NULL

# =============================================================================
# CONSTANTS AND CONFIGURATION
# =============================================================================
#' @keywords internal
ACS_MARRIAGE_AGE_GROUPS <- list(
  group_15_19 = 15:19,
  group_20_24 = 20:24,
  group_25_29 = 25:29,
  group_30_34 = 30:34,
  group_35_44 = 35:44,
  group_45_54 = 45:54,

  group_55_64 = 55:64,
  group_65_plus = 65:100
)

# =============================================================================
# MAIN FETCHING FUNCTIONS
# =============================================================================

#' Fetch ACS new marriages by age of husband × age of wife
#'
#' @description
#' Downloads ACS PUMS data for persons married in the last 12 months (MARHM=1),
#' then links spouses within households to create marriage grids by
#' age-of-husband crossed with age-of-wife.
#'
#' @param years Integer vector of ACS years (2007-2023; 2020 skipped)
#' @param min_age Minimum age to include (default: 15)
#' @param max_age Maximum age to include (default: 99, with 100+ grouped)
#' @param cache_dir Character: directory for caching downloaded files
#'
#' @return list with:
#'   - grids: List of matrices by year (rows = husband age, cols = wife age)
#'   - summary: data.table with summary statistics by year
#'   - by_age_group: data.table with marriages by age groups (for validation)
#'
#' @details
#' Uses the MARHM (married in past 12 months) variable introduced in 2008.
#' Links spouses within households using SERIALNO (household ID).
#'
#' For 2007, the grid is extrapolated backward from 2008 since MARHM was not
#' available until 2008. Per TR2025, this uses proportional scaling from the
#' 2008 distribution.
#'
#' Note: ACS 2020 1-year data was not released due to COVID-19 data quality issues.
#'
#' @export
fetch_acs_new_marriages <- function(years = 2007:2022,
                                     min_age = 15,
                                     max_age = 99,
                                     cache_dir = here::here("data/cache/acs_marriage")) {

  # Validate inputs
  checkmate::assert_integerish(years, lower = 2007, upper = 2024, min.len = 1)
  checkmate::assert_int(min_age, lower = 14, upper = 99)
  checkmate::assert_int(max_age, lower = min_age, upper = 100)

  # Create cache directory
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Get API key
  api_key <- get_api_key("CENSUS_KEY")

  grids <- list()
  summaries <- list()

  # Track if 2007 was requested (needs extrapolation from 2008)
  needs_2007 <- 2007 %in% years

  for (yr in years) {
    # Skip 2020 - ACS 1-year not released due to COVID
    if (yr == 2020) {
      cli::cli_alert_warning("Skipping 2020 - ACS 1-year not released due to COVID")
      next
    }

    # Skip 2007 - MARHM not available, will extrapolate from 2008
    if (yr == 2007) {
      cli::cli_alert_info("2007 will be extrapolated from 2008 (MARHM not available)")
      next
    }

    cache_file <- file.path(cache_dir, sprintf("new_marriages_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached new marriages for {yr}")
      cached <- readRDS(cache_file)
      grids[[as.character(yr)]] <- cached$grid
      summaries[[as.character(yr)]] <- cached$summary
      next
    }

    cli::cli_alert("Fetching ACS new marriages for {yr}...")

    tryCatch({
      result <- fetch_acs_new_marriages_year(yr, min_age, max_age, api_key)

      if (!is.null(result$grid)) {
        # Cache the result
        saveRDS(result, cache_file)
        cli::cli_alert_success("Cached new marriages for {yr} (total: {format(sum(result$grid), big.mark=',')})")
        grids[[as.character(yr)]] <- result$grid
        summaries[[as.character(yr)]] <- result$summary
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  # Extrapolate 2007 from 2008 if requested
  if (needs_2007 && "2008" %in% names(grids)) {
    cli::cli_alert("Extrapolating 2007 from 2008 distribution...")
    grid_2007 <- extrapolate_2007_marriage_grid(grids[["2008"]], cache_dir)
    if (!is.null(grid_2007)) {
      grids[["2007"]] <- grid_2007
      summaries[["2007"]] <- calculate_marriage_summary(grid_2007, 2007)
      cli::cli_alert_success("Created 2007 marriage grid (extrapolated)")
    }
  } else if (needs_2007) {
    cli::cli_alert_warning("Cannot extrapolate 2007 - 2008 data not available")
  }

  if (length(grids) == 0) {
    cli::cli_abort("No ACS new marriage data retrieved")
  }

  # Combine summaries
  summary_dt <- data.table::rbindlist(summaries, use.names = TRUE)
  data.table::setorder(summary_dt, year)

  cli::cli_alert_success(
    "Retrieved new marriage grids for {length(grids)} years"
  )

  list(
    grids = grids,
    summary = summary_dt,
    by_age_group = calculate_marriages_by_age_group(grids)
  )
}

#' Fetch ACS new marriages for a single year
#'
#' @param year Integer: year to fetch
#' @param min_age Integer: minimum age
#' @param max_age Integer: maximum age
#' @param api_key Character: Census API key
#'
#' @return list with grid matrix and summary statistics
#'
#' @keywords internal
fetch_acs_new_marriages_year <- function(year, min_age, max_age, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  # Variables needed:
  # - SERIALNO: Household ID (to link spouses)
  # - AGEP: Age
  # - SEX: Sex (1=Male, 2=Female)
  # - MARHM: Married in past 12 months (1=Yes, 2=No)
  # - PWGTP: Person weight
  # Note: We don't need REL/RELP/RELSHIPP - we link spouses via SERIALNO

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = "SERIALNO,AGEP,SEX,MARHM,PWGTP",
      # Filter to those married in past 12 months
      MARHM = "1",
      key = api_key
    ) |>
    httr2::req_timeout(600) |>  # Large query
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    cli::cli_alert_warning("No data returned for {year}")
    return(list(grid = NULL, summary = NULL))
  }

  # Convert to data.table
  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  # Parse variables
  dt[, serialno := as.character(SERIALNO)]
  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, weight := as.numeric(PWGTP)]

  # Map sex codes
  dt[sex_code == 1, sex := "male"]
  dt[sex_code == 2, sex := "female"]

  # Filter to valid ages
  dt <- dt[age >= min_age & age <= max_age & !is.na(sex)]

  if (nrow(dt) == 0) {
    cli::cli_alert_warning("No valid records for {year}")
    return(list(grid = NULL, summary = NULL))
  }

  # Build marriage grid by linking spouses
  grid <- build_new_marriage_grid(dt, min_age, max_age, year)

  # Calculate summary statistics
  summary_stats <- calculate_marriage_summary(grid, year)

  list(grid = grid, summary = summary_stats)
}

#' Build marriage grid from newly married person records
#'
#' @description
#' Links newly married persons within households to create
#' husband age × wife age marriage grid.
#'
#' @param dt data.table with newly married person records
#' @param min_age Integer: minimum age for grid
#' @param max_age Integer: maximum age for grid
#' @param year Integer: survey year
#'
#' @return matrix (rows = husband age, cols = wife age)
#'
#' @keywords internal
build_new_marriage_grid <- function(dt, min_age, max_age, year) {
  # Split by sex
  males <- dt[sex == "male", .(serialno, husband_age = age, husband_weight = weight)]
  females <- dt[sex == "female", .(serialno, wife_age = age, wife_weight = weight)]

  # Link spouses by household ID
  # This creates all possible pairings within households with newly married persons
  couples <- merge(males, females, by = "serialno", allow.cartesian = TRUE)

  if (nrow(couples) == 0) {
    cli::cli_alert_warning("No spouse pairs found for {year}")
    # Return empty grid
    ages <- min_age:max_age
    return(matrix(0, nrow = length(ages), ncol = length(ages),
                  dimnames = list(husband = ages, wife = ages)))
  }

  # Use geometric mean of weights for the couple (per TR2025 methodology)
  couples[, couple_weight := sqrt(husband_weight * wife_weight)]

  # Aggregate by husband age × wife age
  grid_data <- couples[, .(marriages = sum(couple_weight)),
                       by = .(husband_age, wife_age)]

  # Convert to matrix form
  ages <- min_age:max_age
  n_ages <- length(ages)
  grid_matrix <- matrix(0, nrow = n_ages, ncol = n_ages,
                        dimnames = list(husband = ages, wife = ages))

  for (i in seq_len(nrow(grid_data))) {
    h_age <- grid_data$husband_age[i]
    w_age <- grid_data$wife_age[i]

    # Cap ages at max_age (100+ grouped)
    h_age <- min(h_age, max_age)
    w_age <- min(w_age, max_age)

    h_idx <- h_age - min_age + 1
    w_idx <- w_age - min_age + 1

    if (h_idx >= 1 && h_idx <= n_ages && w_idx >= 1 && w_idx <= n_ages) {
      grid_matrix[h_idx, w_idx] <- grid_matrix[h_idx, w_idx] + grid_data$marriages[i]
    }
  }

  # Add metadata
  attr(grid_matrix, "year") <- year
  attr(grid_matrix, "total_marriages") <- sum(grid_matrix)
  attr(grid_matrix, "min_age") <- min_age
  attr(grid_matrix, "max_age") <- max_age
  attr(grid_matrix, "data_source") <- "ACS PUMS MARHM"

  grid_matrix
}

#' Extrapolate 2007 marriage grid from 2008
#'
#' @description
#' Creates a 2007 marriage grid by scaling the 2008 distribution.
#' MARHM was not available in 2007, so we extrapolate backward using
#' NCHS total marriage counts.
#'
#' @param grid_2008 Matrix: 2008 marriage grid
#' @param cache_dir Character: cache directory
#'
#' @return Matrix: extrapolated 2007 marriage grid
#'
#' @keywords internal
extrapolate_2007_marriage_grid <- function(grid_2008, cache_dir) {
  # NCHS marriage totals for scaling
  # 2007: 2,197,000
  # 2008: 2,157,000
  # Scale factor = 2007 NCHS / 2008 NCHS * (our 2008 coverage ratio)
  nchs_2007 <- 2197000
  nchs_2008 <- 2157000
  scale_factor <- nchs_2007 / nchs_2008

  # Scale the 2008 grid
  grid_2007 <- grid_2008 * scale_factor

  # Update metadata
  attr(grid_2007, "year") <- 2007
  attr(grid_2007, "total_marriages") <- sum(grid_2007)
  attr(grid_2007, "data_source") <- "Extrapolated from 2008"

  # Cache the extrapolated grid
  cache_file <- file.path(cache_dir, "new_marriages_2007.rds")
  result <- list(grid = grid_2007, summary = calculate_marriage_summary(grid_2007, 2007))
  saveRDS(result, cache_file)

  grid_2007
}

#' Calculate summary statistics for marriage grid
#'
#' @param grid Matrix: marriage grid
#' @param year Integer: survey year
#'
#' @return data.table with summary statistics
#'
#' @keywords internal
calculate_marriage_summary <- function(grid, year) {
  total <- sum(grid)

  if (total == 0) {
    return(data.table::data.table(
      year = year,
      total_marriages = 0,
      avg_husband_age = NA_real_,
      avg_wife_age = NA_real_,
      avg_age_difference = NA_real_,
      same_sex_pct = NA_real_
    ))
  }

  ages <- as.integer(rownames(grid))
  husband_marginal <- rowSums(grid)
  wife_marginal <- colSums(grid)

  # Average ages (weighted)
  avg_husband_age <- sum(ages * husband_marginal) / total
  avg_wife_age <- sum(ages * wife_marginal) / total

  # Average age difference (husband - wife)
  age_diff_sum <- 0
  for (h in seq_along(ages)) {
    for (w in seq_along(ages)) {
      if (grid[h, w] > 0) {
        age_diff_sum <- age_diff_sum + (ages[h] - ages[w]) * grid[h, w]
      }
    }
  }
  avg_age_diff <- age_diff_sum / total

  data.table::data.table(
    year = year,
    total_marriages = round(total),
    avg_husband_age = round(avg_husband_age, 1),
    avg_wife_age = round(avg_wife_age, 1),
    avg_age_difference = round(avg_age_diff, 1)
  )
}

#' Calculate marriages by age group
#'
#' @description
#' Aggregates single-year marriage grids into age group totals
#' for comparison with NCHS data.
#'
#' @param grids List of marriage grid matrices by year
#'
#' @return data.table with marriages by age group of husband × wife
#'
#' @export
calculate_marriages_by_age_group <- function(grids) {
  results <- list()

  for (yr in names(grids)) {
    grid <- grids[[yr]]
    year <- as.integer(yr)

    # Get age ranges from grid
    ages <- as.integer(rownames(grid))

    for (h_group_name in names(ACS_MARRIAGE_AGE_GROUPS)) {
      h_ages <- ACS_MARRIAGE_AGE_GROUPS[[h_group_name]]
      h_ages <- h_ages[h_ages %in% ages]

      for (w_group_name in names(ACS_MARRIAGE_AGE_GROUPS)) {
        w_ages <- ACS_MARRIAGE_AGE_GROUPS[[w_group_name]]
        w_ages <- w_ages[w_ages %in% ages]

        if (length(h_ages) > 0 && length(w_ages) > 0) {
          h_idx <- which(ages %in% h_ages)
          w_idx <- which(ages %in% w_ages)
          marriages <- sum(grid[h_idx, w_idx])

          results[[length(results) + 1]] <- data.table::data.table(
            year = year,
            husband_age_group = gsub("group_", "", h_group_name),
            wife_age_group = gsub("group_", "", w_group_name),
            marriages = marriages
          )
        }
      }
    }
  }

  if (length(results) == 0) {
    return(data.table::data.table())
  }

  data.table::rbindlist(results)
}

# =============================================================================
# UNMARRIED POPULATION FUNCTIONS
# =============================================================================

#' Get unmarried population from Phase 4 marital status cache
#'
#' @description
#' Uses existing Phase 4 marital status cache to derive unmarried population.
#' Unmarried = divorced + never_married + widowed (everything except married).
#' This is much faster than re-fetching from the API.
#'
#' @param years Integer vector of years (2006-2023 available)
#' @param min_age Integer: minimum age (default: 15)
#' @param max_age Integer: maximum age (default: 99)
#' @param cache_file Character: path to Phase 4 marital status cache
#'
#' @return data.table with columns: year, age, sex, unmarried_population
#'
#' @export
get_unmarried_population_from_cache <- function(
    years = 2007:2022,
    min_age = 15,
    max_age = 99,
    cache_file = here::here("data/cache/acs_pums/marital_all_years.rds")
) {
  if (!file.exists(cache_file)) {
    cli::cli_abort("Phase 4 marital status cache not found: {cache_file}")
  }

  cli::cli_alert("Loading unmarried population from Phase 4 cache...")
  marital_data <- readRDS(cache_file)

  # Filter to requested years and ages
  dt <- marital_data[year %in% years & age >= min_age & age <= max_age]

  # Skip 2020 if present (ACS 1-year not released)
  dt <- dt[year != 2020]

  # Sum all unmarried statuses (divorced, never_married, widowed)
  unmarried <- dt[marital_status != "married",
                  .(unmarried_population = sum(population)),
                  by = .(year, age, sex)]

  data.table::setorder(unmarried, year, sex, age)

  years_available <- unique(unmarried$year)
  cli::cli_alert_success(
    "Loaded unmarried population for {length(years_available)} years from cache"
  )

  unmarried
}

#' Fetch ACS unmarried population by age and sex
#'
#' @description
#' Downloads ACS PUMS unmarried population (single + widowed + divorced).
#' Used as denominators for marriage rate calculations.
#'
#' NOTE: Consider using get_unmarried_population_from_cache() instead,
#' which uses existing Phase 4 data and is much faster.
#'
#' @param years Integer vector of ACS years
#' @param min_age Integer: minimum age (default: 15)
#' @param max_age Integer: maximum age (default: 99)
#' @param cache_dir Character: directory for caching
#'
#' @return data.table with columns: year, age, sex, unmarried_population
#'
#' @export
fetch_acs_unmarried_population <- function(years,
                                            min_age = 15,
                                            max_age = 99,
                                            cache_dir = here::here("data/cache/acs_marriage")) {

  checkmate::assert_integerish(years, lower = 2005, upper = 2024, min.len = 1)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  api_key <- get_api_key("CENSUS_KEY")

  results <- list()

  for (yr in years) {
    # Skip 2020
    if (yr == 2020) {
      cli::cli_alert_warning("Skipping 2020 - ACS 1-year not released")
      next
    }

    cache_file <- file.path(cache_dir, sprintf("unmarried_pop_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached unmarried population for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS unmarried population for {yr}...")

    tryCatch({
      dt <- fetch_acs_unmarried_year(yr, min_age, max_age, api_key)

      if (!is.null(dt) && nrow(dt) > 0) {
        saveRDS(dt, cache_file)
        cli::cli_alert_success("Cached unmarried population for {yr}")
        results[[as.character(yr)]] <- dt
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    cli::cli_abort("No ACS unmarried population data retrieved")
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year, sex, age)

  cli::cli_alert_success(
    "Retrieved unmarried population for {length(unique(combined$year))} years"
  )

  combined
}

#' Fetch ACS unmarried population for a single year
#'
#' @keywords internal
fetch_acs_unmarried_year <- function(year, min_age, max_age, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  # MAR: 1=Married, 2=Widowed, 3=Divorced, 4=Separated, 5=Never married
 # Fetch each unmarried status separately with server-side filtering
  # This is MUCH faster than fetching all and filtering locally

  results <- list()

  for (mar_status in c(2, 3, 4, 5)) {  # Widowed, Divorced, Separated, Never married
    req <- httr2::request(base_url) |>
      httr2::req_url_query(
        get = "AGEP,SEX,PWGTP",
        MAR = as.character(mar_status),
        key = api_key
      ) |>
      httr2::req_timeout(120) |>
      httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

    resp <- tryCatch({
      api_request_with_retry(req, max_retries = 3)
    }, error = function(e) NULL)

    if (is.null(resp)) next

    tryCatch({
      check_api_response(resp, sprintf("ACS PUMS API (%d, MAR=%d)", year, mar_status))
      json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

      if (length(json_data) >= 2) {
        headers <- json_data[1, ]
        data_rows <- json_data[-1, , drop = FALSE]
        dt <- data.table::as.data.table(data_rows)
        data.table::setnames(dt, headers)

        dt[, age := as.integer(AGEP)]
        dt[, sex_code := as.integer(SEX)]
        dt[, weight := as.numeric(PWGTP)]

        # Filter to valid ages
        dt <- dt[age >= min_age & age <= max_age]

        # Map sex
        dt[sex_code == 1, sex := "male"]
        dt[sex_code == 2, sex := "female"]

        results[[as.character(mar_status)]] <- dt[!is.na(sex), .(age, sex, weight)]
      }
    }, error = function(e) NULL)
  }

  if (length(results) == 0) {
    return(NULL)
  }

  # Combine all unmarried statuses
  combined <- data.table::rbindlist(results, use.names = TRUE)

  # Aggregate by age and sex
  result <- combined[, .(unmarried_population = sum(weight)), by = .(age, sex)]
  result[, year := year]

  data.table::setcolorder(result, c("year", "age", "sex", "unmarried_population"))
  data.table::setorder(result, age, sex)

  result
}

# =============================================================================
# GEOMETRIC MEAN POPULATION (for rate denominators)
# =============================================================================

#' Calculate geometric mean unmarried population for marriage rates
#'
#' @description
#' Calculates the geometric mean of unmarried male and female populations
#' for each age combination, as used in marriage rate denominators.
#'
#' P_{x,y} = sqrt(unmarried_male_x * unmarried_female_y)
#'
#' @param unmarried_pop data.table from fetch_acs_unmarried_population
#' @param min_age Integer: minimum age
#' @param max_age Integer: maximum age
#'
#' @return data.table with columns: year, husband_age, wife_age, geometric_mean
#'
#' @export
calculate_geometric_mean_population <- function(unmarried_pop,
                                                  min_age = 15,
                                                  max_age = 99) {
  results <- list()

  for (yr in unique(unmarried_pop$year)) {
    yr_data <- unmarried_pop[year == yr]

    males <- yr_data[sex == "male", .(age, male_pop = unmarried_population)]
    females <- yr_data[sex == "female", .(age, female_pop = unmarried_population)]

    # Create grid of all age combinations
    ages <- min_age:max_age
    grid_dt <- data.table::CJ(husband_age = ages, wife_age = ages)

    # Merge male and female populations
    grid_dt <- merge(grid_dt, males, by.x = "husband_age", by.y = "age", all.x = TRUE)
    grid_dt <- merge(grid_dt, females, by.x = "wife_age", by.y = "age", all.x = TRUE)

    # Replace NAs with 0
    grid_dt[is.na(male_pop), male_pop := 0]
    grid_dt[is.na(female_pop), female_pop := 0]

    # Calculate geometric mean
    grid_dt[, geometric_mean := sqrt(male_pop * female_pop)]
    grid_dt[, year := yr]

    results[[as.character(yr)]] <- grid_dt[, .(year, husband_age, wife_age, geometric_mean)]
  }

  data.table::rbindlist(results)
}

# =============================================================================
# MARRIAGE RATE CALCULATION
# =============================================================================

#' Calculate marriage rates from marriages and population
#'
#' @description
#' Calculates age-specific marriage rates using the formula:
#' m_{x,y} = M_{x,y} / P_{x,y}
#'
#' where P_{x,y} is the geometric mean of unmarried male (age x) and
#' unmarried female (age y) populations.
#'
#' @param marriage_grids List of marriage grid matrices by year
#' @param unmarried_pop data.table from fetch_acs_unmarried_population
#' @param rate_scale Numeric: rate multiplier (default: 100000 for per 100,000)
#'
#' @return list with:
#'   - rates: List of rate matrices by year
#'   - summary: data.table with rate summary statistics
#'
#' @export
calculate_acs_marriage_rates <- function(marriage_grids,
                                          unmarried_pop,
                                          rate_scale = 100000) {

  # Get geometric mean populations
  min_age <- attr(marriage_grids[[1]], "min_age")
  max_age <- attr(marriage_grids[[1]], "max_age")
  geo_mean_pop <- calculate_geometric_mean_population(unmarried_pop, min_age, max_age)

  rate_grids <- list()
  summaries <- list()

  for (yr in names(marriage_grids)) {
    year <- as.integer(yr)
    marriages <- marriage_grids[[yr]]
    ages <- as.integer(rownames(marriages))

    # Get population for this year
    pop_yr <- geo_mean_pop[year == year]

    if (nrow(pop_yr) == 0) {
      cli::cli_alert_warning("No population data for year {year}, skipping")
      next
    }

    # Convert population to matrix form
    pop_matrix <- matrix(0, nrow = length(ages), ncol = length(ages),
                         dimnames = list(husband = ages, wife = ages))

    for (i in seq_len(nrow(pop_yr))) {
      h_age <- pop_yr$husband_age[i]
      w_age <- pop_yr$wife_age[i]
      h_idx <- which(ages == h_age)
      w_idx <- which(ages == w_age)

      if (length(h_idx) == 1 && length(w_idx) == 1) {
        pop_matrix[h_idx, w_idx] <- pop_yr$geometric_mean[i]
      }
    }

    # Calculate rates (marriages per 100,000)
    # Avoid division by zero
    rate_matrix <- marriages / pmax(pop_matrix, 1) * rate_scale

    # Set rates to 0 where population is 0
    rate_matrix[pop_matrix == 0] <- 0

    # Add metadata
    attr(rate_matrix, "year") <- year
    attr(rate_matrix, "rate_scale") <- rate_scale

    rate_grids[[yr]] <- rate_matrix

    # Calculate summary
    summaries[[yr]] <- data.table::data.table(
      year = year,
      total_marriages = sum(marriages),
      total_population = sum(pop_matrix),
      crude_rate = sum(marriages) / sum(pop_matrix) * rate_scale,
      mean_rate = mean(rate_matrix[rate_matrix > 0]),
      max_rate = max(rate_matrix)
    )
  }

  list(
    rates = rate_grids,
    summary = data.table::rbindlist(summaries)
  )
}

# =============================================================================
# SAME-SEX MARRIAGE HANDLING
# =============================================================================

#' Fetch ACS same-sex marriages
#'
#' @description
#' Downloads ACS data on same-sex marriages for years 2013+.
#' Prior to 2013, same-sex marriages were not consistently recorded.
#'
#' @param years Integer vector of ACS years (2013+ recommended)
#' @param cache_dir Character: directory for caching
#'
#' @return data.table with same-sex marriage counts by age and sex combination
#'
#' @export
fetch_acs_same_sex_marriages <- function(years = 2013:2022,
                                          cache_dir = here::here("data/cache/acs_marriage")) {

  checkmate::assert_integerish(years, lower = 2013, upper = 2024, min.len = 1)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  api_key <- get_api_key("CENSUS_KEY")

  results <- list()

  for (yr in years) {
    if (yr == 2020) {
      next
    }

    cache_file <- file.path(cache_dir, sprintf("same_sex_marriages_%d.rds", yr))

    if (file.exists(cache_file)) {
      cli::cli_alert_success("Loading cached same-sex marriages for {yr}")
      results[[as.character(yr)]] <- readRDS(cache_file)
      next
    }

    cli::cli_alert("Fetching ACS same-sex marriages for {yr}...")

    tryCatch({
      dt <- fetch_acs_same_sex_year(yr, api_key)

      if (!is.null(dt) && nrow(dt) > 0) {
        saveRDS(dt, cache_file)
        cli::cli_alert_success("Cached same-sex marriages for {yr}")
        results[[as.character(yr)]] <- dt
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
    })
  }

  if (length(results) == 0) {
    return(data.table::data.table())
  }

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setorder(combined, year)

  combined
}

#' Fetch ACS same-sex marriages for a single year (totals only)
#'
#' @keywords internal
fetch_acs_same_sex_year <- function(year, api_key) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  # SSMC (Same-sex married couple) is available in newer years
  # Alternatively, identify same-sex couples by linking household members
  rel_var <- if (year >= 2019) "RELSHIPP" else "RELP"

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = paste0("SERIALNO,AGEP,SEX,MARHM,", rel_var, ",PWGTP"),
      MARHM = "1",
      key = api_key
    ) |>
    httr2::req_timeout(600) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    return(NULL)
  }

  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  dt[, serialno := as.character(SERIALNO)]
  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, weight := as.numeric(PWGTP)]

  # Identify same-sex couples by finding households where
  # both newly married persons are same sex
  hh_sex <- dt[, .(
    n_males = sum(sex_code == 1),
    n_females = sum(sex_code == 2),
    total_weight = sum(weight)
  ), by = serialno]

  # Same-sex households have only males or only females
  same_sex_hh <- hh_sex[(n_males >= 2 & n_females == 0) |
                          (n_females >= 2 & n_males == 0)]

  if (nrow(same_sex_hh) == 0) {
    return(data.table::data.table(
      year = year,
      sex_combo = character(),
      marriages = numeric()
    ))
  }

  # Classify and count
  same_sex_hh[n_males >= 2, sex_combo := "male_male"]
  same_sex_hh[n_females >= 2, sex_combo := "female_female"]

  result <- same_sex_hh[, .(marriages = sum(total_weight) / 2), by = sex_combo]
  result[, year := year]

  result
}

#' Fetch ACS same-sex marriage grids by age for a single year
#'
#' @description
#' Downloads ACS PUMS data and builds age grids for same-sex marriages.
#' Returns separate grids for male-male and female-female couples.
#'
#' @param year Integer: ACS year
#' @param api_key Character: Census API key
#' @param min_age Integer: minimum age for grid (default: 15)
#' @param max_age Integer: maximum age for grid (default: 99)
#'
#' @return list with:
#'   - male_male_grid: Matrix of male-male marriages by age
#'   - female_female_grid: Matrix of female-female marriages by age
#'   - totals: data.table with total counts by sex_combo
#'   - year: The year
#'
#' @keywords internal
fetch_acs_same_sex_grids_year <- function(year, api_key, min_age = 15, max_age = 99) {
  base_url <- sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)

  rel_var <- if (year >= 2019) "RELSHIPP" else "RELP"

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      get = paste0("SERIALNO,AGEP,SEX,MARHM,", rel_var, ",PWGTP"),
      MARHM = "1",
      key = api_key
    ) |>
    httr2::req_timeout(600) |>
    httr2::req_user_agent("ARTEMIS OASDI Projection Model (R)")

  resp <- api_request_with_retry(req, max_retries = 3)
  check_api_response(resp, sprintf("ACS PUMS API (%d)", year))

  json_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(json_data) < 2) {
    return(NULL)
  }

  headers <- json_data[1, ]
  data_rows <- json_data[-1, , drop = FALSE]
  dt <- data.table::as.data.table(data_rows)
  data.table::setnames(dt, headers)

  dt[, serialno := as.character(SERIALNO)]
  dt[, age := as.integer(AGEP)]
  dt[, sex_code := as.integer(SEX)]
  dt[, weight := as.numeric(PWGTP)]

  # Identify same-sex households
  hh_info <- dt[, .(
    n_males = sum(sex_code == 1),
    n_females = sum(sex_code == 2)
  ), by = serialno]

  # Male-male households: 2+ males, 0 females
  mm_hh <- hh_info[n_males >= 2 & n_females == 0, serialno]
  # Female-female households: 2+ females, 0 males
  ff_hh <- hh_info[n_females >= 2 & n_males == 0, serialno]

  # Initialize grids
  ages <- min_age:max_age
  n_ages <- length(ages)

  mm_grid <- matrix(0, nrow = n_ages, ncol = n_ages,
                    dimnames = list(ages, ages))
  ff_grid <- matrix(0, nrow = n_ages, ncol = n_ages,
                    dimnames = list(ages, ages))

  # Process male-male couples
  if (length(mm_hh) > 0) {
    mm_data <- dt[serialno %in% mm_hh]

    # For each household, get the two ages and weight
    mm_pairs <- mm_data[, {
      if (.N >= 2) {
        # Sort ages to ensure consistent ordering (younger first)
        sorted_ages <- sort(age)
        avg_weight <- mean(weight)
        list(age1 = sorted_ages[1], age2 = sorted_ages[2], weight = avg_weight)
      } else {
        list(age1 = NA_integer_, age2 = NA_integer_, weight = NA_real_)
      }
    }, by = serialno]

    mm_pairs <- mm_pairs[!is.na(age1) & !is.na(age2)]

    # Clip ages to grid range
    mm_pairs[age1 < min_age, age1 := min_age]
    mm_pairs[age1 > max_age, age1 := max_age]
    mm_pairs[age2 < min_age, age2 := min_age]
    mm_pairs[age2 > max_age, age2 := max_age]

    # Aggregate by age pair
    mm_agg <- mm_pairs[, .(marriages = sum(weight)), by = .(age1, age2)]

    # Fill grid
    for (i in seq_len(nrow(mm_agg))) {
      a1 <- mm_agg$age1[i]
      a2 <- mm_agg$age2[i]
      m <- mm_agg$marriages[i]
      row_idx <- which(ages == a1)
      col_idx <- which(ages == a2)
      if (length(row_idx) > 0 && length(col_idx) > 0) {
        mm_grid[row_idx, col_idx] <- mm_grid[row_idx, col_idx] + m
      }
    }
  }

  # Process female-female couples
  if (length(ff_hh) > 0) {
    ff_data <- dt[serialno %in% ff_hh]

    ff_pairs <- ff_data[, {
      if (.N >= 2) {
        sorted_ages <- sort(age)
        avg_weight <- mean(weight)
        list(age1 = sorted_ages[1], age2 = sorted_ages[2], weight = avg_weight)
      } else {
        list(age1 = NA_integer_, age2 = NA_integer_, weight = NA_real_)
      }
    }, by = serialno]

    ff_pairs <- ff_pairs[!is.na(age1) & !is.na(age2)]

    ff_pairs[age1 < min_age, age1 := min_age]
    ff_pairs[age1 > max_age, age1 := max_age]
    ff_pairs[age2 < min_age, age2 := min_age]
    ff_pairs[age2 > max_age, age2 := max_age]

    ff_agg <- ff_pairs[, .(marriages = sum(weight)), by = .(age1, age2)]

    for (i in seq_len(nrow(ff_agg))) {
      a1 <- ff_agg$age1[i]
      a2 <- ff_agg$age2[i]
      m <- ff_agg$marriages[i]
      row_idx <- which(ages == a1)
      col_idx <- which(ages == a2)
      if (length(row_idx) > 0 && length(col_idx) > 0) {
        ff_grid[row_idx, col_idx] <- ff_grid[row_idx, col_idx] + m
      }
    }
  }

  # Calculate totals
  totals <- data.table::data.table(
    sex_combo = c("male_male", "female_female"),
    marriages = c(sum(mm_grid), sum(ff_grid)),
    year = year
  )

  list(
    male_male_grid = mm_grid,
    female_female_grid = ff_grid,
    totals = totals,
    year = year
  )
}

#' Fetch ACS same-sex marriage grids for multiple years
#'
#' @description
#' Downloads ACS PUMS data and builds age grids for same-sex marriages
#' for multiple years. These grids can be used to develop age-specific
#' same-sex marriage rate patterns per TR2025 methodology.
#'
#' @param years Integer vector of ACS years (2015+ recommended, post-Obergefell)
#' @param cache_dir Character: directory for caching
#' @param min_age Integer: minimum age for grid (default: 15)
#' @param max_age Integer: maximum age for grid (default: 99)
#'
#' @return list with:
#'   - grids: List of grids by year, each containing male_male_grid and female_female_grid
#'   - totals: data.table with totals by year and sex_combo
#'   - average_male_male: Average male-male grid across years (normalized)
#'   - average_female_female: Average female-female grid across years (normalized)
#'
#' @export
fetch_acs_same_sex_grids <- function(years = 2015:2022,
                                      cache_dir = here::here("data/cache/acs_marriage"),
                                      min_age = 15,
                                      max_age = 99) {

  checkmate::assert_integerish(years, lower = 2013, upper = 2024, min.len = 1)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  api_key <- get_api_key("CENSUS_KEY")

  # Check for cached combined result
  cache_file <- file.path(cache_dir, "same_sex_grids_combined.rds")
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached same-sex marriage grids")
    cached <- readRDS(cache_file)
    # Filter to requested years
    if (all(years %in% names(cached$grids))) {
      return(cached)
    }
  }

  cli::cli_h2("Fetching ACS Same-Sex Marriage Grids")

  grids <- list()
  all_totals <- list()

  for (yr in years) {
    if (yr == 2020) {
      cli::cli_alert_info("Skipping 2020 (no ACS 1-year data)")
      next
    }

    yr_cache <- file.path(cache_dir, sprintf("same_sex_grid_%d.rds", yr))

    if (file.exists(yr_cache)) {
      cli::cli_alert_success("Loading cached same-sex grid for {yr}")
      yr_data <- readRDS(yr_cache)
    } else {
      cli::cli_alert("Fetching same-sex marriage grid for {yr}...")

      tryCatch({
        yr_data <- fetch_acs_same_sex_grids_year(yr, api_key, min_age, max_age)

        if (!is.null(yr_data)) {
          saveRDS(yr_data, yr_cache)
          cli::cli_alert_success("Cached same-sex grid for {yr}")
        }
      }, error = function(e) {
        cli::cli_alert_warning("Failed for {yr}: {conditionMessage(e)}")
        yr_data <- NULL
      })
    }

    if (!is.null(yr_data)) {
      grids[[as.character(yr)]] <- yr_data
      all_totals[[as.character(yr)]] <- yr_data$totals
    }
  }

  if (length(grids) == 0) {
    cli::cli_abort("No same-sex marriage data could be fetched")
  }

  totals <- data.table::rbindlist(all_totals)

  # Calculate average grids (normalized to sum to 1)
  ages <- min_age:max_age
  n_ages <- length(ages)

  avg_mm <- matrix(0, nrow = n_ages, ncol = n_ages, dimnames = list(ages, ages))
  avg_ff <- matrix(0, nrow = n_ages, ncol = n_ages, dimnames = list(ages, ages))

  n_years <- length(grids)
  for (yr_name in names(grids)) {
    yr_data <- grids[[yr_name]]

    # Normalize each year's grid to sum to 1, then average
    mm_total <- sum(yr_data$male_male_grid)
    ff_total <- sum(yr_data$female_female_grid)

    if (mm_total > 0) {
      avg_mm <- avg_mm + yr_data$male_male_grid / mm_total / n_years
    }
    if (ff_total > 0) {
      avg_ff <- avg_ff + yr_data$female_female_grid / ff_total / n_years
    }
  }

  # Summary statistics
  mm_total <- sum(totals[sex_combo == "male_male", marriages])
  ff_total <- sum(totals[sex_combo == "female_female", marriages])
  total_ss <- mm_total + ff_total

  cli::cli_alert_success("Processed {length(grids)} years of same-sex marriage data")
  cli::cli_alert_info("Total male-male: {format(round(mm_total), big.mark=',')} ({round(100*mm_total/total_ss, 1)}%)")
  cli::cli_alert_info("Total female-female: {format(round(ff_total), big.mark=',')} ({round(100*ff_total/total_ss, 1)}%)")

  result <- list(
    grids = grids,
    totals = totals,
    average_male_male = avg_mm,
    average_female_female = avg_ff,
    years = as.integer(names(grids)),
    min_age = min_age,
    max_age = max_age
  )

  # Cache combined result

  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached combined same-sex grids")

  result
}

#' Calculate same-sex marriage fraction by age
#'
#' @description
#' Calculates the fraction of marriages that are same-sex by age,
#' using ACS data. This can be used to separate total marriage rates
#' into opposite-sex and same-sex components.
#'
#' @param same_sex_grids Result from fetch_acs_same_sex_grids()
#' @param opposite_sex_grids List of opposite-sex marriage grids by year
#'   From fetch_acs_new_marriages()
#' @param years Years to use for calculation (default: all available)
#'
#' @return list with:
#'   - fraction_by_age: Matrix of same-sex fraction by age pair
#'   - overall_fraction: Overall same-sex fraction
#'   - male_male_fraction: Fraction that is male-male (of same-sex)
#'   - female_female_fraction: Fraction that is female-female (of same-sex)
#'
#' @export
calculate_same_sex_fraction <- function(same_sex_grids,
                                         opposite_sex_grids,
                                         years = NULL) {

  if (is.null(years)) {
    years <- same_sex_grids$years
  }

  # Sum same-sex grids across years
  ages <- same_sex_grids$min_age:same_sex_grids$max_age
  n_ages <- length(ages)

  total_ss <- matrix(0, nrow = n_ages, ncol = n_ages, dimnames = list(ages, ages))
  total_os <- matrix(0, nrow = n_ages, ncol = n_ages, dimnames = list(ages, ages))

  for (yr in years) {
    yr_char <- as.character(yr)

    # Same-sex
    if (yr_char %in% names(same_sex_grids$grids)) {
      ss_data <- same_sex_grids$grids[[yr_char]]
      total_ss <- total_ss + ss_data$male_male_grid + ss_data$female_female_grid
    }

    # Opposite-sex
    if (yr_char %in% names(opposite_sex_grids)) {
      os_grid <- opposite_sex_grids[[yr_char]]
      # Align dimensions if needed
      os_ages <- as.integer(rownames(os_grid))
      for (i in seq_along(os_ages)) {
        for (j in seq_along(os_ages)) {
          a1 <- os_ages[i]
          a2 <- os_ages[j]
          if (a1 >= same_sex_grids$min_age && a1 <= same_sex_grids$max_age &&
              a2 >= same_sex_grids$min_age && a2 <= same_sex_grids$max_age) {
            r <- which(ages == a1)
            c <- which(ages == a2)
            if (length(r) > 0 && length(c) > 0) {
              total_os[r, c] <- total_os[r, c] + os_grid[i, j]
            }
          }
        }
      }
    }
  }

  # Calculate fraction
  total_all <- total_ss + total_os
  fraction_by_age <- total_ss / total_all
  fraction_by_age[is.na(fraction_by_age) | is.infinite(fraction_by_age)] <- 0

  # Overall fractions
  sum_ss <- sum(total_ss)
  sum_os <- sum(total_os)
  sum_all <- sum_ss + sum_os

  overall_fraction <- sum_ss / sum_all

  # Male-male vs female-female split
  sum_mm <- sum(sapply(same_sex_grids$grids[as.character(years)], function(x) sum(x$male_male_grid)))
  sum_ff <- sum(sapply(same_sex_grids$grids[as.character(years)], function(x) sum(x$female_female_grid)))

  list(
    fraction_by_age = fraction_by_age,
    overall_fraction = overall_fraction,
    male_male_fraction = sum_mm / sum_ss,
    female_female_fraction = sum_ff / sum_ss,
    total_same_sex = sum_ss,
    total_opposite_sex = sum_os
  )
}

#' Calculate same-sex prevalence grids for marriage type separation
#'
#' @description
#' Calculates prevalence grids (probability at each age cell) for same-sex,
#' male-male, and female-female marriages. This is the preferred approach
#' for separating marriage types because it guarantees that separated rates
#' never exceed total rates at any cell.
#'
#' The prevalence at each cell represents: "What fraction of marriages at
#' this age combination are same-sex (or male-male, or female-female)?"
#'
#' @param same_sex_grids Result from fetch_acs_same_sex_grids()
#' @param opposite_sex_grids List of opposite-sex marriage grids by year
#'   (aligned to MarGrid dimensions)
#' @param years Years to use for calculation (default: all available)
#' @param smooth Logical: apply smoothing to handle sparse cells (default: TRUE)
#' @param min_count Minimum count threshold for reliable prevalence (default: 10)
#'
#' @return list with:
#'   - ss_prevalence: Matrix of same-sex prevalence by age pair (0 to 1)
#'   - mm_prevalence: Matrix of male-male prevalence by age pair (0 to 1)
#'   - ff_prevalence: Matrix of female-female prevalence by age pair (0 to 1)
#'   - overall_ss_fraction: Overall same-sex fraction for fallback
#'   - overall_mm_fraction: Overall male-male fraction (of total)
#'   - overall_ff_fraction: Overall female-female fraction (of total)
#'   - ages: Age range covered
#'
#' @export
calculate_same_sex_prevalence_grids <- function(same_sex_grids,
                                                  opposite_sex_grids,
                                                  years = NULL,
                                                  smooth = TRUE,
                                                  min_count = 10) {

  if (is.null(years)) {
    years <- same_sex_grids$years
  }

  cli::cli_h2("Calculating Same-Sex Prevalence Grids")

  # Get age range from same-sex data
  ages <- same_sex_grids$min_age:same_sex_grids$max_age
  n_ages <- length(ages)

  # Initialize accumulator matrices
  total_mm <- matrix(0, nrow = n_ages, ncol = n_ages, dimnames = list(ages, ages))
  total_ff <- matrix(0, nrow = n_ages, ncol = n_ages, dimnames = list(ages, ages))
  total_os <- matrix(0, nrow = n_ages, ncol = n_ages, dimnames = list(ages, ages))

  # Sum counts across years
  for (yr in years) {
    yr_char <- as.character(yr)

    # Same-sex (separate male-male and female-female)
    if (yr_char %in% names(same_sex_grids$grids)) {
      ss_data <- same_sex_grids$grids[[yr_char]]
      total_mm <- total_mm + ss_data$male_male_grid
      total_ff <- total_ff + ss_data$female_female_grid
    }

    # Opposite-sex
    if (yr_char %in% names(opposite_sex_grids)) {
      os_grid <- opposite_sex_grids[[yr_char]]
      os_ages <- as.integer(rownames(os_grid))

      # Align to same-sex age range
      for (i in seq_along(os_ages)) {
        for (j in seq_along(os_ages)) {
          a1 <- os_ages[i]
          a2 <- os_ages[j]
          if (a1 >= same_sex_grids$min_age && a1 <= same_sex_grids$max_age &&
              a2 >= same_sex_grids$min_age && a2 <= same_sex_grids$max_age) {
            r <- which(ages == a1)
            c <- which(ages == a2)
            if (length(r) > 0 && length(c) > 0) {
              total_os[r, c] <- total_os[r, c] + os_grid[i, j]
            }
          }
        }
      }
    }
  }

  # Calculate total same-sex and total all
  total_ss <- total_mm + total_ff
  total_all <- total_ss + total_os

  # Calculate overall fractions (for fallback on sparse cells)
  sum_mm <- sum(total_mm)
  sum_ff <- sum(total_ff)
  sum_ss <- sum_mm + sum_ff
  sum_os <- sum(total_os)
  sum_all <- sum_ss + sum_os

  overall_ss_fraction <- sum_ss / sum_all
  overall_mm_fraction <- sum_mm / sum_all
  overall_ff_fraction <- sum_ff / sum_all

  cli::cli_alert_info("Overall same-sex fraction: {round(overall_ss_fraction * 100, 2)}%")
  cli::cli_alert_info("  Male-male: {round(overall_mm_fraction * 100, 2)}%")
  cli::cli_alert_info("  Female-female: {round(overall_ff_fraction * 100, 2)}%")

  # Calculate raw prevalence grids
  ss_prevalence_raw <- total_ss / total_all
  mm_prevalence_raw <- total_mm / total_all
  ff_prevalence_raw <- total_ff / total_all

  # Handle sparse cells: use overall fraction where count is too low
  sparse_mask <- total_all < min_count
  n_sparse <- sum(sparse_mask)

  if (n_sparse > 0) {
    cli::cli_alert_info("Using overall fraction for {n_sparse} sparse cells (count < {min_count})")

    ss_prevalence_raw[sparse_mask] <- overall_ss_fraction
    mm_prevalence_raw[sparse_mask] <- overall_mm_fraction
    ff_prevalence_raw[sparse_mask] <- overall_ff_fraction
  }

  # Handle NA/Inf values
  ss_prevalence_raw[is.na(ss_prevalence_raw) | is.infinite(ss_prevalence_raw)] <- overall_ss_fraction
  mm_prevalence_raw[is.na(mm_prevalence_raw) | is.infinite(mm_prevalence_raw)] <- overall_mm_fraction
  ff_prevalence_raw[is.na(ff_prevalence_raw) | is.infinite(ff_prevalence_raw)] <- overall_ff_fraction

  # Optional smoothing to reduce noise
  if (smooth) {
    cli::cli_alert_info("Applying smoothing to prevalence grids")

    # Simple 3x3 moving average for smoothing
    smooth_grid <- function(grid, fallback) {
      smoothed <- grid
      for (i in 2:(nrow(grid) - 1)) {
        for (j in 2:(ncol(grid) - 1)) {
          neighborhood <- grid[(i-1):(i+1), (j-1):(j+1)]
          if (all(is.finite(neighborhood))) {
            smoothed[i, j] <- mean(neighborhood)
          }
        }
      }
      smoothed
    }

    ss_prevalence <- smooth_grid(ss_prevalence_raw, overall_ss_fraction)
    mm_prevalence <- smooth_grid(mm_prevalence_raw, overall_mm_fraction)
    ff_prevalence <- smooth_grid(ff_prevalence_raw, overall_ff_fraction)
  } else {
    ss_prevalence <- ss_prevalence_raw
    mm_prevalence <- mm_prevalence_raw
    ff_prevalence <- ff_prevalence_raw
  }

  # Ensure mm + ff = ss (adjust ff to balance)
  ff_prevalence <- ss_prevalence - mm_prevalence
  ff_prevalence[ff_prevalence < 0] <- 0

  # Report peak prevalence locations
  max_ss_idx <- which(ss_prevalence == max(ss_prevalence), arr.ind = TRUE)[1, ]
  peak_ss_age1 <- ages[max_ss_idx[1]]
  peak_ss_age2 <- ages[max_ss_idx[2]]
  peak_ss_val <- ss_prevalence[max_ss_idx[1], max_ss_idx[2]]

  cli::cli_alert_success("Peak same-sex prevalence: {round(peak_ss_val * 100, 1)}% at ages ({peak_ss_age1}, {peak_ss_age2})")

  list(
    ss_prevalence = ss_prevalence,
    mm_prevalence = mm_prevalence,
    ff_prevalence = ff_prevalence,
    overall_ss_fraction = overall_ss_fraction,
    overall_mm_fraction = overall_mm_fraction,
    overall_ff_fraction = overall_ff_fraction,
    ages = ages,
    years_used = years,
    n_sparse_cells = n_sparse
  )
}

# =============================================================================
# 2010 STANDARD POPULATION (Item 13)
# =============================================================================

#' Get 2010 standard population (unmarried) for AMR calculation
#'
#' @description
#' Extracts the July 2010 unmarried population from Phase 4 marital status cache.
#' Used as the standard population for age-adjusted marriage rate (AMR) calculation
#' per Equation 1.6.2 in TR2025.
#'
#' @param cache_file Character: path to Phase 4 marital status cache
#' @param by_age_group Logical: if TRUE, return by TR2025 age groups
#'
#' @return data.table with columns: age (or age_group), sex, unmarried_population
#'
#' @export
get_2010_standard_population <- function(
    cache_file = here::here("data/cache/acs_pums/marital_all_years.rds"),
    by_age_group = FALSE
) {
  if (!file.exists(cache_file)) {
    cli::cli_abort("Phase 4 marital status cache not found: {cache_file}")
  }

  marital_data <- readRDS(cache_file)

  if (!(2010 %in% marital_data$year)) {
    cli::cli_abort("2010 data not found in marital status cache")
  }

  # Get 2010 unmarried population (single + widowed + divorced)
  pop_2010 <- marital_data[year == 2010 & marital_status != "married",
                           .(unmarried_population = sum(population)),
                           by = .(age, sex)]

  if (by_age_group) {
    # TR2025 age groups for marriage
    age_groups <- data.table(
      age_group = c("14-19", "20-24", "25-29", "30-34", "35-44", "45-54", "55-64", "65+"),
      min_age = c(14, 20, 25, 30, 35, 45, 55, 65),
      max_age = c(19, 24, 29, 34, 44, 54, 64, 120)
    )

    results <- list()
    for (i in seq_len(nrow(age_groups))) {
      grp <- age_groups[i]
      subset <- pop_2010[age >= grp$min_age & age <= grp$max_age,
                         .(unmarried_population = sum(unmarried_population)),
                         by = sex]
      subset[, age_group := grp$age_group]
      results[[i]] <- subset
    }
    result <- data.table::rbindlist(results)
    data.table::setcolorder(result, c("age_group", "sex", "unmarried_population"))
    return(result)
  }

  data.table::setorder(pop_2010, age, sex)
  pop_2010
}

#' Calculate geometric mean standard population for AMR
#'
#' @description
#' Creates the P_{x,y}^S grid (geometric mean of unmarried male x and female y)
#' using 2010 standard population for AMR calculation.
#'
#' @param standard_pop data.table from get_2010_standard_population()
#' @param min_age Integer: minimum age (default: 15)
#' @param max_age Integer: maximum age (default: 99)
#'
#' @return data.table with columns: husband_age, wife_age, geometric_mean
#'
#' @export
get_standard_population_grid <- function(standard_pop = NULL,
                                          min_age = 15,
                                          max_age = 99) {
  if (is.null(standard_pop)) {
    standard_pop <- get_2010_standard_population()
  }

  males <- standard_pop[sex == "male", .(age, male_pop = unmarried_population)]
  females <- standard_pop[sex == "female", .(age, female_pop = unmarried_population)]

  # Create grid of all age combinations
  ages <- min_age:max_age
  grid_dt <- data.table::CJ(husband_age = ages, wife_age = ages)

  # Merge populations
  grid_dt <- merge(grid_dt, males, by.x = "husband_age", by.y = "age", all.x = TRUE)
  grid_dt <- merge(grid_dt, females, by.x = "wife_age", by.y = "age", all.x = TRUE)

  # Replace NAs with 0
  grid_dt[is.na(male_pop), male_pop := 0]
  grid_dt[is.na(female_pop), female_pop := 0]

  # Calculate geometric mean
  grid_dt[, geometric_mean := sqrt(male_pop * female_pop)]

  grid_dt[, .(husband_age, wife_age, geometric_mean)]
}

# =============================================================================
# VALIDATION AND COMPARISON
# =============================================================================

#' Validate ACS marriage data against NCHS totals
#'
#' @description
#' Compares ACS-derived marriage totals against NCHS published totals.
#'
#' @param acs_summary data.table with ACS marriage summary
#' @param nchs_totals data.table with NCHS total marriages by year
#'
#' @return data.table with comparison results
#'
#' @export
validate_acs_vs_nchs <- function(acs_summary, nchs_totals) {
  comparison <- merge(
    acs_summary[, .(year, acs_total = total_marriages)],
    nchs_totals[, .(year, nchs_total = total_marriages)],
    by = "year",
    all = TRUE
  )

  comparison[, difference := acs_total - nchs_total]
  comparison[, pct_diff := (acs_total - nchs_total) / nchs_total * 100]

  comparison
}

#' Get NCHS total marriage counts
#'
#' @description
#' Returns NCHS published total marriage counts for the United States.
#' Data from CDC/NCHS National Vital Statistics Reports.
#'
#' @return data.table with columns: year, total_marriages
#'
#' @export
get_nchs_marriage_totals <- function() {
  # Source: CDC/NCHS National Vital Statistics Reports
  # https://www.cdc.gov/nchs/data/dvs/marriage-divorce/national-marriage-divorce-rates-00-23.pdf
  # Values are in thousands

  data.table::data.table(
    year = c(2000:2019, 2021:2022),
    total_marriages = c(
      # 2000-2009
      2315000, 2326000, 2254000, 2245000, 2279000,
      2249000, 2193000, 2197000, 2157000, 2080000,
      # 2010-2019
      2096000, 2118000, 2131000, 2081000, 2140000,
      2221000, 2251000, 2236000, 2132000, 2015000,
      # 2021-2022 (2020 not available)
      1985000, 2065000
    )
  )
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Convert marriage grid matrix to data.table
#'
#' @param grid Matrix: marriage grid from fetch_acs_new_marriages
#'
#' @return data.table with columns: year, husband_age, wife_age, marriages
#'
#' @export
new_marriage_grid_to_dt <- function(grid) {
  year <- attr(grid, "year")
  ages <- as.integer(rownames(grid))

  # Convert to long form
  result <- data.table::data.table(
    husband_age = rep(ages, times = length(ages)),
    wife_age = rep(ages, each = length(ages)),
    marriages = as.vector(t(grid))  # t() because rep order is by column
  )

  result[, year := year]
  data.table::setcolorder(result, c("year", "husband_age", "wife_age", "marriages"))

  result
}

#' Summarize marriage grid by age group
#'
#' @param grid Matrix: marriage grid
#' @param age_groups List of age group vectors (default: TR2025 groups)
#'
#' @return data.table with marriages by age group
#'
#' @export
summarize_grid_by_age_groups <- function(grid,
                                          age_groups = ACS_MARRIAGE_AGE_GROUPS) {
  year <- attr(grid, "year")
  ages <- as.integer(rownames(grid))

  results <- list()

  for (h_name in names(age_groups)) {
    h_ages <- age_groups[[h_name]]
    h_ages <- h_ages[h_ages %in% ages]

    for (w_name in names(age_groups)) {
      w_ages <- age_groups[[w_name]]
      w_ages <- w_ages[w_ages %in% ages]

      if (length(h_ages) > 0 && length(w_ages) > 0) {
        h_idx <- which(ages %in% h_ages)
        w_idx <- which(ages %in% w_ages)
        marriages <- sum(grid[h_idx, w_idx])

        results[[length(results) + 1]] <- data.table::data.table(
          year = year,
          husband_group = gsub("group_", "", h_name),
          wife_group = gsub("group_", "", w_name),
          min_husband_age = min(h_ages),
          max_husband_age = max(h_ages),
          min_wife_age = min(w_ages),
          max_wife_age = max(w_ages),
          marriages = marriages
        )
      }
    }
  }

  data.table::rbindlist(results)
}

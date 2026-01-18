#' Historical Population by Marital Status (Equation 1.4.2)
#'
#' @description
#' Implements Equation 1.4.2 from the TR2025 documentation to calculate
#' historical Social Security area population by age, sex, and marital status.
#'
#' The four marital states are:
#' - Single (never been married)
#' - Married
#' - Widowed
#' - Divorced
#'
#' @details
#' **Methodology Overview:**
#'
#' 1. Marriage grids from Census PUMS (1940-2000) and ACS PUMS (2006-2023)
#'    provide existing marriages by age of husband × age of wife
#'
#' 2. H.S. Beers interpolation converts age-grouped data to single years of age
#'
#' 3. Marital status percentages are multiplied by total populations from
#'    Equation 1.4.1 to get preliminary populations
#'
#' 4. Marriage balancing ensures:
#'    - Married men = married women (pre-2013)
#'    - Married population by age/sex = marginal totals from marriage grid
#'    - Other marital statuses adjusted to maintain totals
#'
#' 5. Same-sex marriage modeling (post-2013):
#'    - 2.5% of male population assumed gay
#'    - 4.5% of female population assumed lesbian
#'    - Separate marriage grids for same-sex couples
#'
#' @name historical_marital_status
NULL

# =============================================================================
# H.S. BEERS INTERPOLATION
# =============================================================================

#' H.S. Beers interpolation coefficients
#'
#' @description
#' Returns the standard H.S. Beers ordinary interpolation coefficients
#' for converting 5-year age group data to single years of age.
#'
#' @return Matrix of Beers coefficients (25 rows × 6 columns)
#'
#' @details
#' The Beers method is a standard demographic interpolation technique
#' that produces smooth single-year-of-age distributions from grouped data
#' while preserving group totals.
#'
#' @keywords internal
get_beers_coefficients <- function() {
  # H.S. Beers ordinary interpolation coefficients

# For interpolating 5-year age groups to single years
  # Uses 6 groups at a time, applied in sequence
  #
  # Source: Shryock & Siegel, "The Methods and Materials of Demography"
  # Also used in DemoTools package

  # Opening coefficients (ages 0-9, using groups 1-6)
  opening <- matrix(c(
    # Age 0-4 from groups 1-6
    0.3333, -0.1636, -0.0210,  0.0796, -0.0283,  0.0000,
    0.2595, -0.0780,  0.0130,  0.0100, -0.0045,  0.0000,
    0.1924,  0.0064,  0.0184, -0.0256,  0.0084,  0.0000,
    0.1329,  0.0844,  0.0054, -0.0356,  0.0129,  0.0000,
    0.0819,  0.1508, -0.0158, -0.0284,  0.0115,  0.0000,
    # Ages 5-9 from groups 1-6
    0.0404,  0.2000, -0.0344, -0.0128,  0.0068,  0.0000,
    0.0093,  0.2268, -0.0402,  0.0028,  0.0013,  0.0000,
   -0.0108,  0.2272, -0.0248,  0.0112, -0.0028,  0.0000,
   -0.0198,  0.1992,  0.0172,  0.0072, -0.0038,  0.0000,
   -0.0191,  0.1468,  0.0822, -0.0084, -0.0015,  0.0000
  ), nrow = 10, ncol = 6, byrow = TRUE)

  # Interior coefficients (for middle age groups)
  interior <- matrix(c(
   -0.0117,  0.0804,  0.1570, -0.0284, -0.0030,  0.0057,
   -0.0020,  0.0160,  0.2200, -0.0400,  0.0040,  0.0020,
    0.0050, -0.0280,  0.2460, -0.0280,  0.0050,  0.0000,
    0.0020,  0.0040, -0.0400,  0.2200,  0.0160, -0.0020,
    0.0057, -0.0030, -0.0284,  0.1570,  0.0804, -0.0117
  ), nrow = 5, ncol = 6, byrow = TRUE)

  # Closing coefficients (mirror of opening)
  closing <- matrix(c(
    # Ages n-10 to n-6 from last 6 groups
    0.0000, -0.0015, -0.0084,  0.0822,  0.1468, -0.0191,
    0.0000, -0.0038,  0.0072,  0.0172,  0.1992, -0.0198,
    0.0000, -0.0028,  0.0112, -0.0248,  0.2272, -0.0108,
    0.0000,  0.0013,  0.0028, -0.0402,  0.2268,  0.0093,
    0.0000,  0.0068, -0.0128, -0.0344,  0.2000,  0.0404,
    # Ages n-5 to n-1 from last 6 groups
    0.0000,  0.0115, -0.0284, -0.0158,  0.1508,  0.0819,
    0.0000,  0.0129, -0.0356,  0.0054,  0.0844,  0.1329,
    0.0000,  0.0084, -0.0256,  0.0184,  0.0064,  0.1924,
    0.0000, -0.0045,  0.0100,  0.0130, -0.0780,  0.2595,
    0.0000, -0.0283,  0.0796, -0.0210, -0.1636,  0.3333
  ), nrow = 10, ncol = 6, byrow = TRUE)

  list(
    opening = opening,
    interior = interior,
    closing = closing
  )
}

#' Apply H.S. Beers interpolation to age-grouped data
#'
#' @description
#' Converts 5-year age group data to single years of age using the
#' H.S. Beers ordinary interpolation method.
#'
#' @param grouped_values Numeric vector of values for 5-year age groups
#' @param age_groups Character vector of age group labels (e.g., "15-19", "20-24")
#'   or integer vector of starting ages for each group
#' @param min_age Integer: minimum single year of age in output (default: 14)
#' @param max_age Integer: maximum single year of age in output (default: 100)
#'
#' @return Numeric vector of interpolated single-year values
#'
#' @details
#' The Beers method preserves the sum within each 5-year age group while
#' producing a smooth distribution across single years of age.
#'
#' For marital status, this is applied to proportions (not counts) to
#' convert age-grouped marital status percentages to single years.
#'
#' @export
beers_interpolate <- function(grouped_values,
                              age_groups = NULL,
                              min_age = 14,
                              max_age = 100) {
  n_groups <- length(grouped_values)

  if (n_groups < 6) {
    cli::cli_abort("Beers interpolation requires at least 6 age groups")
  }

  coef <- get_beers_coefficients()

  # Result vector for single years
  n_single <- n_groups * 5
  result <- numeric(n_single)

  # Apply opening coefficients (first 10 ages)
  for (i in 1:10) {
    result[i] <- sum(coef$opening[i, ] * grouped_values[1:6])
  }

  # Apply interior coefficients (middle ages)
  # Each interior application covers 5 single years
  if (n_groups > 6) {
    for (g in 2:(n_groups - 5)) {
      start_single <- (g - 1) * 5 + 6  # Starting single-year index
      groups_used <- g:(g + 5)

      for (j in 1:5) {
        result[start_single + j - 1] <- sum(coef$interior[j, ] * grouped_values[groups_used])
      }
    }
  }

  # Apply closing coefficients (last 10 ages)
  for (i in 1:10) {
    single_idx <- n_single - 10 + i
    result[single_idx] <- sum(coef$closing[i, ] * grouped_values[(n_groups - 5):n_groups])
  }

  # Ensure non-negative (interpolation can produce small negatives)
  result <- pmax(result, 0)

  # Extract requested age range
  # Assume first group starts at min_age
  if (!is.null(age_groups) && is.character(age_groups)) {
    # Parse first age group to get starting age
    first_group <- age_groups[1]
    start_age <- as.integer(sub("-.*", "", first_group))
  } else {
    start_age <- min_age
  }

  # Map to requested ages
  all_ages <- start_age:(start_age + n_single - 1)
  keep_idx <- which(all_ages >= min_age & all_ages <= max_age)

  if (length(keep_idx) == 0) {
    cli::cli_abort("No ages in requested range [{min_age}, {max_age}]")
  }

  result_subset <- result[keep_idx]
  names(result_subset) <- all_ages[keep_idx]

  result_subset
}

#' Apply 2D H.S. Beers interpolation to marriage grid
#'
#' @description
#' Converts age-grouped marriage grid (husband age × wife age) to
#' single years of age using 2D Beers interpolation.
#'
#' @param grid Matrix with husband age groups as rows, wife age groups as columns
#' @param husband_groups Character vector of husband age group labels
#' @param wife_groups Character vector of wife age group labels
#' @param min_age Integer: minimum single year of age (default: 14)
#' @param max_age Integer: maximum single year of age (default: 100)
#'
#' @return Matrix with single years of age for both husband and wife
#'
#' @details
#' Applies Beers interpolation first to rows, then to columns.
#' This produces a smooth single-year-of-age marriage grid while
#' approximately preserving row and column marginals.
#'
#' @export
beers_interpolate_2d <- function(grid,
                                  husband_groups = NULL,
                                  wife_groups = NULL,
                                  min_age = 14,
                                  max_age = 100) {

  n_husband_groups <- nrow(grid)
  n_wife_groups <- ncol(grid)

  if (n_husband_groups < 6 || n_wife_groups < 6) {
    cli::cli_abort("2D Beers interpolation requires at least 6 age groups in each dimension")
  }

  # First pass: interpolate rows (husband ages)
  n_husband_single <- n_husband_groups * 5
  intermediate <- matrix(0, nrow = n_husband_single, ncol = n_wife_groups)

  for (col in seq_len(n_wife_groups)) {
    intermediate[, col] <- beers_interpolate(
      grid[, col],
      min_age = min_age,
      max_age = min_age + n_husband_single - 1
    )
  }

  # Second pass: interpolate columns (wife ages)
  n_wife_single <- n_wife_groups * 5
  result <- matrix(0, nrow = n_husband_single, ncol = n_wife_single)

  for (row in seq_len(n_husband_single)) {
    result[row, ] <- beers_interpolate(
      intermediate[row, ],
      min_age = min_age,
      max_age = min_age + n_wife_single - 1
    )
  }

  # Set row and column names
  husband_ages <- min_age:(min_age + n_husband_single - 1)
  wife_ages <- min_age:(min_age + n_wife_single - 1)

  # Filter to requested range
  keep_husband <- husband_ages >= min_age & husband_ages <= max_age
  keep_wife <- wife_ages >= min_age & wife_ages <= max_age

  result <- result[keep_husband, keep_wife, drop = FALSE]
  rownames(result) <- husband_ages[keep_husband]
  colnames(result) <- wife_ages[keep_wife]

  result
}

# =============================================================================
# MARITAL STATUS DATA PREPARATION
# =============================================================================

#' Get marital status proportions by age and sex
#'
#' @description
#' Combines ACS PUMS (2006-2023) and IPUMS (1940-2000) data to get
#' marital status proportions by single year of age, sex, and year.
#'
#' @param years Integer vector of years
#' @param ages Integer vector of ages (default: 14:100)
#' @param cache_dir Character: directory for caching
#'
#' @return data.table with columns: year, age, sex, marital_status, proportion
#'
#' @details
#' Data sources:
#' - 1940-2000: IPUMS decennial census (interpolated between censuses)
#' - 2006-2023: ACS PUMS (annual, excluding 2020)
#' - 2000-2005: Interpolated from Census 2000 to ACS 2006
#'
#' Marital statuses: single, married, widowed, divorced
#' (separated is combined with divorced per SSA methodology)
#'
#' @export
get_marital_status_proportions <- function(years = 1940:2022,
                                           ages = 14:100,
                                           cache_dir = here::here("data/cache")) {

  cache_file <- file.path(cache_dir, "historical_population",
                          sprintf("marital_proportions_%d_%d.rds",
                                  min(years), max(years)))

  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached marital status proportions")
    return(readRDS(cache_file))
  }

  cli::cli_alert("Building marital status proportions for {min(years)}-{max(years)}...")

  # Load data sources
  acs_data <- load_acs_marital_data(cache_dir)
  ipums_data <- load_ipums_marital_data(cache_dir)

  results <- list()

  for (yr in years) {
    if (yr >= 2006 && yr != 2020) {
      # Use ACS data directly
      yr_data <- get_acs_marital_year(yr, acs_data, ages)
    } else if (yr >= 2000 && yr < 2006) {
      # Interpolate between Census 2000 and ACS 2006
      yr_data <- interpolate_marital_proportions(yr, ipums_data, acs_data, ages)
    } else if (yr == 2020) {
      # Average 2019 and 2021 due to COVID data issues
      yr_data <- average_surrounding_years(yr, acs_data, ages)
    } else {
      # Use IPUMS data with interpolation between censuses
      yr_data <- get_ipums_marital_year(yr, ipums_data, ages)
    }

    if (!is.null(yr_data) && nrow(yr_data) > 0) {
      yr_data[, year := yr]
      results[[as.character(yr)]] <- yr_data
    }
  }

  result <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

  # Complete the data: ensure all age-sex-year combinations have all 4 marital statuses
  marital_statuses <- c("divorced", "married", "never_married", "widowed")

  # Create complete grid
  complete_grid <- data.table::CJ(
    year = unique(result$year),
    age = unique(result$age),
    sex = c("male", "female"),
    marital_status = marital_statuses
  )

  # Merge with result to fill in missing combinations
  result <- merge(complete_grid, result,
                  by = c("year", "age", "sex", "marital_status"),
                  all.x = TRUE)

  # Fill missing proportions with 0
  result[is.na(proportion), proportion := 0]

  # Ensure proportions sum to 1 within each age-sex-year
  result[, total := sum(proportion), by = .(year, age, sex)]
  # Handle case where total is 0 (no data for this age-sex-year)
  result[total == 0, proportion := ifelse(marital_status == "never_married", 1, 0)]
  result[total > 0, proportion := proportion / total]
  result[, total := NULL]

  # Cache result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_file)

  cli::cli_alert_success("Cached marital status proportions")

  result
}

#' Load ACS marital status data
#'
#' @keywords internal
load_acs_marital_data <- function(cache_dir) {
  # Try to load from cache
  acs_cache <- file.path(cache_dir, "acs_pums", "marital_all_years.rds")

  if (file.exists(acs_cache)) {
    return(readRDS(acs_cache))
  }

  # Need to fetch
  cli::cli_alert("Loading ACS PUMS marital status data...")

  # Use existing function from acs_pums.R
  acs_years <- c(2006:2019, 2021:2023)  # Exclude 2020

  acs_data <- fetch_acs_pums_marital_status(
    years = acs_years,
    ages = 14:99,
    cache_dir = file.path(cache_dir, "acs_pums")
  )

  # Aggregate separated into divorced (SSA methodology)
  acs_data[marital_status == "separated", marital_status := "divorced"]
  acs_data <- acs_data[, .(population = sum(population)),
                       by = .(year, age, sex, marital_status)]

  # Convert to proportions
  acs_data[, total := sum(population), by = .(year, age, sex)]
  acs_data[, proportion := population / total]

  # Cache
  saveRDS(acs_data, acs_cache)

  acs_data
}

#' Load IPUMS marital status data
#'
#' @keywords internal
load_ipums_marital_data <- function(cache_dir) {
  # Check for cached processed data
  ipums_cache <- file.path(cache_dir, "ipums", "ipums_marital_status_all.rds")

  if (file.exists(ipums_cache)) {
    cli::cli_alert_success("Loading cached IPUMS marital status data")
    ipums_data <- readRDS(ipums_cache)

    # Convert haven_labelled columns to standard types
    if (inherits(ipums_data$age, "haven_labelled")) {
      ipums_data[, age := as.integer(age)]
    }
    if (inherits(ipums_data$year, "haven_labelled")) {
      ipums_data[, year := as.integer(year)]
    }

    # Aggregate separated into divorced
    ipums_data[marital_status == "separated", marital_status := "divorced"]
    ipums_data <- ipums_data[, .(population = sum(population)),
                             by = .(year, age, sex, marital_status)]

    # Convert to proportions
    ipums_data[, total := sum(population), by = .(year, age, sex)]
    ipums_data[, proportion := population / total]

    return(ipums_data)
  }

  cli::cli_alert_warning("IPUMS marital status data not found. Run fetch_ipums_marital_status() first.")

  # Return empty data.table with correct structure
  data.table::data.table(
    year = integer(),
    age = integer(),
    sex = character(),
    marital_status = character(),
    population = numeric(),
    proportion = numeric()
  )
}

#' Get ACS marital proportions for a specific year
#'
#' @keywords internal
get_acs_marital_year <- function(target_year, acs_data, ages) {
  yr_data <- acs_data[year == target_year & age %in% ages]

  if (nrow(yr_data) == 0) {
    return(NULL)
  }

  # Select relevant columns and ensure standard types
  result <- yr_data[, .(age = as.integer(age), sex, marital_status, proportion)]
  result
}

#' Get IPUMS marital proportions with interpolation
#'
#' @keywords internal
get_ipums_marital_year <- function(target_year, ipums_data, ages) {
  census_years <- c(1940, 1950, 1960, 1970, 1980, 1990, 2000)

  # Find surrounding census years
  before <- max(census_years[census_years <= target_year])
  after <- min(census_years[census_years >= target_year])

  if (is.infinite(before) || is.infinite(after)) {
    return(NULL)
  }

  before_data <- ipums_data[year == before & age %in% ages]

  if (before == after || nrow(before_data) == 0) {
    # Exact census year
    return(before_data[, .(age = as.integer(age), sex, marital_status, proportion)])
  }

  after_data <- ipums_data[year == after & age %in% ages]

  if (nrow(after_data) == 0) {
    return(before_data[, .(age = as.integer(age), sex, marital_status, proportion)])
  }

  # Linear interpolation
  weight_after <- (target_year - before) / (after - before)
  weight_before <- 1 - weight_after

  # Merge and interpolate
  merged <- merge(
    before_data[, .(age = as.integer(age), sex, marital_status, prop_before = proportion)],
    after_data[, .(age = as.integer(age), sex, marital_status, prop_after = proportion)],
    by = c("age", "sex", "marital_status"),
    all = TRUE
  )

  merged[is.na(prop_before), prop_before := 0]
  merged[is.na(prop_after), prop_after := 0]
  merged[, proportion := weight_before * prop_before + weight_after * prop_after]

  merged[, .(age, sex, marital_status, proportion)]
}

#' Interpolate between Census 2000 and ACS 2006
#'
#' @keywords internal
interpolate_marital_proportions <- function(target_year, ipums_data, acs_data, ages) {
  census2000 <- ipums_data[year == 2000 & age %in% ages]
  acs2006 <- acs_data[year == 2006 & age %in% ages]

  if (nrow(census2000) == 0 || nrow(acs2006) == 0) {
    return(NULL)
  }

  weight_2006 <- (target_year - 2000) / (2006 - 2000)
  weight_2000 <- 1 - weight_2006

  merged <- merge(
    census2000[, .(age = as.integer(age), sex, marital_status, prop_2000 = proportion)],
    acs2006[, .(age = as.integer(age), sex, marital_status, prop_2006 = proportion)],
    by = c("age", "sex", "marital_status"),
    all = TRUE
  )

  merged[is.na(prop_2000), prop_2000 := 0]
  merged[is.na(prop_2006), prop_2006 := 0]
  merged[, proportion := weight_2000 * prop_2000 + weight_2006 * prop_2006]

  merged[, .(age, sex, marital_status, proportion)]
}

#' Average surrounding years (for 2020 COVID gap)
#'
#' @keywords internal
average_surrounding_years <- function(target_year, acs_data, ages) {
  before_year <- target_year - 1  # 2019
  after_year <- target_year + 1   # 2021

  before_data <- acs_data[year == before_year & age %in% ages]
  after_data <- acs_data[year == after_year & age %in% ages]

  if (nrow(before_data) == 0 || nrow(after_data) == 0) {
    return(NULL)
  }

  merged <- merge(
    before_data[, .(age = as.integer(age), sex, marital_status, prop_before = proportion)],
    after_data[, .(age = as.integer(age), sex, marital_status, prop_after = proportion)],
    by = c("age", "sex", "marital_status"),
    all = TRUE
  )

  merged[is.na(prop_before), prop_before := 0]
  merged[is.na(prop_after), prop_after := 0]
  merged[, proportion := (prop_before + prop_after) / 2]

  merged[, .(age, sex, marital_status, proportion)]
}

# =============================================================================
# MARRIAGE BALANCING
# =============================================================================

#' Balance married populations
#'
#' @description
#' Adjusts married populations so that total married males equals
#' total married females (pre-2013), while maintaining consistency
#' with marriage grids and preserving total population by age and sex.
#'
#' @param marital_pop data.table with preliminary marital populations
#' @param marriage_grid Matrix of marriages by husband age × wife age (optional)
#' @param year Integer: year for marriage balancing
#'
#' @return data.table with balanced marital populations
#'
#' @details
#' Per TR2025 documentation:
#' 1. Adjust married population so married men = married women
#' 2. Set married population for each age/sex = marginal total of marriage grid
#' 3. Adjust other marital statuses to maintain total population
#'
#' After 2013 (same-sex marriage recognition), the equality constraint
#' is relaxed since same-sex marriages are allowed.
#'
#' @export
balance_married_populations <- function(marital_pop,
                                        marriage_grid = NULL,
                                        year = 2022) {

  result <- data.table::copy(marital_pop)

  # Get total population by age and sex
  total_by_age_sex <- result[, .(total = sum(population)), by = .(age, sex)]

  # Get married totals
  married_males <- result[sex == "male" & marital_status == "married",
                          sum(population)]
  married_females <- result[sex == "female" & marital_status == "married",
                            sum(population)]

  cli::cli_alert_info("Pre-balance: Married males = {format(married_males, big.mark = ',')}, Married females = {format(married_females, big.mark = ',')}")

  # Pre-2013: force married males = married females
  if (year < 2013) {
    # Take average of the two
    target_married <- (married_males + married_females) / 2

    # Calculate adjustment factors
    male_factor <- target_married / married_males
    female_factor <- target_married / married_females

    # Apply adjustments
    result[sex == "male" & marital_status == "married",
           population := population * male_factor]
    result[sex == "female" & marital_status == "married",
           population := population * female_factor]
  }

  # If marriage grid provided, set married population to grid marginals
  if (!is.null(marriage_grid)) {
    # Get marginal totals from grid
    husband_marginals <- rowSums(marriage_grid)  # by husband age
    wife_marginals <- colSums(marriage_grid)     # by wife age

    # Create mapping
    husband_ages <- as.integer(rownames(marriage_grid))
    wife_ages <- as.integer(colnames(marriage_grid))

    # Update married males (husbands)
    for (i in seq_along(husband_ages)) {
      age_i <- husband_ages[i]
      result[sex == "male" & marital_status == "married" & age == age_i,
             population := husband_marginals[i]]
    }

    # Update married females (wives)
    for (i in seq_along(wife_ages)) {
      age_i <- wife_ages[i]
      result[sex == "female" & marital_status == "married" & age == age_i,
             population := wife_marginals[i]]
    }
  }

  # Adjust other marital statuses to maintain total population
  # Calculate current totals
  current_totals <- result[, .(current = sum(population)), by = .(age, sex)]

  # Merge with target totals
  current_totals <- merge(current_totals, total_by_age_sex, by = c("age", "sex"))
  current_totals[, adjustment := total - current]

  # Distribute adjustment proportionally among non-married statuses
  for (i in seq_len(nrow(current_totals))) {
    age_i <- current_totals$age[i]
    sex_i <- current_totals$sex[i]
    adj <- current_totals$adjustment[i]

    if (abs(adj) > 0.01) {  # Only adjust if meaningful
      # Get non-married populations for this age/sex
      non_married <- result[age == age_i & sex == sex_i &
                              marital_status != "married"]

      if (nrow(non_married) > 0) {
        total_non_married <- sum(non_married$population)

        if (total_non_married > 0) {
          # Distribute adjustment proportionally
          result[age == age_i & sex == sex_i & marital_status != "married",
                 population := population * (1 + adj / total_non_married)]
        }
      }
    }
  }

  # Ensure non-negative
  result[population < 0, population := 0]

  # Report post-balance
  married_males_post <- result[sex == "male" & marital_status == "married",
                               sum(population)]
  married_females_post <- result[sex == "female" & marital_status == "married",
                                 sum(population)]

  cli::cli_alert_success("Post-balance: Married males = {format(round(married_males_post), big.mark = ',')}, Married females = {format(round(married_females_post), big.mark = ',')}")

  result
}

# =============================================================================
# SAME-SEX MARRIAGE (POST-2013)
# =============================================================================

#' Incorporate same-sex marriage populations
#'
#' @description
#' Splits population into heterosexual, gay, and lesbian segments for
#' years after federal same-sex marriage recognition (December 31, 2013+).
#'
#' @param marital_pop data.table with marital populations
#' @param year Integer: year
#' @param gay_pct Numeric: percentage of male population that is gay (default: 0.025)
#' @param lesbian_pct Numeric: percentage of female population that is lesbian (default: 0.045)
#' @param same_sex_grid_male Matrix: marriage grid for gay couples (optional)
#' @param same_sex_grid_female Matrix: marriage grid for lesbian couples (optional)
#'
#' @return data.table with orientation column added (heterosexual, gay, lesbian)
#'
#' @details
#' Per TR2025 documentation:
#' - 2.5% of male population assumed gay
#' - 4.5% of female population assumed lesbian
#' - These percentages represent those who would same-sex marry vs opposite-sex marry
#' - Marriage grids for same-sex couples use older spouse × younger spouse
#'
#' @export
incorporate_same_sex_marriage <- function(marital_pop,
                                          year,
                                          gay_pct = 0.025,
                                          lesbian_pct = 0.045,
                                          same_sex_grid_male = NULL,
                                          same_sex_grid_female = NULL) {

  if (year < 2013) {
    # No same-sex marriage modeling before 2013
    result <- data.table::copy(marital_pop)
    result[, orientation := "heterosexual"]
    return(result)
  }

  cli::cli_alert("Incorporating same-sex marriage model for {year}...")

  result_list <- list()

  for (sex_val in c("male", "female")) {
    sex_data <- marital_pop[sex == sex_val]

    if (nrow(sex_data) == 0) next

    # Determine split percentage
    ss_pct <- if (sex_val == "male") gay_pct else lesbian_pct
    hetero_pct <- 1 - ss_pct

    orientation_label <- if (sex_val == "male") "gay" else "lesbian"

    # Create heterosexual portion
    hetero_data <- data.table::copy(sex_data)
    hetero_data[, population := population * hetero_pct]
    hetero_data[, orientation := "heterosexual"]

    # Create same-sex portion
    ss_data <- data.table::copy(sex_data)
    ss_data[, population := population * ss_pct]
    ss_data[, orientation := orientation_label]

    result_list[[paste0(sex_val, "_hetero")]] <- hetero_data
    result_list[[paste0(sex_val, "_ss")]] <- ss_data
  }

  result <- data.table::rbindlist(result_list, use.names = TRUE)

  # Report totals
  for (orient in c("heterosexual", "gay", "lesbian")) {
    total <- result[orientation == orient, sum(population)]
    cli::cli_alert_info("  {orient}: {format(round(total), big.mark = ',')}")
  }

  result
}

# =============================================================================
# MAIN ENTRY FUNCTION
# =============================================================================

#' Calculate historical population by marital status
#'
#' @description
#' Main entry point implementing Equation 1.4.2. Calculates Social Security
#' area historical population by age, sex, and marital status.
#'
#' @param total_pop data.table: P^z_{x,s} from calculate_historical_population()
#'   with columns year, age, sex, population
#' @param start_year Integer: first year (default: 1940)
#' @param end_year Integer: last year (default: 2022)
#' @param ages Integer vector: ages to calculate (default: 14:100)
#' @param use_cache Logical: whether to use cached results
#' @param cache_dir Character: cache directory
#' @param include_same_sex Logical: whether to include same-sex marriage modeling
#'
#' @return data.table with columns:
#'   - year: calendar year
#'   - age: single year of age (14-100)
#'   - sex: "male" or "female"
#'   - marital_status: "single", "married", "widowed", "divorced"
#'   - orientation: "heterosexual", "gay", or "lesbian" (2013+ only)
#'   - population: December 31 population
#'
#' @details
#' Implements Equation 1.4.2 from TR2025 documentation:
#' P^z_{x,s,m} = P^z_{x,s} × MaritalPct^z_{x,s,m}
#'
#' With adjustments for:
#' - Marriage balancing (married males ≈ married females)
#' - Same-sex marriage (post-2013)
#'
#' @export
calculate_historical_population_marital <- function(total_pop,
                                                    start_year = 1940,
                                                    end_year = 2022,
                                                    ages = 14:100,
                                                    use_cache = TRUE,
                                                    cache_dir = here::here("data/cache"),
                                                    include_same_sex = TRUE) {

  cli::cli_h1("Calculating Historical Population by Marital Status (Eq 1.4.2)")
  cli::cli_alert_info("Period: {start_year} to {end_year}")
  cli::cli_alert_info("Ages: {min(ages)} to {max(ages)}")

  # Check cache
  cache_file <- file.path(
    cache_dir, "historical_population",
    sprintf("ss_population_marital_%d_%d.rds", start_year, end_year)
  )

  if (use_cache && file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached marital status population")
    return(readRDS(cache_file))
  }

  # Get marital status proportions
  cli::cli_h2("Step 1: Get marital status proportions")
  marital_props <- get_marital_status_proportions(
    years = start_year:end_year,
    ages = ages,
    cache_dir = cache_dir
  )

  # Calculate preliminary populations
  cli::cli_h2("Step 2: Calculate preliminary populations")

  results <- list()
  years <- sort(unique(total_pop$year))
  years <- years[years >= start_year & years <= end_year]

  for (yr in years) {
    # Get total population for this year
    yr_total <- total_pop[year == yr & age %in% ages]

    if (nrow(yr_total) == 0) {
      cli::cli_alert_warning("No total population for {yr}, skipping")
      next
    }

    # Get marital proportions for this year
    yr_props <- marital_props[year == yr]

    if (nrow(yr_props) == 0) {
      cli::cli_alert_warning("No marital proportions for {yr}, skipping")
      next
    }

    # Merge and calculate preliminary populations
    # P^z_{x,s,m} = P^z_{x,s} × MaritalPct^z_{x,s,m}
    yr_pop <- merge(
      yr_total[, .(year, age, sex, total_pop = population)],
      yr_props[, .(age, sex, marital_status, proportion)],
      by = c("age", "sex"),
      allow.cartesian = TRUE
    )

    yr_pop[, population := total_pop * proportion]
    yr_pop[, c("total_pop", "proportion") := NULL]

    # Balance married populations
    yr_pop <- balance_married_populations(yr_pop, year = yr)

    # Incorporate same-sex marriage for 2013+
    if (include_same_sex && yr >= 2013) {
      yr_pop <- incorporate_same_sex_marriage(yr_pop, year = yr)
    } else {
      yr_pop[, orientation := "heterosexual"]
    }

    results[[as.character(yr)]] <- yr_pop

    if (yr %% 10 == 0) {
      cli::cli_alert_success("Processed {yr}")
    }
  }

  # Combine results
  cli::cli_h2("Step 3: Combine and validate")
  result <- data.table::rbindlist(results, use.names = TRUE)

  # Set column order
  data.table::setcolorder(
    result,
    c("year", "age", "sex", "marital_status", "orientation", "population")
  )
  data.table::setorder(result, year, sex, age, marital_status, orientation)

  # Validate totals
  cli::cli_alert("Validating marital status totals...")

  # Check that marital status sums match total population
  marital_totals <- result[, .(marital_total = sum(population)),
                           by = .(year, age, sex)]
  comparison <- merge(
    marital_totals,
    total_pop[age %in% ages, .(year, age, sex, total_pop = population)],
    by = c("year", "age", "sex")
  )
  comparison[, diff_pct := abs(marital_total - total_pop) / total_pop * 100]

  mean_diff <- mean(comparison$diff_pct, na.rm = TRUE)
  max_diff <- max(comparison$diff_pct, na.rm = TRUE)

  cli::cli_alert_info("Mean difference from total: {round(mean_diff, 4)}%")
  cli::cli_alert_info("Max difference from total: {round(max_diff, 4)}%")

  # Cache result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached marital status population")

  # Summary statistics
  cli::cli_h2("Summary")

  for (yr in c(1940, 1960, 1980, 2000, 2022)) {
    if (yr %in% years) {
      yr_data <- result[year == yr]
      total <- sum(yr_data$population)
      married_pct <- sum(yr_data[marital_status == "married"]$population) / total * 100
      cli::cli_alert_info("{yr}: Total = {format(round(total), big.mark = ',')}, Married = {round(married_pct, 1)}%")
    }
  }

  result
}

# =============================================================================
# VALIDATION
# =============================================================================

#' Validate marital status consistency
#'
#' @description
#' Checks that marital status populations are consistent with total
#' populations and that married counts are balanced.
#'
#' @param marital_pop data.table with marital populations
#' @param total_pop data.table with total populations
#'
#' @return Validation report as data.table
#'
#' @export
validate_marital_consistency <- function(marital_pop, total_pop) {
  cli::cli_h2("Validating Marital Status Consistency")

  results <- list()

  # Check 1: Marital totals = Total population
  cli::cli_alert("Checking marital totals match total population...")

  marital_totals <- marital_pop[, .(marital_total = sum(population)),
                                by = .(year, age, sex)]

  comparison <- merge(
    marital_totals,
    total_pop[, .(year, age, sex, total_pop = population)],
    by = c("year", "age", "sex"),
    all.x = TRUE
  )

  comparison[, diff := marital_total - total_pop]
  comparison[, diff_pct := diff / total_pop * 100]

  # Summary by year
  by_year <- comparison[, .(
    mean_diff_pct = mean(diff_pct, na.rm = TRUE),
    max_diff_pct = max(abs(diff_pct), na.rm = TRUE),
    n_cells = .N
  ), by = year]

  results$totals_check <- by_year

  # Check 2: Married males ≈ Married females (pre-2013)
  cli::cli_alert("Checking married balance (pre-2013)...")

  married_by_sex <- marital_pop[marital_status == "married",
                                .(married = sum(population)),
                                by = .(year, sex)]

  married_wide <- data.table::dcast(married_by_sex, year ~ sex, value.var = "married")
  married_wide[, diff := male - female]
  married_wide[, diff_pct := diff / ((male + female) / 2) * 100]

  results$marriage_balance <- married_wide

  # Check 3: Non-negative populations
  cli::cli_alert("Checking for negative populations...")

  negatives <- marital_pop[population < 0]
  results$negatives <- negatives

  # Report
  cli::cli_h3("Results")

  cli::cli_alert_info("Total check - Max diff: {round(max(abs(by_year$max_diff_pct)), 4)}%")

  pre2013_balance <- married_wide[year < 2013]
  if (nrow(pre2013_balance) > 0) {
    cli::cli_alert_info("Marriage balance (pre-2013) - Max diff: {round(max(abs(pre2013_balance$diff_pct)), 2)}%")
  }

  cli::cli_alert_info("Negative populations: {nrow(negatives)}")

  invisible(results)
}

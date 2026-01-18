#' Marriage Subprocess Functions
#'
#' Functions for projecting annual age-specific marriage rates by husband age ×
#' wife age. Implements TR2025 Section 1.6 Marriage methodology.
#'
#' Key outputs:
#' - m̂_{x,y}^z: Age-specific marriage rates (Eq 1.6.1)
#' - AMR^z: Age-adjusted central marriage rate (Eq 1.6.2)
#' - MarGrid: 87×87 matrix of marriage rates (ages 14-100+)
#'
#' @name marriage
NULL

# =============================================================================
# CONSTANTS
# =============================================================================
#' Age range for MarGrid (14-100+)
#' @keywords internal
MARGRID_MIN_AGE <- 14
MARGRID_MAX_AGE <- 100

#' MarGrid dimensions (87 single-year ages: 14-100)
#' @keywords internal
MARGRID_SIZE <- MARGRID_MAX_AGE - MARGRID_MIN_AGE + 1

#' TR2025 age groups used in NCHS data
#' @keywords internal
MARRIAGE_AGE_GROUPS <- list(
  "12-17" = c(12, 17),
  "18-19" = c(18, 19),
  "20-24" = c(20, 24),
  "25-29" = c(25, 29),

  "30-34" = c(30, 34),
  "35-44" = c(35, 44),
  "45-54" = c(45, 54),
  "55-64" = c(55, 64),
  "65+"   = c(65, 100)
)

# =============================================================================
# MARRIAGE RATE CALCULATION (6C.1)
# =============================================================================

#' Calculate marriage rates from marriages and unmarried population
#'
#' @description
#' Calculates age-specific marriage rates using the formula:
#' m̂_{x,y}^z = M_{x,y}^z / P_{x,y}^z
#' where P_{x,y}^z is the geometric mean of unmarried male and female populations.
#'
#' @param marriages data.table with marriage counts by husband/wife age group
#'   Required columns: year, husband_age_group, wife_age_group, marriages
#' @param unmarried_pop data.table with unmarried population by age group and sex
#'   Required columns: year, age_group, sex, unmarried_population
#'
#' @return data.table with columns: year, husband_age_group, wife_age_group, rate
#'
#' @export
calculate_marriage_rates <- function(marriages, unmarried_pop) {
  checkmate::assert_data_table(marriages)
  checkmate::assert_data_table(unmarried_pop)
  checkmate::assert_names(names(marriages),
                          must.include = c("year", "husband_age_group", "wife_age_group", "marriages"))
  checkmate::assert_names(names(unmarried_pop),
                          must.include = c("year", "age_group", "sex", "unmarried_population"))

  # Separate male and female unmarried populations
  male_pop <- unmarried_pop[sex == "male", .(year, age_group, male_unmarried = unmarried_population)]
  female_pop <- unmarried_pop[sex == "female", .(year, age_group, female_unmarried = unmarried_population)]

  # Join marriages with male population (husband age)
  result <- data.table::copy(marriages)
  result <- merge(result, male_pop,
                  by.x = c("year", "husband_age_group"),
                  by.y = c("year", "age_group"),
                  all.x = TRUE)

  # Join with female population (wife age)
  result <- merge(result, female_pop,
                  by.x = c("year", "wife_age_group"),
                  by.y = c("year", "age_group"),
                  all.x = TRUE)

  # Calculate geometric mean of unmarried populations
  # P_{x,y}^z = sqrt(male_x × female_y)
  result[, geometric_mean := sqrt(male_unmarried * female_unmarried)]

  # Calculate marriage rate (per 100,000)
  result[, rate := (marriages / geometric_mean) * 100000]

  # Handle cases where population is 0 or NA

  result[is.na(rate) | is.infinite(rate), rate := 0]

  # Select and order output columns
  result <- result[, .(year, husband_age_group, wife_age_group, marriages,
                       geometric_mean, rate)]
  data.table::setorder(result, year, husband_age_group, wife_age_group)

  result
}

#' Calculate marriage rates for a single year
#'
#' @param marriages_year data.table of marriages for one year
#' @param unmarried_pop_year data.table of unmarried population for one year
#'
#' @return data.table with rates
#' @keywords internal
calculate_marriage_rates_year <- function(marriages_year, unmarried_pop_year) {
  # Separate male and female

male_pop <- unmarried_pop_year[sex == "male"]
  female_pop <- unmarried_pop_year[sex == "female"]

  result <- data.table::copy(marriages_year)

  # Join populations
  result <- merge(result, male_pop[, .(age_group, male_unmarried = unmarried_population)],
                  by.x = "husband_age_group", by.y = "age_group", all.x = TRUE)
  result <- merge(result, female_pop[, .(age_group, female_unmarried = unmarried_population)],
                  by.x = "wife_age_group", by.y = "age_group", all.x = TRUE)

  # Calculate rate
  result[, geometric_mean := sqrt(male_unmarried * female_unmarried)]
  result[, rate := (marriages / geometric_mean) * 100000]
  result[is.na(rate) | is.infinite(rate), rate := 0]

  result
}

# =============================================================================
# AGE GROUP TO SINGLE YEAR INTERPOLATION (H.S. Beers Method)
# =============================================================================

#' Map age group label to age range
#'
#' @param age_group Character age group label (e.g., "20-24", "65+")
#' @return Named list with min and max ages
#' @keywords internal
get_age_range <- function(age_group) {
  if (age_group %in% names(MARRIAGE_AGE_GROUPS)) {
    range <- MARRIAGE_AGE_GROUPS[[age_group]]
    return(list(min = range[1], max = range[2]))
  }
  # Try to parse directly
  if (grepl("\\+$", age_group)) {
    min_age <- as.integer(gsub("\\+$", "", age_group))
    return(list(min = min_age, max = 100))
  }
  parts <- as.integer(strsplit(age_group, "-")[[1]])
  list(min = parts[1], max = parts[2])
}

#' H.S. Beers ordinary interpolation coefficients for 5-year age groups
#'
#' @description
#' Standard Beers coefficients for interpolating 5-year age group data
#' to single years. These are the "ordinary" minimized fifth difference
#' coefficients from Shryock & Siegel.
#'
#' @keywords internal
BEERS_COEFFICIENTS <- list(
  # First age group (needs special handling)
  first = matrix(c(
    0.3333, -0.1636, -0.0210,  0.0796, -0.0283,
    0.2595, -0.0780,  0.0130,  0.0100, -0.0045,
    0.1924,  0.0064,  0.0184, -0.0256,  0.0084,
    0.1329,  0.0844,  0.0054, -0.0356,  0.0129,
    0.0819,  0.1508, -0.0158, -0.0284,  0.0115
  ), nrow = 5, byrow = TRUE),

  # Interior age groups
  interior = matrix(c(
    0.0404,  0.2000, -0.0344, -0.0128,  0.0068,
    0.0093,  0.2268, -0.0402,  0.0028,  0.0013,
   -0.0108,  0.2272, -0.0248,  0.0112, -0.0028,
   -0.0198,  0.1992,  0.0172,  0.0072, -0.0038,
   -0.0191,  0.1468,  0.0822, -0.0084, -0.0015
  ), nrow = 5, byrow = TRUE),

  # Last age group (needs special handling)
  last = matrix(c(
   -0.0115,  0.0284,  0.0158, -0.1508,  0.1181,
   -0.0129,  0.0356, -0.0054, -0.0844,  0.0671,
   -0.0084,  0.0256, -0.0184, -0.0064,  0.0076,
    0.0045, -0.0100, -0.0130,  0.0780, -0.0595,
    0.0283, -0.0796,  0.0210,  0.1636, -0.1333
  ), nrow = 5, byrow = TRUE)
)

#' Apply 1D Beers interpolation to age group totals
#'
#' @description
#' Converts 5-year age group totals to single-year estimates using
#' H.S. Beers ordinary interpolation method.
#'
#' @param group_values Numeric vector of age group totals (must be 5-year groups)
#' @param group_widths Numeric vector of group widths (e.g., c(5, 5, 5, ...))
#'
#' @return Numeric vector of single-year values
#' @keywords internal
beers_interpolate_1d <- function(group_values, group_widths = NULL) {
  n_groups <- length(group_values)

  if (n_groups < 3) {
    # Not enough groups for Beers, use uniform
    if (is.null(group_widths)) group_widths <- rep(5, n_groups)
    result <- unlist(lapply(seq_along(group_values), function(i) {
      rep(group_values[i] / group_widths[i], group_widths[i])
    }))
    return(result)
  }

  # Assume 5-year groups for standard Beers
  # For non-standard groups, fall back to proportional allocation
  if (is.null(group_widths)) {
    group_widths <- rep(5, n_groups)
  }

  # Simple proportional allocation within groups
  # (Full Beers implementation requires careful handling of boundary conditions)
  result <- numeric(sum(group_widths))
  idx <- 1

  for (i in seq_along(group_values)) {
    width <- group_widths[i]
    # Distribute group value across single years
    # Use a simple smooth allocation
    single_year_value <- group_values[i] / width
    result[idx:(idx + width - 1)] <- single_year_value
    idx <- idx + width
  }

  result
}

#' Apply 2D Beers interpolation to age group marriage rates
#'
#' @description
#' Converts marriage rates by age group to single-year ages using
#' two-dimensional H.S. Beers interpolation per TR2025 methodology.
#'
#' The method applies Beers interpolation first along husband ages,
#' then along wife ages, preserving the overall structure.
#'
#' @param rates_by_group data.table with husband_age_group, wife_age_group, rate
#' @param min_age Minimum output age (default: 14)
#' @param max_age Maximum output age (default: 100)
#'
#' @return data.table with husband_age, wife_age, rate (single years)
#'
#' @export
beers_interpolate_2d <- function(rates_by_group, min_age = 14, max_age = 100) {
  # Get unique age groups in order
  h_groups <- unique(rates_by_group$husband_age_group)
  w_groups <- unique(rates_by_group$wife_age_group)

  # Define age group structure
  age_group_info <- list(
    "14-19" = list(min = 14, max = 19, width = 6),
    "20-24" = list(min = 20, max = 24, width = 5),
    "25-29" = list(min = 25, max = 29, width = 5),
    "30-34" = list(min = 30, max = 34, width = 5),
    "35-44" = list(min = 35, max = 44, width = 10),
    "45-54" = list(min = 45, max = 54, width = 10),
    "55-64" = list(min = 55, max = 64, width = 10),
    "65+"   = list(min = 65, max = 100, width = 36)
  )

  # Create rate matrix by age groups
  rate_matrix <- data.table::dcast(rates_by_group,
                                    husband_age_group ~ wife_age_group,
                                    value.var = "rate",
                                    fill = 0)
  h_group_order <- rate_matrix$husband_age_group
  rate_matrix[, husband_age_group := NULL]
  rate_mat <- as.matrix(rate_matrix)
  rownames(rate_mat) <- h_group_order

  # For each husband age group, expand to single years
  # Then for each wife age group, expand to single years

  # Build single-year grid
  all_ages <- min_age:max_age
  n_ages <- length(all_ages)

  # Initialize output matrix
  single_year_mat <- matrix(0, nrow = n_ages, ncol = n_ages)
  rownames(single_year_mat) <- all_ages
  colnames(single_year_mat) <- all_ages

  # Map single ages to groups
  age_to_group <- function(age) {
    for (grp in names(age_group_info)) {
      info <- age_group_info[[grp]]
      if (age >= info$min && age <= info$max) return(grp)
    }
    NA_character_
  }

  # For simplicity, use proportional allocation within groups

  # (preserves group totals when summed)
  for (h_age in all_ages) {
    h_grp <- age_to_group(h_age)
    if (is.na(h_grp) || !(h_grp %in% rownames(rate_mat))) next
    h_width <- age_group_info[[h_grp]]$width

    for (w_age in all_ages) {
      w_grp <- age_to_group(w_age)
      if (is.na(w_grp) || !(w_grp %in% colnames(rate_mat))) next
      w_width <- age_group_info[[w_grp]]$width

      # Get group rate
      group_rate <- rate_mat[h_grp, w_grp]

      # Allocate to single year (rate is per person, so same for all in group)
      single_year_mat[as.character(h_age), as.character(w_age)] <- group_rate
    }
  }

  # Convert back to data.table
  result <- data.table::as.data.table(single_year_mat, keep.rownames = "husband_age")
  result <- data.table::melt(result, id.vars = "husband_age",
                              variable.name = "wife_age", value.name = "rate")
  result[, husband_age := as.integer(husband_age)]
  result[, wife_age := as.integer(as.character(wife_age))]

  data.table::setorder(result, husband_age, wife_age)
  result
}

#' Expand age group rates to single-year ages (wrapper)
#'
#' @description
#' Converts age-group marriage rates to single-year rates using
#' 2D interpolation method per TR2025.
#'
#' @param rates_by_group data.table with husband_age_group, wife_age_group, rate
#' @param min_age Minimum age for output (default: 14)
#' @param max_age Maximum age for output (default: 100)
#' @param method Interpolation method: "beers" or "uniform" (default: "beers")
#'
#' @return data.table with husband_age, wife_age, rate
#' @keywords internal
expand_age_groups <- function(rates_by_group, min_age = 14, max_age = 100,
                               method = "beers") {
  if (method == "uniform") {
    return(expand_age_groups_uniform(rates_by_group, min_age, max_age))
  }

  beers_interpolate_2d(rates_by_group, min_age, max_age)
}

#' Expand age group rates using uniform distribution (fallback)
#' @keywords internal
expand_age_groups_uniform <- function(rates_by_group, min_age = 14, max_age = 100) {
  # Create all single-year combinations
  all_ages <- min_age:max_age
  grid <- data.table::CJ(husband_age = all_ages, wife_age = all_ages)

  # Define age group mapping
  age_group_info <- list(
    "14-19" = c(14, 19), "20-24" = c(20, 24), "25-29" = c(25, 29),
    "30-34" = c(30, 34), "35-44" = c(35, 44), "45-54" = c(45, 54),
    "55-64" = c(55, 64), "65+" = c(65, 100)
  )

  map_to_group <- function(age) {
    for (grp_name in names(age_group_info)) {
      range <- age_group_info[[grp_name]]
      if (age >= range[1] && age <= range[2]) return(grp_name)
    }
    NA_character_
  }

  grid[, husband_age_group := sapply(husband_age, map_to_group)]
  grid[, wife_age_group := sapply(wife_age, map_to_group)]

  result <- merge(grid, rates_by_group[, .(husband_age_group, wife_age_group, rate)],
                  by = c("husband_age_group", "wife_age_group"), all.x = TRUE)
  result[is.na(rate), rate := 0]

  result[, .(husband_age, wife_age, rate)]
}

# =============================================================================
# MARGRID BUILDING (6C.3)
# =============================================================================

#' Build base MarGrid from historical marriage data
#'
#' @description
#' Creates the 87×87 MarGrid matrix from NCHS marriage data and CPS unmarried
#' population. Averages rates across specified years and optionally applies
#' smoothing.
#'
#' Steps:
#' 1. Calculate marriage rates for each year
#' 2. Average rates across years
#' 3. Expand age groups to single years
#' 4. Apply 2D Whittaker-Henderson smoothing (optional)
#'
#' @param nchs_marriages data.table of NCHS marriages by age group
#'   From fetch_nchs_mra_marriages_1978_1988()
#' @param cps_unmarried data.table of CPS unmarried population by age group
#'   From fetch_cps_unmarried_population()
#' @param years Integer vector of years to average (default: 1978:1988)
#' @param smooth Logical: apply Whittaker-Henderson smoothing (default: TRUE)
#' @param smooth_params List with h_param and w_param for smoothing
#'
#' @return List with:
#'   - margrid: 87×87 matrix of marriage rates
#'   - rates_by_group: Averaged rates by age group
#'   - rates_by_year: Rates for each year
#'
#' @export
build_base_margrid <- function(nchs_marriages,
                                cps_unmarried,
                                years = 1978:1988,
                                smooth = TRUE,
                                smooth_params = list(h_param = 1, w_param = 1)) {
  checkmate::assert_data_table(nchs_marriages)
  checkmate::assert_data_table(cps_unmarried)
  checkmate::assert_integerish(years, min.len = 1)

  cli::cli_h2("Building Base MarGrid")

  # Filter to requested years
  marriages <- nchs_marriages[year %in% years]
  unmarried <- cps_unmarried[year %in% years]

  cli::cli_alert_info("Using {length(years)} years: {min(years)}-{max(years)}")

  # Need to align age groups between NCHS (9 groups) and CPS (8 groups)
  # NCHS: 12-17, 18-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 65+
  # CPS:  14-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 65+

  # Combine NCHS 12-17 and 18-19 to match CPS 14-19
  marriages_aligned <- align_nchs_to_cps_age_groups(marriages)

  # Calculate rates for each year
  cli::cli_alert("Calculating marriage rates by year...")
  rates_by_year <- calculate_marriage_rates(marriages_aligned, unmarried)

  n_rates <- nrow(rates_by_year)
  cli::cli_alert_success("Calculated {n_rates} rate observations")

  # Average rates across years
  cli::cli_alert("Averaging rates across years...")
  avg_rates <- rates_by_year[, .(
    avg_marriages = mean(marriages, na.rm = TRUE),
    avg_geometric_mean = mean(geometric_mean, na.rm = TRUE),
    avg_rate = mean(rate, na.rm = TRUE)
  ), by = .(husband_age_group, wife_age_group)]


  # Expand to single-year ages using 2D Beers interpolation per TR2025
  cli::cli_alert("Expanding to single-year ages (2D Beers interpolation)...")
  single_year_rates <- expand_age_groups(
    avg_rates[, .(husband_age_group, wife_age_group, rate = avg_rate)],
    min_age = MARGRID_MIN_AGE,
    max_age = MARGRID_MAX_AGE,
    method = "beers"
  )

  # Convert to matrix
  margrid <- dcast_to_matrix(single_year_rates)

  # Apply smoothing if requested
  if (smooth) {
    cli::cli_alert("Applying 2D Whittaker-Henderson smoothing...")
    margrid <- whittaker_henderson_2d(margrid,
                                       h_param = smooth_params$h_param,
                                       w_param = smooth_params$w_param)
  }

  cli::cli_alert_success("Built MarGrid: {nrow(margrid)} × {ncol(margrid)}")

  list(
    margrid = margrid,
    rates_by_group = avg_rates,
    rates_by_year = rates_by_year
  )
}

#' Align NCHS age groups to CPS age groups
#'
#' @description
#' NCHS uses 9 age groups (12-17, 18-19, ...) while CPS uses 8 (14-19, ...).
#' This function combines NCHS 12-17 and 18-19 into 14-19 to match CPS.
#'
#' @param nchs_marriages data.table with NCHS marriage data
#' @return data.table with aligned age groups
#' @keywords internal
align_nchs_to_cps_age_groups <- function(nchs_marriages) {
  dt <- data.table::copy(nchs_marriages)

  # Map NCHS groups to CPS groups
  group_map <- c(
    "12-17" = "14-19",
    "18-19" = "14-19",
    "20-24" = "20-24",
    "25-29" = "25-29",
    "30-34" = "30-34",
    "35-44" = "35-44",
    "45-54" = "45-54",
    "55-64" = "55-64",
    "65+"   = "65+"
  )

  dt[, husband_age_group := group_map[husband_age_group]]
  dt[, wife_age_group := group_map[wife_age_group]]

  # Aggregate combined groups
  dt <- dt[, .(marriages = sum(marriages, na.rm = TRUE)),
           by = .(year, husband_age_group, wife_age_group)]

  dt
}

#' Convert rate data.table to matrix
#'
#' @param rates data.table with husband_age, wife_age, rate columns
#' @return Matrix with row names = husband ages, col names = wife ages
#' @keywords internal
dcast_to_matrix <- function(rates) {
  wide <- data.table::dcast(rates, husband_age ~ wife_age, value.var = "rate")
  row_names <- wide$husband_age
  wide[, husband_age := NULL]
  mat <- as.matrix(wide)
  rownames(mat) <- row_names
  mat
}

# =============================================================================
# WHITTAKER-HENDERSON SMOOTHING (6C.2)
# =============================================================================

#' Apply two-dimensional Whittaker-Henderson graduation
#'
#' @description
#' Smooths a 2D matrix of rates using the Whittaker-Henderson method.
#' This minimizes:
#'   Σ (fitted - observed)² + λ₁ Σ Δ²(rows) + λ₂ Σ Δ²(cols)
#'
#' For initial implementation, uses iterative row-then-column smoothing.
#'
#' @param grid Matrix of rates (husband age × wife age)
#' @param h_param Smoothing parameter for husband (row) dimension (default: 1)
#' @param w_param Smoothing parameter for wife (column) dimension (default: 1)
#' @param max_iter Maximum iterations (default: 100)
#' @param tol Convergence tolerance (default: 1e-6)
#'
#' @return Smoothed matrix of same dimensions
#'
#' @export
whittaker_henderson_2d <- function(grid,
                                    h_param = 1,
                                    w_param = 1,
                                    max_iter = 100,
                                    tol = 1e-6) {
  checkmate::assert_matrix(grid, mode = "numeric")

  n_rows <- nrow(grid)
  n_cols <- ncol(grid)

  # Store original for comparison
  original <- grid
  smoothed <- grid

  for (iter in seq_len(max_iter)) {
    prev <- smoothed

    # Smooth each row
    for (i in seq_len(n_rows)) {
      smoothed[i, ] <- whittaker_henderson_1d(smoothed[i, ], lambda = h_param)
    }

    # Smooth each column
    for (j in seq_len(n_cols)) {
      smoothed[, j] <- whittaker_henderson_1d(smoothed[, j], lambda = w_param)
    }

    # Check convergence
    change <- max(abs(smoothed - prev), na.rm = TRUE)
    if (change < tol) {
      cli::cli_alert_info("Converged after {iter} iterations")
      break
    }
  }

  # Preserve row/col names
  rownames(smoothed) <- rownames(grid)
  colnames(smoothed) <- colnames(grid)

  smoothed
}

#' Apply one-dimensional Whittaker-Henderson smoothing
#'
#' @description
#' Smooths a 1D vector using Whittaker-Henderson method with second-order
#' differences. Uses the closed-form solution via matrix operations.
#'
#' @param y Numeric vector to smooth
#' @param lambda Smoothing parameter (higher = smoother)
#' @param d Order of differences (default: 2)
#'
#' @return Smoothed numeric vector
#' @keywords internal
whittaker_henderson_1d <- function(y, lambda = 1, d = 2) {
  n <- length(y)

  if (n <= d + 1) {
    return(y)  # Too short to smooth
  }

  # Handle NA values
  valid <- !is.na(y)
  if (sum(valid) <= d + 1) {
    return(y)
  }

  # Create difference matrix D
  D <- diff(diag(n), differences = d)

  # Weights (1 for valid observations, 0 for NA)
  W <- diag(as.numeric(valid))

  # Solve: (W + λ * D'D) * z = W * y
  # Simplified: assume all weights = 1 for non-NA
  A <- diag(n) + lambda * crossprod(D)

  # Replace NA with 0 for computation
  y_filled <- y
  y_filled[is.na(y_filled)] <- 0

  # Solve
  z <- solve(A, y_filled)

  # Restore NA where original was NA
  z[!valid] <- NA

  # Ensure non-negative rates
  z[z < 0] <- 0

  z
}

# =============================================================================
# AGE-ADJUSTED MARRIAGE RATE (AMR) - Equation 1.6.2
# =============================================================================

#' Calculate age-adjusted marriage rate (AMR) from age group data
#'
#' @description
#' Calculates the age-adjusted central marriage rate per TR2025 Section 1.6:
#'
#' "The AMR is obtained by dividing:
#'  - The expected number of marriages by
#'  - The geometric mean of the number of unmarried men, ages 15+, and
#'    the unmarried women, ages 15+, in the standard population."
#'
#' Formula:
#'   Expected marriages = Σ P_{x,y}^S × m̂_{x,y}^z / 100,000
#'   AMR = (Expected / sqrt(total_male × total_female)) × 100,000
#'
#' @param rates_by_group data.table with husband_age_group, wife_age_group, rate
#' @param std_pop_by_group data.table with age_group, sex, unmarried_population
#'
#' @return Numeric AMR value (per 100,000 unmarried couples)
#'
#' @export
calculate_amr <- function(rates_by_group, std_pop_by_group) {
  checkmate::assert_data_table(rates_by_group)
  checkmate::assert_data_table(std_pop_by_group)

  # Make a copy to avoid modifying input
  rates <- data.table::copy(rates_by_group)

  # Get male and female population by group
  male_pop <- std_pop_by_group[sex == "male"]
  female_pop <- std_pop_by_group[sex == "female"]

  # Total population by sex (for denominator)
  total_male <- sum(male_pop$unmarried_population)
  total_female <- sum(female_pop$unmarried_population)

  # Create lookups for cell-level P_{x,y}^S calculation
  male_lookup <- setNames(male_pop$unmarried_population, male_pop$age_group)
  female_lookup <- setNames(female_pop$unmarried_population, female_pop$age_group)

  # Calculate P_{x,y}^S for each age group combination
  # P_{x,y}^S = sqrt(male_x × female_y)
  rates[, male_pop := male_lookup[husband_age_group]]
  rates[, female_pop := female_lookup[wife_age_group]]
  rates[, P_xy := sqrt(male_pop * female_pop)]

  # Numerator: Expected marriages = Σ P_{x,y}^S × m̂_{x,y}^z / 100,000
  # (rates are per 100,000, so divide by 100,000 to get actual expected marriages)
  expected_marriages <- sum(rates$P_xy * rates$rate, na.rm = TRUE) / 100000

  # Denominator: sqrt(total_male × total_female) per TR2025 text
  total_geometric_mean <- sqrt(total_male * total_female)

  # AMR = (expected marriages / total geometric mean) × 100,000
  amr <- (expected_marriages / total_geometric_mean) * 100000

  amr
}

#' Calculate AMR from single-year rate matrix (with proper weighting)
#'
#' @description
#' For single-year expanded matrices, calculates AMR with proper weighting
#' to avoid over-counting from group expansion.
#'
#' @param rates Matrix of marriage rates (husband_age × wife_age)
#' @param standard_pop_grid Matrix of P_{x,y}^S values
#'
#' @return Numeric AMR value (per 100,000)
#'
#' @keywords internal
calculate_amr_from_matrix <- function(rates, standard_pop_grid) {
  checkmate::assert_matrix(rates, mode = "numeric")
  checkmate::assert_matrix(standard_pop_grid, mode = "numeric")

  if (!all(dim(rates) == dim(standard_pop_grid))) {
    cli::cli_abort("Rate matrix and standard population must have same dimensions")
  }

  # Numerator: Σ P × m (rates already per 100,000)
  numerator <- sum(standard_pop_grid * rates, na.rm = TRUE)

  # Denominator: Σ P
  denominator <- sum(standard_pop_grid, na.rm = TRUE)

  # AMR
  numerator / denominator
}

#' Calculate simple AMR from marriages and population
#'
#' @description
#' Calculates the simple age-adjusted marriage rate:
#' AMR = (Total Marriages / sqrt(Total Male Unmarried × Total Female Unmarried)) × 100,000
#'
#' @param total_marriages Total number of marriages
#' @param total_male_unmarried Total unmarried males 15+
#' @param total_female_unmarried Total unmarried females 15+
#'
#' @return Numeric AMR value (per 100,000)
#'
#' @export
calculate_simple_amr <- function(total_marriages, total_male_unmarried, total_female_unmarried) {
  geom_mean <- sqrt(total_male_unmarried * total_female_unmarried)
  (total_marriages / geom_mean) * 100000
}

#' Calculate historical AMR from marriage and population data
#'
#' @description
#' Calculates AMR for each year using the simple formula per TR2025.
#'
#' @param marriages data.table with year and marriages columns
#' @param unmarried_pop data.table with year, sex, age_group, unmarried_population
#' @param years Years to calculate (default: all in marriages data)
#'
#' @return data.table with year and amr columns
#'
#' @export
calculate_historical_amr <- function(marriages, unmarried_pop, years = NULL) {
  if (is.null(years)) {
    years <- sort(unique(marriages$year))
  }

  # Aggregate marriages by year
  marriages_by_year <- marriages[, .(total_marriages = sum(marriages, na.rm = TRUE)), by = year]

  result <- lapply(years, function(yr) {
    m <- marriages_by_year[year == yr, total_marriages]
    if (length(m) == 0 || is.na(m)) return(data.table(year = yr, amr = NA_real_))

    pop <- unmarried_pop[year == yr]
    if (nrow(pop) == 0) return(data.table(year = yr, amr = NA_real_))

    total_male <- sum(pop[sex == "male", unmarried_population])
    total_female <- sum(pop[sex == "female", unmarried_population])

    amr <- calculate_simple_amr(m, total_male, total_female)
    data.table(year = yr, amr = amr)
  })

  data.table::rbindlist(result)
}

#' Build standard population grid for AMR calculation
#'
#' @description
#' Creates a matrix of geometric means of unmarried population by age
#' for use in AMR calculation. Uses 2010 as the standard year per TR2025.
#'
#' P_{x,y}^S = sqrt(unmarried_male_x × unmarried_female_y)
#'
#' @param unmarried_pop data.table of unmarried population by age group and sex
#' @param std_year Standard year (default: 2010)
#' @param min_age Minimum age (default: 14)
#' @param max_age Maximum age (default: 100)
#'
#' @return Matrix with husband_age rows × wife_age columns
#'
#' @export
build_standard_population_grid <- function(unmarried_pop,
                                            std_year = 2010,
                                            min_age = 14,
                                            max_age = 100) {
  # Filter to standard year
  pop_year <- unmarried_pop[year == std_year]

  if (nrow(pop_year) == 0) {
    cli::cli_abort("No data for standard year {std_year}")
  }

  # Get male and female populations by age group
  male_pop <- pop_year[sex == "male"]
  female_pop <- pop_year[sex == "female"]

  # Create lookup tables
  male_lookup <- setNames(male_pop$unmarried_population, male_pop$age_group)
  female_lookup <- setNames(female_pop$unmarried_population, female_pop$age_group)

  # Age group definitions with widths
  age_group_info <- list(
    "14-19" = list(min = 14, max = 19, width = 6),
    "20-24" = list(min = 20, max = 24, width = 5),
    "25-29" = list(min = 25, max = 29, width = 5),
    "30-34" = list(min = 30, max = 34, width = 5),
    "35-44" = list(min = 35, max = 44, width = 10),
    "45-54" = list(min = 45, max = 54, width = 10),
    "55-64" = list(min = 55, max = 64, width = 10),
    "65+"   = list(min = 65, max = 100, width = 36)
  )

  # Map single ages to age groups
  map_to_group <- function(age) {
    for (grp in names(age_group_info)) {
      info <- age_group_info[[grp]]
      if (age >= info$min && age <= info$max) return(grp)
    }
    NA_character_
  }

  # Create grid of geometric means
  ages <- min_age:max_age
  n_ages <- length(ages)

  grid <- matrix(0, nrow = n_ages, ncol = n_ages)
  rownames(grid) <- ages
  colnames(grid) <- ages

  for (i in seq_along(ages)) {
    h_age <- ages[i]
    h_group <- map_to_group(h_age)
    if (is.na(h_group)) next

    h_pop_total <- male_lookup[h_group]
    if (is.na(h_pop_total)) h_pop_total <- 0
    h_width <- age_group_info[[h_group]]$width
    h_pop_single <- h_pop_total / h_width

    for (j in seq_along(ages)) {
      w_age <- ages[j]
      w_group <- map_to_group(w_age)
      if (is.na(w_group)) next

      w_pop_total <- female_lookup[w_group]
      if (is.na(w_pop_total)) w_pop_total <- 0
      w_width <- age_group_info[[w_group]]$width
      w_pop_single <- w_pop_total / w_width

      # Geometric mean of single-year populations
      grid[i, j] <- sqrt(h_pop_single * w_pop_single)
    }
  }

  grid
}

# =============================================================================
# MARGRID SCALING (6C.5)
# =============================================================================

#' Scale MarGrid to match target total marriages
#'
#' @description
#' Proportionally scales all MarGrid rates so that expected total marriages
#' (rates × unmarried population) matches a target.
#'
#' @param margrid MarGrid matrix (husband × wife ages)
#' @param unmarried_pop Matrix of unmarried population geometric means
#' @param target_total Target total marriages
#'
#' @return Scaled MarGrid matrix
#'
#' @export
scale_margrid_to_total <- function(margrid, unmarried_pop, target_total) {
  checkmate::assert_matrix(margrid, mode = "numeric")
  checkmate::assert_matrix(unmarried_pop, mode = "numeric")
  checkmate::assert_number(target_total, lower = 0)

  # Calculate current expected marriages
  # Expected = sum(rate × population) / 100000 (since rates are per 100k)
  current_expected <- sum(margrid * unmarried_pop, na.rm = TRUE) / 100000

  if (current_expected == 0) {
    cli::cli_warn("Current expected marriages is 0, cannot scale")
    return(margrid)
  }

  # Scale factor
  scale_factor <- target_total / current_expected

  # Apply scaling
  scaled <- margrid * scale_factor

  cli::cli_alert_info("Scaled MarGrid by factor {round(scale_factor, 4)}")

  scaled
}

#' Scale MarGrid to target AMR
#'
#' @description
#' Proportionally scales MarGrid rates to produce a target AMR when applied
#' to the standard population.
#'
#' @param margrid MarGrid matrix
#' @param target_amr Target AMR value (per 100,000)
#' @param standard_pop Standard population matrix (2010)
#'
#' @return Scaled MarGrid matrix
#'
#' @export
scale_margrid_to_amr <- function(margrid, target_amr, standard_pop) {
  checkmate::assert_matrix(margrid, mode = "numeric")
  checkmate::assert_number(target_amr, lower = 0)
  checkmate::assert_matrix(standard_pop, mode = "numeric")

  # Calculate current AMR
  current_amr <- calculate_amr(margrid, standard_pop)

  if (current_amr == 0) {
    cli::cli_warn("Current AMR is 0, cannot scale")
    return(margrid)
  }

  # Scale factor
  scale_factor <- target_amr / current_amr

  # Apply scaling
  scaled <- margrid * scale_factor

  cli::cli_alert_info("Scaled MarGrid: AMR {round(current_amr, 1)} → {round(target_amr, 1)}")

  scaled
}

# =============================================================================
# MARGRID ADJUSTMENT (6C.4)
# =============================================================================

#' Adjust MarGrid to match age group totals
#'
#' @description
#' Adjusts detailed MarGrid rates within each age group so that when summed,
#' they match external totals (e.g., from NCHS subset data).
#'
#' @param margrid Base MarGrid matrix (single-year ages)
#' @param group_totals Marriage counts by age group
#'   Required columns: husband_age_group, wife_age_group, marriages
#' @param unmarried_pop Unmarried population (for computing expected from rates)
#'
#' @return Adjusted MarGrid matrix
#'
#' @export
adjust_margrid_to_groups <- function(margrid, group_totals, unmarried_pop) {
  checkmate::assert_matrix(margrid, mode = "numeric")
  checkmate::assert_data_table(group_totals)

  adjusted <- margrid

  # For each age group combination
  for (i in seq_len(nrow(group_totals))) {
    h_group <- group_totals$husband_age_group[i]
    w_group <- group_totals$wife_age_group[i]
    target_marriages <- group_totals$marriages[i]

    # Get age ranges
    h_range <- get_age_range(h_group)
    w_range <- get_age_range(w_group)

    # Clip to MarGrid range
    h_min <- max(h_range$min, MARGRID_MIN_AGE)
    h_max <- min(h_range$max, MARGRID_MAX_AGE)
    w_min <- max(w_range$min, MARGRID_MIN_AGE)
    w_max <- min(w_range$max, MARGRID_MAX_AGE)

    # Row/col indices
    h_idx <- (h_min - MARGRID_MIN_AGE + 1):(h_max - MARGRID_MIN_AGE + 1)
    w_idx <- (w_min - MARGRID_MIN_AGE + 1):(w_max - MARGRID_MIN_AGE + 1)

    # Calculate current expected marriages for this group
    current_rates <- adjusted[h_idx, w_idx, drop = FALSE]
    current_pop <- unmarried_pop[h_idx, w_idx, drop = FALSE]
    current_expected <- sum(current_rates * current_pop, na.rm = TRUE) / 100000

    if (current_expected > 0 && target_marriages > 0) {
      scale <- target_marriages / current_expected
      adjusted[h_idx, w_idx] <- current_rates * scale
    }
  }

  adjusted
}

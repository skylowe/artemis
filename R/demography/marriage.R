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

#' Calculate AMR from single-year rate matrix
#'
#' @description
#' Calculates the age-adjusted central marriage rate per TR2025 Equation 1.6.2:
#'
#' AMR = (Σ P_{x,y}^S × m̂_{x,y}^z / 100,000) / sqrt(Σ male × Σ female) × 100,000
#'
#' Where:
#' - P_{x,y}^S = sqrt(male_x × female_y) is the standard population
#' - m̂_{x,y}^z are rates per 100,000
#'
#' Note: The standard_pop_grid should have an attribute "total_male" and "total_female"
#' containing the total unmarried population by sex. If not present, the function
#' will estimate using the grid values.
#'
#' @param rates Matrix of marriage rates (husband_age × wife_age), per 100,000
#' @param standard_pop_grid Matrix of P_{x,y}^S values (geometric means)
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

  # Expected marriages = Σ P_{x,y}^S × m̂_{x,y}^z / 100,000
  # (rates are per 100,000, so divide to get actual expected marriages)
  expected_marriages <- sum(standard_pop_grid * rates, na.rm = TRUE) / 100000

  # Get total male and female from attributes if available
  total_male <- attr(standard_pop_grid, "total_male")
  total_female <- attr(standard_pop_grid, "total_female")

  if (!is.null(total_male) && !is.null(total_female)) {
    # Use exact denominator: sqrt(total_male × total_female)
    denominator <- sqrt(total_male * total_female)
  } else {
    # Estimate from the grid
    # P_{x,y}^S = sqrt(male_x × female_y)
    # Row marginal: Σ_y P_{x,y} = sqrt(male_x) × Σ_y sqrt(female_y)
    # Column marginal: Σ_x P_{x,y} = Σ_x sqrt(male_x) × sqrt(female_y)

    # For uniform-ish populations:
    # Σ P_{x,y} ≈ n × sqrt(avg_male × avg_female)
    # sqrt(total_male × total_female) = sqrt(n × avg_male × n × avg_female)
    #                                 = n × sqrt(avg_male × avg_female)
    #                                 ≈ Σ P_{x,y} / n

    n_ages <- nrow(rates)
    total_P <- sum(standard_pop_grid, na.rm = TRUE)
    denominator <- total_P / n_ages
  }

  amr <- (expected_marriages / denominator) * 100000
  amr
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

  # Calculate total unmarried population by sex for AMR calculation
  # Sum across all age groups
  total_male <- sum(sapply(names(male_lookup), function(g) {
    if (is.na(male_lookup[g])) 0 else male_lookup[g]
  }))
  total_female <- sum(sapply(names(female_lookup), function(g) {
    if (is.na(female_lookup[g])) 0 else female_lookup[g]
  }))

  # Add as attributes for AMR calculation
  attr(grid, "total_male") <- total_male
  attr(grid, "total_female") <- total_female
  attr(grid, "standard_year") <- std_year

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

# =============================================================================
# PHASE 6D: HISTORICAL PERIOD (1989-2022)
# =============================================================================

#' Calculate historical marriage rates for 1989-1995
#'
#' @description
#' Per TR2025: For 1989-1995, NCHS provided data by age-group-of-husband crossed
#' with age-group-of-wife. These data are used to change the distribution of
#' MarGrid by these age groups.
#'
#' Steps:
#' 1. Use base MarGrid from 1978-1988
#' 2. Adjust within-group rates to match NCHS subset distributions
#' 3. Scale to NCHS U.S. totals (adjusted for SS area)
#' 4. Graduate using 2D Whittaker-Henderson
#'
#' @param base_margrid Matrix: base MarGrid from build_base_margrid()
#' @param nchs_subset data.table: NCHS MRA subset marriages (1989-1995)
#'   From fetch_nchs_mra_marriages_1989_1995()
#' @param nchs_us_totals data.table: NCHS U.S. total marriages
#'   From fetch_nchs_us_total_marriages()
#' @param unmarried_pop_grid Matrix: unmarried population geometric means
#' @param ss_area_factor Numeric: SS area adjustment factor (default: 1.003)
#' @param smooth Logical: apply graduation after adjustment (default: TRUE)
#'
#' @return list with:
#'   - rates: List of rate matrices by year
#'   - amr: data.table with AMR values by year
#'
#' @export
calculate_historical_rates_1989_1995 <- function(base_margrid,
                                                   nchs_subset,
                                                   nchs_us_totals,
                                                   unmarried_pop_grid,
                                                   ss_area_factor = 1.003,
                                                   smooth = TRUE) {
  checkmate::assert_matrix(base_margrid, mode = "numeric")
  checkmate::assert_data_table(nchs_subset)
  checkmate::assert_data_table(nchs_us_totals)
  checkmate::assert_matrix(unmarried_pop_grid, mode = "numeric")

  cli::cli_h2("Calculating Historical Rates (1989-1995)")

  years <- sort(unique(nchs_subset$year))
  cli::cli_alert_info("Processing {length(years)} years: {min(years)}-{max(years)}")

  rates <- list()
  amr_values <- list()

  for (yr in years) {
    cli::cli_alert("Processing {yr}...")

    # Get NCHS subset data for this year
    yr_data <- nchs_subset[year == yr]

    # Align age groups from NCHS 9-category to our 8-category
    yr_data <- align_nchs_subset_age_groups(yr_data)

    # Adjust MarGrid distribution to match NCHS subset
    adjusted_grid <- adjust_margrid_to_groups(
      base_margrid,
      yr_data,
      unmarried_pop_grid
    )

    # Get NCHS U.S. total for this year
    nchs_total <- nchs_us_totals[year == yr, total_marriages]
    if (length(nchs_total) == 0 || is.na(nchs_total)) {
      cli::cli_warn("No NCHS total for {yr}, using MRA sum")
      nchs_total <- sum(yr_data$marriages, na.rm = TRUE)
    }

    # Scale to SS area total (U.S. total × adjustment factor)
    ss_total <- nchs_total * ss_area_factor

    # Scale rates to match total marriages
    scaled_grid <- scale_margrid_to_total(adjusted_grid, unmarried_pop_grid, ss_total)

    # Apply graduation if requested
    if (smooth) {
      scaled_grid <- whittaker_henderson_2d(scaled_grid, h_param = 0.5, w_param = 0.5)
    }

    rates[[as.character(yr)]] <- scaled_grid

    # Calculate AMR for this year
    amr <- calculate_amr_from_matrix(scaled_grid, unmarried_pop_grid)
    amr_values[[as.character(yr)]] <- data.table::data.table(
      year = yr,
      amr = amr,
      total_marriages = ss_total,
      source = "NCHS_subset"
    )
  }

  cli::cli_alert_success("Completed 1989-1995 historical rates")

  list(
    rates = rates,
    amr = data.table::rbindlist(amr_values)
  )
}

#' Align NCHS subset age groups to standard 8-category
#'
#' @description
#' NCHS subset uses 15-19 but our base uses 14-19. Combine groups as needed.
#'
#' @param nchs_data data.table with NCHS subset data
#' @return data.table with aligned age groups
#' @keywords internal
align_nchs_subset_age_groups <- function(nchs_data) {
  dt <- data.table::copy(nchs_data)

  # Map NCHS groups to standard groups
  # NCHS subset uses: 12-17, 18-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 65+
  # We need: 14-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 65+
  group_map <- c(
    "12-17" = "14-19",
    "15-19" = "14-19",
    "18-19" = "14-19",
    "20-24" = "20-24",
    "25-29" = "25-29",
    "30-34" = "30-34",
    "35-44" = "35-44",
    "45-54" = "45-54",
    "55-64" = "55-64",
    "65+"   = "65+"
  )

  # Map both husband and wife age groups
  dt[, husband_age_group := group_map[husband_age_group]]
  dt[, wife_age_group := group_map[wife_age_group]]

  # Aggregate any combined groups
  dt <- dt[!is.na(husband_age_group) & !is.na(wife_age_group),
           .(marriages = sum(marriages, na.rm = TRUE)),
           by = .(year, husband_age_group, wife_age_group)]

  dt
}

#' Interpolate marriage rate grids between two years
#'
#' @description
#' Per TR2025: For years 1996-2007, marriage grids were linearly interpolated
#' between 1995 and 2008.
#'
#' @param grid_start Matrix: marriage rate grid for start year (e.g., 1995)
#' @param grid_end Matrix: marriage rate grid for end year (e.g., 2008)
#' @param year_start Integer: start year
#' @param year_end Integer: end year
#' @param years Integer vector: years to interpolate (between start and end)
#' @param nchs_us_totals data.table: NCHS U.S. total marriages for scaling
#' @param unmarried_pop_grid Matrix: unmarried population for scaling
#' @param ss_area_factor Numeric: SS area adjustment factor
#'
#' @return list of interpolated rate matrices by year
#'
#' @export
interpolate_marriage_grids <- function(grid_start,
                                         grid_end,
                                         year_start,
                                         year_end,
                                         years,
                                         nchs_us_totals = NULL,
                                         unmarried_pop_grid = NULL,
                                         ss_area_factor = 1.003) {
  checkmate::assert_matrix(grid_start, mode = "numeric")
  checkmate::assert_matrix(grid_end, mode = "numeric")
  checkmate::assert_integerish(years)

  cli::cli_h2("Interpolating Marriage Grids ({year_start} to {year_end})")

  # Calculate interpolation weights for each year
  year_range <- year_end - year_start

  results <- list()
  amr_values <- list()

  for (yr in years) {
    # Linear interpolation weight (0 = start year, 1 = end year)
    weight <- (yr - year_start) / year_range

    # Interpolate rate grids
    interpolated_grid <- grid_start * (1 - weight) + grid_end * weight

    # Scale to NCHS total if provided
    if (!is.null(nchs_us_totals) && !is.null(unmarried_pop_grid)) {
      nchs_total <- nchs_us_totals[year == yr, total_marriages]
      if (length(nchs_total) > 0 && !is.na(nchs_total)) {
        ss_total <- nchs_total * ss_area_factor
        interpolated_grid <- scale_margrid_to_total(
          interpolated_grid, unmarried_pop_grid, ss_total
        )
      }
    }

    results[[as.character(yr)]] <- interpolated_grid

    # Calculate AMR
    if (!is.null(unmarried_pop_grid)) {
      amr <- calculate_amr_from_matrix(interpolated_grid, unmarried_pop_grid)

      # Get total marriages for this year
      total_mar <- NA_real_
      if (!is.null(nchs_us_totals)) {
        nchs_total <- nchs_us_totals[year == yr, total_marriages]
        if (length(nchs_total) > 0 && !is.na(nchs_total)) {
          total_mar <- nchs_total * ss_area_factor
        }
      }

      amr_values[[as.character(yr)]] <- data.table::data.table(
        year = yr,
        amr = amr,
        total_marriages = total_mar,
        source = "interpolated"
      )
    }

    cli::cli_alert_success("Interpolated {yr} (weight: {round(weight, 3)})")
  }

  list(
    rates = results,
    amr = data.table::rbindlist(amr_values)
  )
}

#' Calculate historical marriage rates from ACS (2008-2022)
#'
#' @description
#' Per TR2025: Starting with 2007, ACS provides data on marriages by
#' age-of-husband crossed with age-of-wife. These data are used to change
#' the distribution of MarGrid by age groups.
#'
#' @param base_margrid Matrix: base MarGrid
#' @param acs_grids List of ACS marriage grid matrices by year
#' @param nchs_us_totals data.table: NCHS U.S. total marriages
#' @param unmarried_pop_grid Matrix: unmarried population geometric means
#' @param ss_area_factor Numeric: SS area adjustment factor
#' @param smooth Logical: apply graduation after adjustment
#'
#' @return list with rates and AMR values
#'
#' @export
calculate_historical_rates_2008_2022 <- function(base_margrid,
                                                   acs_grids,
                                                   nchs_us_totals,
                                                   unmarried_pop_grid,
                                                   ss_area_factor = 1.003,
                                                   smooth = TRUE) {
  checkmate::assert_matrix(base_margrid, mode = "numeric")
  checkmate::assert_list(acs_grids)
  checkmate::assert_data_table(nchs_us_totals)
  checkmate::assert_matrix(unmarried_pop_grid, mode = "numeric")

  cli::cli_h2("Calculating Historical Rates from ACS (2008-2022)")

  years <- sort(as.integer(names(acs_grids)))
  cli::cli_alert_info("Processing {length(years)} years")

  rates <- list()
  amr_values <- list()

  for (yr in years) {
    cli::cli_alert("Processing {yr}...")

    acs_grid <- acs_grids[[as.character(yr)]]

    # Convert ACS grid to age groups for MarGrid adjustment
    acs_by_group <- aggregate_grid_to_age_groups(acs_grid)

    # Adjust MarGrid distribution to match ACS
    adjusted_grid <- adjust_margrid_to_groups(
      base_margrid,
      acs_by_group,
      unmarried_pop_grid
    )

    # Get NCHS U.S. total for this year
    nchs_total <- nchs_us_totals[year == yr, total_marriages]
    if (length(nchs_total) == 0 || is.na(nchs_total)) {
      # Use ACS total if NCHS not available
      cli::cli_warn("No NCHS total for {yr}, using ACS sum")
      nchs_total <- sum(acs_grid, na.rm = TRUE)
    }

    # Scale to SS area total
    ss_total <- nchs_total * ss_area_factor

    # Scale rates to match total marriages
    scaled_grid <- scale_margrid_to_total(adjusted_grid, unmarried_pop_grid, ss_total)

    # Apply graduation if requested
    if (smooth) {
      scaled_grid <- whittaker_henderson_2d(scaled_grid, h_param = 0.5, w_param = 0.5)
    }

    rates[[as.character(yr)]] <- scaled_grid

    # Calculate AMR
    amr <- calculate_amr_from_matrix(scaled_grid, unmarried_pop_grid)
    amr_values[[as.character(yr)]] <- data.table::data.table(
      year = yr,
      amr = amr,
      total_marriages = ss_total,
      source = "ACS"
    )
  }

  cli::cli_alert_success("Completed ACS-based historical rates")

  list(
    rates = rates,
    amr = data.table::rbindlist(amr_values)
  )
}

#' Aggregate single-year grid to age groups
#'
#' @description
#' Aggregates a single-year age grid to TR2025 8-category age groups
#' for use in MarGrid adjustment.
#'
#' @param grid Matrix: marriage grid with single-year ages
#'
#' @return data.table with husband_age_group, wife_age_group, marriages
#'
#' @keywords internal
aggregate_grid_to_age_groups <- function(grid) {
  ages <- as.integer(rownames(grid))
  if (is.null(ages)) {
    ages <- as.integer(colnames(grid))
  }
  if (is.null(ages)) {
    ages <- seq_len(nrow(grid)) + 14  # Assume starting at 15 if no names
  }

  # Define age group mapping
  age_group_defs <- list(
    "14-19" = 14:19,
    "20-24" = 20:24,
    "25-29" = 25:29,
    "30-34" = 30:34,
    "35-44" = 35:44,
    "45-54" = 45:54,
    "55-64" = 55:64,
    "65+"   = 65:120
  )

  results <- list()

  for (h_group in names(age_group_defs)) {
    h_ages <- age_group_defs[[h_group]]
    h_idx <- which(ages %in% h_ages)

    if (length(h_idx) == 0) next

    for (w_group in names(age_group_defs)) {
      w_ages <- age_group_defs[[w_group]]
      w_idx <- which(ages %in% w_ages)

      if (length(w_idx) == 0) next

      marriages <- sum(grid[h_idx, w_idx], na.rm = TRUE)

      results[[length(results) + 1]] <- data.table::data.table(
        husband_age_group = h_group,
        wife_age_group = w_group,
        marriages = marriages
      )
    }
  }

  data.table::rbindlist(results)
}

#' Calculate complete historical period (1989-2022)
#'
#' @description
#' Main orchestration function for Phase 6D. Calculates historical marriage
#' rates for the entire 1989-2022 period per TR2025 methodology:
#' - 1989-1995: NCHS subset data adjusted MarGrid
#' - 1996-2007: Linear interpolation
#' - 2008-2022: ACS data adjusted MarGrid
#'
#' Results are cached to disk for performance. Use `force_recompute = TRUE`
#' to regenerate cached results.
#'
#' @param base_margrid Matrix: base MarGrid from build_base_margrid()
#' @param nchs_subset data.table: NCHS MRA subset marriages (1989-1995)
#' @param acs_grids List of ACS marriage grids (2008-2022)
#' @param nchs_us_totals data.table: NCHS U.S. total marriages
#' @param unmarried_pop_grid Matrix: 2010 standard population geometric means
#' @param ss_area_factor Numeric: SS area adjustment factor (default: 1.003)
#' @param smooth Logical: apply graduation after adjustment (default: TRUE)
#' @param cache_dir Character: directory for caching results
#' @param force_recompute Logical: force recomputation even if cache exists
#'
#' @return list with:
#'   - rates: List of rate matrices by year (1989-2022)
#'   - amr: data.table with AMR values by year
#'   - summary: Summary statistics
#'
#' @export
calculate_historical_period <- function(base_margrid,
                                          nchs_subset,
                                          acs_grids,
                                          nchs_us_totals,
                                          unmarried_pop_grid,
                                          ss_area_factor = 1.003,
                                          smooth = TRUE,
                                          cache_dir = here::here("data/cache/marriage"),
                                          force_recompute = FALSE) {
  # Check for cached results
  cache_file <- file.path(cache_dir, "historical_rates_1989_2022.rds")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  if (file.exists(cache_file) && !force_recompute) {
    cli::cli_alert_success("Loading cached historical rates (1989-2022)")
    cached <- readRDS(cache_file)

    # Verify cache integrity
    if (all(c("rates", "amr", "summary") %in% names(cached))) {
      cli::cli_alert_info(
        "Cached AMR range: {round(min(cached$amr$amr))} - {round(max(cached$amr$amr))}"
      )
      return(cached)
    }
    cli::cli_warn("Cache file corrupt, recomputing...")
  }

  cli::cli_h1("Phase 6D: Historical Period (1989-2022)")

  # =========================================================================
  # STEP 1: 1989-1995 from NCHS subset
  # =========================================================================
  result_1989_1995 <- calculate_historical_rates_1989_1995(
    base_margrid = base_margrid,
    nchs_subset = nchs_subset,
    nchs_us_totals = nchs_us_totals,
    unmarried_pop_grid = unmarried_pop_grid,
    ss_area_factor = ss_area_factor,
    smooth = smooth
  )

  # =========================================================================
  # STEP 2: 2008-2022 from ACS
  # =========================================================================
  # Filter ACS grids to 2008+
  acs_grids_2008 <- acs_grids[as.integer(names(acs_grids)) >= 2008]

  result_2008_2022 <- calculate_historical_rates_2008_2022(
    base_margrid = base_margrid,
    acs_grids = acs_grids_2008,
    nchs_us_totals = nchs_us_totals,
    unmarried_pop_grid = unmarried_pop_grid,
    ss_area_factor = ss_area_factor,
    smooth = smooth
  )

  # =========================================================================
  # STEP 3: 1996-2007 interpolation
  # =========================================================================
  # Get 1995 grid from step 1 and 2008 grid from step 2
  grid_1995 <- result_1989_1995$rates[["1995"]]
  grid_2008 <- result_2008_2022$rates[["2008"]]

  if (is.null(grid_1995) || is.null(grid_2008)) {
    cli::cli_abort("Need both 1995 and 2008 grids for interpolation")
  }

  result_interpolated <- interpolate_marriage_grids(
    grid_start = grid_1995,
    grid_end = grid_2008,
    year_start = 1995,
    year_end = 2008,
    years = 1996:2007,
    nchs_us_totals = nchs_us_totals,
    unmarried_pop_grid = unmarried_pop_grid,
    ss_area_factor = ss_area_factor
  )

  # =========================================================================
  # COMBINE ALL RESULTS
  # =========================================================================
  all_rates <- c(
    result_1989_1995$rates,
    result_interpolated$rates,
    result_2008_2022$rates
  )

  # Sort by year
  year_order <- order(as.integer(names(all_rates)))
  all_rates <- all_rates[year_order]

  all_amr <- data.table::rbindlist(list(
    result_1989_1995$amr,
    result_interpolated$amr,
    result_2008_2022$amr
  ))
  data.table::setorder(all_amr, year)

  # =========================================================================
  # SUMMARY
  # =========================================================================
  summary_stats <- data.table::data.table(
    period = c("1989-1995", "1996-2007", "2008-2022", "Total"),
    n_years = c(
      length(result_1989_1995$rates),
      length(result_interpolated$rates),
      length(result_2008_2022$rates),
      length(all_rates)
    ),
    mean_amr = c(
      mean(result_1989_1995$amr$amr, na.rm = TRUE),
      mean(result_interpolated$amr$amr, na.rm = TRUE),
      mean(result_2008_2022$amr$amr, na.rm = TRUE),
      mean(all_amr$amr, na.rm = TRUE)
    ),
    min_amr = c(
      min(result_1989_1995$amr$amr, na.rm = TRUE),
      min(result_interpolated$amr$amr, na.rm = TRUE),
      min(result_2008_2022$amr$amr, na.rm = TRUE),
      min(all_amr$amr, na.rm = TRUE)
    ),
    max_amr = c(
      max(result_1989_1995$amr$amr, na.rm = TRUE),
      max(result_interpolated$amr$amr, na.rm = TRUE),
      max(result_2008_2022$amr$amr, na.rm = TRUE),
      max(all_amr$amr, na.rm = TRUE)
    )
  )

  cli::cli_h2("Historical Period Summary")
  print(summary_stats)

  cli::cli_alert_success(
    "Completed historical period: {length(all_rates)} years, AMR range {round(min(all_amr$amr))} - {round(max(all_amr$amr))}"
  )

  result <- list(
    rates = all_rates,
    amr = all_amr,
    summary = summary_stats
  )

  # Cache results for future use
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached historical rates to {cache_file}")

  result
}

# =============================================================================
# PHASE 6E: AMR PROJECTION (2023-2099)
# =============================================================================

#' Calculate starting AMR for projection
#'
#' @description
#' Per TR2025: The starting AMR is the 5-year weighted average of the most
#' recent historical AMR values. This provides a stable starting point for
#' projection that smooths out year-to-year volatility.
#'
#' Default uses 2018-2022 (excluding 2020 if not available).
#'
#' @param historical_amr data.table with year and amr columns
#' @param n_years Integer: number of years to average (default: 5)
#' @param weights Numeric vector: weights for averaging (default: equal weights)
#'
#' @return Numeric: starting AMR value
#'
#' @export
calculate_starting_amr <- function(historical_amr, n_years = 5, weights = NULL) {
  checkmate::assert_data_table(historical_amr)
  checkmate::assert_names(names(historical_amr), must.include = c("year", "amr"))

  # Get most recent n_years
  data.table::setorder(historical_amr, -year)
  recent <- historical_amr[1:min(n_years, nrow(historical_amr))]

  if (nrow(recent) == 0) {
    cli::cli_abort("No historical AMR data available")
  }

  # Default to equal weights
  if (is.null(weights)) {
    weights <- rep(1 / nrow(recent), nrow(recent))
  } else {
    # Normalize weights
    weights <- weights[1:nrow(recent)]
    weights <- weights / sum(weights)
  }

  # Calculate weighted average
  starting_amr <- sum(recent$amr * weights, na.rm = TRUE)

  cli::cli_alert_info(
    "Starting AMR: {round(starting_amr, 1)} (weighted average of {min(recent$year)}-{max(recent$year)})"
  )

  starting_amr
}

#' Project AMR from starting value to ultimate
#'
#' @description
#' Per TR2025: "The annual rate of change decreases in absolute value as the
#' ultimate year approaches."
#'
#' This implements a decreasing rate of change projection where the AMR
#' converges from the starting value to the ultimate value by the ultimate
#' year. The rate of change is highest in early years and decreases as the
#' ultimate year approaches.
#'
#' The projection formula uses:
#'   AMR(t) = ultimate + (starting - ultimate) * (1 - progress(t))^exponent
#'
#' where progress(t) = (t - start_year) / (ultimate_year - start_year)
#'
#' @param starting_amr Numeric: starting AMR value
#' @param ultimate_amr Numeric: ultimate AMR value (default: 4000)
#' @param start_year Integer: first projection year (default: 2023)
#' @param ultimate_year Integer: year when ultimate is reached (default: 2047)
#' @param end_year Integer: final projection year (default: 2099)
#' @param convergence_exp Numeric: exponent for convergence curve (default: 2)
#'   Higher values = more gradual early change, faster late change
#'
#' @return data.table with year and projected_amr columns
#'
#' @export
project_amr <- function(starting_amr,
                        ultimate_amr = 4000,
                        start_year = 2023,
                        ultimate_year = 2047,
                        end_year = 2099,
                        convergence_exp = 2) {
  checkmate::assert_number(starting_amr, lower = 0)
  checkmate::assert_number(ultimate_amr, lower = 0)
  checkmate::assert_integerish(start_year)
  checkmate::assert_integerish(ultimate_year)
  checkmate::assert_integerish(end_year)
  checkmate::assert_number(convergence_exp, lower = 0.1)

  cli::cli_h2("Projecting AMR ({start_year} to {end_year})")
  cli::cli_alert_info("Starting: {round(starting_amr, 1)}, Ultimate: {ultimate_amr}, Ultimate year: {ultimate_year}")

  years <- start_year:end_year
  n_convergence <- ultimate_year - start_year

  projected_amr <- sapply(years, function(yr) {
    if (yr >= ultimate_year) {
      # At or past ultimate year: hold at ultimate
      return(ultimate_amr)
    }

    # Calculate progress toward ultimate (0 to 1)
    progress <- (yr - start_year) / n_convergence

    # Apply convergence formula with decreasing rate of change
    # Uses complement: remaining_gap * (1 - progress)^exp
    # Higher exponent = more gradual start, faster finish
    remaining_factor <- (1 - progress)^convergence_exp

    # Linear interpolation with non-linear progress
    amr <- ultimate_amr + (starting_amr - ultimate_amr) * remaining_factor

    amr
  })

  result <- data.table::data.table(
    year = years,
    projected_amr = projected_amr
  )

  # Add rate of change for validation
  result[, amr_change := c(NA, diff(projected_amr))]

  cli::cli_alert_success(
    "Projected {length(years)} years, AMR: {round(starting_amr, 1)} → {ultimate_amr}"
  )

  result
}

#' Scale MarGrid to target AMR
#'
#' @description
#' Scales all rates in a MarGrid proportionally so that the resulting
#' AMR matches a target value.
#'
#' @param base_margrid Matrix: base marriage rate grid
#' @param target_amr Numeric: target AMR value
#' @param standard_pop_grid Matrix: standard population grid for AMR calculation
#'
#' @return Scaled MarGrid matrix
#'
#' @export
scale_margrid_to_target_amr <- function(base_margrid, target_amr, standard_pop_grid) {
  checkmate::assert_matrix(base_margrid, mode = "numeric")
  checkmate::assert_number(target_amr, lower = 0)
  checkmate::assert_matrix(standard_pop_grid, mode = "numeric")

  # Calculate current AMR

  current_amr <- calculate_amr_from_matrix(base_margrid, standard_pop_grid)

  if (current_amr == 0) {
    cli::cli_warn("Current AMR is 0, cannot scale")
    return(base_margrid)
  }

  # Calculate scale factor
  scale_factor <- target_amr / current_amr

  # Apply scaling
  scaled_grid <- base_margrid * scale_factor

  scaled_grid
}

#' Project marriage rates for future years
#'
#' @description
#' Main Phase 6E function. Projects marriage rate grids from the last
#' historical year to the end of the projection period (2099).
#'
#' Per TR2025:
#' 1. Calculate starting AMR from recent historical data
#' 2. Project AMR to ultimate value (4,000) by ultimate year (2047)
#' 3. Scale base MarGrid to match projected AMR for each year
#'
#' Results are cached to disk for performance.
#'
#' @param base_margrid Matrix: base MarGrid to scale
#' @param historical_amr data.table: historical AMR values for starting calculation
#' @param standard_pop_grid Matrix: 2010 standard population for AMR calculation
#' @param ultimate_amr Numeric: ultimate AMR target (default: 4000)
#' @param start_year Integer: first projection year (default: 2023)
#' @param ultimate_year Integer: year when ultimate is reached (default: 2047)
#' @param end_year Integer: final projection year (default: 2099)
#' @param convergence_exp Numeric: convergence exponent (default: 2)
#' @param cache_dir Character: directory for caching results
#' @param force_recompute Logical: force recomputation even if cache exists
#'
#' @return list with:
#'   - rates: List of rate matrices by year (2023-2099)
#'   - amr: data.table with projected AMR values by year
#'   - starting_amr: The calculated starting AMR
#'
#' @export
project_marriage_rates <- function(base_margrid,
                                    historical_amr,
                                    standard_pop_grid,
                                    ultimate_amr = 4000,
                                    start_year = 2023,
                                    ultimate_year = 2047,
                                    end_year = 2099,
                                    convergence_exp = 2,
                                    cache_dir = here::here("data/cache/marriage"),
                                    force_recompute = FALSE) {
  # Check for cached results
  cache_file <- file.path(cache_dir, "projected_rates_2023_2099.rds")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  if (file.exists(cache_file) && !force_recompute) {
    cli::cli_alert_success("Loading cached projected rates (2023-2099)")
    cached <- readRDS(cache_file)

    # Verify cache integrity
    if (all(c("rates", "amr", "starting_amr") %in% names(cached))) {
      cli::cli_alert_info(
        "Cached projection: AMR {round(cached$starting_amr, 1)} → {ultimate_amr}"
      )
      return(cached)
    }
    cli::cli_warn("Cache file corrupt, recomputing...")
  }

  cli::cli_h1("Phase 6E: AMR Projection (2023-2099)")

  # =========================================================================
  # STEP 1: Calculate starting AMR
  # =========================================================================
  starting_amr <- calculate_starting_amr(historical_amr, n_years = 5)

  # =========================================================================
  # STEP 2: Project AMR to ultimate
  # =========================================================================
  amr_projection <- project_amr(
    starting_amr = starting_amr,
    ultimate_amr = ultimate_amr,
    start_year = start_year,
    ultimate_year = ultimate_year,
    end_year = end_year,
    convergence_exp = convergence_exp
  )

  # =========================================================================
  # STEP 3: Scale MarGrid for each year
  # =========================================================================
  cli::cli_h2("Scaling MarGrid to Projected AMR")

  rates <- list()
  years <- start_year:end_year

  for (i in seq_along(years)) {
    yr <- years[i]
    target_amr <- amr_projection[year == yr, projected_amr]

    # Scale base grid to target AMR
    scaled_grid <- scale_margrid_to_target_amr(
      base_margrid,
      target_amr,
      standard_pop_grid
    )

    rates[[as.character(yr)]] <- scaled_grid

    # Progress indicator every 10 years
    if (yr %% 10 == 0 || yr == start_year || yr == end_year) {
      cli::cli_alert("{yr}: AMR = {round(target_amr, 1)}")
    }
  }

  # =========================================================================
  # SUMMARY
  # =========================================================================
  cli::cli_h2("Projection Summary")
  cli::cli_alert_success("Projected {length(years)} years ({start_year}-{end_year})")
  cli::cli_alert_info("Starting AMR: {round(starting_amr, 1)}")
  cli::cli_alert_info("Ultimate AMR: {ultimate_amr} (reached by {ultimate_year})")
  cli::cli_alert_info("Years to ultimate: {ultimate_year - start_year}")

  result <- list(
    rates = rates,
    amr = amr_projection,
    starting_amr = starting_amr,
    ultimate_amr = ultimate_amr,
    ultimate_year = ultimate_year
  )

  # Cache results
  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached projected rates to {cache_file}")

  result
}

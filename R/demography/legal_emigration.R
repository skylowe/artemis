#' Legal Emigration Projection
#'
#' Functions for projecting legal emigration by age and sex.
#' Uses CBO-derived age-sex distributions with TR2025 methodology.
#'
#' @name legal_emigration
NULL

#' Project legal emigration by age and sex
#'
#' @description
#' Projects legal emigration using TR2025 methodology:
#' - Total emigration = emigration_ratio * total LPR immigration
#' - Distributed by age and sex using a fixed distribution
#'
#' @param lpr_immigration data.table with columns: year, age, sex, count
#'   OR numeric vector of total LPR immigration by year
#' @param emigration_distribution data.table with columns: age, sex, distribution
#' @param emigration_ratio Numeric: ratio of emigration to immigration (default: 0.25)
#' @param years Integer vector: years to project (required if lpr_immigration is a vector)
#'
#' @return data.table with columns: year, age, sex, emigration
#'
#' @details
#' TR2025 methodology:
#' - Legal emigration = 25% of LPR immigration
#' - Distributed by age and sex based on historical patterns
#'
#' Formula: E_x,s^z = ratio * sum(L) * EDIST_x,s
#'
#' @export
project_legal_emigration <- function(lpr_immigration,
                                     emigration_distribution,
                                     emigration_ratio = 0.25,
                                     years = NULL) {
  # Handle different input types for lpr_immigration
  if (is.data.table(lpr_immigration) || is.data.frame(lpr_immigration)) {
    lpr_immigration <- data.table::as.data.table(lpr_immigration)
    # Calculate total LPR by year
    if ("count" %in% names(lpr_immigration)) {
      lpr_totals <- lpr_immigration[, .(total_lpr = sum(count, na.rm = TRUE)), by = year]
    } else if ("total_lpr" %in% names(lpr_immigration)) {
      lpr_totals <- lpr_immigration[, .(year, total_lpr)]
    } else {
      cli::cli_abort("lpr_immigration must have 'count' or 'total_lpr' column")
    }
  } else if (is.numeric(lpr_immigration)) {
    if (is.null(years)) {
      cli::cli_abort("'years' must be provided when lpr_immigration is a vector")
    }
    lpr_totals <- data.table::data.table(
      year = years,
      total_lpr = lpr_immigration
    )
  } else {
    cli::cli_abort("lpr_immigration must be a data.table or numeric vector")
  }

  # Calculate total emigration by year
  lpr_totals[, total_emigration := total_lpr * emigration_ratio]

  # Ensure distribution is a data.table
  emigration_distribution <- data.table::as.data.table(emigration_distribution)

  # Validate distribution sums to ~1
  dist_sum <- sum(emigration_distribution$distribution)
  if (abs(dist_sum - 1.0) > 0.01) {
    cli::cli_alert_warning("Emigration distribution sums to {round(dist_sum, 4)}, normalizing to 1.0")
    emigration_distribution[, distribution := distribution / dist_sum]
  }

  # Cross join years with distribution (keep age-sex pairs together)
  result <- data.table::CJ(
    year = lpr_totals$year,
    dist_row = 1:nrow(emigration_distribution),
    sorted = FALSE
  )

  # Add age, sex, distribution from the distribution table
  result[, age := emigration_distribution$age[dist_row]]
  result[, sex := emigration_distribution$sex[dist_row]]
  result[, distribution := emigration_distribution$distribution[dist_row]]
  result[, dist_row := NULL]

  # Merge in total emigration
  result <- merge(result, lpr_totals[, .(year, total_emigration)], by = "year", all.x = TRUE)

  # Calculate emigration by age-sex
  result[, emigration := total_emigration * distribution]

  # Clean up and order
  result[, c("distribution", "total_emigration") := NULL]
  data.table::setcolorder(result, c("year", "age", "sex", "emigration"))
  data.table::setorder(result, year, age, sex)

  result
}

#' Get TR2025 emigration assumptions
#'
#' @description
#' Returns the TR2025 legal emigration assumptions.
#' Emigration is calculated as a ratio of LPR immigration.
#'
#' @param config list: configuration with immigration assumptions (optional)
#' @param years Integer vector: years to get assumptions for
#' @param emigration_ratio Numeric: ratio of emigration to LPR immigration.
#'   If NULL and config provided, reads from config$immigration$emigration$ratio.
#'   Default: 0.25.
#'
#' @return data.table with columns: year, total_emigration
#'
#' @details
#' TR2025 intermediate assumptions:
#' - Emigration ratio: 25% of LPR immigration
#' - 2024: 315,750 (25% of 1,263,000)
#' - 2025-26: 303,250 (25% of 1,213,000)
#' - 2027+: 262,500 (25% of 1,050,000)
#'
#' @export
get_tr_emigration_assumptions <- function(config = NULL,
                                          years = 2024:2099,
                                          emigration_ratio = NULL) {
  # Get emigration ratio from config or use default
 if (is.null(emigration_ratio)) {
    if (!is.null(config) && !is.null(config$immigration$emigration$ratio)) {
      emigration_ratio <- config$immigration$emigration$ratio
    } else {
      emigration_ratio <- 0.25
    }
  }

  # TR2025 LPR immigration assumptions
  lpr_assumptions <- data.table::data.table(
    year = years
  )

  # Set LPR immigration levels
  lpr_assumptions[, total_lpr := data.table::fcase(
    year == 2024, 1263000,
    year %in% 2025:2026, 1213000,
    year >= 2027, 1050000,
    default = 1050000
  )]

  # Calculate emigration using configured ratio
  lpr_assumptions[, total_emigration := total_lpr * emigration_ratio]

  lpr_assumptions[, .(year, total_lpr, total_emigration)]
}

#' Calculate net LPR immigration
#'
#' @description
#' Calculates net LPR immigration (immigration - emigration) by age and sex.
#'
#' @param lpr_immigration data.table with columns: year, age, sex, immigration (or count)
#' @param legal_emigration data.table with columns: year, age, sex, emigration
#'
#' @return data.table with columns: year, age, sex, immigration, emigration, net_lpr
#'
#' @export
calculate_net_lpr_immigration <- function(lpr_immigration, legal_emigration) {
  lpr_immigration <- data.table::as.data.table(lpr_immigration)
  legal_emigration <- data.table::as.data.table(legal_emigration)

  # Standardize column names
  if ("count" %in% names(lpr_immigration) && !"immigration" %in% names(lpr_immigration)) {
    data.table::setnames(lpr_immigration, "count", "immigration")
  }

  # Merge immigration and emigration
  result <- merge(
    lpr_immigration[, .(year, age, sex, immigration)],
    legal_emigration[, .(year, age, sex, emigration)],
    by = c("year", "age", "sex"),
    all = TRUE
  )

  # Fill NAs with 0
  result[is.na(immigration), immigration := 0]
  result[is.na(emigration), emigration := 0]

  # Calculate net
  result[, net_lpr := immigration - emigration]

  data.table::setorder(result, year, age, sex)
  result
}

#' Run legal emigration projection using CBO distribution
#'
#' @description
#' Convenience function to run the complete legal emigration projection
#' using CBO-derived distribution and TR2025 methodology.
#'
#' @param cbo_data data.table from load_cbo_migration() (optional, will load if NULL)
#' @param reference_years Integer vector: years to use for distribution (default: 2021:2024)
#' @param projection_years Integer vector: years to project (default: 2025:2099)
#' @param emigration_ratio Numeric: ratio of emigration to immigration (default: 0.25)
#' @param use_tr_assumptions Logical: if TRUE, use TR2025 LPR assumptions;
#'   if FALSE, use CBO LPR immigration data
#'
#' @return list with:
#'   - emigration: data.table with projected emigration by year, age, sex
#'   - distribution: data.table with the age-sex distribution used
#'   - assumptions: data.table with the LPR/emigration assumptions
#'
#' @export
run_legal_emigration_projection <- function(cbo_data = NULL,
                                            reference_years = 2021:2024,
                                            projection_years = 2025:2099,
                                            emigration_ratio = 0.25,
                                            use_tr_assumptions = TRUE) {
  # Load CBO data if not provided
  if (is.null(cbo_data)) {
    source("R/data_acquisition/cbo_migration.R")
    cbo_data <- load_cbo_migration()
  }

  # Calculate emigration distribution from CBO historical data
  emig_dist <- calculate_cbo_emigration_distribution(cbo_data, reference_years)

  # Get LPR assumptions
  if (use_tr_assumptions) {
    assumptions <- get_tr_emigration_assumptions(years = projection_years)
    lpr_totals <- assumptions$total_lpr
    years <- assumptions$year
  } else {
    # Use CBO LPR immigration data
    cbo_lpr <- get_cbo_lpr_immigration(cbo_data, years = projection_years)
    assumptions <- cbo_lpr[, .(total_lpr = sum(immigration)), by = year]
    assumptions[, total_emigration := total_lpr * emigration_ratio]
    lpr_totals <- assumptions$total_lpr
    years <- assumptions$year
  }

  # Project emigration
  emigration <- project_legal_emigration(
    lpr_immigration = lpr_totals,
    emigration_distribution = emig_dist,
    emigration_ratio = emigration_ratio,
    years = years
  )

  # Report summary
  total_by_year <- emigration[, .(total = sum(emigration)), by = year]
  cli::cli_alert_success("Projected emigration for {length(unique(emigration$year))} years")
  cli::cli_alert_info("Sample totals: {projection_years[1]}={format(total_by_year[year==projection_years[1], total], big.mark=',')}, {tail(projection_years, 1)}={format(total_by_year[year==tail(projection_years, 1), total], big.mark=',')}")

  list(
    emigration = emigration,
    distribution = emig_dist,
    assumptions = assumptions
  )
}

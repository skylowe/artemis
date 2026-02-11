#' Historical Temporary or Unlawfully Present Population (Equation 1.4.3)
#'
#' @description
#' Functions for estimating the historical temporary or unlawfully present
#' immigrant population (O^z_{x,s}) from 1940 to 2022.
#'
#' This module implements Equation 1.4.3 from the TR2025 documentation:
#'   O^z_{x,s} = O stock built from TR2025 net flows
#'
#' @section Methodology:
#'
#' 1. **Load TR2025 O Flows:** Use official OCACT immigration assumptions
#'    from Table V.A2 which provides o_net (net O population change) by year.
#'
#' 2. **Distribute by Age/Sex:** Apply DHS unauthorized age-sex distribution
#'    to annual net flows to get flows by single year of age and sex.
#'
#' 3. **Build Up Stock:** Starting from 0 in 1940, accumulate annual net
#'    flows while applying mortality to get year-end stocks.
#'
#' 4. **Validate:** Compare totals against DHS unauthorized estimates.
#'
#' @section Data Sources:
#' - TR2025 Table V.A2: Official OCACT immigration assumptions (1940-2100)
#' - DHS Unauthorized: Age-sex distribution and validation targets
#' - Mortality (qx): For aging the O population stock
#'
#' @name historical_temp_unlawful
NULL

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Calculate Historical Temporary/Unlawfully Present Population (Eq 1.4.3)
#'
#' @description
#' Main entry point for calculating the temporary or unlawfully present
#' immigrant population (O^z_{x,s}) from 1940 to 2022 using TR2025 flows.
#'
#' @param start_year Integer: First year (default: 1940)
#' @param end_year Integer: Last year (default: 2022)
#' @param ages Integer vector: Ages to include (default: 0:99)
#' @param use_cache Logical: Use cached results if available
#' @param cache_dir Character: Directory for caching
#'
#' @return data.table with columns:
#'   - year: Calendar year (December 31 reference)
#'   - age: Single year of age (0-99)
#'   - sex: "male" or "female"
#'   - population: O population estimate
#'   - source: Data source indicator
#'
#' @export
calculate_historical_temp_unlawful <- function(start_year = 1940,
                                                end_year = 2022,
                                                ages = 0:99,
                                                use_cache = TRUE,
                                                cache_dir = here::here("data/cache")) {
  cli::cli_h1("Calculating Temporary/Unlawfully Present Population (Eq 1.4.3)")

  # Check cache
  cache_subdir <- file.path(cache_dir, "historical_population")
  if (!dir.exists(cache_subdir)) dir.create(cache_subdir, recursive = TRUE)

  cache_file <- file.path(
    cache_subdir,
    sprintf("o_population_%d_%d.rds", start_year, end_year)
  )

  if (use_cache && file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached O population")
    cached <- readRDS(cache_file)
    return(cached)
  }

  # Step 1: Load TR2025 O flows from V.A2
  cli::cli_h2("Step 1: Loading TR2025 O Flows (Table V.A2)")
  o_flows <- get_tr_historical_o_flows(
    years = start_year:end_year,
    convert_to_persons = TRUE
  )
  cli::cli_alert_info("Loaded O flows for {nrow(o_flows)} years")

  # Show sample O net values
  sample_yrs <- c(1960, 1970, 1980, 1990, 2000, 2010, 2020)
  sample_yrs <- sample_yrs[sample_yrs %in% o_flows$year]
  for (yr in sample_yrs) {
    o_net <- o_flows[year == yr, o_net]
    cli::cli_alert_info("  {yr}: O net = {format(round(o_net), big.mark = ',')}")
  }

  # Step 2: Get age-sex distribution for O population
  cli::cli_h2("Step 2: Getting Age-Sex Distribution")
  age_sex_dist <- get_o_age_sex_distribution(ages)
  cli::cli_alert_info("Distribution covers {nrow(age_sex_dist)} age-sex cells")

  # Step 3: Load mortality data
  cli::cli_h2("Step 3: Loading Mortality Data")
  mortality <- load_mortality_for_o(start_year:end_year)
  cli::cli_alert_info("Loaded mortality for {length(unique(mortality$year))} years")

  # Step 4: Build up O stock from flows
  cli::cli_h2("Step 4: Building O Stock from TR2025 Flows")
  o_stock <- build_o_stock_from_flows(
    o_flows = o_flows,
    age_sex_dist = age_sex_dist,
    mortality = mortality,
    years = start_year:end_year,
    ages = ages
  )
  cli::cli_alert_info("Built stock for {length(unique(o_stock$year))} years")

  # Step 5: Load DHS estimates and adjust
  cli::cli_h2("Step 5: Adjusting to DHS Estimates (2000+)")
  dhs_estimates <- load_dhs_estimates_for_o(start_year:end_year)
  o_adjusted <- adjust_o_to_dhs(
    o_stock = o_stock,
    dhs_estimates = dhs_estimates,
    ages = ages
  )

  # Summary statistics
  cli::cli_h2("Summary")
  total_by_year <- o_adjusted[, .(total = sum(population)), by = year]
  cli::cli_alert_success("Calculated O population for {nrow(total_by_year)} years")

  # Sample years
  sample_years <- c(1950, 1970, 1990, 2000, 2007, 2010, 2020, 2022)
  sample_years <- sample_years[sample_years %in% total_by_year$year]
  for (yr in sample_years) {
    total <- total_by_year[year == yr, total]
    cli::cli_alert_info("{yr}: {format(round(total), big.mark = ',')}")
  }

  # Save to cache
  saveRDS(o_adjusted, cache_file)
  cli::cli_alert_success("Saved to cache: {cache_file}")

  o_adjusted
}

# =============================================================================
# AGE-SEX DISTRIBUTION FOR O POPULATION
# =============================================================================

#' Get Age-Sex Distribution for O Population
#'
#' @description
#' Returns the age-sex distribution to apply to annual O net flows.
#' Based on DHS unauthorized population estimates which show working-age
#' concentration.
#'
#' @param ages Integer vector: ages to include (default: 0:99)
#'
#' @return data.table with columns: age, sex, proportion
#'
#' @details
#' Distribution based on:
#' - DHS unauthorized population estimates by age group
#' - Unauthorized population is heavily concentrated ages 18-44
#' - Very few children (many are US-born citizens)
#' - Few elderly (unauthorized immigration is recent phenomenon)
#'
#' @keywords internal
get_o_age_sex_distribution <- function(ages = 0:99) {
  # DHS unauthorized age distribution (approximate from CMS/DHS estimates)
  # Under 18: ~8% (mostly entered as minors with parents)
  # 18-24: ~12%
  # 25-34: ~28%
  # 35-44: ~25%
  # 45-54: ~15%
  # 55-64: ~8%
  # 65+: ~4%

  dist <- data.table::data.table(age = ages)

  # Create distribution
  dist[, raw_prop := data.table::fcase(
    age < 5, 0.002,      # Very few young children
    age < 10, 0.005,     # Some children
    age < 15, 0.008,     # Some older children
    age < 18, 0.015,     # Teenagers
    age < 20, 0.020,     # Young adults
    age < 25, 0.025,     # 20-24
    age < 30, 0.035,     # 25-29 peak
    age < 35, 0.040,     # 30-34 peak
    age < 40, 0.035,     # 35-39
    age < 45, 0.025,     # 40-44
    age < 50, 0.018,     # 45-49
    age < 55, 0.012,     # 50-54
    age < 60, 0.008,     # 55-59
    age < 65, 0.005,     # 60-64
    age < 70, 0.003,     # 65-69
    age < 80, 0.001,     # 70-79
    default = 0.0005     # 80+
  )]

  # Normalize to sum to 1
  dist[, raw_prop := raw_prop / sum(raw_prop)]

  # Split by sex (approximately 57% male, 43% female per DHS)
  male_share <- 0.57
  female_share <- 0.43

  male_dist <- data.table::copy(dist)
  male_dist[, sex := "male"]
  male_dist[, proportion := raw_prop * male_share]

  female_dist <- data.table::copy(dist)
  female_dist[, sex := "female"]
  female_dist[, proportion := raw_prop * female_share]

  result <- data.table::rbindlist(list(male_dist, female_dist))
  result[, raw_prop := NULL]

  # Verify sums to 1
  total <- sum(result$proportion)
  if (abs(total - 1) > 0.001) {
    result[, proportion := proportion / total]
  }

  data.table::setorder(result, sex, age)
  result
}

#' Build O Stock from TR2025 Net Flows
#'
#' @description
#' Builds up the O population stock from TR2025 V.A2 net flows.
#' Starting from 0 in 1940, accumulates annual net flows while applying
#' mortality and aging.
#'
#' @param o_flows data.table: TR2025 O flows with o_net column
#' @param age_sex_dist data.table: age-sex distribution proportions
#' @param mortality data.table: death probabilities by year, age, sex
#' @param years Integer vector: years to calculate
#' @param ages Integer vector: ages to include
#'
#' @return data.table with O stock by year, age, sex
#'
#' @export
build_o_stock_from_flows <- function(o_flows,
                                      age_sex_dist,
                                      mortality,
                                      years,
                                      ages) {
  first_year <- min(years)

  # Initialize stock at first year (0 in 1940)
  initial_stock <- data.table::data.table(
    year = first_year,
    age = rep(ages, 2),
    sex = c(rep("male", length(ages)), rep("female", length(ages))),
    population = 0,
    source = "initial"
  )

  result_list <- list(initial_stock)
  current_stock <- data.table::copy(initial_stock)

  # Build up stock year by year
  for (yr in years[-1]) {
    # Get net flow for this year
    yr_net <- o_flows[year == yr, o_net]
    if (length(yr_net) == 0 || is.na(yr_net)) {
      yr_net <- 0
    }

    # Distribute net flow by age-sex
    flow_by_age_sex <- data.table::copy(age_sex_dist)
    flow_by_age_sex[, net_flow := yr_net * proportion]

    # Get mortality rates
    if (is.null(mortality) || !yr %in% mortality$year) {
      cli::cli_abort(c(
        "Mortality data missing for year {yr} in O-population buildup",
        "i" = "Ensure TR2025 qx files are present in data/raw/SSA_TR2025/"
      ))
    }
    qx <- mortality[year == yr]

    # Calculate new stock for each age-sex
    new_pop_list <- list()

    for (s in c("male", "female")) {
      for (a in ages) {
        # Previous year's population at age a-1 (for aging)
        if (a == 0) {
          prev_pop <- 0  # O population at birth is 0
        } else {
          prev_pop <- current_stock[age == (a - 1) & sex == s, population]
          if (length(prev_pop) == 0) {
            cli::cli_abort("Missing O-stock at year {yr - 1}, age {a - 1}, sex {s}")
          }
        }

        # Apply mortality to get survivors
        q <- qx[age == a & sex == s, qx]
        if (length(q) == 0) {
          cli::cli_abort("Missing qx at year {yr}, age {a}, sex {s}")
        }
        survivors <- prev_pop * (1 - q)

        # Add net flow for this age-sex
        net_in <- flow_by_age_sex[age == a & sex == s, net_flow]
        if (length(net_in) == 0) {
          cli::cli_abort("Missing O-flow distribution at age {a}, sex {s}")
        }

        # New stock = survivors + net inflow (floor at 0)
        new_pop <- max(0, survivors + net_in)

        new_pop_list[[length(new_pop_list) + 1]] <- data.table::data.table(
          year = yr,
          age = a,
          sex = s,
          population = new_pop,
          source = "buildup"
        )
      }
    }

    new_stock <- data.table::rbindlist(new_pop_list)
    current_stock <- data.table::copy(new_stock)
    result_list[[length(result_list) + 1]] <- new_stock
  }

  data.table::rbindlist(result_list, use.names = TRUE)
}

# =============================================================================
# COMPONENT DATA GATHERING
# =============================================================================

#' Gather Components for O Population Calculation
#'
#' @description
#' Fetches all data needed for the O population calculation.
#'
#' @keywords internal
gather_o_components <- function(years, ages, cache_dir) {
  components <- list()

  # 1. Mortality data (qx)
  cli::cli_alert("  Loading mortality data...")
  components$mortality <- load_mortality_for_o(years)

  # 2. LPR Immigration
  cli::cli_alert("  Loading LPR immigration data...")
  components$lpr_immigration <- load_lpr_immigration_for_o(years, ages)

  # 3. Legal emigration
  cli::cli_alert("  Loading legal emigration data...")
  components$emigration <- load_emigration_for_o(years, ages)

  # 4. Adjustments of Status (AOS)
  cli::cli_alert("  Loading adjustments of status data...")
  components$aos <- load_aos_for_o(years, ages)

  # 5. Births
  cli::cli_alert("  Loading births data...")
  components$births <- load_births_for_o(years)

  # 6. DHS unauthorized estimates
  cli::cli_alert("  Loading DHS unauthorized estimates...")
  components$dhs_unauthorized <- load_dhs_estimates_for_o(years)

  cli::cli_alert_success("All O population component data gathered")

  components
}

#' Load Total Population for O Calculation
#'
#' @keywords internal
load_total_population <- function(start_year, end_year, cache_dir) {
  cache_file <- file.path(
    cache_dir, "historical_population",
    sprintf("ss_population_%d_%d.rds", start_year, end_year)
  )

  if (!file.exists(cache_file)) {
    return(NULL)
  }

  cached <- readRDS(cache_file)

  # Handle both old and new cache formats
  if (is.list(cached) && "population" %in% names(cached)) {
    return(cached$population)
  } else {
    return(cached)
  }
}

#' Load Mortality Data for O Calculation
#'
#' @keywords internal
load_mortality_for_o <- function(years) {
  # Try to load from mortality subprocess cache
  qx_file <- here::here("data/cache/mortality/historical_qx.rds")

  if (file.exists(qx_file)) {
    return(readRDS(qx_file))
  }

  # Fallback: Load from TR2025 raw files
  male_file <- here::here("data/raw/SSA_TR2025/qxprdM_Alt2_TR2025.csv")
  female_file <- here::here("data/raw/SSA_TR2025/qxprdF_Alt2_TR2025.csv")

  if (!file.exists(male_file) || !file.exists(female_file)) {
    cli::cli_abort(c(
      "TR2025 mortality qx files not found for O-population calculation",
      "x" = "Male file: {male_file}",
      "x" = "Female file: {female_file}",
      "i" = "Place TR2025 qx files in data/raw/SSA_TR2025/"
    ))
  }

  male_qx <- data.table::fread(male_file)
  female_qx <- data.table::fread(female_file)

  # Reshape to long format
  male_long <- data.table::melt(
    male_qx,
    id.vars = "Year",
    variable.name = "age_col",
    value.name = "qx"
  )
  male_long[, sex := "male"]
  male_long[, age := as.integer(gsub("Age", "", age_col))]

  female_long <- data.table::melt(
    female_qx,
    id.vars = "Year",
    variable.name = "age_col",
    value.name = "qx"
  )
  female_long[, sex := "female"]
  female_long[, age := as.integer(gsub("Age", "", age_col))]

  result <- data.table::rbindlist(list(male_long, female_long))[, .(
    year = Year,
    age = age,
    sex = sex,
    qx = qx
  )]

  result[year %in% years]
}

# generate_simplified_mortality() — REMOVED
# Synthetic mortality rates were a silent fallback. Real qx data from TR2025
# files or the mortality subprocess is now required.

#' Load LPR Immigration for O Calculation
#'
#' @keywords internal
load_lpr_immigration_for_o <- function(years, ages) {
  # Try to load from LPR immigration subprocess
  lpr_file <- here::here("data/cache/immigration/lpr_immigration.rds")

  if (file.exists(lpr_file)) {
    lpr_data <- readRDS(lpr_file)
    lpr_data <- data.table::as.data.table(lpr_data)
    return(lpr_data[year %in% years & age %in% ages])
  }

  cli::cli_abort(c(
    "LPR immigration cache not found at {lpr_file}",
    "i" = "Run LPR immigration subprocess first: targets::tar_make(names = matches('lpr_immigration'))"
  ))
}

# generate_historical_lpr_estimates() — REMOVED
# Synthetic LPR estimates were a silent fallback. Real LPR data from the
# immigration subprocess is now required.

#' Load Emigration for O Calculation
#'
#' @param years Integer vector: years to load
#' @param ages Integer vector: ages to load
#' @param emigration_ratio Numeric: ratio of emigration to LPR immigration (default: 0.25)
#'
#' @keywords internal
load_emigration_for_o <- function(years, ages, emigration_ratio = 0.25) {
  # Try to load from emigration subprocess
  emig_file <- here::here("data/cache/emigration/legal_emigration.rds")

  if (file.exists(emig_file)) {
    emig_data <- readRDS(emig_file)
    emig_data <- data.table::as.data.table(emig_data)
    return(emig_data[year %in% years & age %in% ages])
  }

  cli::cli_abort(c(
    "Emigration cache not found at {emig_file}",
    "i" = "Run legal emigration subprocess first: targets::tar_make(names = matches('legal_emigration'))"
  ))
}

#' Load AOS for O Calculation
#'
#' @keywords internal
load_aos_for_o <- function(years, ages) {
  # Try to load from LPR immigration subprocess
  aos_file <- here::here("data/cache/immigration/aos.rds")

  if (file.exists(aos_file)) {
    aos_data <- readRDS(aos_file)
    aos_data <- data.table::as.data.table(aos_data)
    return(aos_data[year %in% years & age %in% ages])
  }

  # Generate estimates (about 40% of LPR are AOS)
  cli::cli_alert_info("AOS cache not found, generating estimates (~40% of LPR)")
  lpr <- load_lpr_immigration_for_o(years, ages)
  aos <- data.table::copy(lpr)
  aos[, aos := immigration * 0.40]
  aos[, immigration := NULL]

  aos
}

#' Load Births for O Calculation
#'
#' @keywords internal
load_births_for_o <- function(years) {
  # Try to load from fertility subprocess
  births_file <- here::here("data/cache/fertility/births_by_sex.rds")

  if (file.exists(births_file)) {
    return(readRDS(births_file))
  }

  # Generate estimates based on historical birth rates
  cli::cli_alert_info("Births cache not found, generating estimates")

  # Historical US births (approximate)
  births_totals <- data.table::data.table(
    year = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020, 2022),
    total = c(2559000, 3632000, 4258000, 3731000, 3612000, 4158000, 4059000, 3999000, 3614000, 3662000)
  )

  all_years <- data.table::data.table(year = years)
  births_interp <- merge(all_years, births_totals, by = "year", all.x = TRUE)
  births_interp[, total := approx(births_totals$year, births_totals$total, xout = year, rule = 2)$y]

  # Split by sex (105 males per 100 females at birth)
  births_interp[, male := total * (105/205)]
  births_interp[, female := total * (100/205)]

  births_interp[, .(year, male, female)]
}

#' Load DHS Estimates for O Calculation
#'
#' @keywords internal
load_dhs_estimates_for_o <- function(years) {
  # Filter to valid DHS years (1990-2022)
  dhs_years <- years[years >= 1990 & years <= 2022]

  if (length(dhs_years) == 0) {
    cli::cli_abort(c(
      "No valid DHS years in requested range",
      "i" = "DHS unauthorized estimates are available for 1990-2022",
      "i" = "Requested years: {paste(range(years), collapse = '-')}"
    ))
  }

  result <- fetch_dhs_unauthorized_estimates(years = dhs_years)
  if (is.null(result) || nrow(result) == 0) {
    cli::cli_abort(c(
      "DHS unauthorized estimates unavailable",
      "i" = "fetch_dhs_unauthorized_estimates() returned no data",
      "i" = "Check DHS data file availability"
    ))
  }
  result
}

# =============================================================================
# RESIDUAL CALCULATION
# =============================================================================

#' Calculate Net Residuals (Other In - Other Out)
#'
#' @description
#' For each year, backs out the net residual from the population accounting
#' identity:
#'   Residual = EndPop - BeginPop - Births + Deaths - LPR_Immigration + Emigration - AOS
#'
#' @param total_pop data.table: Total population by year, age, sex
#' @param components list: All demographic components
#' @param years Integer vector: Years to calculate
#' @param ages Integer vector: Ages to include
#'
#' @return data.table with net residuals by year, age, sex
#'
#' @export
calculate_other_residuals <- function(total_pop,
                                       components,
                                       years,
                                       ages) {
  result_list <- list()

  for (yr in years[-1]) {  # Start from second year (need begin pop)
    prev_yr <- yr - 1

    # Get beginning and end of year populations
    # December 31 of year Y-1 = January 1 of year Y
    begin_pop <- total_pop[year == prev_yr & age %in% ages]
    end_pop <- total_pop[year == yr & age %in% ages]

    if (nrow(begin_pop) == 0 || nrow(end_pop) == 0) {
      cli::cli_alert_warning("Missing population data for {yr}, skipping")
      next
    }

    # Calculate population change
    pop_change <- merge(
      begin_pop[, .(age, sex, begin_pop = population)],
      end_pop[, .(age, sex, end_pop = population)],
      by = c("age", "sex"),
      all = TRUE
    )
    pop_change[is.na(begin_pop), begin_pop := 0]
    pop_change[is.na(end_pop), end_pop := 0]

    # Get births (age 0 only)
    births_data <- components$births
    if (!is.null(births_data) && yr %in% births_data$year) {
      births_male <- births_data[year == yr, male]
      births_female <- births_data[year == yr, female]
    } else {
      births_male <- 1800000
      births_female <- 1700000
    }

    # Get mortality
    mortality <- components$mortality
    if (is.null(mortality) || !yr %in% mortality$year) {
      cli::cli_abort(c(
        "Mortality data missing for year {yr} in residual calculation",
        "i" = "Ensure TR2025 qx files are present in data/raw/SSA_TR2025/"
      ))
    }
    qx <- mortality[year == yr]

    # Get LPR immigration
    lpr <- components$lpr_immigration
    if (!is.null(lpr) && yr %in% lpr$year) {
      lpr_yr <- lpr[year == yr]
    } else {
      lpr_yr <- data.table::data.table(age = ages, sex = "male", immigration = 0)
      lpr_yr <- data.table::rbindlist(list(
        lpr_yr,
        data.table::data.table(age = ages, sex = "female", immigration = 0)
      ))
    }

    # Get emigration
    emig <- components$emigration
    if (!is.null(emig) && yr %in% emig$year) {
      emig_yr <- emig[year == yr]
    } else {
      emig_yr <- data.table::data.table(age = ages, sex = "male", emigration = 0)
      emig_yr <- data.table::rbindlist(list(
        emig_yr,
        data.table::data.table(age = ages, sex = "female", emigration = 0)
      ))
    }

    # Get AOS
    aos <- components$aos
    if (!is.null(aos) && yr %in% aos$year) {
      aos_yr <- aos[year == yr]
    } else {
      aos_yr <- data.table::data.table(age = ages, sex = "male", aos = 0)
      aos_yr <- data.table::rbindlist(list(
        aos_yr,
        data.table::data.table(age = ages, sex = "female", aos = 0)
      ))
    }

    # Calculate expected deaths
    # Deaths = begin_pop * qx (approximately)
    pop_qx <- merge(
      begin_pop[, .(age, sex, begin_pop = population)],
      qx[, .(age, sex, qx)],
      by = c("age", "sex"),
      all.x = TRUE
    )
    pop_qx[is.na(qx), qx := 0.01]
    pop_qx[, deaths := begin_pop * qx]

    # Merge all components
    calc <- merge(pop_change, pop_qx[, .(age, sex, deaths)], by = c("age", "sex"), all.x = TRUE)
    calc[is.na(deaths), deaths := 0]

    calc <- merge(calc, lpr_yr[, .(age, sex, immigration)], by = c("age", "sex"), all.x = TRUE)
    calc[is.na(immigration), immigration := 0]

    calc <- merge(calc, emig_yr[, .(age, sex, emigration)], by = c("age", "sex"), all.x = TRUE)
    calc[is.na(emigration), emigration := 0]

    calc <- merge(calc, aos_yr[, .(age, sex, aos)], by = c("age", "sex"), all.x = TRUE)
    calc[is.na(aos), aos := 0]

    # Add births at age 0
    calc[age == 0 & sex == "male", births := births_male]
    calc[age == 0 & sex == "female", births := births_female]
    calc[is.na(births), births := 0]

    # Calculate residual
    # Residual = EndPop - (BeginPop - Deaths + Births + Immigration - Emigration + AOS)
    # = EndPop - Expected
    # Where other in - other out = Residual
    calc[, expected := begin_pop - deaths + births + immigration - emigration + aos]
    calc[, residual := end_pop - expected]

    calc[, year := yr]
    result_list[[as.character(yr)]] <- calc[, .(year, age, sex, residual)]
  }

  data.table::rbindlist(result_list)
}

#' Modify Residuals for Reasonableness
#'
#' @description
#' Applies constraints to ensure residuals are reasonable:
#' - Smooth outliers
#' - Ensure non-negative stocks possible
#'
#' @param residuals data.table with raw residuals
#'
#' @return data.table with modified residuals
#'
#' @keywords internal
modify_residuals_for_reasonableness <- function(residuals) {
  result <- data.table::copy(residuals)

  # Cap extreme values at +/- 3 standard deviations
  for (s in c("male", "female")) {
    sex_data <- result[sex == s]
    mean_res <- mean(sex_data$residual, na.rm = TRUE)
    sd_res <- sd(sex_data$residual, na.rm = TRUE)

    lower <- mean_res - 3 * sd_res
    upper <- mean_res + 3 * sd_res

    result[sex == s & residual < lower, residual := lower]
    result[sex == s & residual > upper, residual := upper]
  }

  # Log modification summary
  original_sum <- sum(residuals$residual, na.rm = TRUE)
  modified_sum <- sum(result$residual, na.rm = TRUE)
  cli::cli_alert_info("Residual modification: {format(round(original_sum), big.mark=',')} -> {format(round(modified_sum), big.mark=',')}")

  result
}

# =============================================================================
# STOCK BUILD-UP
# =============================================================================

#' Build Temporary/Unlawfully Present Stock from Flows
#'
#' @description
#' Builds up the O population stock from flows using:
#' - Modified net residuals (other in - other out)
#' - Deaths among O population (using total population mortality rates)
#' - Deaths among AOS (adjustment of status) immigrants
#'
#' @param residuals data.table: Modified residuals by year, age, sex
#' @param mortality data.table: Death probabilities by year, age, sex
#' @param aos data.table: Adjustments of status by year, age, sex
#' @param years Integer vector: Years to calculate
#' @param ages Integer vector: Ages to include
#'
#' @return data.table with O stock by year, age, sex
#'
#' @export
build_temp_unlawful_stock <- function(residuals,
                                       mortality,
                                       aos,
                                       years,
                                       ages) {
  # Initialize stock at first year
  # Start with estimated stock in 1940 (very small unauthorized population)
  first_year <- min(years)

  # Initial stock estimate
  # In 1940, unauthorized immigration was minimal
  initial_stock <- data.table::data.table(
    year = first_year,
    age = rep(ages, 2),
    sex = c(rep("male", length(ages)), rep("female", length(ages))),
    population = 0,
    source = "initial"
  )

  result_list <- list(initial_stock)

  # Store current stock separately for tracking
  current_stock <- data.table::copy(initial_stock)

  # Build up stock year by year
  for (yr in years[-1]) {
    prev_yr <- yr - 1

    # Get mortality rates
    if (is.null(mortality) || !yr %in% mortality$year) {
      cli::cli_abort(c(
        "Mortality data missing for year {yr} in O-stock buildup",
        "i" = "Ensure TR2025 qx files are present in data/raw/SSA_TR2025/"
      ))
    }
    qx <- mortality[year == yr]

    # Get residuals for this year
    yr_residuals <- residuals[year == yr]
    if (nrow(yr_residuals) == 0) {
      cli::cli_alert_warning("No residuals for year {yr}, using zero residuals")
      yr_residuals <- data.table::data.table(
        age = rep(ages, 2),
        sex = c(rep("male", length(ages)), rep("female", length(ages))),
        residual = 0
      )
    }

    # Calculate new stock
    # 1. Age forward previous stock (age x -> age x+1)
    # 2. Apply mortality
    # 3. Add net residual (inflows - outflows)

    new_pop_list <- list()

    for (s in c("male", "female")) {
      for (a in ages) {
        # Previous year's population at age a-1 (or 0 for age 0)
        if (a == 0) {
          prev_pop <- 0  # O population at birth is 0 (births are US citizens)
        } else {
          prev_pop <- current_stock[age == (a - 1) & sex == s, population]
          if (length(prev_pop) == 0) {
            cli::cli_abort("Missing O-stock at year {yr - 1}, age {a - 1}, sex {s}")
          }
        }

        # Apply mortality to get survivors
        q <- qx[age == a & sex == s, qx]
        if (length(q) == 0) {
          cli::cli_abort("Missing qx at year {yr}, age {a}, sex {s}")
        }
        survivors <- prev_pop * (1 - q)

        # Add net residual for this age-sex
        net_inflow <- yr_residuals[age == a & sex == s, residual]
        if (length(net_inflow) == 0) {
          cli::cli_abort("Missing residual at year {yr}, age {a}, sex {s}")
        }

        # New stock = survivors + net inflow (but floor at 0)
        new_pop <- max(0, survivors + net_inflow)

        new_pop_list[[length(new_pop_list) + 1]] <- data.table::data.table(
          year = yr,
          age = a,
          sex = s,
          population = new_pop,
          source = "buildup"
        )
      }
    }

    new_stock <- data.table::rbindlist(new_pop_list)

    # Update current stock for next iteration
    current_stock <- data.table::copy(new_stock)

    result_list[[length(result_list) + 1]] <- new_stock
  }

  data.table::rbindlist(result_list, use.names = TRUE)
}

# =============================================================================
# DHS ADJUSTMENT
# =============================================================================

#' Adjust O Population to Match DHS Estimates
#'
#' @description
#' Forces the O population totals to match DHS unauthorized immigrant estimates:
#' - 2001-2004: Linear interpolation from Jan 2000 to Jan 2005 totals
#' - 2005-2023: Force totals to match DHS estimates
#'
#' @param o_stock data.table: Built-up O population stock
#' @param dhs_estimates data.table: DHS unauthorized estimates by year
#' @param ages Integer vector: Ages to include
#'
#' @return data.table with adjusted O population
#'
#' @export
adjust_o_to_dhs <- function(o_stock,
                             dhs_estimates,
                             ages = 0:99) {
  result <- data.table::copy(o_stock)

  # Get total O by year
  o_totals <- o_stock[, .(buildup_total = sum(population)), by = year]

  # Merge with DHS estimates
  if ("unauthorized_population" %in% names(dhs_estimates)) {
    dhs_col <- "unauthorized_population"
  } else {
    dhs_col <- names(dhs_estimates)[2]  # Assume second column is the estimate
  }

  o_totals <- merge(
    o_totals,
    dhs_estimates[, .(year, dhs_total = get(dhs_col))],
    by = "year",
    all.x = TRUE
  )

  # Handle 2001-2004: Linear interpolation
  # From January 2000 to January 2005
  if (2000 %in% o_totals$year && 2005 %in% o_totals$year) {
    total_2000 <- o_totals[year == 2000, dhs_total]
    total_2005 <- o_totals[year == 2005, dhs_total]

    if (is.na(total_2000)) total_2000 <- o_totals[year == 2000, buildup_total]
    if (is.na(total_2005)) total_2005 <- dhs_estimates[year == 2005, get(dhs_col)]

    if (!is.na(total_2000) && !is.na(total_2005)) {
      for (yr in 2001:2004) {
        weight <- (yr - 2000) / (2005 - 2000)
        interpolated <- total_2000 + weight * (total_2005 - total_2000)
        o_totals[year == yr, dhs_total := interpolated]
      }
    }
  }

  # Calculate adjustment ratios
  o_totals[, ratio := dhs_total / buildup_total]
  o_totals[is.na(ratio) | is.infinite(ratio), ratio := 1]
  o_totals[ratio < 0, ratio := 1]  # Handle negative buildup (shouldn't happen)

  # Apply ratios to adjust populations
  result <- merge(result, o_totals[, .(year, ratio)], by = "year", all.x = TRUE)
  result[is.na(ratio), ratio := 1]

  # Only adjust years 2000+
  result[year >= 2000, population := population * ratio]

  # Log adjustment summary
  adjusted_years <- sum(result$ratio != 1) / (length(ages) * 2)  # approx unique years adjusted
  cli::cli_alert_info("Adjusted {round(adjusted_years)} years to DHS estimates")

  # Verify totals for key years
  adjusted_totals <- result[, .(adjusted_total = sum(population)), by = year]
  check_years <- c(2005, 2010, 2020, 2022)
  check_years <- check_years[check_years %in% adjusted_totals$year & check_years %in% dhs_estimates$year]

  for (yr in check_years) {
    adj_total <- adjusted_totals[year == yr, adjusted_total]
    dhs_total <- dhs_estimates[year == yr, get(dhs_col)]
    diff_pct <- (adj_total - dhs_total) / dhs_total * 100
    cli::cli_alert_info("  {yr}: Adjusted={format(round(adj_total), big.mark=',')}, DHS={format(round(dhs_total), big.mark=',')}, Diff={round(diff_pct, 1)}%")
  }

  result[, ratio := NULL]
  result
}

# =============================================================================
# VALIDATION
# =============================================================================

#' Validate O Population Against DHS Estimates
#'
#' @description
#' Compares calculated O population against DHS unauthorized estimates.
#'
#' @param o_pop data.table: Calculated O population
#' @param tolerance Numeric: Acceptable relative difference (default: 0.05 = 5%)
#'
#' @return data.table with validation results
#'
#' @export
validate_o_population <- function(o_pop, tolerance = 0.05) {
  cli::cli_h1("Validating O Population Against DHS Estimates")

  # Load DHS estimates
  dhs <- tryCatch({
    fetch_dhs_unauthorized_estimates(years = 2000:2022)
  }, error = function(e) {
    cli::cli_alert_warning("Could not load DHS estimates: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(dhs)) {
    return(NULL)
  }

  # Calculate totals by year
  o_totals <- o_pop[, .(calculated = sum(population)), by = year]
  o_totals <- merge(
    o_totals,
    dhs[, .(year, dhs = unauthorized_population)],
    by = "year"
  )

  o_totals[, diff_pct := (calculated - dhs) / dhs * 100]
  o_totals[, pass := abs(diff_pct) <= tolerance * 100]

  # Summary
  n_pass <- sum(o_totals$pass)
  n_total <- nrow(o_totals)

  cli::cli_alert_info("Validation: {n_pass}/{n_total} years within {tolerance * 100}% tolerance")

  # Show worst deviations
  worst <- o_totals[order(-abs(diff_pct))][1:5]
  cli::cli_alert("Largest deviations:")
  for (i in seq_len(min(5, nrow(worst)))) {
    cli::cli_alert("  {worst$year[i]}: {round(worst$diff_pct[i], 2)}%")
  }

  o_totals
}

# =============================================================================
# SUMMARY FUNCTIONS
# =============================================================================

#' Summarize O Population by Year
#'
#' @export
summarize_o_population <- function(o_pop) {
  cli::cli_h2("Temporary/Unlawfully Present Population Summary")

  # Total by year
  totals <- o_pop[, .(total = sum(population)), by = year]

  # Sample years
  sample_years <- c(1940, 1960, 1980, 1990, 2000, 2007, 2010, 2020, 2022)
  sample_years <- sample_years[sample_years %in% totals$year]

  cli::cli_alert("O Population totals:")
  for (yr in sample_years) {
    total <- totals[year == yr, total]
    cli::cli_alert("  {yr}: {format(round(total), big.mark = ',')}")
  }

  # By sex for most recent year
  latest_year <- max(o_pop$year)
  by_sex <- o_pop[year == latest_year, .(total = sum(population)), by = sex]

  cli::cli_alert("By sex ({latest_year}):")
  for (i in seq_len(nrow(by_sex))) {
    cli::cli_alert("  {by_sex$sex[i]}: {format(round(by_sex$total[i]), big.mark = ',')}")
  }

  # Age distribution for most recent year
  by_age_group <- o_pop[year == latest_year, .(
    age_group = data.table::fcase(
      age < 18, "Under 18",
      age >= 18 & age < 25, "18-24",
      age >= 25 & age < 45, "25-44",
      age >= 45 & age < 55, "45-54",
      age >= 55 & age < 65, "55-64",
      age >= 65, "65+"
    ),
    population = population
  )][, .(total = sum(population)), by = age_group]

  total_latest <- sum(by_age_group$total)
  by_age_group[, pct := round(total / total_latest * 100, 1)]

  cli::cli_alert("Age distribution ({latest_year}):")
  for (i in seq_len(nrow(by_age_group))) {
    cli::cli_alert("  {by_age_group$age_group[i]}: {format(round(by_age_group$total[i]), big.mark = ',')} ({by_age_group$pct[i]}%)")
  }

  invisible(totals)
}

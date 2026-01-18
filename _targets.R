# ARTEMIS: OASDI Projection Model
# Targets pipeline definition
# =============================================================================

library(targets)
library(tarchetypes)

# Source all R files in R/ subdirectories
tar_source()
tar_source("R/utils")
tar_source("R/data_acquisition")
tar_source("R/demography")
tar_source("R/validation")

# Set global options for targets
tar_option_set(
  packages = c(
    "data.table",
    "dplyr",
    "tidyr",
    "httr2",
    "jsonlite",
    "yaml",
    "readxl",
    "readr",
    "haven",
    "here",
    "glue",
    "cli",
    "checkmate"
  ),
  format = "rds",  # Standard R serialization format
  error = "continue",
  memory = "transient",
  garbage_collection = TRUE
)

# =============================================================================
# PIPELINE DEFINITION
# =============================================================================

list(
  # ===========================================================================
  # CONFIGURATION TARGETS
  # ===========================================================================

  tar_target(
    config_assumptions,
    load_assumptions(here::here("config/assumptions/tr2025.yaml")),
    cue = tar_cue(mode = "thorough")
  ),

  tar_target(
    config_api,
    load_api_config(here::here("config/api_endpoints.yaml")),
    cue = tar_cue(mode = "thorough")
  ),

  # ===========================================================================
  # DATA ACQUISITION TARGETS - FERTILITY
  # ===========================================================================

  # Birth data from NCHS via NBER Stata files (1980-2024)
  # Downloads and caches birth counts by single year of mother's age
  tar_target(
    nchs_births_raw,
    {
      years <- config_assumptions$data_sources$historical_birth_data$start_year:2024
      all_births <- data.table::rbindlist(lapply(years, function(yr) {
        dt <- fetch_nchs_births_by_age(yr)
        dt[, year := yr]
        dt
      }))
      all_births
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Census Bureau female population estimates (1980-2024)
  # Uses file downloads for 1980-1989, API for 1990-2024
  tar_target(
    census_female_pop,
    fetch_census_population_all(
      years = config_assumptions$data_sources$population_estimates$start_year:
              config_assumptions$data_sources$population_estimates$end_year,
      ages = 10:54,  # Wider range to capture all fertility ages after collapse
      sex = "female"
    ),
    cue = tar_cue(mode = "thorough")
  ),

  # Historical birth rates (1917-1979) - DEFERRED
  # Only needed for historical output series, not projection methodology
  # tar_target(
  #   historical_birth_rates,
  #   load_historical_birth_rates(
  #     start_year = config_assumptions$data_sources$historical_rate_data$start_year,
  #     end_year = config_assumptions$data_sources$historical_rate_data$end_year
  #   )
  # ),

  # Provisional birth data - DEFERRED
  # Not needed since we have final 2024 data from NBER
  # May be needed in future when running projections before NBER publishes latest year
  # tar_target(
  #   provisional_births_2024,
  #   fetch_provisional_births(year = 2024, config = config_api)
  # ),

  # ===========================================================================
  # FERTILITY SUBPROCESS TARGETS
  # ===========================================================================

  # Step 1: Calculate historical birth rates (1980-2024)
  tar_target(
    fertility_rates_historical,
    calculate_historical_birth_rates(
      births = nchs_births_raw,
      population = census_female_pop,
      min_age = config_assumptions$fertility$min_fertility_age,
      max_age = config_assumptions$fertility$max_fertility_age
    )
  ),

  # Step 2: Calculate age-to-30 ratios
  tar_target(
    fertility_age30_ratios,
    calculate_age30_ratios(fertility_rates_historical)
  ),

  # Step 3: Calculate trend factors (excluding 1997 due to NCHS method change)
  tar_target(
    fertility_trend_factors,
    calculate_trend_factors(
      ratios = fertility_age30_ratios,
      exclude_years = config_assumptions$fertility$exclude_years
    )
  ),

  # Step 4: Calculate ultimate years by age
  tar_target(
    fertility_ultimate_years,
    calculate_ultimate_years(
      min_age = config_assumptions$fertility$min_fertility_age,
      max_age = config_assumptions$fertility$max_fertility_age,
      base_year = config_assumptions$fertility$projection_start_year,
      end_year = config_assumptions$fertility$ultimate_year
    )
  ),

  # Step 5: Calculate interpolation weights for projection years
  tar_target(
    fertility_weights,
    calculate_interpolation_weights(
      years = config_assumptions$metadata$projection_period$start_year:
              config_assumptions$metadata$projection_period$end_year,
      base_year = config_assumptions$fertility$rate_base_year,
      ultimate_year = config_assumptions$fertility$age30_ultimate_year,
      exponent = config_assumptions$fertility$weight_exponent
    )
  ),

  # Step 6: Solve for ultimate age-30 rate to achieve target CTFR
  tar_target(
    fertility_ultimate_age30,
    solve_ultimate_age30_rate(
      target_ctfr = config_assumptions$fertility$ultimate_ctfr,
      base_age30_rate = fertility_rates_historical[year == config_assumptions$fertility$rate_base_year & age == 30, birth_rate],
      base_ratios = fertility_age30_ratios[year == config_assumptions$fertility$rate_base_year],
      trend_factors = fertility_trend_factors,
      ultimate_years = fertility_ultimate_years,
      base_year = config_assumptions$fertility$rate_base_year
    )
  ),

  # Step 7: Project age-30 rates from base to ultimate
  tar_target(
    fertility_age30_projected,
    project_age30_rates(
      years = config_assumptions$metadata$projection_period$start_year:
              config_assumptions$metadata$projection_period$end_year,
      base_rate = fertility_rates_historical[year == config_assumptions$fertility$rate_base_year & age == 30, birth_rate],
      ultimate_rate = fertility_ultimate_age30,
      weights = fertility_weights
    )
  ),

  # Step 8: Project all age-specific rates
  tar_target(
    fertility_rates_projected,
    project_birth_rates(
      years = config_assumptions$metadata$projection_period$start_year:
              config_assumptions$metadata$projection_period$end_year,
      age30_rates = fertility_age30_projected,
      base_ratios = fertility_age30_ratios[year == config_assumptions$fertility$rate_base_year],
      trend_factors = fertility_trend_factors,
      ultimate_years = fertility_ultimate_years
    )
  ),

  # Combined historical and projected rates (1980-2099)
  tar_target(
    fertility_rates_complete,
    rbind(
      fertility_rates_historical[, .(year, age, birth_rate)],
      fertility_rates_projected
    )
  ),

  # Step 9-10: Calculate TFR (period) and CTFR (cohort) series
  tar_target(
    fertility_totals,
    calculate_fertility_totals(fertility_rates_complete)
  ),

  # ===========================================================================
  # VALIDATION TARGETS
  # ===========================================================================

  # Load official TR tables for comparison
  # tar_target(
  #   tr_tables_official,
  #   load_tr_tables(tr_version = "TR2025")
  # ),

  # Validate fertility outputs
  # tar_target(
  #   fertility_validation,
  #   validate_fertility_outputs(
  #     rates = fertility_rates_complete,
  #     totals = fertility_totals,
  #     official_data = tr_tables_official,
  #     tolerance = 0.01
  #   )
  # )

  # ===========================================================================
  # DATA ACQUISITION TARGETS - MORTALITY
  # ===========================================================================

  # NCHS Deaths data (1968-2023) - downloads and caches by age, sex, cause
  tar_target(
    nchs_deaths_raw,
    {
      years <- 1968:2023
      all_deaths <- data.table::rbindlist(lapply(years, function(yr) {
        cache_file <- sprintf("data/cache/nchs_deaths/deaths_by_age_sex_cause_%d.rds", yr)
        if (file.exists(cache_file)) {
          dt <- readRDS(cache_file)
          dt[, year := yr]
          dt
        } else {
          dt <- fetch_nchs_deaths_by_age(yr)
          dt[, year := yr]
          dt
        }
      }))
      all_deaths
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Census population for mortality calculations (both sexes, ages 0-100)
  tar_target(
    census_population_both,
    {
      cache_file <- "data/cache/census/population_by_age_sex_1980_2023.rds"
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_census_population_all(
          years = 1980:2023,
          ages = 0:100,
          sex = "both"
        )
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Total births by year for q0 calculation (legacy - kept for compatibility)
  tar_target(
    nchs_births_total,
    {
      years <- 1968:2024
      data.table::rbindlist(lapply(years, function(yr) {
        births_file <- sprintf("data/raw/nchs/births_by_month_%d.rds", yr)
        if (file.exists(births_file)) {
          b <- readRDS(births_file)
          data.table::data.table(year = yr, births = sum(b$births))
        } else {
          # Fall back to births_by_age if available
          age_file <- sprintf("data/raw/nchs/births_by_age_%d.rds", yr)
          if (file.exists(age_file)) {
            b <- readRDS(age_file)
            data.table::data.table(year = yr, births = sum(b$births))
          } else {
            NULL
          }
        }
      }))
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Sex-specific births by year for accurate q0 calculation
  tar_target(
    nchs_births_by_sex,
    {
      years <- 1968:2024
      data.table::rbindlist(lapply(years, function(yr) {
        births_file <- sprintf("data/raw/nchs/births_by_month_sex_%d.rds", yr)
        if (file.exists(births_file)) {
          b <- readRDS(births_file)
          # Aggregate monthly data to annual by sex
          b[, .(births = sum(births)), by = .(year, sex)]
        } else {
          NULL
        }
      }))
    },
    cue = tar_cue(mode = "thorough")
  ),

  # ===========================================================================
  # MORTALITY SUBPROCESS TARGETS
  # ===========================================================================

  # Step 1: Calculate historical central death rates (mx) by age, sex, cause
  tar_target(
    mortality_mx_historical,
    calculate_central_death_rates(
      deaths = nchs_deaths_raw,
      population = census_population_both,
      by_cause = TRUE
    )
  ),

  # Step 2: Calculate total mx (sum across causes)
  tar_target(
    mortality_mx_total,
    calculate_total_death_rates(mortality_mx_historical)
  ),

  # Step 3: Calculate historical AAx (annual percentage reduction)
  # Uses weighted regression over 2008-2019 period (weights are hardcoded per SSA spec)
  tar_target(
    mortality_aax_historical,
    calculate_annual_reduction_rates(
      mx = mortality_mx_historical,
      start_year = config_assumptions$mortality$regression_start_year,
      end_year = config_assumptions$mortality$regression_end_year,
      by_cause = TRUE
    )
  ),

  # Step 4: Get starting mx values (fitted values from regression at base year)
  tar_target(
    mortality_mx_starting,
    calculate_starting_mx(
      aax_results = mortality_aax_historical,
      base_year = config_assumptions$mortality$base_year
    )
  ),

  # Step 5: Run full mortality projection
  tar_target(
    mortality_mx_projected,
    run_mortality_projection(
      deaths = nchs_deaths_raw,
      population = census_population_both,
      base_year = config_assumptions$mortality$base_year,
      projection_end = config_assumptions$metadata$projection_period$end_year,
      by_cause = FALSE
    )
  ),

  # Step 6: Calculate qx from mx (including q0 using births)
  # Note: Limited to 1980+ because Census population data starts in 1980
  tar_target(
    mortality_qx_unadjusted,
    {
      # Get years with population data (1980+)
      pop_years <- unique(census_population_both$year)
      mx_filtered <- mortality_mx_total[year %in% pop_years]

      # Calculate qx for ages 1+ from mx
      qx_from_mx <- convert_mx_to_qx(mx_filtered, max_age = 119)

      # Calculate q0 using deaths/births with actual sex-specific births
      infant_deaths <- nchs_deaths_raw[age == 0 & year %in% pop_years,
                                       .(deaths = sum(deaths)), by = .(year, sex)]

      # Use actual sex-specific births data
      q0 <- merge(infant_deaths, nchs_births_by_sex, by = c("year", "sex"), all.x = TRUE)
      q0 <- q0[!is.na(births)]  # Only years with births data
      q0[, qx := deaths / births]
      q0 <- q0[, .(year, age = 0L, sex, qx)]

      # Combine q0 with qx for ages 1+
      qx_ages1plus <- qx_from_mx[age >= 1, .(year, age, sex, qx)]
      result <- data.table::rbindlist(list(q0, qx_ages1plus), use.names = TRUE)
      data.table::setorder(result, year, sex, age)
      result
    }
  ),

  # Step 6b: Adjust qx for ages 85+ using HMD calibration
  # HMD provides well-researched mortality patterns at oldest ages
  tar_target(
    mortality_qx_historical,
    adjust_qx_with_hmd(
      qx = mortality_qx_unadjusted,
      transition_age = 85,
      max_age = 119
    )
  ),

  # Step 7: Calculate life tables from qx
  tar_target(
    mortality_life_tables,
    calculate_life_table(
      qx = mortality_qx_historical,
      radix = 100000,
      max_age = 119
    )
  ),

  # Step 8: Calculate life expectancy at key ages (historical)
  tar_target(
    mortality_life_expectancy_hist,
    calculate_life_expectancy(
      life_table = mortality_life_tables,
      at_ages = c(0, 65)
    )
  ),

  # ===========================================================================
  # PROJECTED MORTALITY TARGETS
  # ===========================================================================

  # Step 9: Extract and adjust projected qx (2024-2099)
  # The projection already has qx, but we need to apply HMD calibration for ages 85+
  tar_target(
    mortality_qx_projected,
    {
      # Get projected qx from the projection output
      qx_proj <- mortality_mx_projected$projected_qx

      # Filter to projection years only (2024+, since 2020-2023 are in historical)
      qx_proj <- qx_proj[year >= 2024]

      # Apply HMD calibration for ages 85+
      adjust_qx_with_hmd(
        qx = qx_proj,
        transition_age = 85,
        max_age = 119
      )
    }
  ),

  # Step 10: Calculate projected life tables
  tar_target(
    mortality_life_tables_projected,
    calculate_life_table(
      qx = mortality_qx_projected,
      radix = 100000,
      max_age = 119
    )
  ),

  # Step 11: Calculate projected life expectancy
  tar_target(
    mortality_life_expectancy_proj,
    calculate_life_expectancy(
      life_table = mortality_life_tables_projected,
      at_ages = c(0, 65)
    )
  ),

  # Step 12: Combine historical and projected life expectancy
  tar_target(
    mortality_life_expectancy,
    {
      combined <- data.table::rbindlist(
        list(mortality_life_expectancy_hist, mortality_life_expectancy_proj),
        use.names = TRUE
      )
      data.table::setorder(combined, year, sex, age)
      combined
    }
  ),

  # ===========================================================================
  # MORTALITY VALIDATION TARGETS
  # ===========================================================================

  # Load TR2025 historical qx for validation
  tar_target(
    tr2025_qx_historical,
    load_tr2025_death_probs_hist(sex = "both")
  ),

  # Load TR2025 life tables for validation
  tar_target(
    tr2025_life_tables,
    load_tr2025_life_tables_hist(sex = "both")
  ),

  # Validate qx against TR2025
  tar_target(
    mortality_qx_validation,
    validate_qx_against_tr2025(
      qx_calculated = mortality_qx_historical,
      years = 2010:2019,
      tolerance = 0.05
    )
  ),

  # Validate life expectancy against TR2025
  tar_target(
    mortality_ex_validation,
    validate_life_expectancy_against_tr2025(
      life_table = mortality_life_tables,
      at_ages = c(0, 65),
      tolerance = 0.5  # 0.5 year tolerance
    )
  ),

  # ===========================================================================
  # DATA ACQUISITION TARGETS - IMMIGRATION
  # ===========================================================================

  # DHS LPR immigration data (2006-2023)
  # Age-sex distributions for new arrivals and adjustments of status
  tar_target(
    dhs_lpr_data,
    {
      cache_file <- "data/cache/dhs_immigration/dhs_lpr_all_years.rds"
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        cli::cli_abort("DHS data not cached - run fetch_dhs_lpr_data_multi() first")
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # CBO migration data (for emigration distribution)
  tar_target(
    cbo_migration_data,
    load_cbo_migration(
      file_path = "data/raw/cbo/grossMigration_byYearAgeSexStatusFlow.csv"
    ),
    cue = tar_cue(mode = "thorough")
  ),

  # ===========================================================================
  # LPR IMMIGRATION SUBPROCESS TARGETS (Hybrid B+C Approach)
  # ===========================================================================
  # Uses CBO data for age-sex distributions, DHS for NEW/AOS ratio, TR2025 for totals
  # Produces all 5 required outputs: L (LPR), NEW, AOS, E (Emigration), NL (Net LPR)

  # Step 1: Calculate LPR immigration distribution from CBO data
  tar_target(
    lpr_distribution,
    calculate_lpr_distribution_cbo(
      cbo_data = cbo_migration_data,
      reference_years = config_assumptions$immigration$lpr$distribution_years
    )
  ),

  # Step 2: Calculate emigration distribution from CBO data
  tar_target(
    emigration_distribution,
    calculate_emigration_distribution_cbo(
      cbo_data = cbo_migration_data,
      reference_years = config_assumptions$immigration$emigration$distribution_years
    )
  ),

  # Step 3: Calculate NEW/AOS ratio from DHS data
  tar_target(
    new_aos_ratio,
    calculate_new_aos_ratio(
      dhs_data = load_dhs_lpr_data(),
      reference_years = config_assumptions$immigration$lpr$new_aos_ratio_years
    )
  ),

  # Step 4: Get TR2025 LPR immigration assumptions
  tar_target(
    lpr_assumptions,
    get_tr2025_lpr_assumptions(
      years = config_assumptions$metadata$projection_period$start_year:
              config_assumptions$metadata$projection_period$end_year
    )
  ),

  # Step 5: Project total LPR immigration (L) by age and sex
  tar_target(
    lpr_immigration_projected,
    project_lpr_immigration(
      assumptions = lpr_assumptions,
      distribution = lpr_distribution
    )
  ),

  # Step 6: Split LPR into NEW and AOS
  # Method controlled by config: "assumption" (default, uses TR2025 V.A2 values)
  # or "ratio" (uses historical DHS ratio)
  tar_target(
    lpr_new_aos_split,
    {
      method <- config_assumptions$immigration$lpr$new_aos_split_method
      if (is.null(method)) method <- "assumption"
      if (method == "assumption") {
        split_lpr_new_aos(
          lpr_immigration = lpr_immigration_projected,
          assumptions = lpr_assumptions,
          method = "assumption"
        )
      } else {
        split_lpr_new_aos(
          lpr_immigration = lpr_immigration_projected,
          new_ratio = new_aos_ratio$new_ratio,
          method = "ratio"
        )
      }
    }
  ),

  # Extract NEW arrivals
  tar_target(
    new_arrivals_projected,
    lpr_new_aos_split$new_arrivals
  ),

  # Extract AOS
  tar_target(
    aos_projected,
    lpr_new_aos_split$aos
  ),

  # Step 7: Project legal emigration (E) by age and sex
  tar_target(
    legal_emigration_projected,
    project_legal_emigration(
      assumptions = lpr_assumptions,
      distribution = emigration_distribution
    )
  ),

  # Step 8: Calculate net LPR immigration (NL = L - E)
  tar_target(
    net_lpr_immigration,
    calculate_net_lpr(
      lpr_immigration = lpr_immigration_projected,
      emigration = legal_emigration_projected
    )
  ),

  # ===========================================================================
  # IMMIGRATION VALIDATION TARGETS
  # ===========================================================================

  # Comprehensive validation of all 5 LPR outputs
  tar_target(
    lpr_immigration_validation,
    {
      # Build projection result list for validation
      projection_result <- list(
        lpr_immigration = lpr_immigration_projected,
        new_arrivals = new_arrivals_projected,
        aos = aos_projected,
        emigration = legal_emigration_projected,
        net_lpr = net_lpr_immigration,
        distributions = list(
          lpr = lpr_distribution,
          emigration = emigration_distribution
        ),
        new_aos_ratio = new_aos_ratio,
        assumptions = lpr_assumptions
      )
      validate_lpr_outputs(projection_result, tolerance = 0.001)
    }
  ),

  # ===========================================================================
  # HISTORICAL POPULATION SUBPROCESS TARGETS (Phase 4)
  # ===========================================================================
  # Implements Equations 1.4.1 - 1.4.4 from TR2025 Documentation
  # Produces: P_x_s, P_x_s_m, O_x_s, C_x_s_m

  # Step 1: Calculate Historical Population by Age and Sex (Eq 1.4.1)
  # P^z_{x,s} = USAF + UC + TERR + FED + DEP + BEN + OTH
  tar_target(
    historical_population,
    calculate_historical_population(
      start_year = 1940,
      end_year = 2022,
      ages = 0:100,
      config = config_assumptions,
      use_cache = TRUE
    )
  ),

  # Step 2: Calculate Historical Population by Marital Status (Eq 1.4.2)
  # P^z_{x,s,m} = P^z_{x,s} × MaritalPct^z_{x,s,m}
  tar_target(
    historical_population_marital,
    calculate_historical_population_marital(
      total_pop = historical_population,
      start_year = 1940,
      end_year = 2022,
      ages = 14:100,
      use_cache = TRUE,
      include_same_sex = TRUE
    )
  ),

  # Step 3: Calculate Temporary/Unlawfully Present Population (Eq 1.4.3)
  # O^z_{x,s} from TR2025 V.A2 flows
  tar_target(
    historical_temp_unlawful,
    calculate_historical_temp_unlawful(
      start_year = 1940,
      end_year = 2022,
      ages = 0:99,
      use_cache = TRUE
    )
  ),

  # Step 4: Calculate Civilian Noninstitutionalized Population (Eq 1.4.4)
  # C^z_{x,s,m} = CivNonInst^z_{x,s} × MaritalPct^z_{x,s,m}
  tar_target(
    historical_civilian_noninst,
    calculate_historical_civilian_noninst(
      start_year = 2010,
      end_year = 2022,
      ages = 0:99,
      include_orientation = TRUE,
      use_cache = TRUE
    )
  ),

  # ===========================================================================
  # HISTORICAL POPULATION VALIDATION TARGETS
  # ===========================================================================

  # Load TR2025 December 31 population for validation
  tar_target(
    tr2025_population_dec,
    {
      file_path <- here::here("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")
      if (file.exists(file_path)) {
        data.table::fread(file_path)
      } else {
        cli::cli_warn("TR2025 population file not found: {file_path}")
        NULL
      }
    }
  ),

  # Validate historical population against TR2025
  tar_target(
    historical_population_validation,
    {
      if (is.null(tr2025_population_dec)) {
        cli::cli_warn("Skipping validation - TR2025 data not available")
        return(list(validated = FALSE, reason = "TR2025 data not available"))
      }

      # Calculate totals by year from our data
      calc_totals <- historical_population[, .(calculated = sum(population)), by = year]

      # Calculate totals from TR2025
      tr_totals <- tr2025_population_dec[, .(tr2025 = sum(Total)), by = Year]
      data.table::setnames(tr_totals, "Year", "year")

      # Merge and compare
      comparison <- merge(calc_totals, tr_totals, by = "year", all.x = TRUE)
      comparison[, diff_pct := (calculated - tr2025) / tr2025 * 100]

      # Summary statistics
      valid_rows <- comparison[!is.na(tr2025)]
      list(
        validated = TRUE,
        mean_abs_error = mean(abs(valid_rows$diff_pct)),
        max_abs_error = max(abs(valid_rows$diff_pct)),
        within_1pct = sum(abs(valid_rows$diff_pct) < 1),
        within_2pct = sum(abs(valid_rows$diff_pct) < 2),
        total_years = nrow(valid_rows),
        comparison = comparison
      )
    }
  ),

  # ===========================================================================
  # DATA ACQUISITION TARGETS - O IMMIGRATION (Phase 5)
  # ===========================================================================
  # Data for temporary/unlawfully present immigration projection

  # DHS Nonimmigrant stock data (3 reference dates)
  tar_target(
    dhs_nonimmigrant_stock,
    {
      cache_file <- "data/cache/dhs/nonimmigrant_stock_age_sex.rds"
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_dhs_nonimmigrant_stock()
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # DHS Beginning-of-year nonimmigrant totals
  tar_target(
    dhs_boy_nonimmigrants,
    {
      cache_file <- "data/cache/dhs/boy_nonimmigrants.rds"
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_dhs_boy_nonimmigrants()
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # DHS DACA grants and stock
  tar_target(
    dhs_daca_data,
    {
      grants_file <- "data/cache/dhs/daca_grants.rds"
      stock_file <- "data/cache/dhs/daca_stock.rds"
      if (file.exists(grants_file) && file.exists(stock_file)) {
        list(
          grants = readRDS(grants_file),
          stock = readRDS(stock_file)
        )
      } else {
        list(
          grants = fetch_dhs_daca_grants(),
          stock = fetch_dhs_daca_stock()
        )
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # ACS Foreign-born new arrivals (2006-2023)
  # Load cached foreign-born flows data
  tar_target(
    acs_foreign_born_flows,
    {
      cache_dir <- "data/cache/acs_pums"
      years <- c(2006:2019, 2021:2023)  # Skip 2020 (COVID data issues)

      data.table::rbindlist(lapply(years, function(yr) {
        cache_file <- file.path(cache_dir, sprintf("foreign_born_flows_%d.rds", yr))
        if (file.exists(cache_file)) {
          readRDS(cache_file)
        } else {
          NULL
        }
      }), fill = TRUE)
    },
    cue = tar_cue(mode = "thorough")
  ),

  # Calculate new arrivals from foreign-born flows
  tar_target(
    acs_foreign_born_arrivals,
    {
      arrivals <- calculate_acs_new_arrivals(
        foreign_born_flows = acs_foreign_born_flows,
        entry_window = 1
      )
      # Aggregate to remove is_cuban dimension (sum Cuban and non-Cuban)
      arrivals[, .(population = sum(population)), by = .(year, age, sex)]
    }
  ),

  # ACS undercount factors for O immigration
  tar_target(
    acs_undercount_factors,
    calculate_acs_undercount_factors(
      years = 2017:2019,
      use_fallback = TRUE  # Use DHS hardcoded factors
    )
  ),

  # ===========================================================================
  # O IMMIGRATION SUBPROCESS TARGETS (Phase 5)
  # ===========================================================================
  # Implements Equations 1.5.1 - 1.5.4 from TR2025 Documentation
  # Produces: OI (O Immigration), OE (O Emigration), NO (Net O), OP (O Population)

  # Step 1: Calculate O immigration distribution (ODIST)
  # From ACS new arrivals minus LPR NEW arrivals
  tar_target(
    o_immigration_odist,
    {
      # Calculate ODIST from ACS and LPR data
      calculate_odist(
        acs_arrivals = acs_foreign_born_arrivals,
        lpr_new = new_arrivals_projected,
        reference_years = 2015:2019,
        dhs_nonimmigrant = dhs_nonimmigrant_stock
      )
    }
  ),

  # Step 2: Get TR2025 O immigration assumptions
  tar_target(
    o_immigration_assumptions,
    get_tr2025_o_assumptions(
      years = config_assumptions$metadata$projection_period$start_year:
              config_assumptions$metadata$projection_period$end_year
    )
  ),

  # Step 3: Project O immigration (Equation 1.5.1)
  # OI = TO × ODIST
  tar_target(
    o_immigration_projected,
    project_o_immigration(
      assumptions = o_immigration_assumptions,
      odist = o_immigration_odist,
      projection_years = config_assumptions$metadata$projection_period$start_year:
                         config_assumptions$metadata$projection_period$end_year
    )
  ),

  # Step 4: Calculate departure rates for O emigration
  tar_target(
    o_departure_rates,
    calculate_simplified_departure_rates(
      config = config_assumptions
    )
  ),

  # Step 5: Run full O immigration projection (Equations 1.5.1-1.5.4)
  # This orchestrates OI, OE, NO, and OP calculations
  tar_target(
    o_immigration_projection,
    run_full_o_projection(
      historical_o_pop = historical_temp_unlawful,
      acs_new_arrivals = acs_foreign_born_arrivals,
      lpr_new_arrivals = new_arrivals_projected,
      lpr_aos = aos_projected,
      mortality_qx = mortality_qx_historical,
      undercount_factors = acs_undercount_factors,
      projection_years = config_assumptions$metadata$projection_period$start_year:
                         config_assumptions$metadata$projection_period$end_year,
      config = config_assumptions
    )
  ),

  # Extract individual outputs from projection
  tar_target(
    o_immigration_oi,
    o_immigration_projection$o_immigration
  ),

  tar_target(
    o_emigration_oe,
    o_immigration_projection$o_emigration
  ),

  tar_target(
    net_o_immigration,
    o_immigration_projection$net_o
  ),

  tar_target(
    o_population_stock,
    o_immigration_projection$o_population
  ),

  # ===========================================================================
  # DACA PROJECTION TARGETS (Phase 5F)
  # ===========================================================================

  # Run DACA population projection
  tar_target(
    daca_projection,
    run_daca_projection(
      acs_2012_data = NULL,  # Use internal estimates
      dhs_stock = dhs_daca_data$stock,
      dhs_grants = dhs_daca_data$grants,
      projection_years = config_assumptions$metadata$projection_period$start_year:
                         config_assumptions$metadata$projection_period$end_year,
      config = config_assumptions
    )
  ),

  # Extract DACA population
  tar_target(
    daca_population_projected,
    daca_projection$daca_population
  ),

  # ===========================================================================
  # O IMMIGRATION VALIDATION TARGETS (Phase 5G)
  # ===========================================================================

  # Comprehensive validation of O immigration outputs
  tar_target(
    o_immigration_validation,
    validate_o_immigration_comprehensive(
      projection = o_immigration_projection,
      daca_projection = daca_projection,
      tolerance_strict = 0.001,
      tolerance_relaxed = 0.15
    )
  ),

  # ===========================================================================
  # DATA ACQUISITION TARGETS - MARRIAGE (Phase 6)
  # ===========================================================================

  # NCHS MRA marriages 1978-1988 (for base MarGrid)
  tar_target(
    nchs_mra_marriages_1978_1988,
    {
      cache_file <- here::here("data/cache/nber_marriage/nchs_mra_marriages_1978_1988.rds")
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_nchs_mra_marriages_1978_1988()
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # CPS unmarried population (1962-1995)
  tar_target(
    cps_unmarried_population,
    {
      cache_file <- here::here("data/cache/ipums_cps/cps_unmarried_1957_1995.rds")
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        cli::cli_abort("CPS unmarried data not cached - run fetch_cps_unmarried_population() first")
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # NCHS MRA subset marriages 1989-1995
  tar_target(
    nchs_mra_subset_1989_1995,
    {
      cache_file <- here::here("data/cache/nber_marriage/nchs_mra_marriages_1989_1995.rds")
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_nchs_mra_marriages_1989_1995()
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # NCHS U.S. total marriages 1989-2022
  tar_target(
    nchs_us_total_marriages,
    fetch_nchs_us_total_marriages(years = 1989:2022)
  ),

  # ACS new marriage grids 2007-2022 (aligned to MarGrid dimensions)
  tar_target(
    acs_marriage_grids,
    {
      cache_dir <- here::here("data/cache/acs_marriage")
      acs_files <- list.files(cache_dir, pattern = "^new_marriages_20[0-9]{2}\\.rds$", full.names = TRUE)

      if (length(acs_files) == 0) {
        cli::cli_abort("ACS marriage grids not cached - run fetch_acs_new_marriages() first")
      }

      aligned_grids <- list()
      for (f in acs_files) {
        yr <- as.integer(gsub(".*new_marriages_([0-9]{4})\\.rds", "\\1", f))
        data <- readRDS(f)
        acs_grid <- data$grid
        acs_ages <- as.integer(rownames(acs_grid))

        # Align to MarGrid dimensions (87×87, ages 14-100)
        aligned <- matrix(0, nrow = 87, ncol = 87,
                          dimnames = list(14:100, 14:100))

        for (i in seq_along(acs_ages)) {
          h_age <- acs_ages[i]
          if (h_age < 14 || h_age > 100) next
          h_idx <- h_age - 14 + 1

          for (j in seq_along(acs_ages)) {
            w_age <- acs_ages[j]
            if (w_age < 14 || w_age > 100) next
            w_idx <- w_age - 14 + 1
            aligned[h_idx, w_idx] <- acs_grid[i, j]
          }
        }
        aligned_grids[[as.character(yr)]] <- aligned
      }
      aligned_grids
    },
    cue = tar_cue(mode = "thorough")
  ),

  # 2010 standard population by age group
  tar_target(
    standard_pop_2010,
    {
      cache_file <- here::here("data/cache/acs_pums/marital_all_years.rds")
      if (file.exists(cache_file)) {
        get_2010_standard_population(cache_file = cache_file, by_age_group = TRUE)
      } else {
        cli::cli_abort("Standard population cache not found - run Phase 4 data acquisition first")
      }
    }
  ),

  # NCHS marriages by prior status 1978-1988
  tar_target(
    nchs_prior_status_1978_1988,
    {
      cache_file <- here::here("data/cache/nber_marriage/nchs_marriages_by_prior_status_1978_1988.rds")
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        fetch_nchs_marriages_by_prior_status_1978_1988()
      }
    },
    cue = tar_cue(mode = "thorough")
  ),

  # ACS same-sex marriage grids 2015-2022
  tar_target(
    acs_same_sex_grids,
    fetch_acs_same_sex_grids(years = 2015:2022),
    cue = tar_cue(mode = "thorough")
  ),

  # Calculate same-sex fraction from ACS data
  tar_target(
    same_sex_fraction,
    {
      ss_result <- calculate_same_sex_fraction(
        same_sex_grids = acs_same_sex_grids,
        opposite_sex_grids = acs_marriage_grids,
        years = 2015:2019
      )
      ss_result$overall_fraction
    }
  ),

  # ===========================================================================
  # MARRIAGE SUBPROCESS TARGETS (Phase 6)
  # ===========================================================================
  # Implements Equations 1.6.1 - 1.6.2 from TR2025 Documentation
  # Produces: m̂_{x,y}^z (age-specific rates), AMR^z (age-adjusted rate)

  # Run complete marriage projection
  tar_target(
    marriage_projection,
    run_marriage_projection(
      nchs_marriages_1978_1988 = nchs_mra_marriages_1978_1988,
      cps_unmarried = cps_unmarried_population,
      nchs_subset = nchs_mra_subset_1989_1995,
      acs_grids = acs_marriage_grids,
      nchs_us_totals = nchs_us_total_marriages,
      standard_pop_by_group = standard_pop_2010,
      prior_status_data = nchs_prior_status_1978_1988,
      same_sex_data = acs_same_sex_grids,
      same_sex_fraction = same_sex_fraction,
      ultimate_amr = config_assumptions$marriage$ultimate_amr,
      ultimate_year = config_assumptions$marriage$ultimate_year,
      end_year = config_assumptions$metadata$projection_period$end_year,
      include_same_sex = TRUE,
      include_prior_status = TRUE,
      force_recompute = FALSE
    )
  ),

  # Extract marriage rates (all years)
  tar_target(
    marriage_rates_all,
    marriage_projection$all_rates
  ),

  # Extract historical AMR
  tar_target(
    marriage_amr_historical,
    marriage_projection$amr_historical
  ),

  # Extract projected AMR
  tar_target(
    marriage_amr_projected,
    marriage_projection$amr_projected
  ),

  # Extract opposite-sex rates
  tar_target(
    marriage_rates_opposite_sex,
    marriage_projection$opposite_sex_rates
  ),

  # Extract same-sex rates
  tar_target(
    marriage_rates_same_sex,
    marriage_projection$same_sex_rates
  ),

  # Extract prior status differentials
  tar_target(
    marriage_status_differentials,
    marriage_projection$status_differentials
  ),

  # Extract base MarGrid
  tar_target(
    marriage_margrid,
    marriage_projection$margrid
  ),

  # ===========================================================================
  # MARRIAGE VALIDATION TARGETS (Phase 6H)
  # ===========================================================================

  # Comprehensive marriage projection validation
  tar_target(
    marriage_validation,
    validate_marriage_comprehensive(
      projection = marriage_projection,
      tolerance_amr = 0.001,
      tolerance_totals = 0.05
    )
  ),

  # Quick marriage validation (for faster iteration)
  tar_target(
    marriage_validation_quick,
    validate_marriage_quick(projection = marriage_projection)
  ),

  # ===========================================================================
  # DIVORCE SUBPROCESS TARGETS (Phase 7)
  # ===========================================================================
  # Implements Equations 1.7.1 - 1.7.2 from TR2025 Documentation
  # Produces: d̂_{x,y}^z (age-specific rates), ADR^z (age-adjusted rate)

  # Run complete divorce projection
  tar_target(
    divorce_projection,
    run_divorce_projection(
      cache_dir = here::here("data/cache"),
      ultimate_adr = config_assumptions$divorce$ultimate_adr,
      ultimate_year = config_assumptions$divorce$ultimate_year,
      end_year = config_assumptions$metadata$projection_period$end_year,
      force = FALSE
    )
  ),

  # Extract projected ADR series (2023-2099)
  tar_target(
    divorce_adr_projected,
    divorce_projection$projected_adr
  ),

  # Extract historical ADR series (1989-2022)
  tar_target(
    divorce_adr_historical,
    divorce_projection$historical$adr_series
  ),

  # Extract complete ADR series (historical + projected)
  tar_target(
    divorce_adr_complete,
    divorce_projection$complete_adr
  ),

  # Extract projected divorce rates (DivGrid by year)
  tar_target(
    divorce_rates_projected,
    divorce_projection$projected_rates
  ),

  # Extract base DivGrid (1979-1988 average)
  tar_target(
    divorce_divgrid_base,
    divorce_projection$historical$base_result$divgrid
  ),

  # Extract adjusted DivGrid (ACS-adjusted)
  tar_target(
    divorce_divgrid_adjusted,
    divorce_projection$historical$adjusted_result$adjusted_divgrid
  ),

  # Extract standard married population (July 1, 2010)
  tar_target(
    divorce_standard_pop,
    divorce_projection$standard_pop
  ),

  # ===========================================================================
  # DIVORCE VALIDATION TARGETS (Phase 7H)
  # ===========================================================================

  # Comprehensive divorce projection validation
  tar_target(
    divorce_validation,
    validate_divorce_comprehensive(
      projection = divorce_projection,
      tolerance_adr = 0.001
    )
  ),

  # Quick divorce validation (for faster iteration)
  tar_target(
    divorce_validation_quick,
    validate_divorce_quick(projection = divorce_projection)
  ),

  # ===========================================================================
  # PLACEHOLDER: Future process targets will be added here
  # ===========================================================================
  # - Projected population targets
  # - Economics process targets
  # - Beneficiaries process targets
  # - Trust fund operations targets

  NULL
)

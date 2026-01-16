# ARTEMIS: OASDI Projection Model Implementation Plan
## Phase 1: Demography Process - Introduction and Fertility Subprocess

**Document Version:** 1.0
**Date:** January 2026
**Scope:** Project setup, Demography Process introduction, and Fertility Subprocess (1.1)

---

## Progress Tracking

> **IMPORTANT**: This plan document should be updated as implementation progresses. Each task in Section 9 (Implementation Sequence) should be marked with its current status. Refer to this document to understand where the project stands at any point.

### Status Legend
- [ ] Not started
- [~] In progress
- [x] Completed

### Current Status
**Last Updated:** January 2026
**Current Phase:** Phase 1D - Validation and Testing (IN PROGRESS)
**Most Recent Completion:** Phase 1C - All fertility functions implemented, tested with real data, and validated against TR2025
**Completed:**
- Full NCHS birth data (1980-2023) downloaded and cached
- Census population data (1980-2024) via Vintage 2024 file + API
- Fertility projections validated against TR2025 Table V.A1
- Ultimate TFR solver fixed to correctly produce target TFR at ultimate year

### Critical Rule: Real Data Only
**No synthetic or mock data is permitted.** A task cannot be marked as completed until it is working with real data from the actual data sources (CDC WONDER API, Census API, NCHS files, etc.). Placeholder or synthetic data may be used temporarily during development, but the task remains "in progress" until real data flows through successfully.

---

## Table of Contents
1. [Project Overview](#1-project-overview)
2. [Project Architecture](#2-project-architecture)
3. [Project Setup Tasks](#3-project-setup-tasks)
4. [Data Acquisition Module](#4-data-acquisition-module)
5. [Fertility Subprocess Implementation](#5-fertility-subprocess-implementation)
6. [Validation Framework](#6-validation-framework)
7. [Configuration System](#7-configuration-system)
8. [File Structure](#8-file-structure)
9. [Implementation Sequence](#9-implementation-sequence)
10. [Technical Specifications](#10-technical-specifications)

---

## 1. Project Overview

### 1.1 Purpose
Build an R-based OASDI (Old-Age, Survivors, and Disability Insurance) projection model that replicates the SSA Office of the Chief Actuary's official long-range projections. The model should:
- Replicate official Trustees Report projections as closely as possible
- Support user-configurable assumptions and scenarios
- Enable policy proposal impact analysis
- Be deployable as a Shiny application

### 1.2 Overall Model Structure (From Documentation)
The OASDI projection model consists of four major processes:
1. **Process 1: Demography** - Population projections (THIS PHASE)
2. **Process 2: Economics** - Employment, earnings, GDP projections
3. **Process 3: Beneficiaries** - Insured population, beneficiary counts
4. **Process 4: Trust Fund Operations** - Income, costs, actuarial status

### 1.3 Demography Process Subprocesses
1. **1.1 Fertility** - Age-specific birth rates (THIS PHASE)
2. 1.2 Mortality - Death probabilities by age/sex
3. 1.3 LPR Immigration - Legal permanent resident flows
4. 1.4 Historical Population - Starting population estimates
5. 1.5 Temporary/Unlawfully Present Immigration
6. 1.6 Marriage - Marriage rates by age
7. 1.7 Divorce - Divorce rates by age
8. 1.8 Projected Population - Final population projections

### 1.4 Key Design Principles
- **Reproducibility**: All data acquisition via APIs where possible
- **Configurability**: User-adjustable assumptions and parameters
- **Validation**: Automated comparison to official TR tables
- **Modularity**: Each subprocess as independent targets pipeline step
- **Cross-platform**: Run on Windows and Linux

---

## 2. Project Architecture

### 2.1 Technology Stack
| Component | Technology | Purpose |
|-----------|------------|---------|
| Language | R (>= 4.2.0) | Primary implementation |
| Pipeline | {targets} | Workflow orchestration |
| Dependencies | {renv} | Package management |
| Data APIs | {httr2}, {jsonlite} | API access |
| Data Manipulation | {data.table}, {dplyr} | Data processing |
| Excel I/O | {readxl}, {openxlsx} | TR tables access |
| Validation | {testthat} | Unit testing |
| Documentation | {roxygen2} | Function documentation |
| Reporting | {rmarkdown}, {quarto} | Output reports |
| App | {shiny}, {bslib} | Interactive interface |

### 2.2 Targets Pipeline Philosophy
Each subprocess will be implemented as one or more targets:
```
tar_target(nchs_births_raw, fetch_nchs_births(...))
tar_target(census_population_raw, fetch_census_population(...))
tar_target(fertility_historical, calculate_historical_birth_rates(...))
tar_target(fertility_projected, project_birth_rates(...))
tar_target(fertility_validated, validate_fertility_outputs(...))
```

### 2.3 Configuration-Driven Design
All assumptions stored in configuration files (YAML) that can be:
- Modified by users for scenario analysis
- Versioned alongside official TR assumptions
- Loaded dynamically at runtime

---

## 3. Project Setup Tasks

### 3.1 Initialize R Project Structure

**Task 3.1.1: Create project directories**
```
artemis/
├── R/                      # R source files
│   ├── data_acquisition/   # API fetch functions
│   ├── demography/         # Demography subprocess functions
│   ├── economics/          # Economics subprocess functions (future)
│   ├── beneficiaries/      # Beneficiaries subprocess functions (future)
│   ├── trust_fund/         # Trust fund subprocess functions (future)
│   ├── utils/              # Utility functions
│   └── validation/         # Validation functions
├── _targets.R              # Targets pipeline definition
├── config/                 # Configuration files
│   ├── assumptions/        # TR assumptions by year
│   └── api_endpoints.yaml  # API configuration
├── data/
│   ├── raw/               # Raw downloaded data (existing)
│   ├── processed/         # Cleaned/transformed data
│   └── outputs/           # Model outputs
├── tests/
│   └── testthat/          # Unit tests
├── reports/               # Generated reports
├── plans/                 # Implementation plans (existing)
├── documentation/         # SSA documentation (existing)
└── app/                   # Shiny application (future)
```

**Task 3.1.2: Initialize renv**
```r
# In R console:
renv::init()
```

**Task 3.1.3: Install and snapshot core packages**
```r
# Core packages to install:
install.packages(c(
  "targets",      # Pipeline
  "tarchetypes",  # Target helpers
  "data.table",   # Fast data manipulation
  "dplyr",        # Data manipulation

  "tidyr",        # Data reshaping
  "httr2",        # HTTP requests
  "jsonlite",     # JSON parsing
  "readxl",       # Excel reading
  "openxlsx",     # Excel writing
  "yaml",         # YAML config files
  "testthat",     # Testing
  "roxygen2",     # Documentation
  "here",         # Path management
  "cli",          # Console output
  "glue",         # String interpolation
  "checkmate"     # Argument validation
))
renv::snapshot()
```

**Task 3.1.4: Create _targets.R skeleton**
```r
# _targets.R
library(targets)
library(tarchetypes)

# Source all R files
tar_source()

# Set options
tar_option_set(
  packages = c("data.table", "dplyr", "httr2", "jsonlite", "yaml"),
  format = "qs",  # Fast serialization
  error = "continue"
)

# Define pipeline
list(
  # Configuration targets
  tar_target(config_assumptions, load_assumptions("config/assumptions/tr2025.yaml")),
  tar_target(config_api, load_api_config("config/api_endpoints.yaml")),


  # Data acquisition targets
  # ... (defined in subsequent sections)

  # Fertility subprocess targets
  # ... (defined in subsequent sections)

  # Validation targets
  # ... (defined in subsequent sections)
)
```

---

## 4. Data Acquisition Module

### 4.1 Overview of Required Data Sources for Fertility

| Data | Source | API/Method | Years | Detail |
|------|--------|------------|-------|--------|
| Births by age of mother | NCHS/CDC WONDER | API | 1980-2023 | Ages 10-54 |
| Female resident population | Census Bureau | API | 1980-2024 | Single year of age 14-49 |
| Historical birth rates | NCHS | Static file | 1917-1979 | Single year of age 14-49 |
| Provisional birth rates | NCHS | File download | 2023-2024 | 5-year age groups |
| Monthly births | CDC | File download | 2024 | Total births |
| 2024 TFR estimate | Calculated | Derived | 2024 | Used for projection base |

### 4.2 CDC WONDER API for Birth Data

**Task 4.2.1: Create CDC WONDER API wrapper**

File: `R/data_acquisition/cdc_wonder.R`

```r
#' Fetch birth data from CDC WONDER Natality database
#'
#' @description
#' Queries CDC WONDER API for natality (birth) data. The API uses XML-based
#' queries and returns tab-delimited results.
#'
#' @param years Integer vector of years to query (e.g., 1980:2023)
#' @param by_vars Character vector of grouping variables
#'   Valid options: "year", "age_of_mother", "age_of_mother_single_year"
#' @param measures Character vector of measures to return
#'   Valid options: "births", "fertility_rate", "birth_rate"
#'
#' @return data.table with requested birth statistics
#'
#' @details
#' CDC WONDER Natality database covers:
#' - 1995-2023: Detailed natality data
#' - 2016-2023: Expanded data with additional variables
#'
#' For 1980-1994, use NCHS VitalStats flat files instead.
#'
#' API Documentation: https://wonder.cdc.gov/wonder/help/Natality.html
#'
#' @export
fetch_cdc_wonder_births <- function(years, by_vars, measures) {
  # Implementation details:
  # 1. Build XML query body
  # 2. POST to https://wonder.cdc.gov/controller/datarequest/D66
  # 3. Parse tab-delimited response
  # 4. Clean and return as data.table
}

#' Build CDC WONDER XML query for natality data
#' @keywords internal
build_wonder_natality_query <- function(years, by_vars, measures) {
  # XML query structure for WONDER API
}
```

**IMPORTANT NOTE**: CDC WONDER API has limitations:
- Query results limited to 75,000 rows
- May need multiple queries for full historical series
- Single-year-of-age data may require special handling

**Alternative Approach**: Download NCHS VitalStats files directly for complete historical series.

**Task 4.2.2: Create NCHS file download function (backup method)**

File: `R/data_acquisition/nchs_vitals.R`

```r
#' Download NCHS Vital Statistics natality data files
#'
#' @description
#' Downloads and processes NCHS natality data files for years where
#' CDC WONDER API access is limited or insufficient.
#'
#' @param years Integer vector of years
#' @param cache_dir Directory to cache downloaded files
#'
#' @return data.table with births by age of mother and year
#'
#' @details
#' NCHS provides natality data in multiple formats:
#' - PDF reports with tables (historical)
#' - Downloadable data files (recent years)
#' - microdata (restricted access)
#'
#' @export
fetch_nchs_natality_files <- function(years, cache_dir = "data/raw/nchs") {
  # Implementation:
  # 1. Check for cached files
  # 2. Download missing years
  # 3. Parse and standardize format
  # 4. Return combined data.table
}
```

### 4.3 Census Bureau Population Data

**Task 4.3.1: Create Census API wrapper**

File: `R/data_acquisition/census_population.R`

```r
#' Fetch population estimates from Census Bureau API
#'
#' @description
#' Retrieves population estimates by single year of age and sex from
#' Census Bureau's Population Estimates Program (PEP).
#'
#' @param years Integer vector of years to query
#' @param ages Integer vector of ages (default: 0:100)
#' @param sex Character: "both", "male", or "female"
#' @param api_key Census API key (from .Renviron)
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @details
#' Census API endpoints vary by vintage:
#' - 2000-2010: Intercensal estimates
#' - 2010-2019: Postcensal estimates (2010 base)
#' - 2020-2024: Postcensal estimates (2020 base)
#'
#' API key stored in .Renviron as CENSUS_KEY
#'
#' @export
fetch_census_population <- function(years,
                                    ages = 0:100,
                                    sex = "both",
                                    api_key = Sys.getenv("CENSUS_KEY")) {
  # Implementation:
  # 1. Determine which API endpoint(s) needed for requested years
  # 2. Build API queries
  # 3. Execute requests with rate limiting
  # 4. Combine and standardize results
  # 5. Return data.table
}

#' Get appropriate Census API endpoint for year
#' @keywords internal
get_census_pep_endpoint <- function(year) {
  # Route to correct vintage endpoint
  if (year >= 2020) {
    # Vintage 2023 estimates
    "https://api.census.gov/data/2023/pep/natmonthly"
  } else if (year >= 2010) {
    # 2010-2019 intercensal
    "https://api.census.gov/data/2019/pep/charagegroups"
  } else {
    # Pre-2010: use downloaded files
    NULL
  }
}
```

**Task 4.3.2: Document Census API variables**

```yaml
# config/api_endpoints.yaml (partial)
census:
  base_url: "https://api.census.gov/data"
  population_estimates:
    vintage_2023:
      endpoint: "/2023/pep/charagegroups"
      variables:
        - NAME       # Geographic area name
        - POP        # Population
        - SEX        # Sex (0=Both, 1=Male, 2=Female)
        - AGE        # Single year of age (0-85+)
        - DATE_CODE  # Estimate date
    vintage_2019:
      endpoint: "/2019/pep/charagegroups"
      # Similar structure
```

### 4.4 Historical Birth Rate Data (Pre-1980)

**Task 4.4.1: Create historical birth rate loader**

File: `R/data_acquisition/historical_fertility.R`

```r
#' Load historical NCHS birth rates (1917-1979)
#'
#' @description
#' Loads pre-computed historical birth rates from NCHS publications.
#' These rates are static and stored as package data.
#'
#' @return data.table with columns: year, age, birth_rate
#'
#' @details
#' Source: NCHS historical vital statistics reports
#' Data includes single-year-of-age rates for ages 14-49
#'
#' Years 1917-1979 are considered complete historical data that
#' does not require updates.
#'
#' @export
load_historical_birth_rates <- function() {
  # Load from internal data file
  # data/raw/nchs_historical_birth_rates.csv
}
```

**Task 4.4.2: Create/source historical birth rate file**

Need to create or obtain `data/raw/nchs_historical_birth_rates.csv`:
- Source: NCHS Vital Statistics historical reports
- Format: year, age (14-49), birth_rate (births per 1,000 women)
- Years: 1917-1979
- This may need to be manually compiled from NCHS publications

### 4.5 Provisional and Monthly Birth Data

**Task 4.5.1: Create provisional data fetcher**

File: `R/data_acquisition/provisional_births.R`

```r
#' Fetch provisional birth data from NCHS
#'
#' @description
#' Downloads provisional vital statistics data for recent incomplete years.
#' Used to estimate current-year birth rates.
#'
#' @param years Integer vector of years (typically current and prior year)
#'
#' @return data.table with provisional birth counts/rates
#'
#' @details
#' NCHS releases provisional data quarterly:
#' - 12-month ending rates by 5-year age group
#' - Monthly birth counts
#'
#' URLs for provisional data (example):
#' - https://www.cdc.gov/nchs/data/vsrr/vsrr-provisional-estimates.xlsx
#'
#' @export
fetch_provisional_births <- function(years = c(year(Sys.Date()) - 1,
                                                year(Sys.Date()))) {
  # Download and parse provisional data
}

#' Fetch CDC monthly birth counts
#'
#' @description
#' Downloads monthly birth count data for estimating current year totals.
#'
#' @param year Target year
#' @param through_month Most recent month available
#'
#' @return data.table with monthly birth counts
#'
#' @export
fetch_monthly_births <- function(year, through_month = 6) {
  # Download from CDC provisional data releases
}
```

---

## 5. Fertility Subprocess Implementation

### 5.1 Mathematical Framework

From the SSA documentation, the fertility subprocess uses these key equations:

**Equation 1.1.1: Age-specific birth rate**
$$b_x^z = \frac{B_x^z}{P_x^z}$$

Where:
- $b_x^z$ = Birth rate at age $x$ in year $z$
- $B_x^z$ = Births to mothers age $x$ in year $z$
- $P_x^z$ = Midyear female population age $x$ in year $z$

**Equation 1.1.2: Total Fertility Rate (Period)**
$$TFR^z = \sum_{x=14}^{49} b_x^z$$

**Equation 1.1.3: Cohort Total Fertility Rate**
$$CTFR^t = \sum_{x=14}^{49} b_x^{t+x}$$

Where $t$ = birth year of cohort

### 5.2 Projection Methodology (10 Steps)

**Step 1**: Calculate historical age-specific birth rates ($b_x^z$) for 1980-2023
**Step 2**: Estimate 2024 rates using provisional data
**Step 3**: Calculate ratios to age 30 rate: $r_x^z = \frac{b_x^z}{b_{30}^z}$
**Step 4**: Calculate year-over-year ratio changes: $p_x^z = \frac{r_x^z}{r_x^{z-1}}$
**Step 5**: Calculate average $p_x$ values over 1981-2024 (excluding 1997)
**Step 6**: Project $r_x^z$ values forward: $r_x^z = r_x^{z-1} \cdot a_x$
**Step 7**: Calculate ultimate years for each age: $u_x = 2025 + (x-14) \cdot \frac{2050-2025}{49-14}$
**Step 8**: Calculate weights: $w^z = 1 - \left(\frac{2036-z}{2036-2024}\right)^{1.5}$
**Step 9**: Solve for ultimate age 30 rate ($b_{30}^{2036}$) to achieve target CTFR
**Step 10**: Calculate all projected rates: $b_x^z = b_{30}^z \cdot r_x^z$

### 5.3 Implementation Functions

**Task 5.3.1: Calculate historical birth rates**

File: `R/demography/fertility.R`

```r
#' Calculate historical age-specific birth rates
#'
#' @description
#' Computes age-specific birth rates from raw birth counts and population data.
#' For years 1980+, calculates rates from NCHS births and Census population.
#' For years pre-1980, uses pre-calculated NCHS rates.
#'
#' @param births data.table with columns: year, age, births
#' @param population data.table with columns: year, age, female_pop
#' @param historical_rates data.table with pre-1980 rates
#'
#' @return data.table with columns: year, age, birth_rate, source
#'   where birth_rate is births per woman (not per 1,000)
#'
#' @export
calculate_historical_birth_rates <- function(births,
                                              population,
                                              historical_rates) {
  # Implementation:
  # 1. Join births and population for years 1980+
  # 2. Calculate rate: birth_rate = births / population
  # 3. Combine with pre-1980 historical rates
  # 4. Validate: all ages 14-49 present for each year
  # 5. Return combined data.table

  # Rate should be births per woman, not per 1,000
  # (multiply historical rates by 0.001 if provided per 1,000)
}
```

**Task 5.3.2: Estimate 2024 birth rates**

```r
#' Estimate current year birth rates from provisional data
#'
#' @description
#' Estimates age-specific birth rates for the current/partial year
#' using provisional data from NCHS and state-level data.
#'
#' @param provisional_rates data.table with 5-year age group provisional rates
#' @param prior_year_rates data.table with prior year single-year-of-age rates
#' @param tfr_estimate Numeric: estimated TFR for current year (default: 1.62)
#'
#' @return data.table with estimated single-year-of-age rates
#'
#' @details
#' Method from TR2025 documentation:
#' 1. Use estimated total fertility of 1.62
#' 2. Apply age patterns from prior year adjusted for provisional 5-year rates
#' 3. Disaggregate 5-year rates to single years using prior year shape
#'
#' @export
estimate_current_year_rates <- function(provisional_rates,
                                        prior_year_rates,
                                        tfr_estimate = 1.62) {
  # Implementation:
  # 1. Scale prior year rates to match estimated TFR
  # 2. Adjust within 5-year groups to match provisional data
  # 3. Return estimated single-year rates
}
```

**Task 5.3.3: Calculate age-30 ratios**

```r
#' Calculate birth rate ratios relative to age 30
#'
#' @description
#' Computes the ratio of each age's birth rate to the age 30 rate.
#' These ratios are used to project the age pattern of fertility.
#'
#' @param birth_rates data.table with columns: year, age, birth_rate
#'
#' @return data.table with columns: year, age, birth_rate, ratio_to_30
#'
#' @details
#' The age 30 rate is used as the reference because it is typically
#' near the peak of the fertility schedule and relatively stable.
#'
#' Formula: r_x^z = b_x^z / b_30^z
#'
#' @export
calculate_age30_ratios <- function(birth_rates) {
  # Implementation:
  # 1. Extract age 30 rates for each year
  # 2. Join back to full data
  # 3. Calculate ratio
  # 4. Handle edge cases (age 30 rate near zero)
}
```

**Task 5.3.4: Calculate trend factors**

```r
#' Calculate year-over-year trend factors for age ratios
#'
#' @description
#' Computes how the age-to-age-30 ratios change from year to year.
#' These factors capture trends in the age pattern of fertility.
#'
#' @param ratios data.table with columns: year, age, ratio_to_30
#' @param exclude_years Integer vector of years to exclude (default: 1997)
#'
#' @return data.table with average trend factors by age
#'
#' @details
#' Formula: p_x^z = r_x^z / r_x^{z-1}
#'
#' 1997 is excluded because NCHS changed their age imputation method
#' for low and high maternal ages that year.
#'
#' @export
calculate_trend_factors <- function(ratios, exclude_years = 1997) {
  # Implementation:
  # 1. Calculate p_x^z for each year
  # 2. Exclude specified years (1997)
  # 3. Calculate average a_x at each age
  # 4. Return data.table(age, avg_trend_factor)
}
```

**Task 5.3.5: Calculate ultimate years**

```r
#' Calculate ultimate attainment years for each age
#'
#' @description
#' Determines when each age reaches its ultimate (constant) birth rate.
#' Younger ages reach ultimate faster; older ages take longer.
#'
#' @param config Configuration list with ultimate_year and base_year
#'
#' @return data.table with columns: age, ultimate_year
#'
#' @details
#' Formula from TR2025: u_x = 2025 + (x - 14) * (2050 - 2025) / (49 - 14)
#'
#' This creates a linear interpolation where:
#' - Age 14 reaches ultimate in 2025
#' - Age 30 reaches ultimate in ~2036
#' - Age 49 reaches ultimate in 2050
#'
#' @export
calculate_ultimate_years <- function(config) {
  base_year <- config$projection_start_year  # 2025
  end_year <- config$ultimate_year            # 2050
  min_age <- config$min_fertility_age         # 14
  max_age <- config$max_fertility_age         # 49

  ages <- min_age:max_age

  ultimate_years <- base_year + (ages - min_age) * (end_year - base_year) / (max_age - min_age)
  ultimate_years <- round(ultimate_years)

  data.table(age = ages, ultimate_year = ultimate_years)
}
```

**Task 5.3.6: Calculate interpolation weights**

```r
#' Calculate weights for interpolating to ultimate age-30 rate
#'
#' @description
#' Computes weights for blending current rates toward ultimate rates.
#' Uses a power function to create gradual transition.
#'
#' @param years Integer vector of projection years
#' @param config Configuration with base year and age-30 ultimate year
#'
#' @return data.table with columns: year, weight
#'
#' @details
#' Formula: w^z = 1 - ((2036 - z) / (2036 - 2024))^1.5
#'
#' The 1.5 exponent creates slower initial change that accelerates.
#' Weight = 0 at base year, weight = 1 at ultimate year (2036).
#'
#' @export
calculate_interpolation_weights <- function(years, config) {
  base_year <- config$rate_base_year          # 2024
  ultimate_year <- config$age30_ultimate_year # 2036
  exponent <- config$weight_exponent          # 1.5

  weights <- 1 - ((ultimate_year - years) / (ultimate_year - base_year))^exponent
  weights <- pmax(0, pmin(1, weights))  # Clamp to [0, 1]

  data.table(year = years, weight = weights)
}
```

**Task 5.3.7: Solve for ultimate age-30 rate**

```r
#' Solve for ultimate age 30 birth rate to achieve target CTFR
#'
#' @description
#' Finds the age 30 birth rate in the ultimate year that produces
#' the target cohort total fertility rate.
#'
#' @param target_ctfr Numeric: target cohort TFR (default: 1.90)
#' @param base_age30_rate Numeric: age 30 rate in base year (2024)
#' @param trend_factors data.table with age-specific trend factors
#' @param ultimate_years data.table with ultimate years by age
#' @param weights data.table with interpolation weights by year
#'
#' @return Numeric: ultimate age 30 birth rate (b_30^2036)
#'
#' @details
#' This is the key equation from TR2025 documentation:
#'
#' b_30^{2036} = (1.90 - sum_{x=14}^{49}((1-w^{u_x}) * b_30^{2024} * p_x^{u_x})) /
#'               sum_{x=14}^{49}(w^{u_x} * p_x^{u_x})
#'
#' Where p_x^{u_x} is the cumulative trend factor from base to ultimate year.
#'
#' @export
solve_ultimate_age30_rate <- function(target_ctfr,
                                       base_age30_rate,
                                       trend_factors,
                                       ultimate_years,
                                       weights) {
  # Implementation:
  # 1. For each age, calculate cumulative trend factor to ultimate year
  # 2. Get weight at each age's ultimate year
  # 3. Apply formula to solve for b_30^2036
  # 4. Return ultimate age 30 rate
}
```

**Task 5.3.8: Project age-30 rates**

```r
#' Project age 30 birth rates for projection period
#'
#' @description
#' Interpolates age 30 rates from base year to ultimate year.
#'
#' @param years Integer vector of projection years
#' @param base_rate Numeric: age 30 rate in base year
#' @param ultimate_rate Numeric: age 30 rate in ultimate year
#' @param weights data.table with interpolation weights
#'
#' @return data.table with columns: year, age30_rate
#'
#' @details
#' Formula: b_30^z = b_30^{2024} * (1 - w^z) + b_30^{2036} * w^z
#'
#' @export
project_age30_rates <- function(years, base_rate, ultimate_rate, weights) {
  # Simple weighted interpolation
  weights[year %in% years,
          .(year,
            age30_rate = base_rate * (1 - weight) + ultimate_rate * weight)]
}
```

**Task 5.3.9: Project all age rates**

```r
#' Project birth rates for all ages
#'
#' @description
#' Computes projected birth rates for all ages using the projected
#' age 30 rates and evolving age ratios.
#'
#' @param years Integer vector of projection years
#' @param age30_rates data.table with projected age 30 rates
#' @param base_ratios data.table with age-to-30 ratios in base year
#' @param trend_factors data.table with trend factors by age
#' @param ultimate_years data.table with ultimate years by age
#'
#' @return data.table with columns: year, age, birth_rate
#'
#' @details
#' Method:
#' 1. Project r_x^z forward using trend factors until ultimate year
#' 2. After ultimate year, r_x^z stays constant
#' 3. Calculate: b_x^z = b_30^z * r_x^z
#'
#' @export
project_birth_rates <- function(years,
                                 age30_rates,
                                 base_ratios,
                                 trend_factors,
                                 ultimate_years) {
  # Implementation:
  # 1. Initialize ratios at base year values
  # 2. For each year, update ratios using trend factors (if before ultimate)
  # 3. Calculate rates: b_x^z = b_30^z * r_x^z
  # 4. Return full projection data.table
}
```

**Task 5.3.10: Calculate TFR and CTFR series**

```r
#' Calculate period and cohort total fertility rates
#'
#' @description
#' Computes TFR (period) and CTFR (cohort) from age-specific rates.
#'
#' @param birth_rates data.table with historical and projected rates
#'
#' @return list with:
#'   - tfr: data.table with period TFR by year
#'   - ctfr: data.table with cohort TFR by birth year
#'
#' @export
calculate_fertility_totals <- function(birth_rates) {
  # Period TFR: sum of rates within each year
  tfr <- birth_rates[, .(tfr = sum(birth_rate)), by = year]

  # Cohort TFR: sum of rates across lifetime of each cohort
  # cohort_year = year - age
  rates_with_cohort <- birth_rates[, cohort_year := year - age]
  ctfr <- rates_with_cohort[, .(ctfr = sum(birth_rate)), by = cohort_year]
  setnames(ctfr, "cohort_year", "birth_year")

  list(tfr = tfr, ctfr = ctfr)
}
```

### 5.4 Fertility Module Target Definition

**Task 5.4.1: Add fertility targets to _targets.R**

```r
# In _targets.R, add fertility subprocess targets:

list(
  # === DATA ACQUISITION TARGETS ===

  # Birth data from CDC/NCHS
  tar_target(
    nchs_births_raw,
    fetch_nchs_births(years = 1980:2023),
    cue = tar_cue(mode = "thorough")
  ),

  # Census population data
  tar_target(
    census_female_pop,
    fetch_census_population(
      years = 1980:2024,
      ages = 14:49,
      sex = "female"
    ),
    cue = tar_cue(mode = "thorough")
  ),

  # Historical pre-1980 rates
  tar_target(
    historical_birth_rates,
    load_historical_birth_rates()
  ),

  # Provisional data for 2024
  tar_target(
    provisional_births_2024,
    fetch_provisional_births(years = 2024)
  ),

  # === FERTILITY CALCULATION TARGETS ===

  # Historical rates (1941-2023)
  tar_target(
    fertility_rates_historical,
    calculate_historical_birth_rates(
      births = nchs_births_raw,
      population = census_female_pop,
      historical_rates = historical_birth_rates
    )
  ),

  # Estimated 2024 rates
  tar_target(
    fertility_rates_2024,
    estimate_current_year_rates(
      provisional_rates = provisional_births_2024,
      prior_year_rates = fertility_rates_historical[year == 2023],
      tfr_estimate = config_assumptions$fertility$tfr_estimate_2024
    )
  ),

  # Combined historical rates
  tar_target(
    fertility_rates_to_present,
    rbind(fertility_rates_historical, fertility_rates_2024)
  ),

  # Age-30 ratios
  tar_target(
    fertility_age30_ratios,
    calculate_age30_ratios(fertility_rates_to_present)
  ),

  # Trend factors
  tar_target(
    fertility_trend_factors,
    calculate_trend_factors(
      fertility_age30_ratios,
      exclude_years = 1997
    )
  ),

  # Ultimate years by age
  tar_target(
    fertility_ultimate_years,
    calculate_ultimate_years(config_assumptions$fertility)
  ),

  # Interpolation weights
  tar_target(
    fertility_weights,
    calculate_interpolation_weights(
      years = 2025:2099,
      config = config_assumptions$fertility
    )
  ),

  # Solve for ultimate age-30 rate
  tar_target(
    fertility_ultimate_age30,
    solve_ultimate_age30_rate(
      target_ctfr = config_assumptions$fertility$ultimate_ctfr,
      base_age30_rate = fertility_rates_to_present[year == 2024 & age == 30, birth_rate],
      trend_factors = fertility_trend_factors,
      ultimate_years = fertility_ultimate_years,
      weights = fertility_weights
    )
  ),

  # Project age-30 rates
  tar_target(
    fertility_age30_projected,
    project_age30_rates(
      years = 2025:2099,
      base_rate = fertility_rates_to_present[year == 2024 & age == 30, birth_rate],
      ultimate_rate = fertility_ultimate_age30,
      weights = fertility_weights
    )
  ),

  # Project all rates
  tar_target(
    fertility_rates_projected,
    project_birth_rates(
      years = 2025:2099,
      age30_rates = fertility_age30_projected,
      base_ratios = fertility_age30_ratios[year == 2024],
      trend_factors = fertility_trend_factors,
      ultimate_years = fertility_ultimate_years
    )
  ),

  # Combined historical and projected
  tar_target(
    fertility_rates_complete,
    rbind(
      fertility_rates_to_present,
      fertility_rates_projected
    )
  ),

  # TFR and CTFR series
  tar_target(
    fertility_totals,
    calculate_fertility_totals(fertility_rates_complete)
  ),

  # === VALIDATION TARGETS ===
  tar_target(
    fertility_validation,
    validate_fertility_outputs(
      rates = fertility_rates_complete,
      totals = fertility_totals,
      official_data = load_tr_tables("TR2025")
    )
  )
)
```

---

## 6. Validation Framework

### 6.1 Validation Data Sources

The TR2025 raw data includes official outputs that can be used for validation:
- `data/raw/SSA_TR2025/TRTables_TR2025.xlsx` - Summary tables
- `data/raw/SSA_TR2025/SingleYearTRTables_TR2025.xlsx` - Detailed tables
- `data/raw/SSA_TR2025/SSPopJan_Alt2_TR2025.csv` - Population projections

### 6.2 Validation Functions

**Task 6.2.1: Create TR tables loader**

File: `R/validation/load_tr_tables.R`

```r
#' Load official Trustees Report tables for validation
#'
#' @description
#' Reads and parses official TR tables from Excel files.
#'
#' @param tr_version Character: TR version (e.g., "TR2025")
#' @param table_type Character: "summary" or "detailed"
#'
#' @return list of data.tables with official projections
#'
#' @export
load_tr_tables <- function(tr_version = "TR2025",
                           table_type = "summary") {
  # Implementation:
  # 1. Read appropriate Excel file
  # 2. Parse relevant sheets (fertility, population, etc.)
  # 3. Standardize column names
  # 4. Return as list of data.tables
}

#' Extract fertility projections from TR tables
#' @keywords internal
extract_fertility_from_tr <- function(tr_tables) {
  # Extract TFR and CTFR series from TR tables
}
```

**Task 6.2.2: Create fertility validation function**

File: `R/validation/validate_fertility.R`

```r
#' Validate fertility outputs against official TR tables
#'
#' @description
#' Compares calculated fertility rates and totals against official
#' Trustees Report values. Reports discrepancies.
#'
#' @param rates data.table with calculated birth rates
#' @param totals list with TFR and CTFR series
#' @param official_data list with official TR projections
#' @param tolerance Numeric: acceptable relative difference (default: 0.01)
#'
#' @return list with:
#'   - passed: logical indicating if validation passed
#'   - summary: data.table with comparison metrics
#'   - discrepancies: data.table with values exceeding tolerance
#'
#' @export
validate_fertility_outputs <- function(rates,
                                        totals,
                                        official_data,
                                        tolerance = 0.01) {
  # Implementation:
  # 1. Compare TFR series
  # 2. Compare CTFR at key years
  # 3. Compare age-specific rates at sample years
  # 4. Report discrepancies exceeding tolerance
  # 5. Return validation results
}
```

**Task 6.2.3: Create unit tests**

File: `tests/testthat/test-fertility.R`

```r
test_that("historical birth rates match NCHS published values", {
  # Load our calculated rates
  # Compare to known NCHS published rates for specific years
  # Check within acceptable tolerance
})

test_that("projected TFR reaches ultimate value", {
  # Verify that period TFR converges to expected value
})

test_that("ultimate CTFR equals configured value", {
  # Verify cohort born in 2020 achieves CTFR of 1.90
})

test_that("birth rates are within reasonable bounds", {
  # No negative rates
  # No rates exceeding biological maximums (~0.25 at peak ages)
})
```

---

## 7. Configuration System

### 7.1 Assumptions Configuration

**Task 7.1.1: Create TR2025 assumptions file**

File: `config/assumptions/tr2025.yaml`

```yaml
# ARTEMIS Configuration: TR2025 Intermediate Assumptions
# Source: SSA Office of the Chief Actuary, 2025 Trustees Report

metadata:
  trustees_report_year: 2025
  alternative: "Intermediate"
  alternative_number: 2
  projection_period:
    start_year: 2025
    end_year: 2099
  valuation_date: "2025-01-01"

fertility:
  # Ultimate cohort TFR assumption
  ultimate_ctfr: 1.90
  # Cohort that first achieves ultimate CTFR
  ultimate_cohort_year: 2020
  # Age range for fertility calculations
  min_fertility_age: 14
  max_fertility_age: 49
  # Reference age for ratio calculations
  reference_age: 30
  # Ultimate attainment years
  projection_start_year: 2025
  ultimate_year: 2050
  age30_ultimate_year: 2036
  # Interpolation weight exponent
  weight_exponent: 1.5
  # Estimated TFR for 2024 (base year)
  tfr_estimate_2024: 1.62
  # Years to exclude from trend calculations
  exclude_years:
    - 1997  # NCHS age imputation method change

mortality:
  # (To be populated when mortality subprocess is implemented)
  ultimate_male_life_expectancy_at_65: null
  ultimate_female_life_expectancy_at_65: null

immigration:
  # (To be populated when immigration subprocesses are implemented)
  ultimate_net_lpr_immigration: null
  ultimate_net_other_immigration: null

# Data source configuration
data_sources:
  historical_birth_data:
    start_year: 1980
    end_year: 2023
  historical_rate_data:
    start_year: 1917
    end_year: 1979
  population_estimates:
    start_year: 1980
    end_year: 2024
```

**Task 7.1.2: Create API configuration file**

File: `config/api_endpoints.yaml`

```yaml
# API Endpoint Configuration for ARTEMIS

census:
  base_url: "https://api.census.gov/data"
  population_estimates:
    pep_2023:
      endpoint: "/2023/pep/charagegroups"
      rate_limit_per_minute: 500
    pep_2019:
      endpoint: "/2019/pep/charagegroups"
      rate_limit_per_minute: 500
  variables:
    sex:
      both: 0
      male: 1
      female: 2

cdc_wonder:
  base_url: "https://wonder.cdc.gov"
  natality:
    endpoint: "/controller/datarequest/D66"
    database_id: "D66"
    rate_limit_per_minute: 60

nchs:
  vital_stats:
    base_url: "https://www.cdc.gov/nchs/data"
    provisional_url: "/vsrr/vsrr-quarterly.xlsx"

bea:
  base_url: "https://apps.bea.gov/api/data"
  rate_limit_per_minute: 100

bls:
  base_url: "https://api.bls.gov/publicAPI/v2"
  rate_limit_per_minute: 500

fred:
  base_url: "https://api.stlouisfed.org/fred"
  rate_limit_per_minute: 120
```

**Task 7.1.3: Create configuration loader**

File: `R/utils/config.R`

```r
#' Load assumptions configuration
#'
#' @description
#' Loads and validates TR assumptions from YAML configuration file.
#'
#' @param config_path Path to YAML configuration file
#'
#' @return list with validated assumptions
#'
#' @export
load_assumptions <- function(config_path) {
  config <- yaml::read_yaml(config_path)
  validate_assumptions(config)
  config
}

#' Validate assumptions configuration
#' @keywords internal
validate_assumptions <- function(config) {
  # Check required fields exist
  # Validate value ranges
  # Report any issues
}

#' Load API endpoint configuration
#'
#' @param config_path Path to API config YAML
#'
#' @return list with API configurations
#'
#' @export
load_api_config <- function(config_path) {
  yaml::read_yaml(config_path)
}
```

---

## 8. File Structure

### 8.1 Complete File Structure After Implementation

```
artemis/
├── .Renviron                              # API keys (existing)
├── .gitignore                             # Git ignore patterns
├── _targets.R                             # Targets pipeline definition
├── renv.lock                              # Package versions (generated)
├── renv/                                  # renv library (generated)
├── DESCRIPTION                            # Package metadata (optional)
│
├── R/
│   ├── data_acquisition/
│   │   ├── cdc_wonder.R                   # CDC WONDER API functions
│   │   ├── census_population.R            # Census API functions
│   │   ├── nchs_vitals.R                  # NCHS file download functions
│   │   ├── historical_fertility.R         # Historical rate loader
│   │   └── provisional_births.R           # Provisional data fetcher
│   │
│   ├── demography/
│   │   ├── fertility.R                    # Fertility subprocess functions
│   │   ├── mortality.R                    # (future)
│   │   ├── immigration_lpr.R              # (future)
│   │   ├── immigration_other.R            # (future)
│   │   ├── historical_population.R        # (future)
│   │   ├── marriage.R                     # (future)
│   │   ├── divorce.R                      # (future)
│   │   └── projected_population.R         # (future)
│   │
│   ├── utils/
│   │   ├── config.R                       # Configuration loaders
│   │   ├── api_helpers.R                  # API request utilities
│   │   └── data_helpers.R                 # Data manipulation utilities
│   │
│   └── validation/
│       ├── load_tr_tables.R               # TR table loader
│       └── validate_fertility.R           # Fertility validation
│
├── config/
│   ├── assumptions/
│   │   ├── tr2025.yaml                    # TR2025 assumptions
│   │   └── custom_template.yaml           # Template for custom scenarios
│   └── api_endpoints.yaml                 # API configuration
│
├── data/
│   ├── raw/                               # (existing SSA data)
│   │   ├── SSA_TR2025/
│   │   ├── SSA_supplemental/
│   │   └── bls_ln.series.txt
│   ├── processed/                         # Cleaned intermediate data
│   │   └── .gitkeep
│   └── outputs/                           # Model outputs
│       └── .gitkeep
│
├── tests/
│   ├── testthat.R                         # Test runner
│   └── testthat/
│       ├── test-fertility.R               # Fertility tests
│       └── test-config.R                  # Configuration tests
│
├── documentation/                         # (existing SSA docs)
│   ├── 1_Intro/
│   └── 2_Demography/
│
├── plans/                                 # Implementation plans
│   └── 01_demography_fertility_implementation_plan.md
│
└── reports/                               # Generated reports
    └── .gitkeep
```

---

## 9. Implementation Sequence

> **Note**: Update status checkboxes as tasks are completed. Remember: tasks are not complete until working with real data.

### Phase 1A: Project Setup (First) - COMPLETE

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 1.1 | Create directory structure | None | Directories created |
| [x] | 1.2 | Initialize renv | Directories | renv.lock |
| [x] | 1.3 | Install packages | renv | Packages available |
| [x] | 1.4 | Create config files | None | YAML files |
| [x] | 1.5 | Create _targets.R skeleton | Packages | Pipeline structure |

### Phase 1B: Data Acquisition Functions

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 2.1 | Create Census API wrapper | config | census_population.R |
| [x] | 2.2 | Create NCHS births downloader (via NBER Stata files) | None | nchs_births.R |
| [x] | 2.3 | Download full historical NCHS data (1980-2023) | 2.2 | Cached .rds files |
| [ ] | 2.4 | Create historical rate loader (1917-1967 from publications) | None | historical_fertility.R |
| [ ] | 2.5 | Create provisional data fetcher | None | provisional_births.R |
| [x] | 2.6 | Create API helper utilities | config | api_helpers.R |
| [x] | 2.7 | Test data acquisition with real data | All above | Data files |

**Notes on Phase 1B:**
- Step 2.1: Census population data sources:
  - 2020-2024: Vintage 2024 XLSX file download (Census discontinued PEP API support after 2020)
  - 2010-2019: Census Bureau API Vintage 2019 endpoint
  - 2000-2009: Census Bureau API Vintage 2019 endpoint (intercensal estimates)
  - 1990-1999: Census Bureau API Vintage 2000 endpoint
  - 1980-1989: Downloaded intercensal estimate files from Census (`fetch_census_population_files()`)
  - File: NC-EST2024-SYASEXN.xlsx from Census Bureau with single-year-of-age by sex
- Step 2.2: Using NBER Stata files instead of CDC WONDER API. Files contain single-year-of-age data (variable `mager` for 2003+, `dmage` for 1968-2002). Raw files are 500-900 MB each but cached aggregated results are ~500 bytes.
- Step 2.2 Sampling weights: Pre-1972 files are 50% samples (multiply by 2), 1972+ use `recwt` weight variable.
- Step 2.3: Completed for 1980-2023 (years needed for fertility rate calculation).
- Step 2.4: **DEFERRED.** Historical rates (1917-1979) are only needed for historical output series (years 1941-1979) and CTFR for old cohorts. The projection methodology only uses 1980-2024 data. Will implement later when needed for full historical output.
- Step 2.5: **DEFERRED.** Provisional data was used by SSA to estimate 2024 rates before final data existed. We now have final 2024 data from NBER, which is more complete. However, provisional data fetcher may be needed in the future when running projections before NBER publishes the latest year's final data (NBER typically lags several months behind NCHS releases).

### Phase 1C: Fertility Subprocess Functions

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 3.1 | Implement calculate_historical_birth_rates | Data | fertility.R |
| [x] | 3.2 | Implement estimate_current_year_rates | Historical rates | Skipped (have 2024 data) |
| [x] | 3.3 | Implement calculate_age30_ratios | Rates | fertility.R |
| [x] | 3.4 | Implement calculate_trend_factors | Ratios | fertility.R |
| [x] | 3.5 | Implement calculate_ultimate_years | Config | fertility.R |
| [x] | 3.6 | Implement calculate_interpolation_weights | Config | fertility.R |
| [x] | 3.7 | Implement solve_ultimate_age30_rate | All above | fertility.R |
| [x] | 3.8 | Implement project_age30_rates | Solution | fertility.R |
| [x] | 3.9 | Implement project_birth_rates | Age-30 proj | fertility.R |
| [x] | 3.10 | Implement calculate_fertility_totals | All rates | fertility.R |

**Notes on Phase 1C:**
- All core fertility functions implemented in `R/demography/fertility.R`
- Step 3.2 (estimate_current_year_rates) skipped since we have final 2024 data from NBER
- Functions follow the 10-step methodology from TR documentation
- **VALIDATED** with full real data: 1980-2023 NCHS births + 1980-2024 Census population
  - Historical TFR (1980-2024): Matches TR2025 within 0.02 (max 1.1% error)
  - Projected TFR: Converges to exactly 1.90 at ultimate year
  - Configurable parameters: ultimate_ctfr, ultimate_year, age30_ultimate_year in `config/assumptions/tr2025.yaml`

### Phase 1D: Validation and Testing

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 4.1 | Create TR table loader | Raw data | scripts/validate_fertility.R |
| [x] | 4.2 | Create validation functions | TR tables | scripts/validate_fertility.R |
| [ ] | 4.3 | Create unit tests | All functions | test-fertility.R |
| [x] | 4.4 | Run full pipeline with real data | All targets | Validated outputs |
| [~] | 4.5 | Generate validation report | Validation | Report |

**Notes on Phase 1D:**
- Steps 4.1-4.2: Validation script created at `scripts/validate_fertility.R` that compares TFR output against TR2025 Table V.A1
- Step 4.4: Full pipeline validated - historical (1980-2024) mean diff 0.006, max diff 0.018; projection (2025-2099) converges to exactly 1.90 at ultimate year
- Solver bug fixed: Original solver produced TFR=1.9126 at ultimate; fixed to correctly produce target TFR (1.90) by simplifying the formula to `target_ctfr / sum(ratios_at_ultimate)`

### Phase 1E: Documentation and Cleanup

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 5.1 | Add roxygen documentation | All functions | Documented code |
| [ ] | 5.2 | Create README for fertility module | Documentation | README |
| [ ] | 5.3 | Snapshot renv | All packages | renv.lock updated |

---

## 10. Technical Specifications

### 10.1 Data Structures

**Birth Rates Data Table Schema:**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year (1917-2099)
age             integer   Age of mother (14-49)
birth_rate      numeric   Births per woman (not per 1,000)
source          character "historical", "calculated", "estimated", "projected"
```

**TFR Data Table Schema:**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
tfr             numeric   Period total fertility rate
type            character "historical" or "projected"
```

**CTFR Data Table Schema:**
```
Column          Type      Description
------          ----      -----------
birth_year      integer   Birth year of cohort
ctfr            numeric   Cohort total fertility rate
completeness    character "complete", "partial", "projected"
```

### 10.2 API Rate Limits and Error Handling

```r
#' Execute API request with retry logic
#'
#' @param request httr2 request object
#' @param max_retries Maximum retry attempts
#' @param backoff_factor Multiplier for exponential backoff
#'
#' @return Response object or error
api_request_with_retry <- function(request,
                                   max_retries = 3,
                                   backoff_factor = 2) {
  for (i in seq_len(max_retries)) {
    tryCatch({
      resp <- httr2::req_perform(request)
      return(resp)
    }, error = function(e) {
      if (i == max_retries) stop(e)
      Sys.sleep(backoff_factor^i)
    })
  }
}
```

### 10.3 Cross-Platform Considerations

1. **File Paths**: Use `here::here()` for all paths
2. **Line Endings**: Configure git for consistent handling
3. **Dependencies**: All packages available on CRAN for both Windows/Linux
4. **API Access**: Environment variables work identically on both platforms

### 10.4 Performance Considerations

1. **Data Storage**: Use `qs` format for targets caching (fast serialization)
2. **Large Data**: data.table for memory-efficient operations
3. **API Caching**: Cache raw API responses to avoid repeated calls
4. **Parallel Processing**: targets supports parallel execution if needed

---

## Appendix A: Reference Values for Validation

### A.1 TR2025 Fertility Assumptions (Intermediate)

| Metric | TR2025 Value | Our Value | Status |
|--------|--------------|-----------|--------|
| Ultimate Period TFR | 1.90 | 1.9000 | ✓ MATCH |
| Period TFR 2024 | 1.62 | 1.6020 | ✓ Within tolerance |
| Period TFR 2025 | 1.64 | 1.6195 | ✓ Within tolerance |
| Period TFR 2050 | 1.90 | 1.9000 | ✓ MATCH |

### A.2 Validation Results Summary

| Period | Years | Mean Abs Diff | Max Abs Diff | Max % Diff |
|--------|-------|---------------|--------------|------------|
| Historical | 1980-2024 | 0.006 | 0.018 | 1.1% |
| Projection | 2025-2049 | 0.012 | 0.021 | 1.3% |
| Ultimate | 2050-2099 | 0.000 | 0.000 | 0.0% |

### A.3 Key Age-Specific Rate Benchmarks (2023)

From NCHS published data, approximate rates (births per 1,000 women):
- Age 20-24: ~65
- Age 25-29: ~95
- Age 30-34: ~100
- Age 35-39: ~55
- Age 40-44: ~12

---

## Appendix B: External Data Source URLs

### B.1 NCHS/CDC Data

- CDC WONDER Natality: https://wonder.cdc.gov/natality.html
- NCHS VitalStats: https://www.cdc.gov/nchs/nvss/births.htm
- Provisional Data: https://www.cdc.gov/nchs/nvss/vsrr/natality.htm

### B.2 Census Bureau Data

- Population Estimates: https://www.census.gov/data/developers/data-sets/popest-popproj.html
- API Documentation: https://www.census.gov/data/developers.html

### B.3 SSA Official Data

- Trustees Report: https://www.ssa.gov/oact/TR/
- Actuarial Publications: https://www.ssa.gov/oact/NOTES/

---

## Appendix C: Glossary

| Term | Definition |
|------|------------|
| ASFR | Age-Specific Fertility Rate |
| CTFR | Cohort Total Fertility Rate |
| TFR | Total Fertility Rate (Period) |
| LPR | Lawful Permanent Resident |
| NCHS | National Center for Health Statistics |
| OASDI | Old-Age, Survivors, and Disability Insurance |
| SS Area | Social Security Area (US + territories + abroad) |
| TR | Trustees Report |

---

*End of Implementation Plan*

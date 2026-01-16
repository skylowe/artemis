# ARTEMIS: OASDI Projection Model Implementation Plan
## Phase 2: Demography Process - Mortality Subprocess (1.2)

**Document Version:** 1.0
**Date:** January 2026
**Scope:** Mortality subprocess implementation following fertility completion
**Source:** SSA 2025 Long-Range Model Documentation, Section 1.2 Mortality

---

## Progress Tracking

> **IMPORTANT**: This plan document should be updated as implementation progresses. Each task should be marked with its current status.

### Status Legend
- [ ] Not started
- [~] In progress
- [x] Completed

### Current Status
**Last Updated:** January 2026
**Current Phase:** Phase 2A - Data Acquisition (NOT STARTED)
**Prior Completion:** Phase 1 - Fertility subprocess validated and complete

### Critical Rule: Real Data Only
**No synthetic or mock data is permitted.** A task cannot be marked as completed until it is working with real data from actual data sources.

---

## Table of Contents
1. [Overview](#1-overview)
2. [Mathematical Framework](#2-mathematical-framework)
3. [Input Data Requirements](#3-input-data-requirements)
4. [Projection Methodology](#4-projection-methodology)
5. [Implementation Functions](#5-implementation-functions)
6. [Configuration](#6-configuration)
7. [Validation Framework](#7-validation-framework)
8. [Implementation Sequence](#8-implementation-sequence)
9. [Technical Specifications](#9-technical-specifications)

---

## 1. Overview

### 1.1 Subprocess Purpose

The MORTALITY subprocess projects annual death probabilities (qx) by age and sex, which are the primary outputs used in population projections. Secondary outputs include:
- Central death rates (mx) by age, sex, and cause of death
- Period life expectancy (ex)
- Age-adjusted death rates (ADR, ASDR)
- Death probabilities by marital status

### 1.2 Key Concepts

| Term | Definition |
|------|------------|
| mx | Central death rate: deaths during year / midyear population |
| qx | Probability of death: prob. person age x dies within one year |
| AAx | Annual percentage reduction in death rates |
| ex | Period life expectancy at age x |
| ADR | Age-adjusted death rate (single sex) |
| ASDR | Age-sex-adjusted death rate (both sexes) |

### 1.3 Causes of Death (6 Categories)

| Code | Cause | ICD-10 Codes |
|------|-------|--------------|
| CVD | Cardiovascular Disease | I00-I78, N02-N03, N05-N07, N26 |
| CAN | Cancer | C00-C97 |
| ACV | Accidents and Violence | U01-U03, V01-Y35, Y40-Y87.2, Y88, Y89.0, Y89.9 |
| RES | Respiratory Disease | J00-J06, J09-J18, J20-J22, J30-J47, J60-J98, U04 |
| DEM | Dementia | F01, F03, G30, G31 |
| OTH | Other | All other ICD-10 codes |

### 1.4 Age Groups for Ultimate Assumptions

| Group | Ages | Used For |
|-------|------|----------|
| 1 | Under 15 | Ultimate AAx assumptions |
| 2 | 15-49 | Ultimate AAx assumptions |
| 3 | 50-64 | Ultimate AAx assumptions |
| 4 | 65-84 | Ultimate AAx assumptions |
| 5 | 85+ | Ultimate AAx assumptions |

---

## 2. Mathematical Framework

### 2.1 Central Death Rate (Equation 1.2.1)

$$m_x^z = \frac{D_x^z}{P_x^z}$$

Where:
- $m_x^z$ = Central death rate at age x in year z
- $D_x^z$ = Deaths to persons age x in year z
- $P_x^z$ = Midyear population age x in year z

For projections, mx are determined by cause of death and then summed:

$$m_x^z = \sum_{c} m_{x,c}^z$$

### 2.2 Annual Percentage Reduction (Equation 1.2.2)

The AAx values quantify historical mortality improvement:

$$AA_x = 1 - e^{\beta}$$

Where β is the slope of the weighted least-squares regression of log(mx) on year over 2008-2019.

**Weights for regression:**
- Years 1-4 (2008-2011): 0.2, 0.4, 0.6, 0.8
- Years 5-10 (2012-2017): 1.0
- Years 11-12 (2018-2019): 2.0, 3.0

**Starting AAx rules:**
- If calculated AAx ≥ 0: use as starting value
- If calculated AAx < 0: use 75% of the value

### 2.3 Transition to Ultimate AAx

For years after base year until year 25 of projection:

$$AA_x^z = {}_y AA_w^u + 0.8 \times (AA_x^{z-1} - {}_y AA_w^u)$$

Where ${}_y AA_w^u$ is the ultimate reduction rate for age group y containing age x.

At year 25, AAx equals ultimate values.

### 2.4 Death Probability Conversion (Equation 1.2.3)

**Ages 2-99:**
$$q_x = \frac{m_x}{1 + \frac{1}{2} \cdot m_x}$$

**Ages 100-104 (males, 1.05 growth):**
$$q_x = q_{x-1} \cdot \left(\frac{q_{99}}{q_{98}} \cdot \frac{104-x}{5} + 1.05 \cdot \frac{x-99}{5}\right)$$

**Ages 105+ (males):**
$$q_x = 1.05 \cdot q_{x-1}$$

**Females:** Same formulas with 1.06 instead of 1.05. Female qx capped at male qx if crossover occurs.

**Age 0:** Special calculation using detailed infant mortality data (deaths by age in days/months).

**Age 1:** Calculated from 4m1 (death rate ages 1-4) using historical ratio q1/4m1.

### 2.5 Life Expectancy (Equation 1.2.4)

$$\mathring{e}_x = \frac{T_x}{l_x}$$

Where:
- $l_x$ = Number surviving to age x (radix = 100,000)
- $T_x$ = Total person-years lived above age x

### 2.6 Age-Adjusted Death Rates (Equations 1.2.5, 1.2.6)

**By sex:**
$$ADR_s^z = \frac{\sum_x SP_x \cdot m_{x,s}^z}{\sum_x SP_x}$$

**Both sexes combined:**
$$ASDR^z = \frac{\sum_s \sum_x SP_{x,s} \cdot m_{x,s}^z}{\sum_s \sum_x SP_{x,s}}$$

Where SPx is the 2010 Census standard population.

---

## 3. Input Data Requirements

### 3.1 Data Sources Summary

| Data | Source | Years | Method | Priority |
|------|--------|-------|--------|----------|
| Deaths by age, sex, cause | NCHS PUMS | 1968-2022 | File download | High |
| Provisional deaths | NCHS WONDER | 2023 | API | High |
| Population ages 0-64 | Census | 1980-2023 | API/File | High (reuse from fertility) |
| Population ages 65+ | CMS Medicare | 1968-2023 | SSA raw data | High |
| Standard population | Census 2010 | 2010 | Static file | Medium |
| Historical deaths (pre-1968) | NCHS | 1900-1967 | Static file | Low |
| Infant deaths detail | NCHS | 1939-2022 | File download | Medium |
| Monthly births | NCHS | 1935-2022 | File download | Medium |

### 3.2 NCHS Death Data (Primary Source)

**File:** NCHS Public Use Mortality Files (PUMS)
**Years:** 1968-2022 (single year of age available)
**Variables needed:**
- Year of death
- Age at death (single year)
- Sex
- Underlying cause of death (ICD-10 codes, 1999+; ICD-9, 1979-1998)
- Marital status (1979+)

**Access method:** Download from CDC/NCHS or NBER (similar to births)

### 3.3 CMS Medicare Data for Ages 65+

The SSA model uses CMS Medicare enrollment and deaths data for ages 65+ because it provides more accurate counts than NCHS vital statistics at older ages.

**Note:** This data may not be publicly available in the same form SSA uses. We may need to:
1. Use existing SSA death probabilities from TR2025 raw data files for validation
2. Use NCHS data for all ages and validate against TR2025 outputs
3. Apply adjustment factors if systematic differences exist

### 3.4 Existing Data Available (from TR2025)

Already in `data/raw/SSA_TR2025/`:
- `DeathProbsE_M_Alt2_TR2025.csv` - Male death probabilities (validation)
- `DeathProbsE_F_Alt2_TR2025.csv` - Female death probabilities (validation)
- `PerLifeTables_M_Alt2_TR2025.csv` - Male period life tables (validation)
- `PerLifeTables_F_Alt2_TR2025.csv` - Female period life tables (validation)

These serve as validation targets.

---

## 4. Projection Methodology

### 4.1 Step-by-Step Process

**Step 1:** Calculate historical central death rates (mx) by age, sex, and cause for 1979-2022

**Step 2:** Calculate annual percentage reductions (AAx) using weighted log-linear regression on 2008-2019

**Step 3:** Apply Whittaker-Henderson smoothing to mx (ages 2-99)

**Step 4:** Determine starting mx values (fitted values from regression, not actual 2019 values)

**Step 5:** For years 2020-2023, replace projections with actual data

**Step 6:** Calculate transition AAx values from starting to ultimate over 24 years

**Step 7:** Project mx forward using AAx

**Step 8:** Apply Whittaker-Henderson smoothing to projected mx

**Step 9:** Convert mx to qx using appropriate formulas by age

**Step 10:** Apply COVID-19 adjustment factors for 2024-2025

**Step 11:** Calculate qx by marital status using relative differentials

**Step 12:** Calculate life expectancy (ex) from qx series

**Step 13:** Calculate age-adjusted death rates (ADR, ASDR)

### 4.2 Whittaker-Henderson Smoothing

Applied to ages 2-99 with:
- Degree parameter: 2
- Smoothing parameter: 0.01

This is a standard actuarial graduation technique. Implementation available in R packages or can be coded directly.

### 4.3 COVID-19 Adjustment Factors

Applied to qx for 2024-2025:

| Year | Age 0 | Ages 1-14 | Ages 15-64 | Ages 65-84 | Ages 85+ |
|------|-------|-----------|------------|------------|----------|
| 2024 | 1.01 | 1.17 | 0.99 | 1.02 | 0.98 |
| 2025 | 1.00 | 1.09 | 1.00 | 1.00 | 1.00 |

### 4.4 Marital Status Differentials

Relative mortality differences by marital status from 2015-2019 data:
1. Calculate preliminary death rates by marital status from NCHS/ACS data
2. Adjust older age rates for consistency
3. Smooth using Whittaker-Henderson (ages 15-94)
4. All marital statuses converge to same value at age 95

---

## 5. Implementation Functions

### 5.1 Data Acquisition Functions

File: `R/data_acquisition/nchs_deaths.R`

```r
#' Fetch NCHS death data by age, sex, and cause
#'
#' @description
#' Downloads and processes NCHS public use mortality files.
#' Returns deaths by single year of age, sex, and cause of death.
#'
#' @param years Integer vector of years (1968-2022 for single age)
#' @param cache_dir Directory for caching downloaded files
#'
#' @return data.table with columns: year, age, sex, cause, deaths
#'
#' @export
fetch_nchs_deaths <- function(years, cache_dir = "data/cache/nchs_deaths") {
  # Implementation:
  # 1. Check for cached aggregated data
  # 2. Download NCHS PUMS files if needed
  # 3. Map ICD codes to 6 cause categories
  # 4. Aggregate by year, age, sex, cause
  # 5. Cache and return results
}

#' Map ICD-10 codes to mortality cause categories
#' @keywords internal
map_icd10_to_cause <- function(icd10_code) {
  # Map to: CVD, CAN, ACV, RES, DEM, OTH
}

#' Fetch provisional deaths from NCHS WONDER
#'
#' @param year Year to fetch (typically most recent)
#'
#' @return data.table with provisional death counts
#'
#' @export
fetch_nchs_wonder_deaths <- function(year = 2023) {
  # Query NCHS WONDER API for provisional data
}
```

File: `R/data_acquisition/census_population_mortality.R`

```r
#' Fetch population for mortality calculations
#'
#' @description
#' Retrieves population estimates by single year of age and sex.
#' Extends fertility population fetch to include both sexes.
#'
#' @param years Integer vector of years
#' @param ages Integer vector of ages (default: 0:100)
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @export
fetch_population_both_sexes <- function(years, ages = 0:100) {
  # Extend existing census_population.R to include males
  # May be able to reuse much of existing infrastructure
}
```

### 5.2 Core Mortality Functions

File: `R/demography/mortality.R`

```r
#' Calculate historical central death rates
#'
#' @description
#' Computes central death rates (mx) from deaths and population data.
#'
#' @param deaths data.table with columns: year, age, sex, cause, deaths
#' @param population data.table with columns: year, age, sex, population
#'
#' @return data.table with columns: year, age, sex, cause, deaths, population, mx
#'
#' @export
calculate_central_death_rates <- function(deaths, population) {
  # Join deaths and population
  # Calculate mx = deaths / population
  # Handle cause-specific and total mx
}

#' Calculate annual percentage reductions (AAx)
#'
#' @description
#' Calculates mortality improvement rates using weighted log-linear regression.
#'
#' @param mx_historical data.table with historical mx values
#' @param start_year First year of regression period (default: 2008)
#' @param end_year Last year of regression period (default: 2019)
#'
#' @return data.table with columns: age, sex, cause, aa_x, starting_aa_x
#'
#' @details
#' Weights: 0.2, 0.4, 0.6, 0.8 for years 1-4; 1.0 for years 5-10; 2.0, 3.0 for years 11-12.
#' If calculated AAx < 0, starting value = 0.75 * AAx.
#'
#' @export
calculate_mortality_reductions <- function(mx_historical,
                                            start_year = 2008,
                                            end_year = 2019) {
  # For each age, sex, cause:
  # 1. Fit weighted log-linear regression
  # 2. Calculate AAx = 1 - exp(slope)
  # 3. Apply 75% rule for negative values
}

#' Apply Whittaker-Henderson smoothing
#'
#' @description
#' Applies WH smoothing to mortality rates for ages 2-99.
#'
#' @param rates Vector of rates to smooth
#' @param degree Degree parameter (default: 2)
#' @param smoothing_param Smoothing parameter (default: 0.01)
#'
#' @return Vector of smoothed rates
#'
#' @export
whittaker_henderson_smooth <- function(rates, degree = 2, smoothing_param = 0.01) {
  # Implement WH smoothing
  # See: http://www.howardfamily.ca/graduation/index.html
}

#' Calculate fitted starting mx values
#'
#' @description
#' Calculates starting mx values from weighted regression fitted values,
#' not actual historical values.
#'
#' @param mx_historical Historical mx data
#' @param aa_x Calculated AAx values
#' @param base_year Base year for projections (2019)
#'
#' @return data.table with starting mx by age, sex, cause
#'
#' @export
calculate_starting_mx <- function(mx_historical, aa_x, base_year = 2019) {
  # Starting mx = exp(intercept + slope * base_year)
  # from weighted regression
}

#' Transition AAx to ultimate values
#'
#' @description
#' Calculates AAx trajectory from starting values to ultimate assumptions.
#'
#' @param starting_aa data.table with starting AAx
#' @param ultimate_aa data.table with ultimate AAx by age group
#' @param years Integer vector of projection years
#' @param transition_years Number of years to reach ultimate (default: 24)
#'
#' @return data.table with AAx by year, age, sex, cause
#'
#' @details
#' Formula: AA_x^z = ultimate + 0.8 * (AA_x^{z-1} - ultimate)
#'
#' @export
transition_aa_to_ultimate <- function(starting_aa, ultimate_aa, years,
                                       transition_years = 24) {
  # For each year from base to base+24:
  # Apply 80% convergence formula
  # After year 25, use ultimate values
}

#' Project central death rates
#'
#' @description
#' Projects mx forward using AAx reduction rates.
#'
#' @param starting_mx Starting mx values
#' @param aa_trajectory AAx by year, age, sex, cause
#' @param years Projection years
#'
#' @return data.table with projected mx
#'
#' @details
#' Formula: mx^z = mx^{z-1} * (1 - AAx^z)
#'
#' @export
project_central_death_rates <- function(starting_mx, aa_trajectory, years) {
  # Apply reduction rates iteratively
}

#' Convert central death rates to death probabilities
#'
#' @description
#' Converts mx to qx using age-appropriate formulas.
#'
#' @param mx data.table with central death rates
#' @param infant_data data.table with detailed infant mortality
#' @param covid_factors data.table with COVID-19 adjustment factors
#'
#' @return data.table with columns: year, age, sex, qx
#'
#' @details
#' Ages 2-99: qx = mx / (1 + 0.5*mx)
#' Ages 100+: 5% (male) or 6% (female) annual growth from age 99
#' Age 0: Special infant mortality calculation
#' Age 1: From 4m1 with historical ratio
#'
#' @export
convert_mx_to_qx <- function(mx, infant_data = NULL, covid_factors = NULL) {
  # Different methods by age group
}

#' Calculate life table functions
#'
#' @description
#' Calculates complete period life table from qx.
#'
#' @param qx data.table with death probabilities by year, age, sex
#' @param radix Starting population (default: 100000)
#'
#' @return data.table with life table columns: year, sex, age, qx, lx, dx, Lx, Tx, ex
#'
#' @export
calculate_life_table <- function(qx, radix = 100000) {
  # Calculate:
  # lx = survivors
  # dx = deaths = lx * qx
  # Lx = person-years lived
  # Tx = total future person-years
  # ex = life expectancy = Tx / lx
}

#' Calculate age-adjusted death rates
#'
#' @description
#' Calculates ADR (by sex) and ASDR (combined) using 2010 standard population.
#'
#' @param mx data.table with central death rates
#' @param standard_pop data.table with 2010 Census standard population
#'
#' @return list with ADR (by sex) and ASDR (combined)
#'
#' @export
calculate_age_adjusted_rates <- function(mx, standard_pop) {
  # ADR_s = sum(SP_x * mx_s) / sum(SP_x)
  # ASDR = sum(SP_x,s * mx_s) / sum(SP_x,s)
}

#' Calculate death probabilities by marital status
#'
#' @description
#' Applies marital status differential factors to qx.
#'
#' @param qx_total data.table with total death probabilities
#' @param marital_factors data.table with relative mortality by marital status
#'
#' @return data.table with qx by year, age, sex, marital_status
#'
#' @export
calculate_qx_by_marital_status <- function(qx_total, marital_factors) {
  # Apply relative factors from 2015-2019 observation
  # All statuses converge at age 95
}
```

### 5.3 Helper Functions

File: `R/demography/mortality_helpers.R`

```r
#' Get ICD-10 to cause mapping table
#' @export
get_icd10_cause_mapping <- function() {
  data.table::data.table(
    cause = c("CVD", "CAN", "ACV", "RES", "DEM", "OTH"),
    icd10_pattern = c(
      "^I[0-6]|^I7[0-8]|^N0[2357]|^N26",
      "^C",
      "^U0[1-3]|^V|^W|^X|^Y[0-3]|^Y4|^Y5|^Y6|^Y7|^Y8[0-7]|^Y88|^Y89[09]",
      "^J0[0-6]|^J09|^J1[0-8]|^J2[0-2]|^J3|^J4[0-7]|^J[6-9]|^U04",
      "^F0[13]|^G3[01]",
      NA  # Everything else
    )
  )
}

#' Get ultimate AAx assumptions from TR2025
#' @export
get_ultimate_aa_assumptions <- function() {
  # From Appendix 1.2-1 in documentation
  # Returns data.table with age_group, sex, cause, ultimate_aa
}

#' Get COVID-19 adjustment factors
#' @export
get_covid_adjustment_factors <- function() {
  data.table::data.table(
    year = c(rep(2024, 5), rep(2025, 5)),
    age_group = rep(c("0", "1-14", "15-64", "65-84", "85+"), 2),
    factor = c(1.01, 1.17, 0.99, 1.02, 0.98,
               1.00, 1.09, 1.00, 1.00, 1.00)
  )
}

#' Get 2010 Census standard population
#' @export
get_standard_population_2010 <- function() {
  # Load or fetch 2010 Census population by age and sex
}
```

---

## 6. Configuration

### 6.1 Mortality Configuration (add to tr2025.yaml)

```yaml
mortality:
  # Projection parameters
  base_year: 2019  # Last year used in regression (COVID-affected years excluded)
  projection_start_year: 2025
  ultimate_year: 2049  # Year 25 of projection (2025 + 24)

  # Regression parameters
  regression_start_year: 2008
  regression_end_year: 2019
  regression_weights:
    year_1_4: [0.2, 0.4, 0.6, 0.8]
    year_5_10: 1.0
    year_11_12: [2.0, 3.0]
  negative_aa_factor: 0.75

  # Transition parameters
  transition_rate: 0.80  # 80% of difference each year
  transition_years: 24

  # Smoothing parameters
  whittaker_henderson:
    degree: 2
    smoothing_param: 0.01
    age_range: [2, 99]

  # Age-specific parameters
  very_old_age_start: 100
  male_mortality_growth: 1.05
  female_mortality_growth: 1.06

  # Causes of death
  causes:
    - code: CVD
      name: Cardiovascular Disease
    - code: CAN
      name: Cancer
    - code: ACV
      name: Accidents and Violence
    - code: RES
      name: Respiratory Disease
    - code: DEM
      name: Dementia
    - code: OTH
      name: Other

  # Age groups for ultimate assumptions
  age_groups:
    - name: under_15
      min_age: 0
      max_age: 14
    - name: age_15_49
      min_age: 15
      max_age: 49
    - name: age_50_64
      min_age: 50
      max_age: 64
    - name: age_65_84
      min_age: 65
      max_age: 84
    - name: age_85_plus
      min_age: 85
      max_age: 119

  # Ultimate annual percentage reductions (TR2025 Intermediate)
  # Same for male and female
  ultimate_aa:
    under_15:
      CVD: 1.9
      CAN: 1.5
      ACV: 1.0
      RES: 2.0
      DEM: 0.1
      OTH: 1.7
    age_15_49:
      CVD: 1.3
      CAN: 1.5
      ACV: 0.7
      RES: 0.5
      DEM: 0.1
      OTH: 0.8
    age_50_64:
      CVD: 1.5
      CAN: 1.5
      ACV: 0.5
      RES: 0.7
      DEM: 0.1
      OTH: 0.6
    age_65_84:
      CVD: 1.9
      CAN: 0.9
      ACV: 0.5
      RES: 0.3
      DEM: 0.1
      OTH: 0.3
    age_85_plus:
      CVD: 1.5
      CAN: 0.5
      ACV: 0.3
      RES: 0.2
      DEM: 0.1
      OTH: 0.3

  # COVID-19 adjustment factors
  covid_adjustments:
    2024:
      age_0: 1.01
      age_1_14: 1.17
      age_15_64: 0.99
      age_65_84: 1.02
      age_85_plus: 0.98
    2025:
      age_0: 1.00
      age_1_14: 1.09
      age_15_64: 1.00
      age_65_84: 1.00
      age_85_plus: 1.00
```

---

## 7. Validation Framework

### 7.1 Validation Data Available

From `data/raw/SSA_TR2025/`:
- Death probabilities (qx) by year, age, sex (historical + projected)
- Period life tables with life expectancy

### 7.2 Validation Points

| Metric | Source | Tolerance |
|--------|--------|-----------|
| qx (ages 0-119) | DeathProbsE files | 1% relative |
| Life expectancy at birth | PerLifeTables files | 0.1 years |
| Life expectancy at 65 | PerLifeTables files | 0.1 years |
| Historical mx (2008-2019) | Calculate from data | 2% relative |

### 7.3 Validation Functions

File: `R/validation/validate_mortality.R`

```r
#' Validate mortality outputs against TR2025
#'
#' @param qx_calculated Calculated death probabilities
#' @param official_qx Official TR2025 death probabilities
#' @param tolerance Relative tolerance (default: 0.01)
#'
#' @return list with validation results
#'
#' @export
validate_mortality_outputs <- function(qx_calculated, official_qx,
                                        tolerance = 0.01) {
  # Compare qx series
  # Compare life expectancy
  # Report discrepancies
}

#' Load TR2025 death probabilities for validation
#' @export
load_tr2025_death_probs <- function(alternative = "Alt2") {
  # Read DeathProbsE files
}

#' Load TR2025 life tables for validation
#' @export
load_tr2025_life_tables <- function(alternative = "Alt2") {
  # Read PerLifeTables files
}
```

---

## 8. Implementation Sequence

### Phase 2A: Data Acquisition (NCHS Deaths)

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 2A.1 | Create NCHS death data downloader | None | nchs_deaths.R |
| [ ] | 2A.2 | Implement ICD-10 to cause mapping | 2A.1 | Cause categorization |
| [ ] | 2A.3 | Download/process 1979-2022 death data | 2A.1, 2A.2 | Cached death counts |
| [ ] | 2A.4 | Extend population fetch to both sexes | Existing census code | Population by sex |
| [ ] | 2A.5 | Fetch NCHS WONDER provisional data | None | 2023 deaths |
| [ ] | 2A.6 | Load 2010 standard population | None | Standard pop file |

### Phase 2B: Historical Mortality Calculations

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 2B.1 | Implement central death rate calculation | 2A | mortality.R |
| [ ] | 2B.2 | Implement weighted regression for AAx | 2B.1 | AAx values |
| [ ] | 2B.3 | Implement Whittaker-Henderson smoothing | None | WH function |
| [ ] | 2B.4 | Calculate historical smoothed mx | 2B.1, 2B.3 | Smoothed mx |
| [ ] | 2B.5 | Validate historical mx against expectations | 2B.4 | Validation |

### Phase 2C: Mortality Projection

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 2C.1 | Implement AAx transition function | Config | AAx trajectory |
| [ ] | 2C.2 | Calculate starting mx from regression | 2B.2 | Starting mx |
| [ ] | 2C.3 | Project mx forward | 2C.1, 2C.2 | Projected mx |
| [ ] | 2C.4 | Apply WH smoothing to projections | 2B.3, 2C.3 | Smoothed projections |
| [ ] | 2C.5 | Implement mx to qx conversion | 2C.4 | qx values |
| [ ] | 2C.6 | Apply COVID adjustments | 2C.5 | Adjusted qx |

### Phase 2D: Life Tables and Summary Statistics

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 2D.1 | Implement life table calculation | 2C.6 | Life tables |
| [ ] | 2D.2 | Calculate life expectancy series | 2D.1 | ex values |
| [ ] | 2D.3 | Implement age-adjusted death rates | 2C.4 | ADR, ASDR |
| [ ] | 2D.4 | Implement marital status differentials | 2C.6 | qx by marital status |

### Phase 2E: Validation and Testing

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 2E.1 | Create TR2025 validation data loader | Raw data | Validation data |
| [ ] | 2E.2 | Validate qx against TR2025 | 2C.6, 2E.1 | Validation report |
| [ ] | 2E.3 | Validate life expectancy against TR2025 | 2D.2, 2E.1 | Validation report |
| [ ] | 2E.4 | Create unit tests | All functions | test-mortality.R |
| [ ] | 2E.5 | Run full pipeline validation | All | Validated outputs |

### Phase 2F: Targets Integration

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 2F.1 | Add mortality targets to _targets.R | All functions | Pipeline targets |
| [ ] | 2F.2 | Test full pipeline execution | 2F.1 | Working pipeline |
| [ ] | 2F.3 | Documentation and cleanup | All | Documented code |

---

## 9. Technical Specifications

### 9.1 Data Structures

**Central Death Rates (mx):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age (0-119)
sex             character "male" or "female"
cause           character CVD, CAN, ACV, RES, DEM, OTH, or "total"
deaths          numeric   Number of deaths
population      numeric   Midyear population
mx              numeric   Central death rate
```

**Death Probabilities (qx):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age (0-119)
sex             character "male" or "female"
qx              numeric   Probability of death
source          character "historical", "projected"
```

**Life Table:**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
sex             character "male" or "female"
age             integer   Exact age
qx              numeric   Probability of death
lx              numeric   Survivors to age x
dx              numeric   Deaths between x and x+1
Lx              numeric   Person-years lived
Tx              numeric   Total future person-years
ex              numeric   Life expectancy at age x
```

### 9.2 Key Differences from Fertility

| Aspect | Fertility | Mortality |
|--------|-----------|-----------|
| Complexity | Single dimension (age) | Multiple dimensions (age, sex, cause) |
| Age range | 14-49 | 0-119 |
| Smoothing | None | Whittaker-Henderson |
| Special cases | None | Ages 0, 1, 100+ require special handling |
| Data sources | 2 (NCHS births, Census pop) | 4+ (NCHS deaths, Census pop, CMS, standard pop) |
| Projection method | Cohort-based TFR target | Cause-specific reduction rates |

### 9.3 Potential Simplifications

Given data availability constraints, consider these simplifications for initial implementation:

1. **Use existing TR2025 qx as starting point** for validation, then work backward to understand the methodology
2. **Skip marital status differentials** initially (Phase 2D.4 can be deferred)
3. **Use NCHS data for all ages** (not CMS Medicare data for 65+) and calibrate to TR2025 outputs
4. **Simplify infant mortality** by using total m0 rather than detailed age-in-days calculation

### 9.4 Whittaker-Henderson Smoothing Implementation

Reference implementation available at: http://www.howardfamily.ca/graduation/index.html

In R, the smoothing can be implemented using matrix operations or the `demography` package.

---

## Appendix A: Ultimate AAx Values (TR2025 Intermediate)

From documentation Appendix 1.2-1:

| Age Group | Cause | Ultimate AAx (%) |
|-----------|-------|-----------------|
| Under 15 | CVD | 1.9 |
| Under 15 | Cancer | 1.5 |
| Under 15 | Accidents | 1.0 |
| Under 15 | Respiratory | 2.0 |
| Under 15 | Dementia | 0.1 |
| Under 15 | Other | 1.7 |
| 15-49 | CVD | 1.3 |
| 15-49 | Cancer | 1.5 |
| 15-49 | Accidents | 0.7 |
| 15-49 | Respiratory | 0.5 |
| 15-49 | Dementia | 0.1 |
| 15-49 | Other | 0.8 |
| 50-64 | CVD | 1.5 |
| 50-64 | Cancer | 1.5 |
| 50-64 | Accidents | 0.5 |
| 50-64 | Respiratory | 0.7 |
| 50-64 | Dementia | 0.1 |
| 50-64 | Other | 0.6 |
| 65-84 | CVD | 1.9 |
| 65-84 | Cancer | 0.9 |
| 65-84 | Accidents | 0.5 |
| 65-84 | Respiratory | 0.3 |
| 65-84 | Dementia | 0.1 |
| 65-84 | Other | 0.3 |
| 85+ | CVD | 1.5 |
| 85+ | Cancer | 0.5 |
| 85+ | Accidents | 0.3 |
| 85+ | Respiratory | 0.2 |
| 85+ | Dementia | 0.1 |
| 85+ | Other | 0.3 |

---

## Appendix B: Data Source URLs

### B.1 NCHS Mortality Data
- Multiple Cause of Death files: https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm
- NCHS WONDER: https://wonder.cdc.gov/mcd.html
- NBER mortality files: https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data

### B.2 Census Population Data
- Population Estimates Program: https://www.census.gov/programs-surveys/popest.html
- 2010 Census data: https://www.census.gov/data/tables/2010/dec/density-data-text.html

### B.3 SSA Actuarial References
- Actuarial Study 120 (Life Tables): https://www.ssa.gov/OACT/NOTES/s2000s.html
- Trustees Report: https://www.ssa.gov/oact/TR/

---

## Appendix C: Comparison with Fertility Subprocess

| Feature | Fertility | Mortality |
|---------|-----------|-----------|
| Primary output | Birth rates (bx) | Death probabilities (qx) |
| Reference year method | Cohort targeting | Percentage reduction |
| Ultimate assumption | TFR = 1.90 | Cause-specific AAx |
| Smoothing | None | Whittaker-Henderson |
| Lines of code (est.) | ~400 | ~1000-1500 |
| Implementation effort | Medium | High |

---

*End of Implementation Plan*

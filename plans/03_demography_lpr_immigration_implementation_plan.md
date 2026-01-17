# ARTEMIS: OASDI Projection Model Implementation Plan
## Phase 3: Demography Process - LPR Immigration Subprocess (1.3)

**Document Version:** 1.0
**Date:** January 2026
**Scope:** LPR Immigration and Legal Emigration subprocess implementation following mortality completion
**Source:** SSA 2025 Long-Range Model Documentation, Section 1.3 LPR Immigration

---

## Progress Tracking

> **IMPORTANT**: This plan document should be updated as implementation progresses. Each task should be marked with its current status.

### Status Legend
- [ ] Not started
- [~] In progress
- [x] Completed

### Current Status
**Last Updated:** January 16, 2026
**Current Phase:** 3A (Complete - core tasks)
**Completed:** 3A.1-3A.3, 3A.6 (core data acquisition complete)
**Pending:** 3A.4-3A.5 (low priority), 3B-3F

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

The LPR IMMIGRATION subprocess produces estimates of:
- **LPR Immigration (L)**: Persons granted lawful permanent resident status
- **Legal Emigration (E)**: LPR immigrants and U.S. citizens departing the Social Security area population
- **Net LPR Immigration (NL)**: L - E

For each year z of the projection period, outputs are produced by age (x) and sex (s).

### 1.2 Key Concepts

| Term | Definition |
|------|------------|
| LPR | Lawful Permanent Resident |
| NEW | New arrivals - persons living abroad granted LPR visa and entering through port of entry |
| AOS | Adjustments of Status - persons already in US (temporary/unlawful) approved for LPR status |
| Refugee/Asylee | Treated as new arrivals in OCACT model (though technically AOS in DHS data) |
| IRCA | Immigration Reform and Control Act of 1986 (one-time legalizations) |
| Beers Formula | Interpolation method to distribute 5-year age groups to single years |

### 1.3 Two Paths to LPR Status

1. **New Arrivals (NEW)**
   - Persons living abroad granted LPR visa
   - Enter U.S. through port of entry
   - Includes refugees and asylees granted LPR status

2. **Adjustments of Status (AOS)**
   - Persons already residing in U.S.
   - Previously temporary or unlawfully present immigrants
   - Application for adjustment to LPR status approved by DHS

### 1.4 Relationship to Other Subprocesses

| Subprocess | Interaction |
|------------|-------------|
| Population Projections (1.8) | Provides net legal immigration component |
| Temporary/Unlawful Immigration (1.5) | AOS transfers people from this population |
| Mortality (1.2) | Immigrants subject to mortality rates |

---

## 2. Mathematical Framework

### 2.1 Primary Equations

**Equation 1.3.1 - New Arrivals:**
$$NEW_{x,s}^z = NEW_{x,s}^z(\cdot)$$

**Equation 1.3.2 - Adjustments of Status:**
$$AOS_{x,s}^z = AOS_{x,s}^z(\cdot)$$

**Equation 1.3.3 - Total LPR Immigration:**
$$L_{x,s}^z = NEW_{x,s}^z + AOS_{x,s}^z$$

**Equation 1.3.4 - Legal Emigration:**
$$E_{x,s}^z = E_{x,s}^z(\cdot)$$

**Equation 1.3.5 - Net LPR Immigration:**
$$NL_{x,s}^z = L_{x,s}^z - E_{x,s}^z$$

### 2.2 Age-Sex Distribution Application

For new arrivals:
$$NEW_{x,s}^z = NEW^z \cdot DIST_{x,s}^{NEW}$$

Where:
- $NEW^z$ = Trustees' assumed total new arrivals for year z
- $DIST_{x,s}^{NEW}$ = Age-sex distribution for new arrivals (from 2016-2020 average)

Similarly for AOS:
$$AOS_{x,s}^z = AOS^z \cdot DIST_{x,s}^{AOS}$$

### 2.3 Legal Emigration Calculation

$$E_{x,s}^z = 0.25 \times \left(\sum_{s=m}^{f} \sum_{x=0}^{100} L_{x,s}^z\right) \cdot EDIST_{x,s}$$

Where:
- 0.25 = Emigration ratio (25% of LPR immigration per TR2025)
- $EDIST_{x,s}$ = Emigration distribution by age and sex (from Census, using Beers interpolation)

### 2.4 Beers Interpolation Formula

The Beers ordinary formula distributes 5-year age group values to single years of age:

For a 5-year age group centered on ages $5n$ to $5n+4$, the single-year values are:

$$y_{5n+j} = \sum_{k=-2}^{2} c_{j,k} \cdot Y_{n+k}$$

Where:
- $Y_n$ = 5-year age group value
- $c_{j,k}$ = Beers coefficients (standard actuarial table)
- $j$ = position within 5-year group (0-4)

---

## 3. Input Data Requirements

### 3.1 Data Sources Summary

| Data | Source | Years | Method | Priority |
|------|--------|-------|--------|----------|
| LPR immigration by age, sex, class | DHS | 1973-2023 | File download | High |
| Historical LPR (5-year age groups) | DHS | 1941-1972 | Static file | Low |
| IRCA legalizations | DHS | 1989-2023 | Static file | Medium |
| Adjustments of status totals | DHS | 1963-1972 | Static file | Low |
| Legal emigration distribution | Census | 1990 | Static file | High |
| Emigration conversion factors | OCACT | - | Static file | High |

### 3.2 DHS LPR Immigration Data (Primary Source)

**Source:** Department of Homeland Security, Office of Immigration Statistics
**URL:** https://www.dhs.gov/immigration-statistics/yearbook

**Data for 1973-2023:**
- Single year of age (0-99, unknown)
- Sex (male, female, unknown)
- Class of admission:
  - New Arrival
  - Adjustment of Status
  - Refugee
  - Asylee
- Fiscal year of entry

**Note:** DHS reports fiscal year data (Oct 1 - Sep 30). Conversion to calendar year required.

**Data for 1941-1972:**
- 5-year age groups (0-4, 5-9, ..., 80-84)
- Sex
- Will not be updated

### 3.3 IRCA Legalizations Data

**Source:** DHS
**Years:** 1989-2023 (legacy applications processing)
**Types:**
- Pre-1982s (persons residing in US since before January 1, 1982)
- SAWs (Special Agricultural Workers)

**Variables:**
- Single year of age (0-99, unknown)
- Sex (including unknown)
- Month of legalization

**Note:** This data will not be updated (IRCA was a one-time program).

### 3.4 Legal Emigration Distribution

**Source:** U.S. Census Bureau (unpublished estimates to OCACT)
**Basis:** Change between 1980 and 1990 censuses
**Format:**
- 5-year age groups (0-4, 5-9, ..., 80-84)
- Sex

**Conversion Factors:**
- Developed by OCACT
- Adjust Census emigration estimates to Social Security area population
- Account for emigrants who weren't in SS area population

### 3.5 Trustees Assumptions (TR2025 Intermediate)

| Parameter | 2024 | 2025-26 | Ultimate (2027+) |
|-----------|------|---------|------------------|
| Total LPR Immigration | 1,263,000 | 1,213,000 | 1,050,000 |
| Legal Emigration | 315,750 | 303,250 | 262,500 |
| Emigration Ratio | 25% | 25% | 25% |

### 3.6 Available Validation Data (from TR2025)

Check `data/raw/SSA_TR2025/` for:
- Population projections including immigration components
- Historical immigration estimates
- Net immigration by age and sex

---

## 4. Projection Methodology

### 4.1 Step-by-Step Process

**Step 1:** Download and process DHS LPR immigration data (1973-2023)

**Step 2:** Reclassify refugees and asylees from AOS to new arrivals

**Step 3:** Convert fiscal year data to calendar year data

**Step 4:** Calculate age-sex distributions for NEW and AOS using 2016-2020 data

**Step 5:** Handle unknown ages and sexes (prorate or impute)

**Step 6:** Apply distributions to Trustees' aggregate assumptions for projection years

**Step 7:** Process legal emigration Census data with conversion factors

**Step 8:** Apply Beers interpolation to convert 5-year groups to single years

**Step 9:** Calculate legal emigration as 25% of LPR immigration, distributed by age-sex

**Step 10:** Calculate net LPR immigration (NL = L - E)

### 4.2 Fiscal-to-Calendar Year Conversion

DHS reports fiscal year data (October 1 - September 30). To convert to calendar year:

$$CY_z = 0.25 \times FY_z + 0.75 \times FY_{z+1}$$

Or equivalently, weight Q4 of prior fiscal year with Q1-Q3 of current fiscal year.

**Alternative approach (if quarterly data unavailable):**
Use simple average: $CY_z = 0.5 \times (FY_z + FY_{z+1})$

### 4.3 Refugee/Asylee Reclassification

DHS classifies refugees and asylees as AOS when they adjust to LPR status (typically 1 year after admission). OCACT treats them as new arrivals because:
- They entered the US as new arrivals (not already present)
- Their entry timing matters for population projections

**Process:**
1. Identify refugee/asylee records in DHS data
2. Remove from AOS category
3. Add to NEW category

### 4.4 Age-Sex Distribution Calculation

**Years used:** 2016-2020 (5 most recent pre-COVID years)

For each admission class c (NEW or AOS):
$$DIST_{x,s}^c = \frac{\sum_{z=2016}^{2020} N_{x,s,c}^z}{\sum_{z=2016}^{2020} \sum_x \sum_s N_{x,s,c}^z}$$

Where $N_{x,s,c}^z$ is the count of immigrants age x, sex s, class c in year z.

### 4.5 Beers Interpolation Implementation

Standard Beers ordinary coefficients for distributing 5-year totals to single years:

| Position | n-2 | n-1 | n | n+1 | n+2 |
|----------|-----|-----|---|-----|-----|
| 0 | 0.0016 | -0.0832 | 0.6096 | 0.5936 | -0.1216 |
| 1 | -0.0336 | 0.0848 | 0.3744 | 0.6784 | -0.1040 |
| 2 | -0.0416 | 0.1136 | 0.2464 | 0.7216 | -0.0400 |
| 3 | -0.0240 | 0.0816 | 0.1840 | 0.7200 | 0.0384 |
| 4 | 0.0016 | 0.0128 | 0.1744 | 0.6816 | 0.1296 |

Special handling required for:
- First two age groups (0-4, 5-9)
- Last two age groups (75-79, 80-84)

---

## 5. Implementation Functions

### 5.1 Data Acquisition Functions

File: `R/data_acquisition/dhs_immigration.R`

```r
#' Fetch DHS LPR immigration data
#'
#' @description
#' Downloads and processes DHS Yearbook of Immigration Statistics data
#' on lawful permanent residents by age, sex, and class of admission.
#'
#' @param years Integer vector of fiscal years to fetch (1973-2023)
#' @param cache_dir Directory for caching downloaded files
#'
#' @return data.table with columns: fiscal_year, age, sex, class_of_admission, count
#'
#' @details
#' DHS data available at: https://www.dhs.gov/immigration-statistics/yearbook
#' Data provided in Excel format with varying structures by year.
#'
#' @export
fetch_dhs_lpr_immigration <- function(years = 1973:2023,
                                       cache_dir = "data/cache/dhs_immigration") {
  # Implementation:
  # 1. Check for cached data
  # 2. Download DHS yearbook tables
  # 3. Parse Excel files (structure varies by year)
  # 4. Standardize column names and categories
  # 5. Handle unknown ages and sexes
  # 6. Cache and return results
}

#' Load historical DHS immigration (1941-1972)
#'
#' @description
#' Loads pre-1973 immigration data with 5-year age groups.
#' This data is static and stored locally.
#'
#' @return data.table with columns: fiscal_year, age_group, sex, count
#'
#' @export
load_historical_dhs_immigration <- function() {
  # Load from static file: data/raw/dhs_historical_1941_1972.csv
}

#' Load IRCA legalization data
#'
#' @description
#' Loads Immigration Reform and Control Act legalization data (1989-2023).
#'
#' @return data.table with columns: year, month, age, sex, type, count
#'
#' @export
load_irca_legalizations <- function() {
  # Load from static file: data/raw/dhs_irca_legalizations.csv
}
```

File: `R/data_acquisition/census_emigration.R`

```r
#' Load Census Bureau legal emigration estimates
#'
#' @description
#' Loads unpublished Census estimates of legal emigration by age and sex
#' based on change between 1980 and 1990 censuses.
#'
#' @return data.table with columns: age_group, sex, emigration_count
#'
#' @details
#' These estimates represent people leaving the United States,
#' not specifically the Social Security area. Conversion factors
#' must be applied separately.
#'
#' @export
load_census_emigration_estimates <- function() {
  # Load from static file: data/raw/census_emigration_1990.csv
}

#' Load emigration conversion factors
#'
#' @description
#' Loads OCACT conversion factors that adjust Census emigration
#' to Social Security area emigration.
#'
#' @return data.table with columns: age_group, sex, conversion_factor
#'
#' @export
load_emigration_conversion_factors <- function() {
  # Load from static file: data/raw/ocact_emigration_factors.csv
}
```

### 5.2 Core LPR Immigration Functions

File: `R/demography/lpr_immigration.R`

```r
#' Reclassify refugees and asylees as new arrivals
#'
#' @description
#' Moves refugee and asylee LPR admissions from the adjustment of status
#' category to the new arrival category per OCACT methodology.
#'
#' @param lpr_data data.table with class_of_admission column
#'
#' @return data.table with reclassified admission categories
#'
#' @export
reclassify_refugees_asylees <- function(lpr_data) {
  # Implementation:
  # 1. Identify records where class_of_admission in c("Refugee", "Asylee")
  # 2. Change class_of_admission to "New Arrival"
  # 3. Return modified data
}

#' Convert fiscal year to calendar year
#'
#' @description
#' Converts DHS fiscal year data (Oct-Sep) to calendar year data (Jan-Dec).
#'
#' @param lpr_data data.table with fiscal_year column
#' @param method Character: "weighted" (0.25/0.75 split) or "simple" (average)
#'
#' @return data.table with calendar_year column replacing fiscal_year
#'
#' @details
#' Fiscal year z runs from Oct 1 of year z-1 to Sep 30 of year z.
#' Calendar year z uses:
#'   - Q4 of fiscal year z (Oct-Dec of calendar z-1) - weight 0.25
#'   - Q1-Q3 of fiscal year z+1 (Jan-Sep of calendar z) - weight 0.75
#'
#' @export
convert_fiscal_to_calendar_year <- function(lpr_data, method = "weighted") {
  # Implementation based on method
}

#' Calculate LPR immigration age-sex distribution
#'
#' @description
#' Calculates the age-sex distribution for new arrivals and adjustments
#' of status using specified reference years.
#'
#' @param lpr_data data.table with calendar year LPR data
#' @param reference_years Integer vector of years to use (default: 2016:2020)
#' @param admission_class Character: "new_arrival" or "adjustment_of_status"
#'
#' @return data.table with columns: age, sex, distribution
#'   where distribution sums to 1.0
#'
#' @export
calculate_lpr_age_sex_distribution <- function(lpr_data,
                                                reference_years = 2016:2020,
                                                admission_class) {
  # Implementation:
  # 1. Filter to reference years and admission class
  # 2. Aggregate counts by age and sex
  # 3. Calculate proportions
  # 4. Return distribution
}

#' Handle unknown ages and sexes
#'
#' @description
#' Prorates counts with unknown age or sex across known categories.
#'
#' @param lpr_data data.table with potential unknown values
#' @param age_var Character: name of age column
#' @param sex_var Character: name of sex column
#'
#' @return data.table with unknowns distributed
#'
#' @export
distribute_unknown_demographics <- function(lpr_data,
                                            age_var = "age",
                                            sex_var = "sex") {
  # Implementation:
  # 1. Calculate distribution of known values
  # 2. Prorate unknown counts across known categories
  # 3. Remove unknown rows
  # 4. Return adjusted data
}
```

### 5.3 Legal Emigration Functions

File: `R/demography/legal_emigration.R`

```r
#' Calculate Beers interpolation coefficients
#'
#' @description
#' Returns the standard Beers ordinary interpolation coefficients
#' for distributing 5-year age group data to single years.
#'
#' @return Matrix of Beers coefficients (5 rows x 5 columns)
#'
#' @details
#' Beers ordinary formula coefficients for interior age groups.
#' Special handling needed for boundary groups.
#'
#' @export
get_beers_coefficients <- function() {
  # Return standard Beers coefficient matrix
  matrix(c(
    0.0016, -0.0832, 0.6096, 0.5936, -0.1216,
   -0.0336,  0.0848, 0.3744, 0.6784, -0.1040,
   -0.0416,  0.1136, 0.2464, 0.7216, -0.0400,
   -0.0240,  0.0816, 0.1840, 0.7200,  0.0384,
    0.0016,  0.0128, 0.1744, 0.6816,  0.1296
  ), nrow = 5, ncol = 5, byrow = TRUE)
}

#' Apply Beers interpolation to 5-year age groups
#'
#' @description
#' Converts 5-year age group data to single year of age using
#' Beers ordinary interpolation formula.
#'
#' @param data data.table with columns: age_group, value
#'   age_group format: "0-4", "5-9", ..., "80-84"
#' @param value_col Character: name of column to interpolate
#'
#' @return data.table with columns: age, value (single year of age)
#'
#' @details
#' Handles boundary age groups (0-4, 5-9, 75-79, 80-84) specially.
#' Ages 85+ are set to 0 or handled separately.
#'
#' @export
beers_interpolate <- function(data, value_col = "value") {
  # Implementation:
  # 1. Parse age groups to numeric ranges

  # 2. Apply Beers coefficients for interior groups
  # 3. Handle boundary groups specially
  # 4. Return single-year data
}

#' Calculate legal emigration distribution
#'
#' @description
#' Processes Census emigration estimates and conversion factors
#' to create an age-sex distribution for emigration projections.
#'
#' @param census_emigration data.table from load_census_emigration_estimates()
#' @param conversion_factors data.table from load_emigration_conversion_factors()
#'
#' @return data.table with columns: age, sex, distribution (sums to 1.0)
#'
#' @export
calculate_emigration_distribution <- function(census_emigration,
                                               conversion_factors) {
  # Implementation:
  # 1. Join emigration with conversion factors
  # 2. Apply conversion: adjusted = emigration * conversion_factor
  # 3. Apply Beers interpolation to get single years
  # 4. Normalize to distribution
}

#' Project legal emigration
#'
#' @description
#' Projects legal emigration by age and sex based on LPR immigration
#' and the configured emigration ratio.
#'
#' @param lpr_immigration data.table with projected LPR immigration by age/sex
#' @param emigration_distribution data.table with age-sex distribution
#' @param emigration_ratio Numeric: ratio of emigration to immigration (default: 0.25)
#'
#' @return data.table with columns: year, age, sex, emigration
#'
#' @details
#' Formula: E_x,s^z = ratio * sum(L) * EDIST_x,s
#'
#' @export
project_legal_emigration <- function(lpr_immigration,
                                     emigration_distribution,
                                     emigration_ratio = 0.25) {
  # Implementation:
  # 1. Calculate total LPR immigration by year
  # 2. Apply emigration ratio
  # 3. Distribute by age-sex using distribution
}
```

### 5.4 Projection Functions

File: `R/demography/lpr_immigration.R` (continued)

```r
#' Apply age-sex distribution to immigration totals
#'
#' @description
#' Distributes Trustees' aggregate immigration assumptions
#' by age and sex using calculated distributions.
#'
#' @param total_new_arrivals Numeric vector: total new arrivals by year
#' @param total_aos Numeric vector: total AOS by year
#' @param new_arrival_dist data.table: age-sex distribution for new arrivals
#' @param aos_dist data.table: age-sex distribution for AOS
#' @param years Integer vector: projection years
#'
#' @return data.table with columns: year, age, sex, new_arrivals, aos, total_lpr
#'
#' @export
apply_immigration_distributions <- function(total_new_arrivals,
                                            total_aos,
                                            new_arrival_dist,
                                            aos_dist,
                                            years) {
  # Implementation:
  # 1. For each year, multiply totals by distributions
  # 2. Calculate total LPR = new arrivals + AOS
  # 3. Return combined data
}

#' Run LPR immigration projection
#'
#' @description
#' Main function to run the complete LPR immigration projection.
#' Orchestrates all steps of the methodology.
#'
#' @param dhs_data data.table: historical DHS LPR data
#' @param census_emigration data.table: Census emigration estimates
#' @param conversion_factors data.table: emigration conversion factors
#' @param config list: configuration with Trustees assumptions
#' @param years Integer vector: projection years (default: 2025:2099)
#'
#' @return list with:
#'   - lpr_immigration: data.table by year, age, sex
#'   - legal_emigration: data.table by year, age, sex
#'   - net_lpr: data.table by year, age, sex
#'   - new_arrivals: data.table by year, age, sex
#'   - adjustments_of_status: data.table by year, age, sex
#'
#' @export
run_lpr_immigration_projection <- function(dhs_data,
                                           census_emigration,
                                           conversion_factors,
                                           config,
                                           years = 2025:2099) {
  # Implementation:
  # 1. Reclassify refugees/asylees
  # 2. Convert fiscal to calendar year
  # 3. Calculate age-sex distributions (2016-2020)
  # 4. Get Trustees assumptions from config
  # 5. Apply distributions to assumptions
  # 6. Calculate emigration distribution
  # 7. Project emigration
  # 8. Calculate net LPR
  # 9. Return all components
}

#' Get Trustees LPR immigration assumptions
#'
#' @description
#' Returns the assumed total LPR immigration and legal emigration
#' for each projection year based on Trustees Report assumptions.
#'
#' @param config list: configuration with immigration assumptions
#' @param years Integer vector: years to get assumptions for
#'
#' @return data.table with columns: year, total_lpr, total_emigration,
#'   new_arrivals_share, aos_share
#'
#' @details
#' TR2025 intermediate assumptions:
#' - 2024: 1,263,000 LPR immigration
#' - 2025-26: 1,213,000 LPR immigration
#' - 2027+: 1,050,000 LPR immigration (ultimate)
#' - Emigration = 25% of immigration
#'
#' @export
get_trustees_lpr_assumptions <- function(config, years) {
  # Implementation based on config values
}
```

---

## 6. Configuration

### 6.1 LPR Immigration Configuration (add to tr2025.yaml)

```yaml
lpr_immigration:
  # Projection parameters
  projection_start_year: 2025
  reference_years: [2016, 2017, 2018, 2019, 2020]  # For age-sex distributions

  # Trustees assumptions (TR2025 Intermediate)
  total_lpr_immigration:
    2024: 1263000
    2025: 1213000
    2026: 1213000
    ultimate: 1050000
    ultimate_year: 2027

  # Emigration parameters
  emigration_ratio: 0.25  # Legal emigration = 25% of LPR immigration

  # Share between NEW and AOS (to be calculated from data)
  # If not specified, calculated from reference years
  new_arrival_share: null
  aos_share: null

  # Age range
  min_age: 0
  max_age: 99

  # Fiscal to calendar year conversion
  fiscal_to_calendar_method: "weighted"  # or "simple"

legal_emigration:
  # Emigration is derived from LPR immigration
  ratio_to_lpr: 0.25

  # Distribution based on Census 1980-1990 change
  distribution_source: "census_1990"

  # Conversion factors applied
  apply_ss_area_conversion: true
```

---

## 7. Validation Framework

### 7.1 Validation Data Sources

Check available data in `data/raw/SSA_TR2025/`:
- Population projection files may contain immigration components
- Historical immigration estimates
- Look for net immigration by age and sex

### 7.2 Validation Points

| Metric | Source | Tolerance |
|--------|--------|-----------|
| Total LPR immigration (annual) | Trustees assumptions | Exact match |
| Age-sex distribution shape | DHS historical data | Visual inspection |
| Net LPR immigration totals | TR summary tables | 1% relative |
| Emigration totals | Calculated as 25% of LPR | Exact match |

### 7.3 Validation Functions

File: `R/validation/validate_lpr_immigration.R`

```r
#' Validate LPR immigration outputs against TR2025
#'
#' @param lpr_projected data.table with projected LPR immigration
#' @param emigration_projected data.table with projected emigration
#' @param official_data list with official TR projections (if available)
#' @param tolerance Numeric: acceptable relative difference
#'
#' @return list with validation results
#'
#' @export
validate_lpr_immigration_outputs <- function(lpr_projected,
                                              emigration_projected,
                                              official_data = NULL,
                                              tolerance = 0.01) {
  # Implementation:
  # 1. Check total LPR matches Trustees assumptions
  # 2. Check emigration = 25% of LPR
  # 3. Validate age-sex distributions sum to 1
  # 4. Compare to official data if available
  # 5. Report discrepancies
}

#' Validate age-sex distribution properties
#'
#' @param distribution data.table with age, sex, distribution columns
#'
#' @return logical: TRUE if valid
#'
#' @export
validate_distribution <- function(distribution) {
  # Check:
  # 1. No negative values
  # 2. Sums to 1.0 (within tolerance)
  # 3. All ages 0-99 present
  # 4. Both sexes present
}
```

---

## 8. Implementation Sequence

### Phase 3A: Data Acquisition - DHS Immigration Data

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 3A.1 | Research DHS data availability and format | None | Data assessment |
| [x] | 3A.2 | Create DHS LPR data downloader/parser | 3A.1 | dhs_immigration.R |
| [x] | 3A.3 | Download DHS LPR data (2006-2023) | 3A.2 | Cached data (18 years, 18.8M records) |
| [ ] | 3A.4 | Create historical data loader (1941-1972) | None | Static file (LOW PRIORITY) |
| [ ] | 3A.5 | Create IRCA legalizations loader | None | Static file (LOW PRIORITY) |
| [x] | 3A.6 | Validate downloaded data completeness | 3A.3 | Validation complete |

**Notes on 3A.3:**
- DHS expanded tables available 2006-2023 (earlier years have different format)
- Data successfully parsed for all 18 years
- Handles two Excel formats (combined sheets 2006-2022, separate sheets 2023+)
- Total LPR immigrants in dataset: 18,780,619

**Notes on 3A.4-3A.5 (LOW PRIORITY):**
- Historical data (1941-1972) and IRCA legalizations are not needed for projections
- Projection methodology uses 2016-2020 as reference years (which we have)
- These can be added later if needed for historical analysis

### Phase 3B: Data Acquisition - Emigration Data

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 3B.1 | Research Census emigration data availability | None | Data assessment |
| [ ] | 3B.2 | Create or obtain emigration estimates file | 3B.1 | census_emigration.R |
| [ ] | 3B.3 | Create or obtain conversion factors file | 3B.1 | Static file |
| [ ] | 3B.4 | Implement Beers interpolation function | None | legal_emigration.R |
| [ ] | 3B.5 | Test Beers interpolation against known values | 3B.4 | Test results |

### Phase 3C: Core Immigration Functions

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 3C.1 | Implement refugee/asylee reclassification | 3A.3 | lpr_immigration.R |
| [ ] | 3C.2 | Implement fiscal-to-calendar year conversion | 3C.1 | lpr_immigration.R |
| [ ] | 3C.3 | Implement age-sex distribution calculation | 3C.2 | lpr_immigration.R |
| [ ] | 3C.4 | Implement unknown demographics handling | 3C.3 | lpr_immigration.R |
| [ ] | 3C.5 | Implement Trustees assumptions loader | Config | lpr_immigration.R |
| [ ] | 3C.6 | Implement distribution application | 3C.3, 3C.5 | lpr_immigration.R |

### Phase 3D: Emigration Functions

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 3D.1 | Implement emigration distribution calculation | 3B.2-3B.4 | legal_emigration.R |
| [ ] | 3D.2 | Implement emigration projection | 3D.1 | legal_emigration.R |
| [ ] | 3D.3 | Implement net LPR calculation | 3C.6, 3D.2 | lpr_immigration.R |

### Phase 3E: Validation and Testing

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 3E.1 | Create validation functions | All above | validate_lpr_immigration.R |
| [ ] | 3E.2 | Validate against Trustees totals | 3E.1 | Validation report |
| [ ] | 3E.3 | Validate distribution properties | 3E.1 | Validation report |
| [ ] | 3E.4 | Create unit tests | All functions | test-lpr-immigration.R |
| [ ] | 3E.5 | Run full pipeline validation | All | Validated outputs |

### Phase 3F: Targets Integration

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 3F.1 | Add LPR immigration targets to _targets.R | All functions | Pipeline targets |
| [ ] | 3F.2 | Test full pipeline execution | 3F.1 | Working pipeline |
| [ ] | 3F.3 | Add configuration to tr2025.yaml | None | Updated config |
| [ ] | 3F.4 | Documentation and cleanup | All | Documented code |

---

## 9. Technical Specifications

### 9.1 Data Structures

**DHS LPR Immigration Data:**
```
Column              Type      Description
------              ----      -----------
fiscal_year         integer   Fiscal year (Oct-Sep)
calendar_year       integer   Calendar year (Jan-Dec), derived
age                 integer   Single year of age (0-99)
sex                 character "male" or "female"
class_of_admission  character "new_arrival", "aos", "refugee", "asylee"
count               numeric   Number of immigrants
```

**Age-Sex Distribution:**
```
Column          Type      Description
------          ----      -----------
age             integer   Single year of age (0-99)
sex             character "male" or "female"
distribution    numeric   Proportion (sums to 1.0)
```

**Projected LPR Immigration:**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year (2025-2099)
age             integer   Single year of age (0-99)
sex             character "male" or "female"
new_arrivals    numeric   Number of new arrivals
aos             numeric   Number of adjustments of status
total_lpr       numeric   Total LPR = new_arrivals + aos
```

**Projected Legal Emigration:**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year (2025-2099)
age             integer   Single year of age (0-99)
sex             character "male" or "female"
emigration      numeric   Number of emigrants
```

**Net LPR Immigration:**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year (2025-2099)
age             integer   Single year of age (0-99)
sex             character "male" or "female"
net_lpr         numeric   LPR immigration - emigration
```

### 9.2 Key Differences from Previous Subprocesses

| Aspect | Fertility | Mortality | LPR Immigration |
|--------|-----------|-----------|-----------------|
| Complexity | Single sex | Both sexes | Both sexes |
| Age range | 14-49 | 0-119 | 0-99 |
| Time basis | Calendar year | Calendar year | Fiscal year (converted) |
| Data sources | 2 (NCHS, Census) | 3 (NCHS, Census, HMD) | 3 (DHS, Census, OCACT) |
| Distribution method | Trend-based | Cause-specific | Fixed proportions |
| Key calculation | Cohort targeting | AAx reduction rates | Beers interpolation |

### 9.3 Potential Challenges

1. **DHS Data Access:** DHS yearbook data format varies by year; may require year-specific parsers

2. **Census Emigration Data:** Unpublished estimates; may need to approximate or use alternative sources

3. **Conversion Factors:** OCACT internal factors; may need to derive from other sources or use simplifying assumptions

4. **IRCA Data:** One-time program data; may be difficult to obtain in machine-readable format

### 9.4 Simplification Options

If data acquisition proves difficult:

1. **Use TR2025 distributions directly:** If available in raw data files, use official age-sex distributions instead of calculating from DHS data

2. **Skip IRCA:** IRCA legalizations are historical and may not materially affect projections

3. **Approximate emigration factors:** Use ratio of 0.9-1.0 as conversion factor if official factors unavailable

4. **Use simple fiscal-to-calendar conversion:** Average method instead of weighted quarterly method

---

## Appendix A: Beers Interpolation Reference

### A.1 Beers Ordinary Coefficients (Complete Table)

For interior 5-year groups (not first two or last two):

| Single Year | Group n-2 | Group n-1 | Group n | Group n+1 | Group n+2 |
|-------------|-----------|-----------|---------|-----------|-----------|
| 5n + 0 | 0.0016 | -0.0832 | 0.6096 | 0.5936 | -0.1216 |
| 5n + 1 | -0.0336 | 0.0848 | 0.3744 | 0.6784 | -0.1040 |
| 5n + 2 | -0.0416 | 0.1136 | 0.2464 | 0.7216 | -0.0400 |
| 5n + 3 | -0.0240 | 0.0816 | 0.1840 | 0.7200 | 0.0384 |
| 5n + 4 | 0.0016 | 0.0128 | 0.1744 | 0.6816 | 0.1296 |

### A.2 Boundary Group Handling

**First group (ages 0-4):** Use modified coefficients or extrapolation
**Second group (ages 5-9):** Use modified coefficients
**Last two groups (75-79, 80-84):** Use modified coefficients or set ages 85+ to zero

---

## Appendix B: DHS Data Source URLs

### B.1 DHS Immigration Statistics

- Yearbook of Immigration Statistics: https://www.dhs.gov/immigration-statistics/yearbook
- LPR supplemental tables: https://www.dhs.gov/immigration-statistics/lawful-permanent-residents
- Historical tables: Available in yearbook archives

### B.2 Data Tables Needed

| Table | Description | Years |
|-------|-------------|-------|
| Table 10 | LPR by age, sex, class of admission | 1973-2023 |
| Historical Table | LPR by age group, sex | 1941-1972 |
| IRCA Tables | Legalization counts by age, sex, type | 1989-2023 |

---

## Appendix C: Census Emigration Data

### C.1 Data Origin

The Census emigration estimates provided to OCACT are based on:
- Comparison of 1980 and 1990 Census populations
- Adjusted for births, deaths, and immigration during the decade
- Residual represents net emigration

### C.2 Alternative Sources

If official Census estimates unavailable:
- UN Population Division emigration estimates
- ACS foreign-born population changes
- Academic demographic research estimates

---

*End of Implementation Plan*

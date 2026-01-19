# Implementation Plan: Mortality Differentials by Marital Status

**Date:** January 18, 2026
**Purpose:** Replace fabricated mortality differentials with real data calculated per TR2025 methodology
**Priority:** High - Required for accurate Phase 8 Projected Population calculations

---

## Problem Statement

The current implementation in `R/demography/mortality.R` uses fabricated formulas for mortality differentials by marital status:

```r
# Current fabricated code in get_default_marital_factors() (lines 1989-2041)
sex_mult <- if (sex_val == "male") 1.0 else 0.85
age_factor <- 1.0 + 0.3 * pmax(0, (65 - age_val)) / 50
widowed_base <- 1.15 + 0.10 * sex_mult
divorced_base <- 1.20 + 0.15 * sex_mult
never_married_base <- 1.25 + 0.20 * sex_mult
```

These arbitrary formulas do not match the TR2025 methodology and should be replaced with factors calculated from actual NCHS deaths and ACS population data.

---

## TR2025 Methodology

From **2025_LR_Model_Documentation_Demography_2_Mortality.pdf**, page 7:

> "In addition, probabilities of death are broken down further into marital status. Historical data indicate that differential in mortality by marital status is significant. To reflect this, future relative differences in death rates by marital status are projected to be the same as observed during calendar years 2015-19."

### 7-Step Calculation Process

1. **Calculate preliminary death rates**: Take single year of age deaths from NCHS public use data by marital status and divide by equivalent population sums from ACS

2. **Adjust older ages**: Adjust older age populations and calculated death rates for consistency with prior ages

3. **Convergence at age 95**: Adjust older age death rates so all marital statuses gradually reach the same value at age 95

4. **Ages under 15**: Assign total death rates for all marital statuses (no differential for children)

5. **Whittaker-Henderson smoothing**: Smooth death rates by single year of age (15-94) using Whittaker-Henderson with degree=2 and smoothing parameter=0.01

6. **Age 15-20 adjustment**: Adjust non-single marital statuses for ages 15-20 to have the same ratio relative to single marital status as age 21

7. **Convert to probabilities**: Convert death rates to probabilities of death (qx). Ages 95+ use standard formulas

---

## Data Sources

### NCHS Deaths by Marital Status (Item 8 in TR2025)

- **Source**: NCHS public use mortality microdata from CDC FTP
- **Years**: 1979-2022 (marital status available from 1979)
- **Reference Period**: 2015-2019
- **URL**: `https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/`

**Field Positions**:

| Years | Position | Values |
|-------|----------|--------|
| 1979-2002 | 77 | 1=Never married, 2=Married, 3=Widowed, 4=Divorced, 8/9=Not stated |
| 2003-2022 | 84 | S=Never married, M=Married, W=Widowed, D=Divorced, U=Unknown |

### ACS PUMS Population by Marital Status (Item 19 in TR2025)

- **Source**: American Community Survey Public Use Microdata Sample
- **Years**: 2000-2019 (use 2015-2019 for reference period)
- **Existing Function**: `fetch_acs_pums_marital_status()` in `R/data_acquisition/acs_pums.R`

---

## Implementation Steps

### Step 1: Modify NCHS Deaths Fetcher

**File**: `R/data_acquisition/nchs_deaths.R`

#### 1.1 Update `get_mortality_file_layout()`

Add marital status field position to the layout specification:

```r
# For 2003+ files
list(
  detail_age = c(start = 70, end = 73),
  sex = c(start = 69, end = 69),
  ucod = c(start = 146, end = 149),
  marital_status = c(start = 84, end = 84),  # ADD THIS
  record_type = c(start = 1, end = 1)
)

# For 1979-2002 files
list(
  detail_age = c(start = 64, end = 66),
  sex = c(start = 59, end = 59),
  ucod = c(start = 142, end = 145),
  marital_status = c(start = 77, end = 77),  # ADD THIS
  record_type = c(start = 20, end = 20)
)
```

#### 1.2 Update `read_mortality_fixed_width()`

Extract marital status field along with other fields.

#### 1.3 Create New Function

```r
#' Fetch NCHS deaths by marital status
#'
#' @description
#' Downloads and processes NCHS mortality microdata to extract deaths
#' by single year of age, sex, and marital status. Used for calculating
#' mortality differentials per TR2025 methodology.
#'
#' @param years Integer vector of years (1979-2022)
#' @param cache_dir Character: cache directory
#'
#' @return data.table with columns: year, age, sex, marital_status, deaths
#'
#' @export
fetch_nchs_deaths_by_marital_status <- function(
    years = 2015:2019,
    cache_dir = here::here("data/cache/nchs_deaths")
)
```

### Step 2: Use Existing ACS PUMS Fetcher

**File**: `R/data_acquisition/acs_pums.R`

The function `fetch_acs_pums_marital_status()` already exists. Call it with:

```r
acs_pop <- fetch_acs_pums_marital_status(years = 2015:2019, ages = 15:94)
```

### Step 3: Create Mortality Differentials Calculator

**File**: `R/demography/mortality.R`

#### 3.1 Main Calculation Function

```r
#' Calculate marital status mortality factors from data
#'
#' @description
#' Calculates relative mortality factors by marital status using NCHS deaths
#' and ACS population data per TR2025 methodology (Section 1.2.c, Equation 1.2.3).
#'
#' @param nchs_deaths data.table with deaths by year, age, sex, marital_status
#' @param acs_population data.table with population by year, age, sex, marital_status
#' @param reference_years Years to use for calculation (default: 2015:2019)
#'
#' @return data.table with columns: age, sex, marital_status, relative_factor
#'
#' @details
#' Implements the 7-step TR2025 methodology:
#' 1. Calculate preliminary death rates (deaths / population)
#' 2. Adjust older age rates for consistency
#' 3. Converge all marital statuses to same rate at age 95
#' 4. Ages under 15 use total death rates
#' 5. Apply Whittaker-Henderson smoothing (degree=2, smoothing=0.01)
#' 6. Adjust ages 15-20 non-single to match age 21 ratio
#' 7. Convert to relative factors (married = 1.0 reference)
#'
#' @export
calculate_marital_mortality_factors <- function(
    nchs_deaths,
    acs_population,
    reference_years = 2015:2019
)
```

#### 3.2 Whittaker-Henderson Smoothing Function

```r
#' Apply Whittaker-Henderson smoothing
#'
#' @param values Numeric vector of values to smooth
#' @param degree Degree parameter (default: 2)
#' @param smoothing Smoothing parameter (default: 0.01)
#'
#' @return Numeric vector of smoothed values
#'
#' @keywords internal
whittaker_henderson_smooth <- function(values, degree = 2, smoothing = 0.01)
```

### Step 4: Replace Fabricated Function

#### 4.1 Replace `get_default_marital_factors()`

```r
#' Get marital status mortality factors
#'
#' @description
#' Returns relative mortality factors by marital status, age, and sex.
#' Calculated from NCHS deaths and ACS population data per TR2025 methodology.
#'
#' @param use_cache Logical: if TRUE, use cached factors if available
#' @param cache_dir Character: directory for cached factors
#'
#' @return data.table with columns: age, sex, marital_status, relative_factor
#'
#' @export
get_marital_mortality_factors <- function(
    use_cache = TRUE,
    cache_dir = here::here("data/cache/mortality")
) {
  cache_file <- file.path(cache_dir, "marital_mortality_factors.rds")

  if (use_cache && file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  # Fetch data and calculate
  nchs_deaths <- fetch_nchs_deaths_by_marital_status(years = 2015:2019)
  acs_pop <- fetch_acs_pums_marital_status(years = 2015:2019)

  factors <- calculate_marital_mortality_factors(nchs_deaths, acs_pop)

  # Cache results
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(factors, cache_file)

  factors
}
```

#### 4.2 Update `calculate_qx_by_marital_status()`

Change the default from fabricated factors to real factors:

```r
calculate_qx_by_marital_status <- function(qx_total, marital_factors = NULL) {
  if (is.null(marital_factors)) {
    marital_factors <- get_marital_mortality_factors()  # Use real factors
  }
  # ... rest of function unchanged
}
```

### Step 5: Add Pipeline Targets

**File**: `_targets.R`

```r
# Mortality marital status differentials (TR2025 Section 1.2.c)
tar_target(
  nchs_deaths_by_marital,
  fetch_nchs_deaths_by_marital_status(years = 2015:2019)
),

tar_target(
  acs_pop_by_marital,
  fetch_acs_pums_marital_status(years = 2015:2019, ages = 15:94)
),

tar_target(
  mortality_marital_factors,
  calculate_marital_mortality_factors(
    nchs_deaths = nchs_deaths_by_marital,
    acs_population = acs_pop_by_marital,
    reference_years = 2015:2019
  )
)
```

---

## Files to Modify

| File | Changes |
|------|---------|
| `R/data_acquisition/nchs_deaths.R` | Add marital status to layout, create `fetch_nchs_deaths_by_marital_status()` |
| `R/demography/mortality.R` | Add `calculate_marital_mortality_factors()`, `whittaker_henderson_smooth()`, replace `get_default_marital_factors()` with `get_marital_mortality_factors()` |
| `_targets.R` | Add targets for marital mortality data pipeline |

---

## Validation Criteria

### Expected Relative Factor Ranges

Based on mortality literature:

| Marital Status | Expected Range | Notes |
|----------------|----------------|-------|
| Married | 1.00 | Reference category |
| Widowed | 1.10 - 1.30 | Higher for males, younger ages |
| Divorced | 1.15 - 1.40 | Higher for males, younger ages |
| Never Married | 1.20 - 1.50 | Highest differential |

### Validation Checks

1. **Married = 1.0**: Verify married is the reference with factor = 1.0
2. **Relative ordering**: Never married > Divorced > Widowed > Married
3. **Sex differences**: Male differentials should be larger than female
4. **Age pattern**: Differentials should be larger at younger ages
5. **Convergence**: All factors should approach 1.0 by age 95
6. **Smoothness**: No erratic jumps in factors across ages
7. **Reasonableness**: Implied death rates should match observed rates within tolerance

---

## Implementation Sequence

| Step | Task | Status |
|------|------|--------|
| 1.1 | Update `get_mortality_file_layout()` with marital status | [ ] |
| 1.2 | Update `read_mortality_fixed_width()` to extract marital status | [ ] |
| 1.3 | Create `fetch_nchs_deaths_by_marital_status()` | [ ] |
| 1.4 | Test NCHS deaths fetcher for years 2015-2019 | [ ] |
| 2.1 | Test existing ACS PUMS fetcher for 2015-2019 | [ ] |
| 3.1 | Implement `whittaker_henderson_smooth()` | [ ] |
| 3.2 | Implement `calculate_marital_mortality_factors()` | [ ] |
| 3.3 | Test calculation against expected ranges | [ ] |
| 4.1 | Replace `get_default_marital_factors()` | [ ] |
| 4.2 | Update `calculate_qx_by_marital_status()` | [ ] |
| 5.1 | Add pipeline targets to `_targets.R` | [ ] |
| 5.2 | Run full validation | [ ] |

---

## References

- TR2025 Mortality Documentation: `documentation/2_Demography/2025_LR_Model_Documentation_Demography_2_Mortality.pdf`
- NCHS Record Layouts: `data/raw/nchs/mortality_documentation/`
- Whittaker-Henderson smoothing: http://www.howardfamily.ca/graduation/index.html

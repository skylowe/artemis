# ARTEMIS: OASDI Projection Model Implementation Plan
## Phase 7: Demography Process - Divorce Subprocess (1.7)

**Document Version:** 1.0
**Date:** January 2026
**Scope:** Divorce subprocess implementation following Marriage subprocess completion
**Source:** SSA 2025 Long-Range Model Documentation, Section 1.7 Divorce

---

## Progress Tracking

> **IMPORTANT**: This plan document should be updated as implementation progresses. Each task should be marked with its current status.

### Status Legend
- [ ] Not started
- [~] In progress
- [x] Completed

### Current Status
**Last Updated:** January 18, 2026
**Current Phase:** Phase 7C - COMPLETE
**Subprocess Status:** IN PROGRESS (Phase 7D/7E next)

### Critical Rule: Real Data Only
**No synthetic or mock data is permitted.** A task cannot be marked as completed until it is working with real data from actual data sources.

---

## Table of Contents
1. [Overview](#1-overview)
2. [Mathematical Framework](#2-mathematical-framework)
3. [Input Data Requirements](#3-input-data-requirements)
4. [Output Development Methodology](#4-output-development-methodology)
5. [Implementation Functions](#5-implementation-functions)
6. [Configuration](#6-configuration)
7. [Validation Framework](#7-validation-framework)
8. [Implementation Sequence](#8-implementation-sequence)
9. [Technical Specifications](#9-technical-specifications)

---

## 1. Overview

### 1.1 Subprocess Purpose

The DIVORCE subprocess projects annual age-specific divorce rates by age-of-husband crossed with age-of-wife for the Social Security area population. This subprocess is critical because it:

- Provides divorce rate projections for the PROJECTED POPULATION subprocess
- Enables projection of marital status distributions over the 75-year projection period
- Affects benefit calculations by determining transition from married to divorced status
- Impacts survivor benefit eligibility calculations

### 1.2 Key Terminology

| Term | Definition |
|------|------------|
| DRA | Divorce Registration Area (31 states + D.C. in 1988, ~48% of U.S. divorces) |
| DivGrid | 87×87 matrix of divorce rates by age-of-husband × age-of-wife (ages 14-100+) |
| d̂_{x,y}^z | Age-specific divorce rate for husband age x, wife age y, year z |
| ADR^z | Age-adjusted central divorce rate for year z |
| P_{x,y}^z | Married couples population at age-of-husband x and age-of-wife y |
| P_{x,y}^S | Standard population (averaged 2009-2010 married couples) |

### 1.3 Primary Outputs

| Output | Symbol | Equation | Description |
|--------|--------|----------|-------------|
| Age-Specific Divorce Rates | d̂_{x,y}^z | 1.7.1 | Divorce rates by age-of-husband × age-of-wife |
| Age-Adjusted Divorce Rate | ADR^z | 1.7.2 | Summary rate for entire population |

### 1.4 TR2025 Assumptions

| Parameter | Value | Notes |
|-----------|-------|-------|
| Ultimate ADR | 1,700 per 100,000 | Per 100,000 married couples |
| Ultimate Year | 25th projection year (2047) | Reached gradually |
| Age Range | 14-100+ | Both husband and wife |
| Starting Rate | 5-year weighted average | From historical data |

### 1.5 Data Periods

| Period | Data Source | Detail Level |
|--------|-------------|--------------|
| 1979-1988 | NCHS DRA | Single year ages (detailed) |
| 1989-2007 | Interpolated/Scaled | DivGrid scaled to totals |
| 2008-2022 | State data + ACS | 2011 state grid for adjustment |
| 2023+ | Projected | Based on ultimate ADR |

### 1.6 Key Differences from Marriage Subprocess

| Aspect | Marriage | Divorce |
|--------|----------|---------|
| Registration Area | MRA (~80% coverage) | DRA (~48% coverage) |
| Denominator | Unmarried population | Married couples |
| Ultimate Rate | 4,000 per 100,000 | 1,700 per 100,000 |
| State data years | N/A | 2009-2012 (18 states) |
| Standard population | July 1, 2010 unmarried | Avg 2009-2010 married |
| Grid name | MarGrid | DivGrid |

---

## 2. Mathematical Framework

### 2.1 Equation 1.7.1 – Age-Specific Divorce Rates

For each projection year z, age-specific divorce rates are calculated as:

$$d̂_{x,y}^z = d̂_{x,y}^z(·)$$

Where:
- x = age of husband (14-100+)
- y = age of wife (14-100+)
- The (·) notation indicates rates are derived from the scaling methodology below

**Historical Rate Calculation (Equation 1.7.3):**

$$d̂_{x,y}^z = \frac{D̂_{x,y}^z}{P_{x,y}^z}$$

Where:
- D̂_{x,y}^z = Number of divorces in year z for husband age x, wife age y (SS area adjusted)
- P_{x,y}^z = Married couples in the SS area with husband age x and wife age y

### 2.2 Equation 1.7.2 – Age-Adjusted Central Divorce Rate

$$ADR^z = \frac{\sum_{x,y} P_{x,y}^S \cdot d̂_{x,y}^z}{\sum_{x,y} P_{x,y}^S}$$

Where:
- P_{x,y}^S = Standard population (averaged 2009-2010 December 31 married couples)
- The numerator = expected number of divorces using current rates and standard population
- The denominator = total standard population of married couples

### 2.3 DivGrid Development

The base divorce grid (DivGrid) is an 87×87 matrix (ages 14-100+ for both husband and wife) developed from NCHS 1979-1988 data:

1. Calculate rates from NCHS divorce counts and married population
2. Average rates across 1979-1988
3. Apply H.S. Beers interpolation for single-year ages
4. Graduate using two-dimensional Whittaker-Henderson smoothing
5. Adjust using 2011 state data grid

### 2.4 Projection Methodology

**Step 1: Calculate Starting ADR**
- Use weighted average of last 5 historical years
- Weight more recent years more heavily

**Step 2: Project ADR to Ultimate**
- Starting ADR → Ultimate ADR (1,700) by year 25
- Annual rate of change decreases in absolute value as ultimate approaches
- Use asymptotic convergence formula

**Step 3: Scale DivGrid**
- For each projection year, proportionally scale DivGrid rates
- Ensure scaled rates produce the projected ADR when applied to standard population

### 2.5 Social Security Area Adjustment

For DRA data (which covers ~48% of U.S. divorces), the SS area total is estimated by:

1. Start with total U.S. divorces
2. Adjust for PR + VI divorces
3. Adjust for population differences between:
   - U.S. + PR + VI
   - Social Security area (includes overseas citizens, etc.)

---

## 3. Input Data Requirements

### 3.1 Data Sources Summary

The Divorce subprocess requires **11 distinct data inputs**:

| Category | Items | Priority |
|----------|-------|----------|
| Demography (From Previous Subprocesses) | 5 | High |
| Trustees Assumptions | 1 | High |
| NCHS Data | 3 | High |
| State Divorce Data | 1 | Medium |
| Census Bureau Data | 1 | Medium |

### 3.2 Long-Range OASDI Projection Data (From Previous Subprocesses)

| # | Data | Source | Years | Status |
|---|------|--------|-------|--------|
| 1 | SS area married couples by age-of-husband × age-of-wife | HISTORICAL POPULATION | 1978-2022 | ✓ Complete (Phase 7B) |
| 2 | Total July 1 SS area population | HISTORICAL POPULATION | 1979-2022 | ✓ Complete (Phase 7B) |
| 3 | Total July 1 U.S. resident + armed forces overseas | Census | 1979-2022 | ✓ Complete (Phase 7B) |
| 4 | Total July 1 PR + VI population | Census | 1988-2022 | ✓ Complete (Phase 7B) |
| 5 | Final historical year | HISTORICAL POPULATION | 2022 | ✓ Complete (Phase 4) |

**Notes:**
- Item 1 is the key denominator for rate calculations (married couples grid)
- Items 2-4 are used for specific years only: 1979-1988, 1998-2000, 2008-2022
- Standard population based on averaged 2009-2010 December 31 married grids from 2015 TR

### 3.3 Trustees Assumptions

| # | Data | Description | Status |
|---|------|-------------|--------|
| 6 | Ultimate ADR | 1,700 per 100,000 married couples, reached in year 25 | In config |

### 3.4 NCHS Data

| # | Data | Years | Detail | Status |
|---|------|-------|--------|--------|
| 7 | Divorces in DRA by age-of-husband × age-of-wife | 1979-1995 | Single year ages | ✓ Complete |
| 8 | Total divorces in U.S. | 1979-2022 | Annual totals | ✓ Complete |
| 9 | Total divorces in PR and VI | 1988, 1998-2000 | Annual totals | ✓ Complete (limited) |

**NCHS Notes:**
- DRA coverage: 42.6% (1979-1988) and 48.9% (1989-1995) of U.S. divorces
- Detailed age data available through 1995 from NBER archive
- Weight field at position 40-41 in NBER 117-char format (not position 8-9)
- From 1992+, total U.S. divorces derived from rate × population

### 3.5 State Divorce Data

| # | Data | Source | Years | Status |
|---|------|--------|-------|--------|
| 10 | State divorces by age-of-husband × age-of-wife | 18 state health depts | 2009-2012 | To research |

**18 States with Data Available:**
Alabama, Alaska, Idaho, Kansas, Kentucky, Michigan, Missouri, Montana, Nebraska, New Hampshire, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, Wyoming

**State Data Notes:**
- SSA directly contacted state health departments
- Years and age groups vary by state (generally 2009-2012)
- Used to develop 2011 age-group grid for DivGrid adjustment
- This data may be difficult to obtain for our implementation

### 3.6 Census Bureau Data

| # | Data | Source | Years | Status |
|---|------|--------|-------|--------|
| 11 | Divorces in Puerto Rico | ACS PUMS | 2008-2022 | To implement |

**ACS Notes:**
- Uses divorce variable from ACS PUMS
- 2020 excluded (COVID impact on ACS)
- Supplements PR/VI data from item 9

### 3.7 Data Availability Assessment

| Data Category | Public Availability | Notes |
|---------------|---------------------|-------|
| NCHS DRA data (1979-1988) | Medium | NBER archive (like marriage data) |
| NCHS total divorces | High | National Vital Statistics Reports |
| State divorce data (18 states) | Low | Direct contact with state health depts |
| ACS divorce data (PR) | High | ACS PUMS microdata |
| Married couples grid | Available | From Phase 4/6 marital status data |

### 3.8 Data Acquisition Priority

**Phase 7A - NCHS Historical Data (High Priority):**
- Item 7: NCHS DRA divorces (1979-1988) from NBER
- Item 8: Total U.S. divorces (1979-2022)
- Item 9: PR/VI divorces

**Phase 7B - Population Data (High Priority):**
- Items 2-4: Population totals for SS area adjustment
- Item 1: Married couples grid (already available)

**Phase 7C - State Data (Medium Priority):**
- Item 10: Research state data availability
- May need to implement alternative approach if unavailable

**Phase 7D - ACS Data (Medium Priority):**
- Item 11: ACS PUMS divorces for Puerto Rico

---

## 4. Output Development Methodology

### 4.1 Step-by-Step Process for DivGrid Development

**Step 1: Load NCHS 1979-1988 Divorce Data**

```
For years 1979-1988:
  1. Load divorces by age-of-husband × age-of-wife from DRA
  2. Inflate to SS area estimate using population ratios
  3. Load married couples population by age from HISTORICAL POPULATION
  4. Calculate rates: d̂_{x,y}^z = D̂_{x,y}^z / P_{x,y}^z
```

**Step 2: Average and Graduate Rates**

```
1. Average rates across 1979-1988
2. Use two-dimensional H.S. Beers to convert grouped ages to single years
3. Apply two-dimensional Whittaker-Henderson graduation for smoothing
4. Store as base DivGrid (87 × 87 matrix)
```

**Step 3: Adjust with 2011 State Data**

```
1. Load 2011 state data grid from 18 states
2. Create weighted average DivGrid:
   - Before 2011: Use 1988 DivGrid base
   - 2011+: Weighted average of 1988 DivGrid and 2011 state grid
3. State single-year grid derived by ratioing 1988 DivGrid using state age-group data
```

### 4.2 Step-by-Step Process for Historical Period (1989-2022)

**Step 1: Estimate SS Area Total Divorces**

```
For each year 1989-2022:
  1. Get total U.S. divorces from NCHS
  2. Add PR + VI divorces (from item 9 or ACS)
  3. Adjust for SS area vs U.S.+PR+VI population difference
  4. Result: estimated total SS area divorces
```

**Step 2: Scale DivGrid to Match Totals**

```
For each year 1989-2022:
  1. Calculate expected divorces from DivGrid × married population
  2. Calculate scaling factor: actual_divorces / expected_divorces
  3. Apply: d̂_{x,y}^z = scale × DivGrid_{x,y}
```

**Step 3: Calculate Historical ADR**

```
For each year:
  1. Apply scaled rates to standard population
  2. ADR^z = sum(P_{x,y}^S × d̂_{x,y}^z) / sum(P_{x,y}^S)
```

### 4.3 Step-by-Step Process for Projection Period (2023-2099)

**Step 1: Calculate Starting ADR**

```
starting_adr = weighted_average(ADR^{2018:2022})
  where weights favor more recent years
```

**Step 2: Project ADR to Ultimate**

```
ultimate_adr = 1700 (per 100,000 married couples)
ultimate_year = first_projection_year + 24 (year 25 = 2047)

For each projection year z:
  if z <= ultimate_year:
    ADR^z = interpolate(starting_adr, ultimate_adr, z, ultimate_year)
    # Rate of change decreases as ultimate approaches
  else:
    ADR^z = ultimate_adr
```

**Step 3: Scale DivGrid to Projected ADR**

```
For each projection year z:
  1. Calculate base expected divorces: base_divorces = sum(P_{x,y}^S × DivGrid)
  2. Calculate scaling factor: scale = (ADR^z × sum(P_{x,y}^S)) / base_divorces
  3. Apply: d̂_{x,y}^z = scale × DivGrid_{x,y}
```

### 4.4 SS Area Adjustment Factor Calculation

```
For each year z:
  ss_area_factor = (SS_area_pop^z) / (US_pop^z + PR_pop^z + VI_pop^z)

SS area total divorces = US_divorces × (1 + PR_VI_ratio) × ss_area_factor

where:
  PR_VI_ratio = (PR_divorces + VI_divorces) / US_divorces
```

---

## 5. Implementation Functions

### 5.1 Data Acquisition Functions

File: `R/data_acquisition/nchs_divorce.R`

```r
#' Fetch NCHS DRA divorce data (1979-1988)
#'
#' @description
#' Loads NCHS Divorce Registration Area data for the detailed period.
#' Data includes divorces by age-of-husband × age-of-wife.
#' Expected source: NBER archive (similar to marriage data)
#'
#' @param years Integer vector of years (default: 1979:1988)
#'
#' @return data.table with columns: year, husband_age_group, wife_age_group, divorces
#'
#' @export
fetch_nchs_dra_divorces <- function(years = 1979:1988)

#' Fetch NCHS total U.S. divorces (1979-2022)
#'
#' @description
#' Loads total annual divorce counts for the United States.
#' From 1992+, these are derived from rate × population.
#'
#' @param years Integer vector of years
#'
#' @return data.table with columns: year, total_divorces
#'
#' @export
fetch_nchs_us_divorces <- function(years = 1979:2022)

#' Fetch PR and VI divorce data
#'
#' @description
#' Loads total divorces for Puerto Rico and Virgin Islands.
#' Direct NCHS data available for 1988, 1998-2000.
#'
#' @param years Integer vector of years
#'
#' @return data.table with columns: year, territory, divorces
#'
#' @export
fetch_pr_vi_divorces <- function(years = c(1988, 1998:2000))
```

File: `R/data_acquisition/state_divorce.R`

```r
#' Fetch state divorce data (2009-2012)
#'
#' @description
#' Loads divorce data from 18 states that provide age-of-husband × age-of-wife.
#' States: AL, AK, ID, KS, KY, MI, MO, MT, NE, NH, TN, TX, UT, VT, VA, WA, WV, WY
#'
#' @return data.table with columns: year, state, husband_age_group, wife_age_group, divorces
#'
#' @details
#' This data may not be publicly available and may require:
#' - Direct contact with state health departments
#' - Alternative estimation approach using available data
#'
#' @export
fetch_state_divorce_data <- function()

#' Build 2011 state divorce grid
#'
#' @description
#' Aggregates state divorce data to create a national-level age grid for 2011.
#' Used to adjust the 1979-1988 DivGrid for more recent patterns.
#'
#' @param state_data State divorce data from fetch_state_divorce_data()
#'
#' @return data.table with columns: husband_age_group, wife_age_group, divorces
#'
#' @export
build_state_divorce_grid_2011 <- function(state_data)
```

File: `R/data_acquisition/acs_divorce.R`

```r
#' Fetch ACS divorces for Puerto Rico
#'
#' @description
#' Downloads ACS PUMS data for divorces in Puerto Rico.
#' Uses marital status change or divorce timing variables.
#'
#' @param years Integer vector of ACS years (default: 2008:2022, excluding 2020)
#'
#' @return data.table with columns: year, divorces
#'
#' @export
fetch_acs_pr_divorces <- function(years = setdiff(2008:2022, 2020))
```

### 5.2 Core Calculation Functions

File: `R/demography/divorce.R`

```r
#' Calculate divorce rates from divorces and married population
#'
#' @description
#' Calculates age-specific divorce rates using the formula:
#' d̂_{x,y}^z = D̂_{x,y}^z / P_{x,y}^z
#' where P_{x,y}^z is the married couples population.
#'
#' @param divorces data.table with divorce counts by husband/wife age
#' @param married_couples Married couples population by age-of-husband × age-of-wife
#'
#' @return data.table with columns: husband_age, wife_age, rate
#'
#' @export
calculate_divorce_rates <- function(divorces, married_couples)

#' Build base divorce grid (DivGrid)
#'
#' @description
#' Creates the 87×87 DivGrid matrix from NCHS 1979-1988 data.
#' Uses averaging, Beers interpolation, and Whittaker-Henderson graduation.
#'
#' @param nchs_divorces NCHS DRA divorce data (1979-1988)
#' @param married_population Married couples population (1979-1988)
#'
#' @return matrix (87 × 87) of divorce rates for ages 14-100+
#'
#' @export
build_base_divgrid <- function(nchs_divorces, married_population)

#' Adjust DivGrid with 2011 state data
#'
#' @description
#' Creates a weighted average DivGrid using 1988 base and 2011 state grid.
#' The 2011 grid is derived by ratioing 1988 DivGrid cells using state data.
#'
#' @param base_divgrid Base 87×87 DivGrid from 1979-1988
#' @param state_grid_2011 State data grid for 2011
#' @param year Target year for weighted average
#'
#' @return Adjusted DivGrid matrix
#'
#' @export
adjust_divgrid_with_state_data <- function(base_divgrid, state_grid_2011, year)

#' Calculate age-adjusted divorce rate (ADR)
#'
#' @description
#' Calculates the age-adjusted central divorce rate using
#' Equation 1.7.2 from TR2025.
#'
#' @param rates Divorce rate grid (husband × wife ages)
#' @param standard_pop Standard population grid (avg 2009-2010 married couples)
#'
#' @return Numeric ADR value (per 100,000 married couples)
#'
#' @export
calculate_adr <- function(rates, standard_pop)

#' Get standard married couples population
#'
#' @description
#' Returns the standard population for ADR calculation.
#' Based on averaged 2009-2010 December 31 married couples grids.
#'
#' @param historical_married Married couples from HISTORICAL POPULATION
#'
#' @return Matrix of married couples by age-of-husband × age-of-wife
#'
#' @export
get_standard_married_population <- function(historical_married)

#' Scale DivGrid to total divorces
#'
#' @description
#' Proportionally scales all DivGrid rates so that expected total
#' divorces match an external total.
#'
#' @param divgrid DivGrid matrix
#' @param married_pop Married population by age
#' @param target_total Target total divorces
#'
#' @return Scaled DivGrid matrix
#'
#' @export
scale_divgrid_to_total <- function(divgrid, married_pop, target_total)

#' Calculate SS area adjustment factor
#'
#' @description
#' Calculates the factor to adjust U.S. divorces to SS area divorces.
#'
#' @param ss_area_pop Total SS area population
#' @param us_pop Total U.S. resident + armed forces overseas
#' @param pr_vi_pop Total PR + VI population
#'
#' @return Numeric adjustment factor
#'
#' @export
calculate_ss_area_divorce_factor <- function(ss_area_pop, us_pop, pr_vi_pop)

#' Estimate SS area total divorces
#'
#' @description
#' Estimates total divorces in the SS area from U.S. data.
#'
#' @param us_divorces Total U.S. divorces
#' @param pr_vi_divorces Total PR + VI divorces
#' @param ss_area_factor SS area adjustment factor
#'
#' @return Estimated SS area divorces
#'
#' @export
estimate_ss_area_divorces <- function(us_divorces, pr_vi_divorces, ss_area_factor)
```

### 5.3 Projection Functions

File: `R/demography/divorce.R` (continued)

```r
#' Project age-adjusted divorce rate to ultimate
#'
#' @description
#' Projects ADR from starting value to ultimate value over specified years.
#' Rate of change decreases as ultimate approaches.
#'
#' @param starting_adr Starting ADR (from historical data)
#' @param ultimate_adr Ultimate ADR assumption (default: 1700)
#' @param years_to_ultimate Years until ultimate reached (default: 25)
#' @param projection_years Years to project
#'
#' @return data.table with columns: year, adr
#'
#' @export
project_adr <- function(starting_adr,
                        ultimate_adr = 1700,
                        years_to_ultimate = 25,
                        projection_years)

#' Scale DivGrid to target ADR
#'
#' @description
#' Proportionally scales DivGrid rates to produce a target ADR
#' when applied to the standard population.
#'
#' @param divgrid Base DivGrid matrix
#' @param target_adr Target ADR value
#' @param standard_pop Standard population (avg 2009-2010)
#'
#' @return Scaled DivGrid matrix
#'
#' @export
scale_divgrid_to_adr <- function(divgrid, target_adr, standard_pop)

#' Project divorce rates (Equation 1.7.1)
#'
#' @description
#' Projects age-specific divorce rates for all projection years.
#' Scales DivGrid to match projected ADR for each year.
#'
#' @param divgrid Base DivGrid matrix
#' @param projected_adr Projected ADR by year
#' @param standard_pop Standard population (avg 2009-2010)
#'
#' @return data.table with columns: year, husband_age, wife_age, rate
#'
#' @export
project_divorce_rates <- function(divgrid, projected_adr, standard_pop)

#' Calculate starting ADR
#'
#' @description
#' Calculates the starting ADR as a weighted average of recent years.
#' More recent years receive higher weights.
#'
#' @param historical_adr Historical ADR values
#' @param n_years Number of years for weighted average (default: 5)
#'
#' @return Numeric starting ADR value
#'
#' @export
calculate_starting_adr <- function(historical_adr, n_years = 5)
```

### 5.4 Main Entry Point

File: `R/demography/divorce.R` (continued)

```r
#' Run complete divorce projection (main entry point)
#'
#' @description
#' Main function orchestrating the complete divorce projection.
#' Implements Equations 1.7.1 and 1.7.2 from TR2025.
#'
#' @param nchs_data List containing NCHS divorce data
#' @param state_data State divorce data (2009-2012) or NULL for alternative approach
#' @param historical_population Historical population by age, sex, marital status
#' @param married_couples Married couples by age-of-husband × age-of-wife
#' @param population_totals Population totals for SS area adjustment
#' @param config Configuration with ultimate ADR and other parameters
#' @param projection_years Years to project (default: 2023:2099)
#'
#' @return list with:
#'   - divorce_rates: d̂_{x,y}^z by year, husband_age, wife_age
#'   - adr_projected: ADR^z by year
#'   - divgrid: Final DivGrid matrix
#'   - historical_adr: Historical ADR values
#'   - ss_area_divorces: Estimated SS area total divorces by year
#'
#' @export
run_divorce_projection <- function(nchs_data,
                                   state_data = NULL,
                                   historical_population,
                                   married_couples,
                                   population_totals,
                                   config,
                                   projection_years = 2023:2099)
```

---

## 6. Configuration

### 6.1 Divorce Configuration (add to tr2025.yaml)

```yaml
divorce:
  # TR2025 assumptions
  ultimate_adr: 1700  # per 100,000 married couples
  ultimate_year_offset: 25  # Years from first projection year

  # Starting rate calculation
  starting_rate_years: 5  # Number of historical years for weighted average
  starting_rate_weights: "linear"  # More weight to recent years

  # Age range
  min_age: 14
  max_age: 100  # 100+ grouped

  # DivGrid parameters
  divgrid_base_years: [1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988]

  # Whittaker-Henderson smoothing parameters
  wh_smoothing:
    h_param: 1.0  # Husband dimension
    w_param: 1.0  # Wife dimension

  # Data periods
  historical_periods:
    detailed_nchs: [1979, 1988]
    scaled_totals: [1989, 2022]
    state_data_year: 2011

  # State data (18 states)
  state_data_states:
    - Alabama
    - Alaska
    - Idaho
    - Kansas
    - Kentucky
    - Michigan
    - Missouri
    - Montana
    - Nebraska
    - New Hampshire
    - Tennessee
    - Texas
    - Utah
    - Vermont
    - Virginia
    - Washington
    - West Virginia
    - Wyoming

  # Standard population
  standard_population_years: [2009, 2010]  # Average of these years
  standard_population_date: "December 31"

  # SS area adjustment
  ss_area_adjustment_years:
    detailed: [1979, 1988]
    interpolated: [1998, 2000]
    acs: [2008, 2022]
```

---

## 7. Validation Framework

### 7.1 Validation Data Available

From TR2025:
- Ultimate ADR assumption (1,700 per 100,000)
- Years to reach ultimate (25 years)
- Published population projections (indirect validation)

External Sources:
- NCHS published divorce counts (1979-2022)
- CDC/NCHS divorce rate statistics
- State vital statistics reports

### 7.2 Validation Points

| Output | Metric | Tolerance | Source |
|--------|--------|-----------|--------|
| Historical ADR | Annual values | Reference only | Calculated internally |
| Total divorces | Annual count | 5% | NCHS published |
| Ultimate ADR | Year 25 value | Exact | TR assumption (1,700) |
| ADR trajectory | Monotonic | Required | Toward ultimate |
| DivGrid sum | Total rate | 1% | Internal consistency |
| Rate positivity | All rates ≥ 0 | Required | Constraint |
| SS area factor | Range check | 1.01-1.03 | Expected range |

### 7.3 Validation Functions

File: `R/validation/validate_divorce.R`

```r
#' Validate divorce projection outputs
#'
#' @param divorce_rates Projected divorce rates
#' @param adr_projected Projected ADR values
#' @param divgrid DivGrid matrix
#' @param historical_adr Historical ADR values
#' @param config Configuration
#'
#' @return Validation report
#'
#' @export
validate_divorce_outputs <- function(divorce_rates,
                                     adr_projected,
                                     divgrid,
                                     historical_adr,
                                     config)

#' Validate ADR reaches ultimate value
#' @export
validate_adr_ultimate <- function(adr_projected, ultimate_adr, ultimate_year)

#' Validate ADR trajectory is monotonic toward ultimate
#' @export
validate_adr_trajectory <- function(adr_projected, starting_adr, ultimate_adr)

#' Validate DivGrid properties
#' @export
validate_divgrid_properties <- function(divgrid)

#' Validate divorce rates against NCHS totals
#' @export
validate_against_nchs_divorce_totals <- function(divorce_rates, nchs_totals, tolerance = 0.05)

#' Validate SS area adjustment factor
#' @export
validate_ss_area_factor <- function(ss_area_factor, expected_range = c(1.01, 1.03))

#' Comprehensive divorce validation
#' @export
validate_divorce_comprehensive <- function(divorce_projection, config)
```

---

## 8. Implementation Sequence

### Phase 7A: NCHS Historical Data Acquisition

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 7A.1 | Research NCHS DRA data availability (1979-1988) | None | Data source assessment |
| [x] | 7A.2 | Implement NCHS divorce data loader | 7A.1 | nchs_divorce.R |
| [x] | 7A.3 | Load/parse NCHS DRA divorces (1979-1988) | 7A.2 | Detailed divorce grids |
| [x] | 7A.4 | Load NCHS total U.S. divorces (1979-2022) | 7A.2 | Annual totals |
| [x] | 7A.5 | Load PR/VI divorce data | 7A.2 | PR/VI totals |
| [x] | 7A.6 | Validate NCHS data completeness | 7A.3-7A.5 | Validation report |

**Phase 7A Notes (January 18, 2026):**
- NBER divorce files (div79-div88.dat) have 117-character records with different layout than NCHS tape format
- Weight field is at positions 40-41 (NOT 8-9 which contains state code)
- 1979-1988 DRA coverage: 42.6% of U.S. total divorces
- 1989-1995 DRA coverage: 48.9% of U.S. total divorces
- All 17 years of microdata (1979-1995) successfully parsed
- U.S. total divorces 1979-2022 hardcoded from NCHS NVSR reports
- PR/VI data available for 1988, 1998-2000 (limited)

**Expected Data Sources:**
- NBER archive (similar to marriage data): https://www.nber.org/research/data/marriage-and-divorce-data-1968-1995
- NCHS Vital Statistics reports for total counts
- PR/VI data from NCHS or Census

### Phase 7B: Population Data & Married Couples Grid

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 7B.1 | Extract married couples grid from Phase 4/6 | Phase 4/6 | Married couples by age |
| [x] | 7B.2 | Calculate standard population (avg 2009-2010) | 7B.1 | Standard pop matrix |
| [x] | 7B.3 | Fetch population totals for SS area adjustment | Phase 4 | Pop totals by year |
| [x] | 7B.4 | Calculate SS area adjustment factors | 7B.3 | Adjustment factors |
| [x] | 7B.5 | Validate population data | 7B.1-7B.4 | Validation report |

**Phase 7B Notes (January 18, 2026):**
- Created `R/demography/divorce.R` with core functions
- Married couples grid uses geometric means: P_{x,y} = sqrt(married_male_x × married_female_y)
- Standard population: averaged 2009-2010 December 31 married couples (per TR2025)
- SS area factors: 1.019 - 1.027 (reuses marriage subprocess calculation)
- Population totals available for all required years (1979-1988, 1998-2000, 2008-2022)
- All 4/4 validation checks pass

### Phase 7C: DivGrid Development

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 7C.1 | Implement divorce rate calculation | None | divorce.R |
| [x] | 7C.2 | Implement 2D Beers interpolation (reuse from marriage) | Phase 6 | Not needed - data already single-year |
| [x] | 7C.3 | Implement 2D Whittaker-Henderson (reuse from marriage) | Phase 6 | graduate_divgrid() |
| [x] | 7C.4 | Build base DivGrid from 1979-1988 average | 7A.3, 7B.1, 7C.1-7C.3 | Base DivGrid |
| [x] | 7C.5 | Implement DivGrid scaling function | 7C.4 | scale_divgrid_to_total(), scale_divgrid_to_adr() |
| [x] | 7C.6 | Validate base DivGrid | 7C.4 | validate_divgrid() - 6/6 checks pass |

**Phase 7C Notes (January 18, 2026):**
- Base DivGrid built from 1979-1988 NCHS DRA data (10-year average)
- NCHS data has single-year ages, so Beers interpolation not needed
- 2D Whittaker-Henderson graduation applied for smoothing
- Key results:
  - Dimensions: 87×87 (ages 14-100+)
  - Peak rate: 342.2 per 100,000 at husband age 25, wife age 23
  - Base ADR: 1,749.4 per 100,000 (very close to TR2025 ultimate of 1,700)
- Cached to `data/cache/divorce/base_divgrid_1979_1988.rds`
- Key functions: `build_base_divgrid()`, `fetch_base_divgrid()`, `validate_divgrid()`

### Phase 7D: State Data & DivGrid Adjustment

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 7D.1 | Research state divorce data availability | None | Data availability report |
| [ ] | 7D.2 | Implement state data loader (if available) | 7D.1 | state_divorce.R |
| [ ] | 7D.3 | Build 2011 state divorce grid | 7D.2 | State grid |
| [ ] | 7D.4 | Implement DivGrid adjustment function | 7C.4, 7D.3 | adjust_divgrid_with_state_data() |
| [ ] | 7D.5 | Alternative: ACS-based adjustment if state data unavailable | 7D.1 | ACS-based grid |

**State Data Alternatives (if direct state data unavailable):**
1. Use ACS PUMS divorce data (available nationally, not just PR)
2. Use only 1979-1988 NCHS base grid without 2011 adjustment
3. Estimate 2011 pattern from total divorce rate trends

### Phase 7E: Historical Period Calculation (1989-2022)

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 7E.1 | Estimate SS area total divorces (1989-2022) | 7A.4, 7A.5, 7B.4 | SS area totals |
| [ ] | 7E.2 | Scale DivGrid to match totals | 7C.5, 7D.4, 7E.1 | Scaled rates |
| [ ] | 7E.3 | Calculate historical ADR series | 7E.2, 7B.2 | Historical ADR |
| [ ] | 7E.4 | Validate historical ADR | 7E.3 | Validation report |

### Phase 7F: ADR Projection

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 7F.1 | Implement ADR projection function | None | project_adr() |
| [ ] | 7F.2 | Calculate starting ADR (5-year weighted average) | 7E.3 | Starting ADR |
| [ ] | 7F.3 | Project ADR to ultimate (year 25) | 7F.1, 7F.2 | Projected ADR |
| [ ] | 7F.4 | Validate ADR projection | 7F.3 | Validation report |

### Phase 7G: Divorce Rate Projection

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 7G.1 | Implement DivGrid-to-ADR scaling | 7C.5, 7F.1 | scale_divgrid_to_adr() |
| [ ] | 7G.2 | Project age-specific rates (2023-2099) | 7G.1, 7F.3 | Projected rates |
| [ ] | 7G.3 | Build run_divorce_projection() entry point | All above | Main function |
| [ ] | 7G.4 | Validate projected rates | 7G.2 | Validation report |

### Phase 7H: Validation & Pipeline Integration

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 7H.1 | Implement validation functions | 7G.3 | validate_divorce.R |
| [ ] | 7H.2 | Validate all outputs | 7H.1 | Validation report |
| [ ] | 7H.3 | Add divorce targets to _targets.R | 7G.3 | Updated pipeline |
| [ ] | 7H.4 | Run full pipeline and validate | 7H.3 | Complete outputs |
| [ ] | 7H.5 | Document limitations and deviations | 7H.4 | Documentation |

---

## 9. Technical Specifications

### 9.1 Data Structures

**DivGrid Matrix:**
```
Structure: 87 × 87 matrix
Rows: Husband ages 14-100+ (87 single years)
Columns: Wife ages 14-100+ (87 single years)
Values: Divorce rates per 100,000 married couples
```

**Divorce Rates (Projected):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year (2023-2099)
husband_age     integer   Single year of age (14-100)
wife_age        integer   Single year of age (14-100)
rate            numeric   Divorce rate per 100,000
```

**Age-Adjusted Divorce Rate (ADR):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
adr             numeric   Age-adjusted rate per 100,000
```

**Standard Population (Married Couples):**
```
Column          Type      Description
------          ----      -----------
husband_age     integer   Single year of age
wife_age        integer   Single year of age
married_couples numeric   Number of married couples (avg 2009-2010)
```

### 9.2 Key Methodological Notes

**DivGrid Development:**
- Base rates from 1979-1988 NCHS DRA data
- Inflated to SS area using population ratios
- 2011 state data used to update age distribution pattern
- Whittaker-Henderson smoothing removes artifacts

**SS Area Adjustment:**
- DRA covers only ~48% of U.S. divorces
- Must inflate to national then to SS area
- Factor varies by year based on population ratios

**ADR Projection:**
- Starting rate uses weighted average (not single year) for stability
- Asymptotic convergence to ultimate prevents overshooting
- Rate of change decreases as ultimate approaches

**State Data Gap:**
- NCHS stopped collecting detailed divorce data after 1988
- 18-state data (2009-2012) fills gap for pattern adjustment
- If unavailable, use simplified approach without 2011 adjustment

### 9.3 Potential Simplifications for Initial Implementation

1. **Skip state data adjustment:**
   - Use 1979-1988 base DivGrid without 2011 adjustment
   - Scale directly to total divorce counts

2. **Use ACS divorce data:**
   - ACS has divorce/separation in last 12 months
   - Can provide age patterns for recent years

3. **Simplified SS area adjustment:**
   - Use constant factor based on population ratio
   - Skip year-by-year calculation

4. **Simplified ADR projection:**
   - Linear interpolation initially
   - Refine to asymptotic convergence

### 9.4 Dependencies on Previous Subprocesses

| Subprocess | Data Needed | Used In |
|------------|-------------|---------|
| HISTORICAL POPULATION (1.4.2) | Married couples by age | Rate denominators |
| HISTORICAL POPULATION (1.4.2) | Population totals | SS area adjustment |
| MARRIAGE (1.6) | 2D interpolation/graduation functions | DivGrid development |

### 9.5 External Data Dependencies

| Source | Data | Frequency | Notes |
|--------|------|-----------|-------|
| NCHS | Total U.S. divorces | Annual | National vital statistics |
| NCHS | DRA detailed (1979-1988) | Static | NBER archive |
| State Health Depts | Age-specific divorces | Variable | 18 states, 2009-2012 |
| ACS PUMS | PR divorces | Annual | 2008-2022 |

### 9.6 Code Reuse from Marriage Subprocess

The following functions from Phase 6 (Marriage) can be reused:

| Function | Source | Use in Divorce |
|----------|--------|----------------|
| `beers_interpolate_2d()` | marriage.R | Age group to single year |
| `whittaker_henderson_2d()` | marriage.R | Rate smoothing |
| `get_ss_area_factor()` | marriage.R | Population adjustment |
| Asymptotic convergence | marriage.R | ADR projection formula |

---

## Appendix A: NCHS Data Sources

### A.1 NBER Marriage and Divorce Data Archive

**URL:** https://www.nber.org/research/data/marriage-and-divorce-data-1968-1995

**Expected Divorce Files:**
- div79.zip through div88.zip: Individual year files
- cpdiv.zip: Combined 1989-1995 (limited detail)
- md_doc.zip: Documentation

**File Format (expected):**
- Fixed-width format similar to marriage files
- Fields: year, state, husband_age, wife_age, count
- Age groups may vary by year

### A.2 NCHS Vital Statistics Reports

**Divorce Statistics:**
- National Vital Statistics Reports (annual)
- Table with total U.S. divorces
- Divorce rates per 1,000 population

### A.3 State Health Department Data

**Contact Information (18 states):**
- Each state's vital records office
- Data may be available via public records request
- Some states publish online

---

## Appendix B: Comparison with Marriage Subprocess

| Aspect | Marriage | Divorce |
|--------|----------|---------|
| Registration Area | MRA (31 states in 1988) | DRA (31 states in 1988) |
| Coverage | ~80% of U.S. | ~48% of U.S. |
| Denominator | Unmarried (single + div + wid) | Married couples |
| Grid name | MarGrid | DivGrid |
| Base years | 1978-1988 (excl. 1980) | 1979-1988 |
| Update data | ACS (2008-2022) | State data (2009-2012) |
| Ultimate rate | 4,000 per 100,000 | 1,700 per 100,000 |
| Standard pop year | July 1, 2010 | Avg Dec 31, 2009-2010 |
| Standard pop base | Unmarried by age | Married couples by age |
| Same-sex | Separated | Not separated (combined) |
| Prior status | Single/div/wid differentials | Not differentiated |

---

## Appendix C: Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| NCHS DRA data unavailable | Low | High | NBER archive should have it |
| State data (18 states) unavailable | High | Medium | Use ACS or skip adjustment |
| DRA coverage adjustment complex | Medium | Medium | Use simplified factor |
| Married couples grid accuracy | Low | Medium | Leverage Phase 4/6 work |
| ADR trend uncertainty | Low | Low | TR assumption anchors ultimate |
| DivGrid instability | Low | Medium | More aggressive smoothing |

---

## Appendix D: Simplified Initial Approach (Recommended)

Given the state data availability constraints, a phased approach is recommended:

### Phase 7 Core (MVP)

1. **Use NCHS DRA data only (1979-1988):**
   - Build base DivGrid from NCHS DRA
   - Skip state data adjustment initially

2. **Simple SS area adjustment:**
   - Use constant population ratio factor
   - Apply to total divorce counts

3. **Scale to NCHS totals:**
   - Use published total U.S. divorces
   - Apply SS area factor

4. **Simple ADR projection:**
   - Calculate historical ADR
   - Linear interpolation to ultimate (1,700)

### Phase 7 Enhanced (Future)

1. **Add state data adjustment:**
   - If/when state data obtained
   - Update DivGrid with 2011 pattern

2. **Refine SS area adjustment:**
   - Year-specific population ratios
   - PR/VI detailed adjustments

3. **Add ACS-based validation:**
   - Compare patterns to ACS divorce data
   - Cross-validate total estimates

---

## Appendix E: Expected Results

Based on TR2025 parameters and historical trends:

**Historical ADR (estimated):**
- 1979-1988: Higher rates (divorce peak era)
- 1990s-2000s: Declining trend
- 2010s-2020s: Continued decline toward 2,000-2,500 range

**Projection:**
- Starting ADR (2022): ~2,200-2,500 (estimate)
- Ultimate ADR (2047): 1,700 (TR assumption)
- Direction: Declining (unlike marriage which increases)

**DivGrid Characteristics:**
- Peak rates at middle ages (30-50 for both spouses)
- Low rates at very young ages (<20)
- Declining rates at older ages (65+)

---

*End of Implementation Plan*

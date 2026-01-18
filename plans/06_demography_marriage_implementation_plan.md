# ARTEMIS: OASDI Projection Model Implementation Plan
## Phase 6: Demography Process - Marriage Subprocess (1.6)

**Document Version:** 1.0
**Date:** January 2026
**Scope:** Marriage subprocess implementation following Temp/Unlawfully Present Immigration completion
**Source:** SSA 2025 Long-Range Model Documentation, Section 1.6 Marriage

---

## Progress Tracking

> **IMPORTANT**: This plan document should be updated as implementation progresses. Each task should be marked with its current status.

### Status Legend
- [ ] Not started
- [~] In progress
- [x] Completed

### Current Status
**Last Updated:** January 18, 2026
**Current Phase:** Phase 6D - Historical Period Calculation (COMPLETE)
**Subprocess Status:** IN PROGRESS - Ready for Phase 6E (AMR Projection)

### Phase 6D Results (COMPLETE)

**Historical Rates 1989-1995:** ✓ COMPLETE
- Adjusted base MarGrid distribution using NCHS subset data (8 age groups)
- Scaled to NCHS U.S. totals (SS area factor 1.003)
- Applied 2D Whittaker-Henderson graduation
- AMR range: 3,778 - 3,952 per 100,000

**Interpolated Rates 1996-2007:** ✓ COMPLETE
- Linear interpolation between 1995 NCHS and 2008 ACS distributions
- Scaled to NCHS U.S. totals for each year

**ACS-based Rates 2008-2022:** ✓ COMPLETE
- Adjusted MarGrid distribution using ACS marriage grids
- Aligned ACS grids (85×85) to MarGrid dimensions (87×87)
- Scaled to NCHS U.S. totals
- AMR range: 3,311 - 3,722 per 100,000

**Historical AMR Series:** ✓ COMPLETE
- 1989: 3,885 | 1990: 3,952 | 1991: 3,837 | 1992: 3,824 | 1993: 3,778
- 1994: 3,825 | 1995: 3,785 | ... (declining trend)
- 2016: 3,722 | 2017: 3,708 | 2018: 3,545 | 2019: 3,355
- 2021: 3,311 | 2022: 3,463
- TR2025 Ultimate AMR target: 4,000 per 100,000 (consistent with historical range)

**Key Functions Implemented:**
- `calculate_historical_rates_1989_1995()` - NCHS subset adjustment
- `interpolate_marriage_grids()` - Linear interpolation
- `calculate_historical_rates_2008_2022()` - ACS-based adjustment
- `calculate_historical_period()` - Main orchestration
- `calculate_amr_from_matrix()` - AMR calculation with correct TR2025 formula

**Key File:** `R/demography/marriage.R`

### Phase 6B Results (COMPLETE)

**1978-1988 Data (Items 4-5):** ✓ COMPLETE
- Downloaded marr78.zip through marr88.zip from NBER archive (11 files, ~1.2GB total)
- Note: TR2025 mentions excluding 1980, but 1980 data IS available from NBER
- Parsed fixed-width format (140-char records) using marr88.pdf layout
- Total records: 8,236,466 across 11 years
- 792 age group combinations (9×9 groups × 11 years - some zero)
- Cached to `data/cache/nber_marriage/nchs_mra_marriages_1978_1988.rds`
- Yearly totals:
  - 1978: 1,800,325 | 1979: 1,876,315 | 1980: 2,275,711
  - 1981: 1,918,846 | 1982: 1,944,448 | 1983: 1,921,778
  - 1984: 1,935,339 | 1985: 1,890,543 | 1986: 1,888,895
  - 1987: 1,874,243 | 1988: 1,886,657

**1989-1995 Data (Item 6):** ✓ COMPLETE
- Downloaded cpmarr.zip from NBER archive (1,357,710 records)
- Parsed fixed-width format (67-char records) using marrlyo.txt layout
- 498 age group combinations (9×9 groups × 7 years)
- MRA covers 75.6%-78.7% of U.S. marriages (expected ~80%)
- Cached to `data/cache/nber_marriage/nchs_mra_marriages_1989_1995.rds`

**Item 8 (Total U.S. marriages, 1989-2022):** ✓ COMPLETE
- Published NCHS totals hardcoded in `fetch_nchs_us_total_marriages()`
- Source: CDC/NCHS National Vital Statistics Reports

**Items 9-11 (Prior marital status):** PARTIAL
- 1989-1995 data available from NBER cpmarr.dat
- TR2025 requires 1979, 1981-1988 (different file format) - NOW AVAILABLE
- 367 rows by age group × sex × prior status
- Cached to `data/cache/nber_marriage/nchs_marriages_by_prior_status_1989_1995.rds`

**Detailed age grids:** ✓ COMPLETE
- 1989-1995: 20,805 rows with single-year ages (12-94)
  - Cached to `data/cache/nber_marriage/nchs_mra_marriages_detailed_1989_1995.rds`
- 1978-1988: Available via `fetch_nchs_mra_marriages_detailed_1978_1988()`

**Item 5 (MRA unmarried population):** Using CPS as proxy
- NCHS marriage files contain event records only, not population counts
- National CPS unmarried population (Item 14) used as denominator for rate calculations
- CPS covers 1978-1988 with 8 age groups × 2 sexes
- MRA covers ~80% of U.S. population with similar demographics, so national proxy is acceptable

**Key file:** `R/data_acquisition/nchs_marriage.R`

### Phase 6A Results
- ACS new marriages fetched for 2007-2022 (2007 extrapolated from 2008, 2020 skipped)
- Marriage grids built: 85×85 matrices (ages 15-99) for each year
- ACS captures ~88% of NCHS total marriages (mean coverage)
- Unmarried population derived from Phase 4 marital status cache
- Historical AMR calculated: Mean 2,837 per 100,000 (2008-2019)
- Current AMR (2022): 2,745 per 100,000
- TR2025 Ultimate AMR target: 4,000 per 100,000
- **Item 13 (2010 standard population):** Implemented via `get_2010_standard_population()`
- **Item 14 (CPS 1962-1995):** ✓ Complete via IPUMS CPS (`ipums_cps.R`)
  - IPUMS CPS ASEC data starts 1962 (not 1957 as documentation states)
  - 34 years of data downloaded and cached
  - 544 rows: 8 age groups × 2 sexes × 34 years
  - Cache: `data/cache/ipums_cps/cps_unmarried_1957_1995.rds`

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

The MARRIAGE subprocess projects annual age-specific marriage rates by age-of-husband crossed with age-of-wife for the Social Security area population. This subprocess is critical because it:

- Provides marriage rate projections for the PROJECTED POPULATION subprocess
- Enables projection of marital status distributions over the 75-year projection period
- Affects benefit calculations for spousal and survivor benefits
- Models both opposite-sex and same-sex marriages separately

### 1.2 Key Terminology

| Term | Definition |
|------|------------|
| MRA | Marriage Registration Area (42 states + D.C. in 1988) |
| MarGrid | 87×87 matrix of marriage rates by age-of-husband × age-of-wife (ages 14-100+) |
| m̂_{x,y}^z | Age-specific marriage rate for husband age x, wife age y, year z |
| AMR^z | Age-adjusted central marriage rate for year z |
| P_{x,y}^z | Theoretical midyear unmarried population (geometric mean of male × female) |
| P_{x,y}^S | Standard population (July 1, 2010 unmarried population) |
| Geometric Mean | Square root of the product of two numbers |

### 1.3 Primary Outputs

| Output | Symbol | Equation | Description |
|--------|--------|----------|-------------|
| Age-Specific Marriage Rates | m̂_{x,y}^z | 1.6.1 | Marriage rates by age-of-husband × age-of-wife |
| Age-Adjusted Marriage Rate | AMR^z | 1.6.2 | Summary rate for entire population |
| Opposite-Sex Marriage Rates | m̂_{x,y}^{z,opp} | - | Rates for opposite-sex marriages |
| Same-Sex Marriage Rates | m̂_{x,y}^{z,same} | - | Rates for same-sex marriages |

### 1.4 TR2025 Assumptions

| Parameter | Value | Notes |
|-----------|-------|-------|
| Ultimate AMR | 4,000 per 100,000 | Per 100,000 unmarried couples |
| Ultimate Year | 25th projection year (2047) | Reached gradually |
| Age Range | 14-100+ | Both husband and wife |
| Starting Rate | 5-year weighted average | From 2018-2022 historical data |

### 1.5 Data Periods

| Period | Data Source | Detail Level |
|--------|-------------|--------------|
| 1978-1988 (excl. 1980) | NCHS MRA | Single year ages (detailed) |
| 1989-1995 | NCHS MRA subset | Age groups only |
| 1996-2007 | Interpolated | Linear between 1995 and 2008 |
| 2008-2022 | ACS PUMS | Age-of-husband × age-of-wife |
| 2023+ | Projected | Based on ultimate AMR |

---

## 2. Mathematical Framework

### 2.1 Equation 1.6.1 – Age-Specific Marriage Rates

For each projection year z, age-specific marriage rates are calculated as:

$$m̂_{x,y}^z = m̂_{x,y}^z(·)$$

Where:
- x = age of husband (14-100+)
- y = age of wife (14-100+)
- The (·) notation indicates rates are derived from the scaling methodology below

**Historical Rate Calculation:**

$$m̂_{x,y}^z = \frac{M̂_{x,y}^z}{P_{x,y}^z}$$

Where:
- M̂_{x,y}^z = Number of marriages in year z for husband age x, wife age y
- P_{x,y}^z = Geometric mean of midyear unmarried men age x and women age y

**Geometric Mean:**

$$P_{x,y}^z = \sqrt{P_x^{z,male,unmarried} \times P_y^{z,female,unmarried}}$$

### 2.2 Equation 1.6.2 – Age-Adjusted Central Marriage Rate

$$AMR^z = \frac{\sum_{x,y} P_{x,y}^S \cdot m̂_{x,y}^z}{\sum_{x,y} P_{x,y}^S}$$

Where:
- P_{x,y}^S = Standard population (July 1, 2010 geometric mean of unmarried by age)
- The numerator = expected number of marriages using current rates and standard population
- The denominator = total standard population (geometric mean of unmarried 15+ by sex)

### 2.3 MarGrid Development

The base marriage grid (MarGrid) is an 87×87 matrix (ages 14-100+ for both husband and wife) developed from NCHS 1978-1988 data:

1. Calculate rates from NCHS marriage counts and unmarried population
2. Average rates across 1978-1988 (excluding 1980)
3. Graduate using two-dimensional Whittaker-Henderson smoothing
4. Store as base distribution pattern

### 2.4 Projection Methodology

**Step 1: Calculate Starting AMR**
- Use weighted average of last 5 historical years (2018-2022)
- Weight more recent years more heavily

**Step 2: Project AMR to Ultimate**
- Starting AMR → Ultimate AMR (4,000) by year 25
- Annual rate of change decreases in absolute value as ultimate approaches
- Use asymptotic convergence formula

**Step 3: Scale MarGrid**
- For each projection year, proportionally scale MarGrid rates
- Ensure scaled rates produce the projected AMR when applied to standard population

**Step 4: Separate Same-Sex and Opposite-Sex**
- Adjust MarGrid rates to produce separate same-sex and opposite-sex rates
- Same-sex marriage data from state vital statistics (2004-2012)

### 2.5 Prior Marital Status Adjustment

Marriage rates vary significantly by prior marital status:
- Never-married
- Divorced
- Widowed

The relative differences by prior marital status are assumed to remain constant at 1979/1981-88 average levels.

---

## 3. Input Data Requirements

### 3.1 Data Sources Summary

The Marriage subprocess requires **15 distinct data inputs**:

| Category | Items | Priority |
|----------|-------|----------|
| Trustees Assumptions | 1 | High |
| Long-Range OASDI Projection Data (Internal) | 2 | High |
| NCHS Data | 8 | High |
| U.S. Census Bureau Data | 3 | High |
| Other Input Data | 1 | Medium |

### 3.2 Trustees Assumptions

| # | Data | Description | Status |
|---|------|-------------|--------|
| 1 | Ultimate AMR | 4,000 per 100,000 unmarried couples, reached in year 25 | In config |

### 3.3 Long-Range OASDI Projection Data (From Previous Subprocesses)

| # | Data | Source | Years | Status |
|---|------|--------|-------|--------|
| 2 | Social Security area population by age, sex, marital status | HISTORICAL POPULATION (Eq 1.4.2) | 1977-2022 | Available (Phase 4) |
| 3 | Final historical year | HISTORICAL POPULATION | 2022 | Available (Phase 4) |

**Notes:**
- Item 2 provides unmarried population (single + widowed + divorced) needed for rate denominators
- These data are available from completed Phase 4 implementation

### 3.4 NCHS Data

| # | Data | Years | Detail | Status |
|---|------|-------|--------|--------|
| 4 | Marriages in MRA by age-of-husband × age-of-wife | 1978-1988 (excl. 1980) | Single year ages (varies) | To implement |
| 5 | Unmarried men and women in MRA | 1978-1988 (excl. 1980) | Single year <40, grouped 40+ | To implement |
| 6 | Marriages in MRA subset by age group | 1989-1995 | Age groups (8 categories) | To implement |
| 7 | Total marriages in MRA (adjusted) | 1957-1988 | Annual totals | To implement |
| 8 | Total marriages in U.S. | 1989-2022 | Annual totals | To implement |
| 9 | Marriages by age group, sex, prior marital status | 1979, 1981-1988 | 8 age groups × 3 statuses | To implement |
| 10 | Unmarried population by age group, sex, prior marital status | 1982-1988 | 8 age groups × 3 statuses | To implement |
| 11 | Total marriages and remarriages | 1979, 1981-1988 | Annual totals | To implement |

**NCHS Age Groups (for items 6, 9, 10):**
- 14-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 65+

**Prior Marital Status Categories:**
- Single (never married)
- Widowed
- Divorced

### 3.5 U.S. Census Bureau Data

| # | Data | Source | Years | Status |
|---|------|--------|-------|--------|
| 12 | New marriages by age-of-husband × age-of-wife | ACS PUMS | 2007-2022 | ✓ Implemented |
| 13 | 2010 standard population (unmarried) | Census/ACS | 2010 | ✓ Implemented |
| 14 | Unmarried men and women | March CPS | 1962-1995 | ✓ Implemented (IPUMS) |

**ACS Marriage Question (starting 2008):**
- "In the past 12 months, did this person get married?"
- Combined with spouse's age provides marriage grids

### 3.6 Other Input Data

| # | Data | Source | Years | Status |
|---|------|--------|-------|--------|
| 15 | Same-sex marriages | State vital statistics | 2004-2012 | To implement |

**Same-Sex Marriage Notes:**
- Data from states that legalized before national legalization
- Massachusetts (2004), California (2008), etc.
- Used to develop same-sex marriage rate patterns

### 3.7 Data Availability Assessment

| Data Category | Public Availability | Notes |
|---------------|---------------------|-------|
| NCHS marriage data (1978-1988) | Low | Historical files, may need manual extraction |
| NCHS marriage counts (1989+) | High | National vital statistics reports |
| ACS new marriages | High | PUMS microdata, married in last 12 months |
| CPS unmarried population | Medium | IPUMS CPS microdata |
| State same-sex marriage | Low | State vital statistics offices |

### 3.8 Data Acquisition Priority

**Phase 6A - ACS Marriage Data (High Priority):**
- Item 12: ACS PUMS new marriages (2007-2022)
- Item 13: 2010 standard population (already available from Phase 4)

**Phase 6B - NCHS Historical Data (High Priority):**
- Items 4-11: NCHS historical marriage data
- May require digitizing historical reports

**Phase 6C - CPS Data (Medium Priority):**
- Item 14: CPS unmarried population for MRA adjustment

**Phase 6D - Same-Sex Marriage Data (Lower Priority):**
- Item 15: State vital statistics
- Can implement with simplified assumptions initially

---

## 4. Output Development Methodology

### 4.1 Step-by-Step Process for MarGrid Development

**Step 1: Load NCHS 1978-1988 Marriage Data**

```
For years 1978-1988 (excluding 1980):
  1. Load marriages by age-of-husband × age-of-wife
  2. Load unmarried population by age and sex in MRA
  3. Calculate midyear unmarried population: P_{x,y}^z = sqrt(male_x × female_y)
  4. Calculate rates: m̂_{x,y}^z = M_{x,y}^z / P_{x,y}^z
```

**Step 2: Average and Graduate Rates**

```
1. Average rates across 1978-1988 (excluding 1980)
2. Use two-dimensional H.S. Beers to convert grouped ages to single years
3. Apply two-dimensional Whittaker-Henderson graduation for smoothing
4. Store as MarGrid (87 × 87 matrix)
```

**Step 3: Calculate Age-Adjusted Rates**

```
For each historical year z:
  1. Multiply MarGrid by standard population: expected_marriages = sum(P_{x,y}^S × m̂_{x,y})
  2. Calculate AMR^z = expected_marriages / sum(P_{x,y}^S)
```

### 4.2 Step-by-Step Process for Historical Period (1989-2022)

**Step 1: Update MarGrid Distribution (1989-1995)**

```
For each year 1989-1995:
  1. Load NCHS marriage counts by age group (subset MRA)
  2. Adjust MarGrid within each age group to match NCHS totals
  3. Rates within groups scaled proportionally
```

**Step 2: Interpolate (1996-2007)**

```
For each year 1996-2007:
  1. Linear interpolation between 1995 distribution and 2008 ACS distribution
  2. Scale to match NCHS total U.S. marriage count
```

**Step 3: Use ACS Data (2008-2022)**

```
For each year 2008-2022:
  1. Load ACS PUMS married-in-last-12-months by age-of-husband × age-of-wife
  2. Use to update MarGrid distribution by age groups
  3. Scale to match Social Security area population
  4. Subtract same-sex marriages for separate handling
```

**Step 4: Scale to Total Marriages**

```
For each year 1989-2022:
  1. Calculate expected marriages from (adjusted) MarGrid × SS area population
  2. Adjust all rates proportionally to match NCHS total U.S. marriages
  3. Convert U.S. marriages to SS area using population ratio adjustment
```

**Step 5: Graduate Final Rates**

```
Apply two-dimensional Whittaker-Henderson graduation to smooth age-specific rates
```

### 4.3 Step-by-Step Process for Projection Period (2023-2099)

**Step 1: Calculate Starting AMR**

```
starting_amr = weighted_average(AMR^{2018:2022})
  where weights favor more recent years
```

**Step 2: Project AMR to Ultimate**

```
ultimate_amr = 4000 (per 100,000 unmarried couples)
ultimate_year = first_projection_year + 24 (year 25)

For each projection year z:
  if z <= ultimate_year:
    AMR^z = interpolate(starting_amr, ultimate_amr, z, ultimate_year)
    # Rate of change decreases as ultimate approaches
  else:
    AMR^z = ultimate_amr
```

**Step 3: Scale MarGrid to Projected AMR**

```
For each projection year z:
  1. Calculate base expected marriages: base_marriages = sum(P_{x,y}^S × MarGrid)
  2. Calculate scaling factor: scale = (AMR^z × sum(P_{x,y}^S)) / base_marriages
  3. Apply: m̂_{x,y}^z = scale × MarGrid_{x,y}
```

### 4.4 Step-by-Step Process for Same-Sex Marriage Separation

**Step 1: Estimate Same-Sex Marriage Rates**

```
From 2004-2012 state data:
  1. Calculate same-sex marriage rates by age group
  2. Develop same-sex MarGrid pattern
```

**Step 2: Separate Rates**

```
For each year z:
  1. m̂_{x,y}^{z,same} = same_sex_factor × m̂_{x,y}^z
  2. m̂_{x,y}^{z,opp} = m̂_{x,y}^z - m̂_{x,y}^{z,same}
```

### 4.5 Step-by-Step Process for Prior Marital Status

**Step 1: Calculate Historical Differentials**

```
From NCHS 1979, 1981-1988 data:
  1. Calculate marriage rates by prior marital status (single, widowed, divorced)
  2. Calculate ratio: rate_status / rate_total for each status
  3. Average ratios across years
```

**Step 2: Apply Differentials to Projections**

```
For each projection year z:
  m̂_{x,y,status}^z = m̂_{x,y}^z × relative_rate_{status}

where relative_rate is the average ratio from 1979, 1981-88
```

---

## 5. Implementation Functions

### 5.1 Data Acquisition Functions

File: `R/data_acquisition/nchs_marriage.R`

```r
#' Fetch NCHS MRA marriage data (1978-1988)
#'
#' @description
#' Loads NCHS Marriage Registration Area data for the detailed period.
#' Data includes marriages by age-of-husband × age-of-wife.
#'
#' @param years Integer vector of years (default: c(1978:1979, 1981:1988))
#'
#' @return data.table with columns: year, husband_age, wife_age, marriages
#'
#' @export
fetch_nchs_mra_marriages <- function(years = c(1978:1979, 1981:1988))

#' Fetch NCHS MRA unmarried population (1978-1988)
#'
#' @description
#' Loads NCHS estimates of unmarried population in the MRA.
#'
#' @param years Integer vector of years
#'
#' @return data.table with columns: year, age, sex, unmarried_population
#'
#' @export
fetch_nchs_mra_unmarried <- function(years = c(1978:1979, 1981:1988))

#' Fetch NCHS MRA subset marriages (1989-1995)
#'
#' @description
#' Loads NCHS marriage data from MRA subset by age groups.
#'
#' @param years Integer vector of years (default: 1989:1995)
#'
#' @return data.table with columns: year, husband_age_group, wife_age_group, marriages
#'
#' @export
fetch_nchs_mra_subset_marriages <- function(years = 1989:1995)

#' Fetch NCHS total U.S. marriages (1989-2022)
#'
#' @description
#' Loads total annual marriage counts for the United States.
#'
#' @param years Integer vector of years
#'
#' @return data.table with columns: year, total_marriages
#'
#' @export
fetch_nchs_us_marriages <- function(years = 1989:2022)

#' Fetch NCHS marriages by prior marital status
#'
#' @description
#' Loads marriage counts by age group, sex, and prior marital status.
#'
#' @param years Integer vector of years (default: c(1979, 1981:1988))
#'
#' @return data.table with columns: year, age_group, sex, prior_status, marriages
#'
#' @export
fetch_nchs_marriages_by_prior_status <- function(years = c(1979, 1981:1988))
```

File: `R/data_acquisition/acs_marriage.R`

```r
#' Fetch ACS new marriages by age of spouses
#'
#' @description
#' Downloads ACS PUMS data for persons married in the last 12 months,
#' creating marriage grids by age-of-husband × age-of-wife.
#'
#' @param years Integer vector of ACS years (default: 2008:2022)
#'
#' @return data.table with columns: year, husband_age_group, wife_age_group, marriages
#'
#' @details
#' Uses the MAR variable (married in last 12 months) combined with
#' AGE and spouse's AGE from household relationships.
#'
#' @export
fetch_acs_new_marriages <- function(years = 2008:2022)

#' Fetch ACS unmarried population
#'
#' @description
#' Downloads ACS PUMS unmarried population by age and sex.
#' Unmarried = single + widowed + divorced.
#'
#' @param years Integer vector of ACS years
#'
#' @return data.table with columns: year, age, sex, unmarried_population
#'
#' @export
fetch_acs_unmarried_population <- function(years = 2008:2022)
```

File: `R/data_acquisition/cps_marriage.R`

```r
#' Fetch CPS unmarried population (1957-1995)
#'
#' @description
#' Downloads March CPS unmarried population for MRA adjustment calculations.
#'
#' @param years Integer vector of years (default: 1957:1995)
#'
#' @return data.table with columns: year, age_group, sex, unmarried_population
#'
#' @export
fetch_cps_unmarried_population <- function(years = 1957:1995)
```

File: `R/data_acquisition/same_sex_marriage.R`

```r
#' Fetch state same-sex marriage data (2004-2012)
#'
#' @description
#' Loads same-sex marriage counts from states that legalized before
#' national legalization (2015).
#'
#' @return data.table with columns: year, state, age_group, sex_combo, marriages
#'
#' @export
fetch_state_same_sex_marriages <- function()
```

### 5.2 Core Calculation Functions

File: `R/demography/marriage.R`

```r
#' Calculate marriage rates from marriages and population
#'
#' @description
#' Calculates age-specific marriage rates using the formula:
#' m̂_{x,y}^z = M_{x,y}^z / P_{x,y}^z
#' where P_{x,y}^z is the geometric mean of unmarried male and female populations.
#'
#' @param marriages data.table with marriage counts by husband/wife age
#' @param unmarried_male Unmarried male population by age
#' @param unmarried_female Unmarried female population by age
#'
#' @return data.table with columns: husband_age, wife_age, rate
#'
#' @export
calculate_marriage_rates <- function(marriages, unmarried_male, unmarried_female)

#' Build base marriage grid (MarGrid)
#'
#' @description
#' Creates the 87×87 MarGrid matrix from NCHS 1978-1988 data.
#' Uses averaging, Beers interpolation, and Whittaker-Henderson graduation.
#'
#' @param nchs_marriages NCHS marriage data (1978-1988)
#' @param nchs_unmarried NCHS unmarried population (1978-1988)
#'
#' @return matrix (87 × 87) of marriage rates for ages 14-100+
#'
#' @export
build_base_margrid <- function(nchs_marriages, nchs_unmarried)

#' Apply two-dimensional Whittaker-Henderson graduation
#'
#' @description
#' Smooths a 2D matrix of rates using Whittaker-Henderson method.
#' Preserves row and column marginal totals while smoothing interior.
#'
#' @param grid Matrix of rates (husband age × wife age)
#' @param h_param Smoothing parameter for husband dimension (default: 1)
#' @param w_param Smoothing parameter for wife dimension (default: 1)
#'
#' @return Smoothed matrix of same dimensions
#'
#' @export
whittaker_henderson_2d <- function(grid, h_param = 1, w_param = 1)

#' Calculate age-adjusted marriage rate (AMR)
#'
#' @description
#' Calculates the age-adjusted central marriage rate using
#' Equation 1.6.2 from TR2025.
#'
#' @param rates Marriage rate grid (husband × wife ages)
#' @param standard_pop Standard population grid (2010 unmarried)
#'
#' @return Numeric AMR value (per 100,000 unmarried couples)
#'
#' @export
calculate_amr <- function(rates, standard_pop)

#' Adjust MarGrid to match age group totals
#'
#' @description
#' Adjusts detailed MarGrid rates so that when summed within age groups,
#' they match external totals (from NCHS subset or ACS).
#'
#' @param margrid Base 87×87 MarGrid matrix
#' @param group_totals Marriage counts by age group of husband/wife
#' @param age_groups Age group definitions
#'
#' @return Adjusted MarGrid matrix
#'
#' @export
adjust_margrid_to_groups <- function(margrid, group_totals, age_groups)

#' Scale MarGrid to total marriages
#'
#' @description
#' Proportionally scales all MarGrid rates so that expected total
#' marriages match an external total.
#'
#' @param margrid MarGrid matrix
#' @param unmarried_pop Unmarried population by age/sex
#' @param target_total Target total marriages
#'
#' @return Scaled MarGrid matrix
#'
#' @export
scale_margrid_to_total <- function(margrid, unmarried_pop, target_total)

#' Calculate prior marital status rate differentials
#'
#' @description
#' Calculates relative marriage rate differentials by prior marital status
#' using NCHS 1979, 1981-1988 data.
#'
#' @param marriages_by_status Marriages by age, sex, prior status
#' @param unmarried_by_status Unmarried population by age, sex, prior status
#'
#' @return data.table with columns: prior_status, age_group, sex, relative_rate
#'
#' @export
calculate_prior_status_differentials <- function(marriages_by_status,
                                                   unmarried_by_status)
```

### 5.3 Projection Functions

File: `R/demography/marriage.R` (continued)

```r
#' Project age-adjusted marriage rate to ultimate
#'
#' @description
#' Projects AMR from starting value to ultimate value over specified years.
#' Rate of change decreases as ultimate approaches.
#'
#' @param starting_amr Starting AMR (from historical data)
#' @param ultimate_amr Ultimate AMR assumption (default: 4000)
#' @param years_to_ultimate Years until ultimate reached (default: 25)
#' @param projection_years Years to project
#'
#' @return data.table with columns: year, amr
#'
#' @export
project_amr <- function(starting_amr,
                        ultimate_amr = 4000,
                        years_to_ultimate = 25,
                        projection_years)

#' Scale MarGrid to target AMR
#'
#' @description
#' Proportionally scales MarGrid rates to produce a target AMR
#' when applied to the standard population.
#'
#' @param margrid Base MarGrid matrix
#' @param target_amr Target AMR value
#' @param standard_pop Standard population (2010)
#'
#' @return Scaled MarGrid matrix
#'
#' @export
scale_margrid_to_amr <- function(margrid, target_amr, standard_pop)

#' Project marriage rates (Equation 1.6.1)
#'
#' @description
#' Projects age-specific marriage rates for all projection years.
#' Scales MarGrid to match projected AMR for each year.
#'
#' @param margrid Base MarGrid matrix
#' @param projected_amr Projected AMR by year
#' @param standard_pop Standard population (2010)
#'
#' @return data.table with columns: year, husband_age, wife_age, rate
#'
#' @export
project_marriage_rates <- function(margrid, projected_amr, standard_pop)

#' Separate same-sex and opposite-sex marriage rates
#'
#' @description
#' Splits projected marriage rates into same-sex and opposite-sex components.
#'
#' @param marriage_rates Total marriage rates by year, husband_age, wife_age
#' @param same_sex_factors Same-sex marriage proportion by age
#'
#' @return list with:
#'   - opposite_sex: Opposite-sex marriage rates
#'   - same_sex: Same-sex marriage rates
#'
#' @export
separate_marriage_types <- function(marriage_rates, same_sex_factors)

#' Apply prior marital status differentials
#'
#' @description
#' Creates marriage rate variants by prior marital status
#' using historical differentials.
#'
#' @param marriage_rates Marriage rates by year, husband_age, wife_age
#' @param status_differentials Relative rates by prior status
#'
#' @return data.table with columns: year, husband_age, wife_age, prior_status, rate
#'
#' @export
apply_prior_status_rates <- function(marriage_rates, status_differentials)
```

### 5.4 Main Entry Point

File: `R/demography/marriage.R` (continued)

```r
#' Run complete marriage projection (main entry point)
#'
#' @description
#' Main function orchestrating the complete marriage projection.
#' Implements Equations 1.6.1 and 1.6.2 from TR2025.
#'
#' @param nchs_data List containing NCHS marriage data
#' @param acs_data ACS new marriages and unmarried population
#' @param historical_population Historical population by age, sex, marital status
#' @param standard_pop_2010 2010 standard population (unmarried)
#' @param config Configuration with ultimate AMR and other parameters
#' @param projection_years Years to project (default: 2023:2099)
#'
#' @return list with:
#'   - marriage_rates: m̂_{x,y}^z by year, husband_age, wife_age
#'   - amr_projected: AMR^z by year
#'   - opposite_sex_rates: Opposite-sex rates
#'   - same_sex_rates: Same-sex rates
#'   - rates_by_status: Rates by prior marital status
#'   - margrid: Final MarGrid matrix
#'   - historical_amr: Historical AMR values
#'
#' @export
run_marriage_projection <- function(nchs_data,
                                     acs_data,
                                     historical_population,
                                     standard_pop_2010,
                                     config,
                                     projection_years = 2023:2099)
```

---

## 6. Configuration

### 6.1 Marriage Configuration (add to tr2025.yaml)

```yaml
marriage:
  # TR2025 assumptions
  ultimate_amr: 4000  # per 100,000 unmarried couples
  ultimate_year_offset: 25  # Years from first projection year

  # Starting rate calculation
  starting_rate_years: 5  # Number of historical years for weighted average
  starting_rate_weights: "linear"  # More weight to recent years

  # Age range
  min_age: 14
  max_age: 100  # 100+ grouped

  # MarGrid parameters
  margrid_base_years: [1978, 1979, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988]

  # Whittaker-Henderson smoothing parameters
  wh_smoothing:
    h_param: 1.0  # Husband dimension
    w_param: 1.0  # Wife dimension

  # Data periods
  historical_periods:
    detailed_nchs: [1978, 1988]  # Excluding 1980
    nchs_subset: [1989, 1995]
    interpolation: [1996, 2007]
    acs: [2008, 2022]

  # Prior marital status
  prior_status_categories:
    - single
    - widowed
    - divorced
  prior_status_differential_years: [1979, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988]

  # Same-sex marriage
  same_sex:
    data_years: [2004, 2012]
    male_gay_pct: 2.5  # Percent of male population assumed gay
    female_lesbian_pct: 4.5  # Percent of female population assumed lesbian

  # Age groups used in various data sources
  age_groups:
    nchs_subset: [14, 19, 20, 24, 25, 29, 30, 34, 35, 44, 45, 54, 55, 64, 65, 100]
    acs: [15, 19, 20, 24, 25, 29, 30, 34, 35, 44, 45, 54, 55, 64, 65, 100]

  # Standard population
  standard_population_year: 2010
  standard_population_date: "2010-07-01"
```

---

## 7. Validation Framework

### 7.1 Validation Data Available

From TR2025:
- Ultimate AMR assumption (4,000 per 100,000)
- Years to reach ultimate (25 years)
- Published population projections (indirect validation)

External Sources:
- NCHS published marriage counts (1989-2022)
- CDC/NCHS marriage rate statistics
- ACS marriage data patterns

### 7.2 Validation Points

| Output | Metric | Tolerance | Source |
|--------|--------|-----------|--------|
| Historical AMR | Annual values | Reference only | Calculated internally |
| Total marriages | Annual count | 5% | NCHS published |
| Ultimate AMR | Year 25 value | Exact | TR assumption (4,000) |
| AMR trajectory | Monotonic | Required | Toward ultimate |
| MarGrid sum | Total rate | 1% | Internal consistency |
| Rate positivity | All rates ≥ 0 | Required | Constraint |

### 7.3 Validation Functions

File: `R/validation/validate_marriage.R`

```r
#' Validate marriage projection outputs
#'
#' @param marriage_rates Projected marriage rates
#' @param amr_projected Projected AMR values
#' @param margrid MarGrid matrix
#' @param historical_amr Historical AMR values
#' @param config Configuration
#'
#' @return Validation report
#'
#' @export
validate_marriage_outputs <- function(marriage_rates,
                                       amr_projected,
                                       margrid,
                                       historical_amr,
                                       config)

#' Validate AMR reaches ultimate value
#' @export
validate_amr_ultimate <- function(amr_projected, ultimate_amr, ultimate_year)

#' Validate AMR trajectory is monotonic toward ultimate
#' @export
validate_amr_trajectory <- function(amr_projected, starting_amr, ultimate_amr)

#' Validate MarGrid properties
#' @export
validate_margrid_properties <- function(margrid)

#' Validate marriage rates against NCHS totals
#' @export
validate_against_nchs_totals <- function(marriage_rates, nchs_totals, tolerance = 0.05)

#' Validate same-sex and opposite-sex rates sum to total
#' @export
validate_marriage_type_split <- function(total_rates, opposite_sex, same_sex)
```

---

## 8. Implementation Sequence

### Phase 6A: ACS Marriage Data Acquisition

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 6A.1 | Implement ACS new marriages fetcher | None | acs_marriage.R |
| [x] | 6A.2 | Fetch ACS PUMS married-in-last-12-months (2007-2022) | 6A.1 | Cached data |
| [x] | 6A.3 | Build marriage grids by age of husband × wife | 6A.2 | Marriage grids |
| [x] | 6A.4 | Fetch ACS unmarried population | Phase 4 | Unmarried pop (from cache) |
| [x] | 6A.5 | Validate ACS marriage grids | 6A.3, 6A.4 | Validation report |

### Phase 6B: NCHS Historical Data Acquisition

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 6B.1 | Research NCHS MRA data availability (1978-1988) | None | NBER data assessed |
| [x] | 6B.2 | Implement NCHS marriage data loader | 6B.1 | nchs_marriage.R |
| [x] | 6B.3 | Load/digitize NCHS marriage grids (1978-1988) | 6B.2 | 792 rows (incl. 1980) |
| [x] | 6B.4 | Load NCHS MRA unmarried population | 6B.2 | Using CPS as proxy (Item 14) |
| [x] | 6B.5 | Load NCHS subset data (1989-1995) | 6B.2 | cpmarr.dat parsed |
| [x] | 6B.6 | Load NCHS total U.S. marriages (1989-2022) | 6B.2 | Annual totals |
| [x] | 6B.7 | Load NCHS marriages by prior marital status | 6B.2 | 1978-1988 + 1989-1995 |

**6B Data Source Details:**
- NBER Marriage and Divorce Data: https://www.nber.org/research/data/marriage-and-divorce-data-1968-1995
- cpmarr.zip: 1989-1995 combined file (93.7 MB, 1,357,710 records)
- marr78.zip through marr88.zip: Individual year files (140-char format, 11 files ~1.2GB)
- Documentation: md_doc.zip contains marrlyo.txt (67-char layout), cpmarr.cbk, marr88.pdf (1978-88 layout)
- **Note:** TR2025 excludes 1980, but 1980 data IS available from NBER and now included

### Phase 6C: MarGrid Development

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 6C.1 | Implement marriage rate calculation | None | marriage.R |
| [x] | 6C.2 | Implement two-dimensional Whittaker-Henderson | None | marriage.R |
| [x] | 6C.3 | Build base MarGrid from 1978-1988 average | 6B.3, 6B.4, 6C.1, 6C.2 | Base MarGrid |
| [x] | 6C.4 | Implement MarGrid adjustment function | 6C.3 | adjust_margrid_to_groups() |
| [x] | 6C.5 | Implement MarGrid scaling function | 6C.3 | scale_margrid_to_total() |
| [x] | 6C.6 | Validate base MarGrid | 6C.3 | Validation report |

**Phase 6C Results:**
- **Key file created:** `R/demography/marriage.R`
- **MarGrid:** 87×87 matrix (ages 14-100+) built from 1978-1988 average rates
- **Smoothing:** 2D Whittaker-Henderson applied iteratively
- **Peak rate:** 3,101 per 100,000 at husband age 26, wife age 24
- **Historical AMR (1978-1988):** 4,439 (1988) to 6,263 (1980), declining trend
  - Note: 1980 appears to be an outlier (explains TR2025 exclusion recommendation)
  - Mean AMR (excl. 1980): ~4,830 per 100,000
- **TR2025 Ultimate AMR target:** 4,000 per 100,000 (consistent with declining trend)
- **Functions implemented:**
  - `calculate_marriage_rates()` - Compute rates from marriages/population
  - `build_base_margrid()` - Build 87×87 MarGrid from NCHS/CPS data
  - `whittaker_henderson_2d()` - 2D smoothing
  - `calculate_amr()` - Age-adjusted marriage rate (Eq 1.6.2)
  - `scale_margrid_to_total()` - Scale rates to match total marriages
  - `scale_margrid_to_amr()` - Scale rates to target AMR
  - `adjust_margrid_to_groups()` - Adjust MarGrid within age groups
  - `beers_interpolate_2d()` - Expand age groups to single years

### Phase 6D: Historical Period Calculation (1989-2022)

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 6D.1 | Calculate historical marriage rates (1989-1995) | 6B.5, 6C.4 | Historical rates |
| [x] | 6D.2 | Interpolate rates (1996-2007) | 6D.1, 6A.3 | Interpolated rates |
| [x] | 6D.3 | Calculate rates from ACS (2008-2022) | 6A.3, 6C.4 | ACS-based rates |
| [x] | 6D.4 | Scale to NCHS U.S. totals | 6D.1-6D.3, 6B.6 | Scaled rates |
| [x] | 6D.5 | Calculate historical AMR series | 6D.4 | Historical AMR |
| [x] | 6D.6 | Validate historical AMR | 6D.5 | Validation report |

### Phase 6E: AMR Projection

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 6E.1 | Implement AMR projection function | None | project_amr() |
| [ ] | 6E.2 | Calculate starting AMR (5-year weighted average) | 6D.5 | Starting AMR |
| [ ] | 6E.3 | Project AMR to ultimate (year 25) | 6E.1, 6E.2 | Projected AMR |
| [ ] | 6E.4 | Validate AMR projection | 6E.3 | Validation report |

### Phase 6F: Marriage Rate Projection

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 6F.1 | Implement MarGrid-to-AMR scaling | 6C.3, 6E.1 | scale_margrid_to_amr() |
| [ ] | 6F.2 | Project age-specific rates (2023-2099) | 6F.1, 6E.3 | Projected rates |
| [ ] | 6F.3 | Implement same-sex/opposite-sex separation | Phase 6G | separate_marriage_types() |
| [ ] | 6F.4 | Implement prior status application | 6B.7 | apply_prior_status_rates() |
| [ ] | 6F.5 | Build run_marriage_projection() entry point | All above | Main function |

### Phase 6G: Same-Sex Marriage (Can be deferred)

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 6G.1 | Research state same-sex marriage data | None | Data assessment |
| [ ] | 6G.2 | Implement same-sex marriage data loader | 6G.1 | same_sex_marriage.R |
| [ ] | 6G.3 | Calculate same-sex marriage rate factors | 6G.2 | Same-sex factors |
| [ ] | 6G.4 | Integrate into main projection | 6G.3, 6F | Updated projection |

### Phase 6H: Validation & Targets Integration

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 6H.1 | Implement validation functions | 6F.5 | validate_marriage.R |
| [ ] | 6H.2 | Validate all outputs | 6H.1 | Validation report |
| [ ] | 6H.3 | Add marriage targets to _targets.R | 6F.5 | Updated pipeline |
| [ ] | 6H.4 | Run full pipeline and validate | 6H.3 | Complete outputs |
| [ ] | 6H.5 | Document limitations and deviations | 6H.4 | Documentation |

---

## 9. Technical Specifications

### 9.1 Data Structures

**MarGrid Matrix:**
```
Structure: 87 × 87 matrix
Rows: Husband ages 14-100+ (87 single years)
Columns: Wife ages 14-100+ (87 single years)
Values: Marriage rates per 100,000 unmarried couples
```

**Marriage Rates (Projected):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year (2023-2099)
husband_age     integer   Single year of age (14-100)
wife_age        integer   Single year of age (14-100)
rate            numeric   Marriage rate per 100,000
marriage_type   character "opposite_sex" or "same_sex"
```

**Age-Adjusted Marriage Rate (AMR):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
amr             numeric   Age-adjusted rate per 100,000
```

**Marriage Rates by Prior Status:**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
husband_age     integer   Single year of age
wife_age        integer   Single year of age
prior_status    character "single", "widowed", "divorced"
rate            numeric   Marriage rate per 100,000
```

**Standard Population (2010):**
```
Column          Type      Description
------          ----      -----------
husband_age     integer   Single year of age
wife_age        integer   Single year of age
geometric_mean  numeric   sqrt(unmarried_male × unmarried_female)
```

### 9.2 Key Methodological Notes

**MarGrid Development:**
- Base rates from 1978-1988 provide stable pattern
- Later years update distribution within age groups only
- Whittaker-Henderson smoothing removes artifacts

**AMR Projection:**
- Starting rate uses weighted average (not single year) for stability
- Asymptotic convergence to ultimate prevents overshooting
- Rate of change decreases as ultimate approaches

**Same-Sex Marriage:**
- Post-2015 (Obergefell) national legalization
- Pre-2015 state-level data used for patterns
- Separate grids for male-male and female-female

**Prior Marital Status:**
- Historical differentials assumed constant
- Divorced typically have higher remarriage rates
- Widowed rates vary significantly by age

### 9.3 Potential Simplifications for Initial Implementation

1. **Start without same-sex separation:**
   - Project total marriage rates first
   - Add same-sex split as enhancement

2. **Skip prior marital status initially:**
   - Use total rates only
   - Add status-specific rates later

3. **Use simplified MarGrid:**
   - If detailed NCHS data unavailable
   - Start with ACS-only MarGrid

4. **Simplified AMR projection:**
   - Linear interpolation initially
   - Refine to asymptotic convergence

### 9.4 Dependencies on Previous Subprocesses

| Subprocess | Data Needed | Used In |
|------------|-------------|---------|
| HISTORICAL POPULATION (1.4.2) | Population by age, sex, marital status | Rate denominators |
| HISTORICAL POPULATION (1.4.2) | 2010 standard population | AMR calculation |

### 9.5 External Data Dependencies

| Source | Data | Frequency | Notes |
|--------|------|-----------|-------|
| NCHS | Total U.S. marriages | Annual | National vital statistics |
| ACS PUMS | Married in last 12 months | Annual | Spouse ages from relationships |
| NCHS Historical | MRA detailed (1978-1988) | Static | May need digitization |
| State Vital Statistics | Same-sex marriages | Variable | Pre-2015 only |

### 9.6 Two-Dimensional Whittaker-Henderson Method

The Whittaker-Henderson graduation method is a smoothing technique that:
- Minimizes sum of squared differences from original values
- Penalizes roughness (second differences)
- Parameter controls smoothness vs. fit tradeoff

For 2D application:
```
Minimize: Σ (fitted - observed)² + λ₁ Σ Δ²(rows) + λ₂ Σ Δ²(cols)

Where:
- Δ² = second difference operator
- λ₁, λ₂ = smoothing parameters for each dimension
```

Implementation can use:
- Iterative row-column smoothing
- Matrix formulation with Kronecker products
- Existing R packages (demography, MortalitySmooth)

---

## Appendix A: NCHS Data Sources

### A.1 Vital Statistics of the United States - Marriage and Divorce

**Volume III (Marriage and Divorce):**
- Published annually through 1988
- Contains MRA marriage counts by age

**National Vital Statistics Reports:**
- Marriage statistics summaries (1989+)
- Total U.S. marriage counts

### A.2 Historical Data Access

**CDC WONDER:**
- Limited marriage data
- Total counts only (no age detail)

**NCHS Data Archive:**
- Historical vital statistics volumes
- May require manual extraction

**IPUMS Historical Census:**
- Can derive marriage patterns from census data
- Alternative to NCHS for some analyses

---

## Appendix B: ACS Marriage Variables

### B.1 Relevant PUMS Variables

| Variable | Description | Values |
|----------|-------------|--------|
| MAR | Married in last 12 months | 1=Yes, 2=No |
| MARHT | Times married | Count |
| AGE | Age | 0-99+ |
| SEX | Sex | 1=Male, 2=Female |
| RELSHIPP | Relationship to householder | Identifies spouse |
| SPORDER | Spouse order | Links to spouse record |

### B.2 Marriage Grid Construction

```
For each ACS year:
  1. Select persons with MAR=1 (married in last 12 months)
  2. Link to spouse using SPORDER or household relationships
  3. Get ages of both spouses
  4. Create cross-tabulation: husband_age × wife_age
  5. Weight by PWGTP (person weight)
```

---

## Appendix C: Comparison with Previous Subprocesses

| Aspect | O Immigration | Marriage |
|--------|---------------|----------|
| Primary output dimension | age × sex × type | husband_age × wife_age |
| Data sources | DHS, ACS, CBO | NCHS, ACS, CPS |
| Projection method | Distribution × total | Scale to AMR |
| TR assumption | Total O (1.35M) | Ultimate AMR (4,000) |
| Historical period | 2015-2019 (COVID gap) | 1978-1988 (detailed) + 2008+ |
| Complexity | High (types, cohorts) | Medium-High (2D grid) |
| Interpolation | Beers 1D | Beers 2D, Whittaker-Henderson 2D |

---

## Appendix D: Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| NCHS detailed data unavailable | Medium | High | Use ACS-only approach |
| ACS spouse linkage complexity | Medium | Medium | Use age group fallback |
| Whittaker-Henderson implementation | Low | Medium | Use existing R packages |
| Same-sex data gaps | High | Low | Use simplified assumptions |
| Prior status data unavailable | Medium | Low | Skip status differentiation |
| MarGrid instability | Low | Medium | More aggressive smoothing |
| AMR trend uncertainty | Low | Low | TR assumption anchors ultimate |

---

## Appendix E: Simplified Initial Approach (Recommended)

Given data availability constraints, a phased approach is recommended:

### Phase 6 Core (MVP)

1. **Use ACS data only (2008-2022):**
   - Skip detailed NCHS 1978-1988 for initial version
   - Build MarGrid from ACS average (2015-2019)
   - Use ACS patterns for husband × wife age distribution

2. **Simple AMR projection:**
   - Calculate historical AMR from ACS years
   - Linear interpolation to ultimate (4,000)

3. **Skip same-sex separation initially:**
   - Project combined rates
   - Add separation later

4. **Skip prior status differentiation initially:**
   - Use total marriage rates
   - Add status-specific later

### Phase 6 Enhanced (Future)

1. **Add NCHS historical data:**
   - If/when detailed data located
   - Refine MarGrid base

2. **Implement same-sex separation:**
   - Use state data patterns
   - Separate male-male and female-female

3. **Add prior marital status:**
   - Differential rates by status
   - Historical ratio application

---

*End of Implementation Plan*

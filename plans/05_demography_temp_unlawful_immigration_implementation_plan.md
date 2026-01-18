# ARTEMIS: OASDI Projection Model Implementation Plan
## Phase 5: Demography Process - Temporary or Unlawfully Present Immigration Subprocess (1.5)

**Document Version:** 1.0
**Date:** January 2026
**Scope:** Temporary or Unlawfully Present Immigration subprocess implementation following Historical Population completion
**Source:** SSA 2025 Long-Range Model Documentation, Section 1.5 Temporary or Unlawfully Present Immigration

---

## Progress Tracking

> **IMPORTANT**: This plan document should be updated as implementation progresses. Each task should be marked with its current status.

### Status Legend
- [ ] Not started
- [~] In progress
- [x] Completed

### Current Status
**Last Updated:** January 18, 2026
**Current Phase:** Phase 5E - Core Projection Functions (COMPLETE)
**Next Phase:** Phase 5F - DACA Projection

### Phase 5C Progress Notes - UPDATED (January 18, 2026)

**Files Created:**
- `R/demography/temp_unlawful_immigration.R` - Core O immigration projection functions

**Functions Implemented (Core):**
- `calculate_o_immigration()` - O = ACS new arrivals (with undercount) - LPR NEW arrivals
- `calculate_odist()` - ODIST distribution from 2015-2019 average (Eq 1.5.1)
- `get_overstay_percentages()` - Age-specific overstay percentages (RAND-based)
- `project_o_immigration()` - Apply ODIST to TR assumptions
- `run_o_immigration_projection()` - Full projection pipeline
- `validate_odist()` - Validation checks
- `get_overstay_percentage_sources()` - Documentation of data sources

**Functions Implemented (Historical Type Interpolation - TR2025 Methodology):**
- `get_type_splits_interpolated()` - TR2025 interpolation with anchor points
- `get_type_anchor_points()` - Defines 1963, 2010, 2015 anchor points
- `calculate_anchor_from_dhs()` - Derives anchor proportions from DHS data
- `get_nonimmigrant_age_distribution()` - NI age concentration (students, workers)
- `calculate_odist_with_interpolation()` - ODIST using TR2025 type interpolation

**TR2025 Historical Type Interpolation:**
Per TR2025: "It is assumed that all temporary or unlawfully present immigrants were
nonimmigrants as of December 31, 1963. Between December 31, 1963, and December 31, 2010,
the percentage...is linearly interpolated."

Anchor points implemented:
- **1963:** 100% nonimmigrant (I=100%, N=0%, V=0%)
- **2010:** Based on DHS estimates (N≈51%, I≈15%, V≈34%)
- **2015:** Based on DHS estimates (N≈49%, I≈16%, V≈35%)
- **2015+:** Use 2015 proportions (stable)

Type proportions evolve over time:
- **1980:** N=23%, I=69%, V=8% (interpolated)
- **2000:** N=44%, I=37%, V=19% (interpolated)
- **2015+:** N≈49%, I≈16%, V≈35% (post-anchor)

**Key Results:**
- O immigration (2015-2019 avg): 2.12M/year
- ODIST validated: sums to 1.0, all non-negative
- Type proportions (with interpolation): N=46.2%, I=28.3%, V=25.4%
- Sex proportions: Female=50.6%, Male=49.4%
- Age distribution: 20% ages 0-17, 47% ages 18-34, 18% ages 35-49

**TR2025 Assumptions Implemented:**
- 2022: 2,200,000
- 2023: 2,700,000
- 2024: 2,600,000
- 2025: 2,000,000
- 2026+: 1,350,000 (ultimate)

**Hardcoded Data with Configuration Options:**
All hardcoded values are clearly marked with `# HARDCODED` comments and block headers.
The following functions accept `config` parameter to override defaults:
- `get_tr2025_o_immigration_assumptions(config=)` - TR2025 assumptions by year
- `get_overstay_percentages(config=)` - Age-specific overstay percentages
- `get_type_anchor_points(config=)` - Override anchor point proportions

**Sources for Hardcoded Values:**
| Value | Source | Notes |
|-------|--------|-------|
| TR2025 O immigration totals | TR2025 Section 1.5 | Official Trustees assumptions |
| Overstay percentages by age | Warren & Kerwin (2017), DHS Overstay Reports | SSA internal RAND values not public |
| 1963 anchor (100% NI) | TR2025 explicit | "all...were nonimmigrants as of December 31, 1963" |
| 2010 anchor | DHS unauthorized (10.8M) + NI stock (1.9M) | Total O ≈ 12.7M |
| 2015 anchor | DHS unauthorized (10.7M) + NI stock (2.1M) | Total O ≈ 12.8M |
| NI age distribution | DHS Yearbook + visa category patterns | F-1 students, H-1B workers concentrated |

### Phase 5D Progress Notes - COMPLETED (January 18, 2026)

**Files Created:**
- `R/demography/temp_unlawful_emigration.R` - Departure rate calculation and O emigration projection

**Functions Implemented (Core):**
- `build_o_stock_for_rates()` - Build O population stocks from 2008-2010 for rate derivation
- `calculate_base_departure_rates()` - Calculate rates by dividing OE by OP
- `smooth_departure_rates()` - 5-year moving average smoothing
- `adjust_rates_for_recession()` - Adjust for 2008-2010 recession effects
- `apply_type_adjustments()` - Type-specific rate adjustments (N/I/V)
- `apply_daca_rate_adjustment()` - Lower rates for DACA-eligible population
- `get_departure_rate_sources()` - Source documentation

**Functions Implemented (Cohort-Tracking):**
- `initialize_cohort_tracking()` - Initialize O population with arrival_status dimension
- `add_new_arrivals_with_cohort()` - Mark new N arrivals as "recent"
- `transition_recent_to_non_recent()` - 1/threshold transitions each year
- `calculate_emigration_with_cohorts()` - Apply 2× rate to recent N arrivals
- `run_o_emigration_with_cohorts()` - Full projection with cohort tracking
- `update_o_population_with_cohorts()` - Update stock preserving cohorts
- `age_population_with_cohorts()` - Age population preserving arrival_status

**NOTE:** Legacy age-based approximation (`split_never_authorized_rates`, `run_o_emigration_projection`)
has been removed in favor of cohort-tracking approach which is more faithful to TR2025.

**TR2025 Methodology Implemented:**
- Stock build-up from 2008-2010 using cohort-component method
- Deaths: OD = qx × OP (same mortality as total population)
- Rates: OE / OP for each age, sex, type
- Recession adjustment factor (0.85)
- Policy periods: Pre-2015 vs Post-2015 (Executive Actions)
- **Cohort-based recent arrivals:** Tracks actual years since arrival, 2× rate for recent N
- DACA eligible: 50% lower departure rates
- Cohort transition: 1/threshold of recent → non_recent each year (default threshold: 5 years)

**Hardcoded Values with Sources:**
| Value | Source | Notes |
|-------|--------|-------|
| Recession factor (0.85) | Professional judgment | 2008-2010 was atypical |
| N pre-2015 multiplier (1.20) | TR2025 methodology | Higher enforcement |
| N post-2015 multiplier (0.80) | TR2025 methodology | After Executive Actions |
| NI transition (0.70→1.00) | TR2025 Section 1.5.c | 2015 to 2025 |
| V pre/post multipliers | TR2025 methodology | 1.10/0.85 |
| DACA rate reduction (0.50) | TR2025 explicit | Lower rates for DACA |
| Recent threshold (5 years) | TR2025 methodology | Time to become non-recent |
| Initial recent proportion (30%) | Flow/stock ratio | Estimated from immigration rates |
| AOS type split (60/40) | LPR subprocess | I and V contribute |

**Configuration Options:**
- `get_default_rate_config(config=)` - Override all rate multipliers
- Recession factor, type multipliers, DACA reduction, recent_threshold all configurable

### Phase 5E Progress Notes - COMPLETED (January 18, 2026)

**Files Created:**
- `R/demography/temp_unlawful_stock.R` - Core projection functions for Equations 1.5.3 and 1.5.4

**Functions Implemented (Equation 1.5.3 - Net O Immigration):**
- `calculate_net_o_immigration()` - NO = OI - OE - AOS per TR2025
- `distribute_aos_to_types()` - Distributes AOS to types I and V (N cannot directly adjust)

**Functions Implemented (Equation 1.5.4 - O Population Stock):**
- `project_o_population_stock()` - Full cohort-component stock projection
- `age_o_population()` - Ages population by one year (x-1 → x)
- `add_type_dimension()` - Applies type splits to population without types
- `prepare_aos_for_projection()` - Prepares AOS data with type distribution

**Functions Implemented (Historical Type Splits):**
- `calculate_historical_type_splits()` - Applies TR2025 interpolation to historical population
- `calibrate_to_dhs_nonimmigrant()` - Calibrates NI totals to DHS stock

**Functions Implemented (Main Entry Point):**
- `run_full_o_projection()` - Complete O immigration projection orchestration
- `calculate_simplified_departure_rates()` - Fallback rates when detailed data unavailable
- `get_default_odist()` - Default ODIST when ACS/LPR data not available
- `validate_o_projection()` - Validates projection outputs

**TR2025 Equations Implemented:**
1. **Equation 1.5.1:** OI = TO × ODIST (O Immigration)
2. **Equation 1.5.2:** OE = ORate × OP (O Emigration with cohort tracking)
3. **Equation 1.5.3:** NO = OI - OE - AOS (Net O Immigration)
4. **Equation 1.5.4:** OP = OP(z-1) + OI - OE - AOS - OD (O Population Stock)

**Key Implementation Details:**
- Deaths use total population mortality: OD = qx × OP
- AOS distributed 60% to type I (nonimmigrant), 40% to type V (overstayer)
- Type N (never-authorized) cannot directly adjust to LPR status
- Cohort tracking preserves 2× rate for recent never-authorized arrivals
- Mid-year population exposure used for death calculation

**Test Results (5/5 passed):**
1. Net O Immigration: NO = OI - OE - AOS calculated correctly
2. ODIST with Interpolation: Sums to 1.0, type proportions reasonable
3. O Immigration Projection: Totals match TR assumptions exactly
4. O Population Stock: Projected for all years, no negative values
5. Full Integration: All components present, 4/4 validation checks passed

**Validation Checks:**
- O immigration totals match TR assumptions exactly
- ODIST sums to 1.0
- All population values non-negative
- Type proportions reasonable (N=50%, I=15%, V=35%)

### Phase 5B Progress Notes - COMPLETED (January 18, 2026)

**Files Created:**
- `R/data_acquisition/acs_foreign_born.R` - New arrivals calculation, DACA eligibility, undercount factors

**Functions Implemented:**
- `calculate_acs_new_arrivals()` - Extracts new foreign-born arrivals from PUMS data
- `calculate_new_arrivals_distribution()` - Calculates ODIST input from 2015-2019 average
- `fetch_acs_2012_daca_eligible()` - Fetches 2012 ACS with education/citizenship for DACA
- `estimate_daca_eligible_2012()` - Applies DACA eligibility criteria
- `calculate_acs_undercount_factors()` - DHS-based undercount factors by age
- `apply_undercount_correction()` - Adjusts new arrivals for undercount

**Data Fetched and Cached:**
- Foreign-born flows: 2006-2023 (17 years, 2020 excluded due to COVID)
- 2012 ACS DACA-eligible population: 2,611 rows by age/sex/entry/education
- All data from Census PUMS API (api.census.gov)

**Key Values:**
- New arrivals (2015-2019 avg): 2.47M/year (uncorrected), 2.74M/year (undercount-corrected)
- DACA-eligible estimate (2012): 1.84M (ages 15-30, meeting entry/education criteria)
- Foreign-born population: 44.8M (2019) → 47.8M (2023)
- Age distribution of arrivals: 21% ages 0-17, 35% ages 18-30, 25% ages 31-45

**Note on Undercount Factors:**
- Uses DHS methodology-based factors (not API data)
- Working-age adults (18-49): 12-15% undercount
- Children and elderly: 3-5% undercount

### Phase 5A Progress Notes - COMPLETED (January 18, 2026)

**Files Created:**
- `R/data_acquisition/dhs_nonimmigrant.R` - Nonimmigrant stock, BOY totals, admissions
- `R/data_acquisition/dhs_daca.R` - DACA grants and population stock

**Data Fetched and Cached:**
- Nonimmigrant stock: 3 reference dates (Apr 2008, Dec 2010, Apr 2016) by age group/sex
- BOY nonimmigrants: 2005-2022 (18 years, some interpolated)
- Nonimmigrant admissions: 2005-2022 summary totals
- DACA grants: FY 2013-2023 (11 years)
- DACA stock: 2013-2023 by age group/sex (132 rows)
- Unauthorized estimates: Already available from Phase 4

**Key Values:**
- Nonimmigrant stock: 1.9M (2008) → 2.1M (2016)
- BOY nonimmigrants: 1.78M (2005) → 2.05M (2022)
- DACA peak: 800K (2017) → 580K (2023)
- Unauthorized: 11.0M (2022)

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

The TEMPORARY OR UNLAWFULLY PRESENT IMMIGRATION subprocess produces estimates of non-LPR immigrants entering and leaving the Social Security area. This population includes:

1. **Temporary immigrants (nonimmigrants)**: Persons lawfully admitted for a limited period (temporary workers, foreign students)
2. **Never-authorized immigrants**: Persons who entered without authorization
3. **Visa-overstayers**: Nonimmigrants who overstayed their authorized period

This subprocess is critical because it:
- Completes the immigration picture beyond LPR immigration
- Provides inputs for the PROJECTED POPULATION subprocess
- Affects OASDI revenue through payroll tax contributions from this population
- Models the stock of temporary/unauthorized residents over time

### 1.2 Key Terminology

| Term | Definition |
|------|------------|
| O Population | Temporary or unlawfully present immigrant population |
| OI | Temporary or unlawfully present immigration (inflows) |
| OE | Temporary or unlawfully present emigration (outflows) |
| NO | Net temporary or unlawfully present immigration |
| Type (t) | Classification: never-authorized, nonimmigrant, visa-overstayer |
| DACA | Deferred Action for Childhood Arrivals |
| Nonimmigrant | Temporary legal immigrant (students, workers, tourists) |

### 1.3 Primary Outputs

| Output | Symbol | Equation | Description |
|--------|--------|----------|-------------|
| Temporary/Unlawfully Present Immigration | OI^z_{x,s,t} | 1.5.1 | Annual inflows by age, sex, type |
| Temporary/Unlawfully Present Emigration | OE^z_{x,s,t} | 1.5.2 | Annual outflows by age, sex, type |
| Net O Immigration | NO^z_{x,s,t} | 1.5.3 | Net flow = OI - OE - AOS |
| O Population Stock | OP^z_{x,s,t} | 1.5.4 | End-of-year population stock |
| DACA Population | DACA^z_{x,s} | - | DACA recipients by age, sex |

### 1.4 Type Categories

| Code | Type | Description |
|------|------|-------------|
| N | Never-authorized | Entered without authorization |
| I | Nonimmigrant | Legal temporary immigrants |
| V | Visa-overstayer | Overstayed authorized period |

### 1.5 TR2025 Assumptions

| Year | Total O Immigration (TO^z) |
|------|---------------------------|
| 2022 | 2,200,000 |
| 2023 | 2,700,000 |
| 2024 | 2,600,000 |
| 2025 | 2,000,000 |
| 2026+ (ultimate) | 1,350,000 |

---

## 2. Mathematical Framework

### 2.1 Equation 1.5.1 – Temporary or Unlawfully Present Immigration (OI)

For each projection year z, temporary or unlawfully present immigration is distributed using historical age-sex-type distributions:

$$OI^z_{x,s,t} = TO^z \cdot ODIST_{x,s,t}$$

Where:
- $TO^z$ = Total temporary or unlawfully present immigration for year z (Trustees assumption)
- $ODIST_{x,s,t}$ = Age-sex-type distribution (from 2015-2019 average)

**ODIST Development:**
- Based on average historical estimates of O immigrants entering the country from 2015 through 2019
- Derived from ACS foreign-born new arrivals minus LPR new arrivals
- Split by type using DHS nonimmigrant stock estimates and unauthorized population estimates

### 2.2 Equation 1.5.2 – Temporary or Unlawfully Present Emigration (OE)

Emigration is calculated using stock-based departure rates:

$$OE^z_{x,s,t} = ORate_{x,s,t} \cdot OP^{z-1}_{x,s,t}$$

Where:
- $ORate_{x,s,t}$ = Departure rate by age, sex, and type
- $OP^{z-1}_{x,s,t}$ = Beginning-of-year O population stock

**Rate Development (from 2008-2010 period):**
1. Build up O population stocks from 2008 through 2010
2. Calculate deaths: $OD^z_{x,s,t} = q^z_{x,s} \cdot OP^z_{x,s,t}$
3. Divide $OE^z_{x,s,t}$ by $OP^z_{x,s,t}$ for each age, sex, type
4. Smooth and adjust for recession effects

**Type-Specific Adjustments:**
- Never-authorized recent arrivals: Exposed to 2× departure rates
- DACA/DAPA eligible: Lower departure rates
- Nonimmigrants: Rates set to initial (2015) and gradually increase to ultimate (2025)

### 2.3 Equation 1.5.3 – Net O Immigration (NO)

$$NO^z_{x,s,t} = OI^z_{x,s,t} - OE^z_{x,s,t} - AOS^z_{x,s,t}$$

Where:
- $AOS^z_{x,s,t}$ = Adjustments of status to LPR (from LPR IMMIGRATION subprocess)

### 2.4 Equation 1.5.4 – O Population Stock (OP)

$$OP^z_{x,s,t} = OP^{z-1}_{x-1,s,t} + OI^z_{x,s,t} - OE^z_{x,s,t} - AOS^z_{x,s,t} - OD^z_{x,s,t}$$

Where:
- $OD^z_{x,s,t}$ = Deaths in O population (using same mortality as total population)

**Historical Type Splits:**
- December 31, 1963: All O immigrants assumed to be nonimmigrants
- 1963-2010: Linear interpolation of type percentages
- 2010-2015: Linear interpolation of type percentages
- Final adjustment ensures nonimmigrant totals match DHS estimates

### 2.5 DACA Population Projection

The DACA population is modeled as a subset of the O population:

1. **Eligibility Estimation:**
   - Age requirements
   - Residency requirements
   - Educational requirements

2. **Application Rates:**
   - Rates applied to eligible population
   - Age and sex-specific factors

3. **Stock Calibration:**
   - Adjusted to match DHS DACA stock estimates (2013-2019)

---

## 3. Input Data Requirements

### 3.1 Data Sources Summary

The Temporary/Unlawfully Present Immigration subprocess requires **32 distinct data inputs**:

| Category | Items | Priority |
|----------|-------|----------|
| Trustees Assumptions | 1 | High |
| Long-Range OASDI Projection Data (Internal) | 7 | High |
| Department of Homeland Security | 7 | High |
| U.S. Census Bureau | 4 | High |
| Other Input Data | 13 | Medium |

### 3.2 Trustees Assumptions

| # | Data | Description | Status |
|---|------|-------------|--------|
| 1 | Total annual O immigration | 2,200,000 (2022) to 1,350,000 (2026+) | In config |

### 3.3 Long-Range OASDI Projection Data (From Previous Subprocesses)

| # | Data | Source | Years | Status |
|---|------|--------|-------|--------|
| 2 | Probabilities of death (qx) | MORTALITY subprocess | 1941-2105 | Available (Phase 2) |
| 3 | Historical net O immigration | HISTORICAL subprocess | 1961-2022 | Available (Phase 4) |
| 4 | Historical Dec 31 O population | HISTORICAL subprocess | 1963-2022 | Available (Phase 4) |
| 5 | Historical Jul 1 O population | HISTORICAL subprocess | 1964-2022 | Available (Phase 4) |
| 6 | Final historical year | HISTORICAL subprocess | 2022 | Available (Phase 4) |
| 7 | Historical NEW arrivals | LPR IMMIGRATION subprocess | 1941-2022 | Available (Phase 3) |
| 8 | Historical + projected AOS | LPR IMMIGRATION subprocess | 1941-2105 | Available (Phase 3) |

**Notes:**
- Items 2-8 are available from completed subprocesses
- Age -1 represents births during the year in historical data

### 3.4 Department of Homeland Security Data

| # | Data | Years | Detail | Status |
|---|------|-------|--------|--------|
| 9 | Unauthorized immigrant components | 2005-12 | By year | To implement |
| 10 | Unauthorized immigrant undercounts | 2015-2020, 2022 | By year | To implement |
| 11 | LPR population components | 2005-11, 2012-23 | By year | To implement |
| 12 | Nonimmigrant stock estimates | Apr 2008, Dec 2010, Apr 2016 | Age group, sex | To implement |
| 13 | Nonimmigrant admissions | 1981-2016 | By class | To implement |
| 14 | Beginning-of-year nonimmigrants | 2005-22 | Total | To implement |
| 15 | DACA initial grants | FY 2013-18 | Annual | To implement |
| 16 | DACA stock population | 2013-19 | Age, sex | To implement |

### 3.5 U.S. Census Bureau Data (ACS)

| # | Data | Years | Detail | Status |
|---|------|-------|--------|--------|
| 17 | Foreign-born new persons | 2000-22 | Entry year, age, sex | Partial (Phase 4) |
| 18 | Total population (for undercount) | 2006-22 | Total + PR | Available |
| 19 | 2012 ACS special populations | 2012 | Entry year, age, sex | To implement |
| 20 | 2012 ACS TPS eligible | 2012 | Entry year, age, sex | To implement |

### 3.6 Other Input Data

| # | Data | Source | Status |
|---|------|--------|--------|
| 21 | DACA eligibility by age/sex | Migration Policy Institute | To implement |
| 22 | 2014 DACA potential eligible | Internal (not updated) | Skip |
| 23 | DAPA potential eligible | Internal (not updated) | Skip |
| 24 | DACA attainment factors | Internal | To derive |
| 25 | Nonimmigrant OI distribution factors | Internal (not updated) | To derive |
| 26 | Overstay percentages by age | RAND (1980s) + DHS | To implement |
| 27 | Never-authorized departure rates (non-recent) | Internal | To derive |
| 28 | Nonimmigrant departure rates | Internal | To derive |
| 29 | Visa-overstayer departure rates | Internal | To derive |
| 30 | Pre-2015 never-authorized departure rates | Internal | To derive |
| 31 | Pre-2015 visa-overstayer departure rates | Internal | To derive |
| 32 | O immigrants by age/sex (2015-2019 avg) | Internal | To derive from ACS |
| 33 | NCHS births by month/sex | NCHS | 1931-2023 | Available (Phase 1) |

### 3.7 Data Availability Assessment

| Data Category | Public Availability | Notes |
|--------------|---------------------|-------|
| DHS unauthorized estimates | Medium | Published reports |
| DHS nonimmigrant stock | Low | Only 3 point-in-time estimates |
| DHS DACA data | Medium | USCIS published data |
| ACS foreign-born flows | High | PUMS microdata |
| Internal departure rates | Low | Must derive from published data |
| MPI eligibility estimates | Medium | Research publications |

### 3.8 Data Acquisition Priority

**Phase 5A - Core DHS Data (High Priority):**
- Items 9-11: Unauthorized immigrant components and LPR population
- Items 12-14: Nonimmigrant stock and admissions
- Items 15-16: DACA population data

**Phase 5B - ACS Data (High Priority):**
- Items 17-20: Foreign-born flows and special populations

**Phase 5C - Derived Parameters (Medium Priority):**
- Items 24-32: Internal factors and departure rates (derive from public data)

**Phase 5D - MPI and Other (Low Priority):**
- Item 21: DACA eligibility estimates from MPI

---

## 4. Output Development Methodology

### 4.1 Step-by-Step Process for OI (Equation 1.5.1)

**Step 1: Develop Age-Sex-Type Distribution (ODIST)**

```
From ACS (2015-2019):
  1. Get foreign-born new arrivals by entry year, age, sex
  2. Apply undercount factors for ACS vs Census differences
  3. Subtract LPR new arrivals to get O arrivals
  4. Smooth the series

From DHS:
  1. Get nonimmigrant stock estimates (2008, 2010, 2016)
  2. Get unauthorized population by type
  3. Split O arrivals into types: never-authorized, nonimmigrant, visa-overstayer

Calculate:
  ODIST_{x,s,t} = AvgOI_{x,s,t} / sum(AvgOI) for 2015-2019
```

**Step 2: Apply Distribution to Trustees Assumption**

```
For each projection year z:
  OI^z_{x,s,t} = TO^z × ODIST_{x,s,t}
```

### 4.2 Step-by-Step Process for OE (Equation 1.5.2)

**Step 1: Build Historical O Population Stock (2008-2010)**

```
Starting from Dec 31, 2007 O population:
  For each year 2008-2010:
    OP^z = OP^{z-1} + OI^z - OE^z - AOS^z - OD^z
    where OD^z = qx × OP^z
```

**Step 2: Calculate Departure Rates**

```
For 2008-2010 period:
  ORate_{x,s,t} = OE^z_{x,s,t} / OP^z_{x,s,t}

Smooth rates and adjust for:
  - Recession effects (2008-2010 was atypical)
  - Executive actions effects (2015 decreased deportation)
```

**Step 3: Apply Type-Specific Rate Adjustments**

```
Never-authorized:
  - Recent arrivals: 2× base rate
  - Non-recent: base rate (before 2015), adjusted rate (2015+)

Nonimmigrants:
  - Initial rates in 2015
  - Gradually increase to ultimate rates by 2025

Visa-overstayers:
  - Base rate (before 2015), adjusted rate (2015+)

DACA/DAPA eligible:
  - Lower departure rates than non-eligible
```

**Step 4: Project Emigration**

```
For each projection year z:
  OE^z_{x,s,t} = AdjustedRate_{x,s,t} × OP^{z-1}_{x,s,t}
```

### 4.3 Step-by-Step Process for Type Splits

**Historical Type Interpolation:**

```
Reference points:
  - Dec 31, 1963: 100% nonimmigrant
  - Dec 31, 2010: DHS-based type distribution
  - Dec 31, 2015: DHS-based type distribution

For years 1964-2010:
  Pct_type^z = linear interpolation(Pct_type^1963, Pct_type^2010)

For years 2011-2015:
  Pct_type^z = linear interpolation(Pct_type^2010, Pct_type^2015)

Adjustment:
  Ensure nonimmigrant totals match DHS admissions or stock estimates
```

### 4.4 Step-by-Step Process for DACA Population

**Step 1: Estimate Eligible Population**

```
Eligibility criteria (2012 DACA):
  - Age: 15-30 at application (born after June 15, 1981)
  - Residency: In US since June 15, 2007
  - Education: In school, high school graduate, or military veteran

From 2012 ACS:
  - Foreign-born non-citizens by entry year, age, sex
  - Filter by school enrollment and educational attainment
  - Add citizen parents of citizen children variant
```

**Step 2: Apply Attainment Rates**

```
DACA_attain_{x,s}^z = EligiblePop_{x,s}^z × AttainRate_{x,s}

Where AttainRate varies by:
  - First DACA year
  - Second DACA year
  - Ultimate DACA years
```

**Step 3: Calibrate to DHS Stock**

```
Adjust DACA population to match DHS published stock (2013-2019)
```

### 4.5 Step-by-Step Process for OP (Equation 1.5.4)

**Stock Accumulation:**

```
For each projection year z:
  OP^z_{x,s,t} = OP^{z-1}_{x-1,s,t} + OI^z_{x,s,t} - OE^z_{x,s,t} - AOS^z_{x,s,t} - OD^z_{x,s,t}

Where:
  - Age x-1 at z-1 becomes age x at z (cohort aging)
  - OD^z = q^z_{x,s} × OP^z_{x,s,t} (deaths use total population mortality)
```

---

## 5. Implementation Functions

### 5.1 Data Acquisition Functions

File: `R/data_acquisition/dhs_unauthorized.R` (extend existing)

```r
#' Fetch DHS unauthorized immigrant components
#'
#' @description
#' Downloads DHS unauthorized immigrant population estimates and components.
#'
#' @param years Integer vector of years (2005-2012 for components, 2015-2022 for undercounts)
#'
#' @return data.table with unauthorized immigrant data by year
#'
#' @export
fetch_dhs_unauthorized_components <- function(years = 2005:2022)

#' Fetch DHS LPR population components
#'
#' @description
#' Downloads DHS LPR population components from unauthorized reports.
#'
#' @param years Integer vector of years
#'
#' @return data.table with LPR population components
#'
#' @export
fetch_dhs_lpr_components <- function(years = 2005:2023)
```

File: `R/data_acquisition/dhs_nonimmigrant.R`

```r
#' Fetch DHS nonimmigrant stock estimates
#'
#' @description
#' Loads DHS nonimmigrant stock estimates for April 2008, December 2010, April 2016.
#'
#' @return data.table with nonimmigrant stock by age group and sex
#'
#' @export
fetch_dhs_nonimmigrant_stock <- function()

#' Fetch DHS nonimmigrant admissions
#'
#' @description
#' Downloads nonimmigrant admissions by class of admission.
#'
#' @param years Integer vector of fiscal years
#'
#' @return data.table with admissions by class and year
#'
#' @export
fetch_dhs_nonimmigrant_admissions <- function(years = 1981:2016)

#' Fetch DHS beginning-of-year nonimmigrants
#'
#' @description
#' Downloads total beginning-of-year nonimmigrant estimates.
#'
#' @param years Integer vector of years
#'
#' @return data.table with BOY nonimmigrant totals
#'
#' @export
fetch_dhs_boy_nonimmigrants <- function(years = 2005:2022)
```

File: `R/data_acquisition/dhs_daca.R`

```r
#' Fetch DHS DACA initial grants
#'
#' @description
#' Downloads DACA initial grant approvals by fiscal year.
#'
#' @param years Integer vector of fiscal years
#'
#' @return data.table with DACA grants by year
#'
#' @export
fetch_dhs_daca_grants <- function(years = 2013:2018)

#' Fetch DHS DACA population stock
#'
#' @description
#' Downloads DACA population stock by age and sex.
#'
#' @param years Integer vector of years
#'
#' @return data.table with DACA stock by year, age, sex
#'
#' @export
fetch_dhs_daca_stock <- function(years = 2013:2019)
```

File: `R/data_acquisition/acs_foreign_born.R`

```r
#' Fetch ACS foreign-born new arrivals
#'
#' @description
#' Downloads ACS PUMS data for foreign-born persons by entry year.
#' Calculates new arrivals as foreign-born entering in each year.
#'
#' @param acs_years Integer vector of ACS years (2000-2022)
#'
#' @return data.table with foreign-born by ACS year, entry year, age, sex
#'
#' @export
fetch_acs_foreign_born_new_arrivals <- function(acs_years = 2000:2022)

#' Fetch ACS 2012 special populations
#'
#' @description
#' Downloads 2012 ACS data for DACA/DAPA eligibility estimation.
#' Includes foreign-born citizens, non-citizens in school/graduates,
#' and citizen parents of citizen children.
#'
#' @return data.table with special populations by entry year, age, sex
#'
#' @export
fetch_acs_2012_special_populations <- function()

#' Fetch ACS 2012 TPS eligible populations
#'
#' @description
#' Downloads 2012 ACS data for TPS eligibility by country of origin.
#'
#' @return data.table with TPS-eligible by country, entry year, age, sex
#'
#' @export
fetch_acs_2012_tps_eligible <- function()
```

File: `R/data_acquisition/mpi_daca.R`

```r
#' Fetch MPI DACA eligibility estimates
#'
#' @description
#' Downloads Migration Policy Institute estimates of DACA-eligible population.
#'
#' @return data.table with eligibility by age group and sex
#'
#' @export
fetch_mpi_daca_eligible <- function()
```

### 5.2 Core Projection Functions

File: `R/demography/temp_unlawful_immigration.R`

```r
#' Calculate O immigration distribution from historical data
#'
#' @description
#' Develops age-sex-type distribution for O immigration using
#' ACS foreign-born new arrivals minus LPR new arrivals.
#'
#' @param acs_foreign_born ACS foreign-born new arrivals
#' @param lpr_new_arrivals LPR new arrivals from LPR Immigration subprocess
#' @param undercount_factors Undercount adjustment factors
#' @param reference_years Years for distribution (default: 2015:2019)
#'
#' @return data.table with columns: age, sex, type, distribution
#'
#' @export
calculate_o_immigration_distribution <- function(acs_foreign_born,
                                                  lpr_new_arrivals,
                                                  undercount_factors,
                                                  reference_years = 2015:2019)

#' Split O immigration distribution by type
#'
#' @description
#' Allocates O immigration distribution to types: never-authorized,
#' nonimmigrant, and visa-overstayer.
#'
#' @param o_distribution Total O distribution by age, sex
#' @param dhs_nonimmigrant_stock DHS nonimmigrant stock estimates
#' @param dhs_unauthorized DHS unauthorized estimates
#' @param overstay_pcts Overstay percentages by age
#'
#' @return data.table with type-specific distributions
#'
#' @export
split_o_distribution_by_type <- function(o_distribution,
                                          dhs_nonimmigrant_stock,
                                          dhs_unauthorized,
                                          overstay_pcts)

#' Project O immigration (Equation 1.5.1)
#'
#' @description
#' Projects temporary or unlawfully present immigration by applying
#' distribution to Trustees assumptions.
#'
#' @param years Integer vector of projection years
#' @param distribution Age-sex-type distribution (ODIST)
#' @param assumptions Trustees assumptions for total O immigration
#'
#' @return data.table with columns: year, age, sex, type, immigration
#'
#' @export
project_o_immigration <- function(years, distribution, assumptions)
```

File: `R/demography/temp_unlawful_emigration.R`

```r
#' Calculate O emigration departure rates
#'
#' @description
#' Calculates departure rates from 2008-2010 stock build-up,
#' with smoothing and recession adjustments.
#'
#' @param o_population Historical O population stock
#' @param o_emigration Historical O emigration flows
#' @param o_immigration Historical O immigration flows
#' @param mortality_qx Death probabilities
#' @param aos Adjustments of status
#' @param reference_years Years for rate calculation (default: 2008:2010)
#'
#' @return data.table with departure rates by age, sex, type
#'
#' @export
calculate_o_departure_rates <- function(o_population,
                                         o_emigration,
                                         o_immigration,
                                         mortality_qx,
                                         aos,
                                         reference_years = 2008:2010)

#' Adjust departure rates by type and policy period
#'
#' @description
#' Applies type-specific adjustments to departure rates:
#' - Never-authorized recent arrivals: 2× rate
#' - DACA eligible: Lower rates
#' - Nonimmigrants: Gradual increase to ultimate
#'
#' @param base_rates Base departure rates by age, sex, type
#' @param years Projection years
#' @param config Configuration with rate adjustment parameters
#'
#' @return data.table with adjusted rates by year, age, sex, type
#'
#' @export
adjust_departure_rates <- function(base_rates, years, config)

#' Project O emigration (Equation 1.5.2)
#'
#' @description
#' Projects temporary or unlawfully present emigration by applying
#' departure rates to stock population.
#'
#' @param years Integer vector of projection years
#' @param o_population O population stock at beginning of year
#' @param departure_rates Adjusted departure rates
#'
#' @return data.table with columns: year, age, sex, type, emigration
#'
#' @export
project_o_emigration <- function(years, o_population, departure_rates)
```

File: `R/demography/temp_unlawful_stock.R`

```r
#' Calculate historical O population type splits
#'
#' @description
#' Splits historical O population into types using linear interpolation
#' between reference points (1963, 2010, 2015).
#'
#' @param o_population Total O population by year, age, sex
#' @param type_pcts_1963 Type percentages at Dec 31, 1963 (100% nonimmigrant)
#' @param type_pcts_2010 Type percentages at Dec 31, 2010
#' @param type_pcts_2015 Type percentages at Dec 31, 2015
#' @param dhs_nonimmigrant_stock DHS nonimmigrant stock for adjustment
#'
#' @return data.table with O population by year, age, sex, type
#'
#' @export
calculate_historical_type_splits <- function(o_population,
                                              type_pcts_1963,
                                              type_pcts_2010,
                                              type_pcts_2015,
                                              dhs_nonimmigrant_stock)

#' Project O population stock (Equation 1.5.4)
#'
#' @description
#' Projects O population stock using cohort-component method.
#'
#' @param starting_pop Starting O population (Dec 31 of last historical year)
#' @param o_immigration Projected O immigration
#' @param o_emigration Projected O emigration
#' @param aos Projected adjustments of status
#' @param mortality_qx Death probabilities
#'
#' @return data.table with columns: year, age, sex, type, population
#'
#' @export
project_o_population_stock <- function(starting_pop,
                                        o_immigration,
                                        o_emigration,
                                        aos,
                                        mortality_qx)

#' Calculate net O immigration (Equation 1.5.3)
#'
#' @description
#' Calculates net temporary or unlawfully present immigration.
#'
#' @param o_immigration O immigration by year, age, sex, type
#' @param o_emigration O emigration by year, age, sex, type
#' @param aos Adjustments of status by year, age, sex
#'
#' @return data.table with net O immigration
#'
#' @export
calculate_net_o_immigration <- function(o_immigration, o_emigration, aos)
```

File: `R/demography/daca_projection.R`

```r
#' Estimate DACA-eligible population
#'
#' @description
#' Estimates population eligible for DACA based on age, residency,
#' and educational requirements.
#'
#' @param acs_2012_special 2012 ACS special populations
#' @param mpi_estimates MPI eligibility estimates
#' @param config Configuration with eligibility criteria
#'
#' @return data.table with eligible population by age, sex
#'
#' @export
estimate_daca_eligible <- function(acs_2012_special, mpi_estimates, config)

#' Project DACA population
#'
#' @description
#' Projects DACA population using eligibility estimates and attainment rates.
#'
#' @param eligible_pop DACA-eligible population
#' @param attainment_rates DACA attainment rates by age, sex
#' @param dhs_stock DHS DACA stock for calibration
#' @param years Projection years
#'
#' @return data.table with DACA population by year, age, sex
#'
#' @export
project_daca_population <- function(eligible_pop,
                                     attainment_rates,
                                     dhs_stock,
                                     years)
```

### 5.3 Main Entry Point

File: `R/demography/temp_unlawful_immigration.R` (continued)

```r
#' Run complete O immigration projection (main entry point)
#'
#' @description
#' Main function orchestrating the complete temporary or unlawfully present
#' immigration projection. Implements Equations 1.5.1 - 1.5.4.
#'
#' @param acs_foreign_born ACS foreign-born data
#' @param lpr_data LPR immigration data from Phase 3
#' @param mortality_qx Death probabilities from Phase 2
#' @param historical_o Historical O population from Phase 4
#' @param dhs_data DHS data (unauthorized, nonimmigrant, DACA)
#' @param projection_years Years to project (default: 2023:2099)
#'
#' @return list with:
#'   - o_immigration: OI by year, age, sex, type
#'   - o_emigration: OE by year, age, sex, type
#'   - net_o: NO by year, age, sex, type
#'   - o_population: OP by year, age, sex, type
#'   - daca_population: DACA by year, age, sex
#'   - distributions: ODIST
#'   - departure_rates: Departure rates by age, sex, type
#'
#' @export
run_o_immigration_projection <- function(acs_foreign_born,
                                          lpr_data,
                                          mortality_qx,
                                          historical_o,
                                          dhs_data,
                                          projection_years = 2023:2099)
```

---

## 6. Configuration

### 6.1 O Immigration Configuration (add to tr2025.yaml)

```yaml
o_immigration:
  # TR2025 aggregate assumptions
  total_o_immigration:
    2022: 2200000
    2023: 2700000
    2024: 2600000
    2025: 2000000
    ultimate: 1350000
    ultimate_year: 2026

  # Distribution source and reference years
  distribution_source: "acs"
  distribution_years: [2015, 2016, 2017, 2018, 2019]

  # Type categories
  types:
    - code: "N"
      name: "never_authorized"
      description: "Entered without authorization"
    - code: "I"
      name: "nonimmigrant"
      description: "Legal temporary immigrants"
    - code: "V"
      name: "visa_overstayer"
      description: "Overstayed authorized period"

  # Departure rate parameters
  departure_rates:
    base_period: [2008, 2009, 2010]
    never_authorized_recent_multiplier: 2.0
    nonimmigrant_initial_year: 2015
    nonimmigrant_ultimate_year: 2025
    daca_rate_reduction: 0.50  # 50% lower rates for DACA

  # Historical type split reference points
  type_splits:
    reference_year_1: 1963  # 100% nonimmigrant
    reference_year_2: 2010  # DHS-based split
    reference_year_3: 2015  # DHS-based split

  # DACA parameters
  daca:
    program_start_year: 2012
    eligibility_cutoff_birth_year: 1981  # Born after June 15, 1981
    entry_date_cutoff: "2007-06-15"
    age_range_at_application: [15, 30]
    stock_calibration_years: [2013, 2014, 2015, 2016, 2017, 2018, 2019]
    new_grants_assumed: false  # No new grants assumed 2019+

  # Data sources
  data_sources:
    acs_years: [2000, 2022]
    dhs_unauthorized_years: [2005, 2022]
    dhs_nonimmigrant_stock_years: [2008, 2010, 2016]
    dhs_daca_years: [2013, 2019]

  # Age range
  ages:
    min_age: 0
    max_age: 99
    max_age_plus: 100  # 100+ grouped
```

---

## 7. Validation Framework

### 7.1 Validation Data Available

From TR2025:
- Table V.A2: Net immigration assumptions (includes O immigration totals)
- DHS published unauthorized population estimates (external)
- DHS published DACA population (external)

### 7.2 Validation Points

| Output | Metric | Tolerance | Source |
|--------|--------|-----------|--------|
| Total OI | Annual total immigration | Exact | TR assumptions |
| Total NO | Net O immigration | 5% | TR2025 V.A2 |
| OP total | O population stock | 10% | DHS unauthorized |
| DACA pop | DACA stock | 5% | DHS DACA |
| Type split | Nonimmigrant share | 15% | DHS estimates |

### 7.3 Validation Functions

File: `R/validation/validate_o_immigration.R`

```r
#' Validate O immigration outputs against TR2025 and DHS
#'
#' @param o_immigration Projected O immigration
#' @param o_emigration Projected O emigration
#' @param net_o Net O immigration
#' @param o_population O population stock
#' @param daca_population DACA population
#' @param tr2025_assumptions TR2025 assumptions
#' @param dhs_estimates DHS external estimates
#'
#' @return Validation report
#'
#' @export
validate_o_immigration_outputs <- function(o_immigration,
                                            o_emigration,
                                            net_o,
                                            o_population,
                                            daca_population,
                                            tr2025_assumptions,
                                            dhs_estimates)

#' Validate O immigration totals match Trustees assumptions
#' @export
validate_o_totals <- function(o_immigration, assumptions)

#' Validate O population against DHS unauthorized estimates
#' @export
validate_o_against_dhs <- function(o_population, dhs_unauthorized)

#' Validate DACA population against DHS stock
#' @export
validate_daca_against_dhs <- function(daca_population, dhs_daca_stock)

#' Validate distribution properties
#' @export
validate_o_distribution <- function(distribution)
```

---

## 8. Implementation Sequence

### Phase 5A: DHS Data Acquisition - COMPLETED (January 18, 2026)

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 5A.1 | Implement DHS unauthorized components fetcher | None | dhs_unauthorized.R (existing) |
| [x] | 5A.2 | Implement DHS nonimmigrant stock fetcher | None | dhs_nonimmigrant.R |
| [x] | 5A.3 | Implement DHS nonimmigrant admissions fetcher | None | dhs_nonimmigrant.R |
| [x] | 5A.4 | Implement DHS BOY nonimmigrants fetcher | None | dhs_nonimmigrant.R |
| [x] | 5A.5 | Implement DHS DACA grants fetcher | None | dhs_daca.R |
| [x] | 5A.6 | Implement DHS DACA stock fetcher | None | dhs_daca.R |
| [x] | 5A.7 | Fetch and cache all DHS data | 5A.1-5A.6 | Cached data |

### Phase 5B: ACS Data Acquisition

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 5B.1 | Implement ACS foreign-born flows fetcher | None | acs_pums.R (existing) |
| [x] | 5B.2 | Implement ACS 2012 special populations fetcher | None | acs_foreign_born.R |
| [x] | 5B.3 | Implement undercount factor calculation | ACS data | acs_foreign_born.R |
| [x] | 5B.4 | Fetch and cache ACS data (2006-2023) | 5B.1-5B.3 | Cached data |

### Phase 5C: Distribution Development

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 5C.1 | Implement O immigration distribution calculation | 5A, 5B, Phase 3 | temp_unlawful_immigration.R |
| [x] | 5C.2 | Implement type split function | 5C.1 | temp_unlawful_immigration.R |
| [x] | 5C.3 | Implement overstay percentage estimation | DHS data | temp_unlawful_immigration.R |
| [x] | 5C.4 | Calculate and validate ODIST | 5C.1-5C.3 | Distribution data |

### Phase 5D: Departure Rate Development - COMPLETED (January 18, 2026)

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 5D.1 | Implement departure rate calculation | Phase 4, Phase 2 | temp_unlawful_emigration.R |
| [x] | 5D.2 | Implement rate smoothing and adjustment | 5D.1 | temp_unlawful_emigration.R |
| [x] | 5D.3 | Implement type-specific rate adjustments | 5D.2 | temp_unlawful_emigration.R |
| [x] | 5D.4 | Implement policy-period rate transitions | 5D.3 | temp_unlawful_emigration.R |
| [x] | 5D.5 | Calculate and validate departure rates | 5D.1-5D.4 | Rate data |

### Phase 5E: Core Projection Functions - COMPLETED (January 18, 2026)

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 5E.1 | Implement project_o_immigration (Eq 1.5.1) | 5C | temp_unlawful_immigration.R |
| [x] | 5E.2 | Implement project_o_emigration (Eq 1.5.2) | 5D | temp_unlawful_emigration.R |
| [x] | 5E.3 | Implement calculate_net_o_immigration (Eq 1.5.3) | 5E.1, 5E.2 | temp_unlawful_stock.R |
| [x] | 5E.4 | Implement historical type splits | 5A | temp_unlawful_stock.R |
| [x] | 5E.5 | Implement project_o_population_stock (Eq 1.5.4) | 5E.1-5E.4 | temp_unlawful_stock.R |
| [x] | 5E.6 | Implement run_full_o_projection | All above | temp_unlawful_stock.R |

### Phase 5F: DACA Projection

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 5F.1 | Implement estimate_daca_eligible | 5B | daca_projection.R |
| [ ] | 5F.2 | Implement DACA attainment rate estimation | 5F.1, 5A | daca_projection.R |
| [ ] | 5F.3 | Implement project_daca_population | 5F.1, 5F.2 | daca_projection.R |
| [ ] | 5F.4 | Calibrate DACA to DHS stock | 5F.3 | daca_projection.R |

### Phase 5G: Validation

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 5G.1 | Implement validation functions | All above | validate_o_immigration.R |
| [ ] | 5G.2 | Validate OI totals against TR assumptions | 5G.1 | Validation report |
| [ ] | 5G.3 | Validate O population against DHS | 5G.1 | Validation report |
| [ ] | 5G.4 | Validate DACA against DHS stock | 5G.1 | Validation report |

### Phase 5H: Targets Integration

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 5H.1 | Add O immigration targets to _targets.R | All above | Updated pipeline |
| [ ] | 5H.2 | Test pipeline with cached data | 5H.1 | Pipeline execution |
| [ ] | 5H.3 | Run full projection and validate | 5H.2 | Complete outputs |

---

## 9. Technical Specifications

### 9.1 Data Structures

**O Immigration (OI):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year (2023-2099)
age             integer   Single year of age (0-99, 100+)
sex             character "male" or "female"
type            character "never_authorized", "nonimmigrant", "visa_overstayer"
immigration     numeric   Annual immigration count
```

**O Emigration (OE):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age
sex             character "male" or "female"
type            character Type category
emigration      numeric   Annual emigration count
```

**Net O Immigration (NO):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age
sex             character "male" or "female"
type            character Type category
net_immigration numeric   Net immigration (OI - OE - AOS)
```

**O Population Stock (OP):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age
sex             character "male" or "female"
type            character Type category
population      numeric   December 31 population
```

**DACA Population:**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age
sex             character "male" or "female"
population      numeric   December 31 DACA population
status          character "eligible", "recipient", "aged_out"
```

**O Distribution (ODIST):**
```
Column          Type      Description
------          ----      -----------
age             integer   Single year of age
sex             character "male" or "female"
type            character Type category
distribution    numeric   Proportion (sums to 1.0)
```

**Departure Rates:**
```
Column          Type      Description
------          ----      -----------
age             integer   Single year of age
sex             character "male" or "female"
type            character Type category
rate            numeric   Annual departure rate (0-1)
policy_period   character "pre_2015", "transition", "post_2015"
```

### 9.2 Key Methodological Notes

**Distribution Development:**
- Based on 2015-2019 average (pre-COVID reference period)
- ACS undercount adjustment critical for accurate estimates
- Type splits rely heavily on limited DHS point-in-time estimates

**Departure Rates:**
- 2008-2010 period reflects recession conditions
- Smoothing required to avoid artifacts
- Policy changes (2015 Executive Actions) create structural breaks

**DACA Considerations:**
- No new grants assumed after 2019 per TR methodology
- Program uncertainty not modeled (assumed to continue)
- Age-out of eligible population accounted for

**Type Interpolation:**
- 1963 baseline (100% nonimmigrant) is an assumption
- Linear interpolation is simplification of complex dynamics
- Nonimmigrant total constraints improve consistency

### 9.3 Potential Simplifications for Initial Implementation

1. **Start with aggregate totals:**
   - Project total OI and OE first
   - Add type splits as enhancement

2. **Simplified departure rates:**
   - Use single average rate initially
   - Add type-specific and policy-adjusted rates later

3. **Skip DACA initially:**
   - DACA is subset of O population
   - Implement core O projection first
   - Add DACA as separate enhancement

4. **Use TR2025 V.A2 directly:**
   - V.A2 contains net O flows by year
   - Can use these directly as validation targets

### 9.4 Dependencies on Previous Subprocesses

| Subprocess | Data Needed | Used In |
|------------|-------------|---------|
| MORTALITY | Death probabilities (qx) | O deaths calculation |
| LPR IMMIGRATION | NEW arrivals | O immigration calculation |
| LPR IMMIGRATION | AOS by type | Net O and stock calculation |
| HISTORICAL | Historical O population | Starting stock |
| HISTORICAL | Historical O net flows | Distribution development |

### 9.5 External Data Dependencies

| Source | Data | Frequency | Notes |
|--------|------|-----------|-------|
| DHS OHSS | Unauthorized estimates | Annual (with lag) | May require web scraping |
| DHS USCIS | DACA statistics | Quarterly | Published data |
| DHS I-94 | Nonimmigrant arrivals | Annual | Yearbook tables |
| Census ACS | Foreign-born flows | Annual | PUMS microdata |
| MPI | DACA eligibility | Occasional | Research reports |

---

## Appendix A: DHS Data Sources

### A.1 Unauthorized Immigrant Population Reports

**URL Pattern:** `https://www.dhs.gov/immigration-statistics/population-estimates/unauthorized-resident`

**Available Reports:**
- Estimates of the Unauthorized Immigrant Population (various years)
- Components and methodology documentation
- Undercount adjustments

### A.2 Nonimmigrant Stock Estimates

**Published Years:**
- April 2008 (by age group and sex)
- December 2010 (by age group and sex)
- April 2016 (by age group and sex)

**Age Groups:**
- Under 18, 18-24, 25-34, 35-44, 45-54, 55-64, 65+

### A.3 DACA Statistics

**URL:** `https://www.uscis.gov/tools/reports-and-studies/immigration-and-citizenship-data`

**Available Data:**
- Initial approvals by fiscal year
- Population by state, age, sex
- Renewal statistics

---

## Appendix B: Comparison with Previous Subprocesses

| Aspect | LPR Immigration | O Immigration |
|--------|-----------------|---------------|
| Annual total | 1,050,000 (ultimate) | 1,350,000 (ultimate) |
| Type dimension | NEW vs AOS | Never-auth, Nonimm, Overstay |
| Distribution source | CBO | ACS - LPR |
| Emigration method | 25% of immigration | Stock-based rates |
| Special populations | None | DACA |
| Data quality | High (DHS published) | Medium (residual calculation) |
| Complexity | Medium | High |

---

## Appendix C: Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| DHS data gaps | High | Medium | Interpolate between available years |
| ACS undercount uncertainty | Medium | Medium | Use multiple adjustment methods |
| Departure rate instability | Medium | High | Smooth rates, sensitivity testing |
| DACA program changes | High | Low | Modular DACA implementation |
| Type split assumptions | High | Medium | Document assumptions, allow override |
| Limited nonimmigrant stock data | High | Medium | Interpolate from 3 reference points |
| Policy discontinuities (2015) | High | Medium | Explicit transition modeling |

---

## Appendix D: Simplified Approach (Recommended for Initial Implementation)

Given the complexity and data limitations, a phased approach is recommended:

### Phase 5 Core (MVP)

1. **Use TR2025 V.A2 net O flows directly**
   - Net O immigration totals available in V.A2
   - Apply age-sex distribution from historical data
   - Skip type dimension initially

2. **Simple departure rate model**
   - Calculate average departure rate from historical stock changes
   - Apply uniformly by age-sex

3. **Stock from cohort-component**
   - Use simplified: OP^z = OP^{z-1} + NO^z - OD^z
   - Deaths from mortality rates

### Phase 5 Enhanced (Future)

1. **Type splits**
   - Implement full type dimension
   - DHS nonimmigrant calibration

2. **Policy-adjusted departure rates**
   - Pre/post 2015 rate transitions
   - DACA-eligible lower rates

3. **DACA submodule**
   - Eligibility estimation
   - Attainment projection
   - Stock calibration

---

*End of Implementation Plan*

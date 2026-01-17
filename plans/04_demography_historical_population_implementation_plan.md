# ARTEMIS: OASDI Projection Model Implementation Plan
## Phase 4: Demography Process - Historical Population Subprocess (1.4)

**Document Version:** 1.0
**Date:** January 2026
**Scope:** Historical Population subprocess implementation following LPR Immigration completion
**Source:** SSA 2025 Long-Range Model Documentation, Section 1.4 Historical Population

---

## Progress Tracking

> **IMPORTANT**: This plan document should be updated as implementation progresses. Each task should be marked with its current status.

### Status Legend
- [ ] Not started
- [~] In progress
- [x] Completed

### Current Status
**Last Updated:** January 17, 2026
**Current Phase:** Phase 4A - Core Census Data Acquisition (COMPLETE)
**Next Step:** Phase 4B - ACS and IPUMS Data Acquisition

### Phase 4A Progress Notes - COMPLETED (January 17, 2026)

**Implementation:**
- Created `R/data_acquisition/census_historical_population.R` with main entry point `fetch_census_historical_population()`
- Supports 4 population concepts: resident, resident_usaf, civilian, civilian_noninst
- Supports 2 reference dates: jul1 (July 1) and jan1 (January 1, via interpolation)
- Territory populations via `fetch_territory_populations()` with fallback to hardcoded estimates

**Data Fetched:**
- Resident/USAF populations: 1980-2024 (July 1), 1981-2024 (January 1)
- Civilian populations: 2010-2024 (July 1), 2011-2024 (January 1)
- Civilian noninstitutionalized: 2010-2024 (July 1), 2011-2024 (January 1)
- Territory populations: 2010-2023 (PR, VI, GU, MP, AS) via hardcoded estimates

**Validation:**
- Population totals match expected U.S. figures (227M in 1980 → 337M in 2023)
- Age structure reasonable (73M ages 0-17, 204M ages 18-64, 59M ages 65+ in 2023)

**Notes:**
- Civilian and civilian noninstitutionalized currently use resident as proxy (~1% difference)
- Census IDB API requires different parameters; using hardcoded territory estimates as fallback
- True USAF (armed forces overseas) data uses resident as proxy (~0.1% difference)

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

The HISTORICAL subprocess provides estimates of the Social Security area population for each year from December 31, 1940, through December 31, 2022. This subprocess is critical because it:

1. Establishes the starting population for all future projections
2. Provides the foundation for the PROJECTED POPULATION subprocess (Section 1.8)
3. Disaggregates population by marital status for auxiliary benefit calculations
4. Tracks temporary/unlawfully present immigrant populations

### 1.2 Social Security Area Population Components

The Social Security area population consists of:

| Component | Description |
|-----------|-------------|
| U.S. Resident Population | Residents of 50 states and D.C. |
| Armed Forces Overseas | U.S. military personnel stationed abroad |
| Net Census Undercount | Adjustment for census enumeration errors |
| Territory Residents | Puerto Rico, Virgin Islands, Guam, Northern Mariana Islands, American Samoa |
| Federal Civilian Employees Overseas | Government employees stationed abroad |
| Dependents Overseas | Dependents of armed forces and federal employees overseas |
| Residual Beneficiaries Abroad | OASDI beneficiaries living outside U.S. |
| Other Citizens Overseas | Other U.S. citizens living abroad |

### 1.3 Primary Outputs

| Output | Symbol | Description |
|--------|--------|-------------|
| Historical Population by age and sex | P^z_{x,s} | Total SS area population |
| Historical Population by age, sex, and marital status | P^z_{x,s,m} | Population with marital status |
| Temporary/Unlawfully Present Population | O^z_{x,s} | Unauthorized/temporary immigrants |
| Civilian Noninstitutionalized Population | C^z_{x,s,m} | Non-military, non-institutional population |

### 1.4 Marital Status Categories

| Code | Status | Definition |
|------|--------|------------|
| S | Single | Never been married |
| M | Married | Currently married |
| W | Widowed | Previously married, spouse deceased |
| D | Divorced | Previously married, legally divorced |

For civilian noninstitutionalized population, "Married" is split into:
- Married, spouse present
- Separated

### 1.5 Tab Years

Historical populations are estimated precisely for certain "tab years":
- 1940, 1950, 1956, 1960
- Each December from 1969 through 2009
- Last year of historical data (2022 for TR2025)

Populations for years between tab years are interpolated using components of change (births, deaths, migration).

---

## 2. Mathematical Framework

### 2.1 Equation 1.4.1 – Historical Population by Age and Sex (P^z_{x,s})

For ages 0-84 at each tab year:

$$P^z_{x,s} = USAF^z_{x,s} + UC^z_{x,s} + TERR^z_{x,s} + FED^z_{x,s} + DEP^z_{x,s} + BEN^z_{x,s} + OTH^z_{x,s}$$

Where:
- USAF = U.S. resident population + Armed Forces overseas (from Census Bureau)
- UC = Net census undercount adjustment
- TERR = Territory residents (PR, VI, Guam, CNMI, AS)
- FED = Federal civilian employees overseas
- DEP = Dependents of armed forces and federal employees overseas
- BEN = Residual beneficiaries living abroad
- OTH = Other citizens overseas

**For ages 85+:**

$$P^z_{x,s} = \frac{BuiltUp^z_{x,s} \times Total85+_s}{TotalBuiltUp85+_s}$$

Where built-up estimates account for deaths and immigration from previous tab years.

**For inter-tab years:**
Populations are estimated using components of change and then ratio-adjusted to eliminate closure error at tab years.

### 2.2 Equation 1.4.2 – Historical Population by Age, Sex, and Marital Status (P^z_{x,s,m})

$$P^z_{x,s,m} = P^z_{x,s} \times MaritalPct^z_{x,s,m}$$

Where marital percentages are derived from:
1. Decennial Census PUMS (1940-2000)
2. American Community Survey (2000-2023)
3. H.S. Beers interpolation for single-year ages

**Consistency constraints:**
- Number of married men = Number of married women (pre-2013)
- After 2013: Same-sex marriages recognized (2.5% male, 4.5% female assumed same-sex eligible)
- Marriage grids maintain consistency across age-spouse combinations

### 2.3 Equation 1.4.3 – Temporary/Unlawfully Present Population (O^z_{x,s})

$$O^z_{x,s} = Residual^z_{x,s}$$

Where residual is backed out from:
- Beginning and end of year populations
- Births, deaths
- LPR immigrants, adjustments of status
- Legal emigrants

**Post-2000 adjustments:**
- 2001-2004: Linear interpolation between 2000 and 2005 totals
- 2005-2023: Forced to match DHS estimates

### 2.4 Equation 1.4.4 – Civilian Noninstitutionalized Population (C^z_{x,s,m})

$$C^z_{x,s,m} = CivNonInst^z_{x,s} \times MaritalPct^z_{x,s,m}$$

Where:
- CivNonInst totals come directly from Census Bureau
- Marital percentages from ACS PUMS
- Available from 2010 onwards
- "Married" split into "married, spouse present" and "separated"

---

## 3. Input Data Requirements

### 3.1 Data Sources Summary

The Historical Population subprocess requires **44 distinct data inputs** organized into three categories:

| Category | Items | Priority |
|----------|-------|----------|
| Long-Range OASDI Projection Data (Internal) | 6 | High |
| U.S. Census Bureau Data | 24 | High |
| Other Input Data | 14 | Medium-High |

### 3.2 Long-Range OASDI Projection Data (From Previous Subprocesses)

| # | Data | Source | Years | Status |
|---|------|--------|-------|--------|
| 1 | Probabilities of death (qx) | MORTALITY subprocess | 1941-2023 | Available (Phase 2) |
| 2 | LPR immigrants (NEW + AOS) | LPR IMMIGRATION subprocess | 1941-2022 | Available (Phase 3) |
| 3 | Legal emigrants | LPR IMMIGRATION subprocess | 1941-2022 | Available (Phase 3) |
| 4 | IRCA legalizations | LPR IMMIGRATION subprocess | 1986-1991 | Not implemented |
| 5 | Non-IRCA adjustments of status | LPR IMMIGRATION subprocess | 1941-2022 | Available (Phase 3) |
| 6 | Birth rates by age of mother | FERTILITY subprocess | 1941-2023 | Available (Phase 1) |

**Notes:**
- Items 1, 2, 3, 5, 6 are available from completed subprocesses
- Item 4 (IRCA) requires historical data that may not be publicly available

### 3.3 U.S. Census Bureau Data

| # | Data | Years | Reference Date | Age Detail | API/Source |
|---|------|-------|----------------|------------|------------|
| 7 | USAF population estimates | 1940-79 | July 1 | 0-84, 85+ | Static file |
| 8 | Decennial census resident pop | 1970-2020 | April 1 | 0-85+ | Census API |
| 9 | Total resident + USAF for census | 1990-2020 | January | Total only | Census API |
| 10 | Resident population estimates | 1980-2023 | July 1 | 0-99, 100+ | Census API |
| 11 | Resident + USAF estimates | 1980-2023 | July 1 | 0-99, 100+ | Census API |
| 12 | Resident population estimates | 1981-2023 | January 1 | 0-99, 100+ | Census API |
| 13 | Resident + USAF estimates | 1981-2023 | January 1 | 0-99, 100+ | Census API |
| 14 | Civilian population | 2010-2023 | July 1 | 0-99, 100+ | Census API |
| 15 | Civilian noninstitutionalized pop | 2010-2023 | July 1 | 0-99, 100+ | Census API |
| 16 | Civilian population | 2011-2023 | January 1 | 0-99, 100+ | Census API |
| 17 | Civilian noninstitutionalized pop | 2011-2023 | January 1 | 0-99, 100+ | Census API |
| 18 | Population by marital status (ACS) | 2000-2023 | Survey | Age groups | ACS PUMS |
| 19 | CivNonInst by marital status (ACS) | 2006-2023 | Survey | Age groups | ACS PUMS |
| 20 | Census undercount factors | Decennial | Census | 0-85+ | Static/Research |
| 21 | Territory total populations | 1951-2023 | Annual | Total | Census IDB |
| 22 | Territory decennial census | 1950-2000 | Decennial | 18 age groups | Static files |
| 23 | Territory July populations | 2000-2023 | July 1 | Single year | Census API |
| 24 | Existing marriages (ACS) | 2006-2023 | Survey | Age grid | ACS PUMS |
| 25 | Foreign-born flows (ACS) | 2013-2023 | Survey | Single year | ACS PUMS |
| 26 | Existing marriages (decennial) | 1940-2000 | Decennial | Age groups | IPUMS |
| 27 | Population by marital status (dec) | 1940-2000 | Decennial | Age groups | IPUMS |
| 28 | Net immigration estimates | 2000-2023 | April-July | By age/sex | Census |
| 29 | Americans overseas estimate | ~2003 | Various | Total | Census/International |
| 30 | Alaska/Hawaii civilian pop | 1940-49 | Annual | Total | Static |
| 31 | Alaska/Hawaii census pop | 1940, 1950 | Decennial | Age groups | Static |

### 3.4 Other Input Data

| # | Data | Source | Years | Purpose |
|---|------|--------|-------|---------|
| 32 | Outside area populations | Dept of State | 1951-1990 | Fed employees overseas |
| 33 | OASDI beneficiaries abroad (total) | SSA Supplement | 1957-2022 | Residual beneficiaries |
| 34 | OASDI beneficiaries abroad (by age) | SSA Supplement | 1997+ | Age distribution |
| 35 | Monthly births by sex | NCHS | 1931-2023 | Birth cohort estimates |
| 36 | NSFG same-sex marriage eligibility | NCHS | 2011-15 | Same-sex marriage model |
| 37 | NSFG same-sex vs opposite-sex | NCHS | 2011-17 | Same-sex marriage model |
| 38 | Unauthorized immigrants (DHS) | DHS | 2005-2023 | O population estimates |
| 39 | Federal employees overseas (total) | OPM | 1998-2013 | Fed employees component |
| 40 | Federal employees overseas (by age) | OPM | 1980-2023 | Age distribution |
| 41 | Armed forces in territories | DoD | 1990-2020 | Territory adjustment |
| 42 | 1940 85+ distribution | Assumed | 1940 | Historical starting point |
| 43 | Territory addition populations | Assumed | 1951, 1957, 1961 | When territories added |
| 44 | Historical armed forces counts | Historical | 1940-57 | Early years adjustment |

### 3.5 Data Acquisition Priority

**Phase 4A - Core Census Data (High Priority):**
- Items 10-13: Modern Census population estimates (single-year ages)
- Items 14-17: Civilian and civilian noninstitutionalized populations
- Items 18-19: ACS marital status data
- Item 21: Territory populations

**Phase 4B - Historical Census Data (Medium Priority):**
- Items 7-9: Pre-1980 Census data
- Items 20, 22: Undercount factors and historical territory data
- Items 26-27: Historical marital status (IPUMS)
- Items 30-31: Alaska/Hawaii historical

**Phase 4C - Other Sources (Medium-Low Priority):**
- Items 32-34: Department of State and SSA data
- Items 38-41: DHS, OPM, DoD data
- Items 36-37: NSFG same-sex marriage data

**Phase 4D - Internal Subprocess Data (Already Available):**
- Items 1-6: From completed fertility, mortality, immigration subprocesses

### 3.6 Data Availability Assessment

| Data Category | Public Availability | Notes |
|--------------|---------------------|-------|
| Census population estimates | High | Census API accessible |
| ACS PUMS microdata | High | census.gov/ipums.org |
| IPUMS decennial data | High | ipums.org |
| Territory populations | Medium | Census International Database |
| SSA beneficiaries abroad | Medium | Annual Statistical Supplement |
| OPM federal employees | Medium | OPM website |
| DHS unauthorized estimates | Medium | Published reports |
| Census undercount factors | Low | Research publications |
| Dept of State historical | Low | May need archival research |
| NCHS NSFG microdata | Medium | CDC website |

---

## 4. Output Development Methodology

### 4.1 Step-by-Step Process for P^z_{x,s}

**Step 1: Establish Tab Year Populations (Ages 0-84)**

For each tab year, set:
```
P^z_{x,s} = USAF_adjusted + Undercount + Territories + FedEmployees + Dependents + Beneficiaries + OtherCitizens
```

Where USAF_adjusted is:
- Pre-1970: Average of surrounding July 1 Census counts
- 1970-2000 decennials: Modified April 1 Census populations
- Post-2000: January 1 Census estimates (ages 65+ modified by OCACT mortality)

**Step 2: Build Up Ages 85+ for Tab Years**

Starting from previous tab year:
1. Age forward survivors using mortality rates
2. Add immigrants by age
3. Subtract emigrants by age
4. Ratio-adjust to match Census 85+ total

**Step 3: Interpolate Between Tab Years**

For years between tab years:
1. Calculate expected population change from components:
   - Births to population
   - Deaths from population
   - LPR immigration in
   - Legal emigration out
   - Adjustments of status (internal transfer)
2. Apply closure ratios to eliminate error

**Step 4: Extend to Single Year of Age 100+**

For ages 100+:
- Continue same methodology
- Apply age-specific mortality rates
- Constrain to Census 100+ totals where available

### 4.2 Step-by-Step Process for P^z_{x,s,m}

**Step 1: Obtain Marital Status Distributions**

From ACS PUMS (2006+) or decennial Census PUMS (1940-2000):
- Extract population counts by age group, sex, marital status
- Convert to proportions within each age-sex cell

**Step 2: Apply Beers Interpolation**

Use H.S. Beers method to convert age-grouped proportions to single years of age (14-100+).

**Step 3: Calculate Preliminary Populations**

```
P^z_{x,s,m} = P^z_{x,s} × MaritalPct^z_{x,s,m}
```

**Step 4: Apply Consistency Constraints**

1. Adjust married counts so married males ≈ married females
2. Reconcile marriage grids with marginal married totals
3. Rebalance other marital statuses to maintain P^z_{x,s} totals

**Step 5: Incorporate Same-Sex Marriage (2013+)**

For December 31, 2013, and later:
- Split population into heterosexual (97.5% male, 95.5% female) and gay/lesbian
- Apply separate marriage grids for same-sex couples

### 4.3 Step-by-Step Process for O^z_{x,s}

**Step 1: Calculate Net Residual**

For each year:
```
Residual = EndPop - BeginPop - Births + Deaths - LPR_Immigration + Emigration - AOS
```

**Step 2: Modify Residuals for Reasonableness**

Apply constraints to ensure non-negative values and smooth time series.

**Step 3: Build Up Stock Estimates**

Using:
- Modified residuals as flows
- Deaths among temporary/unlawful population
- Same mortality rates as total population

**Step 4: Post-2000 Adjustments**

- 2001-2004: Linear interpolation from 2000 to 2005 totals
- 2005-2023: Force totals to match DHS estimates

### 4.4 Step-by-Step Process for C^z_{x,s,m}

**Step 1: Obtain Civilian Noninstitutionalized Totals**

From Census Bureau estimates for 2010-2023.

**Step 2: Apply Marital Status Distribution**

From ACS PUMS data, with "married" split into:
- Married, spouse present
- Separated

**Step 3: Validate Against Total Population**

Ensure consistency with P^z_{x,s,m} totals.

---

## 5. Implementation Functions

### 5.1 Data Acquisition Functions

File: `R/data_acquisition/census_historical_population.R`

```r
#' Fetch Census population estimates by reference date
#'
#' @description
#' Downloads Census population estimates for various reference dates
#' (January 1, April 1, July 1) and population concepts.
#'
#' @param years Integer vector of years
#' @param reference_date "jan1", "apr1", or "jul1"
#' @param concept "resident", "resident_usaf", "civilian", "civilian_noninst"
#' @param ages Integer vector of ages (default: 0:100)
#'
#' @return data.table with columns: year, age, sex, population
#'
#' @export
fetch_census_population <- function(years,
                                     reference_date = "jan1",
                                     concept = "resident_usaf",
                                     ages = 0:100)

#' Fetch decennial census population
#'
#' @param census_years Decennial years (1970, 1980, ..., 2020)
#'
#' @return data.table with April 1 populations
#'
#' @export
fetch_decennial_census_population <- function(census_years = seq(1970, 2020, 10))

#' Fetch territory populations from Census International Database
#'
#' @param territories Vector of territory codes
#' @param years Integer vector of years
#'
#' @return data.table with territory populations
#'
#' @export
fetch_territory_populations <- function(territories = c("PR", "VI", "GU", "MP", "AS"),
                                         years = 1951:2023)
```

File: `R/data_acquisition/acs_marital_status.R`

```r
#' Fetch ACS population by marital status
#'
#' @description
#' Downloads ACS PUMS data and tabulates population by marital status.
#'
#' @param years Integer vector of years (2006-2023)
#' @param concept "total" or "civilian_noninst"
#'
#' @return data.table with columns: year, age_group, sex, marital_status, population
#'
#' @export
fetch_acs_marital_status <- function(years = 2006:2023,
                                      concept = "total")

#' Fetch existing marriages grid from ACS
#'
#' @description
#' Creates age-of-husband by age-of-wife marriage grid.
#'
#' @param years Integer vector of years
#' @param include_same_sex Logical: include same-sex marriages (2012+)
#'
#' @return list of matrices by year
#'
#' @export
fetch_marriage_grids <- function(years = 2006:2023,
                                  include_same_sex = TRUE)
```

File: `R/data_acquisition/ipums_historical.R`

```r
#' Fetch historical marital status from IPUMS
#'
#' @description
#' Downloads decennial census microdata from IPUMS for marital status.
#'
#' @param census_years Decennial years (1940, 1950, ..., 2000)
#'
#' @return data.table with historical marital status distributions
#'
#' @export
fetch_ipums_marital_status <- function(census_years = seq(1940, 2000, 10))

#' Fetch historical marriage grids from IPUMS
#'
#' @param census_years Decennial years
#'
#' @return list of matrices by census year
#'
#' @export
fetch_ipums_marriage_grids <- function(census_years = seq(1940, 2000, 10))
```

File: `R/data_acquisition/other_population_sources.R`

```r
#' Fetch OPM federal employees overseas
#'
#' @param years Integer vector of years
#'
#' @return data.table with federal employees by age, sex
#'
#' @export
fetch_opm_federal_employees <- function(years = 1980:2023)

#' Fetch SSA beneficiaries living abroad
#'
#' @param years Integer vector of years
#'
#' @return data.table with beneficiaries by age group
#'
#' @export
fetch_ssa_beneficiaries_abroad <- function(years = 1957:2022)

#' Fetch DHS unauthorized immigrant estimates
#'
#' @param years Integer vector of years
#'
#' @return data.table with unauthorized estimates by year
#'
#' @export
fetch_dhs_unauthorized_estimates <- function(years = 2005:2023)
```

### 5.2 Core Population Functions

File: `R/demography/historical_population.R`

```r
#' Calculate tab year populations (ages 0-84)
#'
#' @description
#' Calculates Social Security area population for tab years by combining
#' Census base population with other components.
#'
#' @param census_pop Census USAF population estimates
#' @param undercount_factors Census undercount adjustment factors
#' @param territory_pop Territory population estimates
#' @param fed_employees Federal employees overseas
#' @param dependents_overseas Dependents of fed employees and armed forces
#' @param beneficiaries_abroad OASDI beneficiaries living abroad
#' @param other_citizens Other citizens overseas
#' @param tab_years Vector of tab years
#'
#' @return data.table with columns: year, age, sex, population, source
#'
#' @export
calculate_tab_year_populations <- function(census_pop,
                                            undercount_factors,
                                            territory_pop,
                                            fed_employees,
                                            dependents_overseas,
                                            beneficiaries_abroad,
                                            other_citizens,
                                            tab_years)

#' Build up ages 85+ for tab years
#'
#' @description
#' Estimates 85+ population by aging forward from previous tab year
#' and adjusting to match Census totals.
#'
#' @param previous_pop Population at previous tab year
#' @param mortality_qx Death probabilities
#' @param immigration LPR immigration by age, sex
#' @param emigration Legal emigration by age, sex
#' @param census_85plus Census 85+ totals for adjustment
#' @param years Years to build up
#'
#' @return data.table with 85+ population by single year of age
#'
#' @export
build_up_ages_85_plus <- function(previous_pop,
                                   mortality_qx,
                                   immigration,
                                   emigration,
                                   census_85plus,
                                   years)

#' Interpolate populations between tab years
#'
#' @description
#' Uses components of change to interpolate populations for non-tab years
#' and applies closure ratios for consistency.
#'
#' @param tab_year_pops Tab year population estimates
#' @param births Annual births by sex
#' @param deaths Annual deaths by age, sex
#' @param immigration LPR immigration by age, sex
#' @param emigration Legal emigration by age, sex
#' @param aos Adjustments of status by age, sex
#'
#' @return data.table with complete historical population
#'
#' @export
interpolate_populations <- function(tab_year_pops,
                                     births,
                                     deaths,
                                     immigration,
                                     emigration,
                                     aos)

#' Calculate historical population by age and sex (main entry)
#'
#' @description
#' Main function orchestrating the complete historical population calculation.
#' Implements Equation 1.4.1.
#'
#' @param ... All required input data
#'
#' @return data.table P_x_s: columns year, age, sex, population
#'
#' @export
calculate_historical_population <- function(...)
```

File: `R/demography/historical_marital_status.R`

```r
#' Apply Beers interpolation to marital status proportions
#'
#' @description
#' Uses H.S. Beers method to convert age-grouped marital status
#' proportions to single years of age.
#'
#' @param marital_pcts Marital status proportions by age group
#' @param target_ages Single years of age (default: 14:100)
#'
#' @return data.table with single-year marital status proportions
#'
#' @export
beers_interpolate_marital_status <- function(marital_pcts,
                                              target_ages = 14:100)

#' Calculate preliminary marital status populations
#'
#' @description
#' Applies marital status proportions to total population.
#'
#' @param total_pop Total population by age, sex
#' @param marital_pcts Marital status proportions
#'
#' @return data.table with preliminary marital status populations
#'
#' @export
calculate_preliminary_marital_pop <- function(total_pop, marital_pcts)

#' Balance married populations
#'
#' @description
#' Adjusts married populations so total married males ≈ total married females,
#' while maintaining age-sex totals.
#'
#' @param marital_pop Preliminary marital status populations
#' @param marriage_grid Age-of-husband × age-of-wife marriage grid
#'
#' @return data.table with balanced marital status populations
#'
#' @export
balance_married_populations <- function(marital_pop, marriage_grid)

#' Incorporate same-sex marriage populations
#'
#' @description
#' Splits population into heterosexual and gay/lesbian segments
#' and applies separate marriage models (post-2013).
#'
#' @param marital_pop Balanced marital status populations
#' @param gay_pct Percentage male population that is gay (default: 0.025)
#' @param lesbian_pct Percentage female population that is lesbian (default: 0.045)
#' @param same_sex_grids Same-sex marriage age grids
#'
#' @return data.table including same-sex married populations
#'
#' @export
incorporate_same_sex_marriage <- function(marital_pop,
                                           gay_pct = 0.025,
                                           lesbian_pct = 0.045,
                                           same_sex_grids)

#' Calculate historical population by marital status (main entry)
#'
#' @description
#' Main function implementing Equation 1.4.2.
#'
#' @param total_pop P_x_s from historical_population.R
#' @param acs_marital ACS marital status data
#' @param ipums_marital Historical IPUMS marital data
#' @param marriage_grids Marriage prevalence grids
#'
#' @return data.table P_x_s_m: columns year, age, sex, marital_status, population
#'
#' @export
calculate_historical_population_marital <- function(total_pop,
                                                     acs_marital,
                                                     ipums_marital,
                                                     marriage_grids)
```

File: `R/demography/historical_temp_unlawful.R`

```r
#' Calculate temporary/unlawfully present residuals
#'
#' @description
#' Backs out net residual (other in minus other out) from population
#' accounting identity.
#'
#' @param begin_pop Beginning of year population
#' @param end_pop End of year population
#' @param births Births during year
#' @param deaths Deaths during year
#' @param lpr_immigration LPR immigration during year
#' @param emigration Legal emigration during year
#' @param aos Adjustments of status during year
#'
#' @return data.table with net residuals by age, sex
#'
#' @export
calculate_other_residuals <- function(begin_pop,
                                       end_pop,
                                       births,
                                       deaths,
                                       lpr_immigration,
                                       emigration,
                                       aos)

#' Build up temporary/unlawfully present stock
#'
#' @description
#' Builds stock estimates from flows using modified residuals
#' and mortality rates.
#'
#' @param residuals Net residuals by year, age, sex
#' @param mortality_qx Death probabilities
#'
#' @return data.table with O population stocks
#'
#' @export
build_temp_unlawful_stock <- function(residuals, mortality_qx)

#' Adjust O population to DHS estimates
#'
#' @description
#' Forces 2005-2023 totals to match DHS unauthorized estimates.
#' Linearly interpolates 2001-2004.
#'
#' @param o_stock Built-up O population stock
#' @param dhs_estimates DHS unauthorized immigrant estimates
#'
#' @return data.table with adjusted O population
#'
#' @export
adjust_o_to_dhs <- function(o_stock, dhs_estimates)

#' Calculate temporary/unlawfully present population (main entry)
#'
#' @description
#' Main function implementing Equation 1.4.3.
#'
#' @param ... All required inputs
#'
#' @return data.table O_x_s: columns year, age, sex, population
#'
#' @export
calculate_historical_temp_unlawful <- function(...)
```

File: `R/demography/historical_civilian_noninst.R`

```r
#' Calculate civilian noninstitutionalized population
#'
#' @description
#' Main function implementing Equation 1.4.4.
#' Available for 2010 onwards.
#'
#' @param census_civ_noninst Census civilian noninstitutionalized totals
#' @param acs_marital ACS marital status for civilian noninstitutionalized
#'
#' @return data.table C_x_s_m: columns year, age, sex, marital_status, population
#'
#' @export
calculate_civilian_noninst_population <- function(census_civ_noninst,
                                                   acs_marital)
```

### 5.3 Helper Functions

File: `R/demography/historical_population_helpers.R`

```r
#' Get list of tab years
#' @export
get_tab_years <- function() {
  c(1940, 1950, 1956, 1960, 1969:2009, 2022)
}

#' H.S. Beers interpolation (2D version for marriage grids)
#'
#' @param grouped_data Data by age groups
#' @param age_groups Age group definitions
#'
#' @return Single-year interpolated values
#'
#' @export
beers_interpolate_2d <- function(grouped_data, age_groups)

#' Convert reference dates
#'
#' @description
#' Converts population from one reference date to another
#' using linear interpolation.
#'
#' @param pop Population data
#' @param from_date Source reference date
#' @param to_date Target reference date
#'
#' @return Population at target reference date
#'
#' @export
convert_reference_date <- function(pop, from_date, to_date)

#' Apply census undercount adjustment
#'
#' @param census_pop Census population
#' @param undercount_factors Undercount factors by age, sex
#'
#' @return Adjusted population
#'
#' @export
apply_undercount_adjustment <- function(census_pop, undercount_factors)

#' Calculate closure ratios
#'
#' @description
#' Calculates ratios to eliminate closure error between
#' component-based estimates and tab year populations.
#'
#' @param estimated_pop Estimated population from components
#' @param tab_year_pop Known tab year population
#'
#' @return Closure ratios by age, sex
#'
#' @export
calculate_closure_ratios <- function(estimated_pop, tab_year_pop)
```

---

## 6. Configuration

### 6.1 Historical Population Configuration (add to tr2025.yaml)

```yaml
historical_population:
  # Time period
  start_year: 1940
  end_year: 2022  # Updated each year
  reference_date: "dec31"  # December 31

  # Tab years (for precise estimation)
  tab_years:
    - 1940
    - 1950
    - 1956
    - 1960
    - years_1969_2009: true  # All years 1969-2009
    - latest: 2022  # Updated each TR

  # Population concepts
  concepts:
    - resident
    - resident_usaf
    - civilian
    - civilian_noninstitutional

  # Age ranges
  ages:
    standard: [0, 84]
    extended: [0, 100]
    oldest: [0, 119]

  # Marital status categories
  marital_statuses:
    total_population:
      - single
      - married
      - widowed
      - divorced
    civilian_noninst:
      - single
      - married_spouse_present
      - separated
      - widowed
      - divorced

  # Same-sex marriage parameters (post-2013)
  same_sex_marriage:
    start_year: 2013
    gay_percent: 0.025  # 2.5% of male population
    lesbian_percent: 0.045  # 4.5% of female population

  # Territory codes
  territories:
    - code: PR
      name: Puerto Rico
      added_year: 1951
    - code: VI
      name: Virgin Islands
      added_year: 1951
    - code: GU
      name: Guam
      added_year: 1951
    - code: MP
      name: Northern Mariana Islands
      added_year: 1978
    - code: AS
      name: American Samoa
      added_year: 1961

  # Data source reference years
  data_sources:
    acs_start_year: 2006
    civilian_noninst_start_year: 2010
    dhs_unauthorized_start_year: 2005
    same_sex_marriage_data_years: [2012, 2013, 2014]

  # O population (temporary/unlawful) adjustments
  o_population:
    dhs_adjustment_start_year: 2005
    interpolation_years: [2001, 2002, 2003, 2004]
    reference_jan2000: true
    reference_jan2005: true
```

---

## 7. Validation Framework

### 7.1 Validation Data Available

From `data/raw/SSA_TR2025/`:
- `SSPopJan_Alt2_TR2025.csv` - January 1 populations
- `SSPopJul_Alt2_TR2025.csv` - July 1 populations
- `SSPopDec_Alt2_TR2025.csv` - December 31 populations

### 7.2 Validation Points

| Output | Metric | Tolerance | Source |
|--------|--------|-----------|--------|
| P_x_s | Total population by year | 0.1% | SSPopDec |
| P_x_s | Population by age group | 1% | SSPopDec |
| P_x_s | Sex ratio by age | 2% | SSPopDec |
| P_x_s_m | Married population total | 2% | External Census tables |
| O_x_s | Total temp/unlawful | 5% | DHS estimates |
| C_x_s_m | Civilian noninst total | 0.5% | Census estimates |

### 7.3 Validation Functions

File: `R/validation/validate_historical_population.R`

```r
#' Validate historical population against TR2025
#'
#' @param calculated_pop Calculated historical population
#' @param tr2025_pop Official TR2025 population
#' @param tolerance Relative tolerance
#'
#' @return Validation report
#'
#' @export
validate_historical_population <- function(calculated_pop,
                                            tr2025_pop,
                                            tolerance = 0.01)

#' Load TR2025 population files
#' @export
load_tr2025_population <- function(reference_date = "dec",
                                    alternative = "Alt2")

#' Validate marital status consistency
#'
#' @description
#' Checks that marital status populations sum to total
#' and married males ≈ married females.
#'
#' @export
validate_marital_consistency <- function(marital_pop, total_pop)

#' Validate O population against DHS
#' @export
validate_o_against_dhs <- function(o_pop, dhs_estimates)
```

---

## 8. Implementation Sequence

### Phase 4A: Core Census Data Acquisition - COMPLETE

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 4A.1 | Implement Census population API fetcher | None | census_historical_population.R |
| [x] | 4A.2 | Fetch January 1 populations (1981-2023) | 4A.1 | Cached data |
| [x] | 4A.3 | Fetch July 1 populations (1980-2023) | 4A.1 | Cached data |
| [x] | 4A.4 | Fetch civilian populations (2010-2023) | 4A.1 | Cached data |
| [x] | 4A.5 | Fetch civilian noninst populations (2010-2023) | 4A.1 | Cached data |
| [x] | 4A.6 | Implement territory population fetcher | None | Census IDB data |
| [x] | 4A.7 | Fetch territory populations (1951-2023) | 4A.6 | Cached data (hardcoded fallback)

### Phase 4B: ACS and IPUMS Data Acquisition

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 4B.1 | Implement ACS PUMS marital status fetcher | None | acs_marital_status.R |
| [ ] | 4B.2 | Fetch ACS total population by marital status | 4B.1 | ACS data 2006-2023 |
| [ ] | 4B.3 | Fetch ACS civ noninst by marital status | 4B.1 | ACS data 2006-2023 |
| [ ] | 4B.4 | Implement ACS marriage grid fetcher | 4B.1 | Marriage grids |
| [ ] | 4B.5 | Fetch ACS marriage grids (2006-2023) | 4B.4 | Cached grids |
| [ ] | 4B.6 | Implement IPUMS historical data fetcher | None | ipums_historical.R |
| [ ] | 4B.7 | Fetch IPUMS marital status (1940-2000) | 4B.6 | Historical marital data |
| [ ] | 4B.8 | Fetch IPUMS marriage grids (1940-2000) | 4B.6 | Historical grids |

### Phase 4C: Other Data Sources

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 4C.1 | Implement OPM federal employees fetcher | None | OPM data |
| [ ] | 4C.2 | Implement SSA beneficiaries abroad fetcher | None | SSA Supplement data |
| [ ] | 4C.3 | Implement DHS unauthorized estimates fetcher | None | DHS data |
| [ ] | 4C.4 | Compile undercount factors | Research | Undercount data |
| [ ] | 4C.5 | Compile historical pre-1980 data | Archives | Static files |

### Phase 4D: Core Population Calculations

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 4D.1 | Implement tab year population calculation | 4A, 4C | historical_population.R |
| [ ] | 4D.2 | Implement 85+ build-up | 4D.1, Mortality | Ages 85+ populations |
| [ ] | 4D.3 | Implement inter-tab interpolation | 4D.1, 4D.2 | Full time series |
| [ ] | 4D.4 | Calculate P_x_s (Eq 1.4.1) | 4D.1-4D.3 | Total population |
| [ ] | 4D.5 | Validate P_x_s against TR2025 | 4D.4, TR2025 | Validation report |

### Phase 4E: Marital Status Calculations

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 4E.1 | Implement Beers interpolation | None | beers_interpolate_marital_status() |
| [ ] | 4E.2 | Implement marriage grid interpolation | 4E.1 | beers_interpolate_2d() |
| [ ] | 4E.3 | Implement marital status allocation | 4D.4, 4B | Preliminary P_x_s_m |
| [ ] | 4E.4 | Implement marriage balancing | 4E.3 | Balanced populations |
| [ ] | 4E.5 | Implement same-sex marriage model | 4E.4 | Post-2013 populations |
| [ ] | 4E.6 | Calculate P_x_s_m (Eq 1.4.2) | 4E.1-4E.5 | Marital populations |
| [ ] | 4E.7 | Validate marital status consistency | 4E.6 | Validation report |

### Phase 4F: Temporary/Unlawful Population

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 4F.1 | Implement residual calculation | 4D.4, LPR Immig | Residuals |
| [ ] | 4F.2 | Implement O stock build-up | 4F.1, Mortality | O stock estimates |
| [ ] | 4F.3 | Implement DHS adjustment | 4F.2, 4C.3 | Adjusted O |
| [ ] | 4F.4 | Calculate O_x_s (Eq 1.4.3) | 4F.1-4F.3 | O population |
| [ ] | 4F.5 | Validate O against DHS | 4F.4, DHS | Validation report |

### Phase 4G: Civilian Noninstitutionalized Population

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 4G.1 | Implement C_x_s_m calculation | 4A.5, 4B.3 | historical_civilian_noninst.R |
| [ ] | 4G.2 | Calculate C_x_s_m (Eq 1.4.4) | 4G.1 | Civ noninst population |
| [ ] | 4G.3 | Validate C against Census | 4G.2 | Validation report |

### Phase 4H: Targets Integration

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 4H.1 | Add historical population targets to _targets.R | All | Pipeline targets |
| [ ] | 4H.2 | Test full pipeline execution | 4H.1 | Working pipeline |
| [ ] | 4H.3 | Documentation and cleanup | All | Documented code |

---

## 9. Technical Specifications

### 9.1 Data Structures

**Historical Population by Age and Sex (P_x_s):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year (1940-2022)
age             integer   Single year of age (0-119)
sex             character "male" or "female"
population      numeric   December 31 population
source          character "tab_year", "interpolated"
```

**Historical Population by Marital Status (P_x_s_m):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age (14-119)
sex             character "male" or "female"
marital_status  character "single", "married", "widowed", "divorced"
orientation     character "heterosexual", "gay", "lesbian" (2013+)
population      numeric   December 31 population
```

**Temporary/Unlawful Population (O_x_s):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age (0-99)
sex             character "male" or "female"
population      numeric   January 1 population
source          character "residual", "dhs_adjusted"
```

**Civilian Noninstitutionalized (C_x_s_m):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year (2010+)
age             integer   Single year of age (0-119)
sex             character "male" or "female"
marital_status  character 5 categories (includes separated)
population      numeric   January 1 population
```

### 9.2 Key Methodological Notes

**Tab Year Approach:**
- Precise estimation only at tab years
- Components of change used for interpolation
- Closure ratios eliminate accumulated error

**85+ Population Challenge:**
- Census 85+ totals less reliable
- Build-up approach uses mortality and migration
- Ratio adjustment to Census constrains total

**Marital Status Consistency:**
- Two-sex constraint for marriages
- Marriage grids provide joint distribution
- Beers interpolation for single years

**Same-Sex Marriage Modeling:**
- Federal recognition from 2013
- Separate marriage grids
- Based on ACS, NSFG, state data

### 9.3 Potential Simplifications for Initial Implementation

1. **Start with modern period (1980-2022):**
   - Census API data readily available
   - Skip complex pre-1970 data compilation

2. **Defer same-sex marriage modeling:**
   - Start with simple marital status
   - Add same-sex marriage as enhancement

3. **Use published DHS estimates directly:**
   - Skip residual calculation for O population
   - Match to DHS published numbers

4. **Simplify marital status initially:**
   - Use ACS proportions directly
   - Skip complex marriage grid balancing

### 9.4 Dependencies on Previous Subprocesses

| Subprocess | Data Needed | Used In |
|------------|-------------|---------|
| FERTILITY | Birth rates by age | Births calculation |
| MORTALITY | Death probabilities | Deaths calculation, 85+ build-up |
| LPR IMMIGRATION | Immigration by age, sex | Population components |
| LPR IMMIGRATION | Emigration by age, sex | Population components |
| LPR IMMIGRATION | Adjustments of status | O population residual |

---

## Appendix A: Census API Endpoints

### A.1 Population Estimates Program (PEP)

**2020-2023 (post-2020 Census):**
```
https://api.census.gov/data/{year}/pep/natage
Variables: NAME, AGE, SEX, POP
```

**2010-2019 (intercensal):**
```
https://api.census.gov/data/{year}/pep/charagegroups
```

**2000-2009 (intercensal):**
```
https://api.census.gov/data/{year}/pep/int_population
```

### A.2 American Community Survey

```
https://api.census.gov/data/{year}/acs/acs1/pums
Variables: PWGTP, AGEP, SEX, MAR
```

### A.3 Census International Database

```
https://api.census.gov/data/timeseries/idb/5year
```

---

## Appendix B: H.S. Beers Interpolation

The Beers method is used to convert 5-year age group data to single years of age. The standard coefficients are:

**For opening ages (0-4 from groups 0-4, 5-9):**
```
b[1] =  0.3333  *g[1] + 0.2761*g[2] - 0.0800*g[3] + 0.0317*g[4] - 0.0144*g[5]
...
```

**For interior ages:**
Standard 5-term oscillatory formula

**For closing ages:**
Reflection of opening formula

For 2-dimensional grids (husband-age × wife-age), apply Beers iteratively to rows then columns.

---

## Appendix C: Comparison with Previous Subprocesses

| Aspect | Fertility | Mortality | LPR Immigration | Historical Population |
|--------|-----------|-----------|-----------------|----------------------|
| Primary output | Birth rates | Death probabilities | Immigration flows | Population stocks |
| Time coverage | 1941-2023 | 1941-2099 | 1941-2099 | 1940-2022 |
| Age coverage | 14-49 | 0-119 | 0-99 | 0-119 |
| Additional dimensions | None | Cause | NEW/AOS | Marital status |
| Data sources | 2 | 4+ | 3 | 44 |
| Complexity | Medium | High | Medium | Very High |
| External validation | TR TFR | TR qx, ex | TR totals | TR populations |

---

## Appendix D: Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Census API rate limits | Medium | Medium | Implement caching, batch requests |
| IPUMS registration required | High | Low | Create account, plan for delays |
| Undercount factors not published | Medium | Medium | Use research estimates, document assumptions |
| Pre-1980 data hard to obtain | High | Medium | Start with 1980+, expand later |
| Marriage grid complexity | Medium | High | Implement in phases, validate incrementally |
| Same-sex marriage data gaps | Medium | Low | Use published ACS proportions |
| DHS estimates methodology changes | Low | Medium | Document vintage, handle breaks |

---

*End of Implementation Plan*

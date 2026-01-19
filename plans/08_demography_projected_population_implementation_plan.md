# ARTEMIS: OASDI Projection Model Implementation Plan
## Phase 8: Demography Process - Projected Population Subprocess (1.8)

**Document Version:** 1.0
**Date:** January 2026
**Scope:** Projected Population subprocess implementation following Divorce subprocess completion
**Source:** SSA 2025 Long-Range Model Documentation, Section 1.8 Projected Population

---

## Progress Tracking

> **IMPORTANT**: This plan document should be updated as implementation progresses. Each task should be marked with its current status.

### Status Legend
- [ ] Not started
- [~] In progress
- [x] Completed

### Current Status
**Last Updated:** January 18, 2026
**Current Phase:** Phase 8A - Complete
**Subprocess Status:** Phase 8A Data Assembly Complete

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

The PROJECTED POPULATION subprocess is the culmination of the demography process. It projects the Social Security area population from the starting year (December 31, 2022) through the 75-year projection period (2023-2099). This subprocess is critical because it:

- Integrates all previous demography subprocess outputs (fertility, mortality, immigration, marriage, divorce)
- Provides the population base for all economic and benefit calculations
- Projects population by age, sex, marital status, and population status (heterosexual/gay/lesbian)
- Tracks children by parent survival status for survivor benefit calculations
- Projects the civilian noninstitutionalized (CNI) population for employment calculations

### 1.2 Key Terminology

| Term | Definition |
|------|------------|
| Component Method | Population projection using births, deaths, and net migration |
| Population Status | Heterosexual, gay, or lesbian (for marriage modeling) |
| Marital Status | Single, married, widowed, divorced |
| Parent Fate | Both parents alive, only father alive, only mother alive, both deceased |
| CNI | Civilian Noninstitutionalized population |
| USAF | U.S. resident + Armed Forces overseas |
| Starting Year | 2022 (December 31 basis) for TR2025 |

### 1.3 Primary Outputs

| Output | Symbol | Equation | Description |
|--------|--------|----------|-------------|
| Births | B^z_{s,p} | 1.8.1 | Births by sex and population status |
| Deaths | D^z_{x,s,p} | 1.8.2 | Deaths by age, sex, and population status |
| Net Immigration | NI^z_{x,s} | 1.8.3 | Total net immigration (LPR + O) |
| Population by Age/Sex/Status | P^z_{x,s,p} | 1.8.4 | SS area population |
| Population by Marital Status | P^z_{x,s,p,m} | 1.8.5 | Population with marital status |
| Children by Parent Survival | C^z_{x,s,g,f} | 1.8.6 | Children ages 0-18 by parent fate |
| CNI Population | N^z_{x,s,m} | 1.8.7 | Civilian noninstitutionalized |

### 1.4 TR2025 Assumptions

| Parameter | Value | Notes |
|-----------|-------|-------|
| Starting Year | 2022 | December 31 basis |
| Projection Period | 2023-2099 | 77 years |
| Extended Period | 2023-2105 | For some outputs |
| Sex Ratio at Birth | 1,048 males per 1,000 females | Standard assumption |
| Gay Percentage | 2.5% of males | For same-sex marriage modeling |
| Lesbian Percentage | 4.5% of females | For same-sex marriage modeling |

### 1.5 Population Statuses

Beginning December 31, 2013, populations are modeled with three statuses:
- **Heterosexual**: 97.5% of males, 95.5% of females
- **Gay**: 2.5% of males (eligible for same-sex male marriage)
- **Lesbian**: 4.5% of females (eligible for same-sex female marriage)

These percentages are assumed constant for all newborn cohorts and existing population.

### 1.6 Dependencies on Previous Subprocesses

| Subprocess | Phase | Key Outputs Used |
|------------|-------|------------------|
| FERTILITY | 1 | Birth rates by age of mother (14-49) |
| MORTALITY | 2 | Death probabilities by age, sex; marital status differentials |
| LPR IMMIGRATION | 3 | Net LPR immigration (NEW arrivals - legal emigrants + AOS) |
| HISTORICAL POPULATION | 4 | Starting population (Dec 31, 2022) |
| TEMP/UNLAWFUL IMMIGRATION | 5 | Net O immigration and O population stock |
| MARRIAGE | 6 | Marriage rates by husband-age × wife-age, prior marital status |
| DIVORCE | 7 | Divorce rates by husband-age × wife-age |

---

## 2. Mathematical Framework

### 2.1 Equation 1.8.1 – Births

The number of births in year z by sex (s) and population status (p):

$$B^z_{s,p} = B^z_{s,p}(\cdot)$$

**Calculation:**

$$B^z_x = b^z_x \times \frac{FP^z_x + FP^{z+1}_x}{2}$$

Where:
- B^z_x = Births to mothers age x in year z
- b^z_x = Birth rate for mothers age x in year z (from FERTILITY subprocess)
- FP^z_x = Female population age x at beginning of year z

**Sex Disaggregation:**
- Total births × 1,048/(1,048+1,000) = male births
- Total births × 1,000/(1,048+1,000) = female births

**Population Status Disaggregation:**
- Male births: 2.5% gay, 97.5% heterosexual
- Female births: 4.5% lesbian, 95.5% heterosexual

### 2.2 Equation 1.8.2 – Deaths

Deaths by age (x), sex (s), and population status (p):

$$D^z_{x,s,p} = q^z_{x,s} \times P^z_{x,s,p}$$

Where:
- q^z_{x,s} = Probability of death at age x, sex s, year z (from MORTALITY subprocess)
- P^z_{x,s,p} = Population at age x, sex s, status p at beginning of year z

**Marital Status Distribution:**
Deaths are distributed by marital status using mortality differentials observed during 2015-2019 (from MORTALITY subprocess).

### 2.3 Equation 1.8.3 – Net Immigration

Total net immigration by age (x) and sex (s):

$$NI^z_{x,s} = NL^z_{x,s} + NO^z_{x,s}$$

Where:
- NL^z_{x,s} = Net LPR immigration (from LPR IMMIGRATION subprocess)
- NO^z_{x,s} = Net temporary/unlawfully present immigration (from TEMP/UNLAWFUL IMMIGRATION subprocess)

Net immigration is further disaggregated by population status (p) into NI^z_{x,s,p}.

**Note:** Age -1 represents births occurring during the year to immigrants.

### 2.4 Equation 1.8.4 – Population by Age, Sex, and Population Status

For age 0:
$$P^z_{0,s,p} = B^z_{s,p} - D^z_{0,s,p} + NI^z_{0,s,p}$$

For ages > 0:
$$P^z_{x,s,p} = P^{z-1}_{x-1,s,p} - D^z_{x,s,p} + NI^z_{x,s,p}$$

Where P^z_{x,s,p} is the population as of December 31 of year z.

### 2.5 Equation 1.8.5 – Population by Marital Status

Population disaggregated into four marital statuses (single, married, widowed, divorced):

$$P^z_{x,s,p,m} = P^z_{x,s,p,m}(\cdot)$$

**Marital Status Transitions:**

1. **Births**: Assigned to single marital status
2. **Deaths**: Distributed by marital status using mortality differentials
3. **Immigration**: Distributed by marital status using beginning-of-year distribution
4. **Marriages**: Calculated from marriage rates × geometric mean of unmarried populations
5. **Divorces**: Calculated from divorce rates × midyear married couples
6. **Widowings**: Calculated from death rates applied to married population

**Marriage Calculation:**
For opposite-sex marriages at husband age x, wife age y:
$$M^z_{x,y} = m^z_{x,y} \times \sqrt{UP^z_{x,male} \times UP^z_{y,female}}$$

Where:
- m^z_{x,y} = Marriage rate from MARRIAGE subprocess
- UP^z = Unmarried population (single + divorced + widowed)

**Divorce Calculation:**
$$D^z_{x,y} = d^z_{x,y} \times MC^z_{x,y}$$

Where:
- d^z_{x,y} = Divorce rate from DIVORCE subprocess
- MC^z_{x,y} = Midyear married couples at husband age x, wife age y

### 2.6 Equation 1.8.6 – Children by Parent Survival Status

Children (ages 0-18) by age (x), parent sex (s), parent age group (g), and parent fate (f):

$$C^z_{x,s,g,f} = C^z_{x,s,g,f}(\cdot)$$

**Parent Fates (f):**
1. Both parents alive
2. Only father alive
3. Only mother alive
4. Both parents deceased

**Calculation Method:**
1. Distribute births to age of husband using marriage grid proportions
2. Roll forward children each year to next age of husband, wife, and child
3. Calculate parent survival using mortality rates
4. Adjust to match total children in population

### 2.7 Equation 1.8.7 – Civilian Noninstitutionalized Population

CNI population by age (x), sex (s), and marital status (m):

$$N^z_{x,s,m} = N^z_{x,s,m}(\cdot)$$

**Marital Status Categories (5):**
- Single (never married)
- Married (spouse present)
- Separated
- Widowed
- Divorced

**Calculation Method:**
1. Project USAF population forward using births, deaths, net immigration
2. Calculate residential population by subtracting armed forces overseas
3. Calculate civilian population by subtracting total armed forces
4. Apply CNI/civilian ratios from starting year
5. Disaggregate by marital status using historical adjustments

---

## 3. Input Data Requirements

### 3.1 Data Sources Summary

The Projected Population subprocess requires **33 distinct data inputs** from multiple sources:

| Category | Items | Priority |
|----------|-------|----------|
| Previous Demography Subprocesses | 28 | High |
| U.S. Census Bureau Data | 4 | Medium |
| Other Input Data | 1 | Medium |

### 3.2 Long-Range OASDI Projection Data (From Previous Subprocesses)

#### FERTILITY (Phase 1)

| # | Data | Years | Status |
|---|------|-------|--------|
| 1 | Historical birth rates by age of mother (14-49) | 1941-2022 | ✓ Complete |
| 2 | Projected birth rates by age of mother (14-49) | 2023-2105 | ✓ Complete |

#### MORTALITY (Phase 2)

| # | Data | Years | Status |
|---|------|-------|--------|
| 3 | Historical death probabilities qx by age (0-99, 100+) and sex | 1941-2022 | ✓ Complete |
| 4 | Projected death probabilities qx by age (0-99, 100+) and sex | 2023-2105 | ✓ Complete |
| 5 | Mortality differentials by marital status (ages 14-100+) | 2015-2019 | ✓ Complete |

#### LPR IMMIGRATION (Phase 3)

| # | Data | Years | Status |
|---|------|-------|--------|
| 6 | Projected LPR immigrants (NEW) by age (-1 to 100) and sex | 2023-2105 | ✓ Complete |
| 7 | Projected legal emigrants by age (-1 to 100) and sex | 2023-2105 | ✓ Complete |
| 8 | Projected LPR immigrants (AOS) by age (-1 to 100) and sex | 2023-2105 | ✓ Complete |

**Note:** Age -1 represents births during the year.

#### HISTORICAL POPULATION (Phase 4)

| # | Data | Years | Status |
|---|------|-------|--------|
| 9 | SS area population by age (0-99, 100+), sex, marital status, population status | 1940-2022 | ✓ Complete |
| 10 | Married couples by age of spouse 1 × age of spouse 2, marriage type | 1940-2022 | ✓ Complete |
| 11 | Temp/unlawfully present population by age and sex | 1963-2022 | ✓ Complete |
| 12 | Final historical year (December 31 basis) | 2022 | ✓ Complete |
| 13 | December CNI population by sex, age, marital status | 2010-2022 | ✓ Complete |
| 14 | December overseas armed forces by sex and age | 2010-2022 | ✓ Complete |
| 15 | December armed forces by sex and age | 2010-2022 | ✓ Complete |
| 16 | January U.S. resident pop (SS area adjusted) by sex and age | 2011-2023 | ✓ Complete |
| 17 | January U.S. resident + armed forces overseas by sex and age | 2011-2023 | ✓ Complete |
| 18 | CNI/total population ratios by sex, age, marital status | Assumed | ✓ Complete |
| 19 | Marital status adjustments by sex and age | Averaged | ✓ Complete |
| 20 | Separated/total married ratios by sex and age | Assumed | ✓ Complete |

#### TEMPORARY OR UNLAWFULLY PRESENT IMMIGRATION (Phase 5)

| # | Data | Years | Status |
|---|------|-------|--------|
| 21 | Projected O immigrants entering by age (-1 to 100) and sex | 2023-2105 | ✓ Complete |
| 22 | Projected O emigrants leaving by age (-1 to 100) and sex | 2023-2105 | ✓ Complete |
| 23 | Projected O population by age and sex | 2023-2105 | ✓ Complete |

#### MARRIAGE (Phase 6)

| # | Data | Years | Status |
|---|------|-------|--------|
| 24 | Projected opposite-sex marriage rates (husband age × wife age) | 2023-2099 | ✓ Complete |
| 25 | Projected same-sex marriage rates (spouse 1 age × spouse 2 age) | 2023-2099 | ✓ Complete |
| 26 | Marriage rates by age, sex, prior marital status | 1979-88 avg | ✓ Complete |
| 27 | Total marriages | 1989-2022 | ✓ Complete |

#### DIVORCE (Phase 7)

| # | Data | Years | Status |
|---|------|-------|--------|
| 28 | Projected divorce rates (husband age × wife age) | 2023-2099 | ✓ Complete |

### 3.3 U.S. Census Bureau Data

| # | Data | Source | Years | Status |
|---|------|--------|-------|--------|
| 29 | CPS children per married couple by age of householder | CPS | 1960-2022 | To implement |
| 30 | January CNI population by sex and age | Census | 2011-2023 | ✓ Complete |
| 31 | January civilian population by sex and age | Census | 2011-2023 | ✓ Complete |
| 32 | July CNI population by sex and age | Census | 2010-2023 | ✓ Complete |

### 3.4 Other Input Data

| # | Data | Source | Years | Status |
|---|------|--------|-------|--------|
| 33 | Births by month and sex | NCHS | 1931-2023 | To verify/implement |

**Note on Item 33:** Phase 1 (Fertility) downloaded annual births by age of mother, but may not have downloaded monthly births by sex. A `fetch_nchs_births_by_month_sex()` function exists in `R/data_acquisition/nchs_births.R` for years 1968-2024, but the data may not be cached yet. The 1931-1967 period would require historical vital statistics reports. Need to verify what data is currently available and implement fetching if needed.

### 3.5 Data Availability Assessment

| Data Category | Availability | Notes |
|---------------|--------------|-------|
| Previous subprocess outputs | High | All complete from Phases 1-7 |
| Census population estimates | High | Already implemented in Phase 4 |
| CPS children per couple | Medium | Available from IPUMS CPS |
| NCHS monthly births | High | Already have annual; monthly detail available |

### 3.6 Data Acquisition Priority

**Phase 8A - Data Assembly (High Priority):**
- Verify all previous subprocess outputs available in correct format
- Items 1-28 from previous phases
- Implement CPS children per couple (Item 29)

**Phase 8B - Starting Population (High Priority):**
- Extract December 31, 2022 population from HISTORICAL POPULATION
- Verify marital status and population status disaggregation
- Items 9-20 from Phase 4

**Phase 8C - Component Flows (High Priority):**
- Assemble birth rates, death rates, net immigration
- Items 1-8, 21-28 from previous phases

---

## 4. Output Development Methodology

### 4.1 Step-by-Step Process for Equation 1.8.4 (Core Population)

**Step 1: Initialize Starting Population**

```
For December 31, 2022:
  Load P^{2022}_{x,s,p} from HISTORICAL POPULATION
  Verify totals match TR2025 starting population
```

**Step 2: Calculate Births (Equation 1.8.1)**

```
For each projection year z (2023-2099):
  For each mother age x (14-49):
    1. Get birth rate b^z_x from FERTILITY subprocess
    2. Get midyear female population: (FP^z_x + FP^{z+1}_x) / 2
    3. Calculate births: B^z_x = b^z_x × midyear_pop

  Total births = sum(B^z_x for x in 14:49)
  Male births = total × 1048/2048
  Female births = total × 1000/2048

  Disaggregate by population status:
    Male gay births = male × 0.025
    Male hetero births = male × 0.975
    Female lesbian births = female × 0.045
    Female hetero births = female × 0.955
```

**Step 3: Calculate Deaths (Equation 1.8.2)**

```
For each projection year z:
  For each age x, sex s, status p:
    1. Get death probability q^z_{x,s} from MORTALITY subprocess
    2. Apply to beginning-of-year population
    3. D^z_{x,s,p} = q^z_{x,s} × P^z_{x,s,p}
```

**Step 4: Calculate Net Immigration (Equation 1.8.3)**

```
For each projection year z:
  For each age x, sex s:
    NL = LPR_NEW - Legal_Emigration + AOS  (from Phase 3)
    NO = O_Immigration - O_Emigration       (from Phase 5)
    NI^z_{x,s} = NL + NO

  Disaggregate by population status using BOY population proportions
```

**Step 5: Project Population Forward (Equation 1.8.4)**

```
For each projection year z (2023-2099):
  For age 0:
    P^z_{0,s,p} = B^z_{s,p} - D^z_{0,s,p} + NI^z_{0,s,p}

  For ages 1-100+:
    P^z_{x,s,p} = P^{z-1}_{x-1,s,p} - D^z_{x,s,p} + NI^z_{x,s,p}

  Constrain 100+ to accumulate properly
```

### 4.2 Step-by-Step Process for Equation 1.8.5 (Marital Status)

**Step 1: Initialize Marital Status from Starting Year**

```
For December 31, 2022:
  Load P^{2022}_{x,s,p,m} from HISTORICAL POPULATION
  Load married couples grid by age of husband × age of wife
```

**Step 2: Assign Marital Status to Births**

```
All births assigned to "single" marital status
```

**Step 3: Distribute Deaths by Marital Status**

```
For each age x, sex s, status p:
  Apply mortality differentials from MORTALITY subprocess
  Deaths_m = Total_Deaths × mortality_factor_m × marital_proportion_m / sum(...)
```

**Step 4: Assign Marital Status to Immigrants**

```
For each age x, sex s, status p:
  Distribute using BOY marital distribution for that age/sex
  Imm_m = Total_Imm × BOY_marital_proportion_m
```

**Step 5: Calculate New Marriages**

```
For each year z:
  For opposite-sex marriages at husband age x, wife age y:
    1. Get marriage rate m^z_{x,y} from MARRIAGE subprocess
    2. Calculate midyear unmarried male pop at age x
    3. Calculate midyear unmarried female pop at age y
    4. Marriages = m^z_{x,y} × sqrt(UP_male_x × UP_female_y)

  For same-sex marriages:
    Similar calculation using same-sex rates and gay/lesbian populations

  Distribute marriages by prior marital status (single, divorced, widowed)
    using rates from MARRIAGE subprocess
```

**Step 6: Calculate Divorces**

```
For each year z:
  For husband age x, wife age y:
    1. Get divorce rate d^z_{x,y} from DIVORCE subprocess
    2. Calculate midyear married couples at (x, y)
    3. Divorces_{x,y} = d^z_{x,y} × MC^z_{x,y}

  Same-sex divorces calculated similarly
```

**Step 7: Calculate Widowings**

```
For each year z:
  Apply death rates to married population
  Widowings reconciled with deaths by marital status for consistency
```

**Step 8: Update Marital Status Populations**

```
For each age x, sex s, status p, year z:
  Unmarried_EOY = Unmarried_BOY
                  - Deaths_unmarried
                  - Marriages
                  + New_unmarried_immigrants
                  + Widowings
                  + Divorces

  Married_EOY = Married_BOY
                - Divorces
                - Widowings
                - Deaths_married
                - Both_spouse_deaths
                + New_married_immigrants
                + Marriages
```

### 4.3 Step-by-Step Process for Equation 1.8.6 (Children by Parent Fate)

**Step 1: Initialize from Historical Data**

```
Load from HISTORICAL POPULATION:
  - Number of children ages 0-18
  - Number of women ages 14-49
  - Married couples grid by husband age × wife age
```

**Step 2: Distribute Births to Parent Ages**

```
For each year z:
  For mother age y (14-49):
    births_y = birth_rate_y × female_pop_y

  Distribute to husband age using married couples grid proportions:
    births_{x,y} = births_y × MC_{x,y} / sum(MC_{*,y})
```

**Step 3: Roll Forward Each Year**

```
For each year z:
  Move children to next age of child (c+1)
  Move to next age of father (x+1)
  Move to next age of mother (y+1)

  Calculate parent survival:
    P(father alive) = 1 - q_{x,male}
    P(mother alive) = 1 - q_{y,female}

  Update fate categories:
    Both alive: prev_both × P(f) × P(m)
    Only father: prev_both × P(f) × (1-P(m)) + prev_only_f × P(f)
    Only mother: prev_both × (1-P(f)) × P(m) + prev_only_m × P(m)
    Both deceased: remainder
```

**Step 4: Adjust to Population Totals**

```
Scale calculated children to match total children in projected population
Apply ratio adjustment if discrepancy exists
```

**Step 5: Project Mean Children per Couple**

```
Use linear regression on CPS historical data
Predict mean children by age of householder for projection years
```

### 4.4 Step-by-Step Process for Equation 1.8.7 (CNI Population)

**Step 1: Project USAF Population**

```
For each year z:
  USAF^z_{x,s} = USAF^{z-1}_{x-1,s}
                 - Deaths^z_{x,s}
                 + Births^z_s (for age 0)
                 + NI^z_{x,s} × (USAF_BOY / SS_Area_BOY)
```

**Step 2: Calculate Residential Population**

```
Residential^z_{x,s} = USAF^z_{x,s} - Armed_Forces_Overseas_{x,s}
(Armed forces overseas assumed constant from starting year)
```

**Step 3: Calculate Civilian Population**

```
Civilian^z_{x,s} = USAF^z_{x,s} - Total_Armed_Forces_{x,s}
(Total armed forces assumed constant from starting year)
```

**Step 4: Calculate CNI Population**

```
CNI^z_{x,s} = Civilian^z_{x,s} × CNI_Civilian_Ratio^{2022}_{x,s}
(Ratio held constant from starting year)
```

**Step 5: Disaggregate by Marital Status**

```
Apply:
  1. Smoothed marital status adjustments by sex and age
  2. Separated/married ratios by sex and age
  3. CNI/total ratios by sex, age, marital status
```

---

## 5. Implementation Functions

### 5.1 Core Projection Functions

File: `R/demography/projected_population.R`

```r
#' Project births for a year (Equation 1.8.1)
#'
#' @description
#' Calculates total births by applying age-specific birth rates
#' to midyear female population, then disaggregates by sex and
#' population status.
#'
#' @param birth_rates Birth rates by age of mother (14-49)
#' @param female_pop_boy Female population at beginning of year
#' @param female_pop_eoy Female population at end of year
#' @param sex_ratio Male births per 1000 female births (default: 1048)
#' @param gay_pct Percentage males that are gay (default: 0.025)
#' @param lesbian_pct Percentage females that are lesbian (default: 0.045)
#'
#' @return data.table with columns: sex, pop_status, births
#'
#' @export
calculate_projected_births <- function(birth_rates,
                                        female_pop_boy,
                                        female_pop_eoy,
                                        sex_ratio = 1048,
                                        gay_pct = 0.025,
                                        lesbian_pct = 0.045)

#' Calculate deaths for a year (Equation 1.8.2)
#'
#' @description
#' Applies death probabilities to beginning-of-year population
#' by age, sex, and population status.
#'
#' @param death_probs Death probabilities qx by age and sex
#' @param population_boy Population at beginning of year
#'
#' @return data.table with columns: age, sex, pop_status, deaths
#'
#' @export
calculate_projected_deaths <- function(death_probs, population_boy)

#' Calculate net immigration for a year (Equation 1.8.3)
#'
#' @description
#' Combines net LPR immigration and net O immigration.
#'
#' @param net_lpr Net LPR immigration by age and sex
#' @param net_o Net O immigration by age and sex
#' @param pop_status_dist Population status distribution for disaggregation
#'
#' @return data.table with columns: age, sex, pop_status, net_immigration
#'
#' @export
calculate_net_immigration <- function(net_lpr, net_o, pop_status_dist)

#' Project population forward one year (Equation 1.8.4)
#'
#' @description
#' Applies the cohort component method to project population
#' from year z-1 to year z.
#'
#' @param population_prev Population at end of previous year
#' @param births Births during year
#' @param deaths Deaths during year
#' @param net_immigration Net immigration during year
#'
#' @return data.table with columns: age, sex, pop_status, population
#'
#' @export
project_population_year <- function(population_prev,
                                     births,
                                     deaths,
                                     net_immigration)

#' Run full population projection (main entry point for Eq 1.8.4)
#'
#' @description
#' Main function orchestrating the complete population projection
#' from starting year through 2099.
#'
#' @param starting_population Population at December 31, 2022
#' @param birth_rates Projected birth rates by year and age
#' @param death_probs Projected death probabilities by year, age, sex
#' @param net_lpr_immigration Net LPR immigration by year, age, sex
#' @param net_o_immigration Net O immigration by year, age, sex
#' @param config Configuration parameters
#' @param projection_years Years to project (default: 2023:2099)
#'
#' @return list with:
#'   - population: P^z_{x,s,p} by year, age, sex, population status
#'   - births: B^z_{s,p} by year, sex, population status
#'   - deaths: D^z_{x,s,p} by year, age, sex, population status
#'   - net_immigration: NI^z_{x,s,p} by year, age, sex, population status
#'
#' @export
run_population_projection <- function(starting_population,
                                       birth_rates,
                                       death_probs,
                                       net_lpr_immigration,
                                       net_o_immigration,
                                       config,
                                       projection_years = 2023:2099)
```

### 5.2 Marital Status Functions

File: `R/demography/projected_marital_status.R`

```r
#' Distribute deaths by marital status
#'
#' @description
#' Allocates total deaths by marital status using mortality
#' differentials from 2015-2019.
#'
#' @param deaths Total deaths by age and sex
#' @param marital_pop Population by age, sex, marital status
#' @param mortality_differentials Mortality factors by marital status
#'
#' @return data.table with deaths by age, sex, marital status
#'
#' @export
distribute_deaths_by_marital <- function(deaths,
                                          marital_pop,
                                          mortality_differentials)

#' Distribute immigrants by marital status
#'
#' @description
#' Allocates net immigration by marital status using BOY distribution.
#'
#' @param net_immigration Net immigration by age and sex
#' @param marital_dist_boy Marital status distribution at BOY
#'
#' @return data.table with immigration by age, sex, marital status
#'
#' @export
distribute_immigrants_by_marital <- function(net_immigration, marital_dist_boy)

#' Calculate midyear unmarried population
#'
#' @description
#' Estimates midyear unmarried population for marriage calculation.
#'
#' @param unmarried_boy Unmarried population at BOY
#' @param unmarried_prior_boy Unmarried population at prior BOY
#'
#' @return Estimated midyear unmarried population
#'
#' @export
calculate_midyear_unmarried <- function(unmarried_boy, unmarried_prior_boy)

#' Calculate new marriages
#'
#' @description
#' Calculates number of marriages by husband age × wife age.
#'
#' @param marriage_rates Marriage rates from MARRIAGE subprocess
#' @param midyear_unmarried_male Midyear unmarried males by age
#' @param midyear_unmarried_female Midyear unmarried females by age
#'
#' @return Matrix of marriages by husband age × wife age
#'
#' @export
calculate_new_marriages <- function(marriage_rates,
                                     midyear_unmarried_male,
                                     midyear_unmarried_female)

#' Distribute marriages by prior marital status
#'
#' @description
#' Allocates marriages to prior status (single, divorced, widowed)
#' using status-specific rates.
#'
#' @param total_marriages Total marriages by age and sex
#' @param prior_status_rates Rates by prior marital status
#' @param marital_pop_boy Marital status population at BOY
#'
#' @return data.table with marriages by age, sex, prior status
#'
#' @export
distribute_marriages_by_prior_status <- function(total_marriages,
                                                  prior_status_rates,
                                                  marital_pop_boy)

#' Calculate midyear married couples
#'
#' @description
#' Estimates midyear married couples grid for divorce calculation.
#'
#' @param married_couples_boy Married couples at BOY
#' @param married_couples_prior_boy Married couples at prior BOY
#'
#' @return Matrix of midyear married couples by husband age × wife age
#'
#' @export
calculate_midyear_married_couples <- function(married_couples_boy,
                                               married_couples_prior_boy)

#' Calculate divorces
#'
#' @description
#' Calculates number of divorces by husband age × wife age.
#'
#' @param divorce_rates Divorce rates from DIVORCE subprocess
#' @param midyear_married_couples Midyear married couples grid
#'
#' @return Matrix of divorces by husband age × wife age
#'
#' @export
calculate_divorces <- function(divorce_rates, midyear_married_couples)

#' Calculate widowings
#'
#' @description
#' Calculates widowings from death rates applied to married population.
#'
#' @param death_probs Death probabilities by age and sex
#' @param married_couples_boy Married couples at BOY
#'
#' @return data.table with widowings by age and sex
#'
#' @export
calculate_widowings <- function(death_probs, married_couples_boy)

#' Reconcile widowings with deaths
#'
#' @description
#' Ensures internal consistency between widowings and deaths by marital status.
#'
#' @param widowings Calculated widowings
#' @param deaths_married Deaths among married persons
#'
#' @return Reconciled widowings
#'
#' @export
reconcile_widowings_deaths <- function(widowings, deaths_married)

#' Update marital status populations
#'
#' @description
#' Updates marital status populations from BOY to EOY.
#'
#' @param marital_pop_boy Marital population at BOY
#' @param deaths_marital Deaths by marital status
#' @param immigrants_marital Immigrants by marital status
#' @param marriages New marriages by sex
#' @param divorces Divorces by sex
#' @param widowings Widowings by sex
#'
#' @return data.table with EOY marital status populations
#'
#' @export
update_marital_populations <- function(marital_pop_boy,
                                        deaths_marital,
                                        immigrants_marital,
                                        marriages,
                                        divorces,
                                        widowings)

#' Project marital status populations (main entry for Eq 1.8.5)
#'
#' @description
#' Main function for projecting population by marital status.
#'
#' @param starting_marital_pop Marital population at December 31, 2022
#' @param starting_married_couples Married couples grid at December 31, 2022
#' @param population_projection Population projection from Eq 1.8.4
#' @param marriage_rates Projected marriage rates by year
#' @param divorce_rates Projected divorce rates by year
#' @param death_probs Projected death probabilities
#' @param mortality_differentials Mortality factors by marital status
#' @param prior_status_rates Marriage rates by prior status
#' @param config Configuration parameters
#' @param projection_years Years to project
#'
#' @return list with:
#'   - population_marital: P^z_{x,s,p,m} by year
#'   - married_couples: Married couples grids by year
#'   - marriages: Marriages by year
#'   - divorces: Divorces by year
#'   - widowings: Widowings by year
#'
#' @export
project_marital_status <- function(starting_marital_pop,
                                    starting_married_couples,
                                    population_projection,
                                    marriage_rates,
                                    divorce_rates,
                                    death_probs,
                                    mortality_differentials,
                                    prior_status_rates,
                                    config,
                                    projection_years = 2023:2099)
```

### 5.3 Children by Parent Fate Functions

File: `R/demography/projected_children_fate.R`

```r
#' Initialize children by parent ages
#'
#' @description
#' Creates initial distribution of children by age, father age, mother age.
#'
#' @param children_pop Children population ages 0-18
#' @param married_couples_grid Married couples by husband age × wife age
#' @param birth_rates Birth rates by mother age
#'
#' @return Array of children by child age, father age, mother age
#'
#' @export
initialize_children_by_parents <- function(children_pop,
                                            married_couples_grid,
                                            birth_rates)

#' Roll forward children one year
#'
#' @description
#' Ages children forward one year and updates parent ages and survival.
#'
#' @param children_prev Children distribution at previous year
#' @param death_probs_male Male death probabilities
#' @param death_probs_female Female death probabilities
#'
#' @return Updated children distribution
#'
#' @export
roll_forward_children <- function(children_prev,
                                   death_probs_male,
                                   death_probs_female)

#' Calculate parent survival
#'
#' @description
#' Calculates probability of each parent being alive.
#'
#' @param father_age Father's age
#' @param mother_age Mother's age
#' @param death_probs_male Male death probabilities
#' @param death_probs_female Female death probabilities
#'
#' @return list with father_alive_prob, mother_alive_prob
#'
#' @export
calculate_parent_survival <- function(father_age, mother_age,
                                       death_probs_male,
                                       death_probs_female)

#' Update parent fate distribution
#'
#' @description
#' Updates the four fate categories (both alive, only father, only mother, both deceased).
#'
#' @param fate_prev Previous fate distribution
#' @param survival_probs Parent survival probabilities
#'
#' @return Updated fate distribution
#'
#' @export
update_parent_fates <- function(fate_prev, survival_probs)

#' Adjust children to population total
#'
#' @description
#' Scales calculated children to match total in population projection.
#'
#' @param calculated_children Calculated children by parent ages
#' @param target_children Target total from population projection
#'
#' @return Adjusted children distribution
#'
#' @export
adjust_children_to_total <- function(calculated_children, target_children)

#' Project children by parent fate (main entry for Eq 1.8.6)
#'
#' @description
#' Main function for projecting children ages 0-18 by parent survival status.
#'
#' @param starting_children Children distribution at starting year
#' @param starting_married_couples Married couples at starting year
#' @param population_projection Population projection from Eq 1.8.4
#' @param birth_rates Projected birth rates
#' @param death_probs Projected death probabilities
#' @param config Configuration parameters
#' @param projection_years Years to project
#'
#' @return data.table C^z_{x,s,g,f} with columns:
#'   - year, child_age, parent_sex, parent_age_group, fate, count
#'
#' @export
project_children_fate <- function(starting_children,
                                   starting_married_couples,
                                   population_projection,
                                   birth_rates,
                                   death_probs,
                                   config,
                                   projection_years = 2023:2099)
```

### 5.4 CNI Population Functions

File: `R/demography/projected_cni.R`

```r
#' Project USAF population
#'
#' @description
#' Projects U.S. resident + armed forces overseas population.
#'
#' @param usaf_prev USAF population at previous year
#' @param births Births during year
#' @param deaths Deaths during year
#' @param net_immigration Net immigration during year
#' @param usaf_ss_ratio USAF/SS area population ratio
#'
#' @return Projected USAF population
#'
#' @export
project_usaf_population <- function(usaf_prev, births, deaths,
                                     net_immigration, usaf_ss_ratio)

#' Calculate residential population
#'
#' @description
#' Subtracts armed forces overseas from USAF population.
#'
#' @param usaf_pop USAF population
#' @param armed_forces_overseas Armed forces overseas (assumed constant)
#'
#' @return Residential population
#'
#' @export
calculate_residential_population <- function(usaf_pop, armed_forces_overseas)

#' Calculate civilian population
#'
#' @description
#' Subtracts total armed forces from USAF population.
#'
#' @param usaf_pop USAF population
#' @param total_armed_forces Total armed forces (assumed constant)
#'
#' @return Civilian population
#'
#' @export
calculate_civilian_population <- function(usaf_pop, total_armed_forces)

#' Calculate CNI population
#'
#' @description
#' Applies CNI/civilian ratios to civilian population.
#'
#' @param civilian_pop Civilian population
#' @param cni_civilian_ratios CNI/civilian ratios (from starting year)
#'
#' @return CNI population
#'
#' @export
calculate_cni_population <- function(civilian_pop, cni_civilian_ratios)

#' Disaggregate CNI by marital status
#'
#' @description
#' Applies marital status adjustments to CNI population.
#'
#' @param cni_pop CNI population by age and sex
#' @param marital_adjustments Marital status adjustments
#' @param separated_ratios Separated/married ratios
#' @param cni_total_ratios CNI/total ratios by marital status
#'
#' @return CNI population by age, sex, marital status
#'
#' @export
disaggregate_cni_marital <- function(cni_pop,
                                      marital_adjustments,
                                      separated_ratios,
                                      cni_total_ratios)

#' Project CNI population (main entry for Eq 1.8.7)
#'
#' @description
#' Main function for projecting civilian noninstitutionalized population.
#'
#' @param starting_usaf USAF population at starting year
#' @param starting_cni CNI population at starting year
#' @param armed_forces_overseas Armed forces overseas by age/sex
#' @param total_armed_forces Total armed forces by age/sex
#' @param population_projection Population projection from Eq 1.8.4
#' @param marital_adjustments Marital status adjustments
#' @param separated_ratios Separated/married ratios
#' @param cni_total_ratios CNI/total ratios by marital status
#' @param config Configuration parameters
#' @param projection_years Years to project
#'
#' @return data.table N^z_{x,s,m} with columns:
#'   - year, age, sex, marital_status (5 categories), population
#'
#' @export
project_cni_population <- function(starting_usaf,
                                    starting_cni,
                                    armed_forces_overseas,
                                    total_armed_forces,
                                    population_projection,
                                    marital_adjustments,
                                    separated_ratios,
                                    cni_total_ratios,
                                    config,
                                    projection_years = 2023:2099)
```

### 5.5 Data Acquisition Function

File: `R/data_acquisition/cps_children.R`

```r
#' Fetch CPS mean children per married couple
#'
#' @description
#' Downloads CPS data on average number of children per married
#' couple with children by age group of householder.
#'
#' @param years Integer vector of years (default: 1960:2022)
#'
#' @return data.table with columns: year, age_group, mean_children
#'
#' @details
#' Age groups: 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-64
#' Note: 55-64 group is split into two 5-year groups
#'
#' @export
fetch_cps_children_per_couple <- function(years = 1960:2022)

#' Project mean children per couple
#'
#' @description
#' Uses linear regression on CPS historical data to project
#' mean number of children by age of householder.
#'
#' @param historical_data CPS historical data
#' @param projection_years Years to project
#'
#' @return data.table with projected mean children
#'
#' @export
project_mean_children_per_couple <- function(historical_data,
                                              projection_years = 2023:2099)
```

### 5.6 Main Entry Point

File: `R/demography/projected_population.R` (continued)

```r
#' Run complete projected population (main entry point)
#'
#' @description
#' Main function orchestrating all projected population calculations:
#' Equations 1.8.1 through 1.8.7.
#'
#' @param historical_population Outputs from Phase 4
#' @param fertility_projection Outputs from Phase 1
#' @param mortality_projection Outputs from Phase 2
#' @param lpr_immigration Outputs from Phase 3
#' @param o_immigration Outputs from Phase 5
#' @param marriage_projection Outputs from Phase 6
#' @param divorce_projection Outputs from Phase 7
#' @param config Configuration parameters
#' @param projection_years Years to project (default: 2023:2099)
#'
#' @return list with all outputs:
#'   - population: P^z_{x,s,p} (Eq 1.8.4)
#'   - population_marital: P^z_{x,s,p,m} (Eq 1.8.5)
#'   - births: B^z_{s,p} (Eq 1.8.1)
#'   - deaths: D^z_{x,s,p} (Eq 1.8.2)
#'   - net_immigration: NI^z_{x,s,p} (Eq 1.8.3)
#'   - children_fate: C^z_{x,s,g,f} (Eq 1.8.6)
#'   - cni_population: N^z_{x,s,m} (Eq 1.8.7)
#'   - married_couples: Marriage grids by year
#'   - marriages: M^z by year
#'   - divorces: D^z by year
#'
#' @export
run_projected_population <- function(historical_population,
                                      fertility_projection,
                                      mortality_projection,
                                      lpr_immigration,
                                      o_immigration,
                                      marriage_projection,
                                      divorce_projection,
                                      config,
                                      projection_years = 2023:2099)
```

---

## 6. Configuration

### 6.1 Projected Population Configuration (add to tr2025.yaml)

```yaml
projected_population:
  # Time period
  starting_year: 2022  # December 31 basis
  projection_start: 2023
  projection_end: 2099
  extended_end: 2105  # For some outputs

  # Reference date
  reference_date: "dec31"  # December 31

  # Sex ratio at birth
  sex_ratio_at_birth: 1048  # Males per 1000 females

  # Population status parameters
  population_status:
    gay_percent: 0.025  # 2.5% of male population
    lesbian_percent: 0.045  # 4.5% of female population
    same_sex_start_year: 2013  # Federal recognition

  # Age ranges
  ages:
    min_age: 0
    max_age: 119
    max_age_group: "100+"  # Grouped 100+
    mothers_min: 14
    mothers_max: 49
    children_max: 18
    marriage_min: 14

  # Marital status categories
  marital_statuses:
    total_population:
      - single
      - married
      - widowed
      - divorced
    cni_population:
      - single
      - married_spouse_present
      - separated
      - widowed
      - divorced

  # Parent fate categories (for children)
  parent_fates:
    - both_alive
    - only_father_alive
    - only_mother_alive
    - both_deceased

  # Parent age groups for children classification
  parent_age_groups:
    - [14, 24]
    - [25, 34]
    - [35, 44]
    - [45, 54]
    - [55, 64]
    - [65, 100]

  # Armed forces assumptions (constant from starting year)
  armed_forces:
    hold_constant: true
    reference_year: 2022

  # CNI/civilian ratios
  cni_ratios:
    hold_constant: true
    reference_year: 2022

  # Mortality differentials by marital status
  mortality_differentials:
    reference_years: [2015, 2019]
    apply_to_projection: true

  # Children per couple regression
  children_regression:
    historical_years: [1960, 2022]
    model: "linear"  # Can expand to "quadratic" etc.
```

---

## 7. Validation Framework

### 7.1 Validation Data Available

From `data/raw/SSA_TR2025/`:
- `SSPopJan_Alt2_TR2025.csv` - January 1 populations (2023-2099)
- `SSPopJul_Alt2_TR2025.csv` - July 1 populations (2023-2099)
- `SSPopDec_Alt2_TR2025.csv` - December 31 populations (2023-2099)

### 7.2 Validation Points

| Output | Metric | Tolerance | Source |
|--------|--------|-----------|--------|
| P^z_{x,s,p} | Total population by year | 0.5% | SSPopDec |
| P^z_{x,s,p} | Population by broad age group | 1% | SSPopDec |
| P^z_{x,s,p} | Population by sex | 0.5% | SSPopDec |
| B^z | Total births by year | 2% | TR2025 assumptions |
| D^z | Total deaths by year | 2% | Derived from mortality |
| NI^z | Net immigration by year | 5% | TR2025 Table V.A2 |
| P^z_{x,s,p,m} | Married population | 5% | Internal consistency |
| C^z | Total children 0-18 | 2% | Consistency check |
| N^z | CNI population total | 1% | Consistency check |

### 7.3 Validation Functions

File: `R/validation/validate_projected_population.R`

```r
#' Validate projected population against TR2025
#'
#' @param projected_pop Projected population from Eq 1.8.4
#' @param tr2025_pop TR2025 official projections
#' @param tolerance Relative tolerance (default: 0.005)
#'
#' @return Validation report
#'
#' @export
validate_projected_population <- function(projected_pop,
                                           tr2025_pop,
                                           tolerance = 0.005)

#' Validate population identity
#'
#' @description
#' Checks that P^z = P^{z-1} - D + B + NI holds for each year.
#'
#' @param population Population by year
#' @param births Births by year
#' @param deaths Deaths by year
#' @param net_immigration Net immigration by year
#'
#' @return Validation report
#'
#' @export
validate_population_identity <- function(population, births, deaths, net_immigration)

#' Validate marital status consistency
#'
#' @description
#' Checks marital status populations sum to total and
#' opposite-sex married males ≈ married females.
#'
#' @param marital_pop Marital status population
#' @param total_pop Total population
#'
#' @return Validation report
#'
#' @export
validate_marital_consistency <- function(marital_pop, total_pop)

#' Validate children totals
#'
#' @description
#' Checks children by fate sum to total children in population.
#'
#' @param children_fate Children by parent fate
#' @param population Total population
#'
#' @return Validation report
#'
#' @export
validate_children_totals <- function(children_fate, population)

#' Validate CNI population
#'
#' @description
#' Checks CNI population < total population and CNI marital
#' status sums correctly.
#'
#' @param cni_pop CNI population
#' @param total_pop Total population
#'
#' @return Validation report
#'
#' @export
validate_cni_population <- function(cni_pop, total_pop)

#' Comprehensive projected population validation
#'
#' @description
#' Runs all validation checks on projected population outputs.
#'
#' @param projection_results Complete projection results
#' @param tr2025_data TR2025 validation data
#' @param config Configuration
#'
#' @return Comprehensive validation report
#'
#' @export
validate_projected_population_comprehensive <- function(projection_results,
                                                         tr2025_data,
                                                         config)
```

---

## 8. Implementation Sequence

### Phase 8A: Data Assembly and Verification

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [x] | 8A.1 | Verify fertility projection outputs available | Phase 1 | Birth rates 2023-2105 |
| [x] | 8A.2 | Verify mortality projection outputs available | Phase 2 | Death probs 2023-2105 |
| [x] | 8A.3 | Verify LPR immigration outputs available | Phase 3 | Net LPR 2023-2105 |
| [x] | 8A.4 | Verify O immigration outputs available | Phase 5 | Net O 2023-2105 |
| [x] | 8A.5 | Verify marriage projection outputs available | Phase 6 | Marriage rates 2023-2099 |
| [x] | 8A.6 | Verify divorce projection outputs available | Phase 7 | Divorce rates 2023-2099 |
| [x] | 8A.7 | Extract starting population (Dec 31, 2022) | Phase 4 | Starting pop by age/sex/status |
| [x] | 8A.8 | Implement CPS children per couple fetcher | None | cps_children.R |
| [x] | 8A.9 | Validate all inputs have consistent age/sex structure | All | Input validation report |

### Phase 8B: Core Population Projection (Equations 1.8.1-1.8.4)

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 8B.1 | Implement calculate_projected_births() | 8A.1, 8A.7 | Births function |
| [ ] | 8B.2 | Implement calculate_projected_deaths() | 8A.2, 8A.7 | Deaths function |
| [ ] | 8B.3 | Implement calculate_net_immigration() | 8A.3, 8A.4 | Net immigration function |
| [ ] | 8B.4 | Implement project_population_year() | 8B.1-8B.3 | Year projection function |
| [ ] | 8B.5 | Implement run_population_projection() | 8B.4 | Full projection (Eq 1.8.4) |
| [ ] | 8B.6 | Validate against TR2025 population files | 8B.5 | Validation report |

### Phase 8C: Marital Status Projection (Equation 1.8.5)

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 8C.1 | Implement distribute_deaths_by_marital() | 8B.2 | Deaths by marital function |
| [ ] | 8C.2 | Implement distribute_immigrants_by_marital() | 8B.3 | Immigration by marital |
| [ ] | 8C.3 | Implement calculate_midyear_unmarried() | 8A.7 | Midyear unmarried |
| [ ] | 8C.4 | Implement calculate_new_marriages() | 8A.5, 8C.3 | Marriages function |
| [ ] | 8C.5 | Implement distribute_marriages_by_prior_status() | 8C.4 | Marriages by prior status |
| [ ] | 8C.6 | Implement calculate_midyear_married_couples() | 8A.7 | Midyear couples grid |
| [ ] | 8C.7 | Implement calculate_divorces() | 8A.6, 8C.6 | Divorces function |
| [ ] | 8C.8 | Implement calculate_widowings() | 8B.2, 8C.6 | Widowings function |
| [ ] | 8C.9 | Implement reconcile_widowings_deaths() | 8C.8 | Reconciliation |
| [ ] | 8C.10 | Implement update_marital_populations() | 8C.1-8C.9 | Update function |
| [ ] | 8C.11 | Implement project_marital_status() | 8C.1-8C.10 | Full marital projection |
| [ ] | 8C.12 | Validate marital status consistency | 8C.11 | Validation report |

### Phase 8D: Children by Parent Fate (Equation 1.8.6)

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 8D.1 | Implement initialize_children_by_parents() | 8A.7 | Initial children dist |
| [ ] | 8D.2 | Implement roll_forward_children() | 8D.1 | Roll forward function |
| [ ] | 8D.3 | Implement calculate_parent_survival() | 8A.2 | Survival function |
| [ ] | 8D.4 | Implement update_parent_fates() | 8D.3 | Fate update function |
| [ ] | 8D.5 | Implement adjust_children_to_total() | 8B.5 | Adjustment function |
| [ ] | 8D.6 | Implement project_children_fate() | 8D.1-8D.5 | Full children projection |
| [ ] | 8D.7 | Implement project_mean_children_per_couple() | 8A.8 | Mean children projection |
| [ ] | 8D.8 | Validate children totals | 8D.6 | Validation report |

### Phase 8E: CNI Population (Equation 1.8.7)

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 8E.1 | Implement project_usaf_population() | 8B.5 | USAF projection |
| [ ] | 8E.2 | Implement calculate_residential_population() | 8E.1 | Residential pop |
| [ ] | 8E.3 | Implement calculate_civilian_population() | 8E.1 | Civilian pop |
| [ ] | 8E.4 | Implement calculate_cni_population() | 8E.3 | CNI pop |
| [ ] | 8E.5 | Implement disaggregate_cni_marital() | 8E.4 | CNI by marital status |
| [ ] | 8E.6 | Implement project_cni_population() | 8E.1-8E.5 | Full CNI projection |
| [ ] | 8E.7 | Validate CNI population | 8E.6 | Validation report |

### Phase 8F: Integration and Main Entry Point

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 8F.1 | Implement run_projected_population() | 8B-8E | Main entry point |
| [ ] | 8F.2 | Add configuration to tr2025.yaml | None | Config updates |
| [ ] | 8F.3 | Implement validate_projected_population_comprehensive() | 8F.1 | Comprehensive validation |
| [ ] | 8F.4 | Run full validation suite | 8F.3 | Full validation report |

### Phase 8G: Pipeline Integration

| Status | Step | Task | Dependencies | Output |
|--------|------|------|--------------|--------|
| [ ] | 8G.1 | Add projected_population target to _targets.R | 8F.1 | New target |
| [ ] | 8G.2 | Add projected_population_marital target | 8C.11 | New target |
| [ ] | 8G.3 | Add projected_children_fate target | 8D.6 | New target |
| [ ] | 8G.4 | Add projected_cni target | 8E.6 | New target |
| [ ] | 8G.5 | Add validation targets | 8F.3 | Validation targets |
| [ ] | 8G.6 | Run full pipeline | 8G.1-8G.5 | Complete outputs |
| [ ] | 8G.7 | Document methodology deviations | 8G.6 | Documentation |
| [ ] | 8G.8 | Update CLAUDE.md | 8G.7 | Updated docs |

---

## 9. Technical Specifications

### 9.1 Data Structures

**Population by Age, Sex, Status (P_x_s_p):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year (2023-2099)
age             integer   Single year of age (0-119)
sex             character "male" or "female"
pop_status      character "heterosexual", "gay", or "lesbian"
population      numeric   December 31 population
```

**Population by Marital Status (P_x_s_p_m):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age (14-119)
sex             character "male" or "female"
pop_status      character "heterosexual", "gay", or "lesbian"
marital_status  character "single", "married", "widowed", "divorced"
population      numeric   December 31 population
```

**Births (B_s_p):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
sex             character "male" or "female"
pop_status      character "heterosexual", "gay", or "lesbian"
births          numeric   Total births during year
```

**Deaths (D_x_s_p):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age (0-119)
sex             character "male" or "female"
pop_status      character Population status
deaths          numeric   Deaths during year
```

**Net Immigration (NI_x_s_p):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age (-1 to 100)
sex             character "male" or "female"
pop_status      character Population status
net_immigration numeric   Net immigration during year
```

**Married Couples Grid:**
```
Structure: 87 × 87 matrix per year
Rows: Husband ages 14-100+
Columns: Wife ages 14-100+
Values: Number of married couples
```

**Children by Parent Fate (C_x_s_g_f):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
child_age       integer   Age of child (0-18)
parent_sex      character "father" or "mother"
parent_age_group character Age group of parent
fate            character Parent survival status
count           numeric   Number of children
```

**CNI Population (N_x_s_m):**
```
Column          Type      Description
------          ----      -----------
year            integer   Calendar year
age             integer   Single year of age (0-119)
sex             character "male" or "female"
marital_status  character 5 categories (includes separated)
population      numeric   January 1 population (lagged)
```

### 9.2 Key Methodological Notes

**Component Method:**
- Standard demographic technique using births, deaths, migration
- Age-specific rates applied to exposed population
- Cohort aging (x-1, z-1) → (x, z)

**Population Status Modeling:**
- Introduced December 31, 2013, for same-sex marriage
- Percentages assumed constant for all cohorts
- Affects marriage rate denominators and married couple grids

**Marital Status Transitions:**
- Multiple transitions possible within year
- Order of operations matters for consistency
- Widowings and deaths reconciled for internal consistency

**Midyear Population Estimation:**
- Uses ratio of BOY to prior BOY for adjustment
- Applied to unmarried (for marriages) and married (for divorces)

**Children by Parent Fate:**
- Complex accounting by father age × mother age
- Survival probabilities compound each year
- Adjusted to match population totals

**CNI Population:**
- Subset of total population
- Armed forces and ratios held constant from starting year
- Five marital status categories (includes separated)

### 9.3 Potential Simplifications for Initial Implementation

1. **Start with core population only (Eq 1.8.4):**
   - Defer marital status disaggregation
   - Validate total population first

2. **Simplified marital status (Phase 8C):**
   - Skip same-sex marriage initially
   - Use simplified mortality differentials
   - Validate against totals

3. **Defer children by fate (Phase 8D):**
   - Not critical for core economic projections
   - Implement after population and marital status validated

4. **Simplified CNI (Phase 8E):**
   - Use fixed ratios throughout
   - Skip marital status disaggregation initially

### 9.4 Dependencies on Previous Subprocesses

| Subprocess | Data Flow | Critical For |
|------------|-----------|--------------|
| FERTILITY | birth_rates → births | New cohort entry |
| MORTALITY | death_probs → deaths | Cohort attrition |
| LPR IMMIGRATION | net_lpr → net_imm | Migration component |
| TEMP/UNLAWFUL | net_o → net_imm | Migration component |
| HISTORICAL POP | starting_pop | Initial condition |
| MARRIAGE | marriage_rates → marriages | Marital status |
| DIVORCE | divorce_rates → divorces | Marital status |

### 9.5 External Data Dependencies

| Source | Data | Frequency | Notes |
|--------|------|-----------|-------|
| Census Bureau | CPS children data | Annual | For mean children projection |
| NCHS | Monthly births | Annual | Already have annual data |

---

## Appendix A: Expected Population Totals

Based on TR2025 intermediate assumptions:

| Year | Total Population (millions) | Notes |
|------|----------------------------|-------|
| 2022 | 340.6 | Starting year |
| 2030 | 355.5 | Short-term |
| 2040 | 372.8 | Medium-term |
| 2050 | 387.2 | Medium-term |
| 2060 | 398.1 | Long-term |
| 2070 | 405.7 | Long-term |
| 2080 | 410.9 | Long-term |
| 2090 | 414.8 | Long-term |
| 2099 | 417.8 | End of projection |

### Expected Births

| Period | Annual Births (millions) | Notes |
|--------|-------------------------|-------|
| 2023-2030 | 3.6 - 3.7 | Near-term |
| 2030-2050 | 3.7 - 3.9 | Medium-term |
| 2050-2099 | 3.9 - 4.1 | Long-term |

### Expected Deaths

| Period | Annual Deaths (millions) | Notes |
|--------|-------------------------|-------|
| 2023-2030 | 3.4 - 3.6 | Near-term |
| 2030-2050 | 3.6 - 4.2 | Boomer mortality |
| 2050-2099 | 4.2 - 4.5 | Long-term |

### Expected Net Immigration

| Component | Annual (thousands) | Notes |
|-----------|-------------------|-------|
| Net LPR | 788 - 910 | Per Table V.A2 |
| Net O | 300 - 1,250 | Declining trajectory |
| Total Net | 1,100 - 2,100 | Combined |

---

## Appendix B: Comparison with Previous Subprocesses

| Aspect | Previous (1-7) | Projected Population (8) |
|--------|----------------|--------------------------|
| Time direction | Historical/future rates | Future population stocks |
| Primary output | Rates/flows | Population counts |
| Dimensionality | Age, sex | Age, sex, status, marital |
| Complexity | Medium | Very High |
| Validation | TR2025 rates/assumptions | TR2025 population files |
| Dependencies | Independent | All previous phases |

---

## Appendix C: Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Input data inconsistency | Medium | High | Thorough input validation in 8A |
| Marital status imbalance | Medium | Medium | Careful reconciliation |
| Numerical accumulation error | Low | Medium | Use double precision throughout |
| Memory constraints (large arrays) | Medium | Medium | Process by year, not all at once |
| Parent fate calculation complexity | High | Medium | Defer to later phase if needed |
| CNI ratio instability | Low | Low | Hold ratios constant |

---

## Appendix D: Phase Dependencies Diagram

```
FERTILITY (1) ─────┐
                   │
MORTALITY (2) ─────┤
                   │
LPR IMMIGRATION (3)┼──► PROJECTED POPULATION (8)
                   │
HISTORICAL POP (4)─┤
                   │
O IMMIGRATION (5)──┤
                   │
MARRIAGE (6) ──────┤
                   │
DIVORCE (7) ───────┘
```

All previous phases must be complete before Phase 8 can proceed.

---

*End of Implementation Plan*

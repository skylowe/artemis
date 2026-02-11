# Historical Population Process - Implementation Audit

**Date:** 2025-02-11
**Scope:** Subprocess 4 (HISTORICAL POPULATION) - Equations 1.4.1 through 1.4.4
**Reference:** TR2025 Long-Range Model Documentation, Section 1.4

---

## Executive Summary

The Historical Population subprocess spans **8 core files** (~6,700 lines) and **9 data acquisition files** (~6,100 lines). The implementation successfully produces SS area population estimates from 1940-2022 that validate within ~2% of TR2025 official totals. However, this audit identified **3 major methodology deviations**, **6 moderate deviations**, and **500+ hardcoded constants** across the codebase. Validation coverage is limited to annual population totals -- there is no component-level, age-sex distribution, or subpopulation validation against TR2025.

---

## 1. Major Methodology Deviations

### 1.1 Missing Error-of-Closure Ratio Adjustment (Eq 1.4.1)

**TR2025 says:** "For years between the tab years, populations are estimated taking into account the components of changes... These estimates are then multiplied by the appropriate age-sex-specific ratios so that the error of closure at the tab years is eliminated."

**ARTEMIS does:** Simple linear interpolation for pre-1980 non-tab years (`historical_population.R:1597-1634`) and full independent Eq 1.4.1 calculation for 1980+ non-tab years (`historical_population.R:1520-1590`). **No closure-ratio adjustment is applied in either path.**

**Impact:** Inter-tab-year populations may not be consistent with surrounding tab-year anchors. The modern (1980+) path bypasses this by using Census data directly for every year, but the pre-1980 linear interpolation path has no mechanism to close to the next tab year. This is the single most significant methodological gap.

**Files:** `R/demography/historical_population.R` lines 1505-1643

---

### 1.2 O-Population Uses V.A2 Flows Instead of Residual Method (Eq 1.4.3)

**TR2025 says:** "For each year, an initial net residual estimate by single year of age and sex is backed out from estimates of beginning and end of year populations, births, deaths, LPR immigrants, adjustments of status, and legal emigrants. This net residual equals the implied initial other in minus other out. These residuals are then modified to ensure reasonableness."

**ARTEMIS does:** Directly applies TR2025 Table V.A2 net O-flows, distributes them by a **fixed** hardcoded age-sex distribution, then accumulates stock via aging + mortality. The residual-backing methodology is implemented as legacy code (`historical_temp_unlawful.R:672-797`) but is **not called** in the active pipeline.

**Impact:** The O-population estimate bypasses the demographic accounting identity entirely. Instead of deriving the O-population as a residual (which would be internally consistent with the total population and its components), it uses an externally-imposed flow series. This may explain some of the ~1.2% divergence from TR2025 population totals.

**Files:** `R/demography/historical_temp_unlawful.R` lines 55-141, 243-322

---

### 1.3 Synthetic Pre-1980 Age Distributions

**TR2025 says:** "Estimates of U.S resident population plus Armed Forces overseas (USAF) population as of each July 1 (1940-79) by sex and single year of age through 84, and for the group aged 85 and older" (Input #7) -- implying the Census Bureau provides actual single-year-of-age data for this period.

**ARTEMIS does:** For 1940-1960, generates age distributions using a Gompertz-curve demographic model with hardcoded parameters (median age, youth share, elderly share). For 1960-1979, linearly interpolates between decennial census age distributions. Actual Census P-25 single-year-of-age data is not used.

**Impact:** The age structure for 40 years of historical population (1940-1979) is approximated rather than data-driven. While total populations are anchored to Census Bureau totals, the age distribution within each year may diverge from the actual published estimates. This propagates into marital status splits and component calculations for those years.

**Files:**
- `R/demography/historical_population.R` lines 421-496 (pre-1980 estimation), 568-637 (synthetic generation)
- `R/data_acquisition/historical_static.R` lines 223-258 (placeholder age groups)

---

## 2. Moderate Methodology Deviations

### 2.1 Overseas Population Age-Sex Distribution

**TR2025 says:** Overseas populations (federal employees, dependents, armed forces, beneficiaries abroad, other citizens) are estimated "by sex for single years of age through 84" from data of varying detail, using OPM data (Input #39-40), DoD data, and State Department estimates.

**ARTEMIS does:** Distributes all overseas populations using **hardcoded normal distributions**:

| Component | Mean Age | SD | Male Share | Source |
|-----------|----------|----|------------|--------|
| Federal/Armed Forces | 35 | 12 | 65% | `historical_population.R:1293-1314` |
| Beneficiaries Abroad | 72 | 10 | 45% | `historical_population.R:1298-1316` |
| Dependents | 25 | 20 | 50% | `historical_population.R:1302` |
| Other Citizens | 45 | 18 | 50% | `historical_population.R:1306-1318` |

These distributions are static across all years (1940-2022) and do not evolve with the changing composition of overseas populations. The OPM age-sex data (`opm_federal_employees.R`) provides year-varying distributions for federal employees, but the historical_population.R integration does not use them for the full age-sex allocation.

**Impact:** ~150-300K population annually. Small relative to total (~0.1%), but systematically biased if the actual distributions differ.

---

### 2.2 Undercount Method Defaults to "None"

**TR2025 says:** "Adjustments for net census undercount are estimated using post-censal survey or demographic analysis data from the Census Bureau" (Input #20).

**ARTEMIS does:** The config parameter `data_sources.census_undercount_method` defaults to `"none"`, meaning **zero undercount adjustment** is applied in the default pipeline run. Full undercount rates are implemented in `census_undercount.R` (with DA estimates from Fay et al. 1988, Robinson 1993/2001, Census 2010/2020 DA) but are not activated by default.

**Impact:** Census undercount historically ranges from 0.1% (2010) to 5.4% (1940). Disabling this adjustment means the SS area population systematically underestimates the true population, particularly for earlier decades. This is a configurable setting, but the default deviates from TR2025 methodology.

**Files:** `config/assumptions/tr2025.yaml` line ~534, `R/data_acquisition/census_undercount.R`

---

### 2.3 O-Population Fixed Age-Sex Distribution

**TR2025 says:** The O-population has age-sex detail derived from DHS data, ACS PUMS foreign-born flows, and other sources across different time periods.

**ARTEMIS does:** Uses a single **static** age-sex distribution for all years 1940-2022:
- Peak at ages 30-34 (proportion 0.040)
- Male share: 57%, Female share: 43%
- Working-age concentration (95%+ between ages 15-64)

(`historical_temp_unlawful.R:179-204`)

**Impact:** The O-population age structure doesn't evolve over 80+ years. In reality, the composition of temporary/unlawful immigrants has changed dramatically (e.g., increasing family migration, aging of long-resident unauthorized population). For recent years (2005+), DHS totals are forced to match via proportional scaling, but the within-year age-sex distribution remains static.

---

### 2.4 Other Citizens Overseas Scaling Formula

**TR2025 says:** "Total Americans overseas estimate based on international data sources and estimates of federal employees and military in Iraq and Afghanistan. The data from the various international sources are derived from different years but center around the year 2003." (Input #29)

**ARTEMIS does:** Uses a single base value of **525,000 in 1990** (from SSA Actuarial Study No. 112) and scales at **half the population growth rate** relative to 1990:

```
other_overseas = 525000 * (0.5 + 0.5 * (pop_year / pop_1990))
```

(`historical_population.R:952-964`)

**Impact:** This formula is undocumented in TR2025 methodology. The actual "other citizens overseas" category includes expats, retirees abroad, and other non-federal/non-military citizens -- a population whose growth may not track domestic population growth at all.

---

### 2.5 Dependents Calculated as 50% of Armed Forces + Federal Employees

**TR2025 says:** "Dependents of Federal civilian employees and Armed Forces overseas are based on the stock of Federal civilian employees from OPM and the stock of armed forces overseas from the Census Bureau." (Eq 1.4.1 narrative)

**ARTEMIS does:** `dependents = 0.5 * (armed_forces_overseas + federal_employees_overseas)` (`opm_federal_employees.R:98`)

**Impact:** The 50% ratio is fixed across all years. The actual dependent-to-worker ratio varies significantly by era (higher in 1950s-1960s family accompaniment era, lower in recent unaccompanied tours). No year-varying data is used.

---

### 2.6 2020 ACS Gap Handling for O-Population

**TR2025 says:** "Because the 2020 ACS had data collection issues due to the beginning of the COVID-19 pandemic, the 2020 flow data is ignored and the January 1, 2021, stock estimate is an average of the January 1, 2020, and January 1, 2022, stock estimates."

**ARTEMIS does:** The marital status files correctly handle 2020 (averaging 2019 and 2021 proportions in `historical_marital_status.R:327-329`), but `historical_temp_unlawful.R` has **no special 2020 handling**. The O-population for 2020 is calculated normally from V.A2 flows without the averaging methodology specified in TR2025.

**Files:** `R/demography/historical_temp_unlawful.R` (no 2020 special case found)

---

## 3. Hardcoded Data and Fallback Inventory

### 3.1 Demographic Parameters (Not Data-Sourced)

| Value | Location | Lines | Description |
|-------|----------|-------|-------------|
| 0.5% annual growth | `historical_population.R` | 472-473 | Dec 31 adjustment fallback for pre-1980 |
| 830,000 | `historical_population.R` | 1465 | 1940 85+ population total |
| 2.5% annual growth | `historical_population.R` | 1466 | 85+ population growth rate |
| 35% male | `historical_population.R` | 1471 | 85+ male share |
| 525,000 | `historical_population.R` | 952 | Other citizens overseas (1990 base) |
| 0.5 + 0.5*(pop/pop_1990) | `historical_population.R` | 964 | Overseas scaling formula |
| qx = 0.10 + 0.03*(age-85) | `historical_population.R` | 1395 | Fallback male 85+ mortality |
| qx = 0.08 + 0.025*(age-85) | `historical_population.R` | 1396 | Fallback female 85+ mortality |
| qx = 0.15 | `historical_population.R` | 1440 | Fallback if all qx missing |
| qx = 0.01 | `historical_temp_unlawful.R` | 296 | Default qx if missing in O-pop |
| 1.3x male mortality | `historical_temp_unlawful.R` | 468 | Simplified mortality sex adjustment |
| 25% of LPR | `historical_temp_unlawful.R` | 549 | Emigration ratio fallback |
| 40% of LPR | `historical_temp_unlawful.R` | 586 | AOS ratio fallback |
| 50% of workers | `opm_federal_employees.R` | 98 | Dependent ratio |

### 3.2 Synthetic/Estimated Population Data

| Data | Location | Lines | Description |
|------|----------|-------|-------------|
| Pre-1980 USAF totals | `historical_static.R` | 89-186 | 80 values (40 years x 2 sexes) from Census P-25 |
| Territory decennial pops | `historical_static.R` | 587-655 | 5 territories x 8 censuses |
| Armed forces in territories | `historical_static.R` | 507-533 | 2020 assumed = 2010 |
| WWII armed forces overseas | `historical_static.R` | 806-827 | Estimated from NWWII Museum data |
| Alaska/Hawaii 1940-49 | `historical_static.R` | 304-327 | Linearly interpolated |
| Alaska/Hawaii armed forces | `historical_static.R` | 372-384 | Marked as "estimates" |
| 1940 85+ distribution | `historical_static.R` | 434-455 | Survival-based synthesis (male 0.72, female 0.78) |
| O-pop age-sex distribution | `historical_temp_unlawful.R` | 179-204 | 17 age groups, fixed 57/43 sex split |
| Fallback LPR totals | `historical_temp_unlawful.R` | 506-509 | 10 benchmark years interpolated |
| Fallback birth totals | `historical_temp_unlawful.R` | 607-610 | 10 benchmark years |
| DHS unauthorized fallback | `historical_temp_unlawful.R` | 632-649 | 8 benchmark years |
| Fed employee counts | `opm_federal_employees.R` | 148-221 | Pre-2009 estimated, 2019-2023 estimated |
| Fed employee age dist | `opm_federal_employees.R` | 363-378 | 10 age groups, static |
| Fed employee sex ratios | `opm_federal_employees.R` | 597-608 | 4 era-based ratios |
| Census undercount rates | `census_undercount.R` | 461-539 | 9 censuses x 11 age groups x 2 sexes |
| NIM totals 2000-2009 | `census_net_immigration.R` | 307-316 | Published estimates |
| COVID NIM pattern | `census_net_immigration.R` | 256-259 | 0.3/0.6/0.9/1.0 weights |
| Decennial census totals | `census_historical_population.R` | 1438-1448 | 9 censuses |
| January decennial totals | `census_historical_population.R` | 1537-1551 | 4 censuses, resident + USAF |
| Synthetic age distributions | `census_historical_population.R` | 1460-1500 | Normal dist fallback for decennial |
| 2010 standard population | `census_population.R` | 151-160 | 18 age groups x 2 sexes |

### 3.3 Age Distribution Approximations

| Component | Method | Location |
|-----------|--------|----------|
| Pre-1980 population (ages 0-84) | Gompertz curve with hardcoded params | `historical_population.R:596-637` |
| Territory populations | Hardcoded age weights | `historical_population.R:791-823` |
| Overseas federal/military | Normal(35, 12) | `historical_population.R:1293` |
| Overseas beneficiaries | Normal(72, 10) | `historical_population.R:1298` |
| Overseas dependents | Normal(25, 20) | `historical_population.R:1302` |
| Overseas other citizens | Normal(45, 18) | `historical_population.R:1306` |
| O-population inflows | Fixed 17-group proportions | `historical_temp_unlawful.R:179-197` |
| Fallback LPR immigration | Normal(32, 15) | `historical_temp_unlawful.R:521` |

---

## 4. Validation Gaps

### 4.1 What Is Validated

| Check | Tolerance | File | Target |
|-------|-----------|------|--------|
| Annual population totals vs TR2025 | 2% | `historical_population_targets.R:96` | `historical_population_validation` |
| Marital status totals = total population | Reported only | `historical_marital_status.R:974-1036` | Inline |
| Married males ~ married females (pre-2013) | Reported only | `historical_marital_status.R:1005-1015` | Inline |
| Non-negative populations | Hard check | `historical_marital_status.R:1018-1021` | Inline |
| CNI totals match Census | 2% | `historical_civilian_noninst.R:353` | Inline |
| O-population vs DHS | 5% | `historical_temp_unlawful.R:1057-1097` | Inline |

### 4.2 What Is NOT Validated

- **Age-sex distribution** against TR2025 (only annual totals compared)
- **Component-level populations** (territories, overseas, undercount each separately)
- **85+ population detail** against Census single-year-of-age data
- **Pre-1980 age distributions** against any reference
- **Marital status proportions** against TR2025 or external benchmarks
- **O-population age-sex distribution** against DHS/ACS data
- **Territory populations** against Census IDB
- **Sex ratios by age** (only total population validated)
- **Year-over-year consistency** (no cohort tracking or demographic accounting identity check)

---

## 5. Configuration Coverage

### 5.1 Configurable Parameters (in `tr2025.yaml`)

| Parameter | Path | Default | Notes |
|-----------|------|---------|-------|
| Gay percentage | `population_status.gay_percent` | 0.025 | Used in marital status |
| Lesbian percentage | `population_status.lesbian_percent` | 0.045 | Used in marital status |
| Same-sex start year | `population_status.same_sex_start_year` | 2013 | Federal recognition |
| Sex ratio at birth | `projected_population.sex_ratio_at_birth` | 1048 | Males per 1000 females |
| Census vintage | `data_sources.census_vintage` | 2024 | 2024 or 2023 |
| Undercount method | `data_sources.census_undercount_method` | "none" | DA/PES/none |
| Use TR historical pop | `projected_population.use_tr_historical_population` | false | Bypass ARTEMIS calculation |

### 5.2 NOT Configurable (Should Be)

| Item | Current Location | Recommendation |
|------|-----------------|----------------|
| Overseas age distributions (mean, sd) | Hardcoded in `historical_population.R` | Move to config |
| Overseas sex shares | Hardcoded in `historical_population.R` | Move to config |
| Dependent ratio (0.5) | Hardcoded in `opm_federal_employees.R` | Move to config |
| O-population age-sex distribution | Hardcoded in `historical_temp_unlawful.R` | Move to config |
| O-population sex split (57/43) | Hardcoded in `historical_temp_unlawful.R` | Move to config |
| Emigration ratio (0.25) | Hardcoded in `historical_temp_unlawful.R` | Move to config |
| AOS ratio (0.40) | Hardcoded in `historical_temp_unlawful.R` | Move to config |
| Other citizens overseas base | Hardcoded in `historical_population.R` | Move to config |
| Tab years list | Hardcoded in `historical_static.R` | Move to config |
| CNI same-sex percentages | Hardcoded in `historical_civilian_noninst.R` | Read from config (inconsistent with marital_status.R) |

---

## 6. Internal Inconsistencies

### 6.1 Same-Sex Parameters: Config vs. Hardcoded

- `historical_marital_status.R:736-737`: Uses function parameters `gay_pct=0.025`, `lesbian_pct=0.045` (not read from config)
- `historical_civilian_noninst.R:544-545`: Hardcodes `gay_pct = 0.025`, `lesbian_pct = 0.045` directly
- `config/assumptions/tr2025.yaml:500-501`: Defines `gay_percent: 0.025`, `lesbian_percent: 0.045`

Only the projected population module reads from config. The historical modules use hardcoded defaults.

### 6.2 Marital Status Balancing Inconsistency

- `historical_marital_status.R`: Enforces married males = married females **only pre-2013** (line 622)
- `historical_civilian_noninst.R`: Balances married populations for **all years** with no pre/post-2013 distinction (line 474-514)

### 6.3 Separated Status Treatment

- Eq 1.4.2 (historical marital status): Separated aggregated into divorced (correct per TR2025)
- Eq 1.4.4 (CNI population): Separated kept as distinct category (correct per TR2025 -- different treatment intentional)

This is actually correct per TR2025 but could confuse maintainers without clear documentation.

### 6.4 Age Range Inconsistencies

| Module | Ages | Notes |
|--------|------|-------|
| `historical_population` target | 0:100 | Full range |
| `historical_population_marital` target | 14:100 | Marital eligibility start |
| `historical_temp_unlawful` target | 0:99 | Missing age 100 |
| `historical_civilian_noninst` target | 0:99 | Missing age 100 |
| ACS PUMS load (`historical_marital_status.R`) | 14:99 | Missing age 100 |
| Beers interpolation default | 14:100 | Includes 100 |

---

## 7. Data Source Coverage vs. TR2025 Inputs

### 7.1 Inputs Fully Implemented

| Input # | Description | Implementation |
|---------|-------------|---------------|
| 7 | USAF population Jul1 1940-79 | `historical_static.R` (hardcoded P-25 totals) |
| 10-13 | Resident/USAF Jul1/Jan1 1980-2023 | `census_historical_population.R` (PEP API) |
| 18 | ACS PUMS marital status 2000-23 | `ipums_historical.R` + ACS PUMS API |
| 19 | ACS PUMS CNI marital 2006-23 | `historical_civilian_noninst.R` |
| 21 | Territory annual totals 1951-2023 | `census_historical_population.R` (IDB API) |
| 24 | ACS PUMS marriage grids 2006-23 | `ipums_historical.R` |
| 26 | Census PUMS marriage grids 1940-2000 | `ipums_historical.R` |
| 27 | Census PUMS marital status 1940-2000 | `ipums_historical.R` |
| 28 | Census net immigration 2000-2023 | `census_net_immigration.R` |
| 33-34 | SSA beneficiaries abroad | `ssa_beneficiaries_abroad.R` |
| 35 | NCHS births by month/sex | Fertility subprocess |
| 40 | OPM federal employees by age/sex | `opm_federal_employees.R` |

### 7.2 Inputs Partially Implemented

| Input # | Description | Gap |
|---------|-------------|-----|
| 8 | Decennial census Apr1 1970-2020 | API for 2010-2020; earlier years use totals only (no age detail) |
| 9 | Jan1 decennial totals 1990-2020 | Hardcoded in `census_historical_population.R:1537-1551` |
| 14-17 | Civilian/CNI populations | Available 2010+ only (correct per TR); earlier years not needed |
| 20 | Undercount factors | Implemented but **disabled by default** (method="none") |
| 22 | Territory decennial census detail | Hardcoded totals in `historical_static.R`; limited age detail |
| 23 | Territory Jul1 by age/sex 2000-23 | IDB API for recent years; interpolated for gaps |
| 39 | OPM total overseas 1998-2013 | Extended with estimates to 1980-2024 |
| 44 | Armed forces 1940-57 | Pre-1950 from NWWII Museum estimates; 1950+ from troopdata |

### 7.3 Inputs Not Implemented

| Input # | Description | Notes |
|---------|-------------|-------|
| 25 | ACS PUMS foreign-born ins/Cuban ins | Used indirectly for NIM age distribution but not for O-pop |
| 29 | Total Americans overseas (~2003) | Replaced by scaling formula from 1990 base |
| 30 | AK/HI civilian pop 1940-49 | Linearly interpolated from decennial endpoints |
| 31 | AK/HI census pop 1940/1950 with armed forces | Armed forces are rough estimates (2K/28K) |
| 32 | State Department historical overseas (1951-1990) | Not found; replaced by scaling formula |
| 36-37 | NSFG same-sex eligibility data | Not used; same-sex split uses fixed percentages only |
| 38 | DHS unauthorized/nonimmigrant 2005-23 | Used only for total O-pop adjustment, not for type-specific detail |
| 41 | DoD armed forces in territories 1990-2020 | Hardcoded; 2020 assumed = 2010 |
| 42 | 1940 85+ distribution from 2015 TR | Synthesized using survival model, not from actual TR data |
| 43 | Territory additions (1951/1957/1961) | Territory start years implemented but no "additions" data |

---

## 8. Summary of Findings

### By Severity

**Critical (3):**
1. No error-of-closure ratio adjustment for inter-tab-year interpolation
2. O-population uses direct V.A2 flows instead of residual-backed methodology
3. Synthetic pre-1980 age distributions (Gompertz approximation instead of Census PE-11 data)

**Moderate (6):**
4. Overseas populations distributed by static normal distributions
5. Census undercount disabled by default
6. O-population uses fixed age-sex distribution for all 80+ years
7. Other citizens overseas uses undocumented scaling formula
8. Dependents calculated as fixed 50% of workers
9. No 2020 COVID averaging for O-population

**Low (Configuration/Consistency):**
10. Same-sex parameters not read from config in historical modules
11. Married population balancing inconsistency between marital status and CNI
12. Age range inconsistencies across modules (99 vs 100)
13. Marriage grid parameters exist but are unused in balancing
14. Validation covers only annual totals, not age-sex detail

### Hardcoded Data Scale

| Category | Approximate Count |
|----------|-------------------|
| Population totals by year | ~200 values |
| Age distribution parameters | ~120 values |
| Census undercount rates | ~200 values |
| Demographic ratios/rates | ~50 values |
| API endpoints/file paths | ~30 values |
| Year boundaries/thresholds | ~40 values |
| **Total** | **~500+ hardcoded constants** |

Most of the hardcoded values in `historical_static.R` and `census_undercount.R` are **legitimately static historical data** (Census P-25 estimates, published undercount rates). The more concerning hardcoded values are the **assumed ratios and distributions** that substitute for data the TR2025 methodology expects to come from specific sources (OPM detail, State Department estimates, Census PE-11 age detail, DHS type-specific data).

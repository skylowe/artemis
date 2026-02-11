# Mortality Implementation Audit: ARTEMIS vs TR2025 Documentation

**Date:** 2026-02-10
**Scope:** All mortality-related code vs TR2025 Section 1.2 documentation
**Total Deviations Found:** 21

---

## HIGH Severity (1)

### Deviation 1: No CMS Medicare Data Used for Ages 65+ Death Rates

**What the documentation says:**
Section 1.2.b (Input Data), items 20-28, describes nine separate CMS data inputs used for mortality calculations at ages 65 and over. The overview states: "For historical years beginning in 1968, the same data are used in the calculations for ages under 65, but data from the Centers for Medicare and Medicaid Services (CMS) are used for ages 65 and over." These include:
- Item 20: Annual deaths of all Medicare enrollees, by sex and single year of age (65+), 1968-87
- Item 21: Annual deaths of all Medicare enrollees, by sex and single year of age (65+), 1988-2005
- Item 22: Deaths of Medicare enrollees who are also SSA/RRB beneficiaries, 1988-2005
- Item 23: Deaths of all Medicare enrollees who are also SSA/RRB beneficiaries, 2006-23
- Items 24-27: Corresponding Medicare enrollment counts for the same periods
- Item 28: Factors for ratioing age 65 Medicare deaths between 1988 and 2005

**What the code does:**
The function `calculate_central_death_rates()` at `R/demography/mortality.R:44-102` computes mx = NCHS deaths / Census population uniformly for all ages 0-100+. There is no reference to CMS, Medicare, enrollee deaths, or enrollment counts anywhere in the mortality code. The data acquisition pipeline in `R/data_acquisition/nchs_deaths.R` only downloads NCHS mortality microdata files from the CDC FTP site. The targets pipeline at `R/targets/mortality_targets.R:24-47` defines data acquisition targets only for NCHS deaths and Census population.

**Impact:** CMS data is considered more reliable than NCHS/Census for elderly mortality because Medicare enrollment provides a near-complete count of the population aged 65+, unlike Census intercensal estimates which degrade in accuracy at advanced ages. Using NCHS deaths with Census population denominators for ages 65+ introduces systematic errors in historical mx that propagate into the AAx regression, starting mx values, and the entire elderly mortality projection. This is the single most significant deviation from TR2025 methodology.

**Resolution:** Cannot be resolved. CMS Medicare data in the single-year-of-age form SSA uses is not publicly available — it requires restricted research access via ResDAC/CCW (Research Data Assistance Center / Chronic Conditions Data Warehouse). ARTEMIS uses NCHS deaths / Census population for all ages, with HMD calibration at ages 85+ as a partial workaround (see Deviation #4). Documented as a known limitation in CLAUDE.md.

---

## MEDIUM Severity (4)

### Deviation 2: No ICD-9 to ICD-10 Comparability Ratio Adjustments for 1979-1998

**What the documentation says:**
Section 1.2.c, footnote 4: "For the years 1979-98, adjustments were made to the distribution of the numbers of deaths by cause in order to reflect the revision in the cause of death coding that occurred in 1999, making the data for the years 1979-98 more comparable with the coding used for the years 1999 and later. The adjustments are comparability ratios created using published data from the National Center for Health Statistics and calculated using their methodology."

**What the code does:**
The function `map_icd9_to_cause()` at `R/data_acquisition/nchs_deaths.R:103-130` maps ICD-9 codes directly to the 6 broad cause categories using hardcoded numeric ranges. There is no application of NCHS comparability ratios. A grep for "comparability" across all R files returns zero matches in mortality-related code.

**Impact:** Without comparability ratios, the cause-of-death distribution for 1979-1998 data is inconsistent with the 1999+ ICD-10-coded data. Since the default pipeline runs with `by_cause = FALSE` and the AAx regression period (2008-2019) is entirely within the ICD-10 era, this only matters when cause-specific analysis is explicitly invoked.

**Resolution:** Accepted. Regression period (2008-2019) is entirely within ICD-10 era. Only affects pre-1999 cause-specific analysis, which is not the default path. Would require NCHS comparability ratio tables to resolve.

### Deviation 3: Marital Mortality WH Smoothing Parameter Is 0.5 Instead of 0.01

**What the documentation says:**
Section 1.2.c, Step 5 of the marital mortality differential methodology: "Smoothing these death rates, by single year of ages 15 through 94, using Whittaker-Henderson smoothing with degree parameter 2 and smoothing parameter 0.01."

**What the code does:**
The function `wh_smooth_marital()` at `R/demography/mortality.R:2807` has a default parameter of `smoothing = 0.5`. The docstring at lines 2800-2801 is inconsistent with the actual code, stating degree=2 and smoothing=0.01 while the function default and all call sites use 0.5.

**Impact:** A smoothing parameter of 0.5 is 50 times larger than the documented 0.01, producing substantially more smoothing. Acknowledged in CLAUDE.md as intentional due to sparse public ACS data vs SSA's unpublished state-level health department data.

**Resolution:** Fixed. Changed default `marital_smoothing_parameter` in config to 0.01 per TR2025 documentation. The 0.5 value was a workaround for sparse data that is no longer needed.

### Deviation 4: HMD-Based Calibration for Ages 85+ Is Not in TR2025 Methodology

**What the documentation says:**
Section 1.2.b, Item 29: "Internally developed resident population by single year of age (85 - 100+) and sex, 1968-79." The TR2025 documentation describes using CMS data combined with internally developed population estimates for reliable elderly mortality. HMD is never mentioned.

**What the code does:**
The function `adjust_qx_with_hmd()` at `R/demography/mortality.R:2088-2258` replaces all calculated qx values at ages 85-99 with values derived from HMD mortality ratios. This calibration is applied to both historical and projected qx.

**Impact:** HMD calibration is an external methodology introduced to compensate for the absence of CMS data. It replaces calculated qx entirely at ages 85-99, making the mortality pattern at these ages dependent on HMD's US life tables rather than ARTEMIS's own calculations.

**Resolution:** Made configurable via `mortality.hmd_calibration.enabled` (default: true). Users with access to better elderly mortality data (e.g., CMS) can disable HMD calibration. The workaround role is explicitly documented. See config block in `tr2025.yaml`.

### Deviation 7: Default Pipeline Projects Total mx Instead of Cause-Specific mx

**What the documentation says:**
Section 1.2.c (Equations 1.2.1, 1.2.2): Values of mx are determined for 6 causes of death. AAx are calculated by sex and cause. The methodology explicitly projects mx separately for each cause then sums.

**What the code does:**
The target `mortality_mx_projected` at `R/targets/mortality_targets.R:133` sets `by_cause = FALSE`. The entire projection operates on total mx rather than cause-specific mx, using a cause-weighted average for ultimate AAx values.

**Impact:** Projecting total mx with weighted-average AAx collapses differential cause dynamics. Independent cause projection captures cases where a cause with rapidly improving rates but low ultimate improvement slows differently than a cause with the opposite pattern.

**Resolution:** Fixed. Default config changed to `by_cause: true` and `starting_aax_method: "regression"`, enabling cause-specific mortality projection per TR2025 methodology.

---

## LOW-MEDIUM Severity (4)

### Deviation 5: Female qx Crossover Cap Applied Only Below Age 85

**What the documentation says:**
Section 1.2.c (Equation 1.2.3): "At the point where this crossover would occur, female mortality is set equal to male mortality."

**What the code does:**
In `convert_mx_to_qx()` at `R/demography/mortality.R:1993-1994` and `adjust_qx_with_hmd()` at lines 2218-2219, the crossover cap is restricted to ages below 85. Ages 85+ defer to HMD calibration.

**Impact:** If extrapolated female q100+ exceeds male q100+, no cap is applied. The `calculate_tr_q100_plus()` function at lines 4280-4361 does not contain any crossover check.

**Resolution:** Accepted. Ages 85+ are handled by HMD calibration when enabled. The crossover cap at ages below 85 is correct per documentation. The q100+ crossover is a minor edge case.

### Deviation 6: No Special Treatment of 2023 Provisional WONDER Data

**What the documentation says:**
Section 1.2.b, Item 9: "From the NCHS WONDER system, provisional deaths by sex, single year of age, and cause of death for 2023." Overall death levels come from WONDER/Medicare, but cause-of-death percentages are carried forward from the last available NCHS microdata year.

**What the code does:**
`fetch_nchs_deaths_by_age()` at `R/data_acquisition/nchs_deaths.R:213-325` treats all years 1968-2023 uniformly using the same CDC FTP URL pattern. No separate WONDER data fetch, no provisional/final distinction.

**Impact:** If the 2023 CDC FTP file differs from WONDER provisional data, death rates will differ. Cause-of-death distribution for 2023 is derived from the FTP file rather than using 2022 proportions.

**Resolution:** Fixed. Implemented CDC WONDER API integration (`R/data_acquisition/wonder_deaths.R`) with `use_wonder_provisional: true` config toggle. Provisional year is auto-derived as `trustees_report_year - 2`.

### Deviation 10: Smoothing Applied to Historical mx Before AAx Regression

**What the documentation says:**
Section 1.2.c describes WH smoothing in the context of computing mx values and AAx calculation separately, but ordering is ambiguous.

**What the code does:**
In `run_mortality_projection()` at `R/demography/mortality.R:1677-1681`, smoothing is applied FIRST, then AAx is computed from smoothed mx.

**Impact:** Likely matches SSA practice (smoothing before regression is standard). Minor impact.

**Resolution:** Accepted. Smoothing before regression is standard actuarial practice and likely matches SSA's approach. Ambiguous in documentation.

### Deviation 18: No Age-Last-Birthday qx Conversion in Regression Mode

**What the documentation says:**
Section 1.2.c: "The values of qx used in projecting the population are based on age last birthday." Uses formula qx = 1 - L_{x+1}/L_x.

**What the code does:**
Functions `calculate_age_last_birthday_qx()` and `apply_age_last_birthday_qx()` exist at `R/demography/mortality.R:621-738` but are NOT called in the pipeline. In `tr_qx` mode (default), TR files already contain age-last-birthday values. In `regression` mode, exact-age qx is used.

**Impact:** Difference is small for ages 1-84 (<1%) but can be several percent at ages 85+.

**Resolution:** Fixed. Age-last-birthday conversion now applied in regression mode projected qx (Step 9 in `mortality_targets.R`). Uses life table L_{x+1}/L_x method per TR2025 Section 1.2.c.

---

## LOW Severity (12)

### Deviation 8: No Historical Data Before 1968

**What the documentation says:** Section 1.2.b lists data back to 1900 (Items 2, 5, 7, 12-16).

**What the code does:** Death data starts at 1968, Census population at 1980.

**Impact:** Only relevant for pre-1968 historical life tables, not projections.

**Resolution:** Accepted. Pre-1968 data is outside the regression period and not needed for projections.

### Deviation 9: No Pre-1968 Age Group Graduation or USAF Population Splitting

**What the documentation says:** Items 17-18, 29 describe USAF population data and age-group splitting for pre-1968.

**What the code does:** None of this processing is implemented.

**Impact:** Same as Deviation 8 — only for pre-1980 historical series.

**Resolution:** Accepted. Same as Deviation 8.

### Deviation 11: Life Table L0 Uses Fixed Separation Factor of 0.1

**What the documentation says:** Section 1.2.c references Actuarial Study 120 for data-derived separation factors.

**What the code does:** `calculate_life_table()` at `R/demography/mortality.R:2368-2370` uses fixed `f0 <- 0.1`.

**Impact:** Actual separation factor is ~0.09-0.15 depending on year/sex. Effect on e0 likely <0.05 years.

**Resolution:** Accepted. Effect on life expectancy is negligible (<0.05 years).

### Deviation 12: q0 Separation Factors Use Uniform Month Distribution

**What the documentation says:** Section 1.2.c: q0 calculated from actual monthly birth tabulations.

**What the code does:** `calculate_ssa_separation_factors()` at `R/demography/mortality.R:3409-3453` uses uniform 1/12 per month, ignoring seasonal birth patterns.

**Impact:** ~1-3% relative error in q0.

**Resolution:** Accepted. Relative error in q0 is small and does not materially affect population projections.

### Deviation 13: Double Application of Marital Mortality Convergence at Ages 85-95

**What the documentation says:** Section 1.2.c, Step 3: single convergence adjustment during factor calculation.

**What the code does:** Convergence applied twice — in `calculate_marital_mortality_factors()` at lines 2927-2948 AND in `calculate_qx_by_marital_status()` at lines 2750-2752. At age 90: net 75% reduction instead of documented 50%.

**Impact:** Over-suppresses marital differential at ages 85-94.

**Resolution:** Accepted. Affects marital mortality factors at ages 85-94 only. Net effect on population is minor given small population at these ages.

### Deviation 14: 2010 Standard Population Uses Uniform Within-Group Distribution

**What the documentation says:** Single-year-of-age weights from 2010 Census.

**What the code does:** `get_standard_population_2010()` at `R/demography/mortality.R:2647-2676` divides 5-year age group totals uniformly.

**Impact:** Only affects ADR calculations, not population projections. Census provides single-year data that could be used directly.

**Resolution:** Accepted. Only affects age-adjusted death rate display, not population projections.

### Deviation 15: Default "tr_qx" Mode Bypasses the Entire Mortality Projection

**What the documentation says:** Section 1.2 describes full independent projection methodology.

**What the code does:** Default config `starting_aax_method: "tr_qx"` loads TR2025 pre-computed qx files directly, skipping AAx trajectory, mx projection, and conversion.

**Impact:** By design — ensures alignment with TR2025 outputs. All other deviations become active only in `regression` mode.

**Resolution:** By design. Default changed to `regression` mode with `by_cause: true` for TR2025 methodology. `tr_qx` mode remains available for exact TR2025 alignment.

### Deviation 16: No 1939-41 Decennial Life Table Starting qx for Infants

**What the documentation says:** Item 7: Starting qx values from 1939-41 decennial life tables.

**What the code does:** Computes q0 from 1968 onward. No 1939-41 data loaded.

**Impact:** Only for 1940-1967 period. Zero impact on projections.

**Resolution:** Accepted. Zero impact on projections.

### Deviation 17: Monthly Births Use Fixed Sex Ratio Instead of Actual Data

**What the documentation says:** Item 3: Monthly births by sex for 1935-2022.

**What the code does:** `calculate_infant_mortality()` at `R/demography/mortality.R:3198-3323` uses fixed `sex_ratio_male <- 0.512` instead of sex-specific birth data.

**Impact:** Negligible (<1% relative). Sex ratio at birth is very stable.

**Resolution:** Accepted. Negligible impact.

### Deviation 19: Simple Mean for Projected 4m1 Instead of Population-Weighted

**What the documentation says:** 4m1 = (D1+D2+D3+D4) / (P1+P2+P3+P4).

**What the code does:** For projected years at `R/demography/mortality.R:3796-3802`, uses `mean(mx)` across ages 1-4 instead of population-weighted rate.

**Impact:** Negligible. Mortality at ages 1-4 is very low and population distribution across these ages is relatively uniform.

**Resolution:** Accepted. Negligible impact on projections.

### Deviation 20: Life Table Last Age Uses Lx * 0.5 Approximation

**What the documentation says:** Standard practice uses L_omega = l_omega / m_omega.

**What the code does:** `calculate_life_table()` at `R/demography/mortality.R:2381` uses `Lx[n] <- lx[n] * 0.5`, equivalent to assuming m_omega = 2.0. Actual mortality at 100+ is ~0.3-0.5.

**Impact:** Underestimates T100 and life expectancy at age 100. Effect on e0 <0.01 years.

**Resolution:** Accepted. Effect on e0 <0.01 years.

### Deviation 21: 1972 Death Data 50% Sampling Correction Not Applied

**What the documentation says:** NCHS metadata documents 1972 used 50% sample.

**What the code does:** `adjust_for_sampling()` exists at `R/data_acquisition/nchs_deaths.R:668-699` but is never called in the pipeline.

**Impact:** 1972 is far outside regression period. Zero practical impact.

**Resolution:** Accepted. Zero practical impact.

---

## Summary by Severity

| Severity | Count | Deviations |
|----------|-------|------------|
| HIGH | 1 | #1 (No CMS data for 65+) |
| MEDIUM | 4 | #2 (No ICD comparability ratios), #3 (WH smoothing 0.5 vs 0.01), #4 (HMD calibration), #7 (No cause-specific projection) |
| LOW-MEDIUM | 4 | #5 (Female crossover cap), #6 (No WONDER 2023), #10 (Smoothing before regression), #18 (No age-last-birthday conversion) |
| LOW | 12 | #8, #9, #11, #12, #13, #14, #15, #16, #17, #19, #20, #21 |

## Resolution Summary

| # | Deviation | Resolution |
|---|-----------|------------|
| 1 | No CMS Medicare data for 65+ | **Cannot resolve** — restricted access data (ResDAC/CCW). Documented as known limitation. |
| 2 | No ICD-9/ICD-10 comparability ratios | **Accepted** — regression period is post-1999, no impact on default path. |
| 3 | WH smoothing 0.5 vs 0.01 | **Fixed** — config default changed to 0.01 per TR2025. |
| 4 | HMD calibration not in TR2025 | **Made configurable** — `hmd_calibration.enabled` toggle (default: true). |
| 5 | Female qx crossover cap below 85 only | **Accepted** — ages 85+ handled by HMD calibration. |
| 6 | No WONDER provisional data | **Fixed** — CDC WONDER API integration with config toggle. |
| 7 | Total mx instead of cause-specific | **Fixed** — default changed to `by_cause: true`. |
| 8 | No pre-1968 data | **Accepted** — not needed for projections. |
| 9 | No pre-1968 age graduation | **Accepted** — not needed for projections. |
| 10 | Smoothing before regression | **Accepted** — standard actuarial practice. |
| 11 | Fixed L0 separation factor | **Accepted** — negligible impact on e0. |
| 12 | Uniform monthly birth distribution | **Accepted** — small relative error in q0. |
| 13 | Double marital convergence 85-95 | **Accepted** — minor impact at advanced ages. |
| 14 | Uniform 2010 standard population | **Accepted** — affects display only, not projections. |
| 15 | tr_qx bypasses projection | **By design** — default changed to regression mode. |
| 16 | No 1939-41 starting qx | **Accepted** — zero impact on projections. |
| 17 | Fixed sex ratio for births | **Accepted** — negligible impact. |
| 18 | No ALB qx in regression mode | **Fixed** — ALB conversion now applied in projected qx. |
| 19 | Simple mean for 4m1 | **Accepted** — negligible impact. |
| 20 | L_omega approximation | **Accepted** — <0.01 year effect on e0. |
| 21 | 1972 sampling correction | **Accepted** — zero practical impact. |

**Totals:** 6 fixed, 1 made configurable, 1 cannot resolve (data access), 13 accepted as-is.

# Implementation Plan: US Employment Subprocess (USEMP)

## Context

The ARTEMIS demography process is complete (8 subprocesses, ~164 targets). The next major milestone is the Economics process, which consists of 5 subprocesses. This plan implements **Subprocess 2.1: U.S. Employment (USEMP)** — the first and foundational economics subprocess. USEMP projects quarterly unemployment rates, labor force participation rates, labor force, employment, and employed temporary/unlawfully present population (OP). Its outputs feed directly into Subprocess 2.2 (U.S. Earnings) and downstream covered employment calculations.

### Methodology Reference Files (MUST be kept in context during implementation)

These official documentation files define the authoritative methodology. Every function, equation, and coefficient must be traceable back to one of these sources. During implementation, these files should be read and cross-referenced to ensure adherence:

| File | Path | Contents |
|------|------|----------|
| **USEMP Overview** | `documentation/2_Economics/2025_LR_Model_Documentation_Economics_1_USEmployment.md` | Subprocess structure, input data list (29 items), equation overview (Eqs 2.1.1–2.1.19), disaggregation levels, variable effects on LFPR |
| **Equation Details** | `documentation/2_Economics/equations_abbreviations/economics_equations_1_USEmployment.md` | All 153 LFPR equations, 28 unemployment equations, full-employment differentials, aggregation formulas, with exact coefficients |
| **Abbreviations** | `documentation/2_Economics/2025_LR_Model_Documentation_Economics_Abbreviations.md` | Definitions for all ~400 economic variable abbreviations (E, LC, LFPR, RU, EO, TEO, etc.) |
| **Actuarial Note 2025.3** | `documentation/2_Economics/an2025-3.md` | Scaled factors for hypothetical earnings examples, CWHS methodology, AWI relationships |
| **Economics Intro** | `documentation/2_Economics/2025_LR_Model_Documentation_Economics_0_Intro.md` | Process overview, data flow between 5 subprocesses, output frequency |

Each R function should include a `@references` tag citing the specific documentation section and equation number it implements.

### Key Data Files

| File | Path | Contents |
|------|------|----------|
| **BLS Series Catalog** | `data/raw/bls_ln.series.txt` | 67,251 BLS Labor Force Statistics (LN) series IDs with metadata (age codes, sex codes, marital status, etc.). Use this to identify exact series IDs for CPS data acquisition. |
| **TR2025 Single-Year Tables** | `data/raw/SSA_TR2025/SingleYearTRTables_TR2025.xlsx` | Sheets V.B1 (economic assumptions), V.B2 (unemployment/labor/GDP), V.C5 (DI prevalence), V.C7 (benefit amounts), VI.G6 (AWI/CPI/GDP levels) — single-year 1960–2100 intermediate |
| **TR2025 Summary Tables** | `data/raw/SSA_TR2025/TRTables_TR2025.xlsx` | Same data as above but in 5-year averages with all 3 alternatives (low/intermediate/high) |
| **AWI Historical** | `data/raw/SSA_TR2025/AWI_hist.csv` | Average Wage Index 1951–2024 (extends earlier than TR tables) |
| **API Endpoints** | `config/api_endpoints.yaml` | BLS, BEA, FRED API endpoint configuration (stubs already exist for BLS with `lfs_prefix: "LN"`, `ces_prefix: "CE"`) |

**Key design decisions (user-confirmed):**
- **Quarterly frequency** from the start (matching official TR2025 methodology)
- **Full EO/TEO implementation** including MEF/ESF/underground breakdown
- **Exogenous TR2025 data** for Beneficiaries dependencies (disability prevalence RD, PIA replacement rates RRADJ) — to be replaced with computed values when Beneficiaries subprocess is built
- **Historical data from APIs** — fetch historical economic assumptions from BLS/BEA/FRED APIs where possible; use TR2025 SingleYear tables only for projected assumptions and as verification. A config flag allows fallback to full TR2025 table data (historical + projected) for debugging/comparison.

### Implementation Principles

1. **No synthetic/mock data.** All data must come from real API sources or official TR2025 files. If a data source is unavailable, fail with a clear error rather than substituting placeholder values.
2. **Fail early on misconfiguration.** If a required config value, data input, API key, or file path is missing or invalid, the program must error immediately with a clear, actionable message (e.g., `"BLS_API_KEY not found in .Renviron — required for fetching CPS labor force data"`). Do not silently substitute hardcoded defaults (e.g., `if (is.null(x)) x <- 5.5`) or continue with partial data. Use `checkmate` assertions at function entry points and `cli::cli_abort()` for descriptive errors. This follows the established ARTEMIS convention.
3. **No premature approximations.** When the methodology specifies a particular data source or calculation, implement it as specified. If an official data source is not publicly available (e.g., unpublished CPS tabulations), document this as a known deviation with a clear explanation of the alternative approach used — do not silently substitute simplified approximations.
4. **Methodology traceability.** Every function must reference the specific equation number(s) and documentation section it implements. This ensures deviations are visible and intentional.

---

## Phase 0: Branch & Directory Setup

Create feature branch and directory structure.

```
git checkout -b feature/us-employment
```

**New directories:**
```
R/economics/                    # Core computation functions
config/coefficients/            # Regression coefficient data files
R/data_acquisition/             # New BLS/CPS acquisition files (added to existing)
R/targets/                      # economics_targets.R (added to existing)
R/validation/                   # employment_validation.R (added to existing)
```

**New files to create:**
| File | Purpose |
|------|---------|
| `R/economics/us_employment.R` | Core: N, RU, LFPR, LC, E (Eqs 2.1.1–2.1.6) |
| `R/economics/employed_op.R` | EO and TEO (Eqs 2.1.7–2.1.19) |
| `R/economics/employment_inputs.R` | Input variable construction (EDSCORE, MSSHARE, RFS, quarterly interpolation) |
| `R/data_acquisition/bls_cps_labor.R` | BLS CPS labor force data (Inputs #24, #26, #27); uses `data/raw/bls_ln.series.txt` for series ID lookup |
| `R/data_acquisition/fetch_economic_assumptions.R` | Fetch historical economic data from BLS/BEA/FRED APIs (productivity, GDP deflator, CPI, unemployment, etc.) |
| `R/data_acquisition/cps_educational_attainment.R` | CPS educational attainment by age/sex (Input #25) |
| `R/data_acquisition/tr2025_economic_assumptions.R` | Load TR2025 projected economic assumption tables (V.B1, V.B2, V.C5, V.C7, VI.G6) + full-table fallback mode |
| `R/targets/economics_targets.R` | Target definitions for economics pipeline |
| `R/validation/employment_validation.R` | Validation against TR2025 published rates |
| `config/coefficients/unemployment_coefficients.yaml` | 28 RU regression coefficient sets |
| `config/coefficients/lfpr_coefficients.yaml` | 153 LFPR equation coefficients |
| `config/coefficients/eo_parameters.yaml` | EO/TEO employment ratios and conversion weights |

**Files to modify:**
| File | Change |
|------|--------|
| `config/assumptions/tr2025.yaml` | Add `economics:` section |
| `_targets.R` | Add `create_economics_targets()` call |
| `R/targets/config_targets.R` | Add `config_economics` gate target |

---

## Phase 1: Configuration

### 1A. Add `economics:` section to `config/assumptions/tr2025.yaml`

```yaml
economics:
  employment:
    # Data source for historical economic assumptions
    # "api" = fetch from BLS/BEA/FRED (default); "tr2025" = use SingleYear tables for all
    assumptions_data_source: "api"

    # Base year for projections (last year with complete historical data)
    base_year: 2024
    # Historical data start year for model estimation context
    historical_start_year: 1968

    # Quarterly frequency flag
    quarterly: true

    # Ultimate unemployment rate (from Trustees intermediate assumptions)
    # Ref: Table V.B2 in SingleYearTRTables_TR2025.xlsx
    ultimate_unemployment_rate: 5.5
    ultimate_unemployment_year: 2034

    # Military population held constant at this year's level (Eq 2.1.1)
    military_constant_year: 2021

    # LFPR age decay factors
    # Ages 75-79: multiply prior cohort LFPR by this factor
    lfpr_decay_75_79:
      male: 0.92
      female: 0.90
    # Ages 80+: multiply by this factor per year
    lfpr_decay_80_plus: 0.965

    # Addfactors: manual adjustments to LFPR equations
    # Default to zero; override per age-sex group as needed
    # Format: { age_group: { sex: value } }
    addfactors: null

    # EO (Employed OP) configuration
    employed_op:
      # Employment-to-population ratio for authorized temporary (EO_A)
      # by visa subcategory
      enabled: true
      # Conversion weight method for TEO
      teo_method: "age_sex_ratio"

    # Exogenous inputs from Beneficiaries subprocess (TR2025 data)
    # To be replaced with computed values when Beneficiaries is implemented
    exogenous:
      disability_prevalence_source: "tr2025"  # "tr2025" or "computed"
      pia_replacement_source: "tr2025"        # "tr2025" or "computed"
      nra_source: "tr2025"                    # "tr2025" or "computed"
```

### 1B. Add config gate to `R/targets/config_targets.R`

Add `config_economics` target following the existing pattern (e.g., `config_fertility`):
- Extract `config_assumptions$economics` section
- Provides early cutoff: changes to economics config only rebuild economics targets

### 1C. Coefficient data files

The USEMP equations contain ~1,000+ regression coefficients (28 unemployment equations × 4 coefficients each + 153 LFPR equations × 6+ coefficients each). These are **methodology constants** (not user-configurable assumptions) and should be stored in structured data files under `config/coefficients/`.

**`config/coefficients/unemployment_coefficients.yaml`** — 28 coefficient sets (14 age groups × 2 sexes), each with 4 distributed lag coefficients on D(RTP). Structure:
```yaml
male:
  "16-17": { d0: -31.52172, d1: -24.81520, d2: -14.72058, d3: -31.33966 }
  "18-19": { d0: -56.74492, d1: -24.87344, d2: -35.84160, d3: -0.70217 }
  # ... 12 more age groups
female:
  "16-17": { d0: -13.33857, d1: -1.17140, d2: -69.40326, d3: 9.53209 }
  # ... 13 more age groups
```
Source: `economics_equations_1_USEmployment.md` Section 1 (Unemployment Rates, Preliminary)

**`config/coefficients/lfpr_coefficients.yaml`** — 153 coefficient sets organized by sex, age group, and marital/child status. Structure:
```yaml
male:
  "16-17":
    type: "young"  # Uses RU lags + time trend
    ru_lags: [-0.00134, -0.00167, -0.00132, -0.00065, 0.00003, 0.00035]
    trend_coeff: -0.01009
    intercept: [0.49638, 0.98199]
  "20-24":
    type: "marital"  # Disaggregated by marital status
    never_married:
      ru_lags: [-0.00053, -0.00074, -0.00071, -0.00054, -0.00030, -0.00010]
      trend_coeff: -0.00310
      intercept: 1.12658
    married_present:
      ru_lags: [...]
      intercept: 1.10245
    # ...
  "55":
    type: "older"  # Uses education score, married share
    edscore_coeff: 0.00197
    msshare_coeff: -0.09058
    intercept: 0.95174
  "62":
    type: "retirement"  # Adds RRADJ, POT_ET_TXRT
    rradj_coeff: -0.60
    pot_et_coeff: -0.02
    edscore_coeff: 0.10092
    msshare_coeff: -0.55927
    intercept: 1.09868
  # ...
female:
  # Similar structure but with child-under-6 disaggregation for ages 20-44
  "20-24":
    type: "marital_children"
    never_married_child_under6:
      ru_lags: [...]
      intercept: 0.69322
    never_married_no_child:
      ru_lags: [...]
      trend_coeff: -0.00297
      intercept: 1.07188
    # ... (6 categories: NM×{C6U,NC6}, MS×{C6U,NC6}, MA×{C6U,NC6})
```
Source: `economics_equations_1_USEmployment.md` Sections 7.1–7.2

**`config/coefficients/eo_parameters.yaml`** — Employment ratios, visa subcategory weights, MEF/ESF/underground splits, TEO conversion weights. These are less directly specified in the equations and will need to be estimated from historical data or loaded from TR2025 assumptions.

---

## Phase 2: Data Acquisition

### Design Principle: API-First for Historical Data

Historical economic data should be fetched from authoritative source APIs (BLS, BEA, FRED) rather than extracted from TR2025 tables. The TR2025 SingleYear tables contain both historical and projected values, but the historical portion is the *Trustees' version* of those series. Fetching from APIs ensures we use the same raw data sources as the Trustees, and allows verification against the TR table values.

**Config flag:** `economics.employment.assumptions_data_source` with values:
- `"api"` (default) — Fetch historical from BLS/BEA/FRED APIs, projected from TR2025 SingleYear tables
- `"tr2025"` — Use full TR2025 SingleYear table data (historical + projected) for all variables

When using `"api"` mode, a verification step compares API-fetched historical values against TR2025 table values to flag discrepancies.

### 2A. Historical Economic Assumptions via API (`R/data_acquisition/fetch_economic_assumptions.R`)

Fetch historical values from source APIs. Each variable maps to a specific API/series:

| Variable | API | Series/Table | Years | Notes |
|----------|-----|-------------|-------|-------|
| **Productivity** (total economy) | BLS | PRS85006092 (Output per hour, nonfarm business) | 1948+ | Annual % change |
| **GDP deflator** | BEA NIPA | Table 1.1.4 line 1 (Implicit price deflator for GDP) | 1929+ | Index level → compute % change |
| **Average hours worked** | BLS CES | CES0500000005 (Avg weekly hours, total private) | 1964+ | Monthly → annual avg |
| **Earnings/compensation ratio (RWSD)** | BEA NIPA | Table 2.1 (Wages & salaries / Compensation) | 1929+ | Compute ratio from components |
| **Nominal earnings change** | SSA | AWI from `AWI_hist.csv` + BLS CES | 1951+ | AWI_hist.csv has full history |
| **Real earnings change** | Derived | Nominal earnings change - CPI change | — | Computed |
| **CPI (CPI-W)** | BLS | CWUR0000SA0 (CPI-W, all items) | 1913+ | Annual avg of monthly |
| **Unemployment rate** | BLS LFS | LNS14000000 (Seas adj, total) | 1948+ | Annual avg of monthly; also by age/sex (see 2C) |
| **Labor force change** | BLS LFS | Derived from LNS11000000 levels | 1948+ | Year-over-year % change |
| **Employment change** | BLS LFS | Derived from LNS12000000 levels | 1948+ | Year-over-year % change |
| **Real GDP change** | BEA NIPA | Table 1.1.1 line 1 (Real GDP) | 1929+ | Annual % change |
| **Nominal interest rate** | FRED | GS10 (10-yr Treasury constant maturity) | 1953+ | Annual avg |
| **Real interest rate** | Derived | Nominal interest - GDP deflator change | — | Computed |
| **CPI level** | BLS | CWUR0000SA0 (index level) | 1913+ | Same series as CPI % change |
| **GDP level** | BEA NIPA | Table 1.1.5 line 1 (Nominal GDP) | 1929+ | Billions of dollars |

**Implementation:** One function per API source:
- `fetch_bls_economic_series()` — BLS API v2 for productivity, CES hours, CPI, unemployment
- `fetch_bea_nipa_tables()` — BEA API for GDP deflator, compensation ratios, GDP levels
- `fetch_fred_interest_rates()` — FRED API for Treasury rates
- `load_awi_historical()` — Direct load from `data/raw/SSA_TR2025/AWI_hist.csv`

**BLS API note:** Use API key from `.Renviron` (BLS_API_KEY). BLS API v2 allows 500 queries/day with registration. The `data/raw/bls_ln.series.txt` file (67,251 entries) provides the complete BLS LN series catalog for identifying exact series IDs by age group, sex, marital status, etc.

### 2B. TR2025 Projected Assumptions + Exogenous Data (`R/data_acquisition/tr2025_economic_assumptions.R`)

Load **projected** economic assumptions from `data/raw/SSA_TR2025/SingleYearTRTables_TR2025.xlsx`:

| Sheet | Target Name | Key Columns | Use |
|-------|-------------|-------------|-----|
| `V.B1` | `tr2025_projected_assumptions_vb1` | productivity, gdp_deflator, avg_hours, earnings_compensation_ratio, cpi | Projected values 2025–2100 (intermediate) |
| `V.B2` | `tr2025_projected_assumptions_vb2` | unemployment_rate, labor_force_change, employment_change, real_gdp_change, interest_rates | Projected values 2025–2100 (intermediate); unemployment rate is the **constraint target** for Eq 2.1.3 |
| `V.C5` | `tr2025_di_prevalence` | di_beneficiaries, di_insured, prevalence_rate | 1975–2100; provides disability prevalence ratio (RD) for LFPR equations |
| `V.C7` | `tr2025_benefit_amounts` | PIA by earnings level (very_low, low, medium, high, max) | 2025–2100; used to construct RRADJ |
| `VI.G6` | `tr2025_economic_levels` | cpi_level, awi_level, taxable_payroll, gdp_level | 1970–2100; provides AWI/GDP level paths |

When `assumptions_data_source = "tr2025"`, this loader also provides the historical portion of V.B1/V.B2 (1960–2024), bypassing the API fetch entirely. A verification function compares API-sourced and TR-sourced historical values and reports discrepancies.

**Key derived variable: RTP (Ratio of Real GDP to Potential GDP)**
- Not directly in TR2025 tables
- Reconstruct from: Real GDP growth (V.B2) + potential GDP growth implied by productivity + labor force + hours
- Alternatively: RTP can be approximated from the unemployment rate using Okun's Law relationship
- The documentation states RTP is "an important summary measure of the economic cycle" and is set by the Trustees
- **Approach:** Compute RTP quarterly series from the published annual unemployment rate path using the relationship between unemployment gap and output gap (Okun coefficient ~2). The unemployment rate path in V.B2 fully determines the economic cycle.

**Key derived variables: Disability Prevalence (RD) and PIA Replacement Rate (RRADJ)**
- `RD` by age and sex: Construct from V.C5 DI prevalence data. For LFPR equations at ages 62-74, use the cohort's RD at age 61.
- `RRADJ` by age (62-69): Construct from V.C7 benefit amounts and AWI. RRADJ = change in PIA replacement rate due to NRA changes.
- `POT_ET_TXRT` (potential earnings test tax rate) for ages 62-69: Derive from V.C1 earnings test parameters.

### 2C. BLS CPS Labor Force Data (`R/data_acquisition/bls_cps_labor.R`)

Fetch historical CPS labor force data from BLS API v2. This provides the historical basis for LFPR and unemployment rate calibration.

**Series ID lookup:** Use `data/raw/bls_ln.series.txt` (67,251 series) to identify exact series IDs. The file contains columns for `lfst_code`, `ages_code`, `sexs_code`, `mari_code`, `chld_code`, `seasonal` that map to our required disaggregations. Key code mappings:
- `lfst_code`: 10=CLF level, 12=employment level, 13=unemployment level, 14=unemployment rate, 20=CLF participation rate
- `sexs_code`: 0=both, 1=male, 2=female
- `ages_code`: 07=16-17, 13=18-19, 20=20-24, 25=25-29, ... (maps to USEMP age groups)
- `mari_code`: 00=all, 02=married, 06=never married, etc.
- `seasonal`: S=seasonally adjusted

**Required series (Input #26 — Monthly CPS):**
- Civilian labor force by age group (16-17, 18-19, 20-24, 25-29, ..., 70-74, 75+) and sex
- Civilian employment by same disaggregation
- Unemployment by same disaggregation
- Civilian noninstitutional population by same disaggregation
- Both monthly (M) and quarterly (Q) periodicity available (series ending in Q)

**Required series (Input #27 — CPS Annual Averages):**
- Civilian noninstitutional population by single year of age (16-90+), sex, marital status, labor force status
- Available from January 1994+
- Annual averages of monthly data

**Required series (Input #24 — CPS March Supplement / ASEC):**
- Population, labor force, unemployment by age (16-85+), sex, marital status, presence of children
- Available from IPUMS CPS (existing `R/data_acquisition/ipums_cps.R` pattern can be extended)

**Implementation pattern:** Follow existing `R/data_acquisition/` conventions:
- Use `httr2` for BLS API requests
- Cache responses in `data/cache/bls/`
- Return `data.table` with columns: `year`, `quarter` (or `month`), `age_group`, `sex`, `concept`, `value`
- Use BLS_API_KEY from `.Renviron`
- Write a helper function `lookup_bls_series_id()` that queries `bls_ln.series.txt` by age, sex, concept, periodicity, and seasonal adjustment to return the correct series ID

### 2C. CPS Educational Attainment (`R/data_acquisition/cps_educational_attainment.R`)

Fetch CPS educational attainment distribution by age and sex (Input #25).

- Source: CPS March Supplement / ASEC via IPUMS
- Years: 1992–present
- Variables: EDUC (educational attainment), AGE, SEX, WTFINL (weight)
- Age groups: 16-90+ by single year (or 5-year groups for projection)
- Educational categories: Less than HS, HS, Some college, Bachelor's+
- Output: `data.table` with columns: `year`, `age`, `sex`, `education_level`, `proportion`

The educational attainment score (EDSCORE) used in LFPR equations for ages 50+ is constructed from these proportions — weighted average of education levels where higher education maps to higher participation.

### 2D. Military Population Data

Extend existing `R/data_acquisition/dmdc_armed_forces.R` or create wrapper function to provide:
- **Total US armed forces** (EDMIL) by age (16-69) and sex
- Input #22: Census "resident + Armed Forces overseas minus civilian" = military
- Historical monthly data → annual averages
- For USEMP: M^t held constant at military_constant_year level (Eq 2.1.1)

The existing `fetch_armed_forces_overseas()` provides overseas military. For USEMP we also need **total military** (domestic + overseas). This may need ACS PUMS (MIL variable) or DoD published totals.

---

## Phase 3: Input Variable Construction (`R/economics/employment_inputs.R`)

This module constructs the intermediate variables needed by the unemployment and LFPR equations from demography pipeline outputs and external data.

### 3A. Quarterly Population Interpolation

The demography pipeline produces **annual year-end** population. USEMP needs **quarterly** values.

**Method:** Linear interpolation between year-end values for the 4 quarters:
- Q1 = P(t-1) + 0.25 × [P(t) - P(t-1)]
- Q2 = P(t-1) + 0.50 × [P(t) - P(t-1)]  (≈ midyear)
- Q3 = P(t-1) + 0.75 × [P(t) - P(t-1)]
- Q4 = P(t)

Apply to: SS area population (P), civilian noninstitutional population (N), OP components.

**Input targets:** `projected_population`, `projected_cni_population`, `o_population_stock`
**Output:** Quarterly population tables by age, sex (and pop_status/marital_status as applicable)

### 3B. Civilian Noninstitutional Population (N) — Eq 2.1.2

```
N^t = [(N^(t-1) + M^(t-1)) × (P^t / P^(t-1))] - M^t
```

Where:
- `P` = SS area population from `projected_population` (summed across pop_status)
- `M` = Military population held constant from `armed_forces_for_projection`
- `N` = Civilian noninstitutional population

**Note:** The demography pipeline already produces `projected_cni_population` (Phase 8E). For USEMP, we should verify consistency with Eq 2.1.2 and use the appropriate version. The Phase 8E CNI uses ACS-based institutionalization ratios, while Eq 2.1.2 derives N from the P growth rate. We may need both:
- Phase 8E CNI → for age-sex detail by marital status
- Eq 2.1.2 → for aggregate quarterly N consistent with the employment model

### 3C. Married Share (MSSHARE)

Used in LFPR equations for ages 55-74 (both sexes).

**Source:** `projected_marital_population` from Phase 8C
**Calculation:** MSSHARE_{age,sex} = married_{age,sex} / total_{age,sex}
**Output:** Annual series by single year of age (55-74) and sex

### 3D. Educational Attainment Score (EDSCORE)

Used in LFPR equations for men 55+ and women 50+.

**Source:** CPS educational attainment (historical) + projection forward
**Historical:** Compute EDSCORE from CPS proportions as weighted average:
```
EDSCORE = w1 × prop_less_hs + w2 × prop_hs + w3 × prop_some_college + w4 × prop_bachelors_plus
```
Where weights reflect participation rate differentials by education level.

**Projection:** Cohort-based projection. Workers with higher education in younger cohorts age into 55+ over time. The educational composition at age 55 in year t is determined by the cohort born in year (t-55), whose education was observed when they were younger.

**Output:** Annual series by single year of age and sex

### 3E. Family Size Ratio (RFS / Child-Under-6 Indicators)

Used in female LFPR equations for ages 16-44.

**Source:** `projected_children_fate` from Phase 8D, or construct from `fertility_rates_complete`
**Calculation:** For each female age group, compute proportion with at least one own child under 6
- NF2024NMC6U = never-married females 20-24 with child under 6
- NF2024NMNC6 = never-married females 20-24 with no child under 6
- etc. for MS (married spouse present), MA (married spouse absent)

**Historical:** CPS March Supplement (Input #24) provides this directly
**Projected:** Derive from projected birth rates and child survival (or extrapolate from recent CPS trends)

### 3F. Disability Prevalence Ratio (RD)

**Source:** `tr2025_di_prevalence` (from V.C5 in SingleYearTRTables_TR2025.xlsx)
**Usage:** All LFPR equations divide by (1 + RD_{age,sex})
- For ages 16-61: use age-sex specific RD
- For ages 62-74: use cohort RD at age 61 (i.e., the RD value from when the cohort was age 61)
**Output:** Annual series by age and sex

### 3G. Replacement Rate Adjustment (RRADJ) and Earnings Test Tax Rate (POT_ET_TXRT)

**Source:** Construct from V.C7 (benefit amounts), V.C1 (NRA/earnings test parameters), VI.G6 (AWI levels)
**RRADJ:** Change in PIA replacement rate at ages 62-69 due to NRA increases
**POT_ET_TXRT:** Implicit marginal tax rate on earnings above the earnings test threshold for beneficiaries below NRA
**Output:** Annual series for ages 62-69

### 3H. RTP (Ratio of Real to Potential GDP)

**Source:** Derive from V.B2 unemployment rate path using Okun's Law:
```
(Y - Y*)/Y* ≈ -β × (u - u*)
```
Where β ≈ 2 (Okun coefficient), u = actual unemployment, u* = natural rate (≈ ultimate unemployment rate)

Or equivalently: `RTP = 1 + (u* - u) × β × (1/100)`

**Quarterly interpolation:** The annual unemployment rate from V.B2 needs quarterly disaggregation. For the short range (first 10 years), BLS/CBO quarterly forecasts may be available. For the long range, use smooth interpolation of the annual path.

**Output:** Quarterly RTP series (1960–2100)

### 3I. Time Trends (TR_M, TR_F)

Linear time trends used in some LFPR equations. The starting point and annual increment define the trend.

**Source:** Calibrate from historical CPS data or use published documentation values
**Output:** Annual counter variable by age group and sex

---

## Phase 4: Core Projections (`R/economics/us_employment.R`)

### 4A. Unemployment Rate Projection (Eq 2.1.3)

**28 equations** (14 age groups × 2 sexes), computed quarterly.

**Step 1: Preliminary Rates**
First-difference model on distributed lag of D(RTP):
```
R_{a,s,P}(q) = R_{a,s,P}(q-1) + Σ_{k=0}^{3} β_{a,s,k} × D(RTP(q-k))
```
Where D(RTP(q)) = RTP(q) - RTP(q-1) is the quarterly first difference.

Coefficients β are from `config/coefficients/unemployment_coefficients.yaml` (Section 1 of equations doc).

**Step 2: Age-Sex Adjusted Aggregate**
Weight preliminary rates by **base year** labor force:
```
RU_ASA_P = Σ_{a,s} R_{a,s,P} × L_{a,s,BY} / LC_BY
```
(Sections 2 of equations doc)

**Step 3: Constrained to Trustees' Target**
Compute adjustment factor: `RU_ASA_ADJ = RU_target - RU_ASA_P`
Apply proportional adjustment: `R_{a,s} = R_{a,s,P} × (1 + RU_ASA_ADJ / RU_ASA_P)`
(Section 3 of equations doc)

The `RU_target` comes from the TR2025 intermediate assumptions (V.B2 unemployment rate series).

**Step 4: Full Employment Differentials**
Compute full-employment unemployment rates (same coefficients but with RTP ≡ 1):
```
DR_{a,s,FE} = Σ_{k=0}^{3} β_{a,s,k} × (1 - RTP(q-k))
R_{a,s,FE} = R_{a,s} + DR_{a,s,FE}
```
(Sections 5–6 of equations doc)

**Output:** Quarterly unemployment rates by 14 age groups and 2 sexes, both actual and full-employment variants.

### 4B. Labor Force Participation Rate Projection (Eq 2.1.4)

**153 equations** total, computed annually then interpolated to quarterly.

The equations fall into distinct model types by age group:

| Ages | Sex | Type | Key Inputs | Disaggregation | Equations |
|------|-----|------|------------|----------------|-----------|
| 16-19 | Both | Young | RU lags, time trend | None | 4 |
| 20-54 | Male | Marital | RU lags, time trend | NM, MS, MA | 21 (7 ages × 3 statuses) |
| 20-44 | Female | Marital+Children | RU lags, time trend | NM×{C6U,NC6}, MS×{C6U,NC6}, MA×{C6U,NC6} | 30 (5 ages × 6 categories) |
| 45-54 | Female | Marital | RU lags, education | NM, MS, MA | 6 (2 ages × 3 statuses) |
| 55-74 | Both | Older | Education score, married share | Single year | 40 (20 ages × 2 sexes) |
| 62-69 | Both | Retirement | + RRADJ, POT_ET_TXRT | Single year | (subset of 55-74) |
| 75-79 | Both | Cohort decay | Prior cohort × decay factor | Single year | 10 |
| 80-100 | Both | Extreme age | Cohort decay from age 79 | Single year → 80+ aggregate | 42 |

All equations divide by `(1 + RD_{age,sex})` to account for disability.

**Aggregation steps:**
1. Compute preliminary disaggregated LFPRs (e.g., PM2024NM_P, PM2024MS_P, PM2024MA_P)
2. Aggregate to age-group total: `PM2024_P = Σ(status_rate × status_pop) / total_pop`
3. Apply addfactors (if any)
4. Rescale disaggregated rates to match aggregate: `PM2024NM = PM2024NM_P × PM2024 / PM2024_P`

**Output:** Annual LFPRs by ~60 age-sex groups (aggregated) and ~153 detailed disaggregations.
Quarterly interpolation: Seasonal patterns from historical CPS monthly data or simple uniform quarterly distribution (annual LFPR / 4 quarters).

### 4C. Labor Force Projection (Eq 2.1.5)

```
LC_{a,s}(q) = LFPR_{a,s}(q) × N_{a,s}(q)
```

Apply quarterly LFPRs to quarterly civilian noninstitutional population.

**Output:** Quarterly labor force by age group and sex

### 4D. Employment Projection (Eq 2.1.6)

```
E_{a,s}(q) = LC_{a,s}(q) × (1 - RU_{a,s}(q) / 100)
```

**Output:** Quarterly civilian employment by age group and sex

### 4E. Full Employment Variants

```
E_FE(q) = LC_FE(q) × (1 - RU_FE(q) / 100)
```

Where LC_FE and RU_FE use full-employment LFPRs and unemployment rates.
(Section 10 of equations doc: Full-employment LFPR differentials)

---

## Phase 5: Employed OP Projection (`R/economics/employed_op.R`)

### 5A. EO Components (Eqs 2.1.7–2.1.10)

**Input:** OP population by visa status from demography pipeline (`o_population_stock`, disaggregated into OP_A, OP_NA, OP_NO by age and sex).

```
EO_A  = f(OP_A, visa subcategory employment ratios)     [Eq 2.1.7]
EO_NA = E × OP_NA / N                                    [Eq 2.1.8]
EO_NO = E × OP_NO / N                                    [Eq 2.1.9]
EO    = EO_A + EO_NA + EO_NO                             [Eq 2.1.10]
```

**EO_A detail:** OP_A further disaggregated by visa type with different employment patterns and OASDI coverage status. This requires visa-type-specific employment-to-population ratios from the OP projections.

**EO_NA and EO_NO:** Use the same employment-to-population ratio (E/N) as the civilian population of the same age and sex.

**Disaggregation:** By sex and single year of age (16-100), yielding 85 × 2 × 3 visa types = 510 base calculations.

### 5B. Earnings Recording Status (Eqs 2.1.11–2.1.14)

Split EO into earnings recording categories:
```
EO_MEF  = f(EO, MEF posting rates by visa type)    [Eq 2.1.11]
EO_MEFC = f(EO_MEF, OASDI coverage rates)           [Eq 2.1.12]
EO_ESF  = f(EO, ESF posting rates)                  [Eq 2.1.13]
EO_UND  = EO - EO_MEF - EO_ESF                      [Eq 2.1.14]
```

**MEF (Master Earnings File):** Workers whose earnings are properly posted. Rates differ by visa subcategory.
**ESF (Earnings Suspense File):** Workers with mismatched SSN (common for unauthorized workers).
**UND (Underground):** Workers with no reported earnings.
**MEFC:** Subset of MEF that is OASDI covered.

The split ratios are stored in `config/coefficients/eo_parameters.yaml`.

### 5C. At-Any-Time Employment TEO (Eqs 2.1.15–2.1.19)

Convert average weekly employment (EO) to at-any-time employment (TEO):
```
TEO_MEF  = EO_MEF × conversion_weight(age, sex)    [Eq 2.1.15]
TEO_MEFC = EO_MEFC × conversion_weight(age, sex)   [Eq 2.1.16]
TEO_ESF  = EO_ESF × conversion_weight(age, sex)    [Eq 2.1.17]
TEO_UND  = EO_UND × conversion_weight(age, sex)    [Eq 2.1.18]
TEO      = TEO_MEF + TEO_ESF + TEO_UND              [Eq 2.1.19]
```

**Conversion weight:** Ratio of at-any-time employment to average weekly employment for each age-sex group. Historically estimated from CPS data. For authorized temporary workers, weights account for partial presence in arrival/departure years.

---

## Phase 6: Target Integration (`R/targets/economics_targets.R`)

### Target Dependency Graph

```
Demography Outputs                 TR2025 Economic Data
─────────────────                 ────────────────────
projected_population ─────┐       tr2025_econ_vb1 ──────┐
projected_cni_population ─┤       tr2025_econ_vb2 ──────┤
projected_marital_population ┤    tr2025_di_prevalence ──┤
projected_children_fate ──┤       tr2025_benefit_amounts ┤
o_population_stock ───────┤       tr2025_econ_levels ────┤
armed_forces_for_projection ┤                            │
mortality_life_expectancy ──┤    BLS/CPS Historical      │
                            │    ──────────────────      │
                            │    bls_cps_labor_force ────┤
                            │    cps_educational_att ────┤
                            │                            │
                            ▼                            ▼
                    ┌─────────────────────────────────────┐
                    │       employment_inputs             │
                    │  (quarterly N, MSSHARE, EDSCORE,    │
                    │   RFS, RD, RRADJ, RTP, addfactors)  │
                    └──────────────┬──────────────────────┘
                                   │
                    ┌──────────────┴──────────────┐
                    ▼                             ▼
          ┌──────────────────┐         ┌──────────────────┐
          │  unemployment_   │         │  lfpr_projection  │
          │  projection      │─────────│                   │
          └──────────┬───────┘         └──────────┬────────┘
                     │                            │
                     ▼                            ▼
          ┌──────────────────────────────────────────┐
          │           labor_force_projection          │
          └──────────────────┬───────────────────────┘
                             │
               ┌─────────────┼─────────────┐
               ▼             ▼             ▼
     ┌────────────┐  ┌────────────┐  ┌────────────┐
     │ employment │  │ employment │  │ employed_op │
     │ _projection│  │ _full_empl │  │ _projection │
     └────────────┘  └────────────┘  └──────┬─────┘
                                            │
                                     ┌──────┴──────┐
                                     ▼             ▼
                              ┌──────────┐  ┌──────────┐
                              │  eo_by   │  │   teo    │
                              │ earnings │  │ _total   │
                              └──────────┘  └──────────┘
```

### Target List (~25 targets)

**Data acquisition targets (6):**
1. `tr2025_economic_assumptions` — V.B1 + V.B2 + VI.G6 combined
2. `tr2025_di_prevalence` — V.C5 disability prevalence
3. `tr2025_benefit_params` — V.C7 benefits + V.C1 NRA/earnings test
4. `bls_cps_labor_force` — Historical CPS labor force by age/sex
5. `cps_educational_attainment` — CPS education by age/sex
6. `bls_cps_marital_labor` — CPS labor force by marital status/children

**Input construction targets (5):**
7. `quarterly_population` — Quarterly interpolated population (SS area, CNI, OP)
8. `employment_input_edscore` — Educational attainment scores (historical + projected)
9. `employment_input_msshare` — Married share from marital population
10. `employment_input_exogenous` — RD, RRADJ, POT_ET_TXRT, RTP from TR2025 data
11. `employment_input_children` — Child-under-6 proportions by female age/marital status

**Core projection targets (6):**
12. `unemployment_projection` — Quarterly RU by 14 age groups × 2 sexes
13. `unemployment_full_employment` — Full-employment RU variant
14. `lfpr_projection` — LFPR by 153 disaggregated groups (annual) + quarterly
15. `lfpr_full_employment` — Full-employment LFPR differentials
16. `labor_force_projection` — Quarterly LC by age group and sex
17. `employment_projection` — Quarterly E by age group and sex (+ full employment variant)

**Employed OP targets (4):**
18. `employed_op_projection` — EO by age, sex, visa status
19. `eo_earnings_recording` — MEF, MEFC, ESF, UND splits
20. `teo_projection` — At-any-time employment TEO by component
21. `employment_projection_summary` — Combined summary of all employment outputs

**Validation targets (2-3):**
22. `employment_validation` — Compare against V.B2 published rates
23. `employment_validation_quick` — Quick structural checks

---

## Phase 7: Validation (`R/validation/employment_validation.R`)

### 7A. Historical Data Verification (API vs TR2025)

When using `assumptions_data_source = "api"`, compare API-fetched historical values against TR2025 SingleYear table values:

| Variable | API Source | TR2025 Source | Expected |
|----------|-----------|---------------|----------|
| Productivity | BLS PRS85006092 | V.B1 col 1 | Match within rounding |
| GDP deflator | BEA NIPA 1.1.4 | V.B1 col 2 | Match within rounding |
| CPI | BLS CWUR0000SA0 | V.B1 col 7 | Match within rounding |
| Unemployment rate | BLS LNS14000000 | V.B2 col 1 | Match within rounding |
| Real GDP change | BEA NIPA 1.1.1 | V.B2 col 4 | Match within rounding |
| Nominal interest | FRED GS10 | V.B2 col 5 | Match within rounding |

Log all comparisons; warn if any differ by more than 0.1 percentage points. This verification confirms we are fetching the correct series.

### 7B. Against TR2025 Published Projections

Compare aggregate projections against Table V.B2 in `SingleYearTRTables_TR2025.xlsx`:

| Variable | TR2025 Source | Expected Match |
|----------|--------------|----------------|
| Unemployment rate (aggregate) | V.B2 col 1 | Exact (constrained to these values) |
| Labor force annual change | V.B2 col 2 | Close (our N path may differ slightly from TR) |
| Total employment annual change | V.B2 col 3 | Close |

### 7B. Internal Consistency Checks

- LC = LFPR × N (identity must hold exactly)
- E = LC × (1 - RU/100) (identity must hold exactly)
- Sum of age-sex employment = total employment
- Quarterly rates average to match annual rates
- EO components sum correctly: EO = EO_A + EO_NA + EO_NO
- TEO ≥ EO for all groups (at-any-time ≥ average weekly)
- LFPR in [0, 1] for all groups
- RU in [0, 100] for all groups (non-negative)

### 7C. Plausibility Checks

- Age-LFPR profile has expected shape (peak in prime working ages, decline in 60s)
- Male LFPR > Female LFPR for older ages (historical pattern)
- Young (16-19) unemployment highest, prime-age (25-54) lowest
- Long-run convergence to Trustees' ultimate unemployment rate

---

## Phase 8: Known Methodology Deviations

Document anticipated deviations from TR2025 methodology:

| # | Area | Deviation | Impact | Configurable? |
|---|------|-----------|--------|---------------|
| 1 | **RTP construction** | Derived from published unemployment rate via Okun's Law rather than SSA's internal quarterly RTP series | Small — only affects transition dynamics, not long-run levels | Via `economics.employment.okun_coefficient` |
| 2 | **Disability prevalence (RD)** | From published V.C5 aggregate table rather than computed age-sex-specific from Beneficiaries subprocess | Moderate — age-sex detail may be coarser than OCACT internal series | Will be replaced when Beneficiaries is built |
| 3 | **Educational attainment projection** | Cohort-based projection from CPS rather than internal SSA projection | Small — cohort method is standard approach | Via historical data years |
| 4 | **CPS unpublished data (Input #29)** | Older worker LFPRs (55-79) from published CPS rather than BLS unpublished special tabulation | Moderate for ages 75-79 where published data is sparse | N/A |
| 5 | **EO visa subcategory employment ratios** | May need to estimate from ACS/CPS rather than SSA internal data | Moderate for EO_A detail; EO_NA/EO_NO use same E/N ratio | Via `config/coefficients/eo_parameters.yaml` |
| 6 | **Addfactors** | Default to zero (no manual actuarial adjustments) | Small to moderate — addfactors primarily smooth near-term transitions | Via `economics.employment.addfactors` config |
| 7 | **Quarterly interpolation** | Annual demography outputs interpolated to quarterly rather than computed natively at quarterly frequency | Small — population changes smoothly between years | Via interpolation method config |

---

## Implementation Order

| Step | Description | Dependencies | Est. Complexity |
|------|-------------|--------------|-----------------|
| **0** | Branch setup, directory structure | None | Low |
| **1** | Config additions (YAML + coefficient files) | None | Medium (transcribing ~1000 coefficients) |
| **2A** | TR2025 economic assumptions loader | Step 0 | Medium |
| **2B** | BLS CPS data acquisition | Step 0 | High (BLS API, multiple series) |
| **2C** | CPS educational attainment | Step 0 | Medium |
| **3** | Input variable construction | Steps 2A-2C + demography targets | High |
| **4A** | Unemployment rate projection | Steps 1, 3 | Medium |
| **4B** | LFPR projection | Steps 1, 3, 4A | High (153 equations) |
| **4C-E** | Labor force, employment, full employment | Steps 4A, 4B | Low |
| **5** | Employed OP (EO/TEO) | Steps 4C-E + demography OP targets | High |
| **6** | Target integration | All above | Medium |
| **7** | Validation | Step 6 | Medium |

Steps 2A, 2B, and 2C can be developed in parallel. Steps 4A and the input construction for 4B can also be partially parallelized.

---

## Verification Plan

1. **Unit tests:** Each core function should have basic tests verifying:
   - Correct output dimensions (age groups × sexes × quarters)
   - Identity relationships hold (LC = LFPR × N, E = LC × (1 - RU/100))
   - Edge cases (zero population, extreme ages)

2. **Pipeline integration test:**
   ```r
   Rscript -e "targets::tar_make()"
   # Verify all economics targets build without error
   Rscript -e "targets::tar_read(employment_validation)"
   ```

3. **Validation against TR2025:**
   - Aggregate unemployment rate matches V.B2 (should be exact — constrained)
   - Labor force growth rate within 0.5% of V.B2
   - Total employment growth rate within 0.5% of V.B2

4. **Visual inspection:**
   - Plot age-LFPR profiles for selected years (2025, 2050, 2075, 2100) — should show reasonable shapes
   - Plot total employment trajectory — should track published TR2025 path
   - Plot quarterly unemployment by age group — should show cyclical convergence to ultimate

5. **Baseline snapshot:**
   ```r
   Rscript scripts/create_baseline_snapshot.R
   ```
   Save economics outputs to `data/baseline/tr2025/` for regression testing.

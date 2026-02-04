# US Employment (USEMP) Subprocess Implementation Plan

## Overview

This plan implements the US Employment subprocess (Section 2.1) of the ARTEMIS OASDI projection model Economics process. The subprocess projects labor force, employment, and unemployment by age, sex, marital status, and population type from 2023-2099.

## Key Equations Summary

| Equation | Description | Key Variables |
|----------|-------------|---------------|
| 2.1.1 | Military population | M^t (constant at 2021 level) |
| 2.1.2 | Civilian noninstitutional population | N^t by age/sex/marital |
| 2.1.3 | Unemployment rate | RU^t (28 equations, distributed lag) |
| 2.1.4 | Labor force participation rates | LFPR^t (153 equations by demographic) |
| 2.1.5 | Labor force | LC^t = LFPR × N |
| 2.1.6 | Employment | E^t = LC × (1 - RU) |
| 2.1.7-2.1.14 | Employed O population | EO^t, TEO^t, and detailed breakdowns |
| 2.1.15-2.1.19 | At-any-time employment | TE^t, SEE, self-employment |

## Implementation Phases

---

### Phase 9A: Data Acquisition and Infrastructure

**Objective:** Create data acquisition functions and add economics configuration

#### Tasks

- [ ] **9A.1** Create `R/data_acquisition/bls_employment.R`
  - `fetch_bls_unemployment_rates()` - Historical unemployment rates from BLS
  - `fetch_bls_labor_force()` - Labor force statistics
  - `fetch_bls_employment()` - Employment levels
  - Use BLS API v2 with API key from `.Renviron`

- [ ] **9A.2** Create `R/data_acquisition/cps_lfpr.R`
  - `fetch_cps_lfpr()` - Labor force participation rates by age/sex
  - `fetch_cps_marital_employment()` - Employment by marital status
  - Parse CPS ASEC microdata or published tables

- [ ] **9A.3** Create `R/data_acquisition/military_population.R`
  - `fetch_military_population()` - DoD military personnel data
  - Get 2021 baseline for constant projection

- [ ] **9A.4** Add economics section to `config/assumptions/tr2025.yaml`
  ```yaml
  economics:
    us_employment:
      base_year: 2022
      projection_start_year: 2023
      projection_end_year: 2099
      military_population_year: 2021  # Held constant
      ultimate_unemployment_rate: 0.055  # 5.5%
      ultimate_year: 2034  # Year to reach ultimate RU
      # RU distributed lag parameters
      ru_coefficients:
        rtp_lag_weights: [0.25, 0.20, 0.15, 0.10, ...]
      # LFPR configuration
      lfpr:
        age_groups: [16-17, 18-19, 20-24, ..., 70-74, 75+]
        marital_categories: [single, married, widowed, divorced]
  ```

- [ ] **9A.5** Create `R/economics/` directory structure
  - Mirror pattern from `R/demography/`

#### Outputs
- Data acquisition functions for BLS, CPS, military data
- Economics configuration in tr2025.yaml
- Directory structure for economics code

---

### Phase 9B: Core Population Framework (Eq 2.1.1-2.1.2)

**Objective:** Implement military and civilian noninstitutional population calculations

#### Tasks

- [ ] **9B.1** Create `R/economics/military_population.R`
  - `calculate_military_population()` - Eq 2.1.1
  - Hold military population constant at 2021 level
  - Split by age and sex using historical distribution
  ```r
  # M^t_{x,s} = M^{2021}_{x,s} for all t >= 2023
  ```

- [ ] **9B.2** Create `R/economics/civilian_population.R`
  - `calculate_civilian_noninst_population()` - Eq 2.1.2
  - Formula: N^t = [(N^{t-1} + M^{t-1}) × (P^t / P^{t-1})] - M^t
  - Uses projected population from demography subprocess
  - Disaggregate by age, sex, marital status
  ```r
  calculate_civilian_noninst_population <- function(
    projected_population,     # From Phase 8
    military_population,
    base_year = 2022
  ) {
    # Implementation
  }
  ```

- [ ] **9B.3** Add validation functions
  - `validate_military_constant()` - Verify M constant across years
  - `validate_cni_population()` - Cross-check with demography CNI output

#### Outputs
- Military population by age/sex (constant 2021 level)
- Civilian noninstitutional population projection (2023-2099)
- Validation checks

---

### Phase 9C: Unemployment Rate Projection (Eq 2.1.3)

**Objective:** Implement unemployment rate projection using distributed lag model

#### Key Methodology (from Appendix 2-1)
- First-difference model: `RU_P(t) = RU_P(t-1) + Σ[β_i * D(RTP(t-i))]` for i=0..3
- D(RTP) = change in real taxable payroll
- 28 preliminary equations (14 male + 14 female age groups)
- Age-sex adjustment: `RU = RU_P * (1 + RU_ASA_ADJ / RU_ASA_P)`
- Full employment differentials: `RU_FE = RU + DRU_FE`

#### Exact Coefficient Structure (from PDF)
```
# Example Male 16-17:
RM1617_P = RM1617_P(-1) + [-31.52172*D(RTP) - 24.81520*D(RTP(-1))
                           - 14.72058*D(RTP(-2)) - 31.33966*D(RTP(-3))]
# 14 male age groups: 16-17, 18-19, 20-24, 25-29, 30-34, 35-39, 40-44,
#                     45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75+
# 14 female age groups: same breakdown
```

#### Tasks

- [ ] **9C.1** Create `R/economics/unemployment_rate.R`
  - `calculate_ru_preliminary()` - Preliminary RU using distributed lag
  - `calculate_ru_age_sex_adjusted()` - Apply ASA adjustment
  - `calculate_ru_full_employment()` - Full employment differentials
  - `get_ru_coefficients()` - Load all 28 × 4 = 112 coefficients

- [ ] **9C.2** Store 28 RU equation coefficients
  ```r
  # Data structure for coefficients
  ru_coefficients <- data.table(
    age_group = c("1617", "1819", "2024", "2529", "3034", "3539",
                  "4044", "4549", "5054", "5559", "6064", "6569", "7074", "75O"),
    sex = rep(c("male", "female"), each = 14),
    beta_0 = c(-31.52172, -56.74492, ...),  # D(RTP) coefficient
    beta_1 = c(-24.81520, -24.87344, ...),  # D(RTP(-1)) coefficient
    beta_2 = c(-14.72058, -35.84160, ...),  # D(RTP(-2)) coefficient
    beta_3 = c(-31.33966, -0.70217, ...)    # D(RTP(-3)) coefficient
  )
  ```

- [ ] **9C.3** Implement age-sex adjusted aggregation
  ```r
  # RUM_ASA_P = Σ(RM_age_P × LM_age_BY) / LCM_BY
  # RUF_ASA_P = Σ(RF_age_P × LF_age_BY) / LCF_BY
  # RU_ASA_P = (RUM_ASA_P × LCM_BY + RUF_ASA_P × LCF_BY) / LC_BY
  # Final: RU = RU_P × (1 + RU_ASA_ADJ / RU_ASA_P)
  ```

- [ ] **9C.4** Implement full employment differentials
  ```r
  # DRM_FE = β_0×(1-RTP) + β_1×(1-RTP(-1)) + β_2×(1-RTP(-2)) + β_3×(1-RTP(-3))
  # RM_FE = RM + DRM_FE
  ```

- [ ] **9C.5** Handle circular dependency with RTP
  - RU depends on RTP (from Taxable Payroll subprocess)
  - Initial implementation: use assumed RTP trajectory
  - Document placeholder for iteration

- [ ] **9C.6** Create validation
  - `validate_unemployment_rate()` - Check convergence, bounds

#### Outputs
- Preliminary RU by age/sex (RU_P)
- Age-sex adjusted RU
- Full employment RU (RU_FE)
- All 112 coefficients stored in configuration

---

### Phase 9D: Labor Force Participation Rates (Eq 2.1.4)

**Objective:** Implement LFPR projection with 153 demographic equations

#### Key Methodology (from Appendix 2-1)
The LFPR equations vary by age group with different functional forms:

**Ages 16-54:** Unemployment rate lag model with 6 lags
```
PM_P = [ Σ(β_i × RM(t-i)) + trend_term + intercept ] / (1 + RM_DI)
# β_i for i=0..5, plus optional TR_* trend and disability adjustment
```

**Ages 55-74:** Education and marital share model
```
PM_P = [ α×EDSCORE + β×MSSHARE + γ×RRADJ + δ×POT_ET_TXRT + intercept ] / (1 + RM_DI)
# Ages 62-69 include retirement rate adjustment (RRADJ) and earnings test (POT_ET_TXRT)
```

**Ages 75-79:** Decay model (0.92 factor)
```
PM75_P = PM74(-4) × 0.92
PM76_P = PM75(-4) × 0.92
...
```

**Ages 80+:** Cohort tracking with 0.965 decay
```
PM80_P = PM79(-4) × 0.965^1
PM85_P = MOVAVG(8, PM79(-24)) × 0.965^6
PM95_P = PM94_P × 0.965
```

**Women 20-44 with children dimension:**
- C6U = at least one child under 6
- NC6 = no children under 6
- NM = never married, MS = married spouse present, MA = married spouse absent

#### Tasks

- [ ] **9D.1** Create `R/economics/lfpr.R`
  - `calculate_lfpr_young()` - Ages 16-54 with RU lags
  - `calculate_lfpr_older()` - Ages 55-74 with EDSCORE/MSSHARE
  - `calculate_lfpr_elderly()` - Ages 75+ with decay factors
  - `calculate_lfpr_female_children()` - Women 20-44 by child status

- [ ] **9D.2** Store equation coefficients by model type
  ```r
  # Type 1: RU lag model (ages 16-54)
  lfpr_ru_model <- data.table(
    age_group, sex, marital, children,
    ru_beta_0, ru_beta_1, ru_beta_2, ru_beta_3, ru_beta_4, ru_beta_5,
    trend_coef, intercept
  )

  # Type 2: Education/marital model (ages 55-74)
  lfpr_edscore_model <- data.table(
    age, sex,
    edscore_coef, msshare_coef, rradj_coef, pot_et_coef, intercept
  )
  ```

- [ ] **9D.3** Implement male LFPR equations
  - Ages 16-17, 18-19: Simple RU model
  - Ages 20-54 by marital: NM (never married), MS (married present), MA (married absent)
  - Ages 55-74: Single-year ages with EDSCORE, MSSHARE
  - Ages 75-79: Decay 0.92
  - Ages 80-100: Decay 0.965 with MOVAVG smoothing

- [ ] **9D.4** Implement female LFPR equations
  - Ages 16-17, 18-19: Include RF*CU6 (child under 6 ratio)
  - Ages 20-44 by marital × children: 30 equations
    - NMC6U, NMNC6, MSC6U, MSNC6, MAC6U, MANC6 for each 5-year group
  - Ages 45-54 by marital: NM, MS, MA (no children dimension)
  - Ages 55-74: EDSCORE, MSSHARE model
  - Ages 75-100: Same decay structure as males

- [ ] **9D.5** Implement aggregation formulas
  ```r
  # PM2024_P = (PM2024NM_P × NM2024NM + PM2024MS_P × NM2024MS
  #           + PM2024MA_P × NM2024MA) / NM2024
  # PM16O_P = Σ(PM_age_P × NM_age) / NM16O
  # P16O_P = (PM16O_P × NM16O + PF16O_P × NF16O) / (NM16O + NF16O)
  ```

- [ ] **9D.6** Implement input variable projections
  - EDSCORE: Education score by age/sex (ages 55+ men, 50+ women)
  - MSSHARE: Married share by age/sex (ages 55-74)
  - RRADJ: Retirement rate adjustment (ages 62-69)
  - POT_ET_TXRT: Potential earnings test tax rate (ages 62-69)

- [ ] **9D.7** Implement disability prevalence adjustment (per Section 2.1.c)
  ```r
  # All LFPRs adjusted: LFPR_adj = LFPR / (1 + RD)
  # RD = disabled worker beneficiaries / disability-insured population
  # Ages 62-74: use cohort RD at age 61 (avoid retirement benefit confounding)
  # Ages 75+: lagged cohort RD for disability influence
  ```

- [ ] **9D.8** Implement NRA effects on ages 62-69
  - Replacement rate: PIA / career-average wage, adjusted for early retirement reduction
  - Delayed retirement credit adjustment
  - POT_ET_TXRT: Tax rate on benefits for pre-NRA workers who continue earning

- [ ] **9D.9** Implement life expectancy effects (ages 40+)
  - Addfactors applied to reflect projected life expectancy changes
  - Higher life expectancy → higher LFPR

- [ ] **9D.10** Create validation
  - `validate_lfpr()` - Check bounds (0-1), age patterns

#### Outputs
- LFPR by age/sex/marital/children (2023-2099)
- 153 equations total (69 men + 84 women per Section 2.1.c)
- Aggregate LFPR (PM16O, PF16O, P16O)

---

### Phase 9E: Labor Force and Employment (Eq 2.1.5-2.1.6)

**Objective:** Calculate labor force and employment from population, LFPR, and RU

#### Tasks

- [ ] **9E.1** Create `R/economics/labor_force.R`
  - `calculate_labor_force()` - Eq 2.1.5: LC = LFPR × N
  - By age, sex, marital status

- [ ] **9E.2** Create `R/economics/employment.R`
  - `calculate_employment()` - Eq 2.1.6: E = LC × (1 - RU)
  - By age, sex, marital status
  ```r
  calculate_employment <- function(
    labor_force,         # LC^t_{x,s,m}
    unemployment_rate    # RU^t_{x,s}
  ) {
    # E^t_{x,s,m} = LC^t_{x,s,m} × (1 - RU^t_{x,s})
  }
  ```

- [ ] **9E.3** Create summary tables
  - Total labor force by year
  - Total employment by year
  - Employment-to-population ratio

- [ ] **9E.4** Create validation
  - `validate_employment()` - Check E < LC < N

#### Outputs
- Labor force by age/sex/marital (2023-2099)
- Employment by age/sex/marital (2023-2099)
- Summary statistics

---

### Phase 9F: Employed O Population (Eq 2.1.7-2.1.14)

**Objective:** Calculate employment for temporary/unlawfully present (O) population

#### Key Methodology (from Section 2.1.c)
- EO estimated by sex and single-year age (16-100) based on OP and employment-to-population ratios
- OP disaggregated by visa status: OP_A (authorized), OP_NA (overstayed), OP_NO (never authorized)
- OP_NA and OP_NO assumed to have same E/P ratio as LPR population of same age/sex
- **4,250 equations** total (85 ages × 2 sexes × 25 components/subgroups)

#### Exact Equations (from Appendix)
```
EO_A   = EO_A(·)                    (2.1.7)  # Authorized, by visa type subgroups
EO_NA  = E × OP_NA / N              (2.1.8)  # Overstayed authorization
EO_NO  = E × OP_NO / N              (2.1.9)  # Never authorized
EO     = EO_A + EO_NA + EO_NO       (2.1.10) # Total employed O
EO_MEF  = EO_MEF(·)                 (2.1.11) # Posted to Master Earnings File
EO_MEFC = EO_MEFC(·)                (2.1.12) # OASDI covered subset of MEF
EO_ESF  = EO_ESF(·)                 (2.1.13) # Posted to Earnings Suspense File
EO_UND  = EO - EO_MEF - EO_ESF      (2.1.14) # Underground economy
```

#### Tasks

- [ ] **9F.1** Create `R/economics/o_employment.R`
  - `calculate_eo_authorized()` - Eq 2.1.7: EO_A by visa type subgroups
  - `calculate_eo_overstayed()` - Eq 2.1.8: EO_NA = E × OP_NA / N
  - `calculate_eo_never_authorized()` - Eq 2.1.9: EO_NO = E × OP_NO / N
  - `calculate_eo_total()` - Eq 2.1.10: Sum of components

- [ ] **9F.2** Implement earnings file breakdowns (Eq 2.1.11-2.1.14)
  - `calculate_eo_mef()` - Posted to Master Earnings File
  - `calculate_eo_mefc()` - OASDI covered portion
  - `calculate_eo_esf()` - Posted to Earnings Suspense File
  - `calculate_eo_und()` - Underground economy (residual)

- [ ] **9F.3** Handle OP_NO pre-2002 vs post-2002 split
  - Those working 2001 and earlier more likely to have OASDI covered wages
  - Those working 2002 and later treated differently

- [ ] **9F.4** Link to O population from Phase 5
  - Use `o_immigration_projection` target (OP_A, OP_NA, OP_NO)
  - Apply employment rates by visa status component

- [ ] **9F.5** Create validation
  - EO ≤ O population by component
  - EO_UND = EO - EO_MEF - EO_ESF

#### Outputs
- EO by age/sex/visa status (16-100 × 2 × 3+ components)
- EO_MEF, EO_MEFC, EO_ESF, EO_UND breakdowns
- 4,250+ equations total

---

### Phase 9G: At-Any-Time Employed O Population (Eq 2.1.15-2.1.19)

**Objective:** Convert point-in-time EO to at-any-time TEO

#### Key Methodology (from Section 2.1.c)
- EO = average weekly employment of O population during calendar year
- TEO = total individuals in O population who had **any** employment during calendar year
- EO ≈ average jobs worked; TEO ≈ total individuals who worked those jobs
- Conversion uses age-sex weights: TEO = EO × (TE_total / E_total) for each age-sex group
- Authorized workers/students: weights account for partial presence in arrival/departure years

#### Exact Equations (from Appendix)
```
TEO_MEF  = TEO_MEF(·)                     (2.1.15) # At-any-time MEF
TEO_MEFC = TEO_MEFC(·)                    (2.1.16) # At-any-time OASDI covered
TEO_ESF  = TEO_ESF(·)                     (2.1.17) # At-any-time Suspense File
TEO_UND  = TEO_UND(·)                     (2.1.18) # At-any-time Underground
TEO      = TEO_MEF + TEO_ESF + TEO_UND    (2.1.19) # Total at-any-time EO
```

#### Tasks

- [ ] **9G.1** Create `R/economics/at_any_time_employment.R`
  - `calculate_teo_conversion_weights()` - Age-sex ratio of total economy at-any-time to point-in-time
  - `calculate_teo_mef()` - Eq 2.1.15: At-any-time MEF
  - `calculate_teo_mefc()` - Eq 2.1.16: At-any-time OASDI covered

- [ ] **9G.2** Implement remaining TEO components
  - `calculate_teo_esf()` - Eq 2.1.17: At-any-time Suspense File
  - `calculate_teo_und()` - Eq 2.1.18: At-any-time Underground
  - `calculate_teo_total()` - Eq 2.1.19: Sum of components

- [ ] **9G.3** Handle partial-year presence for authorized workers
  - Adjust conversion weights for year of arrival
  - Adjust conversion weights for year of departure

- [ ] **9G.4** Create summary tables
  - TEO by component and year
  - Ratio of TEO to EO by age-sex group

- [ ] **9G.5** Create validation
  - TEO ≥ EO for all components
  - TEO = TEO_MEF + TEO_ESF + TEO_UND

#### Outputs
- TEO by age/sex/component
- Conversion weights by age-sex
- Summary comparing point-in-time vs at-any-time

---

### Phase 9H: Targets Pipeline Integration

**Objective:** Integrate all components into targets pipeline

#### Tasks

- [ ] **9H.1** Create `R/targets/economics_targets.R`
  ```r
  create_economics_targets <- function() {
    list(
      # Data acquisition
      tar_target(bls_unemployment, fetch_bls_unemployment_rates()),
      tar_target(cps_lfpr, fetch_cps_lfpr()),
      tar_target(military_pop_2021, fetch_military_population()),

      # Core population
      tar_target(military_projection, calculate_military_population(military_pop_2021)),
      tar_target(cni_projection_econ, calculate_civilian_noninst_population(
        population_projection, military_projection
      )),

      # Unemployment rate
      tar_target(ru_projection, project_unemployment_rate(
        bls_unemployment, config_tr2025
      )),

      # LFPR
      tar_target(lfpr_projection, project_lfpr(cps_lfpr, config_tr2025)),

      # Labor force and employment
      tar_target(labor_force_projection, calculate_labor_force(
        cni_projection_econ, lfpr_projection
      )),
      tar_target(employment_projection, calculate_employment(
        labor_force_projection, ru_projection
      )),

      # O employment
      tar_target(eo_projection, calculate_eo_population(
        o_immigration_projection, employment_rates
      )),

      # Validation
      tar_target(employment_validation, validate_employment(
        employment_projection, labor_force_projection
      ))
    )
  }
  ```

- [ ] **9H.2** Update `_targets.R`
  - Add `tar_source("R/economics")`
  - Add `create_economics_targets()` to pipeline

- [ ] **9H.3** Define target dependencies
  - Link to demography outputs: `population_projection`, `o_immigration_projection`
  - Document circular dependency with RTP (placeholder)

- [ ] **9H.4** Add economics packages to `tar_option_set()`
  - Any additional packages needed

#### Outputs
- Complete targets factory function
- Updated pipeline definition
- Documented dependencies

---

### Phase 9I: Validation and Testing

**Objective:** Comprehensive validation against TR2025

#### Tasks

- [ ] **9I.1** Create `R/validation/validate_employment.R`
  - `validate_employment_comprehensive()` - All checks
  - Compare to TR2025 published tables

- [ ] **9I.2** Create test files
  - `tests/testthat/test-employment.R`
  - Unit tests for each function
  - Integration tests for full pipeline

- [ ] **9I.3** Create validation targets
  ```r
  tar_target(employment_validation, validate_employment_comprehensive(
    employment_projection,
    labor_force_projection,
    ru_projection,
    lfpr_projection
  ))
  ```

- [ ] **9I.4** Document validation results
  - Update CLAUDE.md with employment subprocess status
  - Note any methodology deviations

- [ ] **9I.5** Create summary report
  - Key metrics by year
  - Comparison to TR2025 benchmarks

#### Outputs
- Validation functions
- Test suite
- Documentation updates

---

## Dependencies

### From Demography Process
- `population_projection` - Total population by age/sex/marital (Phase 8)
- `projected_cni_population` - Civilian noninstitutional (Phase 8E)
- `o_immigration_projection` - O population stock (Phase 5)
- `marital_projection` - Population by marital status (Phase 8C)

### To Economics Process (Later)
- Employment outputs feed into:
  - US Earnings subprocess (Section 2.2)
  - Covered Employment & Earnings (Section 2.3)
  - Taxable Payroll (Section 2.4)
  - Revenues (Section 2.5)

### Circular Dependencies
- RU depends on RTP (Real Taxable Payroll from 2.4)
- Initial implementation uses assumed RTP trajectory
- Full model requires iteration between subprocesses

---

## File Structure

```
R/
├── economics/
│   ├── military_population.R      # Eq 2.1.1
│   ├── civilian_population.R      # Eq 2.1.2
│   ├── unemployment_rate.R        # Eq 2.1.3 (28 equations)
│   ├── lfpr.R                     # Eq 2.1.4 (153 equations)
│   ├── labor_force.R              # Eq 2.1.5
│   ├── employment.R               # Eq 2.1.6
│   ├── o_employment.R             # Eq 2.1.7-2.1.14
│   └── at_any_time_employment.R   # Eq 2.1.15-2.1.19
├── data_acquisition/
│   ├── bls_employment.R           # BLS API
│   ├── cps_lfpr.R                 # CPS data
│   └── military_population.R      # DoD data
├── targets/
│   └── economics_targets.R        # Factory function
└── validation/
    └── validate_employment.R      # Validation functions

config/
└── assumptions/
    └── tr2025.yaml                # Add economics section

data/
└── raw/
    └── BLS/                       # BLS downloaded data
```

---

## Estimated Complexity

| Phase | Files | Functions | Equations | Complexity |
|-------|-------|-----------|-----------|------------|
| 9A | 4 | 6 | - | Medium |
| 9B | 2 | 4 | 2 | Low |
| 9C | 1 | 6 | 28 prelim + 28 ASA + 28 FE = 84 | High |
| 9D | 1 | 10 | 153 LFPR (69 men + 84 women) + aggregation | Very High |
| 9E | 2 | 4 | 2 | Low |
| 9F | 1 | 8 | 4,250 (85 ages × 2 sexes × 25 components) | Very High |
| 9G | 1 | 6 | ~4,250 (TEO counterparts) | Very High |
| 9H | 2 | 1 | - | Medium |
| 9I | 2 | 4 | - | Medium |

**Total:** 16 new files, ~49 new functions, ~8,700+ equations (per TR2025 methodology)

### Key Variables Required (from Abbreviations PDF)
- **N**: Civilian noninstitutional population (from Phase 8)
- **M**: Military population
- **RU**: Unemployment rate
- **LFPR/P**: Labor force participation rate
- **LC**: Labor force (civilian)
- **E**: Employment
- **RTP**: Real taxable payroll (circular dependency with Section 2.4)
- **EDSCORE**: Education score by age/sex
- **MSSHARE**: Married share by age/sex
- **RRADJ**: Retirement rate adjustment
- **POT_ET_TXRT**: Potential earnings test tax rate
- **DI**: Disability incidence adjustment

---

## Success Criteria

1. All 19 primary equations (2.1.1-2.1.19) implemented
2. 28 RU equations with correct coefficients
3. 153 LFPR equations with correct coefficients
4. Employment projections within 2% of TR2025 benchmarks
5. All validation checks pass
6. Full pipeline runs without error
7. Documentation updated in CLAUDE.md

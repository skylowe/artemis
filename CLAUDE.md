# ARTEMIS - OASDI Projection Model

## Project Overview
R-based replication of the SSA Office of the Chief Actuary's long-range OASDI projection model. Uses `{targets}` for pipeline orchestration and `{renv}` for dependency management.

## Current Status
**Phase:** 5 - Temporary/Unlawfully Present Immigration Subprocess (COMPLETE)
**Most Recent Completion:** Phase 5H - Targets Integration (January 18, 2026)
**Next Step:** Phase 6 - Population Projection

### Fertility Subprocess Status (COMPLETE)
- All 10 projection methodology steps implemented in `R/demography/fertility.R`
- Data acquisition complete: NCHS births (1980-2024), Census population (1980-2024)
- Validation against TR2025: Historical and projected TFR match within 2% tolerance
- Ultimate TFR correctly reaches configured target (1.90) at ultimate year
- Configuration-driven: ultimate rate, years, and other parameters adjustable in `config/assumptions/tr2025.yaml`

### Mortality Subprocess Status (COMPLETE)
- All projection methodology steps implemented in `R/demography/mortality.R`
- Data acquisition complete: NCHS deaths (1968-2023), Census population (1980-2023)
- Sex-specific births (1968-2024) downloaded for accurate q0 calculation
- HMD calibration implemented for ages 85+ (substitutes for Medicare data we don't have)
- Historical qx validated against TR2025: 95-100% within 5% tolerance for ages 0-64
- Life expectancy validated against TR2025: 100% within 0.5 year tolerance
- Projections extend to 2099 per TR methodology
- Key files: `R/demography/mortality.R`, `R/data_acquisition/nchs_deaths.R`, `R/data_acquisition/nchs_births.R`

### LPR Immigration Subprocess Status (COMPLETE)
- Core projection functions in `R/demography/lpr_immigration.R` and `R/demography/legal_emigration.R`
- Data acquisition: DHS expanded tables (2006-2023), CBO migration data (2021-2099), TR2025 Table V.A2
- Age-sex distributions calculated from CBO 2021-2024 reference years
- Beers interpolation converts 5-year age groups to single years
- LPR immigration matches TR2025 assumptions: 1,213,000 (2025-26), 1,050,000 (2027+)
- Net LPR immigration: 910,000 (2025-26), 788,000 (2027+) from V.A2
- 11 targets integrated into pipeline, all passing validation
- Key files: `R/demography/lpr_immigration.R`, `R/demography/legal_emigration.R`, `R/data_acquisition/dhs_immigration.R`, `R/data_acquisition/cbo_migration.R`, `R/data_acquisition/tr2025_data.R`

**V.A2 Integration (January 18, 2026):**
- `get_tr2025_lpr_assumptions()` now loads from Table V.A2 instead of hardcoded values
- Provides actual emigration values from V.A2 (not estimated 25% rule)
- NEW vs AOS breakdown available: Total LPR = NEW arrivals + Adjustments of Status
- Historical LPR data available (1940-2024) for validation
- Fallback to hardcoded values if V.A2 file unavailable

**Methodology Deviations from TR2025:**
1. Emigration distribution uses CBO 2021-2024 data instead of unpublished Census 1980-1990 estimates
2. Refugee/asylee reclassification not implemented (DHS expanded tables don't separate by refugee status)

### Historical Population Subprocess Status (COMPLETE)
- **Purpose:** Estimate Social Security area population for Dec 31, 1940 through Dec 31, 2022
- **Key Outputs:**
  - P^z_{x,s} - Population by age/sex (Eq 1.4.1): 16,766 rows
  - P^z_{x,s,m} - Population by age/sex/marital status (Eq 1.4.2): 64,728 rows
  - O^z_{x,s} - Temporary/unlawfully present population (Eq 1.4.3): 16,600 rows
  - C^z_{x,s,m} - Civilian noninstitutionalized population (Eq 1.4.4): 17,747 rows
- **Data Sources:** 38/44 inputs implemented from Census, ACS, IPUMS, DHS, OPM, SSA
- **Validation:** Mean absolute error 1.55% vs TR2025 population files
- **Key files:** `R/demography/historical_population.R`, `historical_marital_status.R`, `historical_temp_unlawful.R`, `historical_civilian_noninst.R`
- **Pipeline targets:** `historical_population`, `historical_population_marital`, `historical_temp_unlawful`, `historical_civilian_noninst`

### Temp/Unlawfully Present Immigration Subprocess Status (COMPLETE)
- **Purpose:** Project non-LPR immigration (nonimmigrants, unauthorized, visa-overstayers) for 2025-2099
- **Key Outputs:**
  - OI^z_{x,s,t} - O immigration by age/sex/type (Eq 1.5.1)
  - OE^z_{x,s,t} - O emigration by age/sex/type (Eq 1.5.2)
  - NO^z_{x,s,t} - Net O immigration (Eq 1.5.3)
  - OP^z_{x,s,t} - O population stock (Eq 1.5.4)
  - DACA population by age/sex
- **TR2025 Assumptions:** Ultimate 1,350,000/year starting 2026
- **Completed Phases:** 5A-5H (all complete)
- **Key files:**
  - `R/data_acquisition/dhs_nonimmigrant.R` - Nonimmigrant stock/admissions
  - `R/data_acquisition/dhs_daca.R` - DACA grants and stock
  - `R/data_acquisition/acs_foreign_born.R` - New arrivals, DACA eligibility, undercount
  - `R/demography/temp_unlawful_immigration.R` - ODIST calculation, O immigration projection
  - `R/demography/temp_unlawful_emigration.R` - Departure rates, cohort-tracking emigration
  - `R/demography/temp_unlawful_stock.R` - Net O, population stock, main entry point
  - `R/demography/daca_projection.R` - DACA eligibility, attainment, and population projection
  - `R/validation/validate_o_immigration.R` - Comprehensive O immigration validation
- **Pipeline targets:** `o_immigration_projection`, `daca_projection`, `o_immigration_validation`

**AOS Fix (January 18, 2026):**
- Fixed NEW/AOS split to use TR2025 V.A2 assumption values instead of historical ratio
- Previous bug: Calculated AOS from DHS 2016-2019 ratio (~50.8% AOS) producing 533K-616K AOS
- TR2025 expects: Fixed 450K AOS regardless of total LPR
- Impact: Net O improved from 20-45% error to -2.2% average error
- Configuration option `new_aos_split_method` in `config/assumptions/tr2025.yaml`:
  - `"assumption"` (default): Uses TR2025 V.A2 values (450K AOS)
  - `"ratio"`: Uses historical DHS ratio (for backward compatibility)
- Modified files: `R/demography/lpr_immigration.R`, `_targets.R`, `config/assumptions/tr2025.yaml`

**Phase 5H Implementation (January 18, 2026):**
- All O immigration targets integrated into `_targets.R` pipeline
- Data acquisition targets: `dhs_nonimmigrant_stock`, `dhs_daca_data`, `acs_foreign_born_*`
- Projection targets: `o_immigration_projection` (runs full projection 2025-2099)
- DACA targets: `daca_projection`, `daca_population_projected`
- Validation target: `o_immigration_validation`
- Key results (2025-2099 projection):
  - O Immigration: 2,000,000 (2025) → 1,350,000 (2026+)
  - O Emigration: ~304K (2025) → ~601K (2099)
  - Net O: ~1.25M (2025) → ~299K (2099)
  - O Population: 12.13M (2025) → 23.28M (2099)

**Phase 5G Implementation (January 18, 2026):**
- Comprehensive validation module created with 12 validation functions
- TR2025 assumption validation: OI totals must match exactly
- DHS comparison: O population vs unauthorized estimates (10% tolerance)
- DHS comparison: DACA population vs stock (5% tolerance)
- Equation validation: Net O (1.5.3) and Stock (1.5.4) balance checks
- Distribution validation: ODIST sums to 1.0, all types/ages/sexes present
- 7/7 unit tests passed

**Phase 5E Implementation (January 18, 2026):**
- All TR2025 equations implemented: 1.5.1 (OI), 1.5.2 (OE), 1.5.3 (NO), 1.5.4 (OP)
- `run_full_o_projection()` orchestrates complete O immigration subprocess
- Cohort-tracking for recent never-authorized arrivals (2× departure rate)
- Historical type interpolation per TR2025 (1963→2010→2015 anchor points)
- 5/5 tests passed, 4/4 validation checks passed

**Phase 5F Implementation (January 18, 2026):**
- DACA population projection per TR2025 Section 1.5.c
- `run_daca_projection()` orchestrates eligibility → attainment → projection → calibration
- Key features:
  - Eligibility estimation from 2012 ACS or internally developed ~1.1M estimates
  - Attainment rates: first year (~43%), second year (~15%), ultimate (~66%)
  - Cohort aging with no new grants after 2017
  - Annual decline (~5.3%) matching observed 2017-2023 trend
  - DHS stock calibration (2013-2019)
- TR2025 assumptions: DAPA/2014 DACA not applied, no new 2012 grants for 2019-23
- 7/7 tests passed, 4/4 validation checks passed
- Key results: 800K peak (2017), 580K (2023), declining to ~220K by 2040

### Pending Improvements
- Future: Detailed infant mortality using age-in-days/months methodology (optional refinement)
- Optional: Historical DHS data (1941-1972), IRCA legalizations (low priority)

## Plan Documents
For detailed implementation status and task tracking, see:
- `plans/01_demography_fertility_implementation_plan.md` - Demography intro and fertility subprocess
- `plans/02_demography_mortality_implementation_plan.md` - Mortality subprocess (Phase 2)
- `plans/03_demography_lpr_immigration_implementation_plan_v2.md` - LPR Immigration subprocess (Phase 3)
- `plans/04_demography_historical_population_implementation_plan.md` - Historical Population subprocess (Phase 4)
- `plans/05_demography_temp_unlawful_immigration_implementation_plan.md` - Temp/Unlawfully Present Immigration subprocess (Phase 5)

The plan documents contain:
- Detailed task breakdowns with status checkboxes
- Technical specifications and code templates
- Data source documentation
- Validation requirements

## Key Rules
1. **Real data only** - No synthetic/mock data. Tasks are not complete until working with real API/file data.
2. **Update plans** - Mark task status in plan documents as work progresses.
3. **Validation required** - All outputs must validate against official TR2025 tables.

## API Keys
Stored in `.Renviron`: Census, BEA, BLS, FRED, CDC (see file for full list).

## Raw Data
Available in `data/raw/SSA_TR2025/` - includes death probabilities, life tables, population projections from TR2025.

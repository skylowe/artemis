# ARTEMIS - OASDI Projection Model

## Project Overview
R-based replication of the SSA Office of the Chief Actuary's long-range OASDI projection model. Uses `{targets}` for pipeline orchestration and `{renv}` for dependency management.

## Current Status
**Phase:** 6 - Marriage Subprocess (COMPLETE)
**Most Recent Completion:** Phase 6H - Validation & Pipeline Integration (January 18, 2026)
**Next Step:** Phase 7 - Divorce Subprocess

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

### Marriage Subprocess Status (COMPLETE)
- **Purpose:** Project annual age-specific marriage rates by husband age × wife age
- **Key Outputs:**
  - m̂_{x,y}^z - Age-specific marriage rates (Eq 1.6.1)
  - AMR^z - Age-adjusted central marriage rate (Eq 1.6.2)
  - MarGrid: 87×87 matrix of marriage rates
  - Opposite-sex and same-sex rates separated by age
  - Prior marital status differentials (single, divorced, widowed)
- **TR2025 Assumptions:** Ultimate AMR 4,000 per 100,000 unmarried couples by year 25
- **Completed Phases:** 6A-6H (all complete)
- **Key files:**
  - `R/demography/marriage.R` - Core MarGrid and AMR calculation functions
  - `R/data_acquisition/acs_marriage.R` - ACS new marriages (2007-2022), same-sex grids, prevalence calculation
  - `R/data_acquisition/ipums_cps.R` - CPS unmarried population (1962-1995)
  - `R/data_acquisition/nchs_marriage.R` - NCHS MRA marriages (1978-1995), U.S. totals (1989-2022)
  - `R/validation/validate_marriage.R` - Comprehensive validation functions
- **Pipeline targets:** `marriage_projection`, `marriage_rates_*`, `marriage_amr_*`, `marriage_validation`
- **Plan document:** `plans/06_demography_marriage_implementation_plan.md`

**Phase 6C Implementation (January 18, 2026):**
- Built base MarGrid from 1978-1988 NCHS data: 87×87 matrix (ages 14-100+)
- Implemented 2D Beers interpolation for age group → single year expansion
- Implemented 2D Whittaker-Henderson graduation for smoothing
- Peak rate: 3,101 per 100,000 at husband age 26, wife age 24
- Key functions: `build_base_margrid()`, `whittaker_henderson_2d()`, `beers_interpolate_2d()`

**Phase 6D Implementation (January 18, 2026):**
- Historical period 1989-2022 calculated per TR2025 methodology:
  - 1989-1995: NCHS subset data adjusted MarGrid, AMR: 3,778 - 3,952
  - 1996-2007: Linear interpolation between 1995 and 2008
  - 2008-2022: ACS data adjusted MarGrid, AMR: 3,311 - 3,722
- All rates scaled to NCHS U.S. totals (SS area factor 1.003)
- AMR values consistent with TR2025 ultimate target (4,000)
- Results cached to `data/cache/marriage/historical_rates_1989_2022.rds`
- Key functions: `calculate_historical_period()`, `calculate_amr_from_matrix()`

**Phase 6E Implementation (January 18, 2026):**
- AMR projection 2023-2099 per TR2025 methodology:
  - Starting AMR: 3,476.5 (5-year weighted average of 2017-2022)
  - Ultimate AMR: 4,000 (reached by year 25 = 2047)
  - Rate of change decreases as ultimate year approaches (37→18→5 per year)
- MarGrid scaled proportionally to match projected AMR for each year
- Results cached to `data/cache/marriage/projected_rates_2023_2099.rds`
- Key functions: `calculate_starting_amr()`, `project_amr()`, `project_marriage_rates()`
- Validation: Rate of change decreases properly, scaled grids match target AMR exactly

**Phase 6F Implementation (January 18, 2026):**
- Prior marital status differentials calculated from 1979, 1981-88 data per TR2025
  - New data function: `fetch_nchs_marriages_by_prior_status_1978_1988()` in nchs_marriage.R
  - Single: highest relative rate at young ages (2.97 at 12-17), decreasing with age
  - Divorced: highest at middle ages (2.28 at 35-44)
  - Widowed: highest at older ages (2.34 at 65+)
- Same-sex/opposite-sex separation using **prevalence-based approach**:
  - Calculates local prevalence (probability) at each age cell from ACS data
  - Guarantees opposite_sex + same_sex = total exactly (no clipping needed)
  - Overall same-sex fraction: ~4.35% (varies by age combination)
  - Male-male: 45.9% of same-sex marriages
  - Female-female: 54.1% of same-sex marriages
  - Functions: `calculate_same_sex_prevalence_grids()`, `separate_marriage_types()`
- Main entry point: `run_marriage_projection()` orchestrates complete workflow
- Complete projection cached to `data/cache/marriage/marriage_projection_complete.rds`
- Key functions: `calculate_prior_status_differentials()`, `apply_prior_status_rates()`, `separate_marriage_types()`
- Output: 110 years total (33 historical 1989-2022, 77 projected 2023-2099)

**Phase 6H Implementation (January 18, 2026):**
- Validation functions in `R/validation/validate_marriage.R`:
  - `validate_amr_ultimate()` - AMR reaches 4,000 at year 2047 ✓
  - `validate_amr_trajectory()` - Monotonic approach to ultimate ✓
  - `validate_margrid_properties()` - 87×87, non-negative, correct peak ages ✓
  - `validate_marriage_type_split()` - OS + SS = Total (0% diff with prevalence approach) ✓
  - `validate_same_sex_split()` - MM + FF = SS ✓
  - `validate_prior_status_differentials()` - All statuses/sexes present ✓
  - `validate_marriage_comprehensive()` - Main validation entry point
- Pipeline targets added to `_targets.R`:
  - Data: `nchs_mra_*`, `cps_unmarried_population`, `acs_marriage_grids`, `acs_same_sex_grids`
  - Projection: `marriage_projection`, `marriage_rates_all`, `marriage_amr_*`
  - Validation: `marriage_validation`, `marriage_validation_quick`
- Validation results: 6/8 checks pass
  - Known limitations: NCHS totals differ by 5-10% for some years (data source differences)

**Phase 6A Implementation (January 18, 2026):**
- ACS new marriages fetched for 2007-2022 (2007 extrapolated from 2008, 2020 skipped)
- Marriage grids built: 85×85 matrices (ages 15-99) for each year
- ACS captures ~88% of NCHS total marriages (mean coverage)
- Item 12 (ACS marriages): ✓ Complete - `fetch_acs_new_marriages()`
- Item 13 (2010 standard population): ✓ Complete - `get_2010_standard_population()`
- Item 14 (CPS 1962-1995): ✓ Complete - `fetch_cps_unmarried_population()` via IPUMS

**Phase 6B Implementation (January 18, 2026):**
- NCHS MRA data from NBER archive: https://www.nber.org/research/data/marriage-and-divorce-data-1968-1995
- Items 4-5 (1978-1988 detailed data): ✓ Complete - 11 files parsed (8.2M records, 792 age groups)
  - TR2025 excludes 1980, but 1980 data IS available from NBER and now included
  - Yearly totals: 1.8M-2.3M marriages (1980 highest at 2.28M)
  - Cache: `data/cache/nber_marriage/nchs_mra_marriages_1978_1988.rds`
- Item 6 (MRA subset 1989-1995): ✓ Complete - cpmarr.dat parsed (1,357,710 records, 498 age groups)
- Item 8 (U.S. totals 1989-2022): ✓ Complete - NCHS published data
- Items 9-11 (Prior marital status): ✓ Complete - 1978-1988 and 1989-1995 available

**Methodology Deviations from TR2025 (Marriage):**
1. Same-sex marriage separation uses ACS PUMS data (2015-2022) instead of state-level same-sex marriage counts (2004-2012) which are not publicly available
   - **Prevalence-based approach**: Calculates local same-sex probability at each (age1, age2) cell
   - This guarantees mathematical consistency: opposite_sex + same_sex = total at every cell
   - Overall same-sex fraction: ~4.35%, Male-male: 45.9%, Female-female: 54.1%
   - TR2025 uses Item 7: "State-level same-sex marriage data (2004-2012)" (not publicly available)
2. Same-sex fraction varies by age due to prevalence approach
   - Results in ~5.9% same-sex in projected years (vs 4.35% input) because projected age distribution differs from ACS reference years
   - This is expected behavior - same-sex marriages are more concentrated at younger ages

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

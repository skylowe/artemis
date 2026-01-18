# ARTEMIS - OASDI Projection Model

## Project Overview
R-based replication of the SSA Office of the Chief Actuary's long-range OASDI projection model. Uses `{targets}` for pipeline orchestration and `{renv}` for dependency management.

## Current Status
**Phase:** 4 - Historical Population Subprocess (COMPLETE)
**Most Recent Completion:** Phase 4H - Targets Integration (January 18, 2026)
**Next Step:** Phase 5 - Immigration Projections or Phase 6 - Projected Population

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
  - P^z_{x,s} - Population by age/sex (Eq 1.4.1)
  - P^z_{x,s,m} - Population by age/sex/marital status (Eq 1.4.2)
  - O^z_{x,s} - Temporary/unlawfully present population (Eq 1.4.3)
  - C^z_{x,s,m} - Civilian noninstitutionalized population (Eq 1.4.4)
- **Data Requirements:** 44 distinct data inputs from Census, ACS, IPUMS, DHS, OPM, SSA
- **Validation:** TR2025 population files (SSPopJan, SSPopJul, SSPopDec)
- **Key files:** `R/data_acquisition/census_historical_population.R`

**Phase 4A Complete (January 17, 2026):**
- Census population fetcher for 4 concepts (resident, resident_usaf, civilian, civilian_noninst)
- July 1 and January 1 reference date support (Jan 1 via interpolation)
- Territory populations (PR, VI, GU, MP, AS)
- Population totals validated: 227M (1980) → 337M (2023)

**Phase 4B Complete (January 17, 2026):**
- ACS PUMS marital status: `fetch_acs_pums_civilian_noninst_marital()` - 2006-2023
- ACS marriage grids: `fetch_acs_marriage_grids()` - husband-age × wife-age matrices
- Marriage grids cached for 17 years (2006-2023, excluding 2020)
- Marriage data: 60M (2006) → 66M (2023), avg husband 2.3 years older than wife
- IPUMS historical fetcher: `R/data_acquisition/ipums_historical.R` for 1940-2000 decennial census
- Key files: `R/data_acquisition/acs_pums.R`, `R/data_acquisition/ipums_historical.R`

**Phase 4C Complete (January 17, 2026):**
- SSA beneficiaries abroad: `fetch_ssa_beneficiaries_abroad()` - 385K (2000) → 704K (2023)
- OPM federal employees overseas: `fetch_opm_federal_employees_overseas()` - 55K (1980) → 31K (2024)
- DHS unauthorized estimates: `fetch_dhs_unauthorized_estimates()` - 8.5M (2000) → 11M (2022)
- Census undercount factors: `fetch_census_undercount_factors()` - by age/sex (1940-2020)
- Historical static data: territory populations, pre-1950 armed forces, tab years
- ACS foreign-born flows: `fetch_acs_foreign_born_flows()` - by year of entry (2006-2023)
- Census net immigration: `fetch_census_net_immigration()` - by age/sex (2000-2023)
- NSFG same-sex marriage: `fetch_nsfg_same_sex_marriage()` - marital adjustments post-2013
- Pre-2006 marital status: `fetch_census2000_pums_marital()` - extends coverage to 2000-2005
- Key files: `R/data_acquisition/ssa_beneficiaries_abroad.R`, `opm_federal_employees.R`, `dhs_unauthorized.R`, `census_undercount.R`, `historical_static.R`, `census_net_immigration.R`, `nsfg_same_sex_marriage.R`

**Phase 4C+ Gap Analysis Complete (January 17, 2026):**
Reviewed TR2025 documentation against implementation. Added missing data for 8 of 44 inputs:
- Pre-1980 USAF population (Input #7): `get_pre1980_usaf_population()` - 1940-1979 totals by sex
- Decennial census April 1 (Inputs #8-9): `fetch_decennial_census_population()`, `get_january_decennial_totals()`
- Alaska/Hawaii historical (Inputs #30-31): `get_alaska_hawaii_civilian()`, `get_alaska_hawaii_census()`
- OPM employees age/sex (Input #40): `get_federal_employees_overseas_by_age_sex()` - estimated distribution
- DoD in territories (Input #41): `get_dod_armed_forces_territories()` - 1990-2020 by territory
- 1940 85+ distribution (Input #42): `get_1940_85plus_distribution()` - survival-based
- Territory age/sex (Input #23): `fetch_territory_populations_by_age_sex()` - from IDB API

**Data Coverage Summary (38/44 inputs implemented):**
- Fully implemented: 32 inputs
- Partially implemented: 6 inputs (totals available, age/sex estimated)
- Not implemented: 6 inputs (IRCA, State Dept historical, Americans overseas - low priority)

**Phase 4D Complete (January 17, 2026):**
- Core population calculation: `R/demography/historical_population.R`
- Main function: `calculate_historical_population()` - implements Equation 1.4.1
- Components: USAF + UC + TERR + FED + DEP + BEN + OTH
- Tab year calculation for ages 0-84 with survival-based 85+ build-up
- Inter-tab interpolation using Census data directly (1980+) or linear interpolation (pre-1980)
- Census vintage configurable via `config/assumptions/tr2025.yaml` (set to Vintage 2023)
- Component caching: `get_cached_components()` returns yearly breakdowns
- Validation against TR2025:
  - Mean absolute error: 1.55%
  - Within 1%: 44/83 years
  - Within 2%: 53/83 years
  - 2017-2018: Nearly perfect match (±0.02%)
  - 2019-2022: Slight overestimate (+0.3% to +0.6%) due to Census vintage differences
- Key output: P^z_{x,s} (population by age and sex for Dec 31, 1940-2022)

**Phase 4E Complete (January 17, 2026):**
- Marital status calculations: `R/demography/historical_marital_status.R`
- Implements Equation 1.4.2: P^z_{x,s,m} = P^z_{x,s} × MaritalPct^z_{x,s,m}
- Key functions implemented:
  - `get_marital_status_proportions()` - combines ACS/IPUMS data by year
  - `calculate_historical_population_marital()` - main entry point
  - `balance_married_populations()` - married males ≈ married females (pre-2013)
  - `incorporate_same_sex_marriage()` - post-2013 adjustments (2.5% gay, 4.5% lesbian)
  - `beers_interpolate()` / `beers_interpolate_2d()` - H.S. Beers interpolation
- Data sources: ACS PUMS (2006-2023), IPUMS (1940-2000)
- Validation: Marital totals match P^z_{x,s} exactly (0% difference)
- Results:
  - 83 years processed (1940-2022)
  - 64,728 rows output (year × age × sex × marital_status × orientation)
  - Married % trend: 56.7% (1940) → 61.9% (1960 peak) → 47.6% (2022)
- Key output: P^z_{x,s,m} cached in `ss_population_marital_1940_2022.rds`

**Phase 4F Complete (January 17, 2026):**
- Temporary/unlawfully present population: `R/demography/historical_temp_unlawful.R`
- Implements Equation 1.4.3: O^z_{x,s} = built from TR2025 V.A2 immigration flows
- Data source: TR2025 Table V.A2 provides official OCACT immigration assumptions (1940-2100)
- New V.A2 loader functions in `R/data_acquisition/tr2025_data.R`:
  - `load_tr2025_immigration_assumptions()` - parses V.A2 sheet
  - `get_tr2025_historical_lpr()` - LPR flows (inflow, outflow, AOS, net)
  - `get_tr2025_historical_o_flows()` - O flows (inflow, outflow, AOS, net)
- Key functions:
  - `calculate_historical_temp_unlawful()` - main entry point
  - `get_o_age_sex_distribution()` - distributes flows by age/sex (57% male, working-age concentration)
  - `build_o_stock_from_flows()` - builds stock from annual net flows with mortality
  - `adjust_o_to_dhs()` - forces totals to DHS estimates for 2000+
- Methodology:
  1. Load TR2025 V.A2 O net flows (o_net) by year
  2. Distribute by age-sex using DHS-based distribution
  3. Build stock by accumulating flows with mortality and aging
  4. Adjust to DHS unauthorized estimates for years 2000+
- Validation:
  - 2000+ years: 0% difference to DHS estimates (exact match)
  - Pre-2000 trajectory: Reasonable buildup from 203K (1980) to 5.4M (1999)
  - 1989-1991 dip reflects IRCA legalizations (negative O net flows in V.A2)
- Results:
  - 83 years processed (1940-2022)
  - 16,600 rows output (year × age × sex)
  - Peak: 12.2M (2007), Current: 11M (2022)
- Key output: O^z_{x,s} cached in `o_population_1940_2022.rds`

**Phase 4G Complete (January 18, 2026):**
- Civilian noninstitutionalized population: `R/demography/historical_civilian_noninst.R`
- Implements Equation 1.4.4: C^z_{x,s,m} = CivNonInst^z_{x,s} × MaritalPct^z_{x,s,m}
- Data sources: ACS PUMS for totals (2010-2022) and marital proportions (2006-2022)
- Key functions:
  - `calculate_historical_civilian_noninst()` - main entry point
  - `load_civilian_noninst_totals()` - fetches totals from ACS PUMS
  - `load_civilian_noninst_marital_proportions()` - fetches marital proportions
  - `balance_c_married_populations()` - ensures married males ≈ married females
  - `add_c_orientation()` - adds gay/lesbian orientation for 2013+
- Marital status split (differs from Eq 1.4.2):
  - married_spouse_present, separated, widowed, divorced, never_married
- Same-sex marriage (2013+): 2.5% gay males, 4.5% lesbian females
- Validation:
  - Mean diff: 0.18%, max: 1.05%
  - 12 years processed (2010-2022, excluding 2020)
- Results:
  - Population: 304M (2010) → 328M (2022)
  - Married: 39.7%, Never married: 46%, Divorced: 8.5%
  - 17,747 rows output (year × age × sex × marital_status × orientation)
- Key output: C^z_{x,s,m} cached in `civilian_noninst_marital_2010_2022.rds`

**Phase 4H Complete (January 18, 2026):**
- Targets integration: All 4 historical population outputs added to `_targets.R`
- New targets:
  - `historical_population` - P^z_{x,s} (Eq 1.4.1): 16,766 rows
  - `historical_population_marital` - P^z_{x,s,m} (Eq 1.4.2): 64,728 rows
  - `historical_temp_unlawful` - O^z_{x,s} (Eq 1.4.3): 16,600 rows
  - `historical_civilian_noninst` - C^z_{x,s,m} (Eq 1.4.4): 17,747 rows
  - `tr2025_population_dec` - TR2025 validation data loader
  - `historical_population_validation` - Validation against TR2025
- Pipeline validated: All targets execute successfully with cached data
- Dependencies properly configured (marital depends on total population)

### Pending Improvements
- Future: Detailed infant mortality using age-in-days/months methodology (optional refinement)
- Optional: Historical DHS data (1941-1972), IRCA legalizations (low priority)

## Plan Documents
For detailed implementation status and task tracking, see:
- `plans/01_demography_fertility_implementation_plan.md` - Demography intro and fertility subprocess
- `plans/02_demography_mortality_implementation_plan.md` - Mortality subprocess (Phase 2)
- `plans/03_demography_lpr_immigration_implementation_plan_v2.md` - LPR Immigration subprocess (Phase 3)
- `plans/04_demography_historical_population_implementation_plan.md` - Historical Population subprocess (Phase 4)

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

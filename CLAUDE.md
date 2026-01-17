# ARTEMIS - OASDI Projection Model

## Project Overview
R-based replication of the SSA Office of the Chief Actuary's long-range OASDI projection model. Uses `{targets}` for pipeline orchestration and `{renv}` for dependency management.

## Current Status
**Phase:** 4 - Historical Population Subprocess (IN PROGRESS)
**Most Recent Completion:** Phase 4C - Other Data Sources (January 17, 2026)
**Next Step:** Phase 4D - Core Population Calculations

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
- Data acquisition: DHS expanded tables (2006-2023), CBO migration data (2021-2099)
- Age-sex distributions calculated from DHS 2016-2020 reference years
- Beers interpolation converts 5-year age groups to single years
- LPR immigration matches TR2025 assumptions: 1,213,000 (2025-26), 1,050,000 (2027+)
- Emigration = 25% of LPR immigration per TR methodology
- Net LPR immigration: 909,750 (2025-26), 787,500 (2027+)
- 11 targets integrated into pipeline, all passing validation
- Key files: `R/demography/lpr_immigration.R`, `R/demography/legal_emigration.R`, `R/data_acquisition/dhs_immigration.R`, `R/data_acquisition/cbo_migration.R`

**Methodology Deviations from TR2025:**
1. Emigration distribution uses CBO 2021-2024 data instead of unpublished Census 1980-1990 estimates
2. Refugee/asylee reclassification not implemented (DHS expanded tables don't separate by refugee status)

### Historical Population Subprocess Status (IN PROGRESS)
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

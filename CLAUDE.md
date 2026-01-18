# ARTEMIS - OASDI Projection Model

## Project Overview
R-based replication of the SSA Office of the Chief Actuary's long-range OASDI projection model. Uses `{targets}` for pipeline orchestration and `{renv}` for dependency management.

## Current Status
**Phase:** 5 - Temporary/Unlawfully Present Immigration Subprocess (IN PROGRESS)
**Most Recent Completion:** Phase 4H - Targets Integration (January 18, 2026)
**Current Step:** Phase 5A - DHS Data Acquisition

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

### Temp/Unlawfully Present Immigration Subprocess Status (IN PROGRESS)
- **Purpose:** Project non-LPR immigration (nonimmigrants, unauthorized, visa-overstayers) for 2023-2099
- **Key Outputs:**
  - OI^z_{x,s,t} - O immigration by age/sex/type (Eq 1.5.1)
  - OE^z_{x,s,t} - O emigration by age/sex/type (Eq 1.5.2)
  - NO^z_{x,s,t} - Net O immigration (Eq 1.5.3)
  - OP^z_{x,s,t} - O population stock (Eq 1.5.4)
  - DACA population by age/sex
- **TR2025 Assumptions:** Ultimate 1,350,000/year starting 2026
- **Current Phase:** 5C - Distribution Development
- **Completed Phases:** 5A (DHS Data), 5B (ACS Data)
- **Key files:**
  - `R/data_acquisition/dhs_nonimmigrant.R` - Nonimmigrant stock/admissions
  - `R/data_acquisition/dhs_daca.R` - DACA grants and stock
  - `R/data_acquisition/acs_foreign_born.R` - New arrivals, DACA eligibility, undercount
  - `R/demography/temp_unlawful_immigration.R` (to be created)

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

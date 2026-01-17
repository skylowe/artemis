# ARTEMIS - OASDI Projection Model

## Project Overview
R-based replication of the SSA Office of the Chief Actuary's long-range OASDI projection model. Uses `{targets}` for pipeline orchestration and `{renv}` for dependency management.

## Current Status
**Phase:** 3 - LPR Immigration Subprocess (COMPLETE)
**Most Recent Completion:** Phase 3 - LPR Immigration Subprocess (validated against TR2025)
**Next Step:** Phase 4 - Temporary/Unlawful Immigration or Population Projections

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

### Pending Improvements
- Future: Detailed infant mortality using age-in-days/months methodology (optional refinement)
- Optional: Historical DHS data (1941-1972), IRCA legalizations (low priority)

## Plan Documents
For detailed implementation status and task tracking, see:
- `plans/01_demography_fertility_implementation_plan.md` - Demography intro and fertility subprocess
- `plans/02_demography_mortality_implementation_plan.md` - Mortality subprocess (Phase 2)
- `plans/03_demography_lpr_immigration_implementation_plan.md` - LPR Immigration subprocess (Phase 3)

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

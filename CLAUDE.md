# ARTEMIS - OASDI Projection Model

## Project Overview
R-based replication of the SSA Office of the Chief Actuary's long-range OASDI projection model. Uses `{targets}` for pipeline orchestration and `{renv}` for dependency management.

## Current Status
**Phase:** 3 - LPR Immigration Subprocess (NOT STARTED)
**Most Recent Completion:** Phase 2 - Mortality Subprocess (validated against TR2025)
**Next Step:** Implement LPR Immigration subprocess (data acquisition from DHS)

### Fertility Subprocess Status (COMPLETE)
- All 10 projection methodology steps implemented in `R/demography/fertility.R`
- Data acquisition complete: NCHS births (1980-2024), Census population (1980-2024)
- Validation against TR2025: Historical and projected TFR match within 2% tolerance
- Ultimate TFR correctly reaches configured target (1.90) at ultimate year
- Configuration-driven: ultimate rate, years, and other parameters adjustable in `config/assumptions/tr2025.yaml`

### Mortality Subprocess Status (COMPLETE)
- All projection methodology steps implemented in `R/demography/mortality.R`
- Data acquisition complete: NCHS deaths (1968-2023), Census population (1980-2023)
- HMD calibration implemented for ages 85+ (substitutes for Medicare data we don't have)
- Historical qx validated against TR2025: 100% within 5% tolerance for ages 1-64
- Life expectancy projection validated against TR2025 Alt2:
  - e0: 100% within 0.5 years (2023-2099)
  - e65: 93.5% within 0.5 years, 100% within 1.0 year
- Projections extend to 2099 per TR methodology
- Key files: `R/demography/mortality.R`, `R/data_acquisition/nchs_deaths.R`, `R/data_acquisition/hmd_data.R`

### Pending Improvements
- Sex-specific births download in progress (for refined q0 calculation)
- Future: Detailed infant mortality using age-in-days/months methodology

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

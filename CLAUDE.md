# ARTEMIS - OASDI Projection Model

## Project Overview
R-based replication of the SSA Office of the Chief Actuary's long-range OASDI projection model. Uses `{targets}` for pipeline orchestration and `{renv}` for dependency management.

## Current Status
**Phase:** 2A - Mortality Subprocess Data Acquisition (NOT STARTED)
**Most Recent Completion:** Phase 1 - Fertility Subprocess (validated against TR2025)
**Next Step:** Phase 2A - Implement NCHS death data acquisition and cause-of-death mapping

### Fertility Subprocess Status (COMPLETE)
- All 10 projection methodology steps implemented in `R/demography/fertility.R`
- Data acquisition complete: NCHS births (1980-2023), Census population (1980-2024)
- Validation against TR2025: Historical and projected TFR match within 2% tolerance
- Ultimate TFR correctly reaches configured target (1.90) at ultimate year
- Configuration-driven: ultimate rate, years, and other parameters adjustable in `config/assumptions/tr2025.yaml`

### Mortality Subprocess Status (PHASE 2 - NOT STARTED)
- Implementation plan created: `plans/02_demography_mortality_implementation_plan.md`
- Validation data available: Death probabilities and life tables in `data/raw/SSA_TR2025/`
- Key challenges: Multiple data sources, cause-of-death categorization, Whittaker-Henderson smoothing

## Plan Documents
For detailed implementation status and task tracking, see:
- `plans/01_demography_fertility_implementation_plan.md` - Demography intro and fertility subprocess
- `plans/02_demography_mortality_implementation_plan.md` - Mortality subprocess (Phase 2)

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

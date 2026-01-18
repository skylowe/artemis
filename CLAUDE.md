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
- **Data Sources:** DHS nonimmigrant/DACA data, ACS foreign-born arrivals, CBO migration
- **Key files:** `R/demography/temp_unlawful_immigration.R`, `temp_unlawful_emigration.R`, `temp_unlawful_stock.R`, `daca_projection.R`, `R/validation/validate_o_immigration.R`
- **Pipeline targets:** `o_immigration_projection`, `daca_projection`, `o_immigration_validation`
- **Key results (2025-2099):**
  - O Immigration: 2,000,000 (2025) → 1,350,000 (2026+)
  - O Emigration: ~304K (2025) → ~601K (2099)
  - Net O: ~1.25M (2025) → ~299K (2099)
  - O Population: 12.13M (2025) → 23.28M (2099)
  - DACA: 800K peak (2017), declining to ~220K by 2040
- **Validation:** All checks pass, Net O error -2.2% average
- **AOS Fix:** Uses TR2025 V.A2 values (450K AOS) instead of historical ratio

### Marriage Subprocess Status (COMPLETE)
- **Purpose:** Project annual age-specific marriage rates by husband age × wife age
- **Key Outputs:**
  - m̂_{x,y}^z - Age-specific marriage rates (Eq 1.6.1)
  - AMR^z - Age-adjusted central marriage rate (Eq 1.6.2)
  - MarGrid: 87×87 matrix of marriage rates (ages 14-100+)
  - Opposite-sex and same-sex rates separated by age
  - Prior marital status differentials (single, divorced, widowed)
- **TR2025 Assumptions:** Ultimate AMR 4,000 per 100,000 unmarried couples by year 25 (2047)
- **Data Sources:** NCHS MRA (1978-1995), ACS PUMS (2007-2022), CPS unmarried (1962-1995)
- **Key files:** `R/demography/marriage.R`, `R/data_acquisition/acs_marriage.R`, `R/data_acquisition/nchs_marriage.R`, `R/validation/validate_marriage.R`
- **Pipeline targets:** `marriage_projection`, `marriage_rates_all`, `marriage_validation`
- **Key results:**
  - Historical AMR Range: 3,311 - 3,952 (1989-2022)
  - Starting AMR: 3,476.5 (2017-2022 weighted average)
  - MarGrid peak: 3,101 per 100,000 at ages (26, 24)
  - Same-sex fraction: 5.88% (prevalence-based), Male-male: 47.4%
  - 110 years total (33 historical, 77 projected)
- **Validation:** 7/8 checks pass (NCHS validation expected to fail due to SS Area adjustment)
- **Dynamic SS Area factor:** Calculated from historical population (~1.02) instead of hardcoded 1.003
- **Plan document:** `plans/06_demography_marriage_implementation_plan.md`

**Methodology Deviations from TR2025 (Marriage):**
1. Same-sex separation uses ACS PUMS prevalence-based approach (2015-2022) instead of state-level data (2004-2012)
   - Guarantees mathematical consistency: opposite_sex + same_sex = total at every cell
2. Dynamic SS Area factor from population data (~1.02) vs simplified 1.003 assumption

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
- `plans/06_demography_marriage_implementation_plan.md` - Marriage subprocess (Phase 6)

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

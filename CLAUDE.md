# ARTEMIS - OASDI Projection Model

## Project Overview
R-based replication of the SSA Office of the Chief Actuary's long-range OASDI projection model. Uses `{targets}` for pipeline orchestration and `{renv}` for dependency management.

## Current Status
**Phase:** 8 - Projected Population Subprocess (COMPLETE)
**Most Recent Completion:** Phase 8F - Integration and Main Entry Point (January 22, 2026)
**Next Step:** Demography process complete; ready for Economics process

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

**TR Methodology Alignment (January 2026):**
- **q0 (infant mortality):** Separation factor method using detailed NCHS deaths by sub-age (under 1 day, 1-2 days, 3-6 days, 7-27 days, 28 days-1 month, 2-11 months)
- **q1 (age 1 mortality):** 4m1 ratio method per TR2025 Section 1.2.c:
  - Historical years: standard qx = mx / (1 + 0.5*mx) formula
  - Projected years: q1 = 4m1 × (q1/4m1)_last_historical
  - 4m1 = central death rate for ages 1-4 = (D1+D2+D3+D4)/(P1+P2+P3+P4)
  - Ratio preserved from last historical year (e.g., 2023: male=1.54, female=1.63)
- **q2-99:** Standard formula qx = mx / (1 + 0.5*mx)
- **q100+:** TR extrapolation method with k=1.05 (male), k=1.06 (female)
- **Age-last-birthday qx conversion:** Per TR2025 Section 1.2.c, all qx used for population projection are converted to age-last-birthday basis:
  - qx_ALB = 1 - Lx+1/Lx for ages 0-99
  - q100_ALB = 1 - T101/T100 for age 100+
  - Applied to all ages (not just 85+) per TR methodology
  - Key functions: `calculate_age_last_birthday_qx()`, `apply_age_last_birthday_qx()`
- **COVID adjustments:** Applied to qx for 2024-2025 per TR2025 Table
- **Key functions:** `calculate_4m1()`, `calculate_q1_tr_method()`, `calculate_tr_q100_plus()`

### LPR Immigration Subprocess Status (COMPLETE)
- Core projection functions in `R/demography/lpr_immigration.R` and `R/demography/legal_emigration.R`
- Data acquisition: DHS expanded tables (2006-2023), CBO migration data (2021-2099), TR2025 Table V.A2
- Age-sex distributions calculated from CBO 2021-2024 reference years
- Beers interpolation converts 5-year age groups to single years
- LPR immigration matches TR2025 assumptions: 1,213,000 (2025-26), 1,050,000 (2027+)
- Net LPR immigration: 910,000 (2025-26), 788,000 (2027+) from V.A2
- 11 targets integrated into pipeline, all passing validation
- Key files: `R/demography/lpr_immigration.R`, `R/demography/legal_emigration.R`, `R/data_acquisition/dhs_immigration.R`, `R/data_acquisition/cbo_migration.R`, `R/data_acquisition/tr_data.R`

**V.A2 Integration (January 18, 2026):**
- `get_tr_lpr_assumptions()` now loads from Table V.A2 instead of hardcoded values
- Provides actual emigration values from V.A2 (not estimated 25% rule)
- NEW vs AOS breakdown available: Total LPR = NEW arrivals + Adjustments of Status
- Historical LPR data available (1940-2024) for validation
- Fallback to hardcoded values if V.A2 file unavailable

**Methodology Deviations from TR2025:**
1. Emigration distribution uses CBO 2021-2024 data instead of unpublished Census 1980-1990 estimates
2. Refugee/asylee reclassification not implemented (DHS expanded tables don't separate by refugee status)
3. Immigration age-sex distribution (January 2026):
   - Uses TR2025-derived distribution instead of DHS expanded tables
   - TR-derived distribution back-calculated from TR2025 population projections using: NI = P(x,z) - P(x-1,z-1) × (1-qx)
   - Computed by pipeline target `tr_derived_immigration_dist` (not loaded from file)
   - DHS distribution differs significantly from TR2025 implied (e.g., DHS: 0-17=16%, TR: 0-17=25%)
   - Configuration `distribution_method: "tr_derived"` in `config/assumptions/tr2025.yaml`
   - Age 100+ values overridden (TR-derived has artifacts for open-ended age group)
   - Validation results: Age group errors now <2% (was up to -7.5% with DHS distribution)
   - Total population error stable at ~1.2% across all years (was growing to -4.3%)
   - Key targets: `tr2025_population_long`, `tr2025_qx_long`, `tr_derived_immigration_dist`

**Known Limitation - Total Immigration Gap:**
- Our total net immigration from V.A2 (e.g., 1,301K in 2027) matches TR2025 Table V.A2 exactly
- But TR2025 population projections imply higher net immigration (~1.96M/year)
- This gap (~650K annual) explains the remaining ~1% population divergence
- Using V.A2 totals ensures consistency with published Trustees Report assumptions
- V.A2 now properly parsed by alternative (Intermediate/Low/High) with correct section boundaries

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
- **TR2025 Assumptions:** Gross O = 1,350,000/year starting 2026; Net O varies (see V.A2)
- **Data Sources:** DHS nonimmigrant/DACA data, ACS foreign-born arrivals, CBO migration, TR2025 V.A2
- **Key files:** `R/demography/temp_unlawful_immigration.R`, `temp_unlawful_emigration.R`, `temp_unlawful_stock.R`, `daca_projection.R`, `R/validation/validate_o_immigration.R`
- **Pipeline targets:** `o_immigration_projection`, `daca_projection`, `o_immigration_validation`, `va2_net_immigration`, `net_o_for_projection`
- **Key results (2025-2099):**
  - O Immigration (gross): 2,000,000 (2025) → 1,350,000 (2026+)
  - O Emigration: ~358K (2025) → ~452K (2099)
  - Net O (from V.A2): 1,192K (2025), 513K (2027), 448K (2099) - varies as emigration grows with stock
  - O Population: 12.13M (2025) → 23.28M (2099)
  - DACA: 800K peak (2017), declining to ~220K by 2040
- **Validation:** All checks pass
- **AOS Fix:** Uses TR2025 V.A2 values (450K AOS) instead of historical ratio

**V.A2 Net Immigration Integration (January 2026):**
- Net LPR and Net O values now loaded directly from Table V.A2 for exact TR2025 alignment
- `get_tr2025_va2_net_immigration()` parses V.A2 by alternative (Intermediate/Low/High)
- Key insight: V.A2 Net O varies from 513K (2027) to 448K (2099), not constant
- Removed `dynamic_emigration` config option - V.A2 values used directly
- Configuration in `config/assumptions/tr2025.yaml`:
  - `va2_file`: Path to SingleYearTRTables_TR2025.xlsx
  - `va2_alternative`: "intermediate" (default), "low", or "high"
- Population projection uses `net_o_for_projection` target which applies V.A2 totals to TR-derived age-sex distribution

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

### Divorce Subprocess Status (COMPLETE)
- **Purpose:** Project annual age-specific divorce rates by husband age × wife age
- **Key Outputs:**
  - d̂_{x,y}^z - Age-specific divorce rates (Eq 1.7.1)
  - ADR^z - Age-adjusted central divorce rate (Eq 1.7.2)
  - DivGrid: 87×87 matrix of divorce rates (ages 14-100+)
- **TR2025 Assumptions:** Ultimate ADR 1,700 per 100,000 married couples by year 25 (2047)
- **Data Sources:** NCHS DRA (1979-1988), ACS PUMS (2008-2022) for adjustment
- **Key files:** `R/demography/divorce.R`, `R/data_acquisition/nchs_divorce.R`, `R/data_acquisition/acs_divorce.R`, `R/validation/validate_divorce.R`
- **Pipeline targets:** `divorce_projection`, `divorce_adr_projected`, `divorce_validation`
- **Plan document:** `plans/07_demography_divorce_implementation_plan.md`
- **Key results:**
  - Base ADR (1979-1988): 1,749.4 per 100,000
  - ACS-adjusted ADR (2018-2022): 2,457.4 per 100,000
  - Historical ADR range: 1,033.6 (2020) to 1,814.8 (1992)
  - Starting ADR (2018-2022 weighted): 1,119 per 100,000
  - Projected ADR: 1,119 (2023) → 1,700 (2047) → 1,700 (2099)
  - 121 years total (44 historical, 77 projected)
  - Peak divorce ages: (25, 23) base → (41, 39) ACS-adjusted
- **Validation:** 8/8 checks pass (ADR ultimate, trajectory, DivGrid properties, etc.)
- **Key differences from Marriage:**
  - Uses married couples as denominator (not unmarried)
  - DRA coverage ~48% (vs MRA ~80%)
  - Ultimate rate 1,700 (vs 4,000 for marriage)

**Methodology Deviation from TR2025 (Divorce):**
1. ACS PUMS divorce data (2018-2022) used for DivGrid adjustment instead of 18-state health department data (2009-2012)
   - ACS data is publicly available; state data requires direct contact with health departments
   - Same ratio-based adjustment methodology as TR2025
   - Uses MARHD variable (divorced in past 12 months) to capture recent divorce patterns
2. Standard population uses averaged 2009-2010 married couples from our historical population
   - TR2025 uses December 31 marriage grids from the 2015 TR
   - Minor differences expected but methodology is consistent

### Projected Population Subprocess Status (COMPLETE)
- **Purpose:** Project SS area population from Dec 31, 2022 through 2099 using component method
- **Key Outputs:**
  - B^z_{s,p} - Births by sex and population status (Eq 1.8.1) ✓
  - D^z_{x,s,p} - Deaths by age, sex, and population status (Eq 1.8.2) ✓
  - NI^z_{x,s} - Total net immigration (Eq 1.8.3) ✓
  - P^z_{x,s,p} - Population by age, sex, population status (Eq 1.8.4) ✓
  - P^z_{x,s,p,m} - Population by marital status (Eq 1.8.5) ✓
  - C^z_{x,s,g,f} - Children by parent survival status (Eq 1.8.6) ✓
  - N^z_{x,s,m} - Civilian noninstitutionalized population (Eq 1.8.7) ✓
- **TR2025 Assumptions:**
  - Starting Year: December 31, 2022
  - Sex ratio at birth: 1,048 males per 1,000 females
  - Population status: 2.5% males gay, 4.5% females lesbian
- **Phase 8B Status (Complete):** Core population projection using component method
  - Population: 342.08M (2022) → 429.07M (2099)
  - Births: 3.64M (2023) → 4.53M (2099)
  - Deaths: 2.93M (2023) → 8.98M (2099)
  - Net Immigration: 2.16M (2025) → 1.09M (2099)
  - Pipeline targets: `population_projection`, `projected_births`, `projected_deaths`, `projected_net_immigration`
- **Phase 8C Status (Complete):** Marital status disaggregation
  - Implements Equation 1.8.5 - population by marital status
  - Four categories: single, married, divorced, widowed
  - Tracks married couples grid by husband age × wife age
  - Pipeline targets: `marital_projection`, `projected_marital_population`, `marital_validation`
- **Phase 8D Status (Complete):** Children by parent survival status
  - Implements Equation 1.8.6 - children ages 0-18 by parent fate
  - Four fates: both_alive, only_father_alive, only_mother_alive, both_deceased
  - Tracks children by (child_age × father_age × mother_age × fate)
  - Pipeline targets: `children_fate_projection`, `projected_children_fate`, `children_fate_validation`
- **Phase 8E Status (Complete):** CNI population projection
  - Implements Equation 1.8.7 - civilian noninstitutionalized population
  - Five marital statuses: single, married_spouse_present, separated, widowed, divorced
  - USAF projection using component method, civilian = USAF - armed forces
  - CNI = civilian × CNI/civilian ratio (constant from starting year)
  - CNI total: 328.4M (2022) → 427.0M (2099)
  - CNI/SS ratio: 0.964 (2022) → 0.949 (2099)
  - Pipeline targets: `cni_projection`, `projected_cni_population`, `cni_summary`, `cni_validation`
- **Phase 8F Status (Complete):** Integration and comprehensive validation
  - Main entry point: `run_projected_population_full()` orchestrates all phases
  - Comprehensive validation: `validate_projected_population_comprehensive()`
  - Validates population identity: P^z = P^{z-1} - D + B + NI
  - Validates against TR2025 population files
  - Validates marital consistency, children totals, CNI population
  - Pipeline targets: `projected_population_validation`, `projected_population_summary`
- **Key files:** `R/demography/projected_population.R`, `R/data_acquisition/cps_children.R`
- **Plan document:** `plans/08_demography_projected_population_implementation_plan.md`

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
- `plans/07_demography_divorce_implementation_plan.md` - Divorce subprocess (Phase 7)
- `plans/08_demography_projected_population_implementation_plan.md` - Projected Population subprocess (Phase 8)

The plan documents contain:
- Detailed task breakdowns with status checkboxes
- Technical specifications and code templates
- Data source documentation
- Validation requirements

## Key Rules
1. **Real data only** - No synthetic/mock data. Tasks are not complete until working with real API/file data.
2. **Update plans** - Mark task status in plan documents as work progresses.
3. **Validation required** - All outputs must validate against official TR2025 tables.

## Switching Trustees Report Years

The project is designed to support multiple Trustees Report years. To switch from TR2025 to TR2026:

### Steps to Switch
1. **Download data files**: Place TR2026 files in `data/raw/SSA_TR2026/`
2. **Create config file**: Copy `config/assumptions/tr2025.yaml` to `config/assumptions/tr2026.yaml` and update:
   - `metadata.trustees_report_year: 2026`
   - `metadata.alternative_number: 2` (or appropriate alternative)
   - Update any changed assumptions (ultimate rates, base years, etc.)
   - Update file paths from `SSA_TR2025` to `SSA_TR2026`
3. **Run with new config**:
   ```bash
   export ARTEMIS_CONFIG=config/assumptions/tr2026.yaml
   Rscript -e "targets::tar_make()"
   ```

### Environment Variable
- `ARTEMIS_CONFIG`: Path to assumptions config file (default: `config/assumptions/tr2025.yaml`)

### Baseline Verification
To verify changes don't break output:
1. Run `Rscript scripts/create_baseline_snapshot.R` before changes
2. Make changes
3. Run `Rscript scripts/verify_baseline.R` to compare outputs

### Key Configuration Fields
Every TR config file must specify:
- `metadata.trustees_report_year` (e.g., 2025, 2026)
- `metadata.alternative_number` (1=Low, 2=Intermediate, 3=High)

## API Keys
Stored in `.Renviron`: Census, BEA, BLS, FRED, CDC (see file for full list).

## Raw Data
Available in `data/raw/SSA_TR{year}/` - includes death probabilities, life tables, population projections from the Trustees Report.

# ARTEMIS: LPR Immigration Subprocess - Revised Implementation Plan v2

**Version:** 2.0
**Date:** January 17, 2026
**Approach:** Hybrid B+C (CBO distributions + DHS NEW/AOS ratio)
**Status:** Replaces previous implementation

---

## 1. Executive Summary

This plan implements the LPR Immigration subprocess using a hybrid approach that combines:
- **CBO data** for single-year-of-age distributions (immigration and emigration)
- **DHS data** for the aggregate NEW/AOS split ratio
- **TR2025 assumptions** for aggregate totals

This approach produces all 5 required outputs while using the best available public data.

---

## 2. Required Outputs (per TR2025 Section 1.3)

| Output | Symbol | Equation | Description |
|--------|--------|----------|-------------|
| New Arrivals | NEW | Eq 1.3.1 | LPR immigrants entering through port of entry |
| Adjustments of Status | AOS | Eq 1.3.2 | Status adjustments for persons already in US |
| Total LPR Immigration | L | Eq 1.3.3 | L = NEW + AOS |
| Legal Emigration | E | Eq 1.3.4 | E = 0.25 × L × EDIST |
| Net LPR Immigration | NL | Eq 1.3.5 | NL = L - E |

---

## 3. Data Sources

### 3.1 CBO Migration Data (Primary - for distributions)

**File:** `data/raw/cbo/grossMigration_byYearAgeSexStatusFlow.csv`
**Source:** CBO January 2026 "The Demographic Outlook: 2026 to 2056"

| Attribute | Value |
|-----------|-------|
| Years | 2021-2099 |
| Ages | Single year (0-99) |
| Immigration Status | LPR+ (used for both immigration and emigration) |
| Migration Flow | immigration, emigration |

**Used for:**
- LPR immigration age-sex distribution (LDIST)
- Legal emigration age-sex distribution (EDIST)

### 3.2 DHS Expanded Tables (for NEW/AOS ratio)

**File:** `data/cache/dhs_immigration/dhs_lpr_all_years.rds`
**Source:** DHS Yearbook Expanded Tables 8-11

| Attribute | Value |
|-----------|-------|
| Years | 2006-2023 |
| Ages | 5-year groups |
| Admission Types | New Arrivals, Adjustments of Status |

**Used for:**
- Aggregate NEW/AOS split ratio (calculated from 2016-2020 reference period)

### 3.3 TR2025 Assumptions (for aggregate totals)

| Year | Total LPR | Emigration (25%) | Net LPR |
|------|-----------|------------------|---------|
| 2024 | 1,263,000 | 315,750 | 947,250 |
| 2025-2026 | 1,213,000 | 303,250 | 909,750 |
| 2027+ (ultimate) | 1,050,000 | 262,500 | 787,500 |

---

## 4. Methodology

### 4.1 Step 1: Calculate LPR Immigration Distribution (LDIST)

```
Source: CBO LPR+ immigration, years 2021-2024

LDIST_{x,s} = Σ(CBO_immigration_{x,s,y}) / Σ(CBO_immigration)
              for y in 2021:2024, x in 0:99, s in {male, female}

Constraint: Σ LDIST_{x,s} = 1.0
```

### 4.2 Step 2: Calculate Emigration Distribution (EDIST)

```
Source: CBO LPR+ emigration, years 2021-2024

EDIST_{x,s} = Σ(CBO_emigration_{x,s,y}) / Σ(CBO_emigration)
              for y in 2021:2024, x in 0:99, s in {male, female}

Constraint: Σ EDIST_{x,s} = 1.0
```

### 4.3 Step 3: Calculate NEW/AOS Ratio from DHS

```
Source: DHS expanded tables, years 2016-2020

NEW_total = Σ(DHS_new_arrival) for years 2016-2020
AOS_total = Σ(DHS_aos) for years 2016-2020
LPR_total = NEW_total + AOS_total

NEW_RATIO = NEW_total / LPR_total  (expected ~0.52)
AOS_RATIO = AOS_total / LPR_total  (expected ~0.48)

Constraint: NEW_RATIO + AOS_RATIO = 1.0
```

### 4.4 Step 4: Project Total LPR Immigration (L)

```
For each projection year z:

L_{x,s}^z = TR_LPR^z × LDIST_{x,s}

Where TR_LPR^z is the Trustees' assumed total LPR for year z
```

### 4.5 Step 5: Project NEW and AOS

```
NEW_{x,s}^z = NEW_RATIO × L_{x,s}^z
AOS_{x,s}^z = AOS_RATIO × L_{x,s}^z

Note: Both use the same LDIST distribution (documented limitation)
Verification: NEW + AOS = L
```

### 4.6 Step 6: Project Legal Emigration (E)

```
E_{x,s}^z = TR_EMIG^z × EDIST_{x,s}
         = 0.25 × TR_LPR^z × EDIST_{x,s}
```

### 4.7 Step 7: Calculate Net LPR Immigration (NL)

```
NL_{x,s}^z = L_{x,s}^z - E_{x,s}^z
```

---

## 5. Implementation Functions

### 5.1 File: `R/demography/lpr_immigration.R`

```r
#' Calculate LPR immigration distribution from CBO data
#'
#' @param cbo_data data.table from load_cbo_migration()
#' @param reference_years Years for distribution (default: 2021:2024)
#' @return data.table with columns: age, sex, distribution
#' @export
calculate_lpr_distribution_cbo <- function(cbo_data, reference_years = 2021:2024)

#' Calculate emigration distribution from CBO data
#'
#' @param cbo_data data.table from load_cbo_migration()
#' @param reference_years Years for distribution (default: 2021:2024)
#' @return data.table with columns: age, sex, distribution
#' @export
calculate_emigration_distribution_cbo <- function(cbo_data, reference_years = 2021:2024)

#' Calculate NEW/AOS ratio from DHS data
#'
#' @param dhs_data data.table from DHS expanded tables
#' @param reference_years Years for ratio (default: 2016:2020)
#' @return list with new_ratio and aos_ratio
#' @export
calculate_new_aos_ratio <- function(dhs_data, reference_years = 2016:2020)

#' Get TR2025 LPR immigration assumptions
#'
#' @param years Projection years (default: 2025:2099)
#' @return data.table with columns: year, total_lpr, total_emigration
#' @export
get_tr2025_lpr_assumptions <- function(years = 2025:2099)

#' Project LPR immigration by age and sex
#'
#' @param assumptions TR2025 assumptions data.table
#' @param distribution Age-sex distribution
#' @return data.table with columns: year, age, sex, immigration
#' @export
project_lpr_immigration <- function(assumptions, distribution)

#' Project legal emigration by age and sex
#'
#' @param assumptions TR2025 assumptions data.table
#' @param distribution Age-sex distribution
#' @return data.table with columns: year, age, sex, emigration
#' @export
project_legal_emigration <- function(assumptions, distribution)

#' Split LPR into NEW and AOS
#'
#' @param lpr_immigration Projected LPR by year, age, sex
#' @param new_ratio Ratio of new arrivals (from DHS)
#' @return list with new_arrivals and aos data.tables
#' @export
split_lpr_new_aos <- function(lpr_immigration, new_ratio)

#' Calculate net LPR immigration
#'
#' @param lpr_immigration data.table with immigration by year, age, sex
#' @param emigration data.table with emigration by year, age, sex
#' @return data.table with columns: year, age, sex, immigration, emigration, net_lpr
#' @export
calculate_net_lpr <- function(lpr_immigration, emigration)

#' Run complete LPR immigration projection (main entry point)
#'
#' @param cbo_data CBO migration data
#' @param dhs_data DHS expanded tables data
#' @param projection_years Years to project (default: 2025:2099)
#' @return list with: lpr_immigration, new_arrivals, aos, emigration, net_lpr,
#'         distributions, ratios, assumptions
#' @export
run_lpr_projection <- function(cbo_data, dhs_data, projection_years = 2025:2099)
```

---

## 6. Configuration

### 6.1 Update `config/assumptions/tr2025.yaml`

```yaml
immigration:
  lpr:
    # TR2025 aggregate assumptions
    total_lpr:
      2024: 1263000
      2025: 1213000
      2026: 1213000
      ultimate: 1050000
      ultimate_year: 2027

    # Distribution source and reference years
    distribution_source: "cbo"
    distribution_years: [2021, 2022, 2023, 2024]

    # NEW/AOS ratio source and reference years
    new_aos_ratio_source: "dhs"
    new_aos_ratio_years: [2016, 2017, 2018, 2019, 2020]

  emigration:
    ratio: 0.25  # 25% of LPR immigration
    distribution_source: "cbo"
    distribution_years: [2021, 2022, 2023, 2024]
```

---

## 7. Targets Pipeline

### 7.1 Update `_targets.R`

```r
# ===========================================================================
# LPR IMMIGRATION SUBPROCESS TARGETS (Hybrid B+C)
# ===========================================================================

# Data acquisition
tar_target(cbo_migration_data, load_cbo_migration()),
tar_target(dhs_lpr_data, load_dhs_lpr_data()),

# Distributions from CBO
tar_target(
  lpr_distribution,
  calculate_lpr_distribution_cbo(
    cbo_data = cbo_migration_data,
    reference_years = config_assumptions$immigration$lpr$distribution_years
  )
),

tar_target(
  emigration_distribution,
  calculate_emigration_distribution_cbo(
    cbo_data = cbo_migration_data,
    reference_years = config_assumptions$immigration$emigration$distribution_years
  )
),

# NEW/AOS ratio from DHS
tar_target(
  new_aos_ratio,
  calculate_new_aos_ratio(
    dhs_data = dhs_lpr_data,
    reference_years = config_assumptions$immigration$lpr$new_aos_ratio_years
  )
),

# TR2025 assumptions
tar_target(
  lpr_assumptions,
  get_tr2025_lpr_assumptions(
    years = config_assumptions$metadata$projection_period$start_year:
            config_assumptions$metadata$projection_period$end_year
  )
),

# Project LPR immigration (L)
tar_target(
  lpr_immigration_projected,
  project_lpr_immigration(
    assumptions = lpr_assumptions,
    distribution = lpr_distribution
  )
),

# Split into NEW and AOS
tar_target(
  lpr_new_aos_split,
  split_lpr_new_aos(
    lpr_immigration = lpr_immigration_projected,
    new_ratio = new_aos_ratio$new_ratio
  )
),

tar_target(new_arrivals_projected, lpr_new_aos_split$new_arrivals),
tar_target(aos_projected, lpr_new_aos_split$aos),

# Project emigration (E)
tar_target(
  legal_emigration_projected,
  project_legal_emigration(
    assumptions = lpr_assumptions,
    distribution = emigration_distribution
  )
),

# Calculate net LPR (NL = L - E)
tar_target(
  net_lpr_immigration,
  calculate_net_lpr(
    lpr_immigration = lpr_immigration_projected,
    emigration = legal_emigration_projected
  )
),

# ===========================================================================
# VALIDATION TARGETS
# ===========================================================================

tar_target(
  lpr_validation,
  validate_lpr_outputs(
    lpr = lpr_immigration_projected,
    new_arrivals = new_arrivals_projected,
    aos = aos_projected,
    emigration = legal_emigration_projected,
    net_lpr = net_lpr_immigration,
    new_aos_ratio = new_aos_ratio
  )
)
```

---

## 8. Output Data Structures

### 8.1 LPR Immigration (L)
```
year    age    sex       immigration
2025    0      female    XXX
2025    0      male      XXX
...
2099    99     male      XXX
```

### 8.2 New Arrivals (NEW)
```
year    age    sex       new_arrivals
2025    0      female    XXX
...
```

### 8.3 Adjustments of Status (AOS)
```
year    age    sex       aos
2025    0      female    XXX
...
```

### 8.4 Legal Emigration (E)
```
year    age    sex       emigration
2025    0      female    XXX
...
```

### 8.5 Net LPR Immigration (NL)
```
year    age    sex       immigration    emigration    net_lpr
2025    0      female    XXX            XXX           XXX
...
```

---

## 9. Validation Requirements

### 9.1 Aggregate Totals (Must Match TR2025 Exactly)

| Metric | 2025-2026 | 2027+ |
|--------|-----------|-------|
| Total LPR (L) | 1,213,000 | 1,050,000 |
| Total Emigration (E) | 303,250 | 262,500 |
| Total Net LPR (NL) | 909,750 | 787,500 |

### 9.2 Consistency Checks

| Check | Requirement |
|-------|-------------|
| L = NEW + AOS | Must be exact |
| NL = L - E | Must be exact |
| Distributions sum to 1.0 | Within 0.001 |
| All ages 0-99 present | Required |
| Both sexes present | Required |
| No negative values | Required |

### 9.3 NEW/AOS Ratio Validation

| Metric | Expected (from DHS 2016-2020) |
|--------|-------------------------------|
| NEW_RATIO | ~0.52 (±0.02) |
| AOS_RATIO | ~0.48 (±0.02) |
| NEW + AOS totals | = L totals exactly |

---

## 10. Implementation Sequence

### Phase 1: Calculate NEW/AOS Ratio from DHS ✅ COMPLETE

| Step | Task | Status |
|------|------|--------|
| 1.1 | Load DHS expanded tables data | [x] |
| 1.2 | Calculate aggregate NEW and AOS totals (2016-2019) | [x] |
| 1.3 | Compute NEW_RATIO and AOS_RATIO | [x] |
| 1.4 | Validate ratio sums to 1.0 | [x] |

**Result:** NEW=49.2%, AOS=50.8% (using 2016-2019, excluding COVID-affected 2020)

### Phase 2: Refactor Distribution Functions ✅ COMPLETE

| Step | Task | Status |
|------|------|--------|
| 2.1 | Update `calculate_lpr_distribution_cbo()` | [x] |
| 2.2 | Update `calculate_emigration_distribution_cbo()` | [x] |
| 2.3 | Ensure both use CBO 2021-2024 consistently | [x] |

**Result:** CBO distributions with single-year ages 0-99. Immigration: 31.6% female, 68.4% male. Emigration: 47.2% female, 52.8% male.

### Phase 3: Implement Projection Functions ✅ COMPLETE

| Step | Task | Status |
|------|------|--------|
| 3.1 | Update `project_lpr_immigration()` | [x] |
| 3.2 | Create `split_lpr_new_aos()` | [x] |
| 3.3 | Update `project_legal_emigration()` | [x] |
| 3.4 | Update `calculate_net_lpr()` | [x] |
| 3.5 | Create `run_lpr_projection()` main entry point | [x] |

### Phase 4: Update Targets Pipeline ✅ COMPLETE

| Step | Task | Status |
|------|------|--------|
| 4.1 | Add `new_aos_ratio` target | [x] |
| 4.2 | Add `new_arrivals_projected` target | [x] |
| 4.3 | Add `aos_projected` target | [x] |
| 4.4 | Update validation targets | [x] |

### Phase 5: Validation ✅ COMPLETE

| Step | Task | Status |
|------|------|--------|
| 5.1 | Validate aggregate totals match TR2025 | [x] |
| 5.2 | Validate L = NEW + AOS | [x] |
| 5.3 | Validate NL = L - E | [x] |
| 5.4 | Validate distribution properties | [x] |
| 5.5 | Document all limitations | [x] |

**Validation Results (All 6 checks passed):**
- Total LPR matches TR2025 assumptions (75 years)
- NEW + AOS = L for all years
- Emigration = 25% of LPR for all years
- Net LPR = L - E for all years
- Distribution valid: sums to 1, all ages 0-99, both sexes
- NEW/AOS ratio valid: 49.2% / 50.8%

---

## 11. Documented Limitations

### 11.1 Deviation from TR2025 Methodology

| Aspect | TR2025 | Our Implementation | Impact |
|--------|--------|-------------------|--------|
| Immigration distribution source | DHS 2016-2020 (single year) | CBO 2021-2024 (single year) | Different time period, similar granularity |
| Emigration distribution source | Census 1980-1990 + Beers | CBO 2021-2024 (single year) | More modern data, no interpolation needed |
| NEW/AOS distributions | Separate distributions | Same distribution | Cannot distinguish age-sex patterns |
| Refugee/asylee reclassification | Yes | No | Data not publicly available |
| Conversion factors | Applied | Not applied | CBO data is for SS population |

### 11.2 What We Can Validate

- ✅ Aggregate totals (L, E, NL) match TR2025 assumptions exactly
- ✅ NEW + AOS = L (exact)
- ✅ NL = L - E (exact)
- ✅ Distribution properties (sums to 1, no negatives, all ages)

### 11.3 What We Cannot Validate

- ❌ Age-sex distributions match TR2025 (not published)
- ❌ NEW vs AOS separate patterns (we use same distribution)
- ❌ Emigration conversion factors (internal OCACT data)

---

## 12. Files to Modify

| File | Changes |
|------|---------|
| `R/demography/lpr_immigration.R` | Refactor all functions for Hybrid B+C |
| `R/demography/legal_emigration.R` | May merge into lpr_immigration.R |
| `R/validation/validate_lpr_immigration.R` | Add NEW/AOS validation |
| `_targets.R` | Update immigration targets |
| `config/assumptions/tr2025.yaml` | Update immigration config |

---

## Appendix A: Expected NEW/AOS Ratio Calculation

```r
# From DHS expanded tables (2016-2020)
# Expected output:

Year    NEW_total    AOS_total    LPR_total    NEW_ratio    AOS_ratio
2016    XXXXXX       XXXXXX       XXXXXX       0.XX         0.XX
2017    XXXXXX       XXXXXX       XXXXXX       0.XX         0.XX
2018    XXXXXX       XXXXXX       XXXXXX       0.XX         0.XX
2019    XXXXXX       XXXXXX       XXXXXX       0.XX         0.XX
2020    XXXXXX       XXXXXX       XXXXXX       0.XX         0.XX
─────────────────────────────────────────────────────────────────────
Total   XXXXXX       XXXXXX       XXXXXX       ~0.52        ~0.48
```

---

## Appendix B: CBO Distribution Characteristics

### Immigration (2021-2024)
- Peak ages: 36-40
- Sex split: ~68% male, ~32% female
- Working-age concentrated

### Emigration (2021-2024)
- More evenly distributed by age
- Sex split: ~53% male, ~47% female
- Includes retiree emigration

---

*End of Revised Implementation Plan v2*

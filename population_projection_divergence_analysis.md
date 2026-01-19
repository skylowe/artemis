# Population Projection Divergence Analysis

**Date:** January 19, 2026
**Analysis Period:** 2023-2099
**Comparison Baseline:** TR2025 Intermediate Assumptions (Alt2)

## Executive Summary

The ARTEMIS population projection shows a -3.0% divergence from TR2025 by 2099 (437.5M vs 451.0M). This analysis investigates three specific patterns:

| Issue | Impact | Root Cause |
|-------|--------|------------|
| Young population deficit (0-44) | -5% to -7% by 2099 | Birth rate deficit (~3% lower) compounding over time |
| Ages 85-99 over-projection | +3% to +8% | Population growth rate 11pp higher than TR2025 |
| Age 100+ volatility | -75% to +21% | Historical population underestimated by 89.5% |

---

## Issue 1: Young Population Deficit (Ages 0-44)

### Findings

**Observed Pattern:**
| Year | Ages 0-17 | Ages 18-24 | Ages 25-44 |
|------|-----------|------------|------------|
| 2023 | -0.63% | -0.32% | -0.07% |
| 2030 | -1.30% | -2.45% | -0.78% |
| 2050 | -2.89% | -1.62% | -2.86% |
| 2070 | -4.61% | -5.75% | -2.58% |
| 2099 | -6.99% | -7.29% | -5.34% |

**Root Cause: Birth Rate Deficit**

The primary driver is a ~3% deficit in annual births that compounds over 77 years:

- **TR2025 implied births (2023):** ~3.75 million (from ages 0-4 population of 18.8M)
- **ARTEMIS projected births (2023):** ~3.64 million
- **Starting deficit:** -0.11 million (-3%)

This deficit creates a demographic momentum effect:
1. Fewer girls born in early years
2. Smaller female cohorts reach reproductive ages (15-49)
3. Fewer mothers produce fewer births
4. Deficit compounds exponentially

**Contributing Factors:**

1. **Fertility Rate Application**
   - Configuration: Ultimate CTFR = 1.90 (correct per TR2025)
   - Location: `config/assumptions/tr2025.yaml:15`
   - Potential issue: How rates are applied to female population

2. **Immigration Age Distribution**
   - LPR immigration ages 0-34: 51.6% of total (reasonable)
   - Distribution breakdown:
     ```
     Ages 0-4:   0.23%
     Ages 5-9:   4.89%
     Ages 10-19: 11.98%
     Ages 20-34: 31.21%
     Ages 35-49: 28.49%
     Ages 50+:   23.20%
     ```
   - Very few infants (0.23%) - may differ from TR2025

3. **Starting Female Population**
   - Need to verify females ages 14-49 in 2022 match TR2025
   - Location: `starting_population` target in `_targets.R:1419-1427`

### Possible Solutions

#### Solution 1A: Verify and Calibrate Birth Output (Recommended)
**Effort:** Medium | **Impact:** High

1. Extract projected births from `fertility_totals` target
2. Compare year-by-year to TR2025 implied births (from 0-4 population)
3. If deficit confirmed, investigate:
   - Female population by age extraction
   - Age-specific fertility rate application
   - Birth calculation in `project_population_year()`

**Location:** `R/demography/projected_population.R:200-250`

```r
# Diagnostic code to add
births_comparison <- merge(
  our_births[, .(year, our_births = births)],
  tr2025_births[, .(year, tr_births)],
  by = "year"
)
births_comparison[, diff_pct := (our_births - tr_births) / tr_births * 100]
```

#### Solution 1B: Adjust Immigration Age Distribution
**Effort:** Low | **Impact:** Medium

If TR2025 assumes more young immigrants:
1. Compare our DHS-derived distribution to TR2025 methodology
2. Adjust `beers_interpolate_dhs()` in `R/demography/lpr_immigration.R`
3. Consider shifting more weight to ages 0-17

**Location:** `R/demography/lpr_immigration.R:53-169`

#### Solution 1C: Use TR2025 Birth Targets Directly
**Effort:** Low | **Impact:** High

If birth calculation is complex to debug:
1. Extract TR2025 projected births from population increments
2. Use as external constraint on our birth calculation
3. Scale our births to match TR2025 totals

---

## Issue 2: Ages 85-99 Over-Projection

### Findings

**Observed Pattern:**
| Year | ARTEMIS | TR2025 | Excess | % Excess |
|------|---------|--------|--------|----------|
| 2030 | 8.93M | 8.30M | +631K | +7.60% |
| 2050 | 17.74M | 16.89M | +855K | +5.06% |
| 2070 | 20.45M | 19.26M | +1.19M | +6.15% |
| 2099 | 26.88M | 25.94M | +939K | +3.62% |

**Key Finding: Mortality Rates Are Correct**

Verification confirmed our qx values **exactly match** TR2025:
- Using `starting_aax_method: "tr_qx"` bypasses calculations
- Sample (Males, 2030, age 85): 0.088131 ✓
- HMD calibration correctly skipped for tr_qx method

**Root Cause: Population Growth Rate Anomaly**

Despite identical qx values and a **lower** starting population:
- **Ages 85-99 growth 2022→2030:**
  - ARTEMIS: +44.7% growth
  - TR2025: +33.8% growth
  - **Difference: +10.9 percentage points**

Starting with fewer people but ending with more suggests excess accumulation in the projection algorithm.

**Potential Mechanisms:**

1. **Age 100+ Aggregation Effect**
   - Location: `_targets.R:1271` - `aged_pop[age > max_age, age := max_age]`
   - Population pushed to age 100 may create artificial flows

2. **Population Status Disaggregation**
   - Gay/lesbian population ratios at older ages
   - Location: `R/demography/projected_population.R`

3. **Immigration at Older Ages**
   - Net LPR immigration ages 85-99: 7,630/year (0.84% of total)
   - Too small to explain 7.6% excess alone

4. **Rounding/Accumulation Errors**
   - Year-by-year calculation may accumulate small errors
   - 77 years × small positive bias = measurable excess

### Possible Solutions

#### Solution 2A: Compare Intermediate Years (Diagnostic)
**Effort:** Low | **Impact:** Diagnostic

Run detailed comparison for years 2023, 2025, 2027, 2029 to identify when excess accumulates:

```r
for (yr in 2023:2030) {
  compare_age_group(our_pop, tr_pop, ages = 85:99, year = yr)
}
```

#### Solution 2B: Verify Death Calculation Formula
**Effort:** Medium | **Impact:** High

Manually verify the death calculation in `project_population_year()`:

```r
# Expected: deaths = pop * qx
# Check: are we using (1 - qx) correctly for survivors?

# Location: R/demography/projected_population.R
# Look for: survivors <- pop * (1 - qx)
```

**Location:** `R/demography/projected_population.R:270-320`

#### Solution 2C: Review Age 100+ Interaction
**Effort:** Medium | **Impact:** Medium

The aggregation at age 100 may be creating backflow effects:
1. Check if age 99→100 transition preserves correct mortality
2. Verify age 100+ deaths use correct aggregate qx
3. Ensure no double-counting at boundary

**Location:** `R/demography/projected_population.R:139`

#### Solution 2D: Apply Calibration Factor
**Effort:** Low | **Impact:** Medium

If root cause is elusive, apply empirical calibration:
1. Calculate ratio of TR2025/ARTEMIS at ages 85-99 by year
2. Apply as adjustment factor to qx or population
3. Document as known limitation

---

## Issue 3: Age 100+ Volatility

### Findings

**Observed Pattern:**
| Year | ARTEMIS | TR2025 | Difference |
|------|---------|--------|-----------|
| 2023 | 0.02M | 0.08M | -74.81% |
| 2030 | 0.12M | 0.10M | +21.46% |
| 2050 | 0.29M | 0.31M | -5.73% |
| 2099 | 1.36M | 1.22M | +11.22% |

**Root Cause: Historical Population Severely Underestimated**

The 2022 starting population at age 100 is **89.5% too low**:

| Age | ARTEMIS | TR2025 | Difference |
|-----|---------|--------|-----------|
| 100 | 8,021 | 76,702 | -89.5% |
| 99 | 14,489 | 47,194 | -69.3% |
| 98 | 25,092 | 69,681 | -64.0% |
| 95 | 102,370 | 177,166 | -42.2% |

**Technical Root Cause: Flawed Survival-Based Distribution**

The historical population module uses a survival model to distribute the Census 85+ aggregate:

```r
# Location: R/demography/historical_population.R:1343-1432
# Method: Lx[i] = Lx[i-1] * (1 - qx[i-1])
# Problem: Exponential decay assigns only 0.76% to age 100
```

This methodology assumes:
- Pure survival curve distribution
- Ignores actual cohort sizes from historical birth patterns
- Ignores Census administrative records

**Reality:** TR2025 uses actual Census counts showing age 100 = 76,702 (not 8,021).

**Impact on Projections:**

With undersized base population:
- Small absolute changes create large percentage swings
- +5,000 immigrants to 8,000 base = +62.5% swing
- Same flow to 76,000 base = +6.6% swing

This explains the volatility pattern: percentages swing wildly on small base.

### Possible Solutions

#### Solution 3A: Use TR2025 Historical Population for Ages 91-100 (Recommended)
**Effort:** Medium | **Impact:** High

Replace survival-based estimates with TR2025 data for recent years:

```r
# Load TR2025 December population
tr_pop <- fread("data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv")

# For years where TR2025 has data (1940-2024):
# Use TR2025 values directly for ages 91-100
historical_pop[year >= 1940 & age >= 91,
               population := tr_pop_lookup(year, age, sex)]
```

**Files to modify:**
- `R/demography/historical_population.R:1343-1432`
- `_targets.R` (add TR2025 population dependency)

#### Solution 3B: Calibrate Survival Distribution to Census Totals
**Effort:** Medium | **Impact:** Medium

Adjust the survival distribution parameters to match known Census totals:

1. Get Census 100+ total for 2020 (available from Census SF)
2. Back-calculate required survival curve parameters
3. Apply adjusted distribution

**Location:** `R/demography/historical_population.R:1405-1432`

#### Solution 3C: Use ACS/Census Microdata for Oldest Ages
**Effort:** High | **Impact:** High

For years 2005+, ACS provides individual-year ages up to 99:
1. Download ACS PUMS with individual ages
2. Use direct counts for ages 85-99
3. Only apply survival model for age 100+ and pre-2005 years

**Location:** `R/data_acquisition/` (new data source)

#### Solution 3D: Smooth Transition from Known to Estimated
**Effort:** Low | **Impact:** Medium

1. Use TR2025 population for 2022 exactly (known good starting point)
2. Apply our projection forward from correct base
3. Document that historical population pre-2022 may have age 85+ issues

**Implementation:**
```r
# In _targets.R, starting_population target:
starting_pop <- tr_pop[Year == 2022]  # Use TR2025 directly
```

---

## Recommended Action Plan

### Priority 1: Fix Age 100+ Starting Population (Issue 3)
**Impact:** Fixes volatility, partially addresses 85-99 excess

1. Implement Solution 3A: Use TR2025 historical population for ages 91-100
2. Verify 2022 starting population matches TR2025 exactly
3. Re-run projection and measure improvement

### Priority 2: Investigate Birth Deficit (Issue 1)
**Impact:** Fixes young population deficit

1. Run diagnostic comparing our births to TR2025 implied births
2. If deficit confirmed, trace through fertility calculation
3. Consider Solution 1C as fallback (use TR2025 birth targets)

### Priority 3: Diagnose 85-99 Excess (Issue 2)
**Impact:** Fixes older population excess

1. Run Solution 2A to identify when excess accumulates
2. Verify death calculation formula (Solution 2B)
3. If root cause found, fix; otherwise apply calibration (Solution 2D)

---

## Technical References

### Key Files
| File | Relevance |
|------|-----------|
| `R/demography/projected_population.R` | Population projection algorithm |
| `R/demography/historical_population.R:1343-1432` | Age 85+ distribution |
| `R/demography/fertility.R:206-266` | Fertility rate solver |
| `R/demography/mortality.R:493-500` | HMD calibration bypass |
| `R/demography/lpr_immigration.R:53-169` | Immigration age distribution |
| `config/assumptions/tr2025.yaml` | Model assumptions |
| `_targets.R` | Pipeline orchestration |

### TR2025 Data Files
| File | Contents |
|------|----------|
| `SSPopDec_Alt2_TR2025.csv` | December 31 population by age/sex/marital |
| `DeathProbsE_*_Alt2_TR2025.csv` | Death probabilities (qx) by age 0-119 |

### Key Equations
- **Births:** B^z = Σ(f_x × P^z_{x,female}) for ages 14-49
- **Deaths:** D^z_x = q_x × P^{z-1}_x
- **Population:** P^z_x = P^{z-1}_{x-1} × (1 - q_{x-1}) + NI^z_x
- **Age 100+:** P^z_{100+} = P^{z-1}_{100+} × (1 - q_{100+}) + P^{z-1}_{99} × (1 - q_{99}) + NI^z_{100+}

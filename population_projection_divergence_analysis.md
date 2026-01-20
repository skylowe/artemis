

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

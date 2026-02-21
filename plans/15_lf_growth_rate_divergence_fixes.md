# Plan: Fix LF Growth Rate Divergence (4 Causes)

## Context

After fixing the ages 0-15 population bug, 80+ double counting, EDSCORE data flow, and cohort EDSCORE projection, the ARTEMIS labor force projection diverges from TR2025 V.B2 by -28M (2050) to -44M (2100). The gap decomposes into four independent causes:

| Cause | Impact by 2050 | Impact by 2100 | Status |
|-------|----------------|----------------|--------|
| 1. Population deficit (immigration) | -17.1M | -24.1M | Plan only (not implemented now) |
| 2. Young-age LFPR → 0 (trend bug) | -6.1M | -13.5M | **Fix now** |
| 3. Older-age LFPR decline (55+ MSSHARE) | -2.6M | -3.0M | **Fix now** |
| 4. Prime-age 25-54 residual | -2.4M | -3.4M | Addressed by Fix 2 |

**Design constraint:** No legacy fallbacks. Fail early with clear errors when data is missing.

---

## Fix 1 (Cause 1): CNI Population Scaling — PLAN ONLY

### Problem

ARTEMIS CNI 16+ population = 256M vs BLS = 267M (11M gap). This stems from the demography pipeline's V.A2 net immigration (~1.3M/yr) being lower than what TR2025 population projections imply (~2.0M/yr). The gap compounds over time and accounts for 60% of the LF gap by 2050.

### Root Cause

`compute_cni_population()` in `R/economics/employment_inputs.R` (lines 84-151) projects CNI using Eq 2.1.2 applied to SS area population from the demography pipeline. The demography pipeline underestimates immigration, so the population base is too low.

### Recommended Fix: CNI Population Scaling Factor

**File: `config/assumptions/tr2025.yaml`**

Add under `economics.employment`:
```yaml
# Optional scaling factor for CNI population to compensate for known
# immigration deficit in demography pipeline (V.A2 ~1.3M/yr vs TR implied ~2.0M/yr).
# Set to null to use unscaled demography output.
# Value of 1.043 would scale 256M → 267M (matching BLS 2024 CNI 16+).
cni_population_scaling_factor: null
```

**File: `R/economics/employment_inputs.R`** (in `compute_cni_population()`)

After line 144 (`result <- total_pop[...`), add:
```r
scaling_factor <- config_employment$cni_population_scaling_factor
if (!is.null(scaling_factor)) {
  cli::cli_alert_info("Applying CNI population scaling factor: {scaling_factor}")
  result[, population := population * scaling_factor]
}
```

**Alternative approaches (not recommended for now):**
- Use BLS published CNI population directly (requires new data acquisition pipeline)
- Adjust immigration totals in demography config (changes the entire demography pipeline)

---

## Fix 2 (Cause 2): Freeze LFPR Time Trend After Short Range

### Problem

LFPR equations for ages 16-29 include a linear time trend `TR_M`/`TR_F` with negative coefficients. The trend accumulates as `calibrated_tr_base + (year - base_year)`, decreasing LFPR linearly **forever**:

| Age Group | trend_coeff | LFPR 2025 | LFPR 2100 | Impact |
|-----------|-------------|-----------|-----------|--------|
| 16-17 M | -0.01009 | 25.6% | 0% (capped) | -25.6pp |
| 18-19 M | -0.00780 | 52.4% | 0% (capped) | -52.4pp |
| 20-24 M | -0.00310 | 73.9% | 49.2% | -24.8pp |
| 25-29 M | -0.00091 | 89.5% | 82.5% | -7.0pp |

The TR documentation (Section 2.1.c, Input #20) references **"addfactors"** that include "modifying LFPRs for specific age-sex groups for reasonable age-LFPR profile shape." These are unpublished manual corrections by SSA. The documentation also states ultimate economic values are "typically reached during the last half of the short range (first 10 years)."

### Implementation

**Approach:** Freeze the trend contribution after a configurable number of years (default: 10, matching the "short range" from TR documentation). After the freeze year, `trend_val` stops accumulating and holds constant.

**File: `config/assumptions/tr2025.yaml`**

Add under `economics.employment` (after `lfpr_decay_80_plus`):
```yaml
# Number of years after base_year to freeze LFPR time trends.
# After this many years, the trend term stops accumulating and holds constant.
# Set to null to never freeze (trend accumulates forever — not recommended).
# TR2025 documentation states ultimate values are reached "during the last
# half of the short range (first 10 years)."
lfpr_trend_freeze_years: 10
```

**File: `R/economics/us_employment.R`** (in `project_lfpr()`)

Read the config value near the top of `project_lfpr()` (around line 520):
```r
trend_freeze_years <- config_employment$lfpr_trend_freeze_years
```

**Three locations to modify:**

1. **Section 1: Young ages (line 650)**
   ```r
   # Before:
   young_grid[, trend_val := calibrated_tr_base + (year - base_year)]
   # After:
   trend_increment <- year - base_year
   if (!is.null(trend_freeze_years)) trend_increment <- pmin(trend_increment, trend_freeze_years)
   young_grid[, trend_val := calibrated_tr_base + trend_increment]
   ```

2. **Section 2: Male marital ages 20-54 (line 710)**
   ```r
   # Before:
   male_grid[, trend_val := calibrated_tr_base + (year - base_year)]
   # After:
   trend_increment <- year - base_year
   if (!is.null(trend_freeze_years)) trend_increment <- pmin(trend_increment, trend_freeze_years)
   male_grid[, trend_val := calibrated_tr_base + trend_increment]
   ```

3. **Section 3: Female marital×children ages 20-44 (line 814)**
   ```r
   # Before:
   fmc_grid[, trend_val := calibrated_tr_base + (year - base_year)]
   # After:
   trend_increment <- year - base_year
   if (!is.null(trend_freeze_years)) trend_increment <- pmin(trend_increment, trend_freeze_years)
   fmc_grid[, trend_val := calibrated_tr_base + trend_increment]
   ```

Note: `year - base_year` is already a column operation on the grid (vectorized). The `pmin` operates on the full vector. Since `trend_freeze_years` is a scalar and `year` is a column, the computation is: `pmin(young_grid$year - base_year, trend_freeze_years)`.

### Expected Impact

With `lfpr_trend_freeze_years = 10` (base_year = 2024, freeze at 2034):
- Ages 16-17: LFPR stabilizes at ~15% (instead of → 0%)
- Ages 18-19: LFPR stabilizes at ~44% (instead of → 0%)
- Ages 20-24: LFPR stabilizes at ~71% (instead of → 49%)
- Ages 25-29: LFPR stabilizes at ~88% (instead of → 83%)
- Aggregate LF recovery: ~+6M by 2050, ~+13.5M by 2100

This also addresses **Cause 4** (prime-age 25-54 residual), since the 20-29 trend terms account for most of the 25-54 LFPR decline.

---

## Fix 3 (Cause 3): Cap MSSHARE Deviation for Ages 55+

### Problem

The demography pipeline projects married share (MSSHARE) rising from ~0.67 to ~0.87 for males aged 62-64 over 75 years. Combined with large negative LFPR coefficients (age 64: `-1.016 × MSSHARE`), each 1pp increase in married share produces ~1pp decrease in LFPR:

| Age | MSSHARE 2025 | MSSHARE 2100 | Change | msshare_coeff | LFPR impact |
|-----|-------------|-------------|--------|---------------|-------------|
| 62 M | 0.671 | 0.867 | +0.196 | -0.55927 | -11.0pp |
| 63 M | 0.670 | 0.867 | +0.197 | -0.95029 | -18.7pp |
| 64 M | 0.666 | 0.867 | +0.201 | -1.01565 | -20.4pp |

Some female ages show implausible increases (age 69 F: 20.4% → 42.7%) from positive MSSHARE coefficients amplifying the same rising married share.

The LFPR coefficients were estimated from historical data where MSSHARE was relatively stable (~0.55-0.67). A +20pp extrapolation is well outside the estimation range.

### Implementation

**Approach:** Add a configurable maximum deviation from the base-year MSSHARE value. Default to ±0.10 (10pp), which limits the LFPR impact to at most ~10pp at the most sensitive ages while still allowing meaningful demographic evolution.

**File: `config/assumptions/tr2025.yaml`**

Add under `economics.employment` (after `lfpr_trend_freeze_years`):
```yaml
# Maximum allowed deviation of MSSHARE from base-year values for LFPR equations.
# Prevents extrapolation outside the coefficient estimation range.
# Set to null to use uncapped demography projections.
# A value of 0.10 limits MSSHARE change to ±10pp from base year.
msshare_max_deviation: 0.10
```

**File: `R/economics/us_employment.R`** (in `project_lfpr()`, older ages section)

After the MSSHARE merge (line 1019) and before the LFPR computation (line 1058), add capping logic:

```r
# Cap MSSHARE deviation from base year to prevent coefficient extrapolation
msshare_max_dev <- config_employment$msshare_max_deviation
if (!is.null(msshare_max_dev)) {
  # Get base-year MSSHARE values
  base_msshare <- older_grid[year == base_year, .(age, sex, msshare_base = msshare_val)]
  older_grid <- merge(older_grid, base_msshare, by = c("age", "sex"), all.x = TRUE, sort = FALSE)
  # For years before or at base year, no capping needed
  older_grid[!is.na(msshare_base),
             msshare_val := pmin(pmax(msshare_val,
                                       msshare_base - msshare_max_dev),
                                  msshare_base + msshare_max_dev)]
  older_grid[, msshare_base := NULL]
}
```

### Expected Impact

With `msshare_max_deviation = 0.10`:
- Male age 64 MSSHARE capped at 0.67 + 0.10 = 0.77 (instead of 0.87)
- LFPR impact limited to ~10pp (instead of ~20pp)
- Male 64 LFPR stabilizes around ~58% (instead of ~48%)
- Female implausible increases also capped (e.g., age 69 F capped to reasonable range)
- Aggregate 55+ LF recovery: ~+1-2M

---

## Fix 4 (Cause 4): Prime-Age Residual — No Separate Fix Needed

The 25-54 LFPR decline (85.3% → 82.9%) is driven by:
- **20-29 trend terms** (addressed by Fix 2's trend freeze)
- **30-54 marital composition shifts** (legitimate demographic effect; individual age-group LFPRs are stable at 91-94% male, 74-82% female)

After Fix 2, the 20-29 trend contribution stabilizes, eliminating ~80% of this cause. The remaining ~0.5M residual from ages 30-54 is a correct model output reflecting demographic evolution.

---

## Implementation Order

| # | Fix | Files | Complexity |
|---|-----|-------|------------|
| 2 | Trend freeze | `us_employment.R`, `tr2025.yaml` | Low |
| 3 | MSSHARE cap | `us_employment.R`, `tr2025.yaml` | Low |
| 1 | CNI scaling (plan only) | — | — |

Fix 2 first (largest impact), then Fix 3.

---

## Verification

After implementing Fixes 2 and 3:

1. **Pipeline build**: `targets::tar_make()` — all targets build without error

2. **Run comparison**: `Rscript scripts/compare_vb2.R` — check:
   - LFPR 16-17 should stabilize at ~15% (not → 0%)
   - LFPR 20-24 should stabilize at ~71% (not → 49%)
   - Aggregate LFPR should decline less steeply (target ~58-60% long-run vs current 52%)
   - LF growth should be closer to TR: positive near-term instead of negative
   - Male 62-64 LFPR should not decline more than ~10pp from 2025 levels

3. **Spot-check individual ages**:
   - Young ages: verify LFPR holds constant after base_year + 10
   - Ages 62-64 male: verify MSSHARE is capped at base + 0.10
   - Female age 69: verify implausible increase is bounded

4. **No regressions**: All economics targets (EO, TEO, validation) still build and pass

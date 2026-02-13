# Plan: Fix Marriage Subprocess (Subprocess 6) Methodology Deviations from TR2025 Section 1.6

## Context

An audit of the marriage subprocess (`audits/06_marriage_audit.md`) found 11 methodology deviations (1 critical, 4 significant, 4 moderate, 2 minor), ~25 hardcoded values across 14 categories, and 6 silent fallback defaults. The most critical deviation is that the two-dimensional H.S. Beers interpolation -- central to converting age-grouped rates to the single-year 87x87 MarGrid -- performs simple uniform allocation despite Beers coefficients being defined in code.

**Goal:** Fix all feasible deviations, move all hardcoded assumptions to YAML config, remove all silent fallback defaults, and maintain backward compatibility with the `{targets}` pipeline.

**Branch:** `fix/marriage-methodology` (created from `main`)

## Deviations Summary

| # | Deviation | Severity | Fixable? | Phase |
|---|-----------|----------|----------|-------|
| 1.1 | Beers 2D interpolation not implemented (uniform allocation) | Critical | Yes | 1 |
| 1.2 | Ultimate year 2047 instead of 2049 | Significant | Yes | 2 |
| 1.3 | Same-sex marriages not subtracted before historical rate computation | Significant | Yes | 3 |
| 1.4 | Prior status differentials use proportions, not rates | Significant | Yes (partial -- CPS proxy for Input #10) | 4 |
| 1.5 | Same-sex data source (ACS PUMS vs state vital statistics) | Significant | No (data unavailable) | 6 (document) |
| 1.6 | CPS population (Input #14) used instead of MRA population (Input #5) | Moderate | No (data unavailable) | 6 (document) |
| 1.7 | No MRA-to-U.S. total adjustment (Input #7) | Moderate | No (data unavailable) | 6 (document) |
| 1.8 | Convergence exponent hardcoded as 2 | Moderate | Yes | 2 |
| 1.9 | Starting AMR: n_years=5 and equal weights hardcoded | Moderate | Yes | 2 |
| 1.10 | W-H smoothing parameters not configurable | Minor | Yes | 2 |
| 1.11 | Standard population year not configurable | Minor | Yes | 2 |
| 2.1 | SS area factor fallback (1.02) -- 6 occurrences | -- | Yes | 5 |
| 2.2 | Same-sex fraction fallback (0.045, 0.459/0.541) | -- | Yes | 5 |
| 2.11 | Age group definitions duplicated 6 times | -- | Yes | 5 |
| 2.12 | Beers coefficients defined but unused (dead code) | -- | Yes | 1 |
| 2.14 | Validation constant TR2025_ULTIMATE_YEAR = 2047 | -- | Yes | 2 |

## Phase 1: Implement True H.S. Beers Two-Dimensional Interpolation

- Replace `beers_interpolate_1d()` and `beers_interpolate_2d()` with proper Beers implementation
- Pre-process 8 non-standard marriage age groups into ~17 standard 5-year pseudo-groups
- Reuse `get_beers_coefficients()` from `historical_marital_status.R`
- Re-normalize within original groups to preserve group totals
- Remove dead `BEERS_COEFFICIENTS` constant

## Phase 2: Correct Ultimate Year and Move Hardcoded Assumptions to YAML

- Change `ultimate_year` from 2047 to 2049
- Expand YAML config with ~20 new parameters (convergence, smoothing, starting AMR, etc.)
- Add `config` parameter to `run_marriage_projection()` and wire to sub-functions
- Update validation constant in `validate_marriage.R`

## Phase 3: Fix Same-Sex Marriage Handling Order

- Subtract same-sex marriage estimates from NCHS totals BEFORE computing historical rates
- Create `estimate_same_sex_marriages_by_year()` function
- Thread estimates through `calculate_historical_period()`

## Phase 4: Fix Prior Marital Status Differentials to Use Rates

- Add `extract_cps_unmarried_by_prior_status()` to `ipums_cps.R`
- Modify `calculate_prior_status_differentials()` to use rate-based computation
- Add CPS population pipeline target

## Phase 5: Remove Silent Fallback Defaults and Consolidate Duplications

- Replace SS area factor 1.02 fallback with `cli::cli_abort()`
- Move same-sex defaults to config, abort if no config
- Create single canonical `MARGRID_AGE_GROUPS` constant

## Phase 6: Document Missing TR2025 Inputs

- Add methodology deviation documentation block to `marriage.R`
- Add data substitution comments to YAML config

## Phase 7: Pipeline Integration, Validation, and CLAUDE.md Updates

- Add validation checks for Beers interpolation and same-sex subtraction
- Update `CLAUDE.md` known deviations section
- End-to-end pipeline testing

## Unfixable Deviations (Document Only)

| Deviation | Why Unfixable | Mitigation |
|-----------|---------------|------------|
| 1.5: Same-sex data source | State vital statistics (2004-2012) not publicly available | ACS PUMS (2015-2019); configurable reference years |
| 1.6: CPS vs MRA unmarried pop | MRA-specific population not in NBER public archive | CPS national data; config selector for future swap |
| 1.7: MRA-to-national adjustment | MRA state composition by year unavailable | SS area factor partially compensates in 1989-2022 |

# Plan: Configurable Projection Start/End Years

## Context

ARTEMIS currently works for TR2025 with many year references scattered across config, targets, and R functions. Switching to TR2026 (or any future TR) requires manually updating dozens of year values. The goal is to make `trustees_report_year` and `projection_period.end_year` the **master controls** so all dependent years derive automatically — while still allowing explicit overrides for any individual parameter.

The codebase is already ~90% config-driven. This plan fixes the remaining hardcoded spots and adds an automatic derivation layer.

**Branch:** `feature/configurable-projection-years`

---

## Year Derivation Rules

All derived from `TR_YEAR` = `metadata.trustees_report_year` and `END_YEAR` = `metadata.projection_period.end_year`:

| Config Key | Derivation Formula | TR2025 | TR2026 |
|---|---|---|---|
| `metadata.projection_period.start_year` | TR_YEAR - 2 | 2023 | 2024 |
| `metadata.projection_period.end_year` | (independent) | 2099 | 2100/2105 |
| `fertility.projection_start_year` | TR_YEAR | 2025 | 2026 |
| `fertility.ultimate_year` | TR_YEAR + 25 | 2050 | 2051 |
| `fertility.rate_base_year` | TR_YEAR - 1 | 2024 | 2025 |
| `mortality.starting_tr_qx.base_year` | proj_start | 2023 | 2024 |
| `marriage.ultimate_year` | TR_YEAR + 24 | 2049 | 2050 |
| `marriage.acs_end` | TR_YEAR - 3 | 2022 | 2023 |
| `divorce.ultimate_year` | TR_YEAR + 24 | 2049 | 2050 |
| `divorce.historical_end_year` | TR_YEAR - 3 | 2022 | 2023 |
| `historical_population.end_year` | TR_YEAR - 3 | 2022 | 2023 |
| `historical_population.tab_years.final` | [TR_YEAR - 3] | [2022] | [2023] |
| `projected_population.starting_year` | proj_start - 1 | 2022 | 2023 |
| `projected_population.projection_start` | proj_start | 2023 | 2024 |
| `projected_population.projection_end` | END_YEAR | 2099 | 2100 |
| `data_sources.historical_birth_data.end_year` | TR_YEAR - 1 | 2024 | 2025 |
| `data_sources.population_estimates.end_year` | TR_YEAR - 1 | 2024 | 2025 |
| TR file paths (va2_file, qx files, pop files) | Derived from TR_YEAR + alt_num | SSA_TR2025/ | SSA_TR2026/ |

---

## Implementation Steps

### Step 1: Create branch

```bash
git checkout -b feature/configurable-projection-years
```

### Step 2: Add `derive_config_defaults()` to `R/utils/config.R`

**File:** `R/utils/config.R`

Add a new function that runs after YAML parsing but before validation. For each derivable key, apply `%||%` logic: if explicitly set in config, keep it; if NULL/missing, compute from TR_YEAR.

```r
derive_config_defaults <- function(config) {
  tr_year <- config$metadata$trustees_report_year
  if (is.null(tr_year)) return(config)
  alt_num <- config$metadata$alternative_number %||% 2L
  tr_data_dir <- paste0("data/raw/SSA_TR", tr_year)

  # Master projection period
  proj_start <- config$metadata$projection_period$start_year %||% (tr_year - 2L)
  proj_end <- config$metadata$projection_period$end_year %||% (tr_year + 74L)
  config$metadata$projection_period$start_year <- proj_start
  config$metadata$projection_period$end_year <- proj_end

  # Fertility
  fert <- config$fertility
  if (!is.null(fert)) {
    config$fertility$projection_start_year <- fert$projection_start_year %||% tr_year
    config$fertility$ultimate_year <- fert$ultimate_year %||% (tr_year + 25L)
    config$fertility$rate_base_year <- fert$rate_base_year %||% (tr_year - 1L)
  }

  # Marriage
  if (!is.null(config$marriage)) {
    config$marriage$ultimate_year <- config$marriage$ultimate_year %||% (tr_year + 24L)
    config$marriage$acs_end <- config$marriage$acs_end %||% (tr_year - 3L)
  }

  # Divorce
  if (!is.null(config$divorce)) {
    config$divorce$ultimate_year <- config$divorce$ultimate_year %||% (tr_year + 24L)
    config$divorce$historical_end_year <- config$divorce$historical_end_year %||% (tr_year - 3L)
  }

  # Historical population
  if (!is.null(config$historical_population)) {
    config$historical_population$end_year <- config$historical_population$end_year %||% (tr_year - 3L)
    hist_end <- config$historical_population$end_year
    if (is.null(config$historical_population$tab_years$final)) {
      config$historical_population$tab_years$final <- list(hist_end)
    }
  }

  # Projected population
  pp <- config$projected_population
  if (!is.null(pp)) {
    config$projected_population$starting_year <- pp$starting_year %||% (proj_start - 1L)
    config$projected_population$projection_start <- pp$projection_start %||% proj_start
    config$projected_population$projection_end <- pp$projection_end %||% proj_end
    # Note: extended_end is not auto-derived — set explicitly in config if needed
  }

  # Data sources
  ds <- config$data_sources
  if (!is.null(ds)) {
    config$data_sources$historical_birth_data$end_year <-
      ds$historical_birth_data$end_year %||% (tr_year - 2L)
    config$data_sources$population_estimates$end_year <-
      ds$population_estimates$end_year %||% (tr_year - 1L)
  }

  # Mortality starting qx
  if (!is.null(config$mortality$starting_tr_qx)) {
    config$mortality$starting_tr_qx$base_year <-
      config$mortality$starting_tr_qx$base_year %||% proj_start
  }

  # TR file paths
  if (!is.null(config$projected_population)) {
    config$projected_population$tr_historical_population_file <-
      config$projected_population$tr_historical_population_file %||%
      file.path(tr_data_dir, sprintf("SSPopDec_Alt%d_TR%d.csv", alt_num, tr_year))
  }
  if (!is.null(config$immigration)) {
    config$immigration$va2_file <-
      config$immigration$va2_file %||%
      file.path(tr_data_dir, sprintf("SingleYearTRTables_TR%d.xlsx", tr_year))
  }

  config
}
```

**Modify `load_assumptions()`** (line ~18) to call it:
```r
load_assumptions <- function(config_path) {
  ...
  config <- derive_config_defaults(config)  # Add this line before validate_assumptions
  validate_assumptions(config)
  ...
}
```

**Fix `get_projection_years()` fallback defaults** (lines 376, 397):
- Line 376: Change `default = list(start_year = 2023, end_year = 2099)` to derive from config's `metadata$trustees_report_year` if available, else keep numeric fallback
- Line 397: Change `default = 2049` to `default = (config$metadata$trustees_report_year %||% 2025L) + 24L`

**Fix `resolve_tr_file_path()` legacy fallback** (line 332):
- Change `error = function(e) "data/raw/SSA_TR2025"` to derive from config if available

### Step 3: Fix hardcoded years in target files

#### `R/targets/fertility_targets.R`
- **Line 28**: `...start_year:2024` -> `...start_year:config_data_sources$historical_birth_data$end_year`

#### `R/targets/projected_population_targets.R`
- **Lines 73**: Remove `%||% "data/raw/SSA_TR2025/SSPopDec_Alt2_TR2025.csv"` fallback (derive_config_defaults ensures it's set)
- **Lines 97-99**: Replace hardcoded TR2025 life table paths with `resolve_tr_file()` using `config_metadata`; change `end_year = 2099` to `config_metadata$projection_period$end_year`
- **Lines 124-127**: Replace `%||% "data/raw/SSA_TR2025/..."` fallbacks and `%||% 2023` / `%||% 2099` with direct config reads (derive_config_defaults ensures these are set)
- **Line 99**: `end_year = 2099` -> `end_year = config_metadata$projection_period$end_year`

#### `R/targets/immigration_targets.R`
- **Lines 43, 51-52**: Replace hardcoded `"data/raw/SSA_TR2025/..."` paths with `resolve_tr_file()` calls using `config_metadata`
- **Lines 145, 149, 164, 168, 379**: Replace `"data/raw/SSA_TR2025"` fallback with `get_tr_data_dir(list(metadata = config_metadata))`

#### `R/targets/marriage_divorce_targets.R`
- **Line 56**: `years = 1989:2022` -> `years = 1989:config_marriage$acs_end`
- **Line 93**: `years = 2015:2022` -> `years = min(config_marriage$same_sex$reference_years):config_marriage$acs_end`

### Step 4: Fix hardcoded years in Shiny app

#### `app/global.R` (lines 69, 203-204)
- **Line 69**: Derive `DEFAULT_CONFIG_PATH` from `ARTEMIS_CONFIG` env var with fallback
- **Lines 203-204**: Derive `MIN_YEAR` and `MAX_YEAR` from the loaded baseline config:
```r
.baseline_config <- tryCatch(yaml::read_yaml(DEFAULT_CONFIG_PATH), error = function(e) list())
MIN_YEAR <- .baseline_config$projected_population$starting_year %||% 2022L
MAX_YEAR <- .baseline_config$metadata$projection_period$end_year %||% 2099L
MID_YEAR <- .baseline_config$metadata$trustees_report_year %||% 2025L
MID_YEAR <- MID_YEAR + 25L  # e.g., 2050 for TR2025
```

#### `app/R/scenario_engine.R` (lines 246-264)
- Replace `year == 2050` and `year == 2099` with config-derived milestone years
- Add config parameter to the function or read from the scenario config
- Keep `pop_2050`/`pop_2099` field names for backward compat, add `pop_mid`/`pop_end` aliases

#### Shiny UI modules (all use `MIN_YEAR`/`MAX_YEAR` pattern)
Replace all hardcoded year values in sliders/inputs with the global constants:

| File | Lines | Change |
|---|---|---|
| `app/ui.R` | 36-37 | `min=2022, max=2099, value=2050` -> `min=MIN_YEAR, max=MAX_YEAR, value=MID_YEAR` |
| `app/modules/mod_population_viz.R` | 18-19 | Same pattern |
| `app/modules/mod_timeseries_viz.R` | 45-46 | Same pattern |
| `app/modules/mod_comparison_view.R` | 57-58, 110-111 | Same pattern |
| `app/modules/mod_fertility_viz.R` | 33-34, 46-47 | Same pattern |
| `app/modules/mod_mortality_viz.R` | 43-44, 69-70 | Same pattern |
| `app/modules/mod_immigration_viz.R` | 31-32, 44-45 | Same pattern |
| `app/modules/mod_marriage_divorce_viz.R` | 42-43, 62-63 | Same pattern |
| `app/modules/mod_config_editor.R` | 69, 510 | `value = 2050` -> `value = MID_YEAR` |
| `app/modules/mod_scenario_manager.R` | 145-147 | Use config-derived years in display |
| `app/server.R` | 89, 142, 159, 225, 277, 382 | Replace `2050` defaults with `MID_YEAR`, `"2023-2099"` with dynamic string |

#### Epoch labels in viz modules
- `mod_immigration_viz.R` lines 91, 97, 244, 252: Replace `2031-2050`, `2051-2099` with dynamic ranges
- `mod_timeseries_viz.R` lines 334-337: Replace hardcoded epoch ranges
- `mod_marriage_divorce_viz.R` lines 291-292: Replace hardcoded breaks

### Step 5: Add derivation-rule comments to `config/assumptions/tr2025.yaml`

Add comments to each derived key documenting its formula. No structural YAML changes -- existing explicit values remain. Example:

```yaml
fertility:
  projection_start_year: 2025     # Default: TR_YEAR (= 2025)
  ultimate_year: 2050             # Default: TR_YEAR + 25 (= 2050)
  rate_base_year: 2024            # Default: TR_YEAR - 1 (= 2024)
```

### Step 6: Add cross-validation warnings to `validate_assumptions()`

Add warnings (not errors) when derived values seem inconsistent:
- `fertility.projection_start_year != metadata.trustees_report_year`
- `projected_population.starting_year != projection_period.start_year - 1`
- `marriage.ultimate_year` or `divorce.ultimate_year` outside projection range
- `historical_population.end_year >= projection_period.start_year`

---

## Files Modified (Summary)

| File | Changes |
|---|---|
| `R/utils/config.R` | Add `derive_config_defaults()`, modify `load_assumptions()`, fix `get_projection_years()` defaults, fix `resolve_tr_file_path()` fallback |
| `R/targets/fertility_targets.R` | Line 28: remove hardcoded 2024 |
| `R/targets/projected_population_targets.R` | Lines 73, 97-99, 124-129: remove hardcoded paths/years |
| `R/targets/immigration_targets.R` | Lines 43, 51-52, 145, 149, 164, 168, 379: remove hardcoded TR2025 paths |
| `R/targets/marriage_divorce_targets.R` | Lines 56, 93: remove hardcoded year ranges |
| `app/global.R` | Lines 69, 203-204: derive from config |
| `app/R/scenario_engine.R` | Lines 246-264: config-derived milestone years |
| `app/ui.R` | Lines 36-37: use global constants |
| `app/modules/mod_*.R` (8 files) | Replace hardcoded slider min/max/value |
| `app/server.R` | Lines 89, 142, 159, 225, 277, 382: use global constants |
| `R/data_acquisition/tr_data.R` | ~12 functions: change `data_dir` default from hardcoded to NULL |
| `config/assumptions/tr2025.yaml` | Add derivation-rule comments |

**Existing utilities reused:**
- `resolve_tr_file()` at `R/utils/config.R:272` -- pattern-based TR file path resolution
- `get_tr_data_dir()` at `R/utils/config.R:240` -- TR data directory from config
- `get_config_with_default()` at `R/utils/config.R` -- safe nested config access
- `%||%` operator -- null-coalescing throughout

---

### Step 7: Update `R/data_acquisition/tr_data.R` function defaults

About 12 functions have `data_dir = "data/raw/SSA_TR2025"` as a default parameter. Change each to `data_dir = NULL` with internal derivation at the top of the function body:

```r
# Pattern for each function:
some_function <- function(..., data_dir = NULL) {
  if (is.null(data_dir)) data_dir <- "data/raw/SSA_TR2025"  # Legacy fallback for interactive use
  ...
}
```

This removes the hardcoded TR2025 reference from function signatures while preserving backward compatibility. The targets always pass explicit `data_dir` values derived from config, so this change only affects interactive/REPL usage.

**Functions to update** (all in `R/data_acquisition/tr_data.R`):
- `load_tr_population_long()`
- `load_tr_qx_long()`
- `get_tr_va2_net_immigration()`
- `get_tr_lpr_assumptions()`
- `get_tr_historical_lpr()`
- `get_tr_historical_o_flows()`
- `load_tr_period_life_tables()`
- `load_tr_death_probs_hist()`
- `load_tr_life_tables_hist()`
- `load_tr_qx_all_years()`
- `load_tr_starting_population()`
- Any others found with `data_dir = "data/raw/SSA_TR2025"` default

---

## Verification

1. **Baseline preservation**: Run pipeline with current `tr2025.yaml` -> `Rscript -e "targets::tar_make()"` -> verify all 164 targets produce identical results via `Rscript scripts/verify_baseline.R`
2. **Config derivation test**: Create a minimal config with just `trustees_report_year: 2025` and verify `derive_config_defaults()` produces all expected values
3. **Override test**: Set `trustees_report_year: 2026` but keep `fertility.rate_base_year: 2024` -> verify explicit override is preserved
4. **Outdated check with TR2026**: Copy config, set `trustees_report_year: 2026` -> `ARTEMIS_CONFIG=... Rscript -e "targets::tar_outdated()"` -> verify all targets are flagged outdated (no crash during config load)
5. **Shiny app**: Launch dashboard, verify slider ranges match config, verify scenario display uses correct years
6. **Docker**: Rebuild image, verify container starts correctly

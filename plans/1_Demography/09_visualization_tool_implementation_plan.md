# ARTEMIS Interactive Population Projection Visualization Tool

**Phase:** 9 - Interactive Visualization Tool
**Status:** Planning Complete
**Created:** January 23, 2026

## Overview

Build a full-featured R Shiny application for visualizing ARTEMIS population projections with interactive configuration editing and scenario comparison capabilities.

## User Requirements

- **Framework**: R Shiny
- **Deployment**: Local-first (keeping cloud options open)
- **Comparison**: Overlaid lines + difference visualizations
- **Scenarios**: Ability to save and compare named scenarios
- **Scope**: Full feature set

---

## Architecture

### Application Structure

```
app/
├── app.R                     # Main entry point
├── global.R                  # Package loading, baseline data
├── ui.R                      # Main UI assembly
├── server.R                  # Main server orchestration
├── modules/
│   ├── mod_config_editor.R       # Configuration editing interface
│   ├── mod_scenario_manager.R    # Scenario save/load/compare
│   ├── mod_population_viz.R      # Population pyramids
│   ├── mod_timeseries_viz.R      # Time series charts
│   ├── mod_fertility_viz.R       # Fertility visualizations
│   ├── mod_mortality_viz.R       # Mortality curves
│   ├── mod_immigration_viz.R     # Immigration visualizations
│   ├── mod_marriage_divorce_viz.R # Marriage/divorce heatmaps
│   ├── mod_comparison_view.R     # Scenario comparison overlays
│   └── mod_summary_dashboard.R   # Summary statistics
├── R/
│   ├── scenario_engine.R         # Projection execution engine
│   ├── config_utils.R            # Config manipulation
│   ├── data_cache.R              # Caching layer
│   ├── plot_themes.R             # ggplot2 themes
│   └── plot_helpers.R            # Visualization helpers
├── www/
│   └── styles.css                # Custom styling
└── data/
    ├── baseline/                 # Pre-computed TR2025 baseline
    └── scenarios/                # Saved user scenarios
```

### Module Dependency Graph

```
                    ┌─────────────────┐
                    │ mod_config_     │
                    │ editor          │
                    └────────┬────────┘
                             │
                             ▼
                    ┌────────┴────────┐
                    │ scenario_engine │
                    └────────┬────────┘
                             │
        ┌────────────┬───────┴───────┬────────────┐
        │            │               │            │
        ▼            ▼               ▼            ▼
┌───────────┐ ┌───────────┐ ┌───────────┐ ┌───────────┐
│population │ │ fertility │ │ mortality │ │immigration│
│_viz       │ │ _viz      │ │ _viz      │ │ _viz      │
└─────┬─────┘ └─────┬─────┘ └─────┬─────┘ └─────┬─────┘
      │             │             │             │
      └─────────────┴──────┬──────┴─────────────┘
                           │
                           ▼
                    ┌──────┴──────┐
                    │ comparison_ │
                    │ view        │
                    └──────┬──────┘
                           │
                           ▼
                    ┌──────┴──────┐
                    │ scenario_   │
                    │ manager     │
                    └─────────────┘
```

---

## Configuration Editor Design

### Parameter Categories

#### CRITICAL (Always Visible)

| Parameter | Config Path | Input Type | Range |
|-----------|-------------|------------|-------|
| Ultimate TFR | `fertility.ultimate_ctfr` | Slider | 1.0-3.0, step 0.05 |
| Immigration Scenario | `immigration.va2_alternative` | Dropdown | low/intermediate/high |
| Ultimate Marriage Rate | `marriage.ultimate_amr` | Slider | 2000-6000, step 100 |
| Ultimate Divorce Rate | `divorce.ultimate_adr` | Slider | 1000-2500, step 50 |

#### HIGH (Collapsed Accordion)

| Parameter | Config Path | Input Type |
|-----------|-------------|------------|
| Fertility Ultimate Year | `fertility.ultimate_year` | Numeric (2030-2080) |
| Mortality Ultimate Year | `mortality.ultimate_year` | Numeric (2030-2080) |
| Marriage/Divorce Ultimate Year | `marriage.ultimate_year` | Numeric (2030-2080) |
| Emigration Ratio | `immigration.emigration.ratio` | Slider (0.1-0.5) |
| COVID Adjustments | `mortality.covid_adjustments` | Toggle + age-specific sliders |

#### DATA SOURCES (Collapsed Accordion - TR2025 vs ARTEMIS Data)

This section allows users to choose between using official TR2025 reference data or ARTEMIS-computed data for various inputs. This is key for validating ARTEMIS calculations against TR2025 or running fully independent projections.

| Parameter | Config Path | Options | Description |
|-----------|-------------|---------|-------------|
| Starting Population | `projected_population.use_tr_historical_population` | TR2025 / Census-computed | Use TR2025 Dec 31, 2022 population vs ARTEMIS historical population from Census |
| Mortality Starting Values | `mortality.starting_aax_method` | TR2025 qx / Regression / Capped | "tr_qx" uses TR2025 death probabilities; "regression" computes from NCHS data |
| Immigration Age Distribution | `immigration.lpr.distribution_method` | TR-Derived / DHS Historical | "tr_derived" back-calculates from TR2025; "dhs" uses DHS expanded tables |
| Fertility for Recent Years | `fertility.use_tr2025_births_for_years` | TR2025 / NCHS | Substitute TR2025's implied births for 2023-2024 vs use NCHS data |
| TFR Constraints | `fertility.constrain_tfr` | Enable/Disable + values | Force specific TFR values for 2023/2024 to match TR2025 |

**UI Design for Data Sources Panel:**
```r
bslib::accordion_panel(
  "Data Sources (TR2025 vs ARTEMIS)",
  icon = bsicons::bs_icon("database"),

  h5("Choose data sources for each component:"),
  p("Select TR2025 for validation runs, or ARTEMIS-computed for independent projections.",
    class = "text-muted small"),

  radioButtons(ns("use_tr_historical_pop"), "Starting Population (Dec 31, 2022)",
               choices = c("TR2025 Reference" = "true",
                          "ARTEMIS (Census-computed)" = "false"),
               selected = "true"),

  radioButtons(ns("starting_aax_method"), "Mortality Starting Values",
               choices = c("TR2025 Death Probabilities" = "tr_qx",
                          "ARTEMIS (NCHS Regression)" = "regression",
                          "ARTEMIS (Capped)" = "capped"),
               selected = "tr_qx"),

  radioButtons(ns("distribution_method"), "Immigration Age-Sex Distribution",
               choices = c("TR2025-Derived" = "tr_derived",
                          "ARTEMIS (DHS Historical)" = "dhs"),
               selected = "tr_derived"),

  radioButtons(ns("fertility_source"), "Recent Fertility Data (2023-2024)",
               choices = c("TR2025 Implied Births" = "tr2025",
                          "ARTEMIS (NCHS Data)" = "nchs"),
               selected = "tr2025"),

  hr(),
  actionButton(ns("use_all_tr2025"), "Use All TR2025 Data",
               icon = icon("check-double"), class = "btn-outline-primary btn-sm"),
  actionButton(ns("use_all_artemis"), "Use All ARTEMIS Data",
               icon = icon("calculator"), class = "btn-outline-secondary btn-sm")
)

#### ADVANCED (Hidden by Default)

| Parameter | Config Path | Description |
|-----------|-------------|-------------|
| Ultimate AAx Matrix | `mortality.ultimate_aax` | Editable table (5 age groups × 6 causes) |
| Census Undercount | `data_sources.census_undercount_method` | Dropdown (none/DA/PES) |
| Weight Exponent | `fertility.weight_exponent` | Numeric (0.5-3.0) |
| Population Status % | `projected_population.population_status` | Gay/lesbian percentages |
| Elderly Immigration Override | `immigration.lpr.elderly_override_tr_derived` | Complex nested controls |

### Config Editor UI Structure

```r
mod_config_editor_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Scenario name header
    textInput(ns("scenario_name"), "Scenario Name", "Custom Scenario"),
    actionButton(ns("reset_defaults"), "Reset to TR2025"),

    # CRITICAL parameters - always visible
    wellPanel(
      h4("Core Assumptions"),
      sliderInput(ns("ultimate_ctfr"), "Ultimate TFR", 1.0, 3.0, 1.90, 0.05),
      selectInput(ns("va2_alternative"), "Immigration Scenario",
                  c("Low" = "low", "Intermediate" = "intermediate", "High" = "high")),
      sliderInput(ns("ultimate_amr"), "Ultimate Marriage Rate", 2000, 6000, 4000, 100),
      sliderInput(ns("ultimate_adr"), "Ultimate Divorce Rate", 1000, 2500, 1700, 50)
    ),

    # HIGH priority - accordion panels
    bslib::accordion(
      bslib::accordion_panel("Timeline Settings", ...),
      bslib::accordion_panel("Mortality Methodology", ...),
      bslib::accordion_panel("Immigration Methodology", ...)
    ),

    # ADVANCED - conditionally shown
    checkboxInput(ns("show_advanced"), "Show Advanced Settings"),
    conditionalPanel(...),

    # Action buttons
    actionButton(ns("run_projection"), "Run Projection", class = "btn-primary"),
    actionButton(ns("save_scenario"), "Save Scenario"),
    downloadButton(ns("export_config"), "Export YAML")
  )
}
```

---

## Visualization Components

### 1. Population Pyramids (`mod_population_viz.R`)

**Features:**
- Horizontal bar chart (male left, female right)
- Age 0-100 on Y-axis (single year or 5-year groups)
- Year selector (2022-2099)
- Optional marital status overlay (stacked colors: single, married, divorced, widowed)
- Animation capability over time using gganimate
- Export as PNG/SVG

**Data Source:** `projected_population`, `projected_marital_population`

### 2. Time Series (`mod_timeseries_viz.R`)

**Metrics Available:**
- Total Population
- Annual Births
- Annual Deaths
- Net Immigration (LPR + O)
- Total Fertility Rate
- Life Expectancy at Birth (e0)
- Life Expectancy at 65 (e65)
- Old-Age Dependency Ratio
- Young Dependency Ratio
- Median Age

**Features:**
- Breakdown: Total, By Sex, By Marital Status
- Year range slider (2022-2099)
- Include historical data toggle
- Logarithmic scale option
- Interactive tooltips via plotly

**Data Source:** `projected_population`, `projected_births`, `projected_deaths`, `projected_net_immigration`

### 3. Mortality Curves (`mod_mortality_viz.R`)

**Tabs:**
1. **qx Curves**: Death probabilities by age for selected years
   - Multi-year selection
   - Sex filter (male/female/both)
   - Log scale option
   - Age range filter

2. **Life Expectancy Trends**: ex over time
   - Multiple starting ages (0, 20, 40, 65, 85)
   - Sex comparison

3. **AAx Improvement Rates**: Heatmap
   - Age groups × causes of death
   - Color-coded improvement rates

**Data Source:** `mortality_qx_projected`, life table calculations

### 4. Fertility Curves (`mod_fertility_viz.R`)

**Tabs:**
1. **Age-Specific Rates (ASFR)**: Fertility rates by age (14-49)
   - Multi-year overlay
   - Historical comparison

2. **TFR Time Series**: Total fertility rate over time
   - Historical + projected
   - Ultimate TFR reference line

3. **Cohort Fertility**: CTFR by birth cohort

**Data Source:** `fertility_rates_complete`

### 5. Immigration Visualizations (`mod_immigration_viz.R`)

**Features:**
- Net immigration by age-sex distribution
- LPR vs O immigration breakdown
- Time series of total net immigration
- Immigration scenario comparison (low/intermediate/high)

**Data Source:** `projected_net_immigration`, `net_lpr_immigration`, `net_o_for_projection`

### 6. Marriage/Divorce Grids (`mod_marriage_divorce_viz.R`)

**Tabs:**
1. **Marriage Grid**: 87×87 heatmap (husband age × wife age)
   - Year selector
   - Type filter (total/opposite-sex/same-sex)
   - Age range zoom

2. **Divorce Grid**: 87×87 heatmap
   - Year selector
   - Age range zoom

3. **AMR/ADR Time Series**: Age-adjusted rates over time

**Data Source:** `marriage_projection`, `divorce_projection`

### 7. Dependency Ratios (`mod_dependency_viz.R`)

**Features:**
- Configurable age thresholds
- Young dependency: (0-17) / (18-66)
- Old-age dependency: (67+) / (18-66)
- Total dependency ratio
- Key statistics table

**Data Source:** `projected_population`

---

## Scenario Management System

### Scenario Data Structure

```r
scenario <- list(
  metadata = list(
    name = "High Fertility Scenario",
    description = "Tests impact of TFR reaching 2.1 (replacement level)",
    created = Sys.time(),
    modified = Sys.time(),
    author = "User",
    baseline = "tr2025_intermediate"
  ),

  config = list(
    # Modified YAML config as nested list
    # Stores full config (not just differences)
    fertility = list(ultimate_ctfr = 2.1, ...),
    mortality = list(...),
    immigration = list(...),
    ...
  ),

  results = list(
    # Cached projection outputs (data.tables)
    population = ...,
    births = ...,
    deaths = ...,
    net_immigration = ...,
    marital_population = ...,
    fertility_rates = ...,
    mortality_qx = ...
  ),

  checksum = "md5_hash_of_config"  # For cache invalidation
)
```

### Scenario Manager Module (`mod_scenario_manager.R`)

**Features:**
- List all saved scenarios with metadata
- Load scenario (populates config editor + results)
- Save current scenario
- Duplicate scenario for modification
- Delete scenario
- Import scenario from RDS file
- Export scenario to RDS file
- Export config as YAML

### Comparison View Module (`mod_comparison_view.R`)

**Comparison Modes:**

1. **Overlaid Time Series**
   - Multiple scenarios as colored lines on same chart
   - Color-coded legend with scenario names
   - All time series metrics available

2. **Difference from Baseline**
   - Absolute difference (Scenario - Baseline)
   - Percentage difference ((Scenario - Baseline) / Baseline × 100)
   - Toggle between absolute and percentage

3. **Population Pyramid Comparison**
   - Side-by-side pyramids
   - Overlaid pyramids (outline vs filled)
   - Year selector

4. **Summary Table**
   - Key metrics comparison across all selected scenarios
   - Columns: Metric, Baseline, Scenario 1, Scenario 2, ...
   - Rows: 2099 Population, Life Expectancy, TFR, etc.

---

## Data Flow and Integration

### Integration with Targets Pipeline

```r
# scenario_engine.R

execute_scenario <- function(config_list, scenario_id, use_cache = TRUE) {
  # 1. Check cache
  cache_file <- file.path("app/data/scenarios", paste0(scenario_id, "_results.rds"))
  if (use_cache && file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    if (cached$checksum == digest::digest(config_list)) {
      return(cached$results)
    }
  }

  # 2. Write temporary config
  temp_config <- tempfile(fileext = ".yaml")
  yaml::write_yaml(config_list, temp_config)

  # 3. Set environment variable
  withr::local_envvar(ARTEMIS_CONFIG = temp_config)

  # 4. Run targets
  targets::tar_make(
    names = c(
      "projected_population",
      "projected_births",
      "projected_deaths",
      "projected_net_immigration",
      "projected_marital_population",
      "fertility_rates_complete",
      "mortality_qx_projected"
    ),
    callr_function = NULL  # Same process for Shiny
  )

  # 5. Load and cache results
  results <- list(
    population = targets::tar_read(projected_population),
    births = targets::tar_read(projected_births),
    ...
  )

  saveRDS(
    list(results = results, checksum = digest::digest(config_list)),
    cache_file
  )

  results
}
```

### Caching Strategy

| Cache Type | Location | Purpose |
|------------|----------|---------|
| Baseline | `app/data/baseline/` | Pre-computed TR2025 intermediate results |
| Scenario | `app/data/scenarios/` | User scenario results (by config hash) |
| Session | Reactive values | Current session data for fast UI |

### Cache Invalidation

- MD5 hash of config determines if recomputation needed
- If config unchanged, load cached results
- If config changed, invalidate and rerun pipeline

---

## Package Dependencies

### New Packages to Install

```r
# Core Shiny
shiny >= 1.8.0
bslib >= 0.6.0         # Bootstrap 5 theming
bsicons >= 0.1.2       # Bootstrap icons
shinyWidgets >= 0.8.0  # Enhanced widgets

# Visualization
ggplot2 >= 3.4.0
plotly >= 4.10.0       # Interactive plots
gganimate >= 1.0.8     # Animated plots (optional)
scales >= 1.3.0
viridis >= 0.6.0       # Color palettes
patchwork >= 1.2.0     # Plot composition

# Tables
DT >= 0.30             # Interactive tables
rhandsontable >= 0.3.8 # Editable tables for AAx matrix

# Utilities
shinyjs >= 2.1.0       # JavaScript utilities
waiter >= 0.2.5        # Loading indicators
digest >= 0.6.33       # Hashing for cache
withr >= 2.5.0         # Temporary environment

# Testing
shinytest2 >= 0.3.1    # Shiny testing
```

### Installation Script

```r
# scripts/install_shiny_deps.R
packages <- c(
  "shiny", "bslib", "bsicons", "shinyWidgets",
  "ggplot2", "plotly", "gganimate", "scales", "viridis", "patchwork",
  "DT", "rhandsontable",
  "shinyjs", "waiter", "digest", "withr",
  "shinytest2"
)
renv::install(packages)
renv::snapshot()
```

---

## Critical Files Reference

| File | Purpose |
|------|---------|
| `config/assumptions/tr2025.yaml` | Configuration format template |
| `R/utils/config.R` | `load_assumptions()`, `validate_assumptions()` |
| `R/targets/projected_population_targets.R` | Pipeline target definitions |
| `R/demography/projected_population.R` | Output data structures |
| `_targets.R` | Pipeline orchestration |

---

## Implementation Tasks

### Phase 1: Foundation
- [ ] Create `app/` directory structure
- [ ] Run `scripts/install_shiny_deps.R` and update renv
- [ ] Create `app/global.R` with package loading and ARTEMIS integration
- [ ] Create `app/ui.R` with bslib navigation (sidebar + tabbed main panel)
- [ ] Create `app/server.R` orchestration shell
- [ ] Implement `app/R/scenario_engine.R` for running projections
- [ ] Implement `app/modules/mod_config_editor.R` (CRITICAL parameters only)
- [ ] Pre-compute and save baseline TR2025 results to `app/data/baseline/`

### Phase 2: Core Visualizations
- [ ] Create `app/R/plot_themes.R` with ARTEMIS ggplot2 theme
- [ ] Create `app/R/plot_helpers.R` with common visualization functions
- [ ] Implement `app/modules/mod_population_viz.R` (population pyramids)
- [ ] Implement `app/modules/mod_timeseries_viz.R` (generic time series)
- [ ] Implement `app/modules/mod_summary_dashboard.R` (key statistics)
- [ ] Wire up modules to scenario engine results

### Phase 3: Detailed Visualizations
- [ ] Implement `app/modules/mod_fertility_viz.R`
- [ ] Implement `app/modules/mod_mortality_viz.R`
- [ ] Implement `app/modules/mod_immigration_viz.R`
- [ ] Implement `app/modules/mod_marriage_divorce_viz.R`
- [ ] Add dependency ratio calculations and visualization

### Phase 4: Scenario System
- [ ] Implement `app/modules/mod_scenario_manager.R`
- [ ] Implement scenario save/load to `app/data/scenarios/`
- [ ] Implement `app/modules/mod_comparison_view.R`
- [ ] Add overlaid line comparison
- [ ] Add difference visualization (absolute + percentage)
- [ ] Add summary comparison table

### Phase 5: Polish
- [ ] Complete config editor (HIGH parameters in accordion)
- [ ] Complete config editor (ADVANCED parameters with AAx table)
- [ ] Add input validation with informative error messages
- [ ] Add loading indicators with waiter package
- [ ] Performance optimization for large datasets
- [ ] Create `app/www/styles.css` for custom styling
- [ ] Write shinytest2 automated tests
- [ ] Create user documentation

---

## Verification Plan

### Manual Testing Checklist

1. **Launch**: `shiny::runApp("app")` starts without errors
2. **Baseline Load**: TR2025 baseline data displays correctly on startup
3. **Config Editor**:
   - Each CRITICAL parameter slider updates reactively
   - Accordion panels expand/collapse correctly
   - Advanced settings toggle works
4. **Projection Run**:
   - "Run Projection" button triggers pipeline execution
   - Loading indicator displays during computation
   - Results populate all visualization modules
5. **Visualizations**:
   - Population pyramid displays correctly for all years
   - Time series shows all metrics with correct scales
   - Mortality/fertility curves render properly
   - Marriage/divorce heatmaps display
6. **Scenario Management**:
   - Save scenario creates file in `app/data/scenarios/`
   - Load scenario restores config and results
   - Export YAML produces valid configuration file
7. **Comparison**:
   - Overlaid lines show multiple scenarios correctly
   - Difference view calculates correctly (verify with manual calculation)
   - Summary table displays all scenarios

### Automated Testing

```r
# Run all Shiny tests
shinytest2::test_app("app")

# Test specific modules
testthat::test_file("app/tests/testthat/test-mod_config_editor.R")
```

### Validation Points

- [ ] Config editor produces YAML matching `tr2025.yaml` format
- [ ] Scenario engine correctly sets `ARTEMIS_CONFIG` env var
- [ ] Cached results match fresh projection results (hash comparison)
- [ ] Comparison difference calculations verified manually
- [ ] All visualizations handle edge cases (single year, empty data)

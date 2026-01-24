# ARTEMIS Interactive Population Projection Visualization Tool

An R Shiny application for visualizing ARTEMIS population projections with interactive configuration editing and scenario comparison.

## Quick Start

```r
# From the project root directory
setwd("path/to/artemis")

# Install required packages (first time only)
source("app/install_dependencies.R")

# Run the application
shiny::runApp("app")
```

## Prerequisites

1. **Run the ARTEMIS pipeline first** to generate baseline data:
   ```r
   targets::tar_make()
   ```

2. **Install required packages** (if not already installed):
   ```r
   source("app/install_dependencies.R")
   ```

## Features

### Dashboard
- Population trend visualization
- Interactive population pyramids
- Components of change (births, deaths, immigration)
- Dependency ratio tracking

### Population Tab
- Animated population pyramids by year
- Single-year vs 5-year age grouping
- Optional marital status overlay
- Comparison to baseline

### Fertility Tab
- TFR time series
- Age-specific fertility rates (ASFR)
- 3D rate surface visualization
- Cohort TFR tracking

### Mortality Tab
- Life expectancy trends (e0, e65, e85)
- Death probability curves (qx)
- Mortality surface visualization
- Improvement rate analysis (AAx)

### Immigration Tab
- Net immigration trends
- LPR vs O population components
- Age-sex distribution analysis

### Marriage/Divorce Tab
- AMR/ADR rate trends
- Rate grid heatmaps (husband age × wife age)
- Age pattern analysis
- Marital status flow

### Scenarios Tab
- Configure projection assumptions
- Run custom projections
- Save and load scenarios
- Export configuration as YAML

### Compare Tab
- Side-by-side scenario comparison
- Absolute and percent difference views
- Population pyramid comparisons
- Summary statistics table

## Configuration Parameters

### Critical Parameters (Always Visible)
| Parameter | Config Path | Range |
|-----------|-------------|-------|
| Ultimate TFR | `fertility.ultimate_ctfr` | 1.0-3.0 |
| Immigration Scenario | `immigration.va2_alternative` | low/intermediate/high |
| Ultimate Marriage Rate | `marriage.ultimate_amr` | 2000-6000 |
| Ultimate Divorce Rate | `divorce.ultimate_adr` | 1000-2500 |

### High Priority Parameters
- Fertility/Mortality ultimate years
- Starting AAx method
- Immigration distribution method
- Emigration ratio

### Data Source Toggles
- Starting population: TR2025 vs Census
- Recent births: TR2025 vs NCHS
- Mortality starting point: TR qx vs NCHS-computed

## Directory Structure

```
app/
├── app.R                   # Main entry point
├── global.R                # Package loading, baseline data
├── ui.R                    # Main UI assembly
├── server.R                # Main server orchestration
├── install_dependencies.R  # Package installation script
├── modules/
│   ├── mod_config_editor.R
│   ├── mod_scenario_manager.R
│   ├── mod_population_viz.R
│   ├── mod_timeseries_viz.R
│   ├── mod_fertility_viz.R
│   ├── mod_mortality_viz.R
│   ├── mod_immigration_viz.R
│   ├── mod_marriage_divorce_viz.R
│   └── mod_comparison_view.R
├── R/
│   ├── scenario_engine.R   # Projection execution
│   ├── config_utils.R      # Configuration helpers
│   ├── data_cache.R        # Caching layer
│   └── plot_themes.R       # Visualization styling
├── www/
│   └── styles.css          # Custom CSS
└── data/
    ├── baseline/           # Pre-computed baseline
    └── scenarios/          # Saved scenarios
```

## Saved Scenarios

Scenarios are saved as RDS files in `app/data/scenarios/` with the following structure:

```r
scenario <- list(
  metadata = list(
    name = "High Fertility",
    description = "TFR reaches 2.1",
    created = Sys.time(),
    baseline = "tr2025_intermediate"
  ),
  config = list(...),   # Modified configuration
  results = list(...),  # Cached projection outputs
  checksum = "md5_hash" # For cache validation
)
```

## Troubleshooting

### "No baseline data loaded"
Run the ARTEMIS pipeline first:
```r
targets::tar_make()
```

### Missing packages
Run the installation script:
```r
source("app/install_dependencies.R")
```

### Slow performance
- Use 5-year age groups instead of single-year
- Limit year ranges in time series
- Clear old cached scenarios in `app/data/scenarios/`

## Development

### Adding a New Visualization Module

1. Create `modules/mod_yourmodule_viz.R` with UI and server functions
2. Add module UI call to `ui.R`
3. Add module server call to `server.R`
4. Source the module file in `app.R`

### Modifying the Theme

Edit `R/plot_themes.R` for ggplot2 styling and `www/styles.css` for Shiny UI styling.

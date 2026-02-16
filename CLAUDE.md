# ARTEMIS - OASDI Projection Model

## Project Overview
R-based replication of the SSA Office of the Chief Actuary's long-range OASDI projection model, targeting the 2025 Trustees Report (TR2025) intermediate assumptions. Projects the Social Security area population from 2022 through 2099 using the component method (births, deaths, immigration).

**Stack:** R 4.5, `{targets}` pipeline, `{renv}` dependencies, `{bslib}` Shiny dashboard, Docker/JupyterHub deployment.

## Current Status
- **Demography process:** COMPLETE (all 8 subprocesses)
- **Next:** Economics process (not started)
- **Pipeline:** ~140 targets, runs in ~60 seconds
- **Validation:** All checks pass; population within ~1.2% of TR2025 projections

### Key Projection Outputs (Pure NCHS Baseline)
| Metric | 2022 | 2050 | 2099 |
|--------|------|------|------|
| Population | 340.4M | 388.1M | 445.1M |
| TFR | 1.607 | 1.900 | 1.900 |
| Births/yr | - | 3.9M | 4.5M |
| Deaths/yr | - | 3.8M | 9.0M |

## Project Structure

```
R/
  demography/          # Core projection functions (15 files)
  data_acquisition/    # API fetching and data loading (27 files)
  targets/             # {targets} pipeline definitions (8 files)
  validation/          # Validation helpers (6 files)
  utils/               # Shared utilities
app/                   # Shiny dashboard
  modules/             # 9 visualization/config modules
  global.R, ui.R, server.R
config/
  assumptions/tr2025.yaml  # All projection assumptions
data/
  raw/SSA_TR2025/      # Official TR2025 data files (gitignored, bind-mounted in Docker)
  cache/               # Downloaded data cache (gitignored)
  baseline/tr2025/     # Baseline snapshot .rds files (gitignored)
docker/
  start.sh             # Container startup script
  jupyter_shiny_proxy.py
plans/                 # Implementation plan documents (01-08)
documentation/         # TR2025 methodology docs (markdown + PDF)
scripts/               # Utility scripts (baseline snapshot, verification)
```

## Demography Subprocesses

All 8 subprocesses are complete and integrated:

1. **Fertility** (`R/demography/fertility.R`) - 10-step TR2025 methodology. NCHS births / Census population. Projects age-specific birth rates to ultimate CTFR of 1.90.
2. **Mortality** (`R/demography/mortality.R`) - Age-specific qx from NCHS deaths. HMD calibration for 85+. COVID adjustments. Marital differentials. Projections to 2099.
3. **LPR Immigration** (`R/demography/lpr_immigration.R`, `legal_emigration.R`) - DHS/CBO data. TR2025 V.A2 totals. TR-derived age-sex distribution.
4. **Historical Population** (`R/demography/historical_population.R` + 3 related files) - SS area population 1940-2022 by age/sex/marital status. Census/ACS/IPUMS data.
5. **Temp/Unlawful Immigration** (`R/demography/temp_unlawful_immigration.R` + 3 related files) - Non-LPR immigration, DACA, O-population stock projections.
6. **Marriage** (`R/demography/marriage.R`) - 87x87 MarGrid. Ultimate AMR 4,000 per 100K by 2049.
7. **Divorce** (`R/demography/divorce.R`) - 87x87 DivGrid. Ultimate ADR 1,700 per 100K by 2047.
8. **Projected Population** (`R/demography/projected_population.R`) - Component method integration. Phases 8A-8F: core projection, marital status, children fate, CNI population.

## Configuration

All assumptions live in `config/assumptions/tr2025.yaml`. Key configurable parameters:

| Parameter | Config Path | Default | Description |
|-----------|------------|---------|-------------|
| Ultimate TFR | `fertility.ultimate_ctfr` | 1.90 | Target cohort TFR |
| Custom Recent TFR | `fertility.custom_recent_tfr` | null | Override TFR for specific years (e.g., `{2023: 1.62, 2024: 1.62}`) |
| Immigration Scenario | `immigration.va2_alternative` | intermediate | low/intermediate/high from V.A2 |
| Ultimate Marriage Rate | `marriage.ultimate_amr` | 4000 | Per 100K unmarried |
| Ultimate Divorce Rate | `divorce.ultimate_adr` | 1700 | Per 100K married |
| Starting Population Source | `projected_population.use_tr_historical_population` | false | Use TR2025 files vs ARTEMIS-computed |
| Immigration Distribution | `immigration.lpr.distribution_method` | tr_derived | tr_derived vs dhs |

### Environment Variable
- `ARTEMIS_CONFIG`: Path to assumptions config file (default: `config/assumptions/tr2025.yaml`)

### Switching Trustees Report Years
1. Place new TR data files in `data/raw/SSA_TR{year}/`
2. Copy `config/assumptions/tr2025.yaml` to new file, update `metadata.trustees_report_year` and assumptions
3. Run: `ARTEMIS_CONFIG=config/assumptions/tr2026.yaml Rscript -e "targets::tar_make()"`

## Running the Pipeline

**Important:** The `config_assumptions` target is cached by `{targets}`. After editing `config/assumptions/tr2025.yaml`, you must invalidate it before running the pipeline or the old config values will be used:

```bash
# After editing config YAML, invalidate the config target first
Rscript -e "targets::tar_invalidate(matches('config_assumptions'))"

# Full pipeline
Rscript -e "targets::tar_make()"

# Check what's outdated
Rscript -e "targets::tar_outdated()"

# Invalidate specific targets (forces rebuild)
Rscript -e "targets::tar_invalidate(matches('fertility'))"

# Read a target result
Rscript -e "targets::tar_read(fertility_rates_complete)"
```

### Baseline Snapshots
```bash
# Create snapshot of current pipeline outputs
Rscript scripts/create_baseline_snapshot.R

# Verify outputs match snapshot after changes
Rscript scripts/verify_baseline.R
```

Baseline files are saved to `data/baseline/tr2025/` (gitignored, ~90MB of .rds files).

## Docker / JupyterHub Deployment

### Architecture
- **JupyterHub** runs as a systemd service (`/etc/systemd/system/jupyterhub.service`)
- **Config:** `/etc/jupyterhub/jupyterhub_config.py`
- **DockerSpawner** launches per-user containers from the `artemis:latest` image
- Each container runs Shiny on port 3838 (proxied via jupyter-server-proxy) and JupyterLab on 8888
- Users land on the Shiny dashboard by default (`/proxy/3838/`)
- Idle containers are culled after 1 hour

### Volume Mounts (configured in jupyterhub_config.py)
| Host Path | Container Path | Mode |
|-----------|---------------|------|
| `/home/jupyterhub/users/{username}` | `/home/artemis/persist` | rw |
| `projects/artemis/data/raw` | `/home/artemis/project/data/raw` | ro |
| `projects/artemis/data/cache` | `/home/artemis/project/data/cache` | ro |
| `projects/artemis/_targets` | `/home/artemis/baseline_targets` | ro |
| `~/.Renviron` | `/home/artemis/.Renviron` | ro |

The container startup script (`docker/start.sh`) copies baseline `_targets` from the read-only mount into the user's writable persist volume on each start, ensuring a clean baseline.

### Updating the Dashboard After Code Changes

After making changes to R source code, config, or Shiny app files:

```bash
# 1. Build the new Docker image
docker build -t artemis:latest /home/skylowe/projects/artemis

# 2. Remove the existing user container (JupyterHub will respawn with new image)
docker rm -f jupyter-skylowe

# 3. Visit the JupyterHub URL to trigger a new container spawn
#    The new container will use the updated image
```

If the `_targets` cache also needs updating (e.g., after changing config defaults):

```bash
# Run the pipeline on the host first
Rscript -e "targets::tar_make()"

# Then rebuild the image and remove the container (steps 1-2 above)
# The new container will pick up the updated _targets via the bind mount
```

If JupyterHub itself needs restarting:

```bash
sudo systemctl restart jupyterhub
```

### Important Notes
- The Docker image bakes in R source code, config, Shiny app, and renv packages
- Raw data and `_targets` cache are **bind-mounted**, not baked into the image
- After rebuilding the image, you must remove the old container for changes to take effect
- The container name follows the pattern `jupyter-{username}` (e.g., `jupyter-skylowe`)
- JupyterHub's idle culler removes containers after 1 hour of inactivity

## Known Methodology Deviations from TR2025

1. **Fertility Step 1 (2024):** Uses actual NCHS preliminary data (TFR=1.602) instead of TR estimate (1.62). Configurable via `custom_recent_tfr`.
2. **Mortality ages 65+:** Uses NCHS deaths / Census population instead of CMS Medicare data. CMS data is not publicly available in the single-year-of-age form SSA uses (requires ResDAC/CCW restricted access). HMD calibration at ages 85+ partially compensates (configurable via `hmd_calibration.enabled`).
3. **Immigration distribution:** TR-derived age-sex distribution back-calculated from TR2025 projections instead of unpublished Census data.
4. **Immigration totals:** V.A2 net immigration totals (~1.3M/yr) are lower than what TR2025 population projections imply (~2.0M/yr), causing ~1.2% population divergence.
5. **Marriage same-sex data (Input #15):** Same-sex separation uses ACS PUMS prevalence (2015-2022) instead of state vital statistics (2004-2012). Configurable via `marriage.same_sex.reference_years`.
12. **Marriage MRA unmarried population (Input #5):** Uses CPS national unmarried population instead of MRA-specific data (not in NBER public archive). SS area factor partially compensates.
13. **Marriage MRA-to-national adjustment (Input #7):** MRA state composition by year unavailable; adjustment omitted. SS area factor partially compensates.
14. **Marriage prior status population (Input #10):** Uses CPS national unmarried population by prior status instead of MRA-specific data. Configurable via `marriage.prior_status.population_source`.
6. **Divorce DivGrid adjustment (Input #10):** ACS PUMS data (2018-2022) for DivGrid adjustment instead of 18-state health department data (~2009-2012). ACS provides only marginal age distributions (cannot link ex-spouses), so geometric-mean ratioing substitutes for TR2025's cell-level ratioing. Adjustment year is 2020 (ACS midpoint) vs TR2025's 2011, lengthening the transition from 23 to 32 years. Configurable via `divorce.acs_years` and `divorce.adjustment_year`.
15. **Divorce standard population (Input #1):** TR2025 specifies grids "from the 2015 TR." ARTEMIS computes from Census/ACS data; values may differ slightly. Configurable via `divorce.standard_population_years`.
16. **Divorce PR/VI gap-year estimation:** TR2025 provides PR/VI data for 1988, 1998-2000, and 2008-2022. For gap years (1989-1997, 2001-2007), ARTEMIS interpolates using exponential decay from the 1988 anchor. Configurable via `divorce.pr_vi.*`.
7. **O-population stock (Eq 1.4.3):** TR2025 builds stock directly from residuals, then modifies stock levels. ARTEMIS uses V.A2 o_net totals for annual stock levels and residuals only for age-sex distribution shape. This is because ARTEMIS component inputs (immigration distributions, AOS ratios) differ from OCACT's internal data, causing residual-built stocks to diverge pre-2000. Configurable via `historical_population.o_population.method` (`"residual"` default, `"va2_flows"` alternative).
8. **ACS undercount (O immigration):** Uses single calibrated factor per age group instead of TR2025's three-component model (ACS-DHS gap, PUMS-Census gap, PR foreign-born). Individual components not published by SSA. Configurable via `immigration.o_immigration.acs_undercount_factors`.
9. **O departure rates:** Approximated via multiplicative adjustments on a single base rate schedule instead of TR2025's separate rate tables from the 2014 TR stock build-up. Actual SSA internal rates (Inputs #26-30) not published. Configurable via `immigration.o_immigration.departure_rates`.
10. **DHS admissions NI calibration:** Not implemented (placeholder). I-94 detail data at required age/sex granularity not publicly available.
11. **NI "other in" factor:** Not implemented (placeholder). DHS parolee/EWI data not available at required granularity.

## Key Rules
1. **Real data only** - No synthetic/mock data. Tasks are not complete until working with real API/file data.
2. **Validation required** - All outputs must validate against official TR2025 tables.
3. **Config-driven** - Assumptions belong in `config/assumptions/tr2025.yaml`, not hardcoded.

## API Keys
Stored in `.Renviron` (gitignored): Census, BEA, BLS, FRED, CDC. See file for full list.

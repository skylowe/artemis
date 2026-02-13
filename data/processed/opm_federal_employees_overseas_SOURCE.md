# OPM Federal Employees Overseas — Data Provenance

## Description
Total federal civilian employees stationed in foreign countries, by year.

## Sources

### Primary: CRS Report R43590, Table 2
- **2009-2015**: Congressional Research Service (2016). "Federal Workforce Statistics Sources: OPM and OMB." CRS Report R43590. Table 2: Federal Civilian Employees On-Board Personnel, by Location. https://www.everycrsreport.com/files/20161207_R43590
- **2016-2018**: Congressional Research Service (2020). CRS Report R43590 (updated). https://www.everycrsreport.com/files/20200325_R43590

### Secondary: Pew Research Center
- **2024**: Pew Research Center (2025). "What the data says about federal workers." Analysis of March 2024 OPM FedScope data showing ~30,800 overseas employees. https://www.pewresearch.org/short-reads/2025/01/07/what-the-data-says-about-federal-workers/

### Estimates
- **1980-1997**: Pre-FedScope trend extrapolations based on Cold War-era federal workforce statistics.
- **1998-2008**: Interpolated from 2009 FedScope baseline (FedScope coverage begins Sept 1998).
- **2019-2023**: FedScope trend estimates.

## Data Exclusions
FedScope data does NOT include: CIA, DIA, NSA, State Dept Foreign Service (excluded since March 2006), USPS, foreign nationals employed overseas, active duty military, non-appropriated fund employees, contractors.

## Format
- `year`: Calendar year
- `employees_overseas`: Total federal civilian employees in foreign countries
- `source`: Data provenance for each observation

# NCHS US Total Divorces — Data Provenance

## Description
Total United States divorces and annulments, 1979-2022. Used as Item 8 in
TR2025 Section 1.7 (Divorce subprocess) for scaling DRA-based age-specific
rates to national totals.

## Source
- CDC/NCHS National Vital Statistics Reports (NVSR)
- National Marriage and Divorce Rate Trends: https://www.cdc.gov/nchs/nvss/marriage-divorce.htm
- NVSS provisional reports for recent years

## Notes

### Reporting coverage
- 1979-1999: All 50 states + DC reported divorce counts to NCHS
- 2000 onwards: Some states stopped reporting (CA, GA, HI, IN, LA, MN stopped
  at various points). Published counts may reflect 45-47 state totals rather
  than true national totals. From 1992+, NCHS derives counts as rate x population.

### 2016 data correction
The value 776,000 is used for 2016. An earlier version of the codebase used
827,000, which is inconsistent with the CDC NVSS published rate of 3.2 per
1,000 for that year. The 776,000 figure is consistent with CDC NVSS published
tables and matches the validate_divorce.R reference data.

### 2022 value
674,000 is used (from NVSS provisional data), consistent with the published
rate of 2.4 per 1,000.

## Format
- `year`: Calendar year (1979-2022)
- `total_divorces`: Total US divorces and annulments
- `rate_per_1000`: Crude divorce rate per 1,000 total population
- `source`: Data source identifier

# O Nonimmigrant Age Distribution — Data Provenance

## Description
Age-group weights and sex skew factors for the nonimmigrant (type I) component of the
O (temporary/unlawfully present) population. Used to distribute aggregate nonimmigrant
stock estimates to single-year ages and by sex.

## Sources
- DHS Yearbook of Immigration Statistics (2019). Nonimmigrant Admissions by
  Class of Admission and Country of Citizenship.
  https://www.dhs.gov/immigration-statistics/yearbook
- DHS nonimmigrant stock reports (various years).
  Total nonimmigrant stock estimated at ~2.1M based on DHS reports.

## Age Pattern Rationale
- Ages 0-17: Small proportion — mostly F-2 dependents of workers/students
- Ages 18-24: Large proportion — F-1 students dominate
- Ages 25-39: Largest proportion — peak H-1B and L-1 worker categories
- Ages 40-59: Declining — fewer new work visas at older ages
- Ages 60+: Minimal — few elderly nonimmigrants

## Sex Skew
- Male skew of 1.10 for ages 25-49 reflects H-1B/L-1 worker category gender composition
- Female skew of 0.90 for ages 50+ reflects lower female labor force participation abroad

## Format
- `age_min`, `age_max`: Age range (inclusive)
- `ni_weight`: Relative weight for this age group (sums to ~1.0 before normalization)
- `male_skew`: Multiplier for male proportion (1.0 = equal, >1 = more males)
- `source`: Citation

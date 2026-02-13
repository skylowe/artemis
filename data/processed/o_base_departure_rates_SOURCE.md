# O Base Departure Rates — Data Provenance

## Description
Simplified base departure rates by age group for the O (temporary/unlawfully present)
population. Used when the detailed 2008-2010 stock build-up is not available.

Per TR2025: "rates are calculated by dividing OE by OP for each age, sex, and type"
from the 2008-2010 stock build-up period.

## Known Deviation
TR2025 uses separate rate tables derived from actual 2008-2010 stock build-up data
(Inputs #26-30). ARTEMIS approximates these using age-group base rates with
multiplicative type and sex adjustments. The actual SSA internal rates are not
publicly available.

## Sources
- TR2025 Section 1.5.c describes the methodology for departure rate derivation.
- Age pattern: Based on migration literature showing higher mobility for young adults.
- Type multipliers: N > V > I (never-authorized most vulnerable to removal;
  nonimmigrants have legal status providing stability).

## Format
- `age_min`, `age_max`: Age range (inclusive)
- `base_rate`: Annual departure rate (proportion of stock)
- `n_multiplier`: Multiplier for never-authorized type
- `i_multiplier`: Multiplier for nonimmigrant type
- `v_multiplier`: Multiplier for visa-overstayer type
- `male_multiplier`: Multiplier for males (1.0 for females)
- `source`: Citation

# O DHS Anchor Points — Data Provenance

## Description
DHS unauthorized and nonimmigrant stock totals used as anchor points for the
TR2025 historical type interpolation methodology (1963 → 2010 → 2015).
These totals determine the type split (N/I/V) at the 2010 and 2015 anchor years.

## Sources

### 2010 Anchor
- DHS (2011). "Estimates of the Unauthorized Immigrant Population Residing in the
  United States: January 2010." Hoefer, Rytina, Baker. Total: 10.8 million.
- DHS nonimmigrant stock estimate, December 2010: ~1.9 million.
- Warren & Kerwin (2017). Overall overstay proportion of unauthorized: ~40%.

### 2015 Anchor
- DHS (2021). "Estimates of the Unauthorized Immigrant Population Residing in the
  United States: January 2015-2018." Baker. Total: 10.7 million (Jan 2015).
- DHS nonimmigrant stock estimate, April 2016 (closest available): ~2.1 million.
- Updated Warren & Kerwin overstay estimate: ~42%.

## Usage
Type proportions at each anchor:
- I (nonimmigrant) = total_nonimmigrant / total_O
- V (overstayer) = (total_unauthorized × overstay_pct) / total_O
- N (never-authorized) = remainder

## Format
- `year`: Anchor point year
- `total_unauthorized`: DHS estimated unauthorized population
- `total_nonimmigrant`: DHS nonimmigrant stock
- `overstay_pct_overall`: Overall visa overstay percentage of unauthorized
- `source`: Citation

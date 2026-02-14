# NCHS PR/VI Divorces — Data Provenance

## Description
Divorce counts for Puerto Rico and the US Virgin Islands for years with NCHS
data (1988, 1998-2000). Used as Item 9 in TR2025 Section 1.7 (Divorce subprocess)
for computing the Social Security area adjustment factor.

## Source
- TR2025 Input #9: "The total number of divorces in Puerto Rico and the
  Virgin Islands for years 1988, 1998, 1999, and 2000."
- 1988 VI value (380): Verified from DRA microdata (div88b.txt, Virgin Islands
  records with weight=1)
- All PR values: NCHS estimates (exact source reports not independently verified)
- 1998-2000 VI values: NCHS estimates (not independently verified)

## Verification Status
Only the 1988 Virgin Islands value of 380 has been verified against source
microdata. All Puerto Rico values and 1998-2000 Virgin Islands values are
labeled as NCHS estimates and should be verified against NCHS reports if
higher accuracy is needed.

## Format
- `year`: Calendar year
- `territory`: "Puerto Rico" or "Virgin Islands"
- `divorces`: Total divorces in that territory
- `source`: Data source ("DRA microdata" or "NCHS estimate")
- `verified`: Whether the value has been independently verified against source data

# O Overstay Percentages by Age — Data Provenance

## Description
Age-specific overstay percentages used to split the unauthorized immigrant population
into never-authorized (N) and visa-overstayer (V) types. TR2025 Input #25 specifies
"internally developed overstay percentages by age" based on a RAND Corporation document
using 1980s data, adjusted with DHS insights. Since SSA's internal values are not
published, these are approximations from published research.

## Sources

### Primary
- Warren, R. & Kerwin, D. (2017). "The 2,000 Mile Wall in Search of a Purpose:
  Since 2007 Visa Overstays have Outnumbered Undocumented Border Crossers by a
  Half Million." Center for Migration Studies.
  https://cmsny.org/publications/jmhs-visa-overstays-border-wall/
  Finding: ~42% of unauthorized population (as of 2014) are visa overstayers.

### Supporting
- DHS Entry/Exit Overstay Reports (2016-2022).
  https://www.dhs.gov/immigration-statistics/overstay
  Finding: F-1/J-1 student and exchange visitor visas show elevated overstay rates
  among young adults ages 18-30.

- Passel, J.S. & Cohn, D. (2019). "Mexicans decline to less than half the U.S.
  unauthorized immigrant population for the first time." Pew Research Center.
  Finding: Age distribution analysis shows younger unauthorized immigrants more
  likely to have entered legally (visa overstay).

## Age Pattern Rationale
- Children (0-17): Lower rates — usually accompany unauthorized parents
- Young adults (18-30): Highest rates — students, tourists who overstay
- Middle age (30-50): Medium rates — mixed entry modes
- Older adults (50+): Lower rates — long-term EWI residents

## Format
- `age_min`, `age_max`: Age range (inclusive)
- `overstay_pct`: Proportion of unauthorized in this age group who are visa overstayers
- `source`: Citation for the estimate

## TR2025 Reference
Input #25: "Internally developed overstay percentages by age. These data are based
off a RAND Corporation document using data from the 1980s, and are adjusted based
on insights from the DHS."

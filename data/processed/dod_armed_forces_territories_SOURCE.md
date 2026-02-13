# DoD Armed Forces in Territories — Data Provenance

## Description
Total numbers of armed forces stationed in U.S. territories for decennial census years. Used to adjust territory populations by removing military personnel (counted elsewhere in USAF component). Corresponds to TR2025 Input #41.

## Sources
- Department of Defense. Defense Manpower Data Center (DMDC).
- Census Bureau. Census 2000 and 2010 Special Tabulations.
- Statistical Abstract of the United States (military personnel tables).

## Notes
- Per TR2025: "2020 data is assumed to be the same as 2010 until the data is available to OCACT."
- Puerto Rico: Roosevelt Roads Naval Station closed 2004, reducing presence significantly.
- Guam: Andersen AFB and Naval Base Guam; Asia-Pacific rebalance increased presence.

## Format
- `census_year`: Decennial census year
- `territory`: Territory code (PR, VI, GU, AS)
- `armed_forces`: Military personnel stationed in territory
- `source`: Data provenance

# 2.1. U.S. EMPLOYMENT (USEMP)

## 2.1.a. Overview

The Bureau of Labor Statistics (BLS) publishes historical monthly estimates for civilian U.S. employment-related concepts from the Current Population Survey (CPS). The principal measures include:

| Measure | Symbol | Definition |
|---------|--------|------------|
| Civilian Labor Force | LC | Sum of employment and unemployment |
| Employment | E | Employed persons |
| Unemployment | U | Unemployed persons |
| Civilian Noninstitutional Population | N | Base population |
| Labor Force Participation Rate | LFPR | LC / N |
| Unemployment Rate | RU | U / LC × 100 |

For many of these concepts, the BLS publishes historical data disaggregated by age, sex, marital status, and presence of children.

### Core Equations (Total Economy)

For various disaggregated groups, USEMP projects quarterly and annual values for these principal measures. The subprocess' overall structure:

| Eq. | Formula | Description |
|-----|---------|-------------|
| 2.1.1 | M^t = M^2021 | Military population constant over projection |
| 2.1.2 | N^t = [(N^(t-1) + M^(t-1)) × (P^t / P^(t-1))] - M^t | Civilian noninstitutional population grows with SS area population |
| 2.1.3 | RU = RU(·) | Unemployment rate (modeled) |
| 2.1.4 | LFPR = LFPR(·) | Labor force participation rate (modeled) |
| 2.1.5 | LC = LFPR × N | Labor force |
| 2.1.6 | E = LC × (1 - RU/100) | Employment |

*Note: Superscript t represents the projection year.*

### Temporary or Unlawfully Present Population Employment

The Demography Process estimates the temporary or unlawfully present population (OP), disaggregated by visa status:

| Component | Description |
|-----------|-------------|
| OP_A | Temporarily authorized to reside or work in the US |
| OP_NA | Overstayed their authorization |
| OP_NO | Never authorized to reside or work in the US |

USEMP projects employed OP (EO) and its components:

| Eq. | Formula | Description |
|-----|---------|-------------|
| 2.1.7 | EO_A = EO_A(·) | Employed authorized temporary |
| 2.1.8 | EO_NA = E × OP_NA / N | Employed overstayed (same E/N ratio) |
| 2.1.9 | EO_NO = E × OP_NO / N | Employed never authorized (same E/N ratio) |
| 2.1.10 | EO = EO_A + EO_NA + EO_NO | Total employed OP |

### EO by Earnings Recording Status

| Eq. | Formula | Description |
|-----|---------|-------------|
| 2.1.11 | EO_MEF = EO_MEF(·) | Earnings posted to Master Earnings File |
| 2.1.12 | EO_MEFC = EO_MEFC(·) | MEF earnings that are OASDI covered |
| 2.1.13 | EO_ESF = EO_ESF(·) | Earnings posted to Earnings Suspense File |
| 2.1.14 | EO_UND = EO - EO_MEF - EO_ESF | Underground economy |

### At-Any-Time Employment (TEO)

EO represents average weekly employment during a calendar year. TEO represents total individuals with any employment during the year.

| Eq. | Formula | Description |
|-----|---------|-------------|
| 2.1.15 | TEO_MEF = TEO_MEF(·) | At-any-time MEF |
| 2.1.16 | TEO_MEFC = TEO_MEFC(·) | At-any-time MEF covered |
| 2.1.17 | TEO_ESF = TEO_ESF(·) | At-any-time ESF |
| 2.1.18 | TEO_UND = TEO_UND(·) | At-any-time underground |
| 2.1.19 | TEO = TEO_MEF + TEO_ESF + TEO_UND | Total at-any-time employed OP |

---

## 2.1.b. Input Data

### Long-Range OASDI Projection Data

*Updated each year.*

#### From Demography Process

1. Social Security area population as of year-end (1941-2105) by age, marital status (single, married, widowed, divorced) and sex (M, F)
2. Temporary or unlawfully present population as of year-end (1964-2105) by age, sex (M, F), and visa status (OP_A, OP_NA, OP_NO)
3. Number of children by age of child and age of mother (1960-2105)
4. Life expectancy by age and sex (1950-2105)
5. Exit rates (probability of leaving the temporary or unlawfully present population by other than death) by age and sex
6. Mortality rates by age and sex (1941-2105)
7. Civilian noninstitutionalized population by age, sex and marital status (2010-2105)

#### From Trust Fund Operations and Actuarial Status

*No direct input, but LFPRs use input from prior year's Trustees Report:*

8. Disability prevalence rates by age and sex (from Beneficiaries subprocess)
9. Disability-insured population (from Beneficiaries subprocess)
10. Primary insurance amount (PIA) replacement rates by age and sex

**Definitions:**
- **Disability prevalence rate**: Ratio of disabled worker beneficiaries to disability-insured population
- **PIA replacement rate**: Ratio of hypothetical medium-scaled worker's PIA to career-average indexed earnings

### Trustees' Assumptions

Ultimate average annual growth rates for key economic variables:

| # | Variable | Symbol |
|---|----------|--------|
| 11 | Real wage | - |
| 12 | Total economy productivity | - |
| 13 | Average hours worked | - |
| 14 | Ratio of wages to compensation | RWSD |
| 15 | Ratio of compensation to GDP | RWSSY |
| 16 | GDP deflator | PGDP |
| 17 | Consumer Price Index | CPI |

Ultimate values:

| # | Variable |
|---|----------|
| 18 | Annual trust fund real interest rate |
| 19 | Unemployment rate |

These ultimate values are typically reached during the last half of the short range (first 10 years). Earlier projected values provide smooth transition from historical to ultimate values. As a by-product, real GDP and potential GDP are set. The ratio (RTP) of real to potential GDP is an important summary measure of the economic cycle.

### Addfactors

20. **Addfactors** are adjustments that move an estimate closer to an expected value. Uses include:
    - Life expectancy effects on male/female LFPRs (starting around age 40)
    - Modifying LFPRs for specific age-sex groups for reasonable age-LFPR profile shape
    - Phasing out differences between model-predicted values and recent historical data in first few projection years

### Other Input Data

| # | Data Source | Description | Years | Frequency |
|---|-------------|-------------|-------|-----------|
| 21 | Census/DoD | U.S. armed forces (EDMIL) by age (17-64) and sex | 1948-2000 | Monthly (discontinued) |
| 22 | Census | EDMIL by age (16-69) and sex, calculated as resident+Armed Forces overseas minus civilian | Apr 2000-present | Monthly, updated annually |
| 23 | DoD | Mobilized military reservist population by branch | Sep 2001-Sep 2016 | Weekly (discontinued) |
| 24 | CPS March Supplement | Civilian noninstitutional population, labor force, military, unemployment by age (16-85+), sex, marital status, presence of children | 1968-present | Annual |
| 25 | CPS March Supplement | Civilian noninstitutional population by age (16-80+), sex, educational attainment | 1992-present | Annual |
| 26 | CPS | Civilian employment, labor force, unemployment, population, rates by age group and sex | Jan 1948-present | Monthly |
| 27 | CPS | Civilian noninstitutional population by age (16-90+), sex, marital status, labor force status, reason not in LF | Jan 1994-present | Monthly → Annual averages |
| 28 | CES | Establishment employment, average hourly/weekly earnings, average weekly hours by sector | 1964-present | Monthly |
| 29 | CPS (unpublished) | Male/female civilian LFPRs for older workers by age (55-79) and groups (75+, 80+) | 1965-present | Monthly |

---

## 2.1.c. Development of Output

### Equation 2.1.3 - Unemployment Rate (RU)

**Disaggregation:** 28 equations (14 age groups × 2 sexes)

**Age groups:** 16-17, 18-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75+

**Model specification:**
- First-difference model
- Depends on distributed lag in change of ratio of real to potential GDP (RTP)
- Adjustment ensures convergence to estimated trend level
- Coefficients estimated by regression
- Constrained to ultimate age-sex-adjusted RU set by Trustees

The aggregate age-sex-adjusted RU depends on projected distribution of labor force by age and sex.

### Equation 2.1.4 - Labor Force Participation Rate (LFPR)

**Disaggregation:** 153 equations (69 male + 84 female)

**Age groups:**
- 16-17, 18-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54 (5-year groups)
- 55, 56, ..., 99, 100+ (single years for ages 55+)

**Additional disaggregation:**
- Ages 20-54: By marital status (never married, married spouse present, married spouse absent)
- Female ages 20-44 by marital status: By presence of own child (child under 6, no child under 6)

**Key Input Variable Effects:**

| Variable | Effect on LFPR | Applicable Ages |
|----------|---------------|-----------------|
| **Disability prevalence ratio (RD)** | Increase in RD → decrease in LFPR (divided by 1+RD) | All ages; for 62-74, use cohort RD at age 61 |
| **Unemployment rate (RU)** | Increase in lagged/current RU → decrease in LFPR | Up to age 54 |
| **Normal retirement age (NRA)** | Increase in NRA → decrease in replacement rates → increase in LFPR | Ages 62-69 |
| **Potential earnings test tax rate (POT_ET_TXRT)** | Increase → decrease in LFPR | Ages 62-69 |
| **Educational attainment** | Higher education → higher LFPR | Men 55+, Women 50+ |
| **Marital status composition** | Affects via disaggregation or proportion married | Up to 54 (disaggregated), 55-74 (proportion) |
| **Lagged cohort variables** | Cohort effects | Age 75+ |
| **Life expectancy** | Increase → increase in LFPR | Age 40+ |

**NRA Effect Details:**
- Replacement rate = PIA / career-average wage for medium-scaled workers retiring at ages 62-69
- Adjusted for early retirement reduction and delayed retirement credit
- NRA increase from 66 to 67 increases potential tax rate for age 66, decreasing their LFPR

### Equations 2.1.7-2.1.19 - Employed OP (EO) and At-Any-Time Employed OP (TEO)

**Disaggregation:** 4,250 equations (85 ages × 2 sexes × 25 components/subgroups)

**EO Estimation:**
- By sex and single-year of age (16-100)
- Based on OP and estimated employment-to-population ratios by visa-status component
- OP_A further disaggregated by visa type (employment patterns, OASDI coverage status)
- OP_NA and OP_NO assumed to have same E/N ratio as LPR population of same age and sex
- EO_NO separated into: worked 2001 and earlier vs. began working 2002 and later (earlier workers more likely to have OASDI covered wages)

**TEO Conversion:**
- Each EO sub-component converted to TEO using age-sex conversion weight
- Conversion weight = total at-any-time employed / (military + CPS civilian employment) for that age-sex group
- For authorized workers/students on temporary visas, weights account for partial presence in arrival/departure years

---

## Related Documentation

- [Equation Details (Appendix 2-1)](equations_abbreviations/2025_LR_Model_Documentation_economics_equations_1_USEmployment.pdf)
- [Abbreviations (Appendix 2-2)](2025_LR_Model_Documentation_Economics_Abbreviations.md)

## References

- **Actuarial Note 2025.3**: Scaled Factors for Hypothetical Earnings Examples
  - Local: [an2025-3.md](an2025-3.md)
  - SSA: [www.ssa.gov/OACT/NOTES/ran3/index.html](https://www.ssa.gov/OACT/NOTES/ran3/index.html)

# 1.2. MORTALITY

## 1.2.a. Overview

The National Center for Health Statistics (NCHS) collects data on annual numbers of deaths and the U.S. Census Bureau produces estimates of the U.S. resident population. Central death rates ($_ym_x$) are defined as the ratio of (1) the number of deaths occurring during the year to persons between exact ages x and x+y to (2) the midyear population between exact ages x and x+y. When y equals 1, central death rates are often displayed simply as $m_x$. For historical years prior to 1968, $_ym_x$ are calculated from NCHS and Census data by sex. For historical years beginning in 1968, the same data are used in the calculations for ages under 65, but data from the Centers for Medicare and Medicaid Services (CMS) are used for ages 65 and over. Single year of age historical data, $m_x$, are only available starting in 1968 and are used for the projections. Based on death by cause data from the NCHS, the $m_x$ are distributed by cause of death for years 1979 and later.[^4]

Over the last century, death rates have decreased substantially. The historical improvement in mortality can be quantified by calculating the percentage reductions in log linear regressions of central death rates ($AA_x$). In order to project future $m_x$, the Board of Trustees of the OASDI Trust Funds assumes an ultimate annual percentage reduction ($_{y}AA_w^u$) that will be realized during the projection period for each sex and cause of death.

The basic mortality outputs of the MORTALITY subprocess that are used in projecting the population are probabilities of death by age and sex ($q_x$). The probability that a person age x will die within one year ($q_x$) is calculated from the central death rates (the series of $m_x$).

Period life expectancy ($\mathring{e}_x$) is defined as the average number of years of life remaining for people who are age x and are assumed to experience the probabilities of death for a given year throughout their lifetime. It is a summary statistic of overall mortality for that year.

Age-adjusted death rates ($ADR$) are also used to summarize the mortality experience of a single year, making different years comparable to each other. Age-adjusted death rates are a weighted average of the $m_x$, where the weights used are the numbers of people, male and female combined, in the corresponding age groups of the standard population, the 2010 U.S. Census resident population ($SP_x$). Thus, if the age-adjusted death rate for a particular year and sex is multiplied by the total 2010 U.S. Census resident population, the result gives the number of deaths that would have occurred for that sex in the 2010 U.S. Census resident population if the $m_x$ for that particular year and sex had been experienced. Age-sex-adjusted death rates ($ASDR$) are calculated to summarize death rates for both sexes combined. They are calculated as a weighted average of the $m_x$, where each weight is the number of people in the corresponding age and sex of the 2010 U.S. Census resident population.

MORTALITY projects annual $m_x$, which are then used to calculate additional outputs. The equations for this subprocess are given below:

$$m_x = m_x(\cdot) \tag{1.2.1}$$

$$AA_x = AA_x(\cdot) \tag{1.2.2}$$

$$q_x = q_x(\cdot) \tag{1.2.3}$$

$$\mathring{e}_x = \mathring{e}_x(\cdot) \tag{1.2.4}$$

$$ADR_s^z = \frac{\sum_x SP_x \cdot m_{x,s}^z}{\sum_x SP_x} \tag{1.2.5}$$

$$ASDR^z = \frac{\sum_s \sum_x SP_{x,s} \cdot m_{x,s}^z}{\sum_s \sum_x SP_{x,s}} \tag{1.2.6}$$

where $m_{x,s}^z$ refers to the central death rate between exact age x and x+1, by sex, in year z;
$SP_x$ denotes the number of people in the standard population (male and female combined) who are between exact age x and x+1; and $SP_{x,s}$ denotes the number of people, by sex, in the standard population who are between exact age x and x+1.

## 1.2.b. Input Data

*Trustees Assumptions -*

1. Each year the Board of Trustees of the OASDI Trust Funds sets the assumed ultimate values for the $_yAA_w$ by sex, age group (less than 15, 15-49, 50-64, 65-84, and 85+), and cause of death (Cardiovascular Disease, Cancer, Accidents and Violence, Respiratory Disease, Dementia, and All Other). The annual percentage reductions reach their ultimate values in the 25th year of the 75-year projection period. The ultimate rates of reduction by sex, age group, and cause of death can be found in Appendix 1.2-1. The Trustees also assume that the COVID-19 pandemic will affect death rates through 2025. See the discussion of equation 1.2.3 for more details about the COVID-19 effects.

*NCHS Data -*

2. Annual numbers of registered deaths by sex and age group for the period 1900-67. These data are not updated. Registered deaths refer to deaths in the Death Registration area. Since 1933, the Death Registration area has included all of the U.S.

3. The monthly number of births, by sex, for years 1935-2022. These data are updated annually, when the NCHS provides an additional year of data.

4. The number of infant deaths, by age, sex, and age group (under 1 day, 1-2 days, 3-6 days, 7-27 days, 28 days-1 month, 2 months, 3 months, ..., 11 months, 1 year, 2 years, 3 years, and 4 years), for years 1939-2022. These data are updated annually, when the NCHS provides an additional year of data.

5. The population of states in the Death Registration area by age group (0, 1-4, 5-14, 15-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75-84, and 85+) and sex, for years 1900-39. These data are not updated.

6. The number of registered deaths, by sex and age group (85-89, 90-94, and 95+), used for the years 1900-67. These data are not updated.

7. Starting values for $q_x$ for infant and toddler age groups (under 1 day, 1-2 days, 3-6 days, 7-27 days, 28 days-1 month, 2 months, 3 months, ..., 11 months, 1 year, 2 years, 3 years, and 4 years) from 1939-41 decennial life tables. These data are not updated.

8. From the NCHS public use records, deaths by sex, single year of age, cause of death, and marital status from 1968-2022. Marital status is not available until 1979, and cause of death is only used for 1979 and later.

9. From the NCHS WONDER system,[^5] provisional deaths by sex, single year of age, and cause of death for 2023.

*U.S. Census Bureau Data -*

10. Estimates of the July 1 resident population by single year of age (0 through 100+) for years 1980-2023. Each year, Census provides an additional year of data and updated data for years after the most recent decennial census.

11. Standard population by year, sex, and age: 2010. Used to standardize death rates for comparison. Updated every decennial census.

12. U.S. resident population by sex, age group, and year 1900-39 (0-4, 5-9, ..., 70-74, and 75+). These data are not updated.

13. The resident population at ages 75-79 and 80-84, by sex, for years 1900-40 (at ten-year intervals). These data are not updated.

14. The resident population, by sex and age group (0, 1-4, 5-9, ..., 80-84, and 85+), for 1940-59. These data are not updated.

15. Resident population by single year of age and sex for 1960-69 (ages 0 through 60+). These data are not updated.

16. Resident population by single year of age and sex for 1970-79 (ages 0 through 85+). These data are not updated.

17. The United States and Armed Forces overseas (USAF) population, by sex and single year of age for 1960-69 (ages 60 through 85+ for 1960-67 and ages 60 through 84 for 1968-69). These data are not updated.

18. Population, by 5-year age group to split 85+ pre-1968. These data are not updated.

19. Estimates of the population by marital status, sex, and age from the American Community Survey (ACS) public use microdata sample (PUMS) files for years 2000-19. In general, an additional year of data is available each year.

*CMS Data -*

20. Annual numbers of deaths of all Medicare enrollees, by sex and single year of age (ages 65 and over), 1968-87. These data are not updated.

21. Annual numbers of deaths of all Medicare enrollees, by sex and single year of age (ages 65 and over), for the period 1988-2005. These data are not updated.

22. Annual numbers of deaths of Medicare enrollees who are also Social Security or Railroad Retirement Board beneficiaries, by sex and single year of age (ages 65 and over), for the period 1988-2005. These data are not updated.

23. Annual numbers of deaths of all Medicare enrollees who are also Social Security or Railroad Retirement Board beneficiaries, by sex and single year of age (ages 65 and over), for the period 2006-23. These data are updated annually, when the CMS provides an additional year or years of preliminary data and replaces prior year (or years) preliminary data with final data.

24. Annual numbers of all Medicare enrollees, by sex and single year of age (ages 65 and over), 1968-88. These data are not updated.

25. Annual numbers of all Medicare enrollees, by sex and single year of age (ages 65 and over), for the period 1988-2006. These data are not updated.

26. Annual numbers of Medicare enrollees who are also Social Security or Railroad Retirement Board beneficiaries, by sex and single year of age (ages 65 and over), for the period 1988-2006. These data are not updated.

27. Annual numbers of all Medicare enrollees who are also Social Security or Railroad Retirement Board beneficiaries, by sex and single year of age (ages 65 and over), for the period 2006-23. These data are updated annually, when the CMS provides an additional year (or years) of preliminary data and replaces prior year (or years) preliminary data with final data.

28. Factors for ratioing age 65 Medicare deaths between 1988 and 2005. These data are not updated.

*Other Input Data -*

29. Internally developed resident population by single year of age (85 - 100+) and sex, 1968-79. These data were developed from USAF population data and are not updated.

30. List of NCHS 113 cause of death codes found in PUMS data file mapped to the causes used in Trustees Report.

## 1.2.c. Development of Output

### Equation 1.2.2 - Percentage Reductions in Log Linear Regressions of Central Death Rates ($AA_x$)

The $AA_x$, by sex and cause, are calculated based on the decline in the $m_x$ for the period 2008 through 2019, and distributed by single year of age for ages 0 - 99, 2 sexes, and 6 causes of death.[^6] The values are calculated as the complement of the exponential of the slope of the least-squares line through the logarithms of the $m_x$.

The assumed ultimate values for the central death rates ($_{y}AA_w^u$), as set by the Board of Trustees of the OASDI Trust Funds, are assumed to be reached in the 25th year of the 75-year projection period. These ultimate values are specified by six causes of death for the following five age groups: under 15, 15-49, 50-64, 65-84, and 85 and older. Male and female values are assumed to be equal to each other.

The starting values of $AA_x$, by single year of age 0 - 99, sex, and cause, are assumed to equal the percentage reductions in log linear regressions of $m_x$ for the period 2008-19 when that percentage reduction is nonnegative. However, if that percentage reduction is negative, then the starting values are assumed to be 75 percent of the percentage reduction. The weights are 0.2, 0.4, 0.6, and 0.8 for the earliest four years of the 12 years, 1.0 for the next six years and 2.0 and 3.0 for the last two years. Available Medicare preliminary data is used for overall levels with the last available NCHS data year cause of death percentages carrying forward. For each year after the last data year used in the regressions, the $AA_x$ are calculated by transitioning from the starting values of $AA_x$ to the associated Trustees' assumed ultimate values, $_{y}AA_w^u$ for that age. This is accomplished by repeating the following steps for each historical year after the last data year and for the first 24 years of the projection:

1. The difference between the prior year's calculated $AA_x$ and the associated assumed ultimate $_{y}AA_w^u$ for that age x is calculated. Note that for the first year of this process, the starting values of $AA_x$, as defined above, are used instead of the prior year's $AA_x$.

2. The current year's $AA_x$ is the assumed associated ultimate $_{y}AA_w^u$ plus 80 percent of the difference calculated in step 1.

For the 25th year of the projection, the $AA_x$ are set equal to their assumed associated ultimate values, $_{y}AA_w^u$.

### Equation 1.2.1 - Central Death Rates ($m_x$)

Values of $m_x$ are determined for each historical and projected year by single year of age 0 - 99, 2 sexes, and 6 causes of death. Whittaker-Henderson smoothing[^7] with degree parameter equal to 2 and smoothing parameter equal to 0.01 is applied from ages 2 through 99. The base year for the projections of the $m_x$ is 2019, and is the most recent data year used in the regressions. However, instead of using the historical data for $m_x$ in this year as the starting point for mortality projections, starting $m_x$ values are calculated to be consistent with the trend inherent in the last 12 years of available data. Each starting value for the $m_x$, by sex and cause of death, is computed as the exponential of the value for the most recent year falling on a weighted least square line, where the logarithm of $m_x$ is regressed on year, over the last 12 years. The weights are 0.2, 0.4, 0.6, and 0.8 for the earliest four years of the 12 years, 1.0 for the next six years and 2.0 and 3.0 for the last two years.

For years after 2019, $m_x$ are projected, by sex and cause of death, by applying the respective $AA_x$ to the prior year $m_x$. For 2020 through 2023, because actual data are available, the projected $m_x$ values are overwritten with values calculated from actual data. Then, Whittaker-Henderson smoothing with degree parameter equal to 2 and smoothing parameter equal to 0.01 is applied for ages 2 through 99.

### Equation 1.2.3 - Probabilities of death ($q_x$)

In order to project population by age and sex, probabilities of death are applied to determine the projected number of deaths that will occur in the population. These probabilities, denoted as $q_x$, reflect the probability a person age x will die within one year, where x refers to age last birthday as of the beginning of each year. For each year in the historical and projection period, separate $q_x$ series are estimated by sex.

Different methods of projecting $q_x$ are used for age 0, for age 1, for ages 2 through 99, and for ages 100 and above. The following descriptions provide a brief discussion of these different methods. Additional detail is provided in Actuarial Study number 120. Note, however, that this study does not have updated methods of using single year of age data directly for ages 2 and older included in it and the study also starts the final method at age 95 versus age 100 like is done in the current model. This study, titled Life Tables for the United States Social Security area 1900-2100, can be found at: http://www.ssa.gov/OACT/NOTES/s2000s.html. (Choose study number 120.)

- **Values for $q_x$ at age 0:** During the first year of life, mortality starts at an extremely high level, which becomes progressively lower. This is unlike mortality at other ages, which does not change very much within a single year of age. Thus, it is particularly important at age 0 to estimate accurately the pattern of mortality throughout the year of age, as described above, for the calculation of $q_0$. For the period 1940 through the last historical year, $q_0$ is calculated directly from tabulations of births by month and from tabulations of deaths at ages 0, 1-2, 3-6, 7-28 days, 1 month, 2 months, ..., and 11 months. After the last historical year, $q_0$ is calculated from $m_0$, assuming that the ratio of $q_0$ to $m_0$ measured for the last historical year would remain constant thereafter.

- **Values for $q_x$ at age 1:** For the period 1940 through the last year of historical data, probabilities of death are calculated from tabulations of births by year and from deaths at age 1. After the last historical year, each $q_1$ is calculated from $_4m_1$ assuming that the ratio of $q_1$ to $_4m_1$ measured for the last historical year would remain constant thereafter.

- **Values for $q_x$ at ages 2 - 99:** Probabilities of death for these ages are calculated from the projected central death rates, $m_x$. This formula, which assumes uniform distribution of deaths throughout the year, is:

$$q_x = \frac{m_x}{1 + \frac{1}{2} \cdot m_x}$$

- **Values for $q_x$ at ages 100+:** It has been observed that the mortality rates of women, though lower than those of men, tend to increase faster with advancing age than those of men. An analysis of Social Security charter Old-Age Insurance beneficiaries has shown that at the very old ages mortality increases about five percent per year of age for men and about six percent per year for women. For men, probabilities of death at each age 95 and older are calculated as follows:

$$q_x = q_{x-1} \cdot \left(\frac{q_{99}}{q_{98}} \cdot \frac{104-x}{5} + 1.05 \cdot \frac{x-99}{5}\right) \qquad x = 100, 101, 102, 103, 104$$

$$q_x = 1.05 \cdot q_{x-1} \qquad x = 105, 106, 107, \ldots$$

For women, the same formulas are used, except that 1.06 is substituted for 1.05. The larger rate of growth in female mortality would eventually, at a very high age, cause female mortality to be higher than male mortality. At the point where this crossover would occur, female mortality is set equal to male mortality.

The values of $q_x$ used in projecting the population are based on age last birthday and are calculated by sex for $_{1/2}q_0$ (neonatal) and for $q_x$, where x represents age last birthday for ages 0 through 100 (with 100 representing the age group 100 and older). Because life table values of probabilities of death are based on exact ages, values for $q_x$ representing age last birthday are derived as follows:

- $_{1/2}q_0 = 1 - L_0 / l_0$ for neonatal
- $q_x = 1 - L_{x+1} / L_x$ for ages 0 to 99
- $q_{100} = 1 - T_{101} / T_{100}$ for age group 100 and older

See Actuarial Study number 120 for the definitions of the life table terms. This study can be found at: http://www.ssa.gov/OACT/NOTES/s2000s.html. (Choose study number 120; then section IV.A in the table of contents.)

The COVID-19 pandemic impacts death rates. The impacts are assumed to affect death rates through 2025. The following factors, applied to the probabilities of death, were incorporated to account for these impacts:

| Year | Age 0 | Ages 1-14 | Ages 15-64 | Ages 65-84 | Ages 85 and older |
|------|-------|-----------|------------|------------|-------------------|
| 2024 | 1.01  | 1.17      | 0.99       | 1.02       | 0.98              |
| 2025 | 1.00  | 1.09      | 1.00       | 1.00       | 1.00              |

In addition, probabilities of death are broken down further into marital status. Historical data indicate that differential in mortality by marital status is significant. To reflect this, future relative differences in death rates by marital status are projected to be the same as observed during calendar years 2015-19. These rates were developed by:

1. Taking the single year of age deaths from the public use NCHS data by marital status and dividing by the equivalent population sums from the ACS to get preliminary single year of age death rates by marital status.
2. Adjusting the older age populations and calculated death rates for consistency with prior ages.
3. Adjusting the older age death rates further so that all marital statuses would gradually reach the same value at age 95.
4. Adjusting ages under 15 to have the total death rates for all marital statuses.
5. Smoothing these death rates, by single year of ages 15 through 94, using Whittaker-Henderson smoothing with degree parameter 2 and smoothing parameter 0.01.
6. Adjusting non-single marital statuses for ages 15 through 20 to have the same ratio relative to the single marital status for age 21.
7. Converting these death rates to probabilities of death. Ages 95 and older use the same formulas as described above for total death probabilities.

### Equation 1.2.4 - Life expectancy

Actuarial Study number 120 presents background information on the calculation of life expectancy, $\mathring{e}_x$, from the probabilities of death ($q_x$). This study can be found at: http://www.ssa.gov/OACT/NOTES/s2000s.html. (Choose study number 120; then IV.A in the table of contents.)

## Appendix: 1.2-1

The Board of Trustees of the OASDI Trust Funds sets the ultimate rates of mortality reduction by age group and cause of death. For comparison purposes, rates are also presented for two historical periods. Note that although the male and female ultimate rates are the same, the historical rates differ.

### Average Annual Rates of Reduction in Central Death Rates by Age Group, Sex, and Cause

#### Under Age 15

|  | Historical (Male) |  | Alternative II* (Male) |  | Historical (Female) |  | Alternative II* (Female) |  |
|---|---|---|---|---|---|---|---|---|
|  | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) |
| Cardiovascular Disease | 1.94 | 2.10 | 1.9 | 1.9 | 1.67 | 1.59 | 1.9 | 1.9 |
| Cancer | 2.38 | 1.87 | 1.5 | 1.5 | 2.03 | 1.67 | 1.5 | 1.5 |
| Accidents and Violence | 2.42 | 0.37 | 1.0 | 1.0 | 2.10 | 0.08 | 1.0 | 1.0 |
| Respiratory Disease | 2.29 | 2.06 | 2.0 | 2.0 | 2.47 | 2.69 | 2.0 | 2.0 |
| Dementia | 2.45 | 2.13 | 0.1 | 0.1 | 1.67 | -2.30 | 0.1 | 0.1 |
| Other | 2.24 | 1.75 | 1.7 | 1.7 | 2.13 | 1.66 | 1.7 | 1.7 |
| Resulting Total ** | 2.27 | 1.52 | 1.51 | 1.50 | 2.12 | 1.44 | 1.54 | 1.53 |

#### Ages 15 - 49

|  | Historical (Male) |  | Alternative II* (Male) |  | Historical (Female) |  | Alternative II* (Female) |  |
|---|---|---|---|---|---|---|---|---|
|  | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) |
| Cardiovascular Disease | 1.85 | 0.98 | 1.3 | 1.3 | 1.23 | 0.45 | 1.3 | 1.3 |
| Cancer | 1.94 | 2.46 | 1.5 | 1.5 | 1.61 | 1.84 | 1.5 | 1.5 |
| Accidents and Violence | 0.31 | -2.34 | 0.7 | 0.7 | -0.19 | -2.31 | 0.7 | 0.7 |
| Respiratory Disease | 0.52 | 2.16 | 0.5 | 0.5 | -0.43 | 2.17 | 0.5 | 0.5 |
| Dementia | 1.13 | -0.20 | 0.1 | 0.1 | 0.93 | 0.65 | 0.1 | 0.1 |
| Other | 0.20 | 0.51 | 0.8 | 0.8 | -0.07 | -0.02 | 0.8 | 0.8 |
| Resulting Total ** | 0.77 | -0.59 | 0.82 | 0.82 | 0.52 | -0.16 | 0.89 | 0.89 |

#### Ages 50 - 64

|  | Historical (Male) |  | Alternative II* (Male) |  | Historical (Female) |  | Alternative II* (Female) |  |
|---|---|---|---|---|---|---|---|---|
|  | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) |
| Cardiovascular Disease | 2.43 | 0.52 | 1.5 | 1.5 | 1.96 | 0.18 | 1.5 | 1.5 |
| Cancer | 1.62 | 2.38 | 1.5 | 1.5 | 1.23 | 1.48 | 1.5 | 1.5 |
| Accidents and Violence | -0.32 | -3.07 | 0.5 | 0.5 | -0.69 | -2.69 | 0.5 | 0.5 |
| Respiratory Disease | 0.51 | 0.02 | 0.7 | 0.7 | -1.17 | -0.65 | 0.7 | 0.7 |
| Dementia | -2.46 | -2.33 | 0.1 | 0.1 | -3.18 | -3.32 | 0.1 | 0.1 |
| Other | -0.28 | -0.36 | 0.6 | 0.6 | -0.30 | -0.74 | 0.6 | 0.6 |
| Resulting Total ** | 1.30 | 0.35 | 0.95 | 0.94 | 0.82 | 0.12 | 0.97 | 0.96 |

#### Ages 65 - 84

|  | Historical (Male) |  | Alternative II* (Male) |  | Historical (Female) |  | Alternative II* (Female) |  |
|---|---|---|---|---|---|---|---|---|
|  | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) |
| Cardiovascular Disease | 2.72 | 1.25 | 1.9 | 1.9 | 2.60 | 1.59 | 1.9 | 1.9 |
| Cancer | 1.06 | 2.14 | 0.9 | 0.9 | 0.34 | 1.79 | 0.9 | 0.9 |
| Accidents and Violence | 0.24 | -1.84 | 0.5 | 0.5 | -0.14 | -1.69 | 0.5 | 0.5 |
| Respiratory Disease | 0.48 | 1.45 | 0.3 | 0.3 | -1.83 | 0.99 | 0.3 | 0.3 |
| Dementia | -6.71 | -2.03 | 0.1 | 0.1 | -7.76 | -2.56 | 0.1 | 0.1 |
| Other | -0.30 | -0.68 | 0.3 | 0.3 | -0.45 | 0.07 | 0.3 | 0.3 |
| Resulting Total ** | 1.36 | 0.87 | 0.73 | 0.73 | 0.79 | 0.84 | 0.67 | 0.66 |

#### Ages 85 and older

|  | Historical (Male) |  | Alternative II* (Male) |  | Historical (Female) |  | Alternative II* (Female) |  |
|---|---|---|---|---|---|---|---|---|
|  | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) |
| Cardiovascular Disease | 1.73 | 1.11 | 1.5 | 1.5 | 1.97 | 1.34 | 1.5 | 1.5 |
| Cancer | 0.03 | 0.89 | 0.5 | 0.5 | -0.25 | 0.16 | 0.5 | 0.5 |
| Accidents and Violence | -0.76 | -1.70 | 0.3 | 0.3 | -1.10 | -2.04 | 0.3 | 0.3 |
| Respiratory Disease | -0.39 | 1.66 | 0.2 | 0.2 | -1.48 | 0.57 | 0.2 | 0.2 |
| Dementia | -9.37 | -2.02 | 0.1 | 0.1 | -10.17 | -2.24 | 0.1 | 0.1 |
| Other | -0.77 | 0.13 | 0.3 | 0.3 | -0.70 | 0.78 | 0.3 | 0.3 |
| Resulting Total ** | 0.41 | 0.47 | 0.58 | 0.58 | 0.32 | 0.24 | 0.53 | 0.53 |

#### Total

|  | Historical (Male) |  | Alternative II* (Male) |  | Historical (Female) |  | Alternative II* (Female) |  |
|---|---|---|---|---|---|---|---|---|
|  | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) | 1979 to 2019 | 2009 to 2019 | 2024 TR (2048 to 2098) | 2025 TR (2049 to 2099) |
| Cardiovascular Disease | 2.30 | 1.07 | 1.63 | 1.63 | 2.22 | 1.28 | 1.63 | 1.63 |
| Cancer | 1.06 | 1.94 | 0.90 | 0.90 | 0.62 | 1.45 | 0.96 | 0.96 |
| Accidents and Violence | 0.16 | -2.28 | 0.58 | 0.58 | -0.26 | -2.14 | 0.56 | 0.56 |
| Respiratory Disease | 0.22 | 1.39 | 0.31 | 0.31 | -1.51 | 0.69 | 0.33 | 0.32 |
| Dementia | -7.68 | -2.03 | 0.10 | 0.10 | -8.69 | -2.35 | 0.10 | 0.10 |
| Other | -0.17 | -0.18 | 0.43 | 0.43 | -0.23 | 0.19 | 0.43 | 0.43 |
| Resulting Total ** | 1.05 | 0.52 | 0.73 | 0.73 | 0.65 | 0.46 | 0.69 | 0.68 |

\* Alternative 1 is 1/3 times Alternative 2; Alternative 3 is 2.0 times Alternative 2.

\*\* Resulting total represents average annual percent reduction in age-adjusted death rates for the last 50 years of the 75-year projection period.

## Appendix: 1.2-2

The following table shows the mapping of the ICD-10 codes to the six broad categories of causes of death used in the model:

### Mappings from ICD-10 Codes to Broad Causes

| Broad Cause | ICD-10 Codes |
|---|---|
| Cardiovascular Disease | I00 - I78, N02 - N03, N05 - N07, N26 |
| Cancer | C00 - C97 |
| Accidents and Violence | U01 - U03, V01 - Y35, Y40 - Y87.2, Y88, Y89.0, Y89.9 |
| Respiratory Disease | J00 - J06, J09 - J18, J20 - J22, J30 - J47, J60 - J98, U04 |
| Dementia | F01, F03, G30, G31 |
| Other | [All other ICD-10 codes not listed above] |

[^4]: Data needed to project central death rates by cause of death were obtained from Vital Statistics tabulations for years since 1979. For the years 1979-98, adjustments were made to the distribution of the numbers of deaths by cause in order to reflect the revision in the cause of death coding that occurred in 1999, making the data for the years 1979-98 more comparable with the coding used for the years 1999 and later. The adjustments are comparability ratios created using published data from the National Center for Health Statistics and calculated using their methodology.

[^5]: https://wonder.cdc.gov/mcd.html

[^6]: The six causes of death are: Cardiovascular Disease, Cancer, Accidents and Violence, Respiratory Disease, Dementia, and All Other.

[^7]: See http://www.howardfamily.ca/graduation/index.html for more information on Whittaker-Henderson graduation.

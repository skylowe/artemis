# 1.1. FERTILITY

## 1.1.a. Overview

The National Center for Health Statistics (NCHS) collects data on annual numbers of births by single year of age of mother and the U.S. Census Bureau produces estimates of the female resident population by single year of age. Age-specific birth rates ($b_x^z$) for a given year z are defined as the ratio of (1) births ($B_x^z$) during the year to mothers at the specified age x to (2) the midyear female population ($P_x^z$) at that age. The total fertility rate ($TFR^z$) summarizes the age-specific fertility rates for a given year z. The total fertility rate for a given year z equals the sum of the age-specific birth rates for all ages x during the year. One can also interpret the total fertility rate as the average number of children that would be born to a woman if she were to survive her childbearing years and experience, at each age in her life, the age-specific birth rate of year z. There is also a cohort total fertility rate ($CTFR^t$). The cohort total fertility rate for a given cohort born in year t equals the sum of the age-specific birth rates for all ages x in each cohort's lifetime.

The FERTILITY subprocess combines the historical values of $b_x^z$ and the ultimate $CTFR^z$ to develop projections of $b_x^z$. The primary equations of this subprocess are given below:

$$b_x^z = b_x^z(\cdot) \tag{1.1.1}$$

$$TFR^z = \sum_x b_x^z \tag{1.1.2}$$

$$CTFR^t = \sum_x b_x^{t+x} \tag{1.1.3}$$

## 1.1.b. Input Data

*Trustees Assumptions -*

1. Each year the Board of Trustees of the OASDI Trust Funds sets the assumed ultimate values for the cohort TFR. For the 2025 Trustees Report, the ultimate TFR is 1.90 and it is assumed to be attained starting with the cohort born in 2020.

*Other input data -*

2. From the NCHS, annual numbers of births by age of mother[^2] (10-14, 15, 16, 17, ..., 48, 49-54) for years 1980-2023. In general, the NCHS provides an annual update including one additional year of final birth data. Previous historical years are only updated if the NCHS makes a historical revision to their data.

3. From the U.S. Census Bureau, estimates of the July 1st female resident population by single year of age for ages 14-49 for 1980-2024. In general, each year, Census provides updated data for years after the most recent decennial census.

4. From the NCHS, historical birth rates, by single year of age of mother (14-49) for the period 1917-79. No updates of these data are needed.

5. From the NCHS, provisional 12-month ending birth rates, by five-year age group, through quarter one for years 2023 and 2024. In general, the NCHS provides this data for one and two years prior the year of the Trustees Report.

6. From the Centers for Disease Control (CDC), births in 2024, by month, through June. This data is updated as available.

## 1.1.c. Development of Output

### Equation 1.1.1 - Age-specific birth rates

The FERTILITY subprocess produces the age-specific birth rates, by childbearing ages 14 through 49, for years 1941 through the end of the 75-year projection period. For historical years prior to 1980, age-specific birth rates come from the NCHS. For years 1980 through the remainder of the historical period, age-specific birth rates are calculated as: $b_x^z = \frac{B_x^z}{P_x^z}$, using birth data from the NCHS and estimates of the July 1st female resident population from the U.S. Census Bureau.

The age-specific birth rates are projected using a process that is consistent with both the observed trends in recent data and the assumed ultimate cohort total fertility rate. This process consists of the following steps:

1. Calculate the age-specific birth rates, $b_x^z$, for each year during the period 1980-2023. In addition, calculate estimated 2024 births rates, $b_x^{2024}$, using the estimated total fertility of 1.62 using selected state data births and residential populations from Census, NCHS provisional rates by age group, and single year of age birth rates from 2023, $b_x^{2023}$.

2. Calculate the ratios of rates at each age to the age 30 rate, $r_x^z$, in each year from 1980 - 2024, using the formula $r_x^z = \frac{b_x^z}{b_{30}^z}$.

3. Calculate the prior year ratio of $r_x^z$, $p_x^z$, in each year from 1981 - 2024, using the formula $p_x^z = \frac{r_x^z}{r_x^{z-1}}$.

4. Calculate the average values of $p_x^z$, $a_x$, at each age from 1981 - 2024 excluding 1997. 1997 is excluded because NCHS changed their age imputation method for low and high mother ages in 1997.[^3]

5. Calculate projected $r_x^z$ values starting in 2025 using the formula $r_x^z = r_x^{z-1} * a_x$.

6. Calculate ultimate years for each age, $u_x$, using the formula $u_x^z = 2025 + (x - 14) * \frac{2050 - 202}{49 - 14}$ rounded to the nearest year. For instance, age 30 attains its ultimate year in 2036.

7. Calculate the weights to use on the ultimate age 30 rate in each year. This weight is designated by $w^z$ and is calculated using the formula $w^z = 1 - \left(\frac{2036 - z}{2036 - 2024}\right)^{1.5}$.

8. Calculate the ultimate age 30 rate, $b_{30}^{2036}$. This is done by finding the value of $b_{30}^{2036}$ that will hit the ultimate TFR. In formula form, this is done using the formula:

$$b_{30}^{2036} = \left(\frac{1.90 - \sum_{x=14}^{49} (1 - w^{u_x}) * b_{30}^{2024} * p_x^{u_x}}{\sum_{x=14}^{49} w^{u_x} * p_x^{u_x}}\right)$$

9. Calculate age 30 projected rates from 2025 - 2035, $b_{30}^z$, using the formula $b_{30}^z = b_{30}^{2024} * (1 - w_x) + b_{30}^{2036} * w_x$.

10. Calculate the remaining age projected rates, $b_x^z$, using the formula $b_x^z = b_{30}^z * r_x^z$ for years through $u_x^z$. After the ultimate year for each age, $u_x^z$, $b_x^z$ stays constant.

[^2]: Births at ages less than 14 are treated as having occurred at age 14, and births reported to mothers older than 49 are treated as having occurred at age 49.

[^3]: See "Age of mother" on page 88 of *Births: Final Data for 2000* for more information at: https://www.cdc.gov/nchs/data/nvsr/nvsr50/nvsr50_05.pdf.

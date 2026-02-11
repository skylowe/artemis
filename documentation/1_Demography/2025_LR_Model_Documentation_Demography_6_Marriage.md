# 1.6. MARRIAGE

## 1.6.a Overview

The National Center for Health Statistics (NCHS) collected detailed data on the annual number of new marriages in the Marriage Registration Area (MRA), by age of husband crossed with age of wife, for the period 1978 through 1988 (excluding 1980). In 1988, the MRA consisted of 42 States and D.C. and accounted for 80 percent of all marriages in the U.S. Estimates of the unmarried population in the MRA, by single year of age (or age group if single year of age was not available) and sex, were obtained from the NCHS. Marriage rates for this period are calculated from these data. The age-of-husband crossed with age-of-wife marriage grid rates are transformed from age grouped numbers to single year of age figures from ages 14 to 100+ for husband and wife using the two-dimensional H.S. Beers method of interpolation.

Beginning in 1989, the NCHS no longer collected data on the annual number of new marriages in the MRA. However, for years 1989-95, they supplied less detailed data on new marriages from a subset of the MRA. Beginning in 2008, the American Community Survey (ACS) started asking if a person was married in the last 12 months. Using this question, along with ages of spouses, grids of new marriages by age-group-of-husband crossed with age-group-of-wife were developed for years 2007 and later. For the years between 1995 and 2007, the marriage grids were linearly interpolated.

Age-specific marriage rates ($\hat{m}_{x,y}^z$) for a given year (z) are defined as the ratio of (1) the number of marriages for a given age-of-husband (x) crossed with age-of-wife (y) to (2) a theoretical midyear unmarried population at those ages ($P_{x,y}^z$). The theoretical midyear population is defined as the geometric mean[^11] of the midyear unmarried male population and unmarried female population.

An age-adjusted central marriage rate ($\widehat{AMR}^z$) summarizes the $\hat{m}_{x,y}^z$ for a given year. The standard population chosen for age adjusting is the unmarried male population and unmarried female population in the Social Security area population as of July 1, 2010. The first step in calculating the total age-adjusted central marriage rate for a particular year is to determine an expected number of marriages by applying the age-of-husband-age-of-wife specific central marriage rates for that year to the geometric mean of the corresponding age groups in the standard population.

The $\widehat{AMR}^z$ is then obtained by dividing:

- The expected number of marriages by
- The geometric mean of (1) the number of unmarried men, ages 15 and older, and (2) the unmarried women, ages 15 and older, in the standard population.

The MARRIAGE subprocess projects annual $\hat{m}_{x,y}^z$ by age-of-husband crossed with age-of-wife. The equations for this subprocess are given below:

$$\hat{m}_{x,y}^z = \hat{m}_{x,y}^z(\cdot) \tag{1.6.1}$$

$$\widehat{AMR}^z = \frac{\sum_{x,y} P_{x,y}^S \cdot \hat{m}_{x,y}^z}{\sum_{x,y} P_{x,y}^S} \tag{1.6.2}$$

where x and y refer to the age of men and women, respectively, and $P_{x,y}^S$ is the theoretical unmarried population in the Social Security area population as of July 1, 2010 (the geometric mean of the corresponding age groups in the standard population).

## 1.6.b. Input Data

*Long-Range OASDI Projection Data -*

Demography

1. Estimates of the Social Security area population as of December 31, by age, sex, and marital status for years 1977-2022. These data are updated each year based on output of the HISTORICAL POPULATION subprocess.

2. Final historical year (on a December 31 basis). This datum is updated each year from the HISTORICAL POPULATION subprocess.

*Assumptions -*

3. For each Trustees Report, ultimate values for the $\widehat{AMR}^z$ are assumed. The $\widehat{AMR}^z$ reaches its ultimate value in the 25th year of the 75-year projection period. For the 2025 report, the ultimate $\widehat{AMR}^z$ assumption is 4,000 per 100,000 unmarried couples.

*NCHS Data -*

4. Number of new marriages in the MRA, by age-of-husband crossed with age-of-wife, for calendar years 1978 through 1988, excluding 1980. These data are not available for years after 1988. The data vary in detail by year. They are broken out by single year age-of-husband crossed with single year age-of-wife for many ages (particularly younger ages).

5. Number of unmarried men and women in the MRA for calendar years 1978 through 1988, excluding 1980. These data are not available for years after 1988. The data are generally broken out by single year age for ages under 40 and by age groups 40-44, 45-49, 50-54, 55-59, 60-64, 65-74, and 75+.

6. Number of new marriages, in a subset of the MRA, by age-group-of-husband crossed with age-group-of-wife (age groups include 15-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, and 65+), for calendar years 1989-95. These data are updated as new data become available and internal resources are sufficient to examine and interpret such new data.

7. The total number of new marriages in the MRA less marriages in those states not included in the MRA unmarried population for the period 1957-88. These data are not updated.

8. The total number of new marriages in the United States for the period 1989-2022. Normally, each year, the NCHS publishes the total number of marriages for one more year.

9. Number of new marriages in the MRA for years 1979 and 1981-88 by age group (age groups include 14-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, and 65+), sex, and prior marital status (single, widowed, and divorced). These data are not available for years after 1988.

10. Number of unmarried people in the MRA (in thousands) for years 1982-1988 by age group (age groups include 14-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, and 65+), sex, and prior marital status (single, widowed, and divorced). These data are not available for years after 1988.

11. Total marriages and remarriages for years 1979 and 1981-88.

*U.S. Census Bureau Data -*

12. Estimates of new marriages by age-group-of-husband crossed with age-group-of-wife from the American Community Survey (ACS) public use microdata sample (PUMS) files occurring, on average, at the end of years 2007-2022. An additional year of data is available each year.

13. Data for the 2010 standard population. The marriage program uses the unmarried population (single + widow + divorce).

14. Number of unmarried men and women in the March CPS for calendar years 1957 through 1995. These data are not updated. The data are by age groups 14-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, and 65+. These data are not updated.

*Other Input Data -*

15. From the vital statistics offices in various states, number of same-sex marriages from 2004-12. These data are updated as they become available.

## 1.6.c. Development of Output

### Equation 1.6.1 - Age-Specific Marriage Rates

Age-specific marriage rates are determined for a given age-of-husband crossed with age-of-wife, where ages range from 14 through 100+. The historical period includes years of complete NCHS data on the number of marriages and the unmarried population in the MRA for the period 1978 through 1988, excluding 1980. Data for a subset of the MRA, available by age group only, are used for the period 1989 through 1995, and ACS new-married grids by age group are used for the period 2008 through 2022. The marriage grids by age group for the years 1996 through 2007 are linearly interpolated. The total number of marriages from NCHS are also used in the age-specific marriage rate calculations for the period 1989-2022. The projection period of the MARRIAGE subprocess begins in 2023.

The historical age-specific marriage rates are calculated for each year in the historical period based on NCHS data of the number of new marriages by age-of-husband crossed with age-of-wife and the number of unmarried persons by age and sex. The formula used in the calculations is given below:

$$\hat{m}_{x,y}^z = \frac{\hat{M}_{x,y}^z}{P_{x,y}^z}$$

where

- x refers to the age of men and y refers to the age of women;
- $\hat{M}_{x,y}^z$ is the number of marriages in year z; and
- $P_{x,y}^z$ is the geometric mean of the midyear unmarried men and unmarried women in year z.

The rates for the period 1978 through 1988[^12] are then averaged, graduated, and loaded into an 87 by 87 matrix (age-of-husband crossed with age-of-wife for ages 14 through 100+), denoted as MarGrid. This matrix is used in the calculation of the age-specific marriage rates for all later historical years and the years in the projection period.

For the period 1989-1995, the NCHS provided data on the number of marriages by age-group-of-husband crossed with age-group-of-wife (age groups include 15-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, and 65+) and, starting with 2007, the ACS provides data on the number of marriages by age of husband crossed with age of wife. These data are used to change the distribution of MarGrid by these age groups. For each age-group-of-husband crossed with age-group-of-wife, the more detailed marriage rates in MarGrid that are contained within this group are adjusted so that the number of marriages obtained by using the rates in MarGrid match the number implied in the subset.

For each year of the entire 1989-2022 period, an expected total number of marriages is calculated by multiplying the rates in the MarGrid (or the adjusted MarGrid) by the corresponding geometric mean of the unmarried men and unmarried women in the Social Security area population. All rates in MarGrid (or the adjusted MarGrid) are then proportionally adjusted to correspond to the total number of marriages estimated in the year for the Social Security area population. This estimate is obtained by increasing the number of marriages reported in the U.S. to reflect the difference between the Social Security area population and the U.S. population. In addition, we also subtract out same-sex marriages from the NCHS data, as we handle those in a later step. The age-specific rates are then graduated using the two-dimensional Whittaker-Henderson method and are used to calculate the age-adjusted rates for each year.

The age-adjusted marriage rates are expected to reach their ultimate value in the 25th year of the 75-year projection period. Rather than use the last year of data to calculate the starting rate, we calculate the weighted average of the rates for the past five historical data years to derive the starting value. The annual rate of change decreases in absolute value as the ultimate year approaches.

To obtain the age-of-husband-age-of-wife-specific rates for a particular year from the age-adjusted rate projected for that year, the age-of-husband-age-of-wife-specific rates in MarGrid are proportionally scaled so as to produce the age-adjusted rate for the particular year. The MarGrid rates are then adjusted to produce two sets of marriage rates: opposite-sex marriage rates and same-sex marriage rates.

A complete projection of age-of-husband-age-of-wife-specific marriage rates was not done separately for each previous marital status. However, data indicate that the differential in marriage rates by prior marital status is significant. Thus, future relative differences in marriage rates by prior marital status are assumed to be the same as the average of those experienced during 1979 and 1981-88.

[^11]: The geometric mean, as used in this document, is the square root of the product of two numbers.
[^12]: Data for 1980 is not available and is excluded from the calculations.

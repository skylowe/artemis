# 1.7. DIVORCE

## 1.7.a. Overview

For the period 1979 through 1988, the National Center for Health Statistics (NCHS) collected data on the annual number of divorces in the Divorce Registration Area (DRA), by age-group-of-husband crossed with age-group-of-wife. In 1988, the DRA consisted of 31 States and accounted for about 48 percent of all divorces in the U.S. These data are then inflated to represent an estimate of the total number of divorces in the Social Security area. This estimate for the Social Security area is based on the total number of divorces in the 50 States, the District of Columbia, Puerto Rico, and the Virgin Islands. Divorce rates for this period are calculated using this adjusted data on number of divorces and estimates of the married population by age and sex in the Social Security area.

An age-of-husband (x) crossed with age-of-wife (y) specific divorce rate ($\hat{d}_{x,y}^z$) for a given year (z) is defined as the ratio of (1) the number of divorces in the Social Security area for the given age of husband and wife ($\hat{D}_{x,y}^z$) to (2) the corresponding number of married couples in the Social Security area ($P_{x,y}^z$) with the given age of husband and wife. An age-adjusted central divorce rate ($\widehat{ADR}^z$) summarizes the $\hat{d}_{x,y}^z$ for a given year.

The $\widehat{ADR}^z$ is calculated by determining the expected number of divorces by applying:

- The age-of-husband crossed with age-of-wife specific divorce rates to
- The July 1, 2010, population of married couples in the Social Security area by corresponding age-of-husband and age-of-wife.

The DIVORCE subprocess projects annual $\hat{d}_{x,y}^z$ by age-of-husband crossed with age-of-wife. The primary equations are given below:

$$\hat{d}_{x,y}^z = \hat{d}_{x,y}^z(\cdot) \tag{1.7.1}$$

$$\widehat{ADR}^z = \frac{\sum_{x,y} P_{x,y}^S \cdot \hat{d}_{x,y}^z}{\sum_{x,y} P_{x,y}^S} \tag{1.7.2}$$

where x and y refer to the age of husband and age of wife, respectively, and $P_{x,y}^S$ is the number of married couples in the Social Security area population as of July 1, 2010.

## 1.7.b. Input Data

*Long-Range OASDI Projection Data -*

Demography

1. Social Security area population of married couples by age-of-husband crossed with age-of-wife as of December 31 for years 1978-2022. These data are updated each year from the HISTORICAL POPULATION subprocess. In addition, the standard population is based on the averaged 2009 and 2010 December 31 marriage grids from the 2015 TR.

2. The total July 1 population in the Social Security area for years 1979-2022. However, only years 1979-1988, 1998-2000, and 2008-2022 data are used. An additional year of data is added for each additional year of divorce data from the NCHS with a maximum of the final historical year.

3. The total July 1 population in the U.S. resident population plus armed forces overseas for years 1979-2022. However, only years 1979-1988, 1998-2000, and 2008-2022 data are used. An additional year of data is added for each additional year of divorce data from the NCHS with a maximum of the final historical year.

4. The total July 1 population in Puerto Rico and the Virgin Islands for years 1988-2022. However, only years 1988, 1998-2000, and 2008-2022 are used. An additional year of data is added for each additional year of divorce data from the NCHS with a maximum of the final historical year.

5. Final historical year (on a December 31 basis). This datum is updated each year from the HISTORICAL POPULATION subprocess.

*Assumptions -*

6. Each year, the assumed ultimate value for the age-adjusted divorce rate is established. The rate reaches its ultimate value in the 25th year of the 75-year projection period. For the 2025 report, the assumed ultimate $\widehat{ADR}^z$ is 1,700 per 100,000 married couples.

*NCHS Data -*

7. The number of divorces in the DRA, by age-of-husband crossed with age-of-wife, for calendar years 1979 through 1988. These data are not available for years after 1988. The data are broken out by single year age-of-husband crossed with single year age-of-wife for many ages (particularly younger ages).

8. The total number of divorces in the United States for the period for 1979-2022. For years 1992 and later, the number of divorces is derived by multiplying the rate times the population. Data is updated when it becomes available.

9. The total number of divorces in Puerto Rico and the Virgin Islands for years 1988, 1998, 1999, and 2000. New data are incorporated as they become available and resources are sufficient to validate their use.

*State Divorce Data -*

10. Since NCHS stopped collecting state-specific divorce data by age of husband crossed with age of wife, we directly contacted various state health departments for their most recent data. We were able to get this data from 18 states. The years and age groups available vary by state. In general, the years were from 2009-12. These 18 states that had these data available online, or that sent us the data via email, are Alabama, Alaska, Idaho, Kansas, Kentucky, Michigan, Missouri, Montana, Nebraska, New Hampshire, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, and Wyoming.

*Census Bureau Data -*

11. The number of divorces for years 2008-2022 in Puerto Rico, estimated using the 2008-2022 (excluding 2020) American Community Survey (ACS) public use microdata sample (PUMS) files. A new year of data is generally available each year.

## 1.7.c. Development of Output

### Equation 1.7.1

Age-specific divorce rates are calculated for ages 14 through 100+. Detailed NCHS data on the number of divorces by age-group-of-husband crossed with age-group-of-wife are available for the period 1979 through 1988. Data on the total number of divorces in the United States are used for the period 1989 through 2022. With the data from the various states, we developed an age-group-of-husband crossed with age-group-of-wife grid for 2011.

First, the detailed NCHS data on divorces by age group is disaggregated into single year of age of husband (x) and age of wife (y), for ages 14-100+, using the H.S. Beers method of interpolation. Then, the age-specific divorce rates ($\hat{d}_{x,y}^z$), for each year (z) are calculated for the period 1979-1988 by taking the number of divorces (inflated to represent the Social Security area, $\hat{D}_{x,y}^z$) and dividing by the married population in the Social Security area at that age-of-husband and age-of-wife ($P_{x,y}^z$). The formula for this calculation is given below:

$$\hat{d}_{x,y}^z = \frac{\hat{D}_{x,y}^z}{P_{x,y}^z} \tag{1.7.3}$$

These rates are then averaged, graduated,[^13] and loaded into an 87 by 87 matrix (age-of-husband crossed with age-of-wife for ages 14 through 100+), denoted as DivGrid. DivGrid is then adjusted using the state data grid developed for 2011. DivGrid for years after 1988 is a weighted average of the 1988 DivGrid and the 2011 state data single year grid. This state data single year of age grid is derived by ratioing the 1988 DivGrid cells using the original state age-group data. DivGrid is used in the calculation of the age-specific divorce rates for all later years including the projection period.

For each year in the 1989-2022 period, an expected number of total divorces in the Social Security area is obtained by applying the age-of-husband crossed with age-of-wife rates in DivGrid to the corresponding married population in the Social Security area. The rates in DivGrid are then proportionally adjusted so that they would yield an estimate of the total number of divorces in the Social Security area. The estimate of total divorces is obtained by adjusting the reported number of divorces in the U.S. for (1) the differences between the total divorces in the U.S. and in the combined U.S., Puerto Rico, and Virgin Islands area, and (2) the difference between the population in the combined U.S., Puerto Rico, and Virgin Islands area and in the Social Security area.

The starting age-adjusted divorce rate is set to a weighted average of the past five years of data. This age-adjusted rate is assumed to reach its ultimate value in the 25th year of the 75-year projection period. The annual rate of change decreases in absolute value as the ultimate year approaches.

To obtain age-specific rates for use in the projections, the age-of-husband-age-of-wife-specific rates in DivGrid are adjusted proportionally so as to produce the age-adjusted rate assumed for that particular year.

[^13]: Using a two-dimensional Whittaker-Henderson method of graduation from http://www.howardfamily.ca/graduation/index.html. This graduation is also used by the Society of Actuaries in their mortality improvement scales.

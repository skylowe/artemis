# 1.8. PROJECTED POPULATION

## 1.8.a. Overview

For the 2025 Trustees Report, the starting population for the population projections is the December 31, 2022, Social Security area population, by age, sex, and marital status, produced by the HISTORICAL POPULATION subprocess. (For this section, section 1.8, the term "starting year" refers to the year 2022.) The Social Security area population is then projected using a component method. The components of change include births, deaths, net LPR immigration, and net temporary or unlawfully present immigration. The components of change are applied to the starting population by age and sex to prepare estimated populations as of December 31, 2023 and 2024, and to project the population through the 75-year projection period (years 2025-99).

Beginning with December 31, 2013, the historical and projected populations are modeled using the following population statuses: heterosexual, gay, and lesbian. The gay and lesbian populations in the HISTORICAL POPULATION program are broken out assuming 2.5% of the male population and 4.5% of the female population is gay or lesbian, and the same is true for cohorts born in the PROJECTED POPULATION program.

There is a separate equation for each of the components of change as follows:

$$B_{s,p}^z = B_{s,p}^z(\cdot) \tag{1.8.1}$$

where $B_{s,p}^z$ is the number of births of each sex (s) by population status (p) born in year z;

$$D_{x,s,p}^z = D_{x,s,p}^z(\cdot) \tag{1.8.2}$$

where $D_{x,s,p}^z$ is the number of deaths by age (x), sex (s), and population status (p) that occurs in year z; and

$$NI_{x,s}^z = NL_{x,s}^z + NO_{x,s}^z \tag{1.8.3}$$

where $NI_{x,s}^z$ is the total net immigration by age (x), sex (s), and population status (p), $NL_{x,s}^z$ is the net LPR immigration (produced by the LPR IMMIGRATION subprocess), and $NO_{x,s}^z$ is the net temporary or unlawfully present immigration (produced by the TEMPORARY OR UNLAWFULLY PRESENT IMMIGRATION subprocess). The population program further disaggregates the new immigration, $NI_{x,s}^z$, by population status into $NI_{x,s,p}^z$.

Once the components of change are calculated, the following equation is used to calculate the Social Security area population by age, sex, and population status:

$$P_{0,s,p} = B_{s,p}^z - D_{0,s,p}^z + NI_{0,s,p}^z \quad \text{for age = 0}$$

$$P_{x,s,p}^z = P_{x-1,s,p}^{z-1} - D_{x,s,p}^z + NI_{x,s,p}^z \quad \text{for ages > 0} \tag{1.8.4}$$

where $P_{x,s,p}^z$ is the population, by age (x), sex (s), and population status (p), as of December 31st of each year (z).

The population is further disaggregated into the following four marital statuses: single (never married), married, widowed, and divorced. The following equation shows the population by age (x), sex (s), population status (p), and marital status (m) for each year (z):

$$P_{x,s,p,m}^z = P_{x,s,p,m}^z(\cdot) \tag{1.8.5}$$

The children (ages 0-18) population is further disaggregated into the following four parent statuses (i.e., fates): both parents are alive, only father is alive, only mother is alive, and both parents deceased. The following equation shows the children population by age of child (x), sex of parent (s), age group of parent (g), and fate of parent (f) for each year (z):

$$C_{x,s,g,f}^z = C_{x,s,g,f}^z(\cdot) \tag{1.8.6}$$

The civilian noninstitutionalized (CNI) population is also projected using total population from equation 1.8.5, the net immigration from equation 1.8.3, and data from the HISTORICAL POPULATION subprocess. The CNI population is disaggregated into the following five marital statuses: single (never married), married (spouse present), separated, widowed, and divorced. The following equation shows the population by age (x), sex (s), and marital status (m) for each year (z):

$$N_{x,s,m}^z = N_{x,s,m}^z(\cdot) \tag{1.8.7}$$

## 1.8.b. Input Data

*Long-Range OASDI Projection Data -*

Demography

FERTILITY

1. Historical birth rates by single year of age of mother (14-49) for the years beginning with 1941 and ending with the starting year. These data are updated each year.

2. Projected birth rates by single year of age of mother (14-49) for the years beginning with the year after the starting year and ending with 2105. These data are updated each year.

MORTALITY

3. Historical probabilities of death by age last birthday (including neonatal mortality factor, single year of age for ages 0-99, and age group 100+) and sex for years beginning with 1941 and ending with the starting year. These data are updated each year.

4. Projected probabilities of death by age last birthday (including neonatal mortality factor, single year of age for ages 0-99, and age group 100+) and sex for the years beginning with the year after the starting year and ending with 2105. These data are updated each year.

5. Factors to distribute probabilities of death by marital status. They are dimensioned by sex, single year of age (ages 14-100+), and marital status. These data are updated each year.

LPR IMMIGRATION

6. Projected numbers of LPR immigrants who are new arrivals, by single year of age (-1-100) and sex for years beginning with the year after the starting year and ending with 2105. These data are updated each year. Note that age -1 represents births that occur during the year.

7. Projected numbers of legal emigrants by single year of age (-1-100) and sex for years beginning with the year after the starting year and ending with 2105. These data are updated each year. Note that age -1 represents births that occur during the year.

8. Projected numbers of LPR immigrants who are adjustments of status, by single year of age (-1-100) and sex for years beginning with the year after the starting year and ending with 2105. These data are updated each year. Note that age -1 represents births that occur during the year.

HISTORICAL POPULATION

9. Social Security area population by single year of age (0-99 and 100+), sex, marital status, and population status (eligible for or in an opposite-sex, same-sex male, or same-sex female marriage) for the years beginning with 1940 and ending with the starting year. These data are updated each year.

10. Married couples by single year of age of spouse 1 (ages 14-100+) crossed with single year of age of spouse 2 (ages 14-100+) for the years beginning with 1940 and ending with the starting year by marriage type (opposite-sex, same-sex male, and same-sex female). These data are updated each year.

11. Temporary or unlawfully present population by age and sex for the years beginning with 1963 and ending with the starting year. These data are updated each year.

12. Final historical year (on a December 31 basis). This datum is updated each year from the HISTORICAL POPULATION subprocess.

13. December CNI population by sex, single year of age, and marital status from 2010 through the starting year. These data are updated each year.

14. December overseas armed forces population by sex and single year of age from 2010 through the starting year. These data are updated each year.

15. December armed forces population by sex and single year of age from 2010 through the starting year. These data are updated each year.

16. Estimates of January U.S resident population, adjusted for consistency with the Social Security area population, by sex and single year of age for 0 through 99, and ages 100 and older from 2011 through the starting year plus one. These data are updated each year.

17. Estimates of January U.S resident population plus armed forces overseas population, adjusted for consistency with the Social Security area population, by sex and single year of age for 0 through 99, and ages 100 and older from 2011 through the starting year plus one. These data are updated each year.

18. Assumed ratios of CNI population to total population by sex, single year of age, and marital status. These data are updated each year.

19. Averaged and smoothed adjustments to total population data marital status percentages by sex and single year of age in the Historical program. These data are updated each year.

20. Assumed ratios of separated to total married persons by sex and single year of age. These data are updated each year.

TEMPORARY OR UNLAWFULLY PRESENT IMMIGRATION

21. Projected numbers of temporary or unlawfully present immigrants entering the country by age (-1-100) and sex for years beginning with the year after the starting year and ending with 2105. These data are updated each year.

22. Projected numbers of temporary or unlawfully present immigrants leaving the country by age (-1-100) and sex for years beginning with the year after the starting year and ending with 2105. These data are updated each year.

23. Temporary or unlawfully present population by age and sex for the years beginning with the year after the starting year and ending with 2105. These data are updated each year.

MARRIAGE

24. Projected central marriage rates by single year of age of husband (ages 14-100+) crossed with single year of age of wife (ages 14-100+) for each year of the projection period. These data are updated each year.

25. Projected central same-sex marriage rates by single year of age of spouse 1 (ages 14-100+) crossed with single year of age of spouse 2 (ages 14-100+) for each year of the projection period. These data are updated each year.

26. Averaged and graduated marriage rates for the period 1979 and 1981-88 by single year of age (ages 14-100+), sex, and prior marital status (single, divorced, and widowed). These data are updated each year.

27. Total number of marriages for the years beginning with 1989 and ending with 2022. These data are updated each year.

DIVORCE

28. Projected central divorce rates by single year of age of husband (14-100+) crossed with single year of age of wife (14-100+) for each year of the projection period. These data are updated each year.

*U.S. Census Bureau Data -*

29. CPS data on the average number of children per married couple with children by age group of householder (age groups 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, and 55-64) for 1960-2022. (Note that the program splits the last age group, which is a 10-year age group, into two 5-year age groups.) An additional year of data is added each year.

30. January CNI population by sex and single year of age from 2011 - 2023. These data are updated each year.

31. January civilian population by sex and single year of age from 2011 - 2023. These data are updated each year.

32. July CNI population by sex and single year of age from 2010 - 2023. These data are updated each year.

*Other input data -*

33. From the National Center for Health Statistics (NCHS), births by month and sex of child, for years 1931-2023. Each year, NCHS provides another year of data.

## 1.8.c. Development of Output

### Equation 1.8.1 - Births

The number of births in the Social Security area, $B_x^z$, is computed for each year, z, of the projection period by applying the age-specific birth rate to the midyear female population aged 14 to 49 as follows:

$$B_x^z = b_x^z \left(\frac{FP_x^z + FP_x^{z+1}}{2}\right)$$

where,

- $B_x^z$ = number of births to mothers age x in year z;
- $b_x^z$ = birth rate of mothers age x in year z; and
- $FP_x^z$ = female population age x at the beginning of year z.

The total number of births in a given year is the sum of the number of births to mothers at each age. This total number of births is disaggregated by sex by assuming a sex ratio of 1,048 male births for every 1,000 female births. The total number of births is also disaggregated by population status by assuming 2.5% of boys that are born are gay and 4.5% of girls that are born are lesbian. (Note: These percentages include assuming that one-half of those born bisexual will want to enter into a same-sex marriage.)

### Equation 1.8.2 - Deaths

The number of deaths for the Social Security area by age (x), sex (s), and population status (p), $D_{x,s,p}^z$, is computed for each projection year (z) by applying the death probabilities for each age and sex, $q_{x,s}^z$, to the exposed population at the beginning of the year.

$$D_{x,s,p}^z = q_{x,s}^z P_{x,s,p}^z$$

### Equation 1.8.5 - Disaggregating the population by marital status

Once the population is projected by single year of age, sex, and population status, it is then disaggregated by population status into the following four marital states; single, married, widowed, and divorced. Estimates of the Social Security area population by single year of age (0-99 and 100+), sex, marital status, and population status as of the starting year of the population projection are obtained from the HISTORICAL POPULATION subprocess. In addition, the HISTORICAL POPULATION subprocess provides the number of married couples by single year of age of husband crossed with single year of age of wife and number of married male/male and female/female marriages, single year of age of spouse 1 crossed with single year of age of spouse 2, as of the starting year.

All births are assigned to the single marital status. For a given age, sex, and population status, deaths are assigned by marital status according to the relative differences in death rates by marital status observed for that age and sex during the calendar years 2015-2019, as determined in the MORTALITY subprocess. For a given age, sex, and population status, immigrants are assigned by marital status according to the beginning of year marital distribution of the Social Security area population for that age and sex.

Once the number of marriages, divorces, and widowings during a year are determined, the population by age, sex, population status, and marital status is updated to represent end of year. The unmarried population at the end of the year is estimated from the unmarried population at the beginning of the year by factoring in deaths, marriages, new unmarried immigrants, widowings, and divorces during the year. The married population at the end of the year is estimated from the married population at the beginning of the year by factoring in divorces, widowings, dissolutions of marriages when both husband and wife dies, new married immigrants, and marriages during the year.

Numbers of new marriages are determined for each projection year. The annual number of opposite-sex marriages occurring at each age of husband crossed with each age of wife is obtained by multiplying the age-of-husband-specific and age-of-wife-specific marriage rates with the geometric mean of the midyear unmarried male population and the midyear unmarried female population.

The age-specific midyear unmarried male population[^14] is estimated from the beginning of the year unmarried population. It is calculated by adjusting the number of unmarried men at the beginning of the year to represent midyear using the relationship between the prior beginning of year and the current beginning of year unmarried male populations. The midyear female unmarried population is approximated similarly.

The numbers of marriages are then distributed by previous marital status (single, widowed, divorced) in the same proportions as would have been produced by applying the previous marital-status-specific marriage rates from the MARRIAGE subprocess to the population by marital status at the beginning of the year.

Numbers of new divorces are determined for each projection year. The number of divorces during a year, occurring at each age of husband crossed with each age of wife, is obtained by multiplying the age-of-husband crossed with age-of-wife divorce rates for that year with the midyear number of married couples in that age crossing.

The number of age-of-husband crossed with age-of-wife midyear married couples is estimated from the beginning of the year married couples. It is calculated by adjusting the number of married couples at the beginning of the year to represent midyear using the relationship between the number of married couples at the beginning of the prior year and the beginning of the current year.

Marriages and divorces for same-sex couples are calculated similarly.

Widowings are computed by applying general population probabilities of death to the marriage prevalence at the beginning of the year. Widowings and deaths by marital status are then reconciled for internal consistency.

### Equation 1.8.6 - Disaggregating the children by parent survival status

Once the population is projected by single year of age, sex, population status, and marital status, the number of children are then categorized by age of father, age of mother, and orphan status. The HISTORICAL POPULATION subprocess provides the historical number of children (ages 0-18), number of women (ages 14-49), and the number of married couples by single year of age of husband crossed with single year of age of wife. The projected number of children (ages 0-18), number of women (ages 14-49), and marriage grid age of husband crossed with age of wife is calculated in the projected population.

For women aged 14-49, births are calculated by multiplying the age-specific birth rate, from the FERTILITY subprocess, with the number of women at the corresponding age. The births are then distributed to the age of husband in the same proportions as the age of husband crossed with age of wife married couples grid.

Each year the number of children is then rolled forward a year to the next age of husband, age of wife, and child age. Parent survival is calculated based on the deaths rates from the MORTALITY subprocess. The number of orphans consists of children with at least one parent deceased. The calculated number of children by age of father and age of mother must match the number of children in the historical or projected population. To accomplish this, the calculated number of children is multiplied by the ratio of the number of children in the historical or projected population to the number of children by age of father and age of mother that was calculated using the fertility rates. For any remaining difference, an adjustment of one is made for each age of husband crossed with age of wife until the total number of children match.

Once the population is projected by single year of age, sex, population status, marital status, and children, the mean number of children per married couple with children is determined by year and age of householder. The historical mean number of children by year and age of householder in the population program is calculated from the number of children categorized by age of father, age of mother, and the number of married men by age group from the HISTORICAL POPULATION subprocess. Linear regression is used to model the relationship between the mean number of children in the population program to the mean number of children from the U.S. Census Bureau. The regression model is then used to project the mean number of children by age of householder in the population program.

### Equation 1.8.7 - Projecting the civilian noninstitutionalized (CNI) population

Once the Social Security area population is projected by single year of age, sex, and marital status, the CNI population is projected by year, sex, single year of age, and the following five marital states; single, married (spouse present), separated, widowed, and divorced. Estimates of the residential plus armed forces overseas (USAF) and residential populations, by single year of age (0-99 and 100+) and sex, and CNI populations by single year of age (0-99 and 100+), sex, and marital status through the starting year are obtained from the HISTORICAL POPULATION subprocess.

The PROJECTED POPULATION subprocess keeps track of the USAF, residential, civilian, and CNI populations throughout the projection. The USAF population is projected forward by sex and single year of age by subtracting deaths using the same death rates applied to the Social Security area population, adding births in the residential population calculated with the same birth rates applied to the Social Security area, and adding net immigration prorated by the ratio of beginning of year USAF population to the Social Security area population.

The residential population is calculated by subtracting an assumed constant number of armed forces overseas by sex and single year of age from the USAF population. Likewise, the civilian population is calculated by subtracting an assumed constant number of total armed forces by sex and single year of age from the USAF population.

The CNI population is calculated by applying the ratios of CNI population to civilian population by year, sex, and single year of age in the starting year to the civilian population. Then, the CNI population is disaggregated into marital status by using: (1) the averaged and smoothed marital status adjustments to initial smoothed data by sex and single year of age, (2) assumed ratios of separated to total married population by sex and single year of age, (3) assumed ratios of CNI to total population by year, sex, single year of age, and marital status from the HISTORICAL POPULATION subprocess.

[^14]: The midyear population exposed to marriage is the unmarried population (sum of those single, widowed, and divorced).

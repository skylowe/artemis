# 1.5. TEMPORARY OR UNLAWFULLY PRESENT IMMIGRATION

## 1.5.a. Overview

The term "temporary or unlawfully present immigration" refers to persons entering the U.S. in a manner other than being lawfully admitted for permanent residence, and who reside in the U.S. for at least 6 months. This includes temporary immigrants (persons lawfully admitted for a limited period of time, such as temporary workers and foreign students), also called nonimmigrants, in addition to immigrants living in the U.S illegally. The latter immigrants can be split into those that were never authorized to enter the U.S. and those that were nonimmigrants but overstayed their visas (visa-overstayers). The term "temporary or unlawfully present emigration" refers to those in the temporary or unlawfully present immigrant population who leave the Social Security area.

For each year z of the projection period, the TEMPORARY OR UNLAWFULLY PRESENT IMMIGRATION subprocess produces estimates of temporary or unlawfully present immigration ($OI_{x,s,t}^z$), by age (x), sex, and type (t) based on assumptions set by the Trustees. Estimates of projected temporary or unlawfully present emigration ($OE_{x,s,t}^z$), by age, sex, and type are also developed in this subprocess.

The Department of Homeland Security (DHS) estimated the stock of nonimmigrants by age group and sex for April 2008, December 2010, and April 2016. The HISTORICAL POPULATION subprocess already produces historical estimates of the temporary or unlawfully present immigrant population. These historical data are used to develop recent estimates of the temporary or unlawfully present immigrant stock by age, sex, and type, where type is never-authorizeds, nonimmigrants, and visa-overstayers.

The primary equations of TEMPORARY OR UNLAWFULLY PRESENT IMMIGRATION, by age (x), sex (s), and type (t) for each year (z) of the 75-year projection period are summarized below:

$$OI_{x,s,t}^z = OI_{x,s,t}^z(\cdot) \tag{1.5.1}$$

$$OE_{x,s,t}^z = OE_{x,s,t}^z(\cdot) \tag{1.5.2}$$

$$NO_{x,s,t}^z = OI_{x,s,t}^z - OE_{x,s,t}^z - AOS_{x,s,t}^z \tag{1.5.3}$$

where $NO_{x,s,t}^z$ are the number of net temporary or unlawfully present immigrants, by age (x), sex (s), and type (t) for year z, and $AOS_{x,s,t}^z$ are the number of adjustments to LPR status from the LPR IMMIGRATION subprocess, by age (x), sex (s), and type (t) for year z;

$$OP_{x,s,t}^z = OP_{x-1,s,t}^{z-1} + OI_{x,s,t}^z - OE_{x,s,t}^z - AOS_{x,s,t}^z - OD_{x,s,t}^z \tag{1.5.4}$$

where, $OP_{x,s,t}^z$ is equal to the temporary or unlawfully present immigrant population, by age (x), sex (s), and type (t) as of December 31st of each year (z), $OD_{x,s,t}^z$ are the number of deaths in the temporary or unlawfully present immigrant population by age (x), sex (s), and type (t) for each year (z), and $AOS_{x,s,t}^z$ are the number of adjustments to LPR status by age (x), sex (s), and type (t) for each year (z).

## 1.5.b. Input Data

*Trustees Assumptions -*

1. Each year the Board of Trustees of the OASDI Trust Funds specifies the assumed total annual values for temporary or unlawfully present immigration. The ultimate annual level is 1,350,000 for each year beginning in 2026. The level is estimated to be 2,200,000, 2,700,000, 2,600,000, and 2,000,000 for years 2022-25, respectively.

*Long-Range OASDI Projection Data -*

Demography

2. Historical and projected probabilities of death by age last birthday (including a neonatal mortality factor, single year of age for ages 0-99, and age group 100+) and sex, for years 1941-2105. These data are updated each year from the MORTALITY program.

3. Historical net temporary or unlawfully present immigration by single year of age (-1-99+)[^10] and sex for years 1961-2022. These data are updated each year from the HISTORICAL program.

4. Historical December 31 temporary or unlawfully present immigrants by single year of age (0-100+) and sex for years 1963-2022. These data are updated each year from the HISTORICAL program.

5. Historical July 1 temporary or unlawfully present immigrants by single year of age (0-100+) and sex for years 1964-2022. These data are updated each year from the HISTORICAL program.

6. Final historical year (on a December 31 basis). This datum is updated each year from the HISTORICAL program.

7. Historical new arrivals by single year of age (-1-100+) and sex for years 1941-2022. These data are updated each year from the LPR IMMIGRATION program.

8. Historical and projected adjustments of status by single year of age (-1-100+) and sex for years 1941-2105. These data are updated each year from the LPR IMMIGRATION program.

*Department of Homeland Security -*

9. Components of the unauthorized immigrant population by year for 2005-12, and unauthorized immigrant population undercounts by year for 2015-2020 and 2022, from the Unauthorized Immigrant Population Reports. These data are updated as new data become available.

10. Components of the LPR population by year for 2005-11, from the Unauthorized Immigrant Population Reports. Components of the LPR population by year for 2012-23, from unpublished DHS estimates. These data are updated as new data become available.

11. Nonimmigrant stock in April 2008, December 2010, and April 2016 by age-group and sex. These data are updated as new data become available and internal resources are sufficient to examine and interpret such new data.

12. Nonimmigrant admissions by class of admission for various fiscal years 1981-2016. These data are updated as new data become available and internal resources are sufficient to examine and interpret such new data.

13. Total beginning-of-year nonimmigrants for 2005-11, 2018-2020, and 2022, from the Unauthorized Immigrant Population Reports. Total beginning-of-year nonimmigrants for 2012-2017 and 2021, from unpublished DHS estimates. These data are updated as new data become available.

14. Total annual approvals for initial grants under the 2012 Deferred Action for Childhood Arrivals (DACA) initiative, for fiscal years 2013-18. These data are updated as new data become available.

15. End-of-year population of people with DACA status, by sex (including unknown) and single year of age (0-50, including unknown), for years 2013-19. These data are updated as new data become available.

*U.S. Census Bureau -*

16. From the American Community Survey (ACS), foreign-born new persons by ACS year (2000-22), entry year (1900-2022), age (0-100) and sex. These data are updated as new data become available.

17. From the ACS, total population for 2006-22 and total population in Puerto Rico for 2005-19 and 2021-22, used to calculate undercount factors (Note that the population referred to in each case is the beginning-of-year population). These data are updated as new data become available.

18. From the 2012 ACS, persons, by entry year (1900-2012), age (0-100) and sex, that are:
   - Foreign-born citizens
   - Foreign-born non-citizens that are in school or are high-school graduates
   - Non-citizen parents of citizen children
   - Non-citizen parents of citizen children that are in school or are high school graduates

   These data are not updated.

19. From the 2012 ACS, persons, by entry year (1900-2012), age (0-100) and sex, that are eligible for temporary protected status (TPS) based on originating from various countries by certain dates and are:
   - Foreign-born citizens
   - Foreign-born non-citizens that are in school or are high-school graduates
   - Non-citizen parents of citizen children
   - Non-citizen parents of citizen children that are in school or are high school graduates

   These data are not updated.

*Other input data -*

20. The number of those potentially eligible under the 2012 DACA initiative by age group and an overall split by sex, from the Migration Policy Institute. These data are updated as new data become available and internal resources are sufficient to examine and interpret such new data.

21. Internally developed numbers of those that were potentially eligible under the 2014 DACA initiative that were not eligible under the 2012 DACA initiative by age and sex. These data will not be updated.

22. Internally developed numbers of those that were potentially eligible under the Deferred Action for Parents of Americans and LPRs (DAPA) initiative, by age and sex. These data will not be updated.

23. Internally developed factors of potential DACA stock attaining DACA status by sex and ages 5-100 for the first, second, and ultimate DACA years. These data are updated as new data become available and internal resources are sufficient to examine and interpret such new data.

24. Internally developed factors used to create the nonimmigrant other in distribution by age and sex for each year. These factors ensure that there will be enough nonimmigrant stock to transfer to LPR status. These data will not be updated.

25. Internally developed overstay percentages by age. These data are based off a RAND Corporation document using data from the 1980s, and are adjusted based on insights from the DHS. These data are updated as new data become available and internal resources are sufficient to examine and interpret such new data.

26. Internally developed rates of departure for the non-DACA/DAPA potential/actual never-authorizeds for non-recent arrivals. These data are updated as new data become available and internal resources are sufficient to examine and interpret such new data.

27. Internally developed rates of departure for the non-DACA/DAPA potential/actual non-immigrants. These data are set to initial rates in 2015 (when the Executive Actions went into effect including decreased deportation of non-felons) and then gradually increase to the ultimate rates in 2025. These data are updated as new data become available and internal resources are sufficient to examine and interpret such new data.

28. Internally developed rates of departure for the non-DACA/DAPA potential/actual visa overstayers. These data are updated as new data become available and internal resources are sufficient to examine and interpret such new data.

29. Internally developed rates of departure for non-DACA potential/actual never authorizeds for non-recent arrivals to use prior to 2015 (when the Executive Actions went into effect including decreased deportation of non-felons). These data are updated as new data become available and internal resources are sufficient to examine and interpret such new data.

30. Internally developed rates of departure for the non-DACA potential/actual visa overstayers to use prior to 2015 (when the Executive Actions went into effect including decreased deportation of non-felons). These data are updated as new data become available and internal resources are sufficient to examine and interpret such new data.

31. Internally developed number of temporary or unlawfully present immigrants, by age and sex. These data were averaged over years 2015-2019 and will not be updated.

32. From the National Center for Health Statistics (NCHS), births by month and sex of child, for years 1931-2023. Each year, NCHS provides another year of data.

## 1.5.c. Development of Output

The ACS provides data to help derive the number of foreign-born new arrivals, which is then used to separate the historical net temporary or unlawfully present immigration into those entering and those leaving. There are several other key inputs that go into this calculation, including an estimated undercount factor. This factor accounts for (1) differences between the foreign-born data from the ACS and the component pieces obtained from DHS, (2) differences between the ACS (Public Use Microdata Sample) and Census' total population, and (3) the foreign-born residing in Puerto Rico. The estimated temporary or unlawfully present immigration is calculated by taking the foreign born from the ACS (after smoothing and applying the undercount factors) and subtracting the LPR new arrivals. The estimated historical temporary or unlawfully present emigration is then calculated as the difference between the net temporary or unlawfully present immigration (calculated in the HISTORICAL subprocess) and the estimated historical temporary or unlawfully present immigration. A series of steps are then taken to smooth the two categories. Based on various assumptions, the historical temporary or unlawfully present immigrants are split into those who arrive or depart the Social Security area as a never-authorized immigrant, nonimmigrant, and visa-overstayer immigrant.

### Equation 1.5.1 - Temporary or Unlawfully Present Immigration

For each projection year, an age-sex-type distribution is used to distribute the aggregate number of temporary or unlawfully present immigrants by age, sex, and type. This age-sex-type distribution is denoted as $ODIST_{x,s,t}$ and is developed by using average historical estimates of temporary or unlawfully present immigrants entering the country from 2015 through 2019.

The assumed total level of temporary or unlawfully present immigration is denoted by $TO^z$. Thus, for each year (z) temporary or unlawfully present immigration is defined by the following equation:

$$OI_{x,s,t}^z = TO^z \cdot ODIST_{x,s,t}$$

### Equation 1.5.2 - Temporary or Unlawfully Present Emigration

$OE_{x,s,t}^z$ denotes the annual number of temporary or unlawfully present immigrants who depart the Social Security area by age (x), sex (s), and type (t). These estimates are based on 2014 TR build-up of stocks from 2008 through 2010 including temporary or unlawfully present immigration discussed above, deaths, adjustments of status (from the LPR IMMIGRATION subprocess), and assumptions about the number of departures from each type. Deaths for the temporary or unlawfully present immigrant population use the same death probabilities as the total population:

$$OD_{x,s,t}^z = q_{x,s}^z \cdot OP_{x,s,t}^z$$

Then, for this 2008-10 period, rates are calculated by dividing $OE_{x,s,t}^z$ by $OP_{x,s,t}^z$ for each age, sex, and type. After smoothing and adjusting for the effects of the recent recession, these rates are used to calculate $OE_{x,s,t}^z$ in projected years by being applied to the temporary or unlawfully present stock populations $OP_{x,s,t}^{z-1}$ for the overstayer and nonimmigrant stocks. For the never authorized stock, these rates are further adjusted and split into two categories so that recent arrivals are exposed to twice the rates as the residual never authorized stock. For the potential DACA population, and for those that are already in the DACA population, the exit rates are lower than for those not eligible for DACA.

This subprocess also splits historical temporary or unlawfully present immigrants into the various categories. It is assumed that all temporary or unlawfully present immigrants were nonimmigrants as of December 31, 1963. Between December 31, 1963, and December 31, 2010, the percentage of total temporary or unlawfully present immigrants by age and sex in each type is linearly interpolated from the percentages at those two points in time. Between December 31, 2010, and December 31, 2015, a similar interpolation is done from the percentages at those two points in time. A final adjustment ensures the total nonimmigrants are appropriate, based on DHS nonimmigrant admissions or, if available, stock estimates.

Finally, this subprocess also projects the DACA population, a subset of the temporary or unlawfully present immigrant population, by age (x) and sex (s). The DACA population consists of temporary or unlawfully present immigrants who meet specific criteria and are granted authorization to work. The eligible DACA population is estimated separately by those that meet the age, residency, and educational requirements. Rates are applied to the eligible population to estimate the net number of individuals who actually apply and obtain DACA status. A final adjustment ensures that the DACA population by age and sex is appropriate, based on DHS stock estimates.

Note that the DAPA and the 2014 DACA are no longer being applied to this subprocess. Furthermore, it is estimated that there were an insignificant number of new 2012 DACA's for years 2019-20 and 2022-23. These assumptions may change according to future laws, executive actions, and/or court rulings that may affect the DACA program.

[^10]: Age -1 represents births that occur during the year.

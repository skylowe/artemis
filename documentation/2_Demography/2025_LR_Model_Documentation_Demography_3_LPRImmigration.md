# 1.3. LPR IMMIGRATION

## 1.3.a. Overview

LPR immigration is defined as those persons who have been admitted into the United States and been granted lawful permanent resident status. Legal emigration consists of LPR immigrants and U.S. citizens who depart the Social Security area population to reside elsewhere.

For each year z of the projection period, the LPR IMMIGRATION subprocess produces estimates of LPR immigration ($L^z$) and legal emigration ($E^z$), by age and sex, based on assumptions set by the Trustees for each category. In addition, the LPR IMMIGRATION subprocess disaggregates the estimates of $L^z$ into those who have been admitted into the United States during the year ($NEW^z$) and those who adjusted from the temporary or unlawfully present population to LPR status ($AOS^z$).

Each fiscal year,[^8] the Department of Homeland Security (DHS) collects data on the number of persons granted LPR status by age, sex, and class of admission. The U.S Census Bureau provided OCACT with an unpublished estimate of the annual number of legal emigrants, by sex and age, based on the change between the 1980 and 1990 censuses. These historical data are used as a basis for developing age-sex distributions that are applied to the Trustees' aggregate immigration assumptions to produce annual LPR immigration and annual legal emigration estimates by age and sex.

The primary equations of LPR IMMIGRATION, by age (x) and sex (s), for each year (z) of the 75-year projection period are summarized below:

$$NEW_{x,s}^z = NEW_{x,s}^z(\cdot) \tag{1.3.1}$$

$$AOS_{x,s}^z = AOS_{x,s}^z(\cdot) \tag{1.3.2}$$

$$L_{x,s}^z = NEW_{x,s}^z + AOS_{x,s}^z \tag{1.3.3}$$

$$E_{x,s}^z = E_{x,s}^z(\cdot) \tag{1.3.4}$$

$$NL_{x,s}^z = L_{x,s}^z - E_{x,s}^z \tag{1.3.5}$$

where $NL_{x,s}^z$ are the number of net LPR immigrants by age (x) and sex (s) for year z.

## 1.3.b. Input Data

*Trustees Assumptions -*

1. Each year the Board of Trustees of the OASDI Trust Funds specifies the total annual assumed values for LPR immigration and legal emigration. For the 2025 Trustees Report, the ultimate values for LPR immigration and legal emigration are 1,050,000 and 262,500, respectively (both reached in 2027). The level of LPR immigration is estimated to be 1,263,000 in 2024 and then decrease to 1,213,000 for years 2025-26.

*Department of Homeland Security -*

2. Historical LPR immigration by fiscal year (1941-72), 5-year age group (0-4, 5-9, ..., and 80-84), and sex. These data will not be updated.

3. Legalizations due to Immigration Reform and Control Act of 1986 (IRCA) by type (pre-1982s and SAWs), single year of age (0-99 and unknown age), sex (including unknown) and month for the years 1989-2023. These data will not be updated.

4. Historical LPR immigration for fiscal years 1973-2013, the last three months of calendar year 2013, and calendar years 2014-2023, by single year of age (0 through 99, and unknown age), sex (including unknown), and class of admission (New Arrival, Adjustment of Status, Refugee, and Asylee). These data are updated annually, with the DHS providing an additional year of data each year.

5. Total adjustments of status for the years 1963 to 1972. These data will not be updated.

*U.S. Census Bureau -*

6. Unpublished estimates of annual legal emigration by five-year age groups (0-4, 5-9, ..., and 80-84) and sex for 1990 based on the change between the 1980 and 1990 censuses. These data are updated occasionally (based on having new data from an outside source and on OCACT resource time constraints).

*Other input data -*

7. Legal emigration conversion factors. These estimates were developed internally by five-year age groups (0-4, 5-9, ..., and 80-84) and sex to reflect the fact that the estimated number of people leaving the United States is not equivalent to the number of people leaving the Social Security area. These data are updated when annual legal emigration estimates are updated (see above).

## 1.3.c. Development of Output

### Equations 1.3.1 and 1.3.2 - LPR Immigration

The Trustees specify the aggregate number of LPR immigrants for each year of the 75-year projection period. In order to incorporate the numbers of new immigrants into the Social Security area population projections, the total level of new immigrants is disaggregated by age and sex.

There are two ways for an immigrant to be admitted into the U.S. for lawful permanent residence:

(1) New arrivals, such as persons living abroad who are granted an LPR visa and then enter the U.S. through a port of entry. Refugees and asylees that are granted LPR status are also treated as new arrivals in the OCACT model.

(2) Adjustments of status, who are people already residing in the U.S. as temporary or unlawfully present immigrants and have an application for adjustment to LPR status approved by the DHS.

The DHS provides data on LPR immigrants by sex, single year of age, classification of admission, and fiscal year of entry. The data for years 2016-2020 are used to calculate separate age-sex distributions for both new arrivals and adjustments of status by taking the following steps:

1. Refugee and Asylee LPR admissions are subtracted from the DHS adjustment of status data and added into the new arrival category.
2. The data are converted from fiscal year data to calendar year data.
3. For each class of admission (new arrivals and adjustments of status), the historical data for years 2016-2020 are combined to create an average age-sex distribution.

$NEW_{x,s}^z$, the expected number of new arrival LPR immigrants by age (x) and sex (s) for each year (z), is calculated by applying the age-sex distribution for new arrivals to the Trustees' assumed level of new arrivals. The Trustees' assumed number of adjustments of status is multiplied by the age-sex distribution of adjustments of status to calculate $AOS_{x,s}^z$.

### Equation 1.3.4 - Legal Emigration

The Trustees specify the aggregate amount of legal emigration for each year of the projection period. This is done by assuming a ratio of legal emigration to LPR immigration. For the 2025 Trustees Report, the ratio is assumed to be 25 percent.

In order to produce the number of emigrants from the Social Security area population, the total level of emigrants is disaggregated by age and sex. The disaggregation is based on a distribution of emigrants, by sex and five-year age groups, provided to OCACT in unpublished estimates by Census that are based on changes between the 1980 and 1990 censuses. Since the emigration numbers estimated by Census are for all people leaving the United States, they are adjusted downward by a series of conversion factors so the data correspond to the number of people leaving the Social Security area population.

For each sex (s), the Beers formula is used to interpolate and distribute each five-year age group into a single year of age (x) distribution, $EDIST_{x,s}$. For each projection year, this distribution is used to distribute the assumed level of total legal emigrants by age and sex using the following equation:

$$E_{x,s}^z = .25 * \left(\sum_{s=m}^{f} \sum_{x=0}^{100} L_{x,s}^z\right) \cdot EDIST_{x,s}$$

[^8]: The federal fiscal year begins on October 1 of the previous calendar year and ends on September 30 of the specified calendar year.

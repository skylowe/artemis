# Test Census API
library(httr2)
library(data.table)
library(cli)
library(glue)
library(checkmate)

# Load our functions
source("R/utils/api_helpers.R")
source("R/utils/config.R")
source("R/data_acquisition/census_population.R")

# Test 1: Recent years (2020-2023) - vintage_2023 endpoint
cat("\n=== Test 1: 2020-2023 (vintage_2023) ===\n")
result_2020s <- fetch_census_population(
  years = 2020:2023,
  ages = 14:49,
  sex = "female"
)
print(result_2020s[1:10])
cat("\nSummary by year:\n")
print(result_2020s[, .(total_pop = sum(population)), by = year])

# Test 2: 2010s (vintage_2019 endpoint)
cat("\n\n=== Test 2: 2015-2019 (vintage_2019) ===\n")
result_2010s <- fetch_census_population(
  years = 2015:2019,
  ages = 14:49,
  sex = "female"
)
print(result_2010s[1:10])
cat("\nSummary by year:\n")
print(result_2010s[, .(total_pop = sum(population)), by = year])

# Test 3: 2000s (vintage_2000 endpoint)
cat("\n\n=== Test 3: 2005-2009 (vintage_2000) ===\n")
result_2000s <- fetch_census_population(
  years = 2005:2009,
  ages = 14:49,
  sex = "female"
)
print(result_2000s[1:10])
cat("\nSummary by year:\n")
print(result_2000s[, .(total_pop = sum(population)), by = year])

# Test 4: 1990s (vintage_1990 endpoint)
cat("\n\n=== Test 4: 1995-1999 (vintage_1990) ===\n")
result_1990s <- fetch_census_population(
  years = 1995:1999,
  ages = 14:49,
  sex = "female"
)
print(result_1990s[1:10])
cat("\nSummary by year:\n")
print(result_1990s[, .(total_pop = sum(population)), by = year])

# Test 5: Multi-decade fetch
cat("\n\n=== Test 5: Multi-decade (1995-2023) ===\n")
result_all <- fetch_census_population(
  years = c(1995, 2000, 2005, 2010, 2015, 2020, 2023),
  ages = 14:49,
  sex = "female"
)
cat("\nSummary by year:\n")
print(result_all[, .(total_pop = sum(population)), by = year])

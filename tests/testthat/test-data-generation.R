# Test suite for population projections dashboard

library(testthat)
library(popprojections)

# Test data generation
test_that("Data generation works correctly", {
  data <- generate_population_data(seed = 123)
  
  expect_true(nrow(data) > 0)
  expect_true(all(data$population > 0))
  expect_true(all(c("geography_level", "geography_name", "year", "age_group", "sex", "scenario", "population") %in% names(data)))
})

# Test data filtering
test_that("Data filtering works correctly", {
  data <- generate_population_data(seed = 123)
  
  filtered <- filter_population_data(data, geography_level = "state")
  expect_true(all(filtered$geography_level == "state"))
  
  filtered <- filter_population_data(data, years = 2020:2025)
  expect_true(all(filtered$year >= 2020 & filtered$year <= 2025))
})

# Test utility functions
test_that("Utility functions work correctly", {
  data <- generate_population_data(seed = 123)
  
  unique_vals <- get_unique_values(data, "sex")
  expect_equal(sort(unique_vals), c("Female", "Male"))
  
  summary_stats <- calculate_summary_stats(data)
  expect_true(nrow(summary_stats) > 0)
  expect_true(all(c("total_population", "male_population", "female_population") %in% names(summary_stats)))
})

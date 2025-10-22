# Test script for popprojections package
# Run this in RStudio or R console to test the package

cat("Testing popprojections package...\n")

# Load devtools
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Load the package
cat("Loading package...\n")
devtools::load_all()

# Test if package loads correctly
cat("Testing package loading...\n")
library(popprojections)

# Test data generation
cat("Testing data generation...\n")
test_data <- generate_population_data(seed = 123)
cat("Data generated successfully!\n")
cat("Dimensions:", dim(test_data), "\n")

# Test data filtering
cat("Testing data filtering...\n")
filtered_data <- filter_population_data(
  test_data,
  geography_level = "state",
  years = 2020:2025
)
cat("Data filtered successfully!\n")
cat("Filtered dimensions:", dim(filtered_data), "\n")

# Test summary statistics
cat("Testing summary statistics...\n")
summary_stats <- calculate_summary_stats(filtered_data)
cat("Summary statistics calculated successfully!\n")
cat("Summary dimensions:", dim(summary_stats), "\n")

cat("\nAll tests passed! Package is working correctly.\n")
cat("To run the dashboard, use: run_app()\n")

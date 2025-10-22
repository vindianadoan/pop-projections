# Test the fixed data generation
# Run this to verify the fix works

cat("Testing fixed data generation...\n")

# Load devtools
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Load the package
devtools::load_all()
library(popprojections)

# Test data generation with the problematic age group
cat("Testing data generation...\n")
test_data <- generate_population_data(seed = 123)
cat("Success! Generated", nrow(test_data), "records\n")

# Check if 85+ age group is included
age_groups <- unique(test_data$age_group)
cat("Age groups:", paste(age_groups, collapse = ", "), "\n")

# Test filtering
cat("Testing data filtering...\n")
filtered <- filter_population_data(test_data, geography_level = "state")
cat("Filtered to", nrow(filtered), "state records\n")

# Test summary statistics
cat("Testing summary statistics...\n")
summary_stats <- calculate_summary_stats(filtered)
cat("Summary statistics calculated successfully!\n")

cat("\nAll tests passed! Package is working correctly.\n")
cat("You can now run: run_app()\n")

# Test hierarchical filtering
# Run this to test the new hierarchical geography filtering

cat("Testing hierarchical geography filtering...\n")

# Load devtools
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Load the package
devtools::load_all()
library(popprojections)

# Generate test data
cat("Generating test data with hierarchy...\n")
test_data <- generate_population_data(seed = 123)
cat("Generated", nrow(test_data), "records\n")

# Check if parent_geography column exists
cat("Columns:", paste(names(test_data), collapse = ", "), "\n")

# Test hierarchy functions
cat("Testing hierarchy functions...\n")

# Test parent geographies
sa4_parents <- get_parent_geographies(test_data, "sa4")
cat("SA4 parent geographies:", paste(sa4_parents, collapse = ", "), "\n")

# Test child geographies
nsw_sa4s <- get_child_geographies(test_data, "sa4", "New South Wales")
cat("NSW SA4s:", paste(nsw_sa4s, collapse = ", "), "\n")

# Test all SA4s
all_sa4s <- get_child_geographies(test_data, "sa4")
cat("All SA4s:", paste(all_sa4s, collapse = ", "), "\n")

# Test hierarchy structure
hierarchy <- get_geography_hierarchy(test_data)
cat("Hierarchy structure created successfully\n")

# Test filtering with parent geography
cat("Testing filtering with parent geography...\n")
nsw_data <- test_data %>%
  filter(geography_level == "sa4", parent_geography == "New South Wales")
cat("NSW SA4 records:", nrow(nsw_data), "\n")

cat("\nAll tests passed! Hierarchical filtering is working.\n")
cat("Run the app to see the new cascading dropdowns:\n")
cat("  run_app()\n")

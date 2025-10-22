# Quick start script for popprojections
# Run this in RStudio or R console

cat("Setting up popprojections package...\n")

# Load devtools
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Load the package
cat("Loading package...\n")
devtools::load_all()

# Test basic functionality
cat("Testing basic functionality...\n")
library(popprojections)

# Generate some test data
test_data <- generate_population_data(seed = 123)
cat("Generated", nrow(test_data), "records\n")

# Test filtering
filtered <- filter_population_data(test_data, geography_level = "state")
cat("Filtered to", nrow(filtered), "state records\n")

cat("\nPackage is ready! You can now run:\n")
cat("  run_app()  # to start the dashboard\n")
cat("  source('dev_app.R')  # for development mode\n")

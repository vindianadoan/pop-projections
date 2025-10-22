# Generate and save population projection data
# This script creates the synthetic data and saves it for the application

# Load required libraries
library(dplyr)

# Source the data generation function
source("R/data_generation.R")

# Generate the data
cat("Generating population projection data...\n")
population_data <- generate_population_data(seed = 123)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Save the data
saveRDS(population_data, "data/population_projections.rds")

# Print summary
cat("Data generated successfully!\n")
cat("Total records:", nrow(population_data), "\n")
cat("Geography levels:", paste(unique(population_data$geography_level), collapse = ", "), "\n")
cat("Age groups:", length(unique(population_data$age_group)), "\n")
cat("Years:", min(population_data$year), "to", max(population_data$year), "\n")
cat("Scenarios:", paste(unique(population_data$scenario), collapse = ", "), "\n")
cat("Data saved to: data/population_projections.rds\n")

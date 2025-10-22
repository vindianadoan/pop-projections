# Development and debugging script
# Use this script to test and debug the application

# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinyWidgets)
library(shinyjs)

# Enable debugging options
options(shiny.reactlog = TRUE)
options(shiny.trace = TRUE)

# Load the package
devtools::load_all()
library(popprojections)

# Generate test data
cat("Generating test data...\n")
test_data <- generate_population_data(seed = 123)

# Test data generation
cat("Testing data generation...\n")
cat("Data dimensions:", dim(test_data), "\n")
cat("Unique geography levels:", length(unique(test_data$geography_level)), "\n")
cat("Unique age groups:", length(unique(test_data$age_group)), "\n")

# Test filtering functions
cat("Testing data filtering...\n")
filtered_data <- filter_population_data(
  test_data,
  geography_level = "state",
  years = 2020:2025,
  scenarios = "Medium"
)
cat("Filtered data dimensions:", dim(filtered_data), "\n")

# Test summary statistics
cat("Testing summary statistics...\n")
summary_stats <- calculate_summary_stats(filtered_data)
cat("Summary stats dimensions:", dim(summary_stats), "\n")

# Test unique values function
cat("Testing unique values...\n")
unique_sexes <- get_unique_values(test_data, "sex")
cat("Unique sexes:", paste(unique_sexes, collapse = ", "), "\n")

# Run the app
cat("Starting Shiny application...\n")
cat("Press Ctrl+C to stop the application\n")
cat("Press Ctrl+F3 to open reactive log visualizer\n")

# Run the app
shiny::runApp("inst/app", launch.browser = TRUE)

# Test the Shiny app directly
# Run this to test the app without package installation

cat("Testing Shiny app directly...\n")

# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinyWidgets)
library(shinyjs)

# Source the functions directly
source("R/data_generation.R")
source("R/data_utils.R")

# Test data generation
cat("Generating test data...\n")
test_data <- generate_population_data(seed = 123)
cat("Generated", nrow(test_data), "records\n")

# Test the app
cat("Starting Shiny app...\n")
cat("Press Ctrl+C to stop\n")

# Run the app directly
shiny::runApp("inst/app", launch.browser = TRUE)

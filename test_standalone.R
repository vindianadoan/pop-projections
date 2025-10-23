# Test script for standalone app
# This script tests if the standalone app can load without errors

cat("Testing standalone app...\n")

# Test if we can source the required files
tryCatch({
  source("R/data_utils.R")
  cat("✓ data_utils.R loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading data_utils.R:", e$message, "\n")
})

tryCatch({
  source("R/data_generation.R")
  cat("✓ data_generation.R loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading data_generation.R:", e$message, "\n")
})

# Test if we can load the data
tryCatch({
  population_data <- load_population_data()
  cat("✓ Population data loaded successfully\n")
  cat("  Rows:", nrow(population_data), "\n")
  cat("  Columns:", paste(names(population_data), collapse = ", "), "\n")
}, error = function(e) {
  cat("✗ Error loading population data:", e$message, "\n")
})

# Test if we can load required libraries
required_packages <- c("shiny", "dplyr", "ggplot2", "plotly", "DT", "shinyWidgets", 
                       "shinyjs", "collapse", "absmapsdata", "tidyr", "scales", 
                       "viridis", "RColorBrewer", "lubridate", "stringr", 
                       "purrr", "magrittr")

cat("Testing required packages...\n")
for (pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("✓", pkg, "loaded successfully\n")
  }, error = function(e) {
    cat("✗ Error loading", pkg, ":", e$message, "\n")
  })
}

cat("Standalone app test completed!\n")

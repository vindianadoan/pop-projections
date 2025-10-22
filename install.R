# Installation script for Population Projections Dashboard
# Run this script to install all required dependencies

cat("Installing required R packages...\n")

# List of required packages
required_packages <- c(
  "shiny",
  "dplyr", 
  "ggplot2",
  "plotly",
  "DT",
  "shinyWidgets",
  "shinyjs",
  "tidyr",
  "scales",
  "viridis",
  "RColorBrewer",
  "lubridate",
  "stringr",
  "purrr",
  "magrittr",
  "devtools",
  "testthat"
)

# Install packages if not already installed
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    cat("Installing", package, "...\n")
    install.packages(package)
  } else {
    cat(package, "is already installed.\n")
  }
}

# Install absmapsdata from GitHub
cat("Installing absmapsdata from GitHub...\n")
if (!requireNamespace("absmapsdata", quietly = TRUE)) {
  devtools::install_github("wfmackey/absmapsdata")
} else {
  cat("absmapsdata is already installed.\n")
}

# Generate data
cat("Generating synthetic data...\n")
source("generate_data.R")

cat("\nInstallation complete!\n")
cat("To run the dashboard, use:\n")
cat("  source('dev_app.R')\n")
cat("  or\n")
cat("  devtools::load_all()\n")
cat("  run_app()\n")

# Population Projections Dashboard

A comprehensive R Shiny dashboard for comparing population projections across multiple Australian Bureau of Statistics (ABS) geographies, age groups, and sexes.

## Features

- **Multi-level Geography Support**: Compare projections at National, State, SA4, SA3, and GCC levels
- **Interactive Visualizations**: Dynamic plots with Plotly integration
- **Age-Sex Analysis**: Population pyramids and demographic breakdowns
- **Scenario Comparison**: Low, Medium, and High projection scenarios
- **Data Export**: Export filtered data in multiple formats
- **Responsive Design**: Modern UI with ShinyWidgets

## Installation

### Prerequisites

- R version 4.0.0 or higher
- Required R packages (see DESCRIPTION file)

### Setup

1. Clone or download this repository
2. Install dependencies:

```r
pkgs <- c(
  "shiny", "dplyr", "ggplot2", "plotly", "DT", "sf",
  "leaflet", "shinyWidgets", "shinyjs", "tidyr",
  "scales", "viridis", "RColorBrewer", "lubridate",
  "stringr", "purrr", "magrittr", "devtools"
)
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))
renv::install(pkgs)

# GitHub package (no devtools needed)
renv::install("wfmackey/absmapsdata")
```

3. Build and install the package:

```r
# Build the package
devtools::load_all()

# Or install the package
devtools::install()
```

## Usage

### Running the Dashboard

```r
# Load the package
library(popprojections)

# Run the dashboard
run_app()
```

### Development Mode

For development and debugging:

```r
# Load all functions
devtools::load_all()

# Run the app directly
shiny::runApp("inst/app")
```

## Project Structure

```
popprojections/
├── R/                          # R source code
│   ├── run_app.R              # Main app launcher
│   ├── data_generation.R       # Synthetic data generation
│   └── data_utils.R           # Data manipulation utilities
├── inst/
│   └── app/                   # Shiny application
│       ├── app.R              # Main Shiny app
│       └── www/               # Static assets (CSS, JS, images)
├── data/                      # Data files
├── tests/                     # Test suite
│   └── testthat/
├── man/                       # Documentation
├── DESCRIPTION                # Package metadata
├── NAMESPACE                  # Package namespace
└── README.md                  # This file
```

## Data Structure

The synthetic data includes:

- **Geography Levels**: National, State, SA4, SA3, GCC
- **Age Groups**: 0-4, 5-9, ..., 85+ (5-year age groups)
- **Sex**: Male, Female
- **Years**: 2020-2050
- **Scenarios**: Low, Medium, High projections

## Development Guidelines

### Best Practices

1. **Modular Code**: Functions are organized by purpose in separate R files
2. **Testing**: Unit tests are included for core functionality
3. **Documentation**: Functions are documented with roxygen2
4. **Error Handling**: Graceful error handling throughout the application
5. **Performance**: Efficient data filtering and aggregation

### Debugging

The application includes several debugging features:

1. **Console Logging**: Use `message()` for debugging output
2. **Reactive Logging**: Enable with `options(shiny.reactlog = TRUE)`
3. **Browser Debugging**: Insert `browser()` statements for interactive debugging

### Adding New Features

1. **New Visualizations**: Add new plot functions in `R/data_utils.R`
2. **New Data Sources**: Extend `generate_population_data()` function
3. **New Geography Levels**: Update geography definitions in data generation
4. **New UI Components**: Add to the UI in `inst/app/app.R`

## Testing

Run the test suite:

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-data-generation.R")
```

## Dependencies

### Core Packages
- `shiny`: Web application framework
- `dplyr`: Data manipulation
- `ggplot2`: Static plotting
- `plotly`: Interactive plotting
- `DT`: Interactive tables

### Additional Packages
- `sf`: Spatial data handling
- `leaflet`: Interactive maps
- `shinyWidgets`: Enhanced UI components
- `shinyjs`: JavaScript integration
- `absmapsdata`: ABS geography data

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Submit a pull request

## License

MIT License - see LICENSE file for details

## Support

For issues and questions:
1. Check the test suite for expected behavior
2. Review the debugging section above
3. Create an issue with detailed description and reproducible example

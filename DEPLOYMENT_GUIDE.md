# Population Projections Dashboard - Deployment Guide

## Problem
When deploying your dashboard elsewhere, you get an error that the `popprojections` package is not installed. This happens because your app is structured as an R package and tries to load it with `library(popprojections)`.

## Solutions

### Solution 1: Use the Standalone Version (Recommended)

I've created a standalone version (`standalone_app.R`) that doesn't require the package to be installed. This is the easiest solution for deployment.

#### Files needed for deployment:
- `standalone_app.R` (the main app file)
- `R/data_utils.R` (utility functions)
- `R/data_generation.R` (data generation functions)
- `data/` folder (contains your data files)

#### Steps:
1. Copy these files to your deployment location
2. Make sure all required R packages are installed on the target system
3. Run: `Rscript standalone_app.R` or open in RStudio and click "Run App"

### Solution 2: Install the Package on Target System

If you want to keep using the package structure:

#### Option A: Install from local source
```r
# On the target system, navigate to your project directory and run:
devtools::install()
```

#### Option B: Install dependencies manually
```r
# Install all required packages
install.packages(c("shiny", "dplyr", "ggplot2", "plotly", "DT", "shinyWidgets", 
                   "shinyjs", "absmapsdata", "tidyr", "scales", "viridis", 
                   "RColorBrewer", "lubridate", "stringr", "purrr", "magrittr", 
                   "collapse"))
```

### Solution 3: Deploy to Shiny Server/Connect

#### For Shiny Server:
1. Use the standalone version
2. Place files in `/srv/shiny-server/your-app-name/`
3. Ensure all dependencies are installed on the server

#### For RStudio Connect:
1. Use the standalone version
2. Deploy as a Shiny application
3. RStudio Connect will handle dependency management

### Solution 4: Docker Deployment

Create a Dockerfile for containerized deployment:

```dockerfile
FROM rocker/shiny:4.3

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'dplyr', 'ggplot2', 'plotly', 'DT', 'shinyWidgets', 'shinyjs', 'absmapsdata', 'tidyr', 'scales', 'viridis', 'RColorBrewer', 'lubridate', 'stringr', 'purrr', 'magrittr', 'collapse'), repos='https://cran.rstudio.com/')"

# Copy app files
COPY standalone_app.R /srv/shiny-server/
COPY R/ /srv/shiny-server/R/
COPY data/ /srv/shiny-server/data/

# Set working directory
WORKDIR /srv/shiny-server

# Expose port
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp(port=3838, host='0.0.0.0')"]
```

## Required Dependencies

Make sure these R packages are installed on the target system:
- shiny
- dplyr
- ggplot2
- plotly
- DT
- shinyWidgets
- shinyjs
- absmapsdata
- tidyr
- scales
- viridis
- RColorBrewer
- lubridate
- stringr
- purrr
- magrittr
- collapse

## Quick Test

To test if everything works:

### Option 1: Using RStudio (Recommended)
1. Copy the standalone files to a new location
2. Open RStudio in that location
3. Open `standalone_app.R`
4. Click "Run App" or press Ctrl+Shift+Enter

### Option 2: Using R Console
1. Copy the standalone files to a new location
2. Open R console in that location
3. Run: `source("standalone_app.R")`

### Option 3: Test Dependencies First
Run the test script to check if all dependencies are available:
```r
source("test_standalone.R")
```

## Troubleshooting

### Common Issues:

1. **Package not found**: Use the standalone version
2. **Data files not found**: Make sure the `data/` folder is in the same directory as the app
3. **Function not found**: Make sure `R/data_utils.R` and `R/data_generation.R` are accessible
4. **Dependencies missing**: Install all required packages listed above

### File Structure for Deployment:
```
your-deployment-folder/
├── standalone_app.R
├── R/
│   ├── data_utils.R
│   └── data_generation.R
└── data/
    ├── population_projections.csv
    ├── population_projections.rds
    └── data_summary.rds
```

## Recommendation

**Use the standalone version (`standalone_app.R`)** - it's the most portable and deployment-friendly option. It eliminates the package dependency issue entirely while maintaining all the functionality of your original dashboard.

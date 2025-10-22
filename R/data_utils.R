#' Load and prepare population data
#'
#' @return Processed population data
#' @export
load_population_data <- function() {
  # Check if data exists, if not generate it
  data_file <- system.file("data", "population_projections.rds", package = "popprojections")
  
  # If not found in package, check project root
  if (data_file == "" || !file.exists(data_file)) {
    data_file <- "data/population_projections.rds"
  }
  
  if (file.exists(data_file)) {
    cat("Loading data from:", data_file, "\n")
    return(readRDS(data_file))
  } else {
    cat("No pre-generated data found. Generating data now...\n")
    cat("For faster startup, run: source('scripts/generate_data.R')\n")
    return(generate_population_data())
  }
}

#' Filter population data based on user selections
#'
#' @param data Population data
#' @param geography_level Selected geography level
#' @param geography_name Selected geography name
#' @param age_groups Selected age groups
#' @param sexes Selected sexes
#' @param years Selected years
#' @param scenarios Selected scenarios
#' @return Filtered data
#' @export
filter_population_data <- function(data, geography_level = NULL, geography_name = NULL,
                                  age_groups = NULL, sexes = NULL, years = NULL,
                                  scenarios = NULL) {
  filtered_data <- data
  
  if (!is.null(geography_level)) {
    filtered_data <- filtered_data[filtered_data$geography_level %in% geography_level, ]
  }
  
  if (!is.null(geography_name)) {
    filtered_data <- filtered_data[filtered_data$geography_name %in% geography_name, ]
  }
  
  if (!is.null(age_groups)) {
    filtered_data <- filtered_data[filtered_data$age_group %in% age_groups, ]
  }
  
  if (!is.null(sexes)) {
    filtered_data <- filtered_data[filtered_data$sex %in% sexes, ]
  }
  
  if (!is.null(years)) {
    filtered_data <- filtered_data[filtered_data$year %in% years, ]
  }
  
  if (!is.null(scenarios)) {
    filtered_data <- filtered_data[filtered_data$scenario %in% scenarios, ]
  }
  
  return(filtered_data)
}

#' Get unique values for dropdowns using collapse
#'
#' @param data Population data
#' @param column Column name
#' @return Unique values
#' @export
get_unique_values <- function(data, column) {
  unique(data[[column]])
}

#' Calculate summary statistics using collapse
#'
#' @param data Population data
#' @return Summary statistics
#' @export
calculate_summary_stats <- function(data) {
  data %>%
    fgroup_by(geography_level, geography_name, year, scenario) %>%
    fsummarise(
      total_population = fsum(population, na.rm = TRUE),
      male_population = fsum(population[sex == "Male"], na.rm = TRUE),
      female_population = fsum(population[sex == "Female"], na.rm = TRUE)
    )
}

#' Get parent geographies for a given geography level using collapse
#'
#' @param data Population data
#' @param geography_level Geography level
#' @return Vector of parent geography names
#' @export
get_parent_geographies <- function(data, geography_level) {
  if (geography_level == "national") {
    return(character(0))
  }
  
  # Use subset with logical indexing instead of fsubset
  subset_data <- data[data$geography_level == geography_level, ]
  unique_parents <- unique(subset_data$parent_geography)
  sort(unique_parents)
}

#' Get child geographies for a given parent using collapse
#'
#' @param data Population data
#' @param geography_level Geography level
#' @param parent_geography Parent geography name
#' @return Vector of child geography names
#' @export
get_child_geographies <- function(data, geography_level, parent_geography = NULL) {
  if (is.null(parent_geography)) {
    # Return all geographies for this level
    subset_data <- data[data$geography_level == geography_level, ]
    unique_children <- unique(subset_data$geography_name)
    sort(unique_children)
  } else {
    # Return geographies filtered by parent
    subset_data <- data[data$geography_level == geography_level & 
                       data$parent_geography == parent_geography, ]
    unique_children <- unique(subset_data$geography_name)
    sort(unique_children)
  }
}

#' Get geography hierarchy information using collapse
#'
#' @param data Population data
#' @return List with geography hierarchy structure
#' @export
get_geography_hierarchy <- function(data) {
  hierarchy <- list()
  
  for (level in unique(data$geography_level)) {
    hierarchy[[level]] <- list()
    
    if (level == "national") {
      hierarchy[[level]] <- unique(data$geography_name[data$geography_level == level])
    } else {
      level_data <- data[data$geography_level == level, ]
      parents <- unique(level_data$parent_geography)
      
      for (parent in parents) {
        children <- unique(level_data$geography_name[level_data$parent_geography == parent])
        hierarchy[[level]][[parent]] <- children
      }
    }
  }
  
  return(hierarchy)
}

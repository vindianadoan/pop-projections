#' Generate synthetic population projection data
#'
#' Creates realistic fake data for population projections across ABS geographies
#'
#' @param seed Random seed for reproducibility
#' @return A data frame with population projection data
#' @export
generate_population_data <- function(seed = 123) {
  set.seed(seed)
  
  # Define ABS geographies with realistic names and hierarchies
  geographies <- list(
    national = list(
      name = "Australia",
      parent = NA
    ),
    state = list(
      "New South Wales" = list(parent = "Australia"),
      "Victoria" = list(parent = "Australia"),
      "Queensland" = list(parent = "Australia"),
      "South Australia" = list(parent = "Australia"),
      "Western Australia" = list(parent = "Australia"),
      "Tasmania" = list(parent = "Australia"),
      "Northern Territory" = list(parent = "Australia"),
      "Australian Capital Territory" = list(parent = "Australia")
    ),
    sa4 = list(
      "Sydney - Inner West" = list(parent = "New South Wales"),
      "Sydney - City and Inner South" = list(parent = "New South Wales"),
      "Sydney - Eastern Suburbs" = list(parent = "New South Wales"),
      "Melbourne - Inner" = list(parent = "Victoria"),
      "Melbourne - Inner East" = list(parent = "Victoria"),
      "Melbourne - Inner South" = list(parent = "Victoria"),
      "Brisbane Inner City" = list(parent = "Queensland"),
      "Brisbane South" = list(parent = "Queensland"),
      "Brisbane North" = list(parent = "Queensland"),
      "Adelaide Central and Hills" = list(parent = "South Australia"),
      "Adelaide - North" = list(parent = "South Australia"),
      "Adelaide - South" = list(parent = "South Australia"),
      "Perth - Inner" = list(parent = "Western Australia"),
      "Perth - North East" = list(parent = "Western Australia"),
      "Perth - South East" = list(parent = "Western Australia")
    ),
    sa3 = list(
      "Sydney Inner City" = list(parent = "Sydney - Inner West"),
      "Sydney Eastern Suburbs" = list(parent = "Sydney - Eastern Suburbs"),
      "Sydney Inner West" = list(parent = "Sydney - Inner West"),
      "Melbourne City" = list(parent = "Melbourne - Inner"),
      "Melbourne Inner" = list(parent = "Melbourne - Inner"),
      "Melbourne South East" = list(parent = "Melbourne - Inner South"),
      "Brisbane Inner" = list(parent = "Brisbane Inner City"),
      "Brisbane South" = list(parent = "Brisbane South"),
      "Brisbane North" = list(parent = "Brisbane North"),
      "Adelaide City" = list(parent = "Adelaide Central and Hills"),
      "Adelaide Hills" = list(parent = "Adelaide Central and Hills"),
      "Adelaide North" = list(parent = "Adelaide - North"),
      "Perth City" = list(parent = "Perth - Inner"),
      "Perth North" = list(parent = "Perth - North East"),
      "Perth South" = list(parent = "Perth - South East")
    ),
    gcc = list(
      "Greater Sydney" = list(parent = "New South Wales"),
      "Greater Melbourne" = list(parent = "Victoria"),
      "Greater Brisbane" = list(parent = "Queensland"),
      "Greater Adelaide" = list(parent = "South Australia"),
      "Greater Perth" = list(parent = "Western Australia"),
      "Greater Hobart" = list(parent = "Tasmania"),
      "Greater Darwin" = list(parent = "Northern Territory"),
      "Australian Capital Territory" = list(parent = "Australian Capital Territory")
    )
  )
  
  # Define age groups and sexes
  age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                  "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                  "65-69", "70-74", "75-79", "80-84", "85+")
  sexes <- c("Male", "Female")
  
  # Projection years (full range for complete data)
  years <- 2020:2050
  
  # Pre-calculate multipliers for efficiency
  geo_multipliers <- c(
    "national" = 1000000,
    "state" = 100000,
    "sa4" = 10000,
    "sa3" = 5000,
    "gcc" = 50000
  )
  
  age_multipliers <- c(
    "0-4" = 0.06, "5-9" = 0.06, "10-14" = 0.06, "15-19" = 0.06,
    "20-24" = 0.07, "25-29" = 0.08, "30-34" = 0.08, "35-39" = 0.08,
    "40-44" = 0.08, "45-49" = 0.08, "50-54" = 0.07, "55-59" = 0.06,
    "60-64" = 0.05, "65-69" = 0.04, "70-74" = 0.03, "75-79" = 0.02,
    "80-84" = 0.01, "85+" = 0.005
  )
  
  # Generate base data more efficiently
  data_list <- list()
  
  for (geo_level in names(geographies)) {
    if (geo_level == "national") {
      geo_name <- geographies[[geo_level]]$name
      parent_geo <- geographies[[geo_level]]$parent
      
      for (year in years) {
        for (sex in sexes) {
          for (age_group in age_groups) {
            base_pop <- generate_realistic_population_fast(
              geo_level, geo_name, age_group, sex, year,
              geo_multipliers, age_multipliers
            )
            
            data_list[[length(data_list) + 1]] <- data.frame(
              geography_level = geo_level,
              geography_name = geo_name,
              parent_geography = parent_geo,
              year = year,
              age_group = age_group,
              sex = sex,
              population = base_pop,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    } else {
      for (geo_name in names(geographies[[geo_level]])) {
        parent_geo <- geographies[[geo_level]][[geo_name]]$parent
        
        for (year in years) {
          for (sex in sexes) {
            for (age_group in age_groups) {
              base_pop <- generate_realistic_population_fast(
                geo_level, geo_name, age_group, sex, year,
                geo_multipliers, age_multipliers
              )
              
              data_list[[length(data_list) + 1]] <- data.frame(
                geography_level = geo_level,
                geography_name = geo_name,
                parent_geography = parent_geo,
                year = year,
                age_group = age_group,
                sex = sex,
                population = base_pop,
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
    }
  }
  
  # Combine all data
  population_data <- do.call(rbind, data_list)
  
  # Add scenarios more efficiently
  scenarios <- c("Low", "Medium", "High")
  scenario_multipliers <- c("Low" = 0.95, "Medium" = 1.0, "High" = 1.05)
  
  scenario_data <- list()
  for (scenario in scenarios) {
    temp_data <- population_data
    temp_data$scenario <- scenario
    temp_data$population <- round(temp_data$population * scenario_multipliers[scenario])
    scenario_data[[scenario]] <- temp_data
  }
  
  # Combine scenarios
  final_data <- do.call(rbind, scenario_data)
  
  return(final_data)
}

#' Generate realistic population numbers (fast version)
#'
#' Optimized helper function to generate realistic population numbers
#'
#' @param geo_level Geography level
#' @param geo_name Geography name
#' @param age_group Age group
#' @param sex Sex
#' @param year Year
#' @param geo_multipliers Pre-calculated geography multipliers
#' @param age_multipliers Pre-calculated age multipliers
#' @return Population number
generate_realistic_population_fast <- function(geo_level, geo_name, age_group, sex, year, geo_multipliers, age_multipliers) {
  # Use pre-calculated multipliers
  geo_multiplier <- geo_multipliers[geo_level]
  age_multiplier <- age_multipliers[age_group]
  
  # Sex multiplier (slight male bias in younger ages, female bias in older ages)
  # Extract starting age from age group
  if (age_group == "85+") {
    start_age <- 85
  } else {
    start_age <- as.numeric(strsplit(age_group, "-")[[1]][1])
  }
  
  sex_multiplier <- if (sex == "Male") {
    if (start_age < 65) 1.02 else 0.95
  } else {
    if (start_age < 65) 0.98 else 1.05
  }
  
  # Year growth factor (slight growth over time)
  year_factor <- 1 + (year - 2020) * 0.01
  
  # Generate base population with some randomness
  base_pop <- geo_multiplier * age_multiplier * sex_multiplier * year_factor
  random_factor <- runif(1, 0.8, 1.2)
  
  return(round(base_pop * random_factor))
}

#' Generate realistic population numbers (original version)
#'
#' Helper function to generate realistic population numbers based on geography, age, and sex
#'
#' @param geo_level Geography level
#' @param geo_name Geography name
#' @param age_group Age group
#' @param sex Sex
#' @param year Year
#' @return Population number
generate_realistic_population <- function(geo_level, geo_name, age_group, sex, year) {
  # Base multipliers by geography level
  geo_multiplier <- switch(geo_level,
                          "national" = 1000000,
                          "state" = 100000,
                          "sa4" = 10000,
                          "sa3" = 5000,
                          "gcc" = 50000)
  
  # Age-specific multipliers (realistic age distribution)
  age_multiplier <- switch(age_group,
                          "0-4" = 0.06,
                          "5-9" = 0.06,
                          "10-14" = 0.06,
                          "15-19" = 0.06,
                          "20-24" = 0.07,
                          "25-29" = 0.08,
                          "30-34" = 0.08,
                          "35-39" = 0.08,
                          "40-44" = 0.08,
                          "45-49" = 0.08,
                          "50-54" = 0.07,
                          "55-59" = 0.06,
                          "60-64" = 0.05,
                          "65-69" = 0.04,
                          "70-74" = 0.03,
                          "75-79" = 0.02,
                          "80-84" = 0.01,
                          "85+" = 0.005)
  
  # Sex multiplier (slight male bias in younger ages, female bias in older ages)
  # Extract starting age from age group
  if (age_group == "85+") {
    start_age <- 85
  } else {
    start_age <- as.numeric(strsplit(age_group, "-")[[1]][1])
  }
  
  sex_multiplier <- if (sex == "Male") {
    if (start_age < 65) 1.02 else 0.95
  } else {
    if (start_age < 65) 0.98 else 1.05
  }
  
  # Year growth factor (slight growth over time)
  year_factor <- 1 + (year - 2020) * 0.01
  
  # Generate base population with some randomness
  base_pop <- geo_multiplier * age_multiplier * sex_multiplier * year_factor
  random_factor <- runif(1, 0.8, 1.2)
  
  return(round(base_pop * random_factor))
}

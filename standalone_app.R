# Population Projections Dashboard - Standalone Version
# This version doesn't require the popprojections package to be installed

# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(collapse)
library(absmapsdata)
library(tidyr)
library(scales)
library(viridis)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(purrr)
library(magrittr)

# Source the package functions directly
source("R/data_utils.R")
source("R/data_generation.R")

# Load pre-generated data efficiently
cat("=== POPULATION PROJECTIONS DASHBOARD STARTUP ===\n")
cat("Loading population data...\n")
population_data <- load_population_data()

# Data validation and summary
cat("=== DATA VALIDATION ===\n")
cat("Data loaded successfully!\n")
cat("Total rows:", nrow(population_data), "\n")
cat("Columns:", paste(names(population_data), collapse = ", "), "\n")
cat("Geography levels:", paste(unique(population_data$geography_level), collapse = ", "), "\n")
cat("Years range:", min(population_data$year), "to", max(population_data$year), "\n")
cat("Scenarios:", paste(unique(population_data$scenario), collapse = ", "), "\n")
cat("Age groups:", paste(unique(population_data$age_group), collapse = ", "), "\n")
cat("Sexes:", paste(unique(population_data$sex), collapse = ", "), "\n")

# Check for missing values
missing_summary <- sapply(population_data, function(x) sum(is.na(x)))
if (any(missing_summary > 0)) {
  cat("WARNING: Missing values found:\n")
  print(missing_summary[missing_summary > 0])
} else {
  cat("✓ No missing values found\n")
}

# Check population values
cat("Population statistics:\n")
cat("  Min:", min(population_data$population, na.rm = TRUE), "\n")
cat("  Max:", max(population_data$population, na.rm = TRUE), "\n")
cat("  Mean:", round(mean(population_data$population, na.rm = TRUE), 0), "\n")
cat("  Total:", sum(population_data$population, na.rm = TRUE), "\n")

cat("=== DASHBOARD READY ===\n")

# UI
ui <- fluidPage(
  useShinyjs(),
  
  # Include custom CSS and fonts
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Population Projections Dashboard")
  ),
  
  # Header
  h1("Population Projections Dashboard"),
  p("Compare population projections across ABS geographies"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Filters & Controls"),
      
      # Geography Level Selection
      selectInput("geography_level", 
                  "Geography Level",
                  choices = c("National" = "national",
                             "States and Territories" = "state", 
                             "GCC" = "gcc",
                             "SA4" = "sa4",
                             "SA3" = "sa3"),
                  selected = "state"),
      
      # State Filter (for sub-state levels only)
      uiOutput("state_filter_ui"),
      
      # GCC Filter (for SA3/SA4 levels only)
      uiOutput("gcc_filter_ui"),
      
      # Geography Name Selection (dynamic)
      uiOutput("geography_name_ui"),
      
      # Age Group Selection
      pickerInput("age_groups",
                  "Age Groups",
                  choices = get_unique_values(population_data, "age_group"),
                  selected = get_unique_values(population_data, "age_group"),
                  options = list(`actions-box` = TRUE, 
                                `live-search` = TRUE,
                                `selected-text-format` = "count > 3"),
                  multiple = TRUE),
      
      # Sex Selection
      checkboxGroupInput("sexes",
                         "Sex",
                         choices = get_unique_values(population_data, "sex"),
                         selected = get_unique_values(population_data, "sex")),
      
      # Year Range Selection
      sliderInput("years",
                  "Year Range",
                  min = min(population_data$year),
                  max = max(population_data$year),
                  value = c(2020, 2030),
                  step = 1,
                  sep = ""),
      
      # Scenario Selection
      checkboxGroupInput("scenarios",
                         "Scenarios",
                         choices = get_unique_values(population_data, "scenario"),
                         selected = "Medium"),
      
      # Vertical Line Year Selection
      selectInput("vline_year",
                  "Vertical Line Year",
                  choices = c("None" = "", sort(unique(population_data$year))),
                  selected = ""),
      
      # Apply Filter Button
      br(),
      uiOutput("apply_filters_button"),
      
      # Reset Filter Button
      br(),
      actionButton("reset_filters", 
                   "Reset Filters", 
                   class = "btn-secondary",
                   style = "width: 100%; padding: 8px;")
    ),
    
    # Main panel with outputs
    mainPanel(
      width = 9,
      
      # Status indicator
      uiOutput("filter_status"),
      
      # Tabset for different views
      tabsetPanel(
        id = "main_tabs",
        
        # Population Trends Tab
        tabPanel("Population Trends",
                 plotlyOutput("population_trend_plot", height = "500px")
        ),
        
        # Demographics Comparison Tab
        tabPanel("Demographics Comparison",
                 uiOutput("demographics_plot_ui")
        ),
        
        # Data Table Tab
        tabPanel("Data Table",
                 DT::dataTableOutput("data_table")
        ),
        
        # Summary Statistics Tab
        tabPanel("Summary Statistics",
                 DT::dataTableOutput("summary_table")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  cat("=== SERVER STARTED ===\n")
  cat("Session started at:", Sys.time(), "\n")
  
  # Reactive values to store filter state
  filter_state <- reactiveValues(
    applied = FALSE,
    data = NULL,
    current_filters = list(),
    filters_changed = FALSE
  )
  
  # Track filter changes - set filters_changed to TRUE when any input changes
  observe({
    # This reactive will trigger whenever any of these inputs change
    input$geography_level
    input$state_filter
    input$gcc_filter
    input$geography_name
    input$age_groups
    input$sexes
    input$years
    input$scenarios
    input$vline_year
    
    # Only set filters_changed to TRUE if filters have been applied at least once
    # and we're not currently in the process of applying filters
    if (filter_state$applied && !is.null(filter_state$current_filters) && length(filter_state$current_filters) > 0) {
      # Check if any current input differs from the last applied filters
      current_inputs <- list(
        geography_level = input$geography_level,
        state_filter = input$state_filter,
        gcc_filter = input$gcc_filter,
        geography_name = input$geography_name,
        age_groups = input$age_groups,
        sexes = input$sexes,
        years = input$years,
        scenarios = input$scenarios
      )
      
      # Compare with last applied filters
      if (!identical(current_inputs, filter_state$current_filters)) {
        filter_state$filters_changed <- TRUE
      }
    }
  })
  
  # Function to generate dynamic filter message
  generate_filter_message <- function(filters) {
    if (is.null(filters) || length(filters) == 0) {
      return("✅ Filters Applied")
    }
    
    message_parts <- c("✅ Filters Applied:")
    
    # Geography level
    if (!is.null(filters$geography_level) && filters$geography_level != "all") {
      geo_text <- paste0("Geography: ", stringr::str_to_title(filters$geography_level))
      if (!is.null(filters$geography_name) && length(filters$geography_name) > 0) {
        if (length(filters$geography_name) <= 3) {
          geo_text <- paste0(geo_text, " (", paste(filters$geography_name, collapse = ", "), ")")
        } else {
          geo_text <- paste0(geo_text, " (", length(filters$geography_name), " selected)")
        }
      }
      message_parts <- c(message_parts, geo_text)
    }
    
    # Age groups
    if (!is.null(filters$age_groups) && length(filters$age_groups) > 0) {
      if (length(filters$age_groups) == length(get_unique_values(population_data, "age_group"))) {
        age_text <- "Age Groups: All"
      } else if (length(filters$age_groups) <= 3) {
        age_text <- paste0("Age Groups: ", paste(filters$age_groups, collapse = ", "))
      } else {
        age_text <- paste0("Age Groups: ", length(filters$age_groups), " selected")
      }
      message_parts <- c(message_parts, age_text)
    }
    
    # Sex
    if (!is.null(filters$sexes) && length(filters$sexes) > 0) {
      if (length(filters$sexes) == length(get_unique_values(population_data, "sex"))) {
        sex_text <- "Sex: All"
      } else {
        sex_text <- paste0("Sex: ", paste(filters$sexes, collapse = ", "))
      }
      message_parts <- c(message_parts, sex_text)
    }
    
    # Years
    if (!is.null(filters$years) && length(filters$years) == 2) {
      year_text <- paste0("Years: ", filters$years[1], " - ", filters$years[2])
      message_parts <- c(message_parts, year_text)
    }
    
    # Scenarios
    if (!is.null(filters$scenarios) && length(filters$scenarios) > 0) {
      if (length(filters$scenarios) == length(get_unique_values(population_data, "scenario"))) {
        scenario_text <- "Scenarios: All"
      } else {
        scenario_text <- paste0("Scenarios: ", paste(filters$scenarios, collapse = ", "))
      }
      message_parts <- c(message_parts, scenario_text)
    }
    
    return(paste(message_parts, collapse = " | "))
  }
  
  # Reactive data filtering - only updates when Apply Filters is clicked
  filtered_data <- reactive({
    if (!filter_state$applied) {
      return(data.frame())  # Return empty data frame until filters are applied
    }
    return(filter_state$data)
  })
  
  # Apply filters when button is clicked
  observeEvent(input$apply_filters, {
    cat("\n=== APPLYING FILTERS ===\n")
    cat("Filter application started at:", Sys.time(), "\n")
    req(input$geography_level)
    
    geography_level <- input$geography_level
    cat("Geography level:", geography_level, "\n")
    
    geography_name <- if (geography_level == "national") {
      "Australia"  # Always set to Australia for national level
    } else if (is.null(input$geography_name) || length(input$geography_name) == 0) {
      NULL
    } else {
      input$geography_name  # This is already a vector of selected geographies
    }
    
    # Use the full population data for filtering
    filtered_pop_data <- population_data
    
    # Apply state filtering for sub-state levels
    if (!is.null(geography_level) && !is.null(input$state_filter) && 
        length(input$state_filter) > 0 && 
        geography_level %in% c("sa4", "sa3", "gcc")) {
      # Filter by selected states - check if the geography's parent is one of the selected states
      filtered_pop_data <- filtered_pop_data %>%
        filter(parent_geography %in% input$state_filter)
    }
    
    # Apply GCC filtering for SA3/SA4 levels
    if (!is.null(geography_level) && !is.null(input$gcc_filter) && 
        length(input$gcc_filter) > 0 && 
        geography_level %in% c("sa3", "sa4")) {
      # Filter by checking if the geography's parent is one of the selected GCC regions
      # or if the geography itself is in the selected GCC regions
      filtered_pop_data <- filtered_pop_data %>%
        filter(parent_geography %in% input$gcc_filter | geography_name %in% input$gcc_filter)
    }
    
    # Detailed filter logging
    cat("Filter parameters:\n")
    cat("  Geography level:", geography_level, "\n")
    cat("  State filter:", paste(input$state_filter, collapse = ", "), "\n")
    cat("  GCC filter:", paste(input$gcc_filter, collapse = ", "), "\n")
    cat("  Geography names:", paste(geography_name, collapse = ", "), "\n")
    cat("  Age groups:", paste(input$age_groups, collapse = ", "), "\n")
    cat("  Sexes:", paste(input$sexes, collapse = ", "), "\n")
    cat("  Years:", input$years[1], "to", input$years[2], "\n")
    cat("  Scenarios:", paste(input$scenarios, collapse = ", "), "\n")
    cat("  Initial data rows:", nrow(filtered_pop_data), "\n")
    
    # Apply the filters and store the result
    filter_state$data <- filter_population_data(
      filtered_pop_data,
      geography_level = geography_level,
      geography_name = geography_name,
      age_groups = input$age_groups,
      sexes = input$sexes,
      years = input$years[1]:input$years[2],
      scenarios = input$scenarios
    )
    
    cat("  Final filtered data rows:", nrow(filter_state$data), "\n")
    
    if (nrow(filter_state$data) == 0) {
      cat("WARNING: No data returned after filtering!\n")
    } else {
      cat("✓ Filtering successful\n")
      cat("  Sample data preview:\n")
      print(head(filter_state$data, 3))
    }
    
    # Store current filter selections
    filter_state$current_filters <- list(
      geography_level = geography_level,
      state_filter = input$state_filter,
      gcc_filter = input$gcc_filter,
      geography_name = geography_name,
      age_groups = input$age_groups,
      sexes = input$sexes,
      years = input$years,
      scenarios = input$scenarios
    )
    
    filter_state$applied <- TRUE
    filter_state$filters_changed <- FALSE  # Reset the changed flag when filters are applied
    cat("Filter application completed at:", Sys.time(), "\n")
    cat("=== FILTERS APPLIED ===\n\n")
  })
  
  # Reset filters when button is clicked
  observeEvent(input$reset_filters, {
    filter_state$applied <- FALSE
    filter_state$data <- NULL
    filter_state$current_filters <- list()
    filter_state$filters_changed <- FALSE
    
    # Reset all inputs to default values
    updateSelectInput(session, "geography_level", selected = "state")
    updatePickerInput(session, "age_groups", selected = get_unique_values(population_data, "age_group"))
    updateCheckboxGroupInput(session, "sexes", selected = get_unique_values(population_data, "sex"))
    updateSliderInput(session, "years", value = c(2020, 2030))
    updateCheckboxGroupInput(session, "scenarios", selected = "Medium")
    updateSelectInput(session, "vline_year", selected = "")
  })
  
  # Dynamic apply filters button
  output$apply_filters_button <- renderUI({
    if (!filter_state$applied || filter_state$filters_changed) {
      # Blue button when filters haven't been applied yet OR when filters have changed
      actionButton("apply_filters", 
                   "Apply Filters", 
                   class = "btn-primary",
                   style = "width: 100%; font-weight: bold; padding: 10px; background-color: #2563eb; border-color: #2563eb;")
    } else {
      # Grey button when filters have been applied and haven't changed since
      actionButton("apply_filters", 
                   "Apply Filters", 
                   class = "btn-secondary",
                   style = "width: 100%; font-weight: bold; padding: 10px; background-color: #6b7280; border-color: #6b7280; color: white;")
    }
  })
  
  # Filter status indicator
  output$filter_status <- renderUI({
    if (!filter_state$applied) {
      div(
        class = "alert alert-info",
        style = "margin-bottom: 20px;",
        tags$strong("ℹ️ Filters Not Applied"),
        br(),
        "Please select your filters and click 'Apply Filters' to see the data."
      )
    } else {
      filter_message <- generate_filter_message(filter_state$current_filters)
      div(
        class = "alert alert-success",
        style = "margin-bottom: 20px;",
        tags$strong(filter_message)
      )
    }
  })
  
  # Dynamic state filter (for sub-state levels only)
  output$state_filter_ui <- renderUI({
    cat("\n=== RENDERING STATE FILTER UI ===\n")
    cat("Geography level:", input$geography_level, "\n")
    
    if (input$geography_level %in% c("sa4", "sa3", "gcc")) {
      # Get actual state names (geographies with level "state")
      state_names <- population_data %>%
        filter(geography_level == "state") %>%
        pull(geography_name) %>%
        unique() %>%
        sort()
      
      cat("State filter UI: Showing", length(state_names), "states\n")
      cat("States:", paste(state_names, collapse = ", "), "\n")
      
      div(
        tags$label("Filter by State"),
        pickerInput("state_filter",
                    NULL,
                    choices = state_names,
                    selected = state_names,
                    options = list(`actions-box` = TRUE,
                                  `live-search` = TRUE,
                                  `selected-text-format` = "count > 2"),
                    multiple = TRUE)
      )
    } else {
      return(NULL)
    }
  })
  
  # Dynamic GCC filter (for SA3/SA4 levels only)
  output$gcc_filter_ui <- renderUI({
    if (input$geography_level %in% c("sa3", "sa4")) {
      # Get actual GCC names (geographies with level "gcc")
      gcc_names <- population_data %>%
        filter(geography_level == "gcc") %>%
        pull(geography_name) %>%
        unique() %>%
        sort()
      
      div(
        tags$label("Filter by GCC"),
        pickerInput("gcc_filter",
                    NULL,
                    choices = gcc_names,
                    selected = gcc_names,
                    options = list(`actions-box` = TRUE,
                                  `live-search` = TRUE,
                                  `selected-text-format` = "count > 2"),
                    multiple = TRUE)
      )
    } else {
      return(NULL)
    }
  })
  
  # Dynamic geography name selection
  output$geography_name_ui <- renderUI({
    # For national level, don't show geography name selection
    if (input$geography_level == "national") {
      return(NULL)
    }
    
    # Get available geographies for the selected level
    if (input$geography_level %in% c("sa4", "sa3", "gcc") && 
        !is.null(input$state_filter) && length(input$state_filter) > 0) {
      # For sub-state levels, filter by selected states
      available_geographies <- c()
      for (state in input$state_filter) {
        children <- get_child_geographies(population_data, input$geography_level, state)
        available_geographies <- c(available_geographies, children)
      }
      available_geographies <- unique(available_geographies)
    } else if (input$geography_level %in% c("sa3", "sa4") && 
               !is.null(input$gcc_filter) && length(input$gcc_filter) > 0) {
      # For SA3/SA4 levels, filter by selected GCC regions
      # Get all SA3/SA4 that have the selected GCC regions as their parent
      available_geographies <- population_data %>%
        filter(geography_level == input$geography_level,
               parent_geography %in% input$gcc_filter) %>%
        pull(geography_name) %>%
        unique() %>%
        sort()
    } else {
      # For other levels, get all geographies for the selected level
      available_geographies <- get_child_geographies(population_data, input$geography_level)
    }
    
    if (length(available_geographies) == 0) {
      return(NULL)
    }
    
    div(
      tags$label("Geography"),
      pickerInput("geography_name",
                  NULL,
                  choices = available_geographies,
                  selected = available_geographies,
                  options = list(`actions-box` = TRUE,
                                `live-search` = TRUE,
                                `selected-text-format` = "count > 3"),
                  multiple = TRUE)
    )
  })
  
  # Population trend plot
  output$population_trend_plot <- renderPlotly({
    cat("\n=== GENERATING POPULATION TREND PLOT ===\n")
    data <- filtered_data()
    
    cat("Plot data rows:", nrow(data), "\n")
    if (nrow(data) > 0) {
      cat("Data summary:\n")
      cat("  Unique geographies:", length(unique(data$geography_name)), "\n")
      cat("  Unique scenarios:", paste(unique(data$scenario), collapse = ", "), "\n")
      cat("  Year range:", min(data$year), "to", max(data$year), "\n")
      cat("  Total population range:", min(data$population), "to", max(data$population), "\n")
    }
    
    if (nrow(data) == 0) {
      if (!filter_state$applied) {
        return(plotly_empty() %>% 
               layout(title = "Please apply filters to see the population trends"))
      } else {
        return(plotly_empty() %>% 
               layout(title = "No data available for the selected filters"))
      }
    }
    
    # Aggregate data for trend plot using collapse
    trend_data <- data %>%
      fgroup_by(year, scenario, geography_name) %>%
      fsummarise(total_population = fsum(population))
    
    # Modern color palette
    colors <- c("Low" = "#ef4444", "Medium" = "#2563eb", "High" = "#10b981")
    
    p <- ggplot(trend_data, aes(x = year, y = total_population, 
                                color = scenario, linetype = geography_name,
                                group = interaction(scenario, geography_name))) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 3, alpha = 0.9) +
      {if (!is.null(input$vline_year) && input$vline_year != "" && !is.na(as.numeric(input$vline_year))) {
        geom_vline(xintercept = as.numeric(input$vline_year), linetype = "dashed", color = "red", alpha = 0.7, size = 1)
      }} +
      scale_color_manual(values = colors) +
      scale_linetype_discrete(name = "Geography") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "📈 Population Projections Over Time",
           x = "Year",
           y = "Total Population",
           color = "Scenario",
           linetype = "Geography") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#1f2937"),
        axis.title = element_text(size = 14, color = "#374151"),
        axis.text = element_text(size = 12, color = "#6b7280"),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
        # legend.box = "horizontal",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.key.width = unit(2, "cm"),
        panel.grid.major = element_line(color = "#e5e7eb", size = 0.5),
        panel.grid.minor = element_line(color = "#f3f4f6", size = 0.3),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    plotly_obj <- ggplotly(p, tooltip = c("x", "y", "colour", "linetype")) %>%
      layout(
        font = list(family = "Inter, sans-serif"),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
    
    cat("✓ Population trend plot generated successfully\n")
    cat("=== PLOT COMPLETE ===\n\n")
    
    return(plotly_obj)
  })
  
  # Dynamic UI for demographics plot
  output$demographics_plot_ui <- renderUI({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plotOutput("demographics_comparison_plot", height = "400px"))
    }
    
    # Calculate number of unique combinations for facets using collapse
    unique_combinations <- data %>%
      fselect(age_group, geography_name) %>%
      unique() %>%
      nrow()
    
    # Calculate dynamic height (base height + height per facet)
    base_height <- 250  # Base height for title, legend, etc.
    height_per_facet <- 40  # Height per facet
    dynamic_height <- base_height + (unique_combinations * height_per_facet)
    
    plotOutput("demographics_comparison_plot", height = paste0(dynamic_height, "px"))
  })
  
  # Demographics comparison plot
  output$demographics_comparison_plot <- renderPlot({
    cat("\n=== GENERATING DEMOGRAPHICS PLOT ===\n")
    data <- filtered_data()
    
    cat("Demographics plot data rows:", nrow(data), "\n")
    if (nrow(data) > 0) {
      cat("Demographics data summary:\n")
      cat("  Unique age groups:", length(unique(data$age_group)), "\n")
      cat("  Unique geographies:", length(unique(data$geography_name)), "\n")
      cat("  Facets to generate:", length(unique(data$age_group)) * length(unique(data$geography_name)), "\n")
    }
    
    if (nrow(data) == 0) {
      if (!filter_state$applied) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Please apply filters to see demographics", size = 10) +
               theme_void())
      } else {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected filters", size = 10) +
               theme_void())
      }
    }
    
    # Aggregate data for demographics comparison using collapse
    demo_data <- data %>%
      fgroup_by(age_group, geography_name, scenario, year) %>%
      fsummarise(total_population = fsum(population))
    
    # Modern color palette for scenarios
    colors <- c("Low" = "#ef4444", "Medium" = "#2563eb", "High" = "#10b981")
    
    # Create faceted plot
    p <- ggplot(demo_data, aes(x = year, y = total_population, color = scenario)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 2, alpha = 0.9) +
      facet_wrap(~ geography_name + age_group, scales = "free") +
      {if (!is.null(input$vline_year) && input$vline_year != "" && !is.na(as.numeric(input$vline_year))) {
        geom_vline(xintercept = as.numeric(input$vline_year), linetype = "dashed", color = "red", alpha = 0.7, size = 1)
      }} +
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "📊 Demographics Comparison by Age Group and Geography",
           x = "Year",
           y = "Population",
           color = "Scenario") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", color = "#1f2937"),
        axis.title = element_text(size = 16, color = "#374151"),
        axis.text = element_text(size = 14, color = "#6b7280"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold", color = "#374151"),
        strip.background = element_rect(fill = "#f3f4f6", color = "#e5e7eb"),
        panel.grid.major = element_line(color = "#e5e7eb", size = 0.5),
        panel.grid.minor = element_line(color = "#f3f4f6", size = 0.3),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    print(p)
    cat("✓ Demographics plot generated successfully\n")
    cat("=== DEMOGRAPHICS PLOT COMPLETE ===\n\n")
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    cat("\n=== GENERATING DATA TABLE ===\n")
    data <- filtered_data()
    
    cat("Data table rows:", nrow(data), "\n")
    if (nrow(data) > 0) {
      cat("Data table columns:", paste(names(data), collapse = ", "), "\n")
    }
    
    # DEBUG: Add comprehensive data validation
    cat("DEBUG: Data class:", class(data), "\n")
    cat("DEBUG: Data dimensions:", dim(data), "\n")
    cat("DEBUG: Data is data.frame:", is.data.frame(data), "\n")
    cat("DEBUG: Data is matrix:", is.matrix(data), "\n")
    cat("DEBUG: Data structure:\n")
    str(data)
    
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "Please apply filters to see the data")))
    }
    
    # DEBUG: Ensure data is a proper data frame
    if (!is.data.frame(data)) {
      cat("ERROR: Data is not a data frame! Converting...\n")
      data <- as.data.frame(data)
      cat("DEBUG: After conversion - Data class:", class(data), "\n")
      cat("DEBUG: After conversion - Data dimensions:", dim(data), "\n")
    }
    
    dt_table <- DT::datatable(
      data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        language = list(
          search = "Search:",
          lengthMenu = "Show _MENU_ entries",
          info = "Showing _START_ to _END_ of _TOTAL_ entries",
          paginate = list(
            first = "First",
            last = "Last",
            `next` = "Next",
            previous = "Previous"
          )
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().container()).css({'font-family': 'Inter, sans-serif'});",
          "}"
        )
      ),
      extensions = 'Buttons',
      filter = 'top',
      class = 'display compact hover',
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        columns = names(data),
        fontSize = '13px',
        fontFamily = 'Inter, sans-serif'
      ) %>%
      DT::formatRound(
        columns = 'population',
        digits = 0
      )
    
    cat("✓ Data table generated successfully\n")
    cat("=== DATA TABLE COMPLETE ===\n\n")
    
    return(dt_table)
  })
  
  # Summary statistics table
  output$summary_table <- DT::renderDataTable({
    cat("\n=== GENERATING SUMMARY TABLE ===\n")
    data <- filtered_data()
    
    cat("Summary table data rows:", nrow(data), "\n")
    if (nrow(data) > 0) {
      cat("Calculating summary statistics...\n")
    }
    
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "Please apply filters to see summary statistics")))
    }
    
    summary_data <- calculate_summary_stats(data)
    
    dt_summary <- DT::datatable(
      summary_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        language = list(
          search = "Search:",
          lengthMenu = "Show _MENU_ entries",
          info = "Showing _START_ to _END_ of _TOTAL_ entries"
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().container()).css({'font-family': 'Inter, sans-serif'});",
          "}"
        )
      ),
      class = 'display compact hover',
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        columns = names(summary_data),
        fontSize = '13px',
        fontFamily = 'Inter, sans-serif'
      ) %>%
      DT::formatRound(
        columns = c('total_population', 'male_population', 'female_population'),
        digits = 0
      )
    
    cat("✓ Summary table generated successfully\n")
    cat("Summary statistics rows:", nrow(summary_data), "\n")
    cat("=== SUMMARY TABLE COMPLETE ===\n\n")
    
    return(dt_summary)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

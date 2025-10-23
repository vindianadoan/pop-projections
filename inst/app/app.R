# Population Projections Dashboard
# Main Shiny application

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(collapse)

# Load the package functions
library(popprojections)

# Load pre-generated data efficiently
cat("Loading population data...\n")
population_data <- load_population_data()
cat("Data loaded successfully!\n")

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
                  choices = c("All" = "all", 
                             "National" = "national",
                             "State" = "state", 
                             "SA4" = "sa4",
                             "SA3" = "sa3",
                             "GCC" = "gcc"),
                  selected = "state"),
      
      # Parent Geography Selection (dynamic)
      uiOutput("parent_geography_ui"),
      
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
    input$parent_geography
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
        parent_geography = input$parent_geography,
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
    req(input$geography_level)
    
    geography_level <- if (input$geography_level == "all") {
      NULL
    } else {
      input$geography_level
    }
    
    geography_name <- if (geography_level == "national") {
      "Australia"  # Always set to Australia for national level
    } else if (is.null(input$geography_name) || length(input$geography_name) == 0) {
      NULL
    } else {
      input$geography_name  # This is already a vector of selected geographies
    }
    
    # Additional filtering by state if specified
    filtered_pop_data <- population_data
    
    # Only apply parent geography filtering for sub-state levels (not for national)
    if (!is.null(geography_level) && !is.null(input$parent_geography) && 
        length(input$parent_geography) > 0 && 
        geography_level != "national") {
      # Filter by selected states - check if the geography's parent is one of the selected states
      filtered_pop_data <- filtered_pop_data %>%
        filter(parent_geography %in% input$parent_geography)
    }
    
    # Debug output
    cat("Debug apply_filters - geography_level:", geography_level, "\n")
    cat("Debug apply_filters - geography_name:", paste(geography_name, collapse = ", "), "\n")
    cat("Debug apply_filters - parent_geography:", paste(input$parent_geography, collapse = ", "), "\n")
    cat("Debug apply_filters - age_groups:", paste(input$age_groups, collapse = ", "), "\n")
    cat("Debug apply_filters - sexes:", paste(input$sexes, collapse = ", "), "\n")
    cat("Debug apply_filters - years:", input$years[1], "to", input$years[2], "\n")
    cat("Debug apply_filters - scenarios:", paste(input$scenarios, collapse = ", "), "\n")
    cat("Debug apply_filters - filtered_pop_data rows:", nrow(filtered_pop_data), "\n")
    
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
    
    cat("Debug apply_filters - filtered data rows:", nrow(filter_state$data), "\n")
    
    # Store current filter selections
    filter_state$current_filters <- list(
      geography_level = geography_level,
      parent_geography = input$parent_geography,
      geography_name = geography_name,
      age_groups = input$age_groups,
      sexes = input$sexes,
      years = input$years,
      scenarios = input$scenarios
    )
    
    filter_state$applied <- TRUE
    filter_state$filters_changed <- FALSE  # Reset the changed flag when filters are applied
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
  
  # Dynamic parent geography selection - always show states
  output$parent_geography_ui <- renderUI({
    if (input$geography_level == "all" || input$geography_level == "national") {
      return(NULL)
    }
    
    # Get actual state names (geographies with level "state")
    state_names <- population_data %>%
      filter(geography_level == "state") %>%
      pull(geography_name) %>%
      unique() %>%
      sort()
    
    div(
      tags$label("Filter by State"),
      pickerInput("parent_geography",
                  NULL,
                  choices = state_names,
                  selected = state_names,
                  options = list(`actions-box` = TRUE,
                                `live-search` = TRUE,
                                `selected-text-format` = "count > 2"),
                  multiple = TRUE)
    )
  })
  
  # Dynamic geography name selection
  output$geography_name_ui <- renderUI({
    if (input$geography_level == "all") {
      return(NULL)
    }
    
    # For national level, don't show geography name selection
    if (input$geography_level == "national") {
      return(NULL)
    }
    
    # Get available geographies based on parent selection
    if (is.null(input$parent_geography) || length(input$parent_geography) == 0) {
      available_geographies <- get_child_geographies(population_data, input$geography_level)
    } else {
      available_geographies <- c()
      for (parent in input$parent_geography) {
        children <- get_child_geographies(population_data, input$geography_level, parent)
        available_geographies <- c(available_geographies, children)
      }
      available_geographies <- unique(available_geographies)
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
    data <- filtered_data()
    
    # Debug output
    cat("Debug - filtered_data rows:", nrow(data), "\n")
    if (nrow(data) > 0) {
      cat("Debug - sample data:\n")
      print(head(data, 2))
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
    
    ggplotly(p, tooltip = c("x", "y", "colour", "linetype")) %>%
      layout(
        font = list(family = "Inter, sans-serif"),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
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
    data <- filtered_data()
    
    # Debug output
    cat("Debug demographics - filtered_data rows:", nrow(data), "\n")
    
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
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "Please apply filters to see the data")))
    }
    
    DT::datatable(
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
  })
  
  # Summary statistics table
  output$summary_table <- DT::renderDataTable({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "Please apply filters to see summary statistics")))
    }
    
    summary_data <- calculate_summary_stats(data)
    
    DT::datatable(
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
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Population Projections Dashboard
# Main Shiny application

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinyWidgets)
library(shinyjs)

# Load the package functions
library(popprojections)

# Load data
population_data <- generate_population_data()

# UI
ui <- fluidPage(
  useShinyjs(),
  
  # Include custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # Application title
  titlePanel("Population Projections Dashboard"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Geography Level Selection
      selectInput("geography_level", 
                  "Geography Level:",
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
                  "Age Groups:",
                  choices = get_unique_values(population_data, "age_group"),
                  selected = get_unique_values(population_data, "age_group"),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),
      
      # Sex Selection
      checkboxGroupInput("sexes",
                         "Sex:",
                         choices = get_unique_values(population_data, "sex"),
                         selected = get_unique_values(population_data, "sex")),
      
      # Year Range Selection
      sliderInput("years",
                  "Year Range:",
                  min = min(population_data$year),
                  max = max(population_data$year),
                  value = c(2020, 2030),
                  step = 1,
                  sep = ""),
      
      # Scenario Selection
      checkboxGroupInput("scenarios",
                         "Scenarios:",
                         choices = get_unique_values(population_data, "scenario"),
                         selected = "Medium"),
      
      # Action Buttons
      br(),
      actionButton("update_plot", "Update Visualizations", 
                   class = "btn-primary"),
      br(), br(),
      actionButton("export_data", "Export Data", 
                   class = "btn-success")
    ),
    
    # Main panel with outputs
    mainPanel(
      width = 9,
      
      # Tabset for different views
      tabsetPanel(
        id = "main_tabs",
        
        # Population Trends Tab
        tabPanel("Population Trends",
                 plotlyOutput("population_trend_plot", height = "500px"),
                 br(),
                 plotlyOutput("age_pyramid_plot", height = "400px")
        ),
        
        # Geographic Comparison Tab
        tabPanel("Geographic Comparison",
                 plotlyOutput("geographic_comparison_plot", height = "500px"),
                 br(),
                 plotlyOutput("geographic_bar_plot", height = "400px")
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
  
  # Reactive data filtering
  filtered_data <- reactive({
    req(input$geography_level)
    
    geography_level <- if (input$geography_level == "all") {
      NULL
    } else {
      input$geography_level
    }
    
    geography_name <- if (is.null(input$geography_name)) {
      NULL
    } else {
      input$geography_name
    }
    
    # Additional filtering by parent geography if specified
    filtered_pop_data <- population_data
    
    if (!is.null(geography_level) && !is.null(input$parent_geography) && 
        !"all" %in% input$parent_geography && geography_level != "national") {
      filtered_pop_data <- filtered_pop_data %>%
        filter(parent_geography %in% input$parent_geography)
    }
    
    filter_population_data(
      filtered_pop_data,
      geography_level = geography_level,
      geography_name = geography_name,
      age_groups = input$age_groups,
      sexes = input$sexes,
      years = input$years[1]:input$years[2],
      scenarios = input$scenarios
    )
  })
  
  # Dynamic parent geography selection
  output$parent_geography_ui <- renderUI({
    if (input$geography_level == "all" || input$geography_level == "national") {
      return(NULL)
    }
    
    parent_geographies <- get_parent_geographies(population_data, input$geography_level)
    
    pickerInput("parent_geography",
                "Filter by Parent Geography:",
                choices = c("All" = "all", parent_geographies),
                selected = "all",
                options = list(`actions-box` = TRUE),
                multiple = TRUE)
  })
  
  # Dynamic geography name selection
  output$geography_name_ui <- renderUI({
    if (input$geography_level == "all") {
      return(NULL)
    }
    
    # Get available geographies based on parent selection
    if (is.null(input$parent_geography) || "all" %in% input$parent_geography) {
      available_geographies <- get_child_geographies(population_data, input$geography_level)
    } else {
      available_geographies <- c()
      for (parent in input$parent_geography) {
        if (parent != "all") {
          children <- get_child_geographies(population_data, input$geography_level, parent)
          available_geographies <- c(available_geographies, children)
        }
      }
      available_geographies <- unique(available_geographies)
    }
    
    if (length(available_geographies) == 0) {
      return(NULL)
    }
    
    pickerInput("geography_name",
                "Geography:",
                choices = available_geographies,
                selected = available_geographies[1],
                options = list(`actions-box` = TRUE),
                multiple = TRUE)
  })
  
  # Population trend plot
  output$population_trend_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty())
    }
    
    # Aggregate data for trend plot
    trend_data <- data %>%
      group_by(year, scenario, geography_name) %>%
      summarise(total_population = sum(population), .groups = "drop")
    
    p <- ggplot(trend_data, aes(x = year, y = total_population, 
                                color = scenario, group = interaction(scenario, geography_name))) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Population Projections Over Time",
           x = "Year",
           y = "Total Population",
           color = "Scenario") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Age pyramid plot
  output$age_pyramid_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty())
    }
    
    # Create age pyramid data
    pyramid_data <- data %>%
      group_by(age_group, sex, scenario) %>%
      summarise(population = sum(population), .groups = "drop") %>%
      mutate(population = ifelse(sex == "Male", -population, population))
    
    p <- ggplot(pyramid_data, aes(x = age_group, y = population, fill = sex)) +
      geom_bar(stat = "identity", position = "identity") +
      coord_flip() +
      labs(title = "Age-Sex Population Distribution",
           x = "Age Group",
           y = "Population",
           fill = "Sex") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Geographic comparison plot
  output$geographic_comparison_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty())
    }
    
    # Aggregate by geography
    geo_data <- data %>%
      group_by(geography_name, scenario) %>%
      summarise(total_population = sum(population), .groups = "drop")
    
    p <- ggplot(geo_data, aes(x = reorder(geography_name, total_population), 
                              y = total_population, fill = scenario)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(title = "Population by Geography",
           x = "Geography",
           y = "Total Population",
           fill = "Scenario") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Geographic bar plot (alternative view)
  output$geographic_bar_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty())
    }
    
    # Aggregate by geography and age group
    geo_age_data <- data %>%
      group_by(geography_name, age_group) %>%
      summarise(total_population = sum(population), .groups = "drop")
    
    p <- ggplot(geo_age_data, aes(x = age_group, y = total_population, 
                                  fill = geography_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Population by Age Group and Geography",
           x = "Age Group",
           y = "Total Population",
           fill = "Geography") +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    data <- filtered_data()
    
    DT::datatable(
      data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      extensions = 'Buttons',
      filter = 'top'
    )
  })
  
  # Summary statistics table
  output$summary_table <- DT::renderDataTable({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame()))
    }
    
    summary_data <- calculate_summary_stats(data)
    
    DT::datatable(
      summary_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  
  # Export data functionality
  observeEvent(input$export_data, {
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      # Create filename with timestamp
      filename <- paste0("population_projections_", Sys.Date(), ".csv")
      
      # Download handler
      output$download_data <- downloadHandler(
        filename = filename,
        content = function(file) {
          write.csv(data, file, row.names = FALSE)
        }
      )
      
      # Trigger download
      shinyjs::runjs("$('#download_data')[0].click();")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

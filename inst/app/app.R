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
                  selected = "")
    ),
    
    # Main panel with outputs
    mainPanel(
      width = 9,
      
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
    
    div(
      tags$label("Filter by Parent Geography"),
      pickerInput("parent_geography",
                  NULL,
                  choices = c("All" = "all", parent_geographies),
                  selected = "all",
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
    
    div(
      tags$label("Geography"),
      pickerInput("geography_name",
                  NULL,
                  choices = available_geographies,
                  selected = available_geographies[1],
                  options = list(`actions-box` = TRUE,
                                `live-search` = TRUE,
                                `selected-text-format` = "count > 3"),
                  multiple = TRUE)
    )
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
        axis.title = element_text(size = 12, color = "#374151"),
        axis.text = element_text(size = 10, color = "#6b7280"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
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
    
    # Calculate number of unique combinations for facets
    unique_combinations <- data %>%
      select(age_group, geography_name) %>%
      distinct() %>%
      nrow()
    
    # Calculate dynamic height (base height + height per facet)
    base_height <- 200  # Base height for title, legend, etc.
    height_per_facet <- 40  # Height per facet
    dynamic_height <- base_height + (unique_combinations * height_per_facet)
    
    plotOutput("demographics_comparison_plot", height = paste0(dynamic_height, "px"))
  })
  
  # Demographics comparison plot
  output$demographics_comparison_plot <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
             theme_void())
    }
    
    # Aggregate data for demographics comparison
    demo_data <- data %>%
      group_by(age_group, geography_name, scenario, year) %>%
      summarise(total_population = sum(population), .groups = "drop")
    
    # Modern color palette for scenarios
    colors <- c("Low" = "#ef4444", "Medium" = "#2563eb", "High" = "#10b981")
    
    # Create faceted plot
    p <- ggplot(demo_data, aes(x = year, y = total_population, color = scenario)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 2, alpha = 0.9) +
      facet_wrap(~ age_group + geography_name, scales = "free") +
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
        plot.title = element_text(size = 16, face = "bold", color = "#1f2937"),
        axis.title = element_text(size = 12, color = "#374151"),
        axis.text = element_text(size = 10, color = "#6b7280"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 10, face = "bold", color = "#374151"),
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
      return(DT::datatable(data.frame()))
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

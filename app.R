source("superClean.R")

# Load necessary libraries
library(shiny)
library(ggplot2)
library(data.table)
library(DT)  # For interactive tables

# Sample data table (replace this with your actual data)
dt <- dt_final

# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Frequency Plot"),
  
  sidebarLayout(
    sidebarPanel(
      # Input for bucket size
      sliderInput("bucket_size", "Bucket Size (years):", min = 1, max = 10, value = 3),
      
      # Input for category (dropdown with excluded options)
      selectInput(
        "category", 
        "Category:", 
        choices = setdiff(colnames(dt)[sapply(dt, is.character)], 
                          c("...1", "month", "year")),  # Exclude specific categories
        selected = "channel_network"
      ),
      
      # Input for top N lines to plot
      sliderInput("top_n", "Top N Lines to Plot:", min = 1, max = 20, value = 8),
      
      # Checkbox to include/exclude NA values
      checkboxInput("include_na", "Include NA Values", value = TRUE),
      
      # View Only Filter (inside sidebarPanel)
      wellPanel(
        h4("View Only Filter"),
        helpText("Use this to focus on specific subsets of the data. Select 'All' to include everything."),
        selectInput(
          "view_only_category",
          "View Only Category:",
          choices = c("All", setdiff(colnames(dt)[sapply(dt, is.character)], 
                                     c("...1", "month", "year"))),
          selected = "All"
        ),
        uiOutput("view_only_filter_ui")
      )
    ),
    
    mainPanel(
      plotOutput("frequency_plot",  height = "400px"),
      div(
        style = "height: calc(100vh - 500px); overflow-y: auto;",  # Adjust height dynamically
        DTOutput("frequency_table")
      )
      
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  top_view_only_values <- reactive({
    req(input$view_only_category != "All")
    col <- input$view_only_category
    dt[, .N, by = col][order(-N)][1:10, get(col)]
  })
  
  # Rename UI render for dynamic filter
  output$view_only_filter_ui <- renderUI({
    if(input$view_only_category != "All") {
      selectizeInput(
        "view_only_filter_values",
        paste("View Only", input$view_only_category),
        choices = top_view_only_values(),
        multiple = TRUE,
        options = list(placeholder = "Select values to include")
      )
    }
  })
  
  # Modify the agg_data reactive (first few lines)
  agg_data <- reactive({
    # Existing parameters
    category <- input$category
    bucket_size <- input$bucket_size
    top_n <- input$top_n
    include_na <- input$include_na
    
    dt_clean <- copy(dt)
    
    # NEW: Apply view-only filter first
    if(input$view_only_category != "All" && !is.null(input$view_only_filter_values)) {
      dt_clean <- dt_clean[get(input$view_only_category) %in% input$view_only_filter_values]
    }
    
    dt_clean[, year := as.numeric(year)]
    
    # Create year buckets
    dt_clean[, year_bucket := paste0((year %/% bucket_size) * bucket_size, "-", 
                                     ((year %/% bucket_size) * bucket_size + bucket_size - 1))]
    
    # Filter out NA values if the checkbox is unchecked
    if (!include_na) {
      dt_clean <- dt_clean[!is.na(get(category))]
    }
    
    # Get top N categories
    dt_top_categories <- dt_clean[, .N, by = category][order(-N)][1:top_n][, get(category)]
    
    # Filter data to include only top N categories
    dt_filtered <- dt_clean[get(category) %in% dt_top_categories]
    
    # Aggregate data by year_bucket and category (for the plot)
    dt_agg <- dt_filtered[, .N, by = .(year_bucket, get(category))]
    setnames(dt_agg, "get", "category")  # Rename the column for clarity
    
    # Aggregate data over all years (for the table)
    dt_agg_all_years <- dt_filtered[, .(Total_Frequency = .N), by = category]
    
    # Sort by Total_Frequency in descending order
    dt_agg_all_years <- dt_agg_all_years[order(-Total_Frequency)]
    
    # Return both datasets
    list(plot_data = dt_agg, table_data = dt_agg_all_years)
  })
  
  # Render the plot
  output$frequency_plot <- renderPlot({
    dt_agg <- agg_data()$plot_data
    
    ggplot(dt_agg, aes(x = year_bucket, y = N, color = category, group = category)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
      labs(x = "Year Bucket", y = "Frequency", color = "Category")
  })
  
  # Render the table
  output$frequency_table <- renderDT({
    dt_agg_all_years <- agg_data()$table_data
    
    # Display the table without row IDs
    datatable(
      dt_agg_all_years,
      options = list(
        pageLength = input$top_n,  # Dynamic page length based on top_n
        scrollX = TRUE,
        dom = "t",  # Only show the table (no search, pagination, etc.)
        ordering = FALSE  # Disable column reordering
      ),
      rownames = FALSE  # Remove row IDs
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

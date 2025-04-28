# source("superClean.R")

# Load necessary libraries
library(shiny)
library(ggplot2)
library(data.table)
library(DT)

dt <- readRDS("Datasets/dt_final.rds")
dt[, Time := as.character(Time)]

# Define the UI
ui <- fluidPage(
  titlePanel("Marion Stokes Tapes Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Input for bucket size
      sliderInput(
        "bucket_size",
        "Bucket Size (years):",
        min = 1,
        max = 10,
        value = 3
      ),
      
      # Input for category (dropdown with excluded options)
      selectInput(
        "category",
        "Category:",
        choices = setdiff(colnames(dt), c("...1", "month", "year", "Date")),
        # Exclude specific categories
        selected = "Network"
      ),
      
      # Input for top N lines to plot
      sliderInput(
        "top_n",
        "Top N Lines to Plot:",
        min = 1,
        max = 20,
        value = 8
      ),
      
      # Checkbox to include/exclude NA values
      checkboxInput("include_na", "Include NA Values", value = TRUE),
      
      # View Only Filter (inside sidebarPanel)
      h4("View Only Filter"),
      helpText(
        "Use this to focus on specific subsets of the data. Select 'All' to include everything."
      ),
      selectInput(
        "view_only_category",
        "View Only Category:",
        choices = c("All", setdiff(colnames(dt)[sapply(dt, is.character)], c(
          "...1", "month", "year"
        ))),
        selected = "All"
      ),
      uiOutput("view_only_filter_ui"),
      
      # NEW: Description filter input
      textInput("desc_filter", "Search by Description:", placeholder = "Type text to filter descriptions"),
      
      div(style = "font-size: 0.85em; color: #666;", p(
        HTML("<strong>Last updated:</strong> April 2025")
      ), p(
        HTML("<strong>Author:</strong> Jules Walzer-Goldfeld")
      ), p(
        HTML(
          "<strong>See the code:</strong> <a href = https://github.com/juleswg23/Marion-stokes-tapes-dashboard-shiny-app>Github Link</a>"
        )
      ), p(
        downloadButton("download_dataset", "Download the dataset", class = "btn-sm")
      ), ),
      
      
    ),
    
    
    mainPanel(
      plotOutput("frequency_plot", height = "400px"),
      
      fluidRow(column(
        width = 9,
        div(
          style = "height: calc(100vh - 500px); overflow-y: auto;",
          h4(textOutput("description_table_header")),
          DTOutput("description_table")
        )
      ),
      column(
        width = 3,
        div(style = "height: calc(100vh - 500px); overflow-y: auto;", h4("Frequency"), DTOutput("frequency_table"))
      ))
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
    if (input$view_only_category != "All") {
      selectizeInput(
        "view_only_filter_values",
        paste("View Only", input$view_only_category),
        choices = top_view_only_values(),
        multiple = TRUE,
        options = list(placeholder = "Select values to include")
      )
    }
  })
  
  # Modify the agg_data reactive
  agg_data <- reactive({
    # Existing parameters
    category <- input$category
    bucket_size <- input$bucket_size
    top_n <- input$top_n
    include_na <- input$include_na
    
    dt_clean <- copy(dt)
    
    if (input$view_only_category != "All" &&
        !is.null(input$view_only_filter_values)) {
      dt_clean <- dt_clean[get(input$view_only_category) %in% input$view_only_filter_values]
    }
    
    dt_clean[, year := as.numeric(year)]
    
    # Create year buckets
    dt_clean[, year_bucket := paste0((year %/% bucket_size) * bucket_size,
                                     "-",
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
    
    ggplot(dt_agg,
           aes(
             x = year_bucket,
             y = N,
             color = category,
             group = category
           )) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 70, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),
        
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 11),
      ) +
      labs(x = "Years", y = "Frequency", color = "Category")
  })
  
  # Render the table
  output$frequency_table <- renderDT({
    dt_agg_all_years <- agg_data()$table_data
    
    # Display the table without row IDs
    datatable(
      dt_agg_all_years,
      options = list(
        pageLength = input$top_n,
        # Dynamic page length based on top_n
        scrollX = TRUE,
        dom = "t",
        # Only show the table (no search, pagination, etc.)
        ordering = FALSE  # Disable column reordering
      ),
      rownames = FALSE,
      # Remove row IDs
      colnames = c(colnames(dt_agg_all_years)[1], "Total")
    )
  })
  
  # NEW: Reactive expression for description filtering
  description_filtered_rows <- reactive({
    dt_clean <- copy(dt)
    # Only filter if text is provided; otherwise, return all rows
    if (nzchar(input$desc_filter)) {
      # Assuming your description column is named "description"
      dt_clean <- dt_clean[grepl(input$desc_filter, Description, ignore.case = TRUE)]
    } else {
      return (data.table())
    }
    dt_clean
  })
  
  # NEW: Render the description filtered table
  output$description_table <- renderDT({
    dt_desc <- description_filtered_rows()
    if (nrow(dt_desc) == 0) {
      empty_dt <- data.table(Date = character(), Time = character(), Description = character(), Network = character())
      return(
        datatable(
          empty_dt,
          options = list(scrollX = TRUE, dom = "t"),
          rownames = FALSE
        )
      )
    }
    
    
    # Subset only the desired columns: date, Start Time, Description, Network
    dt_desc_subset <-dt_desc[, .(Date, Time, Description, Network)]
    datatable(
      dt_desc_subset,
      options = list(scrollX = TRUE,
                     dom = "t",
                     pageLength = 200 
      ),
      rownames = FALSE,
    )
  })
  
  description_row_count <- reactive({
    if (nzchar(input$desc_filter)) {
      sum(grepl(input$desc_filter, dt$Description, ignore.case = TRUE))
    } else {
      0
    }
  })
  
  output$description_table_header <- renderText({
    row_count <- description_row_count()
    if (row_count == 0) {
      return ("Type in Search by Description to Get Started")
    }
    paste0("Rows Matching Description (", description_row_count(), ")")
  })
  
  output$download_dataset <- downloadHandler(
    filename = function() {
      paste0("dataset_with_dates_only_", Sys.Date(), ".csv")
    },
    content = function(file) {
      file.copy("Datasets/dataset_with_dates_only.csv", file)
    },
    contentType = "text/csv"
  )
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)





### 3/31 Ideas

## Maybe filter by word in data?

## Right now the filter will lose entries that had bad dates

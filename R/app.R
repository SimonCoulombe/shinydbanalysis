#' Run the Shiny Database Analysis Application
#'
#' @param pool A database connection pool object
#' @param column_info_dir Directory containing column information files
#' @export
run_app <- function(pool, column_info_dir = tempdir()) {
  ui <- fluidPage(
    titlePanel("Dataset Analysis Tool"),
    
    sidebarLayout(
      sidebarPanel(
        table_picker_ui("table", column_info_dir),
        hr(),
        filter_builder_ui("filters"),
        hr(),
        group_builder_ui("groups"),
        hr(),
        summary_builder_ui("summaries"),
        hr(),
        # Revert to original hover implementation
        data_fetcher_ui("fetcher", style = "hover")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Data",
                   # Show executed query and any errors above results
                   div(
                     style = "margin: 15px 0;",
                     h4("Executed Query:", class = "text-muted"),
                     verbatimTextOutput("executed_query"),
                     # Add error display
                     uiOutput("error_display"),
                     hr()
                   ),
                   tableOutput("results")
          ),
          tabPanel("Debug Information",
                   div(class = "debug-panel",
                       # table_picker_server returns selected_table(), column_info()
                       div(class = "debug-section",
                           h4("table_picker_server returns:"),
                           tags$pre(
                             "selected_table():",
                             textOutput("table_selected_table", inline = TRUE),
                             "\n\ncolumn_info():",
                             verbatimTextOutput("table_column_info")
                           )
                       ),
                       
                       # filter_builder_server returns where_clause()
                       div(class = "debug-section",
                           h4("filter_builder_server returns:"),
                           tags$pre(
                             "where_clause():",
                             textOutput("filter_where_clause", inline = TRUE)
                           )
                       ),
                       
                       # group_builder_server returns group_vars()
                       div(class = "debug-section",
                           h4("group_builder_server returns:"),
                           tags$pre(
                             "group_vars():",
                             textOutput("group_vars", inline = TRUE)
                           )
                       ),
                       
                       # summary_builder_server returns summary_specs()
                       div(class = "debug-section",
                           h4("summary_builder_server returns:"),
                           tags$pre(
                             "summary_specs():",
                             verbatimTextOutput("summary_specs")
                           )
                       ),
                       
                       # data_fetcher_server returns data(), error(), executed_query()
                       div(class = "debug-section",
                           h4("data_fetcher_server returns:"),
                           tags$pre(
                             "error():",
                             textOutput("fetcher_error", inline = TRUE),
                             "\n\nexecuted_query():",
                             verbatimTextOutput("fetcher_executed_query"),
                             "\n\ndata():",
                             verbatimTextOutput("fetcher_data_str")
                           )
                       )
                   )
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    # Clean up pool when app stops
    onStop(function() {
      if (DBI::dbIsValid(pool)) {
        poolClose(pool)
      }
    })
    
    # Initialize modules
    table_info <- table_picker_server("table", pool, column_info_dir)
    
    filter_results <- filter_builder_server(
      "filters",
      selected_table = table_info$selected_table,
      column_info = table_info$column_info
    )
    
    group_results <- group_builder_server(
      "groups",
      selected_table = table_info$selected_table,
      column_info = table_info$column_info
    )
    
    summary_results <- summary_builder_server(
      "summaries",
      selected_table = table_info$selected_table,
      column_info = table_info$column_info
    )
    
    fetched_data <- data_fetcher_server(
      "fetcher",
      pool = pool,
      table_info = table_info,
      filter_builder = filter_results,
      group_builder = group_results,
      summary_builder = summary_results
    )
    
    # Error display
    output$error_display <- renderUI({
      error <- fetched_data$error()
      if (!is.null(error)) {
        div(
          class = "alert alert-danger",
          error
        )
      }
    })
    
    # Show executed query above results
    output$executed_query <- renderPrint({
      req(fetched_data$executed_query())
      cat(fetched_data$executed_query())
    })
    
    # Debug outputs with exact names from server modules
    
    # table_picker_server debug
    output$table_selected_table <- renderText({
      table_info$selected_table() %||% "NULL"
    })
    
    output$table_column_info <- renderPrint({
      str(table_info$column_info())
    })
    
    # filter_builder_server debug
    output$filter_where_clause <- renderText({
      filter_results$where_clause() %||% "NULL"
    })
    
    # group_builder_server debug
    output$group_vars <- renderText({
      paste(group_results$group_vars(), collapse = ", ") %||% "NULL"
    })
    
    # summary_builder_server debug
    output$summary_specs <- renderPrint({
      str(summary_results$summary_specs())
    })
    
    # data_fetcher_server debug
    output$fetcher_error <- renderText({
      fetched_data$error() %||% "NULL"
    })
    
    output$fetcher_executed_query <- renderPrint({
      cat(fetched_data$executed_query() %||% "NULL")
    })
    
    output$fetcher_data_str <- renderPrint({
      str(fetched_data$data())
    })
    
    # Results table
    output$results <- renderTable({
      error <- fetched_data$error()
      if (!is.null(error)) {
        return(data.frame(Error = error))
      }
      data <- fetched_data$data()
      if (is.null(data)) {
        return(data.frame(Message = "Click 'Fetch Data' to load data"))
      }
      head(data, 10)
    })
  }
  
  shinyApp(ui = ui, server = server)
}
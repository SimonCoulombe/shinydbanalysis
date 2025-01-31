#' @title Run the Shiny Database Analysis Application
#' 
#' @description Launches a Shiny app for interactive database analysis with filtering, grouping, and summarization capabilities.
#'
#' @param pool A database connection pool object
#' @param column_info_dir Directory containing column information files.
#'            Defaults to temporary directory.
#' @export
#' 
#' @examples
#' \dontrun{
#' # Create a connection pool
#' pool <- dbPool(drv = RSQLite::SQLite(), dbname = ":memory:")
#' 
#' # Write some data
#' dbWriteTable(pool, "iris", iris)
#' create_column_info("iris", pool, output_dir = "column_info")
#' 
#' # Launch the app
#' run_app(pool, "column_info")
#' }
run_app <- function(pool, column_info_dir = tempdir()) {
  # Validate inputs
  if (!inherits(pool, "Pool")) {
    stop("pool must be a valid database connection pool")
  }
  
  if (!dir.exists(column_info_dir)) {
    stop("column_info_dir does not exist: ", column_info_dir)
  }
  
  # Close pool when app stops
  shiny::onStop(function() {
    if (DBI::dbIsValid(pool)) {
      message("Closing database connection pool")
      poolClose(pool)
    }
  })
  
  # UI definition
  ui <- shiny::fluidPage(
    shiny::titlePanel("Dataset Analysis Tool"),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        table_picker_ui("table", column_info_dir),
        shiny::hr(),
        filter_builder_ui("filters"),
        shiny::hr(),
        group_builder_ui("groups"),
        shiny::hr(),
        summary_builder_ui("summaries"),
        shiny::hr(),
        data_fetcher_ui("fetcher", style = "hover")
      ),
      
      shiny::mainPanel(
        shiny::h3("Debug Information:"),
        shiny::verbatimTextOutput("debug_output"),
        shiny::hr(),
        shiny::h3("Fetched Data:"),
        shiny::h3("Query:"),
        shiny::verbatimTextOutput("executed_query"),
        shiny::hr(),
        shiny::tableOutput("results")
      )
    )
  )
  
  server <- function(input, output, session) {
    # Initialize table picker
    table_info <- table_picker_server("table", column_info_dir)
    
    # Initialize other modules with table_info
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
    
    # Initialize data fetcher
    fetched_data <- data_fetcher_server(
      "fetcher",
      pool = pool,
      table_info = table_info,
      filter_builder = filter_results,
      group_builder = group_results,
      summary_builder = summary_results
    )
    
    # Debug output panel
    output$debug_output <- renderPrint({
      cat("=== Debug Information ===\n")
      cat("Selected table:", table_info$selected_table(), "\n")
      cat("WHERE clause:", filter_results$where_clause(), "\n")
      cat("Group vars:", paste(group_results$group_vars(), collapse=", "), "\n")
      cat("Summary specs:\n")
      specs <- summary_results$summary_specs()
      if (length(specs) > 0) {
        for (spec in specs) {
          cat(" -", spec$func, "of", spec$metric, "\n")
        }
      } else {
        cat(" No summary specifications\n")
      }
    })
    
    # Show executed query
    output$executed_query <- renderPrint({
      req(fetched_data$executed_query())
      cat(fetched_data$executed_query())
    })
    
    # Results display
    output$results <- renderTable({
      error <- fetched_data$error()
      if (!is.null(error)) {
        return(data.frame(Error = error))
      }
      data <- fetched_data$data()
      if (is.null(data)) {
        return(data.frame(Message = "No data fetched yet"))
      }
      head(data, 10)
    })
  }
  
  # Run application
  shiny::shinyApp(ui = ui, server = server)
}
#' @title Run the Shiny Database Analysis Application
#' 
#' @description Launches a Shiny app for interactive database analysis with filtering, grouping, and summarization capabilities.
#'
#' @param pool A database connection pool object (optional). If not provided, 
#'            the app will create a new SQLite connection.
#' @param dbname Path to SQLite database file (ignored if pool is provided).
#'            Defaults to an in-memory database.
#' @param column_info_dir Directory to store/generate column information files.
#'            Defaults to temporary directory.
#' @param example_data Whether to load example iris and mtcars data into the database.
#'            Defaults to TRUE.
#' 
#' @importFrom shiny shinyApp
#' @importFrom pool dbPool poolClose
#' @importFrom DBI dbWriteTable dbListTables
#' @export
#' 
#' @examples
#' \dontrun{
#' # Launch with default settings (in-memory database)
#' shinydbanalysis::run_app()
#'
#' # Use persistent database
#' shinydbanalysis::run_app(dbname = "my_data.db")
#' }

run_app <- function(pool = NULL, 
                    dbname = ":memory:",
                    column_info_dir = tempdir(),
                    example_data = TRUE) {
  
  # Create connection pool if not provided
  if (is.null(pool)) {
    pool <- pool::dbPool(
      drv = RSQLite::SQLite(),
      dbname = dbname
    )
    
    # Close pool on app stop
    shiny::onStop(function() {
      if (DBI::dbIsValid(pool)) {
        message("Closing database connection pool")
        poolClose(pool)
      }
    })
  }
  
  # Initialize application
  initialize_app(pool, column_info_dir, example_data)
  
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
        shiny::h3("Fetched Data :"),
        shiny::h3("Query:"),
        shiny::verbatimTextOutput("executed_query"),
        shiny::hr(),
        shiny::tableOutput("results")
      )
    )
  )
  
  server <- function(input, output, session) {
    # Debug observer for all inputs
    observe({
      message("=== Input Debug ===")
      message("All inputs: ", paste(names(input), collapse=", "))
    })
    
    # Initialize table picker
    table_info <- table_picker_server("table", column_info_dir)
    
    # Debug observer for table selection
    observe({
      message("=== Table Info Debug ===")
      message("Selected table: ", table_info$selected_table())
      message("Column info available: ", !is.null(table_info$column_info()))
    })
    
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
    
    # Debug observer for module outputs
    observe({
      message("=== Module Outputs Debug ===")
      message("Filter WHERE clause: ", filter_results$where_clause())
      message("Group variables: ", paste(group_results$group_vars(), collapse=", "))
      message("Summary specs count: ", length(summary_results$summary_specs()))
    })
    
    # Initialize data fetcher with explicit namespacing
    fetched_data <- data_fetcher_server(
      "fetcher",
      pool = pool,
      table_info = table_info,
      filter_builder = filter_results,
      group_builder = group_results,
      summary_builder = summary_results
    )
    
    # Debug observer for fetch button
    observe({
      # Get the fetch button input with correct namespace
      fetch_button <- input[["fetcher-fetch_data"]]
      message("=== Fetch Button Debug ===")
      message("Fetch button value: ", fetch_button)
    })
    
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
      
      fetch_button <- input[["fetcher-fetch_data"]]
      cat("\nFetch button pressed:", !is.null(fetch_button) && fetch_button > 0, "\n")
      
      error <- fetched_data$error()
      if (!is.null(error)) {
        cat("\nError:", error, "\n")
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

#' Initialize application resources
#' @keywords internal
#' @noRd
initialize_app <- function(pool, column_info_dir, example_data) {
  message("Initializing application...")
  
  # Create example tables if requested
  if (example_data) {
    tables <- c("iris", "mtcars")
    for (table in tables) {
      if (!table %in% dbListTables(pool)) {
        message("Creating example table: ", table)
        dbWriteTable(pool, table, getExportedValue("datasets", table))
      }
    }
  }
  
  # Create column info directory if needed
  if (!dir.exists(column_info_dir)) {
    message("Creating column info directory: ", column_info_dir)
    dir.create(column_info_dir, recursive = TRUE)
  }
  
  # Generate column info for all tables
  tables <- dbListTables(pool)
  message("Generating column information for tables: ", paste(tables, collapse = ", "))
  
  for (table in tables) {
    col_info_file <- file.path(column_info_dir, paste0("column_info_", table, ".rds"))
    if (!file.exists(col_info_file)) {
      message("Generating column info for: ", table)
      create_column_info(table, pool, output_dir = column_info_dir)
    }
  }
}

#' Run the Shiny application with debugging
#' @param pool Database connection pool
#' @param column_info_dir Path to directory containing column info files
#' @export
run_app <- function(pool, column_info_dir = "column_info") {

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
        data_fetcher_ui("fetcher", style = "hover")
      ),

      mainPanel(
        h3("Debug Information:"),
        verbatimTextOutput("debug_output"),
        hr(),
        h3("Fetched Data :"),
        h3("Query:"),
        verbatimTextOutput("executed_query"),
        hr(),
        tableOutput("results")
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

  # Run the application
  shinyApp(ui = ui, server = server)
}

#' Run the Shiny application
#' @param pool Database connection pool
#' @export
run_app <- function(pool) {

  ui <- fluidPage(
    titlePanel("Dataset Filter Builder"),
    sidebarLayout(
      sidebarPanel(
        filter_builder_ui("filters", allowed_tables = c("iris", "mtcars")),
        width = 4
      ),
      mainPanel(
        h3("Current Filters:"),
        verbatimTextOutput("filters-filter_summary"),
        br(),
        h3("Filtered Data Preview:"),
        tableOutput("filters-filtered_data"),
        width = 8
      )
    )
  )

  server <- function(input, output, session) {
    # Initialize filter results
    filter_results <- filter_builder_server("filters", pool, c("iris", "mtcars"))

    # Observe filtered data changes
    observe({
      data <- filter_results$filtered_data()
      # Do something with the filtered data
    })

    # Close the pool when the session ends
    session$onSessionEnded(function() {
      if (!is.null(pool) && !pool$pooled_obj$valid) {
        poolClose(pool)
      }
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}

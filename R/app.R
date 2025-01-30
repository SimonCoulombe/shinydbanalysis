#' Run the Shiny application
#' @param pool Database connection pool
#' @export
run_app <- function(pool) {
  ui <- fluidPage(
    filter_builder_ui("filters", c("iris", "mtcars"))
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

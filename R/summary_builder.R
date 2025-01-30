#' Simplified Summary Builder Module
#' @importFrom shiny NS selectizeInput selectInput moduleServer reactive observeEvent
NULL

#' Create summary builder UI components
#'
#' @param id Character. The module ID
#' @return A list of Shiny UI elements
#' @export
summary_builder_ui <- function(id) {
  ns <- NS(id)

  list(
    selectizeInput(
      ns("metrics"),
      "Select metrics:",
      choices = NULL,
      multiple = TRUE
    ),
    selectInput(
      ns("functions"),
      "Select functions:",
      choices = c(
        "Count" = "n",
        "Sum" = "sum",
        "Mean" = "mean",
        "Min" = "min",
        "Max" = "max"
      ),
      multiple = TRUE,
      selected = "n"
    )
  )
}

#' Create summary builder server
#'
#' @param id Character. The module ID
#' @param selected_table Reactive. Selected table from table_picker
#' @param column_info Reactive. Column info from table_picker
#' @return List of reactive expressions containing summary specifications
#' @export
summary_builder_server <- function(id, selected_table, column_info) {
  moduleServer(id, function(input, output, session) {

    # Update available metrics (numeric columns only) when table changes
    observe({
      req(selected_table(), column_info())
      numeric_cols <- names(which(sapply(column_info(), function(x) x$type == "numeric")))
      updateSelectizeInput(
        session,
        "metrics",
        choices = numeric_cols,
        selected = character(0)
      )
    })

    # Build summary specifications
    summary_specs <- reactive({
      req(input$metrics, input$functions)

      # Create list of all metric-function combinations
      specs <- list()
      for (metric in input$metrics) {
        for (func in input$functions) {
          if (func == "n") {
            # Special case for count
            specs[[length(specs) + 1]] <- list(
              metric = metric,
              func = func,
              sql = "COUNT(*)"
            )
          } else {
            # Regular aggregate functions
            specs[[length(specs) + 1]] <- list(
              metric = metric,
              func = func,
              sql = sprintf("%s([%s])", toupper(func), metric)
            )
          }
        }
      }
      specs
    })

    # Return interface
    list(
      summary_specs = summary_specs
    )
  })
}

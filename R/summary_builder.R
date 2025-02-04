#' Create summary builder UI components
#'
#' @param id Character. The module ID
#' @return A list of Shiny UI elements
#' @export
summary_builder_ui <- function(id) {
  ns <- NS(id)
  
  list(
    shiny::checkboxInput(
      ns("include_count"),
      "Include record count",
      value = TRUE
    ),
    selectizeInput(
      ns("metrics"),
      "Select metrics (optional):",
      choices = NULL,
      multiple = TRUE
    ),
    selectInput(
      ns("functions"),
      "Select functions for metrics:",
      choices = c(
        "Mean" = "mean",
        "Sum" = "sum",
        "Min" = "min",
        "Max" = "max"
      ),
      multiple = TRUE,
      selected = "mean"
    )
  )
}

#' Create summary builder server
#'
#' @param id Character. The module ID
#' @param selected_table Reactive. Selected table from table_picker
#' @param column_info Reactive. Column info list containing metadata and distinct values
#' @return List of reactive expressions containing summary specifications
#' @export
summary_builder_server <- function(id, selected_table, column_info) {
  moduleServer(id, function(input, output, session) {
    
    # Update available metrics (numeric columns only) when table changes
    observe({
      req(selected_table(), column_info())
      col_info <- column_info()
      
      # Get numeric columns from metadata
      numeric_cols <- col_info$metadata %>%
        filter(column_type == "numeric") %>%
        pull(column_name)
      
      updateSelectizeInput(
        session,
        "metrics",
        choices = numeric_cols,
        selected = character(0)
      )
    })
    
    # Build summary specifications
    summary_specs <- reactive({
      specs <- list()
      
      # Always add count if selected
      if (input$include_count) {
        specs[[length(specs) + 1]] <- list(
          metric = "*",
          func = "count",
          sql = "n"  # Use dplyr's n() function
        )
      }
      
      # Add metric-function combinations if any metrics selected
      if (length(input$metrics) > 0 && length(input$functions) > 0) {
        for (metric in input$metrics) {
          for (func in input$functions) {
            specs[[length(specs) + 1]] <- list(
              metric = metric,
              func = func,
              sql = func  # Use dplyr function names directly
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
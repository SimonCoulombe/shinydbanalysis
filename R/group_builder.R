#' Create group builder UI components
#'
#' @param id Character. The module ID
#' @return A Shiny UI element
#' @export
group_builder_ui <- function(id) {
  ns <- NS(id)
  
  selectizeInput(
    ns("group_vars"),
    "Select grouping variables:",
    choices = NULL,
    multiple = TRUE
  )
}

#' Create group builder server
#'
#' @param id Character. The module ID
#' @param selected_table Reactive. Selected table from table_picker
#' @param column_info Reactive. Column info list containing metadata and distinct values
#' @return List of reactive expressions
#' @export
group_builder_server <- function(id, selected_table, column_info) {
  moduleServer(id, function(input, output, session) {
    
    # Update available columns when table changes
    observe({
      req(selected_table(), column_info())
      col_info <- column_info()
      
      # Filter to only categorical columns
      categorical_cols <- col_info$metadata %>%
        filter(column_type != "numeric") %>%
        pull(column_name)
      
      updateSelectizeInput(
        session,
        "group_vars",
        choices = categorical_cols,
        selected = character(0)
      )
    })
    
    # Return interface
    list(
      group_vars = reactive(input$group_vars)
    )
  })
}
#' Simplified Group Builder Module
#' @importFrom shiny NS selectizeInput moduleServer reactive
NULL

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
#' @param column_info Reactive. Column info from table_picker
#' @return List of reactive expressions
#' @export
group_builder_server <- function(id, selected_table, column_info) {
  moduleServer(id, function(input, output, session) {

    # Update available columns when table changes
    observe({
      req(selected_table(), column_info())
      updateSelectizeInput(
        session,
        "group_vars",
        choices = names(column_info()),
        selected = character(0)
      )
    })

    # Return interface
    list(
      group_vars = reactive(input$group_vars)
    )
  })
}

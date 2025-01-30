#' Table Picker Module
#' @importFrom shiny NS selectInput moduleServer reactive
NULL

#' Create table picker UI components
#'
#' @param id Character. The module ID
#' @param column_info_dir Path to directory containing column info files
#' @return A Shiny UI element
#' @export
table_picker_ui <- function(id, column_info_dir) {
  ns <- NS(id)

  selectInput(
    ns("table_select"),
    "Select table:",
    choices = c("Select table" = "", list_available_tables(column_info_dir))
  )
}

#' Create table picker server
#'
#' @param id Character. The module ID
#' @param column_info_dir Path to directory containing column info files
#' @return Reactive expression containing selected table and column info
#' @export
table_picker_server <- function(id, column_info_dir) {
  moduleServer(id, function(input, output, session) {

    # Get column information
    column_info <- reactive({
      req(input$table_select)
      validate(need(
        input$table_select %in% list_available_tables(column_info_dir),
        "Please select a valid table"
      ))

      col_info_path <- file.path(
        column_info_dir,
        paste0("column_info_", input$table_select, ".rds")
      )
      validate(need(
        file.exists(col_info_path),
        sprintf("Column info file not found: %s", col_info_path)
      ))

      # Debug log
      message("Loading column info for table: ", input$table_select)
      info <- readRDS(col_info_path)
      message("Loaded columns: ", paste(names(info), collapse=", "))

      info
    })

    # Return interface
    list(
      selected_table = reactive(input$table_select),
      column_info = column_info
    )
  })
}

#' List available tables from column info directory
#' @noRd
list_available_tables <- function(column_info_dir) {
  files <- list.files(column_info_dir, pattern = "^column_info_.*\\.rds$")
  message("Available tables: ", paste(files, collapse=", "))
  gsub("^column_info_(.+)\\.rds$", "\\1", files)
}

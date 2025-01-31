#' Get list of available tables with column info files
#'
#' @param column_info_dir Path to directory containing column info files
#' @param pool Database connection pool
#' @return Named character vector of available tables
#' @noRd
get_available_tables <- function(column_info_dir, pool) {
  # Get all tables from database
  db_tables <- dbListTables(pool)
  
  # Get tables with column info files
  col_info_files <- list.files(column_info_dir, pattern = "^column_info_.*\\.rds$")
  col_info_tables <- gsub("^column_info_(.+)\\.rds$", "\\1", col_info_files)
  
  # Keep only tables that exist in both
  available_tables <- intersect(db_tables, col_info_tables)
  
  # Return as named vector (same names as values for selectInput)
  stats::setNames(available_tables, available_tables)
}

#' Parse schema and table name
#'
#' @param full_table_name Character string that might contain schema (e.g., "schema.table")
#' @return List with schema and table components
#' @noRd
parse_table_name <- function(full_table_name) {
  parts <- strsplit(full_table_name, "\\.", fixed = TRUE)[[1]]
  if (length(parts) == 2) {
    list(schema = parts[1], table = parts[2])
  } else {
    list(schema = NULL, table = parts[1])
  }
}

#' Create table reference using schema if present
#'
#' @param pool Database connection pool
#' @param table_info List containing schema and table names
#' @return A dbplyr table reference
#' @importFrom dplyr tbl
#' @noRd
create_table_ref <- function(pool, table_info) {
  if (!is.null(table_info$schema)) {
    tbl(pool, dbplyr::in_schema(table_info$schema, table_info$table))
  } else {
    tbl(pool, table_info$table)
  }
}

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
    choices = c("Select table" = "")
  )
}

#' Create table picker server
#'
#' @param id Character. The module ID
#' @param column_info_dir Path to directory containing column info files
#' @return List of reactive expressions
#' @export
table_picker_server <- function(id, pool, column_info_dir) {
  moduleServer(id, function(input, output, session) {
    
    # Update available tables
    observe({
      available_tables <- try(get_available_tables(column_info_dir, pool))
      
      if (inherits(available_tables, "try-error")) {
        warning("Error getting available tables: ", available_tables)
        available_tables <- character(0)
      }
      
      updateSelectInput(
        session,
        "table_select",
        choices = c("Select table" = "", available_tables),
        selected = input$table_select
      )
    })
    
    # Parse selected table name and get column info
    table_info <- reactive({
      req(input$table_select)
      validate(need(
        input$table_select != "",
        "Please select a valid table"
      ))
      
      # Parse table name for potential schema
      parse_table_name(input$table_select)
    })
    
    # Get column information
    column_info <- reactive({
      req(table_info())
      
      # We use the full table name (including schema if present) for the file
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
      selected_table = reactive({
        req(table_info())
        input$table_select
      }),
      table_info = table_info,  # New: return parsed table info
      column_info = column_info,
      create_table_ref = reactive({  # New: return function to create table reference
        req(table_info())
        create_table_ref(pool, table_info())
      })
    )
  })
}
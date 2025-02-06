
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
#' @param id Character. The module ID
#' @return A Shiny UI element
#' @export
table_picker_ui <- function(id) {
  ns <- NS(id)
  
  selectInput(
    ns("table_select"),
    "Select table:",
    choices = c("Select table" = "")
  )
}

#' Create table picker server with column restrictions
#' @param id Character. The module ID
#' @param pool Pool object. Database connection pool
#' @param storage_info List containing storage configuration
#' @param restricted_columns Character vector. Column names to restrict
#' @return List of reactive expressions
#' @export
table_picker_server <- function(id, pool, storage_info, restricted_columns = character(0)) {
  moduleServer(id, function(input, output, session) {
    
    # Update available tables
    observe({
      available_tables <- try(get_available_tables(storage_info, pool))
      
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
    
    # Parse selected table name
    table_info <- reactive({
      req(input$table_select)
      validate(need(
        input$table_select != "",
        "Please select a valid table"
      ))
      
      parse_table_name(input$table_select)
    })
    
    # Create filtered table reference
    create_filtered_ref <- reactive({
      req(table_info())
      tbl_ref <- create_table_ref(pool, table_info())
      
      if (length(restricted_columns) > 0) {
        # Get all columns and filter out unavailable ones
        all_cols <- colnames(tbl_ref)
        available_cols <- setdiff(all_cols, restricted_columns)
        tbl_ref %>% select(all_of(available_cols))
      } else {
        tbl_ref
      }
    })
    
    # Return interface with unavailable columns
    list(
      selected_table = reactive(input$table_select),
      table_info = table_info,
      create_table_ref = create_filtered_ref,
      restricted_columns = reactive(restricted_columns)
    )
  })
}

#' Get list of available tables with column info files, excluding restricted columns
#' @param storage_info List containing storage configuration
#' @param pool Database connection pool
#' @param restricted_columns Character vector of columns to restrict
#' @return Named character vector of available tables
#' @noRd
get_available_tables <- function(storage_info, pool) {
  if (storage_info$storage_type == "local") {
    # Get tables with column info files from local storage
    col_info_files <- list.files(storage_info$local_dir, 
                                 pattern = "^column_info_.*\\.parquet$")
    table_names <- gsub("^column_info_(.+)\\.parquet$", "\\1", col_info_files)
  } else {
    # Get tables with column info files from ADLS
    endpoint <- storage_endpoint(storage_info$adls_endpoint, 
                                 sas = storage_info$sas_token)
    container <- storage_container(endpoint, storage_info$adls_container)
    
    # List files in container
    files <- azure_files(container)
    
    # Filter for column info files
    col_info_files <- files$name[grepl("^column_info_.*\\.parquet$", files$name)]
    table_names <- gsub("^column_info_(.+)\\.parquet$", "\\1", col_info_files)
  }
  
  # Verify tables exist in database
  existing_tables <- sapply(table_names, function(name) {
    table_info <- parse_table_name(name)
    table_exists(pool, table_info)
  })
  
  available_tables <- table_names[existing_tables]
  stats::setNames(available_tables, available_tables)
}

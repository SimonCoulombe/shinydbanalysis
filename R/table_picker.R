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
#' @param parsed_table_name List containing schema and table names
#' @return A dbplyr table reference
#' @importFrom dplyr tbl
#' @noRd
create_table_ref <- function(pool, parsed_table_name) {
  if (!is.null(parsed_table_name$schema)) {
    tbl(pool, dbplyr::in_schema(parsed_table_name$schema, parsed_table_name$table))
  } else {
    tbl(pool, parsed_table_name$table)
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
    
    # Reactive value to store available tables
    available_tables <- reactiveVal(character(0))
    
    # Function to update available tables
    update_available_tables <- function() {
      tables <- try(get_available_tables(storage_info, pool))
      
      if (inherits(tables, "try-error")) {
        warning("Error getting available tables: ", tables)
        tables <- character(0)
      }
      
      available_tables(tables)
    }
    
    # Initial call to update available tables
    update_available_tables()
    
    # Observe changes to available tables and update the select input
    observeEvent(available_tables(), {
      tables <- available_tables()
      if (length(tables) > 0) {
        updateSelectInput(
          session,
          "table_select",
          choices = c("Select table" = "", tables),
          selected = NULL
        )
      }
    })
    
    # Reactive value to store the selected table info
    selected_parsed_table_name <- reactiveVal(NULL)
    
    # Observe changes to the table selection input
    observeEvent(input$table_select, {
      req(input$table_select)
      validate(need(
        input$table_select != "",
        "Please select a valid table"
      ))
      selected_parsed_table_name(parse_table_name(input$table_select))
    })
    

        
    # Create table reference without restricted columns
    selected_tbl_ref_without_restricted_columns <- reactive({
      req(selected_parsed_table_name())
      tbl_ref <- create_table_ref(pool, selected_parsed_table_name())
      
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
      selected_table_name = reactive(input$table_select),
      selected_parsed_table_name = selected_parsed_table_name,
      selected_tbl_ref_without_restricted_columns = selected_tbl_ref_without_restricted_columns
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
    col_info_files <- list.files(storage_info$column_info_dir, 
                                 pattern = "^column_info_.*\\.parquet$")
    table_names <- gsub("^column_info_(.+)\\.parquet$", "\\1", col_info_files)
  } else {
    # Get tables with column info files from ADLS
    endpoint <- storage_endpoint(storage_info$adls_endpoint, 
                                 sas = storage_info$sas_token)
    container <- storage_container(endpoint, storage_info$adls_container)
    
    # List files in container
    files <- AzureStor::list_storage_files(container, storage_info$column_info_dir) %>% filter(isdir == FALSE)
    
    # Filter for column info files
    files_basename <- basename (files$name )
    
    col_info_files <- files_basename[grepl("^column_info_.*\\.parquet$", files_basename)]
    table_names <- gsub("^column_info_(.+)\\.parquet$", "\\1", col_info_files)
  }
  
  # Verify tables exist in database
  existing_tables <- sapply(table_names, function(name) {
    parsed_table_name <- parse_table_name(name)
    table_exists(pool, parsed_table_name)
  })
  
  available_tables <- table_names[existing_tables]
  stats::setNames(available_tables, available_tables)
}

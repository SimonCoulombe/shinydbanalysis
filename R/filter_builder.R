#' Create filter builder UI components
#' @param id Character. The module ID
#' @return A list of Shiny UI elements
#' @export
filter_builder_ui <- function(id) {
  ns <- NS(id)
  
  list(
    selectInput(
      ns("add_filter"),
      "Add filter for column:",
      choices = c("Select column" = "")
    ),
    uiOutput(ns("filters"))
  )
}

#' Create filter builder server logic
#' @param id Character. The module ID
#' @param storage_info List with storage configuration
#' @param selected_table Reactive. Selected table name
#' @return List of reactive expressions
#' @export
filter_builder_server <- function(id, storage_info, selected_table) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # State management
    state <- reactiveValues(
      modules = list(),
      filter_states = list()
    )
    
    # Reactive for loading column info
    column_info <- reactive({
      req(selected_table())
      
      read_column_info(
        tablename = selected_table(),
        storage_type = storage_info$storage_type,
        local_dir = storage_info$local_dir,
        adls_endpoint = storage_info$adls_endpoint,
        adls_container = storage_info$adls_container,
        sas_token = storage_info$sas_token
      )
    })
    
    # Clear filters when table changes
    observeEvent(selected_table(), {
      # Immediately clear where clause to prevent invalid queries
      state$filter_states <- list()
      
      # Remove all filter UI elements first
      removeUI(selector = paste0("#", ns("filters"), " > *"))
      
      # Then reset module state
      state$modules <- list()
      
      # Reset the add filter dropdown
      updateSelectInput(
        session,
        "add_filter",
        choices = c("Select column" = "")
      )
    }, ignoreInit = TRUE)
    
    # Update available columns (separate observer to prevent race conditions)
    observe({
      req(selected_table(), column_info())
      col_info <- column_info()
      
      # Get active columns (those already with filters)
      active_columns <- sapply(state$modules, function(mod) mod$instance$column)
      
      # Get all available columns from metadata
      all_columns <- col_info$metadata$column_name
      
      # Get columns without filters
      available_columns <- setdiff(all_columns, active_columns)
      
      updateSelectInput(
        session,
        "add_filter",
        choices = c("Select column" = "", available_columns)
      )
    })
    
    # Add new filter
    observeEvent(input$add_filter, {
      req(input$add_filter != "")
      col_info <- column_info()
      
      # Get metadata for selected column
      col_metadata <- col_info$metadata %>%
        filter(column_name == input$add_filter)
      
      add_new_filter(input$add_filter, state, col_metadata, col_info$distinct_values, session)
      updateSelectInput(session, "add_filter", selected = "")
    })
    
    # Handle filter updates
    observe({
      mods <- state$modules
      current_states <- state$filter_states
      
      for (mod_name in names(mods)) {
        mod <- mods[[mod_name]]
        value <- mod$instance$value()
        if (!is.null(value)) {
          current_states[[mod_name]] <- value
        }
      }
      state$filter_states <- current_states
    })
    
    # Handle filter removal
    observe({
      mods <- state$modules
      for (mod_name in names(mods)) {
        mod <- mods[[mod_name]]
        if (!is.null(mod$instance$remove()) && mod$instance$remove() > 0) {
          state$modules[[mod_name]] <- NULL
          state$filter_states[[mod_name]] <- NULL
          removeUI(selector = paste0("#", mod_name))
        }
      }
    })
    
    # Build WHERE clause reactively
    where_clause <- reactive({
      req(column_info())
      
      where_clauses <- build_where_clauses(state$modules)
      if (length(where_clauses) > 0) {
        paste(where_clauses, collapse = " & ")  # Using & for SQL AND
      } else {
        ""
      }
    })
    
    # Render UI elements
    output$filters <- renderUI({
      req(selected_table(), column_info())
      mods <- state$modules
      states <- state$filter_states
      col_info <- column_info()
      
      filter_list <- lapply(names(mods), function(id) {
        full_id <- ns(id)
        col_name <- mods[[id]]$instance$column
        
        # Get metadata for this column
        col_metadata <- col_info$metadata %>%
          filter(column_name == col_name)
        
        filter_module_ui(
          full_id, 
          list(
            metadata = col_metadata,
            distinct_values = col_info$distinct_values
          ),
          states[[id]]
        )
      })
      
      do.call(tagList, filter_list)
    })
    
    # Return interface
    list(
      where_clause = where_clause,
      current_filters = reactive(state$modules)
    )
  })
}

# Helper Functions ----

#' Add a new filter to the state
#' @noRd
add_new_filter <- function(column_name, state, metadata, distinct_values, session) {
  current_id <- generate_filter_id(column_name)
  
  if (!column_exists_in_modules(column_name, state$modules)) {
    filter_instance <- filter_module_server(
      current_id,
      metadata,
      distinct_values,
      state$filter_states[[current_id]]
    )
    
    state$modules[[current_id]] <- list(
      id = current_id,
      instance = filter_instance
    )
  }
}

#' Generate unique filter ID
#' @noRd
generate_filter_id <- function(column_name) {
  paste0(
    "filter_",
    column_name,
    "_",
    format(Sys.time(), "%H%M%S")
  )
}

#' Check if column already has a filter
#' @noRd
column_exists_in_modules <- function(column_name, modules) {
  any(sapply(modules, function(mod) mod$instance$column == column_name))
}

#' Build WHERE clauses for SQL query
#' @noRd
build_where_clauses <- function(modules) {
  if (length(modules) == 0) return(character(0))
  
  filters <- lapply(modules, function(mod) {
    build_filter_expression(
      mod$instance$column,
      mod$instance$type,
      mod$instance$value()
    )
  })
  
  # Remove NULL filters
  filters <- Filter(Negate(is.null), filters)
  
  if (length(filters) == 0) return(character(0))
  filters
}
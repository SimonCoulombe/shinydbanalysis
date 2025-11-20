#' Create filter builder UI components
#' 
#' @description
#' Creates the user interface for the filter builder module. This module allows users
#' to dynamically add, configure, and remove filters for different columns in a dataset.
#' 
#' The UI consists of:
#' \itemize{
#'   \item A dropdown menu to select which column to add a filter for
#'   \item A dynamic area that displays all active filters with their controls
#' }
#' 
#' @param id Character. The module ID used to create a unique namespace for this instance
#' 
#' @return A list containing two Shiny UI elements:
#' \describe{
#'   \item{selectInput}{A dropdown menu labeled "Add filter for column:" that shows available columns}
#'   \item{uiOutput}{A dynamic UI container that renders all active filter controls}
#' }
#' 
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   filter_builder_ui("my_filters")
#' )
#' }
#' 
#' @seealso [filter_builder_server()] for the server-side logic
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
#' 
#' @description
#' Server-side logic for the filter builder module. This function manages multiple
#' single_filter modules, allowing users to dynamically add and remove filters for
#' different columns. It handles loading column metadata, tracking filter state,
#' and building SQL WHERE clauses from active filters.
#' 
#' The module automatically:
#' \itemize{
#'   \item Loads column metadata from storage when the table changes
#'   \item Updates the dropdown to show only columns without active filters
#'   \item Preserves filter values when the UI re-renders
#'   \item Clears all filters when switching tables
#'   \item Generates SQL WHERE clauses from all active filters
#' }
#' 
#' @param id Character. The module ID matching the UI function
#' @param storage_info List with storage configuration containing:
#' \describe{
#'   \item{storage_type}{Character. Either "local" or "adls"}
#'   \item{column_info_dir}{Character. Directory path for local storage}
#'   \item{adls_endpoint}{Character. Azure Data Lake Storage endpoint (for ADLS)}
#'   \item{adls_container}{Character. Container name (for ADLS)}
#'   \item{sas_token}{Character. SAS token for authentication (for ADLS)}
#' }
#' @param selected_table_name Reactive expression returning the currently selected table name
#' @param restricted_columns Reactive expression or vector of column names to exclude from filtering
#' 
#' @return A named list with two reactive expressions:
#' \describe{
#'   \item{where_clause}{Reactive returning a character string containing the SQL WHERE clause
#'     built from all active filters. Returns an empty string ("") when no filters are active.
#'     Multiple filter conditions are combined with " & " (AND operator). 
#'     Example: "carat >= 0.5 & carat <= 1.5 & cut %in% c('Ideal', 'Premium')"}
#'   \item{current_filters}{Reactive returning a named list of all active filter module instances.
#'     Each element contains an 'id' and 'instance' with the filter's reactive values (column, type, value, etc.).
#'     Useful for debugging or accessing individual filter states.}
#' }
#' 
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   selected_table <- reactive("diamonds")
#'   
#'   storage_info <- list(
#'     storage_type = "local",
#'     column_info_dir = "inst/extdata/column_info"
#'   )
#'   
#'   filters <- filter_builder_server(
#'     "my_filters",
#'     storage_info,
#'     selected_table,
#'     restricted_columns = c("id", "timestamp")
#'   )
#'   
#'   # Use the where_clause in a query
#'   filtered_data <- reactive({
#'     where <- filters$where_clause()
#'     if (where != "") {
#'       tbl(pool, selected_table()) %>%
#'         filter(!!!rlang::parse_exprs(where))
#'     } else {
#'       tbl(pool, selected_table())
#'     }
#'   })
#' }
#' }
#' 
#' @seealso [filter_builder_ui()] for the UI components, [single_filter_server()] for individual filter logic
#' @export
filter_builder_server <- function(id, storage_info, selected_table_name, restricted_columns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    state <- reactiveValues(
      modules = list(),
      filter_states = list()
    )
    
    # Reactive for loading column info
    column_info <- reactive({
      message("[filter_builder] column_info reactive evaluating, selected_table = ", selected_table_name())
      req(selected_table_name())
      
      info <- read_column_info(
        tablename = selected_table_name(),
        storage_type = storage_info$storage_type,
        column_info_dir = storage_info$column_info_dir,
        adls_endpoint = storage_info$adls_endpoint,
        adls_container = storage_info$adls_container,
        sas_token = storage_info$sas_token
      )
      
      # Filter out unavailable columns
      restricted_cols <- if (is.reactive(restricted_columns)) {
        result <- restricted_columns()
        result
      } else {
        restricted_columns
      }
      
      if (length(restricted_cols) > 0) {
        info$metadata <- info$metadata %>%
          filter(!column_name %in% restricted_cols)
        info$distinct_values <- info$distinct_values %>%
          filter(!column_name %in% restricted_cols)
      }
      info
    })
    
    # Clear filters when table changes
    observeEvent(selected_table_name(), {
      message("[filter_builder] observeEvent Clear Filters: selected_table_name changed to: ", selected_table_name())
      state$filter_states <- list()
      removeUI(selector = paste0("#", ns("filters"), " > *"))
      state$modules <- list()
      # REMOVED: Don't clear dropdown here - let the observe block handle it
      # updateSelectInput(session, "add_filter", choices = c("Select column" = ""))
    }, ignoreInit = TRUE)
    
    # Update available columns
    observe({
      message("[filter_builder] observe updating available columns")
      req(selected_table_name(), column_info())
      col_info <- column_info()
      active_columns <- sapply(state$modules, function(mod) mod$instance$column)
      all_columns <- col_info$metadata$column_name
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
      
      add_new_filter(input$add_filter, state, col_info, session)
      updateSelectInput(session, "add_filter", selected = "")
    })
    
    # Handle filter updates
    observe({
      mods <- state$modules
      current_states <- state$filter_states
      
      isolate({
        for (mod_name in names(mods)) {
          mod <- mods[[mod_name]]
          value <- mod$instance$value()
          if (!is.null(value)) {
            current_states[[mod_name]] <- value
          }
        }
        state$filter_states <- current_states
      })
      
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
        paste(where_clauses, collapse = " & ")
      } else {
        ""
      }
    })
    
    # Render UI elements
    output$filters <- renderUI({
      req(selected_table_name(), column_info())
      mods <- state$modules
      states <- state$filter_states
      col_info <- column_info()
      
      filter_list <- lapply(names(mods), function(id) {
        full_id <- ns(id)
        col_name <- mods[[id]]$instance$column
        
        single_filter_ui(
          full_id, 
          column_info = col_info,
          column_name = col_name,
          initial_value = states[[id]]
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

#' Demo app for filter_builder module
#' 
#' @description
#' Launches an interactive Shiny app demonstrating the filter_builder module with
#' the pre-packaged demo datasets (diamonds, iris, gapdata). Users can select a dataset,
#' add multiple filters for different columns, and see the resulting WHERE clause and
#' filtered data in real-time.
#' 
#' Features:
#' \itemize{
#'   \item Select between three demo datasets (diamonds, iris, gapdata)
#'   \item Add multiple filters dynamically (filters cleared when switching datasets)
#'   \item View the generated SQL WHERE clause
#'   \item See active filter details
#'   \item Interactive data table showing filtered results with row count
#' }
#' 
#' @return A Shiny app object (run interactively, no return value)
#' 
#' @examples
#' \dontrun{
#' # Launch the demo app
#' filter_builder_demo()
#' }
#' 
#' @export
filter_builder_demo <- function() {
  
  pool <- get_demo_pool()
  storage_info <- get_demo_storage_info()
  
  ui <- fluidPage(
    titlePanel("Filter Builder Demo"),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h3("Dataset Selection"),
        selectInput(
          "table_select",
          "Choose a dataset:",
          choices = c("diamonds", "iris", "gapdata"),
          selected = "diamonds"
        ),
        hr(),
        h3("Filter Configuration"),
        filter_builder_ui("demo_filters"),
        hr(),
        h4("Generated WHERE Clause:"),
        verbatimTextOutput("where_clause_display"),
        hr(),
        h4("Filter Module Details:"),
        p(style = "font-size: 0.9em; color: #666;", 
          "Shows all return values from each filter module, including inactive filters"),
        verbatimTextOutput("active_filters_display")
      ),
      
      mainPanel(
        width = 8,
        h3("Filtered Data"),
        p("Dataset: ", textOutput("dataset_name", inline = TRUE)),
        p("Total rows: ", textOutput("row_count", inline = TRUE)),
        DT::dataTableOutput("filtered_table")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    selected_table <- reactive({
      req(input$table_select)
      input$table_select
    })
    
    filters <- filter_builder_server(
      "demo_filters",
      storage_info,
      selected_table,
      restricted_columns = character(0)
    )
    
    filtered_data <- reactive({
      req(selected_table())
      where <- filters$where_clause()
      
      query <- dplyr::tbl(pool, selected_table())
      
      if (where != "") {
        query <- query %>%
          dplyr::filter(!!!rlang::parse_exprs(where))
      }
      
      query %>% dplyr::collect()
    })
    
    output$dataset_name <- renderText({
      req(selected_table())
      selected_table()
    })
    
    output$where_clause_display <- renderPrint({
      where <- filters$where_clause()
      if (where == "") {
        cat("No filters active")
      } else {
        cat(where)
      }
    })
    
    output$active_filters_display <- renderPrint({
      current <- filters$current_filters()
      if (length(current) == 0) {
        cat("No active filters")
      } else {
        filter_info <- lapply(names(current), function(id) {
          mod <- current[[id]]$instance
          
          # Get all return values from the module
          value_display <- if (!is.null(mod$value()) && length(mod$value()) > 0) {
            paste(mod$value(), collapse = ", ")
          } else {
            "NULL or empty"
          }
          
          remove_count <- if (!is.null(mod$remove())) mod$remove() else 0
          is_active_status <- if (!is.null(mod$is_active)) mod$is_active() else "N/A"
          
          paste0(
            "Filter ID: ", id, "\n",
            "Column: ", mod$column, "\n",
            "Type: ", mod$type, "\n",
            "Value: ", value_display, "\n",
            "Is Active: ", is_active_status, "\n",
            "Remove Count: ", remove_count
          )
        })
        cat(paste(filter_info, collapse = "\n\n"))
      }
    })
    
    output$row_count <- renderText({
      format(nrow(filtered_data()), big.mark = ",")
    })
    
    output$filtered_table <- DT::renderDataTable({
      DT::datatable(
        filtered_data(),
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })
    
    onStop(function() {
      pool::poolClose(pool)
    })
  }
  
  shinyApp(ui, server)
}


# Helper Functions ----

#' Add a new filter to the state
#' 
#' @description
#' Internal helper function that instantiates a new single_filter module and adds it
#' to the reactive state. This function is called when a user selects a column from
#' the "Add filter for column" dropdown.
#' 
#' @param column_name Character. The name of the column to create a filter for
#' @param state reactiveValues object containing:
#'   \describe{
#'     \item{modules}{Named list of active filter modules}
#'     \item{filter_states}{Named list preserving filter values for UI re-renders}
#'   }
#' @param column_info List with metadata and distinct_values dataframes for all columns
#' @param session Shiny session object for creating the module namespace
#' 
#' @details
#' The function generates a unique ID for the filter, checks if a filter for this
#' column already exists (to prevent duplicates), creates a single_filter_server
#' instance, and stores it in the state. If filter_states contains a saved value
#' for this filter ID, it will be passed as initial_value to preserve the user's
#' previous selection.
#' 
#' @return NULL (invisibly). Modifies the state reactiveValues object as a side effect.
#' @noRd
add_new_filter <- function(column_name, state, column_info, session) {
  current_id <- generate_filter_id(column_name)
  
  if (!column_exists_in_modules(column_name, state$modules)) {
    filter_instance <- single_filter_server(
      current_id,
      column_info = column_info,
      column_name = column_name,
      initial_value = state$filter_states[[current_id]]
    )
    
    state$modules[[current_id]] <- list(
      id = current_id,
      instance = filter_instance
    )
  }
}

#' Generate unique filter ID
#' 
#' @description
#' Creates a unique identifier for a filter module by combining the column name
#' with a timestamp. This ensures that if a user removes and re-adds a filter for
#' the same column, it gets a new ID.
#' 
#' @param column_name Character. The name of the column being filtered
#' 
#' @return Character string in the format "filter_{column_name}_{HHMMSS}".
#'   Example: "filter_carat_153042" for a carat filter created at 15:30:42
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
#' 
#' @description
#' Determines whether a filter for a given column name already exists in the
#' active modules list. Used to prevent creating duplicate filters for the same column.
#' 
#' @param column_name Character. The column name to check for
#' @param modules Named list of active filter module instances
#' 
#' @return Logical. TRUE if a filter for this column already exists, FALSE otherwise
#' @noRd
column_exists_in_modules <- function(column_name, modules) {
  any(sapply(modules, function(mod) mod$instance$column == column_name))
}

#' Build WHERE clauses for SQL query
#' 
#' @description
#' Converts all active filter modules into a character vector of R/SQL filter expressions.
#' Each filter is converted using build_single_filter_expression() and NULL filters (inactive
#' or empty) are removed. The resulting expressions can be combined with " & " to
#' create a complete WHERE clause.
#' 
#' @param modules Named list of active filter module instances. Each module must have
#'   an 'instance' element with column, type, and value() reactive values.
#' 
#' @return Character vector where each element is a filter expression string, or
#'   an empty character vector (character(0)) if no active filters exist.
#'   Examples:
#'   \itemize{
#'     \item Numeric: "carat >= 0.5 & carat <= 1.5"
#'     \item Categorical: "cut %in% c('Ideal', 'Premium')"
#'     \item Date: "date >= as.Date('2020-01-01') & date <= as.Date('2020-12-31')"
#'   }
#' @noRd
build_where_clauses <- function(modules) {
  if (length(modules) == 0) return(character(0))
  
  filters <- lapply(modules, function(mod) {
    build_single_filter_expression(
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


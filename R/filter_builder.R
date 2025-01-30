#' Simplified Filter Builder Module
#' @importFrom shiny NS selectInput uiOutput
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent
NULL

#' Create filter builder UI components
#'
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

#' Create filter builder server
#'
#' @param id Character. The module ID
#' @param selected_table Reactive. Selected table from table_picker
#' @param column_info Reactive. Column info from table_picker
#' @return List of reactive expressions
#' @export
filter_builder_server <- function(id, selected_table, column_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # State management
    state <- reactiveValues(
      modules = list(),
      filter_states = list()
    )

    # Clear filters when table changes
    observeEvent(selected_table(), {
      # Reset all state
      state$modules <- list()
      state$filter_states <- list()

      # Remove all filter UI elements
      removeUI(selector = paste0("#", ns("filters"), " > *"))
    }, ignoreInit = TRUE)

    # Update available columns
    observe({
      req(selected_table(), column_info())
      col_info <- column_info()

      active_columns <- sapply(state$modules, function(mod) mod$instance$column)
      available_columns <- setdiff(names(col_info), active_columns)

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
      where_clauses <- build_where_clauses(state$modules)
      if (length(where_clauses) > 0) {
        paste(where_clauses, collapse = " & ")  # Note: changed AND to &
      } else {
        ""
      }
    })

    # Render UI elements
    output$filters <- renderUI({
      req(selected_table())
      mods <- state$modules
      states <- state$filter_states
      col_info <- column_info()

      filter_list <- lapply(names(mods), function(id) {
        col_name <- mods[[id]]$instance$column
        full_id <- ns(id)
        filter_module_ui(full_id, col_info[[col_name]], states[[id]])
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



#' Load column information from file

#' @noRd

load_column_info <- function(table_name) {

  col_info_path <- file.path(

    "column_info",

    paste0("column_info_", table_name, ".rds")

  )



  validate(need(

    file.exists(col_info_path),

    sprintf("Column info file not found: %s", col_info_path)

  ))



  readRDS(col_info_path)

}



#' Add a new filter to the state

#' @noRd

add_new_filter <- function(column_name, state, col_info, session) {

  current_id <- generate_filter_id(column_name)



  if (!column_exists_in_modules(column_name, state$modules)) {

    filter_instance <- filter_module_server(

      current_id,

      col_info[[column_name]],

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







# In filter_builder.R

#' Build WHERE clauses for SQL query
#' @noRd
build_where_clauses <- function(modules) {
  if (length(modules) == 0) return(character(0))

  filters <- lapply(modules, function(mod) {
    if (mod$instance$type == "numeric") {
      build_numeric_clause(mod)
    } else {
      build_categorical_clause(mod)
    }
  })

  # Remove NULL filters
  filters <- Filter(Negate(is.null), filters)

  if (length(filters) == 0) return(character(0))
  filters
}

#' Build numeric WHERE clause
#' @noRd
build_numeric_clause <- function(mod) {
  value <- mod$instance$value()
  if (is.null(value) || length(value) != 2) return(NULL)

  column <- mod$instance$column
  sprintf(
    "%s >= %f & %s <= %f",
    column, value[1],
    column, value[2]
  )
}

#' Build categorical WHERE clause
#' @noRd
build_categorical_clause <- function(mod) {
  values <- mod$instance$value()
  if (is.null(values) || length(values) == 0) return(NULL)

  values_str <- paste(
    sprintf("'%s'", values),
    collapse = ", "
  )
  sprintf("%s %%in%% c(%s)", mod$instance$column, values_str)
}

#' Filter Builder Module
#' @importFrom shiny NS fluidPage sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny selectInput uiOutput verbatimTextOutput tableOutput
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent
#' @importFrom DBI dbGetQuery
#'
NULL

#' Create filter builder UI components
#'
#' @param id Character. The module ID
#' @param allowed_tables Vector. Names of tables that can be filtered
#' @return A list of Shiny UI elements
#' @export
filter_builder_ui <- function(id, allowed_tables) {
  ns <- NS(id)

  list(
    selectInput(
      ns("table_select"),
      "Select table:",
      choices = c("Select table" = "", allowed_tables)
    ),
    selectInput(
      ns("add_filter"),
      "Add filter for column:",
      choices = c("Select column" = "")
    ),
    uiOutput(ns("filters")),
    actionButton(
      ns("fetch_data"),
      "Fetch Data",
      class = "btn-primary"
    )
  )
}

#' Create filter builder server
#'
#' @param id Character. The module ID
#' @param pool Database connection pool
#' @param allowed_tables Vector. Names of tables that can be filtered
#' @return List of reactive expressions
#' @export
filter_builder_server <- function(id, pool, allowed_tables) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # State management
    state <- reactiveValues(
      modules = list(),
      filter_states = list(),
      last_fetched = list(
        filters = NULL,
        count = NULL
      ),
      fetch_count = 0
    )

    # Clear filters when table changes
    observeEvent(input$table_select, {
      # Reset all state
      state$modules <- list()
      state$filter_states <- list()
      state$last_fetched$filters <- NULL
      state$last_fetched$count <- NULL
      state$fetch_count <- 0

      # Remove all filter UI elements
      removeUI(selector = paste0("#", ns("filters"), " > *"))
    }, ignoreInit = TRUE)  # ignoreInit prevents clearing on initial load

    # Get column information
    column_info <- reactive({
      req(input$table_select)
      validate(need(
        input$table_select %in% allowed_tables,
        "Please select a valid table"
      ))

      load_column_info(input$table_select)
    })

    # Update available columns
    observe({
      req(input$table_select)
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
      add_new_filter(input$add_filter, state, column_info(), session)
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

    # Build and execute query
    filtered_data <- eventReactive(
      input$fetch_data,
      {
        req(input$table_select)
        withProgress(
          message = 'Fetching data...',
          {
            incProgress(0.3, detail = "Executing query")
            query <- build_query(input$table_select, state$modules)
            data <- dbGetQuery(pool, query)
            incProgress(0.7, detail = "Processing results")

            # Update last fetched state
            state$last_fetched$filters <- lapply(state$modules, function(mod) {
              list(
                value = mod$instance$value(),
                column = mod$instance$column,
                type = mod$instance$type
              )
            })
            state$last_fetched$count <- nrow(data)
            state$fetch_count <- state$fetch_count + 1

            data
          }
        )
      },
      ignoreNULL = FALSE
    )

    # Render UI elements
    output$filters <- renderUI({
      req(input$table_select)
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

    output$filter_summary <- renderPrint({
      req(input$table_select)
      mods <- state$modules

      cat("Current filters:\n")
      if (length(mods) > 0) {
        for (mod in mods) {
          cat(mod$instance$column, ": ")
          if (mod$instance$type == "numeric") {
            value <- mod$instance$value()
            cat("Range:", value[1], "to", value[2], "\n")
          } else {
            cat("Selected values:", paste(mod$instance$value(), collapse = ", "), "\n")
          }
        }
      } else {
        cat("No active filters\n")
      }

      cat("\n=== Last fetched state ===\n")
            if (state$fetch_count == 0) {
        cat("No data fetched yet")
      } else {
        cat("Number of rows:", state$last_fetched$count, "\n")
        if (length(state$last_fetched$filters) > 0) {
          for (filter in state$last_fetched$filters) {
        cat(filter$column, ": ")
        if (filter$type == "numeric") {
          cat("Range:", filter$value[1], "to", filter$value[2], "\n")
        } else {
          cat("Selected values:", paste(filter$value, collapse = ", "), "\n")
        }
          }
        }
      }
    })

    output$filtered_data <- renderTable({
      if (state$fetch_count == 0) {
        return(data.frame(Message = "Press 'Fetch Data' to load results"))
      }

      data <- head(filtered_data(), 10)
      if ("measurement_date" %in% names(data)) {
        data$measurement_date <- format(as.Date(data$measurement_date), "%Y-%m-%d")
      }
      data
    })

    # Return interface
    list(
      filtered_data = filtered_data,
      current_filters = reactive(state$modules),
      selected_table = reactive(input$table_select)
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

#' Build SQL query from current filters
#' @noRd
build_query <- function(table_name, modules) {
  base_query <- sprintf("SELECT * FROM [%s]", table_name)
  where_clauses <- build_where_clauses(modules)

  if (length(where_clauses) > 0) {
    paste(base_query, "WHERE", paste(where_clauses, collapse = " AND "))
  } else {
    base_query
  }
}

#' Build WHERE clauses for SQL query
#' @noRd
build_where_clauses <- function(modules) {
  lapply(modules, function(mod) {
    if (mod$instance$type == "numeric") {
      build_numeric_clause(mod)
    } else {
      build_categorical_clause(mod)
    }
  })
}

#' Build numeric WHERE clause
#' @noRd
build_numeric_clause <- function(mod) {
  value <- mod$instance$value()
  sprintf(
    "[%s] >= %f AND [%s] <= %f",
    mod$instance$column, value[1],
    mod$instance$column, value[2]
  )
}

#' Build categorical WHERE clause
#' @noRd
build_categorical_clause <- function(mod) {
  values_str <- paste(
    sprintf("'%s'", mod$instance$value()),
    collapse = ","
  )
  sprintf("[%s] IN (%s)", mod$instance$column, values_str)
}

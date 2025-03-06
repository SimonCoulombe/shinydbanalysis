library(shiny)
library(DBI)
library(pool)

#' Individual Filter Module UI
#' @param id The module ID
#' @param column_info Column information
#' @param initial_value Initial filter value
filterModuleUI <- function(id, column_info, initial_value = NULL) {
  ns <- function(x) paste0(id, "-", x)  # # Don't create a new namespace - use the ID as is since it's already namespaced
  cat("\nCreating UI for filter:", id, "\n")  # Debug print
  cat("Generated input ID will be:", ns("filter_value"), "\n")  # Debug print

  if(is.null(initial_value)) {
    initial_value <- if(column_info$type == "numeric") {
      c(column_info$values$min, column_info$values$max)
    } else {
      column_info$values
    }
  }

  if(column_info$type == "numeric") {
    filter_input <- sliderInput(
      inputId = ns("filter_value"),
      label = NULL,
      min = column_info$values$min,
      max = column_info$values$max,
      value = initial_value,
      step = (column_info$values$max - column_info$values$min) / 100
    )
  } else {
    filter_input <- checkboxGroupInput(
      inputId = ns("filter_value"),
      label = NULL,
      choices = column_info$values,
      selected = initial_value
    )
  }

  tagList(
    div(
      id = ns("container"),  # Add this ID for debugging
      style = "display: flex; align-items: start; margin-bottom: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;",
      div(
        style = "display: flex; align-items: start; margin-bottom: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;",
        div(
          style = "flex-grow: 1;",
          h4(column_info$name, style = "margin-top: 0;"),
          filter_input
        ),
        div(
          style = "margin-left: 10px;",
          actionButton(
            inputId = ns("remove"),
            label = "×",
            class = "btn-danger",
            style = "padding: 0px 8px;"
          )
        )
      )
    )
  )
}

#' Individual Filter Module UI
#' @param id The module ID
#' @param column_info Column information
#' @param initial_value Initial filter value
filterModule <- function(id, column_info, initial_value = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cat("\nInitializing server for filter:", id, "\n")  # Debug print
    cat("Looking for input ID:", ns("filter_value"), "\n")  # Debug print

    # Add debugging for input values
    observe({
      cat("\nAll available inputs for this module:", "\n")
      cat(paste(names(input), collapse=", "), "\n")
    })

    cat("\nCreating new filter module for:", column_info$name, "\n")  # Debug print

    if(is.null(initial_value)) {
      initial_value <- if(column_info$type == "numeric") {
        c(column_info$values$min, column_info$values$max)
      } else {
        column_info$values
      }
    }

    filter_state <- reactiveVal(initial_value)
    is_active <- reactiveVal(FALSE)

    cat("Setting up observeEvent for filter_value\n")  # Debug print
    observeEvent(input$filter_value, {
      cat("\nFILTER VALUE CHANGED for", column_info$name, "\n")
      cat("Old value:", paste(filter_state(), collapse=", "), "\n")
      cat("New value:", paste(input$filter_value, collapse=", "), "\n")

      if (!identical(filter_state(), input$filter_value)) {
        is_active(TRUE)
      }
      filter_state(input$filter_value)
    }, ignoreInit = FALSE)  # Added ignoreInit parameter

    # Add a print to watch raw input value
    observe({
      cat("\nRaw input value for", column_info$name, ":", paste(input$filter_value, collapse=", "), "\n")
    })

    list(
      value = filter_state,
      remove = reactive(input$remove),
      column = column_info$name,
      type = column_info$type,
      is_active = is_active
    )
  })
}

#' Filter Builder Module UI
#' @param id The module ID
#' @param allowed_tables Vector of table names that can be filtered
#' @export
filterBuilderUI <- function(id, allowed_tables) {
  ns <- NS(id)

  fluidPage(
    titlePanel("Dataset Filter Builder"),

    sidebarLayout(
      sidebarPanel(
        selectInput(ns("table_select"), "Select table:",
                    choices = c("Select table" = "", allowed_tables)),
        selectInput(ns("add_filter"), "Add filter for column:",
                    choices = c("Select column" = "")),
        br(),
        uiOutput(ns("filters")),
        width = 6
      ),

      mainPanel(
        h3("Current Filters:"),
        verbatimTextOutput(ns("filter_summary")),
        br(),
        actionButton(ns("fetch_data"), "Fetch Data",
                     class = "btn-primary"),
        h3("Filtered Data Preview:"),
        tableOutput(ns("filtered_data")),
        width = 6
      )
    )
  )
}

#' Filter Builder Module Server
#' @param id The module ID
#' @param pool Database connection pool
#' @param allowed_tables Vector of table names that can be filtered
#' @export
filterBuilderServer <- function(id, pool, allowed_tables) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    modules <- reactiveVal(list())
    filter_states <- reactiveVal(list())
    last_fetched_filters <- reactiveVal(list())
    last_fetched_count <- reactiveVal(NULL)
    fetch_count <- reactiveVal(0)

    # Get column information based on selected table
    column_info <- reactive({
      req(input$table_select)
      validate(
        need(input$table_select %in% allowed_tables, "Please select a valid table")
      )

      col_info_path <- file.path("column_info", paste0("column_info_", input$table_select, ".rds"))
      validate(
        need(file.exists(col_info_path),
             sprintf("Column info file not found: %s", col_info_path))
      )

      readRDS(col_info_path)
    })

    # Update column choices when table is selected
    observe({
      req(input$table_select)
      col_info <- column_info()
      current_modules <- modules()

      active_columns <- sapply(current_modules, function(mod) {
        mod$instance$column
      })

      available_columns <- setdiff(names(col_info), active_columns)

      updateSelectInput(session, "add_filter",
                        choices = c("Select column" = "", available_columns))
    })

    # Add new filter
    observeEvent(input$add_filter, {
      req(input$add_filter != "")

      current_id <- paste0("filter_", input$add_filter, "_", format(Sys.time(), "%H%M%S"))
      cat("\nCreating new filter with ID:", current_id, "\n")  # Debug print
      current_modules <- modules()
      current_states <- filter_states()
      col_info <- column_info()

      column_name <- input$add_filter
      existing_filter <- FALSE
      for(mod in current_modules) {
        if(mod$instance$column == column_name) {
          existing_filter <- TRUE
          break
        }
      }

      if (!existing_filter) {
        # Create the module instance first
        filter_instance <- filterModule(
          id = current_id,
          col_info[[column_name]],
          current_states[[current_id]]
        )

        current_modules[[current_id]] <- list(
          id = current_id,
          instance = filter_instance
        )

        cat("Module created, instance stored with id:", current_id, "\n")  # Debug print

        modules(current_modules)
        filter_states(current_states)
      }

      updateSelectInput(session, "add_filter", selected = "")
    })


    # Update filter states when values change
    observe({
      cat("\nUpdating filter states\n")  # Debug print
      mods <- modules()
      current_states <- filter_states()
      for (mod_name in names(mods)) {
        mod <- mods[[mod_name]]
        value <- mod$instance$value()
        if (!is.null(value)) {
          cat("Module", mod_name, "value:", paste(value, collapse=", "), "\n")  # Debug print
          current_states[[mod_name]] <- value
        }
      }
      filter_states(current_states)
    })

    # Handle module removal
    observe({
      cat("\nChecking for modules to remove\n")  # Debug print
      mods <- modules()
      for (mod_name in names(mods)) {
        mod <- mods[[mod_name]]
        if (!is.null(mod$instance$remove()) && mod$instance$remove() > 0) {
          cat("Removing module:", mod_name, "\n")  # Debug print
          current_modules <- modules()
          current_states <- filter_states()

          # Remove from our tracking lists
          current_modules[[mod_name]] <- NULL
          current_states[[mod_name]] <- NULL

          # Remove the UI element
          removeUI(
            selector = paste0("#", mod_name)
          )

          modules(current_modules)
          filter_states(current_states)
        }
      }
    })



    observe({
      message("running namespace observer")
      mods <- modules()
      # Get all current namespaces
      current_namespaces <- names(mods)

      # Print to console for debugging
      cat("\nCurrent UI namespaces:\n")
      if(length(current_namespaces) > 0) {
        cat(paste0("- ", current_namespaces, collapse ="\n"))
      } else {
        cat("No active namespaces\n")
      }
    })

    # Build SQL query
    build_query <- reactive({
      message("building query!!")
      req(input$table_select)
      mods <- modules()

      base_query <- sprintf("SELECT * FROM [%s]", input$table_select)
      where_clauses <- c()

      for (mod in mods) {
        col_name <- mod$instance$column
        filter_value <- mod$instance$value()

        if (mod$instance$type == "numeric") {
          where_clauses <- c(
            where_clauses,
            sprintf("[%s] >= %f AND [%s] <= %f",
                    col_name, filter_value[1],
                    col_name, filter_value[2])
          )
        } else {
          values_str <- paste(
            sprintf("'%s'", filter_value),
            collapse = ","
          )
          where_clauses <- c(
            where_clauses,
            sprintf("[%s] IN (%s)", col_name, values_str)
          )
        }
      }

      if (length(where_clauses) > 0) {
        paste(base_query, "WHERE", paste(where_clauses, collapse = " AND "))
      } else {
        base_query
      }
    })

    # Get filtered dataset
    filtered_data <- eventReactive(input$fetch_data, {
      req(input$table_select)
      withProgress(message = 'Fetching data...', {
        incProgress(0.3, detail = "Executing query")
        query <- isolate(build_query())
        data <- DBI::dbGetQuery(pool, query)
        incProgress(0.7, detail = "Processing results")
        return(data)
      })
    }, ignoreNULL = FALSE)

    # Update last fetched state
    observeEvent(input$fetch_data, {
      mods <- modules()
      fetched_state <- lapply(mods, function(mod) {
        list(
          value = mod$instance$value(),
          column = mod$instance$column,
          type = mod$instance$type
        )
      })
      last_fetched_filters(fetched_state)
      last_fetched_count(nrow(filtered_data()))
      fetch_count(fetch_count() + 1)
    })

    # Render outputs
    output$filters <- renderUI({
      req(input$table_select)
      mods <- modules()
      states <- filter_states()
      col_info <- column_info()

      filter_list <- lapply(names(mods), function(id) {
        col_name <- mods[[id]]$instance$column
        # Add the parent namespace to the id
        full_id <- ns(id)  # This creates the 'filters-' prefix
        filterModuleUI(full_id, col_info[[col_name]], states[[id]])
      })
      do.call(tagList, filter_list)
    })

    output$filter_summary <- renderPrint({
      req(input$table_select)
      mods <- modules()

      cat("Current filters:\n")
      if (length(mods) > 0) {
        for (mod in mods) {
          cat(mod$instance$column, ": ")
          if (mod$instance$type == "numeric") {
            value <- mod$instance$value()
            cat("Range:", value[1], "to", value[2],"\n")
          } else {
            cat("Selected values:", paste(mod$instance$value(), collapse = ", "),"\n")
          }
        }
      } else {
        cat("No active filters\n")
      }

      fetched_filters <- last_fetched_filters()

      cat("\n=== Last fetched state ===\n")
      if (length(fetched_filters) > 0) {
        for (filter in fetched_filters) {
          cat(filter$column, ": ")
          if (filter$type == "numeric") {
            cat("Range:", filter$value[1], "to", filter$value[2],"\n")
          } else {
            cat("Selected values:", paste(filter$value, collapse = ", "),"\n")
          }
        }
      } else {
        cat("No active filters\n")
      }

      if (!is.null(last_fetched_count())) {
        cat("\nNumber of rows: ", last_fetched_count())
      } else if (fetch_count() == 0) {
        cat("\nNo data fetched yet")
      }
    })

    output$filtered_data <- renderTable({
      req(input$table_select)
      if (fetch_count() == 0) {
        return(data.frame(Message = "Press 'Fetch Data' to load results"))
      }

      data <- head(filtered_data(), 10)
      if("measurement_date" %in% names(data)) {
        data$measurement_date <- format(as.Date(data$measurement_date), "%Y-%m-%d")
      }
      data
    })

    # Return reactive expressions that might be useful to the parent module
    return(list(
      filtered_data = filtered_data,
      current_filters = reactive(modules()),
      selected_table_name = reactive(input$table_select)
    ))
  })
}

# Example usage:
if (FALSE) {
  library(shiny)
  library(pool)

  # Create a connection pool
  pool <- dbPool(
    drv = RSQLite::SQLite(),
    dbname = "iris.db"
  )

  # Define UI
  ui <- fluidPage(
    filterBuilderUI("filters", c("iris", "mtcars"))
  )

  # Define server
  server <- function(input, output, session) {
    filter_results <- filterBuilderServer("filters", pool, c("iris", "mtcars"))

    # You can access the filtered data using:
    observe({
      data <- filter_results$filtered_data()
      # Do something with the filtered data
    })
  }

  # Run the app
  shinyApp(ui, server)
}

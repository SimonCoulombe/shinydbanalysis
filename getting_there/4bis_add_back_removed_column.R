

library(shiny)
library(RSQLite)
library(DBI)

# Module UI function
filterModuleUI <- function(id, column_info, initial_value = NULL) {
  ns <- NS(id)

  if(is.null(initial_value)) {
    initial_value <- if(column_info$type == "numeric") {
      c(column_info$values$min, column_info$values$max)
    } else {
      column_info$values
    }
  }

  # Create different UI based on column type
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
}

# Module server function
# Replace the existing filterModule function with this version
filterModule <- function(id, column_info, initial_value = NULL) {
  moduleServer(id, function(input, output, session) {
    # Initialize filter state
    if(is.null(initial_value)) {
      initial_value <- if(column_info$type == "numeric") {
        c(column_info$values$min, column_info$values$max)
      } else {
        column_info$values
      }
    }

    filter_state <- reactiveVal(initial_value)

    # Update filter state when input changes
    observeEvent(input$filter_value, {
      filter_state(input$filter_value)
    })

    # Return filter state and removal status
    list(
      value = filter_state,
      remove = reactive(input$remove),  # Simply return the remove button value
      column = column_info$name,
      type = column_info$type
    )
  })
}

# Main UI
ui <- fluidPage(
  titlePanel("Dataset Filter Builder"),

  sidebarLayout(
    sidebarPanel(
      selectInput("add_filter", "Add filter for column:",
                  choices = c("Select column" = "")),
      br(),
      uiOutput("filters"),
      width = 6
    ),

    mainPanel(
      h3("Current Filters:"),
      verbatimTextOutput("filter_summary"),
      h3("Filtered Data Preview:"),
      tableOutput("filtered_data"),
      verbatimTextOutput("namespace_debug"),
      width = 6

    )
  )
)

# Main server
server <- function(input, output, session) {
  # Initialize database connection
  con <- dbConnect(RSQLite::SQLite(), "iris.db")

  # Check if table exists, if not create it
  if (!dbExistsTable(con, "iris")) {
    dbWriteTable(con, "iris", iris)
  }

  # Ensure connection is closed when session ends
  onStop(function() {
    dbDisconnect(con)
  })

  # Get column information
  column_info <- reactive({
    # Get column names
    cols <- dbListFields(con, "iris")

    # Create column info list
    col_info <- lapply(cols, function(col) {
      if(col == "Species") {
        # For known categorical column
        values <- dbGetQuery(con, paste0("SELECT DISTINCT [", col, "] FROM iris ORDER BY [", col, "]"))[[1]]
        list(
          name = col,
          type = "categorical",
          values = values
        )
      } else {
        # For numeric columns
        range_query <- paste0("SELECT MIN([", col, "]) as min, MAX([", col, "]) as max FROM iris")
        range_values <- dbGetQuery(con, range_query)
        list(
          name = col,
          type = "numeric",
          values = list(min = range_values$min, max = range_values$max)
        )
      }
    })
    names(col_info) <- cols
    col_info
  })

  # Store module instances and their filter states
  modules <- reactiveVal(list())
  filter_states <- reactiveVal(list())

  # Update column choices when column info is available
  observe({
    col_info <- column_info()
    # Get current modules to check which columns are already filtered
    current_modules <- modules()

    # Extract just the column names (without timestamps) from active filters
    active_columns <- sapply(current_modules, function(mod) {
      mod$instance$column
    })

    # Only show columns that aren't currently being filtered
    available_columns <- setdiff(names(col_info), active_columns)

    updateSelectInput(session, "add_filter",
                      choices = c("Select column" = "", available_columns))
  })

  # Add new filter
  # Modify the add filter observeEvent in the server function
  observeEvent(input$add_filter, {
    req(input$add_filter != "")

    # Generate a unique ID with timestamp to force a fresh instance
    current_id <- paste0("filter_", input$add_filter, "_", format(Sys.time(), "%H%M%S"))

    # Only add if this column isn't already being filtered
    current_modules <- modules()
    current_states <- filter_states()
    col_info <- column_info()

    # Check if column is already filtered (using the column name, not the full ID)
    column_name <- input$add_filter
    existing_filter <- FALSE
    for(mod in current_modules) {
      if(mod$instance$column == column_name) {
        existing_filter <- TRUE
        break
      }
    }

    if (!existing_filter) {
      # Initialize filter state
      current_states[[current_id]] <- if(col_info[[column_name]]$type == "numeric") {
        c(col_info[[column_name]]$values$min,
          col_info[[column_name]]$values$max)
      } else {
        col_info[[column_name]]$values
      }

      # Create new module instance with current state
      current_modules[[current_id]] <- list(
        id = current_id,
        instance = filterModule(current_id,
                                col_info[[column_name]],
                                current_states[[current_id]])
      )

      modules(current_modules)
      filter_states(current_states)
    }

    # Reset the select input
    updateSelectInput(session, "add_filter", selected = "")
  })

  # Update filter states when values change
  observe({
    mods <- modules()
    current_states <- filter_states()

    for (mod in mods) {
      if (!is.null(mod$instance$value())) {
        current_states[[mod$id]] <- mod$instance$value()
      }
    }

    filter_states(current_states)
  })

  # Handle module removal
  observe({
    mods <- modules()
    for (mod in mods) {
      if (!is.null(mod$instance$remove()) && mod$instance$remove() > 0) {
        current_modules <- modules()
        current_states <- filter_states()

        module_id_to_remove <- mod$id

        # Remove the UI element
        removeUI(
          selector = paste0("#", module_id_to_remove)
        )

        # Remove from our tracking lists
        current_modules[[module_id_to_remove]] <- NULL
        current_states[[module_id_to_remove]] <- NULL

        modules(current_modules)
        filter_states(current_states)
      }
    }
  })

  # Add this after the existing module removal observer
  observe({
    mods <- modules()
    # Get all current namespaces
    current_namespaces <- names(mods)

    # Print to console for debugging
    cat("\nCurrent UI namespaces:\n")
    if(length(current_namespaces) > 0) {
      cat(paste("- ", current_namespaces, "\n", sep=""))
    } else {
      cat("No active namespaces\n")
    }

    # Also show in UI by adding a verbatimTextOutput
    output$namespace_debug <- renderPrint({
      cat("Active UI namespaces:\n")
      if(length(current_namespaces) > 0) {
        cat(paste("- ", current_namespaces, "\n", sep=""))
      } else {
        cat("No active namespaces\n")
      }
    })
  })

  # Render filters
  output$filters <- renderUI({
    mods <- modules()
    states <- filter_states()
    col_info <- column_info()

    filter_list <- lapply(names(mods), function(id) {
      col_name <- mods[[id]]$instance$column
      filterModuleUI(id, col_info[[col_name]], states[[id]])
    })
    do.call(tagList, filter_list)
  })

  # Build SQL query based on filters
  build_query <- reactive({
    mods <- modules()

    base_query <- "SELECT * FROM iris"
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
  filtered_data <- reactive({
    query <- build_query()
    dbGetQuery(con, query)
  })

  # Display filter summary
  output$filter_summary <- renderPrint({
    mods <- modules()
    if (length(mods) > 0) {
      cat("Active filters:\n")
      for (mod in mods) {
        cat("\n", mod$instance$column, ":\n")
        if (mod$instance$type == "numeric") {
          value <- mod$instance$value()
          cat("Range:", value[1], "to", value[2])
        } else {
          cat("Selected values:", paste(mod$instance$value(), collapse = ", "))
        }
      }
      cat("\n\nNumber of rows after filtering:", nrow(filtered_data()))
    } else {
      total_rows <- dbGetQuery(con, "SELECT COUNT(*) as count FROM iris")$count
      cat("No active filters\n")
      cat("Total rows:", total_rows)
    }
  })

  # Display filtered data preview
  output$filtered_data <- renderTable({
    head(filtered_data(), 10)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

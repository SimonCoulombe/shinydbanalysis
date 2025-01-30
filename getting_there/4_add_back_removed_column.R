library(shiny)
library(RSQLite)
library(DBI)

# Module UI function
filterModuleUI <- function(id, column_info, initial_value = NULL) {
  ns <- NS(id)

  if(is.null(initial_value)) {
    initial_value <- if(column_info$type == "numeric") {
      c(column_info$values$min, column_info$values$max)
    } else if(column_info$type == "date") {
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
  } else if(column_info$type == "date") {
    filter_input <- dateRangeInput(
      inputId = ns("filter_value"),
      label = NULL,
      start = initial_value[1],
      end = initial_value[2],
      min = column_info$values$min,
      max = column_info$values$max
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
filterModule <- function(id, column_info, initial_value = NULL) {
  moduleServer(id, function(input, output, session) {
    # Initialize filter state
    if(is.null(initial_value)) {
      initial_value <- if(column_info$type == "numeric") {
        c(column_info$values$min, column_info$values$max)
      } else if(column_info$type == "date") {
        c(column_info$values$min, column_info$values$max)
      } else {
        column_info$values
      }
    }

    filter_state <- reactiveVal(initial_value)
    remove_trigger <- reactiveVal(0)

    # Update filter state when input changes
    observeEvent(input$filter_value, {
      filter_state(input$filter_value)
    })

    # Handle remove button with ignoreInit
    observeEvent(input$remove, {
      remove_trigger(input$remove)
    }, ignoreInit = TRUE)

    # Return filter state and removal status
    list(
      value = filter_state,
      remove = remove_trigger,
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
      width = 6
    )
  )
)

# Main server
server <- function(input, output, session) {
  # Initialize database connection
  con <- dbConnect(RSQLite::SQLite(), "iris.db")

  # Ensure connection is closed when session ends
  onStop(function() {
    dbDisconnect(con)
  })

  # Track active columns (those currently being filtered)
  active_columns <- reactiveVal(character(0))

  # Get column information
  column_info <- reactive({
    # Get column names
    cols <- dbListFields(con, "iris")

    # Create column info list
    col_info <- lapply(cols, function(col) {
      if(col == "Species") {
        # For known categorical column
        values <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT [", col, "] FROM iris ORDER BY [", col, "]"))[[1]]
        list(
          name = col,
          type = "categorical",
          values = values
        )
      } else if(col == "measurement_date") {
        # For date column
        range_query <- paste0("SELECT MIN([", col, "]) as min, MAX([", col, "]) as max FROM iris")
        range_values <- DBI::dbGetQuery(con, range_query)
        list(
          name = col,
          type = "date",
          values = list(
            min = as.Date(range_values$min, format="%Y-%m-%d"),
            max = as.Date(range_values$max, format="%Y-%m-%d")
          )
        )
      } else {
        # For numeric columns
        range_query <- paste0("SELECT MIN([", col, "]) as min, MAX([", col, "]) as max FROM iris")
        range_values <- DBI::dbGetQuery(con, range_query)
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
    current_active <- active_columns()
    available_cols <- setdiff(names(col_info), current_active)

    updateSelectInput(session, "add_filter",
                      choices = c("Select column" = "", available_cols))
  })

  # Add new filter
  observeEvent(input$add_filter, {
    req(input$add_filter != "")

    # Generate unique ID
    current_id <- paste0("filter_", input$add_filter)

    cat("=== ADD OBSERVER ===\n")
    cat("Trying to add filter for:", input$add_filter, "\n")
    cat("Current ID:", current_id, "\n")
    cat("Current modules:", paste(names(modules()), collapse=", "), "\n")

    # Get current modules and states
    current_modules <- modules()
    current_states <- filter_states()
    col_info <- column_info()

    # Only proceed if this column isn't already being filtered
    if (!(current_id %in% names(current_modules))) {
      cat("Module doesn't exist, creating...\n")
      # Update active columns
      current_active <- active_columns()
      active_columns(c(current_active, input$add_filter))

      # Initialize filter state
      current_states[[current_id]] <- if(col_info[[input$add_filter]]$type == "numeric") {
        c(col_info[[input$add_filter]]$values$min,
          col_info[[input$add_filter]]$values$max)
      } else if(col_info[[input$add_filter]]$type == "date") {
        c(col_info[[input$add_filter]]$values$min,
          col_info[[input$add_filter]]$values$max)
      } else {
        col_info[[input$add_filter]]$values
      }

      # Create new module instance with current state
      current_modules[[current_id]] <- list(
        id = current_id,
        instance = filterModule(current_id,
                                col_info[[input$add_filter]],
                                current_states[[current_id]])
      )

      modules(current_modules)
      filter_states(current_states)
      cat("Module created successfully\n")
      cat("New modules list:", paste(names(modules()), collapse=", "), "\n")
    }

    # Reset the select input
    updateSelectInput(session, "add_filter", selected = "")
  })

  # Handle module removal
  observe({
    mods <- modules()
    cat("=== REMOVE OBSERVER ===\n")
    cat("Current modules in remove observer:", paste(names(mods), collapse=", "), "\n")

    for (mod in mods) {
      if (!is.null(mod$instance$remove())) {
        cat("Checking remove button for module:", mod$id, "\n")
        cat("Remove button value:", mod$instance$remove(), "\n")

        if (mod$instance$remove() > 0) {
          cat("Removing module:", mod$id, "\n")
          # Get the column name from the module ID
          col_name <- mod$instance$column

          # Update active columns
          current_active <- active_columns()
          active_columns(setdiff(current_active, col_name))

          # Update modules and states
          current_modules <- modules()
          current_states <- filter_states()

          current_modules[[mod$id]] <- NULL
          current_states[[mod$id]] <- NULL

          modules(current_modules)
          filter_states(current_states)

          cat("After removal, modules:", paste(names(modules()), collapse=", "), "\n")
        }
      }
    }
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
      } else if(mod$instance$type == "date") {
        where_clauses <- c(
          where_clauses,
          sprintf("[%s] >= date('%s') AND [%s] <= date('%s')",
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
    DBI::dbGetQuery(con, query)
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
        } else if(mod$instance$type == "date") {
          value <- mod$instance$value()
          cat("Range:", format(value[1], "%Y-%m-%d"), "to", format(value[2], "%Y-%m-%d"))
        } else {
          cat("Selected values:", paste(mod$instance$value(), collapse = ", "))
        }
      }
      cat("\n\nNumber of rows after filtering:", nrow(filtered_data()))
    } else {
      total_rows <- DBI::dbGetQuery(con, "SELECT COUNT(*) as count FROM iris")$count
      cat("No active filters\n")
      cat("Total rows:", total_rows)
    }
  })

  # Display filtered data preview
  output$filtered_data <- renderTable({
    data <- head(filtered_data(), 10)
    # Format date column
    if("measurement_date" %in% names(data)) {
      data$measurement_date <- format(as.Date(data$measurement_date), "%Y-%m-%d")
    }
    data
  })
}

# Run the app
shinyApp(ui = ui, server = server)

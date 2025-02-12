generate_column_info <- FALSE
if(generate_column_info){
  library(RSQLite)
  library(DBI)

  # Initialize database connection
  con <- dbConnect(RSQLite::SQLite(), "iris.db")


  # Function to determine if a column should be categorical
  is_categorical <- function(values, n_distinct, n_total) {
    # Remove NAs
    values <- values[!is.na(values)]
    if(length(values) == 0) return(FALSE)

    # Rules for categorical:
    # 1. If all values are character/factor
    # 2. If numeric but few distinct values compared to total rows
    # (less than 10% of total rows and less than 20 distinct values)
    if(is.character(values) || is.factor(values)) {
      return(TRUE)
    } else if(is.numeric(values)) {
      return(n_distinct <= 20 && n_distinct <= 0.1 * n_total)
    }
    return(FALSE)
  }

  # Get column names
  cols <- dbListFields(con, "iris")

  # Get total number of rows
  n_total <- DBI::dbGetQuery(con, "SELECT COUNT(*) as count FROM iris")$count

  # Create column info list
  col_info <- lapply(cols, function(col) {
    # Get sample of values and count distinct values
    values_query <- sprintf("SELECT [%s], COUNT(DISTINCT [%s]) as n_distinct
                          FROM iris
                          WHERE [%s] IS NOT NULL
                          GROUP BY [%s]
                          LIMIT 1000", col, col, col, col)
    sample_data <- DBI::dbGetQuery(con, values_query)
    n_distinct <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(DISTINCT [%s]) as n FROM iris", col))$n

    # Get the first column (the values)
    values <- sample_data[[1]]

    # Determine type
    if(is_categorical(values, n_distinct, n_total)) {
      # Categorical type
      distinct_query <- sprintf("SELECT DISTINCT [%s] FROM iris WHERE [%s] IS NOT NULL ORDER BY [%s]",
                                col, col, col)
      distinct_values <- DBI::dbGetQuery(con, distinct_query)[[1]]
      list(
        name = col,
        type = "categorical",
        values = distinct_values
      )
    } else {
      # Numeric type (default)
      range_query <- sprintf("SELECT MIN([%s]) as min, MAX([%s]) as max FROM iris", col, col)
      range_values <- DBI::dbGetQuery(con, range_query)
      list(
        name = col,
        type = "numeric",
        values = list(
          min = as.numeric(range_values$min),
          max = as.numeric(range_values$max)
        )
      )
    }
  })
  names(col_info) <- cols

  # Print detected types
  cat("Detected column types:\n")
  for(col in names(col_info)) {
    cat(sprintf("%s: %s\n", col, col_info[[col]]$type))
  }

  # Disconnect from database
  dbDisconnect(con)

  # Save to file
  saveRDS(col_info, "column_info.rds")
}

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
      br(),
      actionButton("fetch_data", "Fetch Data",
                   class = "btn-primary"),
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


  # Store the last fetched state
  last_fetched_filters <- reactiveVal(list())
  last_fetched_count <- reactiveVal(NULL)
  # Update last fetched state when fetch button is pressed
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
  })


  # Counter for data fetches
  fetch_count <- reactiveVal(0)

  observeEvent(input$fetch_data, {
    fetch_count(fetch_count() + 1)
  })

  # Check if table exists, if not create it
  if (!dbExistsTable(con, "iris")) {
    dbWriteTable(con, "iris", iris)
  }

  # Ensure connection is closed when session ends
  onStop(function() {
    dbDisconnect(con)
  })

  # Get column information
  # Load pre-processed column info
  column_info <- reactive({
    col_info <- readRDS("column_info.rds")
    validate(
      need(file.exists("column_info.rds"), "Column info file not found. Run prepare_column_info.R first.")
    )
    return(col_info)
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
      cat(paste0("- ", current_namespaces, collapse ="\n"))
    } else {
      cat("No active namespaces\n")
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
  query_to_run <- reactive({
    build_query()
  })

  filtered_data <- eventReactive(input$fetch_data, {
    withProgress(message = 'Fetching data...', {
      incProgress(0.3, detail = "Executing query")
      query <- isolate(query_to_run())
      data <- DBI::dbGetQuery(con, query)
      incProgress(0.7, detail = "Processing results")
      return(data)
    })
  }, ignoreNULL = FALSE)  # This makes it run once at startup

  # Display filter summary
  output$filter_summary <- renderPrint({
    mods <- modules()

    # Show current filters

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

    # Show last fetched state if it exists
    fetched_filters <- last_fetched_filters()

      cat("=== Last fetched state ===\n")
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

      # Show row count if available
      if (!is.null(last_fetched_count())) {
        cat("Number of rows: ", last_fetched_count())
      }
     else if (fetch_count() == 0) {
      cat("No data fetched yet")
    }
  })

  # Display filtered data preview
  output$filtered_data <- renderTable({
    if (fetch_count() == 0) {
      return(data.frame(Message = "Press 'Fetch Data' to load results"))
    }

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

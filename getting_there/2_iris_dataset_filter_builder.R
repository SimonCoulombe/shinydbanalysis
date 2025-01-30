library(shiny)

# Get iris column information
DATASET <- iris
COLUMN_INFO <- lapply(names(DATASET), function(col) {
  list(
    name = col,
    type = if(is.numeric(DATASET[[col]])) "numeric" else "categorical",
    values = if(is.numeric(DATASET[[col]])) {
      list(min = min(DATASET[[col]]), max = max(DATASET[[col]]))
    } else {
      sort(unique(DATASET[[col]]))
    }
  )
})
names(COLUMN_INFO) <- names(DATASET)

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
      remove = reactive(input$remove),
      column = column_info$name,
      type = column_info$type
    )
  })
}

# Main UI
ui <- fluidPage(
  titlePanel("Iris Dataset Filter Builder"),

  sidebarLayout(
    sidebarPanel(
      selectInput("add_filter", "Add filter for column:",
                  choices = c("Select column" = "", names(DATASET))),
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
  # Store module instances and their filter states
  modules <- reactiveVal(list())
  filter_states <- reactiveVal(list())

  # Add new filter
  observeEvent(input$add_filter, {
    req(input$add_filter != "")

    current_id <- paste0("filter_", input$add_filter)

    # Only add if this column isn't already being filtered
    current_modules <- modules()
    current_states <- filter_states()

    if (!current_id %in% names(current_modules)) {
      # Initialize filter state
      current_states[[current_id]] <- if(COLUMN_INFO[[input$add_filter]]$type == "numeric") {
        c(COLUMN_INFO[[input$add_filter]]$values$min,
          COLUMN_INFO[[input$add_filter]]$values$max)
      } else {
        COLUMN_INFO[[input$add_filter]]$values
      }

      # Create new module instance with current state
      current_modules[[current_id]] <- list(
        id = current_id,
        instance = filterModule(current_id,
                                COLUMN_INFO[[input$add_filter]],
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
      if (!is.null(mod$instance$remove())) {
        if (mod$instance$remove() > 0) {
          current_modules <- modules()
          current_states <- filter_states()

          current_modules[[mod$id]] <- NULL
          current_states[[mod$id]] <- NULL

          modules(current_modules)
          filter_states(current_states)
        }
      }
    }
  })

  # Render filters
  output$filters <- renderUI({
    mods <- modules()
    states <- filter_states()

    filter_list <- lapply(names(mods), function(id) {
      col_name <- mods[[id]]$instance$column
      filterModuleUI(id, COLUMN_INFO[[col_name]], states[[id]])
    })
    do.call(tagList, filter_list)
  })

  # Get filtered dataset
  filtered_data <- reactive({
    mods <- modules()
    result <- DATASET

    for (mod in mods) {
      col_name <- mod$instance$column
      filter_value <- mod$instance$value()

      if (mod$instance$type == "numeric") {
        result <- result[result[[col_name]] >= filter_value[1] &
                           result[[col_name]] <= filter_value[2], ]
      } else {
        result <- result[result[[col_name]] %in% filter_value, ]
      }
    }

    result
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
      cat("No active filters\n")
      cat("Total rows:", nrow(DATASET))
    }
  })

  # Display filtered data preview
  output$filtered_data <- renderTable({
    head(filtered_data(), 10)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

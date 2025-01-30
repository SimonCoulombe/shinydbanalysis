library(shiny)

# Available colors - moved to global scope so it's accessible everywhere
AVAILABLE_COLORS <- c("Crimson Red", "Ocean Blue", "Forest Green",
                      "Royal Purple", "Sunset Orange", "Golden Yellow",
                      "Midnight Black", "Pearl White")

# Module UI function
colorPickerModuleUI <- function(id, selected = AVAILABLE_COLORS[1]) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; align-items: center; margin-bottom: 10px;",
      div(
        style = "flex-grow: 1;",
        selectInput(
          inputId = ns("color_select"),
          label = paste("Color", id),
          choices = AVAILABLE_COLORS,
          selected = selected
        )
      ),
      div(
        style = "margin-left: 10px; margin-top: 25px;",
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
colorPickerModule <- function(id, selected_color) {
  moduleServer(id, function(input, output, session) {
    # Initialize color with the passed value
    color_value <- reactiveVal(selected_color)

    # Update color value when selection changes
    observeEvent(input$color_select, {
      color_value(input$color_select)
    })

    # Return the selected color and removal status
    list(
      color = color_value,
      remove = reactive(input$remove)
    )
  })
}

# Main UI
ui <- fluidPage(
  titlePanel("Dynamic Color Picker"),

  sidebarLayout(
    sidebarPanel(
      actionButton("add_color", "Add Color"),
      br(), br(),
      uiOutput("color_pickers")
    ),

    mainPanel(
      h3("Selected Colors:"),
      verbatimTextOutput("selected_colors")
    )
  )
)

# Main server
server <- function(input, output, session) {
  # Store module instances and their selected colors
  modules <- reactiveVal(list())
  color_states <- reactiveVal(list())
  next_id <- reactiveVal(1)

  # Add new color picker
  observeEvent(input$add_color, {
    current_id <- paste0("picker_", next_id())

    # Add new module to the list
    current_modules <- modules()
    current_states <- color_states()

    # Initialize with default color
    current_states[[current_id]] <- AVAILABLE_COLORS[1]

    # Create new module instance with current state
    current_modules[[current_id]] <- list(
      id = current_id,
      instance = colorPickerModule(current_id, current_states[[current_id]])
    )

    modules(current_modules)
    color_states(current_states)
    next_id(next_id() + 1)
  })

  # Update color states when selections change
  observe({
    mods <- modules()
    current_states <- color_states()

    for (mod in mods) {
      if (!is.null(mod$instance$color())) {
        current_states[[mod$id]] <- mod$instance$color()
      }
    }

    color_states(current_states)
  })

  # Handle module removal
  observe({
    mods <- modules()
    for (mod in mods) {
      if (!is.null(mod$instance$remove())) {
        if (mod$instance$remove() > 0) {
          # Remove this module from both lists
          current_modules <- modules()
          current_states <- color_states()

          current_modules[[mod$id]] <- NULL
          current_states[[mod$id]] <- NULL

          modules(current_modules)
          color_states(current_states)
        }
      }
    }
  })

  # Render color pickers
  output$color_pickers <- renderUI({
    mods <- modules()
    states <- color_states()

    picker_list <- lapply(names(mods), function(id) {
      colorPickerModuleUI(id, selected = states[[id]])
    })
    do.call(tagList, picker_list)
  })

  # Display selected colors
  output$selected_colors <- renderPrint({
    states <- color_states()
    if (length(states) > 0) {
      cat("Currently selected colors:\n")
      for (id in names(states)) {
        cat(sprintf("%s. %s\n",
                    gsub("picker_", "", id),
                    states[[id]])
        )
      }
    } else {
      cat("No colors selected")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

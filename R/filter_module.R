#' Individual Filter Module with Improved Layout
#' @importFrom shiny NS tagList div h4 sliderInput checkboxGroupInput actionButton
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent removeUI
NULL

#' Create filter module UI
#'
#' @param id Character. The module ID
#' @param column_info List. Column information containing name, type, and possible values
#' @param initial_value Vector. Initial filter value(s)
#' @return A Shiny UI element
#' @export
filter_module_ui <- function(id, column_info, initial_value = NULL) {
  ns <- NS(id)

  # Add validation for column_info
  if (is.null(column_info) || !is.list(column_info)) {
    return(NULL)  # Return NULL instead of erroring
  }

  validate_column_info(column_info)
  initial_value <- initial_value %||% get_default_value(column_info)

  filter_input <- create_filter_input(ns, column_info, initial_value)
  create_filter_container(ns, column_info$name, filter_input)
}



#' Create filter module server
#'
#' @param id Character. The module ID
#' @param column_info List. Column information
#' @param initial_value Vector. Initial filter value(s)
#' @return List of reactive expressions
#' @export
filter_module_server <- function(id, column_info, initial_value = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    validate_column_info(column_info)
    initial_value <- initial_value %||% get_default_value(column_info)

    # Reactive values
    filter_state <- reactiveVal(initial_value)
    is_active <- reactiveVal(FALSE)

    # Update filter state when input changes
    observeEvent(input$filter_value, {
      if (!identical(filter_state(), input$filter_value)) {
        is_active(TRUE)
        filter_state(input$filter_value)
      }
    })

    # Return reactive values and metadata
    list(
      value = filter_state,
      remove = reactive(input$remove),
      column = column_info$name,
      type = column_info$type,
      is_active = is_active
    )
  })
}



#' Create filter container with improved layout
#' @noRd
create_filter_container <- function(ns, name, filter_input) {
  tagList(
    div(
      id = ns("container"),
      class = "filter-container",
      style = "margin-bottom: 15px;",  # Add some spacing between filters
      div(
        class = "filter-content",
        style = "display: flex; align-items: center; gap: 10px;",  # Flexbox layout
        div(
          class = "filter-main",
          style = "flex-grow: 1;",  # Take up remaining space
          h4(
            name,
            class = "filter-title",
            style = "margin-top: 0; margin-bottom: 5px;"
          ),
          div(
            style = "display: flex; align-items: center; gap: 10px;",
            div(
              style = "flex-grow: 1;",
              filter_input
            )
          )
        ),
        div(
          class = "filter-actions",
          style = "padding-top: 20px;",  # Align with input
          actionButton(
            inputId = ns("remove"),
            label = "×",
            class = "btn-danger remove-filter",
            style = "padding: 2px 6px;"
          )
        )
      )
    )
  )
}

#' Create appropriate filter input based on column type
#' @noRd
create_filter_input <- function(ns, column_info, initial_value) {
  if (column_info$type == "numeric") {
    create_numeric_input(ns, column_info, initial_value)
  } else {
    create_categorical_input(ns, column_info, initial_value)
  }
}

#' Create numeric slider input
#' @noRd
create_numeric_input <- function(ns, column_info, initial_value) {
  sliderInput(
    inputId = ns("filter_value"),
    label = NULL,
    min = column_info$values$min,
    max = column_info$values$max,
    value = initial_value,
    step = (column_info$values$max - column_info$values$min) / 100,
    width = "100%"  # Make slider take full width
  )
}

#' Create categorical checkbox input
#' @noRd
create_categorical_input <- function(ns, column_info, initial_value) {
  checkboxGroupInput(
    inputId = ns("filter_value"),
    label = NULL,
    choices = column_info$values,
    selected = initial_value,
    width = "100%"  # Make checkboxes take full width
  )
}
# Helper Functions ----

#' Get default value for filter
#' @noRd
get_default_value <- function(column_info) {
  if (column_info$type == "numeric") {
    c(column_info$values$min, column_info$values$max)
  } else {
    column_info$values
  }
}


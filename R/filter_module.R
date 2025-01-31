#' Individual Filter Module with Improved Layout and Date Support
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
    
    # Helper function to compare values accounting for different types
    values_differ <- function(old_val, new_val) {
      if (column_info$type == "date") {
        # For dates, compare each element separately
        !identical(as.character(old_val), as.character(new_val))
      } else {
        # For other types, use standard comparison
        !identical(old_val, new_val)
      }
    }
    
    # Update filter state when input changes
    observeEvent(input$filter_value, {
      current_state <- filter_state()
      
      # Handle different input types
      if (column_info$type == "date") {
        # Ensure input is a valid date range
        if (length(input$filter_value) == 2 && 
            !is.na(input$filter_value[1]) && 
            !is.na(input$filter_value[2])) {
          if (values_differ(current_state, input$filter_value)) {
            is_active(TRUE)
            filter_state(input$filter_value)
          }
        }
      } else {
        # Existing logic for other types
        if (values_differ(current_state, input$filter_value)) {
          is_active(TRUE)
          filter_state(input$filter_value)
        }
      }
    }, ignoreNULL = FALSE)
    
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
  switch(column_info$type,
         "numeric" = create_numeric_input(ns, column_info, initial_value),
         "categorical" = create_categorical_input(ns, column_info, initial_value),
         "date" = create_date_input(ns, column_info, initial_value),
         stop(paste("Unsupported filter type:", column_info$type))
  )
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

#' Create date range input
#' @noRd
create_date_input <- function(ns, column_info, initial_value) {
  dateRangeInput(
    inputId = ns("filter_value"),
    label = NULL,
    start = initial_value[1],  # Use first value from initial_value
    end = initial_value[2],    # Use second value from initial_value
    min = column_info$values$min,
    max = column_info$values$max,
    startview = "month",
    width = "100%"  # Make date range input take full width
  )
}

# Helper Functions ----

#' Get default value for filter
#' @noRd
get_default_value <- function(column_info) {
  switch(column_info$type,
         "numeric" = c(column_info$values$min, column_info$values$max),
         "categorical" = column_info$values,
         "date" = c(column_info$values$min, column_info$values$max),
         stop(paste("Unsupported filter type:", column_info$type))
  )
}

# Validation Function (ensure you define this or modify as needed)
validate_column_info <- function(column_info) {
  # Validate column_info structure
  if (is.null(column_info$name) || 
      is.null(column_info$type) || 
      is.null(column_info$values)) {
    stop("column_info must contain 'name', 'type', and 'values'")
  }
  
  # Validate supported types
  supported_types <- c("numeric", "categorical", "date")
  if (!(column_info$type %in% supported_types)) {
    stop(paste("Unsupported filter type:", column_info$type, 
               ". Supported types are:", paste(supported_types, collapse = ", ")))
  }
  
  # Type-specific validations
  switch(column_info$type,
         "numeric" = {
           if (!all(c("min", "max") %in% names(column_info$values))) {
             stop("Numeric column_info must have 'min' and 'max' values")
           }
         },
         "categorical" = {
           if (length(column_info$values) == 0) {
             stop("Categorical column_info must have at least one value")
           }
         },
         "date" = {
           if (!all(c("min", "max") %in% names(column_info$values)) ||
               !inherits(column_info$values$min, "Date") ||
               !inherits(column_info$values$max, "Date")) {
             stop("Date column_info must have 'min' and 'max' Date values")
           }
         }
  )
}
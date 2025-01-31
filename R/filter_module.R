#' Individual Filter Module with Enhanced UI Features
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
    return(NULL)
  }
  
  validate_column_info(column_info)
  
  # Modify default value behavior for categorical columns
  if (is.null(initial_value)) {
    initial_value <- get_default_value(column_info)
  }
  
  # Check for categorical columns with too many distinct values
  if (column_info$type == "categorical" && length(column_info$values) > 300) {
    return(div(
      class = "alert alert-warning",
      sprintf("Filter disabled for '%s': Too many distinct values (>300)", column_info$name)
    ))
  }
  
  filter_input <- create_filter_input(ns, column_info, initial_value)
  create_filter_container(ns, column_info$name, filter_input)
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
    if (is.null(initial_value)) {
      initial_value <- get_default_value(column_info)
    }
    
    # Reactive values
    filter_state <- reactiveVal(initial_value)
    is_active <- reactiveVal(FALSE)
    
    # Handle select all/deselect all for categorical inputs
    if (column_info$type == "categorical") {
      observeEvent(input$select_all, {
        if (length(column_info$values) > 8) {
          updateSelectizeInput(session, "filter_value", selected = column_info$values)
        } else {
          updateCheckboxGroupInput(session, "filter_value", selected = column_info$values)
        }
      })
      
      observeEvent(input$deselect_all, {
        if (length(column_info$values) > 8) {
          updateSelectizeInput(session, "filter_value", selected = character(0))
        } else {
          updateCheckboxGroupInput(session, "filter_value", selected = character(0))
        }
      })
    }
    
    # Helper function to compare values accounting for different types
    values_differ <- function(old_val, new_val) {
      if (column_info$type == "date") {
        !identical(as.character(old_val), as.character(new_val))
      } else {
        !identical(old_val, new_val)
      }
    }
    
    # Update filter state when input changes
    observeEvent(input$filter_value, {
      current_state <- filter_state()
      
      if (column_info$type == "date") {
        if (length(input$filter_value) == 2 && 
            !is.na(input$filter_value[1]) && 
            !is.na(input$filter_value[2])) {
          if (values_differ(current_state, input$filter_value)) {
            is_active(TRUE)
            filter_state(input$filter_value)
          }
        }
      } else {
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

#' Create categorical input with enhanced UI features
#' @noRd
create_categorical_input <- function(ns, column_info, initial_value) {
  n_values <- length(column_info$values)
  
  # For many values, start with none selected unless specified
  if (n_values > 8 && is.null(initial_value)) {
    initial_value <- character(0)
  }
  
  if (n_values > 8) {
    # Use selectize input for many values
    tagList(
      div(
        style = "margin-bottom: 10px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span(
            class = "text-muted",
            style = "font-size: 0.9em;",
            sprintf("%d available values", n_values)
          ),
          div(
            style = "display: flex; gap: 10px;",
            actionButton(
              inputId = ns("select_all"),
              label = "Select All",
              class = "btn-sm"
            ),
            actionButton(
              inputId = ns("deselect_all"),
              label = "Clear",
              class = "btn-sm"
            )
          )
        )
      ),
      selectizeInput(
        inputId = ns("filter_value"),
        label = NULL,
        choices = column_info$values,
        selected = initial_value,
        multiple = TRUE,
        options = list(
          plugins = list('remove_button'),
          placeholder = sprintf('Select values (up to %d)...', n_values),
          searchField = 'label',
          sortField = 'label',
          maxItems = n_values,
          maxOptions = n_values,
          hideSelected = FALSE,
          closeAfterSelect = TRUE
        ),
        width = "100%"
      )
    )
  } else {
    # Use checkbox group for few values
    tagList(
      div(
        style = "margin-bottom: 10px;",
        div(
          style = "display: flex; gap: 10px;",
          actionButton(
            inputId = ns("select_all"),
            label = "Select All",
            class = "btn-sm"
          ),
          actionButton(
            inputId = ns("deselect_all"),
            label = "Clear",
            class = "btn-sm"
          )
        )
      ),
      checkboxGroupInput(
        inputId = ns("filter_value"),
        label = NULL,
        choices = column_info$values,
        selected = initial_value,
        width = "100%"
      )
    )
  }
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
         "categorical" = if(length(column_info$values) <= 8) column_info$values else character(0),
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
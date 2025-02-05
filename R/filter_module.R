#' Create filter module UI components
#' @param id Character. The module ID
#' @param column_info List of metadata and distinct values dataframes
#' @param initial_value Vector. Initial filter value(s)
#' @return Shiny UI element
filter_module_ui <- function(id, column_info, initial_value = NULL) {
  ns <- NS(id)
  
  # Extract column metadata
  metadata <- column_info$metadata
  distinct_values <- column_info$distinct_values
  
  filter_input <- create_filter_input(ns, metadata, distinct_values, initial_value)
  create_filter_container(ns, metadata$column_name, filter_input)
}

#' Create appropriate filter input based on column type
#' @noRd
create_filter_input <- function(ns, metadata, distinct_values, initial_value) {
  switch(metadata$column_type,
         "numeric" = create_numeric_input(ns, metadata, initial_value),
         "categorical" = create_categorical_input(ns, metadata, distinct_values, initial_value),
         "date" = create_date_input(ns, metadata, initial_value),
         stop(paste("Unsupported filter type:", metadata$column_type)))
}

#' Create numeric slider input
#' @noRd
create_numeric_input <- function(ns, metadata, initial_value) {
  if (is.null(initial_value)) {
    initial_value <- c(as.numeric(metadata$min_value), 
                       as.numeric(metadata$max_value))
  }
  
  sliderInput(
    inputId = ns("filter_value"),
    label = NULL,
    min = as.numeric(metadata$min_value),
    max = as.numeric(metadata$max_value),
    value = initial_value,
    width = "100%"
  )
}

#' Create date range input
#' @noRd
create_date_input <- function(ns, metadata, initial_value) {
  if (is.null(initial_value)) {
    initial_value <- c(as.Date(metadata$min_value), 
                       as.Date(metadata$max_value))
  }
  
  dateRangeInput(
    inputId = ns("filter_value"),
    label = NULL,
    start = initial_value[1],
    end = initial_value[2],
    min = as.Date(metadata$min_value),
    max = as.Date(metadata$max_value),
    width = "100%"
  )
}

#' Create categorical input with support for large value sets
#' @noRd
create_categorical_input <- function(ns, metadata, distinct_values, initial_value) {
  # Get values for this column
  values <- distinct_values %>%
    filter(column_name == metadata$column_name) %>%
    pull(value)
  
  if (length(values) <= 8) {
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
        choices = values,
        selected = initial_value,
        width = "100%"
      )
    )
  } else {
    # Use selectize with manual input support
    tagList(
      div(
        style = "margin-bottom: 10px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span(
            class = "text-muted",
            style = "font-size: 0.9em;",
            if (length(values) > 0) {
              sprintf("%d available values", length(values))
            } else {
              sprintf("Large column (%d distinct values) - Type to filter", metadata$n_distinct)
            }
          ),
          if (length(values) > 0) {
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
          }
        )
      ),
      selectizeInput(
        inputId = ns("filter_value"),
        label = NULL,
        choices = values,
        selected = initial_value,
        multiple = TRUE,
        options = list(
          plugins = list('remove_button'),
          create = TRUE,  # Allow manual input
          createOnBlur = TRUE,
          placeholder = if (length(values) == 0) {
            sprintf('Type values to filter (max %d items)...', 50)
          } else {
            sprintf('Select or type values (max %d)...', 50)
          },
          maxItems = 50  # Limit number of selected items
        ),
        width = "100%"
      )
    )
  }
}

#' Create filter container with improved layout
#' @noRd
create_filter_container <- function(ns, name, filter_input) {
  tagList(
    div(
      id = ns("container"),
      class = "filter-container",
      style = "margin-bottom: 15px;",
      div(
        class = "filter-content",
        style = "display: flex; align-items: center; gap: 10px;",
        div(
          class = "filter-main",
          style = "flex-grow: 1;",
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
          style = "padding-top: 20px;",
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

#' Create filter module server logic
#' @param id Character. The module ID
#' @param metadata Metadata for the column
#' @param distinct_values Distinct values for categorical columns
#' @param initial_value Initial filter value
#' @return List of reactive values
#' @export
filter_module_server <- function(id, metadata, distinct_values, initial_value = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Set default values if not provided
    if (is.null(initial_value)) {
      initial_value <- switch(metadata$column_type,
                              "numeric" = c(as.numeric(metadata$min_value), 
                                            as.numeric(metadata$max_value)),
                              "date" = c(as.Date(metadata$min_value), 
                                         as.Date(metadata$max_value)),
                              "categorical" = character(0))
    }
    
    # Reactive values
    filter_state <- reactiveVal(initial_value)
    is_active <- reactiveVal(FALSE)
    
    # Handle select all/deselect all for categorical inputs
    if (metadata$column_type == "categorical") {
      col_values <- distinct_values %>%
        filter(column_name == metadata$column_name) %>%
        pull(value)
      
      if (length(col_values) > 0 && length(col_values) <= 300) {
        observeEvent(input$select_all, {
          if (length(col_values) > 8) {
            updateSelectizeInput(session, "filter_value", selected = col_values)
          } else {
            updateCheckboxGroupInput(session, "filter_value", selected = col_values)
          }
        })
        
        observeEvent(input$deselect_all, {
          if (length(col_values) > 8) {
            updateSelectizeInput(session, "filter_value", selected = character(0))
          } else {
            updateCheckboxGroupInput(session, "filter_value", selected = character(0))
          }
        })
      }
    }
    
    # Update filter state when input changes
    observeEvent(input$filter_value, {
      current_state <- filter_state()
      
      if (metadata$column_type == "date") {
        if (length(input$filter_value) == 2 && 
            !is.na(input$filter_value[1]) && 
            !is.na(input$filter_value[2])) {
          if (!identical(as.character(current_state), as.character(input$filter_value))) {
            is_active(TRUE)
            filter_state(input$filter_value)
          }
        }
      } else {
        if (!identical(current_state, input$filter_value)) {
          is_active(TRUE)
          filter_state(input$filter_value)
        }
      }
    }, ignoreNULL = FALSE)
    
    # Return interface
    list(
      value = filter_state,
      remove = reactive(input$remove),
      column = metadata$column_name,
      type = metadata$column_type,
      is_active = is_active
    )
  })
}

#' Build filter expression from filter state
#' @param column_name Column name
#' @param column_type Column type
#' @param filter_value Filter value
#' @return Character string containing filter expression
#' @noRd
build_filter_expression <- function(column_name, column_type, filter_value) {
  if (is.null(filter_value) || length(filter_value) == 0) {
    return(NULL)
  }
  
  switch(column_type,
         "numeric" = sprintf("%s >= %f & %s <= %f",
                             column_name, filter_value[1],
                             column_name, filter_value[2]),
         "date" = sprintf("%s >= as.Date('%s') & %s <= as.Date('%s')",
                          column_name, as.character(filter_value[1]),
                          column_name, as.character(filter_value[2])),
         "categorical" = {
           values_str <- paste(sprintf("'%s'", filter_value), collapse = ", ")
           sprintf("%s %%in%% c(%s)", column_name, values_str)
         })
}
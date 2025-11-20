#' Create single filter UI components
#' 
#' @description
#' Creates a UI component for filtering a single database column. The type of filter
#' widget displayed depends on the column type:
#' - Numeric: slider input with min/max range
#' - Date: date range input
#' - Categorical: checkbox group (<=8 values) or selectize input (>8 values)
#' 
#' @param id Character. The module ID (namespace)
#' @param column_info List with two elements:
#'   \itemize{
#'     \item \code{metadata}: Dataframe with column information including:
#'       \code{column_name}, \code{column_type} ("numeric", "date", or "categorical"),
#'       \code{min_value}/\code{max_value} (for numeric), 
#'       \code{min_date}/\code{max_date} (for date),
#'       \code{n_distinct} (for categorical)
#'     \item \code{distinct_values}: Dataframe with columns \code{column_name} and 
#'       \code{value} containing distinct values for categorical columns
#'   }
#' @param column_name Character. The name of the column to filter
#' @param initial_value Vector. Optional initial filter value(s). For numeric/date,
#'   a vector of length 2 (min, max). For categorical, a character vector of selected values.
#'   If NULL, defaults to full range for numeric/date or no selection for categorical.
#'   
#' @return A Shiny UI tagList containing the filter widget with a remove button
#' @export
#' 
#' @examples
#' if (interactive()) {
#'   column_info <- load_demo_column_info("diamonds")
#'   
#'   ui <- fluidPage(
#'     single_filter_ui("my_filter", column_info, "carat")
#'   )
#' }
single_filter_ui <- function(id, column_info, column_name, initial_value = NULL) {
  ns <- NS(id)
  
  metadata <- column_info$metadata %>%
    filter(column_name == !!column_name) %>%
    as.list()
  
  distinct_values <- column_info$distinct_values
  
  filter_input <- create_filter_input(ns, metadata, distinct_values, initial_value)
  create_filter_container(ns, metadata$column_name, filter_input)
}

#' Create single filter server logic
#' 
#' @description
#' Server logic for a single column filter module. Manages the filter state and 
#' provides reactive values for accessing the current filter selection.
#' 
#' @param id Character. The module ID (must match the ID used in \code{single_filter_ui})
#' @param column_info List with two elements:
#'   \itemize{
#'     \item \code{metadata}: Dataframe with column information (see \code{single_filter_ui})
#'     \item \code{distinct_values}: Dataframe with distinct values for categorical columns
#'   }
#' @param column_name Character. The name of the column to filter
#' @param initial_value Vector. Optional initial filter value (see \code{single_filter_ui})
#' 
#' @return A list with five reactive elements:
#'   \describe{
#'     \item{\code{value}}{reactiveVal containing the current filter value. 
#'       For numeric/date: vector of length 2 (min, max). 
#'       For categorical: character vector of selected values.
#'       Access with \code{result$value()}}
#'     \item{\code{remove}}{Reactive returning the number of times the remove button 
#'       has been clicked. Access with \code{result$remove()}}
#'     \item{\code{column}}{Character string with the column name being filtered. 
#'       Access with \code{result$column}}
#'     \item{\code{type}}{Character string with the column type ("numeric", "date", or "categorical").
#'       Access with \code{result$type}}
#'     \item{\code{is_active}}{reactiveVal indicating if the filter has been modified 
#'       from its default state. Access with \code{result$is_active()}}
#'   }
#'   
#' @export
#' 
#' @examples
#' if (interactive()) {
#'   column_info <- load_demo_column_info("diamonds")
#'   
#'   server <- function(input, output, session) {
#'     filter_result <- single_filter_server("my_filter", column_info, "carat")
#'     
#'     observe({
#'       cat("Current filter value:", filter_result$value(), "\n")
#'       cat("Column:", filter_result$column, "\n")
#'       cat("Type:", filter_result$type, "\n")
#'       cat("Is active:", filter_result$is_active(), "\n")
#'     })
#'   }
#' }
single_filter_server <- function(id, column_info, column_name, initial_value = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    metadata <- column_info$metadata %>%
      filter(column_name == !!column_name) %>%
      as.list()
    
    distinct_values <- column_info$distinct_values
    
    if (is.null(initial_value)) {
      initial_value <- switch(metadata$column_type,
                              "numeric" = c(as.numeric(metadata$min_value), 
                                            as.numeric(metadata$max_value)),
                              "date" = c(as.Date(metadata$min_date), 
                                         as.Date(metadata$max_date)),
                              "categorical" = character(0))
    }
    
    filter_state <- reactiveVal(initial_value)
    is_active <- reactiveVal(FALSE)
    
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
    
    list(
      value = filter_state,
      remove = reactive(input$remove),
      column = metadata$column_name,
      type = metadata$column_type,
      is_active = is_active
    )
  })
}

#' Demo app for single_filter
#'
#' @description
#' Demonstrates the single_filter module with three different column types 
#' (numeric, categorical, and date). Shows all values returned by the server function.
#' 
#' @return A Shiny app object
#' @export
#'
#' @examples
#' if (interactive()) {
#'   single_filter_demo()
#' }
single_filter_demo <- function() {
  
  diamonds_info <- load_demo_column_info("diamonds")
  gapdata_info <- load_demo_column_info("gapdata")
  
  ui <- fluidPage(
    titlePanel("Single Filter Demo - Three Column Types"),
    fluidRow(
      column(4,
             h3("Numeric Filter (carat)"),
             single_filter_ui("filter_numeric", diamonds_info, "carat"),
             h4("Module Returns:"),
             verbatimTextOutput("output_numeric")
      ),
      column(4,
             h3("Categorical Filter (cut)"),
             single_filter_ui("filter_categorical", diamonds_info, "cut"),
             h4("Module Returns:"),
             verbatimTextOutput("output_categorical")
      ),
      column(4,
             h3("Date Filter (date)"),
             single_filter_ui("filter_date", gapdata_info, "date"),
             h4("Module Returns:"),
             verbatimTextOutput("output_date")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    numeric_result <- single_filter_server("filter_numeric", diamonds_info, "carat")
    categorical_result <- single_filter_server("filter_categorical", diamonds_info, "cut")
    date_result <- single_filter_server("filter_date", gapdata_info, "date")
    
    output$output_numeric <- renderPrint({
      cat("$column:", numeric_result$column, "\n")
      cat("$type:", numeric_result$type, "\n")
      cat("$value():", paste(numeric_result$value(), collapse = ", "), "\n")
      cat("$is_active():", numeric_result$is_active(), "\n")
      cat("$remove():", if(is.null(numeric_result$remove())) 0 else numeric_result$remove(), "\n\n")
      
      expr <- build_single_filter_expression(numeric_result$column, 
                                       numeric_result$type, 
                                       numeric_result$value())
      cat("Filter Expression:\n")
      if (is.null(expr)) cat("(empty)") else cat(expr)
    })
    
    output$output_categorical <- renderPrint({
      cat("$column:", categorical_result$column, "\n")
      cat("$type:", categorical_result$type, "\n")
      cat("$value():", paste(categorical_result$value(), collapse = ", "), "\n")
      cat("$is_active():", categorical_result$is_active(), "\n")
      cat("$remove():", if(is.null(categorical_result$remove())) 0 else categorical_result$remove(), "\n\n")
      
      expr <- build_single_filter_expression(categorical_result$column, 
                                       categorical_result$type, 
                                       categorical_result$value())
      cat("Filter Expression:\n")
      if (is.null(expr)) cat("(empty)") else cat(expr)
    })
    
    output$output_date <- renderPrint({
      cat("$column:", date_result$column, "\n")
      cat("$type:", date_result$type, "\n")
      cat("$value():", paste(date_result$value(), collapse = ", "), "\n")
      cat("$is_active():", date_result$is_active(), "\n")
      cat("$remove():", if(is.null(date_result$remove())) 0 else date_result$remove(), "\n\n")
      
      expr <- build_single_filter_expression(date_result$column, 
                                       date_result$type, 
                                       date_result$value())
      cat("Filter Expression:\n")
      if (is.null(expr)) cat("(empty)") else cat(expr)
    })
  }
  
  shinyApp(ui, server)
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
    initial_value <- c(as.Date(metadata$min_date), 
                       as.Date(metadata$max_date))
  }
  
  dateRangeInput(
    inputId = ns("filter_value"),
    label = NULL,
    start = initial_value[1],
    end = initial_value[2],
    min = as.Date(metadata$min_date),
    max = as.Date(metadata$max_date),
    width = "100%"
  )
}

#' Create categorical input with support for large value sets and empty cases
#' @noRd
create_categorical_input <- function(ns, metadata, distinct_values, initial_value, max_distinct_values = 300) {
  values <- distinct_values %>%
    filter(column_name == metadata$column_name) %>%
    pull(value)
  
  n_distinct <- metadata$n_distinct
  
  if (n_distinct == 0) {
    return(
      div(
        class = "alert alert-warning",
        style = "margin-bottom: 0;",
        icon("exclamation-triangle"),
        "This column contains only NULL/NA values"
      )
    )
  }
  
  if (length(values) == 0 && n_distinct > max_distinct_values) {
    return(
      div(
        class = "filter-info",
        style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; border-left: 3px solid #6c757d;",
        div(
          style = "font-weight: bold; margin-bottom: 5px;",
          sprintf("%d distinct values", n_distinct)
        ),
        div(
          style = "color: #666; font-size: 0.9em;",
          "Too many values to display. You can still enter specific values below:"
        ),
        div(
          style = "margin-top: 10px;",
          selectizeInput(
            inputId = ns("filter_value"),
            label = NULL,
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = TRUE,
              createOnBlur = TRUE,
              placeholder = "Type values to filter...",
              maxItems = 50
            ),
            width = "100%"
          )
        )
      )
    )
  }
  
  if (length(values) == 0) {
    return(
      div(
        class = "alert alert-warning",
        style = "margin-bottom: 0;",
        icon("exclamation-triangle"),
        sprintf("Could not load distinct values (count: %d). Please try refreshing.", n_distinct)
      )
    )
  }
  
  if (length(values) <= 8) {
    tagList(
      div(
        style = "margin-bottom: 10px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
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
          ),
          span(
            class = "text-muted",
            style = "font-size: 0.9em;",
            sprintf("%d values", length(values))
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
    tagList(
      div(
        style = "margin-bottom: 10px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
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
          ),
          span(
            class = "text-muted",
            style = "font-size: 0.9em;",
            sprintf("%d values", length(values))
          )
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
          create = TRUE,
          createOnBlur = TRUE,
          placeholder = sprintf('Select or type values (max %d)...', 50),
          maxItems = 50
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

#' Build filter expression from single filter state
#' 
#' @description
#' Converts filter values into R/SQL filter expressions that can be used with dplyr/dbplyr.
#' 
#' @param column_name Character. The name of the column being filtered
#' @param column_type Character. The type of column: "numeric", "date", or "categorical"
#' @param filter_value Vector. The filter value(s):
#'   \itemize{
#'     \item For numeric: numeric vector of length 2 (min, max)
#'     \item For date: Date vector of length 2 (start, end)
#'     \item For categorical: character vector of selected values
#'   }
#'   
#' @return Character string containing the filter expression, or NULL if filter_value is empty.
#'   Examples:
#'   \itemize{
#'     \item Numeric: \code{"price >= 100.000000 & price <= 500.000000"}
#'     \item Date: \code{"date >= as.Date('2020-01-01') & date <= as.Date('2023-12-31')"}
#'     \item Categorical: \code{"cut \%in\% c('Ideal', 'Premium')"}
#'   }
#'   
#' @noRd
build_single_filter_expression <- function(column_name, column_type, filter_value) {
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

#' Create single filter UI components
#' @param id Character. The module ID
#' @param single_column_info List with metadata and distinct_values dataframes
#' @param initial_value Vector. Initial filter value(s)
#' @return Shiny UI element
#' @export
single_filter_ui <- function(id, single_column_info, initial_value = NULL) {
  ns <- NS(id)
  
  metadata <- single_column_info$metadata
  distinct_values <- single_column_info$distinct_values
  
  filter_input <- create_filter_input(ns, metadata, distinct_values, initial_value)
  create_filter_container(ns, metadata$column_name, filter_input)
}

#' Create single filter server logic
#' @param id Character. The module ID
#' @param single_column_info List with metadata and distinct_values
#' @param initial_value Initial filter value
#' @return List of reactive values
#' @export
single_filter_server <- function(id, single_column_info, initial_value = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    metadata <- single_column_info$metadata
    distinct_values <- single_column_info$distinct_values
    
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
#' @return A Shiny app object
#' @export
#'
#' @examples
#' if (interactive()) {
#'   single_filter_demo()
#' }
single_filter_demo <- function() {
  
  column_info <- load_demo_column_info("diamonds")
  
  carat_info <- list(
    metadata = column_info$metadata %>% filter(column_name == "carat") %>% as.list(),
    distinct_values = column_info$distinct_values
  )
  
  cut_info <- list(
    metadata = column_info$metadata %>% filter(column_name == "cut") %>% as.list(),
    distinct_values = column_info$distinct_values
  )
  
  gapdata_info <- load_demo_column_info("gapdata")
  date_info <- list(
    metadata = gapdata_info$metadata %>% filter(column_name == "date") %>% as.list(),
    distinct_values = gapdata_info$distinct_values
  )
  
  ui <- fluidPage(
    titlePanel("Single Filter Demo - Three Column Types"),
    fluidRow(
      column(4,
             h3("Numeric Filter"),
             single_filter_ui("filter_numeric", carat_info),
             verbatimTextOutput("expr_numeric")
      ),
      column(4,
             h3("Categorical Filter"),
             single_filter_ui("filter_categorical", cut_info),
             verbatimTextOutput("expr_categorical")
      ),
      column(4,
             h3("Date Filter"),
             single_filter_ui("filter_date", date_info),
             verbatimTextOutput("expr_date")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    numeric_result <- single_filter_server("filter_numeric", carat_info)
    categorical_result <- single_filter_server("filter_categorical", cut_info)
    date_result <- single_filter_server("filter_date", date_info)
    
    output$expr_numeric <- renderPrint({
      expr <- build_filter_expression(numeric_result$column, 
                                       numeric_result$type, 
                                       numeric_result$value())
      cat("Expression:\n")
      if (is.null(expr)) cat("(empty)") else cat(expr)
    })
    
    output$expr_categorical <- renderPrint({
      expr <- build_filter_expression(categorical_result$column, 
                                       categorical_result$type, 
                                       categorical_result$value())
      cat("Expression:\n")
      if (is.null(expr)) cat("(empty)") else cat(expr)
    })
    
    output$expr_date <- renderPrint({
      expr <- build_filter_expression(date_result$column, 
                                       date_result$type, 
                                       date_result$value())
      cat("Expression:\n")
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

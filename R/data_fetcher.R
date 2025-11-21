#' Create data fetcher UI components
#'
#' Creates the user interface for fetching data from a database query. Displays a "Fetch Data" button
#' with an optional SQL preview (either on hover or collapsible). Also shows a warning message
#' when fetching unsummarized data that may return many records.
#'
#' @param id Character. The module ID used to namespace the UI elements
#' @param style Character. Display style for SQL preview:
#'   \itemize{
#'     \item "hover" (default): Preview appears when hovering over the button area
#'     \item "collapsible": Preview can be toggled with a "Show SQL Preview" link
#'   }
#'
#' @return A Shiny UI element (tagList) containing the fetch button, SQL preview, and warning message placeholder
#'
#' @details
#' The "hover" style is recommended for most use cases as it provides quick access to the SQL preview
#' without cluttering the UI. The "collapsible" style is useful when screen space is limited.
#'
#' @export
#'
#' @examples
#' # In a Shiny UI
#' if (interactive()) {
#'   ui <- fluidPage(
#'     data_fetcher_ui("fetcher", style = "hover")
#'   )
#' }
data_fetcher_ui <- function(id, style = "hover") {
  ns <- NS(id)
  
  if (style == "hover") {
    tagList(
      div(
        id = ns("container"),
        # Warning message
        uiOutput(ns("warning_message")),
        
        # Button and SQL preview
        div(
          class = "preview-trigger",
          actionButton(
            ns("fetch_data"),
            "Fetch Data",
            class = "btn-primary"
          ),
          span(
            "Hover to preview SQL",
            style = "margin-left: 8px; color: #666; font-size: 0.8em;"
          )
        ),
        div(
          class = "hover-preview",
          style = "margin-top: 10px;",
          verbatimTextOutput(ns("query_preview"))
        )
      ),
      tags$head(
        tags$style(sprintf(
          "#%s { position: relative; }
           .hover-preview {
             visibility: hidden;
             position: absolute;
             z-index: 100;
             background: white;
             border: 1px solid #ddd;
             padding: 10px;
             border-radius: 4px;
             box-shadow: 0 2px 8px rgba(0,0,0,0.1);
             max-width: 800px;
             font-family: monospace;
             font-size: 0.9em;
             opacity: 0;
             transition: visibility 0s, opacity 0.2s linear;
           }
           .preview-trigger:hover + .hover-preview,
           .hover-preview:hover {
             visibility: visible;
             opacity: 1;
           }",
          ns("")
        ))
      )
    )
  } else {
    # Default collapsible version
    tagList(
      div(
        style = "margin-bottom: 10px;",
        uiOutput(ns("warning_message")),
        actionButton(
          ns("fetch_data"),
          "Fetch Data",
          class = "btn-primary"
        )
      ),
      div(
        style = "margin-top: 5px;",
        actionLink(
          ns("toggle_preview"),
          "Show SQL Preview",
          style = "color: #666; font-size: 0.9em;"
        ),
        div(
          id = ns("preview_container"),
          style = "visibility: hidden; height: 0; margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-left: 3px solid #dee2e6; font-family: monospace; font-size: 0.9em;",
          verbatimTextOutput(ns("query_preview"))
        )
      )
    )
  }
}


#' Create data fetcher server logic
#'
#' Executes database queries when the user clicks "Fetch Data" and manages the results.
#' Shows a warning when fetching unsummarized data and provides SQL preview on hover.
#'
#' @param id Character. The module ID
#' @param pool Database connection pool
#' @param query Reactive expression returning a dbplyr query object (lazy tbl)
#' @param needs_summary Reactive expression returning a logical indicating if data will be summarized
#'
#' @return A list with three reactive expressions:
#' \describe{
#'   \item{data}{Reactive returning a data.frame or tibble with the fetched results, or NULL if no data has been fetched yet}
#'   \item{error}{Reactive returning a character string with error message, or NULL if no error occurred}
#'   \item{executed_query}{Reactive returning the SQL query text that was executed as a character string, or "" if nothing has been executed yet}
#' }
#'
#' @details
#' The module executes the query lazily - it only fetches data when the user clicks the "Fetch Data" button.
#' This is important because the query may return a large amount of data.
#'
#' When `needs_summary()` is FALSE (indicating the user wants all records, not aggregated data),
#' a warning message is displayed to alert the user that fetching may take time.
#'
#' @export
#'
#' @examples
#' # See data_fetcher_demo() for a complete working example
#' if (interactive()) {
#'   data_fetcher_demo()
#' }
data_fetcher_server <- function(id, pool, query, needs_summary) {
  moduleServer(id, function(input, output, session) {
    # State management
    error_state <- reactiveVal(NULL)
    fetched_data <- reactiveVal(NULL)
    executed_query <- reactiveVal("")
    
    # Warning message output
    output$warning_message <- renderUI({
      req(query())
      
      # Only show warning if we're not summarizing
      if (!needs_summary()) {
        div(
          class = "alert alert-warning",
          style = "margin-bottom: 10px;",
          icon("exclamation-triangle"),
          tags$b("Warning: "),
          "Fetching all data without summarization may take a while.",
          tags$br(),
          "Consider using summary statistics if you don't need individual records."
        )
      }
    })
    
    # Show preview query
    output$query_preview <- renderPrint({
      cat(get_sql_text(query()))
    })
    
    # Execute query when fetch button is clicked
    observeEvent(input$fetch_data, {
      query_val <- query()
      
      if (is.null(query_val)) {
        fetched_data(NULL)
        executed_query("")
        return()
      }
      
      tryCatch({
        # Store the SQL that's about to be executed
        executed_query(get_sql_text(query_val))
        
        # Execute query with progress indicator
        withProgress(
          message = 'Fetching data...',
          {
            result <- collect(query_val)
            fetched_data(result)
            error_state(NULL)
          }
        )
        
      }, error = function(e) {
        error_state(paste("Error executing query:", e$message))
        fetched_data(NULL)
        executed_query("")
      })
    })
    
    # Return interface
    list(
      data = reactive(fetched_data()),
      error = reactive(error_state()),
      executed_query = reactive(executed_query())
    )
  })
}



# Helper Functions ----

#' Convert dbplyr query to SQL text
#'
#' Converts a dbplyr lazy table object to its SQL representation as text.
#' Used to show users the SQL query that will be executed.
#'
#' @param query A dbplyr query object (lazy tbl), or NULL
#' @return Character string containing the SQL query, or a status message if query is NULL or an error occurs
#'
#' @noRd
get_sql_text <- function(query) {
  if (is.null(query)) {
    "Select a table to preview query"
  } else {
    tryCatch({
      paste(capture.output(dplyr::show_query(query)), collapse = "\n")
    }, error = function(e) {
      paste("Error generating SQL:", e$message)
    })
  }
}

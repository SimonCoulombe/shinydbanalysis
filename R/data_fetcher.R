#' Create data fetcher UI components
#'
#' @param id Character. The module ID
#' @param style Character. Either "collapsible" or "hover" for the preview display style
#' @return A Shiny UI element
#' @export
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
#' @param id Character. The module ID
#' @param pool Database connection pool
#' @param table_builder Table picker module instance
#' @param filter_builder Filter builder module instance
#' @param summary_builder Summary builder module instance
#' @return List of reactive expressions
#' @export
data_fetcher_server <- function(id, pool, table_builder, filter_builder, summary_builder) {
  moduleServer(id, function(input, output, session) {
    # State management
    error_state <- reactiveVal(NULL)
    fetched_data <- reactiveVal(NULL)
    executed_query <- reactiveVal("")
    
    # Warning message output
    output$warning_message <- renderUI({
      req(table_builder$selected_table())
      
      # Only show warning if we're not summarizing
      if (!summary_builder$needs_summary()) {
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
    
    # Build query using dbplyr
    preview_query <- reactive({
      table <- table_builder$selected_table()
      
      if (is.null(table) || !nzchar(table)) {
        return(NULL)
      }
      
      tryCatch({
        # Get base table reference
        query <- table_builder$create_table_ref()
        
        # Apply filters if any
        where_clause <- filter_builder$where_clause()
        if (!is.null(where_clause) && nzchar(where_clause)) {
          filter_expr <- parse_filter_expression(where_clause)
          query <- filter(query, !!filter_expr)
        }
        
        # Only apply summarization if specifically requested
        if (summary_builder$needs_summary()) {
          # Get grouping variables if any
          group_vars <- summary_builder$group_vars()
          if (length(group_vars) > 0) {
            query <- group_by(query, !!!syms(group_vars))
          }
          
          # Apply summary specifications
          summary_specs <- summary_builder$summary_specs()
          if (length(summary_specs) > 0) {
            summary_exprs <- build_summary_expressions(summary_specs)
            if (length(summary_exprs) > 0) {
              query <- summarise(query, !!!summary_exprs)
            }
          }
        }
        
        query
        
      }, error = function(e) {
        error_state(paste("Error building query:", e$message))
        NULL
      })
    })
    
    # Create preview text
    preview_text <- reactive({
      query <- preview_query()
      get_sql_text(query)
    })
    
    # Show preview query
    output$query_preview <- renderPrint({
      cat(preview_text())
    })
    
    # Execute query when fetch button is clicked
    observeEvent(input$fetch_data, {
      query <- preview_query()
      
      if (is.null(query)) {
        fetched_data(NULL)
        executed_query("")
        return()
      }
      
      tryCatch({
        # Store the SQL that's about to be executed
        executed_query(get_sql_text(query))
        
        # Execute query with progress indicator
        withProgress(
          message = 'Fetching data...',
          {
            result <- collect(query)
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
      executed_query = reactive(executed_query()),
      preview_query = reactive(preview_text())
    )
  })
}

# Helper Functions ----

#' Convert dbplyr query to SQL text
#' @param query dbplyr query object
#' @return Character string containing the SQL query or status message
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

#' Parse filter expression from WHERE clause
#' @param where_clause Character string containing the filter conditions
#' @return Parsed expression for dplyr filter
#' @noRd
parse_filter_expression <- function(where_clause) {
  # Convert SQL-like syntax to R expression
  expr <- where_clause %>%
    # Keep %in% as is (it's already R syntax)
    gsub(" AND ", " & ", ., fixed = TRUE) %>%
    gsub(" OR ", " | ", ., fixed = TRUE)
  
  rlang::parse_expr(expr)
}

#' Build summary expressions for dplyr summarise
#' @param summary_specs List of summary specifications
#' @return List of quoted expressions for summarise
#' @noRd
build_summary_expressions <- function(summary_specs) {
  summary_exprs <- list()
  
  for (spec in summary_specs) {
    if (spec$func == "count") {
      summary_exprs$record_count <- quo(n())
    } else {
      # Build expression like mean(price), sum(quantity), etc.
      expr <- call(spec$func, sym(spec$metric))
      name <- paste0(spec$func, "_", spec$metric)
      summary_exprs[[name]] <- quo(!!expr)
    }
  }
  
  summary_exprs
}
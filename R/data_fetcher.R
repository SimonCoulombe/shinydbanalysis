#' Create data fetcher UI components
#'
#' @param id Character. The module ID
#' @param style Character. Either "collapsible" or "hover" for the preview display style
#' @return A Shiny UI element
#' @export
data_fetcher_ui <- function(id, style = "collapsible") {
  ns <- NS(id)

  if (style == "collapsible") {
    tagList(
      div(
        style = "margin-bottom: 10px;",
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
          style = "display: none; margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-left: 3px solid #dee2e6; font-family: monospace; font-size: 0.9em;",
          verbatimTextOutput(ns("query_preview"))
        )
      ),
      tags$script(sprintf(
        "$(document).on('click', '#%s', function() {
          $('#%s').toggle();
        });",
        ns("toggle_preview"),
        ns("preview_container")
      ))
    )
  } else if (style == "hover") {
    # Modified hover version
    tagList(
      tags$head(
        tags$style(sprintf(
          "#%s { position: relative; }
           .hover-preview {
             display: none;
             position: absolute;
             z-index: 100;
             background: white;
             border: 1px solid #ddd;
             padding: 10px;
             border-radius: 4px;
             box-shadow: 0 2px 8px rgba(0,0,0,0.1);
             max-width: 800px;
             margin-top: 5px;
             font-family: monospace;
             font-size: 0.9em;
           }
           .preview-trigger:hover + .hover-preview,
           .hover-preview:hover {
             display: block;
           }",
          ns("")
        ))
      ),
      div(
        id = ns("container"),
        div(
          class = "preview-trigger",
          actionButton(
            ns("fetch_data"),
            "Fetch Data",
            class = "btn-primary"
          ),
          span(
            "Hover to preview SQL query",
            style = "margin-left: 8px; color: #666; font-size: 0.8em;"
          )
        ),
        div(
          class = "hover-preview",
          verbatimTextOutput(ns("query_preview"))
        )
      )
    )
  }
}

#' Create data fetcher server
#'
#' @param id Character. The module ID
#' @param pool Database connection pool
#' @param table_info Table picker module instance
#' @param filter_builder Filter builder module instance
#' @param group_builder Group builder module instance
#' @param summary_builder Summary builder module instance
#' @return List of reactive expressions containing fetched data and error state
#' @export
data_fetcher_server <- function(id, pool, table_info, filter_builder, group_builder, summary_builder) {
  moduleServer(id, function(input, output, session) {
    # State management
    error_state <- reactiveVal(NULL)
    fetched_data <- reactiveVal(NULL)
    executed_query <- reactiveVal("")
    preview_visible <- reactiveVal(FALSE)

    # Toggle preview visibility and update link text
    observeEvent(input$toggle_preview, {
      preview_visible(!preview_visible())
      # Update the link text
      updateActionLink(session, "toggle_preview",
                       label = if (preview_visible()) "Hide SQL Preview" else "Show SQL Preview"
      )
    })

    # Build preview query using dbplyr
    preview_query <- reactive({
      req(table_info$selected_table())

      tryCatch({
        message("Building dbplyr query...")

        # Start with the table
        table_name <- table_info$selected_table()
        query <- tbl(pool, table_name)

        # Apply filters if any
        where_clause <- filter_builder$where_clause()
        if (!is.null(where_clause) && where_clause != "") {
          filter_expr <- convert_sql_to_filter(where_clause)
          message("Filter expression: ", filter_expr)
          query <- filter(query, !!parse_expr(filter_expr))
        }

        # Get grouping variables
        group_vars <- group_builder$group_vars()

        # Get summary specifications
        summary_specs <- summary_builder$summary_specs()

        # Only apply grouping and summarizing if both are specified
        if (length(group_vars) > 0 && length(summary_specs) > 0) {
          # Add grouping
          query <- group_by(query, !!!syms(group_vars))

          # Build summarise expressions
          summary_exprs <- list()

          for (spec in summary_specs) {
            if (spec$func == "count") {
              summary_exprs$record_count <- rlang::quo(n())
            } else {
              expr <- call(spec$func, sym(spec$metric))
              summary_exprs[[paste0(spec$func, "_", spec$metric)]] <- rlang::quo(!!expr)
            }
          }

          if (length(summary_exprs) > 0) {
            query <- summarise(query, !!!summary_exprs)
          }
        }

        message("Query built successfully")
        query

      }, error = function(e) {
        message("Error building query: ", e$message)
        error_state(paste("Error building query:", e$message))
        NULL
      })
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
        message("Executing query...")
        sql <- paste(capture.output(dplyr::show_query(query)), collapse = "\n")
        executed_query(sql)

        withProgress(
          message = 'Fetching data...',
          {
            incProgress(0.3, detail = "Executing query")
            result <- collect(query)
            message("Query executed successfully, returned ", nrow(result), " rows")
            fetched_data(result)
            error_state(NULL)
          }
        )
      }, error = function(e) {
        message("Error executing query: ", e$message)
        error_state(paste("Error executing query:", e$message))
        fetched_data(NULL)
        executed_query("")
      })
    })

    # Preview query - updates reactively
    output$query_preview <- renderPrint({
      query <- preview_query()

      if (is.null(query)) {
        if (is.null(table_info$selected_table()) || table_info$selected_table() == "") {
          cat("Please select a table")
        } else {
          cat("Error building query")
        }
      } else {
        sql <- capture.output(dplyr::show_query(query))
        cat(paste(sql, collapse = "\n"))
      }
    })

    # Ensure the query preview updates even when hidden
    outputOptions(output, "query_preview", suspendWhenHidden = FALSE)

    # Return interface
    list(
      data = reactive({ fetched_data() }),
      error = reactive({ error_state() }),
      executed_query = reactive({ executed_query() }),
      preview_query = reactive({
        query <- preview_query()
        if (!is.null(query)) {
          paste(capture.output(dplyr::show_query(query)), collapse = "\n")
        } else {
          ""
        }
      })
    )
  })
}

#' Convert SQL WHERE clause to dplyr filter expression
#' @noRd
convert_sql_to_filter <- function(where_clause) {
  if (is.null(where_clause) || where_clause == "") {
    return(NULL)
  }
  where_clause
}

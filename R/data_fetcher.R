#' Data Fetcher Module Using dbplyr
#' @importFrom dplyr group_by summarise filter tbl collect
#' @importFrom dbplyr sql
#' @importFrom rlang parse_expr sym syms
NULL

#' Create data fetcher UI components
#'
#' @param id Character. The module ID
#' @return A Shiny UI element
#' @export
data_fetcher_ui <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(
      ns("fetch_data"),
      "Fetch Data",
      class = "btn-primary"
    ),
    verbatimTextOutput(ns("query_preview"))
  )
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

# In data_fetcher.R

data_fetcher_server <- function(id, pool, table_info, filter_builder, group_builder, summary_builder) {
  moduleServer(id, function(input, output, session) {
    # Error state
    error_state <- reactiveVal(NULL)
    current_data <- reactiveVal(NULL)
    current_sql <- reactiveVal("")

    # Build and execute query using dbplyr
    query_builder <- reactive({
      req(table_info$selected_table())

      tryCatch({
        message("Building dbplyr query...")

        # Start with the table
        table_name <- table_info$selected_table()
        query <- tbl(pool, table_name)

        # Apply filters if any
        where_clause <- filter_builder$where_clause()
        if (!is.null(where_clause) && where_clause != "") {
          # Convert SQL WHERE clause to dplyr filter expression
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
              summary_exprs$record_count <- quo(n())
            } else {
              # Build expression like mean(col), sum(col), etc.
              expr <- call(spec$func, sym(spec$metric))
              summary_exprs[[paste0(spec$func, "_", spec$metric)]] <- quo(!!expr)
            }
          }

          if (length(summary_exprs) > 0) {
            query <- summarise(query, !!!summary_exprs)
          }
        }

        message("Query built successfully")

        # Update SQL preview
        sql <- capture.output(show_query(query))
        current_sql(paste(sql, collapse = "\n"))

        query

      }, error = function(e) {
        message("Error building query: ", e$message)
        error_state(paste("Error building query:", e$message))
        current_sql("")
        NULL
      })
    })

    # Execute query when fetch button is clicked
    observeEvent(input$fetch_data, {
      query <- query_builder()

      if (is.null(query)) {
        current_data(NULL)
        return()
      }

      tryCatch({
        message("Executing query...")

        withProgress(
          message = 'Fetching data...',
          {
            incProgress(0.3, detail = "Executing query")
            result <- collect(query)
            message("Query executed successfully, returned ", nrow(result), " rows")
            current_data(result)
            error_state(NULL)
          }
        )
      }, error = function(e) {
        message("Error executing query: ", e$message)
        error_state(paste("Error executing query:", e$message))
        current_data(NULL)
      })
    })

    # Preview query
    output$query_preview <- renderPrint({
      sql <- current_sql()
      if (sql == "") {
        if (is.null(table_info$selected_table()) || table_info$selected_table() == "") {
          cat("Please select a table")
        } else {
          cat("Error building query")
        }
      } else {
        cat("SQL Query Preview:\n")
        cat(sql)
      }
    })

    # Return interface as a list of reactive expressions
    list(
      data = reactive({ current_data() }),
      error = reactive({ error_state() }),
      current_query = reactive({ current_sql() })
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

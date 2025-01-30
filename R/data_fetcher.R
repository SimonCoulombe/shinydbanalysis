#' Data Fetcher Module with Enhanced Debugging
#' @importFrom DBI dbGetQuery
#' @importFrom shiny NS actionButton moduleServer reactive eventReactive
NULL

#' Create data fetcher UI components
#'
#' @param id Character. The module ID
#' @return A Shiny UI element
#' @export
data_fetcher_ui <- function(id) {
  ns <- NS(id)

  # Debug the generated ID
  message("Creating fetch button with ID: ", ns("fetch_data"))

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
data_fetcher_server <- function(id, pool, table_info, filter_builder, group_builder, summary_builder) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Debug namespace
    message("Data fetcher module ID: ", id)
    message("Fetch button full ID: ", ns("fetch_data"))

    # Error state
    error_state <- reactiveVal(NULL)

    # Debug observer for inputs
    observe({
      message("=== Data Fetcher Inputs ===")
      message("Available inputs: ", paste(names(input), collapse=", "))
      message("Fetch button value: ", input$fetch_data)
    })

    # Build query
    query <- reactive({
      # Debug inputs
      message("\n=== Query Building Debug ===")
      message("Table selected: ", table_info$selected_table())
      message("Group vars: ", paste(group_builder$group_vars(), collapse=", "))
      message("Summary specs count: ", length(summary_builder$summary_specs()))

      # Get basic components
      table <- table_info$selected_table()

      # Don't proceed if no table selected
      if (is.null(table) || table == "") {
        message("No table selected, returning NULL")
        return(NULL)
      }

      # Get other components
      group_vars <- group_builder$group_vars()
      summary_specs <- summary_builder$summary_specs()
      where_clause <- filter_builder$where_clause()

      message("WHERE clause: ", where_clause)

      # Don't build query if we have no columns to select
      if (length(group_vars) == 0 && length(summary_specs) == 0) {
        message("No columns or aggregations selected, returning NULL")
        return(NULL)
      }

      tryCatch({
        # Build query
        query <- build_query(
          table = table,
          where_clause = where_clause,
          group_vars = group_vars,
          summary_specs = summary_specs
        )

        message("Successfully built query: ", query)
        error_state(NULL)
        query

      }, error = function(e) {
        message("Error building query: ", e$message)
        error_state(paste("Error building query:", e$message))
        NULL
      })
    })

    # Preview query
    output$query_preview <- renderPrint({
      q <- query()
      if (is.null(q)) {
        if (is.null(table_info$selected_table()) || table_info$selected_table() == "") {
          cat("Please select a table")
        } else if (length(group_builder$group_vars()) == 0 && length(summary_builder$summary_specs()) == 0) {
          cat("Please select grouping variables and/or metrics to summarize")
        } else {
          cat("Building query...")
        }
      } else {
        cat("SQL Query Preview:\n")
        cat(q)
      }
    })

    # Execute query
    data <- eventReactive(input$fetch_data, {
      message("\n=== Query Execution Debug ===")
      message("Fetch button pressed")

      q <- query()
      if (is.null(q)) {
        message("No valid query available")
        return(NULL)
      }

      message("Executing query: ", q)

      tryCatch({
        withProgress(
          message = 'Fetching data...',
          {
            incProgress(0.3, detail = "Executing query")
            result <- dbGetQuery(pool, q)
            message("Query executed successfully, returned ", nrow(result), " rows")
            result
          }
        )
      }, error = function(e) {
        message("Error executing query: ", e$message)
        error_state(paste("Error executing query:", e$message))
        NULL
      })
    })

    # Return interface
    list(
      data = data,
      error = error_state,
      current_query = query
    )
  })
}


#' Build complete SQL query
#' @noRd
build_query <- function(table, where_clause, group_vars, summary_specs) {
  # Build SELECT clause
  select_parts <- c()

  # Add grouping variables
  if (length(group_vars) > 0) {
    select_parts <- c(select_parts,
                      sprintf("[%s]", group_vars))
  }

  # Add summary calculations
  if (length(summary_specs) > 0) {
    for (spec in summary_specs) {
      alias <- sprintf("%s_%s", spec$func, spec$metric)
      select_parts <- c(select_parts,
                        sprintf("%s AS [%s]", spec$sql, alias))
    }
  }

  # Combine all parts
  select_clause <- paste(select_parts, collapse = ", ")
  from_clause <- sprintf("FROM [%s]", table)

  # Build complete query
  query_parts <- c(
    sprintf("SELECT %s", select_clause),
    from_clause
  )

  if (!is.null(where_clause) && where_clause != "") {
    query_parts <- c(query_parts,
                     sprintf("WHERE %s", where_clause))
  }

  if (length(group_vars) > 0) {
    group_clause <- paste(sprintf("[%s]", group_vars), collapse = ", ")
    query_parts <- c(query_parts,
                     sprintf("GROUP BY %s", group_clause))
  }

  paste(query_parts, collapse = " ")
}

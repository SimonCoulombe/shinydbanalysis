#' Create summary builder UI components
#' 
#' @description
#' Creates the user interface for the summary builder module. This module allows users
#' to configure summary statistics and grouping operations for data analysis. It combines
#' metric selection with the group_builder module for advanced grouping options.
#' 
#' The UI consists of:
#' \itemize{
#'   \item A checkbox to include record counts
#'   \item A multi-select for numeric columns to summarize
#'   \item A multi-select for summary functions (mean, sum, min, max)
#'   \item Embedded group_builder UI for grouping configuration
#' }
#'
#' @param id Character. The module ID used to create a unique namespace for this instance
#' 
#' @return A Shiny tagList containing a well panel with:
#' \describe{
#'   \item{checkboxInput}{Option to include record count in summary}
#'   \item{selectizeInput}{Multi-select for numeric columns to summarize}
#'   \item{selectInput}{Multi-select for summary functions (shown conditionally)}
#'   \item{group_builder_ui}{Embedded grouping controls for dimensions and transformations}
#' }
#' 
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   summary_builder_ui("my_summary")
#' )
#' }
#' 
#' @seealso [summary_builder_server()] for the server-side logic, [group_builder_ui()] for grouping controls
#' @export
summary_builder_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "well",
      style = "padding: 15px; margin-bottom: 15px;",
      h4("Summary Statistics", style = "margin-top: 0;"),
      
      # Base metrics
      div(
        style = "margin-bottom: 15px;",
        checkboxInput(
          ns("include_count"),
          "Include record count",
          value = TRUE
        )
      ),
      
      # Numeric summaries
      div(
        style = "margin-bottom: 15px;",
        selectizeInput(
          ns("metrics"),
          "Select numeric columns to summarize:",
          choices = NULL,
          multiple = TRUE
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'].length > 0", ns("metrics")),
          selectInput(
            ns("functions"),
            "Select summary functions:",
            choices = c(
              "Mean" = "mean",
              "Sum" = "sum",
              "Min" = "min",
              "Max" = "max"
            ),
            multiple = TRUE,
            selected = "mean"
          )
        )
      ),
      
      # Grouping with banding and regrouping (uses group_builder module)
      div(
        style = "margin-top: 15px;",
        group_builder_ui(ns("grouping"))
      )
    )
  )
}

#' Create summary builder server logic
#' 
#' @description
#' Server-side logic for the summary builder module. This function manages the configuration
#' of summary statistics (aggregations) and grouping dimensions. It orchestrates:
#' 
#' \itemize{
#'   \item Updating available numeric columns based on the selected table
#'   \item Building summary specifications from user selections
#'   \item Managing grouping configuration via the embedded group_builder module
#'   \item Determining whether summarization is needed
#' }
#' 
#' The module automatically:
#' \itemize{
#'   \item Refreshes the metric choices when the table changes
#'   \item Creates summary specifications from metric-function combinations
#'   \item Passes through all group_builder return values (group_vars, banding, regrouping)
#'   \item Tracks whether any summarization operations are configured
#' }
#'
#' @param id Character. The module ID matching the UI function
#' @param selected_table_name Reactive expression returning the currently selected table name
#' @param column_info Reactive expression returning a list with:
#'   \describe{
#'     \item{metadata}{Dataframe with column_name, column_type, etc.}
#'     \item{distinct_values}{Dataframe with column_name and value for categorical columns}
#'   }
#'   
#' @return A named list with five reactive expressions:
#' \describe{
#'   \item{summary_specs}{Reactive returning a list of summary specifications. Each spec
#'     is a list with elements:
#'     \itemize{
#'       \item metric: Column name to summarize (or "*" for count)
#'       \item func: Function name ("count", "mean", "sum", "min", "max")
#'       \item sql: SQL function name (same as func, or "n" for count)
#'     }
#'     Returns empty list when no summary operations are configured.
#'     Example: list(
#'       list(metric = "*", func = "count", sql = "n"),
#'       list(metric = "price", func = "mean", sql = "mean"),
#'       list(metric = "price", func = "sum", sql = "sum")
#'     )}
#'   \item{group_vars}{Reactive returning a character vector of selected grouping column names,
#'     or NULL/empty vector when no grouping. Passed through from group_builder_server().
#'     Example: c("cut", "color")}
#'   \item{needs_summary}{Reactive returning a logical indicating whether any summary
#'     operations are configured (either count is enabled OR metrics with functions are selected).
#'     Used to determine if summarise() should be applied to the query.
#'     Example: TRUE}
#'   \item{banding_configs}{Reactive returning a named list of banding configurations
#'     for numeric grouping columns. Passed through from group_builder_server().
#'     Example: list(carat = list(breaks = c(0.5, 1, 1.5), labels = c("<0.5", "[0.5,1)", "[1,1.5)", ">=1.5")))}
#'   \item{regrouping_configs}{Reactive returning a named list of regrouping configurations
#'     for categorical grouping columns. Passed through from group_builder_server().
#'     Example: list(cut = list(mapping = list("Ideal" = "Premium"), group_unmapped_as_other = TRUE))}
#' }
#' 
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   selected_table <- reactive("diamonds")
#'   col_info <- reactive({
#'     load_demo_column_info(selected_table())
#'   })
#'   
#'   summary <- summary_builder_server(
#'     "my_summary",
#'     selected_table,
#'     col_info
#'   )
#'   
#'   # Use the return values
#'   observe({
#'     if (summary$needs_summary()) {
#'       specs <- summary$summary_specs()
#'       groups <- summary$group_vars()
#'       # Build query with these specifications...
#'     }
#'   })
#' }
#' }
#' 
#' @seealso [summary_builder_ui()] for the UI components, [group_builder_server()] for grouping logic
#' @export
summary_builder_server <- function(id, selected_table_name, column_info) {
  moduleServer(id, function(input, output, session) {
    
    # Update available columns when table changes
    observe({
      req(selected_table_name(), column_info())
      col_info <- column_info()
      
      # Get numeric columns from metadata for metrics
      numeric_cols <- col_info$metadata %>%
        filter(.data$column_type == "numeric") %>%
        pull(.data$column_name)
      
      updateSelectizeInput(
        session,
        "metrics",
        choices = numeric_cols,
        selected = character(0)
      )
    })
    
    grouping_results <- group_builder_server(
      "grouping",
      selected_table_name = selected_table_name,
      column_info = column_info
    )
    
    # Build summary specifications
    summary_specs <- reactive({
      specs <- list()
      
      # Always add count if selected
      if (input$include_count) {
        specs[[length(specs) + 1]] <- list(
          metric = "*",
          func = "count",
          sql = "n"  # Use dplyr's n() function
        )
      }
      
      # Add metric-function combinations if any metrics selected
      if (length(input$metrics) > 0 && length(input$functions) > 0) {
        for (metric in input$metrics) {
          for (func in input$functions) {
            specs[[length(specs) + 1]] <- list(
              metric = metric,
              func = func,
              sql = func  # Use dplyr function names directly
            )
          }
        }
      }
      
      specs
    })
    
    # Determine if summarization is needed
    needs_summary <- reactive({
      # Check if count is included
      count_included <- input$include_count
      
      # Check if any metrics are selected with functions
      has_metrics <- length(input$metrics) > 0 && length(input$functions) > 0
      
      # Return TRUE if either condition is met
      count_included || has_metrics
    })
    
    list(
      summary_specs = summary_specs,
      group_vars = grouping_results$group_vars,
      needs_summary = needs_summary,
      banding_configs = grouping_results$banding_configs,
      regrouping_configs = grouping_results$regrouping_configs
    )
  })
}

#' Demo app for summary_builder module
#' 
#' @description
#' Launches an interactive Shiny app demonstrating the summary_builder module with
#' the pre-packaged demo datasets (diamonds, iris, gapdata). Users can select a dataset,
#' configure summary statistics, choose grouping columns, and see all the values
#' returned by the module.
#' 
#' The demo shows all five return values from summary_builder_server():
#' \itemize{
#'   \item summary_specs - List of summary specifications (metric, func, sql)
#'   \item group_vars - Character vector of grouping columns
#'   \item needs_summary - Logical indicating if summarization is configured
#'   \item banding_configs - Numeric banding configurations from group_builder
#'   \item regrouping_configs - Categorical regrouping configurations from group_builder
#' }
#' 
#' @return A Shiny app object (run interactively, no return value)
#' 
#' @examples
#' \dontrun{
#' # Launch the demo app
#' summary_builder_demo()
#' }
#' 
#' @export
summary_builder_demo <- function() {
  
  pool <- get_demo_pool()
  
  ui <- fluidPage(
    titlePanel("Summary Builder Demo"),
    
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h3("Dataset Selection"),
          selectInput(
            "table_select",
            "Choose a dataset:",
            choices = c("diamonds", "iris", "gapdata"),
            selected = "diamonds"
          ),
          hr(),
          h3("Summary Configuration"),
          summary_builder_ui("demo_summary")
        )
      ),
      column(
        width = 6,
        wellPanel(
          h3("Module Return Values"),
          p(style = "font-size: 0.9em; color: #666;", 
            "Shows the five reactive values returned by summary_builder_server()"),
          verbatimTextOutput("module_returns")
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    selected_table <- reactive({
      req(input$table_select)
      input$table_select
    })
    
    column_info <- reactive({
      req(selected_table())
      load_demo_column_info(selected_table())
    })
    
    summary <- summary_builder_server(
      "demo_summary",
      selected_table,
      column_info
    )
    
    output$module_returns <- renderPrint({
      specs <- summary$summary_specs()
      groups <- summary$group_vars()
      needs <- summary$needs_summary()
      banding <- summary$banding_configs()
      regrouping <- summary$regrouping_configs()
      
      cat("=== summary_specs (reactive) ===\n")
      if (length(specs) == 0) {
        cat("Empty list (no summary configured)\n")
      } else {
        cat("List with", length(specs), "specification(s):\n\n")
        for (i in seq_along(specs)) {
          spec <- specs[[i]]
          cat("[[", i, "]]\n", sep = "")
          cat("  $metric: ", spec$metric, "\n", sep = "")
          cat("  $func: ", spec$func, "\n", sep = "")
          cat("  $sql: ", spec$sql, "\n\n", sep = "")
        }
      }
      
      cat("=== group_vars (reactive) ===\n")
      if (is.null(groups) || length(groups) == 0) {
        cat("NULL or empty character vector\n")
      } else {
        cat("Character vector:\n")
        print(groups)
      }
      
      cat("\n=== needs_summary (reactive) ===\n")
      cat("Logical: ", needs, "\n", sep = "")
      
      cat("\n=== banding_configs (reactive) ===\n")
      if (length(banding) == 0) {
        cat("Empty list (no banding configured)\n")
      } else {
        cat("Named list with", length(banding), "element(s):\n\n")
        for (col_name in names(banding)) {
          cat("$", col_name, "\n", sep = "")
          cat("  $breaks: ", paste(banding[[col_name]]$breaks, collapse = ", "), "\n", sep = "")
          cat("  $labels: ", paste(banding[[col_name]]$labels, collapse = " | "), "\n\n", sep = "")
        }
      }
      
      cat("=== regrouping_configs (reactive) ===\n")
      if (length(regrouping) == 0) {
        cat("Empty list (no regrouping configured)\n")
      } else {
        cat("Named list with", length(regrouping), "element(s):\n\n")
        for (col_name in names(regrouping)) {
          cat("$", col_name, "\n", sep = "")
          cat("  $mapping:\n")
          mapping <- regrouping[[col_name]]$mapping
          for (orig_val in names(mapping)) {
            cat("    '", orig_val, "' -> '", mapping[[orig_val]], "'\n", sep = "")
          }
          cat("  $group_unmapped_as_other: ", 
              regrouping[[col_name]]$group_unmapped_as_other, "\n\n", sep = "")
        }
      }
    })
    
    onStop(function() {
      pool::poolClose(pool)
    })
  }
  
  shinyApp(ui, server)
}


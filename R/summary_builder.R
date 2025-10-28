#' Create summary builder UI components
#'
#' @param id Character. The module ID
#' @return A list of Shiny UI elements
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

#' Create summary builder server
#'
#' @param id Character. The module ID
#' @param selected_table_name Reactive. Selected table from table_picker
#' @param column_info Reactive. Column info list containing metadata and distinct values
#' @return List of reactive expressions containing summary specifications and grouping variables
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

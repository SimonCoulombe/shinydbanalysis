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
      
      # Optional grouping
      div(
        style = "margin-top: 15px;",
        selectizeInput(
          ns("group_vars"),
          "Optional: Group by columns",
          choices = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Select columns to group by (optional)"
          )
        ),
        uiOutput(ns("banding_ui"))
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
        filter(column_type == "numeric") %>%
        pull(column_name)
      
      # Get all columns for grouping (including numeric for banding)
      groupable_cols <- col_info$metadata %>%
        pull(column_name)
      
      updateSelectizeInput(
        session,
        "metrics",
        choices = numeric_cols,
        selected = character(0)
      )
      
      updateSelectizeInput(
        session,
        "group_vars",
        choices = groupable_cols,
        selected = character(0)
      )
    })
    
    # Render banding UI for numeric grouping variables
    output$banding_ui <- renderUI({
      if (is.null(input$group_vars) || length(input$group_vars) == 0) {
        return(NULL)
      }
      
      col_info <- column_info()
      
      # Check which group vars are numeric
      numeric_group_vars <- col_info$metadata %>%
        filter(column_name %in% input$group_vars, column_type == "numeric") %>%
        pull(column_name)
      
      if (length(numeric_group_vars) == 0) {
        return(NULL)
      }
      
      # Create banding UI for each numeric grouping variable
      lapply(numeric_group_vars, function(var_name) {
        div(
          style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-left: 3px solid #007bff;",
          h5(paste("Banding for:", var_name), style = "margin-top: 0;"),
          checkboxInput(
            session$ns(paste0("use_banding_", var_name)),
            "Use numeric banding",
            value = FALSE
          ),
          conditionalPanel(
            condition = sprintf("input['%s']", session$ns(paste0("use_banding_", var_name))),
            textInput(
              session$ns(paste0("band_breaks_", var_name)),
              "Breakpoints (comma-separated):",
              value = "",
              placeholder = "e.g., 2,3,4"
            ),
            helpText("Enter numeric breakpoints to create bands. Example: '2,3,4' creates bands: <2, [2,3), [3,4), ≥4")
          )
        )
      })
    })
    
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
    
    # Build banding configurations for numeric grouping variables
    banding_configs <- reactive({
      if (is.null(input$group_vars) || length(input$group_vars) == 0) {
        return(list())
      }
      
      col_info <- column_info()
      
      # Check which group vars are numeric
      numeric_group_vars <- col_info$metadata %>%
        filter(column_name %in% input$group_vars, column_type == "numeric") %>%
        pull(column_name)
      
      if (length(numeric_group_vars) == 0) {
        return(list())
      }
      
      configs <- list()
      
      for (var_name in numeric_group_vars) {
        use_banding_input <- input[[paste0("use_banding_", var_name)]]
        breaks_input <- input[[paste0("band_breaks_", var_name)]]
        
        if (isTRUE(use_banding_input) && !is.null(breaks_input) && nzchar(trimws(breaks_input))) {
          breaks_text <- trimws(breaks_input)
          
          breaks <- tryCatch({
            as.numeric(unlist(strsplit(breaks_text, ",")))
          }, warning = function(w) NULL, error = function(e) NULL)
          
          if (!is.null(breaks) && !any(is.na(breaks)) && length(breaks) > 0) {
            breaks <- sort(unique(breaks))
            configs[[var_name]] <- list(
              breaks = breaks,
              labels = create_band_labels(breaks)
            )
          }
        }
      }
      
      configs
    })
    
    # Return interface
    list(
      summary_specs = summary_specs,  #named list where each element has a metric, func and sql component 
      group_vars = reactive(input$group_vars),  #  optional list of variables to group on
      needs_summary = needs_summary,  # boolean, indicating we need to generate a summary (count or other metrics  is selected)
      banding_configs = banding_configs  # list of banding configurations for numeric grouping variables
    )
  })
}

create_band_labels <- function(breaks) {
  n <- length(breaks)
  labels <- character(n + 1)
  
  labels[1] <- paste0("<", breaks[1])
  
  if (n > 1) {
    for (i in seq_len(n - 1)) {
      labels[i + 1] <- paste0("[", breaks[i], ",", breaks[i + 1], ")")
    }
  }
  
  labels[n + 1] <- paste0(">=", breaks[n])
  
  labels
}

#' Create group builder UI components
#'
#' @param id Character. The module ID
#' @return A list of Shiny UI elements
#' @export
group_builder_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      selectizeInput(
        ns("group_vars"),
        "Group by columns:",
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Select columns to group by (optional)"
        )
      ),
      uiOutput(ns("banding_ui"))
    )
  )
}

#' Create group builder server
#'
#' @param id Character. The module ID
#' @param selected_table_name Reactive. Selected table from table_picker
#' @param column_info Reactive. Column info list containing metadata and distinct values
#' @param acceptable_dimensions Optional reactive. If provided, restricts groupable columns to this list
#' @return List of reactive expressions containing grouping variables, banding configs, and regrouping configs
#' @export
group_builder_server <- function(id, selected_table_name, column_info, acceptable_dimensions = NULL) {
  
  stopifnot(is.reactive(selected_table_name))
  stopifnot(is.reactive(column_info))
  if (!is.null(acceptable_dimensions)) {
    stopifnot(is.reactive(acceptable_dimensions))
  }
  
  moduleServer(id, function(input, output, session) {
    
    observe({
      req(selected_table_name(), column_info())
      col_info <- column_info()
      
      groupable_cols <- col_info$metadata %>%
        pull(.data$column_name)
      
      if (!is.null(acceptable_dimensions)) {
        acceptable_dims <- acceptable_dimensions()
        if (!is.null(acceptable_dims) && length(acceptable_dims) > 0) {
          groupable_cols <- intersect(groupable_cols, acceptable_dims)
        }
      }
      
      updateSelectizeInput(
        session,
        "group_vars",
        choices = groupable_cols,
        selected = character(0)
      )
    })
    
    output$banding_ui <- renderUI({
      if (is.null(input$group_vars) || length(input$group_vars) == 0) {
        return(NULL)
      }
      
      col_info <- column_info()
      
      numeric_group_vars <- col_info$metadata %>%
        filter(.data$column_name %in% input$group_vars, .data$column_type == "numeric") %>%
        pull(.data$column_name)
      
      categorical_group_vars <- col_info$metadata %>%
        filter(.data$column_name %in% input$group_vars, .data$column_type == "categorical") %>%
        pull(.data$column_name)
      
      ui_elements <- list()
      
      if (length(numeric_group_vars) > 0) {
        numeric_ui <- lapply(numeric_group_vars, function(var_name) {
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
        ui_elements <- c(ui_elements, numeric_ui)
      }
      
      if (length(categorical_group_vars) > 0) {
        categorical_ui <- lapply(categorical_group_vars, function(var_name) {
          cat_values <- col_info$distinct_values %>%
            filter(.data$column_name == var_name) %>%
            pull(.data$value)
          
          if (length(cat_values) == 0) {
            return(NULL)
          }
          
          values_text <- paste(cat_values, collapse = ", ")
          
          div(
            style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border-left: 3px solid #ffc107;",
            h5(paste("Regrouping for:", var_name), style = "margin-top: 0;"),
            div(
              style = "margin-bottom: 10px; padding: 8px; background-color: white; border-radius: 4px; font-size: 0.9em;",
              strong("Available values: "),
              span(style = "color: #666;", values_text)
            ),
            checkboxInput(
              session$ns(paste0("use_regrouping_", var_name)),
              "Enable category regrouping",
              value = FALSE
            ),
            conditionalPanel(
              condition = sprintf("input['%s']", session$ns(paste0("use_regrouping_", var_name))),
              textAreaInput(
                session$ns(paste0("regroup_mapping_", var_name)),
                "Group mappings (one per line):",
                value = "",
                placeholder = "New Group 1: value1, value2, value3\nNew Group 2: value4, value5",
                rows = 5,
                width = "100%"
              ),
              div(
                style = "margin-top: 5px; margin-bottom: 10px;",
                checkboxInput(
                  session$ns(paste0("group_unmapped_as_other_", var_name)),
                  "Group all unmapped values as 'Other'",
                  value = FALSE
                )
              ),
              helpText("Format: 'GroupName: value1, value2, value3'. Each line creates a new group.")
            )
          )
        })
        ui_elements <- c(ui_elements, categorical_ui)
      }
      
      if (length(ui_elements) > 0) {
        tagList(ui_elements)
      } else {
        NULL
      }
    })
    
    banding_configs <- reactive({
      if (is.null(input$group_vars) || length(input$group_vars) == 0) {
        return(list())
      }
      
      col_info <- column_info()
      
      numeric_group_vars <- col_info$metadata %>%
        filter(.data$column_name %in% input$group_vars, .data$column_type == "numeric") %>%
        pull(.data$column_name)
      
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
    
    regrouping_configs <- reactive({
      if (is.null(input$group_vars) || length(input$group_vars) == 0) {
        return(list())
      }
      
      col_info <- column_info()
      
      categorical_group_vars <- col_info$metadata %>%
        filter(.data$column_name %in% input$group_vars, .data$column_type == "categorical") %>%
        pull(.data$column_name)
      
      if (length(categorical_group_vars) == 0) {
        return(list())
      }
      
      configs <- list()
      
      for (var_name in categorical_group_vars) {
        use_regrouping_input <- input[[paste0("use_regrouping_", var_name)]]
        mapping_input <- input[[paste0("regroup_mapping_", var_name)]]
        group_unmapped_as_other <- input[[paste0("group_unmapped_as_other_", var_name)]]
        
        if (isTRUE(use_regrouping_input) && !is.null(mapping_input) && nzchar(trimws(mapping_input))) {
          mapping_text <- trimws(mapping_input)
          
          mapping <- tryCatch({
            parse_regrouping_mapping(mapping_text)
          }, warning = function(w) NULL, error = function(e) NULL)
          
          if (!is.null(mapping) && length(mapping) > 0) {
            configs[[var_name]] <- list(
              mapping = mapping,
              group_unmapped_as_other = isTRUE(group_unmapped_as_other)
            )
          }
        }
      }
      
      configs
    })
    
    list(
      group_vars = reactive(input$group_vars),
      banding_configs = banding_configs,
      regrouping_configs = regrouping_configs
    )
  })
}

parse_regrouping_mapping <- function(mapping_text) {
  lines <- strsplit(mapping_text, "\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]
  
  mapping <- list()
  
  for (line in lines) {
    parts <- strsplit(line, ":")[[1]]
    
    if (length(parts) != 2) {
      next
    }
    
    group_name <- trimws(parts[1])
    values_text <- trimws(parts[2])
    
    if (!nzchar(group_name) || !nzchar(values_text)) {
      next
    }
    
    values <- strsplit(values_text, ",")[[1]]
    values <- trimws(values)
    values <- values[nzchar(values)]
    
    if (length(values) > 0) {
      for (val in values) {
        mapping[[val]] <- group_name
      }
    }
  }
  
  mapping
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

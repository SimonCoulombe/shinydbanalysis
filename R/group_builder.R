#' Create group builder UI components
#' 
#' @description
#' Creates the user interface for the group builder module. This module allows users
#' to select columns to group by and configure advanced grouping options such as:
#' \itemize{
#'   \item Numeric banding - creating ranges from continuous numeric columns
#'   \item Category regrouping - combining categorical values into new groups
#' }
#'
#' @param id Character. The module ID used to create a unique namespace for this instance
#' 
#' @return A Shiny tagList containing:
#' \describe{
#'   \item{selectizeInput}{A multi-select dropdown for choosing grouping columns}
#'   \item{uiOutput}{A dynamic UI container that renders banding/regrouping controls
#'     based on the selected columns and their types}
#' }
#' 
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   group_builder_ui("my_groups")
#' )
#' }
#' 
#' @seealso [group_builder_server()] for the server-side logic
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

#' Create group builder server logic
#' 
#' @description
#' Server-side logic for the group builder module. This function manages column selection
#' for grouping operations and provides advanced transformation options:
#' 
#' **Numeric Banding**: Converts continuous numeric columns into categorical bands using
#' user-specified breakpoints. For example, age values can be banded into groups like
#' "<18", "[18,30)", "[30,50)", ">=50".
#' 
#' **Category Regrouping**: Combines multiple categorical values into new groups. For
#' example, regrouping colors "Red, Orange, Yellow" into "Warm Colors" and "Blue, Green"
#' into "Cool Colors".
#' 
#' The module automatically:
#' \itemize{
#'   \item Updates available columns when the table changes
#'   \item Restricts columns based on acceptable_dimensions if provided
#'   \item Shows banding controls only for numeric columns
#'   \item Shows regrouping controls only for categorical columns
#'   \item Displays available categorical values to help with regrouping
#'   \item Validates user input for breakpoints and mappings
#' }
#'
#' @param id Character. The module ID matching the UI function
#' @param selected_table_name Reactive expression returning the currently selected table name
#' @param column_info Reactive expression returning a list with:
#'   \describe{
#'     \item{metadata}{Dataframe with column_name, column_type, etc.}
#'     \item{distinct_values}{Dataframe with column_name and value for categorical columns}
#'   }
#' @param acceptable_dimensions Optional reactive expression returning a character vector
#'   of column names to restrict the groupable columns. If NULL (default), all columns
#'   from the table are available for grouping.
#'   
#' @return A named list with three reactive expressions:
#' \describe{
#'   \item{group_vars}{Reactive returning a character vector of selected column names
#'     to group by. Returns NULL or empty vector when no columns are selected.
#'     Example: c("cut", "color")}
#'   \item{banding_configs}{Reactive returning a named list of banding configurations
#'     for numeric columns. Each element is named by column and contains:
#'     \itemize{
#'       \item breaks: Numeric vector of breakpoint values (sorted and unique)
#'       \item labels: Character vector of band labels created from breaks
#'     }
#'     Returns empty list when no banding is configured.
#'     Example: list(carat = list(breaks = c(0.5, 1, 1.5), 
#'                                 labels = c("<0.5", "[0.5,1)", "[1,1.5)", ">=1.5")))}
#'   \item{regrouping_configs}{Reactive returning a named list of regrouping configurations
#'     for categorical columns. Each element is named by column and contains:
#'     \itemize{
#'       \item mapping: Named list where names are original values and values are new group names
#'       \item group_unmapped_as_other: Logical indicating whether unmapped values should be grouped as "Other"
#'     }
#'     Returns empty list when no regrouping is configured.
#'     Example: list(cut = list(mapping = list("Ideal" = "Premium", "Premium" = "Premium", 
#'                                              "Good" = "Standard", "Fair" = "Standard"),
#'                              group_unmapped_as_other = TRUE))}
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
#'   groups <- group_builder_server(
#'     "my_groups",
#'     selected_table,
#'     col_info,
#'     acceptable_dimensions = reactive(c("cut", "color", "clarity", "carat"))
#'   )
#'   
#'   # Use the grouping configuration
#'   grouped_data <- reactive({
#'     group_cols <- groups$group_vars()
#'     if (length(group_cols) > 0) {
#'       tbl(pool, selected_table()) %>%
#'         group_by(across(all_of(group_cols)))
#'     } else {
#'       tbl(pool, selected_table())
#'     }
#'   })
#' }
#' }
#' 
#' @seealso [group_builder_ui()] for the UI components
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


#' Demo app for group_builder module
#' 
#' @description
#' Launches an interactive Shiny app demonstrating the group_builder module with
#' the pre-packaged demo datasets (diamonds, iris, gapdata). Users can select a dataset,
#' choose columns to group by, configure numeric banding and categorical regrouping,
#' and see the returned values from the module.
#' 
#' The demo shows all three return values from group_builder_server():
#' \itemize{
#'   \item group_vars - The selected grouping columns
#'   \item banding_configs - Numeric banding configurations (breaks and labels)
#'   \item regrouping_configs - Categorical regrouping configurations (mappings)
#' }
#' 
#' @return A Shiny app object (run interactively, no return value)
#' 
#' @examples
#' \dontrun{
#' # Launch the demo app
#' group_builder_demo()
#' }
#' 
#' @export
group_builder_demo <- function() {
  
  pool <- get_demo_pool()
  
  ui <- fluidPage(
    titlePanel("Group Builder Demo"),
    
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
          h3("Grouping Configuration"),
          group_builder_ui("demo_groups")
        )
      ),
      column(
        width = 6,
        wellPanel(
          h3("Module Return Values"),
          p(style = "font-size: 0.9em; color: #666;", 
            "Shows the three reactive values returned by group_builder_server()"),
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
    
    groups <- group_builder_server(
      "demo_groups",
      selected_table,
      column_info,
      acceptable_dimensions = NULL
    )
    
    output$module_returns <- renderPrint({
      group_cols <- groups$group_vars()
      banding <- groups$banding_configs()
      regrouping <- groups$regrouping_configs()
      
      cat("=== group_vars (reactive) ===\n")
      if (is.null(group_cols) || length(group_cols) == 0) {
        cat("NULL or empty character vector\n")
      } else {
        cat("Character vector:\n")
        print(group_cols)
      }
      
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



#' Parse regrouping mapping from text input
#' 
#' @description
#' Internal helper function that parses user-provided text into a mapping of
#' original categorical values to new group names. The expected format is one
#' mapping per line: "NewGroupName: value1, value2, value3"
#' 
#' @param mapping_text Character string with line-separated mappings
#' 
#' @return Named list where names are original values and values are new group names.
#'   Returns empty list if parsing fails or no valid mappings found.
#'   Example: list("Ideal" = "Premium", "Premium" = "Premium", "Good" = "Standard")
#' @noRd
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

#' Create band labels from breakpoints
#' 
#' @description
#' Internal helper function that generates human-readable labels for numeric bands
#' created from breakpoints. Creates labels in the format: "<X", "[X,Y)", ">=Z"
#' 
#' @param breaks Numeric vector of breakpoint values (should be sorted and unique)
#' 
#' @return Character vector of band labels with length = length(breaks) + 1.
#'   Example: For breaks = c(0.5, 1, 1.5), returns c("<0.5", "[0.5,1)", "[1,1.5)", ">=1.5")
#' @noRd
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


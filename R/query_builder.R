
#' Create query builder server logic
#'
#' Builds a complete dbplyr query by combining filters, grouping, transformations (banding/regrouping),
#' and summarization specifications. 
#'
#' @param id Character. The module ID
#' @param pool Database connection pool
#' @param selected_table_name Reactive expression returning the selected table name as a character string
#' @param selected_tbl_ref_without_restricted_columns Reactive expression returning a tbl reference (dbplyr lazy table)
#' @param where_clause Reactive expression returning the WHERE clause as a character string
#' @param needs_summary Reactive expression returning a logical indicating if summarization is needed
#' @param group_vars Reactive expression returning a character vector of grouping variables
#' @param summary_specs Reactive expression returning a list of summary specifications
#' @param banding_configs Reactive expression returning a named list of banding configurations for numeric variables (optional)
#' @param regrouping_configs Reactive expression returning a named list of regrouping configurations for categorical variables (optional)
#'
#' @return A list with three reactive expressions:
#' \describe{
#'   \item{query}{Reactive returning a dbplyr query object (lazy tbl) with all transformations applied, or NULL if no table selected}
#'   \item{needs_summary}{Reactive returning the logical flag passed in (pass-through)}
#'   \item{error}{Reactive returning error message string or NULL if no error}
#' }
#'
#' @details
#' The query is built in this order:
#' 1. Start with base table reference
#' 2. Apply WHERE clause filters
#' 3. Apply banding transformations (e.g., age ranges)
#' 4. Apply regrouping transformations (e.g., combine categories)
#' 5. Apply GROUP BY if needed
#' 6. Apply summary functions (mean, sum, count, etc.)
#'
#' All operations use dbplyr, so the query remains lazy until explicitly collected.
#'
#' @export
query_builder_server <- function(id, pool, selected_table_name, selected_tbl_ref_without_restricted_columns, where_clause, needs_summary, group_vars, summary_specs, banding_configs = NULL, regrouping_configs = NULL) {
  moduleServer(id, function(input, output, session) {
    # State management
    error_state <- reactiveVal(NULL)
    
    # Build query using dbplyr
    query <- reactive({
      table <- selected_table_name()
      
      if (is.null(table) || !nzchar(table)) {
        return(NULL)
      }
      
      tryCatch({
        # Get base table reference
        query <- selected_tbl_ref_without_restricted_columns()
        
        # Apply filters if any
        if (!is.null(where_clause() ) && nzchar(where_clause())) {
          filter_expr <- parse_filter_expression(where_clause())
          query <- filter(query, !!filter_expr)
        }
        
        # Apply banding transformations before grouping
        if (!is.null(banding_configs) && length(banding_configs()) > 0) {
          for (var_name in names(banding_configs())) {
            config <- banding_configs()[[var_name]]
            band_expr <- create_banding_expression(var_name, config)
            query <- mutate(query, !!sym(var_name) := !!band_expr)
          }
        }
        
        # Apply regrouping transformations before grouping
        if (!is.null(regrouping_configs) && length(regrouping_configs()) > 0) {
          for (var_name in names(regrouping_configs())) {
            config <- regrouping_configs()[[var_name]]
            regroup_expr <- create_regrouping_expression(var_name, config)
            query <- mutate(query, !!sym(var_name) := !!regroup_expr)
          }
        }
        
        # Only apply summarization if specifically requested
        if (needs_summary()) {
          # Get grouping variables if any
          if (length(group_vars()) > 0) {
            query <- group_by(query, !!!syms(group_vars()))
          }
          
          # Apply summary specifications
          if (length(summary_specs()) > 0) {
            summary_exprs <- build_summary_expressions(summary_specs())
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
    
    # Return interface
    list(
      query = reactive(query()),
      needs_summary = reactive(needs_summary()),
      error = reactive(error_state())
    )
  })
}


# Helper Functions ----



#' Parse filter expression from WHERE clause
#'
#' Converts a SQL-like WHERE clause string into an R expression that can be used with dplyr::filter().
#' Replaces SQL operators (AND, OR) with R equivalents (&, |).
#'
#' @param where_clause Character string containing filter conditions in SQL-like syntax.
#'   Example: "price > 1000 AND cut %in% c('Ideal', 'Premium')"
#' @return A parsed R expression suitable for use with dplyr::filter()
#'
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
#'
#' Converts a list of summary specifications into quosures that can be passed to dplyr::summarise().
#' Each spec contains a function name (mean, sum, etc.) and optionally a metric column name.
#'
#' @param summary_specs List of summary specifications. Each spec should have:
#'   \itemize{
#'     \item func: Function name ("mean", "sum", "min", "max", "count")
#'     \item metric: Column name to summarize (not needed for "count")
#'   }
#' @return Named list of quosures suitable for use with dplyr::summarise()
#'
#' @details
#' Special handling for "count" which uses n() instead of a column name.
#' Other functions create expressions like mean(price), sum(quantity), etc.
#'
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


#' Create banding expression for numeric variables
#'
#' Creates a case_when expression to bin numeric values into labeled ranges.
#' For example, converting ages into "0-18", "18-65", "65+" or prices into "Low", "Medium", "High".
#'
#' @param var_name Character string with the variable name to be banded
#' @param config List with two elements:
#'   \itemize{
#'     \item breaks: Numeric vector of breakpoints (e.g., c(18, 65) for ages)
#'     \item labels: Character vector of labels, one more than breaks (e.g., c("0-18", "18-65", "65+"))
#'   }
#' @return A quosure containing a case_when expression that can be used with dplyr::mutate()
#'
#' @details
#' #The function creates conditions where:
#' #- First label: values < first break
#' #- Middle labels: values >= break(i) & values < break(i+1)
#' #- Last label: values >= last break
#'
#' @noRd
create_banding_expression <- function(var_name, config) {
  breaks <- config$breaks
  labels <- config$labels
  n <- length(breaks)
  
  conditions <- list()
  
  conditions[[1]] <- quo(!!sym(var_name) < !!breaks[1] ~ !!labels[1])
  
  if (n > 1) {
    for (i in seq_len(n - 1)) {
      conditions[[i + 1]] <- quo(!!sym(var_name) >= !!breaks[i] & !!sym(var_name) < !!breaks[i + 1] ~ !!labels[i + 1])
    }
  }
  
  conditions[[n + 1]] <- quo(!!sym(var_name) >= !!breaks[n] ~ !!labels[n + 1])
  
  quo(case_when(!!!conditions))
}

#' Create regrouping expression for categorical variables
#'
#' Creates a case_when expression to remap categorical values into new groups.
#' For example, combining multiple colors into broader categories like "Warm" and "Cool",
#' or grouping rare categories together as "Other".
#'
#' @param var_name Character string with the variable name to be regrouped
#' @param config Either:
#'   \itemize{
#'     \item A named list where names are original values and values are new group labels (legacy format)
#'     \item A list with two elements (new format):
#'       \itemize{
#'         \item mapping: Named list of original values to new group labels
#'         \item group_unmapped_as_other: Logical, if TRUE unmapped values become "Other", if FALSE they keep their original values
#'       }
#'   }
#' @return A quosure containing a case_when expression that can be used with dplyr::mutate()
#'
#' @details
#' The function creates conditions for each mapping, plus a default case:
#' - If group_unmapped_as_other is TRUE: unmapped values → "Other"
#' - If FALSE: unmapped values keep their original value
#'
#' @noRd
create_regrouping_expression <- function(var_name, config) {
  # Handle legacy format (plain mapping) or new format (list with mapping + flag)
  if (is.null(config$mapping)) {
    # Legacy format: config is the mapping itself
    mapping <- config
    group_unmapped_as_other <- FALSE
  } else {
    # New format: config has mapping and group_unmapped_as_other
    mapping <- config$mapping
    group_unmapped_as_other <- isTRUE(config$group_unmapped_as_other)
  }
  
  if (length(mapping) == 0) {
    return(quo(!!sym(var_name)))
  }
  
  conditions <- list()
  
  for (original_value in names(mapping)) {
    new_group <- mapping[[original_value]]
    conditions[[length(conditions) + 1]] <- quo(!!sym(var_name) == !!original_value ~ !!new_group)
  }
  
  # Add a default case: either "Other" or keep original values
  if (group_unmapped_as_other) {
    conditions[[length(conditions) + 1]] <- quo(TRUE ~ "Other")
  } else {
    conditions[[length(conditions) + 1]] <- quo(TRUE ~ !!sym(var_name))
  }
  
  quo(case_when(!!!conditions))
}

#' Generate column information for a table
#'
#' @param tablename Character. Name of the table to analyze
#' @param pool Pool object. Database connection pool
#' @param output_dir Character. Directory to save column info files. Defaults to "column_info"
#' @return List of column information (invisibly)
#' 

#' @export
create_column_info <- function(tablename, pool, output_dir = "column_info") {
  # Input validation
  if (!is.character(tablename) || length(tablename) != 1) {
    stop("tablename must be a single character string")
  }
  
  if (!inherits(pool, "Pool")) {
    stop("pool must be a valid database connection pool")
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  tryCatch({
    # Verify table exists
    tables <- dbListTables(pool)
    if (!tablename %in% tables) {
      stop(sprintf("Table '%s' not found in database", tablename))
    }
    
    # Get table reference using dbplyr
    tbl_ref <- tbl(pool, tablename)
    
    # Get column types by examining first row
    sample_data <- tbl_ref %>% 
      head(1) %>% 
      collect()
    
    column_types <- lapply(sample_data, class)
    
    # Get total number of rows
    n_total <- tbl_ref %>%
      summarise(count = n()) %>%
      pull(count)
    
    # Create column info list
    col_info <- lapply(names(column_types), function(col) {
      tryCatch({
        col_class <- column_types[[col]][1]  # Get primary class
        
        # Handle different types
        if (col_class == "Date" || col_class == "POSIXct" || col_class == "POSIXt") {
          # Date type
          range_values <- tbl_ref %>%
            summarise(
              min = min(.data[[col]], na.rm = TRUE),
              max = max(.data[[col]], na.rm = TRUE)
            ) %>%
            collect()
          
          list(
            name = col,
            type = "date",
            values = list(
              min = as.Date(range_values$min),
              max = as.Date(range_values$max)
            )
          )
        } else if (col_class %in% c("character", "factor") || 
                   (col_class == "integer" && {
                     # Check if integer column should be categorical
                     n_distinct <- tbl_ref %>%
                       summarise(n = n_distinct(.data[[col]])) %>%
                       pull(n)
                     n_distinct <= 20 && n_distinct <= 0.1 * n_total
                   })) {
          # Categorical type
          distinct_values <- tbl_ref %>%
            filter(!is.na(.data[[col]])) %>%
            select(all_of(col)) %>%
            distinct() %>%
            arrange(.data[[col]]) %>%
            collect() %>%
            pull(col)
          
          list(
            name = col,
            type = "categorical",
            values = distinct_values
          )
        } else {
          # Numeric type (includes integer and numeric)
          range_values <- tbl_ref %>%
            summarise(
              min = min(.data[[col]], na.rm = TRUE),
              max = max(.data[[col]], na.rm = TRUE)
            ) %>%
            collect()
          
          list(
            name = col,
            type = "numeric",
            values = list(
              min = as.numeric(range_values$min),
              max = as.numeric(range_values$max)
            )
          )
        }
      }, error = function(e) {
        warning(sprintf("Error processing column %s: %s", col, e$message))
        # Return a default numeric type if processing fails
        list(
          name = col,
          type = "numeric",
          values = list(min = 0, max = 1)
        )
      })
    })
    
    names(col_info) <- names(column_types)
    
    # Print detected types
    message("Detected column types:")
    message("Original SQL types:")
    for(col in names(column_types)) {
      message(sprintf("%s: %s", col, paste(column_types[[col]], collapse = ", ")))
    }
    message("\nMapped types:")
    for(col in names(col_info)) {
      message(sprintf("%s: %s", col, col_info[[col]]$type))
    }
    
    # Save to file
    output_file <- file.path(output_dir, sprintf("column_info_%s.rds", tablename))
    saveRDS(col_info, output_file)
    message(sprintf("\nColumn info saved to: %s", output_file))
    
    invisible(col_info)
    
  }, error = function(e) {
    stop(sprintf("Error creating column info: %s", e$message))
  })
}

## helper functions -----

#' Detect if a column contains date values
#' @noRd
is_date <- function(values) {
  # First check if it's already a Date class
  if (inherits(values, "Date")) return(TRUE)
  
  # If numeric, check if it could be a date
  if (is.numeric(values)) {
    # Try to convert from numeric (assuming origin "1970-01-01")
    tryCatch({
      dates <- as.Date(values, origin = "1970-01-01")
      # Check if dates are reasonable (e.g., between 1900 and 2100)
      all(format(dates, "%Y") >= 1900 & format(dates, "%Y") <= 2100)
    }, error = function(e) FALSE)
  } else if (is.character(values)) {
    # Try to parse as date with common formats
    tryCatch({
      as.Date(values[1])
      TRUE
    }, error = function(e) FALSE)
  } else {
    FALSE
  }
}

#' Determine if a column should be categorical
#'
#' @param values Vector of column values
#' @param n_distinct Number of distinct values
#' @param n_total Total number of rows
#' @return Logical indicating if column should be treated as categorical
#' @noRd
is_categorical <- function(values, n_distinct, n_total) {
  # Remove NAs
  values <- values[!is.na(values)]
  if (length(values) == 0) return(FALSE)
  
  # Rules for categorical:
  # 1. If all values are character/factor/Date
  # 2. If numeric but few distinct values compared to total rows
  if (is.character(values) || is.factor(values)) {
    return(TRUE)
  } else if (is.numeric(values)) {
    return(n_distinct <= 20 && n_distinct <= 0.1 * n_total)
  }
  
  return(FALSE)
}

# Example usage:
if (FALSE) {
  library(pool)
  library(RSQLite)

  # Create a connection pool
  pool <- dbPool(
    drv = RSQLite::SQLite(),
    dbname = "database.db"
  )

  # Create column info for iris table
  create_column_info("iris", pool)

  # Create column info for mtcars table
  create_column_info("mtcars", pool)

  # Don't forget to close the pool when done
  poolClose(pool)
}

#' Generate column information for a table
#'
#' @param tablename Character. Name of the table to analyze
#' @param pool Pool object. Database connection pool
#' @param output_dir Character. Directory to save column info files. Defaults to "column_info"
#' @return List of column information (invisibly)
#' @importFrom pool dbPool poolClose
#' @importFrom DBI dbListTables dbWriteTable dbListFields
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

  # Use pool connection with error handling
  tryCatch({
    # Verify table exists
    tables <- dbListTables(pool)
    if (!tablename %in% tables) {
      stop(sprintf("Table '%s' not found in database", tablename))
    }

    # Get column names
    cols <- dbListFields(pool, tablename)

    # Get total number of rows
    n_total <- DBI::dbGetQuery(pool, sprintf("SELECT COUNT(*) as count FROM [%s]", tablename))$count

    # Create column info list
    col_info <- lapply(cols, function(col) {
      tryCatch({
        # Get sample of values and count distinct values
        values_query <- sprintf(
          "SELECT [%s], COUNT(DISTINCT [%s]) as n_distinct
           FROM [%s]
           WHERE [%s] IS NOT NULL
           GROUP BY [%s]
           LIMIT 1000",
          col, col, tablename, col, col
        )
        sample_data <- DBI::dbGetQuery(pool, values_query)

        n_distinct <- DBI::dbGetQuery(
          pool,
          sprintf("SELECT COUNT(DISTINCT [%s]) as n FROM [%s]", col, tablename)
        )$n

        # Get the first column (the values)
        values <- sample_data[[1]]

        if (is_categorical(values, n_distinct, n_total)) {
          # Categorical type
          distinct_query <- sprintf(
            "SELECT DISTINCT [%s] FROM [%s] WHERE [%s] IS NOT NULL ORDER BY [%s]",
            col, tablename, col, col
          )
          distinct_values <- DBI::dbGetQuery(pool, distinct_query)[[1]]

          list(
            name = col,
            type = "categorical",
            values = distinct_values
          )
        } else {
          # Numeric type
          range_query <- sprintf(
            "SELECT MIN([%s]) as min, MAX([%s]) as max FROM [%s]",
            col, col, tablename
          )
          range_values <- DBI::dbGetQuery(pool, range_query)

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

    names(col_info) <- cols

    # Print detected types
    message("Detected column types:")
    for(col in names(col_info)) {
      message(sprintf("%s: %s", col, col_info[[col]]$type))
    }

    # Save to file
    output_file <- file.path(output_dir, sprintf("column_info_%s.rds", tablename))
    saveRDS(col_info, output_file)
    message(sprintf("Column info saved to: %s", output_file))

    invisible(col_info)

  }, error = function(e) {
    stop(sprintf("Error creating column info: %s", e$message))
  })
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
  # 1. If all values are character/factor
  # 2. If numeric but few distinct values compared to total rows
  # (less than 10% of total rows and less than 20 distinct values)
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

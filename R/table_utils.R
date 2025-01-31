
#' Parse schema and table name
#' @param full_table_name Character string that might contain schema (e.g., "schema.table")
#' @return List with schema and table components
#' @noRd
parse_table_name <- function(full_table_name) {
  parts <- strsplit(full_table_name, "\\.")[[1]]
  if (length(parts) == 2) {
    list(schema = parts[1], table = parts[2])
  } else {
    list(schema = NULL, table = parts[1])
  }
}

#' Create safe filename from table name
#' @param full_table_name Character string that might contain schema
#' @return Safe filename string
#' @noRd
make_safe_filename <- function(full_table_name) {
  gsub("\\.", "_", full_table_name)
}

#' Create table reference using schema if present
#' @param pool Database connection pool
#' @param table_info List containing schema and table names
#' @return A dbplyr table reference
#' @importFrom dplyr tbl
#' @noRd
create_table_ref <- function(pool, table_info) {
  if (!is.null(table_info$schema)) {
    tbl(pool, dbplyr::in_schema(table_info$schema, table_info$table))
  } else {
    tbl(pool, table_info$table)
  }
}

#' Check if table exists in database
#' @param pool Database connection pool
#' @param table_info List containing schema and table names
#' @return Boolean indicating if table exists
#' @noRd
table_exists <- function(pool, table_info) {
  tryCatch({
    ref <- create_table_ref(pool, table_info)
    TRUE
  }, error = function(e) {
    FALSE
  })
}


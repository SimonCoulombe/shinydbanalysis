
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

#' Check if table exists in database
#' @param pool Database connection pool
#' @param parsed_table_name List containing schema and table names
#' @return Boolean indicating if table exists
#' @noRd
table_exists <- function(pool, parsed_table_name) {
  tryCatch({
    ref <- create_table_ref(pool, parsed_table_name)
    TRUE
  }, error = function(e) {
    FALSE
  })
}


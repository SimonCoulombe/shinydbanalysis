#' Get path to demo database
#'
#' @return Character path to the demo DuckDB database
#' @export
#'
#' @examples
#' db_path <- get_demo_database_path()
get_demo_database_path <- function() {
  system.file("extdata", "demo.duckdb", package = "shinydbanalysis", mustWork = TRUE)
}

#' Get path to demo column info directory
#'
#' @return Character path to the demo column info directory
#' @export
#'
#' @examples
#' column_info_dir <- get_demo_column_info_path()
get_demo_column_info_path <- function() {
  system.file("extdata", "column_info", package = "shinydbanalysis", mustWork = TRUE)
}

#' Create demo database pool
#'
#' @return A pool connection to the demo database
#' @export
#'
#' @examples
#' if (interactive()) {
#'   pool <- get_demo_pool()
#'   DBI::dbListTables(pool)
#'   pool::poolClose(pool)
#' }
get_demo_pool <- function() {
  if (!requireNamespace("pool", quietly = TRUE)) {
    stop("Package 'pool' is required. Install it with: install.packages('pool')")
  }
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' is required. Install it with: install.packages('duckdb')")
  }
  
  db_path <- get_demo_database_path()
  
  pool::dbPool(
    drv = duckdb::duckdb(),
    dbdir = db_path,
    read_only = TRUE
  )
}

#' Get demo storage info
#'
#' @return List with storage configuration for demo data
#' @export
#'
#' @examples
#' storage_info <- get_demo_storage_info()
get_demo_storage_info <- function() {
  list(
    storage_type = "local",
    column_info_dir = get_demo_column_info_path(),
    adls_endpoint = NULL,
    adls_container = NULL,
    sas_token = NULL
  )
}

#' Load demo column info for a table
#'
#' @param tablename Character. Name of the table ("diamonds", "iris", or "gapdata")
#' @return List with metadata and distinct_values dataframes
#' @export
#'
#' @examples
#' column_info <- load_demo_column_info("diamonds")
#' head(column_info$metadata)
#' head(column_info$distinct_values)
load_demo_column_info <- function(tablename) {
  if (!tablename %in% c("diamonds", "iris", "gapdata")) {
    stop("tablename must be one of: 'diamonds', 'iris', 'gapdata'")
  }
  
  read_column_info(
    tablename = tablename,
    storage_type = "local",
    column_info_dir = get_demo_column_info_path()
  )
}

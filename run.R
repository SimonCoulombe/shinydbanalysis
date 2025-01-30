
# Load required packages
library(dbplyr)
library(dplyr)
library(shiny)
library(DBI)
library(pool)
library(RSQLite)
library(rlang)

# Source all module files
source("R/filter_module.R")
source("R/filter_builder.R")
source("R/group_builder.R")
source("R/summary_builder.R")
source("R/data_fetcher.R")
source("R/column_info_generator.R")
source("R/table_picker.R")
source("R/validation.R")

pool <- dbPool(
  drv = RSQLite::SQLite(),
  dbname = "database.db"
)

initialize_app <- function(pool) {
  message("Initializing application...")

  tables <- c("iris", "mtcars")
  # Create example database if it doesn't exist
  for (table in tables){
    if(!table %in% dbListTables(pool) ){
      dbWriteTable(pool, table, get(table))
    }

  }


  # Create column info directory if it doesn't exist
  if (!dir.exists("column_info")) {
    message("creating column_info directory")
    dir.create("column_info")
  }

  # Generate column info for each table
  message("Generating column information...")

  for (table in tables) {
    if (!file.exists(file.path("column_info", paste0("column_info_", table, ".rds")))) {
      message(sprintf("Generating column info for table: %s", table))
      create_column_info(table, pool)
    } else {
      message(sprintf("Column info already exists for table: %s", table))
    }
  }

  return(pool)
}
# Initialize the app and get the connection pool
initialize_app(pool)


# Run the app
source("R/app.R")
run_app(pool)

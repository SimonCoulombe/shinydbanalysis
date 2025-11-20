library(duckdb)
library(DBI)
library(pool)
library(ggplot2)
library(gapminder)
library(dplyr)
library(shinydbanalysis)

gapdata <- gapminder::gapminder %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  mutate(prout = NA_character_)

db_path <- "inst/extdata/demo.duckdb"

if (file.exists(db_path)) {
  file.remove(db_path)
}

con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

dbWriteTable(con, "diamonds", ggplot2::diamonds)
dbWriteTable(con, "iris", iris)
dbWriteTable(con, "gapdata", gapdata)

dbDisconnect(con, shutdown = TRUE)

pool <- dbPool(
  drv = duckdb::duckdb(),
  dbdir = db_path
)

create_column_info(
  "diamonds",
  pool,
  column_info_dir = "inst/extdata/column_info",
  storage_type = "local"
)

create_column_info(
  "iris",
  pool,
  column_info_dir = "inst/extdata/column_info",
  storage_type = "local"
)

create_column_info(
  "gapdata",
  pool,
  column_info_dir = "inst/extdata/column_info",
  storage_type = "local"
)

poolClose(pool)

message("Demo database and column info created successfully!")
message("Database: ", db_path)
message("Column info: inst/extdata/column_info/")

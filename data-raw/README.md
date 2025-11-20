# Demo Database Setup

This directory contains the script to generate the demo database and column info files that are packaged with the shinydbanalysis package.

## Files

- `create_demo_database.R` - Script to generate demo.duckdb and column info files

## Running the Setup

To regenerate the demo database and column info:

```r
source("data-raw/create_demo_database.R")
```

This will create:
- `inst/extdata/demo.duckdb` - DuckDB database with 3 tables (diamonds, iris, gapdata)
- `inst/extdata/column_info/*.parquet` - Column metadata and distinct values for each table

## Demo Tables

### diamonds (ggplot2::diamonds)
- 53,940 rows
- 10 columns (numeric, categorical, ordered factors)
- Great for testing performance with larger datasets

### iris 
- 150 rows  
- 5 columns (numeric and categorical)
- Small, simple dataset for basic testing

### gapdata (gapminder::gapminder + date column + NA column)
- 1,704 rows
- 7 columns including a date column and a column with all NAs
- Good for testing date filters and edge cases

## Usage in Package

The helper functions in `R/demo_helpers.R` provide easy access:

```r
# Get database path
db_path <- get_demo_database_path()

# Get column info path
col_info_path <- get_demo_column_info_path()

# Create a pool connection
pool <- get_demo_pool()

# Get storage info for modules
storage_info <- get_demo_storage_info()

# Load column info for a table
column_info <- load_demo_column_info("diamonds")
```

## Notes

- The database is opened in read-only mode by `get_demo_pool()`
- Column info includes metadata (types, ranges) and distinct values for categorical columns
- Regenerate after updating the create_column_info() function to ensure consistency

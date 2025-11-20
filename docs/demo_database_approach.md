# Pre-packaged Demo Database Approach

## Summary

We've set up a system to include a pre-built DuckDB database and column info files with the package. This makes it easy to create demo apps and tests without requiring users to set up their own database.

## Structure

```
shinydbanalysis/
├── inst/
│   └── extdata/
│       ├── demo.duckdb                    # Pre-built database with 3 tables
│       └── column_info/                   # Pre-computed column metadata
│           ├── column_info_diamonds.parquet
│           ├── column_info_iris.parquet
│           ├── column_info_gapdata.parquet
│           ├── distinct_values_diamonds.parquet
│           ├── distinct_values_iris.parquet
│           └── distinct_values_gapdata.parquet
├── data-raw/
│   ├── create_demo_database.R             # Script to regenerate demo data
│   └── README.md
└── R/
    ├── demo_helpers.R                     # Functions to access demo data
    └── filter_module.R                    # Updated with demo using real data
```

## Key Files Created

### 1. `data-raw/create_demo_database.R`
Script to generate the demo database and column info. Run this when:
- Setting up the package for the first time
- After updating the `create_column_info()` function
- When you want to change the demo tables

### 2. `R/demo_helpers.R`
Helper functions for accessing packaged demo data:
- `get_demo_database_path()` - Path to demo.duckdb
- `get_demo_column_info_path()` - Path to column_info directory
- `get_demo_pool()` - Create a pool connection (read-only)
- `get_demo_storage_info()` - Storage config for module functions
- `load_demo_column_info(tablename)` - Load column info for a table

### 3. `R/filter_module.R`
Updated `filter_module_demo()` to use real data from the packaged database by default.

## Usage in Demo Functions

```r
filter_module_demo <- function(use_real_data = TRUE) {
  # ...
  if (use_real_data) {
    load_demo_column_info("diamonds")  # Uses packaged data
  } else {
    # synthetic data
  }
}
```

## Usage in Tests

Tests can now use the packaged database:

```r
test_that("filter_module works with real data", {
  column_info <- load_demo_column_info("diamonds")
  
  # Test with actual column metadata
  testServer(filter_module_server, args = list(
    metadata = column_info$metadata %>% filter(column_name == "carat") %>% as.list(),
    distinct_values = column_info$distinct_values
  ), {
    # test code
  })
})
```

## Benefits

1. **No setup required** - Users can run demos immediately after installing
2. **Consistent test data** - All tests use the same baseline data
3. **Real-world examples** - Demos use actual database queries, not mocked data
4. **Offline capability** - No need for external database connections
5. **Fast execution** - Small database loads quickly

## Next Steps

1. **Run the setup script** to generate the demo database:
   ```r
   source("data-raw/create_demo_database.R")
   ```

2. **Document and rebuild** the package:
   ```r
   devtools::document()
   devtools::install()
   ```

3. **Test the demo**:
   ```r
   library(shinydbanalysis)
   filter_module_demo()
   ```

4. **Create more demo functions** following the same pattern:
   - `filter_builder_demo()`
   - `summary_builder_demo()`
   - etc.

5. **Write tests** that use `load_demo_column_info()` and `get_demo_pool()`

## Database Size Considerations

The current demo.duckdb is small (<1MB):
- diamonds: 53,940 rows
- iris: 150 rows
- gapdata: 1,704 rows

This is acceptable for package distribution. If you need larger datasets for performance testing, consider:
- Creating them on-the-fly in tests (not packaged)
- Using separate test fixtures downloaded from a remote source
- Documenting how users can create their own test databases

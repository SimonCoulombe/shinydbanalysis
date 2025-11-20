# Refactoring: filter_module → single_filter

## Summary

Refactored the filter module to use clearer naming and consistent arguments.

## Changes

### 1. New File: `R/single_filter.R`

Created new file with renamed and improved functions:

**Old → New:**
- `filter_module_ui()` → `single_filter_ui()`
- `filter_module_server()` → `single_filter_server()`
- `filter_module_demo()` → `single_filter_demo()`

### 2. Consistent Arguments

**Before:**
```r
# UI took column_info
filter_module_ui(id, column_info, initial_value)

# Server took separate metadata and distinct_values
filter_module_server(id, metadata, distinct_values, initial_value)
```

**After:**
```r
# Both take single_column_info
single_filter_ui(id, single_column_info, initial_value)
single_filter_server(id, single_column_info, initial_value)

# Where single_column_info is:
list(
  metadata = <list with column_name, column_type, min_value, max_value, etc.>,
  distinct_values = <dataframe with column_name and value columns>
)
```

### 3. Improved Demo

**Old demo:** Single filter that could switch columns via dropdown (used uiOutput)

**New demo:** Three filters side-by-side showing all column types:
- Numeric filter (carat from diamonds)
- Categorical filter (cut from diamonds)
- Date filter (date from gapdata)

```r
single_filter_demo()
```

### 4. Reactive Returns

The server function now returns reactive values for `column` and `type` (not just static values):

```r
filter_result <- single_filter_server("filter", single_column_info)

filter_result$value()      # reactiveVal - current filter value
filter_result$column()     # reactive - column name
filter_result$type()       # reactive - column type
filter_result$remove()     # reactive - remove button clicks
filter_result$is_active()  # reactiveVal - whether filter is active
```

This allows for reactive column info (e.g., changing columns dynamically).

### 5. Updated filter_builder.R

Updated to use the new `single_filter_*` functions:

**Changed:**
- `filter_module_ui()` → `single_filter_ui()`
- `filter_module_server()` → `single_filter_server()`
- Updated to pass `single_column_info` consistently
- Updated to call `column()`, `type()` as reactives (with `()`)

### 6. Backward Compatibility

`R/filter_module.R` now contains deprecated wrapper functions that call the new functions with a deprecation message:

```r
filter_module_ui(id, column_info, initial_value)
# → calls single_filter_ui(id, single_column_info = column_info, initial_value)

filter_module_server(id, metadata, distinct_values, initial_value)
# → calls single_filter_server(id, single_column_info = list(...), initial_value)
```

## Benefits

1. **Clearer naming:** "single_filter" makes it obvious this handles one column
2. **Consistent API:** Both UI and server use the same argument structure
3. **Better demo:** Shows all three column types at once
4. **More flexible:** Reactive column info allows dynamic updates
5. **Backward compatible:** Old code still works (with deprecation warnings)

## Migration Guide

### For Package Users

If you're using the old functions, update your code:

```r
# Old way
filter_module_ui("filter", 
  column_info = list(metadata = ..., distinct_values = ...))
  
filter_module_server("filter", 
  metadata = ..., 
  distinct_values = ...)

# New way  
single_filter_ui("filter",
  single_column_info = list(metadata = ..., distinct_values = ...))
  
single_filter_server("filter",
  single_column_info = list(metadata = ..., distinct_values = ...))
```

### For Package Developers

The old functions still work but will show deprecation messages. Plan to:
1. Update any internal code to use `single_filter_*`
2. Remove deprecated functions in a future version

## Testing

Run the new demo to see all three filter types:

```r
devtools::load_all()
single_filter_demo()
```

The demo will fail until you run `source("data-raw/create_demo_database.R")` to generate the demo data.

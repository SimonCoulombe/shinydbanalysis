#' Validation utilities for the filter builder app
#' @importFrom shiny validate need
#'
NULL

#' Validate column information structure
#'
#' @param column_info List. Column information to validate
#' @return TRUE if valid, stops with error if invalid
#' @noRd
validate_column_info <- function(column_info) {
  # Check if column_info is a list
  if (!is.list(column_info)) {
    stop("Column info must be a list")
  }

  # Check required fields
  required_fields <- c("name", "type", "values")
  missing_fields <- setdiff(required_fields, names(column_info))

  if (length(missing_fields) > 0) {
    stop(sprintf(
      "Missing required fields in column_info: %s",
      paste(missing_fields, collapse = ", ")
    ))
  }

  # Validate type field
  if (!column_info$type %in% c("numeric", "categorical")) {
    stop(sprintf(
      "Invalid column type: %s. Must be 'numeric' or 'categorical'",
      column_info$type
    ))
  }

  # Type-specific validation
  if (column_info$type == "numeric") {
    validate_numeric_values(column_info$values)
  } else {
    validate_categorical_values(column_info$values)
  }

  TRUE
}

#' Validate numeric column values
#'
#' @param values List. Must contain min and max values
#' @return TRUE if valid, stops with error if invalid
#' @noRd
validate_numeric_values <- function(values) {
  # Check if values is a list
  if (!is.list(values)) {
    stop("Numeric values must be a list containing min and max")
  }

  # Check required fields
  if (!all(c("min", "max") %in% names(values))) {
    stop("Numeric columns must have min and max values")
  }

  # Check numeric type
  if (!is.numeric(values$min) || !is.numeric(values$max)) {
    stop("Min and max values must be numeric")
  }

  # Check min < max
  if (values$min >= values$max) {
    stop(sprintf(
      "Min value (%f) must be less than max value (%f)",
      values$min, values$max
    ))
  }

  TRUE
}

#' Validate categorical column values
#'
#' @param values Vector. Must be non-empty vector of valid values
#' @return TRUE if valid, stops with error if invalid
#' @noRd
validate_categorical_values <- function(values) {
  # Check if values is non-empty
  if (length(values) == 0) {
    stop("Categorical values cannot be empty")
  }

  # Check for NULL or NA values
  if (any(is.null(values)) || any(is.na(values))) {
    stop("Categorical values cannot contain NULL or NA values")
  }

  # Check for duplicate values
  if (length(unique(values)) != length(values)) {
    stop("Categorical values must be unique")
  }

  TRUE
}

#' Validate filter value against column info
#'
#' @param value Vector. The value to validate
#' @param column_info List. Column information containing type and valid values
#' @return TRUE if valid, stops with error if invalid
#' @noRd
validate_filter_value <- function(value, column_info) {
  if (column_info$type == "numeric") {
    validate_numeric_filter_value(value, column_info$values)
  } else {
    validate_categorical_filter_value(value, column_info$values)
  }
}

#' Validate numeric filter value
#'
#' @param value Numeric vector. Must be length 2 with valid range
#' @param valid_values List. Contains min and max allowed values
#' @return TRUE if valid, stops with error if invalid
#' @noRd
validate_numeric_filter_value <- function(value, valid_values) {
  # Check length
  if (length(value) != 2) {
    stop("Numeric filter value must be a vector of length 2")
  }

  # Check numeric type
  if (!is.numeric(value)) {
    stop("Numeric filter value must be numeric")
  }

  # Check range
  if (value[1] < valid_values$min || value[2] > valid_values$max) {
    stop(sprintf(
      "Filter values must be between %f and %f",
      valid_values$min, valid_values$max
    ))
  }

  # Check order
  if (value[1] > value[2]) {
    stop("First value must be less than or equal to second value")
  }

  TRUE
}

#' Validate categorical filter value
#'
#' @param value Vector. Selected categorical values
#' @param valid_values Vector. All possible valid values
#' @return TRUE if valid, stops with error if invalid
#' @noRd
validate_categorical_filter_value <- function(value, valid_values) {
  # Check if empty
  if (length(value) == 0) {
    stop("Must select at least one categorical value")
  }

  # Check if all values are valid
  invalid_values <- setdiff(value, valid_values)
  if (length(invalid_values) > 0) {
    stop(sprintf(
      "Invalid categorical values: %s",
      paste(invalid_values, collapse = ", ")
    ))
  }

  TRUE
}

#' Validate database table name
#'
#' @param table_name Character. Name of the table
#' @param allowed_tables Vector. List of allowed table names
#' @return TRUE if valid, stops with error if invalid
#' @noRd
validate_table_name <- function(table_name, allowed_tables) {
  if (!table_name %in% allowed_tables) {
    stop(sprintf(
      "Invalid table name: %s. Must be one of: %s",
      table_name,
      paste(allowed_tables, collapse = ", ")
    ))
  }

  TRUE
}

#' Validate module ID
#'
#' @param id Character. The module ID to validate
#' @return TRUE if valid, stops with error if invalid
#' @noRd
validate_module_id <- function(id) {
  if (!is.character(id) || length(id) != 1 || nchar(id) == 0) {
    stop("Module ID must be a non-empty character string")
  }

  if (grepl("[^[:alnum:]_-]", id)) {
    stop("Module ID can only contain alphanumeric characters, underscores, and hyphens")
  }

  TRUE
}

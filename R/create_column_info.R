#' Get storage location helper function
#' @param storage_type Either "local" or "adls"
#' @param column_info_dir Path for local storage
#' @param adls_endpoint ADLS endpoint URL
#' @param adls_container ADLS container name
#' @param sas_token ADLS SAS token
#' @return List with storage information
get_storage_location <- function(storage_type, column_info_dir, adls_endpoint, adls_container, sas_token) {
  if (storage_type == "local") {
    if (!dir.exists(column_info_dir)) {
      dir.create(column_info_dir, recursive = TRUE)
    }
    return(list(type = "local", path = column_info_dir))
  } else if (storage_type == "adls") {
    endpoint <- storage_endpoint(adls_endpoint, sas=sas_token)
    cont <- storage_container(endpoint, adls_container)
    return(list(type = "adls", container = cont, path = column_info_dir))
  }
  stop("Invalid storage type")
}

#' Create column info with flexible storage
#'
#' @param tablename Character. Name of the table to analyze
#' @param pool Pool object. Database connection pool
#' @param storage_type Either "local" or "adls"
#' @param column_info_dir Path for local storage
#' @param adls_endpoint ADLS endpoint URL
#' @param adls_container ADLS container name
#' @param sas_token ADLS SAS token
#' @param max_distinct_values Integer. Maximum number of distinct values to store
#' @param batch_size Integer. Number of columns to process in each batch
#' @return List containing metadata_df and distinct_values_df (invisibly)
#' @export
create_column_info <- function(tablename, 
                               pool,
                               storage_type = "local",
                               column_info_dir = "column_info",
                               adls_endpoint = NULL,
                               adls_container = NULL,
                               sas_token = NULL,
                               max_distinct_values = 300,
                               batch_size = 50) {
  
  parsed_table_name <- parse_table_name(tablename)
  
  # Input validation
  if (!is.character(tablename) || length(tablename) != 1) {
    stop("tablename must be a single character string")
  }
  
  if (!inherits(pool, "Pool")) {
    stop("pool must be a valid database connection pool")
  }
  
  tryCatch({
    if (!table_exists(pool, parsed_table_name)) {
      stop(sprintf("Table '%s' not found in database", tablename))
    }
    
    # Get table reference
    tbl_ref <- create_table_ref(pool, parsed_table_name)
    
    # Get column types from a single row
    message("Getting column types...")
    sample_data <- tbl_ref %>% 
      head(1) %>% 
      collect()
    
    column_types <- lapply(sample_data, class)
    
    # Separate columns by type
    numeric_cols <- names(column_types)[sapply(column_types, function(x) x[1] %in% c("numeric", "integer"))]
    date_cols <- names(column_types)[sapply(column_types, function(x) x[1] %in% c("Date", "POSIXct", "POSIXt"))]
    categorical_cols <- names(column_types)[sapply(column_types, function(x) x[1] %in% c("character", "factor"))]
    
    process_columns_batch <- function(cols, type, tbl_ref) {
      message(sprintf("Processing %d %s columns...", length(cols), type))
      
      if (length(cols) == 0) return(NULL)
      
      # Split columns into batches
      batches <- split(cols, ceiling(seq_along(cols) / batch_size))
      
      # Process each batch
      batch_results <- lapply(batches, function(batch_cols) {
        message(sprintf("Processing batch: %s", paste(batch_cols, collapse = ", ")))
        
        # Build expressions for each column
        summary_exprs <- list()
        
        for (col in batch_cols) {
          # Quote column name with double quotes
          quoted_col <- sprintf('"%s"', col)
          
          # Build SQL expressions with proper column quoting
          min_sql <- dbplyr::sql(sprintf(
            'MIN(CASE WHEN %s IS NOT NULL THEN %s END)', 
            quoted_col, quoted_col
          ))
          max_sql <- dbplyr::sql(sprintf(
            'MAX(CASE WHEN %s IS NOT NULL THEN %s END)', 
            quoted_col, quoted_col
          ))
          n_distinct_sql <- dbplyr::sql(sprintf(
            'COUNT(DISTINCT CASE WHEN %s IS NOT NULL THEN %s END)', 
            quoted_col, quoted_col
          ))
          
          summary_exprs[[paste0(col, "_min")]] <- quo(!!min_sql)
          summary_exprs[[paste0(col, "_max")]] <- quo(!!max_sql)
          summary_exprs[[paste0(col, "_n_distinct")]] <- quo(!!n_distinct_sql)
        }
        
        # Execute the query with all expressions
        stats_query <- tbl_ref %>%
          summarise(
            !!!summary_exprs
          )
        
        collect(stats_query)
      })
      
      # Combine batch results
      if (length(batch_results) > 0) {
        Reduce(dplyr::bind_cols, batch_results)
      } else {
        NULL
      }
    }
    # Process each type of column
    stats_numeric <- process_columns_batch(numeric_cols, "numeric", tbl_ref)
    stats_date <- process_columns_batch(date_cols, "date", tbl_ref)
    stats_categorical <- process_columns_batch(categorical_cols, "categorical", tbl_ref)
    
    # Helper function to safely extract stats
    safe_extract_stat <- function(stats_df, col, stat_suffix, default = NA) {
      if (is.null(stats_df)) return(default)
      col_name <- paste0(col, "_", stat_suffix)
      if (col_name %in% names(stats_df)) stats_df[[col_name]] else default
    }
    
    # Create metadata dataframes for each type
    numeric_metadata <- if (length(numeric_cols) > 0) {
      data.frame(
        column_name = numeric_cols,
        column_type = "numeric",
        min_value = sapply(numeric_cols, function(col) safe_extract_stat(stats_numeric, col, "min", NA_real_)),
        max_value = sapply(numeric_cols, function(col) safe_extract_stat(stats_numeric, col, "max", NA_real_)),
        min_date = as.Date(NA),
        max_date = as.Date(NA),
        n_distinct = sapply(numeric_cols, function(col) as.integer(safe_extract_stat(stats_numeric, col, "n_distinct", 0))),
        created_at = Sys.time(),
        stringsAsFactors = FALSE
      )
    }
    
    date_metadata <- if (length(date_cols) > 0) {
      data.frame(
        column_name = date_cols,
        column_type = "date",
        min_value = NA_real_,
        max_value = NA_real_,
        min_date = as.Date(sapply(date_cols, function(col) safe_extract_stat(stats_date, col, "min", NA))),
        max_date = as.Date(sapply(date_cols, function(col) safe_extract_stat(stats_date, col, "max", NA))),
        n_distinct = sapply(date_cols, function(col) as.integer(safe_extract_stat(stats_date, col, "n_distinct", 0))),
        created_at = Sys.time(),
        stringsAsFactors = FALSE
      )
    }
    
    categorical_metadata <- if (length(categorical_cols) > 0) {
      data.frame(
        column_name = categorical_cols,
        column_type = "categorical",
        min_value = NA_real_,
        max_value = NA_real_,
        min_date = as.Date(NA),
        max_date = as.Date(NA),
        n_distinct = sapply(categorical_cols, function(col) as.integer(safe_extract_stat(stats_categorical, col, "n_distinct", 0))),
        created_at = Sys.time(),
        stringsAsFactors = FALSE
      )
    }
    
    # Combine all metadata
    metadata_df <- do.call(rbind, list(numeric_metadata, date_metadata, categorical_metadata))
    
    # Process distinct values for categorical columns
    distinct_values_list <- list()
    
    for (col in categorical_cols) {
      n_distinct <- metadata_df$n_distinct[metadata_df$column_name == col]
      
      if (n_distinct > 0 && n_distinct <= max_distinct_values) {
        message("Getting distinct values for: ", col)
        values <- tryCatch({
          tbl_ref %>%
            filter(!is.na(.data[[col]])) %>%
            select(all_of(col)) %>%
            distinct() %>%
            arrange(.data[[col]]) %>%
            collect() %>%
            pull(col)
        }, error = function(e) {
          message(sprintf("Warning: Could not get distinct values for column %s: %s", col, e$message))
          character(0)
        })
        
        if (length(values) > 0) {
          if (is.factor(values)) {
            values <- as.character(values)
          }
          
          distinct_values_list[[col]] <- data.frame(
            column_name = col,
            value = values,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    # Combine distinct values
    distinct_values_df <- dplyr::bind_rows(distinct_values_list)
    
    # Storage handling
    storage <- get_storage_location(storage_type, column_info_dir, adls_endpoint, adls_container, sas_token)
    
    if (storage$type == "local") {
      arrow::write_parquet(
        metadata_df,
        file.path(storage$path, sprintf("column_info_%s.parquet", tablename))
      )
      
      if (nrow(distinct_values_df) > 0) {
        arrow::write_parquet(
          distinct_values_df,
          file.path(storage$path, sprintf("distinct_values_%s.parquet", tablename))
        )
      }
      
      message(sprintf("Column info saved to: %s", storage$path))
    } else {
      tmp_file <- tempfile(fileext = ".parquet")
      arrow::write_parquet(metadata_df, tmp_file)
      path <- file.path(storage$path, sprintf("column_info_%s.parquet", tablename))
      AzureStor::storage_upload(storage$container, tmp_file, path)
      unlink(tmp_file)
      
      if (nrow(distinct_values_df) > 0) {
        tmp_file <- tempfile(fileext = ".parquet")
        arrow::write_parquet(distinct_values_df, tmp_file)
        path <- file.path(storage$path, sprintf("distinct_values_%s.parquet", tablename))
        AzureStor::storage_upload(storage$container, tmp_file,  path)
        unlink(tmp_file)
      }
      
      message("Column info saved to ADLS container")
    }
    
    invisible(list(
      metadata = metadata_df,
      distinct_values = distinct_values_df
    ))
    
  }, error = function(e) {
    stop(sprintf("Error creating column info: %s", e$message))
  })
}


#' Read column info from storage, excluding unavailable columns
#' @param tablename Character. Name of the table
#' @param storage_type Either "local" or "adls"
#' @param column_info_dir Path for local storage
#' @param adls_endpoint ADLS endpoint URL
#' @param adls_container ADLS container name
#' @param sas_token ADLS SAS token
#' @param restricted_columns Character vector of columns to exclude
#' @return List containing metadata_df and distinct_values_df
#' @export
read_column_info <- function(tablename,
                             storage_type = "local",
                             column_info_dir = "column_info",
                             adls_endpoint = NULL,
                             adls_container = NULL,
                             sas_token = NULL,
                             restricted_columns = character(0)) {
  
  storage <- get_storage_location(storage_type, column_info_dir, adls_endpoint, adls_container, sas_token)
  
  # Initialize empty data frames with correct structure
  empty_metadata <- data.frame(
    column_name = character(),
    column_type = character(),
    min_value = numeric(),
    max_value = numeric(),
    min_date = as.Date(character()),
    max_date = as.Date(character()),
    n_distinct = integer(),
    created_at = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
  
  empty_distinct_values <- data.frame(
    column_name = character(),
    value = character(),
    stringsAsFactors = FALSE
  )
  
  if (storage$type == "local") {
    metadata_path <- file.path(storage$path, sprintf("column_info_%s.parquet", tablename))
    if (!file.exists(metadata_path)) {
      warning(sprintf("Column info not found for table: %s", tablename))
      return(list(metadata = empty_metadata, distinct_values = empty_distinct_values))
    }
    
    metadata_df <- tryCatch({
      df <- arrow::read_parquet(metadata_path)
      if (length(restricted_columns) > 0) {
        df <- df %>% filter(!column_name %in% restricted_columns)
      }
      df
    }, error = function(e) {
      warning(sprintf("Error reading metadata: %s", e$message))
      empty_metadata
    })
    
    distinct_values_path <- file.path(storage$path, 
                                      sprintf("distinct_values_%s.parquet", tablename))
    distinct_values_df <- if (file.exists(distinct_values_path)) {
      tryCatch({
        df <- arrow::read_parquet(distinct_values_path)
        if (length(restricted_columns) > 0) {
          df <- df %>% filter(!column_name %in% restricted_columns)
        }
        df
      }, error = function(e) {
        warning(sprintf("Error reading distinct values: %s", e$message))
        empty_distinct_values
      })
    } else {
      empty_distinct_values
    }
  } else {
    # For ADLS storage
    metadata_df <- tryCatch({
      tmp_file <- tempfile(fileext = ".parquet")
      path <- file.path(storage$path, sprintf("column_info_%s.parquet", tablename))
      AzureStor::storage_download(storage$container, 
                       path, 
                       tmp_file)
      df <- arrow::read_parquet(tmp_file)
      unlink(tmp_file)
      if (length(restricted_columns) > 0) {
        df <- df %>% filter(!column_name %in% restricted_columns)
      }
      df
    }, error = function(e) {
      warning(sprintf("Error reading metadata from ADLS: %s", e$message))
      empty_metadata
    })
    
    distinct_values_df <- tryCatch({
      tmp_file <- tempfile(fileext = ".parquet")
      path <- file.path(storage$path, sprintf("distinct_values_%s.parquet", tablename))
      AzureStor::storage_download(storage$container,
                       path,
                       tmp_file)
      df <- arrow::read_parquet(tmp_file)
      unlink(tmp_file)
      if (length(restricted_columns) > 0) {
        df <- df %>% filter(!column_name %in% restricted_columns)
      }
      df
    }, error = function(e) {
      warning(sprintf("Error reading distinct values from ADLS: %s", e$message))
      empty_distinct_values
    })
  }
  
  # Ensure all expected columns are present
  expected_cols <- names(empty_metadata)
  missing_cols <- setdiff(expected_cols, names(metadata_df))
  
  if (length(missing_cols) > 0) {
    warning(sprintf("Missing columns in metadata: %s", paste(missing_cols, collapse = ", ")))
    for (col in missing_cols) {
      metadata_df[[col]] <- empty_metadata[[col]][0]
    }
  }
  
  list(
    metadata = metadata_df,
    distinct_values = distinct_values_df
  )
}

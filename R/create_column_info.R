#' Get storage location helper function
#' @param storage_type Either "local" or "adls"
#' @param local_dir Path for local storage
#' @param adls_endpoint ADLS endpoint URL
#' @param adls_container ADLS container name
#' @param sas_token ADLS SAS token
#' @return List with storage information
get_storage_location <- function(storage_type, local_dir, adls_endpoint, adls_container, sas_token) {
  if (storage_type == "local") {
    if (!dir.exists(local_dir)) {
      dir.create(local_dir, recursive = TRUE)
    }
    return(list(type = "local", path = local_dir))
  } else if (storage_type == "adls") {
    endpoint <- storage_endpoint(adls_endpoint, sas=sas_token)
    cont <- storage_container(endpoint, adls_container)
    return(list(type = "adls", container = cont))
  }
  stop("Invalid storage type")
}

#' Create column info with flexible storage
#' @param tablename Character. Name of the table to analyze
#' @param pool Pool object. Database connection pool
#' @param storage_type Either "local" or "adls"
#' @param local_dir Path for local storage
#' @param adls_endpoint ADLS endpoint URL
#' @param adls_container ADLS container name
#' @param sas_token ADLS SAS token
#' @param max_distinct_values Integer. Maximum number of distinct values to store
#' @return List containing metadata_df and distinct_values_df (invisibly)
#' @export
create_column_info <- function(tablename, 
                               pool,
                               storage_type = "local",
                               local_dir = "column_info",
                               adls_endpoint = NULL,
                               adls_container = NULL,
                               sas_token = NULL,
                               max_distinct_values = 300) {
  
  table_info <- parse_table_name(tablename)
  
  # Input validation
  if (!is.character(tablename) || length(tablename) != 1) {
    stop("tablename must be a single character string")
  }
  
  if (!inherits(pool, "Pool")) {
    stop("pool must be a valid database connection pool")
  }
  
  tryCatch({
    if (!table_exists(pool, table_info)) {
      stop(sprintf("Table '%s' not found in database", tablename))
    }
    
    # Get table reference
    tbl_ref <- create_table_ref(pool, table_info)
    
    # First pass: Get basic column info
    message("First pass: Getting basic column info...")
    sample_data <- tbl_ref %>% 
      head(1) %>% 
      collect()
    
    column_types <- lapply(sample_data, class)
    
    # Initialize metadata
    metadata_rows <- list()
    distinct_values_list <- list()
    
    # Process each column
    for (col in names(column_types)) {
      message("Processing column: ", col)
      col_class <- column_types[[col]][1]
      
      if (col_class == "Date" || col_class == "POSIXct" || col_class == "POSIXt") {
        # Date type
        range_values <- tbl_ref %>%
          summarise(
            min = min(.data[[col]], na.rm = TRUE),
            max = max(.data[[col]], na.rm = TRUE),
            n_distinct = n_distinct(.data[[col]])
          ) %>%
          collect()
        
        metadata_rows[[col]] <- data.frame(
          column_name = col,
          column_type = "date",
          min_value = as.character(as.Date(range_values$min)),
          max_value = as.character(as.Date(range_values$max)),
          n_distinct = range_values$n_distinct,
          stringsAsFactors = FALSE
        )
        
      } else if (col_class %in% c("character", "factor")) {
        # Categorical type
        n_distinct <- tbl_ref %>%
          summarise(n = n_distinct(.data[[col]])) %>%
          pull(n)
        
        metadata_rows[[col]] <- data.frame(
          column_name = col,
          column_type = "categorical",
          min_value = NA_character_,
          max_value = NA_character_,
          n_distinct = n_distinct,
          stringsAsFactors = FALSE
        )
        
        # Get distinct values if count is below threshold
        if (n_distinct <= max_distinct_values) {
          message("Getting distinct values for: ", col)
          values <- tbl_ref %>%
            filter(!is.na(.data[[col]])) %>%
            select(all_of(col)) %>%
            distinct() %>%
            arrange(.data[[col]]) %>%
            collect() %>%
            pull(col)
          
          if (is.factor(values)) {
            values <- as.character(values)
          }
          
          distinct_values_list[[col]] <- data.frame(
            column_name = col,
            value = values,
            stringsAsFactors = FALSE
          )
        }
        
      } else {
        # Numeric type
        summary_values <- tbl_ref %>%
          summarise(
            min = min(.data[[col]], na.rm = TRUE),
            max = max(.data[[col]], na.rm = TRUE),
            n_distinct = n_distinct(.data[[col]])
          ) %>%
          collect()
        
        metadata_rows[[col]] <- data.frame(
          column_name = col,
          column_type = "numeric",
          min_value = as.character(summary_values$min),
          max_value = as.character(summary_values$max),
          n_distinct = summary_values$n_distinct,
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Combine metadata rows
    metadata_df <- bind_rows(metadata_rows)
    
    # Add table information
    metadata_df$table_name <- tablename
    metadata_df$created_at <- Sys.time()
    
    # Reorder columns
    metadata_df <- metadata_df %>%
      select(table_name, column_name, column_type, min_value, max_value, 
             n_distinct, created_at)
    
    # Combine distinct values
    distinct_values_df <- bind_rows(distinct_values_list)
    if (nrow(distinct_values_df) > 0) {
      distinct_values_df$table_name <- tablename
      distinct_values_df <- distinct_values_df %>%
        select(table_name, column_name, value)
    }
    
    # Get storage location
    storage <- get_storage_location(storage_type, local_dir, adls_endpoint, adls_container, sas_token)
    
    # Save both tables
    if (storage$type == "local") {
      # Save metadata
      arrow::write_parquet(
        metadata_df,
        file.path(storage$path, sprintf("column_info_%s.parquet", tablename))
      )
      
      # Save distinct values if any exist
      if (nrow(distinct_values_df) > 0) {
        arrow::write_parquet(
          distinct_values_df,
          file.path(storage$path, sprintf("distinct_values_%s.parquet", tablename))
        )
      }
      
      message(sprintf("Column info saved to: %s", storage$path))
    } else {
      # For ADLS
      # Save metadata
      tmp_file <- tempfile(fileext = ".parquet")
      arrow::write_parquet(metadata_df, tmp_file)
      storage_upload(storage$container, tmp_file, 
                     sprintf("column_info_%s.parquet", tablename))
      unlink(tmp_file)
      
      # Save distinct values if any exist
      if (nrow(distinct_values_df) > 0) {
        tmp_file <- tempfile(fileext = ".parquet")
        arrow::write_parquet(distinct_values_df, tmp_file)
        storage_upload(storage$container, tmp_file, 
                       sprintf("distinct_values_%s.parquet", tablename))
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

#' Read column info from storage
#' @param tablename Character. Name of the table
#' @param storage_type Either "local" or "adls"
#' @param local_dir Path for local storage
#' @param adls_endpoint ADLS endpoint URL
#' @param adls_container ADLS container name
#' @param sas_token ADLS SAS token
#' @return List containing metadata_df and distinct_values_df
#' @export
read_column_info <- function(tablename,
                             storage_type = "local",
                             local_dir = "column_info",
                             adls_endpoint = NULL,
                             adls_container = NULL,
                             sas_token = NULL) {
  
  storage <- get_storage_location(storage_type, local_dir, adls_endpoint, adls_container, sas_token)
  
  if (storage$type == "local") {
    # Read metadata
    metadata_path <- file.path(storage$path, sprintf("column_info_%s.parquet", tablename))
    if (!file.exists(metadata_path)) {
      stop(sprintf("Column info not found for table: %s", tablename))
    }
    metadata_df <- arrow::read_parquet(metadata_path)
    
    # Read distinct values if they exist
    distinct_values_path <- file.path(storage$path, 
                                      sprintf("distinct_values_%s.parquet", tablename))
    distinct_values_df <- if (file.exists(distinct_values_path)) {
      arrow::read_parquet(distinct_values_path)
    } else {
      data.frame(
        table_name = character(),
        column_name = character(),
        value = character(),
        stringsAsFactors = FALSE
      )
    }
  } else {
    # For ADLS
    # Read metadata
    tmp_file <- tempfile(fileext = ".parquet")
    storage_download(storage$container, 
                     sprintf("column_info_%s.parquet", tablename), 
                     tmp_file)
    metadata_df <- arrow::read_parquet(tmp_file)
    unlink(tmp_file)
    
    # Try to read distinct values
    distinct_values_df <- tryCatch({
      tmp_file <- tempfile(fileext = ".parquet")
      storage_download(storage$container,
                       sprintf("distinct_values_%s.parquet", tablename),
                       tmp_file)
      df <- arrow::read_parquet(tmp_file)
      unlink(tmp_file)
      df
    }, error = function(e) {
      data.frame(
        table_name = character(),
        column_name = character(),
        value = character(),
        stringsAsFactors = FALSE
      )
    })
  }
  
  list(
    metadata = metadata_df,
    distinct_values = distinct_values_df
  )
}

# Example usage:
if (FALSE) {
  # Local storage example
  metadata <- create_column_info(
    tablename = "diamonds",
    pool = pool,
    storage_type = "local",
    local_dir = "column_info"
  )
  
  # ADLS storage example
  metadata <- create_column_info_enhanced(
    tablename = "diamonds",
    pool = pool,
    storage_type = "adls",
    adls_endpoint = "https://myaccount.dfs.core.windows.net",
    adls_container = "mycontainer",
    sas_token = "mysastoken"
  )
  
  # Read metadata (in new tabular format)
  metadata_df <- read_column_info(
    tablename = "diamonds",
    storage_type = "local",
    local_dir = "column_info"
  )
  
  # Read metadata (in legacy list format for backward compatibility)
  col_info <- read_column_info(
    tablename = "diamonds",
    storage_type = "local",
    local_dir = "column_info",
    legacy_format = TRUE
  )
}
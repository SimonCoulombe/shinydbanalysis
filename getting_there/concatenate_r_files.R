#!/usr/bin/env Rscript

# Function to recursively find all .R files in a directory
list_r_files <- function(path) {
  files <- list.files(
    path = path,
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = TRUE
  )
  return(files)
}

# Function to concatenate files with separators
concatenate_files <- function(files, output_file = "repo.R") {
  # Create or truncate the output file
  file.create(output_file)

  # Process each file
  for (file_path in files) {
    # Add file separator
    cat(sprintf("\n// File: %s\n", file_path), file = output_file, append = TRUE)

    # Read and append file content
    tryCatch({
      content <- readLines(file_path)
      cat(paste(content, collapse = "\n"), "\n", file = output_file, append = TRUE)
    }, error = function(e) {
      warning(sprintf("Error reading file %s: %s", file_path, e$message))
    })
  }
}

# Main execution
main <- function() {
  # Get the current working directory or specify your repository path
  repo_path <- here::here("R")

  # List all R files
  r_files <- list_r_files(repo_path)

  # Print found files
  cat("Found the following R files:\n")
  cat(paste(r_files, collapse = "\n"), "\n\n")

  # Concatenate files
  if (length(r_files) > 0) {
    concatenate_files(r_files)
    cat(sprintf("Successfully concatenated %d files into repo.R\n", length(r_files)))
  } else {
    cat("No R files found in the repository.\n")
  }
}

# Run the script
main()



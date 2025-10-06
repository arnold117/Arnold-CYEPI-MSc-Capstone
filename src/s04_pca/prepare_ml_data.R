# Load necessary libraries
library(tidyverse)

# Function to copy and rename PCA files to ml_ready folder
copy_pca_to_ml_ready <- function(pca_results_dir, ml_ready_dir) {
  cat("Copying PCA files to ML-ready folder...\n")
  cat("Source:", pca_results_dir, "\n")
  cat("Destination:", ml_ready_dir, "\n\n")
  
  # Create ml_ready directory if it doesn't exist
  if (!dir.exists(ml_ready_dir)) {
    dir.create(ml_ready_dir, recursive = TRUE)
    cat("Created directory:", ml_ready_dir, "\n")
  }
  
  # Get all dataset subdirectories
  dataset_dirs <- list.dirs(pca_results_dir, full.names = TRUE, recursive = FALSE)
  
  if (length(dataset_dirs) == 0) {
    cat("No dataset subdirectories found in:", pca_results_dir, "\n")
    return()
  }
  
  cat("Found", length(dataset_dirs), "dataset directories to process:\n")
  
  success_count <- 0
  total_files <- 0
  
  for (dataset_dir in dataset_dirs) {
    dataset_name <- basename(dataset_dir)
    cat("\nProcessing dataset:", dataset_name, "\n")
    
    # Look for train and test PCA complete files in the dataset directory
    train_file <- file.path(dataset_dir, "train_pca_complete.rds")
    test_file <- file.path(dataset_dir, "test_pca_complete.rds")
    
    # Process train file
    if (file.exists(train_file)) {
      total_files <- total_files + 1
      new_filename <- create_ml_filename(dataset_name, "train")
      new_path <- file.path(ml_ready_dir, new_filename)
      
      tryCatch({
        file.copy(train_file, new_path, overwrite = TRUE)
        cat("  ✓ Copied: train_pca_complete.rds\n")
        cat("      -> ", new_filename, "\n")
        success_count <- success_count + 1
      }, error = function(e) {
        cat("  ✗ Error copying train file:", e$message, "\n")
      })
    } else {
      cat("  ⚠ Train file not found in", dataset_name, "\n")
    }
    
    # Process test file
    if (file.exists(test_file)) {
      total_files <- total_files + 1
      new_filename <- create_ml_filename(dataset_name, "test")
      new_path <- file.path(ml_ready_dir, new_filename)
      
      tryCatch({
        file.copy(test_file, new_path, overwrite = TRUE)
        cat("  ✓ Copied: test_pca_complete.rds\n")
        cat("      -> ", new_filename, "\n")
        success_count <- success_count + 1
      }, error = function(e) {
        cat("  ✗ Error copying test file:", e$message, "\n")
      })
    } else {
      cat("  ⚠ Test file not found in", dataset_name, "\n")
    }
  }
  
  cat("\nCopy completed:", success_count, "of", total_files, "files copied successfully.\n")
  cat("Files available in:", ml_ready_dir, "\n")
  
  # List the files in ml_ready directory
  ml_files <- list.files(ml_ready_dir, pattern = "_pca_.*\\.(rds|csv)$")
  if (length(ml_files) > 0) {
    cat("\nPCA files in ml_ready folder:\n")
    for (file in ml_files) {
      cat("  -", file, "\n")
    }
  }
}

# Helper function to create ML-ready filename from dataset name
create_ml_filename <- function(dataset_name, data_split) {
  # Parse dataset_name to extract data_type and config
  if (grepl("_l2_", dataset_name)) {
    parts <- strsplit(dataset_name, "_l2_")[[1]]
    data_type <- paste0(parts[1], "_l2")
    config <- parts[2]
  } else if (grepl("_log_", dataset_name)) {
    parts <- strsplit(dataset_name, "_log_")[[1]]
    data_type <- paste0(parts[1], "_log")
    config <- parts[2]
  } else {
    parts <- strsplit(dataset_name, "_data_")[[1]]
    data_type <- paste0(parts[1], "_data")
    config <- parts[2]
  }
  
  # Create new filename with PCA indicator
  new_filename <- paste0(data_type, "_pca_", config, "_", data_split, ".rds")
  return(new_filename)
}

# Function to integrate with existing ML pipeline
prepare_pca_for_ml <- function() {
  cat("Preparing PCA data for machine learning pipeline...\n\n")
  
  # Define paths
  if (exists("paths")) {
    pca_results_dir <- file.path(paths$pca_outputs, "pca_results")
    ml_ready_dir <- paths$data_dl_ready
  } else {
    cat("Error: 'paths' variable not found. Please ensure paths are set up correctly.\n")
    return(FALSE)
  }
  
  # Check if source directory exists
  if (!dir.exists(pca_results_dir)) {
    cat("Error: PCA results directory not found:", pca_results_dir, "\n")
    cat("Please run PCA analysis first.\n")
    return(FALSE)
  }
  
  # Copy and rename PCA files
  copy_pca_to_ml_ready(pca_results_dir, ml_ready_dir)
  
  cat("\nPCA data preparation complete!\n")
  cat("You can now use these files in your deep learning pipeline by setting:\n")
  cat("  PCA_trans = TRUE\n")
  
  return(TRUE)
}

# Execute the preparation function
if (prepare_pca_for_ml()) {
  cat("PCA data is ready for deep learning!\n")
} else {
  cat("PCA data preparation failed. Please check the logs for details.\n")
}


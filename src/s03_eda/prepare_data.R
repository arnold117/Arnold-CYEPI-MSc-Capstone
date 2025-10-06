# src/s03_eda/prepare_data.R - Prepare data for EDA

# Load necessary libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

cat("Preparing data for EDA...\n")

# Ensure configuration is loaded
if (!exists("config") || !exists("paths") || !exists("project_root")) {
  source("config/setup.R")
}

# Load aggregated data for EDA
cat("Loading aggregated datasets...\n")

# Initialize data list
eda_data <- list()

# Dataset names to load
dataset_names <- c(
  "sensor_data_agg",
  "alpha_data_agg", 
  "spd_data_agg",
  "sensor_data_l2_agg",
  "alpha_data_l2_agg",
  "spd_data_l2_agg"
)

# Load each dataset
for (dataset_name in dataset_names) {
  file_path <- file.path(paths$data_aggregated, paste0(dataset_name, ".rds"))
  
  if (file.exists(file_path)) {
    data <- readRDS(file_path)
    eda_data[[dataset_name]] <- data
    
    cat("✓ Loaded", dataset_name, ":", nrow(data), "records,", ncol(data), "columns\n")
    
    # Basic data info
    cat("  Participants:", length(unique(data$Id)), "\n")
    cat("  Date range:", as.character(min(data$Datetime, na.rm = TRUE)), 
        "to", as.character(max(data$Datetime, na.rm = TRUE)), "\n")
    
    # Check for main_light distribution if available
    if ("main_light" %in% names(data)) {
      light_dist <- table(data$main_light, useNA = "ifany")
      cat("  Light types:", paste(names(light_dist), light_dist, sep = "=", collapse = ", "), "\n")
    }
    
  } else {
    cat("✗ File not found:", file_path, "\n")
  }
  cat("\n")
}

# Create summary information
cat("=== EDA Data Summary ===\n")
cat("Total datasets loaded:", length(eda_data), "\n")
if (length(eda_data) > 0) {
  cat("Datasets available:\n")
  for (name in names(eda_data)) {
    cat("  -", name, ":", nrow(eda_data[[name]]), "records\n")
  }
}

# Make data globally available for other EDA scripts
assign("eda_data", eda_data, envir = .GlobalEnv)

cat("Data preparation for EDA completed successfully!\n")

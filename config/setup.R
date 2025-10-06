# config/setup.R - Configuration and setup script for LightSPAN Cyepi Analysis

# Load required libraries
suppressPackageStartupMessages({
  library(here)
  library(yaml)
})

# Set project root
if (!exists("project_root")) {
  project_root <- here::here()
}

# Load configuration from YAML
config_file <- file.path(project_root, "config", "analysis_config.yaml")
if (!file.exists(config_file)) {
  stop("Configuration file not found: ", config_file)
}

config <- yaml::read_yaml(config_file)

# Set global options
options(
  stringsAsFactors = FALSE,
  digits = 4,
  scipen = 999
)

# Define paths based on YAML configuration
paths <- list(
  # Data paths
  external_data = file.path(project_root, config$data$external_data_path),
  data_raw = file.path(project_root, config$data$raw_data_path),
  data_processed = file.path(project_root, config$data$processed_data_path),
  data_aggregated = file.path(project_root, config$data$aggregated_data_path),
  data_split = file.path(project_root, config$data$split_path),
  data_dl_ready = file.path(project_root, config$data$dl_ready_path),
  
  # Output paths
  outputs = file.path(project_root, config$outputs$path),
  reports = file.path(project_root, config$outputs$path, "reports/"),
  eda_outputs = file.path(project_root, config$outputs$path, "EDA/"),
  pca_outputs = file.path(project_root, config$outputs$path, "PCA/"),
  deep_learning_outputs = file.path(project_root, config$outputs$path, "deep_learning/"),
  baseline_outputs = file.path(project_root, config$outputs$path, "baseline_classification/"),

  # Other paths
  temp = file.path(project_root, "temp"),
  config = file.path(project_root, "config")
)

# Create directories if they don't exist
dirs <- c(
  paths$data_raw,
  paths$data_processed,
  paths$data_aggregated,
  paths$data_dl_ready,
  paths$data_split,
  paths$outputs,
  paths$reports,
  paths$eda_outputs,
  paths$pca_outputs,
  paths$deep_learning_outputs,
  paths$baseline_outputs,
  file.path(paths$reports, "interim"),
  file.path(paths$reports, "final"),
  paths$temp
)

for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}

# Print configuration summary
cat("=== LightSPAN Cyepi Analysis Configuration ===\n")
cat("Project:", config$project$name, "v", config$project$version, "\n")
cat("Author:", config$project$author, "\n")
cat("Project root:", project_root, "\n")
cat("Configuration loaded from:", config_file, "\n")
cat("Output directories created successfully.\n")
cat("===============================================\n")

# Make config and paths available globally
assign("config", config, envir = .GlobalEnv)
assign("paths", paths, envir = .GlobalEnv)
assign("project_root", project_root, envir = .GlobalEnv)
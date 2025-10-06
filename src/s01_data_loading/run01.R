# src/run01_data_loading.R - Data loading control script

cat("=== Step 1: Data Loading ===\n")

# Ensure configuration is loaded
if (!exists("config") || !exists("paths") || !exists("project_root")) {
  source("config/setup.R")
}

# Check external data path
if (!dir.exists(paths$external_data)) {
  warning("External data path not found: ", paths$external_data)
  cat("Please ensure the cyepi-data-share directory is accessible.\n")
}

# Load Actlumus data
cat("\nLoading Actlumus data...\n")
tryCatch({
  source("src/s01_data_loading/load_actlumus.R")
  cat("✓ Actlumus data loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading Actlumus data:", e$message, "\n")
  stop(e)
})

# Load mHLEA data
cat("\nLoading mHLEA data...\n")
tryCatch({
  source("src/s01_data_loading/load_hlea_paper.R")
  cat("✓ mHLEA data loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading mHLEA data:", e$message, "\n")
  stop(e)
})

# Simple data summary
cat("\n=== Data Loading Summary ===\n")
actlumus_file <- file.path(paths$data_raw, "actlumus.rds")
hlea_file <- file.path(paths$data_raw, "mHLEA_paper.rds")

if (file.exists(actlumus_file) && file.exists(hlea_file)) {
  # Read data for quick checks
  actlumus_data <- readRDS(actlumus_file)
  hlea_data <- readRDS(hlea_file)

  # Check participant overlap
  actlumus_pids <- unique(actlumus_data$Id)
  hlea_pids <- unique(hlea_data$Id)
  overlap_pids <- intersect(actlumus_pids, hlea_pids)
  
  cat("Actlumus participants:", length(actlumus_pids), "\n")
  cat("mHLEA participants:", length(hlea_pids), "\n")
  cat("Overlapping participants:", length(overlap_pids), "\n")
  
  if (length(overlap_pids) > 0) {
    cat("Participant IDs with both datasets:", paste(sort(overlap_pids), collapse = ", "), "\n")
  }
  
  cat("✓ All data loaded successfully!\n")
} else {
  stop("✗ Data loading failed - missing output files")
}

cat("=============================\n")
# src/run02_preprocessing.R - Data preprocessing control script

cat("=== Step 2: Data Preprocessing ===\n")

# Ensure configuration is loaded
if (!exists("config") || !exists("paths") || !exists("project_root")) {
  source("config/setup.R")
}

# Load raw data
cat("\nLoading raw data...\n")
tryCatch({
  actlumus_data <- readRDS(file.path(paths$data_raw, "actlumus.rds"))
  hlea_data <- readRDS(file.path(paths$data_raw, "mHLEA_paper.rds"))

  cat("✓ Raw data loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading raw data:", e$message, "\n")
  stop(e)
})

# Reconstruct actlumus data
cat("\nReconstructing actlumus data...\n")
tryCatch({
  # define reconstruct weights path
  path <- file.path(project_root,
                    config$preprocessing$actlumus$reconstruct$path)
  spd <- file.path(path,
                   config$preprocessing$actlumus$reconstruct$spd_weights)
  alpha <- file.path(path,
                     config$preprocessing$actlumus$reconstruct$alpha_weights)

  source("src/s02_preprocessing/reconstruct_actlumus.R")
  cat("✓ Actlumus data reconstructed successfully\n")
}, error = function(e) {
  cat("✗ Error reconstructing actlumus data:", e$message, "\n")
  stop(e)
})

# Transform mHLEA labels
cat("\nTransforming mHLEA labels...\n")
tryCatch({
  source("src/s02_preprocessing/transform_hlea.R")
  cat("✓ mHLEA labels transformed successfully\n")
}, error = function(e) {
  cat("✗ Error transforming mHLEA labels:", e$message, "\n")
  stop(e)
})

# Log Transformation
cat("\nPerforming log transformation...\n")
tryCatch({
  source("src/s02_preprocessing/log_transform.R")
  cat("✓ Log transformation completed successfully\n")
}, error = function(e) {
  cat("✗ Error during log transformation:", e$message, "\n")
  stop(e)
})

# L2 Normalisation
cat("\nPerforming L2 normalization...\n")
tryCatch({
  cat("L2 normalization on actlumus data...\n")
  source("src/s02_preprocessing/l2norm_actlumus.R")
  cat("✓ L2 normalization on actlumus data completed successfully\n")
}, error = function(e) {
  cat("✗ Error during L2 normalization:", e$message, "\n")
  stop(e)
})

# Data Aggregation
cat("\nAggregating data...\n")
tryCatch({
  source("src/s02_preprocessing/aggregate.R")
  cat("✓ Data aggregation completed successfully\n")
}, error = function(e) {
  cat("✗ Error during data aggregation:", e$message, "\n")
  stop(e)
})

# Participant-wise Train/Test Split
cat("\nPerforming participant-wise train/test split...\n")
tryCatch({
  source("src/s02_preprocessing/split_training.R")
  cat("✓ Participant-wise train/test split completed successfully\n")
}, error = function(e) {
  cat("✗ Error during participant-wise split:", e$message, "\n")
  stop(e)
})


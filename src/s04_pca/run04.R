

cat("=== Step 4: PCA ===\n")
# Load necessary libraries
library(factoextra)  # For PCA visualization
library(corrplot)    # For correlation plots

# Ensure configuration is loaded
if (!exists("config") || !exists("paths") || !exists("project_root")) {
  source("config/setup.R")
}


# Prepare data
cat("\nPreparing data for PCA...\n")
tryCatch({
  source("src/s04_pca/prepare_data.R")
  cat("✓ Data prepared successfully for PCA\n")
}, error = function(e) {
  cat("✗ Error preparing data for PCA:", e$message, "\n")
  stop(e)
})

# PCA
cat("\nPerforming PCA...\n")
tryCatch({
  source("src/s04_pca/perform_pca.R")
  cat("✓ PCA performed successfully\n")
}, error = function(e) {
  cat("✗ Error performing PCA:", e$message, "\n")
  stop(e)
})

# Create advanced PCA plots
cat("\nCreating advanced PCA plots...\n")
tryCatch({
  source("src/s04_pca/create_advanced_pca_plots.R")
  cat("✓ Advanced PCA plots created successfully\n")
}, error = function(e) {
  cat("✗ Error creating advanced PCA plots:", e$message, "\n")
  stop(e)
})

# Prepare for Deep Learning
cat("\nPreparing data for Deep Learning...\n")
tryCatch({
  source("src/s04_pca/prepare_ml_data.R")
  cat("✓ Data prepared successfully for Deep Learning\n")
}, error = function(e) {
  cat("✗ Error preparing data for Deep Learning:", e$message, "\n")
  stop(e)
})
  

cat("All PCA configurations completed!\n")
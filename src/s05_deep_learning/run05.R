# src/s05_deep_learning/run05.R - Deep learning model training script

cat("=== Step 5: Deep Learning ===\n")
# Load necessary libraries
library(tidyverse)

# Ensure configuration is loaded

source("config/setup.R")

# define some variables
feature <- "alpha_data"
target <- "light_type"
log_trans <- TRUE
L2_trans <- TRUE
PCA_trans <- FALSE
parwise_split <- FALSE
binary_classification <- TRUE
imbalance_handling <- "None"
include_hour <- TRUE

# Load training and testing data
cat("\nLoading training and testing data...\n")
tryCatch({
  source("src/s05_deep_learning/load_train_test.R")
  cat("✓ Training and Testing data loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading training and testing data:", e$message, "\n")
  stop(e)
})

# Train deep learning model
cat("\nTraining deep learning model...\n")
tryCatch({
  # source("src/s05_deep_learning/train_model.R")
  source("src/s05_deep_learning/bayesian_r_wrapper.R")
  cat("✓ Model trained successfully\n")
}, error = function(e) {
  cat("✗ Error training model:", e$message, "\n")
  stop(e)
})

# Evaluate deep learning model
cat("\nEvaluating deep learning model...\n")
tryCatch({
  # source("src/s05_deep_learning/evaluate_model.R")
  source("src/s05_deep_learning/evaluate_r_wrapper.R")
  cat("✓ Model evaluated successfully\n")
}, error = function(e) {
  cat("✗ Error evaluating model:", e$message, "\n")
  stop(e)
})
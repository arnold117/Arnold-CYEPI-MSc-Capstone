# src/s03_eda/run03.R - Exploratory Data Analysis Control Script

cat("=== Step 3: Exploratory Data Analysis (EDA) ===\n")

# ensure configuration is loaded
if (!exists("config") || !exists("paths") || !exists("project_root")) {
  source("config/setup.R")
}

# Prepare data
cat("\nPreparing data for EDA...\n")
tryCatch({
  source("src/s03_eda/prepare_data.R")
  cat("✓ Data prepared successfully for EDA\n")
}, error = function(e) {
  cat("✗ Error preparing data for EDA:", e$message, "\n")
  stop(e)
})

# label anaysis
cat("\nPerforming label analysis...\n")
tryCatch({
  source("src/s03_eda/label_analysis.R")
  cat("✓ Label analysis completed successfully\n")
}, error = function(e) {
  cat("✗ Error in label analysis:", e$message, "\n")
  stop(e)
})

# feature analysis
cat("\nPerforming feature analysis...\n")
tryCatch({
  source("src/s03_eda/feature_analysis.R")
  cat("✓ Feature analysis completed successfully\n")
}, error = function(e) {
  cat("✗ Error in feature analysis:", e$message, "\n")
  stop(e)
})

# label-feature relationship analysis
cat("\nPerforming label-feature relationship analysis...\n")
tryCatch({
  source("src/s03_eda/label_feature_analysis.R")
  cat("✓ Label-feature relationship analysis completed successfully\n")
}, error = function(e) {
  cat("✗ Error in label-feature relationship analysis:", e$message, "\n")
  stop(e)
})

cat("\n=== EDA Summary ===\n")
cat("EDA completed successfully!\n")
cat("Generated visualizations are saved in:", paths$eda_outputs, "\n")
cat("Check the EDA_Report.md for a complete summary of all generated plots.\n")
# The main processing programme

# Set working directory and load configuration
source("config/setup.R")

# Define which steps to run
run_steps <- list(
  data_loading = FALSE,
  preprocessing = TRUE,
  eda = FALSE,
  pca = FALSE,
  machine_learning = FALSE
)

# Run pipeline
cat("Starting LightSPAN Cyepi Analysis Pipeline...\n")

if (run_steps$data_loading) {
  cat("Step 1: Data Loading...\n")
  source("src/s01_data_loading/run01.R")
}

if (run_steps$preprocessing) {
  cat("Step 2: Preprocessing...\n")
  source("src/s02_preprocessing/run02.R")
}

if (run_steps$eda) {
  cat("Step 3: Exploratory Data Analysis...\n")
  source("src/s03_eda/run03.R")
}

if (run_steps$pca) {
  cat("Step 4: PCA Analysis...\n")
  source("src/s04_pca/run04.R")
}

if (run_steps$machine_learning) {
  cat("Step 5: Machine Learning...\n")
  source("src/s05_machine_learning/run05.R")
}

cat("Pipeline completed!\n")
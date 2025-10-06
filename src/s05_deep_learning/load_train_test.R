# Build file name based on features and transformations
if (log_trans) {
  feature <- paste0(feature, "_log")
}

if (L2_trans) {
  feature <- paste0(feature, "_l2")
  if (!is.na(include_l2_intensity) && !include_l2_intensity) {
    feature <- paste0(feature, "_noi")
  }
}

if (PCA_trans) {
  feature <- paste0(feature, "_pca")
}

file_name <- paste0(feature, "_agg")

if (include_hour) {
  file_name <- paste0(file_name, "_inctime")
}

if (parwise_split) {
  file_name <- paste0(file_name, "_parwise")
} else {
  file_name <- paste0(file_name, "_stratified")
}

file_name <- paste0(file_name, "_", target)

# Add binary classification suffix if applicable
if (exists("binary_classification") && binary_classification) {
  file_name <- paste0(file_name, "_binary")
} else {
  file_name <- paste0(file_name, "_multiclass")
}

# Add imbalance handling suffix if applicable
if (imbalance_handling != "None") {
  file_name <- paste0(file_name, "_", imbalance_handling)
}

if (PCA_trans) {
  # Try to load preprocessed data from the new pipeline
  train_file <- file.path(paths$data_dl_ready, paste0(file_name, "_train.rds"))
  test_file <- file.path(paths$data_dl_ready, paste0(file_name, "_test.rds"))
} else {
  # Fallback to split folder for non-PCA data
  train_file <- file.path(paths$data_split, paste0(file_name, "_train.rds"))
  test_file <- file.path(paths$data_split, paste0(file_name, "_test.rds"))
}

# Initialize data variables
train_data <- NULL
test_data <- NULL

cat("Looking for files:\n")
cat("  Train:", train_file, "\n")
cat("  Test:", test_file, "\n")

if (file.exists(train_file) && file.exists(test_file)) {
  cat("✓ Loading data from new preprocessing pipeline...\n")
  train_data <- readRDS(train_file)
  test_data <- readRDS(test_file)
  
  # Analyze feature types in the loaded data
  cat("\nAnalyzing feature types in the data...\n")
  all_cols <- names(train_data)
  
  # 1. Identifiers (always remove)
  identifier_cols <- intersect(c("Id", "Datetime", "hour"), all_cols)
  
  # 2. Target variable
  target_cols <- intersect(target, all_cols)
  
  # 3. Time features (hour_sin, hour_cos)
  time_feature_cols <- intersect(c("hour_sin", "hour_cos"), all_cols)
  
  # 4. PCA components (PC1, PC2, PC3, ...)
  pca_cols <- grep("^PC\\d+$", all_cols, value = TRUE)
  
  # 5. Light vector features (original spectral features)
  # These typically start with specific patterns
  light_vector_patterns <- c("^norm_", "^F\\d+$", "^PHO$", "^MEDI$", "^SC$", "^MC$", "^LC$", "^RH$", 
                            "^X\\d+$", "^CLEAR$", "^IR\\.LIGHT$")
  light_vector_cols <- c()
  for (pattern in light_vector_patterns) {
    light_vector_cols <- c(light_vector_cols, grep(pattern, all_cols, value = TRUE))
  }
  light_vector_cols <- unique(light_vector_cols)
  
  # 6. Other numeric features (intensity, etc.)
  other_numeric_cols <- setdiff(
    all_cols[sapply(train_data, is.numeric)],
    c(identifier_cols, target_cols, time_feature_cols, pca_cols, light_vector_cols)
  )

  # if include_hour is False, remove hour_sin and hour_cos
  if (!include_hour) {
    train_data <- train_data %>%
      select(-any_of(time_feature_cols))
    test_data <- test_data %>%
      select(-any_of(time_feature_cols))
  }
  
  # Standardize 'Intensity' column if it exists
  if ("Intensity" %in% all_cols) {
    cat("Standardizing 'Intensity' column...\n")
    train_data <- train_data %>%
      mutate(Intensity = as.numeric(scale(Intensity)))
    test_data <- test_data %>%
      mutate(Intensity = as.numeric(scale(Intensity)))
  }

  # Cleaning NA values
  cat("Cleaning NA values...\n")
  train_data <- train_data %>%
    select(-any_of(identifier_cols)) %>%
    filter(complete.cases(.)) %>%
    filter(rowSums(select(., -any_of(target_cols))) != 0)
  test_data <- test_data %>%
    select(-any_of(identifier_cols)) %>%
    filter(complete.cases(.)) %>%
    filter(rowSums(select(., -any_of(target_cols))) != 0)

  cat("Feature analysis:\n")
  cat("  Identifiers (", length(identifier_cols), "):", paste(identifier_cols, collapse = ", "), "\n")
  cat("  Target (", length(target_cols), "):", paste(target_cols, collapse = ", "), "\n")
  # only print time features when include_hour is True
  if (include_hour && length(time_feature_cols) > 0) {
    cat("  Time features (", length(time_feature_cols), "):", paste(time_feature_cols, collapse = ", "), "\n")
  } else {
    cat("  Time features: None\n")
  }
  cat("  PCA components (", length(pca_cols), "):", if(length(pca_cols) > 0) paste(head(pca_cols, 5), collapse = ", ") else "None", 
      if(length(pca_cols) > 5) "..." else "", "\n")
  cat("  Light vectors (", length(light_vector_cols), "):", if(length(light_vector_cols) > 0) paste(head(light_vector_cols, 5), collapse = ", ") else "None",
      if(length(light_vector_cols) > 5) "..." else "", "\n")
  cat("  Other numeric (", length(other_numeric_cols), "):", paste(other_numeric_cols, collapse = ", "), "\n")
  
  # Remove identifiers and ensure target is factor
  train_data <- train_data %>% 
    select(-any_of(identifier_cols)) %>%
    mutate(across(all_of(target), as.factor))
  test_data <- test_data %>% 
    select(-any_of(identifier_cols)) %>%
    mutate(across(all_of(target), as.factor))
  
  # Convert any matrix columns to data.frame format (for H2O compatibility)
  cat("Converting data to data.frame format for H2O compatibility...\n")
  train_data <- as.data.frame(train_data)
  test_data <- as.data.frame(test_data)
  
  # # Additional check: convert any remaining matrix columns to numeric vectors
  # for (col in names(train_data)) {
  #   if (is.matrix(train_data[[col]])) {
  #     cat("Converting matrix column", col, "to vector...\n")
  #     train_data[[col]] <- as.numeric(train_data[[col]])
  #   }
  # }
  # for (col in names(test_data)) {
  #   if (is.matrix(test_data[[col]])) {
  #     cat("Converting matrix column", col, "to vector...\n")
  #     test_data[[col]] <- as.numeric(test_data[[col]])
  #   }
  # }
  
  cat("✓ Data loaded successfully from new pipeline\n")
  cat("Final feature count:", ncol(train_data) - length(target_cols), "features +", length(target_cols), "target\n")
  
}

# Create output directory name
output_file_name <- file_name

# Create output directory
training_output_dir <- file.path(paths$deep_learning_outputs, output_file_name)
if (!dir.exists(training_output_dir)) {
  dir.create(training_output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Save summary and distribution
cat("\nSaving dataset summary...\n")

summary_file <- file.path(training_output_dir, "dataset_summary.txt")

# 将输出写入文件
cat(
  "Dataset Summary:\n",
  "Training:", nrow(train_data), "x", ncol(train_data), "\n",
  "Testing:", nrow(test_data), "x", ncol(test_data), "\n\n",
  
  "Target Distribution (", target, "):\n",
  "Train:", paste(names(table(train_data[[target]])), table(train_data[[target]]), sep=":", collapse=" | "), "\n",
  "Test: ", paste(names(table(test_data[[target]])), table(test_data[[target]]), sep=":", collapse=" | "), "\n",
  
  file = summary_file
)

cat("✓ Summary saved to:", summary_file, "\n")
# Load necessary libraries
library(tidyverse)

# load data from ml_ready folder
cat("Loading data for PCA...\n")

# get all files in the split folder, excluding PCA-processed files
train_files <- list.files(paths$data_split, pattern = "train", full.names = TRUE)

train_datasets <- lapply(train_files, readRDS)
names(train_datasets) <- tools::file_path_sans_ext(basename(train_files))

test_files <- list.files(paths$data_split, pattern = "test", full.names = TRUE)

test_datasets <- lapply(test_files, readRDS)
names(test_datasets) <- tools::file_path_sans_ext(basename(test_files))

cat("Loaded", length(train_datasets), "training datasets:", paste(names(train_datasets), collapse = ", "), "\n")
cat("Loaded", length(test_datasets), "testing datasets:", paste(names(test_datasets), collapse = ", "), "\n")

# Function to extract base dataset name (remove _train/_test suffix)
get_base_name <- function(dataset_name) {
  base_name <- gsub("_(train|test)$", "", dataset_name)
  return(base_name)
}

# Function to parse dataset names and split data by feature types
parse_and_split_datasets <- function(train_datasets, test_datasets) {
  parsed_datasets <- list()

  # Get unique base names
  train_base_names <- sapply(names(train_datasets), get_base_name)
  test_base_names <- sapply(names(test_datasets), get_base_name)
  common_base_names <- intersect(train_base_names, test_base_names)
  
  cat("Found", length(common_base_names), "dataset pairs:", paste(common_base_names, collapse = ", "), "\n\n")
  
  for (base_name in common_base_names) {
    # Find corresponding train and test datasets
    train_name <- names(train_datasets)[train_base_names == base_name]
    test_name <- names(test_datasets)[test_base_names == base_name]
    
    if (length(train_name) != 1 || length(test_name) != 1) {
      cat("Warning: Multiple or missing datasets for base name:", base_name, "\n")
      next
    }
    
    # Parse dataset name
    if (grepl("_l2_", base_name)) {
      parts <- strsplit(base_name, "_l2_")[[1]]
      data_type <- paste0(parts[1], "_l2")
      config <- parts[2]
    } else {
      parts <- strsplit(base_name, "_data_")[[1]]
      data_type <- paste0(parts[1], "_data")
      config <- parts[2]
    }
    
    # Get the datasets
    train_dataset <- train_datasets[[train_name]]
    test_dataset <- test_datasets[[test_name]]  
    
    # 根据列名前缀和类型分离特征
    all_cols <- names(train_dataset)
    
    # 1. 光谱特征（用于PCA降维）- 使用与机器学习相同的模式识别
    light_vector_patterns <- c("^norm_", "^F\\d+$", "^PHO$", "^MEDI$", "^SC$", "^MC$", "^LC$", "^RH$", 
                              "^X\\d+$", "^CLEAR$", "^IR\\.LIGHT$")
    light_vector_cols <- c()
    for (pattern in light_vector_patterns) {
      light_vector_cols <- c(light_vector_cols, grep(pattern, all_cols, value = TRUE))
    }
    light_vector_cols <- unique(light_vector_cols)
    
    # 2. 时间特征（保留）
    time_cols <- intersect(c("hour_sin", "hour_cos"), all_cols)
    
    # 3. 标签列
    label_cols <- intersect(c("light_type", "location"), all_cols)
    
    # 4. 标识符列（直接从数据中识别）
    identifier_cols <- intersect(c("Id", "Datetime", "hour"), all_cols)
    
    # 5. 其他数值特征（强度等，保留原样）
    other_numeric_cols <- setdiff(
      all_cols[sapply(train_dataset, is.numeric)],
      c(light_vector_cols, time_cols, identifier_cols)
    )
    
    # 6. 处理函数：分离不同类型的特征
    split_features <- function(dataset, dataset_type) {
      return(list(
        light_vectors = dataset[, light_vector_cols, drop = FALSE],      # 光谱特征 → PCA
        time_features = dataset[, time_cols, drop = FALSE],              # 时间特征 → 保留
        other_features = dataset[, other_numeric_cols, drop = FALSE],    # 其他数值特征 → 保留
        labels = dataset[, label_cols, drop = FALSE],                    # 标签 → 保留
        identifiers = dataset[, identifier_cols, drop = FALSE]          # 标识符 → 保留
      ))
    }
    
    # 分离训练和测试数据
    train_split <- split_features(train_dataset, "train")
    test_split <- split_features(test_dataset, "test")
    
    # Store parsed information
    parsed_datasets[[base_name]] <- list(
      data_type = data_type,
      config = config,
      train = train_split,
      test = test_split,
      feature_summary = list(
        light_vectors = length(light_vector_cols),
        time_features = length(time_cols),
        other_features = length(other_numeric_cols),
        labels = length(label_cols),
        identifiers = length(identifier_cols)
      )
    )
    
    cat("Parsed:", base_name, "\n")
    cat("  Data type:", data_type, "\n")
    cat("  Config:", config, "\n")
    cat("  Light vectors (for PCA):", length(light_vector_cols), "cols\n")
    cat("  Time features:", length(time_cols), "cols\n")
    cat("  Other features:", length(other_numeric_cols), "cols\n")
    cat("  Labels:", length(label_cols), "cols\n")
    cat("  Identifiers:", length(identifier_cols), "cols\n")
    cat("  Train samples:", nrow(train_dataset), "| Test samples:", nrow(test_dataset), "\n\n")
  }
  
  return(parsed_datasets)
}

# Parse and split datasets
cat("Parsing and splitting datasets...\n")

parsed_datasets <- parse_and_split_datasets(train_datasets, test_datasets)

# Function to clean only light vector features and return valid row indices
clean_light_vectors <- function(light_vectors_df, dataset_name, data_type) {
  original_cols <- ncol(light_vectors_df)
  original_rows <- nrow(light_vectors_df)
  
  if (original_cols == 0) {
    cat("No light vector features found in", dataset_name, "(", data_type, ")\n")
    return(list(
      light_vectors_cleaned = light_vectors_df,
      valid_rows = rep(TRUE, original_rows)
    ))
  }
  
  # Remove columns with all NA values
  na_cols <- sapply(light_vectors_df, function(x) all(is.na(x)))
  light_vectors_cleaned <- light_vectors_df[, !na_cols, drop = FALSE]
  
  # Remove columns with all zeros
  zero_cols <- sapply(light_vectors_cleaned, function(x) all(x == 0, na.rm = TRUE))
  light_vectors_cleaned <- light_vectors_cleaned[, !zero_cols, drop = FALSE]
  
  # Identify rows with complete cases (no NA values) in light vectors
  valid_rows <- complete.cases(light_vectors_cleaned)
  light_vectors_cleaned <- light_vectors_cleaned[valid_rows, , drop = FALSE]
  
  removed_rows <- original_rows - nrow(light_vectors_cleaned)
  
  cat("Cleaned light vectors for", dataset_name, "(", data_type, "):\n")
  cat("  Removed", sum(na_cols), "all-NA columns\n")
  cat("  Removed", sum(zero_cols), "all-zero columns\n")
  cat("  Removed", removed_rows, "rows with NA values\n")
  cat("  Final light vectors:", ncol(light_vectors_cleaned), "cols x", nrow(light_vectors_cleaned), "rows\n\n")
  
  return(list(
    light_vectors_cleaned = light_vectors_cleaned,
    valid_rows = valid_rows
  ))
}

# Clean datasets - 只清理光谱特征，其他特征保持原样
cat("Cleaning light vector features...\n")
for (base_name in names(parsed_datasets)) {
  # Clean training light vectors and get valid row indices
  train_cleaning_result <- clean_light_vectors(
    parsed_datasets[[base_name]]$train$light_vectors, 
    base_name, 
    "train"
  )
  
  # Apply row filtering to all train data components
  valid_train_rows <- train_cleaning_result$valid_rows
  parsed_datasets[[base_name]]$train$light_vectors_cleaned <- train_cleaning_result$light_vectors_cleaned
  parsed_datasets[[base_name]]$train$time_features <- parsed_datasets[[base_name]]$train$time_features[valid_train_rows, , drop = FALSE]
  parsed_datasets[[base_name]]$train$other_features <- parsed_datasets[[base_name]]$train$other_features[valid_train_rows, , drop = FALSE]
  parsed_datasets[[base_name]]$train$labels <- parsed_datasets[[base_name]]$train$labels[valid_train_rows, , drop = FALSE]
  parsed_datasets[[base_name]]$train$identifiers <- parsed_datasets[[base_name]]$train$identifiers[valid_train_rows, , drop = FALSE]
  
  # Clean testing light vectors and get valid row indices
  test_cleaning_result <- clean_light_vectors(
    parsed_datasets[[base_name]]$test$light_vectors, 
    base_name, 
    "test"
  )
  
  # Apply row filtering to all test data components
  valid_test_rows <- test_cleaning_result$valid_rows
  parsed_datasets[[base_name]]$test$light_vectors_cleaned <- test_cleaning_result$light_vectors_cleaned
  parsed_datasets[[base_name]]$test$time_features <- parsed_datasets[[base_name]]$test$time_features[valid_test_rows, , drop = FALSE]
  parsed_datasets[[base_name]]$test$other_features <- parsed_datasets[[base_name]]$test$other_features[valid_test_rows, , drop = FALSE]
  parsed_datasets[[base_name]]$test$labels <- parsed_datasets[[base_name]]$test$labels[valid_test_rows, , drop = FALSE]
  parsed_datasets[[base_name]]$test$identifiers <- parsed_datasets[[base_name]]$test$identifiers[valid_test_rows, , drop = FALSE]
  
  # Verify row counts match across all components
  train_rows <- nrow(parsed_datasets[[base_name]]$train$light_vectors_cleaned)
  test_rows <- nrow(parsed_datasets[[base_name]]$test$light_vectors_cleaned)
  
  cat("Verification for", base_name, ":\n")
  cat("  Train: light vectors", train_rows, "rows")
  cat(", time features", nrow(parsed_datasets[[base_name]]$train$time_features), "rows")
  cat(", other features", nrow(parsed_datasets[[base_name]]$train$other_features), "rows\n")
  cat("  Test:  light vectors", test_rows, "rows")
  cat(", time features", nrow(parsed_datasets[[base_name]]$test$time_features), "rows")
  cat(", other features", nrow(parsed_datasets[[base_name]]$test$other_features), "rows\n\n")
}

cat("All datasets cleaned!\n")
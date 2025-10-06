# src/s05_machine_learning/run_all_binary.R - Run all binary classification combinations

cat("=== Running All Binary Classification Combinations ===\n")
# Load necessary libraries
library(tidyverse)

# Ensure configuration is loaded
if (!exists("config") || !exists("paths") || !exists("project_root")) {
  source("config/setup.R")
}

check_run <- function(feature, target, log_trans, L2_trans, PCA_trans, include_hour, parwise_split, imbalance_handling, binary_classification, include_l2_intensity) {
  # 构建输出文件夹名
  feature_folder <- feature
  if (log_trans) feature_folder <- paste0(feature_folder, "_log")
  if (L2_trans) {
    feature_folder <- paste0(feature_folder, "_l2")
    if (!is.na(include_l2_intensity) && !include_l2_intensity) {
      feature_folder <- paste0(feature_folder, "_noi")
    }
  }
  if (PCA_trans) feature_folder <- paste0(feature_folder, "_pca")
  file_name <- paste0(feature_folder, "_agg")
  if (include_hour) file_name <- paste0(file_name, "_inctime")
  if (parwise_split) {
    file_name <- paste0(file_name, "_parwise")
  } else {
    file_name <- paste0(file_name, "_stratified")
  }
  file_name <- paste0(file_name, "_", target)
  if (exists("binary_classification") && binary_classification) {
    file_name <- paste0(file_name, "_binary")
  } else {
    file_name <- paste0(file_name, "_multiclass")
  }
  if (imbalance_handling != "None") {
    file_name <- paste0(file_name, "_", imbalance_handling)
  }
  output_dir <- file.path(paths$deep_learning_outputs, file_name)

  # 检查输出文件夹
  if (dir.exists(output_dir)) {
    files_in_dir <- list.files(output_dir, recursive = FALSE)
    if (length(files_in_dir) > 1) {
      return(TRUE)  # 已经存在多个文件，跳过
    }
  }
  return(FALSE)  # 不存在或只有一个文件，继续运行
}

# Create parameter grid for all binary classification combinations
create_all_binary_params <- function() {
  # 基础特征类型
  features <- c("alpha_data", "sensor_data", "spd_data")
  
  # 目标变量
  targets <- c("light_type", "location")

  trans_log <- c(TRUE, FALSE)
  trans_l2 <- c(TRUE, FALSE)
  trans_pca <- c(TRUE, FALSE)

  inctime <- c(TRUE, FALSE)

  split_parwise <- c(TRUE, FALSE)
  
  # 不平衡处理方法 (包括无处理和SMOTE-TOMEK)
  imbalance_methods <- c("None", "smote_tomek")

  binary_classification <- c(TRUE, FALSE)
  
  include_l2_intensity_opts <- c(TRUE, FALSE)
  
  # 创建所有组合
  param_combinations <- data.frame(
    feature = character(),
    target = character(),
    log_trans = logical(),
    L2_trans = logical(),
    PCA_trans = logical(),
    include_hour = logical(),
    parwise_split = logical(),
    imbalance_handling = character(),
    binary_classification = logical(),
    include_l2_intensity = logical(),
    stringsAsFactors = FALSE
  )
  
  for (feature in features) {
    for (target in targets) {
      for (log in trans_log) {
        for (l2 in trans_l2) {
          for (pca in trans_pca) {
            for (hour in inctime) {
              for (split in split_parwise) {
                for (binary in binary_classification) {
                  for (imbalance in imbalance_methods) {
                    if (l2) {
                      for (include_l2_intensity in include_l2_intensity_opts) {
                        new_row <- data.frame(
                          feature = feature,
                          target = target,
                          log_trans = log,
                          L2_trans = l2,
                          PCA_trans = pca,
                          include_hour = hour,
                          parwise_split = split,
                          imbalance_handling = imbalance,
                          binary_classification = binary,
                          include_l2_intensity = include_l2_intensity,
                          stringsAsFactors = FALSE
                        )
                        param_combinations <- rbind(param_combinations, new_row)
                      }
                    } else {
                      new_row <- data.frame(
                        feature = feature,
                        target = target,
                        log_trans = log,
                        L2_trans = l2,
                        PCA_trans = pca,
                        include_hour = hour,
                        parwise_split = split,
                        imbalance_handling = imbalance,
                        binary_classification = binary,
                        include_l2_intensity = NA,
                        stringsAsFactors = FALSE
                      )
                      param_combinations <- rbind(param_combinations, new_row)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  return(param_combinations)
}

# 创建参数网格
param_grid <- create_all_binary_params()

cat("Created parameter grid with", nrow(param_grid), "binary classification combinations:\n")
cat("Features:", paste(unique(param_grid$feature), collapse = ", "), "\n")
cat("Targets:", paste(unique(param_grid$target), collapse = ", "), "\n")
cat("Transformations: l2, l2_pca, log, log_pca\n")
cat("Imbalance handling: None, SMOTE-TOMEK\n")
cat("Classification: Binary only\n\n")

cat("Processing all", nrow(param_grid), "combinations.\n\n")

# 训练每个组合
successful_runs <- 0
failed_runs <- 0
failed_combinations <- c()

for (i in 1:nrow(param_grid)) {
  cat("\n", strrep("=", 80), "\n")
  cat("Processing combination", i, "of", nrow(param_grid), "\n")
  cat(strrep("=", 80), "\n")
  
  # 直接使用参数网格的值
  feature <- param_grid$feature[i]
  target <- param_grid$target[i]
  log_trans <- param_grid$log_trans[i]
  L2_trans <- param_grid$L2_trans[i]
  PCA_trans <- param_grid$PCA_trans[i]
  include_hour <- param_grid$include_hour[i]
  parwise_split <- param_grid$parwise_split[i]
  imbalance_handling <- param_grid$imbalance_handling[i]
  binary_classification <- TRUE # Always binary for this script
  include_l2_intensity <- param_grid$include_l2_intensity[i]
  
  cat("Parameters:\n")
  cat("  feature:", feature, "\n")
  cat("  target:", target, "\n")
  cat("  log_trans:", log_trans, "\n")
  cat("  L2_trans:", L2_trans, "\n")
  cat("  include_l2_intensity:", include_l2_intensity, "\n")
  cat("  PCA_trans:", PCA_trans, "\n")
  cat("  include_hour:", include_hour, "\n")
  cat("  parwise_split:", parwise_split, "\n")
  cat("  imbalance_handling:", imbalance_handling, "\n")
  cat("  binary_classification:", binary_classification, "\n")
  
  tryCatch({
    if (check_run(feature, target, log_trans, L2_trans, PCA_trans, include_hour, parwise_split, imbalance_handling, binary_classification, include_l2_intensity)) {
      cat("Skipping combination", i, "- output folder already contains more than one file.\n")
      next
    }
    # Load training and testing data
    cat("Loading training and testing data...\n")
    source("src/s05_deep_learning/load_train_test.R")
    
    # Check if data was loaded successfully
    if (is.null(train_data) || is.null(test_data)) {
      stop("Failed to load training or testing data")
    }
    cat("✓ Training and Testing data loaded successfully\n")
    
    # Train deep learning model
    cat("Training deep learning model...\n")
    source("src/s05_deep_learning/bayesian_r_wrapper.R")
    cat("✓ Model trained successfully\n")
    
    # Evaluate deep learning model
    cat("Evaluating deep learning model...\n")
    source("src/s05_deep_learning/evaluate_r_wrapper.R")
    cat("✓ Model evaluated successfully\n")
    
    successful_runs <- successful_runs + 1
    cat("✓ Combination", i, "completed successfully!\n")
    
  }, error = function(e) {
    failed_runs <- failed_runs + 1
    combination_id <- paste(feature, target, L2_trans, PCA_trans, imbalance_handling, binary_classification, sep="_")
    failed_combinations <- c(failed_combinations, combination_id)
    
    cat("✗ Error in combination", i, ":", e$message, "\n")
    cat("Failed combination:", combination_id, "\n")
    
    # 记录错误到文件
    error_log_file <- file.path(paths$deep_learning_outputs, "error_log.txt")
    cat(
      paste0("Timestamp: ", Sys.time(), "\n"),
      paste0("Combination: ", combination_id, "\n"),
      paste0("Error: ", e$message, "\n"),
      paste0("Parameters: feature=", feature, ", target=", target, 
             ", L2=", L2_trans, ", PCA=", PCA_trans, 
             ", imbalance=", imbalance_handling, 
             ", binary=", binary_classification, "\n"),
      strrep("-", 50), "\n\n",
      file = error_log_file,
      append = TRUE
    )
  })
}

# 最终总结
cat("\n", strrep("=", 80), "\n")
cat("FINAL SUMMARY\n")
cat(strrep("=", 80), "\n")
cat("Total combinations processed:", nrow(param_grid), "\n")
cat("Successful runs:", successful_runs, "\n")
cat("Failed runs:", failed_runs, "\n")

if (failed_runs > 0) {
  cat("\nFailed combinations:\n")
  for (combo in failed_combinations) {
    cat("  -", combo, "\n")
  }
  cat("\nError details have been logged to:", file.path(paths$deep_learning_outputs, "error_log.txt"), "\n")
}

cat("\nAll binary classification training completed!\n")
cat("Results saved in:", paths$deep_learning_outputs, "\n")

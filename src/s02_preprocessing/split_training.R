# Load necessary libraries
library(tidyverse)
library(themis)  # For SMOTE-TOMEK

# Ensure configuration is loaded
if (!exists("config") || !exists("paths") || !exists("project_root")) {
  source("config/setup.R")
}

# Load aggregated data
cat("Loading aggregated data...\n")

# 自动读取 data_aggregated 目录下所有 .rds 文件
agg_files <- list.files(paths$data_aggregated, pattern = "\\.rds$", full.names = TRUE)
datasets <- list()
for (file in agg_files) {
  dataset_name <- sub("\\.rds$", "", basename(file))
  cat("Loading", dataset_name, "...\n")
  datasets[[dataset_name]] <- readRDS(file)
  cat("✓ Loaded", dataset_name, "successfully\n")
  cat("  Records:", nrow(datasets[[dataset_name]]), "\n")
  cat("  Participants:", length(unique(datasets[[dataset_name]]$Id)), "\n")
}

# Remove NULL datasets (files that weren't found)
datasets <- datasets[!sapply(datasets, is.null)]

cat("Successfully loaded", length(datasets), "aggregated datasets\n")
cat("\n=== Participant-wise Train/Test Split ===\n")

# 获取所有参与者ID（使用第一个数据集作为参考）
if (length(datasets) > 0) {
  all_participants <- unique(datasets[[1]]$Id)
  n_participants <- length(all_participants)
  
  # 设置随机种子确保可重现性
  set.seed(config$machine_learning$random_seed)
  
  # 按参与者分割（80/20）
  train_size <- floor(0.8 * n_participants)
  train_participants <- sample(all_participants, size = train_size)
  test_participants <- setdiff(all_participants, train_participants)
  
  cat("Total participants:", n_participants, "\n")
  cat("Train participants:", length(train_participants), "\n")
  cat("Test participants:", length(test_participants), "\n")
  
  # 定义要处理的标签任务
  label_tasks <- list(
    light_type = list(
      target_column = "light_type",
      remove_columns = c("location", "main_light")
    ),
    location = list(
      target_column = "location", 
      remove_columns = c("light_type", "main_light")
    )
  )
  
  # 定义分类类型配置
  classification_types <- list(
    binary = list(
      suffix = "binary",
      exclude_classes = list(
        light_type = c("Dark"),      # light_type二分类排除Dark
        location = c("Dark")         # location二分类也排除Dark
      )
    ),
    multiclass = list(
      suffix = "multiclass", 
      exclude_classes = NULL
    )
  )
  
  # 定义SMOTE-TOMEK处理配置
  smote_tomek_options <- list(
    none = list(
      suffix = "",
      apply_smote_tomek = FALSE
    ),
    smote_tomek = list(
      suffix = "_smote_tomek",
      apply_smote_tomek = TRUE
    )
  )
  
  # 加载handle_imbalance函数
  source("src/s02_preprocessing/handle_imbalance.R")
  
  cat("\n=== Processing all combinations ===\n")
  
  # 新增分割方式和时间变量组合
  split_methods <- list(
    parwise = list(
      suffix = "parwise",
      split_fun = function(data) {
        list(
          train = data %>% filter(Id %in% train_participants),
          test = data %>% filter(Id %in% test_participants)
        )
      }
    ),
    stratified = list(
      suffix = "stratified",
      split_fun = function(data, strat_col) {
        # 按strat_col分层抽样
        if (!is.null(strat_col) && strat_col %in% names(data)) {
          set.seed(config$machine_learning$random_seed)
          train_idx <- unlist(
            tapply(seq_len(nrow(data)), data[[strat_col]], function(idx) {
              sample(idx, size = floor(0.8 * length(idx)))
            })
          )
          list(
            train = data[train_idx, ],
            test = data[-train_idx, ]
          )
        } else {
          # fallback: 全部分到train
          list(train = data, test = data[0, ])
        }
      }
    )
  )
  include_time_options <- list(
    no_time = list(suffix = ""),
    inctime = list(suffix = "inctime")
  )

  # 六层循环处理所有组合
  for (dataset_name in names(datasets)) {
    cat("\n--- Processing dataset:", dataset_name, "---\n")
    data <- datasets[[dataset_name]] %>% ungroup()
    for (include_time in names(include_time_options)) {
      time_config <- include_time_options[[include_time]]
      # 是否保留时间变量
      time_vars <- c("hour_cos", "hour_sin")
      for (split_method in names(split_methods)) {
        split_config <- split_methods[[split_method]]
        for (task_name in names(label_tasks)) {
          task_config <- label_tasks[[task_name]]
          cat("\n  Task:", task_name, "(", task_config$target_column, ")\n")
          # 检查目标列是否存在
          if (!task_config$target_column %in% names(data)) {
            cat("    ⚠ Warning: Target column", task_config$target_column, "not found in", dataset_name, "- skipping\n")
            next
          }
          # 分割数据，stratified时传入当前标签列
          if (split_method == "stratified") {
            split_data <- split_config$split_fun(data, task_config$target_column)
          } else {
            split_data <- split_config$split_fun(data)
          }
          train_data_base <- split_data$train
          test_data_base <- split_data$test
          # 是否包含时间变量
          if (time_config$suffix == "") {
            train_data_base <- train_data_base %>% select(-any_of(time_vars))
            test_data_base <- test_data_base %>% select(-any_of(time_vars))
          }
          for (class_type in names(classification_types)) {
            class_config <- classification_types[[class_type]]
            cat("    Classification:", class_type, "\n")
            for (smote_option in names(smote_tomek_options)) {
              smote_config <- smote_tomek_options[[smote_option]]
              cat("      SMOTE-TOMEK:", smote_option, "\n")
              # ...existing code...
              train_processed <- train_data_base %>%
                # 删除不需要的列
                select(-any_of(task_config$remove_columns)) %>%
                # 处理缺失值
                filter(complete.cases(.)) %>%
                # 移除数值特征完全相同的行（排除目标列和Intensity列）
                {
                  # 获取数值列（排除目标列、Intensity列和非数值列）
                  numeric_cols <- sapply(., is.numeric)
                  numeric_cols[task_config$target_column] <- FALSE  # 排除目标列
                  if ("Intensity" %in% names(.)) {
                    numeric_cols["Intensity"] <- FALSE  # 排除Intensity列
                  }
                  numeric_data <- select(., which(numeric_cols))
                  if (ncol(numeric_data) > 0) {
                    # 检查是否有数值特征完全相同的行
                    original_rows <- nrow(.)
                    # 计算每行的唯一值数量，如果所有数值特征都相同，唯一值数量为1
                    row_variance <- apply(numeric_data, 1, function(x) {
                      if (all(is.na(x))) return(0)  # 全NA行
                      if (length(unique(x[!is.na(x)])) <= 1) return(0)  # 所有非NA值都相同
                      return(1)  # 有变化
                    })
                    # 保留有变化的行
                    result <- filter(., row_variance > 0)
                    removed_rows <- original_rows - nrow(result)
                    if (removed_rows > 0) {
                      cat("        Removed", removed_rows, "rows with identical numeric features\n")
                    }
                    result
                  } else {
                    # 如果没有数值列，保持原样
                    .
                  }
                } %>%
                {
                  # 处理分类类型（二分类时排除特定类别）
                  if (class_config$suffix == "binary" && !is.null(class_config$exclude_classes)) {
                    # 根据目标列获取对应的排除类别
                    exclude_classes_for_task <- class_config$exclude_classes[[task_config$target_column]]
                    if (!is.null(exclude_classes_for_task)) {
                      cat("        Excluding classes for binary classification:", paste(exclude_classes_for_task, collapse=", "), "\n")
                      original_rows <- nrow(.)
                      for (exclude_class in exclude_classes_for_task) {
                        . <- filter(., .[[task_config$target_column]] != exclude_class)
                      }
                      removed_rows <- original_rows - nrow(.)
                      cat("        Removed", removed_rows, "rows\n")
                      .
                    } else {
                      cat("        No exclusion rules defined for", task_config$target_column, "binary classification\n")
                      .
                    }
                  } else {
                    .
                  }
                } %>%
                # 转换目标列为factor
                mutate(across(all_of(task_config$target_column), as.factor))
              # 处理测试数据（与训练数据相同的预处理，但不应用SMOTE-TOMEK）
              test_processed <- test_data_base %>%
                select(-any_of(task_config$remove_columns)) %>%
                filter(complete.cases(.)) %>%
                # 移除数值特征完全相同的行（排除目标列和Intensity列）
                {
                  # 获取数值列（排除目标列、Intensity列和非数值列）
                  numeric_cols <- sapply(., is.numeric)
                  numeric_cols[task_config$target_column] <- FALSE  # 排除目标列
                  if ("Intensity" %in% names(.)) {
                    numeric_cols["Intensity"] <- FALSE  # 排除Intensity列
                  }
                  numeric_data <- select(., which(numeric_cols))
                  if (ncol(numeric_data) > 0) {
                    # 检查是否有数值特征完全相同的行
                    original_rows <- nrow(.)
                    # 计算每行的唯一值数量，如果所有数值特征都相同，唯一值数量为1
                    row_variance <- apply(numeric_data, 1, function(x) {
                      if (all(is.na(x))) return(0)  # 全NA行
                      if (length(unique(x[!is.na(x)])) <= 1) return(0)  # 所有非NA值都相同
                      return(1)  # 有变化
                    })
                    # 保留有变化的行
                    result <- filter(., row_variance > 0)
                    removed_rows <- original_rows - nrow(result)
                    if (removed_rows > 0) {
                      cat("        Removed", removed_rows, "rows with identical numeric features\n")
                    }
                    result
                  } else {
                    # 如果没有数值列，保持原样
                    .
                  }
                } %>%
                {
                  if (class_config$suffix == "binary" && !is.null(class_config$exclude_classes)) {
                    exclude_classes_for_task <- class_config$exclude_classes[[task_config$target_column]]
                    if (!is.null(exclude_classes_for_task)) {
                      original_rows <- nrow(.)
                      for (exclude_class in exclude_classes_for_task) {
                        . <- filter(., .[[task_config$target_column]] != exclude_class)
                      }
                      .
                    } else {
                      .
                    }
                  } else {
                    .
                  }
                } %>%
                mutate(across(all_of(task_config$target_column), as.factor))
              # 应用SMOTE-TOMEK（仅对训练数据）
              if (smote_config$apply_smote_tomek) {
                cat("        Applying SMOTE-TOMEK...\n")
                # 临时设置输出目录（handle_imbalance.R需要这个全局变量）
                temp_output_dir <- file.path("temp", "smote_tomek_logs")
                if (!dir.exists(temp_output_dir)) {
                  dir.create(temp_output_dir, recursive = TRUE, showWarnings = FALSE)
                }
                training_output_dir <<- temp_output_dir
                # 保存标识符列以便之后恢复
                identifier_cols <- c("Id", "Datetime", "hour","hour_sin", "hour_cos")
                existing_identifier_cols <- intersect(identifier_cols, names(train_processed))
                # 如果有标识符列，先保存它们
                if (length(existing_identifier_cols) > 0) {
                  train_identifiers <- train_processed %>% select(all_of(existing_identifier_cols))
                  # 从数据中移除标识符列进行SMOTE-TOMEK处理
                  train_for_smote <- train_processed %>% select(-all_of(existing_identifier_cols))
                } else {
                  train_for_smote <- train_processed
                }
                # 获取特征列（排除目标列）
                feature_cols <- setdiff(names(train_for_smote), task_config$target_column)
                # 应用SMOTE-TOMEK
                train_balanced <- apply_imbalance_handling(
                  train_for_smote,
                  feature_cols,
                  task_config$target_column,
                  "SMOTE-TOMEK"
                )
                # 如果原来有标识符列，需要为新生成的行创建标识符
                if (length(existing_identifier_cols) > 0) {
                  # 对于新生成的行，创建新的标识符
                  original_rows <- nrow(train_for_smote)
                  balanced_rows <- nrow(train_balanced)
                  if (balanced_rows > original_rows) {
                    # 有新生成的行
                    new_rows <- balanced_rows - original_rows
                    cat("        Generated", new_rows, "new synthetic samples\n")
                    # 为原始行恢复原来的标识符
                    extended_identifiers <- train_identifiers[rep(seq_len(original_rows), length.out = balanced_rows), , drop = FALSE]
                    # 为新生成的行创建合成标识符
                    if ("Id" %in% existing_identifier_cols) {
                      # 为新行创建合成ID（添加"_synthetic"后缀）
                      synthetic_start <- original_rows + 1
                      for (i in synthetic_start:balanced_rows) {
                        original_idx <- ((i - 1) %% original_rows) + 1
                        extended_identifiers$Id[i] <- paste0(train_identifiers$Id[original_idx], "_synthetic_", i - original_rows)
                      }
                    }
                    # 重新组合数据
                    train_processed <- bind_cols(extended_identifiers, train_balanced)
                  } else {
                    # 没有新生成的行，直接组合
                    extended_identifiers <- head(train_identifiers, balanced_rows)
                  }
                  train_processed <- bind_cols(extended_identifiers, train_balanced)
                } else {
                  train_processed <- train_balanced
                }
                cat("        ✓ SMOTE-TOMEK applied successfully\n")
              }
              # 构建文件名
              file_suffix <- paste0(
                ifelse(time_config$suffix == "", "", paste0(time_config$suffix, "_")),
                split_config$suffix, "_",
                task_name, "_",
                class_config$suffix,
                smote_config$suffix
              )
              train_file <- file.path(paths$data_split, paste0(dataset_name, "_", file_suffix, "_train.rds"))
              test_file <- file.path(paths$data_split, paste0(dataset_name, "_", file_suffix, "_test.rds"))
              # 保存文件
              saveRDS(train_processed, train_file)
              saveRDS(test_processed, test_file)
              cat("        ✓ Saved:", basename(train_file), "(", nrow(train_processed), "rows)\n")
              cat("        ✓ Saved:", basename(test_file), "(", nrow(test_processed), "rows)\n")
              # 打印类别分布信息
              train_dist <- table(train_processed[[task_config$target_column]])
              test_dist <- table(test_processed[[task_config$target_column]])
              cat("        Train distribution:", paste(names(train_dist), train_dist, sep="=", collapse=", "), "\n")
              cat("        Test distribution:", paste(names(test_dist), test_dist, sep="=", collapse=", "), "\n")
            }
          }
        }
      }
    }
    # 保存参与者分割信息以确保一致性
    participant_splits <- list(
      train_participants = train_participants,
      test_participants = test_participants,
      split_method = "participant_wise",
      split_ratio = 0.8,
      random_seed = config$machine_learning$random_seed
    )
    saveRDS(participant_splits, file.path(paths$data_split, "participant_splits.rds"))
    cat("\n✓ Saved participant split information\n")
  }
}
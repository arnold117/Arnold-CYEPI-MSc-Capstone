library(tidyverse)
library(stringr)
library(readr)
library(ggplot2)
library(viridis)

# 确保配置已加载
source("config/setup.R")

# 1. 递归查找所有目标文件
find_all_result_files <- function(root_dir) {
  folders <- list.dirs(root_dir, recursive = FALSE, full.names = TRUE)
  result <- tibble()
  for (folder in folders) {
    bo_file <- file.path(folder, "bayesian_optimization_history.csv")
    metrics_file <- file.path(folder, "metrics_and_confusion_matrix.txt")
    if (file.exists(bo_file) && file.exists(metrics_file)) {
      result <- bind_rows(result, tibble(folder = folder, bo_file = bo_file, metrics_file = metrics_file))
    }
  }
  return(result)
}

# 2. 解析文件夹名为参数
parse_folder_name <- function(folder_name) {
  # 例：spd_data_log_pca_agg_inctime_stratified_location_binary_smote_tomek
  parts <- str_split(basename(folder_name), "_")[[1]]
  # 默认值
  info <- list(
    data = NA, log = FALSE, l2 = "False", pca = FALSE, hour = FALSE,
    split = NA, target = NA, binary = FALSE, imbalance = "None"
  )
  # 数据类型
  if (str_starts(parts[1], "alpha")) info$data <- "alpha"
  if (str_starts(parts[1], "sensor")) info$data <- "sensor"
  if (str_starts(parts[1], "spd")) info$data <- "spd"
  # 变换
  info$log <- "log" %in% parts
  if ("l2" %in% parts) {
    if ("noi" %in% parts) info$l2 <- "Exclude Intensity"
    else info$l2 <- "Include Intensity"
  }
  info$pca <- "pca" %in% parts
  info$hour <- "inctime" %in% parts
  # split
  info$split <- ifelse("parwise" %in% parts, "parwise", "stratified")
  # target
  if ("light" %in% parts) info$target <- "light_type"
  if ("location" %in% parts) info$target <- "location"
  # binary
  info$binary <- "binary" %in% parts
  # imbalance
  if ("smote" %in% parts && "tomek" %in% parts) info$imbalance <- "smote_tomek"
  else if ("smote" %in% parts) info$imbalance <- "smote"
  else if ("tomek" %in% parts) info$imbalance <- "tomek"
  return(info)
}

# 3. 读取bayesian optimization第一行
read_bayesian_first_row <- function(bo_file) {
  df <- read_csv(bo_file, show_col_types = FALSE)
  if (nrow(df) > 0) return(df[1, ])
  else return(NULL)
}

# 4. 读取metrics
read_metrics <- function(metrics_file) {
  lines <- readLines(metrics_file, warn = FALSE)
  # 只提取AUC、accuracy、precision、recall
  auc <- as.numeric(str_match(lines[grepl("AUC", lines)], "([0-9.]+)")[,2])
  acc <- as.numeric(str_match(lines[grepl("Accuracy", lines)], "([0-9.]+)")[,2])
  prec <- as.numeric(str_match(lines[grepl("Precision", lines)], "([0-9.]+)")[,2])
  recall <- as.numeric(str_match(lines[grepl("Recall", lines)], "([0-9.]+)")[,2])
  tibble(auc = auc, accuracy = acc, precision = prec, recall = recall)
}

# 5. 聚合所有结果
aggregate_results <- function(root_dir) {
  files <- find_all_result_files(root_dir)
  bo_results <- list()
  metrics_results <- list()
  for (i in seq_len(nrow(files))) {
    info <- parse_folder_name(files$folder[i])
    bo_row <- read_bayesian_first_row(files$bo_file[i])
    metrics_row <- read_metrics(files$metrics_file[i])
    # 聚合
    bo_results[[i]] <- bind_cols(as_tibble(info), bo_row)
    metrics_results[[i]] <- bind_cols(as_tibble(info), metrics_row)
  }
  bo_df <- bind_rows(bo_results)
  metrics_df <- bind_rows(metrics_results)
  list(bo = bo_df, metrics = metrics_df)
}

# 6. 保存为csv
save_results <- function(bo_df, metrics_df, out_dir) {
  write_csv(bo_df, file.path(out_dir, "all_bayesian_first_row.csv"))
  write_csv(metrics_df, file.path(out_dir, "all_test_metrics.csv"))
}

# ======= 主程序入口 =======
compile_csv <- function() {
  root_dir <- paths$deep_learning_outputs
  out_dir <- root_dir
  results <- aggregate_results(root_dir)
  save_results(results$bo, results$metrics, out_dir)
  cat("汇总完成，结果已保存到：", out_dir, "\n")
}

compile_csv()

# source('src/s05_deep_learning/visualisation.R')
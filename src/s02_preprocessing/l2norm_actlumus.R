# src/s02_preprocessing/l2norm_actlumus.R - L2 Normalization for Actlumus Data

# Load necessary libraries
library(tidyverse)

source("src/utils/l2norm.R")

# 获取所有 *.rds 文件
files <- list.files(paths$data_processed, pattern = ".*\\.rds$", full.names = TRUE)
# 去掉l2.rds的文件
files <- files[!grepl("_l2\\.rds$", files)]
# 去掉transformed_hlea.rds的文件
files <- files[!grepl("transformed_hlea\\.rds$", files)]

for (file in files) {
  cat("Processing:", file, "\n")
  data <- readRDS(file)
  # 提取元数据和数值数据
  meta_cols <- intersect(c("Id", "Datetime"), names(data))
  meta <- data %>% ungroup() %>% select(all_of(meta_cols))
  values <- data %>% ungroup() %>% select(-all_of(meta_cols))
  col_names <- names(values)
  # L2归一化和强度
  normalized <- t(apply(values, 1, l2_norm))
  intensity <- apply(values, 1, calc_intensity)
  normalized_df <- as.data.frame(normalized)
  names(normalized_df) <- paste0("norm_", col_names)
  # 合并
  result <- meta %>% bind_cols(normalized_df) %>% mutate(Intensity = intensity)
  # 保存
  out_name <- sub("\\.rds$", "_l2.rds", basename(file))
  out_path <- file.path(paths$data_processed, out_name)
  saveRDS(result, file = out_path)
  cat("L2 Normalized data saved to:", out_path, "\n")
  cat("Records:", nrow(result), "\n")
}
cat("L2 normalization for all *.rds data completed successfully.\n")
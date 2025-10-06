# src/s02_preprocessing/reconstruct_actlumus.R - Reconstruct Actlumus Data

# load necessary libraries
library(tidyverse)
library(lubridate)

# select useful columns
sensor_data <- actlumus_data %>%
  select(Id, Datetime, F1, F2, F3, F4, F5, F6, F7, F8, CLEAR, IR.LIGHT)

output_path <- file.path(paths$data_processed, "sensor_data.rds")
saveRDS(sensor_data, output_path)

meta_data <- actlumus_data %>%
  select(Id, Datetime)

sensor_sel <- sensor_data %>%
  ungroup() %>%
  select(F1, F2, F3, F4, F5, F6, F7, F8, CLEAR, IR.LIGHT)

# reconstruct alpha-opics from actlumus data
cat("\nReconstructing alpha-opics from actlumus data...\n")
if (file.exists(alpha)) {
  alpha_weight <- read.csv(alpha, header = TRUE)
  alpha_name <- gsub("W", "", alpha_weight[,1])

  alpha_value <- as.matrix(sensor_sel) %*% as.matrix(t(alpha_weight[,-1]))
  alpha_df <- setNames(as.data.frame(alpha_value), alpha_name)
  alpha_data <- cbind(meta_data, alpha_df)
  # ensure alpha_data is greater than 0
  alpha_data[alpha_data < 0] <- 0
  # save reconstructed alpha datas
  output_path <- file.path(paths$data_processed, "alpha_data.rds")
  saveRDS(alpha_data, output_path)
  cat("Alpha-opics reconstruction completed successfully.\n")
  cat("  Saved to:", output_path, "\n")
  cat("  Records:", nrow(alpha_data), "\n")
} else {
  print("Alpha weights file not found: ", alpha)
  print("No reconstruction performed.")
}

# reconstruct spectral power distribution from actlumus data
cat("\nReconstructing spectral power distribution from actlumus data...\n")
if (file.exists(spd)) {
  spd_weight <- read.csv(spd, header = TRUE)
  spd_name <- gsub("W", "", spd_weight[,1])

  spd_value <- as.matrix(sensor_sel) %*% as.matrix(t(spd_weight[,-1]))
  spd_df <- setNames(as.data.frame(spd_value), spd_name)
  spd_data <- cbind(meta_data, spd_df)
  # ensure spd_data is greater than 0
  spd_data[spd_data < 0] <- 0
  # save reconstructed spd data
  output_path <- file.path(paths$data_processed, "spd_data.rds")
  saveRDS(spd_data, output_path)
  cat("SPD reconstruction completed successfully.\n")
  cat("  Saved to:", output_path, "\n")
  cat("  Records:", nrow(spd_data), "\n")
} else {
  print("SPD weights file not found: ", spd)
  print("No reconstruction performed.")
}

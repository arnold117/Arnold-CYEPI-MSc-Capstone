# src/s02_preprocessing/log_transform.R
# 对 sensor_data, alpha_data, spd_data 进行 log10 变换并保存

log_transform_and_save <- function(data, cols, out_path, log_add = 1e-4) {
  data[cols] <- lapply(data[cols], function(x) log10(x + log_add))
  saveRDS(data, out_path)
  return(data)
}

# 仅对存在的数据集进行log变换和保存
if (exists("sensor_data")) {
  sensor_cols <- names(sensor_data)[sapply(sensor_data, is.numeric)]
  sensor_log <- log_transform_and_save(
    sensor_data, 
    sensor_cols, 
    file.path(paths$data_processed, "sensor_data_log.rds"))
  cat("✓ sensor_data log transformed and saved\n")
}
if (exists("alpha_data")) {
  alpha_cols <- names(alpha_data)[sapply(alpha_data, is.numeric)]
  alpha_log <- log_transform_and_save(
    alpha_data, 
    alpha_cols, 
    file.path(paths$data_processed, "alpha_data_log.rds"), 
    log_add = 0.01)
  cat("✓ alpha_data log transformed and saved\n")
}
if (exists("spd_data")) {
  spd_cols <- names(spd_data)[sapply(spd_data, is.numeric)]
  spd_log <- log_transform_and_save(
    spd_data, 
    spd_cols, 
    file.path(paths$data_processed, "spd_data_log.rds"))
  cat("✓ spd_data log transformed and saved\n")
}

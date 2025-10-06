get_location_colors <- function() {
  c(
    "Out" = "#FF8C00",
    "In" = "#87CEEB",
    "Dark" = "#000000"
  )
}
get_light_type_colors <- function() {
  c(
    "Natural" = "#FF8C00",
    "Artificial" = "#0066CC",
    "Dark" = "#000000"
  )
}
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
library(gridExtra)
library(ggpubr)
source(here("src", "utils", "medoid.R"))



# 画单个分布（可选log转换，无medoid）
plot_sensor_distribution <- function(data, group_col, sensor_cols, log_transform = FALSE, facet_col = NULL, output_path, xlabel = "Sensor") {
  df <- data %>%
    select(all_of(c(group_col, "Id", sensor_cols))) %>%
    pivot_longer(cols = all_of(sensor_cols), names_to = "sensor", values_to = "value") %>%
    # remove leading norm or norm_ prefix (case-insensitive) so x labels are cleaner (e.g. norm_F1 -> F1)
    mutate(sensor = sub('^norm[_]?', '', sensor, ignore.case = TRUE)) %>%
    filter(!is.na(.data[[group_col]]), !is.na(value))
  p <- ggplot(df, aes(x = sensor, y = value, fill = .data[[group_col]])) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.7, position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = if (group_col == "location") get_location_colors() else get_light_type_colors()) +
    labs(x = xlabel, y = "Intensity", fill = group_col) +
    theme_bw(base_size = 14) +
    theme(legend.position = "right")
  if (!is.null(facet_col)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_col))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  ggsave(output_path, p, width = 16, height = 10, dpi = 300)
}

# 画medoid曲线（可选分participant）
plot_medoid_curve <- function(data, group_col, sensor_cols, log_transform = FALSE, facet_col = NULL, output_path, xlabel = "Sensor") {
  df <- data %>%
    select(all_of(c(group_col, "Id", sensor_cols))) %>%
    pivot_longer(cols = all_of(sensor_cols), names_to = "sensor", values_to = "value") %>%
    # normalize sensor name for plotting (case-insensitive)
    mutate(sensor = sub('^norm[_]?', '', sensor, ignore.case = TRUE)) %>%
    filter(!is.na(.data[[group_col]]), !is.na(value))
  # Medoid calculation on raw value
    if (is.null(facet_col)) {
    medoid_df <- df %>%
      group_by(.data[[group_col]]) %>%
      summarise(medoid = list(find_medoid(matrix(value, ncol = length(sensor_cols), byrow = FALSE))), .groups = "drop")
    medoid_df <- medoid_df %>%
      mutate(sensor = list(sensor_cols)) %>%
    unnest(cols = c(medoid, sensor)) %>%
    # sensor_cols may contain prefixes like "norm_..."; strip them for plotting (case-insensitive)
    mutate(sensor = sub('^norm[_]?', '', as.character(sensor), ignore.case = TRUE))
    p <- ggplot(medoid_df, aes(x = sensor, y = medoid, color = .data[[group_col]], group = .data[[group_col]])) +
      geom_line(size = 1.5) +
      scale_color_manual(values = if (group_col == "location") get_location_colors() else get_light_type_colors()) +
      labs(x = xlabel, y = "Intensity", color = group_col) +
      theme_bw(base_size = 14) +
      theme(legend.position = "right")
  } else {
    medoid_df <- df %>%
      group_by(.data[[group_col]], .data[[facet_col]]) %>%
      summarise(medoid = list(find_medoid(matrix(value, ncol = length(sensor_cols), byrow = FALSE))), .groups = "drop")
    medoid_df <- medoid_df %>%
      mutate(sensor = list(sensor_cols)) %>%
      unnest(cols = c(medoid, sensor)) %>%
      mutate(sensor = sub('^norm[_]?', '', as.character(sensor)))
    p <- ggplot(medoid_df, aes(x = sensor, y = medoid, color = .data[[group_col]], group = .data[[group_col]])) +
      geom_line(size = 1.2) +
      scale_color_manual(values = if (group_col == "location") get_location_colors() else get_light_type_colors()) +
      labs(x = xlabel, y = "Intensity", color = group_col) +
      theme_bw(base_size = 14) +
      theme(legend.position = "right") +
      facet_wrap(as.formula(paste("~", facet_col))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  ggsave(output_path, p, width = 16, height = 10, dpi = 300)
}

# 绘制传感器均值和误差带
plot_sensor_mean_error <- function(data, group_col, sensor_cols, log_transform = TRUE, facet_col = NULL, output_path, xlabel = "Sensor") {

  # 1. 数据准备：转换为长格式并过滤NA值
  df_long <- data %>%
    select(all_of(c(group_col, "Id", sensor_cols))) %>%
    pivot_longer(cols = all_of(sensor_cols), names_to = "sensor", values_to = "value") %>%
    # remove norm prefix if present so labels are tidy (case-insensitive)
    mutate(sensor = sub('^norm[_]?', '', sensor, ignore.case = TRUE)) %>%
    filter(!is.na(.data[[group_col]]), !is.na(value))

  # 2. 如果需要对值进行对数转换
  if (log_transform) {
    ylabel <- "Log10 Intensity"
  } else {
    ylabel <- "Intensity"
  }

  # 3. 计算每个组别、每个传感器的均值和标准差
  df_summary <- df_long %>%
    group_by(across(all_of(c(group_col, "sensor", facet_col)))) %>% # 确保按facet_col分组如果存在
    summarise(
      mean_val = mean(value, na.rm = TRUE),
      sd_val = sd(value, na.rm = TRUE),
      .groups = 'drop' # 完成汇总后取消分组
    )

  # 4. 绘图
  p <- ggplot(df_summary, aes(x = sensor, y = mean_val, group = .data[[group_col]], color = .data[[group_col]])) +
    # 绘制均值线
    geom_line(aes(linetype = .data[[group_col]]), size = 1) + # 可以根据group_col使用不同的线型
    geom_point(size = 2) + # 添加数据点

    # 绘制误差带 (使用geom_ribbon)
    geom_ribbon(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val, fill = .data[[group_col]]), alpha = 0.2, color = NA) + # color=NA避免边框

    # 绘制误差线 (如果喜欢误差线而不是带，可以替换上面的geom_ribbon或同时使用)
    # geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val), width = 0.2, position = position_dodge(width = 0.7)) +

    scale_color_manual(values = if (group_col == "location") get_location_colors() else get_light_type_colors()) +
    scale_fill_manual(values = if (group_col == "location") get_location_colors() else get_light_type_colors()) + # fill for ribbon
    labs(x = xlabel, y = ylabel, color = group_col, fill = group_col, linetype = group_col) +
    theme_bw(base_size = 14) +
    theme(legend.position = "right")

  # 如果有分面
  if (!is.null(facet_col)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_col)), scales = "free_y") + # scales="free_y" 如果希望每个分面Y轴范围不同
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

  # 保存图表
  ggsave(output_path, p, width = 8, height = 5, dpi = 300)
}

# -------------------------
# Wrappers for alpha / spd
# -------------------------
plot_alpha_mean_error <- function(data, group_col, output_path, log_transform = TRUE, facet_col = NULL, xlabel = "Alpha Opics") {
  sensor_cols <- c("PHO", "MEDI", "SC", "MC", "LC", "RH")
  plot_sensor_mean_error(data = data, group_col = group_col, sensor_cols = sensor_cols,
                         log_transform = log_transform, facet_col = facet_col,
                         output_path = output_path, xlabel = xlabel)
}

plot_spd_mean_error <- function(data, group_col, output_path, log_transform = TRUE, facet_col = NULL, xlabel = "Wavelength (nm)") {
  sensor_cols <- names(data)[startsWith(names(data), "X")]
  plot_sensor_mean_error(data = data, group_col = group_col, sensor_cols = sensor_cols,
                         log_transform = log_transform, facet_col = facet_col,
                         output_path = output_path, xlabel = xlabel)
}

# Convenience functions to read RDS and save standard outputs for alpha / spd
plot_all_alpha_mean_error <- function(data_path = here("data", "d03_aggregated", "alpha_data_log_agg.rds")) {
  data <- readRDS(data_path) %>%
    filter(
      is.na(location) | location != "Dark",
      is.na(light_type) | light_type != "Dark"
    )
  plot_alpha_mean_error(data, group_col = "light_type", output_path = here("outputs", "alpha_dist_lighttype_mean_error.png"))
  plot_alpha_mean_error(data, group_col = "location", output_path = here("outputs", "alpha_dist_location_mean_error.png"))
}

plot_all_spd_mean_error <- function(data_path = here("data", "d03_aggregated", "spd_data_log_agg.rds")) {
  data <- readRDS(data_path) %>%
    filter(
      is.na(location) | location != "Dark",
      is.na(light_type) | light_type != "Dark"
    )
  plot_spd_mean_error(data, group_col = "light_type", output_path = here("outputs", "spd_dist_lighttype_mean_error.png"))
  plot_spd_mean_error(data, group_col = "location", output_path = here("outputs", "spd_dist_location_mean_error.png"))
}

# 主入口函数，boxplot和medoid分开
plot_all_sensor_distributions <- function(data_path = here("data", "d03_aggregated", "sensor_data_log_agg.rds")) {
  data <- readRDS(data_path)
  # 去除 location 和 light_type 中的 "Dark" 类别
  data <- data %>%
    filter(
      is.na(location) | location != "Dark",
      is.na(light_type) | light_type != "Dark"
    )
  sensor_cols <- c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "IR.LIGHT", "CLEAR")
  # # 1. 整体分布
  # plot_sensor_distribution(data, group_col = "location", sensor_cols, output_path = here("outputs", "sensor_dist_location_boxplot.png"), xlabel = "Sensor")
  # plot_sensor_distribution(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "sensor_dist_lighttype_boxplot.png"), xlabel = "Sensor")
  # # 2. 分participant组图
  # plot_sensor_distribution(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "sensor_dist_location_by_participant_boxplot.png"), xlabel = "Sensor")
  # plot_sensor_distribution(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "sensor_dist_lighttype_by_participant_boxplot.png"), xlabel = "Sensor")
  # # 3. medoid曲线
  # plot_medoid_curve(data, group_col = "location", sensor_cols, output_path = here("outputs", "sensor_dist_location_medoid.png"), xlabel = "Sensor")
  # plot_medoid_curve(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "sensor_dist_lighttype_medoid.png"), xlabel = "Sensor")
  # # 4. medoid曲线分participant
  # plot_medoid_curve(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "sensor_dist_location_by_participant_medoid.png"), xlabel = "Sensor")
  # plot_medoid_curve(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "sensor_dist_lighttype_by_participant_medoid.png"), xlabel = "Sensor")


  plot_sensor_mean_error(
    data = data,
    group_col = "light_type",
    sensor_cols = sensor_cols,
    output_path = here("outputs", "sensor_dist_light_type_mean_error.png"), xlabel = "Sensor"
  )

    plot_sensor_mean_error(
    data = data,
    group_col = "location",
    sensor_cols = sensor_cols,
    output_path = here("outputs", "sensor_dist_location_mean_error.png"), xlabel = "Sensor"
  )
}

plot_all_alpha_distributions <- function(data_path = here("data", "d03_aggregated", "alpha_data_log_hlea_hourly_agg.rds")) {
  data <- readRDS(data_path)
  sensor_cols <- c("PHO", "MEDI", "SC", "MC", "LC", "RH")
  # 1. 整体分布
  plot_sensor_distribution(data, group_col = "location", sensor_cols, output_path = here("outputs", "alpha_dist_location_boxplot.png"), xlabel = "Alpha Opics")
  plot_sensor_distribution(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "alpha_dist_lighttype_boxplot.png"), xlabel = "Alpha Opics")
  # 2. 分participant组图
  plot_sensor_distribution(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "alpha_dist_location_by_participant_boxplot.png"), xlabel = "Alpha Opics")
  plot_sensor_distribution(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "alpha_dist_lighttype_by_participant_boxplot.png"), xlabel = "Alpha Opics")
  # 3. medoid曲线
  plot_medoid_curve(data, group_col = "location", sensor_cols, output_path = here("outputs", "alpha_dist_location_medoid.png"), xlabel = "Alpha Opics")
  plot_medoid_curve(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "alpha_dist_lighttype_medoid.png"), xlabel = "Alpha Opics")
  # 4. medoid曲线分participant
  plot_medoid_curve(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "alpha_dist_location_by_participant_medoid.png"), xlabel = "Alpha Opics")
  plot_medoid_curve(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "alpha_dist_lighttype_by_participant_medoid.png"), xlabel = "Alpha Opics")
}

plot_all_spd_distributions <- function(data_path = here("data", "d03_aggregated", "spd_data_log_hlea_hourly_agg.rds")) {
  data <- readRDS(data_path)
  sensor_cols <- names(data)[startsWith(names(data), "X")]
  # 1. 整体分布
  plot_sensor_distribution(data, group_col = "location", sensor_cols, output_path = here("outputs", "spd_dist_location_boxplot.png"), xlabel = "Wavelength (nm)")
  plot_sensor_distribution(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "spd_dist_lighttype_boxplot.png"), xlabel = "Wavelength (nm)")
  # 2. 分participant组图
  plot_sensor_distribution(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "spd_dist_location_by_participant_boxplot.png"), xlabel = "Wavelength (nm)")
  plot_sensor_distribution(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "spd_dist_lighttype_by_participant_boxplot.png"), xlabel = "Wavelength (nm)")
  # 3. medoid曲线
  plot_medoid_curve(data, group_col = "location", sensor_cols, output_path = here("outputs", "spd_dist_location_medoid.png"), xlabel = "Wavelength (nm)")
  plot_medoid_curve(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "spd_dist_lighttype_medoid.png"), xlabel = "Wavelength (nm)")
  # 4. medoid曲线分participant
  plot_medoid_curve(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "spd_dist_location_by_participant_medoid.png"), xlabel = "Wavelength (nm)")
  plot_medoid_curve(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "spd_dist_lighttype_by_participant_medoid.png"), xlabel = "Wavelength (nm)")
}

# L2数据分布绘图函数
plot_all_sensor_l2_distributions <- function(data_path = here("data", "d03_aggregated", "sensor_data_l2_agg.rds")) {
  data <- readRDS(data_path)
  # 去除 location 和 light_type 中的 "Dark" 类别
  data <- data %>%
    filter(
      is.na(location) | location != "Dark",
      is.na(light_type) | light_type != "Dark"
    )
  sensor_cols <- c(names(data)[startsWith(names(data), "norm")])
  # # 1. 整体分布
  # plot_sensor_distribution(data, group_col = "location", sensor_cols, output_path = here("outputs", "sensor_dist_location_boxplot_l2.png"), xlabel = "Sensor")
  # plot_sensor_distribution(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "sensor_dist_lighttype_boxplot_l2.png"), xlabel = "Sensor")
  # # 2. 分participant组图
  # plot_sensor_distribution(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "sensor_dist_location_by_participant_boxplot_l2.png"), xlabel = "Sensor")
  # plot_sensor_distribution(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "sensor_dist_lighttype_by_participant_boxplot_l2.png"), xlabel = "Sensor")
  # # 3. medoid曲线
  # plot_medoid_curve(data, group_col = "location", sensor_cols, output_path = here("outputs", "sensor_dist_location_medoid_l2.png"), xlabel = "Sensor")
  # plot_medoid_curve(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "sensor_dist_lighttype_medoid_l2.png"), xlabel = "Sensor")
  # # 4. medoid曲线分participant
  # plot_medoid_curve(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "sensor_dist_location_by_participant_medoid_l2.png"), xlabel = "Sensor")
  # plot_medoid_curve(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "sensor_dist_lighttype_by_participant_medoid_l2.png"), xlabel = "Sensor")
  
  plot_sensor_mean_error(
    data = data,
    group_col = "light_type",
    sensor_cols = sensor_cols,
    output_path = here("outputs", "sensor_dist_light_type_mean_error_l2.png"), xlabel = "Sensor L2 Normalised"
  )

  plot_sensor_mean_error(
    data = data,
    group_col = "location",
    sensor_cols = sensor_cols,
    output_path = here("outputs", "sensor_dist_location_mean_error_l2.png"), xlabel = "Sensor L2 Normalised"
  )
}

plot_all_alpha_l2_distributions <- function(data_path = here("data", "d03_aggregated", "alpha_data_l2_hlea_hourly_agg.rds")) {
  data <- readRDS(data_path)
  sensor_cols <- c(names(data)[startsWith(names(data), "norm")])
  # 1. 整体分布
  plot_sensor_distribution(data, group_col = "location", sensor_cols, output_path = here("outputs", "alpha_dist_location_boxplot_l2.png"), xlabel = "Alpha Opics")
  plot_sensor_distribution(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "alpha_dist_lighttype_boxplot_l2.png"), xlabel = "Alpha Opics")
  # 2. 分participant组图
  plot_sensor_distribution(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "alpha_dist_location_by_participant_boxplot_l2.png"), xlabel = "Alpha Opics")
  plot_sensor_distribution(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "alpha_dist_lighttype_by_participant_boxplot_l2.png"), xlabel = "Alpha Opics")
  # 3. medoid曲线
  plot_medoid_curve(data, group_col = "location", sensor_cols, output_path = here("outputs", "alpha_dist_location_medoid_l2.png"), xlabel = "Alpha Opics")
  plot_medoid_curve(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "alpha_dist_lighttype_medoid_l2.png"), xlabel = "Alpha Opics")
  # 4. medoid曲线分participant
  plot_medoid_curve(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "alpha_dist_location_by_participant_medoid_l2.png"), xlabel = "Alpha Opics")
  plot_medoid_curve(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "alpha_dist_lighttype_by_participant_medoid_l2.png"), xlabel = "Alpha Opics")
}

plot_all_spd_l2_distributions <- function(data_path = here("data", "d03_aggregated", "spd_data_l2_hlea_hourly_agg.rds")) {
  data <- readRDS(data_path)
  sensor_cols <- c(names(data)[startsWith(names(data), "norm")])
  # 1. 整体分布
  plot_sensor_distribution(data, group_col = "location", sensor_cols, output_path = here("outputs", "spd_dist_location_boxplot_l2.png"), xlabel = "Wavelength (nm)")
  plot_sensor_distribution(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "spd_dist_lighttype_boxplot_l2.png"), xlabel = "Wavelength (nm)")
  # 2. 分participant组图
  plot_sensor_distribution(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "spd_dist_location_by_participant_boxplot_l2.png"), xlabel = "Wavelength (nm)")
  plot_sensor_distribution(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "spd_dist_lighttype_by_participant_boxplot_l2.png"), xlabel = "Wavelength (nm)")
  # 3. medoid曲线
  plot_medoid_curve(data, group_col = "location", sensor_cols, output_path = here("outputs", "spd_dist_location_medoid_l2.png"), xlabel = "Wavelength (nm)")
  plot_medoid_curve(data, group_col = "light_type", sensor_cols, output_path = here("outputs", "spd_dist_lighttype_medoid_l2.png"), xlabel = "Wavelength (nm)")
  # 4. medoid曲线分participant
  plot_medoid_curve(data, group_col = "location", sensor_cols, facet_col = "Id", output_path = here("outputs", "spd_dist_location_by_participant_medoid_l2.png"), xlabel = "Wavelength (nm)")
  plot_medoid_curve(data, group_col = "light_type", sensor_cols, facet_col = "Id", output_path = here("outputs", "spd_dist_lighttype_by_participant_medoid_l2.png"), xlabel = "Wavelength (nm)")
}

# -------------------------
# Alpha / SPD L2 mean±sd wrappers
# -------------------------
plot_alpha_l2_mean_error <- function(data, group_col, output_path, log_transform = TRUE, facet_col = NULL, xlabel = "Alpha Opics") {
  # L2 alpha columns typically start with 'norm' in alpha L2 datasets
  sensor_cols <- names(data)[startsWith(names(data), "norm")]
  plot_sensor_mean_error(data = data, group_col = group_col, sensor_cols = sensor_cols,
                         log_transform = log_transform, facet_col = facet_col,
                         output_path = output_path, xlabel = xlabel)
}

plot_spd_l2_mean_error <- function(data, group_col, output_path, log_transform = TRUE, facet_col = NULL, xlabel = "Wavelength (nm)") {
  # L2 SPD columns also use 'norm' prefix
  sensor_cols <- names(data)[startsWith(names(data), "norm")]
  plot_sensor_mean_error(data = data, group_col = group_col, sensor_cols = sensor_cols,
                         log_transform = log_transform, facet_col = facet_col,
                         output_path = output_path, xlabel = xlabel)
}

plot_all_alpha_l2_mean_error <- function(data_path = here("data", "d03_aggregated", "alpha_data_l2_agg.rds")) {
  data <- readRDS(data_path) %>%
    filter(
      is.na(location) | location != "Dark",
      is.na(light_type) | light_type != "Dark"
    )
  plot_alpha_l2_mean_error(data, group_col = "light_type", output_path = here("outputs", "alpha_l2_dist_lighttype_mean_error.png"))
  plot_alpha_l2_mean_error(data, group_col = "location", output_path = here("outputs", "alpha_l2_dist_location_mean_error.png"))
}

plot_all_spd_l2_mean_error <- function(data_path = here("data", "d03_aggregated", "spd_data_l2_agg.rds")) {
  data <- readRDS(data_path) %>%
    filter(
      is.na(location) | location != "Dark",
      is.na(light_type) | light_type != "Dark"
    )
  plot_spd_l2_mean_error(data, group_col = "light_type", output_path = here("outputs", "spd_l2_dist_lighttype_mean_error.png"))
  plot_spd_l2_mean_error(data, group_col = "location", output_path = here("outputs", "spd_l2_dist_location_mean_error.png"))
}

# 运行主入口函数
# plot_all_sensor_distributions()
# plot_all_alpha_distributions()
# plot_all_spd_distributions()
# plot_all_sensor_l2_distributions()
# plot_all_alpha_l2_distributions()
# plot_all_spd_l2_distributions()

# plot_all_alpha_mean_error()
# plot_all_spd_mean_error()
plot_all_alpha_l2_mean_error()
plot_all_spd_l2_mean_error()
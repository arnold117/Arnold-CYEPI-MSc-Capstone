# label_overall.R

library(ggplot2)
library(dplyr)
library(here)
library(gridExtra)
library(grid) # for textGrob, gpar
library(ggpubr)
library(tidyr)
library(ggstream)

# 颜色方案（参考 l2_comparison.R）

get_light_type_colors <- function() {
  c(
    "Natural" = "#FF8C00",
    "Artificial" = "#0066CC",
    "Dark" = "#000000"
  )
}
get_location_colors <- function() {
  c(
    "Out" = "#FF8C00",
    "In" = "#87CEEB",
    "Dark" = "#000000"
  )
}

# main_light配色和标签（参考l2_comparison.R）
get_main_light_colors <- function() {
  c(
    "O" = "#FF8C00",  # Outside daylight
    "I" = "#FFD700",  # Indoor daylight
    "L" = "#87CEEB",  # LED/electric indoor
    "S" = "#FFB6C1",  # Street lights
    "E" = "#0066CC",  # Displays
    "W" = "#9370DB",  # Window light during sleep
    "X" = "#808080",  # Sleeping
    "D" = "#000000"   # Darkness
  )
}
get_main_light_labels <- function() {
  c(
    "O" = "Outside\nDaylight",
    "I" = "Indoor\nDaylight",
    "L" = "LED/Electric\nIndoors",
    "S" = "Street\nLights",
    "E" = "Emissive\nDisplays",
    "W" = "Window Light\nDuring sleep",
    "X" = "Sleeping\nPeriod",
    "D" = "Darkness"
  )
}

# 统计分布函数
get_label_dist <- function(data, label_col) {
  data %>%
    filter(!is.na(.data[[label_col]])) %>%
    count(.data[[label_col]]) %>%
    mutate(prop = n / sum(n)) %>%
    rename(!!label_col := 1)
}


# 单变量分布barchart函数
plot_bar <- function(df, label_col, color_map, title) {
  df <- df %>% arrange(desc(prop))
  ggplot(df, aes(x = reorder(!!sym(label_col), -prop), y = prop, fill = !!sym(label_col))) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(values = color_map) +
    geom_text(aes(label = sprintf("%.1f%%", prop * 100)), vjust = -0.5, size = 7, color = "black", fontface = "bold") +
    labs(title = title, x = NULL, y = "Proportion") +
    ylim(0, 0.7) +
    theme_minimal(base_size = 20) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 26, face = "bold"),
      axis.text.x = element_text(angle = 30, hjust = 1, size = 24, face = "bold"),
      axis.text.y = element_text(size = 18)
    )
}

plot_main_light <- function(main_light_dist) {
  ggplot(main_light_dist, aes(x = reorder(label, -prop), y = prop, fill = main_light)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(values = get_main_light_colors()) +
    geom_text(aes(label = sprintf("%.1f%%", prop * 100)), vjust = -0.5, size = 7, color = "black", fontface = "bold") +
    labs(title = "Main Light Distribution", x = NULL, y = "Proportion") +
    ylim(0, 0.7) +
    theme_minimal(base_size = 20) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 26, face = "bold"),
      axis.text.x = element_text(angle = 30, hjust = 1, size = 24, face = "bold"),
      axis.text.y = element_text(size = 18)
    )
}

plot_label_overall_layout <- function(p_main_light, p_location, p_light_type, output_path) {
  big_title <- textGrob("Overall Label Distribution", gp = gpar(fontsize = 32, fontface = "bold"))
  full <- gridExtra::arrangeGrob(
    big_title,
    gridExtra::arrangeGrob(
      p_main_light,
      gridExtra::arrangeGrob(p_location, p_light_type, ncol = 2),
      ncol = 1,
      heights = c(1.2, 1)
    ),
    nrow = 2,
    heights = c(0.13, 1)
  )
  ggsave(output_path, full, width = 14, height = 14, dpi = 300)
  grid::grid.newpage(); grid::grid.draw(full)
  invisible(full)
}

# 画整体24小时每小时label分布（location和light_type），上下排列
plot_label_hourly_distribution <- function(data_path = here::here("data", "d02_transformed", "transformed_hlea.rds"),
                                          output_path = here::here("outputs", "label_hourly_distribution.png")) {
  data <- readRDS(data_path)
  # 假设有hour列（0-23），否则需先处理
  if (!"hour" %in% colnames(data)) {
    stop("数据缺少hour列，无法绘制每小时分布。请先添加hour信息。")
  }
  # location
  location_hour <- data %>%
    filter(!is.na(location), !is.na(hour)) %>%
    group_by(hour, location) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(hour) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    complete(hour = 0:23, location = unique(location), fill = list(n = 0, prop = 0)) %>%
    group_by(hour) %>%
    mutate(total = sum(prop), prop = ifelse(total > 1, prop / total, prop)) %>%
    select(-total)

  p_location <- ggplot(location_hour, aes(x = hour, y = prop, fill = location)) +
    geom_area(position = "stack", alpha = 0.9) +
    scale_fill_manual(values = get_location_colors()) +
    scale_x_continuous(breaks = 0:23) +
    labs(title = "Hourly Location Distribution (Overall)", x = "Hour of Day", y = "Proportion") +
    theme_minimal(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
          axis.text.x = element_text(size = 20),
          axis.title.x = element_text(size = 28),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 28),
          legend.title = element_blank())

  # light_type
  light_type_hour <- data %>%
    filter(!is.na(light_type), !is.na(hour)) %>%
    group_by(hour, light_type) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(hour) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    complete(hour = 0:23, light_type = unique(light_type), fill = list(n = 0, prop = 0)) %>%
    group_by(hour) %>%
    mutate(total = sum(prop), prop = ifelse(total > 1, prop / total, prop)) %>%
    select(-total)

  p_light_type <- ggplot(light_type_hour, aes(x = hour, y = prop, fill = light_type)) +
    geom_area(position = "stack", alpha = 0.9) +
    scale_fill_manual(values = get_light_type_colors()) +
    scale_x_continuous(breaks = 0:23) +
    labs(title = "Hourly Light Type Distribution (Overall)", x = "Hour of Day", y = "Proportion") +
    theme_minimal(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
          axis.text.x = element_text(size = 20),
          axis.title.x = element_text(size = 28),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 28),
          legend.title = element_blank())

  full <- gridExtra::arrangeGrob(
    p_location, p_light_type,
    ncol = 1
  )
  ggsave(output_path, full, width = 14, height = 14, dpi = 300)
  grid::grid.newpage(); grid::grid.draw(full)
  invisible(full)
}

main_label_overall <- function(data_path = here::here("data", "d02_transformed", "transformed_hlea.rds"),
                               output_path = here::here("outputs", "label_overall_bar_combined.png")) {
  data <- readRDS(data_path)

  # Label distribution bar charts
  main_light_dist <- get_label_dist(data, "main_light")
  light_type_dist <- get_label_dist(data, "light_type")
  location_dist <- get_label_dist(data, "location")
  main_light_labels <- get_main_light_labels()
  main_light_dist <- main_light_dist %>% mutate(label = main_light_labels[as.character(main_light)]) %>% arrange(desc(prop))
  location_dist <- location_dist %>% arrange(desc(prop))
  light_type_dist <- light_type_dist %>% arrange(desc(prop))
  p_main_light <- plot_main_light(main_light_dist)
  p_location   <- plot_bar(location_dist, "location", get_location_colors(), "Location Distribution")
  p_light_type <- plot_bar(light_type_dist, "light_type", get_light_type_colors(), "Light Type Distribution")
  plot_label_overall_layout(p_main_light, p_location, p_light_type, output_path)

  #
}

# 通用个体 hourly distribution 画图函数
plot_individual_hourly_distribution_generic <- function(data, label_col, color_map, label_map, legend_title, output_path) {
  if (!"hour" %in% colnames(data) || !"Id" %in% colnames(data)) {
    stop("数据缺少 hour 或 Id 列。")
  }
  legend_plot <- ggplot(data.frame(x=1, y=1, fill=names(color_map)), aes(x=x, y=y, fill=fill)) +
    geom_tile() +
    scale_fill_manual(values = color_map, labels = label_map, breaks = names(color_map)) +
    theme_minimal() +
    theme(legend.position = "top", legend.box = "horizontal",
          legend.text = element_text(size = 18), legend.title = element_text(size = 22)) +
    guides(fill = guide_legend(nrow = 1, title = legend_title))
  shared_legend <- ggpubr::get_legend(legend_plot)

  participants <- sort(unique(data$Id))
  plot_list <- lapply(participants, function(pid) {
    df <- data %>%
      filter(Id == pid, !is.na(.data[[label_col]]), !is.na(hour)) %>%
      group_by(hour, .data[[label_col]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(hour) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup() %>%
      complete(hour = 0:23, !!label_col := unique(.data[[label_col]]), fill = list(n = 0, prop = 0)) %>%
      group_by(hour) %>%
      mutate(total = sum(prop), prop = ifelse(total > 0, prop / total, 0)) %>%
      select(-total) %>%
      arrange(hour)
    ggplot(df, aes(x = hour, y = prop, fill = .data[[label_col]])) +
      geom_stream(type = "proportional", alpha = 0.85) +
      scale_fill_manual(values = color_map) +
      labs(title = paste("Participant", pid), x = "Hour of Day", y = "Proportion") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.text.x = element_text(size = 10),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 10),
            axis.title.y = element_text(size = 14),
            legend.position = "none",
            aspect.ratio = 1,
            panel.background = element_rect(fill = "white"),
            plot.background = element_blank(),
            panel.grid.major = element_line(linewidth = 0.2, linetype = 'solid', color = 'gray'),
            panel.border = element_blank()) +
      scale_x_continuous(breaks = seq(0, 23, 3), limits = c(0, 23))
  })
  grid <- ggarrange(plotlist = plot_list, nrow = 5, ncol = 6, common.legend = FALSE)
  final_plot <- ggarrange(shared_legend, grid, nrow = 2, heights = c(0.05, 0.95))
  ggsave(output_path, final_plot, width = 18, height = 16.3, dpi = 300, bg = "white")
  grid::grid.newpage(); grid::grid.draw(final_plot)
  invisible(final_plot)
}

# 主入口函数，分别画 location 和 light_type
plot_individual_hourly_distribution <- function(data_path = here::here("data", "d02_transformed", "transformed_hlea.rds")) {
  data <- readRDS(data_path)
  # location
  plot_individual_hourly_distribution_generic(
    data = data,
    label_col = "location",
    color_map = get_location_colors(),
    label_map = c("Out" = "Outdoors", "In" = "Indoors", "Dark" = "Dark"),
    legend_title = "Location",
    output_path = here::here("outputs", "individual_hourly_distribution_location.png")
  )
  # light_type
  plot_individual_hourly_distribution_generic(
    data = data,
    label_col = "light_type",
    color_map = get_light_type_colors(),
    label_map = c("Natural" = "Natural", "Artificial" = "Artificial", "Dark" = "Dark"),
    legend_title = "Light Type",
    output_path = here::here("outputs", "individual_hourly_distribution_lighttype.png")
  )
}

# 直接运行主函数
# main_label_overall()

plot_label_hourly_distribution()

# plot_individual_hourly_distribution()

# 显式声明变量以避免 NSE 错误
globalVariables(c("prop", "total", "hour", "location", "light_type"))
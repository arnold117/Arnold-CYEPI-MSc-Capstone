# =============================
# 多处理方法对模型整体表现的可视化（AUC）
# 小提琴图 + 箱线图 + 统计汇总 + 显著性检验
# =============================

# ---- 环境与依赖 ----
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(ggplot2)
  library(stringr)
  library(purrr)
})
source("config/setup.R")

# Declare common column names used with dplyr/ggplot non-standard evaluation
if (getRversion() >= "2.15.1") utils::globalVariables(
  c("target_lbl", "auc", "factor", "level", "level_lab", "n", "data", "l2", "level_lab", "level", "split", "n_steps", "mean_auc", "step_data", "step_log", "step_l2", "step_hour", "step_pca", "imbalance_char", "split_char", "step_split", "step_sampling")
)

# ---- 1) 读取结果 ----
file_res <- file.path(paths$deep_learning_outputs, "all_bayesian_first_row.csv")
res <- read_csv(file_res, show_col_types = FALSE)

# 输出文件夹
out_dir <- file.path(paths$deep_learning_outputs, "visualizations")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 2) 轻量清洗（仅必要可读化，避免把值变 NA）----
res1 <- res %>%
  mutate(
    target_lbl = recode(target,
                        "location"   = "Indoor vs Outdoor",
                        "light_type" = "Artificial vs Natural",
                        .default = target),
  # 保证 data 列存在并可作图
  data = as.factor(data),
  imbalance = fct_na_value_to_level(factor(imbalance), level = "None"),
  log  = as.factor(log),
  # l2 现在有三类：False, Exclude Intensity, Include Intensity
  l2   = factor(l2, levels = c("False", "Exclude Intensity", "Include Intensity")),
  pca  = as.factor(pca),
  hour = as.factor(hour),
  split = as.factor(split)
  ) %>%
  filter(!is.na(auc))

# 列名自检（防拼写/缺列）
factors_to_plot <- c("log", "l2", "pca", "hour", "imbalance", "split")
missing_cols <- setdiff(c(factors_to_plot, "target_lbl", "auc"), names(res1))
if (length(missing_cols) > 0) {
  stop("下列列在 res1 中不存在：", paste(missing_cols, collapse = ", "))
}

# 长表：factor(哪一个处理), level(该处理的取值), auc, target_lbl
long_df <- res1 %>%
  mutate(across(all_of(factors_to_plot), ~ as.character(.))) %>%  # 统一字符
  tidyr::pivot_longer(cols = all_of(factors_to_plot),
                      names_to = "factor", values_to = "level") %>%
  mutate(
    level = dplyr::if_else(is.na(level) | level == "", "None", level),
    factor = factor(
      factor,
      levels = c("log","l2","pca","hour","imbalance","split"),
      labels = c("LOG","L2","PCA","HOUR","Imbalance","Split")
    )
  )

# 组图函数：只画 violin + boxplot；y 轴按分位数收缩
plot_violin_box_grid <- function(df, target_value, outfile,
                                 q = c(0.02, 0.98),  # 分位数范围，可调成 c(0.05,0.95)
                                 pad = 0.01,         # 上下各留一点边距
                                 ylim_override = NULL) {
  d <- df %>% dplyr::filter(target_lbl == target_value, !is.na(auc))
  if (nrow(d) == 0) { warning("No rows for target: ", target_value); return(invisible(NULL)) }

  # y 轴范围：用分位数 + 边距，最后仍限制在 [0,1] 内
  if (!is.null(ylim_override)) {
    ylim <- ylim_override
  } else {
    rng <- stats::quantile(d$auc, probs = q, na.rm = TRUE)
    ylim <- c(max(0, rng[1] - pad), min(1, rng[2] + pad))
  }

  # x 轴使用原始 level 标签（不附样本数）
  d <- d %>% dplyr::mutate(level_lab = as.character(level))

  p <- ggplot(d, aes(x = level_lab, y = auc, fill = level_lab)) +
    geom_violin(trim = TRUE, alpha = 0.5, width = 0.9) +
    geom_boxplot(width = 0.12, outlier.size = 0.6, color = "black") +
    stat_summary(fun = median, geom = "point", shape = 21, size = 1.8,
                 fill = "white", color = "black") +
    facet_wrap(~ factor, ncol = 3, scales = "free_x") +
    coord_cartesian(ylim = ylim) +
    labs(
      title = paste0(target_value, " : AUC by Processing Factors"),
      x = NULL, y = "AUC"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 28, hjust = 1)
    )

  # 确保 outfile 保存在 out_dir
  outfile_full <- file.path(out_dir, outfile)
  ggsave(outfile_full, p, width = 12, height = 7, dpi = 300)
  message("Saved: ", outfile_full)
  invisible(p)
}

# 生成六张图：按 target (location / light_type) × data (alpha / sensor / spd)
plot_target_data_grid <- function(wide_df, target_value, data_value, outfile,
                                  q = c(0.02, 0.98), pad = 0.01,
                                  ylim_override = NULL) {
  df_w <- wide_df %>% dplyr::filter(target_lbl == target_value, data == data_value, !is.na(auc))
  if (nrow(df_w) == 0) { warning("No rows for target/data: ", target_value, "/", data_value); return(invisible(NULL)) }

  d <- df_w %>%
    mutate(across(all_of(factors_to_plot), ~ as.character(.))) %>%
    tidyr::pivot_longer(cols = all_of(factors_to_plot), names_to = "factor", values_to = "level") %>%
    mutate(level = dplyr::if_else(is.na(level) | level == "", "None", level),
           factor = factor(
             factor,
             levels = c("log","l2","pca","hour","imbalance","split"),
             labels = c("LOG","L2","PCA","HOUR","Imbalance","Split")
           ))

  if (!is.null(ylim_override)) {
    ylim <- ylim_override
  } else {
    rng <- stats::quantile(d$auc, probs = q, na.rm = TRUE)
    ylim <- c(max(0, rng[1] - pad), min(1, rng[2] + pad))
  }

  d <- d %>% dplyr::mutate(level_lab = as.character(level))

  p <- ggplot(d, aes(x = level_lab, y = auc, fill = level_lab)) +
    geom_violin(trim = TRUE, alpha = 0.5, width = 0.9) +
    geom_boxplot(width = 0.12, outlier.size = 0.6, color = "black") +
    stat_summary(fun = median, geom = "point", shape = 21, size = 1.8,
                 fill = "white", color = "black") +
    facet_wrap(~ factor, ncol = 3, scales = "free_x") +
    coord_cartesian(ylim = ylim) +
    labs(title = paste0(target_value, " / ", data_value, " : AUC by Processing Factors"), x = NULL, y = "AUC") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none", panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"), axis.text.x = element_text(angle = 28, hjust = 1))

  outfile_full <- file.path(out_dir, outfile)
  ggsave(outfile_full, p, width = 12, height = 7, dpi = 300)
  message("Saved: ", outfile_full)
  invisible(p)
}

data_types <- c("alpha", "sensor", "spd")
targets <- c("Indoor vs Outdoor", "Artificial vs Natural")
for (t in targets) {
  for (dt in data_types) {
    short_t <- ifelse(t == "Indoor vs Outdoor", "location", "lighttype")
    outfile <- paste0("violin_box_", short_t, "_", dt, ".png")
    # apply target-specific overrides: location -> c(0.55,0.8), lighttype -> c(0.75,1.0)
    ylim_t <- if (t == "Indoor vs Outdoor") c(0.55, 0.8) else c(0.75, 1.0)
    plot_target_data_grid(res1, t, dt, outfile, q = c(0,1), ylim_override = ylim_t)
  }
}

## 生成两张图：当 target 为 location / light_type 时，展示不同 data 类型的 AUC 分布
plot_data_type_auc_by_target <- function(wide_df, target_value, outfile, ylim_override = NULL) {
  d <- wide_df %>% dplyr::filter(target_lbl == target_value, !is.na(auc))
  if (nrow(d) == 0) { warning("No rows for target: ", target_value); return(invisible(NULL)) }

  d <- d %>% dplyr::mutate(data = as.factor(data))

  p <- ggplot(d, aes(x = data, y = auc, fill = data)) +
    geom_violin(trim = TRUE, alpha = 0.5) +
    geom_boxplot(width = 0.12, outlier.size = 0.6, color = "black") +
    stat_summary(fun = median, geom = "point", shape = 21, size = 1.8,
                 fill = "white", color = "black") +
    labs(title = paste0(target_value, " : AUC by Data Type"), x = "Data type", y = "AUC") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none", panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 25, hjust = 1))

  if (!is.null(ylim_override)) p <- p + coord_cartesian(ylim = ylim_override)

  outfile_full <- file.path(out_dir, outfile)
  ggsave(outfile_full, p, width = 8, height = 5, dpi = 300)
  message("Saved: ", outfile_full)
  invisible(p)
}

# 2 张图：location 与 light_type (apply same overrides)
plot_data_type_auc_by_target(res1, "Indoor vs Outdoor",     "violin_data_by_target_location.png",     ylim_override = c(0.55, 0.8))
plot_data_type_auc_by_target(res1, "Artificial vs Natural", "violin_data_by_target_lighttype.png", ylim_override = c(0.75, 1.0))

# 另外再生成两张：每个 target 的 6 个处理方法组图（one file per target）
plot_violin_box_grid(long_df, "Indoor vs Outdoor",     "violin_box_location.png",   q = c(0,1), ylim_override = c(0.55, 0.8))
plot_violin_box_grid(long_df, "Artificial vs Natural", "violin_box_lighttype.png", q = c(0,1), ylim_override = c(0.75, 1.0))

# 新增：按处理步骤数量（n_steps）观察 AUC 是否随步骤增多而提升
# 规则：对于 factors_to_plot 中每个非默认/非 None/非 False 值计为一步；
# 当 data 不为 "sensor"（即为 alpha 或 spd）再计 +1 步。
plot_auc_by_nsteps <- function(wide_df, target_value, outfile, ylim_override = NULL) {
  d <- wide_df %>% dplyr::filter(target_lbl == target_value, !is.na(auc))
  if (nrow(d) == 0) { warning("No rows for target: ", target_value); return(invisible(NULL)) }

  # 不再限定为仅 parwise；但把 split 是否为 parwise 当作一步计入 n_steps

  # 计算明确的步骤：
  #  - data: sensor -> alpha/spd counts as +1
  #  - log: 非 False/0/NA 计 +1
  #  - l2: 非 False/NA 计 +1 (表示包含 intensity 或其它 L2 处理)
  #  - hour: 非默认/NA 计 +1
  #  - pca: 非 False/NA 计 +1
  # 不计 imbalance
  d <- d %>% mutate(
    data_char = as.character(data),
    log_char  = as.character(log),
    l2_char   = as.character(l2),
    pca_char  = as.character(pca),
  hour_char = as.character(hour),
  imbalance_char = as.character(imbalance),
  split_char = as.character(split)
  ) %>% mutate(
    step_data = ifelse(data_char != "sensor", 1L, 0L),
    step_log  = ifelse(!(log_char %in% c(NA, "", "None", "False", "0", "FALSE")), 1L, 0L),
    # l2 scoring: False -> 0, Exclude Intensity -> 1, Include Intensity -> 2
    step_l2 = dplyr::case_when(
      stringr::str_to_lower(trimws(l2_char)) == "include intensity" ~ 2L,
      stringr::str_to_lower(trimws(l2_char)) == "exclude intensity" ~ 1L,
      TRUE ~ 0L
    ),
    step_hour = ifelse(!(hour_char %in% c(NA, "", "0", "FALSE", "None")), 1L, 0L),
    step_pca  = ifelse(!(pca_char %in% c(NA, "", "None", "False", "0", "FALSE")), 1L, 0L),
  # split: parwise -> 1, others -> 0
  step_split = ifelse(stringr::str_detect(stringr::str_to_lower(split_char), "parwise"), 1L, 0L),
  # sampling: treat absence of 'smote' or 'tomek' in imbalance as an extra step (i.e., not using smote-tomek counts)
  step_sampling = ifelse(stringr::str_detect(stringr::str_to_lower(imbalance_char), "smote|tomek"), 0L, 1L),
  n_steps   = step_data + step_log + step_l2 + step_hour + step_pca + step_split + step_sampling
  ) %>% dplyr::mutate(n_steps = as.integer(n_steps))

  # 摘要供趋势线使用
  summary_df <- d %>% dplyr::group_by(n_steps) %>% dplyr::summarize(mean_auc = mean(auc, na.rm = TRUE), .groups = "drop")

  # 计算每个 n_steps 的样本数，用于 x 轴标签
  counts_df <- d %>% dplyr::count(n_steps) %>% dplyr::arrange(n_steps)
  counts_named <- setNames(as.integer(counts_df$n), as.character(counts_df$n_steps))

  # 计算 ylim
  if (!is.null(ylim_override)) {
    ylim <- ylim_override
  } else {
    rng <- stats::quantile(d$auc, probs = c(0.02, 0.98), na.rm = TRUE)
    ylim <- c(max(0, rng[1] - 0.01), min(1, rng[2] + 0.01))
  }

  p <- ggplot(d, aes(x = factor(n_steps), y = auc, fill = factor(n_steps))) +
    geom_violin(trim = TRUE, alpha = 0.5) +
    geom_boxplot(width = 0.12, outlier.size = 0.6, color = "black") +
    stat_summary(fun = median, geom = "point", shape = 21, size = 1.8, fill = "white", color = "black") +
    geom_line(data = summary_df, aes(x = as.factor(n_steps), y = mean_auc, group = 1), inherit.aes = FALSE, color = "black") +
    geom_point(data = summary_df, aes(x = as.factor(n_steps), y = mean_auc), inherit.aes = FALSE, color = "black") +
    coord_cartesian(ylim = ylim) +
    scale_x_discrete(labels = function(x) {
      sapply(x, function(xx) {
        nval <- counts_named[[xx]]
        if (is.null(nval)) nval <- 0L
        paste0(xx, "\n(n=", nval, ")")
      })
    }) +
    labs(title = paste0(target_value, " : AUC by # of Processing Steps"), x = "# processing steps", y = "AUC") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5))
  # 统计检验：Jonckheere-Terpstra (若可用) + Kendall 作为补充
  jt_text <- NULL
  if (requireNamespace("clinfun", quietly = TRUE)) {
    jt_res <- tryCatch(clinfun::jonckheere.test(d$auc, d$n_steps, alternative = "increasing"), error = function(e) NULL)
    if (!is.null(jt_res)) {
      jt_p <- jt_res$p.value
      jt_text <- paste0("JT p=", format.pval(jt_p, digits = 3))
    } else {
      jt_text <- "JT: error"
    }
  } else {
    jt_text <- "JT: clinfun not installed"
  }

  # Kendall tau (effect size + p)
  kend_text <- tryCatch({
    kend <- cor.test(d$n_steps, d$auc, method = "kendall", alternative = "greater")
    paste0("Kendall tau=", round(as.numeric(kend$estimate), 3), ", p=", format.pval(kend$p.value, digits = 3))
  }, error = function(e) "Kendall: error")

  anno_text <- paste(jt_text, kend_text, sep = "; ")
  # 将注释放到图的右上角（超出一点以免覆盖数据）
  p <- p + annotate("text", x = Inf, y = Inf, label = anno_text, hjust = 1.02, vjust = 1.2, size = 3.2)

  outfile_full <- file.path(out_dir, outfile)
  ggsave(outfile_full, p, width = 8, height = 5, dpi = 300)
  message("Saved: ", outfile_full)
  invisible(p)
}

# 两张图：按 target 展示 n_steps vs AUC（使用与其他图相同的 target-specific ylim）
plot_auc_by_nsteps(res1, "Indoor vs Outdoor",     "auc_by_nsteps_location.png",     ylim_override = c(0.55, 0.8))
plot_auc_by_nsteps(res1, "Artificial vs Natural", "auc_by_nsteps_lighttype.png", ylim_override = c(0.75, 1.0))
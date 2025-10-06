# src/s06_baseline_classification/threshold_classification_simplified.R
# Simplified MEDI Threshold-based Indoor/Outdoor Classification

cat("=== SIMPLIFIED MEDI THRESHOLD CLASSIFICATION ===\n")

# Load necessary libraries
library(tidyverse)
library(pROC)
library(caret)
library(ggplot2)
library(scales)

# Ensure configuration is loaded
if (!exists("config") || !exists("paths") || !exists("project_root")) {
  source("config/setup.R")
}

# =====================================================================
# 1. Data Loading Function
# =====================================================================
load_location_data <- function() {
  msg <- "Loading location classification data...\n"
  cat(msg)
  
  train_file <- file.path(paths$data_split, "alpha_data_log_agg_location_binary_train.rds")
  test_file <- file.path(paths$data_split, "alpha_data_log_agg_location_binary_test.rds")
  
  if (!file.exists(train_file) || !file.exists(test_file)) {
    stop("Data files not found!")
  }
  
  train_data <- readRDS(train_file)
  test_data <- readRDS(test_file)
  
  msg <- "✓ Data loaded successfully\n"
  cat(msg)
  
  return(list(train = train_data, test = test_data))
}

# =====================================================================
# 2. Data Preparation Function
# =====================================================================
prepare_data <- function(data_list) {
  msg <- "\nPreparing data for threshold classification...\n"
  cat(msg)
  
  train_data <- data_list$train
  test_data <- data_list$test
  
  # Check if MEDI column exists
  if (!"MEDI" %in% names(train_data)) {
    stop("MEDI column not found in data!")
  }
  
  if (!"location" %in% names(train_data)) {
    stop("location column not found in data!")
  }
  
  # Extract MEDI and location for training
  train_prepared <- train_data %>%
    select(MEDI, location) %>%
    filter(complete.cases(.)) %>%
    mutate(
      # Set MEDI <= 0 to 0 (preprocessing step)
      MEDI = ifelse(MEDI <= 0, 0, MEDI),
      # Combine Dark with In (Indoor)
      location = case_when(
        location == "Out" ~ "Outdoor",
        location == "In" ~ "Indoor", 
        location == "Dark" ~ "Indoor",
        TRUE ~ as.character(location)
      ),
      location = as.factor(location)
    )
  
  # Extract MEDI and location for testing
  test_prepared <- test_data %>%
    select(MEDI, location) %>%
    filter(complete.cases(.)) %>%
    mutate(
      # Set MEDI <= 0 to 0 (preprocessing step)
      MEDI = ifelse(MEDI <= 0, 0, MEDI),
      # Combine Dark with In (Indoor)
      location = case_when(
        location == "Out" ~ "Outdoor",
        location == "In" ~ "Indoor",
        location == "Dark" ~ "Indoor", 
        TRUE ~ as.character(location)
      ),
      location = as.factor(location)
    )
  
  msg <- "✓ Data prepared successfully\n"
  cat(msg)
  
  return(list(train = train_prepared, test = test_prepared))
}

# =====================================================================
# 3. Simplified Threshold Grid Search Function (1 lux increments)
# =====================================================================
threshold_grid_search <- function(train_data, test_data, output_dir) {
  log_file <- file.path(output_dir, "log.txt")
  
  msg <- "\nPerforming detailed threshold grid search from 10 to 10000 lx mEDI (1 lux increments)...\n"
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  # Create threshold range: 10, 11, 12, ..., 10000 lux
  thresholds_lux <- seq(10, 10000, by = 1)
  thresholds_log <- log10(thresholds_lux)
  
  msg <- paste("Testing", length(thresholds_lux), "threshold values from 10 to 10000 lux...\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  # Initialize results storage
  grid_results <- data.frame(
    threshold_lux = thresholds_lux,
    threshold_log = thresholds_log,
    indoor_precision = NA,
    outdoor_precision = NA,
    overall_accuracy = NA,
    sensitivity = NA,  # Outdoor recall
    specificity = NA,  # Indoor recall
    f1_score = NA,
    auc = NA
  )
  
  # Calculate AUC once (it's the same for all thresholds since it's based on raw MEDI values)
  y_true_binary <- ifelse(test_data$location == "Outdoor", 1, 0)
  unique_classes <- unique(y_true_binary)
  
  if (length(unique_classes) >= 2) {
    tryCatch({
      roc_obj <- roc(y_true_binary, test_data$MEDI, quiet = TRUE)
      auc_value <- as.numeric(auc(roc_obj))
    }, error = function(e) {
      auc_value <- NA
    })
  } else {
    auc_value <- NA
  }
  
  # Test each threshold
  for (i in 1:length(thresholds_log)) {
    thresh <- thresholds_log[i]
    
    # Make predictions on test data
    predictions <- ifelse(test_data$MEDI > thresh, "Outdoor", "Indoor")
    
    # Calculate overall accuracy
    correct_predictions <- sum(predictions == test_data$location)
    overall_accuracy <- correct_predictions / length(predictions)
    grid_results$overall_accuracy[i] <- overall_accuracy
    
    # Calculate confusion matrix components
    true_outdoor <- test_data$location == "Outdoor"
    pred_outdoor <- predictions == "Outdoor"
    
    tp <- sum(true_outdoor & pred_outdoor)    # True Positive
    tn <- sum(!true_outdoor & !pred_outdoor)  # True Negative
    fp <- sum(!true_outdoor & pred_outdoor)   # False Positive
    fn <- sum(true_outdoor & !pred_outdoor)   # False Negative
    
    # Calculate precision for each class
    if (tp + fp > 0) {
      outdoor_precision <- tp / (tp + fp)
      grid_results$outdoor_precision[i] <- outdoor_precision
    } else {
      grid_results$outdoor_precision[i] <- NA
    }
    
    if (tn + fn > 0) {
      indoor_precision <- tn / (tn + fn)
      grid_results$indoor_precision[i] <- indoor_precision
    } else {
      grid_results$indoor_precision[i] <- NA
    }
    
    # Calculate sensitivity and specificity
    if (tp + fn > 0) {
      sensitivity <- tp / (tp + fn)  # Outdoor recall
      grid_results$sensitivity[i] <- sensitivity
    } else {
      grid_results$sensitivity[i] <- NA
    }
    
    if (tn + fp > 0) {
      specificity <- tn / (tn + fp)  # Indoor recall
      grid_results$specificity[i] <- specificity
    } else {
      grid_results$specificity[i] <- NA
    }
    
    # F1 Score (for Outdoor class)
    if (tp + fp > 0 && tp + fn > 0) {
      precision_outdoor <- tp / (tp + fp)
      recall_outdoor <- tp / (tp + fn)
      if (precision_outdoor + recall_outdoor > 0) {
        f1 <- 2 * (precision_outdoor * recall_outdoor) / (precision_outdoor + recall_outdoor)
        grid_results$f1_score[i] <- f1
      } else {
        grid_results$f1_score[i] <- NA
      }
    } else {
      grid_results$f1_score[i] <- NA
    }
    
    # Set AUC (same for all)
    grid_results$auc[i] <- auc_value
  }
  
  msg <- "✓ Grid search completed\n"
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  # Save grid search results
  write.csv(grid_results, file.path(output_dir, "threshold_grid_search_detailed.csv"), row.names = FALSE)
  
  # Find best outdoor precision
  best_outdoor_idx <- which.max(grid_results$outdoor_precision)
  best_outdoor_lux <- grid_results$threshold_lux[best_outdoor_idx]
  best_outdoor_log <- grid_results$threshold_log[best_outdoor_idx]
  
  # Find log10(1000) = 3 threshold
  log1000_lux <- 1000
  log1000_log <- log10(1000)
  log1000_idx <- which.min(abs(grid_results$threshold_lux - log1000_lux))
  
  # Report key results
  msg <- "\n=== KEY RESULTS ===\n"
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("Best Outdoor Precision at", best_outdoor_lux, "lux (log10 =", round(best_outdoor_log, 3), "):\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  Outdoor Precision:", round(grid_results$outdoor_precision[best_outdoor_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  Indoor Precision:", if(is.na(grid_results$indoor_precision[best_outdoor_idx])) "N/A" else round(grid_results$indoor_precision[best_outdoor_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  Overall Accuracy:", round(grid_results$overall_accuracy[best_outdoor_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  Sensitivity (Outdoor Recall):", if(is.na(grid_results$sensitivity[best_outdoor_idx])) "N/A" else round(grid_results$sensitivity[best_outdoor_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  Specificity (Indoor Recall):", if(is.na(grid_results$specificity[best_outdoor_idx])) "N/A" else round(grid_results$specificity[best_outdoor_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  F1 Score:", if(is.na(grid_results$f1_score[best_outdoor_idx])) "N/A" else round(grid_results$f1_score[best_outdoor_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  AUC:", if(is.na(grid_results$auc[best_outdoor_idx])) "N/A" else round(grid_results$auc[best_outdoor_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("\nAt log10(1000) = 3 threshold (", log1000_lux, " lux):\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  Outdoor Precision:", if(is.na(grid_results$outdoor_precision[log1000_idx])) "N/A" else round(grid_results$outdoor_precision[log1000_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  Indoor Precision:", if(is.na(grid_results$indoor_precision[log1000_idx])) "N/A" else round(grid_results$indoor_precision[log1000_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  Overall Accuracy:", round(grid_results$overall_accuracy[log1000_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  Sensitivity (Outdoor Recall):", if(is.na(grid_results$sensitivity[log1000_idx])) "N/A" else round(grid_results$sensitivity[log1000_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  Specificity (Indoor Recall):", if(is.na(grid_results$specificity[log1000_idx])) "N/A" else round(grid_results$specificity[log1000_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  F1 Score:", if(is.na(grid_results$f1_score[log1000_idx])) "N/A" else round(grid_results$f1_score[log1000_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  msg <- paste("  AUC:", if(is.na(grid_results$auc[log1000_idx])) "N/A" else round(grid_results$auc[log1000_idx], 3), "\n")
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  # Create metrics vs threshold plot
  msg <- "\nCreating metrics vs threshold plot...\n"
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  plot_data <- grid_results %>%
    select(threshold_log, indoor_precision, outdoor_precision, auc) %>%
    pivot_longer(cols = c(indoor_precision, outdoor_precision, auc), 
                 names_to = "metric_type", 
                 values_to = "metric") %>%
    filter(!is.na(metric)) %>%
    mutate(
      metric_type = case_when(
        metric_type == "indoor_precision" ~ "Indoor Precision",
        metric_type == "outdoor_precision" ~ "Outdoor Precision",
        metric_type == "auc" ~ "AUC"
      )
    )
  
  # Create the plot with only two key threshold lines
  p_metrics <- ggplot(plot_data, aes(x = threshold_log, y = metric, color = metric_type)) +
    geom_line(size = 1.2) +
    geom_vline(xintercept = best_outdoor_log, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = log1000_log, color = "blue", linetype = "dashed", linewidth = 1) +
    scale_x_continuous(
      name = "Threshold (log10 mEDI)",
      breaks = seq(1, 4, 0.5),
      sec.axis = sec_axis(
        trans = ~ 10^.,
        name = "Threshold (mEDI lux)",
        breaks = c(10, 32, 100, 316, 1000, 3162, 10000),
        labels = c("10", "32", "100", "316", "1K", "3.2K", "10K")
      )
    ) +
    scale_y_continuous(
      name = "Metrics",
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format(accuracy = 1)
    ) +
    scale_color_manual(
      name = "Class",
      values = c("Indoor Precision" = "#2E8B57", "Outdoor Precision" = "#FF6347", "AUC" = "#4682B4")
    ) +
    labs(
      title = "Metrics vs Threshold for Indoor/Outdoor Classification",
      subtitle = paste("Red: Best Outdoor Precision (", best_outdoor_lux, " lux); Blue: 1000 lux")
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  # Save the plot
  ggsave(file.path(output_dir, "metrics_vs_threshold_simplified.png"), 
         p_metrics, width = 6, height = 4, dpi = 300)
  
  # Create MEDI distribution plot with only two key thresholds
  p_distribution <- ggplot(train_data, aes(x = MEDI, fill = location)) +
    geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
    geom_vline(xintercept = best_outdoor_log, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = log1000_log, color = "blue", linetype = "dashed", linewidth = 1) +
    labs(title = "MEDI Distribution by Location (Training Set)",
         subtitle = paste("Red: Best Outdoor Precision (", best_outdoor_lux, " lux); Blue: 1000 lux\n(Dark environments are combined with Indoor)"),
         x = "MEDI Values (log10)", y = "Count") +
    scale_fill_manual(values = c("Indoor" = "lightblue", "Outdoor" = "orange")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  ggsave(file.path(output_dir, "medi_distribution_simplified.png"), 
         p_distribution, width = 6, height = 4, dpi = 300)
  
  msg <- "✓ Simplified plots saved\n"
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
  
  return(list(
    grid_results = grid_results,
    best_outdoor_threshold = list(
      lux = best_outdoor_lux,
      log = best_outdoor_log,
      metrics = grid_results[best_outdoor_idx, ]
    ),
    log1000_threshold = list(
      lux = log1000_lux,
      log = log1000_log,
      metrics = grid_results[log1000_idx, ]
    )
  ))
}

# =====================================================================
# 4. Simplified Main Function
# =====================================================================
run_threshold_classification <- function() {
  # Setup output directory and log file
  output_dir <- file.path(paths$outputs, "baseline_classification", "medi_threshold")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  log_file <- file.path(output_dir, "log.txt")
  # Clear previous log file
  if (file.exists(log_file)) file.remove(log_file)
  
  # Start logging
  cat(strrep("=", 80), "\n", file = log_file, append = TRUE)
  cat("SIMPLIFIED MEDI THRESHOLD CLASSIFICATION\n", file = log_file, append = TRUE)
  cat(strrep("=", 80), "\n", file = log_file, append = TRUE)
  
  cat(strrep("=", 80), "\n")
  cat("SIMPLIFIED MEDI THRESHOLD CLASSIFICATION\n")
  cat(strrep("=", 80), "\n")
  
  # Load and prepare data
  data_list <- load_location_data()
  prepared_data <- prepare_data(data_list)
  
  # Run threshold grid search (1 lux increments from 10 to 10000)
  results <- threshold_grid_search(prepared_data$train, prepared_data$test, output_dir)
  
  # Final summary
  cat("\n", strrep("=", 80), "\n")
  cat("THRESHOLD CLASSIFICATION COMPLETED\n")
  cat("Results saved to:", output_dir, "\n")
  cat(strrep("=", 80), "\n")
  
  cat("\n", strrep("=", 80), "\n", file = log_file, append = TRUE)
  cat("THRESHOLD CLASSIFICATION COMPLETED\n", file = log_file, append = TRUE)
  cat("Results saved to: ", output_dir, "\n", file = log_file, append = TRUE)
  cat(strrep("=", 80), "\n", file = log_file, append = TRUE)
  
  return(results)
}

# =====================================================================
# 5. Run Main Function
# =====================================================================
if (!interactive()) {
  # Run automatically when sourced
  threshold_results <- run_threshold_classification()
} else {
  # Manual execution in interactive mode
  cat("To run simplified threshold classification, execute: threshold_results <- run_threshold_classification()\n")
}

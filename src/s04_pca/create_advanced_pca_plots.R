# Advanced PCA Visualization Script
# This script creates comprehensive PCA visualizations including:
# 1. PC score matrix plots (PC1-PC10 relationships)
# 2. Complete loadings heatmaps
# 3. Advanced analysis plots

# Load necessary libraries
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(GGally)
library(pheatmap)
library(RColorBrewer)

# Ensure configuration is loaded
if (!exists("config") || !exists("paths") || !exists("project_root")) {
  source("config/setup.R")
}

# Function to create PC score matrix plot (pairs plot)
create_pca_scores_matrix <- function(pca_result, train_labels, dataset_name, output_dir, max_pc = 10) {
  cat("  Creating PCA scores matrix plot...\n")
  
  # Create plot directory first
  plot_dir <- file.path(output_dir, "pca_plots", dataset_name)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  # Extract PC scores up to max_pc
  available_pc <- min(max_pc, ncol(pca_result$pca_model$ind$coord))
  if (available_pc < 2) {
    cat("    Warning: Less than 2 components available, skipping matrix plot\n")
    return()
  }
  
  pca_scores <- as.data.frame(pca_result$pca_model$ind$coord[, 1:available_pc, drop = FALSE])
  colnames(pca_scores) <- paste0("PC", 1:available_pc)
  
  # Add labels if available
  if (!is.null(train_labels) && "light_type" %in% names(train_labels)) {
    pca_scores$light_type <- train_labels$light_type
    
    # Create pairs plot with light_type coloring
    p_matrix <- ggpairs(
      pca_scores, 
      columns = 1:available_pc,
      aes(color = light_type, alpha = 0.7),
      title = paste("PCA Scores Matrix -", dataset_name, "by Light Type"),
      upper = list(continuous = "cor", combo = "box_no_facet"),
      lower = list(continuous = "points", combo = "dot_no_facet"),
      diag = list(continuous = "densityDiag")
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 8),
      axis.text = element_text(size = 6),
      legend.position = "bottom"
    )
    
    # Save the plot
    ggsave(
      file.path(plot_dir, "pca_scores_matrix_light_type.png"), 
      p_matrix, 
      width = 2 * available_pc, 
      height = 2 * available_pc, 
      dpi = 300,
      limitsize = FALSE
    )
    cat("    ✓ Light type matrix plot saved\n")
  }
  
  # Also create location-based matrix if available
  if (!is.null(train_labels) && "location" %in% names(train_labels)) {
    pca_scores$location <- train_labels$location
    
    p_matrix_loc <- ggpairs(
      pca_scores, 
      columns = 1:available_pc,
      aes(color = location, alpha = 0.7),
      title = paste("PCA Scores Matrix -", dataset_name, "by Location"),
      upper = list(continuous = "cor", combo = "box_no_facet"),
      lower = list(continuous = "points", combo = "dot_no_facet"),
      diag = list(continuous = "densityDiag")
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 8),
      axis.text = element_text(size = 6),
      legend.position = "bottom"
    )
    
    ggsave(
      file.path(plot_dir, "pca_scores_matrix_location.png"), 
      p_matrix_loc, 
      width = 2 * available_pc, 
      height = 2 * available_pc, 
      dpi = 300,
      limitsize = FALSE
    )
    cat("    ✓ Location matrix plot saved\n")
  }
}

# Function to create complete loadings heatmap
create_loadings_heatmap <- function(pca_result, dataset_name, output_dir, variable_order = NULL) {
  cat("  Creating complete loadings heatmap...\n")
  
  plot_dir <- file.path(output_dir, "pca_plots", dataset_name)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  # Get all loadings
  loadings_matrix <- pca_result$pca_model$var$coord
  
  # Reorder variables if specified
  if (!is.null(variable_order)) {
    # Ensure all variables in variable_order exist in the loadings
    available_vars <- intersect(variable_order, rownames(loadings_matrix))
    missing_vars <- setdiff(rownames(loadings_matrix), available_vars)
    final_order <- c(available_vars, missing_vars)
    loadings_matrix <- loadings_matrix[final_order, , drop = FALSE]
  }
  
  # Create heatmap using pheatmap
  pheatmap(
    loadings_matrix,
    cluster_rows = FALSE,  # Keep variable order
    cluster_cols = FALSE,  # Keep PC order
    scale = "none",
    color = colorRampPalette(c("blue", "white", "red"))(100),
    breaks = seq(-1, 1, length.out = 101),
    main = paste("Complete PCA Loadings -", dataset_name),
    fontsize = 10,
    fontsize_row = 8,
    fontsize_col = 8,
    filename = file.path(plot_dir, "complete_loadings_heatmap.png"),
    width = max(6, ncol(loadings_matrix) * 0.5),
    height = max(6, nrow(loadings_matrix) * 0.3)
  )
  
  # Also create a ggplot version for more control
  loadings_df <- loadings_matrix %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    pivot_longer(-Variable, names_to = "PC", values_to = "Loading")
  
  # Set variable order
  if (!is.null(variable_order)) {
    available_vars <- intersect(variable_order, unique(loadings_df$Variable))
    missing_vars <- setdiff(unique(loadings_df$Variable), available_vars)
    final_order <- c(available_vars, missing_vars)
    loadings_df$Variable <- factor(loadings_df$Variable, levels = final_order)
  }
  
  # Set PC order
  pc_levels <- paste0("Dim.", 1:ncol(loadings_matrix))
  loadings_df$PC <- factor(loadings_df$PC, levels = pc_levels)
  
  p_heatmap <- ggplot(loadings_df, aes(x = PC, y = Variable, fill = Loading)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, limits = c(-1, 1)) +
    labs(title = paste("Complete PCA Loadings -", dataset_name),
         x = "Principal Component", y = "Variable") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(hjust = 0.5)
    )
  
  ggsave(
    file.path(plot_dir, "complete_loadings_heatmap_ggplot.png"),
    p_heatmap,
    width = max(8, ncol(loadings_matrix) * 0.6),
    height = max(6, nrow(loadings_matrix) * 0.3),
    dpi = 300
  )
  
  cat("    ✓ Complete loadings heatmap saved\n")
}

# Function to create detailed scree plot with cumulative variance
create_detailed_scree_plot <- function(pca_result, dataset_name, output_dir) {
  cat("  Creating detailed scree plot...\n")
  
  plot_dir <- file.path(output_dir, "pca_plots", dataset_name)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  # Extract eigenvalues and variance explained
  eigenvalues <- pca_result$pca_model$eig
  
  # Create data frame for plotting
  scree_data <- data.frame(
    PC = 1:nrow(eigenvalues),
    Eigenvalue = eigenvalues[, 1],
    Variance_Percent = eigenvalues[, 2],
    Cumulative_Variance = eigenvalues[, 3]
  )
  
  # Scree plot with both individual and cumulative variance
  p_scree <- ggplot(scree_data, aes(x = PC)) +
    geom_col(aes(y = Variance_Percent), fill = "steelblue", alpha = 0.7) +
    geom_line(aes(y = Cumulative_Variance), color = "red", linewidth = 1) +
    geom_point(aes(y = Cumulative_Variance), color = "red", size = 2) +
    scale_x_continuous(breaks = 1:nrow(eigenvalues)) +
    labs(
      title = paste("Detailed Scree Plot -", dataset_name),
      x = "Principal Component",
      y = "Variance Explained (%)",
      subtitle = "Bars: Individual variance, Line: Cumulative variance"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray60")
    )
  
  ggsave(
    file.path(plot_dir, "detailed_scree_plot.png"),
    p_scree,
    width = 10, height = 6, dpi = 300
  )
  
  cat("    ✓ Detailed scree plot saved\n")
}

# Function to create contribution plots (top contributors for each PC)
create_contribution_plots <- function(pca_result, dataset_name, output_dir) {
  cat("  Creating contribution plots...\n")
  
  plot_dir <- file.path(output_dir, "pca_plots", dataset_name)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  # Get loadings
  loadings <- pca_result$pca_model$var$coord
  n_pc <- ncol(loadings)
  
  # Create contribution plot for each PC
  for (i in 1:n_pc) {
    pc_loadings <- loadings[, i, drop = FALSE]
    pc_contrib <- data.frame(
      Variable = rownames(pc_loadings),
      Loading = abs(pc_loadings[, 1]),
      Sign = ifelse(pc_loadings[, 1] > 0, "Positive", "Negative")
    ) %>%
    arrange(desc(Loading))
    
    p_contrib <- ggplot(pc_contrib, aes(x = reorder(Variable, Loading), y = Loading, fill = Sign)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Positive" = "steelblue", "Negative" = "coral")) +
      labs(
        title = paste("Top Contributors to PC", i, "-", dataset_name),
        x = "Variable",
        y = "Absolute Loading"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(
      file.path(plot_dir, paste0("pc", i, "_top_contributors.png")),
      p_contrib,
      width = 8, height = 6, dpi = 300
    )
  }
  
  cat("    ✓ Contribution plots saved for top", n_pc, "PCs\n")
}

# Main function to create all advanced PCA plots
create_all_advanced_pca_plots <- function(pca_results, output_dir) {
  cat("Creating advanced PCA visualizations...\n\n")
  
  for (dataset_name in names(pca_results)) {
    cat(strrep("=", 50), "\n")
    cat("Processing advanced plots for:", dataset_name, "\n")
    
    pca_result <- pca_results[[dataset_name]]$pca_result
    train_labels <- NULL
    
    # Try to get labels from train_complete data
    if ("train_complete" %in% names(pca_results[[dataset_name]])) {
      train_complete <- pca_results[[dataset_name]]$train_complete
      if ("light_type" %in% names(train_complete) || "location" %in% names(train_complete)) {
        train_labels <- train_complete[, intersect(c("light_type", "location"), names(train_complete)), drop = FALSE]
      }
    }
    
    # Set variable order based on dataset type
    variable_order <- NULL
    if (grepl("sensor", dataset_name, ignore.case = TRUE)) {
      if (grepl("_l2_", dataset_name)) {
        variable_order <- c("norm_F1", "norm_F2", "norm_F3", "norm_F4", "norm_F5", "norm_F6", "norm_F7", "norm_F8", "norm_CLEAR", "norm_IR.LIGHT")
      } else {
        variable_order <- c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "CLEAR", "IR.LIGHT")
      }
    } else if (grepl("alpha", dataset_name, ignore.case = TRUE)) {
      if (grepl("_l2_", dataset_name)) {
        variable_order <- c("norm_PHO", "norm_MEDI", "norm_SC", "norm_MC", "norm_LC", "norm_RH")
      } else {
        variable_order <- c("PHO", "MEDI", "SC", "MC", "LC", "RH")
      }
    } else if (grepl("spd", dataset_name, ignore.case = TRUE)) {
      if (grepl("_l2_", dataset_name)) {
        variable_order <- c("norm_415", "norm_445", "norm_480", "norm_515", "norm_555", "norm_590", "norm_630", "norm_680", "norm_750")
      } else {
        variable_order <- c("X415", "X445", "X480", "X515", "X555", "X590", "X630", "X680", "X750")
      }
    }
    
    # Create all advanced plots
    create_pca_scores_matrix(pca_result, train_labels, dataset_name, output_dir)
    create_loadings_heatmap(pca_result, dataset_name, output_dir, variable_order)
    create_detailed_scree_plot(pca_result, dataset_name, output_dir)
    create_contribution_plots(pca_result, dataset_name, output_dir)
    
    cat("Completed advanced plots for", dataset_name, "\n\n")
  }
  
  cat("All advanced PCA visualizations completed!\n")
}

# Execute if PCA results are available
if (exists("pca_results_all")) {
  cat("Creating advanced PCA visualizations...\n")
  create_all_advanced_pca_plots(pca_results_all, paths$pca_outputs)
} else {
  cat("Please run perform_pca.R first to generate PCA results.\n")
  cat("Usage: After running perform_pca.R, run this script to create advanced visualizations.\n")
}

# 粘贴式主成分loadings分面图函数
plot_pca_loadings_facet <- function(pca_result, variable_order = NULL, n_pc = NULL, output_path = NULL, var_label = "Variable") {
  # 如果没有指定n_pc，则使用所有可用的主成分
  if (is.null(n_pc)) {
    n_pc <- ncol(pca_result$pca_model$var$coord)
  }
  
  # 提取 loadings
  loadings <- as.data.frame(pca_result$pca_model$var$coord[, 1:min(n_pc, ncol(pca_result$pca_model$var$coord)), drop = FALSE])
  loadings$Variable <- rownames(loadings)
  if (!is.null(variable_order)) {
    loadings$Variable <- factor(loadings$Variable, levels = variable_order)
  }
  # 转长表
  loadings_long <- tidyr::pivot_longer(
    loadings,
    cols = starts_with("Dim"),
    names_to = "PC",
    values_to = "Loading"
  )
  # 画图
  p <- ggplot(loadings_long, aes(x = Variable, y = Loading, group = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(linewidth = 1) +
    geom_point(aes(color = Variable), size = 3) +
    facet_wrap(~ PC, scales = "free_y", ncol = 1) +
    labs(title = "Variable Contributions to Principal Components",
         x = var_label, y = "Loading") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if (!is.null(output_path)) {
    ggsave(output_path, p, width = 8, height = 2 * min(n_pc, ncol(pca_result$pca_model$var$coord)))
  }
  return(p)
}


# Load necessary libraries
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)

# Function to scale and perform PCA on training data
perform_pca_with_scaling <- function(train_features, dataset_name, scale_data = TRUE) {
  cat("Processing", dataset_name, "...\n")
  # Check data dimensions first
  cat("  Data dimensions:", nrow(train_features), "samples x", ncol(train_features), "features\n")
  
  # Check if data appears to be L2 normalized
  sample_norms <- sqrt(rowSums(train_features^2, na.rm = TRUE))
  is_l2_normalized <- all(abs(sample_norms - 1) < 0.01, na.rm = TRUE)
  if (is_l2_normalized) {
    cat("  Data appears to be L2 normalized (row norms ≈ 1)\n")
  }
  
  # Check feature variances
  feature_vars <- apply(train_features, 2, var, na.rm = TRUE)
  var_ratio <- max(feature_vars, na.rm = TRUE) / min(feature_vars, na.rm = TRUE)
  cat("  Feature variance ratio (max/min):", round(var_ratio, 2), "\n")
  
  # 1. Scale training data if requested
  if (scale_data) {
    cat("  Applying standardization (centering and scaling)...\n")
    train_scaled <- scale(train_features)
    scaling_params <- list(
      center = attr(train_scaled, "scaled:center"),
      scale = attr(train_scaled, "scaled:scale"),
      scaled = TRUE
    )
  } else {
    cat("  Skipping standardization (using raw L2 normalized data)...\n")
    train_scaled <- as.matrix(train_features)
    scaling_params <- list(
      center = rep(0, ncol(train_features)),
      scale = rep(1, ncol(train_features)),
      scaled = FALSE
    )
  }
  
  # 2. Perform PCA
  pca_model <- PCA(train_scaled, graph = FALSE, scale.unit = FALSE)  # Already scaled if needed
  # 3. Check available components
  max_components <- min(nrow(train_scaled) - 1, ncol(train_scaled))
  available_components <- ncol(pca_model$ind$coord)
  cat("  Available PCA components:", available_components, "\n")
  # 4. Use all available components (no variance threshold limit)
  variance_explained <- pca_model$eig[, 2] / 100  # percentage of variance
  cumulative_var <- cumsum(variance_explained)
  # Keep all available components
  n_components <- available_components
  cat("  Selected components:", n_components, "of", available_components, "available (keeping all)\n")
  # 5. Extract PCA scores
  train_pca_scores <- as.data.frame(pca_model$ind$coord[, 1:n_components, drop = FALSE])
  colnames(train_pca_scores) <- paste0("PC", 1:n_components)
  cat("  Features:", ncol(train_features), "-> PCs:", n_components)
  cat(" (", round(cumulative_var[n_components]*100, 1), "% variance - all components)\n")
  return(list(
    pca_model = pca_model,
    scaling_params = scaling_params,
    train_scores = train_pca_scores,
    eigenvalues = pca_model$eig,
    n_components = n_components,
    variance_explained = cumulative_var[n_components],
    max_available_components = available_components
  ))
}

# Function to project test data using trained PCA model
project_test_data <- function(test_features, pca_result, dataset_name) {
  cat("Projecting test data for", dataset_name, "...\n")
  
  # Scale test data using training parameters (if scaling was applied)
  if (pca_result$scaling_params$scaled) {
    test_scaled <- scale(test_features, 
                        center = pca_result$scaling_params$center,
                        scale = pca_result$scaling_params$scale)
  } else {
    test_scaled <- as.matrix(test_features)
  }
  
  # Project to PCA space
  test_projected <- predict(pca_result$pca_model, newdata = test_scaled)
  
  # Ensure we don't exceed available components
  n_components <- min(pca_result$n_components, ncol(test_projected$coord))
  test_pca_scores <- as.data.frame(test_projected$coord[, 1:n_components, drop = FALSE])
  colnames(test_pca_scores) <- paste0("PC", 1:n_components)
  
  cat("  Test data projected:", nrow(test_pca_scores), "samples x", ncol(test_pca_scores), "components\n")
  
  return(test_pca_scores)
}

# Function to create PCA score plots colored by labels
create_pca_score_plots <- function(pca_result, train_labels, dataset_name, plot_dir) {
  cat("  Creating PCA score plots...\n")
  
  # 提取前两个主成分的得分
  if (ncol(pca_result$pca_model$ind$coord) < 2) {
    cat("  Warning: Less than 2 components available, skipping score plots\n")
    return()
  }
  
  pca_scores <- data.frame(
    Dim1 = pca_result$pca_model$ind$coord[, 1],
    Dim2 = pca_result$pca_model$ind$coord[, 2]
  )
  
  # 获取方差解释比例
  var_explained <- pca_result$pca_model$eig[1:2, 2]
  x_label <- paste0("Dim1 (", round(var_explained[1], 1), "%)")
  y_label <- paste0("Dim2 (", round(var_explained[2], 1), "%)")
  
  # 按 light_type 着色（如果存在）
  if ("light_type" %in% names(train_labels) && nrow(train_labels) > 0) {
    pca_scores$light_type <- train_labels$light_type
    
    p_light <- ggplot(pca_scores, aes(x = Dim1, y = Dim2, color = light_type)) +
      geom_point(alpha = 0.7, size = 2) +
      stat_ellipse(aes(fill = light_type), alpha = 0.2, geom = "polygon") +
      labs(x = x_label, y = y_label,
           title = paste("PCA Score Plot -", dataset_name, "by Light Type"),
           color = "Light Type", fill = "Light Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggsave(file.path(plot_dir, "pca_scores_light_type.png"), p_light, 
           width = 10, height = 8, dpi = 300)
    cat("    ✓ Light type score plot saved\n")
  }
  
  # 按 location 着色（如果存在）
  if ("location" %in% names(train_labels) && nrow(train_labels) > 0) {
    pca_scores$location <- train_labels$location
    
    p_location <- ggplot(pca_scores, aes(x = Dim1, y = Dim2, color = location)) +
      geom_point(alpha = 0.7, size = 2) +
      stat_ellipse(aes(fill = location), alpha = 0.2, geom = "polygon") +
      labs(x = x_label, y = y_label,
           title = paste("PCA Score Plot -", dataset_name, "by Location"),
           color = "Location", fill = "Location") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggsave(file.path(plot_dir, "pca_scores_location.png"), p_location, 
           width = 10, height = 8, dpi = 300)
    cat("    ✓ Location score plot saved\n")
  }
}


# Function to create PCA visualizations
create_pca_plots <- function(pca_result, dataset_name, output_dir, train_labels = NULL) {
  plot_dir <- file.path(output_dir, "pca_plots", dataset_name)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  # Scree plot
  scree_plot <- fviz_eig(pca_result$pca_model, addlabels = TRUE, ylim = c(0, 50)) +
    ggtitle(paste("Scree Plot -", dataset_name))
  ggsave(file.path(plot_dir, "scree_plot.png"), scree_plot, width = 10, height = 6)
  
  # PCA Score plots (if labels are provided)
  if (!is.null(train_labels)) {
    create_pca_score_plots(pca_result, train_labels, dataset_name, plot_dir)
  }
  
  cat("  Plots saved to:", plot_dir, "\n")
}

# Function to save PCA results and combine with all feature types
save_pca_complete <- function(pca_result, test_scores, train_data, test_data, dataset_name, output_dir) {
  # Create dataset-specific subdirectory for PCA results
  pca_dir <- file.path(output_dir, "pca_results", dataset_name)
  if (!dir.exists(pca_dir)) dir.create(pca_dir, recursive = TRUE)
  
  # Combine PCA scores with other feature types
  train_complete <- cbind(
    train_data$identifiers,
    train_data$labels,
    train_data$time_features,
    train_data$other_features,
    pca_result$train_scores
  )
  
  test_complete <- cbind(
    test_data$identifiers,
    test_data$labels,
    test_data$time_features,
    test_data$other_features,
    test_scores
  )
  
  # Save complete datasets
  saveRDS(train_complete, file.path(pca_dir, "train_pca_complete.rds"))
  saveRDS(test_complete, file.path(pca_dir, "test_pca_complete.rds"))
  
  # Save PCA model and parameters
  saveRDS(pca_result$pca_model, file.path(pca_dir, "pca_model.rds"))
  saveRDS(pca_result$scaling_params, file.path(pca_dir, "scaling_params.rds"))
  
  # Save all loadings (no component limit)
  loadings <- as.data.frame(pca_result$pca_model$var$coord)
  saveRDS(loadings, file.path(pca_dir, "loadings.rds"))
  
  # Save summary
  pca_summary <- list(
    dataset_name = dataset_name,
    original_features = ncol(pca_result$pca_model$call$X),
    n_components = pca_result$n_components,
    max_available_components = pca_result$max_available_components,
    variance_explained = pca_result$variance_explained,
    eigenvalues = pca_result$eigenvalues,
    train_samples = nrow(train_complete),
    test_samples = nrow(test_complete),
    feature_summary = list(
      light_vectors_pca = ncol(pca_result$train_scores),
      time_features = ncol(train_data$time_features),
      other_features = ncol(train_data$other_features),
      labels = ncol(train_data$labels),
      identifiers = ncol(train_data$identifiers)
    )
  )
  saveRDS(pca_summary, file.path(pca_dir, "summary.rds"))
  
  cat("  Complete results saved to:", pca_dir, "\n")
  return(list(train = train_complete, test = test_complete))
}

# Main processing function
process_all_datasets_pca <- function(parsed_datasets, scale_data = TRUE) {
  cat("Starting complete PCA processing...\n")
  cat("Using all available principal components (no dimension reduction)\n")
  if (scale_data) {
    cat("Using standardization (recommended even after L2 normalization)\n\n")
  } else {
    cat("Skipping standardization (using L2 normalized data directly)\n\n")
  }
  
  all_results <- list()
  output_dir <- paths$pca_outputs
  
  for (base_name in names(parsed_datasets)) {
    cat(strrep("=", 60), "\n")
    cat("Processing dataset:", base_name, "\n")
    
    # Get data - use new structure
    train_light_vectors <- parsed_datasets[[base_name]]$train$light_vectors_cleaned
    test_light_vectors <- parsed_datasets[[base_name]]$test$light_vectors_cleaned
    train_data <- parsed_datasets[[base_name]]$train
    test_data <- parsed_datasets[[base_name]]$test
    
    # Check data validity
    if (is.null(train_light_vectors) || ncol(train_light_vectors) < 3) {
      cat("  Skipping - insufficient light vector features\n\n")
      next
    }
    
    # 1. Scale and perform PCA on training light vectors only (keep all components)
    pca_result <- perform_pca_with_scaling(train_light_vectors, base_name, scale_data = scale_data)
    
    # 2. Project test data
    test_scores <- project_test_data(test_light_vectors, pca_result, base_name)
    
    # 3. Create basic visualizations
    create_pca_plots(pca_result, base_name, output_dir, train_labels = train_data$labels)
    
    # 4. Save complete results with all feature types
    complete_data <- save_pca_complete(pca_result, test_scores, train_data, test_data, base_name, output_dir)
    
    all_results[[base_name]] <- list(
      pca_result = pca_result,
      train_complete = complete_data$train,
      test_complete = complete_data$test
    )
    
    cat("Completed processing for", base_name, "\n\n")
  }
  
  cat("All PCA processing completed!\n")
  return(all_results)
}

# Execute if datasets are available
if (exists("parsed_datasets")) {
  cat("Running complete PCA analysis...\n")
  pca_results_all <- process_all_datasets_pca(parsed_datasets, scale_data = TRUE)
  cat("PCA analysis complete! Results saved in", file.path(paths$pca_outputs, "pca_results"), "\n")
  cat("\nTo create advanced visualizations, run:\n")
  cat("source('src/s04_pca/create_advanced_pca_plots.R')\n")
} else {
  cat("Please run prepare_data.R first to load datasets.\n")
}
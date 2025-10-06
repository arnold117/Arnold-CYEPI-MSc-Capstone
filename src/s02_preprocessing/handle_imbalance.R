
# Load necessary libraries
library(themis)

balance_smote_tomek <- function(data, feature_cols, target_col, k_neighbors = 5) {
  # Ensure target variable is a factor
  data[[target_col]] <- as.factor(data[[target_col]])
  
  # Build formula
  fml <- as.formula(paste(target_col, "~", paste(feature_cols, collapse = " + ")))
  
  # Create recipe
  data_recipe <- recipe(fml, data = data) %>%
    step_smote(all_outcomes(), seed = 1234) %>%
    step_tomek(all_outcomes()) %>%
    prep()
  
  # Extract processed data
  balanced_data <- bake(data_recipe, new_data = NULL)

  # Log the class distribution before and after balancing
  class_dist <- table(balanced_data[[target_col]])
  log_text <- paste0(
    "SMOTE-TOMEK balancing completed:\n",
    "  Original: ", nrow(data), " -> Balanced: ", nrow(balanced_data), "\n",
    "  Distribution: ", paste(names(class_dist), class_dist, sep="=", collapse=", "), "\n"
  )
  cat(log_text,
  file = file.path(training_output_dir, "imbalance_handling_log.txt"))
  
  return(balanced_data)
}

# Apply imbalance handling based on configuration
apply_imbalance_handling <- function(train_data, feature_cols, target_col, method = "none") {
  if (method == "SMOTE-TOMEK") {

    # Apply SMOTE-TOMEK
    train_balanced <- balance_smote_tomek(train_data, feature_cols, target_col)

    cat("✓ SMOTE-TOMEK balancing completed\n")
    return(train_balanced)
  }
  else if (method == "None") {
    cat("No class imbalance handling applied.\n")
    return(train_data)
  } else {
    stop("Unknown imbalance handling method: ", method)
  }
}


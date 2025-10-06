# src/s02_preprocessing/aggregate.R - Data Aggregation Script

# Load necessary libraries
library(tidyverse)

# load utility functions
source("src/utils/medoid.R")

cat("Starting data aggregation...\n")


# 自动读取 data_processed 文件夹下所有 .rds 文件（不包括 transformed_hlea.rds）
all_rds_files <- list.files(paths$data_processed, pattern = "\\.rds$", full.names = TRUE)
all_rds_files <- all_rds_files[!grepl("transformed_hlea\\.rds$", all_rds_files)]
loaded_datasets <- list()
for (file in all_rds_files) {
  dataset_name <- sub("\\.rds$", "", basename(file))
  cat("Loading", dataset_name, "...\n")
  loaded_datasets[[dataset_name]] <- readRDS(file)
  cat("✓ Loaded", dataset_name, "successfully\n")
}

# Load transformed mHLEA labels
transformed_hlea <- readRDS(file.path(paths$data_processed, "transformed_hlea.rds"))
cat("✓ Loaded transformed mHLEA labels successfully\n")

# Data Cleaning
cat("\nCleaning data...\n")
# Data cleaning for all datasets

cleaned_datasets <- list()

for (dataset_name in names(loaded_datasets)) {
  cat("Cleaning", dataset_name, "...\n")

  # Check if there are any NA values in the dataset
  na_count <- sum(is.na(loaded_datasets[[dataset_name]]))
  cat("Number of NA values in", dataset_name, ":", na_count, "\n")

  if (na_count == 0) {
    cat("No NA values found in", dataset_name, ". Skipping cleaning step.\n\n")
    cleaned_datasets[[dataset_name]] <- loaded_datasets[[dataset_name]]
    next
  }

  # Remove rows with any NA values
  cleaned_datasets[[dataset_name]] <- loaded_datasets[[dataset_name]] %>%
    ungroup() %>%
    filter(complete.cases(.))

  # Check how many rows were removed
  cat("Rows before cleaning:", nrow(loaded_datasets[[dataset_name]]), "\n")
  cat("Rows after cleaning:", nrow(cleaned_datasets[[dataset_name]]), "\n")
  cat("Rows removed:", nrow(loaded_datasets[[dataset_name]]) - nrow(cleaned_datasets[[dataset_name]]), "\n\n")
}

# Hourly aggregation
cat("\nAggregating data to hourly level...\n")

# Loop through all datasets and aggregate with HLEA
aggregated_datasets <- list()

for (dataset_name in names(cleaned_datasets)) {
  cat("Aggregating", dataset_name, "to hourly level...\n")

  # Get hourly data
  hourly_data <- cleaned_datasets[[dataset_name]] %>%
    mutate(Datetime = floor_date(Datetime, unit = "hour")) %>%
    group_by(Id, Datetime)

  # Nested list to store hourly data
  hourly_nested <- hourly_data %>%
    summarise(
      hour_data = list({
        # Convert group to regular dataframe first
        group_df <- data.frame(cur_data())
        # Then select columns normally
        group_df[, !names(group_df) %in% c("Id", "Datetime"), drop = FALSE]
      }),
      datetime_points = list(Datetime),
      n_points = n(),
      .groups = "drop"
    ) %>%
    ungroup()

  # Medoid calculation
  hourly_aggregated <- hourly_nested %>%
    mutate(
      aggregated_data = map(hour_data, find_medoid)
    )

  # Extract aggregated features and convert to dataframe
  hourly_final <- hourly_aggregated %>%
    select(Id, Datetime, aggregated_data) %>%
    mutate(
      aggregated_df = map(aggregated_data, ~{
        if (is.null(.)) return(data.frame())
        # Create dataframe with proper column names
        result_df <- as.data.frame(t(.))
        # Get original column names from the first hour_data sample
        original_cols <- names(hourly_nested$hour_data[[1]])
        names(result_df) <- original_cols
        result_df
      }),
      n_cols = map_int(aggregated_df, ncol)
    ) %>%
    filter(n_cols > 0)

  # Expand results into a tidy dataframe
  hourly_final <- hourly_final %>%
    select(-aggregated_data, -n_cols) %>%
    unnest(aggregated_df) %>%
    mutate(
      Datetime = as.POSIXct(Datetime,
                           tz = config$preprocessing$actlumus$timezone),
      Id = as.character(Id)
    )

  # Join with HLEA data
  aggregated_datasets[[dataset_name]] <- hourly_final %>%
    left_join(transformed_hlea, by = c("Id", "Datetime" = "timestamp"))

  cat("✓ Completed aggregation for", dataset_name, "\n")

  # Save aggregated data
  output_path <- file.path(paths$data_aggregated, paste0(dataset_name, "_agg.rds"))
  saveRDS(aggregated_datasets[[dataset_name]], output_path)
  cat("✓ Saved aggregated data for", dataset_name, "to", output_path, "\n")
}

# src/s01_data_loading/load_hlea_paper.R - Load mHLEA data from external files

# Load necessary libraries
library(lubridate)
library(tidyverse)
library(readxl)
library(stringr)

# Use global variables defined in setup.R
cat("  Reading from:", paths$external_data, "\n")

# Find mHLEA files
file_paths <- list.files(
  paths$external_data,
  pattern = "_mHLEA_paper.*\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

cat("  Found", length(file_paths), "mHLEA files\n")

if (length(file_paths) == 0) {
  stop("No mHLEA files found in: ", paths$external_data)
}

# Use config parameters
tz <- config$preprocessing$hlea$timezone
fix_backwards <- config$preprocessing$hlea$fix_backwards_timestamps
# Function to extract participant ID
extract_id <- function(file_path) {
  id <- str_extract(basename(file_path), "^[0-9]+")
  id
}

# Function to fix timestamps
fix_timestamps <- function(data) {
  if (!fix_backwards || nrow(data) <= 1) {
    return(data)
  }

  for (i in 2:nrow(data)) {
    if (!is.na(data$timestamp[i]) && !is.na(data$timestamp[i - 1])) {
      if (data$timestamp[i] <= data$timestamp[i - 1]) {
        data$timestamp[i] <- data$timestamp[i - 1] + hours(1)
      }
    }
  }
  data
}

# Load each file
hlea_list <- lapply(file_paths, function(file_path) {
  pid <- extract_id(file_path)
  if (!is.na(pid)) {
    data <- read_excel(file_path)
    data$Id <- pid

    # Process timestamps
    data$timestamp <- as.POSIXct(data$timestamp, format="%d/%m/%Y %H:%M", tz = tz)
    data <- fix_timestamps(data)
    data$timestamp <- floor_date(data$timestamp, unit = "hour")

    # Separate lightsource column
    if ("lightsource" %in% names(data)) {
      data <- data %>%
        separate(lightsource,
                into = c("main_light", "second_light"),
                sep = "[+]",
                fill = "right")
    }

    # Select relevant columns
    data %>% select(Id, timestamp, main_light, second_light)
  }
})

# Combine all data
hlea_all <- bind_rows(hlea_list)

# Save data to defined path
output_path <- file.path(paths$data_raw, "mHLEA_paper.rds")
saveRDS(hlea_all, file = output_path)

cat("  Saved to:", output_path, "\n")
cat("  Records:", nrow(hlea_all), "\n")
cat("  Participants:", length(unique(hlea_all$Id)), "\n")
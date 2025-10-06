# src/s01_data_loading/load_actlumus.R - Load Actlumus data from external files

# Load necessary libraries
library(LightLogR)
library(lubridate)

cat("  Reading from:", paths$external_data, "\n")

# Find Actlumus files
file_paths <- list.files(
  paths$external_data,
  pattern = "_actlumus_Log_.*\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

# Filter out report files
file_paths <- file_paths[!grepl("_Report", file_paths)]

cat("  Found", length(file_paths), "Actlumus files\n")

if (length(file_paths) == 0) {
  stop("No Actlumus files found in: ", paths$external_data)
}

# Use timezone from config
tz <- config$preprocessing$actlumus$timezone
pattern <- "^(\\d{3})"

# Load data
actlumus_data <- import$ActLumus(
  file_paths,
  tz = tz,
  auto.id = pattern,
  print_n = 50,
  dst_adjustment = TRUE
)

# Save data to defined path
output_path <- file.path(paths$data_raw, "actlumus.rds")
saveRDS(actlumus_data, file = output_path)

cat("  Saved to:", output_path, "\n")
cat("  Records:", nrow(actlumus_data), "\n")
cat("  Participants:", length(unique(actlumus_data$Id)), "\n")
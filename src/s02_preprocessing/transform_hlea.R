# src/s02_preprocessing/transform_hlea.R - Transform mHLEA Labels

# load necessary libraries
library(tidyverse)
library(lubridate)

# load hlea data
hlea_data <- readRDS(file.path(paths$data_raw, "mHLEA_paper.rds"))

# transform hlea data
hlea_trans <- hlea_data %>%
  select(-second_light) %>%
  mutate(
    location = case_when(
      is.na(main_light) ~ NA_character_,
      main_light %in% c("I", "L", "E") ~ "In",
      main_light %in% c("O", "S") ~ "Out",
      main_light %in% c("X", "W") ~ "Dark",
      TRUE ~ NA_character_
    ),
    light_type = case_when(
      is.na(main_light) ~ NA_character_,
      main_light %in% c("I", "O") ~ "Natural",
      main_light %in% c("L", "S", "E") ~ "Artificial",
      main_light %in% c("X", "W") ~ "Dark",
      TRUE ~ NA_character_
    ),
    hour = lubridate::hour(timestamp),
    hour_sin = sin(2 * pi * hour / 24),
    hour_cos = cos(2 * pi * hour / 24)
  )

# save transformed data
output_path <- file.path(paths$data_processed, "transformed_hlea.rds")
saveRDS(hlea_trans, output_path)

cat("mHLEA labels transformed successfully.\n")
cat("  Saved to:", output_path, "\n")
cat("  Records:", nrow(hlea_trans), "\n")
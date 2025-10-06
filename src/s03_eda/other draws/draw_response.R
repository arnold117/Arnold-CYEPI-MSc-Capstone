# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Read the Excel file
excel_file <- "ActLumus Light Sensor Norm Response.xlsx"
# Read from row 4 onwards as the first 3 rows contain headers
sensor_data_raw <- read_excel(excel_file, skip = 3)

# Clean the data - remove empty rows and convert to numeric
# First check the actual column structure
cat("Raw data structure:\n")
print(str(sensor_data_raw))
cat("\nColumn names:\n")
print(colnames(sensor_data_raw))

# Clean the data using the first column (regardless of its name)
first_col_name <- colnames(sensor_data_raw)[1]
sensor_data <- sensor_data_raw %>%
  filter(!is.na(.data[[first_col_name]])) %>%  # Remove rows where wavelength is NA
  mutate(across(everything(), as.numeric)) %>%
  filter(!is.na(.data[[first_col_name]]))  # Remove any remaining NA wavelengths

# Set proper column names based on the Excel structure
# First column is wavelength, then 10 channels
colnames(sensor_data) <- c("Wavelength", "F1_415nm", "F2_445nm", "F3_480nm", 
                          "F4_515nm", "F5_555nm", "F6_590nm", "F7_630nm", 
                          "F8_680nm", "CLEAR_750nm", "IR_900nm")

# Display the structure of the cleaned data
cat("Data structure after cleaning:\n")
print(str(sensor_data))
cat("\nFirst few rows:\n")
print(head(sensor_data))
cat("\nLast few rows:\n")
print(tail(sensor_data))

# Create a long format for plotting
sensor_long <- sensor_data %>%
  pivot_longer(cols = -Wavelength, 
               names_to = "Channel", 
               values_to = "Response")

# Define custom colors matching the spectral characteristics
custom_colors <- c(
  "F1_415nm" = "#8B00FF",     # Violet for 415nm
  "F2_445nm" = "#0000FF",     # Blue for 445nm  
  "F3_480nm" = "#00BFFF",     # Cyan for 480nm
  "F4_515nm" = "#00FF00",     # Green for 515nm
  "F5_555nm" = "#7FFF00",     # Lime green for 555nm
  "F6_590nm" = "#FFFF00",     # Yellow for 590nm
  "F7_630nm" = "#FF8C00",     # Orange for 630nm
  "F8_680nm" = "#FF0000",     # Red for 680nm
  "CLEAR_750nm" = "#808000",  # Olive for CLEAR/750nm
  "IR_900nm" = "#4B0082"      # Indigo for IR/900nm
)

# Create the response curve plot
p1 <- ggplot(sensor_long, aes(x = Wavelength, y = Response, color = Channel)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = custom_colors, name = "Channel") +
  labs(x = "Wavelength (nm)",
       y = "Normalized Response") +
  theme_minimal() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))

# Display the plot
print(p1)

# Create individual subplot for each channel with custom colors
p2 <- ggplot(sensor_long, aes(x = Wavelength, y = Response, color = Channel)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = custom_colors) +
  facet_wrap(~Channel, scales = "free_y", ncol = 5) +
  labs(title = "Individual Channel Response Curves",
       x = "Wavelength (nm)",
       y = "Normalized Response") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none")  # Remove legend for individual plots

# Display the subplot
print(p2)

# Save the plots
ggsave("sensor_response_curves_combined.png", plot = p1, 
       width = 12, height = 8, dpi = 300)
ggsave("sensor_response_curves_individual.png", plot = p2, 
       width = 15, height = 10, dpi = 300)

# Create a summary statistics table
summary_stats <- sensor_long %>%
  group_by(Channel) %>%
  summarise(
    Min_Response = min(Response, na.rm = TRUE),
    Max_Response = max(Response, na.rm = TRUE),
    Mean_Response = mean(Response, na.rm = TRUE),
    Peak_Wavelength = Wavelength[which.max(Response)],
    .groups = 'drop'
  )

cat("\nSummary statistics for each channel:\n")
print(summary_stats)

# Optional: Create a heatmap view
p3 <- ggplot(sensor_long, aes(x = Wavelength, y = Channel, fill = Response)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Response") +
  labs(title = "Sensor Response Heatmap",
       x = "Wavelength (nm)",
       y = "Channel") +
  theme_minimal()

print(p3)
ggsave("sensor_response_heatmap.png", plot = p3, 
       width = 12, height = 6, dpi = 300)

cat("\nPlots saved as PNG files in the current directory.\n")
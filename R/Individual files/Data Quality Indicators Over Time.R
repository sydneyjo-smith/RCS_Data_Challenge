# Load necessary libraries
library(dplyr)
library(readxl)
library(tidyr)
library(pheatmap)
library(lubridate)
library(tibble)
library(stringr)

# File path
file_path <- "cancerdata_combined.xlsx"

# Get sheet names
sheet_names <- excel_sheets(file_path)

# Read all sheets into a list
data_list <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
})

# Name the list elements by sheet names
names(data_list) <- sheet_names

# Access individual sheets
data <- data_list[["Indicators_trust"]]
data_qual <- data_list[["data_quality"]]
data_qual_targets <- data_list[["data_quality_targets"]]

# Combine data quality and targets
quality_combined <- data_qual %>%
  left_join(data_qual_targets, by = c("audit", "metric_name"))

# Ensure pact values are in the correct range (0-1)
quality_combined_vals <- quality_combined %>%
  mutate(pact = if_else(pact > 1, pact / 100, pact))

# Convert the 'date' column to Date type
quality_combined_vals$date <- as.Date(quality_combined_vals$date, format = "%Y-%m-%d")

# Create 'quarter_year' column in "YYYY-Qn" format
quality_combined_vals <- quality_combined_vals %>%
  mutate(quarter_year = paste0(format(date, "%Y"), "-Q", quarter(date)))

# Step 1: Create a status column based on pact vs target
quality_combined_vals <- quality_combined_vals %>%
  mutate(status = case_when(
    is.na(pact) ~ "Missing",  # Grey for missing values
    pact >= target ~ "Above Target",  # Green for above target
    pact >= (target - 0.02) ~ "Within 2%",  # Yellow for within 2% below target
    TRUE ~ "Below Target"  # Red for more than 2% below target
  ))

# Step 2: Prepare heatmap data with numeric values for pheatmap
heatmap_data <- quality_combined_vals %>%
  filter(audit == "NBOCA", trust_code == "RJ1") %>%  # Apply your filters
  group_by(metric_name, quarter_year) %>%
  summarise(status = ifelse(all(is.na(status)), "Missing", max(status, na.rm = TRUE))) %>%  # Handle NA status
  ungroup() %>%  # Ungroup to avoid issues with spread
  mutate(status_num = case_when(
    status == "Above Target" ~ 3,
    status == "Within 2%" ~ 2,
    status == "Below Target" ~ 1,
    status == "Missing" ~ 0
  )) %>%
  select(-status) %>%  # Remove the categorical status column
  spread(key = quarter_year, value = status_num) %>%  # Spread quarters into columns
  column_to_rownames(var = "metric_name")  # Use metric names as row names

# Step 3: Ensure quarters are ordered chronologically
# Extract unique quarters and sort them
sorted_quarters <- colnames(heatmap_data) %>%
  str_replace("Q", "") %>%  # Remove "Q" to convert to sortable format
  as.numeric() %>%  # Convert to numeric for sorting
  order()  # Get the order of quarters

# Reorder the columns in heatmap_data
heatmap_data <- heatmap_data[, sorted_quarters]

# Step 4: Define color mapping
color_mapping <- c("0" = "grey", "1" = "red", "2" = "yellow", "3" = "green")

# Step 5: Create heatmap with pheatmap (without dendrograms)
pheatmap(heatmap_data,
         color = color_mapping,
         scale = "none",
         cluster_rows = FALSE,  # Disable row clustering to remove branches
         cluster_cols = FALSE,  # Disable column clustering to preserve order
         show_rownames = TRUE,
         show_colnames = TRUE,
         main = "Data Quality Indicators Over Time",
         legend_breaks = c(0, 1, 2, 3),
         legend_labels = c("Missing", "Below Target", "Within 2%", "Above Target"),
         na_col = "grey")


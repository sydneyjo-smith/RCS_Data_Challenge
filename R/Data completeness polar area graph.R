library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

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
audit_meta_info <- data_list[["audit_meta_info"]]
Indicators_trust <- data_list[["Indicators_trust"]]
Indicators_CA <- data_list[["Indicators_CA"]]
data_quality <- data_list[["data_quality"]]
metric_info <- data_list[["metric_info"]]

# Calculate percentage of non-NA values for each audit in Guy's
non_na_percent <- Indicators_trust %>%
  filter(trust_name %in% c("Guys and St Thomas NHS Foundation Trust", "Guy's and St Thomas' NHS Foundation Trust")) %>%
  group_by(audit) %>%
  summarise(
    non_na_percentage = mean(!is.na(mav)) * 100  # Calculate percentage of non-NA MAV values
  )

ggplot(non_na_percent, aes(x = audit, y = non_na_percentage, fill = audit)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "x") +
  geom_hline(yintercept = seq(20, 100, by = 20), color = "gray60", linetype = "dashed") +
  geom_text(data = grid_labels, aes(x = 1.5, y = y, label = label), inherit.aes = FALSE, 
            hjust = 1, size = 3, color = "gray30", angle = 0) + 
  scale_fill_manual(values = rainbow_colors) + 
  labs(
    title = "Percentage of Complete Moving Average Values at\nGuy's and St Thomas' NHS Foundation Trust",
    x = NULL,
    y = NULL,  
    fill = "Cancer Audit"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, angle = 0),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  )

# Filter data for Birmingham Women's and Children's NHS Foundation Trust
# Which only has data for NOCA
non_na_percent <- Indicators_trust %>%
  filter(trust_name == "Birmingham Women's and Children's NHS Foundation Trust") %>%
  group_by(audit) %>%
  summarise(
    non_na_percentage = mean(!is.na(mav)) * 100  # Calculate percentage of non-NA MAV values
  )

# Create a data frame for gridline labels
grid_labels <- data.frame(
  y = seq(20, 100, by = 20),  # Match gridline intercepts
  label = paste0(seq(20, 100, by = 20), "%")
)

# Create the polar area chart with labeled gridlines
ggplot(non_na_percent, aes(x = audit, y = non_na_percentage, fill = audit)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "x") +
  geom_hline(yintercept = seq(20, 100, by = 20), color = "gray60", linetype = "dashed") +
  geom_text(data = grid_labels, aes(x = 1.5, y = y, label = label), inherit.aes = FALSE, 
            hjust = 1, size = 3, color = "gray30", angle = 0) + 
  scale_fill_manual(values = rainbow_colors) + 
  labs(
    title = "Percentage of Complete Moving Average Values at\nBirmingham Women's and Children's NHS Foundation Trust",
    x = NULL,
    y = NULL,  
    fill = "Cancer Audit"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, angle = 0),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  )

# Calculate percentage of non-NA values for each audit in University Hospitals Dorset NHS Foundation Trust
non_na_percent <- Indicators_trust %>%
  filter(trust_name == "University Hospitals Dorset NHS Foundation Trust") %>%
  group_by(audit) %>%
  summarise(
    non_na_percentage = mean(!is.na(mav)) * 100  # Calculate percentage of non-NA MAV values
  )

ggplot(non_na_percent, aes(x = audit, y = non_na_percentage, fill = audit)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "x") +
  geom_hline(yintercept = seq(20, 100, by = 20), color = "gray60", linetype = "dashed") +
  geom_text(data = grid_labels, aes(x = 1.5, y = y, label = label), inherit.aes = FALSE, 
            hjust = 1, size = 3, color = "gray30", angle = 0) + 
  scale_fill_manual(values = rainbow_colors) + 
  labs(
    title = "Percentage of Complete Moving Average Values at\nUniversity Hospitals Dorset NHS Foundation Trust",
    x = NULL,
    y = NULL,  
    fill = "Cancer Audit"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, angle = 0),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  )

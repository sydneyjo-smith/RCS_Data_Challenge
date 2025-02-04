# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

file_path <- "cancerdata_combined.xlsx"

sheet_names <- excel_sheets(file_path)

data_list <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
})

names(data_list) <- sheet_names

audit_meta_info <- data_list[["audit_meta_info"]]
Indicators_trust <- data_list[["Indicators_trust"]]
Indicators_CA <- data_list[["Indicators_CA"]]
data_quality <- data_list[["data_quality"]]
metric_info <- data_list[["metric_info"]]

# Calculate percentage of non-NA values for each audit in Guy's
non_na_percent <- Indicators_trust %>%
  filter(trust_name %in% c("Guys and St Thomas NHS Foundation Trust", 
                           "Guy's and St Thomas' NHS Foundation Trust")) %>%
  group_by(audit) %>%
  summarise(
    non_na_percentage = mean(!is.na(mav)) * 100
  )

# Create grid labels for reference lines
grid_labels <- data.frame(
  y = seq(20, 100, by = 20),
  label = paste0(seq(20, 100, by = 20), "%")
)

# Color-blind friendly palette using RColorBrewer
color_blind_palette <- brewer.pal(n = max(3, length(unique(non_na_percent$audit))), name = "Set2")

# Polar area graph
ggplot(non_na_percent, aes(x = audit, y = non_na_percentage, fill = audit)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "x") +
  geom_hline(yintercept = seq(20, 100, by = 20), color = "gray60", linetype = "dashed") +
  geom_text(data = grid_labels, aes(x = 1.5, y = y, label = label), inherit.aes = FALSE, 
            hjust = 1, size = 3, color = "gray30", angle = 0) + 
  scale_fill_manual(values = color_blind_palette) + 
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
################## Data analysis and plot

library("readr")
library(dplyr)
library(ggplot2)
library(ggpattern)
library(plotly)
library(tidyr)
library(grid)

########################## Load pre-processed data #############################

indicators_trust <- read_csv("data/processed_data/indicators_trust.csv")
data_quality <- read_csv("data/processed_data/data_quality.csv")


#################################### PLOT 1  ####################################



#################################### PLOT 2  ####################################

# Load necessary libraries
library(dplyr)
library(readxl)
library(tidyr)
library(plotly)
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

# Create 'quarter_year' column in "Qn-YYYY" format
quality_combined_vals <- quality_combined_vals %>%
  mutate(quarter_year = paste0("Q", quarter(date), "-", format(date, "%Y")))

# Step 1: Create a status column based on pact vs target
quality_combined_vals <- quality_combined_vals %>%
  mutate(status = case_when(
    is.na(pact) ~ "Missing",  # Grey for missing values
    pact >= target ~ "Above Target",  # Green for above target
    pact >= (target - 0.02) ~ "Within 2%",  # Yellow for within 2% below target
    TRUE ~ "Below Target"  # Red for more than 2% below target
  ))

# Step 2: Clean metric names and prepare heatmap data
# quality_combined_vals <- quality_combined_vals %>%
#  mutate(metric_name = str_replace(metric_name, "Data completeness for ", ""))

# Now we prepare the data for the heatmap
heatmap_data <- quality_combined_vals %>%
  filter(audit == "NBOCA", trust_code == "RJ1") %>%
  group_by(metric_name, quarter_year) %>%
  summarise(status = ifelse(all(is.na(status)), "Missing", max(status, na.rm = TRUE)), .groups = "drop") %>%
  mutate(status_num = case_when(
    status == "Above Target" ~ 3,
    status == "Within 2%" ~ 2,
    status == "Below Target" ~ 1,
    status == "Missing" ~ 0
  )) %>%
  select(-status) %>%
  spread(key = quarter_year, value = status_num) %>%
  column_to_rownames(var = "metric_name")

# Step 3: Ensure quarters are ordered chronologically
sorted_quarters <- colnames(heatmap_data) %>%
  str_extract("\\d{4}$") %>%
  paste0("-", str_extract(colnames(heatmap_data), "Q\\d")) %>%
  order()  # Get the correct order

# Reorder the columns in heatmap_data
heatmap_data <- heatmap_data[, order(match(colnames(heatmap_data), colnames(heatmap_data)[sorted_quarters]))]

# Step 4: Define color mapping
color_mapping <- c("0" = "#999999", "1" = "#F8766D", "2" = "#FFD700", "3" = "#00BA38")

# Step 5: Create heatmap with plotly

# Convert heatmap_data to a matrix
heatmap_matrix <- as.matrix(heatmap_data)

# Create a matrix of status labels corresponding to z values
status_labels <- matrix(
  c("Missing", "Below Target", "Within 2% of Meeting Target", "Met Target")[heatmap_matrix + 1], # +1 for R's 1-based indexing
  nrow = nrow(heatmap_matrix),
  ncol = ncol(heatmap_matrix)
)

# Define a discrete colorscale with four equal blocks
colorscale <- list(
  list(0.0, "#999999"),    # Grey: 0.0 to 0.25 (z = 0)
  list(0.25, "#999999"),
  list(0.25, "#F8766D"),   # Red: 0.25 to 0.5 (z = 1)
  list(0.5, "#F8766D"),
  list(0.5, "#FFD700"),    # Yellow: 0.5 to 0.75 (z = 2)
  list(0.75, "#FFD700"),
  list(0.75, "#00BA38"),   # Green: 0.75 to 1.0 (z = 3)
  list(1.0, "#00BA38")
)

# Generate shapes for cell borders
n_cols <- ncol(heatmap_matrix)
n_rows <- nrow(heatmap_matrix)
shapes <- list()
for (j in 1:n_cols) {
  for (i in 1:n_rows) {
    shapes[[length(shapes) + 1]] <- list(
      type = "rect",
      x0 = j - 1.5,   # Left edge of cell (0-based index - 0.5)
      x1 = j - 0.5,   # Right edge of cell (0-based index + 0.5)
      y0 = i - 1.5,   # Bottom edge of cell (0-based index - 0.5)
      y1 = i - 0.5,   # Top edge of cell (0-based index + 0.5)
      line = list(color = "black", width = 0.5),
      fillcolor = "transparent",
      xref = "x",
      yref = "y"
    )
  }
}

plotly_heatmap <- plot_ly(
  x = colnames(heatmap_matrix),  # X-axis: Quarter-Year
  y = rownames(heatmap_matrix),  # Y-axis: Metric Name
  z = heatmap_matrix,            # Z-axis: Status values (0-3)
  text = status_labels,          # Custom text for hover
  type = "heatmap",
  colorscale = colorscale,       # Use the predefined discrete colorscale
  zmin = 0,                      # Minimum z-value (0)
  zmax = 3,                      # Maximum z-value (3)
  colorbar = list(
    tickvals = c(0.125, 0.375, 0.625, 0.875),  # Midpoints of each color block
    ticktext = c("Missing", "Below Target", "Within 2%", "Met Target"),
    lenmode = "fraction",
    len = 0.75,
    thickness = 20,
    ticklen = 0
  ),
  hovertemplate = paste(
    "<b>Quarter-Year:</b> %{x}<br>",
    "<b>Data Quality Indicator:</b> %{y}<br>",
    "<b>Status:</b> %{text}<br>",  
    "<extra></extra>"              
  )
) %>%
  layout(
    title = "Data Quality Indicators Over Time",
    xaxis = list(
      title = "Quarter-Year", 
      tickangle = -45,           
      showgrid = FALSE,          # Disable default grid lines
      type = "category"          # Treat x-axis as categorical
    ),
    yaxis = list(
      title = "Metric Name", 
      autorange = "reversed",    # Reverse y-axis
      showgrid = FALSE,          # Disable default grid lines
      type = "category"          # Treat y-axis as categorical
    ),
    plot_bgcolor = "white",      
    paper_bgcolor = "white",     
    shapes = shapes              # Add cell borders
  )

# Display the plotly heatmap
plotly_heatmap

#################################### PLOT 3  ####################################

############ Data preparation ############

plot_2_data <- indicators_trust

unique_quarters <- plot_2_data %>%
  group_by(date, quarter_year) %>%
  summarise(date = first(date)) %>%
  ungroup

quarter_levels <- unique_quarters$quarter_year

plot_2_data$quarter_year <- factor(plot_2_data$quarter_year,
                            levels = quarter_levels,
                            ordered = TRUE)


# Reshape the data to long format
plot_2_data_long <- plot_2_data %>%
  pivot_longer(cols = c(mav, mav_ca, mav_nat),
               names_to = "category",
               values_to = "percentage")

plot_2_data_long <- plot_2_data_long %>%
  mutate(
    den_label = if_else(is.na(denominator),"Suppressed", as.character(denominator)),
    text = paste(
      "<b>",if_else(category == "mav", "Trust moving average", if_else(category == "mav_ca", "Cancer alliance moving average", "National moving average")), "</b>",
      "\nQuarter year: ", quarter_year,
      "\nMoving average: ", round(percentage*100, 2), "%",
      if_else(category=="mav", paste("\nCase volume: ", den_label), ""), sep="")
  )

# Convert to factor with custom labels
plot_2_data_long <- plot_2_data_long %>%
  mutate(category = factor(category,
                           levels = c("mav", "mav_ca", "mav_nat"),
                           labels = c("Trust moving average",
                                      "Cancer alliance moving average",
                                      "National moving average")))

############ Save prepared data ############

write.csv(plot_2_data_long, "data/processed_data/data_plot_3.csv", row.names = FALSE,na = "")


############ Plotting ############

plot_2_data_filtered <- plot_2_data_long %>%
  filter(audit == 'NNHLA' &
           trust_code == 'RJ1')

# Line plot with ggplot2
performance_plot <- ggplot(plot_2_data_filtered, aes(x = quarter_year, y = percentage, color = category, group = category, text=text)) +
  geom_line(data = filter(plot_2_data_filtered, category == "Trust moving average"), size = 1, ) +  # Lines for ca_mav and na_mav
  geom_line(data = filter(plot_2_data_filtered, category != "Trust moving average"), size = 1, linetype="dotted") +  # Lines for ca_mav and na_mav
  geom_point(data = filter(plot_2_data_filtered, category == "Trust moving average"),
             aes(size = denominator),
             alpha = 0.8) +  # Points for mav, size based on denominator
  scale_size_continuous(range = c(5, 10)) +  # Adjusts point size scale
  geom_point(data = filter(plot_2_data_filtered, category == "Trust moving average" & is.na(denominator)),
             shape = 21,  # Shape 21 allows color & fill customization
             fill = "white",
             color = "red",
             stroke = 1,  # Controls border thickness
             size = 4,
             alpha = 1) +
  theme_minimal() +
  labs(title = "",
       x = "Quarter year",
       y = "Percentage",
       color = "Category") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Format as percentage
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for readability
  facet_wrap(~ metric_name, scales = "free_y",labeller = label_wrap_gen(55))  + # Creates a plot for each metric_name
  theme(panel.spacing = unit(2, "lines"),legend.position="bottom")

# Convert to an interactive plotly plot
interactive_line_plot <- ggplotly(performance_plot, tooltip = "text")
interactive_line_plot <- interactive_line_plot %>%
  layout(legend = list(orientation = 'h'))
interactive_line_plot

#################################### PLOT 4  ####################################

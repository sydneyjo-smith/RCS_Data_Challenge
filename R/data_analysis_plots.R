################## Data analysis and plot

library("readr")
library(dplyr)
library(ggplot2)
library(ggpattern)
library(plotly)
library(tidyr)
library(grid)
library(reactable)
library(tidyverse)

########################## Load pre-processed data #############################

indicators_trust <- read_csv("data/processed_data/indicators_trust.csv")
data_quality <- read_csv("data/processed_data/data_quality.csv")


#################################### PLOT 1  ####################################
plot_1_data <- indicators_trust

unique_quarters <- plot_1_data %>%
  group_by(date, quarter_year) %>%
  summarise(date = first(date)) %>%
  ungroup

quarter_levels <- unique_quarters$quarter_year

plot_1_data$quarter_year <- factor(plot_1_data$quarter_year,
                                   levels = quarter_levels,
                                   ordered = TRUE)

plot_1_data_previous <- plot_1_data %>%
  arrange(audit, metric_name, quarter_year)%>%
  mutate(
    # Split quarter_year into quarter and year
    quarter = substr(quarter_year, 1, 2),
    year = as.integer(substr(quarter_year, 4, 7)),
    # Calculate the previous quarter and year
    previous_quarter_year = case_when(
      quarter == "Q1" ~ paste0("Q4-", year - 1), # For Q1, link to Q4 of the previous year
      TRUE ~ paste0("Q", as.integer(substr(quarter, 2, 2)) - 1, "-", year) # For Q2-Q4
    )
  ) %>%
  # Join the previous quarter's value
  left_join(Data_Guys, select(audit, quarter_year, metric_name, pact, mav, mav_nat, mav_ca),
            by = c("audit" = "audit", "metric_name"="metric_name", "previous_quarter_year" = "quarter_year"), suffix = c("", "_prev")) %>%
  select(audit, audit_name_full, trust_code, date, quarter_year, metric_name, metric_type, denominator, mav, mav_ca, mav_nat, previous_quarter_year, denominator_prev,
         mav_prev, mav_nat_prev, mav_ca_prev)


plot_1_data_previous<- plot_1_data_previous %>% mutate(mav=mav*100)%>%
  mutate(mav_ca=mav_ca*100)%>%
  mutate(mav_nat=mav_nat*100)%>%
  mutate(mav_prev=mav_prev*100)%>%
  mutate(mav_ca=mav_ca_prev*100)%>%
  mutate(mav_nat=mav_nat_prev*100)

# generating performance difference variables.
plot_1_data_previous<- plot_1_data_previous%>% mutate(variance=mav-mav_nat)%>%
  mutate(variance_prev_quarter=mav-mav_prev)

# selecting columns for tables
plot_1_data_previous<-plot_1_data_previous%>% select(audit, audit_name_full, trust_code, metric_name, metric_type, denominator, mav,variance, variance_prev_quarter)

#########################save prepared data for plot1 ###############################

write.csv(plot_1_data_previous, "data/processed_data/plot_1_data_previous.csv", row.names = FALSE, na = "")

################plotting plot 1 #########################################################
plot_1_data_filtered <- plot_1_data_previous %>%
  filter(audit == 'NNHLA' &
           trust_code == 'RJ1')


#code to allocate header rows by RowType
header_rows <- plot_1_data_filtered %>%
  distinct(audit_name_full) %>%
  mutate(
    audit_name_full=audit_name_full,
    metric_name = NA,
    metric_type=NA,
    mav=NA,
    denominator=NA,
    variance=NA,
    variance_prev_quarter=NA,
    RowType = "Header"  # Column to differentiate header rows
  )


# Add header rows to the dataset- generate the rest of variables as RowType=Data
df_with_titles <- plot_1_data_filtered %>%
  mutate(RowType = "Data") %>%
  bind_rows(header_rows) %>%
  mutate(RowType = factor(RowType, levels = c("Header", "Data")))%>%
  arrange(audit_name_full, RowType)  # Ensures audit headers appear before metrics


# Display in reactable with styling

table_1<-reactable(df_with_titles,
                  filterable = TRUE,
                  searchable = TRUE,
                  columns = list(
                    audit=colDef(show=FALSE),
                    trust_code=colDef(show=FALSE),
                    audit_name_full = colDef(show=FALSE),
                    RowType = colDef(show = FALSE),
                    metric_name = colDef(
                      name = "Metric Name",
                      cell = function(value, index) {
                        if (df_with_titles$RowType[index] == "Header") {
                          return(df_with_titles$audit_name_full[index])
                        } else {
                          return(value)
                        }
                      }
                    ),
                    denominator = colDef(name="Case Volume",
                                         cell = function(value, index) {
                                           if (df_with_titles$RowType[index] == "Header") {
                                             return("")
                                           } else {
                                             return(sprintf("%.2f", value))
                                           }
                                         }
                    ),
                    mav = colDef(name = "Current Quarter Moving Average Performance (%)",
                                 cell = function(value, index) {
                                   if (df_with_titles$RowType[index] == "Header") {
                                     return("")
                                   } else {
                                     return(sprintf("%.2f", value))
                                   }
                                 }
                    ),
                    variance = colDef(name="Comparison to National Performance (%)",
                                      cell = function(value, index) {
                                        if (df_with_titles$RowType[index] == "Header") {
                                          return("")  # Empty cell for header rows
                                        } else {
                                          metric_type <- df_with_titles$metric_type[index]
                                          color <- get_color_2(value, metric_type)  # Apply color gradient function
                                          return(div(
                                            style = paste("background-color:", color, "; padding: 8px; border-radius: 4px;"),
                                            round(value, 2)  # Round for better readability
                                          ))
                                        }
                                      }
                    ),
                    variance_prev_quarter = colDef( name="Comparison to Previous Quarter Performance (%)",
                                                    cell = function(value, index) {
                                                      if (df_with_titles$RowType[index] == "Header") {
                                                        return("")  # Empty cell for header rows
                                                      } else {
                                                        metric_type <- df_with_titles$metric_type[index]
                                                        color <- get_color_2(value, metric_type)  # Apply color gradient function
                                                        return(div(
                                                          style = paste("background-color:", color, "; padding: 8px; border-radius: 4px;"),
                                                          round(value, 2)  # Round for better readability
                                                        ))
                                                      }
                                                    }
                    )
                  ),
                  rowStyle = function(index) {
                    if (df_with_titles$RowType[index] == "Header") {
                      return(list(
                        background = "blue",
                        color = "white",
                        fontWeight = "bold"
                      ))  # Entire row is blue with white bold text
                    } else {
                      return(NULL)  # Default row style
                    }
                  }
)

table_1


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

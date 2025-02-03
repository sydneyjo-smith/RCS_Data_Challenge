################## Data analysis and plot

library(readr)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(plotly)
library(tidyr)
library(grid)
library(reactable)
library(tidyverse)
library(lubridate)
library(tibble)
library(stringr)

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
  select(audit, audit_name_full, trust_name, trust_code, date, quarter_year, metric_name, metric_type, denominator, mav, mav_ca, mav_nat, previous_quarter_year, denominator_prev,
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
plot_1_data_previous<-plot_1_data_previous%>% select(audit, audit_name_full, trust_name, quarter_year, trust_code, metric_name, metric_type, denominator, mav,variance, variance_prev_quarter)


#code to allocate header rows by RowType
header_rows <- plot_1_data_previous %>%
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
df_with_titles <- plot_1_data_previous %>%
  mutate(RowType = "Data") %>%
  bind_rows(header_rows) %>%
  mutate(RowType = factor(RowType, levels = c("Header", "Data")))%>%
  arrange(audit_name_full, RowType)  # Ensures audit headers appear before metrics

#########################save prepared data for plot1 ###############################

write.csv(df_with_titles, "data/processed_data/plot1.csv", row.names = FALSE, na = "")

################plotting plot 1 #########################################################
plot_1_data_filtered <- df_with_titles %>%
  filter(audit == 'NNHLA' &
           trust_code == 'RJ1')

# Display in reactable with styling

table_1<-reactable(plot_1_data_filtered,
                  filterable = TRUE,
                  searchable = TRUE,
                  columns = list(
                    audit=colDef(show=FALSE),
                    trust_code=colDef(show=FALSE),
                    trust_name=colDef(show=FALSE),
                    quarter_year=colDef(show=FALSE),
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

############ Data preparation ############

plot_2_data <- data_quality

# Step 1: Create a status column based on pact vs target
plot_2_data <- plot_2_data %>%
  mutate(status = case_when(
    is.na(pact) ~ "Missing",  # Grey for missing values
    pact >= target ~ "Met Target",  # Green for above target
    pact >= (target - 0.02) ~ "Within 2%",  # Yellow for within 2% below target
    TRUE ~ "Below Target"  # Red for more than 2% below target
  ))

# Step 2: Clean metric names and prepare heatmap data
plot_2_data <- plot_2_data %>%
  mutate(metric_name = str_remove(metric_name, "^Data completeness for ") %>%
           str_replace("^([a-zA-Z])", ~ toupper(.x)))


heatmap_data <- plot_2_data %>%
  group_by(audit_name_full, trust_name, metric_name, quarter_year) %>%
  summarise(status = ifelse(all(is.na(status)), "Missing", max(status, na.rm = TRUE)), .groups = "drop") %>%
  mutate(status_num = case_when(
    status == "Above Target" ~ 3,
    status == "Within 2%" ~ 2,
    status == "Below Target" ~ 1,
    status == "Missing" ~ 0
  )) %>%
  select(-status) %>%
  pivot_wider(
    names_from = quarter_year,   # Convert quarter_year values into column names
    values_from = status_num     # Fill new columns with status_num
  )

heatmap_data <- plot_2_data %>%
  group_by(audit_name_full, trust_name, metric_name, quarter_year) %>%
  summarise(status = ifelse(all(is.na(status)), "Missing", max(status, na.rm = TRUE)), .groups = "drop") %>%
  mutate(status_num = case_when(
    status == "Above Target" ~ 3,
    status == "Within 2%" ~ 2,
    status == "Below Target" ~ 1,
    status == "Missing" ~ 0
  )) %>%
  select(-status)

heatmap_data <- plot_2_data %>%
  group_by(audit_name_full, trust_name, metric_name, quarter_year) %>%
  summarise(status = first(status)) %>%
  mutate(status_num = case_when(
    status == "Met Target" ~ 3,
    status == "Within 2%" ~ 2,
    status == "Below Target" ~ 1,
    status == "Missing" ~ 0
  )) %>%
  select(-status)


############ Save prepared data ############
write.csv(heatmap_data, "data/processed_data/data_plot_2.csv", row.names = FALSE,na = "")

############ Plotting ############

# Define color mapping for ggplot
color_mapping <- c("0" = "#999999", "1" = "#F8766D", "2" = "#FFD700", "3" = "#00BA38")

heatmap_data_filtered <- heatmap_data %>%
  filter(trust_name == "Guy's and St Thomas' NHS Foundation Trust" & audit_name_full == "National Bowel Cancer Audit")

# Create ggplot heatmap
gg_heatmap <- ggplot(heatmap_data_filtered, aes(x = quarter_year, y = metric_name, fill = as.factor(status_num))) +
  geom_tile(color = "black", size = 0.5) +  # Black gridlines for cell borders
  scale_fill_manual(values = color_mapping, name = "Status",
                    labels = c("Missing", "Below Target", "Within 2%", "Met Target")) +
  labs(title = "Data Quality Indicators Over Time", x = "Quarter-Year", y = "Metric Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Convert to interactive plotly heatmap
plotly_heatmap <- ggplotly(gg_heatmap, tooltip = "fill")

# Display interactive heatmap
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

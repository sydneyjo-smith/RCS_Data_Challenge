################## Data analysis and plot  #####################################

#' This script prepares the data for the three main plots used in the dashboard.
#' There is also commented out code for each plot. This code is run in the
#' dashboard itself, but is included here for testing/troubleshooting
#'
#' Input:
#' - Processed indicator data
#' - Processed data quality data
#' Output:
#' - Data for plot 1 - quarterly summary table
#' - Data for plot 2 - data quality heatmap
#' - Data for plot 3 - indicator performance line plot
#'
################################################################################

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
### quarterly summary table

############ Data preparation ############

plot_1_data <- indicators_trust

unique_quarters <- plot_1_data %>%
  group_by(date, quarter_year) %>%
  summarise(date = first(date)) %>%
  ungroup

quarter_levels <- unique_quarters$quarter_year

plot_1_data$quarter_year <- factor(plot_1_data$quarter_year,
                                   levels = quarter_levels,
                                   ordered = TRUE)


plot_1_data_unique <- plot_1_data %>%
  group_by(audit, trust_code, quarter_year, metric_name) %>%
  slice_head(n = 1) %>%  # Selects the first record in each group
  ungroup()  # Remove grouping after selection

plot_1_data_unique <- plot_1_data_unique %>%
  mutate(quarter_order = match(quarter_year, quarter_levels))


plot_1_data_unique_prev <- plot_1_data_unique %>%
  arrange(audit, trust_code, metric_name, quarter_year, quarter_order) %>%  # Ensure correct grouping & ordering
  group_by(audit, trust_code, metric_name) %>%  # Group by specified columns
  mutate(mav_prev = lag(mav, n = 1)) %>%  # Get previous quarter's mav within each group
  ungroup()  # Ungroup to avoid unintended side effects

plot_1_data_final <- plot_1_data_unique_prev %>%
  mutate(variance_nat = mav-mav_nat,
         variance_ca = mav-mav_ca,
         variance_prev = mav-mav_prev)

plot_1_data_final <- plot_1_data_final %>%
  mutate(variance_nat = ifelse(metric_type == 0,variance_nat*-1,variance_nat),
         variance_ca = ifelse(metric_type == 0,variance_ca*-1,variance_ca),
         variance_prev = ifelse(metric_type == 0,variance_prev*-1,variance_prev))

#code to allocate header rows by RowType
header_rows <- plot_1_data_final %>%
  distinct(audit_name_full, quarter_year, trust_name) %>%
  mutate(
    audit_name_full=audit_name_full,
    trust_name = trust_name,
    quarter_year = quarter_year,
    metric_name = audit_name_full,
    metric_type=NA,
    mav=NA,
    denominator=NA,
    variance_nat=NA,
    variance_ca=NA,
    variance_prev=NA,
    RowType = "Header"  # Column to differentiate header rows
  )


# Add header rows to the dataset- generate the rest of variables as RowType=Data
plot_1_data_final_titles <- plot_1_data_final %>%
  mutate(RowType = "Data") %>%
  bind_rows(header_rows) %>%
  mutate(RowType = factor(RowType, levels = c("Header", "Data")))%>%
  arrange(audit_name_full, RowType)  # Ensures audit headers appear before metrics

plot_1_data_final_titles <- plot_1_data_final_titles %>%
  select(audit, audit_name_full, trust_name, quarter_year, trust_code, metric_name, metric_type, denominator, mav, variance_prev, variance_ca, variance_nat, RowType)

################ save prepared data for plot 1 ######

write.csv(plot_1_data_final_titles, "data/processed_data/data_plot_1.csv", row.names = FALSE, na = "")

################ plotting plot 1 #############

# Commented out as plots are run through dashboard

# plot_1_data_filtered <- plot_1_data_final_titles %>%
#   filter(trust_code == 'RCF' &
#            quarter_year == 'Q3-2021')
#
# # Display in reactable with styling
# orange_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ff9500"))(x), maxColorValue = 255)
# positive_pal <- function(x) rgb(colorRamp(c("red","#ffffff", "green"))(x), maxColorValue = 255)
# negative_pal <- function(x) rgb(colorRamp(c("green","#ffffff", "red"))(x), maxColorValue = 255)
# blue_pal <- function(x) rgb(colorRamp(c("green","#ffffff", "red"))(x), maxColorValue = 255)
#
#
# table_1<-reactable(plot_1_data_filtered,
#                    defaultPageSize = 50,
#                    filterable = TRUE,
#                    searchable = TRUE,
#                    columns = list(
#                      audit=colDef(show=FALSE),
#                      trust_code=colDef(show=FALSE),
#                      trust_name=colDef(show=FALSE),
#                      quarter_year=colDef(show=FALSE),
#                      audit_name_full = colDef(show=FALSE),
#                      metric_type = colDef(show=TRUE),
#                      RowType = colDef(show=FALSE),
#                      metric_name = colDef(
#                        name = "Metric Name"),
#                      denominator = colDef(name="Case Volume"),
#                      mav = colDef(name = "Current Quarter Moving Average Performance (%)",
#                                   format = colFormat(percent = TRUE,digits = 0),
#                                   style = function(value, index) {
#                                     row_type <- plot_1_data_filtered[index, "RowType"]  # Get RowType
#
#                                     if (is.na(value)) {
#                                       if (row_type == "Header") {
#                                         return(list(background = "blue", color = "white"))  # Blue background for Header NA
#                                       } else {
#                                         return(list(background = "white", color = "black"))  # White background for non-Header NA
#                                       }
#                                     }
#
#                                     # Normal styling for non-NA values
#                                     normalized <- (value - 0) / (1 - 0)  # Adjust min/max as needed
#                                     normalized <- max(0, min(1, normalized))  # Clamp values
#                                     color <- orange_pal(normalized)
#                                     list(background = color)
#                                   }),
#                      variance_nat = colDef(name="Comparison to National Performance (%)",
#                                        format = colFormat(percent = TRUE,digits = 0),
#                                        style = function(value, index) {
#                                          row_type <- plot_1_data_filtered[index, "RowType"]  # Get RowType
#
#                                          if (is.na(value)) {
#                                            if (row_type == "Header") {
#                                              return(list(background = "blue", color = "white"))  # Blue background for Header NA
#                                            } else {
#                                              return(list(background = "white", color = "black"))  # White background for non-Header NA
#                                            }
#                                          }
#
#                                          # Normal styling for non-NA values
#                                          normalized <- (value - (-1)) / (1 - (-1))  # Adjust min/max as needed
#                                          normalized <- max(0, min(1, normalized))  # Clamp values
#                                          if (metric_type == 0){
#                                            color <- negative_pal(normalized)
#                                          }
#                                          else{
#                                            color <- positive_pal(normalized)
#                                          }
#                                          list(background = color)
#                                        }
#                      ),
#                      variance_prev = colDef( name="Comparison to Previous Quarter Performance (%)",
#                                                      format = colFormat(percent = TRUE,digits = 0),
#                                                      style = function(value, index) {
#                                                        row_type <- plot_1_data_filtered[index, "RowType"]  # Get RowType
#
#                                                        if (is.na(value)) {
#                                                          if (row_type == "Header") {
#                                                            return(list(background = "blue", color = "white"))  # Blue background for Header NA
#                                                          } else {
#                                                            return(list(background = "white", color = "black"))  # White background for non-Header NA
#                                                          }
#                                                        }
#
#                                                        # Normal styling for non-NA values
#                                                        normalized <- (value - (-1)) / (1 - (-1))  # Adjust min/max as needed
#                                                        normalized <- max(0, min(1, normalized))  # Clamp values
#                                                        color <- blue_pal(normalized)
#                                                        list(background = color)
#                                                      }
#                      )
#                    ),
#                    rowStyle = function(index) {
#                      if (plot_1_data_filtered$RowType[index] == "Header") {
#                        return(list(
#                          background = "blue",
#                          color = "white",
#                          fontWeight = "bold"
#                        ))  # Entire row is blue with white bold text
#                      } else {
#                        return(NULL)  # Default row style
#                      }
#                    }
# )
#
# table_1


#################################### PLOT 2  ####################################
### data quality heatmap

############ Data preparation ############

plot_2_data <- data_quality

# Step 1: Create a status column based on pact vs target
plot_2_data <- plot_2_data %>%
  mutate(status = case_when(
    is.na(pact) ~ "Missing",  # Grey for missing values
    pact >= target ~ "Met Target",  # Green for above target
    pact >= (target - 0.02) ~ "Within 2% of Target",  # Yellow for within 2% below target
    TRUE ~ "Below Target"  # Red for more than 2% below target
  ))

# Step 2: Clean metric names and prepare heatmap data
plot_2_data <- plot_2_data %>%
  mutate(metric_name = str_remove(metric_name, "^Data completeness for ") %>%
           str_replace("^([a-zA-Z])", ~ toupper(.x)))

heatmap_data <- plot_2_data %>%
  group_by(audit_name_full, trust_name, metric_name, quarter_year) %>%
  summarise(status = first(status),
            pact= first(pact))

# Add text column used by plotly
heatmap_data <- heatmap_data %>%
  mutate(
    text = paste(
      "Quarter year: ", quarter_year,
      "\nStatus: ", status,
      "\nPercentage: ", round(pact*100,2), "%", sep="")
  )

unique_quarters <- plot_2_data %>%
  group_by(date, quarter_year) %>%
  summarise(date = first(date)) %>%
  ungroup %>%
  arrange(date)

quarter_levels <- unique_quarters$quarter_year

heatmap_data$quarter_year <- factor(heatmap_data$quarter_year, levels = quarter_levels)


############ Save prepared data ############

write.csv(heatmap_data, "data/processed_data/data_plot_2.csv", row.names = FALSE,na = "")

############ Plotting ############

# Commenting out as plots are run through dashboard


# # Define color mapping for ggplot
# color_mapping <- c("Missing" = "#999999", "Below Target" = "#F8766D", "Within 2%" = "#FFD700", "Met Target" = "#00BA38")
#
# heatmap_data_filtered <- heatmap_data %>%
#   filter(trust_name == "Guy's and St Thomas' NHS Foundation Trust" & audit_name_full == "National Bowel Cancer Audit")
#
# # Create ggplot heatmap
# gg_heatmap <- ggplot(heatmap_data_filtered, aes(x = quarter_year, y = metric_name, fill = as.factor(status))) +
#   geom_tile(color = "azure2", size = 0.5) +  # Black gridlines for cell borders
#   scale_fill_manual(values = color_mapping, name = "Status",
#                     labels = c("Missing", "Below Target", "Within 2%", "Met Target")) +
#   labs(title = "Data Quality Indicators Over Time", x = "Quarter-Year", y = "Metric Name") +
#   scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "bottom")
#
# # Convert to interactive plotly heatmap
# plotly_heatmap <- ggplotly(gg_heatmap, tooltip = "text")
#
# # Display interactive heatmap
# plotly_heatmap

#################################### PLOT 3  ####################################
### indicator performance line plot

############ Data preparation ############

plot_3_data <- indicators_trust

unique_quarters <- plot_3_data %>%
  group_by(date, quarter_year) %>%
  summarise(date = first(date)) %>%
  ungroup

quarter_levels <- unique_quarters$quarter_year

plot_3_data$quarter_year <- factor(plot_3_data$quarter_year,
                            levels = quarter_levels,
                            ordered = TRUE)




# Use Wilson Score Confidence Interval due to small denominators

# Function to calculate Wilson Score CI
wilson_ci <- function(successes, n, conf_level = 0.95) {
  if (n == 0 || is.na(n) || is.na(successes)) return(c(NA, NA))  # Handle cases where denominator is 0 or NA
  z <- qnorm(1 - (1 - conf_level) / 2)
  p_hat <- successes / n
  denominator <- 1 + (z^2 / n)
  center <- p_hat + (z^2 / (2 * n))
  margin <- z * sqrt((p_hat * (1 - p_hat) / n) + (z^2 / (4 * n^2)))
  lower <- (center - margin) / denominator
  upper <- (center + margin) / denominator
  return(c(lower, upper))
}

# Add lower and upper CI to the entire indicators_trust dataframe
plot_3_data_ci <- plot_3_data %>%
  arrange(date) %>%
  mutate(
    # quarter_year = paste0("Q", quarter(date), "-", year(date)),
    # quarter_year = factor(quarter_year, levels = unique(quarter_year), ordered = TRUE),
    # Calculate CI lower and upper bounds for all rows
    CI_lower = mapply(function(x, y) wilson_ci(y * x, x)[1], denominator, pact),
    CI_upper = mapply(function(x, y) wilson_ci(y * x, x)[2], denominator, pact)
  )


# Reshape the data to long format
plot_3_data_long <- plot_3_data_ci %>%
  pivot_longer(cols = c(mav, mav_ca, mav_nat),
               names_to = "category",
               values_to = "percentage")

# Add text column used by plotly
plot_3_data_long <- plot_3_data_long %>%
  mutate(
    den_label = if_else(is.na(denominator),"Suppressed", as.character(denominator)),
    per_label = if_else(is.na(pact),"Suppressed",
                        paste(as.character(round(pact*100, 2)),
                              "%",
                              " (CI ",
                              as.character(round(CI_lower*100, 0)),
                              ", ",
                              as.character(round(CI_upper*100, 0)),
                              ")",sep="")),
    text = paste(
      "<b>",if_else(category == "mav", "Trust moving average", if_else(category == "mav_ca", "Cancer alliance moving average", "National moving average")), "</b>",
      "\nQuarter year: ", quarter_year,
      "\nMoving average: ", round(percentage*100, 2), "%",
      if_else(category=="mav", paste("\nCase volume: ", den_label, "\nPercentage: ", per_label), ""), sep="")
  )

# Convert to factor with custom labels
plot_3_data_long <- plot_3_data_long %>%
  mutate(category = factor(category,
                           levels = c("mav", "mav_ca", "mav_nat"),
                           labels = c("Trust moving average",
                                      "Cancer alliance moving average",
                                      "National moving average")))
plot_3_data_long$quarter_year <- factor(plot_3_data_long$quarter_year, levels = quarter_levels)


############ Save prepared data ############

write.csv(plot_3_data_long, "data/processed_data/data_plot_3.csv", row.names = FALSE,na = "")

############ Plotting ############

# Commenting out as plots are run through dashboard

# plot_2_data_filtered <- plot_2_data_long %>%
#   filter(audit == 'NNHLA' &
#            trust_code == 'RJ1')
#
# plot_2_data_filtered$quarter_year <- factor(plot_2_data_filtered$quarter_year, levels = quarter_levels)
#
# # Line plot with ggplot2
# performance_plot <- ggplot(plot_2_data_filtered, aes(x = quarter_year, y = percentage, color = category, group = category, text=text)) +
#   geom_line(data = filter(plot_2_data_filtered, category == "Trust moving average"), size = 1, ) +  # Lines for ca_mav and na_mav
#   geom_line(data = filter(plot_2_data_filtered, category != "Trust moving average"), size = 1, linetype="dotted") +  # Lines for ca_mav and na_mav
#   geom_point(data = filter(plot_2_data_filtered, category == "Trust moving average"),
#              aes(size = denominator),
#              alpha = 0.8) +  # Points for mav, size based on denominator
#   scale_size_continuous(range = c(5, 10)) +  # Adjusts point size scale
#   geom_point(data = filter(plot_2_data_filtered, category == "Trust moving average" & is.na(denominator)),
#              shape = 21,  # Shape 21 allows color & fill customization
#              fill = "white",
#              color = "red",
#              stroke = 1,  # Controls border thickness
#              size = 4,
#              alpha = 1) +
#   theme_minimal() +
#   labs(title = "",
#        x = "Quarter year",
#        y = "Percentage",
#        color = "Category") +
#   scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Format as percentage
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for readability
#   facet_wrap(~ metric_name, scales = "free_y",labeller = label_wrap_gen(55))  + # Creates a plot for each metric_name
#   theme(panel.spacing = unit(2, "lines"),legend.position="bottom")
#
# # Convert to an interactive plotly plot
# interactive_line_plot <- ggplotly(performance_plot, tooltip = "text")
# interactive_line_plot <- interactive_line_plot %>%
#   layout(legend = list(orientation = 'h'))
# interactive_line_plot

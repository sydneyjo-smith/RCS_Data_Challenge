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

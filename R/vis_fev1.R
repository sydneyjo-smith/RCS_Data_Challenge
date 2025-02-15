#       _/_/    _/  _/      _/_/      _/   
#    _/    _/  _/  _/    _/    _/  _/_/    
#       _/    _/_/_/_/    _/_/_/    _/     
#    _/          _/          _/    _/      
# _/_/_/_/      _/    _/_/_/    _/_/_/     

# Exploratory Data Analysis of FEV1 data

install.packages("GGally")

library(tidyverse)
library(GGally)

# read the data in

fev1 <- read_csv("data/fev1.csv", col_types = list('id' = 'f'))

# sample the data so that we have 20 patients with more than 6 observations

fev1_sampled <- fev1 %>% 
    count(id) %>%
    filter(n > 6) %>%
    slice_sample(n = 20) %>%
    select(id) %>%
    inner_join(fev1)

fev1_sampled

# Activity 5 - A simple scatter plot

# Calculate the correlation between age and FEV1
# (yes, this isn't strictly correct because there's repeated measures)



# Build a plot that shows the relationship between FEV1 and age

fev1_plot <- ggplot(data = fev1_sampled, 
                    aes(x = FEV1, y = age)) +
    geom_point()

fev1_plot

# Activity 6 - Improving the plot

# Add meaningful labels for the $x$ and $y$ axes, including units, and change the plot's colour theme from the default.

# Add a smooth line of best fit to the plot. 

fev1_plot <- ggplot(data = fev1_sampled, 
                    aes(x = FEV1, y = age)) +
  geom_point(color = "blue") +                      
  labs(x = "FEV1 (L)",                             
       y = "Age (years)",                          
       title = "Scatter Plot of FEV1 vs. Age") +    
  theme_minimal() +                                
  geom_smooth(method = "lm", se = FALSE, color = "red")  # Add a linear smooth line of best fit

# Display the plot
print(fev1_plot)

# Activity 7

# Activity 7a - Showing further structure

# Determine a way to highlight which observations belong to the same individual in your plot
 
# Matthew 

fev1_plot_2 <- ggplot(data = fev1_sampled, 
                      aes(x = FEV1, y = age, group = id, color = as.factor(id))) +
  geom_point(size = 2) +  # Scatter points with size for visibility
  labs(
    x = "FEV1 (L)", 
    y = "Age (years)", 
    title = "Scatter Plot of FEV1 vs. Age",
    color = "Individual ID"
  ) +
  theme_minimal() 

# Display the plot
print(fev1_plot_2)





# Activity 7b - How many observations per individual?

# Catherine

# Count the number of times that each `id` is measured and make a bar plot 

id_count <- fev1_sampled %>%
  group_by(id) %>%
  summarise(count = n())

id_plot <- ggplot(id_count, aes(x = as.factor(id), y = count)) +
  geom_bar(stat = "identity", fill = "darkmagenta") +
  labs(x = "Individual ID", y = "Number of Observations", title = "Number of observations per individual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(id_plot)


# Activity 7c - Incorporating height

# Catherine

# Make a plot that shows both FEV1 and age but also includes height

ggplot(fev1_sampled, aes(x = age, y = FEV1)) +
  geom_point(aes(color = height), size = 2.5, alpha = 0.6) +
  scale_color_gradient(low = "darkred", high = "orange") +
  labs(x = "Age (years)", y = "FEV1 (L)", title = "FEV1 vs age with Height") +
  theme_minimal()

# Activity 7d - skimr
#sydney woohoooo
# Use skimr::skim() to generate a summary table of the data.
install.packages("skimr")
??skim
skimr::skim(fev1)
# skim grouped by age group 
fev1 |>
  mutate(age_group = cut(age, breaks = seq(0, 20, by = 2))) |>
  group_by(age_group) |>
  skimr::skim()

# Heads up- You'll need to install skimr if you don't already have it

# Activity 7e - GGally MATT R

# Generate a pairs plot with GGally::ggpairs(), for all columns except id
# You'll need to install GGally if you don't already have it

ggpairs(fev1_sampled, columns= 2:4 )

# Activity 7f - Accounting for repeat measurement

# Build a regression model to look at how FEV1 varies with age, accounting for the
# structure by including a random effect mean for each id and a spline curve for
# the effect of age
# Case load per quarter per cancer for Guys 

library(dplyr)
library(readxl)

# File path (update if necessary)
file_path <- "cancerdata_combined.xlsx"

# Load data
data_quality <- read_excel(file_path, sheet = "data_quality")
indicators_trust <- read_excel(file_path, sheet = "Indicators_trust")

# Define trust name
target_trust <- c("Guy's and St Thomas' NHS Foundation Trust", "Guys and St Thomas' NHS Foundation Trust")

#OR 
# target_trust <- "Guy's and St Thomas' NHS Foundation Trust"
#see which one fits better for your raw data 

# Define audit names
audit_types <- c("NAoMe", "NAoPri", "NBOCA", "NKCA", "NNHLA", "NOCA", "NOGCA", "NPaCA")

# Define the quarters from Q1-2021 to Q4-2023
years <- 2021:2023
quarters <- c("Q1", "Q2", "Q3", "Q4")
all_quarters <- c(outer(quarters, years, paste, sep = "-")) # Creates Q1-2021 to Q4-2023

# Create an empty dataframe to store results
results <- data.frame()

# Loop through each audit type and quarter
for (audit in audit_types) {
  for (quarter in all_quarters) {
    
    # Filter data for the given trust, audit, and quarter
    filtered_data_quality <- data_quality %>%
      filter(
        trust_name == target_trust,
        audit == audit,
        quarter_year == quarter
      )
    
    filtered_indicators_trust <- indicators_trust %>%
      filter(
        trust_name == target_trust,
        audit == audit,
        quarter_year == quarter
      )
    
    # Combine both datasets
    combined_data <- bind_rows(
      filtered_data_quality %>% select(trust_name, denominator),
      filtered_indicators_trust %>% select(trust_name, denominator)
    )
    
    # Calculate max denominator for the quarter
    if (nrow(combined_data) > 0) {
      max_denominator <- max(combined_data$denominator, na.rm = TRUE)
    } else {
      max_denominator <- NA
    }
    
    # Append result
    results <- rbind(results, data.frame(
      Audit = audit,
      Quarter = quarter,
      Trust = target_trust,
      Max_Denominator = max_denominator
    ))
  }
}

# Arrange results by audit and quarter
results <- results %>%
  arrange(Audit, Quarter)

# Print results
print(results)
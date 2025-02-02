# Load necessary libraries
library(dplyr)
library(readxl)

# File path
file_path <- "cancerdata_combined.xlsx"

# Read data from the Excel file
data_quality <- read_excel(file_path, sheet = "data_quality")
indicators_trust <- read_excel(file_path, sheet = "Indicators_trust")

#-------
# checking whether denominators are the same

# Step 1: Filter data for Manchester University NHS Foundation Trust, NPaCA, Q3-2022 (or any other desired values)
filtered_data_quality <- data_quality %>%
  filter(
    trust_name == "Manchester University NHS Foundation Trust",  # Change trust name if needed
    audit == "NPaCA",
    quarter_year == "Q3-2022"  # Change quarter year if needed
  )

filtered_indicators_trust <- indicators_trust %>%
  filter(
    trust_name == "Manchester University NHS Foundation Trust",  # Change trust name if needed
    audit == "NPaCA",
    quarter_year == "Q3-2022"  # Change quarter year if needed
  )

# Step 2: Combine the data from both datasets (data_quality and indicators_trust)
combined_data <- bind_rows(
  filtered_data_quality %>%
    select(metric_name, denominator), 
  filtered_indicators_trust %>%
    select(metric_name, denominator)
)

# Step 3: Count how many times each denominator appears and identify the most common denominator
denominator_summary <- combined_data %>%
  group_by(denominator) %>%
  summarise(
    count = n(),
    metrics = paste(metric_name, collapse = ", "),  # List metric names for each denominator value
    .groups = "drop"
  )

# Step 4: Identify the most common denominator
most_common_denominator <- denominator_summary %>%
  arrange(desc(count)) %>%
  slice(1) %>%
  pull(denominator)

# Step 5: Identify any anomalies (denominators that are not the most common one)
anomalies <- combined_data %>%
  filter(denominator != most_common_denominator) %>%
  distinct(denominator, metric_name)

# Step 6: Output the summary and the anomalies
print("Denominator Summary:")
print(denominator_summary)

print("Anomalies (denominators that do not match the most common one):")
print(anomalies)

#-----
#same as above, but ask for user input for trust, date and audit

library(dplyr)

# Step 1: Ask for user input
trust_name_input <- readline(prompt = "Enter the trust name: ")
quarter_year_input <- readline(prompt = "Enter the quarter year (e.g., Q3-2022): ")
audit_input <- readline(prompt = "Enter the audit name (e.g., NPaCA): ")

# Ensure the inputs are converted to character strings (this is a safety check)
trust_name_input <- as.character(trust_name_input)
quarter_year_input <- as.character(quarter_year_input)
audit_input <- as.character(audit_input)

# Step 2: Filter data for user input values
filtered_data_quality <- data_quality %>%
  filter(
    trust_name == trust_name_input,  # Use the user input trust name
    audit == audit_input,            # Use the user input audit
    quarter_year == quarter_year_input  # Ensure quarter_year_input is treated as a string
  )

filtered_indicators_trust <- indicators_trust %>%
  filter(
    trust_name == trust_name_input,  # Use the user input trust name
    audit == audit_input,            # Use the user input audit
    quarter_year == quarter_year_input  # Ensure quarter_year_input is treated as a string
  )

# Step 3: Combine the data from both datasets (data_quality and indicators_trust)
combined_data <- bind_rows(
  filtered_data_quality %>%
    select(metric_name, denominator), 
  filtered_indicators_trust %>%
    select(metric_name, denominator)
)

# Step 4: Count how many times each denominator appears and identify the most common denominator
denominator_summary <- combined_data %>%
  group_by(denominator) %>%
  summarise(
    count = n(),
    metrics = paste(metric_name, collapse = ", "),  # List metric names for each denominator value
    .groups = "drop"
  )

# Step 5: Identify the most common denominator
most_common_denominator <- denominator_summary %>%
  arrange(desc(count)) %>%
  slice(1) %>%
  pull(denominator)

# Step 6: Identify any anomalies (denominators that are not the most common one)
anomalies <- combined_data %>%
  filter(denominator != most_common_denominator) %>%
  distinct(denominator, metric_name)

# Step 7: Output the summary and the anomalies
print("Denominator Summary:")
print(denominator_summary)

print("Anomalies (denominators that do not match the most common one):")
print(anomalies)


#-----
#categorizing mav, mav_na and mav_ca by <1 =1 and >1
library(dplyr)

# Categorize the 'mav', 'mav_ca', and 'mav_nat' values and count how many fall into each category
mav_summary <- indicators_trust %>%
  mutate(
    mav_category = case_when(
      mav > 1 ~ ">1",
      mav < 1 ~ "<1",
      TRUE ~ "Equal to 1"
    ),
    mav_ca_category = case_when(
      mav_ca > 1 ~ ">1",
      mav_ca < 1 ~ "<1",
      TRUE ~ "Equal to 1"
    ),
    mav_nat_category = case_when(
      mav_nat > 1 ~ ">1",
      mav_nat < 1 ~ "<1",
      TRUE ~ "Equal to 1"
    )
  ) %>%
  # Summarize counts for each category for mav, mav_ca, and mav_nat
  summarise(
    mav_gt_1 = sum(mav_category == ">1", na.rm = TRUE),
    mav_lt_1 = sum(mav_category == "<1", na.rm = TRUE),
    mav_eq_1 = sum(mav_category == "Equal to 1", na.rm = TRUE),
    
    mav_ca_gt_1 = sum(mav_ca_category == ">1", na.rm = TRUE),
    mav_ca_lt_1 = sum(mav_ca_category == "<1", na.rm = TRUE),
    mav_ca_eq_1 = sum(mav_ca_category == "Equal to 1", na.rm = TRUE),
    
    mav_nat_gt_1 = sum(mav_nat_category == ">1", na.rm = TRUE),
    mav_nat_lt_1 = sum(mav_nat_category == "<1", na.rm = TRUE),
    mav_nat_eq_1 = sum(mav_nat_category == "Equal to 1", na.rm = TRUE)
  )

# Reshape the summary into a more readable format
mav_summary_table <- tibble(
  metric = c("mav", "mav_ca", "mav_nat"),
  `>1` = c(mav_summary$mav_gt_1, mav_summary$mav_ca_gt_1, mav_summary$mav_nat_gt_1),
  `<1` = c(mav_summary$mav_lt_1, mav_summary$mav_ca_lt_1, mav_summary$mav_nat_lt_1),
  `=1` = c(mav_summary$mav_eq_1, mav_summary$mav_ca_eq_1, mav_summary$mav_nat_eq_1)
)

# Print the summary table
print(mav_summary_table)


#------
#checking whether a single trust would have mav of <1 and >1 across the same ca

library(dplyr)

# Step 1: Identify rows where mav > 1
mav_gt_1_records <- indicators_trust %>%
  filter(mav > 1) %>%
  select(trust_name, audit, quarter_year, mav)

# Step 2: For each row in mav_gt_1_records, check if the same trust and audit have any records with mav < 1
mav_check_results <- mav_gt_1_records %>%
  rowwise() %>%
  mutate(
    records_with_mav_lt_1 = nrow(
      indicators_trust %>%
        filter(
          trust_name == trust_name,  # Same trust
          audit == audit,            # Same cancer (audit)
          mav < 1,                   # Records with mav < 1
          quarter_year != quarter_year  # Exclude the same year-quarter as the current row
        )
    )
  ) %>%
  ungroup()

# Step 3: Summarize the results
summary_table <- mav_check_results %>%
  summarise(
    total_mav_gt_1_rows = n(),
    total_with_mav_lt_1 = sum(records_with_mav_lt_1 > 0),
    total_without_mav_lt_1 = sum(records_with_mav_lt_1 == 0)
  )

# Print the results
print("Summary of checks:")
print(summary_table)

# Optional: View detailed results for rows with mav > 1 and the counts of mav < 1
print("Detailed results for each row:")
print(mav_check_results)



#-----
#checking whether a single trust would have mav of <1 and >1 across diff ca
library(dplyr)

# Step 1: Identify rows where mav > 1
mav_gt_1_records <- indicators_trust %>%
  filter(mav > 1) %>%
  select(trust_name, audit, quarter_year, mav)

# Step 2: Cross-audit check for each row in mav_gt_1_records
mav_check_results <- mav_gt_1_records %>%
  rowwise() %>%
  mutate(
    records_with_mav_lt_1_in_other_audit = nrow(
      indicators_trust %>%
        filter(
          trust_name == trust_name,  # Same trust
          audit != audit,            # Different audit
          mav < 1                    # Records with mav < 1
        )
    )
  ) %>%
  ungroup()

# Step 3: Summarize the results
summary_table <- mav_check_results %>%
  summarise(
    total_mav_gt_1_rows = n(),
    total_with_mav_lt_1_in_other_audit = sum(records_with_mav_lt_1_in_other_audit > 0),
    total_without_mav_lt_1_in_other_audit = sum(records_with_mav_lt_1_in_other_audit == 0)
  )

# Print the results
print("Summary of cross-audit checks:")
print(summary_table)

# Optional: View detailed results for rows with mav > 1
print("Detailed results for each row:")
print(mav_check_results)


#-----
# Check for the size of the trust 

# Allow user input for the audit (cancer type), and quarter of interest

cat("Enter Audit Name (e.g., NPaCA): ")
target_audit <- readline()
cat("Enter Quarter-Year (e.g., Q3-2022): ")
target_quarter <- readline()
filtered_data_quality <- data_quality %>%
  filter(
    audit == target_audit,
    quarter_year == target_quarter
  )

filtered_indicators_trust <- indicators_trust %>%
  filter(
    audit == target_audit,
    quarter_year == target_quarter
  )

# Combine data from both sheets
combined_data <- bind_rows(
  filtered_data_quality %>% select(trust_name, denominator),
  filtered_indicators_trust %>% select(trust_name, denominator)
)

# Check if combined data has rows
if (nrow(combined_data) > 0) {
  # Group by trust and calculate the maximum denominator, then arrange by descending order
  trust_denominator_summary <- combined_data %>%
    group_by(trust_name) %>%
    summarise(
      max_denominator = max(denominator, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(max_denominator))
  
  # Output the result
  print(trust_denominator_summary)
} else {
  cat("No data available for the specified criteria.\n")
}

#--------
# Case load for each cancer for Guys and St thomas


# Define trust name
target_trust <- c("Guy's and St Thomas' NHS Foundation Trust", "Guys and St Thomas' NHS Foundation Trust")

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



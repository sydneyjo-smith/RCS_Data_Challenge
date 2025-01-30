
################### LOAD PACKAGES #############

library(dplyr)
library(readxl)

################ IMPORT DATA ##########

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


##################### CANCER AND METRIC COMPLETENESS ###################


# The below code will become a loop over all trusts/cancer alliances 
# For each trust we need to know (output as a table of some sort)
# 1) How many cancers do they have data for
# 2) Within each cancer what is the data completeness and consistency

# Finding all the unique trust names
unique_data_quality_trusts <- unique(data_quality$trust_name)

# Max number of audits
max_audits <- length(unique(data_quality$audit))

# Initialise dataframe with final info
summary_data_quality <- data.frame(
  trust_name = character(0),
  num_audits = integer(0)
)

# Add the audit columns
all_audits <- c("NAoMe", "NAoPri", "NBOCA", "NKCA", "NNHLA", "NOCA", "NOGCA", "NPaCA")
for (audit in all_audits) {
  summary_data_quality[[paste0(audit)]] <- integer()
}

for (trust in unique_data_quality_trusts) {
  
  data <- data_quality %>% filter(trust_name == trust)
  
  # Look at audits
  unique_audits <- unique(data$audit)
  num_audits <- length(unique_audits)
  
  # Initialize a named list to store metric counts
  audit_metrics <- setNames(rep(NA, paste0(audit), length(all_audits)), all_audits)
  
  # Loop through each audit and count unique metrics
  for (audit in unique_audits) {
    audit_data <- data %>% filter(audit == audit)
    audit_metrics[[audit]] <- length(unique(audit_data$metric_name))
  }
  
  # Create a new row for the trust
  new_row <- data.frame(
    trust_name = trust,  # Ensure the trust name is correct
    num_audits = num_audits,
    t(audit_metrics)  # Transpose the audit metrics to make them individual columns
  )
                        
  # Append the new row to the summary data frame
  summary_data_quality <- rbind(summary_data_quality, new_row)
  
}

######################## POSSIBLE METRICS ###################

# Step 1: Identify unique audits
unique_audits <- unique(data_quality$audit)

# Step 2: Create a list where each element contains unique metric names for each audit
audit_metrics_list <- lapply(unique_audits, function(audit) {
  unique(data_quality$metric_name[data_quality$audit == audit])
})

# Step 3: Find the maximum number of metric names for any audit (for padding)
max_len <- max(sapply(audit_metrics_list, length))

# Step 4: Pad the list with NA values so that each list element has the same number of rows
audit_metrics_list <- lapply(audit_metrics_list, function(metrics) {
  length(metrics) <- max_len  # Pad with NAs to the maximum length
  return(metrics)
})

# Step 5: Convert the padded list into a data frame
metrics_data_quality <- as.data.frame(audit_metrics_list)

# Step 6: Set the column names of the data frame to the unique audits
colnames(audit_metrics_df) <- unique_audits

######################## METRIC COMPLETENESS ###################

# For all metrics there is denominator, pact, moving average
# For a given audit and metric we want to know if the completeness and characteristics of the data

# Define the audit and metric values
audit_val <- "NAoMe"
metric_val <- "People with data recorded for having contact with a Clinical Nurse Specialist (CNS)"

# Get the unique trust names from the data
unique_data_quality_trusts <- unique(data_quality$trust_name)

# Create an empty list to store the results for each trust
result_list <- list()

# Loop over each unique trust
for (i in seq_along(unique_data_quality_trusts)) {
  trust_val <- unique_data_quality_trusts[i]  # Get the current trust name
  
  # Filter the data for the current audit, metric, and trust
  data <- data_quality %>% 
    filter(audit == audit_val & trust_name == trust_val & metric_name == metric_val)
  
  # Count the number of rows for this trust
  row_count <- nrow(data)
  
  # Count the number of NAs in the columns 'denominator', 'pact', and 'mav'
  na_denominator <- sum(is.na(data$denominator))
  na_pact <- sum(is.na(data$pact))
  na_mav <- sum(is.na(data$mav))
  
  # Append the results for the current trust to the result list
  result_list[[i]] <- data.frame(
    trust_name = trust_val,
    row_count = row_count,
    na_denominator = na_denominator,
    na_pact = na_pact,
    na_mav = na_mav
  )
}

# Combine the list of results into a single data frame
result_df <- do.call(rbind, result_list)



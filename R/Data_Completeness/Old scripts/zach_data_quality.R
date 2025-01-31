
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


##################### DATA QUALITY COMPLETENESS ###################


# The below code will become a loop over all trusts/cancer alliances 
# For each trust we need to know (output as a table of some sort)
# 1) How many cancers do they have data for
# 2) Within each cancer what is the data completeness and consistency

# Finding all the unique trust names
unique_data_quality_trusts <- unique(data_quality$trust_name)

# Max number of audits
max_audits <- length(unique(data_quality$audit))

# All audits vector
all_audits <- c("NAoMe", "NAoPri", "NBOCA", "NKCA", "NNHLA", "NOCA", "NOGCA", "NPaCA")


# Initialise dataframe with final info
summary_data_quality <- data.frame(
  trust_name = character(0),
  num_audits = integer(0),
  NAoMe = integer(0),
  NAoPri = integer(0), 
  NBOCA = integer(0),
  NKCA = integer(0),
  NNHLA = integer(0),
  NOCA = integer(0),
  NOGCA = integer(0),
  NPaCA = integer(0)
)

for (trust in unique_data_quality_trusts) {
  
  data <- data_quality %>% filter(trust_name == trust)
  
  # Look at audits
  unique_audits <- unique(data$audit)
  num_audits <- length(unique_audits)
  
  # Initialize a named list to store metric counts
  audit_metrics <- setNames(vector("list", length(all_audits)), all_audits)
  
  # Loop through each audit and count unique metrics
  for (cancer in all_audits) {
    audit_data <- data %>% filter(audit == cancer)
    num_metrics <- length(unique(audit_data$metric_name))
    audit_metrics[[cancer]] <- num_metrics
  }
  
  # Create a new row for the trust
  audit_metrics <- as.data.frame(audit_metrics)
  audit_metrics$trust_name <- trust
  audit_metrics$num_audits <- num_audits
  audit_metrics <- audit_metrics[, c("trust_name", "num_audits", colnames(audit_metrics)[1:(ncol(audit_metrics) - 2)])]
                        
  # Append the new row to the summary data frame
  summary_data_quality <- rbind(summary_data_quality, audit_metrics)
  
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
colnames(metrics_data_quality) <- unique_audits

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

######################## NUMBER TRUSTS IN AN ALLIANCE ###################

# Total number of alliances
unique_alliances <- unique(Indicators_trust$alliance_name)

# Initialize a data frame to hold all the information
alliance_trust_numbers <- data.frame(
  alliance = character(0),
  number_of_trusts = integer(0)
)

for (alliance_var in unique_alliances) {
  data <- Indicators_trust %>% filter(alliance_name == alliance_var)
  num_trusts <- length(unique(data$trust_name))
  new_row <- data.frame(alliance = alliance_var, number_of_trusts = num_trusts)
  alliance_trust_numbers <- rbind(alliance_trust_numbers, new_row)
}

alliance_trust_numbers <- alliance_trust_numbers %>%
  arrange(alliance)


######################## NUMBER OF QUARTERS REPORTED FOR EACH METRIC [CHECKING FOR EVERY TRUST] ###################

# Define the audit value
# Options are "NAoMe", "NAoPri", "NBOCA", "NKCA", "NNHLA", "NOCA", "NOGCA", "NPaCA"
audit_val <- "NOCA"

# Get the unique metrics for this audit
Indicators_trust_audit_val <- Indicators_trust %>% filter(audit == audit_val)
unique_metric <- as.vector(unique(Indicators_trust_audit_val$metric_name))

# Get the unique trust names from the data
unique_Indicators_trusts <- unique(Indicators_trust$trust_name)

# Initialize results dataframe
results <- data.frame(metric = character(), row_count = integer(), stringsAsFactors = FALSE)

for (metric_val in unique_metric) { 
  
  # Initialize a data frame for trust counts
  trust_row_counts <- data.frame(trust_name = character(), row_count = integer(), stringsAsFactors = FALSE)
  
  # Checking an individual metric 
  # metric_val <- "One-year survival (case-mix adjusted)"
  
  for (trust_val in unique_Indicators_trusts) {  # Iterate over trust names
    
    # Filter the data for the current audit, metric, and trust
    data <- Indicators_trust %>% 
      filter(trust_name == trust_val & metric_name == metric_val)
    
    # Count the number of rows for this trust
    num_quarters <- nrow(data)
    
    # Append to trust_row_counts
    trust_row_counts <- rbind(trust_row_counts, data.frame(trust_name = trust_val, row_count = num_quarters))
  }
  
  # Filter cases where row count is 12 or 0 (missing quarters)
  row_count_missing_quarter <- nrow(trust_row_counts %>% filter(row_count != 12 & row_count != 0))
  
  result_metric <- data.frame(metric = metric_val, row_count = row_count_missing_quarter)
  
  # Append to final results dataframe
  results <- rbind(results, result_metric)
}

# Print the results dataframe
print(results)

















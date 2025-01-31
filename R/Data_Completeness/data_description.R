

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

#------------------------------------------------------------- NUMBER OF TRUSTS

#######  DATA QUALITY

unique_trusts_data_quality <- as.data.frame(unique(data_quality$trust_name)) %>%
  rename(trust_name = 1) %>%
  arrange(trust_name)

nrow(unique_trusts_data_quality)

#######  PERFORMANCE INDICATOR

unique_trusts_performance_indicator <- as.data.frame(unique(Indicators_trust$trust_name)) %>%
  rename(trust_name = 1) %>%
  arrange(trust_name)

nrow(unique_trusts_performance_indicator)

#------------------------------------------------------------- POSSIBLE METRICS

#######  DATA QUALITY

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

####### PERFORMANCE INDICATOR

# Step 1: Identify unique audits
unique_audits <- unique(Indicators_trust$audit)

# Step 2: Create a list where each element contains unique metric names for each audit
audit_metrics_list <- lapply(unique_audits, function(audit) {
  unique(Indicators_trust$metric_name[Indicators_trust$audit == audit])
})

# Step 3: Find the maximum number of metric names for any audit (for padding)
max_len <- max(sapply(audit_metrics_list, length))

# Step 4: Pad the list with NA values so that each list element has the same number of rows
audit_metrics_list <- lapply(audit_metrics_list, function(metrics) {
  length(metrics) <- max_len  # Pad with NAs to the maximum length
  return(metrics)
})

# Step 5: Convert the padded list into a data frame
metrics_indicators <- as.data.frame(audit_metrics_list)

# Step 6: Set the column names of the data frame to the unique audits
colnames(metrics_indicators) <- unique_audits

#------------------------------------------------------------- NUMBER TRUSTS IN AN ALLIANCE

########## DATA QUALITY

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

########## PERFORMANCE INDICATOR

# Total number of alliances
unique_alliances <- unique(data_quality$alliance_name)

# Initialize a data frame to hold all the information
alliance_trust_numbers <- data.frame(
  alliance = character(0),
  number_of_trusts = integer(0)
)

for (alliance_var in unique_alliances) {
  data <- data_quality %>% filter(alliance_name == alliance_var)
  num_trusts <- length(unique(data$trust_name))
  new_row <- data.frame(alliance = alliance_var, number_of_trusts = num_trusts)
  alliance_trust_numbers <- rbind(alliance_trust_numbers, new_row)
}

alliance_trust_numbers <- alliance_trust_numbers %>%
  arrange(alliance)

#------------------------------------------------------------- WHICH AUDITS COVERED BY WHICH ALLIANCE

####### DATA QUALITY

# Finding all the unique trust names
unique_alliances <- unique(data_quality$alliance_name)

# Max number of audits
max_audits <- length(unique(data_quality$audit))

# All audits vector
all_audits <- c("NAoMe", "NAoPri", "NBOCA", "NKCA", "NNHLA", "NOCA", "NOGCA", "NPaCA")


# Initialise dataframe with final info
summary_data_quality_alliance <- data.frame(
  alliance_var = character(0),
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

for (alliance in unique_alliances) {
  
  data <- data_quality %>% filter(alliance_name == alliance)
  
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
  
  # Create a new row for the alliance
  audit_metrics <- as.data.frame(audit_metrics)
  audit_metrics$alliance_var <- alliance
  audit_metrics$num_audits <- num_audits
  audit_metrics <- audit_metrics[, c("alliance_var", "num_audits", colnames(audit_metrics)[1:(ncol(audit_metrics) - 2)])]
  
  # Append the new row to the summary data frame
  summary_data_quality_alliance <- rbind(summary_data_quality_alliance, audit_metrics) %>% arrange(alliance_var)
}


#### PERFORMANCE INDICATOR

# Finding all the unique trust names
unique_alliances <- unique(Indicators_trust$alliance_name)

# Max number of audits
max_audits <- length(unique(Indicators_trust$audit))

# All audits vector
all_audits <- c("NAoMe", "NAoPri", "NBOCA", "NKCA", "NNHLA", "NOCA", "NOGCA", "NPaCA")


# Initialise dataframe with final info
summary_indicators_alliance <- data.frame(
  alliance_var = character(0),
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

for (alliance in unique_alliances) {
  
  data <- Indicators_trust %>% filter(alliance_name == alliance)
  
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
  
  # Create a new row for the alliance
  audit_metrics <- as.data.frame(audit_metrics)
  audit_metrics$alliance_var <- alliance
  audit_metrics$num_audits <- num_audits
  audit_metrics <- audit_metrics[, c("alliance_var", "num_audits", colnames(audit_metrics)[1:(ncol(audit_metrics) - 2)])]
  
  # Append the new row to the summary data frame
  summary_indicators_alliance <- rbind(summary_indicators_alliance, audit_metrics) %>% arrange(alliance_var)
}








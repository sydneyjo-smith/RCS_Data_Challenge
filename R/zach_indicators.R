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


################## PERFORMANCE INDICATORS #########

# Finding all the unique trust names
unique_Indicators_trusts <- unique(Indicators_trust$trust_name)

# Max number of audits
max_audits <- length(unique(Indicators_trust$audit))

# Initialise dataframe with final info
summary_Indicators_trusts <- data.frame(
  trust_name = character(0),
  num_audits = integer(0)
)

# Add the audit columns
all_audits <- c("NAoMe", "NAoPri", "NBOCA", "NKCA", "NNHLA", "NOCA", "NOGCA", "NPaCA")
for (audit in all_audits) {
  summary_Indicators_trusts[[paste0(audit)]] <- integer()
}

for (trust in unique_Indicators_trusts) {
  
  data <- Indicators_trust %>% filter(trust_name == trust)
  
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
  summary_Indicators_trusts <- rbind(summary_data_quality, new_row)
  
}




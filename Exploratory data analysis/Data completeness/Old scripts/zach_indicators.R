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


################## PERFORMANCE INDICATOR COMPLETENESS #########


# The below code will become a loop over all trusts/cancer alliances 
# For each trust we need to know (output as a table of some sort)
# 1) How many cancers do they have data for
# 2) Within each cancer what is the data completeness and consistency

# Finding all the unique trust names
unique_Indicators_trusts <- unique(Indicators_trust$trust_name)

# Max number of audits
max_audits <- length(unique(Indicators_trust$audit))

# All audits vector
all_audits <- c("NAoMe", "NAoPri", "NBOCA", "NKCA", "NNHLA", "NOCA", "NOGCA", "NPaCA")


# Initialise dataframe with final info
summary_indicators_trust <- data.frame(
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

for (trust in unique_Indicators_trusts) {
  
  data <- Indicators_trust %>% filter(trust_name == trust)
  
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
  summary_indicators_trust <- rbind(summary_indicators_trust, audit_metrics)
  
}



######################## POSSIBLE METRICS ###################

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

######################## METRIC COMPLETENESS ###################

# For all metrics there is denominator, pact, moving average
# For a given audit and metric we want to know if the completeness and characteristics of the data

# Define the audit and metric values
audit_val <- "NAoMe"
metric_val <- "People with newly diagnosed metastatic breast cancer discussed within a multidisciplinary team (MDT)"

# Get the unique trust names from the data
unique_indicators_trusts <- unique(Indicators_trust$trust_name)

# Create an empty list to store the results for each trust
result_list <- list()

# Loop over each unique trust
for (i in seq_along(unique_indicators_trusts)) {
  trust_val <- unique_indicators_trusts[i]  # Get the current trust name
  
  # Filter the data for the current audit, metric, and trust
  data <- Indicators_trust %>% 
    filter(audit == audit_val & trust_name == trust_val & metric_name == metric_val)
  
  # Count the number of rows for this trust
  row_count <- nrow(data)
  
  # Count the number of NAs in the columns 'denominator', 'pact', and 'mav'
  na_denominator <- sum(is.na(data$denominator))
  na_pact <- sum(is.na(data$pact))
  na_mav <- sum(is.na(data$mav))
  na_ucl <- sum(is.na(data$ucl))
  na_lcl <- sum(is.na(data$lcl))
  na_mav_ca <- sum(is.na(data$mav_ca))
  na_mav_nat <- sum(is.na(data$mav_nat))
  
  # Append the results for the current trust to the result list
  result_list[[i]] <- data.frame(
    trust_name = trust_val,
    row_count = row_count,
    na_denominator = na_denominator,
    na_pact = na_pact,
    na_mav = na_mav,
    na_ucl = na_ucl,
    na_lcl = na_lcl,
    na_mav_ca = na_mav_ca,
    na_mav_nat = na_mav_nat
  )
}

# Combine the list of results into a single data frame
result_df <- do.call(rbind, result_list)



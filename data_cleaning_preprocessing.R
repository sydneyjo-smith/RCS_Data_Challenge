################## Data cleaning and pre-processing  #####################################

#' This script prepares loads, cleans and pre-processes the provided NATCAN
#' dataset. It
#'
#' Input:
#' - Combined cancer data excel file from NATCAN
#' - Derived data quality targets file
#' - Derived indicator metric type file
#' Output:
#' - Pre-processed performance indicator data
#' - Pre-processed data quality data
#'
################################################################################

library("readxl")
library("dplyr")

################## Load in data from excel file ##################
audit_meta_info <- read_excel("data/raw_data/cancerdata_combined.xlsx", sheet = "audit_meta_info")
indicators_trust <- read_excel("data/raw_data/cancerdata_combined.xlsx", sheet = "Indicators_trust")
indicators_CA <- read_excel("data/raw_data/cancerdata_combined.xlsx", sheet = "Indicators_CA")
data_quality <- read_excel("data/raw_data/cancerdata_combined.xlsx", sheet = "data_quality")
metric_info <- read_excel("data/raw_data/cancerdata_combined.xlsx", sheet = "metric_info")

data_quality_targets <- read.csv("data/raw_data/data_quality_targets.csv")
performance_indicator_metric_type <- read.csv("data/raw_data/performance_indicator_metric_type.csv")

################## indicators_trust ##################

# trust_name (Guy's / Guys)
indicators_trust <- indicators_trust %>%
  mutate("trust_name" = ifelse(trust_name == "Guys and St Thomas NHS Foundation Trust","Guy's and St Thomas' NHS Foundation Trust", trust_name))

# missing date (just 1 - remove)
# missing calendar_year (just 1 - remove)
indicators_trust <- indicators_trust %>%
  filter(!is.na(date))


# date showing quarter_year (NOGCA, NPaCA)
quarters <- indicators_trust %>%
  group_by(date, quarter_year) %>%
  summarise(date = first(date)) %>%
  ungroup

unique_quarters <- quarters %>%
  slice(1:16) %>%
  mutate(start_date = date) %>%
  arrange(start_date) %>%
  select(-date)

indicators_trust <- indicators_trust %>%
  left_join(unique_quarters, by = "quarter_year") %>%
  mutate(date = start_date) %>%  # Assign start_date to the date column
  select(-start_date)  # Remove redundant column if necessary

# missing alliance_code (NOCA audit)

# alliance_codes <- indicators_trust %>%
#   group_by(alliance_code, alliance_name) %>%
#   summarise(alliance_code = first(alliance_code)) %>%
#   ungroup
#
# unique_alliance_codes <- alliance_codes %>%
#   filter(!is.na(alliance_code)) %>%
#   mutate(ref_code = alliance_code) %>%
#   select(-alliance_code)
#
# indicators_trust <- indicators_trust %>%
#   left_join(unique_alliance_codes, by = "alliance_name") %>%
#   mutate(alliance_code = ref_code) %>%  # Assign start_date to the date column
#   select(-ref_code)  # Remove redundant column if necessary


# missing alliance_name (NOCA audit)

indicators_trust <- indicators_trust %>%
  filter(!(is.na(trust_code) &
           trust_name == "Guy's and St Thomas' NHS Foundation Trust" &
           metric_name == "Emergency admissions prior to diagnosis"))

# missing trust_code (NOCA audit)
unique_trust_names <- indicators_trust %>%
  group_by(trust_code, trust_name) %>%
  summarise(trust_code = first(trust_code)) %>%
  ungroup %>%
  filter(!is.na(trust_code)) %>%
  mutate(ref_code = trust_code) %>%
  select(-trust_code)

indicators_trust <- indicators_trust %>%
  left_join(unique_trust_names, by = "trust_name") %>%
  mutate(trust_code = ref_code) %>%  # Assign start_date to the date column
  select(-ref_code)  # Remove redundant column if necessary


# Values 0.0-1 (NAoME, NAoPri, NNHLA)
# Values 0-100 (NKCA, NOCA, NOGCA, NPaCA)
# Mixed values (NBOCA (unadjusted-30/90 are 0-1, others 0-100))

indicators_trust <- indicators_trust %>%
  mutate(
    pact = if_else(pact > 1, pact/100, pact),
    mav = if_else(mav > 1, mav/100, mav),
    mav_ca = if_else(mav_ca > 1, mav_ca/100, mav_ca),
    mav_nat = if_else(mav_nat > 1, mav_nat/100, mav_nat))


# Join the directional variable
indicators_trust <- indicators_trust %>%
  left_join(performance_indicator_metric_type, by=c("audit", "metric_name"))

# Add cancer audit name
audit_meta_info <- audit_meta_info %>%
  rename(audit = audit_name_acronym)

indicators_trust <- indicators_trust %>%
  left_join(audit_meta_info, by=c("audit"))


# denominator "0" (NNHLA)
# demoninator "1","2","3","4" (NOCA - Clatterbridge Cancer Centre NHS Foundation Trust. Data should be supressed for under 5)

################## Data Quality  ##################


# Values 0.0-1 (NAoME, NAoPri, NNHLA)
# Values 0-100 (NKCA, NOCA, NOGCA, NPaCA)
# Mixed values (NBOCA (unadjusted-30/90 are 0-1, others 0-100))
data_quality <- data_quality %>%
  mutate(
    pact = if_else(pact > 1, pact/100, pact),
    mav = if_else(mav > 1, mav/100, mav))

# date showing quarter_year (NOGCA, NPaCA)
quarters <- data_quality %>%
  group_by(date, quarter_year) %>%
  summarise(date = first(date)) %>%
  ungroup

unique_quarters <- quarters %>%
  slice(1:16) %>%
  mutate(start_date = date) %>%
  arrange(start_date) %>%
  select(-date)

data_quality <- data_quality %>%
  left_join(unique_quarters, by = "quarter_year") %>%
  mutate(date = start_date) %>%  # Assign start_date to the date column
  select(-start_date)  # Remove redundant column if necessary

# Join the directional variable
data_quality <- data_quality %>%
  left_join(data_quality_targets, by=c("audit", "metric_name"))

# Add cancer audit name
data_quality <- data_quality %>%
  left_join(audit_meta_info, by=c("audit"))

################## Saving processed files  ##################

write.csv(indicators_trust, "data/processed_data/indicators_trust.csv", row.names = FALSE,na = "")
write.csv(data_quality, "data/processed_data/data_quality.csv", row.names = FALSE,na = "")


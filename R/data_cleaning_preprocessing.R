################## Data cleaning and pre-processing

library("readxl")
library("dplyr")

### Load in data from excel file
audit_meta_info <- read_excel("data/cancerdata_combined.xlsx", sheet = "audit_meta_info")
indicators_trust <- read_excel("data/raw_data/cancerdata_combined.xlsx", sheet = "Indicators_trust")
indicators_CA <- read_excel("data/cancerdata_combined.xlsx", sheet = "Indicators_CA")
data_quality <- read_excel("data/cancerdata_combined.xlsx", sheet = "data_quality")
metric_info <- read_excel("data/cancerdata_combined.xlsx", sheet = "metric_info")



################## indicators_trust

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

# Join the directional variable


# denominator "0" (NNHLA)
# demoninator "1","2","3","4" (NOCA - Clatterbridge Cancer Centre NHS Foundation Trust. Data should be supressed for under 5)



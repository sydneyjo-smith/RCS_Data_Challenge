################## Data cleaning and pre-processing

library("readxl")

### Load in data from excel file
audit_meta_info <- read_excel("data/cancerdata_combined.xlsx", sheet = "audit_meta_info")
indicators_trust <- read_excel("data/cancerdata_combined.xlsx", sheet = "Indicators_trust")
indicators_CA <- read_excel("data/cancerdata_combined.xlsx", sheet = "Indicators_CA")
data_quality <- read_excel("data/cancerdata_combined.xlsx", sheet = "data_quality")
metric_info <- read_excel("data/cancerdata_combined.xlsx", sheet = "metric_info")



################## indicators_trust

# trust_name (Guy's / Guys)
# date showing quarter_year (NOGCA, NPaCA)
# missing date (just 1 - remove)
# missing calendar_year (just 1 - remove)
# missing alliance_code (NOCA audit)
# missing alliance_name (NOCA audit)
# missing trust_code (NOCA audit)
# denominator "0" (NNHLA)
# demoninator "1","2","3","4" (NOCA - Clatterbridge Cancer Centre NHS Foundation Trust. Data should be supressed for under 5)

# Values 0.0-1 (NAoME, NAoPri, NNHLA)
# Values 0-100 (NKCA, NOCA, NOGCA, NPaCA)
# Mixed values (NBOCA (unadjusted-30/90 are 0-1, others 0-100))

# Join the directional variable





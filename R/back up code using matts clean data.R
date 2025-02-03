library(shiny)
library(dplyr)
library(reactable)
library(tidyverse)


# filter out Guys trust
Data_Guys<-indicators_trust%>% filter(trust_name=="Guy's and St Thomas' NHS Foundation Trust")

# removed "Emergency admissions prior to diagnosis" which is duplicated in "NOCA"
Data_Guys <- Data_Guys %>%
  filter(!(audit == "NOCA" & metric_name == "Emergency admissions prior to diagnosis"))

#code to join so table has previous quarters metrics
Data_Guys_previous <- Data_Guys %>%
  arrange(audit, metric_name, quarter_year)%>%
  mutate(
    # Split quarter_year into quarter and year
    quarter = substr(quarter_year, 1, 2),
    year = as.integer(substr(quarter_year, 4, 7)),
    # Calculate the previous quarter and year
    previous_quarter_year = case_when(
      quarter == "Q1" ~ paste0("Q4-", year - 1), # For Q1, link to Q4 of the previous year
      TRUE ~ paste0("Q", as.integer(substr(quarter, 2, 2)) - 1, "-", year) # For Q2-Q4
    )
  ) %>%
  # Join the previous quarter's value
  left_join(Data_Guys, select(audit, quarter_year, metric_name, pact, mav, mav_nat, mav_ca),
            by = c("audit" = "audit", "metric_name"="metric_name", "previous_quarter_year" = "quarter_year"), suffix = c("", "_prev")) %>%
  select(audit_name_full, date, quarter_year, metric_name, metric_type, denominator, mav, mav_ca, mav_nat, previous_quarter_year, denominator_prev,
         mav_prev, mav_nat_prev, mav_ca_prev)


# filtering for 2 quarters
Data_Guys_20<- Data_Guys_previous %>% filter(quarter_year=="Q1-2022")

# converting to percentages
Data_Guys_20<-Data_Guys_20 %>% mutate(mav=mav*100)%>%
  mutate(mav_ca=mav_ca*100)%>%
  mutate(mav_nat=mav_nat*100)%>%
  mutate(mav_prev=mav_prev*100)%>%
  mutate(mav_ca=mav_ca_prev*100)%>%
  mutate(mav_nat=mav_nat_prev*100)

# generating performance difference variables.
Data_Guys_20<- Data_Guys_20%>% mutate(variance=mav-mav_nat)%>%
  mutate(variance_prev_quarter=mav-mav_prev)




# selecting columns for tables
Data_Guys_20<-Data_Guys_20%>% select(audit_name_full, metric_name, metric_type, denominator, mav,variance, variance_prev_quarter)

#code to allocate header rows by RowType
header_rows <- Data_Guys_20 %>%
  distinct(audit_name_full) %>%
  mutate(
    audit_name_full=audit_name_full,
    metric_name = NA,
    metric_type=NA,
    mav=NA,
    denominator=NA,
    variance=NA,
    variance_prev_quarter=NA,
    RowType = "Header"  # Column to differentiate header rows
  )


# Add header rows to the dataset- generate the rest of variables as RowType=Data
df_with_titles <- Data_Guys_20 %>%
  mutate(RowType = "Data") %>%
  bind_rows(header_rows) %>%
  mutate(RowType = factor(RowType, levels = c("Header", "Data")))%>%
  arrange(audit_name_full, RowType)  # Ensures audit headers appear before metrics

# Display in reactable with styling

table3<-reactable(df_with_titles,
                  filterable = TRUE,
                  searchable = TRUE,
                  columns = list(
                    audit_name_full = colDef(show=FALSE),
                    RowType = colDef(show = FALSE),
                    metric_name = colDef(
                      name = "Metric Name",
                      cell = function(value, index) {
                        if (df_with_titles$RowType[index] == "Header") {
                          return(df_with_titles$audit_name_full[index])
                        } else {
                          return(value)
                        }
                      }
                    ),
                    denominator = colDef(name="Case Volume",
                                         cell = function(value, index) {
                                           if (df_with_titles$RowType[index] == "Header") {
                                             return("")
                                           } else {
                                             return(sprintf("%.2f", value))
                                           }
                                         }
                    ),
                    mav = colDef(name = "Current Quarter Moving Average Performance (%)",
                                 cell = function(value, index) {
                                   if (df_with_titles$RowType[index] == "Header") {
                                     return("")
                                   } else {
                                     return(sprintf("%.2f", value))
                                   }
                                 }
                    ),
                    variance = colDef(name="Comparison to National Performance (%)",
                                      cell = function(value, index) {
                                        if (df_with_titles$RowType[index] == "Header") {
                                          return("")  # Empty cell for header rows
                                        } else {
                                          metric_type <- df_with_titles$metric_type[index]
                                          color <- get_color_2(value, metric_type)  # Apply color gradient function
                                          return(div(
                                            style = paste("background-color:", color, "; padding: 8px; border-radius: 4px;"),
                                            round(value, 2)  # Round for better readability
                                          ))
                                        }
                                      }
                    ),
                    variance_prev_quarter = colDef( name="Comparison to Previous Quarter Performance (%)",
                                                    cell = function(value, index) {
                                                      if (df_with_titles$RowType[index] == "Header") {
                                                        return("")  # Empty cell for header rows
                                                      } else {
                                                        metric_type <- df_with_titles$metric_type[index]
                                                        color <- get_color_2(value, metric_type)  # Apply color gradient function
                                                        return(div(
                                                          style = paste("background-color:", color, "; padding: 8px; border-radius: 4px;"),
                                                          round(value, 2)  # Round for better readability
                                                        ))
                                                      }
                                                    }
                    )
                  ),
                  rowStyle = function(index) {
                    if (df_with_titles$RowType[index] == "Header") {
                      return(list(
                        background = "blue",
                        color = "white",
                        fontWeight = "bold"
                      ))  # Entire row is blue with white bold text
                    } else {
                      return(NULL)  # Default row style
                    }
                  }
)

table3
install.packages("bsicons")

get_color_2 <- function(value, metric_type) {
  if (is.na(value)) return("#808080")  # Default color for NA (gray)

  # Define color scales (color-blind friendly)
  red_palette <- hcl.colors(10, "Red-Blue", rev = TRUE)  # Light to dark red
  green_palette <- hcl.colors(10, "TealGrn", rev = TRUE)

  # Reverse logic if metric_type = 0
  if (metric_type == 0) {
    temp <- red_palette
    red_palette <- green_palette
    green_palette <- temp
  }

  # Compute the scaling for red (negative values)
  if (value < 0) {
    if (value >= -10) {
      color_index <- 2  # Light orange for values between 0 and -10
    } else if (value >= -20) {
      color_index <- 6  # Medium orange for values between -10 and -20
    } else {
      color_index <- 10  # Dark orange for values less than -20
    }
    return(red_palette[color_index])  # Use red gradient with specified index
  }

  # Compute the scaling for green (positive values)
  if (value > 0) {
    if (value >= 10) {
      color_index <- 2  # Light green for values between 0 and 10
    } else if (value >= 20) {
      color_index <- 6  # Medium green for values between -10 and 20
    } else {
      color_index <- 10  # Dark green for values less than 20
    }
    return(green_palette[color_index])
  }

  # Neutral color for 0
  return("white")
}


####test code#####
output$table_1<-renderReactable({reactable(plot_1_dataset(),
                                           filterable = TRUE,
                                           searchable = TRUE,
                                           columns = list(
                                             audit=colDef(show=FALSE),
                                             trust_code=colDef(show=FALSE),
                                             trust_name=colDef(show=FALSE),
                                             quarter_year=colDef(show=FALSE),
                                             audit_name_full = colDef(show=FALSE),
                                             RowType = colDef(show = FALSE),
                                             metric_name = colDef(
                                               name = "Metric Name",
                                               cell = function(value, index) {
                                                 data <- plot_1_dataset()
                                                 if (data$RowType[index] == "Header") {
                                                   return(data$audit_name_full[index])
                                                 } else {
                                                   return(value)
                                                 }
                                               }
                                             ),
                                             denominator = colDef(name="Case Volume",
                                                                  cell = function(value, index) {
                                                                    data <- plot_1_dataset()
                                                                    if (data[index, "RowType"] == "Header") {  # Correct indexing method
                                                                      return("")  # Blank for header rows
                                                                    } else {
                                                                      return(sprintf("%.2f", as.numeric(value)))
                                                                    }
                                                                  }
                                             ),
                                             mav = colDef(name = "Current Quarter Moving Average Performance (%)",
                                                          cell = function(value, index) {
                                                            data <- plot_1_dataset()
                                                            if (data[index, "RowType"] == "Header") {  # Correct indexing method
                                                              return("")  # Blank for header rows
                                                            } else {
                                                              return(sprintf("%.2f", as.numeric(value)))

                                                            }
                                                          }
                                             ),
                                             variance = colDef(name="Comparison to National Performance (%)",
                                                               cell = function(value, index) {
                                                                 data <- plot_1_dataset()
                                                                 if (data[index, "RowType"] == "Header") {
                                                                   return("")  # Empty cell for header rows
                                                                 } else {
                                                                   metric_type <- data[index, "metric_type"]

                                                                   color <- get_color_2(as.numeric(value), metric_type)  # Ensure numeric formatting
                                                                   return(htmltools::div(
                                                                     style = paste("background-color:", color, "; padding: 8px; border-radius: 4px;"),
                                                                     round(as.numeric(value), 2)  # Round for better readability
                                                                   ))
                                                                 }
                                                               }
                                             ),
                                             variance_prev_quarter = colDef( name="Comparison to Previous Quarter Performance (%)",
                                                                             cell = function(value, index) {
                                                                               data <- plot_1_dataset()
                                                                               if (data[index, "RowType"] == "Header") {
                                                                                 return("")  # Empty cell for header rows
                                                                               } else {
                                                                                 metric_type <- data[index, "metric_type"]
                                                                                 color <- get_color_2(as.numeric(value), metric_type)
                                                                                 return(htmltools::div(
                                                                                   style = paste("background-color:", color, "; padding: 8px; border-radius:                                                                  4px;"),
                                                                                   round(as.numeric(value), 2) # Round for better readability
                                                                                 ))
                                                                               }
                                                                             }
                                             )
                                           )
)
})

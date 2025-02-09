reactable(df_with_titles, 
          columns = list(
            audit = colDef(show=FALSE),
            RowType = colDef(show = FALSE),
            metric_name = colDef(
              name = "Metric Name",
              cell = function(value, index) {
                if (df_with_titles$RowType[index] == "Header") {
                  return(df_with_titles$audit[index])
                } else {
                  return(value)
                }
              }
            ),# Hide helper column
            mav = colDef(
              cell = function(value, index) {
                if (df_with_titles$RowType[index] == "Header") {
                  ""  # Empty for header rows
                } else {
                  value
                }
              }
            ),
            denominator = colDef(
              cell = function(value, index) {
                if (df_with_titles$RowType[index] == "Header") {
                  ""  # Empty for header rows
                } else {
                  value
                }
              }
            ),
            variance = colDef(
              cell = function(value, index) {
                if (df_with_titles$RowType[index] == "Header") {
                  ""  # Empty for header rows
                } else {
                  value
                }
              }
            ),
            variance_prev_quarter = colDef(
              cell = function(value, index) {
                if (df_with_titles$RowType[index] == "Header") {
                  ""  # Empty for header rows
                } else {
                  value
                }
              }
            )
          )
)

##code to generate table with colour
table3<-reactable(df_with_titles, 
                  filterable = TRUE,
                  searchable = TRUE,
                  columns = list(
                    audit = colDef(show=FALSE),
                    RowType = colDef(show = FALSE),
                    metric_name = colDef(
                      name = "Metric Name",
                      cell = function(value, index) {
                        if (df_with_titles$RowType[index] == "Header") {
                          return(df_with_titles$audit[index])
                        } else {
                          return(value)
                        }
                      }
                    ),# Hide helper column
                    mav = colDef(name = "Current Quarter Moving Average Performance",
                                 cell = function(value, index) {
                                   if (df_with_titles$RowType[index] == "Header") {
                                     return("")  # Empty cell for header rows
                                   } else {
                                     color <- get_color(value)  # Apply color gradient function
                                     return(div(
                                       style = paste("background-color:", color, "; padding: 8px; border-radius: 4px;"),
                                       round(value, 2)  # Round for better readability
                                     ))
                                   }
                                 }
                    ),
                    denominator = colDef(
                      cell = function(value, index) {
                        if (df_with_titles$RowType[index] == "Header") {
                          ""  # Empty for header rows
                        } else {
                          value
                        }
                      }
                    ),
                    variance = colDef(
                      cell = function(value, index) {
                        if (df_with_titles$RowType[index] == "Header") {
                          ""  # Empty for header rows
                        } else {
                          value
                        }
                      }
                    ),
                    variance_prev_quarter = colDef(
                      cell = function(value, index) {
                        if (df_with_titles$RowType[index] == "Header") {
                          ""  # Empty for header rows
                        } else {
                          value
                        }
                      }
                    )
                  )
)

#################################
code with audit name highlighted

table3<-reactable(df_with_titles, 
                  filterable = TRUE,
                  searchable = TRUE,
                  columns = list(
                    audit = colDef(show=FALSE),
                    RowType = colDef(show = FALSE),
                    metric_name = colDef(
                      name = "Metric Name",
                      cell = function(value, index) {
                        if (df_with_titles$RowType[index] == "Header") {
                          return(div(
                            style = "background-color: blue; color: white; font-weight: bold; padding: 5px;",
                            df_with_titles$audit[index]  # Display the audit name
                          ))
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
                                             value
                                           }
                                         }
                    ),
                    mav = colDef(name = "Current Quarter Moving Average Performance",
                                 cell = function(value, index) {
                                   if (df_with_titles$RowType[index] == "Header") {
                                     return("")
                                   } else {
                                     return(value)
                                   }
                                 }
                    ),
                    variance = colDef(name="Comparison to National Performance",
                                      cell = function(value, index) {
                                        if (df_with_titles$RowType[index] == "Header") {
                                          return("")  # Empty cell for header rows
                                        } else {
                                          color <- get_color(value)  # Apply color gradient function
                                          return(div(
                                            style = paste("background-color:", color, "; padding: 8px; border-radius: 4px;"),
                                            round(value, 2)  # Round for better readability
                                          ))
                                        }
                                      }
                    ),
                    variance_prev_quarter = colDef( name="Comparison to Previous Quarter Performance",
                                                    cell = function(value, index) {
                                                      if (df_with_titles$RowType[index] == "Header") {
                                                        return("")  # Empty cell for header rows
                                                      } else {
                                                        color <- get_color(value)  # Apply color gradient function
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
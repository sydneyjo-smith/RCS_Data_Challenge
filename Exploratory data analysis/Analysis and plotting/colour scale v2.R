install.packages("viridis")
library(viridis)


get_color <- function(value) {
  if (is.na(value)) return("#808080")  # Default color for NA
  
  # Define color scales
  red_palette <- colorRampPalette(c("#ffcccc", "#ff0000")) # Light to dark red
  green_palette <- colorRampPalette(c("#ccffcc", "#008000")) # Light to dark green
  
  # Compute the scaling for red (negative values)
  if (value < 0) {
    # Categorize values into ranges
    if (value >= -10) {
      color_index <- 2  # Light red for values between 0 and -10
    } else if (value >= -20) {
      color_index <- 6  # Medium red for values between -10 and -20
    } else {
      color_index <- 10  # Dark red for values less than -20
    }
    return(red_palette(10)[color_index])  # Use red gradient with specified index
  }
  
  # Compute the scaling for green (positive values)
  if (value > 0) {
    # Calculate the difference from 100 for green
    diff_scaled <- (100 - value) * 0.1  # Adjust the factor to control the sensitivity for green
    color_index <- min(10, ceiling(diff_scaled))  # Scale to 10 levels
    return(green_palette(10)[color_index])  # Use green gradient
  }
  
  # Neutral color for 0
  return("white")
}

table1<- reactable(Data_Guys_v2 %>% select (audit, metric_name, mav_prev,mav, variance),
                  filterable = TRUE,
                  searchable = TRUE,
                  style = list(fontSize = "10px"),
                  theme = reactableTheme(
                    headerStyle = list(
                      backgroundColor = "#5F9EA0",  # Blue header
                      color = "white",           # White text
                      fontWeight = "bold"        # Bold text for better visibility
                    )
                  ),
                  defaultPageSize = 11,
                  defaultColDef = colDef(
                    vAlign = "center",
                    align = "center",
                    headerVAlign = "center",
                    width = 110
                  ),
                  columns = list(
                    audit=colDef(name="Cancer Audit"), 
                    metric_name=colDef(name="Performance Indicator"), # in order: prev quarter, current quarter, 
                    mav_prev= colDef(name = "Previous Quarter Moving Average Performance", cell = function(value) sprintf("%.2f", value)),
                    mav = colDef(name = "Current Quarter Moving Average Performance", cell = function(value) sprintf("%.2f", value)),
                    variance = colDef(name="Difference in Performance", 
                                       cell = function(value) {
                                        color <- get_color(value)
                                        div(style = paste("background-color:", color, "; padding: 8px; border-radius: 4px;"),
                                            round(value, 2))  # Round for better readability
                                      })
                  ),
                  bordered = TRUE,
                  highlight = TRUE)

table1
saveRDS(table1, "./table1.RDS")

server <- function(input, output, session) {
  output$table <- renderReactable({
    if (input$dropdown == "Guys and St Thomas") {
      table1
    } else if (input$dropdown == "South Tyneside and Sunderland NHS Foundation Trust") {
      table2
    } else {
      NULL
    }
  })
}
# app.R
library(shiny)
library(tidyverse)
library(leaflet)

accident_data <- read_csv("data/thai_road_accident_2019_2022.csv", show_col_types = FALSE) %>%
  mutate(
    incident_datetime = as.POSIXct(incident_datetime),
    day_of_week = weekdays(incident_datetime),
    hour = as.integer(format(incident_datetime, "%H"))
  ) %>%
  drop_na(number_of_fatalities, province_en, vehicle_type,
          weather_condition, number_of_vehicles_involved,
          day_of_week, hour) %>%
  filter(province_en != "unknown")

# UI 구성
ui <- navbarPage("Thailand Road Accident Analytics",
                 
                 tabPanel("Overall Trends",
                          fluidPage(
                            h3("Trend of Average Fatalities by Hour and Day"),
                            plotOutput("trendPlot", height = "400px"),
                            br(),
                            h3("Average Fatalities by Day of Week"),
                            plotOutput("byDayPlot", height = "300px"),
                            br(),
                            h3("Average Fatalities by Vehicle Type"),
                            plotOutput("byVehiclePlot", height = "300px"),
                            br(),
                            h3("Fatalities by Province (Heatmap)"),
                            leafletOutput("provinceMap", height = "500px")
                          )
                 ),
                 
                 tabPanel("Predictive Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("province", "Province:",
                                          choices = sort(unique(accident_data$province_en))),
                              selectInput("vehicle", "Vehicle Type:",
                                          choices = sort(unique(accident_data$vehicle_type))),
                              selectInput("weather", "Weather Condition:",
                                          choices = sort(unique(accident_data$weather_condition))),
                              selectInput("day", "Day of Week:",
                                          choices = sort(unique(accident_data$day_of_week))),
                              sliderInput("hour", "Hour of Day:", min = 0, max = 23, value = 12),
                              numericInput("vehicles", "Number of Vehicles Involved:", value = 1, min = 1)
                            ),
                            mainPanel(
                              h3("Predicted Number of Fatalities:"),
                              verbatimTextOutput("prediction"),
                              br(),
                              h4("Visual Comparison with Overall Average:"),
                              plotOutput("riskPlot"),
                              br(),
                              textOutput("riskSummary")
                            )
                          )
                 )
)

# Server
server <- function(input, output) {
  
  model <- glm(number_of_fatalities ~ province_en + vehicle_type +
                 weather_condition + day_of_week + hour +
                 number_of_vehicles_involved,
               family = poisson(), data = accident_data)
  
  output$prediction <- renderText({
    newdata <- data.frame(
      province_en = input$province,
      vehicle_type = input$vehicle,
      weather_condition = input$weather,
      day_of_week = input$day,
      hour = input$hour,
      number_of_vehicles_involved = input$vehicles
    )
    pred <- predict(model, newdata = newdata, type = "response")
    paste0("Estimated fatalities: ", round(pred, 3))
  })
  
  output$riskPlot <- renderPlot({
    newdata <- data.frame(
      province_en = input$province,
      vehicle_type = input$vehicle,
      weather_condition = input$weather,
      day_of_week = input$day,
      hour = input$hour,
      number_of_vehicles_involved = input$vehicles
    )
    
    pred <- predict(model, newdata = newdata, type = "response")
    avg_pred <- mean(predict(model, type = "response"), na.rm = TRUE)
    
    if (is.na(pred) || pred < 0) {
      ggplot() +
        annotate("text", x = 1, y = 0.5, label = "No prediction available", size = 6) +
        theme_void()
    } else {
      bar_data <- tibble(
        Scenario = c("This Scenario", "Overall Avg"),
        Value = c(pred, avg_pred)
      )
      
      ggplot(bar_data, aes(x = Scenario, y = Value, fill = Scenario)) +
        geom_col(width = 0.5) +
        labs(x = NULL, y = "Estimated Fatalities") +
        theme_minimal(base_size = 14) +
        scale_fill_manual(values = c("This Scenario" = "firebrick", "Overall Avg" = "gray")) +
        ylim(0, max(1, pred, avg_pred) * 1.2)
    }
  })
  
  output$riskSummary <- renderText({
    newdata <- data.frame(
      province_en = input$province,
      vehicle_type = input$vehicle,
      weather_condition = input$weather,
      day_of_week = input$day,
      hour = input$hour,
      number_of_vehicles_involved = input$vehicles
    )
    pred <- predict(model, newdata = newdata, type = "response")
    avg_pred <- mean(predict(model, type = "response"), na.rm = TRUE)
    
    if (is.na(pred)) {
      return("No prediction available.")
    }
    
    comparison <- ifelse(pred > avg_pred, "higher than", "lower than")
    paste0("Your predicted fatalities (", round(pred, 3), ") are ", comparison,
           " the overall average (", round(avg_pred, 3), ").")
  })
  
  output$trendPlot <- renderPlot({
    trend_data <- accident_data %>%
      group_by(day_of_week, hour) %>%
      summarise(mean_fatalities = mean(number_of_fatalities, na.rm = TRUE), .groups = "drop")
    
    ggplot(trend_data, aes(x = hour, y = mean_fatalities, color = day_of_week)) +
      geom_line(size = 1.2) +
      geom_vline(xintercept = input$hour, linetype = "dashed", color = "black") +
      labs(title = "Average Fatalities by Hour and Day of Week",
           x = "Hour of Day", y = "Mean Fatalities",
           color = "Day of Week") +
      theme_minimal(base_size = 14)
  })
  
  output$byDayPlot <- renderPlot({
    accident_data %>%
      group_by(day_of_week) %>%
      summarise(mean_fatalities = mean(number_of_fatalities, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = reorder(day_of_week, -mean_fatalities), y = mean_fatalities, fill = day_of_week)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Day", y = "Mean Fatalities") +
      theme_minimal(base_size = 14)
  })
  
  output$byVehiclePlot <- renderPlot({
    accident_data %>%
      group_by(vehicle_type) %>%
      summarise(mean_fatalities = mean(number_of_fatalities, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = reorder(vehicle_type, -mean_fatalities), y = mean_fatalities, fill = vehicle_type)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Vehicle Type", y = "Mean Fatalities") +
      theme_minimal(base_size = 14) +
      coord_flip()
  })
  
  output$provinceMap <- renderLeaflet({
    province_summary <- accident_data %>%
      group_by(province_en) %>%
      summarise(mean_fatalities = mean(number_of_fatalities, na.rm = TRUE), .groups = "drop")
    
    province_coords <- province_summary %>%
      mutate(lat = jitter(runif(n(), 13, 19), amount = 0.2),
             lng = jitter(runif(n(), 98, 105), amount = 0.2))
    
    pal <- colorNumeric(palette = "Reds", domain = province_coords$mean_fatalities)
    
    leaflet(province_coords) %>%
      addTiles() %>%
      setView(lng = 101.5, lat = 15.5, zoom = 6) %>%
      addCircleMarkers(~lng, ~lat,
                       radius = ~sqrt(mean_fatalities) * 30,
                       fillColor = ~pal(mean_fatalities),
                       fillOpacity = 0.7,
                       color = "black",
                       weight = 1,
                       popup = ~paste(province_en, "<br>Mean Fatalities:", round(mean_fatalities, 2))) %>%
      addLegend("bottomright", pal = pal, values = ~mean_fatalities,
                title = "Avg Fatalities", opacity = 0.7)
  })
}

shinyApp(ui = ui, server = server)

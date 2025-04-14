server <- function(input, output, session) {
  
  bs_themer()
  
  # HOME
  
  output$map <- renderMaplibre({
    maplibre(style = carto_style("dark-matter"),
             center = c(-79.381070, 43.656183),
             zoom = 14.5,
             pitch = 45,
             bearing = -17) |> 
      add_fill_layer(id = "bia_boundary",
                     source = bia,
                     fill_color = "#00AEF6",
                     fill_opacity = 0.5) |>
      add_categorical_legend(
        legend_title = "Legend",
        values = c("Downtown Yonge BIA"),
        colors = c("#00AEF6"),
        circular_patches = FALSE,
        position = "bottom-left",
        unique_id = "legend"
      )
  }) 
  
  output$map1 <- renderMaplibre({
    maplibre(style = carto_style("dark-matter"),
             center = c(-79.381070, 43.656183),
             zoom = 11,
             bearing = -17) |> 
      add_fill_layer(id = "bia_boundary",
                     source = bia,
                     fill_color = "#00AEF6",
                     fill_opacity = 0.5)
  }) 
  
  
  
  
  # Visitors ----
  
  # Visitor Levels Reactivity
  visitorLevelsData = reactive({
    req(input$quarter)
    # set the filter conditions based on the selected year and quarter
    if (input$quarter == "Q1") {
      target_end_date = ymd(paste0(input$year, "03", "31"))
    } else if (input$quarter == "Q2") {
      target_end_date = ymd(paste0(input$year, "06", "30"))
    } else if (input$quarter == "Q3") {
      target_end_date = ymd(paste0(input$year, "09", "30"))
    } else {
      target_end_date = ymd(paste0(input$year, "12", "31"))
    }
    
    visitorLevelsFiltered = ff_overall %>%
      dplyr::filter(Date < target_end_date) %>%
      mutate(Percentage = round(Percentage, 2))
  })
  
  
  # Visitor Levels Chart
  output$visitorLevels = renderPlotly({
    # generate the plot
    plotOverallVisits = ggplot(visitorLevelsData(), aes(x = Date, y = Count)) +
      geom_line(size = 1, color = "#00AEF6") +
      geom_point(size = 2, color = "#00AEF6") +
      scale_x_date(
        date_breaks = "3 months",
        date_labels = "%b %y",
        limits = c(ymd("2021-01-01"), ymd("2025-01-01")),
      ) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
      labs(title = "Monthly Visits since 2021", x = "Date", y = "Visits (Thousands)") +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "#fff"),  # match fg
        axis.text = element_text(color = "#fff"),
        axis.title = element_text(color = "#fff")
      )
    
    # convert into a plotly
    ggplotly(plotOverallVisits) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#fff", family = "Inter"),
        title = list(font = list(family = "Roboto Mono", color = "#fff")),
        yaxis = list(fixedrange = TRUE, gridcolor = "#4f4f4f", showline=TRUE, linewidth=1, linecolor='#4f4f4f',mirror=TRUE),
        xaxis = list(gridcolor = "#4f4f4f", showline=TRUE, linewidth=1, linecolor='#4f4f4f',mirror=TRUE)
      )
  })
  
  output$visitorLevels2 <- renderEcharts4r({
    req(visitorLevelsData())
    visitorLevelsData() %>%
      dplyr::mutate(Date = as.Date(Date)) %>%
      dplyr::arrange(Date) %>%
      e_charts(Date) %>%
      e_line(serie = Count, name = "Visits", symbol = "circle", symbol_size = 8,
             lineStyle = list(width = 2, color = "#00AEF6"),
             itemStyle = list(color = "#00AEF6")) %>%
      e_x_axis(
        type = "time",
        min = "2021-01-01",
        max = "2025-01-01",
        axisLine = list(lineStyle = list(color = "#4f4f4f")),
        axisLabel = list(color = "#ffffff")
      ) %>%
      e_y_axis(
        name = "Visits",
        max = "10000000",
        axisLine = list(lineStyle = list(color = "#4f4f4f")),
        splitLine = list(lineStyle = list(color = "#4f4f4f"))
      ) %>%
      e_tooltip(trigger = "axis") %>%
      e_text_style(color = "#ffffff", fontFamily = "Inter") %>%
      e_legend(show = FALSE)
  })
  
  
  
  
  
  # Visitor Types Reactivity
  visitorTypeData = reactive({
    visitorTypeFiltered = ff_type %>%
      dplyr::filter((Quarter == input$quarter) & (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))
  })
  
  # Visitor Types Chart
  output$visitorTypes = renderPlotly({
    # generate the plot
    plotVisitorType = ggplot(visitorTypeData(),
                             aes(x = paste0(input$quarter, " ", as.character(Year)),
                                 y = Visits, fill = factor(Type, levels = c("Infrequent Visitor", "Recurring Visitor", "Resident")),
                                 text = paste("Visits:", scales::comma(round(Visits))))) +
      geom_bar(aes(color = factor(Type, levels = c("Infrequent Visitor", "Recurring Visitor", "Resident"))),
               position = "stack", stat = "identity", width = 0.7, linewidth = 1) +
      scale_fill_manual(values = c("#FFD931", "#002940", "#00AEF6")) +
      scale_color_manual(values = c("#FFD931", "#002940", "#00AEF6")) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
      labs(title = "Visits by Type of Visitor", x = "Year", y = "Visits (Thousands)") +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "#fff"),
        axis.text = element_text(color = "#fff"),
        axis.title = element_text(color = "#fff"),
        axis.title.x = element_blank(),
        legend.position = "top",
      )
    
    # convert to plotly
    ggplotly(plotVisitorType, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#fff", family = "Inter"),
        title = list(font = list(family = "Roboto Mono", color = "#fff")),
        legend = list(
          font = list(size = 12),
          title = "",
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.2, yanchor = "top"
        ),
        yaxis = list(fixedrange = TRUE, gridcolor = "#4f4f4f", showline=TRUE, linewidth=1, linecolor='#4f4f4f',mirror=TRUE),
        xaxis = list(gridcolor = "#4f4f4f", showline=TRUE, linewidth=1, linecolor='#4f4f4f',mirror=TRUE)
      )
  })
  
  
  # Visitor Day of Week Reactivity
  vistorDayData = reactive({
    visitorDayFiltered = ff_day_of_week %>%
      dplyr::filter((Quarter == input$quarter) & (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))
  })
  
  # Visitor Day of Week Chart
  output$visitorDoW = renderPlotly({
    # generate the plot
    plotDoW = ggplot(vistorDayData(), aes(x = factor(Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                                          y = Visits, fill = paste0(input$quarter, " ", as.character(Year)),
                                          text = paste("Visits:", scales::comma(round(Visits))))) +
      geom_bar(position = "dodge", stat = "identity", width = 0.6) +
      scale_fill_manual(values = c("#002940", "#00AEF6")) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
      labs(title = "Visits by Day of the Week", x = "Day of the Week", y = "Visits (Thousands)") +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "#fff"),
        axis.text = element_text(color = "#fff"),
        axis.title = element_text(color = "#fff"),
        axis.title.x = element_blank(),
        legend.position = "top",
      )
    
    # convert to plotly
    ggplotly(plotDoW, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#fff", family = "Inter"),
        title = list(font = list(family = "Roboto Mono", color = "#fff")),
        legend = list(
          font = list(size = 12),
          title = "",
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.2, yanchor = "top"
        ),
        yaxis = list(fixedrange = TRUE, gridcolor = "#4f4f4f", showline=TRUE, linewidth=1, linecolor='#4f4f4f',mirror=TRUE),
        xaxis = list(gridcolor = "#4f4f4f", showline=TRUE, linewidth=1, linecolor='#4f4f4f',mirror=TRUE)
      )
  })
  
  
  # Visitor Time of Day Reactivity
  visitorTimeData = reactive({
    visitorTimeFiltered = ff_time_of_day %>%
      dplyr::filter((Quarter == input$quarter) & (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))
  })
  
  # Visitor Time of Day Chart
  output$visitorToD = renderPlotly({
    # generate the plot
    plotToD = ggplot(visitorTimeData(), aes(x = factor(Time, levels = c("Early Morning: 12am - 6am", "Morning: 6am - 12pm", "Afternoon: 12pm - 6pm", "Evening: 6pm - 12am")),
                                           y = Visits, fill = paste0(input$quarter, " ", as.character(Year)),
                                           text = paste("Visits:", scales::comma(round(Visits))))) +
      geom_bar(position = "dodge", stat = "identity", width = 0.7) +
      scale_fill_manual(values = c("#002A41", "#00AEF3")) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
      labs(title = "Visits by Time of Day", x = "Time of Day", y = "Visits (Thousands)") +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "#fff"),
        axis.text = element_text(color = "#fff"),
        axis.text.x = element_text(angle = 10, vjust = 0.5, hjust = 1),
        axis.title = element_text(color = "#fff"),
        axis.title.x = element_blank(),
        legend.position = "top", 
      )
    
    # convert to a plotly
    ggplotly(plotToD, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#fff", family = "Inter"),
        title = list(font = list(family = "Roboto Mono", color = "#fff")),
        legend = list(
          font = list(size = 12),
          title = "",
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.2, yanchor = "top"
        ),
        yaxis = list(fixedrange = TRUE, gridcolor = "#4f4f4f", showline=TRUE, linewidth=1, linecolor='#4f4f4f',mirror=TRUE),
        xaxis = list(gridcolor = "#4f4f4f", showline=TRUE, linewidth=1, linecolor='#4f4f4f',mirror=TRUE)
      )
  })
  
  
  
  # BUSINESS PROFILE
  
  output$businessMap <- renderMaplibre({
    maplibre(style = carto_style("dark-matter"),
             center = c(-79.381070, 43.656183),
             zoom = 14) |> 
      add_fill_layer(id = "bia_boundary",
                     source = bia,
                     fill_color = "#00AEF6",
                     fill_opacity = 0.5) |> 
      add_circle_layer(
        id = "business-layer",
        source = ms_businesses,
        circle_color = match_expr(
          "Group",
          values = c("Retail", "Services and Other", "Food and Drink"),
          stops = c("#F03838", "#00AEF6", "#43B171")
        ),
        circle_radius = 4,
        circle_stroke_color = "#ffffff",
        circle_stroke_width = 1,
        circle_opacity = 0.8,
        tooltip = "Group",
        hover_options = list(circle_radius = 8)
      ) |>
      add_categorical_legend(
        legend_title = "Business Types",
        values = c("Retail", "Services and Other", "Food and Drink"),
        colors = c("#F03838", "#00AEF6", "#43B171"),
        circular_patches = TRUE,
        position = "bottom-left",
        unique_id = "legend"
      )
  })
  

  
  output$businessTypes2 <- renderEcharts4r({
    business_types |>
      e_charts(group) |>
      e_pie(count, 
            radius = c("30%", "70%"),
            label = list(show = FALSE),
            labelLine = list(show = FALSE)) |>
      e_title("Main Street Business Types", left = "center", textStyle = list(color = "#fff")) |>
      e_tooltip(
        trigger = "item",
        formatter = htmlwidgets::JS("function(params){ return params.name + ': ' + params.value; }")
      ) |>
      e_legend(
        orient = "vertical", 
        left = "left", 
        bottom = 0, 
        textStyle = list(color = "#fff", fontSize = 12)) |>
      e_text_style(color = "#ffffff", fontFamily = "Inter") |>
      e_color(c("#F03838", "#43B171", "#00AEF6"))
  })

  
  
  
  output$civicMap <- renderMaplibre({
    maplibre(style = carto_style("dark-matter"),
             center = c(-79.381070, 43.656183),
             zoom = 14) |> 
      add_fill_layer(id = "bia_boundary",
                     source = bia,
                     fill_color = "#00AEF6",
                     fill_opacity = 0.5) |> 
      add_circle_layer(
        id = "civic-layer",
        source = civic_geo,
        circle_color = match_expr(
          "Group",
          values = c("Arts and Culture", "Education", "Government and Community Services", "Recreation Facilities", "Health and Care Facilities"),
          stops = c("#DB3069", "#F45D09", "#8A4285", "#43B171","#00AEF6")
        ),
        circle_radius = 4,
        circle_stroke_color = "#ffffff",
        circle_stroke_width = 1,
        circle_opacity = 1,
        tooltip = "Group",
        hover_options = list(circle_radius = 8)
      ) |>
      add_categorical_legend(
        legend_title = "Civic Infrastructure  Types",
        values = c(  "Arts and Culture", "Education", "Government and Community Services", "Recreation Facilities", "Health and Care Facilities"),
        colors = c("#DB3069", "#F45D09", "#8A4285", "#43B171","#00AEF6"),
        circular_patches = TRUE,
        position = "bottom-left",
        unique_id = "legend"
      )
  })

  
  output$civicTypes2 <- renderEcharts4r({
    civic_types |>
      e_charts(group) |>
      e_pie(count, 
            radius = c("30%", "70%"),
            label = list(show = FALSE),
            labelLine = list(show = FALSE)) |>
      e_title("Civic Infrastructure Types", left = "center", textStyle = list(color = "#fff")) |>
      e_tooltip(
        trigger = "item",
        formatter = htmlwidgets::JS("function(params){ return params.name + ': ' + params.value; }")
      ) |>
      e_legend(
        orient = "vertical", 
        left = "left", 
        bottom = 0, 
        textStyle = list(color = "#fff", fontSize = 12)) |>
      e_text_style(color = "#ffffff", fontFamily = "Inter") |>
      e_color(c("#DB3069", "#F45D09", "#8A4285", "#00AEF6", "#43B171"))
  })
  
  
# HOUSING PROFILE
  
  output$housingConstruction <- renderEcharts4r({
    # Convert Construction Year to a factor with correct order
    year_levels <- c("Pre 1960", "1961-1980", "1981-1990", "1991-2000", 
                     "2001-2005", "2006-2010", "2011-2015", "2016-2021", "After 2021")
    
    housing_construction$`Construction Year` <- factor(housing_construction$`Construction Year`, 
                                                       levels = year_levels)
    
    # Create a wider format for the echarts4r visualization
    # Split the data by area
    downtown_data <- housing_construction %>%
      filter(Area == "Downtown Yonge") %>%
      rename(Downtown_Yonge = Percentage)
    
    toronto_data <- housing_construction %>%
      filter(Area == "Toronto CMA") %>%
      rename(Toronto_CMA = Percentage)
    
    # Join them together
    plot_data <- downtown_data %>%
      select(`Construction Year`, Downtown_Yonge) %>%
      left_join(
        toronto_data %>% select(`Construction Year`, Toronto_CMA),
        by = "Construction Year"
      )
    
    # Create the echarts4r chart
    plot_data %>%
      e_charts(`Construction Year`) %>%
      e_bar(Downtown_Yonge, name = "Downtown Yonge", 
            itemStyle = list(color = "#00AEF6")) %>%  # light blue
      e_bar(Toronto_CMA, name = "Toronto CMA", 
            itemStyle = list(color = "#002A41")) %>%  # dark blue
      e_tooltip(trigger = "axis") %>%
      e_title("Housing Construction by Year and Area", textStyle = list(color = "#fff")) %>%
      e_y_axis(name = "Percentage (%)", axisLine = list(lineStyle = list(color = "#4f4f4f")), splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_x_axis(name = "Housing", axisLine = list(lineStyle = list(color = "#4f4f4f")), splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal", 
        left = "left", 
        bottom = 0, 
        textStyle = list(color = "#fff", fontSize = 12)) %>%
      e_text_style(color = "#ffffff", fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })
  
  output$housingType <- renderEcharts4r({
    
    # Create a wider format for the echarts4r visualization
    # Split the data by area
    downtown_data <- housing_type %>%
      filter(Area == "Downtown Yonge") %>%
      rename(Downtown_Yonge = Percentage)
    
    toronto_data <- housing_type %>%
      filter(Area == "Toronto CMA") %>%
      rename(Toronto_CMA = Percentage)
    
    # Join them together
    plot_data <- downtown_data %>%
      select(`Housing Type`, Downtown_Yonge) %>%
      left_join(
        toronto_data %>% select(`Housing Type`, Toronto_CMA),
        by = "Housing Type"
      )
    
    # Create the echarts4r chart
    plot_data %>%
      e_charts(`Housing Type`) %>%
      e_bar(Downtown_Yonge, name = "Downtown Yonge", 
            itemStyle = list(color = "#00AEF6")) %>%  # light blue
      e_bar(Toronto_CMA, name = "Toronto CMA", 
            itemStyle = list(color = "#002A41")) %>%  # dark blue
      e_tooltip(trigger = "axis") %>%
      e_title("Housing Type by Year and Area", textStyle = list(color = "#fff")) %>%
      e_y_axis(name = "Percentage (%)", axisLine = list(lineStyle = list(color = "#4f4f4f")), splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_x_axis(name = "Housing", axisLine = list(lineStyle = list(color = "#4f4f4f")), splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal", 
        left = "left", 
        bottom = 0, 
        textStyle = list(color = "#fff", fontSize = 12)) %>%
      e_text_style(color = "#ffffff", fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })
  
  
}


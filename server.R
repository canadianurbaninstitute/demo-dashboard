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
  
  color_mapping_business <- c("Retail" = "#F03838", "Services and Other" = "#00AEF6", "Food and Drink" = "#43B171")
  
  
  output$businessTypes <- renderPlotly({
      # Create the plot with a single x value for all bars to stack them
      plot_ly(
        data = business_types,
        x = rep("Business Types", nrow(business_types)),  # Single x value for all
        y = ~count,
        type = "bar",
        color = ~group,
        colors = color_mapping_business,
        text = ~paste(group, ":", count),
        hoverinfo = "text"
      ) %>%
        layout(
          barmode = "stack",
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = "#fff", family = "Inter"),
          legend = list(
            font = list(size = 12),
            title = "",
            orientation = "h",
            x = 0.5, xanchor = "center",
            y = -0.2, yanchor = "top"
          ),
          yaxis = list(title="", fixedrange = TRUE, gridcolor = "#4f4f4f", showline = TRUE, linewidth = 1, linecolor = '#4f4f4f', mirror = TRUE),
          xaxis = list(title="", gridcolor = "#4f4f4f", showline = TRUE, linewidth = 1, linecolor = '#4f4f4f', mirror = TRUE)
        ) %>%
        config(displayModeBar = FALSE)
    })
  
  color_mapping_civic <- c("Arts and Culture" = "#DB3069", "Education" = "#F45D09", "Government and Community Services" = "#8A4285", "Recreation Facilities" = "#43B171", "Healthcare" = "#00AEF6")
  
  
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
          values = c(  "Arts and Culture", "Education", "Government and Community Services", "Recreation Facilities", "Healthcare"),
          stops = c("#DB3069", "#F45D09", "#8A4285", "#43B171","#00AEF6")
        ),
        circle_radius = 4,
        circle_stroke_color = "#ffffff",
        circle_stroke_width = 1,
        circle_opacity = 0.8,
        tooltip = "Group",
        hover_options = list(circle_radius = 8)
      ) |>
      add_categorical_legend(
        legend_title = "Civic Infrastructure  Types",
        values = c(  "Arts and Culture", "Education", "Government and Community Services", "Recreation Facilities", "Healthcare"),
        colors = c("#DB3069", "#F45D09", "#8A4285", "#43B171","#00AEF6"),
        circular_patches = TRUE,
        position = "bottom-left",
        unique_id = "legend"
      )
  })
  
  output$civicTypes <- renderPlotly({
    # Create the plot with a single x value for all bars to stack them
    plot_ly(
      data = civic_types,
      x = rep("Civic Types", nrow(civic_types)),  # Single x value for all
      y = ~count,
      type = "bar",
      color = ~group,
      colors = color_mapping_civic,
      text = ~paste(group, ":", count),
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "stack",
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#fff", family = "Inter"),
        legend = list(
          font = list(size = 12),
          title = "",
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.2, yanchor = "top"
        ),
        yaxis = list(title="", fixedrange = TRUE, gridcolor = "#4f4f4f", showline = TRUE, linewidth = 1, linecolor = '#4f4f4f', mirror = TRUE),
        xaxis = list(title="", gridcolor = "#4f4f4f", showline = TRUE, linewidth = 1, linecolor = '#4f4f4f', mirror = TRUE)
      ) %>%
      config(displayModeBar = FALSE)
  })
}


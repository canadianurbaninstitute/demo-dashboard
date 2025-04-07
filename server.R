server <- function(input, output, session) {
  
  bs_themer()
  
  output$map <- renderMaplibre({
    maplibre(style = carto_style("dark-matter"),
             center = c(-79.381070, 43.656183),
             zoom = 14) |> 
      add_navigation_control()
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
      labs(x = "Date", y = "Visit Count") +
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
      config(displayModeBar = TRUE) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#fff", family = "Inter"),
        title = list(font = list(family = "Roboto Mono", color = "#fff")),
        yaxis = list(fixedrange = TRUE)
      )
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
      geom_bar(position = "stack", stat = "identity", width = 0.7, linewidth = 1) +
      scale_fill_manual(values = c("#FFD931", "#002940", "#00AEF6")) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
      labs(x = "Year", y = "Visits (Thousands)") +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "#fff"),
        axis.text = element_text(color = "#fff"),
        axis.title = element_text(color = "#fff"),
        axis.title.x = element_blank()
      )
    
    # convert to plotly
    ggplotly(plotVisitorType, tooltip = "text") %>%
      config(displayModeBar = TRUE) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#fff", family = "Inter"),
        title = list(font = list(family = "Roboto Mono", color = "#fff")),
        legend = list(
          font = list(size = 12),
          title = "",
          orientation = "h",
          x = 0.5,
          xanchor = "center"
        ),
        yaxis = list(fixedrange = TRUE)
      )
  })
  
}


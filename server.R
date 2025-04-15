server <- function(input, output, session) {
  
  bs_themer()
  
  # HOME ----
  
  output$map <- renderMaplibre({
    maplibre(style = carto_style("dark-matter"),
             center = c(-79.381070, 43.656183),
             zoom = 14.5,
             pitch = 45,
             bearing = -17) |> 
      add_vector_source(
        id = "openmaptiles",
        url = paste0("https://api.maptiler.com/tiles/v3-openmaptiles/tiles.json?key=y86lJ4kguNAzUQsF9r0N")
      ) |>
      add_fill_extrusion_layer(
        id = "3d-buildings",
        source = 'openmaptiles',
        source_layer = 'building',
        fill_extrusion_color = interpolate(
          column = 'render_height',
          values = c(0, 200, 400),
          stops = c('#222', '#444', '#666')
        ),
        fill_extrusion_height = list(
          'interpolate',
          list('linear'),
          list('zoom'),
          14,
          0,
          15,
          list('get', 'render_height')
        )
      ) |>
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
  
  
  
  
  # VISITOR TRENDS ----
  
  # Visitor Levels Plot
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
  
  output$visitorLevels <- renderEcharts4r({
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
  
  
  # Visitor Types Plot
  output$visitorTypes = renderEcharts4r({
    # filter the data based on the selected Quarter and Year
    visitorTypeFiltered = ff_type %>%
      dplyr::filter((Quarter == input$quarter) & (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))
    
    visitorTypePlot = visitorTypeFiltered %>%
      arrange(Year) %>%
      mutate(Quarter_Year = paste(Quarter, Year)) %>%
      select(Type, Quarter_Year, Visits)
    
    # Convert Visitor Type to a Factor
    type_levels = c("Resident", "Recurring Visitor", "Infrequent Visitor")
    
    visitorTypePlot = visitorTypePlot %>%
      mutate(Type = factor(Type, levels = type_levels))
    
    
    # plot the data using the echarts package
    visitorTypePlot %>%
      group_by(Type) %>%
      e_charts(Quarter_Year) %>%
      e_bar(Visits, stack = "Visits", bind = Visits) %>%
      e_color(c("#00AEF6", "#002940", "#FFD931")) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Visits by Type of Visitor", textStyle = list(color = "#fff")) %>%
      e_y_axis(name = "Visits", axisLine = list(lineStyle = list(color = "#4f4f4f")), splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_x_axis(name = "Year", axisLine = list(lineStyle = list(color = "#4f4f4f")), splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal", 
        left = "center", 
        bottom = 0, 
        textStyle = list(color = "#fff", fontSize = 12)) %>%
      e_text_style(color = "#ffffff", fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })
  
  
  # Visitors Day of Week Plot
  output$visitorDoW = renderEcharts4r({
    # filter the data based on the selected Quarter and Year
      visitorDayFiltered = ff_day_of_week %>%
        dplyr::filter((Quarter == input$quarter) & (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))
    
    visitorDayPlot = visitorDayFiltered %>%
      arrange(Year) %>%
      mutate(Quarter_Year = paste(Quarter, Year)) %>%
      select(Day, Quarter_Year, Visits)
    
    # Convert Day of week to a Factor
    day_of_week_levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    
    visitorDayPlot = visitorDayPlot %>%
      mutate(Day = factor(Day, levels = day_of_week_levels))
    
    
    # plot the data using the echarts package
    visitorDayPlot %>%
      group_by(Quarter_Year) %>%
      e_charts(Day) %>%
      e_bar(Visits) %>%
      e_color(c("#002940", "#00AEF6")) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Visits by Day of the Week", textStyle = list(color = "#fff")) %>%
      e_y_axis(name = "Visits", axisLine = list(lineStyle = list(color = "#4f4f4f")), splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_x_axis(name = "Year", axisLine = list(lineStyle = list(color = "#4f4f4f")), splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal", 
        left = "center", 
        bottom = 0, 
        textStyle = list(color = "#fff", fontSize = 12)) %>%
      e_text_style(color = "#ffffff", fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })
  
  
  # Visitor Time of Day Plot
  output$visitorToD = renderEcharts4r({
    # filter the data based on the selected Quarter and Year
    visitorTimeFiltered = ff_time_of_day %>%
      dplyr::filter((Quarter == input$quarter) & (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))
    
    visitorTimePlot = visitorTimeFiltered %>%
      arrange(Year) %>%
      mutate(Quarter_Year = paste(Quarter, Year)) %>%
      select(Time, Quarter_Year, Visits)
    
    # Convert Day of week to a Factor
    time_of_day_levels = c("Early Morning: 12am - 6am", "Morning: 6am - 12pm", "Afternoon: 12pm - 6pm", "Evening: 6pm - 12am")
    
    visitorTimePlot = visitorTimePlot %>%
      mutate(Time = factor(Time, levels = time_of_day_levels))
    
    
    # plot the data using the echarts package
    visitorTimePlot %>%
      group_by(Quarter_Year) %>%
      e_charts(Time) %>%
      e_bar(Visits) %>%
      e_color(c("#002940", "#00AEF6")) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Visits by Time of Day", textStyle = list(color = "#fff")) %>%
      e_y_axis(name = "Visits", axisLine = list(lineStyle = list(color = "#4f4f4f")), splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_x_axis(name = "Year", axisLine = list(lineStyle = list(color = "#4f4f4f")), splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal", 
        left = "center", 
        bottom = 0, 
        textStyle = list(color = "#fff", fontSize = 12)) %>%
      e_text_style(color = "#ffffff", fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })
  
  
  
  # BUSINESS PROFILE ----
  
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
  
  
  output$employmentSize <- renderMaplibre({
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
        circle_radius = step_expr(
          "EmpSzNm",
          values = c(0, 5, 10, 50, 100, 1000),
          stops = c(2, 4, 6, 8, 10, 12),
          base = 1 # Adjust sizes as needed
        ),
        circle_stroke_color = "#ffffff",
        circle_stroke_width = 1,
        circle_opacity = 0.8,
        tooltip = "Group",
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
  

  
  output$businessTypes <- renderEcharts4r({
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

  
  output$civicTypes <- renderEcharts4r({
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
  
  
  # HOUSING PROFILE ----
  
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
  
  
  ### URBAN FORM
  
  output$urbanFormMap <- renderMaplibre({
    maplibre(style = carto_style("dark-matter"),
             center = c(-79.381070, 43.656183),
             zoom = 14.5,
             pitch = 45,
             bearing = -17) |> 
      add_vector_source(
        id = "openmaptiles",
        url = paste0("https://api.maptiler.com/tiles/v3-openmaptiles/tiles.json?key=y86lJ4kguNAzUQsF9r0N")
      ) |>
      add_fill_extrusion_layer(
        id = "3d-buildings",
        source = 'openmaptiles',
        source_layer = 'building',
        fill_extrusion_color = interpolate(
          column = 'render_height',
          values = c(0, 200, 400),
          stops = c('#222', '#444', '#666')
        ),
        fill_extrusion_height = list(
          'interpolate',
          list('linear'),
          list('zoom'),
          14,
          0,
          15,
          list('get', 'render_height')
        )
      ) |>
      add_line_layer(
        id = "transit",
        source = 'openmaptiles',
        source_layer = 'transportation',
        filter = list("in", "subclass", "tram", "subway", "rail"),
        line_color = "#F03838",
        line_opacity = 0.7
      ) |>
      add_fill_layer(
        id = "greenspace",
        source = "openmaptiles",
        source_layer = "landcover",
        fill_color = "#43B171",
        fill_opacity = 0.5,
        filter = list("==", "class", "grass")
      ) |>
      add_fill_layer(id = "bia_boundary",
                     source = bia,
                     fill_color = "#00AEF6",
                     fill_opacity = 0.5) |>
      add_categorical_legend(
        legend_title = "Legend",
        values = c("Downtown Yonge BIA", "Transit", "Green Space"),
        colors = c("#00AEF6", "#F03838", "#43B171"),
        circular_patches = FALSE,
        position = "bottom-left",
        unique_id = "legend"
      ) 
  }) 
  
  output$commuteMode <- renderEcharts4r({
    
    # Prepare the data
    downtown_data <- commute %>%
      filter(Area == "Downtown Yonge") %>%
      rename(Downtown_Yonge = weighted_mean)
    
    toronto_data <- commute %>%
      filter(Area == "Toronto CMA") %>%
      rename(Toronto_CMA = weighted_mean)
    
    # Merge for grouped format
    plot_data <- downtown_data %>%
      select(variable, Downtown_Yonge) %>%
      left_join(
        toronto_data %>% select(variable, Toronto_CMA),
        by = "variable"
      )
    
    # Create the chart
    plot_data %>%
      e_charts(variable) %>%
      e_bar(Downtown_Yonge, name = "Downtown Yonge",
            itemStyle = list(color = "#00AEF6")) %>%
      e_bar(Toronto_CMA, name = "Toronto CMA",
            itemStyle = list(color = "#002A41")) %>%
      e_tooltip(
        trigger = "axis"
      ) %>%
      e_title("Commute Mode by Area", textStyle = list(color = "#fff")) %>%
      e_y_axis(name = "Percentage (%)",
               axisLine = list(lineStyle = list(color = "#4f4f4f")),
               splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_x_axis(name = "Commute Mode",
               axisLine = list(lineStyle = list(color = "#4f4f4f")),
               splitLine = list(lineStyle = list(color = "#4f4f4f"))) %>%
      e_legend(orient = "horizontal", left = "left", bottom = 0,
               textStyle = list(color = "#fff", fontSize = 12)) %>%
      e_grid(containLabel = TRUE) %>%
      e_text_style(color = "#ffffff", fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })
  
  
  
}


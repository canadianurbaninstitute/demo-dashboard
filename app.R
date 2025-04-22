library(shiny)
library(bslib)
library(plotly)
library(mapgl)
library(tidyverse)
library(sf)
library(echarts4r)
library(jsonlite)


theme_json_string <- paste(readLines("theme/demo_theme.json", warn = FALSE), collapse = "\n")
cat(theme_json_string)





# load in the necessary data

# Visitor Data

ff_overall <- read_csv("./data/ff_monthly.csv") %>%
  select(-...1) %>%
  rename("Date" = date)

ff_day_of_week <- read_csv("./data/ff_day_of_week.csv") %>%
  select(-...1)

ff_time_of_day <- read_csv("./data/ff_time_of_day.csv") %>%
  select(-...1)

ff_type <- read_csv("./data/ff_vis_type.csv") %>%
  select(-...1) %>%
  rename("Visits" = Count)

# BIA

name <- "Downtown Yonge BIA"

bia <- st_read("./data/downtown_yonge.geojson")

toronto_boundary <- st_read("./data/toronto_boundary.geojson")


# Business Data

ms_businesses <- st_read("./data/ms_businesses.geojson")

business_types <- read_csv("./data/business_types.csv")

mytheme <- bs_theme(
  preset = "bootstrap", version = "5", bg = "#222", fg = "#fff",
  primary = "#00AEF6", secondary = "#DB3069", success = "#43B171", danger = "#F03838",
  warning = "#FFD931", base_font = font_google("Inter"), heading_font = font_google("Roboto Mono"),
  navbar_bg = "#00AEF6"
)

# Civic Data

civic_types <- read_csv("./data/civic_types.csv")

civic_geo <- st_read("./data/civic.geojson")


# Housing Data

housing_construction <- read_csv("./data/housing_construction.csv")

housing_type <- read_csv("./data/housing_type.csv")

# Urban Form Data

commute <- read_csv("./data/commute.csv")


# Demographic Data

neighbourhood_demos <- read_csv("./data/demographics.csv") %>%
  select(-...1)


#### UI

ui <- page_navbar(
  title = "Downtown Yonge BIA Dashboard Demo",
  navbar_options = navbar_options(position = "fixed-top", underline = FALSE),
  theme = mytheme |> bs_add_rules("
                    h1 { font-family: 'Inter', sans-serif; }
                  "),
  header = tags$head(
    # Your existing custom CSS styles
    tags$style(HTML(
      "body { padding-top: 70px!important}
       .nav-pills {
         border: 1px solid #494949;
         padding: 0.5em;
         border-radius: 1em;
         margin-bottom: 1em;}
       #legend, .maplibregl-popup-content {background-color: #222!important;}
       .maplibregl-popup-tip {border-top-color: #222!important;}"
    )),
    e_theme_register("theme/demo_theme.json", name = "chartTheme"),
    e_common(theme = "chartTheme")
  ), # End of header

  # MOVE THIS INTO EXTERNAL CSS ^

  nav_panel(
    title = "Home",
    h1(name),
    p("Weclome to the Downtown Yonge BIA Main Street Metrics Dashboard! This dashboard gives you a quick snapshot of the BIA. On this page, you’ll find an interactive map alongside highlights from the latest quarter. To explore more, head to the Visitors page for insights on foot traffic and visitor demographics, or visit the Neighbourhood Profile for a deeper look at the area's characteristics."),
    layout_column_wrap(
      width = 1 / 2,
      card(
        full_screen = FALSE,
        card_body(
          class = "p-0",
          maplibreOutput("map")
        )
      ),
      card(
        full_screen = FALSE,
        card_body(
          class = "p-0",
          maplibreOutput("map1")
        )
      )
    ),
    h2("2024 Q4 Highlights"),
    layout_columns(
      value_box(
        title = "Number of Visitors", value = "28,000 (+5%)",
        theme = "success", showcase = bsicons::bs_icon("arrow-up"),
        showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
        height = NULL
      ),
      value_box(
        title = "Busiest Day of Week", value = "Tuesday", theme = "primary",
        showcase = bsicons::bs_icon("calendar"), showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE, height = NULL
      ),
      value_box(
        title = "Busiest Time of Day", value = "12 - 6 PM", theme = "secondary",
        showcase = bsicons::bs_icon("clock"), showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE, height = NULL
      ),
    )
  ),
  nav_panel(
    title = "Visitors",
    h1(name),
    navset_pill(
      nav_panel(
        title = "Visitor Trends",
        card(
          card_body(
            p("This chart measures the number of pedestrian visits to the BIA on a monthly basis from 2021."),
            echarts4rOutput("visitorLevels")
          )
        ),
        h3("Visitor Info"),
        layout_sidebar(
          sidebar = sidebar(
            selectInput(
              "year",
              "Select Year",
              choices = c("2023", "2024"),
              selected = "2024",
              selectize = TRUE
            ),
            selectInput(
              "quarter",
              "Select Quarter",
              choices = c("Q1", "Q2", "Q3", "Q4"),
              selected = "Q4",
              selectize = TRUE
            )
          ),
          card(
            card_body(
              p("The chart shows the number and type of visitors (resident, recurring, infrequent) for the current quarter and the same quarter from the previous year."),
              echarts4rOutput("visitorTypes")
            )
          ),
          card(
            card_body(
              p("The chart compares visits by day of the week for the current quarter and the same quarter from the previous year."),
              echarts4rOutput("visitorDoW")
            )
          ),
          card(
            card_body(
              p("The chart compares visits by time of the day for the current quarter and the same quarter from the previous year."),
              echarts4rOutput("visitorToD")
            )
          )
        )
      ),
      nav_panel(
        title = "Visitor Demographics"
      )
    )
  ),
  nav_panel(
    "Neighbourhood Profile",
    h1(name),
    navset_pill(
      nav_panel(
        title = "Business",
        h3("Business Distribution"),
        p("The business distribution map shows the location and type of main street businesses across Downtown Yonge."),
        layout_column_wrap(
          width = 1 / 2,
          card(
            full_screen = FALSE,
            card_body(
              class = "p-0",
              maplibreOutput("businessMap")
            )
          ),
          card(
            card_body(
              echarts4rOutput("businessTypes")
            )
          ),
        ),
        layout_columns(
          value_box(
            title = "Number of Main Street Businesses", value = "581",
            theme = "primary", showcase = bsicons::bs_icon("shop"),
            showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
            height = NULL
          ),
          value_box(
            title = "Independent Business Index", value = "0.54",
            theme = "warning", showcase = bsicons::bs_icon("speedometer"),
            showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
            height = NULL
          )
        )
      ),
      nav_panel(
        title = "Civic Infrastructure",
        h3("Civic Infrastructure Distribution"),
        p("The civic infrastructure distribution map shows the location and type of civic infrastructure across Downtown Yonge."),
        layout_column_wrap(
          width = 1 / 2,
          card(
            full_screen = FALSE,
            card_body(
              class = "p-0",
              maplibreOutput("civicMap")
            )
          ),
          card(
            card_body(
              echarts4rOutput("civicTypes")
            )
          )
        ),
        layout_columns(
          value_box(
            title = "Number of Civic Infrastructure Locations", value = "176",
            theme = "primary", showcase = bsicons::bs_icon("building"),
            showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
            height = NULL
          ),
          value_box(
            title = "Complete Community Score", value = "75",
            theme = "success", showcase = bsicons::bs_icon("speedometer"),
            showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
            height = NULL
          )
        )
      ),
      nav_panel(
        title = "Housing",
        h3("Housing"),
        card(
          card_body(
            p("The housing construction chart shows the share of housing units built in different time periods for Downtown Yonge and Toronto CMA."),
            echarts4rOutput("housingConstruction")
          )
        ),
        card(
          card_body(
            p("The housing type chart breaks down the percentage of each dwelling type, such as apartments, single-detached, and duplexes."),
            echarts4rOutput("housingType")
          )
        )
      ),
      nav_panel(
        title = "Urban Form",
        h3("Urban Form Map"),
        p("The urban form map displays Downtown Yonge’s boundary, nearby transit lines, and green spaces."),
        card(
          full_screen = FALSE,
          card_body(
            class = "p-0",
            maplibreOutput("urbanFormMap")
          )
        ),
        h3("Commute"),
        card(
          card_body(
            p("The commute mode chart shows how people in Downtown Yonge and Toronto CMA commute to work by walking, public transit, car, or bike."),
            echarts4rOutput("commuteMode")
          )
        )
      ),
      nav_panel(
        title = "Employment",
        h3("Employment Size"),
        p("The employment size map shows business locations by type and their approximate employment size."),
        card(
          full_screen = FALSE,
          card_body(
            class = "p-0",
            maplibreOutput("employmentSize")
          )
        ),
        p("The employment occupation chart highlights the share of population in different job sectors."),
        layout_column_wrap(
          width = 1 / 2,
          card(
            card_body(
              echarts4rOutput("demoOccupation")
            )
          ),
          card(
            card_body(
              echarts4rOutput("demoOccupationCMA")
            )
          )
        )
      ),
      nav_panel(
        title = "Neighbourhood Demographics",
        h3("Demographic Highlights"),
        layout_columns(
          value_box(
            title = "Average Household Income", value = "$82,000",
            theme = "primary", showcase = bsicons::bs_icon("cash"),
            showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
            height = NULL
          ),
          value_box(
            title = "Average Household Size", value = "1.7",
            theme = "orange", showcase = bsicons::bs_icon("diagram-2-fill"),
            showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
            height = NULL
          ),
          value_box(
            title = "Average Age", value = "28",
            theme = "secondary", showcase = bsicons::bs_icon("calendar-fill"),
            showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
            height = NULL
          )
        ),
        layout_columns(
          uiOutput("VisMinBox"),
          uiOutput("IndigBox"),
          uiOutput("ImmOutput")
        ),
        h3("Demographic Breakdown"),
        card(
          full_screen = FALSE,
          card_body(
            p("The population pyramid shows the age and gender distribution of residents."),
            echarts4rOutput("demoAge")
          )
        ),
        p("The census family structure chart compares Downtown Yonge and Toronto CMA by type of family (e.g., married, common-law)."),
        layout_column_wrap(
          width = 1 / 2,
          card(
            card_body(
              echarts4rOutput("demoFamily")
            )
          ),
          card(
            card_body(
              echarts4rOutput("demoFamilyCMA")
            )
          )
        ),
        card(
          full_screen = FALSE,
          card_body(
            p("The household income chart displays the percentage of households in each income bracket for Downtown Yonge vs. Toronto CMA."),
            echarts4rOutput("demoIncome")
          )
        )
      )
    )
  ),
  nav_spacer()
)


#### SERVER

server <- function(input, output, session) {
  # bs_themer()

  # HOME ----

  output$map <- renderMaplibre({
    maplibre(
      style = carto_style("dark-matter"),
      center = c(-79.381070, 43.656183),
      zoom = 14.5,
      pitch = 45,
      bearing = -17
    ) |>
      add_vector_source(
        id = "openmaptiles",
        url = paste0("https://api.maptiler.com/tiles/v3-openmaptiles/tiles.json?key=y86lJ4kguNAzUQsF9r0N")
      ) |>
      add_fill_extrusion_layer(
        id = "3d-buildings",
        source = "openmaptiles",
        source_layer = "building",
        fill_extrusion_color = interpolate(
          column = "render_height",
          values = c(0, 200, 400),
          stops = c("#444", "#666", "#999")
        ),
        fill_extrusion_height = list(
          "interpolate",
          list("linear"),
          list("zoom"),
          14,
          0,
          15,
          list("get", "render_height")
        ),
        fill_extrusion_opacity = 0.8
      ) |>
      add_fill_layer(
        id = "bia_boundary",
        source = bia,
        fill_color = "#00AEF6",
        fill_opacity = 0.5
      ) |>
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
    maplibre(
      style = carto_style("dark-matter"),
      center = c(-79.381070, 43.6561),
      zoom = 11,
      bearing = -17
    ) |>
      add_fill_layer(
        id = "bia_boundary",
        source = bia,
        fill_color = "#00AEF6",
        fill_opacity = 0.5
      ) |>
      add_line_layer(
        id = "toronto_boundary",
        source = toronto_boundary,
        line_color = "#eee",
        line_opacity = 0.7
      )
  })


  # VISITOR TRENDS ----


  # Visitor Levels Plot
  visitorLevelsData <- reactive({
    req(input$quarter)
    # set the filter conditions based on the selected year and quarter
    if (input$quarter == "Q1") {
      target_end_date <- ymd(paste0(input$year, "03", "31"))
    } else if (input$quarter == "Q2") {
      target_end_date <- ymd(paste0(input$year, "06", "30"))
    } else if (input$quarter == "Q3") {
      target_end_date <- ymd(paste0(input$year, "09", "30"))
    } else {
      target_end_date <- ymd(paste0(input$year, "12", "31"))
    }

    visitorLevelsFiltered <- ff_overall %>%
      dplyr::filter(Date < target_end_date) %>%
      mutate(Percentage = round(Percentage, 2))
  })

  output$visitorLevels <- renderEcharts4r({
    req(visitorLevelsData())
    visitorLevelsData() %>%
      dplyr::mutate(Date = as.Date(Date)) %>%
      dplyr::arrange(Date) %>%
      e_charts(Date) %>%
      e_line(serie = Count, name = "Visits") %>%
      e_theme("chartTheme") %>%
      e_x_axis(
        type = "time",
        min = "2021-01-01",
        max = "2025-01-01",
      ) %>%
      e_y_axis(
        name = "Visits",
        max = "10000000",
      ) %>%
      e_tooltip(trigger = "axis") %>%
      e_text_style(fontFamily = "Inter") %>%
      e_legend(show = FALSE) %>%
      e_datazoom(type = "slider", textStyle = list(color = "#fff"), moveHandleStyle = list(color = "#72CCFE"), emphasis = list(moveHandleStyle = list(color = "#72CCFE"))) %>%
      e_title("Monthly Visits since 2021") %>%
      e_toolbox_feature(feature = "saveAsImage") 
  })


  # Visitor Types Plot
  output$visitorTypes <- renderEcharts4r({
    # filter the data based on the selected Quarter and Year
    visitorTypeFiltered <- ff_type %>%
      dplyr::filter((Quarter == input$quarter) & (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))

    visitorTypePlot <- visitorTypeFiltered %>%
      arrange(Year) %>%
      mutate(Quarter_Year = paste(Quarter, Year)) %>%
      select(Type, Quarter_Year, Visits)

    # Convert Visitor Type to a Factor
    type_levels <- c("Resident", "Recurring Visitor", "Infrequent Visitor")

    visitorTypePlot <- visitorTypePlot %>%
      mutate(Type = factor(Type, levels = type_levels))


    # plot the data using the echarts package
    visitorTypePlot %>%
      group_by(Type) %>%
      e_charts(Quarter_Year) %>%
      e_bar(Visits, stack = "Visits", bind = Visits) %>%
      e_color(c("#00AEF6", "#DB3069", "#43B171")) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Visits by Type of Visitor") %>%
      e_y_axis(name = "Visits",  ) %>%
      e_x_axis(name = "Year",  ) %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal",
        left = "center",
        bottom = 0
      ) %>%
      e_text_style(fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })


  # Visitors Day of Week Plot
  output$visitorDoW <- renderEcharts4r({
    # filter the data based on the selected Quarter and Year
    visitorDayFiltered <- ff_day_of_week %>%
      dplyr::filter((Quarter == input$quarter) & (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))

    visitorDayPlot <- visitorDayFiltered %>%
      arrange(Year) %>%
      mutate(Quarter_Year = paste(Quarter, Year)) %>%
      select(Day, Quarter_Year, Visits)

    # Convert Day of week to a Factor
    day_of_week_levels <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

    visitorDayPlot <- visitorDayPlot %>%
      mutate(Day = factor(Day, levels = day_of_week_levels))


    # plot the data using the echarts package
    visitorDayPlot %>%
      group_by(Quarter_Year) %>%
      e_charts(Day) %>%
      e_bar(Visits) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Visits by Day of the Week") %>%
      e_y_axis(name = "Visits",  ) %>%
      e_x_axis(name = "Year",  ) %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal",
        left = "center",
        bottom = 0
      ) %>%
      e_text_style(fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })


  # Visitor Time of Day Plot
  output$visitorToD <- renderEcharts4r({
    # filter the data based on the selected Quarter and Year
    visitorTimeFiltered <- ff_time_of_day %>%
      dplyr::filter((Quarter == input$quarter) & (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))

    visitorTimePlot <- visitorTimeFiltered %>%
      arrange(Year) %>%
      mutate(Quarter_Year = paste(Quarter, Year)) %>%
      select(Time, Quarter_Year, Visits)

    # Convert Day of week to a Factor
    time_of_day_levels <- c("Early Morning: 12am - 6am", "Morning: 6am - 12pm", "Afternoon: 12pm - 6pm", "Evening: 6pm - 12am")

    visitorTimePlot <- visitorTimePlot %>%
      mutate(Time = factor(Time, levels = time_of_day_levels))


    # plot the data using the echarts package
    visitorTimePlot %>%
      group_by(Quarter_Year) %>%
      e_charts(Time) %>%
      e_bar(Visits) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Visits by Time of Day") %>%
      e_y_axis(name = "Visits",  ) %>%
      e_x_axis(name = "Year",  ) %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal",
        left = "center",
        bottom = 0
      ) %>%
      e_text_style(fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })

  ## VISITOR DEMOGRAPHICS ----

  # NEIGHBOURHOOD PROFILE ----

  ## BUSINESS PROFILE ----

  output$businessMap <- renderMaplibre({
    maplibre(
      style = carto_style("dark-matter"),
      center = c(-79.381, 43.657),
      zoom = 14
    ) |>
      add_fill_layer(
        id = "bia_boundary",
        source = bia,
        fill_color = "#00AEF6",
        fill_opacity = 0.5
      ) |>
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
    maplibre(
      style = carto_style("dark-matter"),
      center = c(-79.381, 43.657),
      zoom = 14
    ) |>
      add_fill_layer(
        id = "bia_boundary",
        source = bia,
        fill_color = "#00AEF6",
        fill_opacity = 0.5
      ) |>
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
        labelLine = list(show = FALSE)
      ) |>
      e_title("Main Street Business Types", left = "center") |>
      e_tooltip(
        trigger = "item",
        formatter = htmlwidgets::JS("function(params){ return params.name + ': ' + params.value; }")
      ) |>
      e_legend(
        orient = "vertical",
        left = "left",
        bottom = 0
      ) |>
      e_text_style(fontFamily = "Inter") |>
      e_color(c("#F03838", "#43B171", "#00AEF6")) |>
      e_toolbox_feature(feature = "saveAsImage") 
  })


  output$civicMap <- renderMaplibre({
    maplibre(
      style = carto_style("dark-matter"),
      center = c(-79.381, 43.657),
      zoom = 14
    ) |>
      add_fill_layer(
        id = "bia_boundary",
        source = bia,
        fill_color = "#00AEF6",
        fill_opacity = 0.5
      ) |>
      add_circle_layer(
        id = "civic-layer",
        source = civic_geo,
        circle_color = match_expr(
          "Group",
          values = c("Arts and Culture", "Education", "Government and Community Services", "Recreation Facilities", "Health and Care Facilities"),
          stops = c("#DB3069", "#F45D09", "#8A4285", "#43B171", "#00AEF6")
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
        values = c("Arts and Culture", "Education", "Government and Community Services", "Recreation Facilities", "Health and Care Facilities"),
        colors = c("#DB3069", "#F45D09", "#8A4285", "#43B171", "#00AEF6"),
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
        labelLine = list(show = FALSE)
      ) |>
      e_title("Civic Infrastructure Types", left = "center") |>
      e_tooltip(
        trigger = "item",
        formatter = htmlwidgets::JS("function(params){ return params.name + ': ' + params.value; }")
      ) |>
      e_legend(
        orient = "vertical",
        left = "left",
        bottom = 0
      ) |>
      e_text_style(fontFamily = "Inter") |>
      e_color(c("#DB3069", "#F45D09", "#8A4285", "#00AEF6", "#43B171")) |>
      e_toolbox_feature(feature = "saveAsImage") 
  })


  ## HOUSING PROFILE ----

  output$housingConstruction <- renderEcharts4r({
    # Convert Construction Year to a factor with correct order
    year_levels <- c(
      "Pre 1960", "1961-1980", "1981-1990", "1991-2000",
      "2001-2005", "2006-2010", "2011-2015", "2016-2021", "After 2021"
    )

    housing_construction$`Construction Year` <- factor(housing_construction$`Construction Year`,
      levels = year_levels
    )

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
      e_bar(Downtown_Yonge,
        name = "Downtown Yonge",
        itemStyle = list(color = "#00AEF6")
      ) %>% # light blue
      e_bar(Toronto_CMA,
        name = "Toronto CMA",
        itemStyle = list(color = "#DB3069")
      ) %>% # dark blue
      e_tooltip(trigger = "axis") %>%
      e_title("Housing Construction by Year and Area") %>%
      e_y_axis(name = "Percentage (%)",  ) %>%
      e_x_axis(name = "Housing",  ) %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal",
        left = "left",
        bottom = 0
      ) %>%
      e_text_style(fontFamily = "Inter") %>%
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
      e_bar(Downtown_Yonge,
        name = "Downtown Yonge",
        itemStyle = list(color = "#00AEF6")
      ) %>% # light blue
      e_bar(Toronto_CMA,
        name = "Toronto CMA",
        itemStyle = list(color = "#DB3069")
      ) %>% # dark blue
      e_tooltip(trigger = "axis") %>%
      e_title("Housing Type by Year and Area") %>%
      e_y_axis(name = "Percentage (%)",  ) %>%
      e_x_axis(name = "Housing",  ) %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal",
        left = "left",
        bottom = 0
      ) %>%
      e_text_style(fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })


  ## URBAN FORM ----

  output$urbanFormMap <- renderMaplibre({
    maplibre(
      style = carto_style("dark-matter"),
      center = c(-79.381070, 43.656183),
      zoom = 14.5,
      pitch = 45,
      bearing = -17
    ) |>
      add_vector_source(
        id = "openmaptiles",
        url = paste0("https://api.maptiler.com/tiles/v3-openmaptiles/tiles.json?key=y86lJ4kguNAzUQsF9r0N")
      ) |>
      add_fill_extrusion_layer(
        id = "3d-buildings",
        source = "openmaptiles",
        source_layer = "building",
        fill_extrusion_color = interpolate(
          column = "render_height",
          values = c(0, 200, 400),
          stops = c("#444", "#666", "#999")
        ),
        fill_extrusion_height = list(
          "interpolate",
          list("linear"),
          list("zoom"),
          14,
          0,
          15,
          list("get", "render_height")
        ),
        fill_extrusion_opacity = 0.8
      ) |>
      add_line_layer(
        id = "transit",
        source = "openmaptiles",
        source_layer = "transportation",
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
      add_fill_layer(
        id = "bia_boundary",
        source = bia,
        fill_color = "#00AEF6",
        fill_opacity = 0.5
      ) |>
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
      e_bar(Downtown_Yonge,
        name = "Downtown Yonge",
        itemStyle = list(color = "#00AEF6")
      ) %>%
      e_bar(Toronto_CMA,
        name = "Toronto CMA",
        itemStyle = list(color = "#DB3069")
      ) %>%
      e_tooltip(
        trigger = "axis"
      ) %>%
      e_title("Commute Mode by Area") %>%
      e_y_axis(
        name = "Percentage (%)",
        
        
      ) %>%
      e_x_axis(
        name = "Commute Mode",
        
        
      ) %>%
      e_legend(
        orient = "horizontal", left = "left", bottom = 0
      ) %>%
      e_grid(containLabel = TRUE) %>%
      e_text_style(fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })


  ## NEIGHBOURHOOD DEMOGRAPHICS ----

  # Demographic Chloropleth Map
  output$demoMap <- renderMaplibre({

  })


  # Demographic Summary Boxes

  # Visible Minority Status
  output$VisMinBox <- renderUI({
    vismin_filtered <- neighbourhood_demos %>%
      select(Area, `Visible Minority Total`) %>%
      filter(Area == "Downtown Yonge")

    value_box(
      title = "Visible Minorities",
      value = paste(round(vismin_filtered$`Visible Minority Total`[1], 0), " %"),
      theme = "danger",
      showcase_layout = "left center",
      showcase = bsicons::bs_icon("people-fill"),
      full_screen = FALSE,
      fill = TRUE,
      height = NULL
    )
  })

  # Indigenous Identity
  output$IndigBox <- renderUI({
    indig_filtered <- neighbourhood_demos %>%
      select(Area, `Indigenous Identity`) %>%
      filter(Area == "Downtown Yonge")

    value_box(
      title = "Indigenous Population",
      value = paste(round(indig_filtered$`Indigenous Identity`[1], 0), " %"),
      theme = "success",
      showcase_layout = "left center",
      showcase = bsicons::bs_icon("person-fill"),
      full_screen = FALSE,
      fill = TRUE,
      height = NULL
    )
  })

  # Immigration Status
  output$ImmOutput <- renderUI({
    imm_filtered <- neighbourhood_demos %>%
      select(Area, Immigrants) %>%
      filter(Area == "Downtown Yonge")

    value_box(
      title = "Immigrants",
      value = paste(round(imm_filtered$Immigrants[1], 0), " %"),
      theme = "warning",
      showcase_layout = "left center",
      showcase = bsicons::bs_icon("globe-americas"),
      full_screen = FALSE,
      fill = TRUE,
      height = NULL
    )
  })

  # Population Period
  output$demoAge <- renderEcharts4r({
    # filter and clean the gender age data
    demoAgeData <- neighbourhood_demos %>%
      select(Area, (20:55)) %>%
      filter(Area == "Downtown Yonge") %>%
      pivot_longer(!Area, names_to = "Groups", values_to = "percentage") %>%
      separate(Groups, into = c("gender", "age_group"), sep = " ", extra = "merge") %>%
      mutate(
        age_group = str_trim(age_group),
        percentage = if_else(gender == "Females", percentage * -1, percentage)
      )

    # convert age_group to a factor
    age_levels <- c(
      "0 To 4", "5 To 9", "10 To 14", "15 To 19", "20 To 24",
      "25 To 29", "30 To 34", "35 To 39", "40 To 44", "45 To 49",
      "50 To 54", "55 To 59", "60 To 64", "65 To 69", "70 To 74",
      "75 To 79", "80 To 84", "85 Or Older"
    )

    demoAgePlot <- demoAgeData %>%
      mutate(age_group = factor(age_group, levels = age_levels))

    # plot the data using the echarts package
    demoAgePlot %>%
      group_by(gender) %>%
      e_charts(age_group) %>%
      e_bar(percentage, stack = "percentage") %>%
      e_flip_coords() %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Population Pyramid") %>%
      e_y_axis(name = "% of Population") %>%
      e_x_axis(
        name = "Percentage of Population",
        axisLabel = list(
          formatter = htmlwidgets::JS("function(value){ return Math.abs(value); }")
        ),
        
        
      ) %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal",
        left = "left",
        bottom = 0
      ) %>%
      e_text_style(fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })

  output$demoFamily <- renderEcharts4r({
    neighbourhood_demos %>%
      filter(Area == "Downtown Yonge") %>%
      select(
        `Total Couple Families`,
        `Married Couple Families`,
        `Common Law Couple Families`,
        `Total Lone Parent Families`
      ) %>%
      pivot_longer(
        cols      = everything(),
        names_to  = "category",
        values_to = "percentage"
      ) %>%
      e_charts(category) %>%
      e_pie(
        serie = percentage,
        radius = c("40%", "70%"),
        name = "Family",
        label = list(show = FALSE),
        labelLine = list(show = FALSE)
      ) %>%
      e_title(
        "Downtown Yonge Family Structure",
        textStyle = list(color = "#fff")
      ) %>%
      e_tooltip(
        trigger   = "item",
        formatter = "{b}: {d}%"
      ) %>%
      e_legend(
        orient = "vertical",
        right = "right",
        bottom = 0
      ) |>
      e_text_style(fontFamily = "Inter") |>
      e_color(c("#DB3069", "#F45D09", "#8A4285", "#00AEF6", "#43B171")) |>
      e_toolbox_feature(feature = "saveAsImage") 
    
  })


  # Toronto CMA donut
  output$demoFamilyCMA <- renderEcharts4r({
    neighbourhood_demos %>%
      filter(Area == "Toronto CMA") %>%
      select(
        `Total Couple Families`,
        `Married Couple Families`,
        `Common Law Couple Families`,
        `Total Lone Parent Families`
      ) %>%
      pivot_longer(
        cols      = everything(),
        names_to  = "category",
        values_to = "percentage"
      ) %>%
      e_charts(category) %>%
      e_pie(
        serie = percentage,
        radius = c("40%", "70%"),
        name = "Family",
        label = list(show = FALSE),
        labelLine = list(show = FALSE)
      ) %>%
      e_title(
        "Toronto CMA Family Structure",
        textStyle = list(color = "#fff")
      ) %>%
      e_tooltip(
        trigger   = "item",
        formatter = "{b}: {d}%"
      ) |>
      e_legend(
        orient = "vertical",
        right = "right",
        bottom = 0
      ) |>
      e_text_style(fontFamily = "Inter") |>
      e_color(c("#DB3069", "#F45D09", "#8A4285", "#00AEF6", "#43B171")) |>
      e_toolbox_feature(feature = "saveAsImage") 
    
  })


  # Household Income
  output$demoIncome <- renderEcharts4r({
    # filter and clean the Income data
    demosIncomePlot <- neighbourhood_demos %>%
      select(Area, (71:76)) %>%
      pivot_longer(!Area, names_to = "category", values_to = "percentage") %>%
      mutate(category = str_remove(category, "Household Income") %>% str_trim())


    # plot the data using the echarts package
    demosIncomePlot %>%
      group_by(Area) %>%
      e_charts(category) %>%
      e_bar(percentage) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Household Income") %>%
      e_y_axis(name = "Percentage of Population") %>%
      e_x_axis(name = "Income Category") %>%
      e_legend(top = "bottom") %>%
      e_grid(containLabel = TRUE) %>%
      e_legend(
        orient = "horizontal",
        left = "left",
        bottom = 0
      ) %>%
      e_text_style(fontFamily = "Inter") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })


  # occupation

  output$demoOccupation <- renderEcharts4r({
    neighbourhood_demos %>%
      filter(Area == "Downtown Yonge") %>%
      # pick just those 9 occupation columns
      select((81:89)) %>%
      pivot_longer(
        cols      = everything(),
        names_to  = "category",
        values_to = "percentage"
      ) |>
      e_charts(category) |>
      e_pie(
        serie = percentage,
        radius = c("40%", "70%"),
        label = list(color = "#fff", borderWidth = "0")
      ) |>
      e_title(
        "Downtown Yonge: Employment by Occupation",
        textStyle = list(color = "#fff")
      ) |>
      e_tooltip(
        trigger   = "item",
        formatter = "{b}: {d}%"
      ) |>
      e_legend(
        show = FALSE
      ) |>
      e_text_style(
        color      = "#ffffff",
        fontFamily = "Inter"
      ) |>
      e_color(c(
        "#DB3069",
        "#F45D09",
        "#8A4285",
        "#00AEF6",
        "#43B171",
        "#F03838",
        "#FFD931",
        "#3030bf",
        "#1be0b9"
      )) |>
      e_toolbox_feature(feature = "saveAsImage") 
    
  })

  # Toronto CMA occupation donut
  output$demoOccupationCMA <- renderEcharts4r({
    neighbourhood_demos %>%
      filter(Area == "Toronto CMA") %>%
      select((81:89)) %>%
      pivot_longer(
        cols      = everything(),
        names_to  = "category",
        values_to = "percentage"
      ) |>
      e_charts(category) |>
      e_pie(
        serie = percentage,
        radius = c("40%", "70%"),
        label = list(color = "#fff", borderWidth = "0")
      ) |>
      e_title(
        "Toronto CMA: Employment by Occupation",
        textStyle = list(color = "#fff")
      ) |>
      e_tooltip(
        trigger   = "item",
        formatter = "{b}: {d}%"
      ) |>
      e_legend(
        show = FALSE,
      ) |>
      e_text_style(
        color      = "#ffffff",
        fontFamily = "Inter"
      ) |>
      e_color(c(
        "#DB3069",
        "#F45D09",
        "#8A4285",
        "#00AEF6",
        "#43B171",
        "#F03838",
        "#FFD931",
        "#3030bf",
        "#1be0b9"
      )) |>
      e_toolbox_feature(feature = "saveAsImage") 
    
  })
}


shinyApp(ui, server)

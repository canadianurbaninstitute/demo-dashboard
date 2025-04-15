ui <- page_navbar(
  title = "Downtown Yonge BIA Dashboard Demo",
  navbar_options = navbar_options(position = "fixed-top", underline = FALSE),
  theme = mytheme |> bs_add_rules("
                    h1 { font-family: 'Inter', sans-serif; }
                  "),
  header = tags$head(tags$style(HTML(
  "body { padding-top: 70px!important;} 
  .nav-pills {
    border: 1px solid #494949;
    padding: 0.5em;
    border-radius: 1em;
    margin-bottom: 1em;}
  #legend, .maplibregl-popup-content {background-color: #222!important;}
  .maplibregl-popup-tip {border-top-color: #222!important;}"
  )
  )
  ),
  
  # MOVE THIS INTO EXTERNAL CSS ^
  
  nav_panel(title = "Home",
            h1(name),
            p("Weclome to the Downtown Yonge BIA Main Street Metrics Dashboard! This dashboard gives you a quick snapshot of the BIA. On this page, you’ll find an interactive map alongside highlights from the latest quarter. To explore more, head to the Visitors page for insights on foot traffic and visitor demographics, or visit the Neighbourhood Profile for a deeper look at the area's characteristics."),
            layout_column_wrap(
              width = 1/2,
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
  
  
  nav_panel(title = "Visitors", 
            h1(name),
            navset_pill(
              nav_panel(
                title = "Visitor Trends",
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
                    card_title("Monthly Visits since 2021"),
                    card_body(
                      p("This chart measures the number of pedestrian visits to the BIA on a monthly basis from 2021."),
                      echarts4rOutput("visitorLevels")
                      )
                  ),
                  card(
                    card_body(echarts4rOutput("visitorTypes"))
                  ),
                  layout_column_wrap(
                    width = 1/2,
                    card(
                      card_body(
                        echarts4rOutput("visitorDoW")
                      )
                    ),
                    card(
                      card_body(
                        echarts4rOutput("visitorToD")
                      )
                    )
                  )
                )
              ),
              nav_panel(
                title = "Visitor Demographics"
              )
            )
  ),
  
  nav_panel("Neighbourhood Profile", 
            h1(name),
            navset_pill(
              nav_panel(
                title = "Business",
                h3("Business Distribution"),
                layout_column_wrap(
                  width = 1/2,
                  card(
                    full_screen = FALSE,
                    card_body(
                      class = "p-0",
                      maplibreOutput("businessMap")
                    )
                  ),
                  card(
                    card_body(
                      echarts4rOutput("businessTypes2")
                    )
                  )
                ),
                layout_columns(
                  value_box(
                    title = "Number of Businesses", value = "581",
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
                layout_column_wrap(
                  width = 1/2,
                  card(
                    full_screen = FALSE,
                    card_body(
                      class = "p-0",
                      maplibreOutput("civicMap")
                    )
                  ),
                  card(
                    card_body(
                      echarts4rOutput("civicTypes2")
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
                      echarts4rOutput("housingConstruction")
                    )
                  ),
                  card(
                    card_body(
                      echarts4rOutput("housingType")
                    )
                  )
              ),
              nav_panel(
                title = "Urban Form"
              ),
              nav_panel(
                title = "Neighbourhood Demographics",
                h3("Demographic Map"),
                card(
                  full_screen = FALSE,
                  card_body(
                    class = "p-0",
                    maplibreOutput("demoMap")
                  )
                ),
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
                  )
                ),
                layout_column_wrap(
                  width = 1/2,
                  card(
                    full_screen = FALSE,
                    card_body(
                      echarts4rOutput("demoAge")
                    )
                  ),
                  card(
                    card_body(
                      echarts4rOutput("demoFamily")
                    )
                  )
                ),
                layout_column_wrap(
                  width = 1/2,
                  card(
                    full_screen = FALSE,
                    card_body(
                      echarts4rOutput("demoIncome")
                    )
                  ),
                  card(
                    card_body(
                      echarts4rOutput("demoOccupation")
                    )
                  )
                )
              )
            )
  ),
  
  nav_spacer()
)


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
                      echarts4rOutput("visitorTypes"))
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
  
  nav_panel("Neighbourhood Profile", 
            h1(name),
            navset_pill(
              nav_panel(
                title = "Business",
                h3("Business Distribution"),
                p("The business distribution map shows the location and type of main street businesses across Downtown Yonge."),
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
                ),
                h3("Employment Size"),
                p("The employment size map shows business locations by type and their approximate employment size."),
                card(
                  full_screen = FALSE,
                  card_body(
                    class = "p-0",
                    maplibreOutput("employmentSize")
                  )
                )
              ),
              nav_panel(
                title = "Civic Infrastructure",
                h3("Civic Infrastructure Distribution"),
                p("The civic infrastructure distribution map shows the location and type of civic infrastructure across Downtown Yonge."),
                
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
                  card(
                    card_body(
                      p("The census family structure chart compares Downtown Yonge and Toronto CMA by type of family (e.g., married, common-law)."),
                      echarts4rOutput("demoFamily")
                    )
                  ),
                  card(
                    full_screen = FALSE,
                    card_body(
                      p("The household income chart displays the percentage of households in each income bracket for Downtown Yonge vs. Toronto CMA."),
                      echarts4rOutput("demoIncome")
                    )
                  ),
                  card(
                    card_body(
                      p("The employment occupation chart highlights the share of population in different job sectors."),
                      echarts4rOutput("demoOccupation")
                    )
                  )
              )
            )
  ),
  
  nav_spacer()
)


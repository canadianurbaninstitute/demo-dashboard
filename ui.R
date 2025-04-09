ui <- page_navbar(
  title = "Dashboard Demo",
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
    margin-bottom: 1em;}")
  )
  ),
  nav_panel(title = "Home",
            h1("Downtown Yonge BIA"),
            card(
              full_screen = FALSE,
              card_body(
                class = "p-0",
                maplibreOutput("map")
              )
            ),
            h1("Highlights"),
            layout_columns(
              value_box(
                title = "Number of Visitors", value = "28,000 (+5%)", ,
                theme = "success", showcase = bsicons::bs_icon("arrow-up"),
                showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
                height = NULL
              ),
              value_box(
                title = "Number of Visitors", value = "10,000 (-3%)", , theme = "danger",
                showcase = bsicons::bs_icon("arrow-down"), showcase_layout = "left center",
                full_screen = FALSE, fill = TRUE, height = NULL
              )
            )
            
            
  ),
  
  
  nav_panel(title = "Visitors", 
            h1("Downtown Yonge BIA"),
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
                    card_body(plotlyOutput("visitorLevels"))
                  ),
                  card(
                    card_body(plotlyOutput("visitorTypes"))
                  ),
                  layout_column_wrap(
                    width = 1/2,
                    
                    card(
                      card_body(
                        plotlyOutput("visitorDoW")
                      )
                    ),
                    
                    card(
                      card_body(
                        plotlyOutput("visitorToD")
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
            h1("Downtown Yonge BIA"),
            navset_pill(
              nav_panel(
                title = "Business",
                h3("Business Map"),
                card(
                  full_screen = FALSE,
                  card_body(
                    class = "p-0",
                    maplibreOutput("businessMap")
                  )
                ),
              ),
              nav_panel(
                title = "Civic Infrastructure"
              ),
              nav_panel(
                title = "Housing"
              ),
              nav_panel(
                title = "Urban Form"
              ),
              nav_panel(
                title = "Neighbourhood Demographics"
              )
            )
  ),
  
  nav_spacer()
)


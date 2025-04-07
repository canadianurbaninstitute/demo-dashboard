ui <- page_navbar(
  title = "Dashboard Demo",
  theme = bs_theme(bg = "#222", fg = "#fff", 
                   primary = "#00AEF6", secondary = "#002940", success = "#43B171", danger = "#F03838",
                   warning = "#FFD931", base_font = font_google("Inter"), heading_font = font_google("Roboto Mono"),
                   navbar_bg = "#00AEF6"
                   ) |> bs_add_rules("
                    h1 { font-family: 'Inter', sans-serif; }
                  "),
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
            navset_card_pill(
              full_screen = FALSE,
              height = 500,
              nav_panel(
                "Visitor Levels",
                card_title("Overall Visitor Patterns"),
                layout_sidebar(
                  fillable = TRUE,
                  height = "100%",
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
                  plotlyOutput("visitorLevels") 
                )
              ),
              nav_panel(
                "Visitor Types",
                card_title("Visits by Visitor Type"),
                layout_sidebar(
                  fillable = TRUE,
                  height = "100%",
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
                  plotlyOutput("visitorTypes")  
                )
              )
            )
  ),
  
  nav_panel("Neighbourhood Profile", 
            p("Third page content.")),
  
  nav_spacer()
)


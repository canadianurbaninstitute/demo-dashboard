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
            h1("Downtown Yonge"),
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
                title = "Number of Visitors", value = "28,000", ,
                theme = "success", showcase = bsicons::bs_icon("arrow-up"),
                showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
                height = NULL
              ),
              value_box(
                title = "Number of Visitors", value = "10,000", , theme = "danger",
                showcase = bsicons::bs_icon("arrow-down"), showcase_layout = "left center",
                full_screen = FALSE, fill = TRUE, height = NULL
              )
            )
            
            
            ),
  
  
  nav_panel(title = "Visitors", 
            p("Second page content.")),
  
  nav_panel("Neighbourhood Profile", 
            p("Third page content.")),
  
  nav_spacer()
)


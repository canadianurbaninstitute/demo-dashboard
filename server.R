server <- function(input, output, session) {
  
  bs_themer()
  
  output$map <- renderMaplibre({
    maplibre(style = carto_style("dark-matter"),
             center = c(-79.381070, 43.656183),
             zoom = 14) |> 
      add_navigation_control()
  })
  
}


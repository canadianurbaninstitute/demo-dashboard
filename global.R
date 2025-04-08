library(shiny)
library(bslib)
library(plotly)
library(mapgl)
library(tidyverse)
library(sf)

# load in the necessary data

# visitor level data
ff_overall = read_csv("./data/ff_monthly.csv") %>%
  select(-...1) %>% rename("Date" = date)
ff_day_of_week = read_csv("./data/ff_day_of_week.csv") %>%
  select(-...1)
ff_time_of_day = read_csv("./data/ff_time_of_day.csv") %>%
  select(-...1)
ff_type = read_csv("./data/ff_vis_type.csv") %>%
  select(-...1) %>% rename("Visits" = Count)

bia <- st_read("./data/downtown_yonge.geojson")

ms_businesses <- st_read("./data/ms_businesses.geojson")

mytheme <- bs_theme(preset="bootstrap", version = "5", bg = "#222", fg = "#fff", 
                  primary = "#00AEF6", secondary = "#002940", success = "#43B171", danger = "#F03838",
                  warning = "#FFD931", base_font = font_google("Inter"), heading_font = font_google("Roboto Mono"),
                  navbar_bg = "#00AEF6")













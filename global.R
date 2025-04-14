library(shiny)
library(bslib)
library(plotly)
library(mapgl)
library(tidyverse)
library(sf)
library(echarts4r)



# load in the necessary data

# Visitor Data

ff_overall = read_csv("./data/ff_monthly.csv") %>%
  select(-...1) %>% rename("Date" = date)

ff_day_of_week = read_csv("./data/ff_day_of_week.csv") %>%
  select(-...1)

ff_time_of_day = read_csv("./data/ff_time_of_day.csv") %>%
  select(-...1)

ff_type = read_csv("./data/ff_vis_type.csv") %>%
  select(-...1) %>% rename("Visits" = Count)

# BIA

name <- "Downtown Yonge BIA"

bia <- st_read("./data/downtown_yonge.geojson")

# Business Data

ms_businesses = st_read("./data/ms_businesses.geojson")

business_types = read_csv("./data/business_types.csv")

mytheme = bs_theme(preset="bootstrap", version = "5", bg = "#222", fg = "#fff", 
                  primary = "#00AEF6", secondary = "#002940", success = "#43B171", danger = "#F03838",
                  warning = "#FFD931", base_font = font_google("Inter"), heading_font = font_google("Roboto Mono"),
                  navbar_bg = "#00AEF6")

# Civic Data

civic_types = read_csv("./data/civic_types.csv")

civic_geo = st_read("./data/civic.geojson")


# Housing Data

housing_construction = read_csv("./data/housing_construction.csv")


housing_type = read_csv("./data/housing_type.csv")

# Demographic Data

neighbourhood_demos = read_csv("./data/demographics.csv")















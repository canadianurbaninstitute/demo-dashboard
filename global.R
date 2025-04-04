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











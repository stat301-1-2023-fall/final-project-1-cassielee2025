# load packages and data
library(tidyverse)
library(skimr)
library(sf)
load("data/full_air_quality_data.rda")

# data quality and overview
full_data %>% 
  st_drop_geometry() %>% 
  skim_without_charts()

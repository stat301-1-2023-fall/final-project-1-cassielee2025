# load packages and data
library(tidyverse)
library(sf)
load("data/full_air_quality_data.rda")

# pollutant variables:
# days over O3 standard, days over 2.5 PM standard
# highway living, highway schools
# parks access
# 5 pollutants
# transportation type

# ozone https://www.epa.gov/ground-level-ozone-pollution/ground-level-ozone-basics ----

# percent living near highway
full_data %>% 
  bivariate_plot(highway_living, days_over_o3_standard) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Days over ozone standard vs population living near a highway",
    x = "Percent of people living within 150 M of a highway",
    y = "Days over ozone standard"
  )

# percent of schools near highway
full_data %>% 
  bivariate_plot(highway_schools, days_over_o3_standard) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Days over ozone standard vs public schools near a highway",
    x = "Percent of public schools within 150 M of a highway",
    y = "Days over ozone standard"
  )

# parks access
full_data %>% 
  bivariate_plot(parks_access, days_over_o3_standard) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Days over ozone standard vs access to parks",
    x = "Percent of people living within 1/2 mile of a park",
    y = "Days over ozone standard"
  )
# well this is weird
# maybe places with more access to parks are highly urban areas with lots of urban parks??
# and urban areas generally have more ozone

# walking transportation
full_data %>% 
  bivariate_plot(transportation_type_walking, days_over_o3_standard) +
  coord_cartesian(
    xlim = c(0, 20),
    ylim = c(0, 50)
  ) +
  labs(
    title = "Days over ozone standard vs main transportation: walking",
    x = "Percent of people who mainly walk",
    y = "Days over ozone standard"
  )

# biking transportation
full_data %>% 
  bivariate_plot(transportation_type_bicycle, days_over_o3_standard) +
  coord_cartesian(
    xlim = c(0, 6),
    ylim = c(0, 50)
  ) +
  labs(
    title = "Days over ozone standard vs main transportation: biking",
    x = "Percent of people who mainly bike",
    y = "Days over ozone standard"
  )
# might have to do with urban areas being more bikable?

# no transportation
full_data %>% 
  bivariate_plot(transportation_none, days_over_o3_standard) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Days over ozone standard vs main transportation: none",
    x = "Percent of people who mainly do not use transportation",
    y = "Days over ozone standard"
  )
# might have to do with urban areas having more virtual workers?

# driving alone
full_data %>% 
  bivariate_plot(occupancy_drove_alone, days_over_o3_standard) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Days over ozone standard vs main transportation: driving alone",
    x = "Percent of people who mainly drive alone",
    y = "Days over ozone standard"
  )
# what

# carpooling
full_data %>% 
  bivariate_plot(occupancy_carpooled, days_over_o3_standard) +
  coord_cartesian(
    xlim = c(0, 20),
    ylim = c(0, 50)
  ) +
  labs(
    title = "Days over ozone standard vs main transportation: carpooling",
    x = "Percent of people who mainly carpool",
    y = "Days over ozone standard"
  )

# public transportation
full_data %>% 
  bivariate_plot(transportation_public, days_over_o3_standard) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Days over ozone standard vs main transportation: public transportation",
    x = "Percent of people who mainly use public transportation",
    y = "Days over ozone standard"
  )

# so these things are mostly showing an urban bias probably

# what counties have the highest ozone?
full_data %>% 
  select(county, state, days_over_o3_standard) %>% 
  st_drop_geometry() %>% 
  slice_max(days_over_o3_standard, n = 10)

# percent days over 2.5 PM standard
full_data %>% 
  bivariate_plot(days_over_pm_standard, days_over_o3_standard) +
  labs(
    title = "Days over ozone standard vs percentage of days over 2.5 PM standard",
    x = "Percent days over 2.5 PM standard",
    y = "Days over ozone standard"
  )

# pollutant benzene
full_data %>% 
  bivariate_plot(pollutant_benzene, days_over_o3_standard) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Days over ozone standard vs benzene pollution",
    x = "Concentration of benzene (µg/m3)",
    y = "Days over ozone standard"
  )

# pollutant formaldehyde
full_data %>% 
  bivariate_plot(pollutant_formaldehyde, days_over_o3_standard) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Days over ozone standard vs formaldehyde pollution",
    x = "Concentration of formaldehyde (µg/m3)",
    y = "Days over ozone standard"
  )

# pollutant acetaldehyde
full_data %>% 
  bivariate_plot(pollutant_acetaldehyde, days_over_o3_standard) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Days over ozone standard vs acetaldehyde pollution",
    x = "Concentration of acetaldehyde (µg/m3)",
    y = "Days over ozone standard"
  )

# pollutant carbon tetrachloride
full_data %>% 
  bivariate_plot(pollutant_carbon_tetrachloride, days_over_o3_standard) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Days over ozone standard vs carbon tetrachloride pollution",
    x = "Concentration of carbon tetrachloride (µg/m3)",
    y = "Days over ozone standard"
  )

# pollutant 1,3-butadiene
full_data %>% 
  bivariate_plot(pollutant_1_3_butadiene, days_over_o3_standard) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Days over ozone standard vs 1,3-butadiene pollution",
    x = "Concentration of 1,3-butadiene (µg/m3)",
    y = "Days over ozone standard"
  )


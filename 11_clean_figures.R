# load packages and data
library(tidyverse)
library(skimr)
library(sf)
library(patchwork)
library(viridis)
load("data/full_air_quality_data.rda")

# air and environmental quality ----

# days over air quality standards
p1 <- full_data %>% 
  ggplot(aes(days_over_o3_standard)) +
  geom_histogram(binwidth = 10, boundary = 0, fill = "#def5e5ff") +
  theme_minimal() +
  labs(
    title = "Days over ozone standard",
    x = "Days",
    y = NULL
  )

p2 <- full_data %>% 
  mutate(pm_standard = days_over_pm_standard/100 * 365) %>% 
  ggplot(aes(pm_standard)) +
  geom_histogram(binwidth = 10, boundary = 0, fill = "#ed6925") +
  theme_minimal() +
  labs(
    title = "Days over PM 2.5 standard",
    x = "Days",
    y = NULL
  )

p1 + p2

full_data %>% 
  mutate(pm_standard = days_over_pm_standard/100 * 365) %>% 
  ggplot(aes(days_over_o3_standard, pm_standard)) +
  geom_jitter() +
  geom_smooth(method = lm, se = FALSE) +
  lims(
    x = c(0, 50),
    y = c(0, 50)
  ) +
  theme_minimal() +
  labs(
    title = "Days over PM 2.5 standard vs days over ozone standard",
    x = "Days ozone standard",
    y = "Days over PM 2.5 standard"
  )
  
p3 <- full_data %>% 
  ggplot(aes(fill = log(days_over_o3_standard), geometry = geometry)) +
  geom_sf() +
  coord_sf(
    xlim = c(-125, -65), 
    ylim = c(20, 50)
  ) + 
  theme_void() + 
  scale_fill_viridis("Log of days over\nozone standard", option = "mako") +
  labs(title = "Distribution of ozone pollution") +
  theme(legend.position = "bottom")

p4 <- full_data %>% 
  mutate(pm_standard = days_over_pm_standard/100 * 365) %>% 
  ggplot(aes(fill = log(pm_standard), geometry = geometry)) +
  geom_sf() +
  coord_sf(
    xlim = c(-125, -65), 
    ylim = c(20, 50)
  ) + 
  theme_void() + 
  scale_fill_viridis("Log of days over\nPM 2.5 standard", option = "inferno") +
  labs(title = "Distribution of PM 2.5 pollution") +
  theme(legend.position = "bottom")

p3 + p4

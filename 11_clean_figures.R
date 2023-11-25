# load packages and data
library(tidyverse)
library(skimr)
library(sf)
library(patchwork)
load("data/full_air_quality_data.rda")

# air and environmental quality ----

# days over air quality standards
p1 <- full_data %>% 
  ggplot(aes(days_over_o3_standard)) +
  geom_histogram(binwidth = 10, boundary = 0, fill = "#f56042") +
  theme_minimal() +
  labs(
    title = "Days over ozone standard",
    x = "Days",
    y = NULL
  )

p2 <- full_data %>% 
  mutate(pm_standard = days_over_pm_standard/100 * 365) %>% 
  ggplot(aes(pm_standard)) +
  geom_histogram(binwidth = 10, boundary = 0, fill = "#f5a442") +
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
  

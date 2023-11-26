# load packages and data
library(tidyverse)
library(skimr)
library(sf)
library(patchwork)
library(viridis)
library(GGally)
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

# pollutants

p5 <- full_data %>% 
  ggplot(aes(pollutant_benzene)) +
  geom_histogram(fill = "#f0f921") +
  theme_minimal() +
  labs(
    title = "Benzene",
    x = "Concentration (µg/m3)",
    y = NULL
  )

p6 <- full_data %>% 
  ggplot(aes(pollutant_formaldehyde)) +
  geom_histogram(fill = "#f89540") +
  theme_minimal() +
  labs(
    title = "Formaldehyde",
    x = "Concentration (µg/m3)",
    y = NULL
  )

p7 <- full_data %>% 
  ggplot(aes(pollutant_acetaldehyde)) +
  geom_histogram(fill = "#cc4778") +
  theme_minimal() +
  labs(
    title = "Acetaldehyde",
    x = "Concentration (µg/m3)",
    y = NULL
  )

p8 <- full_data %>% 
  ggplot(aes(pollutant_carbon_tetrachloride)) +
  geom_histogram(fill = "#7e03a8") +
  theme_minimal() +
  labs(
    title = "Carbon Tetrachloride",
    x = "Concentration (µg/m3)",
    y = NULL
  )

p9 <- full_data %>% 
  ggplot(aes(pollutant_1_3_butadiene)) +
  geom_histogram(fill = "#0d0887") +
  theme_minimal() +
  labs(
    title = "1,3-Butadiene",
    x = "Concentration (µg/m3)",
    y = NULL
  )

(p5 + p6 + p7) / (p8 + p9)

full_data %>% 
  st_drop_geometry() %>% 
  select(contains("pollutant")) %>% 
  rename_with(~str_remove(., "(pollutant_)"), everything()) %>%
  rename_with(~str_to_title(.), everything()) %>% 
  ggcorr(
    hjust = 0.6,
    geom = "circle",
    max_size = 20
  ) 

# all air quality indicators

full_data %>% 
  select(contains("pollutant") | contains("days")) %>% 
  st_drop_geometry() %>% 
  rename_with(~str_remove(., "(pollutant_)?(days_over_)?"), everything()) %>%
  rename_with(~str_to_title(.), everything()) %>% 
  ggcorr(
    hjust = 0.7,
    geom = "circle",
    max_size = 15,
    size = 3
  )

# environmental quality
full_data %>% 
  ggplot(aes(highway_living, days_over_o3_standard)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_minimal() +
  labs(
    title = "Days over ozone standard vs population living near a highway",
    x = "Percent of people living within 150 M of a highway",
    y = "Days over ozone standard"
  )

# lung disease and air quality indicators ----

# asthma
p10 <- full_data %>% 
  ggplot(aes(days_over_o3_standard, asthma_ed_crude)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE)  +
  coord_cartesian(
    x = c(0, 20),
    y = c(0, 150)
  ) +
  labs(
    title = "Days over ozone standard",
    x = "Days over ozone standard",
    y = "Crude emergency department visits"
  ) +
  theme_minimal()

p11 <- full_data %>% 
  ggplot(aes(days_over_pm_standard, asthma_ed_crude)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  coord_cartesian(
    x = c(0, 20),
    y = c(0, 150)
  ) +
  labs(
    title = "Days over PM 2.5 standard",
    x = "Days over PM 2.5 standard",
    y = "Crude emergency department visits"
  ) +
  theme_minimal()
  
p10 + p11

p12 <- full_data %>% 
  ggplot(aes(pollutant_formaldehyde, asthma_adult_crude)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Crude adult asthma prevalance vs formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Crude percentage of adults with asthma"
  ) +
  theme_minimal()

p13 <- full_data %>% 
  ggplot(aes(pollutant_acetaldehyde, asthma_adult_crude)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Crude adult asthma prevalance vs acetaldehyde",
    x = "Concentration (µg/m3)",
    y = "Crude percentage of adults with asthma"
  ) +
  theme_minimal()

p14 <- full_data %>% 
  ggplot(aes(pollutant_acetaldehyde, asthma_child_crude)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Crude child asthma prevalance vs formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Crude percentage of children with asthma"
  ) +
  theme_minimal()

p15 <- full_data %>% 
  ggplot(aes(pollutant_acetaldehyde, asthma_child_crude)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Crude child asthma prevalance vs acetaldehyde",
    x = "Concentration (µg/m3)",
    y = "Crude percentage of children with asthma"
  ) +
  theme_minimal()

(p12 + p13) / (p14 + p15)



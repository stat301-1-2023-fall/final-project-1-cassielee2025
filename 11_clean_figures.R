# load packages and data
library(tidyverse)
library(skimr)
library(sf)
library(patchwork)
library(viridis)
library(GGally)
library(janitor)
load("data/full_air_quality_data.rda")

# air and environmental quality ----

## days over air quality standards ----
p1 <- full_data %>% 
  ggplot(aes(days_over_o3_standard)) +
  geom_histogram(binwidth = 10, boundary = 0, fill = "#0094bf") +
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

p1 + p2 + plot_annotation(
  title = "Distribution of days over air quality standards",
  theme = theme(plot.title = element_text(face = "bold"))
)

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
  ) +
  theme(plot.title = element_text(face = "bold"))
  
p3 <- full_data %>% 
  ggplot(aes(fill = log(days_over_o3_standard), geometry = geometry)) +
  geom_sf() +
  coord_sf(
    xlim = c(-125, -65), 
    ylim = c(20, 50)
  ) + 
  theme_void() + 
  scale_fill_viridis(
    "Log of days over\nozone standard", 
    option = "mako", 
    direction = -1
  ) +
  labs(title = "Ozone") +
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
  scale_fill_viridis(
    "Log of days over\nPM 2.5 standard", 
    option = "magma",
    direction = -1
  ) +
  labs(title = "PM 2.5") +
  theme(legend.position = "bottom")

p3 + p4 + plot_annotation(
  title = "Distribution of days over air quality standards across the US",
  theme = theme(plot.title = element_text(face = "bold"))
)

## pollutants ----

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

(p5 + p6 + p7) / (p8 + p9) + plot_annotation(
  title = "Distribution of air pollutants",
  theme = theme(plot.title = element_text(face = "bold"))
)

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

## all air quality indicators ----

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

## environmental quality ----
full_data %>% 
  ggplot(aes(highway_living, days_over_o3_standard)) +
  geom_point(color = "#0094bf", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black")  +
  coord_cartesian(ylim = c(0, 50)) +
  theme_minimal() +
  labs(
    title = "Days over ozone standard vs population living near a highway",
    x = "Percent of people living within 150 M of a highway",
    y = "Days over ozone standard"
  )

# lung disease and air quality indicators ----

## asthma ----
p10 <- full_data %>% 
  ggplot(aes(days_over_o3_standard, asthma_ed_crude)) +
  geom_point(color = "#0094bf", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black")  +
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
  geom_point(color = "#ed6925", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
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
  
p10 + p11 + plot_annotation(
  title = "Emergency department visits for asthma vs days over air quality standards",
  theme = theme(plot.title = element_text(face = "bold"))
)
  

p12 <- full_data %>% 
  ggplot(aes(pollutant_formaldehyde, asthma_adult_crude)) +
  geom_point(color = "#f89540", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(
    title = "Adult asthma vs formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Crude percentage"
  ) +
  theme_minimal()

p13 <- full_data %>% 
  ggplot(aes(pollutant_acetaldehyde, asthma_adult_crude)) +
  geom_point(color = "#cc4778", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(
    title = "Adult asthma vs acetaldehyde",
    x = "Concentration (µg/m3)",
    y = "Crude percentage"
  ) +
  theme_minimal()

p14 <- full_data %>% 
  ggplot(aes(pollutant_formaldehyde, asthma_child_crude)) +
  geom_point(color = "#f89540", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(
    title = "Child asthma vs formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Crude percentage"
  ) +
  theme_minimal()

p15 <- full_data %>% 
  ggplot(aes(pollutant_acetaldehyde, asthma_child_crude)) +
  geom_point(color = "#cc4778", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(
    title = "Child asthma vs acetaldehyde",
    x = "Concentration (µg/m3)",
    y = "Crude percentage"
  ) +
  theme_minimal()

(p12 + p13) / (p14 + p15) + plot_annotation(
  title = "Prevalence of asthma vs air pollutants",
  theme = theme(plot.title = element_text(face = "bold"))
)

## cancer ----
full_data %>% 
  select(cancer_adjusted, contains("days"), contains("pollutant")) %>% 
  st_drop_geometry() %>% 
  rename_with(~str_remove(., "(pollutant_)?(days_over_)?"), everything()) %>%
  rename_with(~str_remove(., "(_adjusted)?$"), everything()) %>%
  rename_with(~str_to_title(.), everything()) %>% 
  ggcorr(hjust = 0.75,
         geom = "circle",
         max_size = 15,
         size = 3)

p16 <- full_data %>% 
  ggplot(aes(pollutant_formaldehyde, cancer_adjusted)) +
  geom_point(color = "#f89540", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(
    title = "Formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Prevalence of lung and bronchus cancer"
  ) +
  theme_minimal()

p17 <- full_data %>% 
  ggplot(aes(pollutant_acetaldehyde, cancer_adjusted)) +
  geom_point(color = "#cc4778", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(
    title = "Acetaldehyde",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal()

p18 <- full_data %>% 
  ggplot(aes(pollutant_carbon_tetrachloride, cancer_adjusted)) +
  geom_point(color = "#7e03a8", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(
    title = "Carbon tetrachloride",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal()

p16 + p17 + p18 + plot_annotation(
  title = "Prevalence of lung and bronchus cancer vs air pollutants",
  theme = theme(plot.title = element_text(face = "bold"))
)

## copd ----

full_data %>% 
  select(copd_adjusted, contains("days"), contains("pollutant")) %>% 
  st_drop_geometry() %>% 
  rename_with(~str_remove(., "(pollutant_)?(days_over_)?"), everything()) %>%
  rename_with(~str_remove(., "(_adjusted)?$"), everything()) %>%
  rename_with(~str_to_title(.), everything()) %>% 
  rename(COPD = Copd) %>% 
  ggcorr(hjust = 0.75,
         geom = "circle",
         max_size = 15,
         size = 3)

p19 <- full_data %>% 
  ggplot(aes(pollutant_formaldehyde, copd_adjusted)) +
  geom_point(color = "#f89540", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(
    title = "Formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Prevalence of COPD"
  ) +
  theme_minimal()

p20 <- full_data %>% 
  ggplot(aes(pollutant_acetaldehyde, copd_adjusted)) +
  geom_point(color = "#cc4778", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(
    title = "Acetaldehyde",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal()

p21 <- full_data %>% 
  ggplot(aes(pollutant_carbon_tetrachloride, copd_adjusted)) +
  geom_point(color = "#7e03a8", alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(
    title = "Carbon tetrachloride",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal()

p19 + p20 + p21 + plot_annotation(
  title = "Prevalence of COPD vs air pollutants",
  theme = theme(plot.title = element_text(face = "bold"))
)

# sociodemographic effects ----

## age ----
p22 <- full_data %>% 
  ggplot(aes(
    days_over_o3_standard, 
    asthma_ed_crude, 
    color = age_demographics_vulnerable,
  )) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm, se = FALSE) +
  coord_cartesian(
    x = c(0, 50),
    y = c(0, 100)
  ) +
  theme_minimal() +
  labs(
    title = "Days over ozone standard",
    x = "Days over ozone standard",
    y = "Crude emergency department visits"
  ) +
  scale_color_manual(
    "Age vulnerability",
    values = c("black", "#0094bf")
  ) +
  theme(legend.position = "bottom")

p23 <- full_data %>% 
  mutate(pm_standard = days_over_pm_standard/100 * 365) %>% 
  ggplot(aes(
    pm_standard, 
    asthma_ed_crude, 
    color = age_demographics_vulnerable,
  )) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm, se = FALSE) +
  coord_cartesian(
    x = c(0, 70),
    y = c(0, 100)
  ) +
  theme_minimal() +
  labs(
    title = "Days over PM 2.5 standard",
    x = "Days over PM 2.5 standard",
    y = "Crude emergency department visits"
  ) +
  scale_color_manual(
    "Age vulnerability",
    values = c("black", "#ed6925")
  ) +
  theme(legend.position = "bottom")

p22 + p23 + plot_annotation(
  title = "Age vulnerability and emergency department visits for asthma",
  theme = theme(plot.title = element_text(face = "bold"))
)

p24 <- full_data %>% 
  ggplot(aes(
    pollutant_formaldehyde, 
    cancer_adjusted, 
    color = age_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Prevalence of lung and bronchus cancer"
  ) +
  theme_minimal() +
  scale_color_manual(
    "Age vulnerability",
    values = c("black", "#f89540")
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  guides(color = guide_legend(nrow = 2))

p25 <- full_data %>% 
  ggplot(aes(
    pollutant_acetaldehyde, 
    cancer_adjusted,
    color = age_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Acetaldehyde",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal() +
  scale_color_manual(
    "Age vulnerability",
    values = c("black", "#cc4778")
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  guides(color = guide_legend(nrow = 2))

p26 <- full_data %>% 
  ggplot(aes(
    pollutant_carbon_tetrachloride, 
    cancer_adjusted,
    color = age_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Carbon tetrachloride",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal() +
  scale_color_manual(
    "Age vulnerability",
    values = c("black", "#7e03a8")
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  guides(color = guide_legend(nrow = 2))

p24 + p25 + p26 + plot_annotation(
  title = "Age vulnerability and prevalence of lung and bronchus cancer",
  theme = theme(plot.title = element_text(face = "bold"))
)

p27 <- full_data %>% 
  ggplot(aes(
    pollutant_formaldehyde, 
    copd_adjusted, 
    color = age_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Prevalence of COPD"
  ) +
  theme_minimal() +
  scale_color_manual(
    "Age vulnerability",
    values = c("black", "#f89540")
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  guides(color = guide_legend(nrow = 2))

p28 <- full_data %>% 
  ggplot(aes(
    pollutant_acetaldehyde, 
    copd_adjusted,
    color = age_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Acetaldehyde",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal() +
  scale_color_manual(
    "Age vulnerability",
    values = c("black", "#cc4778")
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  guides(color = guide_legend(nrow = 2))

p29 <- full_data %>% 
  ggplot(aes(
    pollutant_carbon_tetrachloride, 
    copd_adjusted,
    color = age_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Carbon tetrachloride",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal() +
  scale_color_manual(
    "Age vulnerability",
    values = c("black", "#7e03a8")
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  guides(color = guide_legend(nrow = 2))

p27 + p28 + p29 + plot_annotation(
  title = "Age vulnerability and prevalence of COPD",
  theme = theme(plot.title = element_text(face = "bold"))
)

## gender ----
p30 <- full_data %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  ggplot(aes(
    days_over_o3_standard, 
    asthma_ed_crude, 
    color = gender_demographics_vulnerable,
  )) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  coord_cartesian(x = c(0, 30)) +
  theme_minimal() +
  labs(
    title = "Ozone",
    x = "Days over ozone standard",
    y = "Crude ED visits"
  ) +
  scale_color_manual(
    "Proportion women",
    values = c("black", "#0094bf")
  ) +
  theme(legend.position = "bottom")

p31<- full_data %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  mutate(pm_standard = days_over_pm_standard/100 * 365) %>% 
  ggplot(aes(
    pm_standard, 
    asthma_ed_crude, 
    color = age_demographics_vulnerable,
  )) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  coord_cartesian(x = c(0, 40)) +
  theme_minimal() +
  labs(
    title = "PM 2.5",
    x = "Days over PM 2.5 standard",
    y = NULL
  ) +
  scale_color_manual(
    "Proportion women",
    values = c("black", "#ed6925")
  ) +
  theme(legend.position = "bottom")

p32 <- full_data %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  ggplot(aes(
    pollutant_formaldehyde, 
    asthma_ed_crude,
    color = gender_demographics_vulnerable
  )) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = lm, se = FALSE) +
  scale_color_manual(
    "Proportion women",
    values = c("black", "#f89540")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Crude ED visits"
  )

p33 <- full_data %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  ggplot(aes(
    pollutant_acetaldehyde, 
    asthma_ed_crude,
    color = gender_demographics_vulnerable
  )) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = lm, se = FALSE) +
  scale_color_manual(
    "Proportion women",
    values = c("black", "#cc4778")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Acetaldehyde",
    x = "Concentration (µg/m3)",
    y = NULL
  ) 

p30 + p31 + p32 + p33 + plot_annotation(
  title = "Gender vulnerability and emergency department visits for asthma",
  theme = theme(plot.title = element_text(face = "bold"))
)

p34 <- full_data %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  ggplot(aes(
    pollutant_formaldehyde, 
    cancer_adjusted, 
    color = gender_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Formaldehyde",
    x = NULL,
    y = "Prevalence of cancer"
  ) +
  theme_minimal() +
  scale_color_manual(
    "Proportion women",
    values = c("black", "#f89540")
  ) +
  theme(legend.position = "none")

p35 <- full_data %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>%
  ggplot(aes(
    pollutant_acetaldehyde, 
    cancer_adjusted,
    color = gender_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Acetaldehyde",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  scale_color_manual(
    "Proportion women",
    values = c("black", "#cc4778")
  ) +
  theme(legend.position = "none")

p36 <- full_data %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>%
  ggplot(aes(
    pollutant_carbon_tetrachloride, 
    cancer_adjusted,
    color = gender_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Carbon tetrachloride",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  scale_color_manual(
    "Proportion women",
    values = c("black", "#7e03a8")
  ) +
  theme(legend.position = "none")

p37 <- full_data %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>%
  ggplot(aes(
    pollutant_formaldehyde, 
    copd_adjusted, 
    color = gender_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    x = "Concentration (µg/m3)",
    y = "Prevalence of COPD"
  ) +
  theme_minimal() +
  scale_color_manual(
    "Proportion women",
    values = c("black", "#f89540")
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  guides(color = guide_legend(nrow = 2))

p38 <- full_data %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>%
  ggplot(aes(
    pollutant_acetaldehyde, 
    copd_adjusted,
    color = gender_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal() +
  scale_color_manual(
    "Proportion women",
    values = c("black", "#cc4778")
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  guides(color = guide_legend(nrow = 2))

p39 <- full_data %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>%
  ggplot(aes(
    pollutant_carbon_tetrachloride, 
    copd_adjusted,
    color = gender_demographics_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal() +
  scale_color_manual(
    "Proportion women",
    values = c("black", "#7e03a8")
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  guides(color = guide_legend(nrow = 2))

(p34 + p35 + p36) / (p37 + p38 + p39) + plot_annotation(
  title = "Gender vulnerability and prevalence of lung and bronchus cancer and COPD",
  theme = theme(plot.title = element_text(face = "bold"))
)

## race ----
p40 <- full_data %>% 
  ggplot(aes(
    pollutant_formaldehyde, 
    asthma_adult_crude, 
    color = majority
  )) +
  geom_point(alpha = 0.3) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Crude percentage"
  ) +
  theme_minimal() +
  scale_color_viridis(
    "Predominant race",
    option = "inferno",
    discrete = TRUE,
    end = 0.8,
    labels = c(
      "American Indian/\nAlaskan Native",
      "Asian/Pacific Islander",
      "Black", 
      "White"
    )
  ) +
  theme(legend.position = "right")

p41 <- full_data %>% 
  ggplot(aes(
    pollutant_acetaldehyde, 
    asthma_adult_crude,
    color = majority
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Acetaldehyde",
    x = "Concentration (µg/m3)",
    y = "Crude percentage"
  ) +
  theme_minimal() +
  scale_color_viridis(
    "Predominant race",
    option = "inferno",
    discrete = TRUE,
    end = 0.8,
    labels = c(
      "American Indian/\nAlaskan Native",
      "Asian/Pacific Islander",
      "Black", 
      "White"
    )
  ) + 
  theme(legend.position = "none")

p40 + p41 + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Race and prevalence of adult asthma",
    theme = theme(plot.title = element_text(face = "bold"))
  )

p42 <- full_data %>% 
  ggplot(aes(
    pollutant_carbon_tetrachloride, 
    cancer_adjusted,
    color = majority
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Prevalence of lung and bronchus cancer"
  ) +
  theme_minimal() +
  scale_color_viridis(
    "Predominant race",
    option = "inferno",
    discrete = TRUE,
    end = 0.8,
    labels = c(
      "American Indian/\nAlaskan Native",
      "Asian/Pacific Islander",
      "Black", 
      "White"
    )
  ) + 
  theme(legend.position = "none")

p43 <- full_data %>% 
  ggplot(aes(
    pollutant_acetaldehyde, 
    cancer_adjusted,
    color = majority
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Acetaldehyde",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal() +
  scale_color_viridis(
    "Predominant race",
    option = "inferno",
    discrete = TRUE,
    end = 0.8,
    labels = c(
      "American Indian/\nAlaskan Native",
      "Asian/Pacific Islander",
      "Black", 
      "White"
    )
  ) + 
  theme(legend.position = "bottom")

p44 <- full_data %>% 
  ggplot(aes(
    pollutant_carbon_tetrachloride, 
    cancer_adjusted,
    color = majority
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Carbon tetrachloride",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal() +
  scale_color_viridis(
    "Predominant race",
    option = "inferno",
    discrete = TRUE,
    end = 0.8,
    labels = c(
      "American Indian/\nAlaskan Native",
      "Asian/Pacific Islander",
      "Black", 
      "White"
    )
  ) + 
  theme(legend.position = "none")

p42 + p43 + p44 + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Race and prevalence of lung and bronchus cancer",
    theme = theme(plot.title = element_text(face = "bold"))
  ) &
  theme(legend.position = "bottom")

p45 <- full_data %>% 
  ggplot(aes(
    pollutant_carbon_tetrachloride, 
    copd_adjusted,
    color = majority
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Formaldehyde",
    x = "Concentration (µg/m3)",
    y = "Prevalence of COPD"
  ) +
  theme_minimal() +
  scale_color_viridis(
    "Predominant race",
    option = "inferno",
    discrete = TRUE,
    end = 0.8,
    labels = c(
      "American Indian/\nAlaskan Native",
      "Asian/Pacific Islander",
      "Black", 
      "White"
    )
  ) + 
  theme(legend.position = "none")

p46 <- full_data %>% 
  ggplot(aes(
    pollutant_acetaldehyde, 
    copd_adjusted,
    color = majority
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Acetaldehyde",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal() +
  scale_color_viridis(
    "Predominant race",
    option = "inferno",
    discrete = TRUE,
    end = 0.8,
    labels = c(
      "American Indian/\nAlaskan Native",
      "Asian/Pacific Islander",
      "Black", 
      "White"
    )
  ) + 
  theme(legend.position = "bottom")

p47 <- full_data %>% 
  ggplot(aes(
    pollutant_carbon_tetrachloride, 
    copd_adjusted,
    color = majority
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Carbon tetrachloride",
    x = "Concentration (µg/m3)",
    y = NULL
  ) +
  theme_minimal() +
  scale_color_viridis(
    "Predominant race",
    option = "inferno",
    discrete = TRUE,
    end = 0.8,
    labels = c(
      "American Indian/\nAlaskan Native",
      "Asian/Pacific Islander",
      "Black", 
      "White"
    )
  ) + 
  theme(legend.position = "none")

p45 + p46 + p47 + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Race and prevalence of COPD",
    theme = theme(plot.title = element_text(face = "bold"))
  ) &
  theme(legend.position = "bottom")

## SVI ----
p48 <- full_data %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  ggplot(aes(
    pollutant_formaldehyde, 
    asthma_adult_crude, 
    color = socioeconomic_vulnerability_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Asthma",
    x = "Concentration (µg/m3)",
    y = "Prevalence of adult asthma"
  ) +
  theme_minimal() +
  scale_color_manual(
    "Social vulnerability",
    values = c("black", "#f89540")
  ) +
  theme(legend.position = "bottom")

p49 <- full_data %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  ggplot(aes(
    pollutant_formaldehyde, 
    cancer_adjusted, 
    color = socioeconomic_vulnerability_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Lung and bronchus cancer",
    x = "Concentration (µg/m3)",
    y = "Prevalence of lung and bronchus cancer"
  ) +
  theme_minimal() +
  scale_color_manual(
    "Social vulnerability",
    values = c("black", "#f89540")
  ) +
  theme(legend.position = "bottom")

p50 <- full_data %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  ggplot(aes(
    pollutant_formaldehyde, 
    copd_adjusted, 
    color = socioeconomic_vulnerability_vulnerable
  )) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "COPD",
    x = "Concentration (µg/m3)",
    y = "Prevalence of COPD"
  ) +
  theme_minimal() +
  scale_color_manual(
    "Social vulnerability",
    values = c("black", "#f89540")
  ) +
  theme(legend.position = "bottom")

p48 + p49 + p50 + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Social vulnerability index and the relationship between\nlung disease and formaldehyde pollution",
    theme = theme(plot.title = element_text(face = "bold"))
  ) &
  theme(legend.position = "bottom")

# appendix ----
age_demographics <- read_csv("data/raw/demographics-age/data_160814.csv") %>% 
  clean_names()

age_demographics2 <- age_demographics %>% 
  mutate(
    age_group = factor(age_group),
    age_group = fct_collapse(
      age_group,
      "0 TO 19" = c("0 TO 4", "5 TO 19")
    )
  ) %>% 
  summarise(
    value = sum(value),
    .by = c(state, county, county_fips, age_group)
  ) %>% 
  filter(age_group %in% c("0 TO 19", "65 AND OLDER"))

age_demographics2 %>% 
  mutate(
    vulnerable = value >= mean(value) + sd(value),
    .by = age_group,
    .keep = "all"
  ) %>% 
  ggplot(aes(value, fill = vulnerable)) +
  geom_histogram(binwidth = 2, boundary = 0) +
  facet_wrap(~age_group)  +
  labs(
    title = "Distribution of percentage of population in each age category",
    x = "Percentage",
    y = NULL
  ) +
  scale_fill_discrete("Vulnerable")

gender_demographics <- read_csv("data/raw/demographics-gender/data_160859.csv") %>% 
  clean_names()

gender_demographics %>% 
  filter(gender == "Female") %>% 
  mutate(
    vulnerable = if_else(
      value <= mean(value) - sd(value),
      "low",
      if_else(
        value >= mean(value) + sd(value),
        "high",
        NA
      )
    ),
    .keep = "all"
  ) %>% 
  ggplot(aes(value, fill = vulnerable)) +
  geom_histogram(binwidth = 2, boundary = 0) +
  labs(
    title = "Distribution of percentage of population that are women",
    x = "Percentage",
    y = NULL
  ) +
  scale_fill_discrete("Vulnerable")

race_and_ethnicity <- read_csv("data/raw/demographics-race-ethnicity/data_161208.csv") %>% 
  clean_names()
race <- race_and_ethnicity %>% 
  separate_wider_regex(
    race_ethnicity,
    patterns = c(
      race = ".*",
      " including Hispanic"
    ),
    too_few = "align_start"
  ) %>% 
  filter(race != "Hispanic All Races")

race <- race %>% 
  filter(race != "All Non-White Races") %>% 
  group_by(county_fips) %>% 
  slice(which.max(value)) %>% 
  arrange(race)

race %>% 
  ggplot(aes(race)) +
  geom_bar() +
  labs(
    title = "Majority race in each county",
    x = "Race",
    y = NULL
  )

socioeconomic_vulnerability <- read_csv("data/raw/socioeconomic-vulnerability-index/data_161326.csv") %>% 
  clean_names()

socioeconomic_vulnerability %>% 
  arrange(value) %>% 
  mutate(
    vulnerable = if_else(
      value >= 0.75, 
      "high vulnerability", 
      "low vulnerability"
    )
  ) %>% 
  ggplot(aes(value, fill = vulnerable)) +
  geom_histogram(boundary = 0) +
  labs(
    title = "Distribution of socioeconomic vulnerability index",
    x = "SVI",
    y = NULL
  ) +
  scale_fill_discrete("Vulnerable")

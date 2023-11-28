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
  scale_fill_viridis("Log of days over\nozone standard", option = "mako") +
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
  scale_fill_viridis("Log of days over\nPM 2.5 standard", option = "inferno") +
  labs(title = "PM 2.5") +
  theme(legend.position = "bottom")

p3 + p4 + plot_annotation(
  title = "Distribution of days over air quality standards across the US",
  theme = theme(plot.title = element_text(face = "bold"))
)

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

# asthma
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

# cancer
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

# copd

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

# age
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
  theme(legend.position = "bottom")

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
  theme(legend.position = "bottom")

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
  theme(legend.position = "bottom")

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
  theme(legend.position = "bottom")

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
  theme(legend.position = "bottom")

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
  theme(legend.position = "bottom")

p27 + p28 + p29 + plot_annotation(
  title = "Age vulnerability and prevalence of COPD",
  theme = theme(plot.title = element_text(face = "bold"))
)

# gender
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
    "Gender vulnerability",
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
    "Gender vulnerability",
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
    "Gender vulnerability",
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
    "Gender vulnerability",
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

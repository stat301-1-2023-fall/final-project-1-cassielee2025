# load packages and data
library(tidyverse)
library(sf)
load("data/full_air_quality_data.rda")

# adult asthma and formaldehyde ----
full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  multivariate_plot(
    pollutant_formaldehyde, 
    asthma_adult_crude, 
    gender_demographics_vulnerable
  )
# counties with a higher population of females has higher pollutant and asthma

full_data %>% 
  multivariate_plot(
    pollutant_formaldehyde, 
    asthma_adult_crude, 
    majority,
    alpha = 0.2
  )
# majority native counties have extremely high asthma despite not having super high formaldehyde levels

full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  multivariate_plot(
    pollutant_formaldehyde, 
    asthma_adult_crude, 
    socioeconomic_vulnerability_vulnerable,
    alpha = 0.2
  )
# high vulnerability has kind of a negative slope...

# adult asthma and acetaldehyde ----
full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  multivariate_plot(
    pollutant_acetaldehyde, 
    asthma_adult_crude, 
    gender_demographics_vulnerable
  )

full_data %>% 
  multivariate_plot(
    pollutant_acetaldehyde, 
    asthma_adult_crude, 
    majority,
    alpha = 0.2
  )

full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  multivariate_plot(
    pollutant_acetaldehyde, 
    asthma_adult_crude, 
    socioeconomic_vulnerability_vulnerable,
    alpha = 0.2
  )

# pretty much the same distributions

# child asthma and formaldehyde ----
full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  multivariate_plot(
    pollutant_formaldehyde, 
    asthma_child_crude, 
    gender_demographics_vulnerable
  )

full_data %>% 
  multivariate_plot(
    pollutant_formaldehyde, 
    asthma_child_crude, 
    majority,
    alpha = 0.2
  )
# counties with a high population of native people have relatively low asthma 
# in children (low count, though) 

full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  multivariate_plot(
    pollutant_formaldehyde, 
    asthma_child_crude, 
    socioeconomic_vulnerability_vulnerable,
    alpha = 0.2
  )
# low vulnerability has much higher slope?

# child asthma and acetaldehyde ----
full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  multivariate_plot(
    pollutant_acetaldehyde, 
    asthma_child_crude, 
    gender_demographics_vulnerable
  )

full_data %>% 
  multivariate_plot(
    pollutant_acetaldehyde, 
    asthma_child_crude, 
    majority,
    alpha = 0.2
  )

full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  multivariate_plot(
    pollutant_acetaldehyde, 
    asthma_child_crude, 
    socioeconomic_vulnerability_vulnerable,
    alpha = 0.2
  )

# doesn't seem like childhood asthma rates are super predictive of effects of 
# long term environmental conditions because they go against adult trends

# child asthma and carbon tetrachloride ----

full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  multivariate_plot(
    pollutant_carbon_tetrachloride, 
    asthma_child_crude, 
    gender_demographics_vulnerable
  )

full_data %>% 
  multivariate_plot(
    pollutant_carbon_tetrachloride, 
    asthma_child_crude, 
    majority,
    alpha = 0.2
  )

full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  multivariate_plot(
    pollutant_carbon_tetrachloride, 
    asthma_child_crude, 
    socioeconomic_vulnerability_vulnerable,
    alpha = 0.2
  )

# child asthma and 1,3 butadiene ----

full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  multivariate_plot(
    pollutant_1_3_butadiene, 
    asthma_child_crude, 
    gender_demographics_vulnerable
  )

full_data %>% 
  multivariate_plot(
    pollutant_1_3_butadiene, 
    asthma_child_crude, 
    majority,
    alpha = 0.2
  )

# very high slopes for native and black majority populations

full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  multivariate_plot(
    pollutant_1_3_butadiene, 
    asthma_child_crude, 
    socioeconomic_vulnerability_vulnerable,
    alpha = 0.2
  )

# asthma ed and formaldehyde ----
full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  multivariate_plot(
    pollutant_formaldehyde, 
    asthma_ed_crude, 
    gender_demographics_vulnerable
  )
# higher asthma ed in areas with high female population

full_data %>% 
  multivariate_plot(
    pollutant_formaldehyde, 
    asthma_ed_crude, 
    majority,
    alpha = 0.2
  )
# counties with a high population of black people has a negative slope???

full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  multivariate_plot(
    pollutant_formaldehyde, 
    asthma_ed_crude, 
    socioeconomic_vulnerability_vulnerable,
    alpha = 0.2
  )
# low vulnerability has much higher slope?

full_data %>% 
  multivariate_plot(
    pollutant_formaldehyde, 
    asthma_ed_crude, 
    age_demographics_vulnerable,
    alpha = 0.2
  )
# pretty much the same

# asthma ed and acetaldehyde ----
full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  multivariate_plot(
    pollutant_acetaldehyde, 
    asthma_ed_crude, 
    gender_demographics_vulnerable
  )

full_data %>% 
  multivariate_plot(
    pollutant_acetaldehyde, 
    asthma_ed_crude, 
    majority,
    alpha = 0.2
  )

full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  multivariate_plot(
    pollutant_acetaldehyde, 
    asthma_ed_crude, 
    socioeconomic_vulnerability_vulnerable,
    alpha = 0.2
  )

full_data %>% 
  multivariate_plot(
    pollutant_acetaldehyde, 
    asthma_ed_crude, 
    age_demographics_vulnerable,
    alpha = 0.2
  )
# pretty much the same as formaldehyde

# asthma ed and 1,3 butadiene ----
full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(gender_demographics_vulnerable)) %>% 
  multivariate_plot(
    pollutant_1_3_butadiene, 
    asthma_ed_crude, 
    gender_demographics_vulnerable
  )

full_data %>% 
  multivariate_plot(
    pollutant_1_3_butadiene, 
    asthma_ed_crude, 
    majority,
    alpha = 0.2
  )
# big difference between majority native and black counties and white

full_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(socioeconomic_vulnerability_vulnerable)) %>% 
  multivariate_plot(
    pollutant_1_3_butadiene, 
    asthma_ed_crude, 
    socioeconomic_vulnerability_vulnerable,
    alpha = 0.2
  )


full_data %>% 
  multivariate_plot(
    pollutant_1_3_butadiene, 
    asthma_ed_crude, 
    age_demographics_vulnerable,
    alpha = 0.2
  )


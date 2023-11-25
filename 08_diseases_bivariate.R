# load packages and data
library(tidyverse)
library(GGally)
load("data/full_air_quality_data.rda")

# bivariate between asthma and pollutants ----

# correlation matrix
full_data %>% 
  select(contains("asthma"), contains("days"), contains("pollutant")) %>% 
  ggcorr()

# interesting that there is not really a realtionship between asthma and PM/ozone
full_data %>% 
  bivariate_plot(days_over_o3_standard, asthma_adult_crude) +
  coord_cartesian(x = c(0, 50))

full_data %>% 
  bivariate_plot(days_over_pm_standard, asthma_adult_crude) +
  coord_cartesian(x = c(0, 20))

# scatterplots between adult asthma and formaldehyde and acetaldehyde
full_data %>% 
  bivariate_plot(pollutant_formaldehyde, asthma_adult_crude)
full_data %>% 
  bivariate_plot(pollutant_acetaldehyde, asthma_adult_crude)

# scatterplots between child asthma and formaldehyde and acetaldehyde
full_data %>% 
  bivariate_plot(pollutant_formaldehyde, asthma_child_crude)
full_data %>% 
  bivariate_plot(pollutant_acetaldehyde, asthma_child_crude)

# bivariate between cancer and pollutants ----

# correlation matrix
full_data %>% 
  select(cancer_adjusted, contains("days"), contains("pollutant")) %>% 
  ggcorr()

# woah carbon tetrachloride
full_data %>% 
  bivariate_plot(pollutant_carbon_tetrachloride, cancer_adjusted) 

# scatterplots between cancer and formaldehyde and acetaldehyde
full_data %>% 
  bivariate_plot(pollutant_formaldehyde, cancer_adjusted)
full_data %>% 
  bivariate_plot(pollutant_acetaldehyde, cancer_adjusted)

# bivariate between copd and pollutants ----
# correlation matrix
full_data %>% 
  select(contains("copd"), contains("days"), contains("pollutant")) %>% 
  ggcorr()

# formaldehyde and acetaldehyde again
full_data %>% 
  bivariate_plot(pollutant_formaldehyde, copd_adjusted)
full_data %>% 
  bivariate_plot(pollutant_acetaldehyde, copd_adjusted)

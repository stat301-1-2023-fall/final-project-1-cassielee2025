# Progress memo 1 ----

# load libraries
library(tidyverse)
library(janitor)
library(naniar)
library(skimr)

# Data quality & complexity check --------------------------------------

# load in the data
asthma_adult_crude <- read_csv("data/raw/adult-asthma-prevalence-crude/data_135355.csv") %>% 
  clean_names()

asthma_ed_adjusted <- read_csv("data/raw/asthma-emergency-department-age-adjusted/data_135130.csv") %>% 
  clean_names()

asthma_ed_crude <- read_csv("data/raw/asthma-emergency-department-crude/data_135101.csv") %>% 
  clean_names()

cancer_gender_adjusted <- read_csv("data/raw/cancer-gender-age-adjusted/data_135607.csv") %>% 
  clean_names()

cancer_race_ethnicity_adjusted <- read_csv("data/raw/cancer-race-ethnicity-age-adjusted/") %>% 
  clean_names()

child_asthma_age_crude <- read_csv("data/raw/child-asthma-prevalence-age-group-crude/data_135451.csv") %>% 
  clean_names()

child_asthma_ethnicity_adjusted <- read_csv("data/raw/cancer-race-ethnicity-age-adjusted/data_135618.csv") %>% 
  clean_names()

child_asthma_race_crude <- read_csv("data/raw/child-asthma-prevalence-race-crude/data_135512.csv") %>% 
  clean_names()

copd_adjusted <- read_csv("data/raw/copd-age-adjusted/data_135253.csv") %>% 
  clean_names()

copd_adjusted <- read_csv("data/raw/copd-crude/data_135330.csv") %>% 
  clean_names()

days_over_o3_standard <- read_csv("data/raw/days-over-O3-standard/data_135022.csv") %>% 
  clean_names()

days_over_pm_standard <- read_csv("data/raw/days-over-pm-standard/data_134954.csv") %>% 
  clean_names()

age_demographics <- read_csv("data/raw/demographics-age/data_160814.csv") %>% 
  clean_names()

gender_demographics <- read_csv("data/raw/demographics-gender/data_160859.csv") %>% 
  clean_names()

race_ethnicity <- read_csv("data/raw/demographics-race-ethnicity/") %>% 
  clean_names()

highway_living <- read_csv("data/raw/highway-living/data_151056.csv") %>% 
  clean_names()

highway_schools <- read_csv("data/raw/highway-schools/data_151116.csv") %>% 
  clean_names()

parks_access <- read_csv("data/raw/parks-access/data_161554.csv") %>% 
  clean_names()

pollutants <- read_csv("data/raw/selected-pollutant-concentration/data_134821.csv") %>% 
  clean_names()

socioeconomic_vulnerability <- read_csv("data/raw/socioeconomic-vulnerability-index/data_161326.csv") %>% 
  clean_names()

# types of variables
age_demographics %>% 
  skim_without_charts()

asthma_adult_crude %>% 
  skim_without_charts()

asthma_ed_adjusted %>% 
  skim_without_charts()

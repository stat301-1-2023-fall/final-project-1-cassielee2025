# Univariate analysis ------------------------------------------------------

# load libraries
library(tidyverse)
library(janitor)
library(naniar)
library(skimr)

# load data
asthma_adult_crude <- read_csv("data/raw/adult-asthma-prevalence-crude/data_135355.csv") %>% 
  clean_names()

asthma_child_crude <- read_csv("data/raw/child-asthma/data_134936.csv") %>% 
  clean_names()

asthma_ed_adjusted <- read_csv("data/raw/asthma-emergency-department-age-adjusted/data_135130.csv") %>% 
  clean_names()

asthma_ed_crude <- read_csv("data/raw/asthma-emergency-department-crude/data_135101.csv") %>% 
  clean_names()

cancer_adjusted <- read_csv("data/raw/cancer-age-adjusted/data_134453.csv") %>% 
  clean_names()

copd_adjusted <- read_csv("data/raw/copd-age-adjusted/data_135253.csv") %>% 
  clean_names()

copd_crude <- read_csv("data/raw/copd-crude/data_135330.csv") %>% 
  clean_names()

days_over_o3_standard <- read_csv("data/raw/days-over-O3-standard/data_135022.csv") %>% 
  clean_names()

days_over_pm_standard <- read_csv("data/raw/days-over-pm-standard/data_134954.csv") %>% 
  clean_names()

age_demographics <- read_csv("data/raw/demographics-age/data_160814.csv") %>% 
  clean_names()

gender_demographics <- read_csv("data/raw/demographics-gender/data_160859.csv") %>% 
  clean_names()

race_ethnicity <- read_csv("data/raw/demographics-race-ethnicity/data_161208.csv") %>% 
  clean_names()

highway_living <- read_csv("data/raw/highway-living/data_151056.csv") %>% 
  clean_names()

highway_schools <- read_csv("data/raw/highway-schools/data_151116.csv") %>% 
  clean_names()

parks_access <- read_csv("data/raw/parks-access/data_143718.csv") %>% 
  clean_names()

pollutants <- read_csv("data/raw/selected-pollutant-concentration/data_134821.csv") %>% 
  clean_names()

socioeconomic_vulnerability <- read_csv("data/raw/socioeconomic-vulnerability-index/data_161326.csv") %>% 
  clean_names()

work_transportation <- read_csv("data/raw/work-transportation/data_151035.csv") %>% 
  clean_names()
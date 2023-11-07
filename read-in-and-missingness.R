# Progress memo 1 --------------------------------------------------------

# load libraries
library(tidyverse)
library(janitor)
library(naniar)
library(skimr)

# Data quality & complexity check --------------------------------------

# load in the data
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

# types of variables -----------------------------------------------------
# readr reads ID variables like state_fips, state, county_fips, and 
# county as categorical

## age ----
age_demographics %>% 
  skim_without_charts()

## asthma adult crude ----
asthma_adult_crude %>% 
  skim_without_charts()

## asthma child crude ----
asthma_child_crude %>% 
  skim_without_charts() # value is character vector here

# modify value variable in asthma_child_crude
asthma_child_crude %>% 
  arrange(desc(value)) # Suppressed values are changing the value to character

asthma_child_crude <- asthma_child_crude %>% 
  mutate(value = na_if(value, "Data Not Collected"),
         value = as.numeric(value)) 

asthma_child_crude %>% 
  skim_without_charts()

## asthma ed adjusted ----
asthma_ed_adjusted %>% 
  skim_without_charts() # value is character vector here

# modify value variable in asthma_ed_adjusted
asthma_ed_adjusted %>% 
  arrange(desc(value)) # Suppressed values are changing the value to character

asthma_ed_adjusted <- asthma_ed_adjusted %>% 
  mutate(value = na_if(value, "Suppressed"),
         value = as.numeric(value)) 

asthma_ed_adjusted %>% 
  skim_without_charts()

## asthma ed crude ----
asthma_ed_crude %>% 
  skim_without_charts() # value is character vector

# modify value variable in asthma_ed_crude
asthma_ed_crude %>% 
  arrange(desc(value)) # Suppressed values are changing the value to character

asthma_ed_crude <- asthma_ed_crude %>% 
  mutate(value = na_if(value, "Suppressed"),
         value = as.numeric(value)) 

asthma_ed_crude %>% 
  skim_without_charts()

## cancer adjusted ----
cancer_adjusted %>% 
  skim_without_charts()

## copd adjusted ----
copd_adjusted %>% 
  skim_without_charts()

## copd crude ----
copd_crude %>% 
  skim_without_charts()

## days over o3 standard ----
days_over_o3_standard %>% 
  skim_without_charts()

## days over pm standard ----
days_over_pm_standard %>% 
  skim_without_charts()

## gender demographics ----
gender_demographics %>% 
  skim_without_charts()

## highway living ----
highway_living %>% 
  skim_without_charts()

## highway schools ----
highway_schools %>% 
  skim_without_charts()

## parks access ----
parks_access %>% 
  skim_without_charts()

## pollutants  ----
pollutants %>% 
  skim_without_charts()

## race ethnicity ----
race_ethnicity %>% 
  skim_without_charts()

## socioeconomic vulnerability ----
socioeconomic_vulnerability %>% 
  skim_without_charts()

## work transportation ----
work_transportation %>% 
  skim_without_charts()

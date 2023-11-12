# join dataset ------------------------------------------------------

# load libraries
library(tidyverse)
library(janitor)

# load data and modify based on univariate analysis ----------------------

age_demographics <- read_csv("data/raw/demographics-age/data_160814.csv") %>% 
  clean_names() %>% 
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
  filter(age_group %in% c("0 TO 19", "65 AND OLDER")) %>% 
  mutate(
    vulnerable = value >= mean(value) + sd(value),
    .by = age_group,
    .keep = "all"
  )

asthma_adult_crude <- read_csv("data/raw/adult-asthma-prevalence-crude/data_135355.csv") %>% 
  clean_names()

asthma_child_crude <- read_csv("data/raw/child-asthma/data_134936.csv") %>% 
  clean_names() %>% 
  mutate(value = na_if(value, "Data Not Collected"),
         value = as.numeric(value))

asthma_ed_adjusted <- read_csv("data/raw/asthma-emergency-department-age-adjusted/data_135130.csv") %>% 
  clean_names() %>% 
  mutate(value = na_if(value, "Suppressed"),
         value = as.numeric(value)) 

asthma_ed_crude <- read_csv("data/raw/asthma-emergency-department-crude/data_135101.csv") %>% 
  clean_names() %>% 
  mutate(value = na_if(value, "Suppressed"),
         value = as.numeric(value)) 

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

gender_demographics <- read_csv("data/raw/demographics-gender/data_160859.csv") %>% 
  clean_names() %>% 
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
  )

race_and_ethnicity <- read_csv("data/raw/demographics-race-ethnicity/data_161208.csv") %>% 
  clean_names() %>% 
  separate_wider_regex(
    race_ethnicity,
    patterns = c(
      race = ".*",
      " including Hispanic"
    ),
    too_few = "align_start"
  ) %>% 
  filter(race != "Hispanic All Races") %>% 
  filter(race != "All Non-White Races") %>% 
  group_by(county_fips) %>% 
  slice(which.max(value))

highway_living <- read_csv("data/raw/highway-living/data_151056.csv") %>% 
  clean_names()

highway_schools <- read_csv("data/raw/highway-schools/data_151116.csv") %>% 
  clean_names()

parks_access <- read_csv("data/raw/parks-access/data_143718.csv") %>% 
  clean_names() %>% 
  filter(distance_to_parks == "Distance to Parks: 1/2 Mile")

pollutants <- read_csv("data/raw/selected-pollutant-concentration/data_134821.csv") %>% 
  clean_names()

socioeconomic_vulnerability <- read_csv("data/raw/socioeconomic-vulnerability-index/data_161326.csv") %>% 
  clean_names() %>% 
  arrange(value) %>% 
  mutate(
    vulnerable = if_else(
      value >= 0.75, 
      "high vulnerability", 
      "low vulnerability"
    )
  )

transportation_active <- read_csv("data/raw/transportation_active/data_155606.csv") %>% 
  clean_names()

transportation_none <- read_csv("data/raw/transportation_none/data_160159.csv") %>% 
  clean_names()

transportation_private <- read_csv("data/raw/transportation_private/data_160024.csv") %>% 
  clean_names()

transportation_public <- read_csv("data/raw/transportation_public/data_160116.csv") %>% 
  clean_names()

# modify variable name in each dataset -------------------------------------
age_demographics <- rename_val_vul(age_demographics, value, vulnerable)
asthma_adult_crude <- rename_val_vul(asthma_adult_crude, value)
asthma_child_crude <- rename_val_vul(asthma_child_crude, value)
asthma_ed_adjusted <- rename_val_vul(asthma_ed_adjusted, value)
asthma_ed_crude <- rename_val_vul(asthma_ed_crude, value)
cancer_adjusted <- rename_val_vul(cancer_adjusted, value)
copd_adjusted <- rename_val_vul(copd_adjusted, value)
copd_crude <- rename_val_vul(copd_crude, value)
days_over_o3_standard <- rename_val_vul(days_over_o3_standard, value)
days_over_pm_standard <- rename_val_vul(days_over_pm_standard, value)
gender_demographics <- rename_val_vul(gender_demographics, value, vulnerable)
highway_living <- rename_val_vul(highway_living, value)
highway_schools <- rename_val_vul(highway_schools, value)
parks_access <- rename_val_vul(parks_access, value)
pollutants <- rename_val_vul(pollutants, value)
race_and_ethnicity <- rename_val_vul(race_and_ethnicity, value)
socioeconomic_vulnerability <- rename_val_vul(socioeconomic_vulnerability, value, vulnerable)
transportation_active <- rename_val_vul(transportation_active, value)
transportation_none <- rename_val_vul(transportation_none, value)
transportation_private <- rename_val_vul(transportation_private, value)
transportation_public <- rename_val_vul(transportation_public, value)

# select variables in each dataset -----------------------------------------
asthma_adult_crude <- asthma_adult_crude %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
asthma_child_crude <- asthma_child_crude %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
asthma_ed_adjusted <- asthma_ed_adjusted %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
asthma_ed_crude <- asthma_ed_crude %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
cancer_adjusted <- cancer_adjusted %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
copd_adjusted <- copd_adjusted %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
copd_crude <- copd_crude %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
days_over_o3_standard <- days_over_o3_standard %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
days_over_pm_standard <- days_over_pm_standard %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
gender_demographics <- gender_demographics %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
highway_living <- highway_living %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
highway_schools <- highway_schools %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
parks_access <- parks_access %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
pollutants <- pollutants %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
race_and_ethnicity <- race_and_ethnicity %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
socioeconomic_vulnerability <- socioeconomic_vulnerability %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
transportation_active <- transportation_active %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
transportation_none <- transportation_none %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
transportation_private <- transportation_private %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))
transportation_public <- transportation_public %>% 
  select(-contains(c("comment", "confidence", "x", "year", "state_fips")))

# join datasets -----------------------------------------
age_demographics %>% 
  full_join(asthma_adult_crude) %>% 
  full_join(asthma_child_crude) %>% 
  full_join(asthma_ed_adjusted) %>% 
  full_join(asthma_ed_crude) %>% 
  full_join(cancer_adjusted) %>% 
  full_join(copd_adjusted) %>% 
  full_join(copd_crude) %>% 
  full_join(days_over_o3_standard) %>% 
  full_join(days_over_pm_standard) %>% 
  full_join(gender_demographics) %>% 
  full_join(highway_living) %>% 
  full_join(highway_schools) %>% 
  full_join(parks_access) %>% 
  view()


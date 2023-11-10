# Join dataset ------------------------------------------------------

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
  clean_names()

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

# modify variable name in each dataset ----

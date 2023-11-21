# join dataset ----

# load libraries
library(tidyverse)
library(janitor)
library(tigris)
library(sf)

# read in counties ----

states <- states(year = 2018, progress_bar = FALSE) %>% 
  clean_names() %>% 
  rename(
    state_fips = statefp,
    state = name
  ) %>% 
  select(c(state, state_fips)) %>% 
  st_drop_geometry()

states_list <- list(
  "Alabama",
  "Alaska",
  "Arizona",
  "Arkansas",
  "California",
  "Colorado",
  "Connecticut",
  "Delaware",
  "District of Columbia",
  "Florida",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Illinois",
  "Indiana",
  "Iowa",
  "Kansas",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "Minnesota",
  "Mississippi",
  "Missouri",
  "Montana",
  "Nebraska",
  "Nevada",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "New York",
  "North Carolina",
  "North Dakota",
  "Ohio",
  "Oklahoma",
  "Oregon",
  "Pennsylvania",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Tennessee",
  "Texas",
  "Utah",
  "Vermont",
  "Virginia",
  "Washington",
  "West Virginia",
  "Wisconsin",
  "Wyoming"
)

counties <- counties(year = 2018, progress_bar = FALSE) %>% 
  clean_names() %>% 
  # make a county_fips that matches the downloaded datasets
  mutate(county_fips = paste0(statefp, countyfp)) %>% 
  rename(
    state_fips = statefp,
    county = name
  ) %>% 
  # select only needed variables
  select(c(county, state_fips, county_fips)) %>% 
  left_join(states) %>% 
  filter(state %in% states_list)

# load data and modify based on univariate analysis -----

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
  # tidy data
  pivot_wider(
    names_from = age_group,
    values_from = value
  ) %>%
  clean_names() %>% 
  # identify vulnerability in either young or old population
  mutate(
    vulnerable = x0_to_19 >= mean(x0_to_19) + sd(x0_to_19) | 
      x65_and_older >= mean(x65_and_older) + sd(x65_and_older),
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
  # tidy data
  pivot_wider(
    names_from = race,
    values_from = value
  ) %>% 
  clean_names() %>% 
  get_max_column(
    c(
      white, 
      black, 
      other, 
      asian_pacific_islander,
      american_indian_alaskan_native
    ), 
    county_fips
  )

highway_living <- read_csv("data/raw/highway-living/data_151056.csv") %>% 
  clean_names()

highway_schools <- read_csv("data/raw/highway-schools/data_151116.csv") %>% 
  clean_names()

parks_access <- read_csv("data/raw/parks-access/data_143718.csv") %>% 
  clean_names() %>% 
  filter(distance_to_parks == "Distance to Parks: 1/2 Mile")

pollutants <- read_csv("data/raw/selected-pollutant-concentration/data_134821.csv") %>% 
  clean_names() %>% 
  # tidy data
  pivot_wider(
    names_from = pollutant,
    values_from = value
  ) %>% 
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
  clean_names() %>% 
  # tidy data
  pivot_wider(
    names_from = transportation_type,
    values_from = value
  ) %>% 
  clean_names()

transportation_none <- read_csv("data/raw/transportation_none/data_160159.csv") %>% 
  clean_names()

transportation_private <- read_csv("data/raw/transportation_private/data_160024.csv") %>% 
  clean_names() %>% 
  # tidy data
  pivot_wider(
    names_from = occupancy,
    values_from = value
  ) %>% 
  clean_names

transportation_public <- read_csv("data/raw/transportation_public/data_160116.csv") %>% 
  clean_names()

# are there any counties that will not get joined? ----
anti_join(age_demographics, counties, join_by(county_fips))
# county fips 02270 and 46113
anti_join(asthma_adult_crude, counties, join_by(county_fips))
# county fips 02270 and 46113
anti_join(asthma_child_crude, counties, join_by(state))
anti_join(asthma_ed_adjusted, counties, join_by(county_fips))
anti_join(asthma_ed_crude, counties, join_by(county_fips))
anti_join(cancer_adjusted, counties, join_by(state))
anti_join(copd_adjusted, counties, join_by(county_fips))
# county fips 02270 and 46113
anti_join(copd_crude, counties, join_by(county_fips))
# county fips 02270 and 46113
anti_join(copd_adjusted, counties, join_by(county_fips))
anti_join(days_over_o3_standard, counties, join_by(county_fips))
anti_join(days_over_pm_standard, counties, join_by(county_fips))
anti_join(gender_demographics, counties, join_by(county_fips))
# county fips 02270 and 46113
anti_join(gender_demographics, counties, join_by(county_fips))
anti_join(highway_living, counties, join_by(county_fips))
# county fips 02063 and 02066
anti_join(highway_schools, counties, join_by(county_fips))
anti_join(parks_access, counties, join_by(county_fips))
# county fips 02063 and 02066
anti_join(pollutants, counties, join_by(county_fips))
anti_join(race_and_ethnicity, counties, join_by(county_fips))
# county fips 02270 and 46113
anti_join(socioeconomic_vulnerability, counties, join_by(county_fips))
# county fips 02270 and 46113
anti_join(transportation_active, counties, join_by(county_fips))
# county fips 02270, 46113, 02063, and 02066
anti_join(transportation_none, counties, join_by(county_fips))
# county fips 02270, 46113, 02063, and 02066
anti_join(transportation_private, counties, join_by(county_fips))
# county fips 02270, 46113, 02063, and 02066
anti_join(transportation_public, counties, join_by(county_fips))
# county fips 02270, 46113, 02063, and 02066

# mutate Wade Hampton (fips 46113) and Shannon (fips 02270) counties to 
# Kusilvak (02158) and Oglala Lakota (fips 46102)
age_demographics <- age_demographics %>% 
  rename_counties(county, county_fips)
asthma_adult_crude <- asthma_adult_crude %>% 
  rename_counties(county, county_fips)
copd_adjusted <- copd_adjusted %>% 
  rename_counties(county, county_fips)
copd_crude <- copd_crude %>% 
  rename_counties(county, county_fips)
gender_demographics <- gender_demographics %>% 
  rename_counties(county, county_fips)
race_and_ethnicity <- race_and_ethnicity %>% 
  rename_counties(county, county_fips)
socioeconomic_vulnerability <- socioeconomic_vulnerability %>% 
  rename_counties(county, county_fips)
transportation_active <- transportation_active %>% 
  rename_counties(county, county_fips)
transportation_none <- transportation_none %>% 
  rename_counties(county, county_fips)
transportation_private <- transportation_private %>% 
  rename_counties(county, county_fips)
transportation_public <- transportation_public %>% 
  rename_counties(county, county_fips)
# modify variable name in each dataset ----
age_demographics <- rename_val_vul(age_demographics, variable2 = vulnerable)
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
socioeconomic_vulnerability <- rename_val_vul(socioeconomic_vulnerability, value, vulnerable)
transportation_none <- rename_val_vul(transportation_none, value)
transportation_public <- rename_val_vul(transportation_public, value)

# select variables in each dataset ----
age_demographics <- age_demographics %>% 
  select(-c("county", "state"))
asthma_adult_crude <- asthma_adult_crude %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
asthma_child_crude <- asthma_child_crude %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "state"))
asthma_ed_adjusted <- asthma_ed_adjusted %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
asthma_ed_crude <- asthma_ed_crude %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
cancer_adjusted <- cancer_adjusted %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "state"))
copd_adjusted <- copd_adjusted %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
copd_crude <- copd_crude %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
days_over_o3_standard <- days_over_o3_standard %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
days_over_pm_standard <- days_over_pm_standard %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
gender_demographics <- gender_demographics %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
highway_living <- highway_living %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
highway_schools <- highway_schools %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
parks_access <- parks_access %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips", "distance_to_parks"))
pollutants <- pollutants %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
race_and_ethnicity <- race_and_ethnicity %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
socioeconomic_vulnerability <- socioeconomic_vulnerability %>% 
  select(-c(contains(c("comment", "confidence", "x")), "year", "county", "state", "state_fips"))
transportation_active <- transportation_active %>% 
  select(-c(contains(c("comment", "confidence", "x", "year")), "county", "state", "state_fips"))
transportation_none <- transportation_none %>% 
  select(-c(contains(c("comment", "confidence", "x", "year")), "county", "state", "state_fips"))
transportation_private <- transportation_private %>% 
  select(-c(contains(c("comment", "confidence", "x", "year")), "county", "state", "state_fips"))
transportation_public <- transportation_public %>% 
  select(-c(contains(c("comment", "confidence", "x", "year")), "county", "state", "state_fips"))

# join datasets ----
full_data <- counties %>% 
  left_join(age_demographics, join_by(county_fips)) %>% 
  left_join(asthma_adult_crude, join_by(county_fips)) %>%   
  left_join(asthma_child_crude, join_by(state_fips)) %>%   
  left_join(asthma_ed_adjusted, join_by(county_fips)) %>%   
  left_join(asthma_ed_crude, join_by(county_fips)) %>%   
  left_join(cancer_adjusted, join_by(state_fips)) %>%   
  left_join(copd_adjusted, join_by(county_fips)) %>%   
  left_join(copd_crude, join_by(county_fips)) %>%   
  left_join(days_over_o3_standard, join_by(county_fips)) %>%   
  left_join(days_over_pm_standard, join_by(county_fips)) %>%   
  left_join(gender_demographics, join_by(county_fips)) %>%   
  left_join(highway_living, join_by(county_fips)) %>%   
  left_join(highway_schools, join_by(county_fips)) %>% 
  left_join(parks_access, join_by(county_fips)) %>% 
  left_join(pollutants, join_by(county_fips)) %>% 
  left_join(race_and_ethnicity, join_by(county_fips)) %>% 
  left_join(socioeconomic_vulnerability, join_by(county_fips)) %>% 
  left_join(transportation_active, join_by(county_fips)) %>% 
  left_join(transportation_none, join_by(county_fips)) %>% 
  left_join(transportation_private, join_by(county_fips)) %>% 
  left_join(transportation_public, join_by(county_fips))

# save data as .rda
save(full_data, file = "data/full_air_quality_data.rda")


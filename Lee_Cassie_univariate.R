# Univariate analysis ------------------------------------------------------

# load libraries
library(tidyverse)
library(janitor)
library(naniar)
library(skimr)

# load data ----------------------------------------------------------------
age_demographics <- read_csv("data/raw/demographics-age/data_160814.csv") %>% 
  clean_names()

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

# Histograms ---------------------------------------------------------------

## age_demographics ----
# percentage of demographic in each age group
age_demographics %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  facet_wrap(~age_group)
# as a frequency polygon
age_demographics %>% 
  ggplot(aes(value, color = age_group)) +
  geom_freqpoly(binwidth = 1, boundary = 0) +
  theme(legend.position = "top")
# could highlight age groups most vulnerable to air pollution
# and then identify counties that are over a certain percent of population 
# in that age group
# children <18 years and older adults over 65

# need to combine 0 to 4 and 5 to 19 age ranges
age_demographics2 <- age_demographics %>% 
  mutate(
    # collapse the age groups
    age_group = factor(age_group),
    age_group = fct_collapse(
      age_group,
      "0 TO 19" = c("0 TO 4", "5 TO 19")
    )
  ) %>% 
  # edit the value variable
  summarise(
    value = sum(value),
    .by = c(state, county, county_fips, age_group)
  ) %>% 
  filter(age_group %in% c("0 TO 19", "65 AND OLDER")) 

# plot it
age_demographics2 %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  facet_wrap(~age_group)

# the certain percentage of population could be one sd above the mean
# since the distributions are pretty normal
age_demographics2 %>% 
  mutate(
    # create a variable that is true if the percent is over one standard
    # deviation above the mean, false if it is not
    vulnerable = value >= mean(value) + sd(value),
    .by = age_group,
    .keep = "all"
  ) %>% 
  # graph it to see if it works
  ggplot(aes(value, fill = vulnerable)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  facet_wrap(~age_group) # yay

age_vulnerability <- age_demographics2 %>% 
  mutate(
    # create a variable that is true if the percent is over one standard
    # deviation above the mean, false if it is not
    vulnerable = value >= mean(value) + sd(value),
    .by = age_group,
    .keep = "all"
  ) 

## asthma_adult_crude ----
# crude percentage of adults with asthma
asthma_adult_crude %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.5, boundary = 0)
# pretty normally distributed
asthma_adult_crude %>% 
  ggplot(aes(value)) +
  geom_histogram(boundary = 0) +
  coord_cartesian(ylim = c(0, 5))
# there are a couple counties with >14% of adults with asthma

## asthma_child_crude ----
# crude percentage of children with asthma
asthma_child_crude %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1, boundary = 0)
# lots of missing data, only a few states have this data reported
# peak is much higher (~12%) than adults

## asthma_ed_adjusted ----
# age-adjusted rate of emergency department visits for asthma 
# per 10,000 population
asthma_ed_adjusted %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 10, boundary = 0)
# right skewed
# might want to convert this to a percent later
asthma_ed_adjusted %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 10, boundary = 0) +
  coord_cartesian(ylim = c(0, 5))
# a couple of counties with >200 ed visits per 10K
# are they the same counties as the ones with high adult asthma prevalence?

## asthma_ed_crude ----
# crude rate of emergency department visits for asthma 
# per 10,000 population
asthma_ed_crude %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 10, boundary = 0)
# right skewed
# might want to convert this to a percent later
asthma_ed_crude %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 10, boundary = 0) +
  coord_cartesian(ylim = c(0, 5))
# a couple of counties with >150 ed visits per 10K
# are they the same counties as the ones with high adult asthma prevalence?

## cancer_adjusted ----
# age adjusted rate of lung and bronchus cancer per 100,000 population
cancer_adjusted %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 8, boundary = 0)
# huh doesn't look like there are outliers

## copd_adjusted ----
# age adjusted percent of copd in adults 
copd_adjusted %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1, boundary = 0)
# units from https://www.cdc.gov/copd/data-and-statistics/county-estimates.html
# kind of skewed right

## copd_crude ----
# crude percent of copd in adults 
copd_crude %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1, boundary = 0)
# units from https://www.cdc.gov/copd/data-and-statistics/county-estimates.html
# also kind of skewed right

## days_over_o3_standard ----
# number of days each year in which the maximum 8-hour ozone concentration 
# within each county exceeded the 8-hr NAAQS of 0.070 ppm
days_over_o3_standard %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 10, boundary = 0)
# super right skewed
days_over_o3_standard %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 10, boundary = 0) +
  coord_cartesian(ylim = c(0, 5))
# couple counties with pretty high days over ozone standard
# once again wonder if that has to do with asthma....
# look into this:
# https://www.epa.gov/ground-level-ozone-pollution/health-effects-ozone-pollution

## days_over_pm_standard ----
# number of days each year in which the maximum PM2.5 concentration within 
# each county exceeded the 24-hr PM2.5 NAAQS of 35 mg/m3
days_over_pm_standard %>% 
  ggplot(aes(value)) +
  geom_histogram(boundary = 0)
# skewed right
days_over_pm_standard %>% 
  ggplot(aes(value)) +
  geom_histogram(boundary = 0) +
  coord_cartesian(ylim = c(0, 5))
# there are a lot fewer days with high PM2.5 than ozone
# wonder if the high values have to do with asthma...

## gender_demographics ----
gender_demographics %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  facet_wrap(~gender) # very tight distribution
# there are counties with very few women and mostly men

# identify counties that have very low percent of women 
# or higher percent of women
gender_demographics %>% 
  # filter out men bc we only care about women
  filter(gender == "Female") %>% 
  mutate(
    vulnerable = if_else(
      value <= mean(value) - sd(value),
      # if low percentage of women,
      "low",
      # if not low percentage of women
      if_else(
        value >= mean(value) + sd(value),
        # if high percentage of women
        "high",
        # if not low or high
        NA
      )
    ),
    .keep = "all"
  ) %>% 
  # graph it to see if it works
  ggplot(aes(value, fill = vulnerable)) +
  geom_histogram(binwidth = 2, boundary = 0)

gender_vulnerability <- gender_demographics %>% 
  # filter out men bc we only care about women
  filter(gender == "Female") %>% 
  mutate(
    vulnerable = if_else(
      value <= mean(value) - sd(value),
      # if low percentage of women,
      "low",
      # if not low percentage of women
      if_else(
        value >= mean(value) + sd(value),
        # if high percentage of women
        "high",
        # if not low or high
        NA
      )
    ),
    .keep = "all"
  )

## highway_living ----
highway_living

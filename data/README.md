# Data source

All data was selected and downloaded from <http://www.cdc.gov/ephtracking>:

Citation: 
Centers for Disease Control and Prevention. National Environmental Public Health Tracking Network. (n.d.) Web. Accessed: 10/20/2023. [www.cdc.gov/ephtracking](www.cdc.gov/ephtracking).

`transportation_active`, `transportation_none`, `transportation_private`, `transportation_public` were accessed on 11/01/2023.

A list of counties was obtained from the `tigris` package.

# Structure

## Raw data

Raw data is located in `/raw`.

## Joined dataset

The dataset `full_air_quality_data` is saved as a .rda file in `/data`. This file was created using `left_join` between all raw datasets, after cleaning data and the list of counties obtained from the `tigris` package.

Within the main folder, `02_univariate_analysis.R` contains documentation of the steps used to determine how to clean the data before joining. `04_funtions_for_join.R` contains documentation for functions used to clean and join the dataset. `03_clean_and_join.R` contains documentation for joining the datasets.
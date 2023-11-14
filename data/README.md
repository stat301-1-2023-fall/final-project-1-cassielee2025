# Data source

All data was selected and downloaded from http://www.cdc.gov/ephtracking:

Citation: 
Centers for Disease Control and Prevention. National Environmental Public Health Tracking Network. (n.d.) Web. Accessed: 10/20/2023. www.cdc.gov/ephtracking.

`transportation_active`, `transportation_none`, `transportation_private`, `transportation_public` were accessed on 11/01/2023.

County level datasets were obtained from https://simplemaps.com/data/us-counties.

# Structure

## Raw data

Raw data is located in /raw.

## Joined dataset

The dataset `full_air_quality_data` is saved as a .rda file in /data. This file was created using `full_join` between all raw datasets, after cleaning data.

`univariate_analysis.R` contains documentation of the steps involved in cleaning the data before joining. `funtions_for_join.R` contains documentation of a function used to rename variables in each dataset before joining. `join_datasets.R` contains documentation for joining the datasets.
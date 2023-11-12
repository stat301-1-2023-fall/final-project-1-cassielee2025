library(tidyverse)

## renaming "value" variable in all datasets ----
rename_value <- function(df, variable) {
  # copy name of dataset
  name <- deparse(substitute(df))
  
  # change value to new name
  df %>%
    rename(!!name := {{variable}}) 
}

# why does this not work in tidy?
# this works
rename_value(age_demographics, value)

# this doesn't work
age_demographics %>% 
  rename_value(value)

# make a reprex ----
library(tidyverse)

# random data
mydata <- tibble(
  variable1 = 1:5,
  value = 6:10
)

# renaming "value" variable in all datasets
rename_value <- function(df, variable) {
  # copy name of dataset
  name <- deparse(substitute(df))
  
  # change value to new name
  df %>%
    rename(!!name := {{variable}}) 
}

# why does this not work in tidy?
# this works
rename_value(mydata, value)

# this doesn't work
mydata %>% 
  rename_value(value)

## try this from campus wire ----
library(tidyverse)

# function
rename_value <- function(df, df_name, variable) {
  df %>%
    rename_with(~ df_name, all_of(variable))
}

# random data
mydata <- tibble(
  variable1 = 1:5,
  value = 6:10
)

# function on data
my_data %>% 
  rename_value(my_data, value)


## rename value and vulnerability together USE THIS----
mydata <- tibble(
  value = 1:5,
  data = 6:10,
  vulnerable = 11:15
)

rename_value_vul <- function(df, variable1, variable2 = NULL) {
  # copy name of dataset
  name <- deparse(substitute(df))
  
  # change value and vulnerable to new name
  df %>%
    rename({{name}} := {{variable1}}) %>% 
    rename_with(~paste(name, .x, sep = "_"), {{variable2}})
  
}

rename_value_vul(mydata, value, vulnerable)

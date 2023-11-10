library(tidyverse)

# renaming "value" variable in all datasets ----
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

# renaming "value" variable in all datasets ----
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

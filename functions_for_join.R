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

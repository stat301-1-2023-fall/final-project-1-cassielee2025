library(tidyverse)

# renaming "value" variable in all datasets ----
rename_value <- function(df, variable) {
  # copy name of dataset
  name <- deparse(substitute(df))
  
  # change value to new name
  df %>%
    rename(!!name := {{variable2}}) 

}

# why does this not work in tidy?
rename_value(age_demographics, value)

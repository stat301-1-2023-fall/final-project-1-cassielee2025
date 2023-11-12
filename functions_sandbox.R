library(tidyverse)

# trying to make the function work ----
## rename "value" variable in all datasets ----
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

# make a reprex for campus wire ----
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

## rename value and vulnerability together ----
mydata <- tibble(
  value = 1:5,
  data = 6:10,
  vulnerable = 11:15
)

mydata2 <- tibble(
  value = 1:5,
  data = 6:10,
)

rename_val_vul <- function(df, variable1, variable2 = NULL) {
  # copy name of dataset
  name <- deparse(substitute(df))
  
  # change value and vulnerable to new name
  df %>%
    rename({{name}} := {{variable1}}) %>% 
    rename_with(~paste(name, .x, sep = "_"), {{variable2}})
  
}

rename_val_vul(mydata, value, vulnerable)

# oh it cannot handle when vulnerable is not in there
rename_val_vul(mydata2, value, vulnerable)

# make an if_else
rename_val_vul2 <- function(df, variable1, variable2 = NULL) {
  # copy name of dataset
  name <- deparse(substitute(df))
  
  # depending on if vulnerable is in the data frame
  if_else("vulnerable" %in% names(mydata),
          # change value and vulnerable to new name
          df %>%
            rename({{name}} := {{variable1}}) %>% 
            rename_with(~paste(name, .x, sep = "_"), {{variable2}}),
          # change value to new name
          df %>%
            rename({{name}} := {{variable1}})
          )

}

rename_val_vul(mydata, value, vulnerable)
rename_val_vul2(mydata, value, vulnerable)


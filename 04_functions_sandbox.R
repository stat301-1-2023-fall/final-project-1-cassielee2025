library(tidyverse)

# rename value variable in all datasets ----
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

## make a reprex for campus wire ----
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

# rename value and vulnerability together ----
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

# USE THIS: rename "value" and "vulnerable" with name of dataframe ----
rename_val_vul <- function(df, variable1 = NULL, variable2 = NULL) {
  # copy name of dataset
  name <- deparse(substitute(df))
  
  # depending on if vulnerable is in the data frame
  if("vulnerable" %in% names(df)){
    df <- df %>%
      rename({{  name  }} := {{  variable1  }}) %>% 
      rename_with(~paste(name, .x, sep = "_"), {{  variable2  }})
  }
  else{
    df <- df %>%
      rename({{  name  }} := {{  variable1  }})
  }
  
  return(df)
  
}


# getting the name of the column with the highest value ----
get_max_column <- function(df, columns, grouping = NULL) {
  df %>% 
    # only work with columns of interest
    select(c({{  columns  }}, {{  grouping  }})) %>% 
    # pivot longer to be able to find max value
    pivot_longer(
      -{{  grouping  }},
      names_to = "category",
      values_to = "max_value"
    ) %>%
    # group by desired grouping
    group_by({{  grouping  }}) %>% 
    # find the maximum value
    slice_max(order_by = max_value) %>%
    # remove the maximum value from the mini dataframe
    select(-max_value) %>% 
    # rename category into majority
    rename("majority" := category) %>%
    # join it back to the main dataframe
    right_join(df, join_by({{  grouping  }}))
}

race_and_ethnicity %>% 
  get_max_column(c(white, black, other, asian_pacific_islander,
                   american_indian_alaskan_native), county_fips) # yay!

# renaming count and fips function ----
rename_counties <- function(df, county, fips) {
  df %>% 
    mutate(
      county = if_else(
        county == "Wade Hampton",
        "Kusilvak",
        county
      ),
      county = if_else(
        county == "Shannon",
        "Oglala Lakota",
        county
      ),
      county_fips = if_else(
        county_fips == "46113",
        "02158",
        county_fips
      ),
      county_fips = if_else(
        county_fips == "02270",
        "46102",
        county_fips
      )
    )
}


# bivariate analysis: scatterplot ----
bivariate_plot <- function(dataframe, variable1, variable2){
  
  dataframe %>% 
    ggplot(aes({{ variable1 }}, {{ variable2 }})) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    theme_minimal()
}

# multivariate analysis: scatterplot with color ----
multivariate_plot <- function(dataframe, var1, var2, var3, ...){
  
  dataframe %>% 
    ggplot(
      aes({{ var1 }}, {{ var2 }}, color = {{ var3 }})
    ) +
    # add alpha using ...
    geom_point(...) +
    geom_smooth(method = lm, se = FALSE) +
    theme_minimal()
}


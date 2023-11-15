library(tidyverse)

# rename "value" and "vulnerable" with name of dataframe ----
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

library(tidyverse)

# rename "value" and "vulnerable" with name of dataframe
rename_val_vul <- function(df, variable1, variable2 = NULL) {
  # copy name of dataset
  name <- deparse(substitute(df))
  
  # depending on if vulnerable is in the data frame
  if("vulnerable" %in% names(df)){
    df <- df %>%
      rename({{name}} := {{variable1}}) %>% 
      rename_with(~paste(name, .x, sep = "_"), {{variable2}})
  }
  else{
    df <- df %>%
      rename({{name}} := {{variable1}})
  }
  
  return(df)
  
}

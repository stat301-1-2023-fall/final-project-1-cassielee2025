library(tidyverse)

# bivariate analysis: scatterplot
bivariate_plot <- function(dataframe, variable1, variable2){
  
  dataframe %>% 
    ggplot(aes({{ variable1 }}, {{ variable2 }})) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    theme_minimal()
}

library(tidyverse)

# multivariate plot ----
multivariate_plot <- function(dataframe, variable1, variable2, variable3){
  
  dataframe %>% 
    ggplot(
      aes({{ variable1 }}, {{ variable2 }}, color = {{ variable3 }})
    ) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    theme_minimal()
}

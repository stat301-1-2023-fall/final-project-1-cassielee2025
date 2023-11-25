library(tidyverse)

# multivariate plot ----
multivariate_plot <- function(dataframe, var1, var2, var3, ...){
  
  dataframe %>% 
    ggplot(
      aes({{ var1 }}, {{ var2 }}, color = {{ var3 }})
    ) +
    geom_point(...) +
    geom_smooth(method = lm, se = FALSE) +
    theme_minimal()
}


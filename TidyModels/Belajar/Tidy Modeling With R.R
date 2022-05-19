# COMBINING BASE R MODELS AND THE TIDYVERSE -------------------------------
r <- iris %>% 
  group_nest(Species) %>% 
  mutate(model = map(data, ~ lm(Sepal.Length ~ Sepal.Width, data = .x)))


library(broom)

r$model[1][[1]]
tidy(r$model[1][[1]])


r %>% 
  mutate(coef = map(model, tidy)) %>% 
  unnest(cols = c(coef))



# Metapackage ------------------------------------------------------------
library(tidymodels)
tidymodels_prefer(quiet = FALSE)

iris %>% filter(Species == 'virginica')





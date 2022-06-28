# membandingkan antar model bisa melalui nilai likelihood
# dari k-fold cross validation

library(tidymodels)
library(tidyverse)
tidymodels_prefer()

data(ames, package = "modeldata")
ames <- ames %>% mutate(Sale_Price = log(Sale_Price))

# Data Splitting ----------------------------------------------------------
set.seed(1)

ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_split

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)


neural_net_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('classification')


ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train)  %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = tune()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Longitude, deg_free = tune("longitude df")) %>% 
  step_ns(Latitude,  deg_free = tune("latitude df"))

recipes_param <- extract_parameter_set_dials(ames_rec)
recipes_param


wflow_param <- 
  workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(neural_net_spec) %>% 
  extract_parameter_set_dials()
wflow_param

hidden_units()
threshold()
spline_degree()

wflow_param %>% extract_parameter_dials("threshold")

extract_parameter_set_dials(ames_rec) %>% 
  update(threshold = threshold(c(0.8, 1.0)))



# correct method to have penalty values between 0.1 and 1.0
penalty(c(-1, 0)) %>% value_sample(1000) %>% summary()
penalty(trans = NULL, range = 10^c(-10, 0))

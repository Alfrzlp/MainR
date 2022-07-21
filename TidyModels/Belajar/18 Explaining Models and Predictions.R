library(tidymodels)
tidymodels_prefer()
options(tidymodels.dark = TRUE)

# -------------------------------------------------------------------------
data(ames, package = "modeldata")
ames <- ames %>% mutate(Sale_Price = log(Sale_Price))



# Data Splitting ----------------------------------------------------------
set.seed(1)

ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_split

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)
dim(ames_test)

set.seed(1)
ames_folds <- vfold_cv(ames_train, v = 10, repeats = 5)
ames_folds

# Model -------------------------------------------------------------------
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

# recipe ------------------------------------------------------------------
lm_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 50)

lm_wflow <- 
  workflow() %>% 
  add_recipe(lm_rec) %>% 
  add_model(lm_model)

rf_wflow <- 
  workflow() %>% 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) %>% 
  add_model(rf_model) 



# training ----------------------------------------------------------------
rf_res <- 
  rf_wflow %>% 
  fit_resamples(
    resamples = ames_folds,
    control = control_resamples(save_pred = T, verbose = T, save_workflow = T)
  )

lm_res <- 
  lm_wflow %>% 
  fit_resamples(
    resamples = ames_folds,
    control = control_resamples(save_pred = T, verbose = T, save_workflow = T)
  )

lm_res %>% 
  collect_metrics()

rf_res %>% 
  collect_metrics()


# best model --------------------------------------------------------------
rf_best <- rf_res %>% 
  select_best()

lm_best <- lm_res %>% 
  select_best()


rf_fit <- rf_wflow %>% 
  finalize_workflow(rf_best) %>%
  fit(ames_train) 

lm_fit <- lm_wflow %>% 
  finalize_workflow(lm_best) %>%
  fit(ames_train) 


# -------------------------------------------------------------------------


library(DALEXtra)
vip_features <- c("Neighborhood", "Gr_Liv_Area", "Year_Built", 
                  "Bldg_Type", "Latitude", "Longitude")

vip_train <- 
  ames_train %>% 
  select(all_of(vip_features))

explainer_lm <- 
  explain_tidymodels(
    lm_fit, 
    data = vip_train, 
    y = ames_train$Sale_Price,
    label = "lm + interactions",
    verbose = FALSE
  )

explainer_rf <- 
  explain_tidymodels(
    rf_fit, 
    data = vip_train, 
    y = ames_train$Sale_Price,
    label = "random forest",
    verbose = FALSE
  )


# local explanation -------------------------------------------------------
duplex <- vip_train[120,]
duplex

lm_breakdown <- predict_parts(explainer = explainer_lm, new_observation = duplex)
lm_breakdown

rf_breakdown <- predict_parts(explainer = explainer_rf, new_observation = duplex)
rf_breakdown

predict_parts(
  explainer = explainer_rf, 
  new_observation = duplex,
  order = lm_breakdown$variable_name
)

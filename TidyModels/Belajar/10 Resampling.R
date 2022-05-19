library(tidyverse)
library(tidymodels)

tidymodels_prefer()


# Data --------------------------------------------------------------------
data(ames, package = "modeldata")
ames <- ames %>% mutate(Sale_Price = log(Sale_Price))

# analysis() -> train
# assessment() -> test

# Data Splitting ----------------------------------------------------------
set.seed(1)

ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_split

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)
dim(ames_test)


# Linear Model ------------------------------------------------------------
ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01, id = "my_id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20) %>% 
  step_pca(matches("(SF$)|(Gr_Liv)"))

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)




# Tree based model --------------------------------------------------------
rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow <- 
  workflow() %>% 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) %>% 
  add_model(rf_model) 

rf_fit <- rf_wflow %>% fit(data = ames_train)





# Perbandingan ------------------------------------------------------------
estimate_perf <- function(model, dat) {
  # Capture the names of the `model` and `dat` objects
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("ames_", "", data_name)
  
  # Estimate these metrics:
  reg_metrics <- metric_set(rmse, rsq)
  
  model %>%
    predict(dat) %>%
    bind_cols(dat %>% select(Sale_Price)) %>%
    reg_metrics(Sale_Price, .pred) %>%
    select(-.estimator) %>%
    mutate(object = obj_name, data = data_name)
}


estimate_perf(rf_fit, ames_train)
estimate_perf(lm_fit, ames_train)


estimate_perf(rf_fit, ames_test)
estimate_perf(lm_fit, ames_test)




# CV ----------------------------------------------------------------------
set.seed(1001)
ames_folds <- vfold_cv(ames_train, v = 10, repeats = 5)
ames_folds


# leave-one-out (LOO) cross-validation
loo_cv()

# Monte Carlo cross-validation
mc_cv(ames_train, prop = 9/10, times = 20)



# Validation set ----------------------------------------------------------
set.seed(1002)
val_set <- validation_split(ames_train, prop = 3/4)
val_set



# Bootstrap resampling ----------------------------------------------------
plot(magick::image_read('https://www.tmwr.org/premade/bootstraps.svg', ))

bootstraps(ames_train, times = 5)




# Time series data resampling ---------------------------------------------
time_slices <- 
  tibble(x = 1:365) %>% 
  rolling_origin(initial = 6 * 30, assess = 30, skip = 29, cumulative = FALSE)


time_slices

data_range <- function(x) {
  summarize(x, first = min(x), last = max(x))
}

map_dfr(time_slices$splits, ~   analysis(.x) %>% data_range())
map_dfr(time_slices$splits, ~ assessment(.x) %>% data_range())


# Estimating Performance --------------------------------------------------
model_spec %>% fit_resamples(formula,  resamples, ...)
model_spec %>% fit_resamples(recipe,   resamples, ...)
workflow   %>% fit_resamples(          resamples, ...)




keep_pred <- control_resamples(
  save_pred = TRUE, save_workflow = TRUE, verbose = T
)

set.seed(1003)
rf_res <- 
  rf_wflow %>% 
  fit_resamples(
    resamples = ames_folds,
    control = keep_pred
  )

rf_res
collect_metrics(rf_res, summarize = FALSE)
# hasil rata2
collect_metrics(rf_res)


# To obtain the assessment set predictions
assess_res <- collect_predictions(rf_res)
assess_res


assess_res %>% 
  ggplot(aes(x = Sale_Price, y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(color = "red") + 
  coord_obs_pred() + 
  ylab("Predicted")


# ada 2 yang tidak normal
over_predicted <- 
  assess_res %>% 
  mutate(residual = Sale_Price - .pred) %>% 
  arrange(desc(abs(residual))) %>% 
  slice(1:2)
over_predicted


ames_train %>% 
  slice(over_predicted$.row) %>% 
  select(Gr_Liv_Area, Neighborhood, Year_Built, Bedroom_AbvGr, Full_Bath)




val_res <- rf_wflow %>% fit_resamples(resamples = val_set)
val_res
collect_metrics(val_res)




# Pararel -----------------------------------------------------------------

# The number of physical cores in the hardware:
parallel::detectCores(logical = FALSE)


# The number of possible independent processes that can 
# be simultaneously used:  
parallel::detectCores(logical = TRUE)






# Saving Resanpling -------------------------------------------------------
ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <-  
  workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(linear_reg() %>% set_engine("lm")) 

lm_fit <- lm_wflow %>% fit(data = ames_train)

# Select the recipe: 
extract_recipe(lm_fit, estimated = TRUE)





get_model <- function(x) {
  extract_fit_parsnip(x) %>% tidy()s
}


ctrl <- control_resamples(extract = get_model)
ctrl

lm_res <- lm_wflow %>%  
  fit_resamples(resamples = ames_folds, control = ctrl)
lm_res

lm_res$.extracts[[1]][[1]]



all_coef <- map_dfr(lm_res$.extracts, ~ .x[[1]][[1]])
# Show the replicates for a single predictor:
filter(all_coef, term == "Year_Built")

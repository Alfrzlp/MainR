library(tidymodels)
tidymodels_prefer()

# Data --------------------------------------------------------------------
# library(modeldata) 
# data(ames) setara seperti dibawah
data(ames, package = "modeldata")

head(ames)
glimpse(ames)


# EDA ---------------------------------------------------------------------
ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col = "white")

# transformasi logaritma
# - tidak ada yg negatif
# - menstabilkan varians
# kekurangan : susah interpretasi
ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col = "white") +
  scale_x_log10()

# grafik log
ggplot(NULL, aes(x = 0:10000, y = log(0:10000))) +
  geom_line()


ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

library(ggspatial)
ames %>% 
  annotation_map_tile(zoom = 7, cahcedir = 'cahcedir_base') +
  ggplot(aes(x = Longitude, y = Latitude, color = Neighborhood)) +
  geom_point() +
  theme(
    legend.position = 'none'
  )


# Data Splitting ----------------------------------------------------------
set.seed(1)

ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_split

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)
dim(ames_test)


# Fitting Model -----------------------------------------------------------
linear_reg() %>% set_engine("lm") %>% translate()
linear_reg() %>% set_engine("stan") %>% translate()
linear_reg(penalty = 1) %>% set_engine("glmnet") %>% translate()


lm_model <- 
  linear_reg() %>% 
  set_engine('stan')

# Dengan Form
lm_fit <- 
  lm_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

# Dengan X Y
lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )

lm_fit %>% 
  predict(ames_test) %>% 
  bind_cols(ames_test %>% select(Sale_Price)) %>% 
  yardstick::rsq_trad(truth = Sale_Price, .pred)






# Ekstrak Hasil modeling seperti hasil package asal ----------------------
fit_manual <- lm(Sale_Price ~ Longitude + Latitude, data = ames_train)

fit_manual %>% vcov()
lm_fit %>% 
  extract_fit_engine() %>% 
  vcov()

# data hasil berbentuk numeric matrix harus convert ke df
# nama kolom yang sulit misal Pr(>|t|) kadang juga Pr(>|z|) 
# dan permasalahan lainnya maka solusinya tidy()
# convert many types of model objects to a tidy structure

tidy(lm_fit)


# Predict -----------------------------------------------------------------
ames_test_small <- ames_test %>% slice(1:5)
predict(lm_fit, new_data = ames_test_small)


ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_fit, ames_test_small)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_fit, ames_test_small, type = "pred_int")) 







# Model tree based --------------------------------------------------------
rf_model <- 
  rand_forest() %>% 
  set_engine('ranger') %>% 
  set_mode('regression')


rf_fit <- 
  rf_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(rf_fit, ames_test_small)) %>% 
  rsq(truth = Sale_Price, .pred)



# addins ------------------------------------------------------------------
# untuk memilih model
parsnip_addin()


# Model Workflow ----------------------------------------------------------

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow


lm_fit <- 
  fit(lm_wflow, ames_train)

lm_fit


ames_test %>% 
  slice(1:5) %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_fit, ames_test %>% slice(1:5)))




# update formula
lm_fit <- lm_fit %>% 
  update_formula(Sale_Price ~ Longitude)
lm_fit

lm_fit <- 
  fit(lm_wflow, ames_train)

ames_test %>% 
  slice(1:5) %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_fit, ames_test %>% slice(1:5)))



# tambah raw variabel
lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

lm_wflow

# bisa juga
# c(ends_with("tude"))
# everything()

fit(lm_wflow, ames_train)






# -------------------------------------------------------------------------
library(multilevelmod)

data(Orthodont, package = 'nlme')

multilevel_spec <- 
  linear_reg() %>% 
  set_engine("lmer")

multilevel_workflow <- 
  workflow() %>% 
  # Pass the data along as-is: 
  add_variables(outcome = distance, predictors = c(Sex, age, Subject)) %>% 
  add_model(multilevel_spec, 
            # This formula is given to the model
            formula = distance ~ Sex + (age | Subject))

multilevel_fit <- fit(multilevel_workflow, data = Orthodont)
multilevel_fit



# -------------------------------------------------------------------------
devtools::install_github("tidymodels/censored")
library(censored)


leuk <- read.csv('D:/__SEMESTER 6/AKH/leuk.csv', stringsAsFactors = T)
head(leuk)

parametric_spec <- 
  survival_reg(mode = "censored regression", engine = "survival") 

parametric_workflow <- 
  workflow() %>% 
  add_variables(outcome = c(waktu, status), predictors = c(GWBC, logWBC, sex)) %>% 
  add_model(parametric_spec, 
            formula = Surv(waktu, status) ~ GWBC + logWBC + strata(sex))

parametric_fit <- fit(parametric_workflow, data = leuk)
parametric_fit



# create multiple workflow ------------------------------------------------
location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)

library(workflowsets)
location_models <- workflow_set(preproc = location, models = list(lm = lm_model))
location_models


location_models$info[[1]]

extract_workflow(location_models, id = "coords_lm")

location_models <-
  location_models %>%
  mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models
location_models$fit[[1]]

location_models %>% 
  mutate(coef = map(fit, tidy)) %>% 
  unnest(coef)



# last fit ----------------------------------------------------------------
# fitting a final model to the training set and evaluating with the test set.
final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res

fitted_lm_wflow <- extract_workflow(final_lm_res)
fitted_lm_wflow

collect_metrics(final_lm_res)
collect_predictions(final_lm_res) %>% slice(1:5)


# Bantuan -----------------------------------------------------------------
# https://www.tidymodels.org/find/
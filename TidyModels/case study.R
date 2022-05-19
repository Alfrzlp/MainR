library(tidymodels)  
library(readr)    
library(vip)       

tidymodels_prefer()

# Data --------------------------------------------------------------------
hotels <- 
  read_csv('D:/_Datasets/hotels.csv') %>%
  mutate_if(is.character, as.factor) 

dim(hotels)
glimpse(hotels)

hotels %>% 
  count(children) %>% 
  mutate(prop = n/sum(n))


# Train Test Spliting -----------------------------------------------------
set.seed(123)
splits <- initial_split(hotels, strata = children)

hotel_other <- training(splits)
hotel_test  <- testing(splits)

hotel_other %>% 
  count(children) %>% 
  mutate(p = n/sum(n))

hotel_test %>% 
  count(children) %>% 
  mutate(p = n/sum(n))

set.seed(234)
val_set <- validation_split(hotel_other, 
                            strata = children, 
                            prop = 0.80)
val_set



# Penalized Logistic Regression -------------------------------------------
# Setting mixture to a value of one means that the glmnet model
# will potentially remove irrelevant predictors and choose 
# a simpler model.
lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")


# recipe ------------------------------------------------------------------
holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

lr_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date, holidays = holidays) %>% 
  step_rm(arrival_date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


# Workfloe ----------------------------------------------------------------
lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)


lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_reg_grid %>% top_n(-5)
lr_reg_grid %>% top_n(5)



# Train and Tune ----------------------------------------------------------
lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

lr_res %>% 
  collect_metrics()
lr_res %>% 
  select_best()


# nilai penalty yang tinggi akan menghapus
# variabel prediktor dari model
lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 

# top models
lr_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) %>% 
  top_n(5)
# pilih nilai penalty tinggi tapi skor juga tinggi




# Best model --------------------------------------------------------------
lr_best <- 
  lr_res %>% 
  select_best()
lr_best


lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Logistic Regression") %>% 
  autoplot()



# TREE-BASED ENSEMBLE -----------------------------------------------------

cores <- parallel::detectCores()
cores

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")

# The mtry hyperparameter sets the number of predictor variables
# that each node in the decision tree "sees" and can learn about,
# so it can range from 1 to the total number of features present;
# when mtry = all possible features, 
# the model is the same as bagging decision trees
# 
# The min_n hyperparameter sets the minimum n to
# split at any node.

# recipe ------------------------------------------------------------------
rf_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date) %>% 
  step_rm(arrival_date) 


# Workflow ----------------------------------------------------------------
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)


# Training RF -------------------------------------------------------------
rf_mod
extract_parameter_set_dials(rf_mod)

set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))



# Result ------------------------------------------------------------------
rf_res %>% 
  show_best(metric = "roc_auc")

autoplot(rf_res)

rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")
rf_best

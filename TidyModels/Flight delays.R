library(tidyverse)
library(tidymodels)
library(themis)
library(bonsai)
library(inspectdf)
library(janitor)
tidymodels_prefer()

# Data --------------------------------------------------------------------
loc <- 'D:/__Datasets/ml/flight-delays-fall-2018/'
list.files(loc)


df_train <- read.csv(file.path(loc, 'flight_delays_train.csv')) %>% 
  clean_names() %>% 
  mutate(
    across(c(day_of_week, dayof_month, month), ~ stringr::str_remove_all(.x, 'c-')),
    dep_delayed_15min = ifelse(dep_delayed_15min == 'N', 0, 1),
    dep_delayed_15min = factor(dep_delayed_15min)
  ) %>% 
  type_convert() %>% 
  rename(delay = dep_delayed_15min)

df_test <- read.csv(file.path(loc, 'flight_delays_test.csv')) %>% 
  clean_names() %>% 
  mutate(
    across(c(day_of_week, dayof_month, month), ~ stringr::str_remove_all(.x, 'c-'))
  ) %>% 
  type_convert() 

df_sub <- read.csv(file.path(loc, 'sample_submission.csv'))

glimpse(df_train)
glimpse(df_test)
glimpse(df_sub)


# EDA ---------------------------------------------------------------------
var_cat <- inspect_cat(df_train)
var_num <- inspect_num(df_train)

var_cat
var_num

# Na Value
df_train %>% is.na() %>% colSums()

# unique
var_cat %>% 
  filter(col_name == 'dest') %>% 
  pull(levels)
var_cat %>% 
  filter(col_name == 'origin') %>% 
  pull(levels)
var_cat %>% 
  filter(col_name == 'unique_carrier') %>% 
  pull(levels)



# FE ----------------------------------------------------------------------
df_train %>% 
  mutate(
    rute = paste0(origin, '_', dest),
    .keep = 'unused'
  ) %>% 
  summarise(length(unique(rute)))
  head()
  
  
df_test %>% 
  mutate(
    rute = paste0(origin, '_', dest),
    .keep = 'unused'
  ) %>% 
  summarise(length(unique(rute)))




# Train test split --------------------------------------------------------
set.seed(1)

delay_split <- initial_split(data = df_train, prop = 0.8, strata = delay)
train_set <- training(delay_split)
test_set <- testing(delay_split)

set.seed(1)
train_fold <- vfold_cv(train_set, v = 5)


# Model -------------------------------------------------------------------
delay_metric <- metric_set(roc_auc)

rf_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine('ranger') %>%
  set_mode('classification')

xgb_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')

logreg_spec <-
  logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine('glmnet')

# Recipe and Workflow -----------------------------------------------------
glimpse(df_train)

my_recipe <-
  recipe(delay ~ ., data = train_set) %>%
  step_interact(~ day_of_week:dayof_month + dayof_month:month) %>%
  step_dummy(unique_carrier, one_hot = TRUE) %>% 
  step_rm(c(origin, dest)) %>% 
  step_downsample(delay)

  
my_recipe %>% 
  summary() %>% 
  as.data.frame()

my_workflow <- 
  workflow() %>% 
  add_model(xgb_spec) %>% 
  add_recipe(my_recipe)


# Tuning ------------------------------------------------------------------
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

# grid search
ctrl_grid <- control_grid(save_pred = TRUE, verbose = T, allow_par = T)
set.seed(1)

model_res <- 
  my_workflow %>% 
  tune_grid(
    resamples = train_fold,
    grid = 1,
    control = ctrl_grid,
    metrics = delay_metric
  )

model_res$.notes[[1]]$note[1]


# Hasil -------------------------------------------------------------------
model_res %>% 
  collect_metrics() %>% 
  filter(.metric == 'roc_auc') %>% 
  arrange(desc(mean))  
  relocate(mean, .before = trees)


model_res %>% 
  collect_metrics() %>% 
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  arrange(desc(kap)) 
  relocate(c(accuracy, kap, f_meas), .before = trees) 



# Model terbaik -----------------------------------------------------------
best_model <- model_res %>% 
  select_best(metric = 'roc_auc')

# best_model <- model_res %>% 
#   collect_metrics() %>% 
#   slice(150)
# 
# best_model <- model_bo %>% 
#   select_best()

final_wf <- my_workflow %>% 
  finalize_workflow(best_model)

final_wf

# fit terakhir
final_fit <- final_wf %>% fit(df_train) 
final_fit



# Evaluasi ----------------------------------------------------------------
# train
df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  delay_metric(truth = buy, estimate = .pred_class)

df_train %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  conf_mat(truth = buy, estimate = .pred_class)


# test
test_set %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  delay_metric(truth = buy, estimate = .pred_class)

test_set %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  conf_mat(truth = buy, estimate = .pred_class)


# Prediksi ----------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds

table(preds)
sum(is.na(preds))


# Submission --------------------------------------------------------------
hasil <- df_sub %>% 
  mutate(dep_delayed_15min = preds)

write.csv(hasil, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  
system('kaggle competitions submit -c flight-delays-fall-2018 -f D:/__Datasets/sub.csv -m "Message"')

  


m1 <- glm(delay ~ (month + dayof_month + day_of_week + dep_time + distance)^2 , data = 
            df_train %>% 
            mutate(
              delay = as.numeric(delay) - 1
            ) %>% 
            mutate_at(
              - delay
            )
      )
m1 <- stats::step(m1, trace = F)

preds <- ifelse(predict(m1, newdata = df_test) > 0.17, 1, 0)
table(preds)
table(df_train$delay)

setdiff(
  unique(df_train$unique_carrier),
  unique(df_test$unique_carrier)
)
setdiff(
  unique(df_test$unique_carrier),
  unique(df_train$unique_carrier)
)

lvl_unique_carrier <- union(
  unique(df_train$unique_carrier),
  unique(df_test$unique_carrier)
)

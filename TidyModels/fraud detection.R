library(tidyverse)
library(tidymodels)
library(janitor)
library(lubridate)

tidymodels_prefer()


# Data' -------------------------------------------------------------------
df_train <- 
  read_csv('D:/_Datasets/ddb-inhouse-day-challenge/train.csv') %>% 
  clean_names()

df_test <- 
  read_csv('D:/_Datasets/ddb-inhouse-day-challenge/test.csv') %>% 
  clean_names()


glimpse(df_train)
glimpse(df_test)


# Data information ---------------------------------------------------------------------

# class
df_train %>% 
  group_by(is_fraud) %>% 
  count()


# n unique
n_unique <- 
  df_train %>% 
  summarise_if(is.character, ~ length(unique(.x))) %>% 
  pivot_longer(everything(), values_to = 'n_unique', names_to = 'variabel')

n_unique


# Get all 
x <- c('issuercountrycode', 'txvariantcode', 'shoppercountrycode', 'currencycode')

df_train[x] %>% apply(2, FUN = function(x) unique(x))
df_test[x] %>% apply(2, FUN = function(x) unique(x))





# step yang akan dilakukan
# drop currencycode card_id
# role date creationdate
OlsonNames()


# Final datasets ----------------------------------------------------------
max_amount <- max(df_train[df_train$is_fraud == 0, ]$amount)
max_amount


df_train <- df_train %>% 
  mutate(
    is_fraud = factor(is_fraud, levels = 0:1),
    creationdate = dmy_hm(creationdate),
    daym = days_in_month(creationdate),
    dayw = weekdays(creationdate),
    time = as.numeric(format(creationdate, '%H')),
    year = as.numeric(format(creationdate, '%Y')),
    amount_besar = ifelse(amount > max_amount, 1, 0),
    cardverificationcodesupplied = as.numeric(cardverificationcodesupplied)
  ) 



df_test <- df_test %>% 
  mutate(
    creationdate = dmy_hm(creationdate),
    daym = days_in_month(creationdate),
    dayw = weekdays(creationdate),
    time = as.numeric(format(creationdate, '%H')),
    amount_besar = ifelse(amount > max_amount, 1, 0),
    year = as.numeric(format(creationdate, '%Y')),
    cardverificationcodesupplied = as.numeric(cardverificationcodesupplied)
  ) 


df_train %>% 
  glimpse()



# EDA ---------------------------------------------------------------------
ggplot(df_train, aes(x = bin)) +
  geom_boxplot() +
  geom_vline(
    xintercept = max(df_train[df_train$is_fraud == 0, ]$amount),
    color = 'red'
  ) +
  facet_grid(is_fraud~., scales = 'free') +
  scale_y_continuous(breaks = NULL)


ggplot(df_train, aes(x = amount)) +
  geom_bar() +
  facet_grid(is_fraud~., scales = 'free')
  scale_y_continuous(breaks = NULL)

ggplot(df_train, aes(y = is_fraud, fill = txvariantcode)) +
  geom_bar() +
  facet_grid(is_fraud~., scales = 'free') 



  

# Missing Value -----------------------------------------------------------
df_train %>% 
    summarise_all(.funs = ~ sum(is.na(.x))) %>% 
    pivot_longer(everything()) %>% 
    mutate(p = value/nrow(df_train)) %>% 
    filter(value > 0) 

df_test %>% 
    summarise_all(.funs = ~ sum(is.na(.x))) %>% 
    pivot_longer(everything()) %>% 
    mutate(p = value/nrow(df_train)) %>% 
    filter(value > 0) 
  





# Train Test Split --------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, strata = is_fraud)
splits

train_set <- training(splits)
test_set  <- testing(splits)


set.seed(1)
val_set <- validation_split(
  train_set, 
  strata = is_fraud, 
  prop = 0.8
)
val_set


# Simple Model ------------------------------------------------------------
cores <- parallel::detectCores()
cores

rf_model <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine('glmnet') 

rf_model <- 
  rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>% 
  set_engine('ranger', num.threads = cores) %>% 
  set_mode('classification')

rf_model <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')



# recipe
rf_recipe <- 
  recipe(
    is_fraud ~ issuercountrycode + amount + amount_besar + cvcresponsecode + cardverificationcodesupplied + shoppercountrycode + dayw + year,
    data = train_set) %>% 
  step_string2factor(all_nominal_predictors()) %>% 
  step_impute_knn(cardverificationcodesupplied) %>% 
  themis::step_upsample(is_fraud, over_ratio = 0.9) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(c(amount))


# workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rf_recipe)


lr_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 90))
lr_grid

# Training ----------------------------------------------------------------
rf_res <- 
  rf_workflow %>% 
  tune_grid(
    val_set,
    grid = lr_grid,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc)
  )


see_notes <- function(res){
  res$.notes[[1]]$note
}


rf_res %>% 
  collect_metrics()

rf_res %>% 
  select_best()



# Analisis hasil ----------------------------------------------------------
rf_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())





# Best Model --------------------------------------------------------------
rf_best <- 
  rf_res %>% 
  select_best()

rf_best




# Final -------------------------------------------------------------------
final_wf <- rf_workflow %>% 
  finalize_workflow(rf_best)

final_wf


final_fit <- 
  final_wf %>%
  fit(df_train) 

final_fit



# Hasil -------------------------------------------------------------------
fraud_metric <- metric_set(f_meas, sensitivity, mcc)

df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  fraud_metric(truth = is_fraud, estimate = .pred_class)

df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  conf_mat(truth = is_fraud, estimate = .pred_class)




# Predict -----------------------------------------------------------------
hasil <- 
  df_test %>% 
  select(txid) %>% 
  mutate(
    is_fraud = predict(final_fit, new_data = df_test) %>% pull()
  )

head(hasil)

hasil %>% 
  write.csv('E:/sub.csv', row.names = F, quote = F)  


# kirim submission --------------------------------------------------------
system(
  'kaggle competitions submit -c ddb-inhouse-day-challenge -f E:/sub.csv -m "Message"'
)

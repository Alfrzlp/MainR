library(tidyverse)
library(tidymodels)
library(lubridate)
tidymodels_prefer()

loc <- 'D:/Downloads/tabular-playground-series-sep-2022'

# Data --------------------------------------------------------------------
df_train <- read.csv(file.path(loc, "train.csv"))
df_test <- read.csv(file.path(loc, "test.csv"))

glimpse(df_train)
glimpse(df_test)


# -------------------------------------------------------------------------
df_train <- df_train %>% 
  mutate(
    date = lubridate::ymd(date),
    day = day(date),
    week = week(date),
    m = as.factor(month(date)),
    w = weekdays(date),
    y = year(date)
  )

df_test <- df_test %>% 
  mutate(
    date = lubridate::ymd(date),
    day = day(date),
    week = week(date),
    m = as.factor(month(date)),
    w = weekdays(date),
    y = year(date)
  )

glimpse(df_train)
glimpse(df_test)



# viz ---------------------------------------------------------------------
ggplot(df_train, aes(x = date, y = num_sold)) +
  geom_line()



# Metric ------------------------------------------------------------------
my_metric <- metric_set(smape)

# Train test split ---------------------------------------------------------
set.seed(1)
splits <- initial_time_split(df_train, prop = 0.8)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(
  train_set, v = 5, strata = 'country'
)

val_set <- validation_time_split(
  train_set,  
  prop = 0.7
)
val_set



# Model -------------------------------------------------------------------
rf_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine('ranger', num.threads = 4) %>%
  set_mode('regression')

model_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

model_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('regression')




# Workflow ----------------------------------------------------------------
glimpse(df_train)

my_recipe <- 
  recipe(num_sold ~ ., data = train_set) %>% 
  update_role(date, new_role = "date") 

my_recipe %>% summary()

my_workflow <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(my_recipe)


# Training ----------------------------------------------------------------
model_res <- 
  my_workflow %>% 
  tune_grid(
    train_fold,
    grid = 25,
    control = control_grid(save_pred = TRUE, verbose = T, allow_par = T),
    metrics = my_metric
  )


model_res %>% 
  collect_metrics() %>% 
  # filter(.metric == 'rmse') %>% 
  arrange(mean) 
  select(mean)


model_res %>% 
  collect_metrics() %>% 
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  arrange(rmse, desc(rsq))



model_res %>% 
  select_best()


# Model terbaik -----------------------------------------------------------
best_model <- model_res %>% 
  select_best(metric = 'smape')

final_wf <- my_workflow %>% 
  finalize_workflow(best_model)

final_wf

# fit terakhir
final_fit <- final_wf %>% fit(df_train) 
final_fit


# Evaluasi ----------------------------------------------------------------
df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  reg_metric(truth = score, estimate = .pred)

test_set %>% 
  bind_cols(predict(final_fit, .)) %>% 
  reg_metric(truth = score, estimate = .pred)


# Prediksi ----------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds

hasil <- df_test %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  select(row_id, num_sold = .pred)


# Submission --------------------------------------------------------------
write.csv(hasil, 'D:/__Datasets/sub.csv', row.names = FALSE, quote = FALSE)  

system(
  'kaggle competitions submit -c tabular-playground-series-sep-2022 -f D:/__Datasets/sub.csv -m "Message"'
)



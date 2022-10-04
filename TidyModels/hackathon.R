library(tidyverse)
library(tidymodels)
library(themis)
tidymodels_prefer()

# Data --------------------------------------------------------------------
loc <- 'D:/Downloads/data-mining'
list.files(path = loc)

df_train <- read.csv(file.path(loc, "training.csv"))
df_test <- read.csv(file.path(loc, "testing.csv"))

glimpse(df_train)
glimpse(df_test)


# Data Prepocessing -------------------------------------------------------
table(df_train$Y)

df_train <- df_train %>% 
  mutate(Y = as.factor(Y)) %>% 
  mutate_at(vars(X1:X4, X7, X9:X16), ~ as.factor(.x)) 

df_test <- df_test %>% 
  mutate_at(vars(X1:X4, X7, X9:X16), ~ as.factor(.x)) 

df_train %>% 
  glimpse()


# EDA ---------------------------------------------------------------------
colSums(is.na(df_train))
colSums(is.na(df_test))

df_train %>% 
  select(Y, X8, X5, X6) %>% 
  pivot_longer(-Y) %>% 
  ggplot(aes(y = Y, x = value, fill = Y)) +
  geom_violin() +
  facet_grid(name~., scales = 'free')

df_train %>% 
  select(Y, X1:X4, X7, X9:X16) %>% 
  pivot_longer(-Y) %>% 
  ggplot(aes(x = value, fill = Y)) +
  geom_bar( position = position_fill()) +
  facet_wrap(facets = ~name, scales = 'free', nrow = 4, ncol = 4)


# Metric ------------------------------------------------------------------
my_metric <- metric_set(accuracy)


# train test split --------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, prop = 0.8, strata = Y)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(df_train, v = 3, strata = Y)


# model -------------------------------------------------------------------

mlp_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('classification')

rf_spec <-
  rand_forest(mtry = tune(), min_n = 2, trees = tune()) %>%
  set_engine('ranger') %>%
  set_mode('classification')

knn_spec <-
  nearest_neighbor(neighbors = tune(), dist_power = 2) %>%
  set_engine('kknn') %>%
  set_mode('classification')


# recipe and workflow -----------------------------------------------------
my_recipe <- 
  recipe(Y ~ ., data = train_set) %>% 
  # update_role(ID, new_role = "id") %>% 
  # step_dummy(all_nominal_predictors()) %>%
  step_zv() 


my_workflow <- 
  workflow_set(
    preproc = list(my_recipe), 
    models = list(
      # knn = knn_spec,
      # nnet = mlp_spec,
      rf = rf_spec
    )
  )


# tuning ------------------------------------------------------------------
grid_ctrl <-
  control_grid(
    # parallel_over = "resampling",
    save_pred = TRUE,
    verbose = T,
    allow_par = T
  )


library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

grid_results <-
  my_workflow %>%
  workflow_map(
    seed = 1,
    resamples = train_fold,
    grid = 5,
    control = grid_ctrl,
    metrics = my_metric,
    verbose = TRUE
  )


# Hasil -------------------------------------------------------------------
metric_name <- 'accuracy'

grid_results %>% 
  rank_results() %>% 
  # filter(.metric == metric_name) %>% 
  select(model, .config, accuracy = mean, rank)


autoplot(
  grid_results,
  # how to order models
  rank_metric = metric_name,  
  # which metric to visualize
  metric = metric_name,       
  # one point per workflow
  select_best = TRUE     
) 


autoplot(
  grid_results,
  id = "recipe_nnet",
  metric = metric_name
)



# finalizing model --------------------------------------------------------
best_model <- 'recipe_knn'

best_results <- 
  grid_results %>% 
  extract_workflow_set_result(best_model) %>% 
  select_best(metric = metric_name)

best_results

final_wf <- 
  grid_results %>% 
  extract_workflow(best_model) %>% 
  finalize_workflow(best_results)

final_wf

final_fit <- final_wf %>% fit(df_train) 
final_fit


# Evaluasi ----------------------------------------------------------------
# train
df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  my_metric(truth = Y, estimate = .pred_class)

df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  conf_mat(truth = Y, estimate = .pred_class)

# test
test_set %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  my_metric(truth = Y, estimate = .pred_class)

test_set %>% 
  bind_cols(predict(final_fit, .)) %>% 
  conf_mat(truth = Y, estimate = .pred_class)


# Prediction --------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds <- predict(rf_model, data = df_test[-1]) 
preds

table(preds)
table(preds$predictions)


# Submission --------------------------------------------------------------
hasil <- df_test %>% 
  select(ID) %>% 
  mutate(Y = rf_pred$predictions)

write.csv(hasil, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  

system(
  'kaggle competitions submit -c data-mining-ta20222023 -f D:/__Datasets/sub.csv -m "Message"'
)



glimpse(df_train)
df_train <- df_train %>% 
  mutate(
    x1 = X5 - X6,
    x2 = X8 - X6
    # x12 = x2*x1
  )

df_test <- df_test %>% 
  mutate(
    x1 = X5 - X6,
    x2 = X8 - X6
    # x12 = x2*x1
  )

df_train <- df_train %>% 
  select(-x12)
df_test <- df_test %>% 
  select(-x12)

colSums(is.na(df_test))
colSums(is.na(df_train))

library(tidyverse)
library(tidymodels)
library(janitor)
library(themis)
tidymodels_prefer()

# Data --------------------------------------------------------------------
loc <- 'D:/__Datasets/ml/2022-classification-data-challenge'
list.files(loc)


df_train <- read.csv(file.path(loc, 'train.csv')) %>% 
  clean_names() %>% 
  mutate(buy = as.factor(buy))
  # mutate_at(
  #   vars(-c(v55, v64, v84)),
  #   ~ as.factor(.x)
  # )

df_test <- read.csv(file.path(loc, 'test.csv')) %>% 
  clean_names() 
  # mutate_at(
  #   vars(-c(v55, v64, v84)),
  #   ~ as.factor(.x)
  # )

df_sub <- read.csv(file.path(loc, 'submission.csv'))

glimpse(df_train)
glimpse(df_test)
glimpse(df_sub)



# all model ---------------------------------------------------------------
logreg_spec <-
  logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine('glmnet')

rf_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine('ranger') %>%
  set_mode('classification')

xgb_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')

mlp_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('classification')

svm_rbf_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
  set_engine('kernlab') %>%
  set_mode('classification')

svm_linear_spec <-
  svm_linear(cost = tune(), margin = tune()) %>%
  set_engine('kernlab') %>%
  set_mode('classification')

svm_poly_spec <-
  svm_poly(cost = tune(), degree = tune(), scale_factor = tune(), margin = tune()) %>%
  set_engine('kernlab') %>%
  set_mode('classification')



# recipe and workflow -----------------------------------------------------
my_recipe <- 
  recipe(buy ~ ., data = train_set) %>% 
  step_rm(id) %>%
  step_pca(num_range("v", 2:34), num_comp = 34, prefix = 'x1_') %>% 
  step_pca(num_range("v", 35:86), num_comp = 36, prefix = 'x2_') %>% 
  step_zv() %>% 
  step_downsample(buy) 


my_workflow <- 
  workflow_set(
    preproc = list(my_recipe), 
    models = list(
      logreg = logreg_spec,
      rf = rf_spec,
      xgb = xgb_spec, 
      nnet = mlp_spec,
      svm_rbf = svm_rbf_spec,
      svm_linear = svm_linear_spec,
      svm_poly = svm_poly_spec
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
    "tune_race_anova",
    seed = 1,
    resamples = train_fold,
    grid = 25,
    control = grid_ctrl,
    metrics = metric_set(kap),
    verbose = TRUE
  )


# Hasil -------------------------------------------------------------------
grid_results %>% 
  rank_results() %>% 
  filter(.metric == "kap") %>% 
  select(model, .config, kap = mean, rank)



autoplot(
  grid_results,
  rank_metric = "kap",  # <- how to order models
  metric = "kap",       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
) +
  geom_text(
    aes(y = min(mean), label = str_remove_all(wflow_id, 'recipe_')),
    angle = 90, hjust = 1, nudge_y = - 0.02
  ) +
  theme(legend.position = "none")


autoplot(
  grid_results,
  id = "recipe_svm_rbf",
  metric = "kap"
)



# finalizing model --------------------------------------------------------
best_model <- 'recipe_nnet'

best_results <- 
  grid_results %>% 
  extract_workflow_set_result(best_model) %>% 
  select_best(metric = "kap")

best_results

final_wf <- 
  grid_results %>% 
  extract_workflow(best_model) %>% 
  finalize_workflow(best_results)

final_wf

final_fit <- final_wf %>% fit(df_train) 
final_fit


# Prediction --------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds

table(preds)
sum(is.na(preds))

# Submission --------------------------------------------------------------
hasil <- df_sub %>% 
  mutate(Predicted = preds)

write.csv(hasil, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  

system(
  'kaggle competitions submit -c classification-data-challenge -f D:/__Datasets/sub.csv -m "Message"'
)


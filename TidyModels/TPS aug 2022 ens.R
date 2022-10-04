library(bonsai)
library(tidyverse)
library(tidymodels)
library(janitor)
library(themis)
library(inspectdf)
library(finetune)
tidymodels_prefer()


# avg ---------------------------------------------------------------------
df_train <- df_train %>% 
  rowwise() %>% 
  mutate(
    avg = mean(c_across(starts_with("measurement")), na.rm = T)
  )

df_test <- df_test %>% 
  rowwise() %>% 
  mutate(
    avg = mean(c_across(starts_with("measurement")), na.rm = T)
  )


# train test split --------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, strata = failure, prop = 0.7)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(train_set, v = 3, strata = failure)



# cor ---------------------------------------------------------------------
ggcorrplot::ggcorrplot(cor(
  df_train %>% select_if(is.numeric)
))

colSums(is.na(df_train))


# model -------------------------------------------------------------------
mlp_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('classification')

logreg_spec <-
  logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine('glmnet') 

lgbm_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('lightgbm') %>%
  set_mode('classification')

xgb_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')

rf_spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine('ranger') %>%
  set_mode('classification')



# recipe ------------------------------------------------------------------
my_recipe <- 
  recipe(failure ~ ., data = train_set) %>% 
  update_role(id, new_role = "id") %>% 
  update_role(product_code, new_role = "code") %>% 
  step_mutate(
    m_3_missing = as.factor(is.na(measurement_3)),
    m_5_missing = as.factor(is.na(measurement_5)),
    v23 = attribute_2 * attribute_3
  ) %>% 
  step_impute_knn(all_nominal_predictors()) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_downsample(failure) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(starts_with("measurement"), threshold = 0.9) %>% 
  step_dummy(all_nominal_predictors()) 

my_recipe %>% 
  summary() %>% 
  as.data.frame()

my_workflow <- 
  workflow_set(
    preproc = list(my_recipe), 
    models = list(
      logreg = logreg_spec,
      lgbm = lgbm_spec
    )
  )


# tuning ------------------------------------------------------------------
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

grid_ctrl <-
  control_grid(
    # parallel_over = "resampling",
    save_workflow = T,
    save_pred = TRUE,
    verbose = T,
    allow_par = T
  )

grid_results <-
  my_workflow %>%
  workflow_map(
    seed = 1,
    resamples = train_fold,
    grid = 5,
    control = grid_ctrl,
    metrics = classf_metric,
    verbose = TRUE
  )



race_ctrl <-
  control_race(
    save_pred = TRUE,
    verbose = T,
    # parallel_over = "everything",
    save_workflow = TRUE
  )

grid_results <-
  my_workflow %>%
  workflow_map(
    "tune_race_anova",
    seed = 1,
    resamples = train_fold,
    grid = 5,
    control = race_ctrl,
    metrics = classf_metric,
    verbose = TRUE
  )

grid_results$result[1]


# hasil -------------------------------------------------------------------
grid_results %>% 
  rank_results() %>% 
  filter(.metric == "roc_auc") %>% 
  select(model, .config, roc_auc = mean, rank)


autoplot(
  grid_results,
  rank_metric = "roc_auc",  # <- how to order models
  metric = "roc_auc",       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
) 
  geom_text(
    aes(y = min(mean), label = str_remove_all(wflow_id, 'recipe_')),
    angle = 90, hjust = 1, nudge_y = - 0.02
  ) +
  theme(legend.position = "none")




# bayesian search ---------------------------------------------------------
model_param <- my_workflow %>% 
  extract_parameter_set_dials()

set.seed(1)
model_bo <-
  my_workflow %>%
  tune_bayes(
    resamples = train_fold,
    metrics = classf_metric,
    initial = model_res,
    param_info = model_param,
    iter = 50,
    control = control_bayes(verbose = TRUE, no_improve = 20)
  )

# finalizing model --------------------------------------------------------
best_model <- 'recipe_logreg'

best_results <- 
  grid_results %>% 
  extract_workflow_set_result(best_model) %>% 
  select_best(metric = "roc_auc")

best_results

final_wf <- 
  grid_results %>% 
  extract_workflow(best_model) %>% 
  finalize_workflow(best_results)

final_wf

final_fit <- final_wf %>% fit(df_train) 
final_fit


# Prediction --------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test, type = "prob")
preds


# Submission --------------------------------------------------------------
hasil <- df_sub %>% mutate(failure = preds$.pred_1)
head(hasil)

write.csv(hasil, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  

system(
  'kaggle competitions submit -c tabular-playground-series-aug-2022 -f D:/__Datasets/sub.csv -m "Message"'
)







# blend -------------------------------------------------------------------
library(stacks)

# We found better performance for these data using the racing results.
# This might be due to the racing method pre-selecting the best model(s)
# from the larger grid.
my_stack <- 
  stacks() %>% 
  add_candidates(
    grid_results
  )

my_stack


# BLEND THE PREDICTIONS ---------------------------------------------------
# Using the lasso penalty can remove candidates 
# (and sometimes whole model types) from the ensemble.
# 
# The correlation between ensemble candidates tends to be very high,
# and regularization helps alleviate this issue.


set.seed(1)
ens <- blend_predictions(
  my_stack,
  mixture = 1,
  penalty = 10^seq(-2, -0.5, length = 20)
)
ens
autoplot(ens)

autoplot(ens, "weights") +
  geom_text(aes(x = weight + 0.01, label = model), hjust = 0) + 
  theme(legend.position = "none") +
  lims(x = c(-0.01, 0.8))


# fit
ens <- fit_members(ens)

# test set result
ens_pred <- 
  predict(ens, test_set) %>% 
  bind_cols(test_set)

ens_pred %>% 
  classf_metric(failure, .pred)






# predict -----------------------------------------------------------------
preds <- predict(ens, new_data = df_test, type = "prob")
preds


# Submission --------------------------------------------------------------
hasil <- df_sub %>% mutate(failure = preds$.pred_1)
head(hasil)

write.csv(hasil, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  

system(
  'kaggle competitions submit -c tabular-playground-series-aug-2022 -f D:/__Datasets/sub.csv -m "Message"'
)


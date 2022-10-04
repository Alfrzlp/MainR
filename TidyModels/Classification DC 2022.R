# Model -------------------------------------------------------------------
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

svm_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
  set_engine('kernlab') %>%
  set_mode('classification')

logreg_spec <-
  logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine('glmnet')


# recipe and workflow -----------------------------------------------------
glimpse(df_train)

my_recipe <- 
  recipe(Y ~ ., data = train_set) %>% 
  update_role(ID, new_role = "id") %>% 
  step_zv() %>% 
  step_downsample(Y) 
  

my_recipe %>% 
  summary() %>% 
  as.data.frame()

my_workflow <- 
  workflow() %>% 
  add_model(logreg_spec) %>% 
  add_recipe(my_recipe)


# Training ----------------------------------------------------------------
my_grid <- tibble(
  mtry = c(18, 5, 15, 27, 33, 57, 48),
  min_n = c(10, 6, 10, 3, 9, 3, 12)
)



library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

# grid search
set.seed(1)
model_res <- 
  my_workflow %>% 
  tune_grid(
    resamples = train_fold,
    grid = 150,
    control = control_grid(
      save_pred = TRUE,
      verbose = T,
      allow_par = T
    ),
    metrics = my_metric
  )

model_res$.notes


# bayesian
ctrl <- control_bayes(verbose = TRUE)
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
    control = ctrl
  )

# model_res$.notes[[1]]$note[1]

# hasil
model_res %>% 
  collect_metrics() %>% 
  filter(.metric == 'kap') %>% 
  arrange(desc(mean))  
  relocate(mean, .before = trees)


model_res %>% 
  collect_metrics() %>% 
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  arrange(desc(kap)) 
  relocate(c(accuracy, kap, f_meas), .before = trees) 



# Model terbaik -----------------------------------------------------------
best_model <- model_res %>% 
  select_best(metric = 'kap')

best_model <- model_res %>% 
  collect_metrics() %>% 
  slice(150)

best_model <- model_bo %>% 
  select_best()

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
  classf_metric(truth = buy, estimate = .pred_class)

df_train %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  conf_mat(truth = buy, estimate = .pred_class)

classf_metric

# test
test_set %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  classf_metric(truth = buy, estimate = .pred_class)

test_set %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  conf_mat(truth = buy, estimate = .pred_class)

# Prediksi ----------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds

table(preds)
sum(is.na(preds))

preds[is.na(preds)] <- 0

# Submission --------------------------------------------------------------
hasil <- df_sub %>% 
  mutate(Predicted = preds)

write.csv(hasil, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  

system(
  'kaggle competitions submit -c classification-data-challenge -f D:/__Datasets/sub.csv -m "Message"'
)


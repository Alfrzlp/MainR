library(tidyverse)
library(tidymodels)
library(themis)
library(bonsai)
tidymodels_prefer()

# Data --------------------------------------------------------------------
loc <- 'D:/Downloads/praktikum-3-dsai-klasifikasi'

df_train <- read.csv(file.path(loc, "data_train.csv"))
df_test <- read.csv(file.path(loc, "data_test.csv"))
df_val <- read.csv(file.path(loc, "data_validate.csv"))

glimpse(df_train)
glimpse(df_test)
glimpse(df_val)



# Data Prepocessing -------------------------------------------------------
unique(df_train$Y)

df_train <- df_train %>% 
  mutate_at(vars(Transport, Licence, Graduate, Employed, Gender), ~ as.factor(.x)) 

df_val <- df_val %>% 
  mutate_at(vars(Transport, Licence, Graduate, Employed, Gender), ~ as.factor(.x)) 

df_test <- df_test %>% 
  mutate_at(vars(Licence, Graduate, Employed, Gender), ~ as.factor(.x)) 

df_train <- rbind(df_train, df_val)
df_train %>% 
  glimpse()


# EDA ---------------------------------------------------------------------
colSums(is.na(df_train))
colSums(is.na(df_test))
colSums(is.na(df_val))

glimpse(df_train)

df_train %>% 
  ggplot(aes(x = Transport, y = Work.Exp)) +
  geom_boxplot() +
  geom_hline(yintercept = 16)

df_train %>% 
  filter(Transport != "Private Transport", Salary > 16)


# Metric ------------------------------------------------------------------
my_metric <- metric_set(f_meas)


# train test split --------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, prop = 0.8, strata = Transport)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(train_set, v = 5, strata = Transport)



# EDA ---------------------------------------------------------------------
colSums(is.na(df_train))




# model -------------------------------------------------------------------
rf_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine('ranger') %>%
  set_mode('classification')

rf2_spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine('randomForest') %>%
  set_mode('classification')

xgb_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')

lgbm_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('lightgbm') %>%
  set_mode('classification')

mlp_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('classification')

multinom_spec <-
  multinom_reg(penalty = tune(), mixture = tune()) %>%
  set_engine('glmnet')





# recipe and workflow -----------------------------------------------------
table(df_train$Transport)

my_recipe <- 
  recipe(Transport ~ ., data = train_set) %>% 
  update_role(ID, new_role = "id") %>% 
  step_mutate(
    age = ifelse(Age > 30, "Ya", "Tidak"),
    s = ifelse(Salary > 16, "Ya", "Tidak"),
    we = ifelse(Work.Exp > 10, "Ya", "Tidak")
  ) %>% 
  step_string2factor(c(age, s, we)) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_scale(all_numeric_predictors()) %>% 
  step_zv() 
  step_downsample(Transport, under_ratio = 0.9) 


my_workflow <- 
  workflow_set(
    preproc = list(my_recipe), 
    models = list(
      xgb = xgb_spec,
      multinom = multinom_spec,
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
    grid = 50,
    control = grid_ctrl,
    metrics = my_metric,
    verbose = TRUE
  )

grid_results$result
collect_notes(grid_results)
# res1 <- grid_results
# res2 <- grid_results

# Hasil -------------------------------------------------------------------
metric_name <- 'f_meas'

grid_results %>% 
  rank_results() %>% 
  filter(.metric == metric_name) %>% 
  select(model, .config, accuracy = mean, rank)




autoplot(
  grid_results,
  rank_metric = metric_name,  # <- how to order models
  metric = metric_name,       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
) 


autoplot(
  grid_results,
  id = "recipe_logreg",
  metric = metric_name
)



# finalizing model --------------------------------------------------------
best_model <- 'recipe_rf'

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

# final_fit <- final_wf %>% fit(rbind(df_train, df_val)) 
final_fit <- final_wf %>% fit(df_train) 
final_fit


# Evaluasi ----------------------------------------------------------------
# train
df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  my_metric(truth = Transport, estimate = .pred_class)

df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  conf_mat(truth = Transport, estimate = .pred_class)

# test
test_set %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  my_metric(truth = Transport, estimate = .pred_class)

test_set %>% 
  bind_cols(predict(final_fit, .)) %>% 
  conf_mat(truth = Transport, estimate = .pred_class)

# Prediction --------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds

table(preds)


# Submission --------------------------------------------------------------
hasil <- df_test %>% 
  select(ID) %>% 
  mutate(Transport = preds)

write.csv(hasil, 'E:/sub2.csv', row.names = F, quote = F)  


system(
  'kaggle competitions submit -c praktikum-3-dsai-klasifikasi -f E:/sub2.csv -m "Message"'
)




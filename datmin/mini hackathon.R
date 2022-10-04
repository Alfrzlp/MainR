library(tidyverse)
library(tidymodels)
library(janitor)
tidymodels::tidymodels_prefer()


# Data --------------------------------------------------------------------
df_train <- read.csv("D:/Training.csv") %>% 
  clean_names() 

df_test <- read.csv("D:/Testing.csv") %>% 
  clean_names()

glimpse(df_train)
glimpse(df_test)



# EDA ---------------------------------------------------------------------
glimpse(df_train)

df_train <- df_train %>% 
  mutate(numid = as.numeric(str_extract(id, "\\d+"))) 

df_test <- df_test %>% 
  mutate(numid = as.numeric(str_extract(id, "\\d+"))) 


# korelasi
dat %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot()


# viz ---------------------------------------------------------------------
dat %>% 
  pivot_longer(-y) %>% 
  ggplot(aes(x = value, y = y, col = name)) +
  geom_point() +
  facet_wrap(~ name, 4, 4, scales = 'free')
 

ggplot(df_train, aes(x = x7, y = y)) +
  geom_point()

# Metric ------------------------------------------------------------------
reg_metric <- metric_set(rmse)

# train test split --------------------------------------------------------
library(rsample)
set.seed(1)
splits <- initial_split(df_train, prop = 0.8)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(train_set, v = 5)


knn_model <- kknn(Y ~ ., train = df_train[-1], test = df_test)
knn_model$fitted.values

# Model -------------------------------------------------------------------
rf_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine('ranger') %>%
  set_mode('regression')

xgb_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

mlp_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('regression')

svm_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
  set_engine('kernlab') %>%
  set_mode('regression')



# recipe and workflow -----------------------------------------------------
glimpse(df_train)

my_recipe <- 
  recipe(y ~ ., data = train_set) %>% 
  update_role(id, new_role = "id") 
  step_interact( ~ all_predictors():all_predictors()) %>% 
  step_pca(threshold = 0.9) 


my_workflow <- 
  workflow_set(
    preproc = list(my_recipe), 
    models = list(
      rf = rf_spec,
      xgb = xgb_spec
      # nnet = mlp_spec,
      # svm_rbf = svm_spec
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

library(finetune)
grid_results <-
  my_workflow %>%
  workflow_map(
    # "tune_race_anova",
    seed = 1,
    resamples = train_fold,
    grid = 50,
    control = grid_ctrl,
    # metrics = reg_metric,
    metrics = metric_set(rmse),
    verbose = TRUE
  )


# Hasil -------------------------------------------------------------------
my_metric <- 'rsq'
my_metric <- 'rmse'


grid_results %>% 
  rank_results() %>% 
  # filter(.metric == "rsq") %>% 
  select(model, .config, rsq = mean, rank)

grid_results %>% 
  rank_results() %>% 
  group_by(model) %>% 
  summarise(best_rsq = min(mean))


autoplot(
  grid_results,
  rank_metric = my_metric,  # <- how to order models
  metric = my_metric,       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
) 
  geom_text(
    aes(y = min(mean), label = str_remove_all(wflow_id, 'recipe_')),
    angle = 90, hjust = 1, nudge_y = - 0.02
  ) +
  theme(legend.position = "none")


autoplot(
  grid_results,
  id = "recipe_rf",
  metric = "rsq"
)



# finalizing model --------------------------------------------------------
best_model <- 'recipe_rf'
best_model <- 'recipe_xgb'

best_results <- 
  grid_results %>% 
  extract_workflow_set_result(best_model) %>% 
  select_best(metric = my_metric)

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


# Prediksi ----------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds


# Submission --------------------------------------------------------------
hasil <- df_test %>% 
  select(ID) %>% 
  mutate(Y = preds)

write.csv(hasil, 'E:/sub.csv', row.names = F, quote = F)  








library(tidyverse)
library(tidymodels)


# data --------------------------------------------------------------------
df_train <- read.csv("D:/Training.csv")
df_test <- read.csv("D:/Testing.csv")

df_train <- df_train %>% 
  mutate(
    Y = ifelse(Y > mean(Y), 1, 0),
    Y = as.factor(Y)
  )



glimpse(df_train)
glimpse(df_test)

colSums(is.na(df_train))
colSums(is.na(df_test))

table(df_train$Y)

# Metric ------------------------------------------------------------------
classf_metric <- metric_set(accuracy)




# Train test split ---------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, prop = 0.7, strata = Y)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(train_set, v = 5, strata = Y)

# all model ---------------------------------------------------------------
logreg_spec <-
  logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine('glmnet')

rf_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine('ranger') %>%
  set_mode('classification')

nb_spec <-
  naive_Bayes(smoothness = tune(), Laplace = tune()) %>%
  set_engine('naivebayes')

knn_spec <-
  nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) %>%
  set_engine('kknn') %>%
  set_mode('classification')

svm_rbf_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
  set_engine('kernlab') %>%
  set_mode('classification')





# recipe and workflow -----------------------------------------------------
my_recipe <- 
  recipe(Y ~ ., data = train_set) %>% 
  step_rm(ID) 


my_workflow <- 
  workflow_set(
    preproc = list(my_recipe), 
    models = list(
      logreg = logreg_spec,
      rf = rf_spec,
      svm_rbf = svm_rbf_spec,
      # nb = nb_spec,
      knn = knn_spec
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
    metrics = metric_set(accuracy),
    verbose = TRUE
  )


# Hasil -------------------------------------------------------------------
my_metric <- "accuracy"

grid_results %>% 
  rank_results() %>% 
  filter(.metric == my_metric) %>% 
  select(model, .config, akurasi = mean, rank) %>% 
  group_by(model) %>% 
  summarise(max(akurasi))



autoplot(
  grid_results,
  rank_metric = my_metric,  # <- how to order models
  metric = my_metric,       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
) 



autoplot(
  grid_results,
  id = "recipe_rf",
  metric = my_metric
)



# finalizing model --------------------------------------------------------
best_model <- 'recipe_rf'

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
df_train %>% 
  bind_cols(pred = predict(final_fit, df_train)) %>% 
  classf_metric(truth = Y, estimate = .pred_class)

test_set %>% 
  bind_cols(pred = predict(rf_model, test_set)) %>% 
  classf_metric(truth = Y, estimate = .pred_class)

rf_model <- final_fit



# Prediction --------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds

table(preds)
sum(is.na(preds))



model <- e1071::naiveBayes(Y ~ ., df_train)
pred_nb <- predict(model, df_train)


library(yardstick)
library(dplyr)
df_train <- read.csv("D:/Training.csv")

df_train <- df_train %>% 
  mutate(
    Y = ifelse(Y > mean(Y), 1, 0),
    Y = as.factor(Y)
  )

model <- glm(Y ~ ., df_train, family = "binomial")
pred_glm <- predict(model, df_train, type = 'res')
pred_glm <- ifelse(pred_glm > 0.5, 1, 0)

table(df_train$Y, pred_glm)
accuracy(df_train, truth = Y, estimate = as.factor(pred_glm))




df_submission <- df_test %>% 
  select(ID) %>% 
  mutate(Y = pred)

library(randomForest)
rf_model <- randomForest(Y ~ ., data = train)
library(tidyverse)
library(tidymodels)
# library(bonsai)
library(themis)
library(janitor)
library(inspectdf)
tidymodels_prefer()


# Data --------------------------------------------------------------------
loc <- 'D:/__Datasets/ml/titanic/'

df_train <- read.csv(file.path(loc, 'train.csv'), row.names = 'PassengerId') %>% 
  mutate(Survived = as.factor(Survived)) %>% 
  clean_names() 

df_test <- read.csv(file.path(loc, 'test.csv'), row.names = 'PassengerId') %>% 
  clean_names() 

df_sub <- read.csv('D:/__Datasets/ml/submission_titanic.csv')
# PassengerId Survived

df_test$Survived <- as.factor(df_sub$Survived)

glimpse(df_train)
glimpse(df_test)
glimpse(df_sub)


# EDA dan FE --------------------------------------------------------------
inspect_cat(df_train)
inspect_num(df_train)

table(df_train$cabin)
# - terlalu banyak NA di cabin

colSums(is.na(df_train))
colSums(is.na(df_test))


# gelar
title_pattern <- '(?<=\\s)(Mr|Mrs|Ms|Miss|Master|Rev|Dr|Col|Mlle|Don|Major|Capt|Sir|Mme|Jonkheer|Lady)(?=\\.\\s)'
df_train <- df_train %>% 
  mutate(
    gelar = str_extract(name, title_pattern),
    is_alone = ifelse(sib_sp == 0 & parch == 0, 1, 0)
  ) 

df_test <- df_test %>% 
  mutate(
    gelar = str_extract(name, title_pattern),
    is_alone = ifelse(sib_sp == 0 & parch == 0, 1, 0)
  ) 

unique(df_train$gelar)
table(df_train$gelar)



# Data splitting ----------------------------------------------------------
set.seed(1)
splits <- initial_split(df_test, prop = 0.8)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(train_set, v = 5)


# Model -------------------------------------------------------------------
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

lgbm_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('lightgbm') %>%
  set_mode('classification')


# Recipe ------------------------------------------------------------------
glimpse(df_train)

no_prepro <- 
  recipe(Survived ~ pclass + sex + age + sib_sp + parch + fare + embarked + gelar + is_alone,
         data = train_set) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) 

linear_rec <- 
  no_prepro %>%
  step_normalize(all_numeric_predictors())



# Workflow ----------------------------------------------------------------
linear_wflow <- 
  workflow_set(
    preproc = list(linear_rec), 
    models = list(logreg = logreg_spec)
  )

tree_wflow <- 
  workflow_set(
    preproc = list(no_prepro),
    models = list(
      rf = rf_spec,
      xgb = xgb_spec
    )
  )


all_workflows <- 
  bind_rows(linear_wflow, tree_wflow) %>% 
  mutate(wflow_id = gsub("(recipe_)|(normalized_)", "", wflow_id))

all_workflows




# Grid Search -------------------------------------------------------------
grid_ctrl <-
  control_grid(
    # parallel_over = "resampling",
    save_workflow = TRUE,
    save_pred = TRUE,
    verbose = T,
    allow_par = T
  )



library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)


grid_results <-
  all_workflows %>%
  workflow_map(
    seed = 1,
    grid = 100,
    resamples = train_fold,
    control = grid_ctrl,
    metrics = metric_set(accuracy),
    verbose = TRUE
  )





# Hasil -------------------------------------------------------------------
grid_results %>% 
  # filter(wflow_id != 'rf') %>% 
  rank_results() %>% 
  filter(.metric == "accuracy") %>% 
  select(model, .config, accuracy = mean, rank)


grid_results %>% 
  # filter(wflow_id != 'rf') %>% 
  rank_results() %>% 
  filter(.metric == "accuracy") %>% 
  ggplot() +
  geom_boxplot(
    aes(x = wflow_id, y = mean, fill = wflow_id)
  )

grid_results %>% 
  # filter(wflow_id != 'rf') %>% 
  autoplot(
    rank_metric = "accuracy",  # <- how to order models
    metric = "accuracy",       # <- which metric to visualize
    select_best = TRUE     # <- one point per workflow
  ) +
  guides(prepocessor = "none")
  geom_text(
    aes(y = min(mean), label = str_remove_all(wflow_id, 'recipe_')),
    angle = 90, hjust = 1, nudge_y = - 0.02
  ) +
  theme(legend.position = "none")


autoplot(
  grid_results,
  id = "rf",
  metric = "accuracy"
)
autoplot(
  grid_results,
  id = "logreg",
  metric = "accuracy"
)



# racing approach ---------------------------------------------------------
library(finetune)

race_ctrl <-
  control_race(
    save_pred = TRUE,
    parallel_over = "resamples",
    save_workflow = TRUE
  )

# hasil
race_results <-
  all_workflows %>%
  filter(wflow_id != 'rf') %>% 
  workflow_map(
    "tune_race_anova",
    seed = 1,
    grid = 25,
    resamples = train_fold,
    control = race_ctrl,
    metrics = metric_set(accuracy),
    verbose = TRUE
  )

race_results

autoplot(
  race_results,
  rank_metric = "accuracy",  
  metric = "accuracy",       
  select_best = TRUE    
)

race_results %>% 
  rank_results()


# Best Model --------------------------------------------------------------
best_results <- 
  race_results %>% 
  extract_workflow_set_result("logreg") %>% 
  select_best(metric = "accuracy")
best_results


final_fit <- 
  race_results %>% 
  extract_workflow("logreg") %>% 
  finalize_workflow(best_results) %>% 
  fit(df_train) 



# grid search
best_results <- 
  grid_results %>% 
  extract_workflow_set_result("rf") %>% 
  select_best(metric = "accuracy")
best_results


final_fit <- 
  grid_results %>% 
  extract_workflow("rf") %>% 
  finalize_workflow(best_results) %>% 
  fit(df_test) 


# Evaluasi ----------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test)

df_sub <- df_sub %>%
  mutate(
    .pred_class = preds$.pred_class,
    Survived = as.factor(Survived)
  ) 
df_sub %>% glimpse()

df_sub %>% 
  accuracy(truth = Survived, .pred_class)

df_sub %>% 
  conf_mat(truth = Survived, .pred_class)





# submission --------------------------------------------------------------
df_sub %>% 
  select(PassengerId, Survived = .pred_class) %>% 
  write.csv('D:/__Datasets/sub.csv', row.names = F, quote = F)  


# kirim submission --------------------------------------------------------
system(
  'kaggle competitions submit -c 10botics-titantic -f D:/__Datasets/sub.csv -m "Message"'
)






# Ensemble ----------------------------------------------------------------
library(stacks)

titanic_stack <- 
  stacks() %>% 
  add_candidates(grid_results)

titanic_stack


set.seed(1)
ens <- blend_predictions(
  titanic_stack,
  penalty = 10^seq(-2, -0.5, length = 20)
)
ens


autoplot(ens, "weights") +
  geom_text(aes(x = weight + 0.01, label = model), hjust = 0) + 
  theme(legend.position = "none") +
  lims(x = c(-0.01, 0.8))







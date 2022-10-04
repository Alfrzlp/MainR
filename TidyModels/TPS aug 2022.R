library(tidyverse)
library(tidymodels)
library(janitor)
library(themis)
library(inspectdf)
tidymodels_prefer()

# Data --------------------------------------------------------------------
loc <- 'D:/__Datasets/ml/tabular-playground-series-aug-2022/'
list.files(loc)


df_train <- read.csv(file.path(loc, 'train.csv')) %>% 
  clean_names() %>% 
  mutate(
    failure = factor(failure)
  )

df_test <- read.csv(file.path(loc, 'test.csv')) %>% 
  clean_names() 

df_sub <- read.csv(file.path(loc, 'sample_submission.csv'))

glimpse(df_train)
glimpse(df_test)
glimpse(df_sub)



# EDA ---------------------------------------------------------------------
dim(df_train)
dim(df_test)


# NA
inspect_na(df_train, df_test)
colSums(is.na(df_train))
colSums(is.na(df_test))

# kategorik
cat_train <- inspect_cat(df_train)
cat_test <- inspect_cat(df_test)

cat_train
cat_test

# numerik
inspect_num(df_train, df_test)


unique(df_train$product_code)

cat_train$levels[2]
cat_test$levels[2]

cat_train$levels[1]
cat_test$levels[1]

cat_train$levels[4]
cat_test$levels[3]




# Metric ------------------------------------------------------------------
classf_metric <- metric_set(roc_auc)


df_train <- df_train %>% 
  mutate(
    attribute_1 = ifelse(attribute_1 == "material_8", "material_7", attribute_1)
  )


# train test split --------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, strata = failure, prop = 0.7)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(train_set, v = 3)



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

df_train %>% 
  rowwise() %>% 
  mutate(
    avg = mean(c_across(starts_with("measurement")), na.rm = T)
  )

# recipe and workflow -----------------------------------------------------
glimpse(df_train)
df_train %>% count(failure)

# X['m_3_missing'] = X.measurement_3.isna()
# X['m_5_missing'] = X.measurement_5.isna()

my_recipe <- 
  recipe(failure ~ ., data = train_set) %>% 
  update_role(id, new_role = "id") %>% 
  update_role(product_code, new_role = "code") %>% 
  step_mutate(
    m_3_missing = as.factor(is.na(measurement_3)),
    m_5_missing = as.factor(is.na(measurement_5)),
    v23 = attribute_2 * attribute_3
  ) %>% 
  step_downsample(failure) %>% 
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_knn(all_nominal_predictors()) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_pca(starts_with("measurement"), threshold = 0.9) %>% 
  step_dummy(all_nominal_predictors()) 
  

my_recipe %>% 
  summary() %>% 
  as.data.frame()

my_workflow <- 
  workflow() %>% 
  add_model(logreg_spec) %>% 
  add_recipe(my_recipe)


# Training ----------------------------------------------------------------
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
    grid = 100,
    control = control_grid(
      save_pred = TRUE,
      verbose = T,
      allow_par = T
    ),
    metrics = classf_metric
  )

collect_notes(model_res)$note[1]

model_res %>% 
  collect_metrics() %>% 
  arrange(desc(mean))



# bayesian search ---------------------------------------------------------
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





# Model terbaik -----------------------------------------------------------
best_model <- model_res %>% 
  select_best()

best_model <- model_bo %>% 
  select_best()

best_model

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
  mutate(.pred_class = as.numeric(.pred_class) - 1) %>% 
  classf_metric(truth = failure, .pred_class)

df_train %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  conf_mat(truth = failure, estimate = .pred_class)



# test
test_set %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  mutate(.pred_class = as.numeric(.pred_class) - 1) %>% 
  classf_metric(truth = failure, .pred_class)

test_set %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  conf_mat(truth = failure, estimate = .pred_class)


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






my_prep <- prep(my_recipe, df_train)
train_data <- bake(my_prep, new_data = df_train)
test_data <- bake(my_prep, new_data = df_test)
glimpse(train_data)

m1 <- glm(
  failure ~ (.)^2, data = train_data %>% 
    select(-c(id, product_code)) %>% 
    mutate(
      failure = as.numeric(failure) - 1
    )
)

pred <- predict(m1, newdata = test_data, type = 'resp')
hasil <- df_sub %>% 
  mutate(failure = pred + 0.25)
head(hasil)

library(tidyverse)
library(tidymodels)
library(janitor)
library(themis)
tidymodels::tidymodels_prefer()


# Data --------------------------------------------------------------------
loc <- 'D:/__Datasets/ml/2022-classification-data-challenge'
list.files(loc)


df_train <- read.csv(file.path(loc, 'train.csv')) %>% 
  clean_names() %>% 
  mutate(buy = as.factor(buy)) 
  mutate_at(
    vars(-c(v55, v64, v84)),
    ~ as.factor(.x)
  )

df_test <- read.csv(file.path(loc, 'test.csv')) %>% 
  clean_names() 
  mutate_at(
    vars(-c(v55, v64, v84)),
    ~ as.factor(.x)
  )

df_sub <- read.csv(file.path(loc, 'submission.csv'))

glimpse(df_train)
glimpse(df_test)
glimpse(df_sub)



# -------------------------------------------------------------------------
n_unique_train <- df_train %>% 
  select(-buy) %>% 
  summarise_all( ~ length(unique(.x))) %>% 
  t() %>% as.data.frame() %>% 
  pull()


n_unique_test <- df_test %>% 
  summarise_all( ~ length(unique(.x))) %>% 
  t() %>% as.data.frame() %>% 
  pull()


data.frame(
  var = colnames(df_test),
  n_train = n_unique_train,
  n_test = n_unique_test
) %>% 
  mutate(
    cek = ifelse(n_train < n_test, 'tidak valid', 'valid')
  )


# EDA ---------------------------------------------------------------------
df_cor <- cor(df_train[-1] %>% mutate(buy = as.numeric(buy) - 1))

ggcorrplot::ggcorrplot(df_cor) +
  theme(
    axis.text.x = element_text(size = 9, angle = 90)
  )
df_cor[1,]


X1 <- princomp(df_train %>% select(v2:v43))
X2 <- princomp(df_train %>% select(v43:v86))

# 34 dan 36
loadings(X1)
loadings(X2)


df_train <- df_train %>% 
  select(buy) %>% 
  bind_cols(
    X1$scores[, 1:34]
  ) %>% 
  bind_cols(
    X2$scores[, 1:36]
  )


# Metric ------------------------------------------------------------------
df_train %>% 
  count(buy)

classf_metric <- metric_set(kap)

# train test split --------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, prop = 0.8)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(train_set, v = 5)



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
  recipe(buy ~ ., data = train_set) %>% 
  step_rm(id) %>%
  step_pca(num_range("v", 2:34), num_comp = 34, prefix = 'x1_') %>% 
  step_pca(num_range("v", 35:86), num_comp = 36, prefix = 'x2_') %>% 
  step_zv() %>% 
  step_interact( ~ all_predictors():all_predictors()) %>% 
  step_downsample(buy) 
  

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
    metrics = classf_metric
  )



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





x <- predict(final_fit, new_data = df_test, type = 'prob')
preds <- x %>% 
  mutate(x = if_else(.pred_1 > 0.515, 1, 0)) %>% 
  pull(x) 
  
preds %>% table()




df_test %>% 
  bind_cols(pred = preds) %>% 
  select(id, pred) %>% 
  table()


df_train %>% 
  filter(buy == 1) %>% 
  select(id, buy) 
  table()

df_test$id



# -------------------------------------------------------------------------
m1 <- glm(buy ~ .,
          data = df_train %>% 
            select(-id) %>% 
            mutate(buy = as.numeric(buy) - 1)
          )
summary(m1)
m1 <- stats::step(m1, trace = F)

preds <- ifelse(
  predict(m1, newdata = df_test, type = 'resp') > 0.065, 1, 0
) 

table(preds)

preds_terbaik <- preds





# save model --------------------------------------------------------------
saveRDS(object = final_fit, file = "model/xgbClassfDC2022.rds")
m1 <- readRDS(file = "model/xgbClassfDC2022.rds")
m1


predict(m1, new_data = df_test)

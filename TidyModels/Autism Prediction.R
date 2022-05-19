library(tidyverse)
library(tidymodels)
library(janitor)

tidymodels_prefer()


# Data' -------------------------------------------------------------------
df_train <- 
  read_csv('D:/_Datasets/Autism_Prediction/train.csv') %>% 
  clean_names()

df_test <- 
  read_csv('D:/_Datasets/Autism_Prediction/test.csv') %>% 
  clean_names()

df_sub <- read_csv('D:/_Datasets/Autism_Prediction/sample_submission.csv')

glimpse(df_train)
glimpse(df_test)



# Data information ---------------------------------------------------------------------

# class
df_train %>% 
  group_by(class_asd) %>% 
  count()


# n unique
n_unique <- 
  df_train %>% 
  summarise_if(is.character, ~ length(unique(.x))) %>% 
  pivot_longer(everything(), values_to = 'n_unique', names_to = 'variabel')

n_unique

# Get all 
df_train[n_unique$variabel] %>% 
  apply(2, FUN = function(x) unique(x))

df_test[n_unique$variabel] %>% 
  apply(2, FUN = function(x) unique(x))




# Final Datasets ----------------------------------------------------------
df_train <- 
  df_train %>% 
  mutate(
    across(c(relation, ethnicity), ~ ifelse(.x == '?', 'other', .x)),
    class_asd = as.factor(class_asd)
  ) %>% 
  select(-age_desc) %>% 
  rowwise() %>% 
  mutate(
    score = sum(across(ends_with('score'))),
    .after = id
  ) %>% 
  ungroup() %>% 
  mutate(
    kat_score = ifelse(score >= 6, 'baik', 'jelek')
  )




df_test <- 
  df_test %>% 
  mutate_at(
    vars(relation, ethnicity), ~ ifelse(.x == '?', 'other', .x)
  ) %>% 
  select(-age_desc) %>% 
  rowwise() %>% 
  mutate(
    score = sum(across(ends_with('score'))),
    .after = id
  ) %>% 
  ungroup() %>% 
  mutate(
    kat_score = ifelse(score >= 6, 'baik', 'jelek')
  )
  


glimpse(df_train)
glimpse(df_test)






# EDA ---------------------------------------------------------------------
# score
ggplot(df_train, aes(x = score, fill = class_asd)) +
  geom_bar(position = position_dodge()) +
  scale_x_continuous(breaks = 0:10) +
  facet_grid(~gender, scales = 'free')

ggplot(df_train, aes(x = a3_score, fill = class_asd)) +
  geom_bar(position = position_dodge()) +
  scale_x_continuous(breaks = 0:10) +
  facet_grid(~gender, scales = 'free')



ggplot(df_train, aes(x = score, fill = class_asd)) +
  geom_bar(position = position_dodge()) +
  scale_x_continuous(breaks = 0:10) +
  facet_grid(~used_app_before, scales = 'free')


# age
ggplot(df_train, aes(x = class_asd, y = age)) +
  geom_boxplot()

ggplot(df_train, aes(x = class_asd, y = result)) +
  geom_boxplot()

ggplot(df_train, aes(y = contry_of_res)) +
  geom_bar()

ggplot(df_train, aes(x = age, y = result)) +
  geom_point() +
  facet_grid(~class_asd)



ggplot(df_train, aes(x = jaundice, y = score)) +
  geom_boxplot() +
  facet_grid(~class_asd)


# Missing Value -----------------------------------------------------------
df_train %>% 
  summarise_all(.funs = ~ sum(is.na(.x))) %>% 
  pivot_longer(everything()) %>% 
  mutate(p = value/nrow(df_train)) %>% 
  filter(value > 0) 

df_test %>% 
  summarise_all(.funs = ~ sum(is.na(.x))) %>% 
  pivot_longer(everything()) %>% 
  mutate(p = value/nrow(df_train)) %>% 
  filter(value > 0) 




glimpse(df_train)
glimpse(df_test)

df_train %>% select_if(is.character) %>% colnames()

# Train Test Split --------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, strata = class_asd)
splits

train_set <- training(splits)
test_set  <- testing(splits)


set.seed(1)
val_set <- validation_split(
  train_set, 
  strata = class_asd, 
  prop = 0.9
)
val_set


# Simple Model ------------------------------------------------------------
cores <- parallel::detectCores()
cores

rf_model <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine('glmnet') 

rf_model <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')



# recipe
rf_recipe <- 
  recipe(
    class_asd ~ score + kat_score + result + austim + a1_score + a2_score + a3_score + a4_score + a5_score + a6_score + a7_score + a8_score + a9_score + a10_score + used_app_before + ethnicity + relation + jaundice + age + gender,
    data = train_set) %>% 
  # step_rm(id) %>%
  step_string2factor(all_nominal_predictors()) %>% 
  step_interact(terms = ~ gender:score) %>% 
  # step_other(contry_of_res, threshold = 0.2) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  themis::step_upsample(class_asd, over_ratio = 0.9) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(c(result, age)) %>% 
  step_pca(ends_with('_score'))


# workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rf_recipe)


lr_grid <- tibble(penalty = c(10^seq(-4, -1, length.out = 90), 0.2))
lr_grid

# Training ----------------------------------------------------------------
rf_res <- 
  rf_workflow %>% 
  tune_grid(
    val_set,
    grid = 200,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc)
  )


rf_res %>% 
  collect_metrics() %>% 
  arrange(desc(mean))

rf_res %>% 
  select_best()



# Analisis hasil ----------------------------------------------------------
rf_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())





# Best Model --------------------------------------------------------------
rf_best <- 
  rf_res %>% 
  select_best()

rf_best




# Final -------------------------------------------------------------------
final_wf <- rf_workflow %>% 
  finalize_workflow(rf_best)

final_wf


final_fit <- 
  final_wf %>%
  fit(df_train) 

final_fit



# Hasil -------------------------------------------------------------------
df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  conf_mat(truth = class_asd, .pred_class)


autism_metric <- metric_set(f_meas, accuracy, sensitivity)

df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  autism_metric(truth = class_asd, estimate = .pred_class)


# Predict -----------------------------------------------------------------
hasil <- 
  df_sub %>% 
  mutate(
    `Class/ASD` = predict(final_fit, new_data = df_test) %>% pull()
  )

head(hasil)

hasil %>% 
  write.csv('E:/sub.csv', row.names = F, quote = F)  


# kirim submission --------------------------------------------------------
system(
  'kaggle competitions submit -c autismdiagnosis -f E:/sub.csv -m "Message"'
)

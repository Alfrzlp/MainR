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
  count() %>% 
  ungroup() %>% 
  mutate(p = n/sum(n))


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
    kat_score = ifelse(score >= 6, 'baik', 'jelek'),
    test_vs_result = score / result
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
    kat_score = ifelse(score >= 6, 'baik', 'jelek'),
    test_vs_result = score / result
  )
  


glimpse(df_train)
glimpse(df_test)






# EDA ---------------------------------------------------------------------

df_train %>% 
  select(id, ends_with('score'), class_asd, -kat_score) %>% 
  pivot_longer(-c(class_asd, id)) 
  ggplot(aes(y = name, fill = class_asd)) +
  geom_bar(position = position_dodge())


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
  prop = 0.8
)
val_set



# Model -------------------------------------------------------------------
rf_model <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('classification')



cores <- parallel::detectCores()
cores

rf_model <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine('glmnet') 

rf_model <- 
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine('ranger', num.threads = 3) %>% 
  set_mode('classification')
  



rf_model <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost', num.threads = cores) %>%
  set_mode('classification')



# recipe
rf_recipe <- 
  recipe(
    class_asd ~ a1_score + a2_score + a3_score + a4_score + a5_score + a6_score + a7_score + a8_score + a9_score + a10_score + age + result + gender + test_vs_result,
    data = train_set) %>% 
  # add_role(id, new_role = "id") %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>% 
  # step_interact(terms = ~ gender:score) %>% 
  # step_other(contry_of_res, threshold = 0.2) %>% 
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>% 
  themis::step_smote(class_asd, over_ratio = 0.9, seed = 1, column = -all_nominal())


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
    grid = lr_grid,
    control = control_grid(save_pred = TRUE, verbose = T, allow_par = T),
    metrics = metric_set(accuracy)
  )


rf_res %>% 
  collect_metrics() %>% 
  arrange(desc(mean)) %>% 
  select(mean)

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
autism_metric <- metric_set(f_meas, accuracy, sensitivity)

df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  conf_mat(truth = class_asd, .pred_class)

df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  autism_metric(truth = class_asd, estimate = .pred_class)


# test
test_set %>% 
  bind_cols(predict(final_fit, .)) %>% 
  conf_mat(truth = class_asd, estimate = .pred_class)

test_set %>% 
  bind_cols(predict(final_fit, .)) %>% 
  autism_metric(truth = class_asd, estimate = .pred_class)



# Predict -----------------------------------------------------------------
hasil <- 
  df_sub %>% 
  mutate(
    `Class/ASD` = predict(final_fit, new_data = df_test) %>% pull()
  )

head(hasil)
table(hasil$`Class/ASD`)

hasil %>% 
  write.csv('E:/sub.csv', row.names = F, quote = F)  


# kirim submission --------------------------------------------------------
system(
  'kaggle competitions submit -c autismdiagnosis -f E:/sub.csv -m "Message"'
)









library(calibrateBinary)
library(randomForest)
mod = randomForest(x = x_train,
                  y = df_train$class_asd,
                  ntree = 1500, mtry = 10, replace = T)

preds <- predict(mod, newdata = df_test)
table(preds)


hasil <- 
  df_sub %>% 
  mutate(
    `Class/ASD` = preds
  )

hasil %>% 
  write.csv('E:/sub.csv', row.names = F, quote = F)  


# kirim submission --------------------------------------------------------
system(
  'kaggle competitions submit -c autismdiagnosis -f E:/sub.csv -m "Message"'
)


range01 <- function(x){(x-min(x))/(max(x)-min(x))}
pred <- ifelse(range01(preds) > 0.75, 1, 0)
table(pred)

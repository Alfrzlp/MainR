library(tidymodels)
library(janitor)
tidymodels_prefer()

system('kaggle competitions download -c lassification-type-of-oil-field')


# Data --------------------------------------------------------------------
df_train <- 
  read_csv('D:/_Datasets/TypeOilField/train_final.csv') %>% 
  clean_names()

df_test <- 
  read_csv('D:/_Datasets/TypeOilField/test_final.csv') %>% 
  clean_names()

df_sub <- read_csv('D:/_Datasets/TypeOilField/sample_submition.csv')

glimpse(df_train)
glimpse(df_test)


# EDA ---------------------------------------------------------------------

df_train %>% 
  group_by(onshore_offshore) %>% 
  count()


# Missing Value -----------------------------------------------------------
df_train %>% 
  summarise_all(.funs = ~ sum(is.na(.x))) %>% 
  pivot_longer(everything()) %>% 
  top_n(5)

df_test %>% 
  summarise_all(.funs = ~ sum(is.na(.x))) %>% 
  pivot_longer(everything(), values_to = 'n') %>% 
  filter(n > 0) %>% 
  arrange(desc('n'))


# unique ------------------------------------------------------------------
n_unique <- df_train %>% 
  summarise_if(is.character, ~ length(unique(.x))) %>% 
  pivot_longer(everything(), values_to = 'n_unique', names_to = 'variabel')

n_unique



# unique tectonic_regime
unique_tectonic_regime <- 
  str_split(df_train$tectonic_regime, pattern = '/') %>% 
  unlist() %>% 
  unique()

unique_tectonic_regime


df_train[str_to_lower(unique_tectonic_regime)] <-
  do.call(cbind, lapply(unique_tectonic_regime, function(x) str_detect(df_train$tectonic_regime, x)))

df_test[str_to_lower(unique_tectonic_regime)] <-
  do.call(cbind, lapply(unique_tectonic_regime, function(x) str_detect(df_test$tectonic_regime, x)))



# reservoir_status
unique(df_train$reservoir_status)




# structural_setting 
unique(df_train$structural_setting)

unique_structural_setting <- 
  str_split(df_train$structural_setting, pattern = '/') %>% 
  unlist() %>% 
  unique()
unique_structural_setting

df_train[str_to_lower(unique_structural_setting)] <-
  do.call(cbind, lapply(unique_structural_setting, function(x) str_detect(df_train$structural_setting, x)))

df_test[str_to_lower(unique_structural_setting)] <-
  do.call(cbind, lapply(unique_structural_setting, function(x) str_detect(df_test$structural_setting, x)))





# period
unique(df_train$period)

unique_period <- 
  str_split(df_train$period, pattern = '-') %>% 
  unlist() %>% 
  unique()
unique_period

df_train[str_to_lower(unique_period)] <-
  do.call(cbind, lapply(unique_period, function(x) str_detect(df_train$period, x)))

df_test[str_to_lower(unique_period)] <-
  do.call(cbind, lapply(unique_period, function(x) str_detect(df_test$period, x)))



# lithology
unique(df_train$lithology)





# Final Datasets ----------------------------------------------------------
df_train <- df_train %>% 
  select(-c(structural_setting, tectonic_regime, period)) %>% 
  # mutate_if(is.character, as.factor) %>% 
  clean_names()

df_test <- df_test %>% 
  select(-c(structural_setting, tectonic_regime, period)) %>% 
  # mutate(
  #   hydrocarbon_type = factor(hydrocarbon_type, levels = levels(df_train$hydrocarbon_type)),
  #   reservoir_status = factor(reservoir_status, levels = levels(df_train$reservoir_status)),
  #   lithology = factor(lithology, levels = levels(df_train$lithology))
  # ) %>% 
  clean_names()

df_train %>% 
  glimpse()



# Visualization -----------------------------------------------------------
df_train %>% 
  select(onshore_offshore, where(is.numeric)) %>% 
  mutate_if(
    is.numeric,
    ~ predict(bestNormalize::yeojohnson(.))
  ) %>% 
  pivot_longer(-onshore_offshore, names_to = 'variabel') %>% 
  ggplot(aes(x = onshore_offshore, y = value)) +
  geom_boxplot() +
  facet_wrap(~variabel, scales = 'free')






df_train %>% 
  select(!where(is.numeric), -onshore_offshore) %>% 
  apply(
    MARGIN = 2,
    FUN = function(x) rcompanion::cramerV(df_train$onshore_offshore, x, bias.correct = T)
  ) %>% 
  tidy() %>% 
  ggplot(aes(x = x, y = reorder(names, x), fill = x)) +
  geom_col() +
  theme_minimal()




# Train Test Split --------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, strata = onshore_offshore)
splits

train_set <- training(splits)
test_set  <- testing(splits)


set.seed(1)
val_set <- validation_split(train_set, 
                            strata = onshore_offshore, 
                            prop = 0.70)
val_set


# Simple Model ------------------------------------------------------------
mr_model <- 
  multinom_reg(penalty = tune(), mixture = 1) %>% 
  set_engine('nnet') 

# recipe
mr_recipe <- 
  recipe(onshore_offshore ~ ., data = train_set) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_upsample(onshore_offshore, over_ratio = 0.8) %>% 
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(all_numeric_predictors()) 

# workflow
mr_workflow <- 
  workflow() %>% 
  add_model(mr_model) %>% 
  add_recipe(mr_recipe)


mr_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))
mr_grid



# Training ----------------------------------------------------------------
mr_res <- 
  mr_workflow %>% 
  tune_grid(
    val_set,
    grid = mr_grid,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(f_meas)
  )

mr_res %>% 
  collect_metrics()
mr_res %>% 
  select_best()



# Best Model --------------------------------------------------------------
mr_best <- 
  mr_res %>% 
  select_best()
mr_best


mr_res %>% 
  collect_predictions(parameters = mr_best) %>% 
  f_meas(truth = onshore_offshore, .pred_class, estimator = 'macro')



# Final -------------------------------------------------------------------
final_wf <- mr_workflow %>% 
  finalize_workflow(mr_best)

final_wf

final_fit <- 
  final_wf %>%
  fit(df_train) 

final_fit

# Predict -----------------------------------------------------------------
hasil <- 
  df_sub %>% 
  mutate(
    `Onshore/Offshore` = predict(final_fit, new_data = df_test) %>% pull()
  )

head(hasil)

hasil %>% 
  write.csv('E:/sub.csv', row.names = F, quote = F)  
  

# kirim submission --------------------------------------------------------
system(
  'kaggle competitions submit -c lassification-type-of-oil-field -f E:/sub.csv -m "Message"'
)


# 0.94258





# -------------------------------------------------------------------------

df_train %>% 
  write.csv('E:/train.csv', row.names = F, quote = F)
df_test %>% 
  write.csv('E:/test.csv', row.names = df_sub$ID, quote = F)

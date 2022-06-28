library(tidyverse)
library(tidymodels)
library(janitor)
tidymodels::tidymodels_prefer()


# Data --------------------------------------------------------------------
loc <- 'D:/__Datasets/ml/delhihousepriceprediction'
list.files(loc)

df_train <- read.csv(str_glue('{loc}/DHP_Train_Revised.csv')) %>%
  clean_names() %>% 
  mutate(
    river_flg = as.factor(river_flg)
  )

df_test <- read.csv(str_glue('{loc}/DHP_Test.csv')) %>% 
  clean_names() %>% 
  rename(tax = tax_01) %>% 
  mutate_at(
    vars(river_flg), ~ as.factor(.x)
  )

df_sub <- read.csv(str_glue('{loc}/DHP_Sample.csv'))

glimpse(df_train)
glimpse(df_test)
glimpse(df_sub)

unique(df_train$rad)
unique(df_test$rad)
unique(df_train$river_flg)

# Metric ------------------------------------------------------------------
reg_metric <- metric_set(rsq, rmse, mape)


# Model awal --------------------------------------------------------------
m1 <- lm(medv ~ (.)^2 , data = df_train)
m1 <- stats::step(m1, trace = F)
summary(m1)
preds <- predict(m1, newdata = df_test)

rmse(df_train, truth = medv, estimate = predict(m1, newdata = df_train))



# EDA ---------------------------------------------------------------------
car::vif(m1)
ggplot(df_train, aes(x = log(medv))) + 
  geom_histogram(bins = 30, col = "white") 



# Train test split ---------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, prop = 0.8)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
val_set <- validation_split(
  train_set,  
  prop = 0.7
)
val_set

# Advance Model -----------------------------------------------------------
model_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine('ranger', num.threads = 4) %>%
  set_mode('regression')

model_spec <-
  boost_tree(tree_depth = tune(), trees = 308, learn_rate = 0.0226132275987791, min_n = tune(), loss_reduction = 7.50463627637477e-09, sample_size = 0.278397969997581, stop_iter = 17) %>%
  set_engine('xgboost') %>%
  set_mode('regression')



# Workflow ----------------------------------------------------------------
my_recipe <- 
  recipe(medv ~ ., data = train_set) %>% 
  # step_rm(id) %>% 
  step_mutate(river_flg = as.numeric(river_flg)) %>% 
  # update_role(Id, new_role = "id") %>% 
  step_YeoJohnson(all_numeric_predictors()) 
  

my_recipe %>% summary()

my_workflow <- 
  workflow() %>% 
  add_model(model_spec) %>% 
  add_recipe(my_recipe)

# Training ----------------------------------------------------------------
my_grid <- 
  crossing(
    mtry = 5:20,
    min_n = 5:7
  )

model_spec %>% extract_parameter_set_dials()


model_res <- 
  my_workflow %>% 
  tune_grid(
    val_set,
    grid = 500,
    control = control_grid(save_pred = TRUE, verbose = T, allow_par = T),
    metrics = reg_metric
  )

model_res$.notes[[1]]$note[1]

model_res %>% 
  collect_metrics() %>% 
  filter(.metric == 'rmse') %>% 
  arrange(mean) %>% 
  select(mean)


model_res %>% 
  collect_metrics() %>% 
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  arrange(rmse, desc(rsq))



model_res %>% 
  select_best()


# Model terbaik -----------------------------------------------------------
best_model <- model_res %>% 
  select_best(metric = 'rmse')

final_wf <- my_workflow %>% 
  finalize_workflow(best_model)

final_wf

# fit terakhir
final_fit <- final_wf %>% fit(df_train) 
final_fit


# Evaluasi ----------------------------------------------------------------
df_train %>% 
  bind_cols(predict(final_fit, .)) %>% 
  reg_metric(truth = medv, estimate = .pred)

test_set %>% 
  bind_cols(predict(final_fit, .)) %>% 
  reg_metric(truth = medv, estimate = .pred)


# Prediksi ----------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds

hasil <- df_test %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  select(Id = id, MEDV = .pred) 
  mutate(MEDV = exp(MEDV))
head(hasil)

preds <- 1 / log10(preds)


# Submission --------------------------------------------------------------
hasil <- df_sub %>% 
  mutate(MEDV = preds)

write.csv(hasil, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  

system(
  'kaggle competitions submit -c delhihousepriceprediction -f D:/__Datasets/sub.csv -m "Message"'
)


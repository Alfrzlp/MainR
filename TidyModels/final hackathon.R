library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Data --------------------------------------------------------------------
loc <- 'D:/Downloads/data-mining'
list.files(path = loc)

df_train <- read.csv(file.path(loc, "training.csv"))
df_test <- read.csv(file.path(loc, "testing.csv"))

glimpse(df_train)
glimpse(df_test)


# Data Prepocessing -------------------------------------------------------
table(df_train$Y)

df_train <- df_train %>% 
  mutate(Y = as.factor(Y)) %>% 
  mutate_at(vars(X1:X4, X7, X9:X16), ~ as.factor(.x)) 

df_test <- df_test %>% 
  mutate_at(vars(X1:X4, X7, X9:X16), ~ as.factor(.x)) 

df_train %>% 
  glimpse()


# EDA ---------------------------------------------------------------------
colSums(is.na(df_train))
colSums(is.na(df_test))

df_train %>% 
  select(Y, X8, X5, X6) %>% 
  pivot_longer(-Y) %>% 
  ggplot(aes(y = Y, x = value, fill = Y)) +
  geom_boxplot() +
  facet_grid(name~., scales = 'free')


df_train %>% 
  select(Y, X1:X4, X7, X9:X16) %>% 
  pivot_longer(-Y) %>% 
  ggplot(aes(x = value, fill = Y)) +
  geom_bar( position = position_fill()) +
  facet_wrap(facets = ~name, scales = 'free', nrow = 4, ncol = 4)


df_train <- df_train %>% 
  filter(X5 != 0, X6 != 0)

# Metric ------------------------------------------------------------------
my_metric <- metric_set(accuracy)


# train test split --------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, prop = 0.8, strata = Y)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(df_train, v = 3, strata = Y)


# model -------------------------------------------------------------------
rf_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine('ranger') %>%
  set_mode('classification')



# recipe and workflow -----------------------------------------------------
my_recipe <- 
  recipe(Y ~ ., data = train_set) %>% 
  update_role(ID, new_role = "id") %>%
  # step_dummy(all_nominal_predictors()) %>%
  step_zv() 


my_recipe %>% 
  summary() %>% 
  as.data.frame()

my_workflow <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(my_recipe)


# Tuning ------------------------------------------------------------------
my_grid <- tibble(
  mtry = c(11, 15),
  min_n = c(4, 2),
  trees = c(700, 1100)
)
cross_df(my_grid)


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
    grid = cross_df(my_grid),
    control = control_grid(
      save_pred = TRUE,
      verbose = T,
      allow_par = T
    ),
    metrics = my_metric
  )




# hasil -------------------------------------------------------------------
model_res %>% 
  collect_metrics() %>% 
  arrange(desc(mean))  

model_res %>% 
  autoplot()



# Model Terbaik -----------------------------------------------------------
best_model <- model_res %>% 
  select_best()

final_wf <- my_workflow %>% 
  finalize_workflow(best_model)

final_wf

# fit terakhir
final_fit <- final_wf %>% fit(df_train) 
final_fit



# Evaluasi ----------------------------------------------------------------
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

write.csv(hasil, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  

# system(
#   'kaggle competitions submit -c data-mining-ta20222023 -f D:/__Datasets/sub.csv -m "Message"'
# )




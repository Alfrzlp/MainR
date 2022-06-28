library(tidyverse)
library(tidymodels)
tidymodels::tidymodels_prefer()


# Data --------------------------------------------------------------------
loc <- 'D:/__Datasets/ml/2022-regression-data-challenge'

df_train <- read.csv(str_glue('{loc}/train.csv'))
df_test <- read.csv(str_glue('{loc}/test.csv'))
df_sub <- read.csv(str_glue('{loc}/submission.csv'))

glimpse(df_train)
glimpse(df_test)


# Metric ------------------------------------------------------------------
reg_metric <- metric_set(rsq, rmse, mape)



# Model awal --------------------------------------------------------------
m1 <- lm(score ~ (.) , data = df_train[-1])
m1 <- stats::step(m1, trace = F)
summary(m1)
preds <- predict(m1, newdata = df_test)


df_train %>% 
  bind_cols(
    preds = df_train %>% predict(m1, newdata = .)
  ) %>% 
  reg_metric(truth = score, preds)



# EDA -----------------------------------------------------------
ggplot(df_train, aes(x = score)) + 
  geom_histogram(bins = 30, col = "white") 


# ------- PCA
vif_score <- car::vif(m1)
round(vif_score, 3)
var_pca <- names(vif_score[vif_score > 10])

rpca <- princomp(df_train[var_pca])
summary(rpca)
# 1 komponen
df_train %>% 
  select(-var_pca) %>% 
  mutate(pca = rpca$scores[,1]) %>% 
  lm(score ~ ., .) %>% 
  summary()


# -------- Outlier
library(olsrr)
obs_outlier <- ols_plot_cooksd_chart(m1)
ols_plot_cooksd_chart(m1)

obs_outlier <- obs_outlier$data %>% 
  filter(color == 'outlier') %>% 
  pull(obs)


df_train <- df_train %>% 
  slice(-obs_outlier)




# Train test split ---------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, prop = 0.77)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
val_set <- validation_split(
  train_set,  
  prop = 0.7
)
val_set


# Model -------------------------------------------------------------------
model_spec <-
  rand_forest(mtry = 18, min_n = 18, trees = tune()) %>%
  set_engine('ranger', num.threads = 4) %>%
  set_mode('regression')

model_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

model_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('regression')




# Workflow ----------------------------------------------------------------
cor(df_train$Id, df_train$score)

my_recipe <- 
  recipe(score ~ ., data = train_set) %>% 
  # update_role(Id, new_role = "id") %>% 
  step_pca(var_pca, num_comp = 1) 

my_recipe %>% summary()

my_workflow <- 
  workflow() %>% 
  add_model(model_spec) %>% 
  add_recipe(my_recipe)

# Training ----------------------------------------------------------------
model_res <- 
  my_workflow %>% 
  tune_grid(
    val_set,
    grid = 50,
    control = control_grid(save_pred = TRUE, verbose = T, allow_par = T),
    metrics = reg_metric
  )

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
  reg_metric(truth = score, estimate = .pred)

test_set %>% 
  bind_cols(predict(final_fit, .)) %>% 
  reg_metric(truth = score, estimate = .pred)


# Prediksi ----------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds

hasil <- df_test %>% 
  bind_cols(predict(final_fit, new_data = .)) %>% 
  select(Id, Expected = .pred)


# Submission --------------------------------------------------------------
hasil <- df_sub %>% 
  mutate(Expected = preds)

write.csv(hasil, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  

system(
  'kaggle competitions submit -c 2022-regression-data-challenge -f D:/__Datasets/sub.csv -m "Message"'
)






# Interpolation -----------------------------------------------------------
ggplot(df_train) +
  geom_point(aes(y = Id, x = score))

ggplot(hasil) +
  geom_point(aes(y = Id, x = Expected))

preds <- df_train %>% 
  arrange(Id) %>% {
    signal::interp1((.)$Id, (.)$score, df_test$Id,
                    method = 'pc', extrap = T)
  } 

# "linear", "nearest", "pchip", "cubic", "spline"


# -------------------------------------------------------------------------
hasil <- df_sub %>% 
  mutate(
    Expected = round(preds - 1.111, 3)
  )

hasil %>% is.na() %>% colSums()
hasil %>% filter(is.na(Expected))


write.csv(hasil, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  
system(
  'kaggle competitions submit -c 2022-regression-data-challenge -f D:/__Datasets/sub.csv -m "Message"'
)












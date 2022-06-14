library(h2o)
h2o.init()


# resep -------------------------------------------------------------------
my_recp <- recipe(
  class_asd ~ a1_score + a2_score + a3_score + a4_score + a5_score + a6_score + a7_score + a8_score + a9_score + a10_score + age + result + gender + test_vs_result,
  data = train_set) %>% 
  # add_role(id, new_role = "id") %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>% 
  # step_interact(terms = ~ gender:score) %>% 
  # step_other(contry_of_res, threshold = 0.2) %>% 
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>% 
  themis::step_smote(class_asd, over_ratio = 0.9, seed = 1, column = -all_nominal())


# data --------------------------------------------------------------------
data_train <- prep(my_recp) %>% 
  bake(train_set) %>% 
  as.h2o()

data_val <- prep(my_recp) %>% 
  bake(test_set) %>% 
  as.h2o()

data_sub <- prep(my_recp) %>% 
  bake(df_test) %>% 
  as.h2o()


y <- "class_asd"
x <- setdiff(names(data_train), y)
x <- x[x != 'id']
x



# Model -------------------------------------------------------------------
h2oAML <- h2o.automl(
  y = y,
  x = x,
  training_frame = data_train,
  validation_frame = data_val,
  project_name = "autism_h20_2",
  balance_classes = F,
  max_runtime_secs = 600,
  seed = 1
)

leaderboard_tbl <- h2oAML@leaderboard %>% as_tibble()
leaderboard_tbl %>% 
  arrange(mean_per_class_error)

id_topmodel <- leaderboard_tbl %>% 
  arrange(auc) %>% 
  top_n(1) %>% 
  pull(model_id)


top_model <- h2o.getModel(id_topmodel)

top_model@model$model_summary %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "Value"
  )




# Predict -----------------------------------------------------------------
h2o_predictions <- 
  h2o.predict(top_model, newdata = data_val) %>%
  as_tibble() %>%
  bind_cols(test_set)

my_metric <- metric_set(f_meas, sensitivity, accuracy, kap)

h2o_predictions %>% 
  my_metric(truth = class_asd, estimate = predict)

h2o_predictions %>% 
  conf_mat(truth = class_asd, estimate = predict)

# Predict -----------------------------------------------------------------
pred <- h2o.predict(top_model, newdata = data_sub) %>% 
  as_tibble() 
pred
table(pred$predict)


hasil <- 
  df_sub %>% 
  mutate(
    `Class/ASD` = pred$predict
  )

head(hasil)

hasil %>% 
  write.csv('E:/sub.csv', row.names = F, quote = F)  


# kirim submission --------------------------------------------------------
system(
  'kaggle competitions submit -c autismdiagnosis -f E:/sub.csv -m "Message"'
)

library(tidymodels)
library(tidyverse)
library(janitor)

tidymodels_prefer()

# Data' -------------------------------------------------------------------
df_train <- 
  read_csv('D:/_Datasets/ristekds2021modeling/flight.csv', show_col_types = F) %>% 
  clean_names()

df_test <- 
  read_csv('D:/_Datasets/ristekds2021modeling/test.csv', show_col_types = F) %>% 
  clean_names()



# jika hotel_id bernilai "None" maka tidak terjadi cross sell.
# Sedangkan bila nilai hotel_id selain itu maka terjadi cross sell
df_train <- df_train %>% 
  mutate(
    cross_sell = if_else(hotel_id == 'None', 0, 1),
    cross_sell = factor(cross_sell, levels = 0:1)
  )



glimpse(df_train)
glimpse(df_test)


# Informasi Data ----------------------------------------------------------
dim(df_train)

# class
df_train %>% 
  group_by(hotel_id) %>% 
  count()


# banyaknya kategori di setiap variabel
n_unique <- 
  df_train %>% 
  summarise_all(~ length(unique(.x))) %>% 
  pivot_longer(everything(), values_to = 'n', names_to = 'variabel')

n_unique


# melihat kategori apa aja itu
x <- n_unique %>% 
  filter(n < 11, variabel != 'cross_sell') %>% 
  pull(variabel)

x 

df_train[x] %>% apply(2, FUN = function(x) unique(x))
df_test[x] %>% apply(2, FUN = function(x) unique(x))

# gender None
df_train %>% 
  count(gender == 'None')
df_test %>% 
  count(gender == 'None')



# step yang akan dilakukan 
# - hapus kolom route (karena cuma 1 nilai)
# - imputasi gender == 'None'





# EDA ---------------------------------------------------------------------
ggplot(df_train, aes(x = 0, fill = airlines_name)) +
  geom_bar() +
  scale_fill_viridis_d() +
  scale_x_continuous(labels = NULL) +
  facet_grid(cross_sell~visited_city, scales = 'free')


ggplot(df_train, aes(y = visited_city, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 

ggplot(df_train, aes(y = airlines_name, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 

ggplot(df_train, aes(y = is_tx_promo, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 

ggplot(df_train, aes(y = trip, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 

ggplot(df_train, aes(y = no_of_seats, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 

ggplot(df_train, aes(y = 0, fill = cross_sell)) +
  geom_bar(position = position_dodge()) +
  scale_y_continuous(labels = NULL) +
  facet_grid(~service_class, scales = 'free')

ggplot(df_train, aes(x = member_duration_days, fill = cross_sell)) +
  geom_histogram(bins = 30) +
  facet_grid(cross_sell~., scales = 'free')

ggplot(df_train, aes(x = cross_sell, y = member_duration_days, fill = cross_sell)) +
  geom_boxplot()

ggplot(df_train, aes(x = cross_sell, y = price, fill = cross_sell)) +
  geom_boxplot()



df_train %>% 
  filter(gender != 'None') %>% 
  ggplot(aes(y = gender, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 

# pernah booking hotel
df_train %>% 
  filter(cross_sell == 1) %>% 
  group_by(account_id) %>% 
  count(cross_sell) %>% 
  mutate(
    cross_sell = as.numeric(levels(cross_sell))[cross_sell],
    p = cross_sell/n
  ) %>% 
  filter(cross_sell < n) %>% 
  arrange(desc(p)) %>% 
  filter(p >= 0.3) %>% 
  ungroup()

id_pernah_booking <- df_train %>% 
  filter(cross_sell == 1) %>% 
  group_by(account_id) %>% 
  count(cross_sell) %>% 
  mutate(cross_sell = as.numeric(cross_sell) - 1) %>% 
  filter(cross_sell < n) %>% 
  pull(account_id)



# Pasti booking
id_pasti_booking <- df_train %>% 
  filter(cross_sell == 1) %>% 
  group_by(account_id) %>% 
  count(cross_sell) %>% 
  mutate(cross_sell = as.numeric(cross_sell) - 1) %>% 
  filter(cross_sell >= n) %>% 
  pull(account_id)


biasa_booking %>% 
  ggplot(aes(y = n, fill = cross_sell)) +
  geom_histogram() +
  facet_grid(~cross_sell, scales = 'free')

length(unique(df_train$account_id))
length(unique(c(df_train$account_id, df_test$account_id)))






# final df -------------------------------------------------------------------
id_pasti_booking <- df_train %>% 
  mutate(cross_sell = as.numeric(levels(cross_sell))[cross_sell]) %>% 
  group_by(account_id) %>% 
  summarise(cross_sell = sum(cross_sell), n = n()) %>% 
  ungroup() %>% 
  filter(
    cross_sell > 0,
    cross_sell >= n
  ) %>% 
  arrange(desc(cross_sell)) %>% 
  pull(account_id)

length(id_pasti_booking)

df_booking <- df_train %>% 
  filter(
    account_id %in% id_sering_booking
  )






# Train Test Split --------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, strata = cross_sell)

splits <- initial_split(df_booking, strata = cross_sell)
splits

train_set <- training(splits)
test_set  <- testing(splits)


set.seed(1)
val_set <- validation_split(
  train_set, 
  strata = cross_sell, 
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
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine('ranger', num.threads = cores) %>% 
  set_mode('classification')

rf_model <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')


glimpse(df_train)

# recipe
rf_recipe <- 
  recipe(
    cross_sell ~ trip + member_duration_days + service_class + is_tx_promo + no_of_seats + visited_city + gender,
    data = train_set) %>% 
  step_string2factor(all_nominal_predictors()) %>% 
  step_impute_knn(gender) %>%
  themis::step_upsample(cross_sell, over_ratio = 0.85, target = 1) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>% 
  step_zv(all_predictors()) 
  step_YeoJohnson(c(member_duration_days))


# workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rf_recipe)


lr_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 90))
lr_grid

# Training ----------------------------------------------------------------
rf_res <- 
  rf_workflow %>% 
  tune_grid(
    val_set,
    grid = 50,
    control = control_grid(save_pred = TRUE, verbose = T),
    metrics = metric_set(mcc)
  )


see_notes <- function(res){
  res$.notes[[1]]$note
}
see_notes(rf_res)



rf_res %>% 
  collect_metrics() %>% 
  select(mean) %>% 
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
  fit(df_booking) 

final_fit



# Hasil -------------------------------------------------------------------
cross_sell_metric <- metric_set(f_meas, sensitivity, mcc)

train_pred <- df_booking %>% 
  bind_cols(predict(final_fit, .))

train_pred %>% 
  cross_sell_metric(truth = cross_sell, estimate = .pred_class)

train_pred %>% 
  conf_mat(truth = cross_sell, estimate = .pred_class)



test_pred <- test_set %>% 
  bind_cols(predict(final_fit, .))

test_pred %>% 
  cross_sell_metric(truth = cross_sell, estimate = .pred_class)

test_pred %>% 
  conf_mat(truth = cross_sell, estimate = .pred_class)









# Explore Log transaction -------------------------------------------------
df_train %>% 
  filter(cross_sell == 1) %>% 
  slice(1:10) %>% 
  pull(log_transaction) %>% 
  lapply(
    FUN = function(x) {
      str_extract_all(x, '[0-9]+.[0-9]*')[[1]]
    }
  )


df_train %>% 
  filter(cross_sell == 0) %>% 
  slice(1:10) %>% 
  pull(log_transaction) %>% 
  lapply(
    FUN = function(x) {
      str_extract_all(x, '[0-9]+.[0-9]*')[[1]]
    }
  )

options(digits = 10)
x <- str_extract_all(df_train$log_transaction[4], '[0-9]+.[0-9]*')[[1]]
as.double(x)

library(tidyverse)
library(tidymodels)
# library(bonsai)
library(janitor)
library(themis)
library(inspectdf)
tidymodels_prefer()


# Data ---------------------------------------------------------------------
loc <- 'D:/__Datasets/ml/laptop-price-july'
list.files(loc)

df_train <- read.csv(file.path(loc, 'train.csv'), row.names = 'id') %>% 
  clean_names()

df_test <- read.csv(file.path(loc, 'test.csv'), row.names = 'id') %>% 
  clean_names() 

df_sub <- read.csv(file.path(loc, 'sample_submission.csv'))

glimpse(df_train)
glimpse(df_test)
glimpse(df_sub)



# EDA ---------------------------------------------------------------------
# NA
inspect_na(df_train, df_test)

# kategorik
cat_train <- inspect_cat(df_train)
cat_test <- inspect_cat(df_test)

cat_train
cat_test

# numerik
inspect_num(df_train, df_test)


# Preprocessing -----------------------------------------------------------

# company
cat_train$levels$company
cat_test$levels$company


# cpu ---------------------------------------------------------------------
cat_train$levels$cpu$value
cat_test$levels$cpu$value


df_train <- df_train %>% 
  extract(
    col = 'cpu', into = c('cpu_company', 'generation', 'speed'),
    regex = '(Intel|Samsung|AMD)\\s(.*)\\s(\\d\\.*\\d*)GHz$'
  ) %>% 
  mutate(
    cpu = str_extract(generation, "A\\d+(?=-S)|i\\d+|\\sM\\s|E-Series|Ryzen|FX|Xeon|Atom|Celeron|Pentium"),
    gen = str_extract(generation, "(?<=\\s)[[:alnum:]]+-*[[:alnum:]]+$"),
    gen = ifelse(gen %in% c('i5', 'i7'), NA, gen),
    type_cpu = str_extract(gen, pattern = '[[:alpha:]]+$'),
    gen = case_when(
      cpu_company == 'AMD' ~ ifelse(
        grepl("[[:alpha:]]",substr(gen, 1, 1)),
        substr(gen, 5, 5),
        substr(gen, 1, 1)),
      cpu_company == 'Intel' & grepl(pattern = 'i\\d+', cpu) ~ substr(gen, 1, 1),
      TRUE ~ '0'
    ),
    gen = ifelse(is.na(gen) & !grepl('i\\d+', cpu), 0, gen),
    type_cpu = case_when(
      cpu_company == 'AMD' ~ 'AMD',
      cpu %in% c('Pentium', 'Celeron') ~ str_extract(generation, 'Dual|Quad'),
      is.na(type_cpu) ~ 'Other',
      TRUE ~ type_cpu
    ),
    cpu = ifelse(is.na(cpu), generation, cpu)
  ) %>% 
  select(-generation) %>%
  type_convert() %>% 
  as_tibble()


df_test <- df_test %>% 
  extract(
    col = 'cpu', into = c('cpu_company', 'generation', 'speed'),
    regex = '(Intel|Samsung|AMD)\\s(.*)\\s(\\d\\.*\\d*)GHz$'
  ) %>% 
  mutate(
    cpu = str_extract(generation, "A\\d+(?=-S)|i\\d+|\\sM\\s|E-Series|Ryzen|FX|Xeon|Atom|Celeron|Pentium"),
    gen = str_extract(generation, "(?<=\\s)[[:alnum:]]+-*[[:alnum:]]+$"),
    gen = ifelse(gen %in% c('i5', 'i7'), NA, gen),
    type_cpu = str_extract(gen, pattern = '[[:alpha:]]+$'),
    gen = case_when(
      cpu_company == 'AMD' ~ ifelse(
        grepl("[[:alpha:]]",substr(gen, 1, 1)),
        substr(gen, 5, 5),
        substr(gen, 1, 1)),
      cpu_company == 'Intel' & grepl(pattern = 'i\\d+', cpu) ~ substr(gen, 1, 1),
      TRUE ~ '0'
    ),
    gen = ifelse(is.na(gen) & !grepl('i\\d+', cpu), 0, gen),
    type_cpu = case_when(
      cpu_company == 'AMD' ~ 'AMD',
      cpu %in% c('Pentium', 'Celeron') ~ str_extract(generation, 'Dual|Quad'),
      is.na(type_cpu) ~ 'Other',
      TRUE ~ type_cpu
    ),
    cpu = ifelse(is.na(cpu), generation, cpu)
  ) %>% 
  select(-generation) %>%
  type_convert() %>% 
  as_tibble() 
  


glimpse(df_train)


# layar -------------------------------------------------------------------
df_train <- df_train %>% 
  mutate(
    ts = grepl('Touchscreen', screen_resolution),
    height = str_extract(screen_resolution, '\\d+(?=x)'),
    width = str_extract(screen_resolution, '(?<=x)\\d+'),
    ips = grepl('IPS', screen_resolution),
    hd = str_extract(screen_resolution, 'Quad HD|Full HD|Ultra HD'),
    hd = ifelse(is.na(hd), 'common', hd),
    retina = grepl('Retina', screen_resolution),
    .keep = 'unused'
  ) 

df_test <- df_test %>% 
  mutate(
    ts = grepl('Touchscreen', screen_resolution),
    height = str_extract(screen_resolution, '\\d+(?=x)'),
    width = str_extract(screen_resolution, '(?<=x)\\d+'),
    ips = grepl('IPS', screen_resolution),
    hd = str_extract(screen_resolution, 'Quad HD|Full HD|Ultra HD'),
    hd = ifelse(is.na(hd), 'common', hd),
    retina = grepl('Retina', screen_resolution),
    .keep = 'unused'
  ) 



glimpse(df_train)


# Ram ---------------------------------------------------------------------
unique(df_train$memory)

df_train <- df_train %>% 
  mutate(
    ram = str_remove_all(ram, 'GB'),
    hdd = str_extract(memory, pattern = '\\d+\\.*\\d*(GB|TB)(?=\\sHDD)'),
    ssd = str_extract(memory, pattern = '\\d+\\.*\\d*(GB|TB)(?=\\sSSD)'),
    sshd = str_extract(memory, pattern = '\\d+\\.*\\d*(GB|TB)(?=\\sHybrid)'),
    fs = str_extract(memory, pattern = '\\d+\\.*\\d*(GB|TB)(?=\\sFlash Storage)'),
    weight = str_remove(weight, 'kg'),
    across(c(hdd, ssd, sshd, fs), ~ str_remove_all(.x, 'GB')),
    across(c(hdd, ssd, sshd, fs), ~ str_replace_all(.x, '\\.0TB|TB', '000')),
    across(c(ssd, fs), ~ ifelse(is.na(.x), 0, .x)),
    ssd = ifelse(memory == '512GB SSD +  512GB SSD', 1028, ssd),
    sshd = ifelse(!is.na(hdd) & is.na(sshd), 0, sshd),
    hdd = ifelse(!is.na(sshd) & is.na(hdd), 0, hdd)
  ) %>% 
  select(-memory) %>%
  type_convert() 

df_test <- df_test %>% 
  mutate(
    ram = str_remove_all(ram, 'GB'),
    hdd = str_extract(memory, pattern = '\\d+\\.*\\d*(GB|TB)(?=\\sHDD)'),
    ssd = str_extract(memory, pattern = '\\d+\\.*\\d*(GB|TB)(?=\\sSSD)'),
    sshd = str_extract(memory, pattern = '\\d+\\.*\\d*(GB|TB)(?=\\sHybrid)'),
    fs = str_extract(memory, pattern = '\\d+\\.*\\d*(GB|TB)(?=\\sFlash Storage)'),
    weight = str_remove(weight, 'kg'),
    across(c(hdd, ssd, sshd, fs), ~ str_remove_all(.x, 'GB')),
    across(c(hdd, ssd, sshd, fs), ~ str_replace_all(.x, '\\.0TB|TB', '000')),
    across(c(ssd, fs), ~ ifelse(is.na(.x), 0, .x)),
    ssd = ifelse(memory == '512GB SSD +  512GB SSD', 1028, ssd),
    sshd = ifelse(!is.na(hdd) & is.na(sshd), 0, sshd),
    hdd = ifelse(!is.na(sshd) & is.na(hdd), 0, hdd)
  ) %>% 
  select(-memory) %>%
  type_convert() 
  

filter(is.na(hdd) & is.na(sshd)) %>% 
  select(product, hdd, sshd, ssd, fs) 
  as.data.frame()

  
# GPU ---------------------------------------------------------------------
unique(df_train$gpu)

df_train <- df_train %>% 
  mutate(
    gpu_type = str_extract(gpu, 'Intel Iris|Nvidia GeForce|AMD Radeon|Intel HD Graphics|Nvidia Quadro|ARM Mali|AMD R4|AMD FirePro'),
    gpu_type = ifelse(is.na(gpu_type), 'Other', gpu_type)
  ) 
  pull(gpu_type) 
  unique()
  
df_test <- df_test %>% 
  mutate(
    gpu_type = str_extract(gpu, 'Intel Iris|Nvidia GeForce|AMD Radeon|Intel HD Graphics|Nvidia Quadro|ARM Mali|AMD R4|AMD FirePro'),
    gpu_type = ifelse(is.na(gpu_type), 'Other', gpu_type)
  ) 

glimpse(df_train)
unique(df_train$product)


# Split -------------------------------------------------------------------
set.seed(1)
splits <- initial_split(df_train, prop = 0.8)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(train_set, v = 5)



# model -------------------------------------------------------------------
xgb_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

linear_reg_spec <-
  linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine('glmnet')

mlp_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('regression')

rf_spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine('ranger') %>%
  set_mode('regression')



# recipe and workflow -----------------------------------------------------
glimpse(df_train)
colSums(is.na(df_train))

my_recipe <- 
  recipe(price_euros ~ ., data = train_set) %>% 
  step_rm(c(product)) %>% 
  step_mutate(
    across(c(ips, ts, retina), ~ as.numeric(.x))
  ) %>% 
  step_impute_knn(c(hdd, sshd, gen)) %>% 
  step_dummy(c(company, type_name, cpu_company, op_sys, hd, gpu, cpu, type_cpu)) 


my_workflow <- 
  workflow_set(
    preproc = list(my_recipe), 
    models = list(
      linear_reg = linear_reg_spec,
      rf = rf_spec,
      xgb = xgb_spec, 
      nnet = mlp_spec
    )
  )


# tuning ------------------------------------------------------------------
grid_ctrl <-
  control_grid(
    # parallel_over = "resampling",
    save_pred = TRUE,
    verbose = T,
    allow_par = T
  )


library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

grid_results <-
  my_workflow %>%
  workflow_map(
    seed = 1,
    resamples = train_fold,
    grid = 50,
    control = grid_ctrl,
    metrics = metric_set(mae),
    verbose = TRUE
  )

grid_results$result[[1]]$.notes[[1]]$note
collect_notes(grid_results)


# Hasil -------------------------------------------------------------------
grid_results %>% 
  filter(wflow_id != 'recipe_rf') %>% 
  rank_results() %>% 
  filter(.metric == "mae") 
  select(model, .config, mae = mean, rank)



autoplot(
  grid_results,
  rank_metric = "kap",  # <- how to order models
  metric = "kap",       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
) +
  geom_text(
    aes(y = min(mean), label = str_remove_all(wflow_id, 'recipe_')),
    angle = 90, hjust = 1, nudge_y = - 0.02
  ) +
  theme(legend.position = "none")


autoplot(
  grid_results,
  id = "recipe_svm_rbf",
  metric = "kap"
)



# finalizing model --------------------------------------------------------
best_model <- 'recipe_xgb'

best_results <- 
  grid_results %>% 
  extract_workflow_set_result(best_model) %>% 
  select_best(metric = "mae")

best_results <- grid_results %>% 
  extract_workflow_set_result(best_model) %>% 
  collect_metrics() %>% 
  arrange(mean, std_err) %>% 
  slice(2)
  select(mean, std_err)

best_results

final_wf <- 
  grid_results %>% 
  extract_workflow(best_model) %>% 
  finalize_workflow(best_results)

final_wf

final_fit <- final_wf %>% fit(df_train) 
final_fit


# Prediction --------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds

df_sub$Price_euros <- preds - 10.000

# Submission --------------------------------------------------------------
write.csv(df_sub, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  

system(
  'kaggle competitions submit -c laptop-price-july -f D:/__Datasets/sub.csv -m "Message"'
)














xgb1 <- final_fit

# Gradien Boosting --------------------------------------------------------
glimpse(df_train)

library(embed)
my_recipe <- 
  recipe(price_euros ~ ., data = train_set) %>% 
  step_rm(c(product)) %>% 
  step_mutate(
    across(c(ips, ts, retina), ~ as.numeric(.x))
  ) %>% 
  step_impute_linear(c(hdd, sshd, gen)) %>% 
  step_lencode_mixed(type_cpu, outcome = vars(price_euros)) %>% 
  step_dummy(c(company, type_name, cpu_company, op_sys, hd, gpu, cpu, type_cpu)) 


my_workflow <- 
  workflow() %>% 
  add_model(xgb_spec) %>% 
  add_recipe(my_recipe)


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
    grid = 50,
    control = control_grid(
      save_pred = TRUE,
      verbose = T,
      allow_par = T
    ),
    metrics = metric_set(mae)
  )

collect_notes(model_res)$note

model_res %>% 
  collect_metrics() %>% 
  filter(.metric == 'mae') %>% 
  arrange(mean) %>% 
  relocate(c(mean, std_err), .before = trees)




ctrl <- control_bayes(verbose = TRUE)
model_param <- my_workflow %>% 
  extract_parameter_set_dials()

set.seed(1)
model_bo <-
  my_workflow %>%
  tune_bayes(
    resamples = train_fold,
    metrics = metric_set(mae),
    initial = model_res,
    param_info = model_param,
    iter = 50,
    control = ctrl
  )

# finalizing model --------------------------------------------------------
best_results <- 
  model_res %>% 
  select_best('mae')

best_results

final_wf <- 
  my_workflow %>% 
  finalize_workflow(best_results)

final_wf

final_fit <- final_wf %>% fit(df_train) 
final_fit


# Prediction --------------------------------------------------------------
preds <- predict(final_fit, new_data = df_test) %>% pull()
preds

df_sub$Price_euros <- preds - 10.000

# Submission --------------------------------------------------------------
write.csv(df_sub, 'D:/__Datasets/sub.csv', row.names = F, quote = F)  

system(
  'kaggle competitions submit -c laptop-price-july -f D:/__Datasets/sub.csv -m "Message"'
)



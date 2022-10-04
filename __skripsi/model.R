library(tidyverse)
library(tidymodels)
tidymodels_prefer()


# Data --------------------------------------------------------------------
df_train <- read.csv('D:/__Skripsi/jatim_ubinan4_V2.csv')

df_train <- df_train %>% 
  select(-system.index, -.geo) %>% 
  janitor::clean_names() %>% 
  select(
    id, jenis, strata, y, yk, b1, b2, b3, b4, b5, b6, 
    b7, b8, b8a, b9, b11, b12, 
    evi, msavi2, ndbi, ndvi, savi
  )

glimpse(df_train)

# b11 swir
# b8 nir

# soil adjected factor
L = 0.5
df_train <- df_train %>% 
  mutate(
    # savi = (nir - red)*(1 + L) / (nir + red + L),
    # lai = - log((0.69 - savi)/0.59)/0.9,
    # p = -3.11 + 1.684 * lai + 12.458 * ndvi
    # prv = 38.46154 * log(evi / 0.103)
    ndwi = (b8 - b11)/(b8 + b11),
    ndre = (b8 - b8a)/(b8 + b8a),
    gndvi = (b8 - b3)/(b8 + b3)
  ) 


# Model Awal --------------------------------------------------------------
glimpse(df_train)
m1 <- lm(
  yk ~ ndvi + savi + msavi2 + ndwi + evi + ndre + gndvi,
  data = df_train
)
summary(m1)
m1 <- stats::step(m1)

df_train %>% 
  yardstick::rmse(yk, predict(m1))


# Metric ------------------------------------------------------------------
reg_metric <- metric_set(rmse)



# EDA -----------------------------------------------------------
glimpse(df_train)
bands <- c('b1','b2','b3','b4','b5','b6','b7','b8', 'b8a', 'b9', 'b11', 'b12')
allvar <- c(bands, "evi", "msavi2", "ndbi", "ndvi", "savi", "ndwi", "gndvi", "ndre")

df_train %>% 
  select(-c(y, strata, jenis)) %>% 
  pivot_longer(-c(id, yk), names_to = 'variabel') %>% 
  # pull(variabel) %>% 
  # unique()
  mutate(
    indeks = ifelse(variabel %in% bands, 'bukan', 'ya'),
    variabel = factor(variabel, levels = allvar)
  ) %>% 
  ggplot(aes(x = variabel, y = value, fill = variabel)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = str_to_upper
  ) +
  ggh4x::facet_grid2(
    ~indeks, scales = "free",
    independent = "y",
    labeller = as_labeller(
      c('bukan'='Band',
        'ya'='Indeks Komposit')
    )
  ) +
  theme(
    legend.position = 'none'
  )
  

df_cor <- df_train %>% 
  select(-c(id, strata, jenis)) %>% 
  cor() 
df_cor

df_cor[, 1] 
ggcorrplot::ggcorrplot(df_cor)



# ------- PCA
vif_score <- car::vif(m1)
round(vif_score, 3)


# -------- Outlier
library(olsrr)
obs_outlier <- ols_plot_cooksd_chart(m1)
ols_plot_cooksd_chart(m1)

obs_outlier <- obs_outlier$data %>% 
  filter(color == 'outlier') %>% 
  pull(obs)
obs_outlier

df_train <- df_train %>% 
  slice(-obs_outlier)


df_train <- df_train %>% 
  filter(!id %in% id_jelek)


# Train test split ---------------------------------------------------------
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
  set_engine('ranger', num.threads = 4) %>%
  set_mode('regression')

xgb_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost', num.threads = 4) %>%
  set_mode('regression')

mlp_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('regression')



# Workflow ----------------------------------------------------------------
table(df_train$strata)

my_recipe <- 
  recipe(yk ~ ndvi + savi + msavi2 + ndwi + evi + ndre + gndvi + ndbi + id, data = train_set) %>% 
  update_role(id, new_role = "id") 


my_recipe %>% summary()

my_workflow <- 
  workflow_set(
    preproc = list(my_recipe), 
    models = list(
      rf = rf_spec,
      xgb = xgb_spec, 
      nnet = mlp_spec
    )
  )


# Training ----------------------------------------------------------------
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
    metrics = reg_metric,
    verbose = TRUE
  )


# Hasil -------------------------------------------------------------------
grid_results %>% 
  rank_results() %>% 
  filter(.metric == "rmse") %>% 
  select(model, .config, rmse = mean, rank) 



autoplot(
  grid_results,
  rank_metric = "rmse",  # <- how to order models
  # which metric to visualize
  metric = "rmse",    
  select_best = TRUE     # <- one point per workflow
) 
  geom_text(
    aes(y = min(mean), label = str_remove_all(wflow_id, 'recipe_')),
    angle = 90, hjust = 1, nudge_y = - 0.02
  ) +
  theme(legend.position = "none")


autoplot(
  grid_results,
  id = "recipe_rf",
  metric = "rmse"
)



# finalizing model --------------------------------------------------------
best_model <- 'recipe_rf'
best_model <- 'recipe_xgb'

best_results <- 
  grid_results %>% 
  extract_workflow_set_result(best_model) %>% 
  select_best(metric = "rmse")

best_results

final_wf <- 
  grid_results %>% 
  extract_workflow(best_model) %>% 
  finalize_workflow(best_results)

final_wf

final_fit <- final_wf %>% fit(df_train) 
final_fit


# Evaluasi  --------------------------------------------------------------
train_set %>% 
  bind_cols(
    predict(final_fit, new_data = train_set)
  ) %>% 
  rmse(yk, .pred)


test_set %>% 
  bind_cols(
    predict(final_fit, new_data = test_set)
  ) %>% 
  rmse(yk, .pred)

df_train %>% 
  bind_cols(
    predict(final_fit, new_data = df_train)
  ) %>% 
  rmse(yk, .pred)



id_jelek <- df_train %>% 
  mutate(
    preds = preds_train,
    selisih = yk - preds
  ) %>% 
  select(c(id, yk, preds, selisih)) %>% 
  arrange(-abs(selisih)) %>% 
  filter(abs(selisih) > 10) %>% 
  pull(id)

dim(df_train)
length(id_jelek)



# fi ----------------------------------------------------------------------
final_fit



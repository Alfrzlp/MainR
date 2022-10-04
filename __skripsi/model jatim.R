library(sf)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()
# 

# write vector data -------------------------------------------------------
df_final <- st_read('D:/__Skripsi/vector/ubinan/ubinan_clean.shp')

# bulan panen
table(df_final$bulan)
table(df_final$tnm)

glimpse(df_final)
colSums(is.na(df_final))

jatim <- df_final %>% 
  filter(prov == 35) 

dim(jatim)

ggplot(jatim) +
  geom_sf()

write_sf(jatim, "D:/__Skripsi/jatim_ubinan/jatim_ubinan.shp")


# Data --------------------------------------------------------------------
x <- lapply(
  list.files('D:/__Skripsi/vector/jatim_padi _full/', full.names = T),
  function(x){
    read.csv(x) %>% 
      select(-c(.geo, system.index), -one_of("desa"))
  }
)

df_train <- do.call(rbind, x)
df_train <- df_train %>% 
  filter(id %in% id_bagus) %>% 
  janitor::clean_names() %>% 
  mutate(
    # savi = (nir - red)*(1 + L) / (nir + red + L),
    # lai = - log(abs(0.69 - savi)/0.59)/0.9,
    # p = -3.11 + 1.684 * lai + 12.458 * ndvi,
    # prv = 38.46154 * log(evi / 0.103),
    ndwi = (b8 - b11)/(b8 + b11),
    ndre = (b8 - b8a)/(b8 + b8a),
    gndvi = (b8 - b3)/(b8 + b3),
    ndvi = (b8 - b4)/(b8 + b4)
  ) %>% 
  select(
    id, jenis, strata, y, yk, b1, b2, b3, b4, b5, b6, 
    b7, b8, b8a, b9, b11, b12, 
    evi, msavi2, ndbi, ndvi, savi, ndwi, ndre, gndvi
  )


glimpse(df_train)


# model regresi -----------------------------------------------------------
m1 <- lm(
  yk ~ ndvi + savi + msavi2 + ndwi + evi + ndre + gndvi + ndbi,
  data = df_train
)
summary(m1)
m1 <- stats::step(m1)
yardstick::rmse(df_train, yk, predict(m1))




# EDA -----------------------------------------------------------
glimpse(df_train)
bands <- c('b1','b2','b3','b4','b5','b6','b7','b8', 'b8a', 'b9', 'b11', 'b12')
ic <- c("evi", "msavi2", "ndbi", "ndvi", "savi", "ndwi", "gndvi", "ndre")
allvar <- c(bands, ic)

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


dim(df_train)


# Korelasi ----------------------------------------------------------------
df_cor <- x %>% 
  select(-c(id, strata, jenis)) %>% 
  cor() 
df_cor

df_cor[, 1] 
ggcorrplot::ggcorrplot(df_cor)



# Outlier -----------------------------------------------------------------
library(olsrr)
obs_outlier <- ols_plot_cooksd_chart(m1)
ols_plot_cooksd_chart(m1)

obs_outlier <- obs_outlier$data %>% 
  filter(color == 'outlier') %>% 
  pull(obs)
obs_outlier



df_train <- df_train %>% 
  filter(
    between(ndvi, 0.38, 0.85)
  )


x <- df_train %>% 
  filter(!id %in% id_jelek)

x <- x %>% 
  filter(!id %in% id_jelek)

# Train test split ---------------------------------------------------------
set.seed(1)
splits <- initial_split(x, prop = 0.8)
splits

train_set <- training(splits)
test_set  <- testing(splits)

set.seed(1)
train_fold <- vfold_cv(train_set, v = 3)


# Model -------------------------------------------------------------------
reg_metric <- metric_set(rmse)

rf_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine('ranger', num.threads = 4) %>%
  set_mode('regression')


# svm_linear_spec <-
#   svm_linear(cost = tune(), margin = tune()) %>%
#   set_engine('kernlab') %>%
#   set_mode('regression')
# 
# svm_poly_spec <-
#   svm_poly(cost = tune(), degree = tune(), scale_factor = tune(), margin = tune()) %>%
#   set_engine('kernlab') %>%
#   set_mode('regression')
# 
# svm_rbf_spec <-
#   svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
#   set_engine('kernlab') %>%
#   set_mode('regression')



# Workflow ----------------------------------------------------------------
glimpse(x)

my_recipe <- 
  recipe(yk ~ ndvi + savi + msavi2 + ndwi + evi + ndre + gndvi + ndbi, data = train_set)
  # recipe(yk ~ ., data = train_set) %>% 
  update_role(id, new_role = "id")  
  step_rm(c(y))

my_recipe %>% summary()

my_workflow <-
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(my_recipe)
  

# Tuning ------------------------------------------------------------------
library(doParallel)
all_cores <- parallel::detectCores(logical = T)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

set.seed(1)
model_res <- 
  my_workflow %>% 
  tune_grid(
    resamples = train_fold,
    grid = 25,
    control = control_grid(
      save_pred = TRUE,
      verbose = T,
      allow_par = T
    ),
    metrics = reg_metric
  )


model_res %>% 
  collect_metrics() %>% 
  arrange(mean) 


best_model <- model_res %>% 
  select_best()
best_model


# final -------------------------------------------------------------------
final_wf <- 
  my_workflow %>% 
  finalize_workflow(best_model)

final_wf

final_fit <- final_wf %>% fit(x) 
final_fit


# Evaluasi ----------------------------------------------------------------
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


x %>% 
  bind_cols(
    predict(final_fit, new_data = x)
  ) %>% 
  rmse(yk, .pred)


# Id jelsk ----------------------------------------------------------------
id_jelek <- x %>% 
  bind_cols(
    predict(final_fit, new_data = x)
  ) %>% 
  mutate(
    selisih = abs(.pred - yk)
  ) %>% 
  select(id, yk, .pred, selisih) %>% 
  arrange(desc(-selisih)) %>% 
  filter(selisih >= 5) %>% 
  pull(id)

dim(x)
length(id_jelek)



id_bagus <- x %>% 
  bind_cols(
    predict(final_fit, new_data = x)
  ) %>% 
  mutate(
    selisih = abs(.pred - yk)
  ) %>% 
  select(id, yk, .pred, selisih) %>% 
  arrange(selisih) %>% 
  filter(selisih <= 1) %>% 
  pull(id)

length(id_bagus)  

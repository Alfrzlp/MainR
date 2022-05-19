library(tidyverse)
library(tidymodels)

tidymodels_prefer()


# Data --------------------------------------------------------------------
data(ames, package = "modeldata")
ames <- ames %>% mutate(Sale_Price = log(Sale_Price))



# Data Splitting ----------------------------------------------------------
set.seed(1)

ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_split

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)
dim(ames_test)



# Recipes -----------------------------------------------------------------
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  # Neighborhood yang persentasenya kecil akan digabung menjadi jenis "Other"
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

# binary encodings biasanya dipakai pada model linear
# karena jika onehot ditakutkan terjadi multikolinearitas





# Interkasi ---------------------------------------------------------------
ggplot(ames_train, aes(x = Gr_Liv_Area, y = 10^Sale_Price)) + 
  geom_point(alpha = .2) + 
  facet_wrap(~ Bldg_Type) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "lightblue") + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Gross Living Area", y = "Sale Price (USD)")



simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # Gr_Liv_Area is on the log scale from a previous step
  # interaksi setelah di dummy
  # namanya akan menjadi Gr_Liv_Area_x_Bldg_Type_Duplex
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )



# Spline Functions ---------------------------------------------------------
library(patchwork)
library(splines)

plot_smoother <- function(deg_free) {
  ggplot(ames_train, aes(x = Latitude, y = 10^Sale_Price)) + 
    geom_point(alpha = .2) + 
    scale_y_log10() +
    # natural splines.
    geom_smooth(
      method = lm,
      formula = y ~ ns(x, df = deg_free),
      color = "lightblue",
      se = FALSE
    ) +
    labs(title = paste(deg_free, "Spline Terms"),
         y = "Sale Price (USD)")
}

( plot_smoother(2) + plot_smoother(5) ) / ( plot_smoother(20) + plot_smoother(100) )




recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude,
       data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, deg_free = 20)




# Feature Extraction ------------------------------------------------------
# total basement size (Total_Bsmt_SF),
# size of the first floor (First_Flr_SF),
# the gross living area (Gr_Liv_Area)

step_pca(matches("(SF$)|(Gr_Liv)"))

# PCA assumes that all of the predictors are on the same scale





# Row Sampling Steps ------------------------------------------------------

# Downsampling the data keeps the minority class and
# takes a random sample of the majority class so that
# class frequencies are balanced.
# 
# Upsampling replicates samples from the minority class
# to balance the classes. Some techniques do this by 
# synthesizing new samples that resemble the minority
# class data while other methods simply add the same minority samples repeatedly.
# 
# Hybrid methods do a combination of both




# General Transformation --------------------------------------------------
step_mutate()
# hati2 dalam menggunakannnya. contoh
# x = w > mean(w) akan menggunakan mean data baru




# Skip Steps --------------------------------------------------------------
# For simple transformations of the outcome columns (Y),
# we strongly suggest that those operations be conducted 
# outside of the recipe.

# otomatis hanya pada training data
# step_adasyn(), step_bsmote(), step_downsample(),
# step_filter(), step_nearmiss(), step_rose(), step_sample(),
# step_slice(), step_smote(), step_smotenc(), step_tomek(),
# and step_upsample()



# tidy for recipes --------------------------------------------------------
ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01, id = "my_id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20) %>% 
  step_pca(matches("(SF$)|(Gr_Liv)"))

tidy(ames_rec)



# Fit ---------------------------------------------------------------------
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)




estimated_recipe <- 
  lm_fit %>% 
  extract_recipe(estimated = TRUE)


tidy(estimated_recipe)
tidy(estimated_recipe, id = "my_id") %>% 
  as.data.frame()
tidy(estimated_recipe, id = "dummy_kD7Rg")




# Role --------------------------------------------------------------------
ames_rec %>% update_role(address, new_role = "street address")
# maka addres tidak akan menjadi variabel prediktor



# Hasil -------------------------------------------------------------------
ames_metrics <- metric_set(rmse, rsq, mae)

ames_test_res <- ames_test %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_fit, new_data = ames_test))
  
ames_test_res %>% 
  ames_metrics(truth = Sale_Price, .pred)





# Judging Model Effectiveness ---------------------------------------------
ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()




# Binnary classification --------------------------------------------------
data(two_class_example)
tibble(two_class_example)

conf_mat(two_class_example, truth = truth, estimate = predicted)

classification_metrics <- metric_set(accuracy, mcc, f_meas)
classification_metrics(two_class_example, truth = truth, estimate = predicted)

# mcc() which measures the quality of both positive and negative 
# examples, the f_meas() metric emphasizes the positive class (the event of interest)



# first level is the most important
f_meas(two_class_example, truth, predicted, event_level = "second")




two_class_curve <- roc_curve(two_class_example, truth, Class1)
two_class_curve
roc_auc(two_class_example, truth, Class1)

autoplot(two_class_curve)



# Multiclass classification metric ----------------------------------------

data(hpc_cv)
tibble(hpc_cv)


accuracy(hpc_cv, obs, pred)

mcc(hpc_cv, obs, pred)



# Macro-averaging computes a set of one-versus-all metrics 
# using the standard two-class statistics. These are averaged.
# 
# Macro-weighted averaging does the same but the average is
# weighted by the number of samples in each class.
# 
# Micro-averaging computes the contribution for each class, 
# aggregates them, then computes a single metric from the aggregates.



class_totals <- 
  count(hpc_cv, obs, name = "totals") %>% 
  mutate(class_wts = totals / sum(totals))

class_totals


cell_counts <- 
  hpc_cv %>% 
  group_by(obs, pred) %>% 
  count() %>% 
  ungroup()
cell_counts


# Compute the four sensitivities using 1-vs-all
one_versus_all <- 
  cell_counts %>% 
  filter(obs == pred) %>% 
  full_join(class_totals, by = "obs") %>% 
  mutate(sens = n / totals)
one_versus_all


# Three different estimates:
one_versus_all %>% 
  summarize(
    macro = mean(sens), 
    macro_wts = weighted.mean(sens, class_wts),
    micro = sum(n) / sum(totals)
  )





sensitivity(hpc_cv, obs, pred, estimator = "macro")

sensitivity(hpc_cv, obs, pred, estimator = "macro_weighted")

sensitivity(hpc_cv, obs, pred, estimator = "micro")





roc_auc(hpc_cv, obs, VF, F, M, L)
roc_auc(hpc_cv, obs, VF, F, M, L, estimator = "macro_weighted")


hpc_cv %>% 
  group_by(Resample) %>% 
  accuracy(obs, pred)


hpc_cv %>% 
  group_by(Resample) %>% 
  roc_curve(obs, VF, F, M, L) %>% 
  autoplot()


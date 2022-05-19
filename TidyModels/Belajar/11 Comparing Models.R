library(tidymodels)
tidymodels_prefer()


# -------------------------------------------------------------------------
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


# Model -------------------------------------------------------------------
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")



# -------------------------------------------------------------------------
basic_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

interaction_rec <- 
  basic_rec %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) 

spline_rec <- 
  interaction_rec %>% 
  step_ns(Latitude, Longitude, deg_free = 50)

preproc <- 
  list(basic = basic_rec, 
       interact = interaction_rec, 
       splines = spline_rec
  )

lm_models <- workflow_set(preproc, list(lm = lm_model), cross = FALSE)
lm_models



# -------------------------------------------------------------------------
set.seed(1001)
ames_folds <- vfold_cv(ames_train, v = 10, repeats = 5)
ames_folds


# Training ----------------------------------------------------------------
lm_models <- 
  lm_models %>% 
  workflow_map(
    "fit_resamples", 
    # Options to `workflow_map()`: 
    seed = 1101,
    verbose = TRUE,
    # Options to `fit_resamples()`: 
    resamples = ames_folds,
    control = control_resamples(save_pred = T, verbose = T)
  )

lm_models



# RF ----------------------------------------------------------------------
rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow <- 
  workflow() %>% 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) %>% 
  add_model(rf_model) 

rf_res <- 
  rf_wflow %>% 
  fit_resamples(
    resamples = ames_folds,
    control = control_resamples(save_pred = T, verbose = T, save_workflow = T)
  )

rf_res



# Hasil -------------------------------------------------------------------
collect_metrics(lm_models) %>% 
  filter(.metric == "rmse")


four_models <- 
  as_workflow_set(random_forest = rf_res) %>% 
  bind_rows(lm_models)

four_models




# Viz hasil ---------------------------------------------------------------
library(ggrepel)

autoplot(four_models, metric = "rmse") +
  geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
  theme(legend.position = "none")




rsq_indiv_estimates <- 
  collect_metrics(four_models, summarize = FALSE) %>% 
  filter(.metric == "rsq") 

rsq_wider <- 
  rsq_indiv_estimates %>% 
  select(wflow_id, .estimate, id) %>% 
  pivot_wider(id_cols = "id", names_from = "wflow_id", values_from = ".estimate")

corrr::correlate(rsq_wider %>% select(-id), quiet = TRUE)



rsq_indiv_estimates %>% 
  mutate(wflow_id = reorder(wflow_id, .estimate)) %>% 
  ggplot(aes(x = wflow_id, y = .estimate, group = id, color = id)) + 
  geom_line(alpha = .5, lwd = 1.25) + 
  theme(legend.position = "none")




rsq_wider %>% 
  unnest(2:5) %>% 
  group_by(id) %>% 
  summarise_at(-1, mean) %>% 
  with( t.test(splines_lm, basic_lm, paired = TRUE) ) %>%
  tidy() %>% 
  select(estimate, p.value, starts_with("conf"))





# -------------------------------------------------------------------------
library(tidyposterior)
library(rstanarm)

options(mc.cores = parallel::detectCores())

# The rstanarm package creates copious amounts of output; those results
# are not shown here but are worth inspecting for potential issues. The
# option `refresh = 0` can be used to eliminate the logging. 
rsq_anova <-
  perf_mod(
    four_models,
    metric = "rsq",
    prior_intercept = rstanarm::student_t(df = 1),
    chains = 4,
    iter = 5000,
    seed = 1102,
    refresh = 0
  )



model_post <- 
  rsq_anova %>% 
  # Take a random sample from the posterior distribution
  # so set the seed again to be reproducible. 
  tidy(seed = 1103) 

glimpse(model_post)



model_post %>% 
  mutate(model = forcats::fct_inorder(model)) %>%
  ggplot(aes(x = posterior)) + 
  geom_histogram(bins = 50, color = "white", fill = "blue", alpha = 0.4) + 
  facet_wrap(~ model, ncol = 1)

# These histograms describe the estimated probability distributions 
# of the mean R2 value for each model. There is some overlap, 
# especially for the three linear models.



autoplot(rsq_anova) +
  geom_text_repel(aes(label = workflow), nudge_x = 1/8, nudge_y = 1/100) +
  theme(legend.position = "none")




rqs_diff <-
  contrast_models(rsq_anova,
                  list_1 = "splines_lm",
                  list_2 = "basic_lm",
                  seed = 1104)

rqs_diff

rqs_diff %>% 
  as_tibble() %>% 
  ggplot(aes(x = difference)) + 
  geom_vline(xintercept = 0, lty = 2) + 
  geom_histogram(bins = 50, color = "white", fill = "red", alpha = 0.4)

# lebih dari 0 maka splines lm lebih baik


summary(rqs_diff) %>% 
  select(-starts_with("pract"))





# THE EFFECT OF THE AMOUNT OF RESAMPLING
ggplot(intervals,
       aes(x = resamples, y = mean)) +
  geom_path() +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = .1) +
  labs(x = "Number of Resamples (repeated 10-fold cross-validation)")

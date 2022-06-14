library(lightgbm)


my_recipe <-
  recipe(
    class_asd ~ a1_score + a2_score + a3_score + a4_score + a5_score + a6_score + a7_score + a8_score + a9_score + a10_score + age + gender + result,
    data = train_set
  ) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  themis::step_upsample(class_asd, target = 1, over_ratio = 0.8) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes())

my_recipe



my_prep <- prep(my_recipe)
my_prep

# Data Train --------------------------------------------------------------
train_set <- bake(my_prep, new_data = train_set)
val_set <- bake(my_prep, new_data = test_set)


# Data Test ---------------------------------------------------------------
test_set <- juice(my_prep)






# Lgbm --------------------------------------------------------------------
ltrain <- lgb.Dataset(
  data = train_set %>%
    select(-class_asd) %>%
    as.matrix(),
  label = train_set$class_asd
)

lvalid <- lgb.Dataset(
  data = val_set %>%
    select(-class_asd) %>%
    as.matrix(),
  label = val_set$class_asd
)


lgb.params <- list(
  objective = "binary",
  learning_rate = 0.08,
  num_leaves = 20
)

train_an_lgb <- function() {
  lgb.train(
    params = lgb.params,
    data = ltrain,
    nrounds = 1000,
    verbose = 1,
    valids = list("lvalid" = lvalid),
    early_stopping_rounds = 50
  )
}


lgb_model <- train_an_lgb()




probs <- predict(
  lgb_model, 
  val_set %>%
    select(-class_asd) %>%
    as.matrix()
)

probs



prob_to_pred <- function(prob) as.integer(prob > 0.5)

val_set %>% 
  mutate(
    prob = probs,
    pred = factor(prob_to_pred(probs), levels = 0:1)
  ) %>% 
  metrics(truth = class_asd, estimate =  pred, prob)

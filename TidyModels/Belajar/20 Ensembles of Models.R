library(tidymodels)
library(stacks)
tidymodels_prefer()

# We found better performance for these data using the racing results.
# This might be due to the racing method pre-selecting the best model(s)
# from the larger grid.
concrete_stack <- 
  stacks() %>% 
  add_candidates(race_results)

concrete_stack


# BLEND THE PREDICTIONS ---------------------------------------------------
# Using the lasso penalty can remove candidates 
# (and sometimes whole model types) from the ensemble.
# 
# The correlation between ensemble candidates tends to be very high,
# and regularization helps alleviate this issue.

set.seed(2001)
ens <- blend_predictions(concrete_stack)
autoplot(ens)


set.seed(2002)
ens <- blend_predictions(
  concrete_stack,
  penalty = 10^seq(-2, -0.5, length = 20)
)
ens


autoplot(ens, "weights") +
  geom_text(aes(x = weight + 0.01, label = model), hjust = 0) + 
  theme(legend.position = "none") +
  lims(x = c(-0.01, 0.8))


# fit
ens <- fit_members(ens)

# test set result
reg_metrics <- metric_set(rmse, rsq)
ens_test_pred <- 
  predict(ens, concrete_test) %>% 
  bind_cols(concrete_test)

ens_test_pred %>% 
  reg_metrics(compressive_strength, .pred)
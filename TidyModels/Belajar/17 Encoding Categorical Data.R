library(tidyverse)
library(tidymodels)
tidymodels_prefer()


# Data --------------------------------------------------------------------
data(ames, package = "modeldata")
ames <- ames %>% mutate(Sale_Price = log(Sale_Price))

# analysis() -> train
# assessment() -> test

# Data Splitting ----------------------------------------------------------
set.seed(1)

ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_split

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)
dim(ames_test)


# -------------------------------------------------------------------------
ames_train %>%
  group_by(Neighborhood) %>%
  summarize(mean = mean(Sale_Price),
            std_err = sd(Sale_Price) / sqrt(length(Sale_Price))) %>% 
  ggplot(aes(y = reorder(Neighborhood, mean), x = mean)) + 
  geom_point() +
  geom_errorbar(aes(xmin = mean - 1.64 * std_err, xmax = mean + 1.64 * std_err)) +
  labs(y = NULL, x = "Price (mean, log scale)")



# likelihood encodings ----------------------------------------------------
# jika banyak kategori
# jika ada kategori baru
# step_lencode_glm(), step_lencode_mixed(), and step_lencode_bayes()
library(embed)

ames_glm <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_lencode_glm(Neighborhood, outcome = vars(Sale_Price)) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

ames_glm


glm_estimates <-
  prep(ames_glm) %>%
  tidy(number = 2)

glm_estimates

glm_estimates %>%
  filter(level == "..new")

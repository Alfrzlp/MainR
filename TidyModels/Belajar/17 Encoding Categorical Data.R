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


# ordinal -----------------------------------------------------------------
# step_unorder(), to convert to regular factors, and
# step_ordinalscore(), which maps specific numeric 
# values to each factor level.


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




ames_mixed <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_lencode_mixed(Neighborhood, outcome = vars(Sale_Price)) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

ames_mixed

mixed_estimates <-
  prep(ames_mixed) %>%
  tidy(number = 2)
mixed_estimates

mixed_estimates %>%
  filter(level == "..new")


# vz ----------------------------------------------------------------------
glm_estimates %>%
  rename(`no pooling` = value) %>%
  left_join(
    mixed_estimates %>%
      rename(`partial pooling` = value), by = "level"
  ) %>%
  left_join(
    ames_train %>% 
      count(Neighborhood) %>% 
      mutate(level = as.character(Neighborhood))
  ) %>%
  ggplot(aes(`no pooling`, `partial pooling`, size = sqrt(n))) +
  geom_abline(color = "gray50", lty = 2) +
  geom_point(alpha = 0.7) +
  coord_fixed()



# FEATURE HASHING ---------------------------------------------------------
# strategy when there are a large number of possible categories.
library(rlang)

ames_hashed <-
  ames_train %>%
  mutate(Hash = map_chr(Neighborhood, hash))

ames_hashed %>%
  select(Neighborhood, Hash)

# The rlang::hash() function generates a 128-bit hash,
# which means there are 2^128 possible hash values

ames_hashed %>%
  ## first make a smaller hash for integers that R can handle
  mutate(Hash = strtoi(substr(Hash, 26, 32), base = 16L),  
         ## now take the modulo
         Hash = Hash %% 16) %>%
  select(Neighborhood, Hash)



library(textrecipes)
ames_hash <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy_hash(Neighborhood, signed = FALSE, num_terms = 16L) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

ames_hash

# Feature hashing is fast and efficient but has a few downsides. 
# For example, different category values often map to the same
# hash value.

# Feature hashing can handle new category levels at prediction time,

# You can reduce hash collisions with a signed hash by using
# signed = TRUE. This expands the values from only 1 to either
# +1 or -1, depending on the sign of the hash.
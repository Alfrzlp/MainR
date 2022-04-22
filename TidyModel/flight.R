library(tidymodels)    
library(nycflights13)   
library(skimr)           # for variable summaries


set.seed(123)

flight_data <- 
  flights %>% 
  mutate(
    # Convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    # We will use the date (not date-time) in the recipe below
    date = lubridate::as_date(time_hour)
  ) %>% 
  # Include the weather data
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  # Only retain the specific columns we will use
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)

flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))


glimpse(flight_data)


flight_data %>% 
  skimr::skim(dest, carrier) 


# Data Splitting ----------------------------------------------------------
set.seed(222)
data_split <- initial_split(flight_data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)


flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") 

summary(flights_rec)

# step_zv() will remove columns from the data when the
# training set data have a single value
flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv()


test_data %>% 
  distinct(dest) %>% 
  # dplyr::filter(dest == 'LEX')
  anti_join(train_data)


# model ---------------------------------------------------------------
lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")


# Workflow ----------------------------------------------------------------
flights_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(flights_rec)

flights_wflow


# Fit ---------------------------------------------------------------------
flights_fit <- 
  flights_wflow %>% 
  fit(data = train_data)

flights_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()


# Predict -----------------------------------------------------------------
flights_aug <- 
  augment(flights_fit, test_data)

flights_aug %>%
  select(arr_delay, time_hour, flight, .pred_class, .pred_on_time)


# Metric ------------------------------------------------------------------
flights_aug %>% 
  roc_curve(truth = arr_delay, .pred_late) %>% 
  autoplot()

flights_aug %>% 
  roc_auc(truth = arr_delay, .pred_late)

flights_aug %>% 
  yardstick::conf_mat(truth = arr_delay, .pred_class)

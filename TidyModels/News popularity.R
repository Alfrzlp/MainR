library(tidymodels)

tidymodels_prefer()

# Data' -------------------------------------------------------------------
df_train <- 
  read_csv('D:/_Datasets/Autism_Prediction/train.csv') %>% 
  clean_names()

df_test <- 
  read_csv('D:/_Datasets/Autism_Prediction/test.csv') %>% 
  clean_names()

df_sub <- read_csv('D:/_Datasets/Autism_Prediction/sample_submission.csv')

glimpse(df_train)
glimpse(df_test)



# Data Information --------------------------------------------------------
# class
df_train %>% 
  group_by(class_asd) %>% 
  count()


# n unique
n_unique <- 
  df_train %>% 
  summarise_if(is.character, ~ length(unique(.x))) %>% 
  pivot_longer(everything(), values_to = 'n_unique', names_to = 'variabel')

n_unique

# Get all 
df_train[n_unique$variabel] %>% 
  apply(2, FUN = function(x) unique(x))

df_test[n_unique$variabel] %>% 
  apply(2, FUN = function(x) unique(x))

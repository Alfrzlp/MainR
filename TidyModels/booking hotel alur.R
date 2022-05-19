library(tidymodels)
library(tidyverse)
library(janitor)

tidymodels_prefer()

kaggle_submission <- function(competition_name, file, message = 'with R'){
  x <- str_glue('kaggle competitions submit -c {competition_name} -f {file} -m "{message}"')
  system(x)
}



# Data --------------------------------------------------------------------
df_train <- 
  read_csv('D:/_Datasets/ristekds2021modeling/flight.csv', show_col_types = F) %>% 
  clean_names()

df_test <- 
  read_csv('D:/_Datasets/ristekds2021modeling/test.csv', show_col_types = F) %>% 
  clean_names()


id_df_train <- df_train$account_id
df_train <- df_train %>% 
  mutate(
    cross_sell = if_else(hotel_id == 'None', 0, 1),
    cross_sell = factor(cross_sell, levels = 0:1)
  )


# -------------------------------------------------------------------------
id_sering_booking <- df_train %>% 
  mutate(cross_sell = as.numeric(levels(cross_sell))[cross_sell]) %>% 
  group_by(account_id) %>% 
  summarise(cross_sell = sum(cross_sell), n = n()) %>% 
  ungroup() %>% 
  filter(
    cross_sell > 0,
    cross_sell >= n - 20
  ) %>% 
  arrange(desc(cross_sell)) %>% 
  pull(account_id)

length(id_sering_booking)

id_ingin_booking <- df_train %>% 
  mutate(cross_sell = as.numeric(levels(cross_sell))[cross_sell]) %>% 
  group_by(account_id) %>% 
  summarise(cross_sell = sum(cross_sell), n = n()) %>% 
  ungroup() %>% 
  filter(
    cross_sell == 0,
    n == 1 
  ) %>% 
  arrange(desc(cross_sell)) %>% 
  pull(account_id)



# Model Tahap 1 -----------------------------------------------------------
hasil1 <- df_test %>%  
  mutate(
    is_cross_sell = ifelse(account_id %in% id_sering_booking, 1, 0),
    is_cross_sell = ifelse(account_id %in% id_pasti_booking, 1, is_cross_sell),
    is_cross_sell = ifelse(account_id %in% id_df_train, is_cross_sell, 0)
  ) %>% 
  select(order_id, is_cross_sell) 

glimpse(hasil1)

table(hasil1$is_cross_sell)
unique(hasil1$is_cross_sell)



# Model Tahap 2 -----------------------------------------------------------
pred <- predict(final_fit, new_data = df_test) %>% pull()
pred <- as.numeric(levels(pred))[pred]
pred

table(pred)



# Hasil -------------------------------------------------------------------
hasil <- hasil1 %>% 
  mutate(
    pred = pred,
    # ganti disini
    is_cross_sell = ifelse(is.na(is_cross_sell), pred, pred)
  ) %>% 
  select(-pred)

head(hasil)
table(hasil$is_cross_sell)



# Submission --------------------------------------------------------------
write.csv(hasil1, 'E:/sub.csv', quote = F, row.names = F)


kaggle_submission(
  'ristek2022tabular',
  'E:/sub.csv',
  'Siip'
)










# EDA lanjutan ------------------------------------------------------------
df_booking <- 
  df_train %>% 
  filter(!account_id %in% id_sering_booking)



ggplot(df_booking, aes(x = 0, fill = airlines_name)) +
  geom_bar() +
  scale_fill_viridis_d() +
  scale_x_continuous(labels = NULL) +
  facet_grid(cross_sell~., scales = 'free')


ggplot(df_booking, aes(y = visited_city, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 

unique(df_train$airlines_name)
c("e35de6a36d385711a660c72c0286154a", "74c5549aa99d55280a896ea50068a211")

ggplot(df_booking, aes(y = airlines_name, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 

# yes
ggplot(df_booking, aes(y = is_tx_promo, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 

# trip
ggplot(df_booking, aes(y = trip, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 

ggplot(df_booking, aes(y = no_of_seats, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 

ggplot(df_booking, aes(y = 0, fill = cross_sell)) +
  geom_bar(position = position_dodge()) +
  scale_y_continuous(labels = NULL) +
  facet_grid(~service_class)

ggplot(df_booking, aes(x = member_duration_days, fill = cross_sell)) +
  geom_histogram(bins = 30) +
  facet_grid(cross_sell~., scales = 'free')

ggplot(df_booking, aes(x = cross_sell, y = member_duration_days, fill = cross_sell)) +
  geom_boxplot()

ggplot(df_booking, aes(x = cross_sell, y = price, fill = cross_sell)) +
  geom_boxplot()



df_booking %>% 
  filter(gender != 'None') %>% 
  ggplot(aes(y = gender, fill = cross_sell)) +
  geom_bar(position = position_dodge()) 




c("'['Jakarta', 'Medan', 'Bali']'", "'['Medan', 'Bali', 'Jakarta']'", "'['Jakarta', 'Bali', 'Medan', 'Jogjakarta', 'Semarang']'", "'['Bali', 'Jakarta', 'Medan']'" )









# -------------------------------------------------------------------------
id_sering_booking <- df_train %>% 
  mutate(cross_sell = as.numeric(levels(cross_sell))[cross_sell]) %>% 
  group_by(account_id) %>% 
  summarise(cross_sell = sum(cross_sell), n = n()) %>% 
  ungroup() %>% 
  filter(
    cross_sell >= 0,
    cross_sell >= n - 20
  ) %>% 
  arrange(desc(cross_sell)) %>% 
  pull(account_id)

length(id_sering_booking)




id_pasti_booking <- df_train %>% 
  mutate(cross_sell = as.numeric(levels(cross_sell))[cross_sell]) %>% 
  group_by(account_id) %>% 
  summarise(cross_sell = sum(cross_sell), n = n()) %>% 
  ungroup() %>% 
  filter(
    cross_sell > 0,
    cross_sell == n
  ) %>% 
  arrange(desc(cross_sell)) %>% 
  pull(account_id)

length(id_pasti_booking)




# Model Lanjutan ----------------------------------------------------------
pred <- predict(final_fit, new_data = df_test) %>% pull()
pred <- as.numeric(levels(pred))[pred]
pred




table(pred)
df_test %>% 
  mutate(pred) %>% 
  filter(!account_id %in% id_sering_booking) %>% 
  pull(pred) %>% 
  table()


hasil1 <- df_test %>%  
  mutate(
    pred,
    is_cross_sell = ifelse(account_id %in% id_sering_booking, pred, 0),
    is_cross_sell = ifelse(account_id %in% id_pasti_booking, 1, is_cross_sell),
    is_cross_sell = ifelse(account_id %in% id_df_train, is_cross_sell, 0)
  ) %>% 
  select(order_id, is_cross_sell) 

glimpse(hasil1)
table(hasil1$is_cross_sell)






write.csv(hasil1, 'E:/sub.csv', quote = F, row.names = F)

kaggle_submission(
  'ristek2022tabular',
  'E:/sub.csv',
  'Siip'
)






pred <- predict(final_fit, new_data = df_test, type = 'prob')
preds <- pred %>% 
  mutate(
    is_cross_sell = ifelse(.pred_1 >= 0.5, 1, 0)
  ) %>% 
  pull(is_cross_sell)

table(preds)

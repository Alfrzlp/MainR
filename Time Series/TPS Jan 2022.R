library(tidyverse)
library(MLmetrics)
library(forecast)

# Data --------------------------------------------------------------------
loc <- 'D:/Downloads/tabular-playground-series-jan-2022'

train <- read.csv(file.path(loc, 'train.csv'), row.names = 'row_id')
test <- read.csv(file.path(loc, 'test.csv'))
sample_sub <- read.csv(file.path(loc, 'sample_submission.csv'))

head(train)
dim(train)
glimpse(train)


head(test)
dim(test)

head(sample_sub)
# Change type Char to Date with library lubridate -------------------------
# Note : ymd = year month date
train <- train %>% 
  mutate(
    date = lubridate::ymd(date),
    day = format(date, '%j'),
    day_m = format(date, '%d'),
    month = format(date, '%m'),
    year = format(date, '%y'),
  ) %>% 
  mutate_at(5:9, ~as.numeric(.x))

train %>% glimpse()
sample_n(train, 10)

test <- test %>% 
  mutate(
    date = lubridate::ymd(date),
    day = format(date, '%j'),
    day_m = format(date, '%d'),
    month = format(date, '%m'),
    year = format(date, '%y'),
    num_sold = NA
  ) %>% 
  mutate_at(6:9, ~as.numeric(.x))

test %>% glimpse()

unique(train$country)
unique(train$store)
unique(train$product)

# Ctrl + Alt + select line (Multiple Cursor in Rstudio)
unique(test$country)
unique(test$store)
unique(test$product)


# EDA ---------------------------------------------------------------------
# Store KaggleMart
train %>% 
  dplyr::filter(store == 'KaggleMart') %>% 
  ggplot(aes(x = date, y=num_sold, col = product)) +
  geom_line() +
  labs(title = 'Store : KaggleMart') +
  theme_minimal() +
  facet_grid(product~country, scales = 'free', space = 'free')

# Store KaggleRama
train %>% 
  dplyr::filter(store == 'KaggleRama') %>% 
  ggplot(aes(x = date, y=num_sold, col = product)) +
  geom_line() +
  labs(title = 'Store : KaggleRama') +
  theme_minimal() +
  facet_grid(product~country, scales = 'free', space = 'free')

# All Store
train %>% 
  ggplot(aes(x = date, y=num_sold, col = store)) +
  geom_line() +
  labs(title = 'Store : All Store') +
  theme_minimal() +
  facet_grid(product~country, scales = 'free', space = 'free')

train %>% 
  ggplot(aes(x = date, y=num_sold, col = store)) +
  geom_line() +
  labs(title = 'Store : All Store') +
  theme_minimal() +
  facet_grid(product~., scales = 'free', space = 'free')

# Saya pikir setiap product akan memiliki model tersendiri, karena pola
# dari setiap negara hampir sama untuk product yang sama


# Pola 1 Tahun ------------------------------------------------------------
train %>% 
  dplyr::filter(date <= as.Date('2016-01-01')) %>% 
  ggplot(aes(x = date, y=num_sold, col = store)) +
  geom_line() +
  labs(title = 'One Year') +
  theme_minimal() +
  facet_grid(product~country, scales = 'free', space = 'free')

train %>% 
  dplyr::filter(date <= as.Date('2015-02-01')) %>% 
  ggplot(aes(x = date, y=num_sold, col = store)) +
  geom_line() +
  labs(title = 'two Month') +
  theme_minimal() +
  facet_grid(product~country, scales = 'free', space = 'free')

# Dari grafik diatas saya berpikir terdapat pola mingguan juga
train %>% 
  dplyr::filter(date <= as.Date('2015-01-15')) %>% 
  ggplot(aes(x = date, y=num_sold, col = store)) +
  geom_line() +
  labs(title = 'two Month') +
  theme_minimal() +
  facet_grid(product~country, scales = 'free', space = 'free')



# SMAPE -------------------------------------------------------------------
SMAPE <- function(y_true, pred){
  x = abs(y_true - pred)/((y_true + pred)/2)
  return(mean(x))
}


# SARIMA ------------------------------------------------------------------
mug <- train %>% dplyr::filter(product == 'Kaggle Mug')
hat <- train %>% dplyr::filter(product == 'Kaggle Hat')
sticker <- train %>% dplyr::filter(product == 'Kaggle Sticker')

test_mug <- test %>% dplyr::filter(product == 'Kaggle Mug')
test_hat <- test %>% dplyr::filter(product == 'Kaggle Hat')
test_sticker <- test %>% dplyr::filter(product == 'Kaggle Sticker')

mug %>% 
  arrange(-num_sold)

mug %>% 
  select()

# Mug ---------------------------------------------------------------------
diff(log(mug$num_sold), lag = 7) %>% 
  ggtsdisplay()

summary(mug$date)

x_mug <- mug %>% 
  dplyr::select(country, store) %>% 
  mutate_all(~ as.numeric(as.factor(.x)) - 1) %>% 
  as.matrix()
head(x_mug)

mug_ts <- ts(mug$num_sold, start = c(2015, 1), frequency = 7)
dim(x_mug)
length(mug_ts)

mug_model <- auto.arima(mug_ts, xreg = x_mug, seasonal = T, lambda = 0)
summary(mug_model)


predict(mug_model, newxreg = test_mug)







mug_ts <- 
  train %>% dplyr::filter(
  product == 'Kaggle Mug',
  store == 'KaggleRama',
  country == 'Finland'
) %>% 
 {ts((.)$num_sold, start = c(2015, 1), frequency = 7)}

mug_ts
length(mug_ts)

hw_model <- hw(mug_ts, lambda = 0, initial = 'optimal')
accuracy(hw_model)

sr_model <- auto.arima(log(mug_ts), seasonal = T, lambda = 0,
                       approximation = F, stepwise = F)
summary(sr_model)

ets_model <- ets(mug_ts, lambda = 0)
accuracy(ets_model)

m <-  Arima(log(log(mug_ts)), order = c(0, 0, 4),
      seasonal = list(order = c(0, 1, 1), period = 7),
      lambda = 0)
summary(m)
MAPE(exp(exp(m$fitted)), mug_ts)*100
# (0,0,4)(0,1,1)[7] 







train %>% dplyr::filter(
  product == 'Kaggle Mug',
  store == 'KaggleRama',
  country == 'Finland'
) %>% 
  select(day, month, num_sold) %>% 
  arrange(desc(num_sold))



mug_ts <- 
  train %>% dplyr::filter(
    product == 'Kaggle Mug',
    store == 'KaggleRama',
    country == 'Finland'
  ) %>% 
  {ts((.)$num_sold, start = c(2015, 1), frequency = 1)}

x_mug <- 
  train %>% dplyr::filter(
    product == 'Kaggle Mug',
    store == 'KaggleRama',
    country == 'Finland'
  ) %>% 
  select(day, month) %>% 
  as.matrix()

head(x_mug)
head(train)

stlm_model <- mug_ts %>% 
  msts(seasonal.periods = c(7*1:102)) %>% 
  stlm(method = "arima", lambda = 0, xreg = x_mug)

accuracy(stlm_model)







sr_model <- mug_ts %>% 
  auto.arima(lambda = 'auto', seasonal = T, xreg = x_mug, 
             approximation = F)
summary(sr_model)
accuracy(sr_model)











accuracy(msts_model)

SMAPE(mug_ts, exp(as.vector(stlm_model$fitted)))
SMAPE(mug_ts, hw_model$fitted)
SMAPE(mug_ts, sr_model$fitted)

pred <- predict(stlm_model, h = 365)

test <- test %>% 
  mutate(
    num_sold = ifelse((product == 'Kaggle Mug' & store == 'KaggleRama' & country == 'Sweden'), pred$mean, num_sold)
  )




train <- train %>% mutate(pred_num_sold = NA)
test <- test %>% mutate(num_sold = NA)

head(train)
head(test)


for(i in unique(train$product)){
      
      df_ts <- 
        train %>% dplyr::filter(
          product == i
        ) %>% 
        {ts((.)$num_sold, start = c(2015, 1), frequency = 7)}
      
      x_reg <- train %>% 
        select(-date) %>% 
        dplyr::filter(product == i) %>% 
        mutate_if(is.character, ~ as.numeric(as.factor(.x)) - 1) %>% 
        select(-num_sold) %>% 
        mutate(
          x = day_m*month*year,
          x2 = country*store*product 
        ) %>% 
        as.matrix() 
      
      stlm_model <- df_ts %>%
        msts(seasonal.periods = c(7*1:52)) %>% 
        stlm(method = "arima", lambda = 0, xreg = x_reg)
      
      print(paste('MAPE   :', MAPE(df_ts, as.vector(stlm_model$fitted))*100))
      #print(paste('SMAPE  :', SMAPE(df_ts, as.vector(stlm_model$fitted))))
      pred <- predict(stlm_model, h = 365*3)
      
      test <- test %>% 
        mutate(
          num_sold = ifelse((product == i), pred$mean, num_sold)
        )
      
      train <- train %>% 
        mutate(
          pred_num_sold = ifelse((product == i), stlm_model$fitted, pred_num_sold)
        )

}





head(test)

write.csv(
  test %>% 
    select(row_id, num_sold),
  'E:/sub.csv', 
  row.names = F,
  quote = F
)

MAPE(train$pred_num_sold, train$num_sold)*100
library(tidyverse)
library(lubridate)
library(forecast)
library(fpp2)


# Utils -------------------------------------------------------------------
eval_forecast <- function(y, f) {
  data.frame(y, f) %>%
    mutate(res = y - f) %>%
    summarise(
      bias = mean(res, na.rm = T),
      mad = mean(abs(res), na.rm = T),
      mse = mean(res^2, na.rm = T),
      rmse = sqrt(mse),
      mape = mean(abs(res) / y, na.rm = T) * 100
    )
}

# Data --------------------------------------------------------------------

bri <-
  read.csv("C:/Users/Ridson Alfarizal/Downloads/Data Historis BBRI.csv") %>%
  janitor::clean_names() %>%
  rename(date = i_tanggal) %>%
  mutate_at(
    2:5, ~ .x * 1000
  ) %>%
  mutate(
    date = dmy(date),
    vol = str_remove_all(vol, "M") %>%
      str_replace_all("\\,", "\\.") %>%
      as.numeric(),
    perubahan = str_remove_all(perubahan, "%") %>%
      str_replace_all("\\,", "\\.") %>%
      as.numeric()
  ) %>%
  mutate(
    hari = wday(date, label = T),
    .after = date
  )

bri %>%
  glimpse()

tail(bri)
head(bri)

# Hari libur --------------------------------------------------------------
Sys.timezone()
Sys.setenv(TZ = "Asia/Jakarta")


full_date <-
  data.frame(
    date = seq.POSIXt(
      as.POSIXct("2020-12-02", "%Y-%m-%d", tz = "Asia/Jakarta"),
      as.POSIXct("2021-12-01", "%Y-%m-%d", tz = "Asia/Jakarta"),
      by = "day"
    ),
    hari = wday(date, label = T)
  )

dim(full_date)

bri <-
  full_date %>%
  left_join(bri, by = "date") %>%
  dplyr::filter(!hari.x %in% c("Min", "Sab")) %>%
  select(-hari.y, hari = hari.x)


# Data yang bersih --------------------------------------------------------
bri <- readxl::read_xlsx("D:/_Datasets/bri.xlsx") %>%
  mutate_at(-c(1:2, 7), ~ .x * 1000)

dim(bri)
head(bri)


bri_ts <- ts(bri$pembukaan[1:261],
  start = c(2020, 12),
  end = c(2021, 12), deltat = 1 / 264
)
bri_ts
bri_ts <- na.interp(bri_ts, lambda = "auto")
bri_ts


bri_ts %>%
  autoplot() +
  labs(x = "Date", y = "Harga Saham")

# Sebelum di diff
bri_ts %>%
  ggtsdisplay()

# Setelah dilakukan first diff
bri_ts %>%
  diff() %>%
  ggtsdisplay()

model <- auto.arima(bri_ts, seasonal = T, stepwise = F, approximation = F)
model

model <- Arima(bri_ts,
  order = c(4, 1, 1),
  seasonal = list(order = c(2, 0, 2), periode = 5)
)
model %>%
  summary()

model %>%
  forecast(10) %>%
  accuracy(bri$pembukaan[252:261])



# Cek Asumsi Residual
checkresiduals(model)

# Forecast
hasil <-
  model %>%
  forecast(10)

hasil %>%
  autoplot()

ts(bri$pembukaan[252:261]) %>%
  autoplot()

data.frame(
  pred = as.vector(hasil$mean),
  # low = hasil$lower[,2],
  # high = hasil$upper[,2],
  actual = bri$pembukaan[252:261],
  date = bri$date[252:261]
) %>%
  pivot_longer(-date) %>%
  ggplot() +
  geom_line(
    aes(x = date, y = value, color = name)
  )
# geom_ribbon(
#   aes(x = date, y = pred, ymin = low, ymax = high), alpha = 0.2,
#   fill = 'steelblue'
# )


holt(bri_ts) %>%
  forecast(10) %>%
  accuracy()
autoplot()


# Model Ngawur ------------------------------------------------------------
dat <-
  data.frame(
    hari = bri$hari[-1],
    open = na.interp(bri$pembukaan[-1]),
    close_kemarin = na.interp(bri$terakhir[-nrow(bri)]),
    high = na.interp(bri$tertinggi[-nrow(bri)]),
    low = na.interp(bri$terendah[-nrow(bri)]),
    vol = na.interp(bri$vol[-nrow(bri)])
  ) %>%
  mutate_at(2:6, ~ as.numeric(.x))


model <- lm(open ~ (.)^2, dat)
summary(model)

m <- lm(open ~ close_kemarin + high + vol, dat)
summary(m)

# Prediksi Kamis
m %>%
  predict(
    data.frame(
      close_kemarin = 4210,
      high = 4250,
      low = 4170,
      vol = 143.23,
      hari = "Kam"
    )
  )
model$residuals

olsrr::ols_step_both_p(model)
summary(m)


eval_forecast(dat$open, model$fitted.values)
eval_forecast(dat$open, m$fitted.values)

# Naive
eval_forecast(dat$open, dat$close_kemarin)

# Simple Model  : 4224.965
# Complex Model : 4227.446
# Naive         : 4210

# ElasticNet --------------------------------------------------------------
library(caret)
library(glmnet)

set.seed(1)
cv_5 <- trainControl(method = "cv", number = 5)

# tuneLength = 10, we will search 10
# alpha values and 10 lambda values for each
enet <- train(
  open ~ .^2,
  data = dat,
  method = "glmnet",
  trControl = cv_5,
  tuneLength = 10
)


get_best_result <- function(caret_fit) {
  best <- which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result <- caret_fit$results[best, ]
  rownames(best_result) <- NULL
  best_result
}


get_best_result(enet)

set.seed(1)
X <- model.matrix(open ~ .^2, dat)[, -1]
y <- dat$open

fit_lasso_cv <- cv.glmnet(X, y, alpha = 1)
sqrt(fit_lasso_cv$cvm[fit_lasso_cv$lambda == fit_lasso_cv$lambda.min]) # CV-RMSE minimum






library(FinTS)
r <- resid(model)
ArchTest(r)

library(fGarch)
arch.fit <- garchFit(~ garch(1, 0), data = r, trace = F)
summary(arch.fit)

bri$ht <- arch.fit@h.t
ggplot(bri, aes(y = ht, x = date)) +
  geom_line(col = "#ff9933") +
  ylab("Conditional Variance") +
  xlab("Date")

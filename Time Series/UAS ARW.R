library(readxl)
library(tidyverse)
library(fpp2)

# Data --------------------------------------------------------------------
loc <- 'D:/__SEMESTER 5/Times Series/Ujian/DATA UAS TS GASAL 2021-2022.xls'

jci <- 
  readxl::read_xls(loc, sheet = 2) %>% 
  janitor::clean_names() %>% 
  rename(delta_close = close_2) %>% 
  select(-8)
  
jci %>% glimpse()
jci <- jci %>% 
  mutate(r = jci$delta_close/c(NA, jci$close[-length(jci$close)]))

jci %>% 
  select(-c(high, low, adj_close)) %>% 
  arrange(r)
# Visualisasi -------------------------------------------------------------
jci %>% 
  ggplot(aes(x = date, y = close)) +
  geom_line(lwd = 0.7, colour = "steelblue") +
  labs(
    x = 'Date', y = 'IHSG',
    title = 'Indonesia Stock Excange',
    subtitle = '02 Januari 2020 - 08 Desember 2020',
    caption = expression(italic('Sumber : www.finance.yahoo.com'))
  ) +
  # biar gak ada yang kepotong
  coord_cartesian(clip = "off") +
  theme_minimal(
    base_family = "Arial Narrow",
    base_size = 13
  ) +
  theme(
    plot.title.position = "plot",
    text = element_text(colour = "gray30"),
    plot.title = element_text(family = "Arial", face = "bold",
                              colour = "black", size = rel(1.75)),
    axis.title.y = element_text(hjust = 1, colour = "gray5"),
    axis.title.x = element_text(hjust = 0, colour = "gray5"),
    axis.line = element_line(colour = "gray70"),
    axis.ticks = element_line(colour = 'gray70'),
    plot.margin = margin(15, 15, 15, 15)
  )

jci %>% 
  ggplot(aes(x = date, y = r)) +
  geom_line(lwd = 0.7, colour = "steelblue") +
  labs(
    x = 'Date', y = 'Return Saham',
    title = 'Indonesia Stock Excange',
    subtitle = '02 Januari 2020 - 08 Desember 2020',
    caption = expression(italic('Sumber : www.finance.yahoo.com'))
  ) +
  # biar gak ada yang kepotong
  coord_cartesian(clip = "off") +
  theme_minimal(
    base_family = "Arial Narrow",
    base_size = 13
  ) +
  theme(
    plot.title.position = "plot",
    text = element_text(colour = "gray30"),
    plot.title = element_text(family = "Arial", face = "bold",
                              colour = "black", size = rel(1.75)),
    axis.title.y = element_text(hjust = 1, colour = "gray5"),
    axis.title.x = element_text(hjust = 0, colour = "gray5"),
    axis.line = element_line(colour = "gray70"),
    axis.ticks = element_line(colour = 'gray70'),
    plot.margin = margin(15, 15, 15, 15)
  )


ggsave(
  'D:/visualisasi/ihsg_bulan.png',
  width = 6.5,
  height = 4,
  limitsize = F,
  dpi = 300,
  type = "cairo-png"
)
# Modeling ----------------------------------------------------------------
series <- ts(jci$close)
Sys.timezone()

library(xts)
series <- xts(jci$r, order.by = jci$date, tzone = 'Asia/Jakarta')[-1,]
series
plot(series, main = 'return')

urca::ur.df(series) %>% summary()

tseries::adf.test(series)
tseries::adf.test(diff(series)[-1])

auto.arima(series)
(model <- arima(series, order = c(2, 0, 2)))
lmtest::coeftest(model)

library(aTSA)
arch.test(model)

library(fGarch)
dseries <- diff(series)[-1]
dseries <- series
plot(dseries)

model01 <- garchFit(~ arma(2, 2) + garch(1, 0), data = dseries, trace = F)
summary(model01)

model11 <- garchFit(~ arma(2, 2) + garch(1, 1), data = dseries, trace = F)
summary(model11)

xts(model11@h.t, jci$date[-1]) %>% 
  plot(main = 'Simpangan Baku')

predict(
  model11, 5, plot = T
)

dim(series)

model11@residuals

# EGARCH dan TGARCH -------------------------------------------------------
library(rugarch)
egarch_spec <-  ugarchspec(variance.model = list(model="eGARCH",         #Other options are egarch, fgarch, etc.
                      garchOrder=c(1, 1)), # You can modify the order GARCH(m,s) here
                      mean.model = list(armaOrder=c(2, 2)), #Specify your ARMA model implying your model should be stationary.
                      distribution.model = "norm")         #Other distribution are "std" for t-distribution, and "ged" for General Error Distribution
egarch <- ugarchfit(spec = egarch_spec, 
                    data = series)
egarch

# leverage effect yang jika nilainya positif,
# bad news akan lebih berpengaruh dibandingkan dengan good news


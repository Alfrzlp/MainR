library(readxl)
library(tidyverse)
library(fpp2)

# Data --------------------------------------------------------------------
loc <- 'D:/__SEMESTER 5/Times Series/Ujian/DATA UAS TS GASAL 2021-2022.xlsx'

jci <- 
  readxl::read_xlsx(loc, sheet = 2) %>% 
  janitor::clean_names() %>% 
  rename(delta_close = close_2) %>% 
  select(-x10)
  
jci %>% glimpse()
jci <- jci %>% 
  mutate(
    ret = jci$delta_close/c(NA, jci$close[-length(jci$close)]),
    r = log(jci$close/c(NA, jci$close[-length(jci$close)]))
  )

jci %>% 
  select(-c(high, low, adj_close)) %>% 
  arrange(r)
# Visualisasi -------------------------------------------------------------
library(ggpubr)

c <- jci %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(x = date, y = close)) +
  geom_line(lwd = 0.7, colour = "steelblue") +
  labs(
    x = NULL, y = 'Close'
  ) +
  # biar gak ada yang kepotong
  coord_cartesian(clip = "off") +
  theme_minimal(
    base_family = "Arial Narrow",
    base_size = 13
  ) +
  scale_x_date(
    date_breaks = "3 month", date_labels =  "%b %Y", 
    limits = c(as.Date('2020-01-01'), as.Date('2021-12-08'))
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

r <- jci %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(x = date, y = lnr)) +
  geom_line(lwd = 0.7, colour = "steelblue") +
  labs(
    x = 'Date', y = 'Return',
    # title = 'Jakarta Composite Index',
    # subtitle = '02 Januari 2020 - 08 Desember 2020',
    caption = expression(italic('Sumber : www.finance.yahoo.com'))
  ) +
  # biar gak ada yang kepotong
  coord_cartesian(clip = "off") +
  theme_minimal(
    base_family = "Arial Narrow",
    base_size = 13
  ) +
  scale_x_date(
    date_breaks = "3 month", date_labels =  "%b %Y", 
    limits = c(as.Date('2020-01-01'), as.Date('2021-12-08'))
  ) +
  theme(
    plot.title.position = "plot",
    text = element_text(colour = "gray30"),
    plot.title = element_text(family = "Arial", face = "bold",
                              colour = "black", size = rel(1.75)),
    axis.title.y = element_text(hjust = 1, colour = "gray5"),
    axis.title.x = element_text(hjust = 0, colour = "gray5"),
    axis.text.x = element_text(angle = 0),
    axis.line = element_line(colour = "gray70"),
    axis.ticks = element_line(colour = 'gray70'),
    plot.margin = margin(15, 15, 15, 15)
  )

fig <- 
  ggarrange(
    c, r,
    ncol = 1, nrow = 2
  )

annotate_figure(
  fig,
  top = text_grob("Jakarta Composite Index", color = "black", face = "bold", size = 15,
                  hjust = 0, x = 0.05)
)

ggsave(
  'D:/visualisasi/hasil.png',
  width = 6.5,
  height = 4,
  limitsize = F,
  dpi = 300,
  type = "cairo-png"
)

summary(jci)
var(jci$close, na.rm = T)
moments::skewness(jci$close, na.rm = T)
moments::kurtosis(jci$close, na.rm = T)
# Modeling ----------------------------------------------------------------
series <- ts(jci$close)
Sys.timezone()

library(xts)
series <- xts(jci$lnr, order.by = jci$date, tzone = 'Asia/Jakarta')[-1,]
series
plot(series, main = 'Return')

library(urca)
ur.df(series, type = 'none') %>% 
  summary()
# lebih dari -1.95 stasioner

library(tseries)
pacf(series, lag.max = 36)
acf(series)

library(forecast)
ggarrange(
  ggAcf(series, lag.max = 36) +
    labs(
      title = NULL
    ) +
    scale_x_continuous(
      n.breaks = 10
    ),
  ggPacf(series, lag.max = 36) +
    labs(
      title = NULL
    ) +
    scale_x_continuous(
      n.breaks = 10
    ),
  ncol = 2, nrow = 1
)






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


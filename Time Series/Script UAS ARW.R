library(tidyverse)
library(rugarch)


# Data --------------------------------------------------------------------
loc <- 'D:/__SEMESTER 5/Times Series/Ujian/DATA UAS TS GASAL 2021-2022.xlsx'

jci <- 
  readxl::read_xlsx(loc, sheet = 2) %>% 
  janitor::clean_names() %>% 
  rename(delta_close = close_2) %>% 
  select(-x10)


library(xts)
series <- xts(jci$lnr, order.by = jci$date, tzone = 'Asia/Jakarta')[-1,]

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

# Uji Stasioneritas -------------------------------------------------------
library(urca)

ur.df(series) %>% summary()


# Plot ACF dan PACF -------------------------------------------------------
library(forecast)
library(ggpubr)

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


# Dugaan model ARIMA ------------------------------------------------------
arima(series, order = c(1, 0, 1))
arima(series, order = c(2, 0, 2))
arima(series, order = c(3, 0, 3))
auto.arima(series, stepwise = T, approximation = F)


# Uji ARCH-LM -------------------------------------------------------------
library(aTSA)
model <- arima(series, order = c(2, 0, 2))
hasil <- arch.test(model) 
hasil

ggAcf(model$residuals^2) +
  ggtitle('Residual kuadrat')


# GARCH -------------------------------------------------------------------
library(fGarch)

model11 <- garchFit(~ arma(2, 2) + garch(1, 1), data = series, trace = F)
summary(model11)



# GACRH 11
spec = ugarchspec(
  mean.model = list(armaOrder=c(2, 2))
)
fit = ugarchfit(data = series, spec = spec)
fit
signbias(fit)


# EGARCH ------------------------------------------------------------------
egarch_spec <-  ugarchspec(variance.model = list(model="eGARCH",        
                                                 garchOrder=c(1, 1)), 
                           mean.model = list(armaOrder=c(2, 2)), 
                           distribution.model = "norm")    
egarch <- ugarchfit(spec = egarch_spec, 
                    data = series)
egarch



# Uji Signiiikansi Parameter dan diagnosis residual -----------------------
normtest::jb.norm.test(scale(egarch@fit$residuals))

dat <- readxl::read_xlsx('D:/_Datasets/__Time Series/jumlah barang melalui KA jawa.xlsx')
dim(dat)
dat$date

dat <- ts(dat$jb, start = c(2006, 1), end = c(2021, 7), frequency = 12)

plot.ts(dat)

library(FinTS)
ArchTest(dat, lags=1, demean = T)


# ARCH --------------------------------------------------------------------
library(tseries)
arch <- garch(dat, c(0,1))
summary(arch)

# Estimated ARCH(1) variance for the 'byd' dataset
hhat <- ts(2*arch$fitted.values[-1,1]^2)
plot.ts(hhat)

hhat

# Evaluasi Model ----------------------------------------------------------
# normalitas error
# keacakan residual
# efek ARCH

PerformanceAnalytics::charts.TimeSeries()
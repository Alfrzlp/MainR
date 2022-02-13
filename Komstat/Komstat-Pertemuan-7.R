# load data dari csv file
market <- read.csv("D:/Datasets/bank.csv", sep = ";")
head(market)

# install ggplot
# install.packages("ggplot2", dependencies = T)
library(ggplot2)

# diagram pencar sederhana
ggplot(market, aes(x = age, y = balance)) +
  geom_point()
qplot(x = age, y = balance, geom = "point", data = market)

ggplot(market, aes(x = age, y = balance, color = marital)) +
  geom_point()

ggplot(market, aes(x = age, y = balance, color = marital)) +
  geom_point() +
  facet_wrap(~marital)

ggplot(market, aes(balance)) +
  geom_histogram() +
  xlab("Balance") +
  ylab("Frequency") +
  ggtitle("Histogram Balance")

ggplot(market, aes(balance)) +
  geom_histogram(fill = "blue", color = "red")

ggplot(market, aes(age)) +
  geom_freqpoly(color = "red")

# Regresion Line
fit1 <- lm(balance ~ age, data = market)
pred <- predict(fit1)
p1 <- ggplot(market, aes(x = age, y = balance))
p1 + geom_point(aes(color = duration)) + geom_line(aes(y = pred))

# data gini ratio
tahun <- rep(c(2009, 2010, 2011, 2012, 2013), 2)
gini <- c(0.36, 0.36, 0.44, 0.42, 0.433, 0.36, 0.36, 0.41, 0.41, 0.411)
prov <- rep(c("DKI", "Jabar"), each = 5)
giniData <- data.frame(tahun, gini, prov)
p5 <- ggplot(giniData, aes(x = tahun, y = gini))
p5 + geom_line(aes(color = prov))

# Menggambar Peta
library(dplyr)
library(ggplot2)

# menggunakan USArresets data
data("USArrests")
USArrests$region <- tolower(rownames(USArrests))
head(USArrests)

state_map <- map_data("state")
head(state_map)
# Digabungkan dengan map data
arrests_map <- left_join(state_map, USArrests, by = "region")

# Create Map Data
ggplot(arrests_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Murder), color = "white") +
  scale_fill_viridis_c(option = "C")

# Some EU Countries
negara.eu <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands", "Italy", "Luxembourg"
)
# retrievethe map data
negara.eu.maps <- map_data("world", region = negara.eu)

# compute the centraid as the mean longitude and latitude
# Used as label cordinate for country's names
region.label <- negara.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(negara.eu.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = region)) +
  geom_text(aes(label = region), data = region.label, size = 3, hjust = 0.5) +
  scale_fill_viridis_d() +
  theme_void() +
  theme(legend.position = "none")

indonesiamaps <- map_data("world", region = "Indonesia")

region.data <- indonesiamaps %>%
  group_by(subregion) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(indonesiamaps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = subregion)) +
  geom_text(aes(label = subregion), data = region.data, size = 3, hjust = 0.5) +
  scale_fill_viridis_d() +
  theme_void() +
  theme(legend.position = "none")

# Distribusi Peluang

# dnorm
dnorm(0) # f(x) untuk dist normal standar mean=0, sd=1
dnorm(0, mean = 4, sd = 10) # f(x) untuk dist normal standar mean=4, sd=10

# pnorm
pnorm(0)
pnorm(1)
pnorm(-1.95)
1 - pnorm(1.64) # f(X>x)

# qnorm
qnorm(0.84134472)
qnorm(0.05)
qnorm(0.975)
qnorm(c(0.975, 0.5)) # mencari lebih dari 1 nilai

# rnorm
# membangkitkan data dari distribusi normal standard
# sejumlah n=1000
x <- rnorm(1000)
head(x)
y <- rnorm(1000, mean = 10, sd = 5)
head(y)

# menggambar distribusi data bangkitan
par(mfrow = c(1, 2))
hist(x, probability = T)
lines(density(x), col = 2, lwd = 2)

hist(y, probability = T)
lines(density(y), col = 2, lwd = 2)

## distribusi data
1 - pnorm(268, mean = 250, sd = 10)
# atau dengan standart normal
1 - pnorm(1.8)

## berada antara 245-260
pnorm(260, mean = 250, sd = 10) - pnorm(245, mean = 250, sd = 10)
# atau dgn Z standard
pnorm(1) - pnorm(-0.5)

## distribusi data
pnorm(20, mean = 16, sd = 3) - pnorm(12, mean = 16, sd = 3)
# atau dgn Z standard
pnorm(1.33333) - pnorm(-1.33333)

# Distribusi t
pt(-3, df = 10)
pt(3, df = 10)
qt(0.05, df = 10)
qt(0.95, df = 10)
z1 <- rt(100, df = 10)
head(z1)
z2 <- rt(100, df = 20)

par(mfrow = c(1, 2))
hist(z1, probability = T)
lines(density(z1), col = 2, lwd = 2)

hist(z2, probability = T)
lines(density(z2), col = 2, lwd = 2)

# Distribusi Binomial
dbinom(6, 10, 0.5) # koin 10x lemparan 6x muncul gambar
pbinom(6, 10, 0.5) # p(x<=6)
qbinom(0.8, 10, 0.5) # kebalikan pbinom

set.seed(12) # untuk mendapat hasil yang sama setiap bangkitan
rbinom(10, 1, 0.5)

set.seed(12)
rbinom(1, 20, 0.5)

set.seed(12)
rbinom(5, 20, 0.5) # 20 lemparan dilakukan sebanyak 5x

1 - pbinom(2, 20, 0.5)

# poisson
dpois(2, lambda = 7.5) # p(x=2)
ppois(2, lambda = 7.5) # p(x<=2)
ppois(5, lambda = 10) # p(x<=5)

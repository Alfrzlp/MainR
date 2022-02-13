# A. Korelasi
data(mtcars)
head(mtcars)
cor(mtcars$mpg, mtcars$hp)
cor.test(mtcars$mpg, mtcars$hp)

# B. Regresi Linier Sederhana
x <- c(40, 55, 32, 55, 50, 52, 61, 44, 30, 22, 40, 64, 58, 48, 44)
y <- c(4, 16, 12, 24, 15, 24, 22, 17, 4, 14, 24, 26, 20, 9, 14)
length(x)
length(y)

plot(x, y)
abline(lm(y ~ x))
lm(y ~ x)

fit <- lm(y ~ x)
summary(fit)

# C. Analisis Residual
layout(matrix(1:4, 2, 2))
plot(fit)

summary(state.x77)
st1 <- data.frame(state.x77)
cor(st1)

moder_reg1 <- lm(Murder ~ Illiteracy, data = st1)
summary(moder_reg1)

# D. Analisis Regresi Berganda
st1 <- data.frame(state.x77)
model_reg1 <- lm(Life.Exp ~ Murder + Population, data = st1)
summary(moder_reg1)

# E. Aplikasi dengan R
# F. Uji Asumsi Regresi Liniear Berganda
#   a. non liniearitas
library(car)
crPlots(model_reg1)
ceresPlots(model_reg1)

#   b. Homoskedastisitas
# Evaluate Homoscedasticity
# non-constat error varians
ncvTest(model_reg1)
library(lmtest)
bptest(model_reg1)

#   c. multikolinearitas
# Evaluate MUltiCollinearty
vif(model_reg1) # variance inflation factors

#   d. normalitas
par <- mfrow()
outlierTest(model_reg1) # Bonferonni p-value for most extreme obs
qqPlot(model_reg1, main = "QQ Plot")

#   e. autokorelasi
durbinWatsonTest(model_reg1)

# Metode pemilihan Model Regresi Terbaik
data("mtcars")
head(mtcars)

# Full model
fit1 <- lm(mpg ~ ., data = mtcars)
summary(fit1)

# backward selection
fit_backward <- step(fit1, direction = "backward", trace = 10)
summary(fit_backward)

# forward selection
fit0 <- lm(mpg ~ 1, data = mtcars)
fit_forward <- step(fit0, scope = list(lower = fit0, upper = fit1), direction = "forward", trace = 10)
summary(fit_forward)

# stepwise selection
fit_stepwise <- step(fit1, direction = "both")
summary(fit_stepwise)

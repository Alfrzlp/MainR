dbinom(0, 16, 0.1)
# seperti tabel walpole tapi tidak kumulatif

df <- data.frame(
  x = c(5.5, 4.8, 4.7, 3.9, 4.5, 6.2, 6, 5.2, 4.7, 4.3, 4.9, 5.4, 5, 6.3, 4.6, 4.3, 5, 5.9, 4.1, 4.7),
  y = c(3.1, 2.3, 3, 1.9, 2.5, 3.7, 3.4, 2.6, 2.8, 1.6, 2, 2.9, 2.3, 3.2, 1.8, 1.4, 2, 3.8, 2.2, 1.5)
)

df <- data.frame(
  x = c(7, 6, 5, 1, 5, 4, 7, 3, 4, 2, 8, 5, 2, 5, 7, 1, 4, 5),
  y = c(97, 86, 78, 10, 75, 62, 101, 39, 53, 33, 118, 65, 25, 71, 105, 17, 49, 68)
)

df <- data.frame(
  x = c(1, 0, 2, 0, 3, 1, 0, 1, 2, 0),
  y = c(16, 9, 17, 12, 22, 13, 8, 15, 19, 11)
)

df <- data.frame(
  x = c(32, 48, 72, 64, 48, 16, 40, 48, 48, 24, 80, 56),
  y = c(230, 262, 323, 298, 255, 199, 248, 279, 267, 214, 359, 305)
)

str <- "3.5 5.3 5.1 5.8 4.2 6 6.8 5.5 3.1 7.2 4.5 4.9
9 20 18 33 31 13 25 30 5 47 25 11
6.1 6.4 7.4 6.7 7.5 5.9 6 4 5.8 8.3 5 6.4
33.2 40.3 38.7 46.8 41.4 37.5 39 40.7 30.1 52.9 38.2 31.8"

str2 <- "8 6.5 6.6 3.7 6.2 7 4 4.5 5.9 5.6 4.8 3.9
23 35 39 21 7 40 35 23 33 27 34 15
7.6 7 5 4.4 5.5 7 6 3.5 4.9 4.3 8 5
43.3 44.1 42.8 33.6 34.2 48 38 35.9 40.4 36.8 45.2 35.1"

df <- rbind(
  read.table(textConnection(str)) %>% t(),
  read.table(textConnection(str2)) %>% t()
) %>%
  as.data.frame() %>%
  remove_rownames() %>%
  `colnames<-`(c("x1", "x2", "x3", "y"))


# brand preference
str <- "4	4	4	4	6	6	6	6	8	8	8	8	10	10	10	10
2	4	2	4	2	4	2	4	2	4	2	4	2	4	2	4
64	73	61	76	72	80	71	83	83	89	86	93	88	95	94	100"

df2 <- read.table(textConnection(str)) %>%
  t() %>%
  as.data.frame() %>%
  `colnames<-`(c("x1", "x2", "y")) %>%
  remove_rownames()

df2
lm(y ~ x1, df2) %>% summary()


f <- lm(y ~ x, df)
f
f2 <- lm(y ~ factor(x), df)
f2


f <- lm(y ~ x1 + x2 + x3, df)
f %>% summary()
f2 <- lm(y ~ factor(x1) + factor(x2), df)
f2

# lack of fit
anova(f, f2)
aov(y ~ x, df) %>% summary()



# (Model regresi yang distandarisasi/dibakukan
lm(scale(y) ~ scale(x), df) %>% summary()
library(QuantPsyc)
lm.beta(lm(y ~ x, df))



# RLB
model2 <- lm(y ~ x1, df2)
model2 %>% summary()
anova(model2)

model <- lm(y ~ x1, df)
model %>% summary()
anova(model)

# matrix
x <- as.matrix(data.frame(1, df$x1, df$x2))
y <- as.matrix(df$y)

x <- as.matrix(data.frame(1, df$x))
y <- as.matrix(df$y)

# =============================================
t(x) %*% x
# (X'X)^-1
solve(t(x) %*% x)
t(x) %*% y
b <- solve(t(x) %*% x) %*% t(x) %*% y
b

n <- nrow(y)
J <- matrix(1, nrow = n, ncol = n)
# Total sum of Square
SSTO <- t(y) %*% y - (1 / n) * t(y) %*% J %*% y
SSTO
# regression sum of square
SSR <- t(b) %*% t(x) %*% y - (1 / n) %*% t(y) %*% J %*% y
SSR
SSE <- SSTO - SSR
SSE

# MSR dan MSE
SSR / (ncol(x) - 1)
SSE / (n - ncol(x))

# F tabel
qf(1 - 0.05, (ncol(x) - 1), (n - ncol(x)))
# p-value
pf(
  (SSR / (ncol(x) - 1)) / (SSE / (n - ncol(x))),
  (ncol(x) - 1), (n - ncol(x)),
  lower.tail = F
)

# matriks varians covarian s^2(b)
vcov(lm(y ~ x1 + x2 + x3, df))
vcov(lm(y ~ x1 + x2 + x3, df)) %>% diag()


confint(lm(y ~ x1 + x2, df2), level = 1 - 0.01 / 4)

# prediksi rata-rata
data <- matrix(c(5.4, 6.2, 6.4, 17, 12, 21, 6, 5.8, 6.1), 3, byrow = T)
data <- xh
predict(model,
  data.frame(t(data)) %>% `colnames<-`(c("x1", "x2", "x3")),
  interval = "confidence", se.fit = T
)

# prediksi observasi baru
hasil <- predict(model,
  data.frame(t(data)) %>% `colnames<-`(c("x1", "x2", "x3")),
  interval = "prediction", se.fit = T,
  level = 0.95
)
hasil$fit
# se ycap new
sqrt(5.75 + hasil$se.fit^2)





xh <- matrix(c(5.4, 6.2, 6.4, 17, 12, 21, 6, 5.8, 6.1), 3, byrow = T)
xh <- matrix(c(50, 152, 182), byrow = T)
xh <- rbind(
  rep(1, ncol(xh)), xh
)
xh

t(xh) %*% b
s2 <- 3.07215 * (t(xh) %*% solve(t(x) %*% x) %*% xh)
s2

s2 <- 5.75 * (1 + t(xh) %*% solve(t(x) %*% x) %*% xh)
s2
sqrt(s2)







library(lmtest)
waldtest.default(lm(y ~ x1 + x2, df))
lmtest::coeftest(lm(y ~ x1 + x2, df))
lmtest::coefci(lm(y ~ x1 + x2, df))

vcov(lm(y ~ x1 + x2 + x3, df))

qt(1 - 0.01 / 4, 13)
pt(1.422, 18 - 2, lower.tail = F)

qf(0.01, 1, 20, lower.tail = F)
pf(1.0445, 2, 11, lower.tail = F)

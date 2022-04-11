s <- '1 3.7 48.5 9.3
2 5.7 65.1 8.0
3 3.8 47.2 10.9
4 3.2 53.2 12.0
5 3.1 55.5 9.7
6 4.6 36.1 7.9
7 2.4 24.8 14.0
8 7.2 33.1 7.6
9 6.7 47.4 8.5
10 5.4 54.1 11.3
11 3.9 36.9 12.7
12 4.5 58.8 12.3
13 3.5 27.8 9.8
14 4.5 40.2 8.4
15 1.5 13.5 10.1
16 8.5 56.4 7.1
17 4.5 71.6 8.2
18 6.5 52.8 10.9
19 4.1 44.1 11.2
20 5.5 40.9 9.4'


# Data --------------------------------------------------------------------
X <- dm::read_string(s)[, -1]
xbar <- matrix(colMeans(X))
S <- var(X)
n <- nrow(X)
p <- ncol(X)
alpha <- 0.05

c2 <- (n - 1) * p * qf(alpha, p, n - p, lower.tail = F) / (n - p)
c2


a <- c(1, 0, 0)
# simultan confidence interval
t(a) %*% xbar - sqrt(c2 * t(a) %*% S %*% a / n)
t(a) %*% xbar + sqrt(c2 * t(a) %*% S %*% a / n)


# bonferoni (lebih pendek)
(ttabel <- qt(1 - alpha / (2 * p), n - 1))
t(a) %*% xbar - ttabel * sqrt(t(a) %*% S %*% a / n)
t(a) %*% xbar + ttabel * sqrt(t(a) %*% S %*% a / n)


s.ci(xbar, S, n, alpha = 0.05)
bon.ci(xbar, S, n)





structure(list(conf.int = bon.ci(xbar, S, n)[1,]), class = "htest")


set.seed(1)
n <- 200
p <- 3
y <- runif(n) > 0.2
y <- y + 1
mu <- matrix(rnorm(2 * p), 2, p)
x <- mu[y, ]
x <- x + matrix(rnorm(n * p), n, p)

library(KLR)

KLRobj <- KLR(y, x, kernel = "polynomial", lambda = 0.001, sigma2 = 2.0, d = 3)
object <- KLR(y, x, "gaussian", lambda = 1.0, sigma2 = 0.5)
contours <- contours.KLR(object, dims = 1:2, res = 100, level = 0.5)
plot(x[, 1], x[, 2], col = y)
sapply(contours, function(c) lines(c$x, c$y, type = "l"))


predict(KLRobj, newdata) # returns probabilities
cv.KLR(y, x, n_folds = 10, lambda = c(0.001, 0.01, 0.1, 1), sigma2 = c(0.1, 0.5, 1.0, 2.0))

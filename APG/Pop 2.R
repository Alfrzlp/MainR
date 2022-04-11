xbar <- matrix(colMeans(dat))
miu <- matrix(c(3.56, 19.75, 75.67))
S <- var(dat)
alpha <- 0.05
n <- nrow(dat)
p <- nrow(xbar)
n
p


# Matrix Varians Populasi diketahui ---------------------------------------
Z2 <- n * t(xbar - miu) %*% Sigma %*% (xbar - miu)
qchisq(0.05, p, lower.tail = F)
# Tolak Ho jika Z^2 > chisq


# Matrix Varians Populasi Tidak Diketahui ---------------------------------
T2 <- n * t(xbar - miu) %*% solve(S) %*% (xbar - miu)
T2

c2 <- (n - 1) * p * qf(0.05, p, n - p, lower.tail = F) / (n - p)
c2
# Nilai T2 > c2 maka Tolak Ho



# Selang Kepercayaan ------------------------------------------------------
s.ci <- function(xbar, sigma, n, alpha = 0.05, adjust = F) {
  p <- ncol(sigma)
  if (n > 30 & adjust) {
    n_kritis <- qchisq(alpha, p, lower.tail = F)
  } else {
    n_kritis <- (n - 1) * p * qf(alpha, p, n - p, lower.tail = F) / (n - p)
  }
  
  data.frame(
    lower = xbar - sqrt(n_kritis) * sqrt(diag(sigma) / n),
    upper = xbar + sqrt(n_kritis) * sqrt(diag(sigma) / n), 
    row.names = paste0('x', 1:p)
  )
}

bon.ci <- function(xbar, sigma, n, alpha = 0.05, adjust = F) {
  p <- ncol(sigma)
  if (n > 30 & adjust) {
    n_kritis <- qnorm(alpha / (2 * p), lower.tail = F)
  } else {
    n_kritis <- qt(alpha / (2 * p), n - 1, lower.tail = F)
  }
  
  data.frame(
    lower = xbar - n_kritis * sqrt(diag(sigma) / n),
    upper = xbar + n_kritis * sqrt(diag(sigma) / n),
    row.names = paste0('x', 1:p)
  )
}


# Package -----------------------------------------------------------------




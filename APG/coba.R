miu <- matrix(c(15, 6, 2.85))
xbar <- matrix(c(28.1, 7.18, 3.09))
S <- matrix(c(140.54, 49.68, 1.94, 49.68, 72.25, 3.68, 1.94, 3.68, 0.25), 3, byrow = T)
n <- 10
p <- nrow(miu)


T2 <- n * t(xbar - miu) %*% S_inv %*% (xbar - miu)
T2

(n - 1) * p * qf(0.05, p, n - p, lower.tail = F) / (n - p)

# Nilai statistik Uji > chisq maka Tolak Ho






miu <- matrix(c(83.1, 86.9, 61.3, 67.6, 77.3))
xbar <- matrix(colMeans(X))
S_inv <- solve(var(X))
n <- nrow(X)
p <- ncol(X)

T2 <- n * t(xbar - miu) %*% S_inv %*% (xbar - miu)
T2

(n - 1) * p * qf(0.05, p, n - p, lower.tail = F) / (n - p)


library(jocre)
jocre::cset(X, "hotelling", 0.05)


styler::style_file(rstudioapi::getActiveDocumentContext()$path)







# Matrix Varians Populasi diketahui ---------------------------------------
Z2 <- n * t(xbar - miu) %*% Sigma %*% (xbar - miu)
qchisq(0.05, p, lower.tail = F)
# Tolak Ho jika Z^2 > chisq


# Matrix Varians Populasi Tidak Diketahui ---------------------------------
T2 <- n * t(xbar - miu) %*% solve(S) %*% (xbar - miu)
T2

n_kritis <- (n - 1) * p * qf(0.05, p, n - p, lower.tail = F) / (n - p)
n_kritis
# Nilai T2 > chisq maka Tolak Ho



# Selang kepercayaan ------------------------------------------------------
i <- 1
# simultan metode hotelling (sample kecil)
c(
  xbar[i] - sqrt(n_kritis) * sqrt(S[i, i] / n),
  xbar[i] + sqrt(n_kritis) * sqrt(S[i, i] / n)
)
# sample besar bisa pakai nilai kritis qchisq(0.05, p)

T.ci <- function(xbar, Sigma, n, alpha = 0.05) {
  p <- ncol(Sigma)
  if (n > 30) {
    n_kritis <- qchisq(alpha, p, lower.tail = F)
  } else {
    n_kritis <- (n - 1) * p * qf(alpha, p, n - p, lower.tail = F) / (n - p)
  }

  data.frame(
    lower = xbar - sqrt(n_kritis) * sqrt(diag(S) / n),
    upper = xbar + sqrt(n_kritis) * sqrt(diag(S) / n)
  )
}


# simultan metode bonferroni
c(
  xbar[i] - qt(0.05 / (2 * p), n - 1, lower.tail = F) * sqrt(S[i, i] / n),
  xbar[i] + qt(0.05 / (2 * p), n - 1, lower.tail = F) * sqrt(S[i, i] / n)
)
c(
  xbar[i] - qnorm(0.05 / (2 * p), lower.tail = F) * sqrt(S[i, i] / n),
  xbar[i] + qnorm(0.05 / (2 * p), lower.tail = F) * sqrt(S[i, i] / n)
)

DescTools::PostHocTest(xbar - miu)



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


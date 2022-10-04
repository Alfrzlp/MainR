library(sae)
library(tidyverse)
sae::eblupFH

data("milk")
glimpse(milk)

dat <- milk
frml <- yi ~ as.factor(MajorArea)
vardir <- milk$SD^2



# package -----------------------------------------------------------------
hasil <- eblupFH(
  dat$yi ~ as.factor(dat$MajorArea),
  method = "ML",
  vardir = vardir
)


# -------------------------------------------------------------------------
df_model <- model.frame(frml, dat, na.action = na.omit)

y <- model.response(df_model)
X <- model.matrix(frml, df_model)

m <- nrow(X)
p <- ncol(X)
Xt <- t(X)


Aest.ML <- 0
Aest.ML[1] <- median(vardir)
k <- 0
result <- list()
MAXITER <- 10
PRECISION <- 1e-04
diff <- PRECISION + 1


while ((diff > PRECISION) & (k < MAXITER)) {
  k <- k + 1
  Vi <- 1/(Aest.ML[k] + vardir)
  XtVi <- t(Vi * X)
  Q <- solve(XtVi %*% X)
  P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
  Py <- P %*% y
  s <- (-0.5) * sum(Vi) + 0.5 * (t(Py) %*% Py)
  F <- 0.5 * sum(Vi^2)
  Aest.ML[k + 1] <- Aest.ML[k] + s/F
  diff <- abs((Aest.ML[k + 1] - Aest.ML[k])/Aest.ML[k])
}

A.ML <- max(Aest.ML[k + 1], 0)
result$fit$iterations <- k
if (k >= MAXITER && diff >= PRECISION) {
  result$fit$convergence <- FALSE
  return(result)
}

Vi <- 1/(A.ML + vardir)
XtVi <- t(Vi * X)
Q <- solve(XtVi %*% X)
beta.ML <- Q %*% XtVi %*% y
varA <- 1/F
std.errorbeta <- sqrt(diag(Q))
tvalue <- beta.ML/std.errorbeta
pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
Xbeta.ML <- X %*% beta.ML
resid <- y - Xbeta.ML
loglike <- (-0.5) * (sum(log(2 * pi * (A.ML + vardir)) + 
                           (resid^2)/(A.ML + vardir)))
AIC <- (-2) * loglike + 2 * (p + 1)
BIC <- (-2) * loglike + (p + 1) * log(m)
goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
coef <- data.frame(beta = beta.ML, std.error = std.errorbeta, 
                   tvalue, pvalue)
variance <- A.ML
EBLUP <- Xbeta.ML + A.ML * Vi * resid





# test --------------------------------------------------------------------
all.equal(EBLUP, hasil$eblup)


library(dplyr)
library(flexsurv)


all_dist <- list("genf", "gengamma", "gengamma.orig", "exp", "weibull", "weibullph", "lnorm", "gamma", "gompertz", "llogis", "exponential", "lognormal")
all_dist <- list("weibull", "llogis", "exponential", "lognormal")


hasil <- lapply(
  all_dist,
  function(x){
    # ganti time, status dan data saja
    x <- flexsurvreg(Surv(time, status) ~ 1,
                     data = dat_final2, dist = x)
    return(AIC(x))
  })

# aic terkecil model terbaik
data.frame(
  dist = unlist(all_dist),
  aic = unlist(hasil)
) %>% 
  arrange(aic) 



# -------------------------------------------------------------------------
# masukkan distribusi dengan aic terkecil
model <- survreg(Surv(time, status) ~ x1 + x2,
                 data = dat_final2, dist = 'lognormal')

summary(model)
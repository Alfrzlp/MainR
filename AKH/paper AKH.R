pak::pak('cuRe')



# -------------------------------------------------------------------------
dat_final %>% 
  drop_na(time, f_tv, internet, proRokok) %>% 
  is.na() %>% 
  colSums()


unique(dat_final$daerah)
unique(dat_final$pend)
unique(dat_final$pend2)
unique(dat_final$kekayaan)
unique(dat_final$internet)
unique(dat_final$f_tv)

table(dat_final$nmprov)


dat_sub <- dat_final %>% 
  dplyr::filter(
    nmprov == 'North Sulawesi'
  )


# clean data --------------------------------------------------------------
dat_final <- dat_final %>% 
  drop_na(time, f_tv, internet, proRokok) %>% 
  mutate(
    psklh = ifelse(pend %in% c('No education'), 'No', 'Yes'),
    status = ifelse(status == 'Yes', 1, 0)
  ) 

glimpse(dat_final)

# Kaplan meier ------------------------------------------------------------
library(survival)
library(survminer)

ggsurvplot(
  survfit(Surv(time, status) ~ 1, data = dat_final),
  conf.int = T,
  ylim = c(0.85, 1),
  xlab = 'Time (Year)'
)




km <- survfit(Surv(time, status) ~ kekayaan,
              data = dat_final)
summary(km)

ggsurvplot(
  km,
  conf.int = F,
  ylim = c(0.8, 1)
)

glimpse(dat_final)
# bekerja
# Log Rank Test -----------------------------------------------------------
obj <- Surv(time, status) ~ bekerja

# logrank
survdiff(obj, data = dat_final)


ggsurvplot(
  survfit(Surv(time, status) ~ bekerja, data = dat_final),
  conf.int = T,
  ylim = c(0.85, 1),
  pval = T,
  pval.coord = c(1, 0.9)
)




ggsurvplot(
  survfit(Surv(time, status) ~ daerah, data = dat_final),
  conf.int = T,
  ylim = c(0.85, 1),
  pval = T,
  pval.coord = c(1, 0.9)
)



ggsurvplot(
  survfit(Surv(time, status) ~ kekayaan, data = dat_final),
  conf.int = T,
  ylim = c(0.85, 1),
  pval = T,
  pval.coord = c(1, 0.9)
)


ggsurvplot(
  survfit(Surv(time, status) ~ proRokok, data = dat_final),
  conf.int = F,
  ylim = c(0.4, 1),
  pval = T,
  pval.coord = c(1, 0.6)
)


ggsurvplot(
  survfit(Surv(time, status) ~ pend3, data = dat_final),
  conf.int = F,
  ylim = c(0.85, 1),
  pval = T,
  pval.coord = c(1, 0.9)
)


ggsurvplot(
  survfit(Surv(time, status) ~ internet, data = dat_final),
  conf.int = F,
  ylim = c(0.8, 1),
  pval = T,
  pval.coord = c(1, 0.9)
)


ggsurvplot(
  survfit(Surv(time, status) ~ pernah_narkoba, data = dat_final),
  conf.int = F,
  ylim = c(0.1, 1),
  pval = T,
  pval.coord = c(1, 0.9)
)


# Cox PH Model ------------------------------------------------------------
mcox <- coxph(Surv(time, status) ~  proRokok + strata(pend3, kekayaan, internet, bekerja), data = dat_final)
summary(mcox)

AIC(mcox)

# Uji Goodness of fit -----------------------------------------------------
test.ph <- cox.zph(mcox)
test.ph






# Model Parametrik --------------------------------------------------------
mexp <- survreg(Surv(time, status) ~ proRokok +
                  kekayaan + internet + bekerja,
                data = dat_final, dist = 'exp')
summary(mexp)
AIC(mexp)



library(flexsurv)
Y_t <- Surv(dat_final$time, dat_final$status)

mexp <- flexsurvreg(Y_t ~ proRokok + daerah + pernah_alkohol + pernah_narkoba + kekayaan,
                    data = dat_final, dist = 'weibull')
plot(mexp)



# Weibull -----------------------------------------------------------------
mwei <- survreg(Surv(time, status) ~ proRokok +
                  kekayaan + bekerja + internet,
                data = dat_final, dist = 'weibull')
summary(mwei)
AIC(mwei)


# Log Logistik ------------------------------------------------------------
mllogis <- survreg(Surv(time, status) ~ proRokok +
                     kekayaan + internet + bekerja,
                data = dat_final, dist = 'loglogistic')
summary(mllogis)


# Log Normal --------------------------------------------------------------
mlnorm <- survreg(Surv(time, status) ~ proRokok +
                    kekayaan + internet + bekerja,
                   data = dat_final, dist = 'lognormal')
summary(mlnorm)







# Hasil -------------------------------------------------------------------
all_model <- list(mcox, mexp, mwei, mllogis, mlnorm) 
nama <- unlist(sapply(all_model, function(x) x$dist))
nama <- c('cox PH', nama)

data.frame(
  model = nama,
  aic = sapply(all_model, AIC),
  loglik = sapply(all_model, logLik)
) %>% 
  arrange(aic)







# -------------------------------------------------------------------------
awal <- survfit(Surv(time, status) ~ 1, data = dat_final)
llog <- flexsurvreg(Surv(time, status) ~ 1, data = dat_final, dist = 'llogis')
wei <- flexsurvreg(Surv(time, status) ~ 1, data = dat_final, dist = 'weibull')

plot(awal, ylim = c(0.85, 1))
lines(lnorm, col = 'blue')
lines(llog)
lines(wei, col = 'green')

library(flexsurv)
lnorm <- flexsurvreg(Surv(time, status) ~ 1, data = dat_final, dist = 'lognormal')
plot(lnorm)

# -------------------------------------------------------------------------


library(cuRe)
cure <- fit.cure.model(Surv(time, status) ~ proRokok + daerah +
                         pernah_alkohol + pernah_narkoba + kekayaan, data = dat_final, 
                      type = "mixture", dist = "weibull", link = "logit")
cure

AIC(cure)





ph_model<-phreg(Yt~dose+clinic+prison,data=mydata,dist="gompertz")
> ph_model

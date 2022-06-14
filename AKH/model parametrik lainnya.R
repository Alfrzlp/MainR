library(survival)


# Data --------------------------------------------------------------------
addicts <- readxl::read_xlsx('D:/_Datasets/addicts.xlsx') %>% rename(time = survt)
head(addicts)
glimpse(addicts)


# Model Log Logistik ------------------------------------------------------
# Model survival yang dapat dibentuk berdasarkan distribusi log-logistik
# adalah Accelerated Failure Time (AFT) Model

# asumsi
# - Survival Odds Ratio (SOR) yang relatif konstan dari waktu ke waktu



# Model Log Normal --------------------------------------------------------

Y_t <- Surv(addicts$time, addicts$status)
Y_t

aft_llog <- survreg(Y_t ~ dose + clinic + prison,
                    data = addicts, dist="loglogistic")
summary(aft_llog)



aft_lnorm <- survreg(Y_t ~ dose + clinic + prison,
                     data = addicts, dist="lognormal")
summary(aft_lnorm)




# cure model --------------------------------------------------------------

library(cuRe)
cure <- fit.cure.model(Surv(time, status) ~ proRokok + daerah +
                         pernah_alkohol + pernah_narkoba + kekayaan, data = dat_final, 
                       type = "mixture", dist = "weibull", link = "logit")
cure

AIC(cure)

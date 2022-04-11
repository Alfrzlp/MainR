library(survival)
library(survminer)

leuk <- read.csv('D:/__SEMESTER 6/AKH/leuk.csv', stringsAsFactors = T)
head(leuk)


cox <- coxph(Surv(waktu, status) ~ treat + logWBC + sex, data = leuk)
# goodness of fit
test.ph <- cox.zph(cox)
test.ph
ggcoxzph(test.ph[3])


scox <- coxph(Surv(waktu, status) ~ treat + logWBC + strata(sex), data = leuk)
summary(scox)

scox_i <- coxph(Surv(waktu, status) ~ treat + logWBC + sex:treat + sex:logWBC + strata(sex), data = leuk)
summary(scox_i)



# Likelihood Ratio Test ---------------------------------------------------
lmtest::lrtest(scox, scox_i)

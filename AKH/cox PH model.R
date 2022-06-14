library(survival)
library(survminer)

leuk <- read.csv('D:/__SEMESTER 6/AKH/leuk.csv', stringsAsFactors = T)
head(leuk)



# Regresi Cox Proporsional Hazard -----------------------------------------
cox <- coxph(Surv(waktu, status) ~ treat + GWBC + sex,
             data = leuk)
cox

summary(cox)
# Nilai Likelihood ratio test merupakan uji simultan




# Uji Asumsi PH -----------------------------------------------------------
# Goodnet of Fit
# Ho: Asumsi PH terpenuhi
# tolak Ho asumsi PH tidak terpenuhi
test.ph <- cox.zph(cox)
test.ph

# plot
ggcoxzph(test.ph)
# plot untuk no 3 saja
ggcoxzph(test.ph[3])





# Stratified Cox Model ----------------------------------------------------
scox <- coxph(Surv(waktu, status) ~ treat + GWBC + strata(sex),
             data = leuk)
scox

summary(scox)






# Hazard Ratio ------------------------------------------------------------
coef(cox)
exp(coef(cox))

# ci
confint(cox)
exp(confint(cox))
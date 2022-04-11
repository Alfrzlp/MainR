# -------------------------------------------------------------------------
s <- '0	21	0	0
1	21	2	0
2	19	2	0
3	17	1	0
4	16	2	0
5	14	2	0
8	12	4	0
11	8	2	0
12	6	2	0
15	4	1	0
17	3	1	0
22	2	1	0
23	1	1	0'


library(purrr)

dm::read_string(s) %>% 
  `colnames<-`(c('tf', 'nf', 'mf', 'qf')) %>% 
  mutate(
    S = (nf - mf)/nf,
    S = accumulate(S, function(prev, new) prev*new)
  )

lrt <- function(lk1, lk2, df1, df2){
  lr <- -2 * lk1 - (-2 * lk2)
  cat('LR :', lr, ' p-value  :', pchisq(lr, df1 - df2, lower.tail = F), '\n')
}

# Kaplan - Meier ----------------------------------------------------------
library(survival)
library(survminer)

leuk <- read.csv('D:/__SEMESTER 6/AKH/leuk.csv', stringsAsFactors = T)
head(leuk)
glimpse(leuk)

levels(leuk$GWBC)
levels(leuk$sex)


km <- survfit(Surv(waktu, status) ~ treat, data = leuk, conf.int = 0.9)
summary(km)

# plot
plot(km)
ggsurvplot(
  km, data = leuk, conf.int = T,
  legend.labs = c('treat = Treatment', 'treat = Placebo'),
  ggtheme = theme_bw()
)


# Uji beda rata2 ----------------------------------------------------------
obj <- Surv(waktu, status) ~ GWBC

# logrank
survdiff(obj, data = leuk)



# Generalized Wilcoxon
library(PHInfiniteEstimates)
gehan.wilcoxon.test(Surv(waktu, status) ~ treat, data = leuk, plot = T)
# gehan = F maka logrank


dt <- dt %>% 
  mutate(
    metode = factor(metode, labels = c(0, 1), levels = c(1, 0))
  )

# Gehan-Breslow and Tarone-Ware
library(coin)
logrank_test(Surv(waktu, status) ~ treat, data = leuk, type = "logrank")
logrank_test(Surv(waktu, status) ~ metode, data = dt, type = "Gehan-Breslow")
logrank_test(Surv(waktu, status) ~ metode, data = dt, type = "Tarone-Ware")



# Regresi Cox Proporsional Hazard -----------------------------------------
cox <- coxph(Surv(waktu, status) ~ treat + logWBC + sex,
             data = leuk %>% 
               mutate(
                 sex = factor(sex, levels = c('male', 'female'))
               ))
cox

cox1 <- coxph(Surv(waktu, status) ~ treat + logWBC + sex:logWBC, data = leuk)
cox1

summary(cox)
lmtest::lrtest(cox, cox1)
# df = df1 - df2

lmtest::lrtest(cox)
# Hazard Ratio ------------------------------------------------------------
coef(cox)
exp(coef(cox))

# ci
confint(cox)
exp(confint(cox))

# interpretasi
# misal 
#    jk 1: laki, 0: perempuan
# kat ref 0: perempuan 
# maka kecenderungan laki2 untuk Y sebesar HR kali dibandingkan
# wanita

# Uji asumsi Propotional Hazard Model -------------------------------------
# yaitu rasio hazardnya tidak bergantung kepada waktu

# 1. Grafik Log-log
fit_treat <- survfit(Surv(waktu, status)~treat, data = leuk)
plot(fit_treat, fun = 'cloglog')


# 2. Goodnet of Fit
# Ho: Asumsi PH tidak terpenuhi
test.ph <- cox.zph(cox)
test.ph

# Ploting schonfield residual
ggcoxzph(test.ph)
ggcoxzph(test.ph[3])
# ada pola linear maka asumsi tidak terpenuhi

# 3. Time dependent Covariate



# Stratified Cox Model ----------------------------------------------------
cox.zph(cox)
ggcoxzph(test.ph)

# ada variabel sex yang tidak memenuhi asumsi PH
# maka alternatifnya pakai Stratified Cox Model
scox <- coxph(Surv(waktu, status) ~ treat + logWBC + strata(sex), data = leuk)
summary(scox)

scox_i <- coxph(Surv(waktu, status) ~ treat + logWBC + sex:treat + sex:logWBC + strata(sex), data = leuk)
summary(scox_i)

leuk %>% 
  dplyr::filter(sex == 'female') %>% 
  coxph(Surv(waktu, status) ~ treat + logWBC, data = .) 


# Likelihood Ratio Test ---------------------------------------------------
# H0: Model 1 lebih baik (tanpa interaksi)
# H1: Model 2 lebih baik
# Df = df1 - df2
# -2 LogLik(M1) - (-2 Loglik(M2))
# pvalue = 1 - pchisq(LR, df)

lmtest::lrtest(scox, scox_i)




# Extended Cox Model ------------------------------------------------------
addicts <- readxl::read_xlsx('D:/_Datasets/addicts.xlsx') %>% rename(time = survt)
head(addicts)
glimpse(addicts)

fit <- coxph(Surv(time, status) ~ clinic + prison + dose, data = addicts)
summary(fit)

# goodnes of fit
cox.zph(fit)
ggcoxzph(cox.zph(fit)[1])




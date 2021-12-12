library(tidyverse)
library(fmsb)

tabkon <- function(x, y, ..., nilai){
  tab <- xtabs(freq ~ .,
               cbind(expand.grid(x, y, ...),
                     freq = nilai)
               
  )
  v <- match.call(expand.dots = F)
  nama <- c(as.character(v[2:3]), as.character(v$...))
  names(dimnames(tab)) <- nama
  
  nama <- c(tail(nama, 1), nama[1:(length(nama)-2)])
  # z x 
  print(ftable(tab, row.vars = nama))
  tab
}

ganja = c('yes', 'no')
rokok = c('yes', 'no')
alkohol = c('yes', 'no')

# input
nilai <- c(911, 538, 44, 456, 3, 43, 2, 279)
# harapan
nilai <- c(911, 44, 538, 456, 3, 2, 43, 279)            

tab <- tabkon(rokok, ganja, alkohol, nilai = nilai)

# odds ratio and relative risk --------------------------------------------

tab <- matrix(c(25, 8, 12, 15), 2, byrow = T)
tab <- matrix(c(15, 5, 40, 30), 2, byrow = T)
tab <- matrix(str2vec('97 307 200 1409'), 2, byrow = T)
tab

prop.test(tab)

or <- fmsb::oddsratio(tab)
or
rr <- fmsb::riskratio(tab[1,1], tab[2,1], sum(tab[1,]), sum(tab[2,])) 
rr

log(or$estimate) - 1.96*sqrt(sum(1/as.vector(tab)))
log(or$conf.int)
sqrt(sum(1/as.vector(tab)))

as.vector(tab)
log(rr$estimate)
log(rr$conf.int)
phi <- tab[,1]/rowSums(tab)
sqrt((1-phi[1])/(phi[1]*colSums(tab)[1]) + 
       (1-phi[2])/(phi[2]*colSums(tab)[2]))

# pearson chisq test & LR test------------------------------------------------------
chisq.test(tab)
chisq.test(tab, correct = F)

# Yates' continuity correction
chisq.test(tab, correct = T)


# likelihood ratio test
library(DescTools)
GTest(tab)

chs <- chisq.test(tab)

2*sum(chs$observed*log(chs$observed/chs$expected))

# koef Gamma --------------------------------------------------------------
library(vcdExtra)

s <- '1 3 10 6
2 3 10 7
1 6 14 12
0 1 9 11'

tab <- read.table(textConnection(s), header = F) %>% 
  as.matrix() 
tab

c_gamma <- GKgamma(tab)
c_gamma$C
c_gamma$D
c_gamma

if(c_gamma$C > c_gamma$D) cat('Hubungan Positif\n')
# Data mentah bisa pakai Hmisc::rcorr.cens(x, y, outx = T)


# coefficient cohen's kappa -----------------------------------------------
# MENGUKUR TINGKAT KESEPAKATAN ATAU KEANDALAN DARI KLASIFIKASI

s <- 
'25 7 5 3 
4 10 4 0
3 2 7 0
3 1 2 4'

tab <- read.table(textConnection(s), header = F) %>% 
  as.matrix()

# pakai unwigthed
# unweighted kappa -> selang
kappa <- psych::cohen.kappa(tab)
kappa



# Breslow-Day Test for Homogeneity of the Odds Ratios ---------------------
# apakah nilai odds ratio semua partial tabel sama (asosiasi homogen)?

# Gagal tolak Ho lanjut CMH
library(DescTools)

tab <- xtabs(freq ~ .,
                  cbind(expand.grid(pelaku  = c("putih", "hitam"),
                                    hukuman_mati  = c("ya", "tidak"),
                                    korban = c("putih", "hitam")),
                        freq = c(53, 11, 414, 37, 0, 4, 16, 139))
)

tab <- xtabs(freq ~ .,
             cbind(expand.grid(pelaku  = c("putih", "hitam"),
                               hukuman_mati = c("ya", "tidak"),
                               korban = c("putih", "hitam")),
                   freq = str2vec('17	10 15	30 19	14 14	11'))
)

tab <- xtabs(freq ~ .,
             cbind(expand.grid(tipe  = c("bunuh diri", "kecelakaan"),
                               luka_parah  = c("ya", "tidak"),
                               lokasi = c("rumah korban", "rumah teman", "lainnya")),
                   freq = c(45, 15, 20, 29, 13, 14, 12, 27, 18, 11, 11, 29))
)
tab

# semua
Desc(xtabs(Freq ~ pelaku + hukuman_mati, tab), plotit = F)

# tabel partial untuk z ke 1
Desc(tab[,, 1], plotit = F)

# tabel partial untuk z ke 2
Desc(tab[,, 2], plotit = F)

# H0: terdapat asosiasi homogen (tidak ada 3-way interaction/association)
# H1: tidak terdapat asosiasi homogen (ada 3-way interaction/association)
BreslowDayTest(tab, correct = F)
BreslowDayTest(tab, correct = TRUE)


# Cochran-Mantel-Haenszel (CMH) test --------------------------------------

# menguji ada/tidaknya conditional independence
# (nilai odds ratio semua partial tabel = 1)

# Gagal Tolak Ho = ada conditional independence
mantelhaen.test(tab)
mantelhaen.test(tab, correct = FALSE)


z = -(log(1.114)/0.554)
exp(-0.717*z - 0.416*z^2)


# GLM ---------------------------------------------------------------------
# Contoh data diatas adalah data kelompok (tabel kontingensi)
data <- 
  data.frame(
    lantai = 1:7,
    mati = c(2, 6, 8, 13, 10, 10, 1),
    jatuh = c(37, 54, 46, 38, 32, 11, 2)
  ) %>% 
  mutate(p = mati/jatuh,
         hidup = jatuh-mati)
data
# cbind sukses gagal
glm(cbind(mati, hidup)~lantai,
    family = binomial(link = 'logit'), data)
# identity
# probit
# cloglog
# log
# cauchit

# gaussian
# Gamma
# inverse.gaussian
# poisson

# cara lain
glm(p ~ lantai, weights = jatuh, 
    family = binomial(link = 'logit'), data)



# Data Non Kelompok / Raw data
# Orang   Lantai   Keadaan
raw_data <- 
  data %>% 
  select(lantai, mati, hidup) %>% 
  pivot_longer(2:3) %>% 
  slice(rep(1:n(), value)) %>% 
  select(-value,  keadaan = name)

raw_data
model_glm <- glm((keadaan == 'mati') ~ lantai, 
                 family = binomial(link = 'logit'), raw_data)

summary(model_glm)


# Loglinear Model ---------------------------------------------------------

# predict yes maka ref = 'no'
dat <- 
  as.data.frame(tab) %>% 
  mutate_at(-4, ~ relevel(.x, ref = 'no')) %>% 
  relocate(3, .before = 1)
  # mutate_at(-ncol(.), ~ fct_relevel(.x, ~ tail(.x, 1)))

library(MASS)
(fit <- loglm(Freq ~ (rokok + ganja + alkohol)^2, dat))


(fit <- glm(Freq ~ rokok + ganja + alkohol,
            data = dat, family = poisson()))

# Model bagus nilai residual deviance mendekati df
(fit <- glm(Freq ~ (rokok + ganja + alkohol)^2,
           data = dat, family = poisson('log')))
summary(fit)
confint(fit)

# p value deviance statistik. (pvaluenya likelihood ratio)
# Ho : data observasi cocok (fit) dg model (tidak ada perbedaan 
# antara estimasi frekuensi harapan dan frekuensi observasi)
pchisq(deviance(fit), df = df.residual(fit), lower.tail = F)


(fit1 <- glm(Freq ~ rokok*ganja*alkohol,
            data = dat, family = poisson('log')))
summary(fit1)
confint(fit1)


# nilai fitted value dekat dengan aslinya
dat %>% 
  mutate(fitted_value = fit$fitted.values) %>% 
  arrange_at(1:3, desc) %>% 
  mutate(res = Freq-fitted_value)


# Ggal Tolak Ho. model 1 lebih baik dari model 2
anova(fit, fit1)
pchisq(anova(fit, fit1)$Deviance[2], df = 1, lower.tail = F)


# ======= interpretasi =======
summary(fit)
# Pada kelompok orang yg mengkonsumsi Marijuana atau tidak,
# orang yg mengkonsumsi alcohol memiliki kecenderungan sebesar
# exp(2,055) = 7,8 kali untuk merokok


# K way and Hinger Order Effect -------------------------------------------
library(vcdExtra)
# Ho : Efek Ordo ke K dan yang lebih tinggi = 0
Kway(Freq ~ smoke + bmi + ecg, data = dat) %>% 
  LRstats()

# K way Effect
# Ho : Efek ordo ke K = 0
Kway(Freq ~ smoke + bmi + ecg, data = dat) %>% 
  anova(test = 'LRT')


# Backward Elimination ----------------------------------------------------
# using AIC
(fit <- MASS::loglm(Freq ~ smoke*bmi*ecg, data = tab))
step(fit, test = 'Chisq')  

backward <- function(model, ...){
  h <- drop1(fit,  test = 'Chisq', ...)
  print(h)
  nama <- c('~.')
  while(sum(h$`Pr(>Chi)` > 0.05, na.rm = T) != 0) {
    nama <- c(nama, rownames(h)[which.max(h$`Pr(>Chi)`)])
    new_formula <- as.formula(paste0(nama, collapse = ' -'))
    h <- drop1(update(fit, new_formula), test = 'Chisq', ...)
    print(h)  
  }
}
fit <- glm(Freq ~ smoke*bmi*ecg, data = tab, family = poisson)

backward(fit)



library(dplyr)
library(survival)
library(flexsurv)


dat <- readxl::read_xlsx('D:/__SEMESTER 6/AKH/Paper AKH/Alkohol_Remaja_Wanita.xlsx')
head(dat)

# mengatur kategori referensi dan urutan
dat <- dat %>% 
  mutate(
    daerah = factor(daerah, levels = c('Perkotaan', 'Perdesaan')),
    proAlkohol = factor(proAlkohol, levels = c("No", "Yes")),
    kekayaan = factor(kekayaan, levels = c('Sangat Miskin', 'Miskin', 'Tidak Miskin', 'Kaya', 'Sangat Kaya')),
    internet = factor(internet, levels = c('Tidak Pernah', 'Minimal 1X sebulan', 'Minimal 1X seminggu', 'Hampir Setiap Hari')),
    pernah_merokok = factor(pernah_merokok, levels = c("No", "Yes")),
    pend = factor(pend, levels = c("Dibawah SMA", "Diatas SMA" ))
  ) 


# pemilihan model
all_dist <- list("genf", "gengamma", "gengamma.orig", "exp", "weibull", "weibullph", "lnorm", "gamma", "gompertz", "llogis", "exponential", "lognormal")

hasil <- lapply(
  all_dist,
  function(x){
    # ganti time, status dan data saja
    x <- flexsurvreg(Surv(time, status) ~ 1, data = dat, dist = x)
    return(AIC(x))
  })

# aic terkecil model terbaik
data.frame(
  dist = unlist(all_dist),
  aic = unlist(hasil)
) %>% 
  arrange(aic) 



# Model Terpilih
model <- survreg(Surv(time, status) ~ pernah_merokok + kekayaan + internet + daerah + proAlkohol + pend,
                 data = dat, dist = 'lognormal')

summary(model)




# viz ---------------------------------------------------------------------
all_model <- lapply(
  all_dist,
  function(x){
    # ganti time, status dan data saja
    x <- flexsurvreg(Surv(time, status) ~ 1, data = dat, dist = x)
    return(x)
  })


y <- all_model[[4]]
str(y)

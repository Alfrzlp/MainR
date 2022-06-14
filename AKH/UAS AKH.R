library(survival)
library(survminer)
library(tidyverse)
library(flexsurv)

# Data --------------------------------------------------------------------
dat <- readxl::read_xlsx('D:/__SEMESTER 6/AKH/UAS AKH/Data_UAS_AKH_2022(1935).xlsx')
glimpse(dat)

dat <- dat %>% 
  setNames(
    c('id', 'umur', 'uk_tmr', 'std_tmr', 'n_simpul_tmr',
      'n_res_horA', 'n_res_horB', 'tgl_awal', 'tgl_akhir', 'jk',
      'terapi', 'status')
  ) %>% 
  mutate(
    std_tmr = factor(std_tmr, levels = 1:3),
    jk = factor(jk, levels = c('Perempuan', 'Laki-laki')),
    terapi = factor(terapi, levels = c('Tidak', 'Ya')),
    
    status = ifelse(status == 'Masih hidup', 0, 1),
    time = as.numeric(tgl_akhir - tgl_awal)
  ) %>% 
  dplyr::select(
    -c(tgl_awal, tgl_akhir)
  )

glimpse(dat)



# FE ----------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    kat_umur  = case_when(
      umur >= 0 & umur < 50 ~ 'dewasa',
      umur >= 50 ~ 'tua'
    )
  )

min(dat$umur)
max(dat$umur)

table(dat$jk)
table(dat$kat_umur)

ggsurvplot(
  survfit(Surv(time, status) ~ kat_umur, data = dat),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.18),
  pval.coord = c(1, x + 0.1),
  xlab = 'Time (days)'
)




# Kaplan meier ------------------------------------------------------------
x <- 0.35
ggsurvplot(
  survfit(Surv(time, status) ~ jk, data = dat),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.18),
  pval.coord = c(1, x + 0.1),
  xlab = 'Time (days)',
  legend.title = 'Jenis Kelamin',
  legend.labs = unique(dat$jk)
)

ggsurvplot(
  survfit(Surv(time, status) ~ terapi, data = dat),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.18),
  pval.coord = c(1, x + 0.1),
  xlab = 'Time (days)',
  legend.title = 'Status Menjalani Terapi Hormon',
  legend.labs = unique(dat$terapi)
)

ggsurvplot(
  survfit(Surv(time, status) ~ std_tmr, data = dat),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.18),
  pval.coord = c(1, x + 0.1),
  xlab = 'Time (days)',
  legend.title = 'Stadium Tumor',
  legend.labs = c(1:3)
)


glimpse(dat)


# Cox PH model ------------------------------------------------------------
mcox <- coxph(Surv(time, status) ~ kat_umur + jk + umur + uk_tmr + std_tmr + n_simpul_tmr + n_res_horA + n_res_horB + terapi, data = dat)
summary(mcox)
AIC(mcox)

# tolak Ho asumsi PH tidak terpenuhi
test.ph <- cox.zph(mcox)
test.ph


# stratified
scox <- coxph(Surv(time, status) ~  uk_tmr + std_tmr + n_simpul_tmr + n_res_horA + strata(jk, terapi, kat_umur), data = dat)
summary(scox)
AIC(scox)

cox.zph(scox)




scox <- coxph(Surv(time, status) ~  kat_umur + jk + umur + uk_tmr + std_tmr + 
                n_simpul_tmr + terapi + strata(n_res_horA, n_res_horB), data = dat)
summary(scox)
AIC(scox)

scox <- coxph(Surv(time, status) ~  (kat_umur + jk + umur + uk_tmr + std_tmr + 
                n_simpul_tmr + terapi) + strata(n_res_horA, n_res_horB), data = dat)



# Model Parametrik --------------------------------------------------------
all_dist <- list("genf", "gengamma", "gengamma.orig", "weibull", "weibullph",
                 "gamma", "gompertz", "llogis", "exponential", "lognormal")

hasil <- lapply(
  all_dist,
  function(x){
    # ganti time, status dan data saja
    x <- flexsurvreg(Surv(time, status) ~ 1, data = dat, dist = x)
    return(AIC(x))
  })

# aic semua model
data.frame(
  dist = unlist(all_dist),
  aic = unlist(hasil)
) %>% 
  arrange(aic) 




# Model Log Normal --------------------------------------------------------
mlnorm <- survreg(Surv(time, status) ~ jk + umur + uk_tmr + std_tmr + n_simpul_tmr + n_res_horA + n_res_horB + terapi,
                  data = dat, dist = 'lognormal')
summary(mlnorm)
AIC(mlnorm)


mlnorm <- survreg(Surv(time, status) ~ uk_tmr + std_tmr + n_simpul_tmr + n_res_horA + terapi,
                  data = dat, dist = 'lognormal')
summary(mlnorm)
AIC(mlnorm)

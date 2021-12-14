library(tidyverse)
library(olsrr)

df <- 
  readxl::read_xlsx('D:/__SEMESTER 5/Metode Penelitian/data.xlsx') %>% 
  `colnames<-`(c('kab', 'ak','tk', 'rb', 'kp')) %>% 
  type_convert() %>% 
  drop_na()

df %>% 
  glimpse()


df <- df %>% 
  mutate(rusak = r + rb)

# missing value -----------------------------------------------------------
library(mice)

anyNA(df)
md.pattern(df)

# perform mice imputation, based on random forests.
miceMod <- mice(df[, !names(df) %in% "kab"])  
df_new <- complete(miceMod)  # generate the completed data.
anyNA(df_new)


df_new <- df %>% 
  mutate_at(3:4, ~scale(.x))

df_new %>% glimpse()

model <- lm(ak ~ rb + sm + kp, df_new) 
model %>% summary()
 

# Amatan berpengaruh ------------------------------------------------------

# cook distance (untuk deteksi outlier juga)
ols_plot_cooksd_bar(model)
ols_plot_cooksd_chart(model)

# DFFITS
ols_plot_dffits(model)

# DFBETAS
ols_plot_dfbetas(model)

# outlier -----------------------------------------------------------------

# studentized residual 
ols_plot_resid_stud(model)
ols_plot_resid_stand(model)

# deleted studentized residual 
ols_plot_resid_stud_fit(model)

car::outlierTest(model)
# baris ke 13 outlier


model <- lm(ak ~ rb + sm + kp, df) 
summary(model)

# Stepwise ----------------------------------------------------------------
library(olsrr)
ols_step_both_p(model)

# Multikolinearitas -------------------------------------------------------
library(car)
vif(model)

# homosketastisitas -------------------------------------------------------
library(lmtest)
bptest(model)

# Normalitas --------------------------------------------------------------
shapiro.test(model$residuals)





# Viz ---------------------------------------------------------------------
mycaption <- expression(italic('Sumber : BPS'))

ggplot(df) +
  geom_col(aes(y = reorder(kab, ak), x = ak, fill = ak)) +
  geom_text(aes(x = ak + 70, y = reorder(kab, ak), label = ak),
            size = 3)+
  geom_point(
    aes(x = ak - 2, y = reorder(kab, ak), color = ak), size = 4) +
  scale_fill_viridis_c(option = 'D') +
  scale_color_viridis_c() +
  labs(
    x = 'Jumlah Kecelakaan Lalu Lintas',
    y = 'Kabupaten / Kota', fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = 'none'
  ) +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(expand = c(0, 0, 0.05, 0))

# 650 600
ggsave(
  'D:/__SEMESTER 5/Metode Penelitian/Blog/ak.png',
  width = 6.5,
  height = 6,
  limitsize = F,
  dpi = 300,
  type = "cairo-png"
)

# total kendaraan bermontor
ggplot(df) +
  geom_col(aes(y = reorder(kab, tk), x = tk, fill = tk)) +
  geom_text(aes(x = tk + 150000, y = reorder(kab, tk), label = tk/1000),
            size = 3)+
  geom_point(aes(x = tk - 2, y = reorder(kab, tk), color = tk), size = 4) +
  scale_fill_viridis_c(option = 'D') +
  scale_color_viridis_c(option = 'D') +
  labs(
    x = 'Jumlah Kendaraan Bermontor',
    y = 'Kabupaten / Kota', fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = 'none'
  ) +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(expand = c(0, 0, 0.05, 0))

# panjang jalan rusak
ggplot(df) +
  geom_col(aes(y = reorder(kab, rb), x = rb, fill = rb)) +
  geom_text(aes(x = rb + 10, y = reorder(kab, rb), label = rb),
            size = 3)+
  geom_point(
    data = df %>% 
      dplyr::filter(
        !rb %in% c(3, 2, 0)
      ),
    inherit.aes = F,
    aes(x = rb - 1, y = reorder(kab, rb), color = rb),
    size = 4
  ) +
  scale_fill_viridis_c(option = 'D') +
  scale_color_viridis_c() +
  labs(
    x = 'Panjang Jalan Rusak Berat (Km)',
    y = 'Kabupaten / Kota', fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = 'none'
  ) +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(expand = c(0, 0, 0.05, 0))

# kepadatan penduduk
ggplot(df) +
  geom_col(
    aes(y = reorder(kab, kp), x = kp, fill = kp)
  ) +
  geom_text(
    aes(x = kp + 700, y = reorder(kab, kp), 
        label = round(kp, 2)),
    size = 3
  )+
  geom_point(aes(x = kp - 2, y = reorder(kab, kp), color = kp), size = 4) +
  scale_fill_viridis_c(option = 'inferno') +
  scale_color_viridis_c(option = 'inferno') +
  labs(
    x = bquote('Kepadatan Penduduk ('*Jiwa/Km^2*')'),
    y = 'Kabupaten / Kota', fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = 'none'
  ) +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(expand = c(0, 0, 0.05, 0))



# nilai vif diuji dulu sebelum masuk
car::vif(lm(ak ~ rb + tk + kp, data = df)) 



# Regrsi Poisson ----------------------------------------------------------
pm <- glm(ak ~ tk + rb + kp, data = df, family = poisson)
summary(pm)


pm$data$ak
pm$fitted.values

chisq.test(rbind(pm$data$ak, pm$fitted.values))
DescTools::GTest(cbind(pm$data$ak, pm$fitted.values))




s <- '(Intercept):1  5.851e+00  1.994e-01  29.342  <2e-16
(Intercept):2 -4.395e+00  1.411e-01 -31.154  <2e-16
tk             2.069e-06  3.550e-07   5.828  5.6e-09
rb            -1.857e-03  9.065e-04  -2.048 0.040516
kp            -1.020e-04  2.754e-05  -3.705 0.000211'

read.table(textConnection(s), header = F) %>% 
  glimpse() %>% 
  copy2c()




# nilai vif diuji dulu sebelum masuk
car::vif(lm(ak ~ rb + sm + kp, data = df))
car::vif(pm)


# jika deviance(pm)/df.residual(pm) > 1 maka overdispersi
deviance(pm)/df.residual(pm)

# H1 : alpha > 0 (kondisi overdispersi model nb lebih baik)
lmtest::lrtest(pm, nbm)

# H1 : rasio dispersi > 1 (mengalami overdispersi)
AER::dispersiontest(pm)



# Negative Binomial Regression --------------------------------------------
library(MASS)
nbm <- glm.nb(ak ~ rb + kp + tk, data = df)
summary(nbm)
nbm <- glm.nb(ak ~ rb + offset(log(p)), data = df)

### uji simultan 
# Tolak Ho : minimal ada 1 variabel yang signifikan
nbm_intercept <- glm.nb(ak ~ 1, data = na.omit(df))

# nilai statistik uji
-2*(logLik(nbm_intercept)[1] - logLik(nbm)[1])
# p-value
pchisq(-2*(logLik(nbm_intercept)[1] - logLik(nbm)[1]), 
       df = df.residual(nbm_intercept)-df.residual(nbm),
       lower.tail = F)

anova(nbm_intercept, nbm)
library(lmtest)
lrtest(nbm_intercept, nbm)

coef(nbm)
(exp(coef(nbm))[-1]-1)*100

summary(pm)


# nilai prediksi adalah rata2nya
newdata = data.frame(rb = 204, 
                     kp = 800,
                     sm = 725000)
predict.glm(nbm, newdata, type = 'response')



# asumsi conditional means are not equal to the conditional variances
pchisq(2 * (logLik(nbm) - logLik(pm)), df = 1, lower.tail = FALSE)
  

# Generalized Poisson Regression ------------------------------------------
library(VGAM)
gpm <- vglm(ak ~ (tk + rb + kp), genpoisson2, data = df, trace = TRUE, model = T)
summary(gpm)
summary(pm)
step4(gpm)



data.frame(
  model = c('Regresi Poisson', 'Negative Binomial', 'Generalized Poisson Regression'),
  aic = c(AIC(pm), AIC(nbm), AIC(gpm)),
  bic = c(BIC(pm), BIC(nbm), BIC(gpm))
)

lmtest::lrtest(pm)


lrtest_vglm(gpm)
fitted.values(gpm)[2]

# hitung fitted value
# miu = exp(b0 + b1*x1 + ....)
(coef(gpm)[1] + sum(select(df[2,], c(tk, rb, kp))*coef(gpm)[3:5]) ) %>% 
  exp
2.07*10^-3



pchisq(deviance(pm), df=df.residual(pm), lower.tail=FALSE)
pchisq(-2*-225.5378, df=61, lower.tail=FALSE)

gpm
# AICc untuk sampel kecil cocok
AICc(gpm)

AIC(gpm)
BIC(gpm)

AIC(nbm)
BIC(nbm)



# Zero Truncated Negative Binomial ----------------------------------------
ztnb <- vglm(ak ~ rb + sm + kp, family = posnegbinomial(), data = df)
summary(ztnb)


ztp <- vglm(ak ~ rb + sm + kp, family = pospoisson(), data = df)
## change in deviance
(dLL <- 2 * (logLik(ztnb) - logLik(ztp)))
## p-value, 1 df---the overdispersion parameter
pchisq(dLL, df = 1, lower.tail = FALSE)
# Based on this, we would conclude that the negative binomial
# model is a better fit to the data.

AIC(ztnb)
BIC(ztnb)


dat <- foreign::read.dta("https://stats.idre.ucla.edu/stat/data/ztp.dta") %>% 
  mutate(
    hmo = factor(hmo),
    died = factor(died)
  )
head(dat)





# Negative Binomial -------------------------------------------------------
dat <- foreign::read.dta("https://stats.idre.ucla.edu/stat/stata/dae/nb_data.dta")
head(dat)
dat <- within(dat, {
  prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})
head(dat)

ggplot(dat, aes(daysabs, fill = prog)) +
  geom_histogram(binwidth = 1) + 
  facet_grid(prog ~ ., margins = TRUE, scales = "free")


# Zero Truncated Negative Binomial ----------------------------------------
dat <- foreign::read.dta("https://stats.idre.ucla.edu/stat/data/ztp.dta") %>% 
  mutate(
    hmo = factor(hmo),
    died = factor(died)
  )
head(dat)
# hmo = punya asuransi kesehatan atau tidak
# age group from 1 to 9

ggplot(dat, aes(stay)) + 
  geom_histogram() + scale_x_log10() +
  # free y agar fleksibel nilai axis y nya 
  facet_grid(hmo ~  died, margins = TRUE, scales = "free_y")
# hmo baris (kanan), died column (atas)

ggplot(dat, aes(factor(age), stay)) +
  geom_violin() +
  geom_jitter(size=1.5) +
  scale_y_log10() +
  stat_smooth(aes(x = age, y = stay, group=1), method="loess")

# lama tinggal tampaknya tidak terlalu bervariasi antar kelompok umur.
# Pengamatan dari data mentah ini dikuatkan oleh garis loess yang relatif datar.


dat %>% 
  mutate(died = relevel(died, 2)) %>% 
  ggplot(., aes(age, fill=died)) +
  geom_histogram(binwidth=.5, position="fill") +
  facet_grid(hmo ~ ., margins=TRUE)

# pada pemilik asuransi kelompok umur muda proporsi yang meningggal
# kecil, namun tidak jauh berbeda pada kelompok umur yang lebih tnggi

m1 <- vglm(stay ~ age + hmo + died, family = posnegbinomial(), data = dat)
summary(m1)
# The number of linear predictors is two, 
# one for the expected mean (lambda) and 
# one for the over dispersion

output <- data.frame(resid = resid(m1)[, 1], fitted = fitted(m1))
ggplot(output, aes(fitted, resid)) + 
  geom_jitter(position = position_jitter(width = 0.25), 
              alpha = 0.5) + 
  stat_smooth(method = "loess")

output <- within(output, {
  broken <- cut(fitted, hist(fitted, plot=FALSE)$breaks)
})
output

ggplot(output, aes(broken, resid)) +
  geom_boxplot() +
  geom_jitter(alpha=.25)

# We can get confidence intervals for the parameters and the exponentiated parameters using bootstrapping
f <- function(data, i, newdata) {
  require(VGAM)
  m <- vglm(formula = stay ~ age + hmo + died, family = posnegbinomial(),
            data = data[i, ], coefstart = c(2.408, 0.569, -0.016, -0.147, -0.218))
  mparams <- as.vector(t(coef(summary(m))[, 1:2]))
  yhat <- predict(m, newdata, type = "response")
  return(c(mparams, yhat))
}

## newdata for prediction
newdata <- expand.grid(age = 1:9, hmo = factor(0:1), died = factor(0:1))
newdata$yhat <- predict(m1, newdata, type = "response")

set.seed(10) %>% 
res <- boot::boot(dat, f, R = 1200, newdata = newdata, parallel = "snow", ncpus = 4)

## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms



## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms

# https://stats.idre.ucla.edu/r/dae/zero-truncated-negative-binomial/
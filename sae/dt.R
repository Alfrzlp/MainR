library(sae)
library(tidyverse)


# -------------------------------------------------------------------------
dat <- foreign::read.spss(
  "D:/_bacaan/SAE/Tahap 1/5. Data Latihan/Data_Indikator_SDGS_latihan_SAE_kabkota.sav",
  to.data.frame = T
)
glimpse(dat)



# Model -------------------------------------------------------------------
mod <- mseFH(y9 ~ jml_sma + jml_pt + Pdesa_angkum + Pdesa_sktm + Pdesa_angkum + Rfas_bank,
             vardir = VarY9, data = dat)
mod$est$eblup
mod$mse



# Plot --------------------------------------------------------------------
data.frame(
  direct = sqrt(dat$VarY9) / dat$Y9,
  eblup = sqrt(mod$mse) / mod$est$eblup[, 1],
  id = 1:length(mod$mse)
) %>% 
  pivot_longer(-c('id'), names_to = "rmse") %>% 
  ggplot(aes(x = id, y = value, col = rmse)) +
  geom_line() +
  scale_color_manual(values = c('red', 'steelblue')) +
  labs(
    y = 'RSE'
  ) +
  theme_bw()





# DT ----------------------------------------------------------------------
library(cartlm)
dt_model <- cartLM(y9 ~ jml_sma + jml_pt + Pdesa_angkum + Pdesa_sktm + Pdesa_angkum + Rfas_bank,
      data = dat, max_depth = 9, min_samples_split = 30, min_samples_leaf = 10)
dt_model

pred <- predict(dt_model, dat, 'lm')
yardstick::rmse(dat, truth = Y9, pred)
yardstick::rmse(dat, truth = Y9, mod$est$eblup[, 1])

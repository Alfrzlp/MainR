library(tidyverse)
options(dplyr.summarise.inform = F)  

# srs ================================================================
attach(data)
n_sampel(jenis_kelamin,
         d = 0.1, alpha = 0.05, type = "wr")

n_sampel(berat_badan, d = 0.1*mean(berat_badan),
         alpha = 0.05, type = "wr")

n_sampel(tinggi_badan, d = 0.1*mean(tinggi_badan),
         alpha = 0.05, type = "wr")

n_sampel(pakai_kacamata,
         d = 0.1, alpha = 0.05, type = "wr")

n_sampel(jumlah_anggota_keluarga, d = 0.1*mean(jumlah_anggota_keluarga), 
         alpha = 0.05, type = "wr")

f = (1 - n/177)

data %>% sample_n(13) %>% 
  summarise(mean = mean(berat_badan),
            "v(ybar)" = var(berat_badan)/n(),
            "se(ybar)" = sqrt(`v(ybar)`),
            "rse(ybar)" = `se(ybar)`*100/mean)


# systematic =========================================================
sys = sample_systematic(data, n = 13, sort_by = berat_badan)

psm(sys$berat_badan, N = 177)
sdm(sys$berat_badan, N = 177)





# stratified =========================================================
stratified(data, strata = kelas, variabel = berat_badan) %>% 
  alokasi.stratified(alokasi = "neyman", daksen = 0.1, alpha = 0.05) %>% 
  mutate(nh = round(nh))
  
sample_stratified(data, strata = kelas, variabel = berat_badan,
                  n_sampel = c(3, 2, 3, 2, 3), metode = "srswor")






# pps ================================================================
library(TeachingSampling)
data_sampel = sample_pps(data, 13, berat_badan, inc_prob = T)
data_sampel

data_sampel = data_sampel %>% 
  select(yi = tinggi_badan, xi = berat_badan, pi, inc_prob)

# HT estimator wor---
E.piPS2(data_sampel$yi, data_sampel$inc_prob)

# desraj ordered estimator
desraj(data_sampel$yi, data_sampel$pi)


# HH estimator wr---
HansenHurwitz(data_sampel$yi, data_sampel$pi)

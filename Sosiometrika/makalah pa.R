s <- 'Aceh 0,71 0,61
Sumatera Utara 1,18 0,97
Sumatera Barat 0,98 0,97
Riau 1,24 0,73
Jambi 0,83 0,79
Sumatera Selatan 1,35 1,07
Bengkulu 1,42 0,95
Lampung 1,42 1,00
Kep. Bangka Belitung 0,66 0,72
Kep. Riau 1,02 1,44
DKI Jakarta 1,15 0,86
Jawa Barat 1,15 0,90
Jawa Tengah 0,80 0,57
DI Yogyakarta 1,35 0,79
Jawa Timur 0,82 0,66
Banten 0,80 0,46
Bali 0,42 0,23
Nusa Tenggara Barat 1,63 1,49
Nusa Tenggara Timur 0,84 0,73
Kalimantan Barat 0,63 0,69
Kalimantan Tengah 0,80 0,55
Kalimantan Selatan 0,84 0,59
Kalimantan Timur 1,19 0,60
Kalimantan Utara 1,53 1,36
Sulawesi Utara 0,78 0,63
Sulawesi Tengah 1,22 1,17
Sulawesi Selatan 0,96 0,73
Sulawesi Tenggara 0,95 0,90
Gorontalo 0,83 0,59
Sulawesi Barat 0,74 0,58
Maluku 1,19 1,09
Maluku Utara 1,16 1,06
Papua Barat 1,66 1,04
Papua 1,36 0,73
INDONESIA 1,01 0,78'

b15 <- '1.75
0.84
0.83
0.77
1.81
1.25
1.99
2.76
1.92
1.00
0.31
1.37
6.38
4.91
7.50
2.12
5.20
12.40
6.69
6.59
0.80
1.55
1.03
3.34
0.21
1.76
7.44
5.00
1.25
6.83
0.58
1.23
2.48
22.10
4.00
'

buta15 <- str2vec(b15)



library(dm)
# data susenas
pkk <- read_pattern(
  s, pos_non_angka = 1,
  pos_angka = 2:3
) %>% 
  separator_convert(v2:v3) %>% 
  select(nmprov = v1, pkk20 = v3)
  
pkk %>% glimpse()

  
  
get_diff(dat$nmprov, str_to_upper(pkk$nmprov))


dat <- readxl::read_xlsx('D:/tugas/dataPA.xlsx') %>% 
  separator_convert(-nmprov) 
dat

dat_final <- pkk %>% 
  mutate(nmprov = str_to_upper(nmprov)) %>% 
  right_join(dat, by = 'nmprov') %>% 
  dplyr::filter(!nmprov %in% c('INDONESIA')) %>% 
  mutate(
    buta15 = buta15[-35],
    buta15 = buta + buta15
  ) %>%
  # dplyr::filter(
  #   nmprov !=  'DKI JAKARTA'
  # ) %>% 
  dplyr::filter(
    !nmprov %in% prov_outlier
  ) %>% 
  mutate_at(vars(-nmprov), ~ as.numeric(scale(.x))) 


glimpse(dat_final)



# Full Model --------------------------------------------------------------
m <- lm(
  p02 ~ tpt_aug + buta + kp, 
  data = dat_final 
) 
m %>% summary()



# Sub Model ---------------------------------------------------------------
msub <- lm(
  tpt_aug ~ kp, 
  data = dat_final
) 

msub %>% summary()


lm(
  tpt_aug ~ kp + laju, 
  data = dat_final 
) %>% 
  summary()


res_m <- get_hasil(tidy(m))
res_msub <- get_hasil(tidy(msub))

# kp
res_m$tpt_aug * res_msub$kp > res_m$kp^2
res_m$tpt_aug * res_msub$kp + res_m$kp^2



# -------------------------------------------------------------------------



prov_outlier <- 
  dat_final %>% 
  slice(c(11, 34)) %>% 
  pull(nmprov)
prov_outlier


dat_final %>% 
  dplyr::filter(nmprov != 'DKI JAKARTA') %>% 
  ggplot(aes(y = tpt_aug, x = kp)) +
  geom_point()






# Path Analysis -----------------------------------------------------------
library(lavaan)
library(semPlot)

mod.id = '
tpt_aug ~ buta15
p02 ~ tpt_aug
pkk20 ~ p02 + tpt_aug + buta15
'


mod.est <- cfa(model = mod.id, data = dat_final)
summary(mod.est, rsquare = T)


semPaths(
  object = mod.est,
  what = "path",
  whatLabels = "par",
  style = "ram",
  layout = "tree",
  residuals = T,
  rotation = 2,
  sizeMan = 7,
  sizeLat = 7,
  color = "lightgray",
  edge.label.cex = 1.2,
  label.cex = 1.3,
  filename = "E:/Visualisasi/tugas/path"
)

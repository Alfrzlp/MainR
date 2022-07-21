# APK yang lebih dari 100 menunjukkan bahwa terdapat penduduk 
# yang bersekolah/berkuliah belum cukup atau melebihi umur yang seharusnya

# APK pt = jml pddk yg sedang bersekola pada jenjang PT / jumlaℎ penduduk@usia 19−23 taℎun

# usia             : 19 <= R407 <= 23
# masih bersekolah : R612 = 2
# jenjang pendidikan : 15 <= R613 <= 20


# -------------------------------------------------------------------------
library(tidyverse)
library(survey)

dat <- foreign::read.spss('D:/_bacaan/allsae/data/KOR18SUMUT.sav', to.data.frame = T)
glimpse(dat)


dat <- dat %>% 
  mutate(
    id = 1:nrow(dat),
    sekolahPT = ifelse(
      (R612 == "Masih bersekolah") & (R613 %in% levels(dat$R613)[15:20]),
      1, 0
    ),
    sekolahPT = ifelse(is.na(sekolahPT), 0, sekolahPT),
    usiaPT = ifelse(R407 > 18 & R407 < 24, 1, 0)
    # usiaPT2 = ifelse(between(R407, 19, 23), 1, 0),
  ) 


# Direct Estimation -------------------------------------------------------
design2 <- svydesign(id = ~id, weights = ~FWT, data = dat)

## estimasi APK perguruan tinggi per kabupate/kota ##
est_apk_kab <- svyby(
  formula = ~ sekolahPT,
  denominator = ~usiaPT,
  ~R102,
  design = design2,
  svyratio, 
  vartype = c("se","ci","ci","cv","cvpct","var")
)

est_apk_kab$APK <- est_apk_kab$`sekolahPT/usiaPT`*100

## estimasi jumlah yang masih bersekolah di jenjang perguruan tinggi ##
est_apk1 <- svyby(
  formula = ~sekolahPT,
  ~R102, design = design2, 
  svytotal, 
  vartype=c("se","ci","ci","cv","cvpct","var")
)


## estimasi jumlah yang berusia 19 sampai 23 tahun ##
est_apk2 <- svyby(
  formula = ~usiaPT,
  ~R102, design = design2,
  svytotal,
  vartype = c("se","ci","ci","cv","cvpct","var")
)

est_apk1$sekolahPT / est_apk2$usiaPT

sum(est_apk1$sekolahPT) / sum(est_apk2$usiaPT)
# 0.3110745
head(est_apk_kab)

#write.csv(est_apk_kabu,"D:\\pelatihan SAE\\est_apk_kabu.csv")










# Rata2 Lama Sekolah ------------------------------------------------------

# umur > 15 tahun  : R407 ≥ 15
dat2 <- foreign::read.spss('D:/_bacaan/allsae/data/KOR18GABW.sav', to.data.frame = T)
glimpse(dat2)

levels(dat2$R615)
lm_sekolah <- c(0, 6, 9, 12, 14, 15, 16, 17, 19, 22)

dat2 <- dat2 %>% 
  dplyr::filter(R407 >= 15) %>% 
  mutate(
    ijazah = case_when(
      R615 == "Tidak punya ijazah SD" ~ 0,
      R615 %in% c("Paket A", "SDLB", "SD", "MI") ~ 6,
      R615 %in% c("Paket B", "SMPLB", "SMP", "MTS") ~ 9,
      R615 %in% c("Paket C", "SMLB", "SMA", "MA", "SMK", "MAK") ~ 12,
      R615 == "D1/D2" ~ 14,
      R615 == "D3" ~ 15,
      R615 == "D4" ~ 16, 
      R615 == "S1" ~ 17,
      R615 == "S2" ~ 19,
      R615 == "S3" ~ 22,
      is.na(R615) ~ 0 
    ),
    lm_sklh = ifelse(R407 >= 15, ijazah, 0)
  ) %>% 
  rowid_to_column('id')




  

library(sf)
library(tidyverse)
tidymodels::tidymodels_prefer()


# Data --------------------------------------------------------------------
jakarta <- st_read('E:/peta/DKIJakarta_kel/DKIJakarta_kel.shp')
jakarta

dat <- readxl::read_xlsx('D:/Downloads/Standar Kelurahan Data Corona (21 Juni 2022 pukul 15.00).xlsx')
dat %>% glimpse()

dat <- dat %>% 
  select(
    nmkel = nama_kelurahan,
    positif = POSITIF,
    sembuh = Sembuh,
    meninggal = 'Meninggal...31',
    isoman = `Self Isolation`) %>% 
  drop_na(nmkel) %>% 
  mutate(nmkel = str_to_title(nmkel))



# Join --------------------------------------------------------------------
jakarta %>% 
  left_join(dat, by = c('FIRST_NAMO' = 'nmkel')) %>% 
  write_sf('E:/peta/latihan/jakarta.geojson')

round(2 * (1 - pt(abs(tvals), enp)), 3)





# -------------------------------------------------------------------------
hasil <- read_csv('E:/peta/pert14/latihan/latihan_listwise.csv')
glimpse(hasil)



# Tolak Ho maka model GWR lebih baik dari model global.
pf(2.846203, 6.788, 26.212, lower.tail = F)

2 * (1 - pt(abs(-1.44), 150 - 1))

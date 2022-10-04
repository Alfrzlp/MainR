library(tidyverse)
library(sf)

# data --------------------------------------------------------------------
dat <- readxl::read_xlsx(
  'D:/__Skripsi/Raw data Final Ubinan 2020.xlsx',
  sheet = 'Padi Eligible Olah'
)
glimpse(dat)

sort(unique(dat$bulan))
unique(dat$subround)
table(dat$r114k)

df_sub <- dat %>% 
  select(
    id = id_ubinan_subs,
    strata,
    tahun,
    bulan = bulan_panen,
    prov,
    kab,
    kec,
    desa,
    lhn = r601,
    uk = r602,
    luas = r603,
    benih = r609,
    tnm = r114k,
    long = x, lat = y,
    y = ubinan_ku_ha,
    yk = ubinan_ku_ha_konversi
  ) %>% 
  drop_na(c(lat, long)) %>% 
  mutate(
    lat = str_replace_all(lat, ',', '\\.'),
    lat = str_remove_all(lat, '\\s'),
    lat = as.numeric(lat)
  )

dim(df_sub)
glimpse(df_sub)

unique(df_sub$uk_ubinan)
unique(df_sub$luas)

table(df_sub$bulan)
table(df_sub$jenis)


# data valid dan tidak valid
valid <- df_sub %>% 
  filter(
    between(long, 94, 142),
    between(lat, -12, 7)
  ) 

tidak_valid <- df_sub %>% 
  filter(!id %in% valid$id)

dim(tidak_valid)
view(tidak_valid)



# Presisi -----------------------------------------------------------------
get_precision <- function(x){
  nchar(str_extract(x, pattern = '(?<=\\.).*'))
}

valid$long_prec <- get_precision(valid$long)
valid$lat_prec <- get_precision(valid$lat)

glimpse(valid)


id_bagus <- valid %>% 
  rowwise() %>% 
  mutate(
    min_prec = min(long_prec, lat_prec)
  ) %>% 
  filter(
    min_prec >= 6,
    tnm %in% 1:2,
    prov == 35
  ) %>% 
  glimpse() %>% 
  pull(id)



# Data final --------------------------------------------------------------
df_final <- valid %>% 
  st_as_sf(coords = c('long', 'lat')) %>% 
  st_set_crs(4326) 

write_sf(df_final, "D:/__Skripsi/vector/ubinan/ubinan.shp")
# select by location lalu import, untuk membersihkan
# data yang tidak benar (misal di laut, dinegara lain dll)
df_final <- st_read('D:/__Skripsi/full_padi_fix/full_padi_fix.shp')

glimpse(df_final)


table(df_final$bulan)
table(df_final$jenis)
table(df_final$strata)
dim(df_final)


df_final %>% 
  mutate(
    pcr = st_precision(geometry)
  )

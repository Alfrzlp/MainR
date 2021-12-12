library(indonesia)
library(tidyverse)

dki <- id_map("jakarta", "kelurahan")

library(jsonlite) 
crim <- fromJSON("https://ramdayz.carto.com/api/v2/sql?q=select%20*,%20st_x(the_geom)%20as%20lon,%20st_y(the_geom)%20as%20lat%20from%20public.jakarta_streetcrime") 
crim <- as.data.frame(crim) 

crim <- crim %>% 
  select(lon=rows.lon, lat=rows.lat, address=rows.nama_jalan, n=rows.jumlah)
crim

#mengubah data.frame menjadi sf data frame
library(sf)
crim <- st_as_sf(crim, coords = c("lon", "lat"))

#digabung
st_crs(crim) <- st_crs(dki)
crim_final = st_join(dki, crim, left=F)

crim_final <- crim_final %>% 
  select(kelurahan = nama_kelurahan, kecamatam = nama_kecamatan, n) %>% 
  group_by(kelurahan, kecamatam) %>% summarise(n = sum(n))

library(ggplot2)
#rect background belakang

ggplot(crim_final)+
  geom_sf(aes(fill=n), color="gray75") +
  theme_void() + theme(legend.position = c(0,0),
                       legend.justification = c(0,0))+
  scale_fill_viridis_c("N")

  

library(sf)
library(dplyr)

jatim <- st_read('E:/peta/pert13/Kab35.shp')
dat <- read.csv('E:/peta/pert13/PekerjaMiskin2019.csv')

dat <- dat %>% 
  mutate(idkab = as.character(idkab))

jatim <- jatim %>% 
  left_join(
    dplyr::select(dat, -nmkab),
    by = 'idkab'
  ) 

write_sf(jatim, 'E:/peta/pert13/jatim_join/jatim.shp')


# -------------------------------------------------------------------------
library(sf)

jatim <- st_read('E:/peta/pert13/jatim.geojson')
jatim %>% 
  glimpse()


library(rgeoda)
library(spdep)


# Univariate Moran's I ----------------------------------------------------
w_gab <- poly2nb(jatim)
ww_gab <- nb2listw(w_gab)

str(w_gab)
str(ww_gab)

moranI <- moran(jatim$Ymajid, ww_gab,
                n = length(ww_gab$neighbours),
                S0 = Szero(ww_gab))
moranI

# moran test
moran.test(jatim$Ymajid, listw = ww_gab)


jatim %>% 
  mutate(idkab = as.character(idkab)) %>% 
  st_write('E:/peta/pert13/jatim/jatim.shp')

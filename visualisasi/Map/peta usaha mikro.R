library(rgdal)
library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)


jum <- 
  readxl::read_xlsx('D:/Downloads/Jumlah Usaha Mikro.xlsx') %>% 
  janitor::clean_names() %>% 
  mutate(
    nama_provinsi = str_to_upper(nama_provinsi)
  )
jum


indo <- st_read('D:/__SEMESTER 6/SIG/Data Praktikum/Data Administrasi', 'Administrasi_Provinsi')
indo <- indo %>% 
  mutate(PROVINSI = if_else(PROVINSI == 'DAERAH ISTIMEWA YOGYAKARTA', 'DI YOGYAKARTA', PROVINSI)) %>% 
  left_join(jum, by = c('PROVINSI' = 'nama_provinsi'))

  
  
# ada di a tapi gaada di b
setdiff(
  unique(indo$PROVINSI),
  unique(jum$nama_provinsi)
)



glimpse(indo)



rosm::osm.types()

ggplot(indo) +
  annotation_map_tile(
    'cartolight',
    zoom = 5,
    cachedir = 'cahcedir_base'
  ) +
  geom_sf(aes(fill = jumlah_usaha_mikro), color = 'white') +
  theme_void() +
  labs(
    title = '  Persebaran Rumah Tangga Usaha Mikro di Indonesia',
    fill = 'Jumlah Usaha Mikro',
    x = NULL, y = NULL
  ) +
  scale_fill_gradientn(colours = c("lightskyblue3", "dodgerblue3", "dodgerblue4")) +
  guides(fill = guide_colorbar(title.position = "top",
                             label.position = 'bottom')) +
  theme(
    plot.title = element_text(size = rel(1.5), face = "bold", margin = margin(0, 0, 10, 10), family = 'sans-serif'),
    legend.position = c(0.15, 0),
    legend.direction = 'horizontal',
    legend.box.just = 'bottom',
    legend.key.height = unit(2, 'mm'),
    legend.key.width = unit(11, 'mm')
  ) 

ggsave(
  filename = "E:/Visualisasi/peta_RTusahamikro_withbase.png",
  width = 10,
  height = 5,
  dpi = 300,
  bg = 'white',
  type = "cairo-png"
)

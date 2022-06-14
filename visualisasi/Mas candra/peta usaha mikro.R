remotes::install_github('alfrzlp/package-dm')

library(ggspatial)
library(ggplot2)
library(dplyr)
library(sf)

jum <- 
  readxl::read_xlsx('D:/Downloads/Jumlah Usaha Mikro.xlsx') %>% 
  janitor::clean_names() %>% 
  mutate(
    nama_provinsi = str_to_upper(nama_provinsi)
  ) %>% 
  tidyr::drop_na()


indo <- st_read('D:/__SEMESTER 6/SIG/Data Praktikum/Data Administrasi', 'Administrasi_Provinsi')
indo <- indo %>% 
  mutate(PROVINSI = if_else(PROVINSI == 'DAERAH ISTIMEWA YOGYAKARTA', 'DI YOGYAKARTA', PROVINSI)) %>% 
  left_join(jum, by = c('PROVINSI' = 'nama_provinsi'))

  

dm::get_diff(
  unique(indo$PROVINSI),
  unique(jum$nama_provinsi)
)


glimpse(indo)
rosm::osm.types()


ggplot(indo) +
  # annotation_map_tile(
  #   'cartolight',
  #   zoom = 5,
  #   cachedir = 'cahcedir_base'
  # ) +
  geom_sf(aes(fill = jumlah_usaha_mikro), color = 'white') +
  theme_void() +
  labs(
    title = '   Persebaran Rumah Tangga Usaha Mikro di Indonesia',
    fill = 'Jumlah Rumah Tangga Usaha Mikro',
    x = NULL, y = NULL
  ) +
  scale_fill_gradientn(colours = c("lightskyblue3", "dodgerblue3", "dodgerblue4")) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0,
    label.position = 'bottom')
  ) +
  theme(
    legend.title = element_text(size = 10),
    plot.title = element_text(size = rel(1.5), face = "bold", margin = margin(0, 0, 10, 10), family = 'Arial'),
    legend.position = c(0.15, 0.1),
    legend.direction = 'horizontal',
    legend.box.just = 'bottom',
    legend.background = element_blank(),
    legend.key.height = unit(2, 'mm'),
    legend.key.width = unit(11, 'mm')
  ) 




ggsave(
  filename = "E:/Visualisasi/rev/peta_RTusahamikro_nobase.png",
  width = 10,
  height = 5,
  dpi = 300,
  bg = 'white',
  type = "cairo-png"
)

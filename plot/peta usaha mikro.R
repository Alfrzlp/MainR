jum <- 
  readxl::read_xlsx('D:/Downloads/Jumlah Usaha Mikro.xlsx') %>% 
  janitor::clean_names() %>% 
  mutate(
    nama_provinsi = str_to_title(nama_provinsi)
  )
jum

library(rgdal)

indo <- readOGR(dsn = "D:/_Datasets/gadm36_IDN_shp", layer = "gadm36_IDN_1")
indo@data
summary(indo@data)


indonesia <- fortify(indo, region = "NAME_1")
indonesia <- indonesia %>% 
  left_join(jum, by = c('id'='nama_provinsi'))

setdiff(
  unique(indonesia$id),
  unique(jum$nama_provinsi)
)



glimpse(indonesia)

library(ggplot2)
ggplot(indonesia, aes(x = long, y = lat)) +
  geom_map(
    map = indonesia, aes(map_id = id, fill = jumlah_usaha_mikro),
    color = "white", size = 0.5
  ) +
  theme_void() +
  coord_sf() +
  labs(
    title = '  Persebaran Usaha Mikro di Indonesia',
    fill = 'Jumlah Usaha Mikro',
    x = NULL, y = NULL
  ) +
  scale_fill_gradientn(colours = c("lightskyblue3", "dodgerblue3", "dodgerblue4")) +
  guides(fill = guide_colorbar(title.position = "top",
                             label.position = 'bottom')) +
  theme(
    plot.title = element_text(size = rel(1.5), face = "bold", margin = margin(0, 0, 10, 10)),
    legend.position = c(0.15, 0),
    legend.direction = 'horizontal',
    legend.box.just = 'bottom',
    legend.key.height = unit(2, 'mm'),
    legend.key.width = unit(11, 'mm')
  ) 

ggsave2(
  filename = "E:/Visualisasi/peta2.png",
  width = 10,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)
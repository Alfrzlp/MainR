library(dm) 

dat <- read_pattern(
  'Boalemo 88,35 100,24 102,37 97,01 97,08
  Kab. Gorontalo 93,58 94,98 93,81 92,67 94,84
  Pohuwato 95,63 99,02 99,94 94,33 93,86
  Bone Bolango 88,27 93,82 97,97 94,48 95,43
  Gorontalo Utara 98,16 99,76 99,14 94,81 95,8
  Kota Gorontalo 92,99 91,31 91,85 91,52 93,45',
  pos_non_angka = 1,
  pos_angka = 2:6
) 

dat <- dat %>% 
  rename(kab = v1) %>% 
  koma_to_titik(vars(contains('v'))) %>% 
  type_convert()
  
  

koma_to_titik <- function(dat, columns) {
  dplyr::mutate_at(
    dat, columns,
    ~ gsub(',', '\\.', .)
  )
}


library(sf)
library(ggrepel)


gorontalo <- 
  st_read("D:/_Datasets/gadm36_IDN_shp", 'gadm36_IDN_2') %>% 
  dplyr::filter(NAME_1 == 'Gorontalo') %>% 
  mutate(
    NAME_2 = if_else(NAME_2 == 'Gorontalo', 'Kab. Gorontalo', NAME_2)
  ) %>% 
  left_join(
    dat, by = c('NAME_2' = 'kab')
  )

gorontalo %>% 
  ggplot(aes(fill = v6)) +
  geom_sf(color = 'white') +
  # geom_sf_text(
  #   data = ~.x %>% 
  #     dplyr::filter(NAME_2 != 'Danau Limboto'),
  #   aes(label = NAME_2)
  # ) +
  geom_label_repel(
    data = cent,
    aes(
      label = stringi::stri_pad_left(str_wrap(paste(kab, ikk), 15), 10, use_length = T),
      x = X, y = Y
    ), 
    hjust = 0,
    xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    seed = 1,
    direction = 'y',
    box.padding = 1,
    inherit.aes = F,
    max.overlaps = 7
  ) +
  geom_point(
    data = cent,
    aes(
      x = X, y = Y
    ),
    color = 'red',
    inherit.aes = F
  ) +
  scale_fill_gradient(
    'IKK',
    low = 'skyblue',
    high = 'skyblue4'
  ) +
  theme_minimal() +
  labs(
    x = 'Longitude',
    y = 'Latitude',
    title = 'Provinsi Gorontalo'
  ) +
  coord_sf(clip = 'off')


stringi::stri_pad_left(str_wrap(paste(kab, ikk), 15))


cent <- 
  st_coordinates(st_centroid(gorontalo)) %>% 
  as_tibble() %>% 
  mutate(
    kab = gorontalo$NAME_2,
    ikk = gorontalo$v6
  ) %>% 
  dplyr::filter(kab != 'Danau Limboto')



ggsave(
  filename = "E:/Visualisasi/gorontalo_ikk.png",
  width = 6,
  height = 4,
  units = "in",
  dpi = 350,
  scale = 1.5,
  bg = 'white'
)

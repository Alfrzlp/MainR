library(ggrepel)
library(ggspatial)

# Data --------------------------------------------------------------------
gini_ratio <- 
  openxlsx::read.xlsx('D:/Downloads/Rasio Gini Menurut Kabupaten_Kota.xlsx') %>% 
  type_convert() %>% 
  setNames(c('kab', 't2019', 't2020', 't2021'))

head(gini_ratio)
glimpse(gini_ratio)


df_gr <- gini_ratio %>% 
  mutate(
    p2020 = t2020 - t2019,
    p2021 = t2021 - t2020
  ) %>% 
  pivot_longer(starts_with('t'), names_to = 'tahun', values_to = 'gini') %>% 
  group_by(tahun) %>% 
  arrange(tahun, gini) %>% 
  mutate(urutan = 1:30) %>% 
  ungroup() %>% 
  mutate(
    urutan = ifelse(tahun == 't2021', urutan, 0)
  )

df_gr %>% 
  arrange(kab)

df_gr %>% 
  filter(
    tahun == 't2021',
    kab != 'Provinsi Papua'
  ) %>% 
  summarise(
    status = gini >= 0.35
  ) %>% 
  count(status)

# Viz ---------------------------------------------------------------------
new_name <- c(
  't2019' = '2019',
  't2020' = '2020',
  't2021' = '2021'
)

df_gr %>% 
  filter(
    kab != 'Provinsi Papua'
  ) %>% 
  mutate(
    kat = case_when(
      gini >= 0.35 ~ 'Sedang',
      gini < 0.35 ~ 'Rendah'
    ),
    kat = factor(kat, levels = c('Rendah', 'Sedang'))
  ) %>% 
  ggplot(aes(x = gini, y = reorder(kab, urutan), fill = kat)) +
  geom_col(width = 0.7) +
  geom_text(
    data = ~ .x %>% 
      distinct(kab, .keep_all = T) %>% 
      pivot_longer(starts_with('p'), names_to = 'perubahan') %>% 
      mutate(
        tahun = str_replace_all(perubahan, 'p', 't'),
        warna_text = case_when(
          round(value, 2) > 0 ~ 'green',
          round(value, 2) < 0 ~ 'red',
          round(value, 2) == 0 ~ 'gray'
        ),
        warna_text = factor(warna_text, levels = c('green', 'red', 'gray'))
      ),
    aes(
      x = 0.45,
      color = warna_text,
      label = paste(scales::comma(value, accuracy = 0.01), '  ')
    ),
    fontface = 2,
    nudge_x = 0.06,
    hjust = 0.6,
    size = 3
  ) +
  scale_fill_manual(
    values = c('#20AC4B', 'lightgoldenrod')
  ) +
  scale_color_manual(
    values = c('red', '#20AC4B', 'gray')
  ) +
  guides(
    color = 'none'
  ) + 
  labs(
    title = 'Perubahan Gini Ratio Tahun 2019-2021',
    subtitle = 'Provinsi Papua',
    x = 'Gini Ratio',
    y = 'Kabupaten/Kota',
    fill = 'Gini ratio'
  ) +
  theme(
    # panel.background = element_rect(fill = "white", colour = "black")
  ) +
  theme_bw() +
  facet_wrap(~tahun, labeller = as_labeller(new_name)) 




# Save --------------------------------------------------------------------
ggsave(
  filename = "E:/Visualisasi/perubahan_giniratio_Papua.png",
  width = 10,
  height = 7,
  units = "in",
  dpi = 500,
  scale = 0.85,
  bg = "white"
)



# boxplot -----------------------------------------------------------------
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

df_gr %>% 
  filter(
    kab != 'Provinsi Papua'
  ) %>% 
  group_by(tahun) %>% 
  mutate(
    outlier = if_else(is_outlier(gini), T, F)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(y = gini)) +
  geom_boxplot() +
  geom_text_repel(
    data = ~ .x %>% filter(outlier == T),
    aes(x = 0,  label = kab),
    color = 'red',
    size = 3.5
  ) +
  # geom_jitter(aes(x = 0), width = 0.1) +
  labs(
    title = 'Perubahan Gini Ratio Tahun 2019-2021',
    subtitle = 'Provinsi Papua',
    y = 'Gini Ratio',
    x = NULL
  ) +
  scale_x_continuous(
    breaks = NULL
  ) +
  facet_wrap(~tahun, labeller = as_labeller(new_name))




ggsave(
  filename = "E:/Visualisasi/boxplot_giniratio_Papua.png",
  width = 7,
  height = 3,
  units = "in",
  dpi = 300,
  scale = 1,
  bg = "white"
)




# Viz 2021 ----------------------------------------------------------------
gini_papua <- df_gr %>% 
  filter(
    tahun == 't2021',
    kab == 'Provinsi Papua'
  ) %>% 
  pull(gini)

gini_papua


df_gr %>% 
  filter(
    tahun == 't2021',
    kab != 'Provinsi Papua'
  ) %>% 
  ggplot(aes(x = gini - 0.011, y = reorder(kab, urutan), fill = gini)) +
  geom_col(
    width = 0.7
  ) +
  geom_point(
    aes(color = gini),
    size = 3.5
  ) +
  geom_vline(
    xintercept = gini_papua,
    color = 'red'
  ) +
  geom_text(
    aes(label = scales::comma(gini, accuracy = 0.01)),
    nudge_x = 0.03,
    size = 3,
    color = 'black'
  ) +
  annotate(
    geom = "text",
    x = 0.4005, y = 'Tolikara',
    label = paste0('Gini Ratio Prov Papua : ', gini_papua),
    color = "red",
    angle = 90,
    vjust = 1,
    size = 3,
    fontface = 2
  ) +
  scale_fill_gradientn(
    colours = c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F')
  ) +
  scale_color_gradientn(
    colours = c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F')
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = 'Gini Ratio Provinsi Papua Tahun 2021',
    x = 'Gini Ratio',
    y = 'Kabupaten / Kota',
    fill = 'Gini ratio'
  ) +
  scale_x_continuous(expand = c(0, 0, 0.05, 0))


ggsave(
  filename = "E:/Visualisasi/giniratio_Papua2021.png",
  width = 7,
  height = 7,
  units = "in",
  dpi = 300,
  scale = 0.9,
  bg = "white"
)



# Map ---------------------------------------------------------------------
library(sf)
nama_kab <- unique(df_gr$kab)
nama_kab <- nama_kab[nama_kab != "Provinsi Papua"]

  
indo <- st_read('D:/_Datasets/gadm36_IDN_shp', 'gadm36_IDN_2')
indo
papua <- indo %>% 
  dplyr::filter(NAME_1 == 'Papua') %>% 
  left_join(gini_ratio[-30,], by = c('NAME_2' = 'kab')) %>% 
  mutate(
    kat = case_when(
      t2021 >= 0.35 ~ 'Sedang',
      t2021 < 0.35 ~ 'Rendah'
    ),
    kat = factor(kat, levels = c('Rendah', 'Sedang'))
  )

papua <- papua %>% 
  left_join(df_miskin, by = c('NAME_2' = 'kab'))


br <- classInt::classIntervals(papua$p0, n = 5, style = 'jenks')$br
br
papua <- papua %>% 
  mutate(
    kat = cut(p0, breaks = br, include.lowest = TRUE)
  )



# ada di a gak ada di b
get_diff <- function(x, y){
  data.frame(
    x = setdiff(unique(x), unique(y)),
    y = setdiff(unique(y), unique(x))
  )
}


get_diff(papua$NAME_2, df_miskin$kab)


ggplot(papua) +
  annotation_map_tile(
    'cartolight',
    zoom = 7,
    cachedir = 'cahcedir_base'
  ) +
  geom_sf(aes(fill = kat), color = 'white') +
  # scale_fill_manual(
  #   values = c('#7AC27F', 'lightgoldenrod')
  # ) +
  scale_fill_manual(
    values = c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA'),
    labels = function(x) str_replace_all(x, ',', ' - ') %>% str_remove_all('\\(|\\]|\\[')
  ) +
  # scale_fill_gradientn(
  #   colours = c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F')
  # ) +
  labs(
    title = 'Persentase Penduduk Miskin Tahun 2021',
    subtitle = 'Provinsi Papua',
    x = 'Longitude',
    y = 'Latitude',
    fill = str_wrap('Persentase Penduduk Miskin (%)', 10)
  ) +
  coord_sf(xlim = c(132, 141.3))

library(patchwork)
p1 + p2

ggsave(
  filename = "E:/Visualisasi/Peta_p0_Papua2021.png",
  width = 7.5,
  height = 5,
  units = "in",
  dpi = 300,
  scale = 1.1,
  bg = "white"
)








# P0123 -------------------------------------------------------------------
s <- '1 Merauke 23,83 10,16 1,46 0,31 387.443
2 Jayawijaya 78,18 37,09 12,89 5,59 501.075
3 Jayapura 15,56 12,13 2,42 0,70 624.845
4 Nabire 35,08 23,83 8,41 4,14 665.325
5 Kepulauan Yapen 25,67 26,09 4,85 1,48 688.204
6 Biak Numfor 36,38 24,45 6,01 2,05 602.931
7 Paniai 62,57 36,59 4,60 0,79 556.861
8 Puncak Jaya 46,39 36,00 12,40 5,58 708.658
9 Mimika 30,95 14,17 2,53 0,69 936.862
10 Boven Digoel 13,88 19,90 3,67 0,97 520.245
11 Mappi 26,36 26,05 4,46 1,17 375.922
12 Asmat 25,04 24,83 4,08 0,91 405.368
13 Yahukimo 73,62 37,64 7,38 2,08 466.113
14 Pegunungan Bintang 23,38 30,46 3,69 0,70 632.210
15 Tolikara 48,16 32,60 3,34 0,48 441.994
16 Sarmi 5,56 13,84 2,33 0,59 554.491
17 Keerom 9,30 16,00 3,16 0,95 704.998
18 Waropen 9,69 29,85 4,72 1,20 731.467
19 Supiori 7,83 37,91 8,45 2,40 498.615
20 Mamberamo Raya 7,04 28,78 3,53 0,91 769.848
21 Nduga 41,17 37,18 11,89 5,42 392.990
22 Lanny Jaya 76,75 38,73 14,87 8,12 537.322
23 Mamberamo Tengah 19,66 36,76 6,51 1,41 449.409
24 Yalimo 22,32 33,25 13,86 7,28 378.167
25 Puncak 40,78 36,26 9,92 3,49 721.541
26 Dogiyai 28,38 28,81 3,68 0,78 557.009
27 Intan Jaya 21,31 41,66 5,92 1,05 725.106
28 Deiyai 30,83 40,59 3,79 0,67 627.742
29 Kota Jayapura 34,79 11,39 2,53 0,79 1.051.297'


df_miskin <- 
  dm::read_pattern(
    s, pos_non_angka = 2,
    pos_angka = c(1, 3:7)
  ) %>% 
  mutate(
    across(v2:6, ~ str_replace_all(.x, ',', '\\.')),
    v7 = str_remove_all(v7, '\\.')
  ) %>% 
  type_convert() %>% 
  dplyr::select(
    kab = v2,
    jpm = v3,
    p0 = v4,
    p1 = v5,
    p2 = v6,
    gk = v7
  ) %>% 
  as_tibble()

options(pillar.sigfig = 5)

df_miskin %>% 
  dplyr::filter(p0 %in% c(min(p0), max(p0))) 


df_miskin %>% 
  filter(p1 %in% c(min(p1), max(p1))) 

df_miskin %>% 
  filter(p2 %in% c(min(p2), max(p2)))   

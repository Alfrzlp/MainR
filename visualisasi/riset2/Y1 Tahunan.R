s <- 'JATILUHUR	-0.88356	4.81925
SUKASARI	9.80719	4.438125
MANIIS	1.150598	7.923
TEGAL WARU	1.322994	3.732125
PLERED	0.936331	3.451875
SUKATANI	4.399134	5.212
DARANGDAN	10.16974	3.21275
BOJONG	6.426949	2.4805
WANAYASA	11.68566	3.093875
KIARAPEDES	9.445228	2.670375
PASAWAHAN	-1.62544	4.768625
PONDOK SALAM	3.125934	3.822625
PURWAKARTA	-0.46413	5.58775
BABAKANCIKAO	4.960842	4.654375
CAMPAKA	8.303075	3.421
CIBATU	3.53856	3.532625
BUNGURSARI	9.936338	3.171
RONGGA	-7.26978	8.10275
GUNUNGHALU	-9.07288	9.29525
SINDANGKERTA	-12.3154	9.2065
CILILIN	-7.81577	8.302375
CIHAMPELAS	-4.88749	5.782375
CIPONGKOR	-12.9709	8.78125
BATUJAJAR	-1.1337	6.44175
SAGULING	-10.4932	9.543125
CIPATAT	9.727657	5.645125
PADALARANG	0.701724	5.556125
NGAMPRAH	-13.905	7.352375
PARONGPONG	-9.68605	5.983
LEMBANG	-5.23066	5.9885
CISARUA	-8.89465	6.079625
CIKALONG WETAN	4.997101	4.833
CIPEUNDEUY	6.781785	6.598875'


laju <- read_pattern(
  s, pos_non_angka = 1,
  pos_angka = 2:3
) %>%
  select(nmkec = v1, y1 = v3) %>%
  type_convert()


gab <- gab %>%
  left_join(laju, by = 'nmkec') %>%
  rename(y1_tahunan = y1)


# gab %>%
#   write_sf('E:/CitraRiset2/gab_rev2.geojson')

library(dm)
library(spdep)
library(sf)
library(ggrepel)
library(rgeoda)


my_col_green <- c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')
my_col <- c('#8b510a', '#e1c27b', '#F6E8C1', "#7ccdc1", "#2f9790")
# baru
my_col <- c('#b36d17', '#e1c27b', '#F6E8C1', "#7ccdc1", "#2f9790")
my_col_lisa <- c('gray70', 'tomato', 'skyblue3', 'orange', 'steelblue', 'black', 'black')

# Data --------------------------------------------------------------------
gab <- st_read('E:/CitraRiset2/gab_rev2.geojson')
head(gab)
batas_kab <- st_read('E:/CitraRiset2/batas_kabBBPWK.geojson')

dat <- readxl::read_xlsx('E:/CitraRiset2/dat_moran.xlsx')
dat


# Peta Tematik ------------------------------------------------------------


ggplot(data = gab) +
  geom_sf(
    aes(fill = Y1_thn_nb), color = "white",
    size = 0.3
  ) +
  geom_sf(
    data = batas_kab,
    color = 'gray30',
    fill = 'transparent',
    size = 0.55,
    inherit.aes = F
  ) +
  geom_label_repel(
    data = subset(gab, X_cen < 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ),
    xlim = c(107, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 0,
    box.padding = 0.17,
    nudge_x = 107 - subset(gab, X_cen < 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e4,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 3 # n control point
  ) +
  geom_label_repel(
    data = subset(gab, X_cen >= 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ),
    xlim = c(-Inf, 107.9), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 1,
    box.padding = 0.17,
    nudge_x = 107 + subset(gab, X_cen >= 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e5,
    max.time = 1,
    segment.angle = 90,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 3 # n control point
  ) +
  geom_point(
    aes(
      x = X_cen, y = Y_cen
    ),
    color = 'black',
    inherit.aes = F,
    size = 0.7
  ) +
  scale_fill_manual(
    # values = c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')
    values = rev(my_col_green)
  ) +
  # scale_fill_brewer(
  #   palette = 'RdYlGn',
  # ) +
  labs(
    title = 'Persentase Laju Alih Fungsi Lahan Sawah Tahunan',
    # title = 'Peta Klasifikasi Laju Perubahan Lahan Sawah Tahunan',
    subtitle = 'Kabupaten Bandung Barat dan Kabupaten Purwakarta Tahun 2013-2021',
    # fill = str_wrap('Kelas Laju Perubahan Lahan Sawah Tahunan Natural Break (%)', 25),
    fill = str_wrap('Kelas Persentase Laju Alih Fungsi Lahan Sawah Tahunan Natural Break (%)', 25),
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(xlim = c(106.99, 107.9), ylim = c(-6.3, -7.2))



ggsave(
  filename = "E:/Visualisasi/riset/Y1_tahunan/y1_tematik.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)
#

# Void --------------------------------------------------------------------
ggplot(data = gab) +
  geom_sf(
    aes(fill = Y1_thn_nb), color = "white",
    size = 0.45
  ) +
  geom_sf(
    data = batas_kab,
    color = 'gray30',
    fill = 'transparent',
    size = 0.55,
    inherit.aes = F
  ) +
  # geom_label_repel(
  #   data = subset(gab, X_cen < 107.45),
  #   aes(
  #     label = str_wrap(nmkec, 15),
  #     x = X_cen, y = Y_cen
  #   ),
  #   xlim = c(107, Inf), ylim = c(-Inf, Inf),
  #   min.segment.length = 0,
  #   size = 2,
  #   seed = 1,
  #   hjust = 0,
  #   box.padding = 0.17,
  #   nudge_x = 107 - subset(gab, X_cen < 107.45)$X_cen,
  #   inherit.aes = F,
  #   max.overlaps = Inf,
  #   max.iter = 1e4,
  #   max.time = 1,
  #   segment.size = 0.15,
  #   segment.curvature = -1e-20, # biar sharp angle
  #   segment.ncp = 3 # n control point
  # ) +
  # geom_label_repel(
  #   data = subset(gab, X_cen >= 107.45),
  #   aes(
  #     label = str_wrap(nmkec, 15),
  #     x = X_cen, y = Y_cen
  #   ),
  #   xlim = c(-Inf, 107.9), ylim = c(-Inf, Inf),
  #   min.segment.length = 0,
  #   size = 2,
  #   seed = 1,
  #   hjust = 1,
  #   box.padding = 0.17,
  #   nudge_x = 107 + subset(gab, X_cen >= 107.45)$X_cen,
  #   inherit.aes = F,
  #   max.overlaps = Inf,
  #   max.iter = 1e5,
  #   max.time = 1,
  #   segment.angle = 90,
  #   segment.size = 0.15,
  #   segment.curvature = -1e-20, # biar sharp angle
  #   segment.ncp = 3 # n control point
  # ) +
  # geom_point(
  #   aes(
  #     x = X_cen, y = Y_cen
  #   ),
  #   color = 'black',
  #   inherit.aes = F,
  #   size = 0.7
  # ) +
  scale_fill_manual(
    # values = c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')
    values = rev(my_col_green)
  ) +
  theme_void() +
  theme(
    legend.position = 'none'
  ) +
  coord_sf(xlim = c(106.99, 107.9), ylim = c(-6.3, -7.2))


ggsave(
  filename = "E:/Visualisasi/riset/Y1_tahunan/y1_tematik_void.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 1
)


# Peta LISA ---------------------------------------------------------------
queen_w <- queen_weights(gab)

# calculate LISA as per GEODA
lisaY1 <- local_moran(queen_w, gab["Y1"])
lisaY2 <- local_moran(queen_w, gab["Y2"])
lisaY3 <- local_moran(queen_w, gab["Y3"])
lisaY1_thn <- local_moran(queen_w, gab["y1_tahunan"])

label <- lisaY1$GetLabels()

gab <- gab %>%
  mutate(
    cluster1 = factor(lisaY1$GetClusterIndicators(), levels = 0:6, labels = label),
    cluster2 = factor(lisaY2$GetClusterIndicators(), levels = 0:6, labels = label),
    cluster3 = factor(lisaY3$GetClusterIndicators(), levels = 0:6, labels = label),
    cluster4 = factor(lisaY1_thn$GetClusterIndicators(), levels = 0:6, labels = label)
  )

gab



# Y1
ggplot(data = gab) +
  geom_sf(
    aes(fill = cluster4),
    color = "white",
    size = 0.15
  ) +
  geom_sf(
    data = batas_kab,
    color = 'gray30',
    fill = 'transparent',
    size = 0.5,
    inherit.aes = F
  ) +
  geom_point(
    data = ~.x %>%
      dplyr::filter(!cluster4 %in% c('Not significant')),
    aes(
      x = X_cen, y = Y_cen
    ),
    color = 'black',
    inherit.aes = F,
    size = 1
  ) +
  geom_label_repel(
    data = ~.x %>%
      dplyr::filter(
        !cluster4 %in% c('Not significant')
      ),
    aes(
      label = str_wrap(str_to_title(nmkec), 15),
      x = X_cen, y = Y_cen
    ),
    force = 0.5,
    xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    seed = 1,
    direction = 'both',
    box.padding = 0.9,
    inherit.aes = F,
    max.overlaps = Inf,
    size = 3,
    max.iter = 1e4,
    max.time = 1,
    segment.curvature = -1e-20,
    segment.ncp = 5
  ) +
  scale_fill_manual(
    values = c('gray75', 'tomato', 'dodgerblue3', 'orange', 'steelblue', 'black', 'black'),
    # values = c('gray', "red", 'navy', 'orange', 'skyblue3','black', 'black'),
    labels = function(x) str_wrap(x, 10)
  ) +
  labs(
    # title = "Peta Klaster Lisa Laju Perubahan Lahan Sawah Tahunan",
    title = "Peta Klaster LISA Persentase Laju Alih Fungsi Lahan Sawah Tahunan",
    subtitle = 'Kabupaten Bandung Barat dan Kabupaten Purwakarta',
    x = 'Longitude', y = 'Latitude', fill = 'cluster'
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(xlim = c(107.1, 107.79), ylim = c(-6.39, -7.15))


ggsave(
  filename = "E:/Visualisasi/riset/Y1_tahunan/y1_LISA.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)


# Moran SC ----------------------------------------------------------------
# dat %>%
#   openxlsx::write.xlsx('E:/CitraRiset2/dat_moran.xlsx')
lm(y4 ~ x4, data = dat) %>%
  coef()

dat %>%
  # ganti ini
  ggplot(aes(x = x4, y = y4)) +
  geom_point(aes(col = cluster4), size = 2.5) +

  geom_vline(xintercept = 0, color = "gray60", lty = 5) +
  geom_hline(yintercept = 0, color = "gray60", lty = 5) +
  geom_smooth(method = "lm", se = F, color = "maroon") +

  geom_text_repel(
    # ganti ini
    aes(color = cluster4, label = str_to_title(nmkec)),
    size = 3,
    min.segment.length = 0,
    seed = 0,
    max.time = 1,
    max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
    box.padding = 0.4, # additional padding around each text label
    max.overlaps = Inf,
    direction = 'both',
    segment.curvature = -1e-20,
    segment.size = 0.15,
    segment.ncp = 3,
  ) +
  scale_color_manual(
    values = c("red", "orange", "skyblue3", "navy")
  ) +
  coord_cartesian(clip = 'off', xlim = c(-2, 2)) +
  labs(
    y = "Spatial Lag",
    x = "Laju Alih Fungsi Lahan",
    # title = "Scatterplot Moran's I Laju Perubahan Lahan Sawah Tahunan",
    title = str_wrap(
      "Scatterplot Moran's I Persentase Laju Alih Fungsi Lahan Sawah Tahunan",
      65
    ),
    subtitle = paste0("Moran's I: ", round(gab_moran4$I, 4))
  ) +
  theme_bw(base_family = 'Arial') +
  theme(
    legend.position = "none",
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30'),
    plot.margin = margin(5.5, r = 15, 5.5, 5.5, "points")
  )





ggsave(
  filename = "E:/Visualisasi/riset/Y1_tahunan/y1_moran_.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 0.9,
  bg = "white"
)


# Some custom -------------------------------------------------------------
library(dm)
my_conflict_prefer()


lokus <- c('Babakancikao', 'Pasawahan', 'Pondok Salam',
           'Purwakarta', 'Tegal Waru',

           'Padalarang', 'Batujajar', 'Cipongkor',
           'Ngamprah', 'Saguling'
)


gab_lokus <- gab %>%
  select(nmkab, nmkec, X_cen, Y_cen) %>%
  mutate(
    nmkec = str_to_title(nmkec),
    lokus = ifelse(nmkec %in% lokus, 'yes', 'no')
  )



ggplot(data = gab_lokus) +
  geom_sf(
    aes(fill = lokus), color = "white",
    size = 0.3
  ) +
  geom_sf(
    data = batas_kab,
    color = 'gray30',
    fill = 'transparent',
    size = 0.55,
    inherit.aes = F
  ) +
  geom_label_repel(
    data = ~ .x %>%
      filter(
        X_cen < 107.45 & lokus == 'yes'
      ),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ),
    xlim = c(107, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 3,
    seed = 1,
    hjust = 0,
    box.padding = 0.17,
    nudge_x = 107.1 - subset(gab_lokus, X_cen < 107.45 & lokus == 'yes')$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e4,
    max.time = 1,
    segment.size = 0.35,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 3 # n control point
  ) +
  geom_label_repel(
    data = ~ .x %>%
      filter(
        X_cen >= 107.45 & lokus == 'yes'
      ),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ),
    xlim = c(-Inf, 107.8), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 3,
    seed = 1,
    hjust = 1,
    box.padding = 0.17,
    nudge_y = -0.03,
    nudge_x = 108 + subset(gab_lokus, X_cen >= 107.45 & lokus == 'yes')$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e5,
    max.time = 1,
    segment.angle = 90,
    segment.size = 0.35,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 3 # n control point
  ) +
  geom_point(
    data = ~ .x %>%
      filter(lokus == 'yes'),
    aes(
      x = X_cen, y = Y_cen
    ),
    color = 'black',
    inherit.aes = F,
    size = 0.7
  ) +
  scale_fill_manual(
    # values = c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')
    values = c('gray85', my_col_green[2])
  ) +
  # scale_fill_brewer(
  #   palette = 'RdYlGn',
  # ) +
  labs(
    title = 'Lokus PKL Riset 2',
    # title = 'Peta Klasifikasi Laju Perubahan Lahan Sawah Tahunan',
    subtitle = 'Kabupaten Bandung Barat dan Kabupaten Purwakarta',
    # fill = str_wrap('Kelas Laju Perubahan Lahan Sawah Tahunan Natural Break (%)', 25),
    # fill = str_wrap('Kelas Persentase Laju Alih Fungsi Lahan Sawah Tahunan Natural Break (%)', 25),
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(xlim = c(107.1, 107.8), ylim = c(-6.4, -7.15))




ggsave(
  filename = "E:/Visualisasi/riset/Y1_tahunan/lokus_v1.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)

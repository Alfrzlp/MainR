library(sf)
library(ggrepel)
library(tidyverse)
library(classInt)

windowsFonts(
  poppins = windowsFont('poppins'),
  tnr = windowsFont('Times New Roman')
)
lab_tokomma <- function(y_lab, arah = 'S'){
  y_lab <- str_replace_all(y_lab, '\\.', ',')
  y_lab <- str_glue('{y_lab}°{arah}')
  return(y_lab)
}


# Data --------------------------------------------------------------------
gab <- st_read('D:/__Datasets/Riset2/gab_rev2.geojson') %>% 
  mutate(nmkec = str_to_title(nmkec)) %>% 
  select(nmkab, nmkec, Y1, X_cen, Y_cen)
head(gab)

batas_kab <- st_read('D:/__Datasets/Riset2/batas_kabBBPWK.geojson')
bb <- gab %>% dplyr::filter(nmkab == 'Bandung Barat')
pwk <- gab %>% dplyr::filter(nmkab == 'Purwakarta')

# st_write(bb, 'E:/peta/bb.geojson')
# st_write(pwk, 'E:/peta/pwk.geojson')

# color -------------------------------------------------------------------
my_col_green <- c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')
my_col <- c('#8b510a', '#e1c27b', '#F6E8C1', "#7ccdc1", "#2f9790")
my_col2 <- c('#20AC4B', '#7AC27F', '#FFFFB5', "#FFBC80", "#E40017")

# my_col2 <- c('#20AC4B', '#7AC27F', '#FFFFB5', "#f5ab73", "#E40017")
my_col2 <- c('#20AC4B', '#A4D29F', '#FFFFB5', "#f7a05e", "#E40017")
my_col2 <- rev(my_col2)
# 
# Natural Breaks ----------------------------------------------------------
br_bb <- classIntervals(bb$Y1, n = 5, style = 'jenks')$br
br_pwk <- classIntervals(pwk$Y1, n = 5, style = 'jenks')$br

br_bb <- c(44.449, 51.534, 64.822, 73.652)
br_pwk <- c(24.751, 35.505, 41.696, 63.384)

get_labelBr <- function(br){
  br <- scales::dollar(br, decimal.mark = ',', big.mark = '.', prefix = '')
  c(
    str_glue('< {br[1]}'),
    str_glue('[{br[1]}; {br[2]})'),
    str_glue('[{br[2]}; {br[3]})'),
    str_glue('[{br[3]}; {br[4]})'),
    str_glue('>= {br[4]}')
  )
}

get_labelBr(br_bb)

bb <- bb %>% 
  mutate(
    Y1 = round(Y1, 3),
    Y1_nb = case_when(
      Y1 < br_bb[1] ~ 1,
      Y1 >= br_bb[1] & Y1 < br_bb[2] ~ 2,
      Y1 >= br_bb[2] & Y1 < br_bb[3] ~ 3,
      Y1 >= br_bb[3] & Y1 < br_bb[4] ~ 4,
      Y1 >= br_bb[4] ~ 5
    ),
    Y1_nb = factor(Y1_nb, levels = 1:5, labels = get_labelBr(br_bb))
  )

pwk <- pwk %>% 
  mutate(
    Y1 = round(Y1, 3),
    Y1_nb = case_when(
      Y1 < br_pwk[1] ~ 1,
      Y1 >= br_pwk[1] & Y1 < br_pwk[2] ~ 2,
      Y1 >= br_pwk[2] & Y1 < br_pwk[3] ~ 3,
      Y1 >= br_pwk[3] & Y1 < br_pwk[4] ~ 4,
      Y1 >= br_pwk[4] ~ 5
    ),
    Y1_nb = factor(Y1_nb, levels = 1:5, labels = get_labelBr(br_pwk))
  )


pwk$Y1_nb
bb$Y1_nb



# NB versi R --------------------------------------------------------------
br_bbr <- classIntervals(bb$Y1, n = 4, style = 'jenks')$br
br_pwkr <- classIntervals(pwk$Y1, n = 4, style = 'jenks')$br


br_bbr <- c(0, br_bbr)
br_pwkr <- c(0, br_pwkr)


bb <- bb %>% 
  mutate(
    Y1_nbr = cut(Y1, breaks = br_bbr, include.lowest = TRUE)
  )

pwk <- pwk %>% 
  mutate(
    Y1_nbr = cut(Y1, breaks = br_pwkr, include.lowest = TRUE)
  )


# viz pwk ---------------------------------------------------------------------
my_family <- 'tnr'
ggplot(data = pwk) +
  geom_sf(
    aes(fill = Y1_nb), color = "white",
    size = 0.3
  ) +
  geom_sf(
    data = dplyr::filter(batas_kab, nmkab == 'Purwakarta'),
    color = 'gray30',
    fill = 'transparent',
    size = 0.55,
    inherit.aes = F
  ) +
  geom_label_repel(
    data = ~ subset(.x, X_cen < 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    family = my_family,
    xlim = c(107, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 0,
    box.padding = 0.17,
    nudge_x = 107 - subset(pwk, X_cen < 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e4,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 3 # n control point
  ) +
  geom_label_repel(
    data = ~ subset(.x, X_cen >= 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    family = my_family,
    xlim = c(-Inf, 107.9), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 1, 
    box.padding = 0.17,
    nudge_x = 107 + subset(pwk, X_cen >= 107.45)$X_cen,
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
    values = rev(my_col2)[1:3]
  ) +
  # scale_fill_brewer(
  #   palette = 'RdYlGn',
  # ) +
  labs(
    title = 'Laju Alih Fungsi Lahan Tahun 2013-2021',
    # title = 'Peta Klasifikasi Laju Perubahan Lahan Sawah Tahunan',
    subtitle = 'Kabupaten Purwakarta',
    # fill = str_wrap('Kelas Laju Perubahan Lahan Sawah Tahunan Natural Break (%)', 25), 
    fill = str_wrap('Kelas Laju Alih Fungsi Lahan Natural Break (%)', 25),
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_bw(base_family = my_family) +
  theme(
    plot.title = element_text(face = 2, vjust = 0),
    plot.subtitle = element_text(colour = 'gray30', vjust = 0),
    axis.text.y = element_text(margin = margin(l = 0))
  ) +
  coord_sf(xlim = c(106.99, 107.9), ylim = c(-6.33, -6.83)) +
  scale_x_continuous(
    labels = lab_tokomma(seq(107, 107.8, by = 0.2), 'E')
  ) +
  scale_y_continuous(
    labels = lab_tokomma(seq(6.8, 6.4, by = -0.1))
  )



ggsave(
  # filename = "E:/Visualisasi/riset/perkab/warna dibalik/pwk.png",
  filename = "E:/Visualisasi/riset/revisi rah/peta/new/laju_pwk_tnr.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)
#


# viz bb ------------------------------------------------------------------
ggplot(data = bb) +
  geom_sf(
    aes(fill = Y1_nb), color = "white",
    size = 0.3
  ) +
  geom_sf(
    data =  dplyr::filter(batas_kab, nmkab == 'Bandung Barat'),
    color = 'gray30',
    fill = 'transparent',
    size = 0.55,
    inherit.aes = F
  ) +
  geom_label_repel(
    data = ~ subset(.x, X_cen < 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    family = my_family,
    xlim = c(107, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 0,
    box.padding = 0.17,
    nudge_x = 107 - subset(bb, X_cen < 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e4,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 3 # n control point
  ) +
  geom_label_repel(
    data = ~ subset(.x, X_cen >= 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    family = my_family,
    xlim = c(-Inf, 107.9), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 1,
    box.padding = 0.17,
    nudge_x = 107 + subset(bb, X_cen >= 107.45)$X_cen,
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
    values = rev(my_col2)
  ) +
  # scale_fill_brewer(
  #   palette = 'RdYlGn',
  # ) +
  labs(
    title = 'Laju Alih Fungsi Lahan Tahun 2013-2021',
    # title = 'Peta Klasifikasi Laju Perubahan Lahan Sawah Tahunan',
    subtitle = 'Kabupaten Bandung Barat',
    # fill = str_wrap('Kelas Laju Perubahan Lahan Sawah Tahunan Natural Break (%)', 25), 
    fill = str_wrap('Kelas Laju Alih Fungsi Lahan Natural Break (%)', 25),
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_bw(base_family = my_family) +
  theme(
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(xlim = c(106.99, 107.9), ylim = c(-6.65, -7.15)) +
  scale_x_continuous(
    labels = lab_tokomma(seq(107, 107.8, by = 0.2), 'E')
  ) +
  scale_y_continuous(
    labels = lab_tokomma(seq(7.1, 6.7, by = -0.1))
  )



ggsave(
  # filename = "E:/Visualisasi/riset/perkab/warna dibalik/bb.png",
  filename = 'E:/Visualisasi/riset/revisi rah/peta/new/laju_bb_tnr.png',
  width = 7,
  height = 4,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)

#

# Void --------------------------------------------------------------------
ggplot(data = bb) +
  geom_sf(
    aes(fill = Y1_nb), color = "white",
    size = 0.3
  ) +
  # geom_sf(
  #   data =  dplyr::filter(batas_kab, nmkab == 'Bandung Barat'),
  #   color = 'gray30',
  #   fill = 'transparent',
  #   size = 0.55,
  #   inherit.aes = F
  # ) +
  scale_fill_manual(
    values = rev(my_col_green)
  ) +
  theme_void() +
  theme(
    legend.position = 'none'
  )

ggsave(
  filename = "E:/Visualisasi/riset/perkab/warna dibalik/bb_void.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 500,
  scale = 1
)




ggplot(data = pwk) +
  geom_sf(
    aes(fill = Y1_nb), color = "white",
    size = 0.3
  ) +
  # geom_sf(
  #   data =  dplyr::filter(batas_kab, nmkab == 'Bandung Barat'),
  #   color = 'gray30',
  #   fill = 'transparent',
  #   size = 0.55,
  #   inherit.aes = F
  # ) +
  scale_fill_manual(
    values = rev(my_col_green)
  ) +
  theme_void() +
  theme(
    legend.position = 'none'
  )


ggsave(
  filename = "E:/Visualisasi/riset/perkab/warna dibalik/pwk_void.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 500,
  scale = 1
)

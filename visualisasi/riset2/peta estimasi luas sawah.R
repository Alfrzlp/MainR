library(sf)
library(classInt)
library(ggrepel)
library(tidyverse)

my_col <- c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')
windowsFonts(
  tnr = windowsFont("Times New Roman"),
  poppins = windowsFont("Poppins")
)
# Data --------------------------------------------------------------------
bb_shape <- st_read("D:/__Datasets/Riset2/bb_LajuLuas.geojson")
pwk_shape <- st_read("D:/__Datasets/Riset2/pwk_LajuLuas.geojson")



# BB ----------------------------------------------------------------------
brLS_bb <- classIntervals(bb_shape$estimasi_luas, n = 5, style = 'jenks')$br

bb_shape <- bb_shape %>%
  mutate(
    ls = cut(estimasi_luas, breaks = brLS_bb, include.lowest = TRUE)
  )

brLS_bb <- scales::dollar(brLS_bb, decimal.mark = ',', big.mark = '.', prefix = '')
label_brBB <- c(
  str_glue('[{brLS_bb[1]}; {brLS_bb[2]}]'),
  str_glue('({brLS_bb[2]}; {brLS_bb[3]}]'),
  str_glue('({brLS_bb[3]}; {brLS_bb[4]}]'),
  str_glue('({brLS_bb[4]}; {brLS_bb[5]}]'),
  str_glue('({brLS_bb[5]}; {brLS_bb[6]}]')
)

label_brBB


# Centroid ----------------------------------------------------------------
cent <- 
  st_coordinates(st_centroid(bb_shape)) %>% 
  as_tibble() %>% 
  mutate(
    nmkec = str_to_title(bb_shape$nmkec)
  )

bb_shape$X_cen <- cent$X
bb_shape$Y_cen <- cent$Y


my_family <- 'tnr'

ggplot(data = bb_shape) +
  geom_sf(
    aes(fill = ls), color = "white",
    size = 0.15
  ) +
  geom_label_repel(
    data = subset(bb_shape, X_cen < 107.45),
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
    box.padding = 0.2,
    nudge_x = 107 - subset(bb_shape, X_cen < 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e4,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 5 # n control point
  ) +
  geom_label_repel(
    data = subset(bb_shape, X_cen >= 107.45),
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
    box.padding = 0.2,
    nudge_x = 107 + subset(bb_shape, X_cen >= 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e5,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 5 # n control point
  ) +
  geom_point(
    aes(
      x = X_cen, y = Y_cen
    ),
    color = 'black',
    size = 0.7
  ) +
  scale_fill_manual(
    values = rev(my_col),
    labels = label_brBB
  ) +
  labs(
    fill = 'Luas (Ha)', 
    title = 'Estimasi Luas Tanam Padi Tahun 2021',
    subtitle = 'Kabupaten Bandung Barat',
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_minimal(base_family = my_family, base_size = 11) +
  theme(
    plot.title = element_text(face = 'bold', vjust = 0),
    plot.subtitle = element_text(colour = 'gray30', vjust = 0)
  ) +
  coord_sf(xlim = c(106.99, 107.91), ylim = c(-6.6, -7.2)) +
  scale_x_continuous(
    labels = lab_tokomma(seq(107, 107.8, by = 0.2), 'E')
  ) +
  scale_y_continuous(
    labels = lab_tokomma(seq(7.2, 6.6, by = -0.1))
  )



lab_tokomma <- function(y_lab, arah = 'S'){
  y_lab <- str_replace_all(y_lab, '\\.', ',')
  y_lab <- str_glue('{y_lab}°{arah}')
  return(y_lab)
}

lab_tokomma(seq(6.6, 7.2, by = 0.1))
lab_tokomma(seq(107, 107.8, by = 0.2), 'E')

ggsave(
  # filename = "E:/Visualisasi/riset/estimasi_luas_bb.png",
  filename = "E:/Visualisasi/riset/revisi rah/peta/estimasi_luas_bb_poppins.png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)




# PWK ---------------------------------------------------------------------
brLS_pwk <- classIntervals(pwk_shape$estimasi_luas, n = 5, style = 'jenks')$br
brLS_pwk[1] <- 434.9
brLS_pwk[2] <- 435.0

pwk_shape <- pwk_shape %>%
  mutate(
    ls = cut(estimasi_luas, breaks = brLS_pwk, include.lowest = TRUE)
  )

brLS_pwk <- scales::dollar(brLS_pwk, decimal.mark = ',', big.mark = '.', prefix = '')

label_br <- c(
  str_glue('[{brLS_pwk[1]}; {brLS_pwk[2]}]'),
  str_glue('({brLS_pwk[2]}; {brLS_pwk[3]}]'),
  str_glue('({brLS_pwk[3]}; {brLS_pwk[4]}]'),
  str_glue('({brLS_pwk[4]}; {brLS_pwk[5]}]'),
  str_glue('({brLS_pwk[5]}; {brLS_pwk[6]}]')
)

label_br

# Centroid ----------------------------------------------------------------
cent <- 
  st_coordinates(st_centroid(pwk_shape)) %>% 
  as_tibble() %>% 
  mutate(
    nmkec = str_to_title(pwk_shape$nmkec)
  )

pwk_shape$X_cen <- cent$X
pwk_shape$Y_cen <- cent$Y


my_family <- 'poppins'
ggplot(data = pwk_shape) +
  geom_sf(
    aes(fill = ls), color = "white",
    size = 0.15
  ) +
  geom_label_repel(
    data = subset(pwk_shape, X_cen < 107.45),
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
    box.padding = 0.2,
    nudge_x = 107 - subset(pwk_shape, X_cen < 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e4,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 5 # n control point
  ) +
  geom_label_repel(
    data = subset(pwk_shape, X_cen >= 107.45),
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
    box.padding = 0.2,
    nudge_x = 107 + subset(pwk_shape, X_cen >= 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e5,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 5 # n control point
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
    values = rev(my_col),
    labels = label_br
  ) +
  labs(
    fill = 'Luas (Ha)', 
    title = 'Estimasi Luas Tanam Padi Tahun 2021',
    subtitle = 'Kabupaten Purwakarta',
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_minimal(base_family = my_family, base_size = 11) +
  theme(
    plot.title = element_text(face = 'bold', vjust = 0),
    plot.subtitle = element_text(colour = 'gray30', vjust = 0)
    # legend.title = element_text(size = 10)
  ) +
  coord_sf(xlim = c(106.99, 107.91), ylim = c(-6.3, -6.95)) +
  scale_x_continuous(
    labels = lab_tokomma(seq(107, 107.8, by = 0.2), 'E')
  ) +
  scale_y_continuous(
    labels = lab_tokomma(seq(6.9, 6.3, by = -0.1))
  )




ggsave(
  filename = "E:/Visualisasi/riset/revisi rah/peta/estimasi_luas_pwk_poppins.png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)
#


# void --------------------------------------------------------------------
ggplot(data = bb_shape) +
  geom_sf(
    aes(fill = ls), color = "white",
    size = 0.3
  ) +
  scale_fill_manual(
    values = rev(my_col),
    labels = label_br
  ) +
  theme_void() +
  theme(
    legend.position = 'none'
  )


ggsave(
  filename = "E:/Visualisasi/riset/estimasi luas/estimasi_luas_bb_void.png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 500,
  scale = 1
)

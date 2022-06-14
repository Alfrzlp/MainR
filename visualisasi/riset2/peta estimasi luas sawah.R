library(sf)
library(classInt)
library(ggrepel)

my_col <- c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')

# Data --------------------------------------------------------------------
bb_shape <- st_read("E:/CitraRiset2/bb_LajuLuas.geojson")
pwk_shape <- st_read("E:/CitraRiset2/pwk_LajuLuas.geojson")



# BB ----------------------------------------------------------------------
brLS_bb <- classIntervals(bb_shape$estimasi_luas, n = 5, style = 'jenks')$br

bb_shape <- bb_shape %>%
  mutate(
    ls = cut(estimasi_luas, breaks = brLS_bb, include.lowest = TRUE)
  )

label_br <- c(
  str_glue('[{brLS_bb[1]}, {brLS_bb[2]}]'),
  str_glue('({brLS_bb[2]}, {brLS_bb[3]}]'),
  str_glue('({brLS_bb[3]}, {brLS_bb[4]}]'),
  str_glue('({brLS_bb[4]}, {brLS_bb[5]}]'),
  str_glue('({brLS_bb[5]}, {brLS_bb[6]}]')
)


# Centroid ----------------------------------------------------------------
cent <- 
  st_coordinates(st_centroid(bb_shape)) %>% 
  as_tibble() %>% 
  mutate(
    nmkec = str_to_title(bb_shape$nmkec)
  )

bb_shape$X_cen <- cent$X
bb_shape$Y_cen <- cent$Y



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
    labels = label_br
  ) +
  labs(
    fill = 'Luas (Ha)', 
    title = 'Estimasi Luas Tanam Padi Tahun 2021',
    subtitle = 'Kabupaten Bandung Barat',
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_minimal(base_family = 'Arial', base_size = 11) +
  theme(
    plot.title = element_text(face = 'bold'),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(xlim = c(106.99, 107.91), ylim = c(-6.6, -7.2))




ggsave(
  filename = "E:/Visualisasi/riset/estimasi_luas_bb.png",
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

label_br <- c(
  str_glue('[{brLS_pwk[1]}, {brLS_pwk[2]}]'),
  str_glue('({brLS_pwk[2]}, {brLS_pwk[3]}]'),
  str_glue('({brLS_pwk[3]}, {brLS_pwk[4]}]'),
  str_glue('({brLS_pwk[4]}, {brLS_pwk[5]}]'),
  str_glue('({brLS_pwk[5]}, {brLS_pwk[6]}]')
)


# Centroid ----------------------------------------------------------------
cent <- 
  st_coordinates(st_centroid(pwk_shape)) %>% 
  as_tibble() %>% 
  mutate(
    nmkec = str_to_title(pwk_shape$nmkec)
  )

pwk_shape$X_cen <- cent$X
pwk_shape$Y_cen <- cent$Y



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
  theme_minimal(base_family = 'Arial', base_size = 11) +
  theme(
    plot.title = element_text(face = 'bold'),
    plot.subtitle = element_text(colour = 'gray30')
    # legend.title = element_text(size = 10)
  ) +
  coord_sf(xlim = c(106.99, 107.91), ylim = c(-6.3, -6.95))




ggsave(
  filename = "E:/Visualisasi/riset/estimasi_luas_pwk.png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)



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

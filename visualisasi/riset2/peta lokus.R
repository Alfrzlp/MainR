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


my_family <- 'tnr'
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
    family = my_family,
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
    family = my_family,
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
  theme_bw(base_family = my_family) +
  theme(
    legend.position = 'none',
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(xlim = c(107.1, 107.8), ylim = c(-6.4, -7.15)) +
  scale_x_continuous(
    labels = lab_tokomma(seq(107.1, 107.8, by = 0.1), 'E')
  ) +
  scale_y_continuous(
    labels = lab_tokomma(seq(7.1, 6.4, by = -0.1))
  )




ggsave(
  filename = "E:/Visualisasi/riset/revisi rah/lokus_tnr.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)

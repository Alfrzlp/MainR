dat <- readxl::read_xlsx('D:/_Datasets/TDIIPaper_fix.xlsx')
head(dat)
tail(dat)

p <- dat %>% 
  dplyr::filter(
    prov != 'Indonesia'
  ) %>% 
  ggplot(aes(y = reorder(prov, k4), x = k4)) +
  geom_col(
    aes(fill = k4)
  ) +
  geom_text(
    aes(label = format(k4, nsmall = 1)),
    size = 3, 
    hjust = -0.6
  ) +
  geom_point(
    aes(color = k4),
    size = 4
  ) +
  labs(
    x = "Persentase Kunjungan Ibu hamil K4",
    y = "Provinsi", fill = NULL
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    labels = function(x) scales::percent(x/100),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_y_discrete(
    expand = c(0, 0)
  ) +
  # scale_fill_gradient2(
  #   low = 'cyan', mid = 'cyan3',
  #   high = '#1D363C', midpoint = 65
  # ) +
  # scale_color_gradient2(
  #   low = 'cyan', mid = 'cyan3',
  #   high = '#1D363C', midpoint = 65
  # ) +
  theme_minimal(base_size = 13, base_family = 'Arrial Narrow') +
  theme(
    plot.title.position = "plot",
    # top,right,bottom,left
    plot.title = element_text(size = rel(1.45), face = "bold",
                              margin = margin(0, 0, 20, 10), family = 'Arrial Narrow'),
    axis.title.x = element_text(colour = "slategray", size = 13.5, hjust = 0),
    axis.title.y = element_text(colour = "slategray", size = 13.5, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    panel.grid.major.x =  element_line(size = 0.5, color = '#DEDEDE'),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    # text label facet grid
    strip.text.x = element_text(size = 10, colour = 'slategray', face = 'bold'),
    legend.position = 'none'
  )


p

ragg::agg_png(
  "E:/Visualisasi/k4.png",
  width = 6.5,
  height = 6,
  units = "in", res = 300,
  scaling = 1, 
  background = 'transparent'
)
p
dev.off()


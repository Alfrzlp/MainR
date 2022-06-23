library(dm)
library(tidyverse)


# -------------------------------------------------------------------------
s <- 'Rumah Tangga Menerima KUR	2,15%
Rumah Tangga Menerima Kredit BPR	0,29%
Rumah Tangga Menerima PKH	2,67%
Rumah Tangga Menerima BPNT	3,73%'


dat <- read_pattern(
    s, pos_non_angka = 1,
    pos_angka = 2
  ) %>% 
  separator_convert(v2) %>% 
  setNames(c('kat', 'p')) %>% 
  mutate(
    kat = str_remove_all(kat, 'Rumah Tangga Menerima |Kredit'),
    kat = str_trim(kat)
  )

dat

dat <- dat %>% 
  bind_rows(
    dat %>% 
      mutate(p = 100 - p, kat = paste('Non', kat))
  )

dat <- dat %>% 
  mutate(gp = c(dat$kat[1:4], dat$kat[1:4]))



dat


# -------------------------------------------------------------------------
ggplot(dat, aes(x = kat, y = p/100, fill = kat)) +
  geom_col() +
  geom_text(
    aes(label = paste(p, '%')),
    nudge_y = 0.003,
    color = 'black',
    fontface = 2
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(NA, 0.15)),
    labels = scales::percent
  ) +
  scale_x_discrete(
    labels = function(x) str_wrap(x, 15)
  ) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    panel.grid.major.y = element_line(colour = 'gray80')
  ) +
  labs(
    y = 'Persentase',
    x = 'Kategori',
    title = str_wrap('Penerimaan BPUM disertai Program Lainnya pada Rumah Tangga Usaha Mikro', 50)
  ) +
  scale_fill_manual(
    values = c('#FDD0A2', "#FD8D3C", "#E6550D", "#A63603")
  )
  # scale_fill_brewer(palette = 'Oranges')





ggsave(
  filename = "E:/Visualisasi/rev/bar_chart.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 300,
  scale = 1,
  bg = 'white'
)



lvl <- c("KUR", "Non KUR", "BPR", "Non BPR", "PKH", "Non PKH", "BPNT", 
  "Non BPNT")
# -------------------------------------------------------------------------
dat %>% 
  ggplot(aes(x = hsize, y = p, fill = factor(kat, lvl))) +
  geom_col(color = "transparent") +
  geom_text(
    data = ~ .x %>% dplyr::filter(!str_detect(kat, 'Non')),
    aes(label = paste0(p, '%')),
    fontface = 2,
    position = position_stack(vjust = 0.5)
  ) +
  coord_polar(theta = "y") +
  xlim(c(1.7, hsize + 0.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = str_wrap('Penerimaan BPUM Disertai Program Lainnya Pada Rumah Tangga Usaha Mikro', 40),
    # title = "BPUM Acceptance is Accompanied by Other Programs \nin The Micro-Enterprises Household",
    fill = NULL
  ) +
  scale_fill_manual(
    # values = c(c('#FD8D3C', '#FDD0A2'), rep(c('white', 'white'), 3)),
    values = rep(c('#FD8D3C', '#FDD0A2'), 4),
    labels = c('Tidak Hanya BPUM', 'Hanya BPUM', rep(c(' ', ' '), 3)),
    # breaks = c('KUR', 'Non KUR')
    # labels = c('BPNT',
    #            'BPR',
    #            'KUR',
    #            'PKH')
  ) +
  # scale_fill_manual(values = c("#A10000", "#BF0404", "#E00000", "#FF5454", "#FF7F7F"))+
  theme(
    plot.title = element_text(size = rel(1.1), face = "bold", margin = margin(b = 15)),
    legend.position = c(1.35, 0.3),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  facet_wrap(.~gp, nrow = 2)



ggsave(
  filename = "E:/Visualisasi/pie chart/pie4_2.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 300,
  scale = 1,
  bg = 'white'
)




hsize <- 3
dat %>% 
  ggplot(aes(x = hsize, y = p, fill = factor(kat, lvl))) +
  geom_col(color = "transparent") +
  geom_text(
    data = ~ .x %>% dplyr::filter(!str_detect(kat, 'Non')),
    aes(label = paste0(p, '%')),
    fontface = 2,
    position = position_stack(vjust = 1)
  ) +
  coord_polar(theta = "y") +
  xlim(c(1.7, hsize + 0.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    # title = "BPUM Acceptance is Accompanied by Other Programs \nin The Micro-Enterprises Household",
    title = str_wrap('Penerimaan BPUM disertai Program Lainnya pada Rumah Tangga Usaha Mikro', 50),
    fill = NULL
  ) +
  scale_fill_manual(
    values = rep(c('#FD8D3C', '#FDD0A2'), 4)
    # labels = c('BPNT',
    #            'BPR',
    #            'KUR',
    #            'PKH')
  ) +
  # scale_fill_manual(values = c("#A10000", "#BF0404", "#E00000", "#FF5454", "#FF7F7F"))+
  theme(
    plot.title = element_text(size = rel(1.3), face = "bold", margin = margin(b = 15)),
    legend.position = 'none',
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  facet_grid(.~gp)

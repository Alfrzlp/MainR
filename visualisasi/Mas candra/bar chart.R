library(dm)
library(tidyverse)


# -------------------------------------------------------------------------
s <- 'Rumah Tangga Menerima KUR	2,15%
Rumah Tangga Menerima Kredit BPR	0,29%
Rumah Tangga Menerima PKH	2,67%
Rumah Tangga Menerima BPNT	3,73%'


dat <- 
  read_pattern(
    s, pos_non_angka = 1,
    pos_angka = 2
  ) %>% 
  separator_convert(v2) %>% 
  setNames(c('kat', 'p'))



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

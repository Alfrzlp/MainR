library(tidyverse)
library(dm)

# Data --------------------------------------------------------------------
s <- 'Sangat Miskin	145	2.92%
Miskin	235	4.73%
Hampir Miskin	296	5.96%
Rentan Miskin	799	16.09%
Tidak Miskin	3490	70.29%'

penerima <- 
  read_pattern(
    s, pos_non_angka = 1, 
    pos_angka = 2:3
  ) %>% 
  setNames(c("sk", "freq",	"p")) %>% 
  type_convert()



s1 <- 'Sangat Miskin	1272	4.51%
Miskin	1624	5.75%
Hampir Miskin	1956	6.93%
Rentan Miskin	4274	15.14%
Tidak Miskin	19105	67.67%'

bkn_penerima <- 
  read_pattern(
    s1, pos_non_angka = 1, 
    pos_angka = 2:3
  ) %>% 
  setNames(c("sk", "freq",	"p")) %>% 
  type_convert()


glimpse(penerima)

lvl <- penerima$sk

# Viz ---------------------------------------------------------------------

penerima %>% 
  mutate(
    ypos = cumsum(p) - 0.5 * p
  ) %>% 
  ggplot(aes(x = "", y = rev(p), fill = sk)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(
    aes(y = ypos, label = paste0(p, "%")), 
    color = "white", size = 4.5,
    fontface = 2
  ) +
  coord_polar("y", start = 0) +
  # scale_fill_manual(
  #   # values = (my_col[1:3]),
  #   labels = function(x) str_wrap(x, 19)
  # ) 
  theme_void(base_size = 13) 
  theme(
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30'),
    legend.text = element_text(size = 12),
    legend.key.width = unit(0.9, 'cm'),
    legend.key.height = unit(0.7, 'cm'),
    legend.spacing.x = unit(0.3, 'cm'),
    legend.spacing.y = unit(0.1, 'cm')
  ) +
  # jika ingin pakai legend.spacing.y wajib ini
  guides(fill = guide_legend(byrow = TRUE)) 
  labs(
    title = str_wrap('Interval Jumlah Anggota Rumah Tangga (Orang)', 50),
    subtitle = 'Kabupaten Purwakarta',
    # fill = str_wrap('Jumlah Anggota Rumah Tangga (Orang)', 20)
    fill = NULL
  )


ggsave(
  filename = 'E:/Visualisasi/riset/revisi terbaru/12_tabel1_pwk_piechart.png',
  width = 10,
  height = 6,
  units = "in",
  dpi = 500,
  scale = 0.6,
  bg = 'white'
)


# bukan penerima ----------------------------------------------------------
my_col <- c('#FDD0A2', "#FD8D3C", "#E6550D", "#A63603", 'black')


penerima %>% 
  mutate(
    # coba ganti2 angka 0.35 biar pas
    ypos = cumsum(p) - 0.2 * p
  ) %>% 
  ggplot(aes(x = "", y = p, fill = factor(sk, lvl))) +
  geom_bar(
    aes(y = ypos),
    stat = "identity", width = 2, color = "white"
  ) +
  # geom_text(
  #   aes(y = ypos, label = paste0(p, "%")), 
  #   color = "black", size = 4.5,
  #   fontface = 2
  # ) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(
    palette = 'Oranges',
    direction = -1,
    labels = function(x) str_wrap(x, 19)
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30'),
    legend.text = element_text(size = 12),
    legend.key.width = unit(0.9, 'cm'),
    legend.key.height = unit(0.7, 'cm'),
    legend.spacing.x = unit(0.3, 'cm'),
    legend.spacing.y = unit(0.1, 'cm')
  ) +
  # jika ingin pakai legend.spacing.y wajib ini
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(
    # title = str_wrap('Interval Jumlah Anggota Rumah Tangga (Orang)', 50),
    # subtitle = 'Kabupaten Purwakarta',
    # fill = str_wrap('Jumlah Anggota Rumah Tangga (Orang)', 20)
    fill = NULL
  )


ggsave(
  filename = 'E:/Visualisasi/rev sidang/penerima.png',
  width = 10,
  height = 6,
  units = "in",
  dpi = 500,
  scale = 0.6,
  bg = 'white'
)



# pie 4 2 -----------------------------------------------------------------
s4 <- 'Rumah Tangga Menerima KUR	2,15%
Rumah Tangga Menerima Kredit BPR	0,29%
Rumah Tangga Menerima PKH	2,67%
Rumah Tangga Menerima BPNT	3,73%'


dat4 <- read_pattern(
  s4, pos_non_angka = 1,
  pos_angka = 2
) %>% 
  separator_convert(v2) %>% 
  setNames(c('kat', 'p')) %>% 
  mutate(
    kat = str_remove_all(kat, 'Rumah Tangga Menerima |Kredit'),
    kat = str_trim(kat)
  )
dat4 <- dat4 %>% 
  bind_rows(
    dat4 %>% 
      mutate(p = 100 - p, kat = paste('Non', kat))
  )
dat4 <- dat4 %>% 
  mutate(gp = c(dat4$kat[1:4], dat4$kat[1:4]))

dat4


hsize <- 3
lvl <- c("KUR", "Non KUR", "BPR", "Non BPR", "PKH", "Non PKH", "BPNT", 
         "Non BPNT")
dat4 %>% 
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
    # title = str_wrap('Penerimaan BPUM Disertai Program Lainnya Pada Rumah Tangga Usaha Mikro', 40),
    # title = "BPUM Acceptance is Accompanied by Other Programs \nin The Micro-Enterprises Household",
    fill = NULL
  ) +
  scale_fill_manual(
    # values = c(c('#FD8D3C', '#FDD0A2'), rep(c('white', 'white'), 3)),
    values = rep(c('#FD8D3C', '#FDD0A2'), 4),
    labels = c('Menerima Keduanya', 'Hanya Menerima BPUM', rep(c(' ', ' '), 3)),
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
  filename = "E:/Visualisasi/rev sidang/pie4_2.png",
  width = 8,
  height = 4,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = 'white'
)


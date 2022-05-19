devtools::install_github('alfrzlp/package-dm@no-ocr')

library(tidyverse)
library(dm)

my_col <- c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')


# Tabel 1 -----------------------------------------------------------------
# copy paste dari excel
s <- 
  'Bandung Barat	33	17.0	113	58.2	48	24.8
  Purwakarta	24	24.2	42	42.4	33	33.4'

jrt_lvl <- c("Di Bawah Rata-rata (1-2)", 'Sekitar Rata-rata (3-4)', 'Di Atas Rata-rata (>4)')


dat1 <- 
  read_pattern(
    s, pos_non_angka = 1, 
    pos_angka = 2:7
  ) %>% 
  select(
    v1, v3, v5, v7
  ) %>% 
  setNames(
    c('kab', jrt_lvl)
  ) %>% 
  pivot_longer(-kab, names_to = 'kat', values_to = 'persentase') %>% 
  mutate(
    kat  = factor(kat, levels = jrt_lvl)
  ) %>% 
  type_convert()

dat1



# viz ---------------------------------------------------------------------
dat1 %>% 
  # ganti filter
  dplyr::filter(
    kab == 'Purwakarta'
  ) %>% 
  mutate(
    # coba ganti2 angka 0.35 biar pas
    ypos = cumsum(persentase) - 0.35 * persentase
  ) %>% 
  ggplot(aes(x = "", y = persentase, fill = kat)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(
    aes(y = ypos, label = paste0(persentase, "%")), 
    color = "white", size = 4.5,
    fontface = 2
  ) +
  coord_polar("y", start = 0) +
  scale_fill_manual(
    values = rev(my_col[1:3]),
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

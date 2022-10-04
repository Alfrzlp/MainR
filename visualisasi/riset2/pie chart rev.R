library(tidyverse)


my_col <- c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')

# Tabel 1 -----------------------------------------------------------------
jrt_lvl <- c("Di Bawah Rata-rata (1-2)", 'Sekitar Rata-rata (3-4)', 'Di Atas Rata-rata (>4)')

dat1 <- 
  tibble(
    kab = rep(c('Bandung Barat', 'Purwakarta'), each = 3),
    kat = rep(jrt_lvl, 2),
    persentase = c(17.0, 58.2, 24.8, 24.2, 42.4, 33.4)
  )

dat1


dat1<- data.frame(
  kab = c("Kabupaten Bandung Barat", "Kabupaten Bandung Barat", "Kabupaten Bandung Barat"),
  kat = c("Dewasa Awal (18-40 tahun)", "Dewasa Madya (41-60 tahun)", "Dewasa Akhir (> 60 tahun)"),
  persentase = c(9.1, 55.6, 35.4)
)

dat1 <- dat1 %>% 
  mutate(
    kat = factor(kat, levels = c("Dewasa Awal (18-40 tahun)", "Dewasa Madya (41-60 tahun)", "Dewasa Akhir (> 60 tahun)")) 
  )
dat1


# viz ---------------------------------------------------------------------
dat1 %>% 
  # ganti filter
  dplyr::filter(
    kab == 'Kabupaten Bandung Barat'
  ) %>% 
  mutate(
    # coba ganti2 angka 0.35 biar pas
    ypos = cumsum(persentase) - 0.35 * persentase
  ) %>% 
  ggplot(aes(x = "", y = rev(persentase), fill = kat)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(
    aes(y = ypos, label = paste0(persentase, "%")), 
    color = "white", size = 4.5,
    fontface = 2
  ) +
  coord_polar("y", start = 0) +
  scale_fill_manual(
    values = (my_col[1:3]),
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







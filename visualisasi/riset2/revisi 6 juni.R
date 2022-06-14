library(dm)
my_col <- c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')

# 1.2 ---------------------------------------------------------------------
s1 <- '57	19.45392491	155	52.90102389	81	27.64505119'
jrt_lvl <- c("Di Bawah Rata-rata (1-2)", 'Sekitar Rata-rata (3-4)', 'Di Atas Rata-rata (>4)')


dat1 <- read_pattern(
    s1,
    pos_angka = 1:6
  ) %>%
  dplyr::select(
    ends_with(as.character(c(2, 4, 6)))
  ) %>%
  setNames(
    c(jrt_lvl)
  ) %>%
  pivot_longer(everything(), names_to = 'kat', values_to = 'persentase') %>%
  mutate(
    kat  = factor(kat, levels = jrt_lvl)
  ) %>%
  type_convert()

dat1


dat1 %>%
  mutate(
    ypos = cumsum(persentase) - 0.5 * persentase
  ) %>%
  ggplot(aes(x = "", y = persentase, fill = rev(kat))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(
    aes(y = ypos, label = scales::percent(persentase/100)),
    color = "white", size = 4.5,
    fontface = 2
  ) +
  coord_polar("y", start = 0) +
  scale_fill_manual(
    values = (my_col[1:3]),
    labels = function(x) rev(str_wrap(x, 19))
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
    title = str_wrap('Persentase (%) Petani Pelaku Alih Fungsi Lahan Menurut Jumlah Anggota Rumah Tangga', 50),
    subtitle = 'Kabupaten Bandung Barat dan Kabupaten Purwakarta',
    # fill = str_wrap('Jumlah Anggota Rumah Tangga (Orang)', 20)
    fill = NULL
  )



ggsave(
  filename = 'E:/Visualisasi/riset/Revisi 5 Mei/1_2.png',
  width = 10,
  height = 6,
  units = "in",
  dpi = 500,
  scale = 0.6,
  bg = 'white'
)



# 1.7 ---------------------------------------------------------------------
s2 <- 'Melalui Transaksi Penjualan	23	21.30	52	48.15	33	30.56
Tidak Melalui Transaksi Penjualan	34	18.38	103	55.68	48	25.95'

dat2 <- read_pattern(
    s2, pos_non_angka = 1,
    pos_angka = 2:7
  ) %>%
  mutate(
    .before = v1
  ) %>%
  dplyr::select(c(mekanisme = v1,  v3, v5, v7)) %>%
  # separator_convert(vars(v3, v5, v7))
  setNames(
    c('mekanisme', jrt_lvl)
  ) %>%
  pivot_longer(-mekanisme, names_to = 'kat', values_to = 'persentase') %>%
  mutate(
    kat = factor(kat, levels = jrt_lvl)
  ) %>%
  type_convert()

dat2




ggplot(dat2, aes(y = persentase, x = kat, fill = mekanisme, group = mekanisme)) +
  geom_bar(
    stat = 'identity',
    position = position_dodge2(0.9)
  ) +
  geom_text(
    aes(y = persentase + 3, label = scales::percent(persentase/100)),
    position = position_dodge(0.9),
    size = 3.9
  ) +
  scale_fill_manual(
    NULL,
    values =  c(my_col[1:2], my_col[1:2]),
    labels = function(x) str_wrap(x, 20)
  ) +
  scale_y_continuous(
    labels = NULL,
    expand = expansion(mult = c(0.01, 0.1))
  ) +
  scale_x_discrete(
    expand = expansion(mult = c(0.01, 0.1)),
    labels = function(x) str_wrap(x, 10)
  ) +
  # facet_grid(
  #   . ~ kab, scales = 'free'
  # ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = 'Kabupaten Bandung Barat dan Kabupaten Purwakarta',
    title = str_wrap('Persentase (%) Mekanisme Alih Fungsi Lahan Menurut Interval Jumlah Anggota Rumah Tangga', 70)
  ) +
  theme_minimal() +
  theme(
    panel.spacing = unit(2, "lines"),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 11.5, face = 2, colour = 'gray30', margin = margin(20, 0, 0, 0)),
    axis.text.x = element_text(size = 10.5, margin = margin(b = 10, 0, 0, 0)),
    panel.grid = element_blank(),
    plot.title = element_text(face = 2, size = 13),
    plot.subtitle = element_text(colour = 'gray30')
  )




ggsave(
  filename = 'E:/Visualisasi/riset/Revisi 5 Mei/1_7.png',
  width = 7.5,
  height = 5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = 'white'
)



# 1.18 --------------------------------------------------------------------
s3 <- 'Padi	140	63.06	51	22.97	31	13.96
Palawija	33	75.00	7	15.91	4	9.09
Hortikultura	6	40.00	7	46.67	2	13.33
Lainnya	12	100.00	0	0.00	0	0.00'
lvl_harga_jual <- c("Rendah (1-250)", "Sedang (251-500)", "Tinggi (>500)"	)

dat_harga <- dm::read_string(
  s3
) %>%
  mutate(
    .before = V1
  ) %>%
  dplyr::select(c(V1, V3, V5, V7)) %>%
  setNames(
    c('komoditas', lvl_harga_jual)
  ) %>%
  pivot_longer(-c(komoditas), names_to = 'harga_jual', values_to = 'p') %>%
  mutate(
    harga_jual = factor(harga_jual, lvl_harga_jual)
  )

dat_harga





lvl_komoditas <- c('Padi', 'Palawija', 'Hortikultura', 'Lainnya')


dat_harga %>%
  mutate(
    col_text = if_else(harga_jual == 'Tinggi (>500)', 'w', 'b')
  ) %>%
  ggplot(
    aes(
      x = p, y = factor(komoditas, rev(lvl_komoditas)),
      fill = harga_jual,
    )
  ) +
  geom_bar(
    stat = 'identity',
    position = position_stack(),
    width = 0.7
  ) +
  geom_text(
    data = ~ .x %>%
      dplyr::filter(
        p > 0
      ),
    aes(
      colour = col_text,
      label = ifelse(
        p %in% 1:100,
        scales::percent(p/100, accuracy = 1),
        scales::percent(p/100, accuracy = 0.01)
      )
    ),
    position = position_stack(vjust = 0.5, reverse = F),
    size = 3.5
  ) +
  scale_colour_manual(values = c('black', 'white')) +
  # scale_fill_brewer(palette = 'Greens') +
  scale_fill_manual(
    values = rev(my_col[1:3]),
    labels = function(x) str_wrap(x, 20)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.20),
    labels = NULL,
    expand = expansion(mult = c(0.005, 0.03))
  ) +
  labs(
    x = NULL, y = NULL,
    fill = NULL,
    subtitle = 'Kabupaten Bandung Barat dan Kabupaten Purwakarta',
    title = expression(bold('Persentase (%) Harga Jual Lahan (ribu per' ~ m^2 ~ ') Menurut Jenis Komoditas'))
  ) +
  theme_minimal(base_line_size = 0) +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    line = element_blank(),
    # legend.position = 'bottom',
    legend.justification = "center",
    # strip.background = element_blank(),
    # strip.placement = "outside",
    # strip.text = element_text(size = 11.5, colour = 'gray30', face = 2),
    axis.text.y = element_text(size = 11),
    plot.subtitle = element_text(colour = 'gray30'),
    legend.text = element_text(size = 9.5),
    legend.spacing.y = unit(0.1, 'cm')
  ) +
  guides(
    colour = 'none',
    fill = guide_legend(byrow = TRUE)
  )




ggsave(
  filename = 'E:/Visualisasi/riset/Revisi 5 Juni/1_18.png',
  width = 10.5,
  height = 5,
  units = "in",
  dpi = 500,
  scale = 0.8,
  bg = 'white'
)

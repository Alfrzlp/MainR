library(tidyverse)
library(dm)


my_col <- c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')
# Tabel 1 -----------------------------------------------------------------

s <- 
  'Bandung Barat	33	17.0	113	58.2	48	24.8
Purwakarta	24	24.2	42	42.4	33	33.4'


jrt_lvl <- c("Di Bawah Rata-rata (1-2)", 'Sekitar Rata-rata (3-4)', 'Di Atas Rata-rata (>4)')


dat1 <- 
  read_pattern(
    s, pos_non_angka = 1, 
    pos_angka = 2:7
  ) %>% 
  dplyr::select(
    ends_with(as.character(c(1, 3, 5, 7)))
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

# Viz ---------------------------------------------------------------------
ggplot(dat1, aes(x = persentase, y = kat, fill = kat)) +
  geom_bar(stat = 'identity') +
  geom_text(
    aes(label = scales::percent(persentase/100)),
    nudge_x = 3
  ) +
  scale_fill_manual(
    values =  my_col[1:3]
  ) +
  scale_x_continuous(
    labels = function(x) scales::percent(x/100),
    expand = expansion(mult = c(0.01, 0.1))
  ) +
  scale_y_discrete(
    labels = function(x) str_wrap(x, 10)
  ) +
  labs(
    x = 'Persentase',
    y = NULL,
    title = 'Interval Jumlah Anggota Rumah Tangga (Orang)'
  ) +
  theme_minimal(base_family = 'Arial') +
  theme(
    legend.position = 'none',
    strip.background = element_blank(),
    strip.placement = "outside", 
    strip.text = element_text(size = 11.5, face = 2, colour = 'gray30'),
    axis.text.x = element_text(size = 10.5),
    panel.grid.major = element_line(colour = 'gray', size = 0),
    plot.title = element_text(face = 2)
  ) +
  facet_grid(kab~., switch = 'y') 




# -------------------------------------------------------------------------
ggsave(
  filename = 'E:/Visualisasi/riset/revisi terbaru/12_tabel1_barchart.png',
  width = 7.5,
  height = 4,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = 'white'
)






# Tabel 2 -----------------------------------------------------------------
s2 <- 
  'Melalui Transaksi Penjualan	9	4,6	28	14,4	16	8,2
  Tidak Melalui Transaksi Penjualan	24	12,4	85	43,8	32	16,5
  Melalui Transaksi Penjualan	14	14,1	24	24,2	17	17,2
  Tidak Melalui Transaksi Penjualan	10	10,1	18	18,2	16	16,2'



dat2 <- 
  read_pattern(
    s2, pos_non_angka = 1,
    pos_angka = 2:7
  ) %>% 
  mutate(
    kab = rep(c('Bandung Barat', 'Purwakarta'), each = 2),
    .before = v1
  ) %>% 
  dplyr::select(c(kab, mekanisme = v1,  v3, v5, v7)) %>% 
  separator_convert(vars(v3, v5, v7)) %>% 
  setNames(
    c('kab', 'mekanisme', jrt_lvl)
  ) %>% 
  pivot_longer(-c(kab, mekanisme), names_to = 'kat', values_to = 'persentase') %>% 
  mutate(
    kat = factor(kat, levels = jrt_lvl)
  )

dat2

# Viz 2 ---------------------------------------------------------------------
my_lab <- c(
  'Melalui Transaksi Penjualan' = str_wrap('Melalui Transaksi Penjualan', 25), 
  'Tidak Melalui Transaksi Penjualan' = str_wrap('Tidak Melalui Transaksi Penjualan', 25)
)

my_lab <- c(
  'Melalui Transaksi Penjualan' = str_wrap('Melalui Transaksi Penjualan', 35), 
  'Tidak Melalui Transaksi Penjualan' = str_wrap('Tidak Melalui Transaksi Penjualan', 35)
)


ggplot(dat2, aes(y = persentase, x = kat, fill = mekanisme, group = mekanisme)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_text(
    aes(label = scales::percent(persentase/100)),
    position = position_dodge(preserve = T)    
  ) +
  scale_fill_manual(
    values =  my_col[1:3]
  ) +
  scale_y_continuous(
    labels = NULL,
    expand = expansion(mult = c(0.01, 0.1))
  ) +
  scale_x_discrete(
    labels = function(x) str_wrap(x, 10)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = 'Persentase (%) Mekanisme Alih Fungsi Lahan Menurut Interval Jumlah Anggota Rumah Tangga'
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    strip.background = element_blank(),
    strip.placement = "outside", 
    strip.text = element_text(size = 11.5, face = 2, colour = 'gray30'),
    axis.text.x = element_text(size = 10.5),
    panel.grid.major = element_line(colour = 'gray', size = 0),
    plot.title = element_text(face = 2)
  ) +
  facet_grid(
    .~kab, scales = 'free_x'
  ) 



ggplot(dat2, aes(y = persentase, x = kat, fill = mekanisme, group = mekanisme)) +
  geom_bar(
    stat = 'identity',
    position = position_dodge2(0.9)
  ) +
  geom_text(
    aes(y = persentase + 3, label = scales::percent(persentase/100)),
    position = position_dodge(0.9),
    size = 3.5
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
  facet_grid(
    . ~ kab, scales = 'free'
  ) +
  labs(
    x = NULL,
    y = NULL,
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
    plot.title = element_text(face = 2, size = 13)
  )




ggsave(
  filename = 'E:/Visualisasi/riset/revisi terbaru/17_Groupbarchart_rev2.png',
  width = 7.5,
  height = 5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = 'white'
)




# Pie Cart ----------------------------------------------------------------
dat1 %>% 
  dplyr::filter(
    kab == 'Bandung Barat'
  ) %>% 
  mutate(
    ypos = cumsum(persentase) - 0.5 * persentase
  ) %>% 
  ggplot(aes(x = "", y = persentase, fill = rev(kat))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(
    aes(y = ypos, label = paste0(persentase, "%")), 
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
    subtitle = 'Kabupaten Bandung Barat',
    # fill = str_wrap('Jumlah Anggota Rumah Tangga (Orang)', 20)
    fill = NULL
  )

dat1
ggsave(
  filename = 'E:/Visualisasi/riset/revisi terbaru/12_tabel1_bb_piechart.png',
  width = 10,
  height = 6,
  units = "in",
  dpi = 500,
  scale = 0.6,
  bg = 'white'
)






# 1.18 --------------------------------------------------------------------
s3 <- 'Padi	74	54,41	38	27,94	24	12,37
Palawija	26	70,27	7	18,92	4	2,06
Hortikultura	4	36,36	5	45,45	2	1,03
Lainnya	10	100	0	0	0	0
Padi	66	76,74	13	15,12	7	3,61
Palawija	7	100	0	0	0	0
Hortikultura	2	50	2	50	0	0
Lainnya	2	100	0	0	0	0'


lvl_harga_jual <- c("Rendah (1-250)", "Sedang (251-500)", "Tinggi (>500)"	)

dm::read_string(
  s3
) %>% 
  mutate(
    kab = rep(c('Bandung Barat', 'Purwakarta'), each = 4),
    .before = V1
  ) %>% 
  dplyr::select(c(kab, V1, V2, V4, V6)) %>% 
  setNames(
    c('kab', 'komoditas', lvl_harga_jual)
  ) %>% 
  pivot_longer(-c(kab, komoditas), names_to = 'harga_jual', values_to = 'n') %>% 
  mutate(
    harga_jual = factor(harga_jual, lvl_harga_jual)
  ) %>% 
  separator_convert(n) %>% 
  group_by(komoditas, kab) %>% 
  mutate(
    p = n/sum(n)
  ) %>% 
  ungroup()

dat_harga








lvl_komoditas <- c('Padi', 'Palawija', 'Hortikultura', 'Lainnya')

dat_harga$harga_jual




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
      label = scales::percent(p, accuracy = 0.01)
    ),
    position = position_stack(vjust = 0.5, reverse = F),
    size = 3
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
  facet_grid(kab ~ ., scales = 'free_x', switch = 'y') +
  labs(
    x = NULL, y = NULL, 
    fill = NULL, 
    subtitle = 'Kabupaten Bandung Barat dan Kabupaten Purwakarta',
    title = expression(bold('Persentase (%) Harga Jual Lahan (ribu per' ~ m^2 ~ ') Menurut Jenis Komoditas'))
  ) +
  theme_minimal(base_line_size = 1) +
  theme(
    # legend.position = 'bottom',
    legend.justification = "center",
    strip.background = element_blank(),
    strip.placement = "outside", 
    strip.text = element_text(size = 11.5, colour = 'gray30', face = 2),
    axis.text.x = element_text(size = 10.5),
    panel.grid.major = element_line(colour = 'gray', size = 0),
    plot.subtitle = element_text(colour = 'gray30', face = 2),
    legend.text = element_text(size = 9.5),
    legend.spacing.y = unit(0.1, 'cm')
  ) +
  guides(
    colour = 'none',
    fill = guide_legend(byrow = TRUE)
  ) 


ggsave(
  filename = 'E:/Visualisasi/riset/revisi terbaru/118_hargajuallahan.png',
  width = 10.5,
  height = 5,
  units = "in",
  dpi = 500,
  scale = 0.8,
  bg = 'white'
)


# -------------------------------------------------------------------------
separator_convert <- function(dat, col, sep = ',', to_sep = '\\.', to_numeric = T){
  dat <- dplyr::mutate_at(
    dat, {{col}}, ~ gsub(sep, to_sep, .x)
  )
  if (to_numeric) {
    return(readr::type_convert(dat))
  } else {
    return(dat)
  }
}



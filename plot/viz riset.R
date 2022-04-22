library(dm)
library(tidyverse)


# Usia Petani berdasarkan tingkat pendidikan ------------------------------
lvl_tp <- c('Tidak Bersekolah', 'Tamat SD', 'Tamat SMP', 'Tamat SMA', 'PT')
lvl_usia <- c('Dewasa Awal (18-40)', 'Dewasa Madya (41-60) ', 'Dewasa Akhir (>60)')
my_col <- c('#0A5446', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')



s1 <- 'Tidak Bersekolah	0	0	0	1	3	4	7	4	11
Tamat SD	6	12	18	22	16	38	26	6	32
Tamat SMP	12	15	27	6	2	8	9	0	9
Tamat SMA	12	7	19	13	3	16	2	0	2
PT	0	0	0	1	2	3	6	1	7
Tidak Bersekolah	1	0	1	5	6	11	9	6	15
Tamat SD	2	1	3	22	3	25	14	3	17
Tamat SMP	2	1	3	2	1	3	1	0	1
Tamat SMA	0	0	0	2	0	2	7	2	9
PT	0	0	0	7	0	7	1	0	1'

dat_usia <- 
  dm::read_pattern(
    s1, pos_non_angka = 1,
    pos_angka = 2:10,
    col_names = c('tp', paste0(rep(c('da', 'dm', 'dak'), each = 3), "_",c('l', 'p', 't')))
  ) %>% 
  dplyr::select(tp, ends_with('_t')) %>% 
  type_convert() %>% 
  mutate(
    kab = rep(c('Bandung Barat', 'Purwakarta'), each = 5),
    .before = tp
  ) %>% 
  pivot_longer(-c(kab, tp), names_to = 'kat_usia', values_to = 'n') %>% 
  group_by(tp, kab) %>% 
  mutate(
    p = n/sum(n),
    kat_usia = str_remove(kat_usia, '_t')
  ) %>% 
  mutate(
    kat_usia = factor(kat_usia, levels = c('da', 'dm', 'dak'))
  )

dat_usia



dat_usia %>% 
  ggplot(
    aes(
      x = p, y = factor(tp, rev(lvl_tp)),
      fill = kat_usia
    )
  ) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  geom_text(
    data = ~ .x %>%
      dplyr::filter(
        p != 0
      ),
    aes(label = scales::percent(p, accuracy = 0.01)),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_brewer(
    palette = 'Greens',
    labels = str_wrap(lvl_usia, 15),
    direction = 1
  ) +
  scale_fill_manual(
    values = rev(my_col[c(2, 3, 5)]),
    labels = str_wrap(lvl_usia, 15)
  ) +
  scale_y_discrete(
    labels = function(x) str_wrap(x, 10)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = scales::percent,
    expand = expansion(mult = c(0.005, 0.03))
  ) +
  facet_grid(kab ~ ., scales = 'free_x', switch = 'y') +
  labs(
    x = NULL, y = NULL, 
    fill = NULL, 
    title = 'Usia Petani Berdasarkan Tingkat Pendidikan'
  ) +
  theme_minimal(base_line_size = 1) +
  theme(
    legend.position = 'bottom',
    legend.justification = "center",
    strip.background = element_blank(),
    strip.placement = "outside", 
    strip.text = element_text(size = 11.5),
    axis.text.x = element_text(size = 10.5),
    panel.grid.major.x = element_line(colour = 'gray', size = 0.7)
  )



ggsave(
  filename = "E:/Visualisasi/riset/1.1 Usia Petani berdasarkan Tingkat Pendidikan_label.png",
  width = 7.5,
  height = 4,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = 'white'
)
# jumlah art --------------------------------------------------------------


s2 <- 
'Bandung Barat	146	47	1
Purwakarta	66	32	1
'

dat_jart <- read_pattern(
  s2, pos_non_angka = 1,
  pos_angka = 2:4,
  col_names = c('kab', '1-4',	'5-8',	'>9')
) %>% 
  pivot_longer(-kab, names_to = 'i_jart', values_to = 'n') %>% 
  type_convert() %>% 
  mutate(
    i_jart = factor(i_jart, levels = c('1-4',	'5-8',	'>9'))
  )

dat_jart %>% 
  mutate(i_jart = fct_rev(i_jart)) %>% 
  ggplot(aes(x = n, y = kab, fill = i_jart)) +
  geom_bar(stat = 'identity') +
  geom_text(
    data = ~ .x %>% 
      dplyr::filter(
        n != 0
      ),
    aes(label = n),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  scale_fill_brewer(
    palette = 'Greens',
    direction = 1
  ) +
  scale_y_discrete(
    labels = function(x) str_wrap(x, 10)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.005, 0.01))
  ) +
  labs(
    x = NULL, y = 'Kabupaten', fill = NULL,
    title = 'Interval Jumlah Anggota Rumah Tangga'
  ) +
  theme_minimal(base_line_size = 1, base_size = 12.5) +
  theme(
    legend.position = 'bottom',
    legend.justification = "center",
    panel.grid.major.x = element_line(colour = 'gray', size = 0.7)
  )



ggsave(
  filename = "E:/Visualisasi/riset/1.3 Jumlah Anggota Rumah Tangga_label.png",
  width = 7,
  height = 3,
  units = "in",
  dpi = 300,
  scale = 1,
  bg = 'white'
)



# harga jual lahan berdasarkan komoditasnya -------------------------------

s3 <- 'Padi	92	33	15
Palawija	23	6	2
Hortikultura	5	4	1
Lainnya	13	0	0
Padi	74	10	2
Palawija	7	0	0
Hortikultura	2	2	0
Lainnya	2	0	0'

dat_harga <- 
  dm::read_string(
    s3, col_names = c('komoditas', '< 300',	'300 - 600',	'> 600') 
  ) %>% 
  mutate(
    kab = rep(c('Bandung Barat', 'Purwakarta'), each = 4),
    .before = komoditas
  ) %>% 
  pivot_longer(-c(kab, komoditas), names_to = 'harga_jual', values_to = 'n') %>% 
  group_by(komoditas, kab) %>% 
  mutate(
    harga_jual = factor(harga_jual, levels = rev(c('< 300',	'300 - 600',	'> 600'))),
    p = n/sum(n)
  )



lvl_komoditas <- c('Padi', 'Palawija', 'Hortikultura', 'Lainnya')

ggplot(
  dat_harga,
  aes(
    x = p, y = factor(komoditas, rev(lvl_komoditas)),
    fill = harga_jual
  )
) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  geom_text(
    data = ~ .x %>%
      dplyr::filter(
        p != 0
      ),
    aes(label = scales::percent(p, accuracy = 0.01)),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_manual(values = rev(my_col[c(2, 3, 5)])) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = scales::percent,
    expand = expansion(mult = c(0.005, 0.03))
  ) +
  facet_grid(kab ~ ., scales = 'free_x', switch = 'y') +
  labs(
    x = NULL, y = NULL, 
    fill = NULL, 
    title = 'Harga Jual Lahan berdasarkan Komoditasnya'
  ) +
  theme_minimal(base_line_size = 1) +
  theme(
    legend.position = 'bottom',
    legend.justification = "center",
    strip.background = element_blank(),
    strip.placement = "outside", 
    strip.text = element_text(size = 11.5),
    axis.text.x = element_text(size = 10.5),
    panel.grid.major.x = element_line(colour = 'gray', size = 0.7)
  )



ggsave(
  filename = "E:/Visualisasi/riset/1.6 Harga Jual Lahan berdasarkan Komoditasnya.png",
  width = 6.7,
  height = 3.7,
  units = "in",
  dpi = 300,
  scale = 1,
  bg = 'white'
)

library(dm)
library(tidyverse)

loc_hasil <- 'E:/Visualisasi/riset/penanggulangan outlier'
my_col <- c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')

# 1.7 Usia Petani berdasarkan tingkat pendidikan ------------------------------
lvl_tp <- c('Tidak Bersekolah', 'Tamat SD', 'Tamat SMP', 'Tamat SMA', 'Tamat PT')
lvl_usia <- c('Dewasa Awal (18-40)', 'Dewasa Madya (41-60) ', 'Dewasa Akhir (>60)')



s1 <- 'Tidak Bersekolah	0	4	11
Tamat SD	18	38	32
Tamat SMP	27	8	9
Tamat SMA	19	16	2
Tamat PT	3	7	0
Tidak Bersekolah	1	11	16
Tamat SD	3	25	17
Tamat SMP	3	3	1
Tamat SMA	2	9	0
Tamat PT	0	7	1
'

dat_usia <- dm::read_pattern(
    s1, pos_non_angka = 1,
    pos_angka = 2:4,
    col_names = c('tp', c('da', 'dm', 'dak'))
  ) %>% 
  type_convert() %>% 
  mutate(
    kab = rep(c('Bandung Barat', 'Purwakarta'), each = 5),
    .before = tp
  ) %>% 
  pivot_longer(-c(kab, tp), names_to = 'kat_usia', values_to = 'n') %>% 
  group_by(tp, kab) %>% 
  mutate(
    p = n/sum(n)
  ) %>% 
  mutate(
    tp = factor(tp, levels = lvl_tp),
    kat_usia = factor(kat_usia, levels = c('da', 'dm', 'dak'), labels = lvl_usia)
  )

dat_usia



dat_usia %>% 
  mutate(col_text = if_else(kat_usia == lvl_usia[3], 'w', 'b')) %>% 
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
    aes(label = scales::percent(p, accuracy = 0.01), colour = col_text),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_brewer(
    palette = 'Greens',
    labels = str_wrap(lvl_usia, 15),
    direction = 1
  ) +
  # scale_fill_manual(
  #   values = rev(my_col[c(2, 3, 5)]),
  #   labels = str_wrap(lvl_usia, 15)
  # ) +
  scale_colour_manual(values = c('black', 'white')) +
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
  guides(colour = 'none') +
  theme_minimal(base_line_size = 1) +
  theme(
    legend.position = 'bottom',
    legend.justification = "center",
    strip.background = element_blank(),
    strip.placement = "outside", 
    strip.text = element_text(size = 11.5),
    axis.text.x = element_text(size = 10.5),
    panel.grid.major = element_line(colour = 'gray', size = 0)
  )



ggsave(
  filename = file.path(
    loc_hasil,
    '1.1 Usia Petani berdasarkan Tingkat Pendidikan_label.png'
  ),
  width = 7.5,
  height = 4,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = 'white'
)
# 1.2 jumlah art --------------------------------------------------------------
lvl_jart <- c('1-2', '3-4', '5-6', '7-8',	'>8')

s2 <- 
'Bandung Barat	33	17,0%	113	58,2%	35	18,0%	12	6,2%	1	0,5%
Purwakarta	24	24,2%	42	42,4%	25	25,3%	7	7,1%	1	1,0%
'

dat_jart <- read_pattern(
  s2, pos_non_angka = 1:11,
) %>% 
  dplyr::select(-c(3, 5, 7, 9, 11)) %>% 
  setNames(c('kab', lvl_jart)) %>% 
  pivot_longer(-kab, names_to = 'i_jart', values_to = 'n') %>% 
  type_convert() %>% 
  mutate(
    i_jart = factor(i_jart, levels = lvl_jart)
  ) %>% 
  group_by(kab) %>% 
  mutate(
    p = n/sum(n)
  ) 




dat_jart %>% 
  mutate(
    i_jart = fct_rev(i_jart),
    col_text = if_else(i_jart == lvl_jart[1:2], 'w', 'b')   
  ) %>% 
  ggplot(
    aes(x = p, y = kab, fill = i_jart)) +
  geom_bar(stat = 'identity') +
  geom_text(
    data = ~ .x %>% 
      dplyr::filter(
        p != 0,
        i_jart != '>8'
      ),
    aes(label = scales::percent(p, accuracy = 0.01), colour = col_text),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  scale_fill_manual(
    values = rev(my_col[c(1, 2, 3, 4, 5)])
  ) +
  scale_y_discrete(
    labels = function(x) str_wrap(x, 10)
  ) +
  scale_x_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0.005, 0.05))
  ) +
  scale_colour_manual(values = c('black', 'white')) +
  guides(colour = 'none') +
  labs(
    x = NULL, y = 'Kabupaten', fill = NULL,
    title = 'Interval Jumlah Anggota Rumah Tangga'
  ) +
  coord_cartesian(clip = 'off') +
  theme_minimal(base_line_size = 1, base_size = 12.5) +
  theme(
    legend.position = 'bottom',
    legend.justification = "center",
    panel.grid.major = element_line(colour = 'gray', size = 0)
  )



ggsave(
  filename = file.path(
    loc_hasil,
    '1.2 Jumlah Anggota Rumah Tangga_label.png'
  ),
  width = 6,
  height = 3,
  units = "in",
  dpi = 300,
  scale = 1.1,
  bg = 'white'
)



# 1.18 harga jual lahan berdasarkan komoditasnya -------------------------------

s3 <- 'Padi	53	30	19	11	23
Palawija	20	8	3	2	4
Hortikultura	2	4	3	0	2
Lainnya	10	0	0	0	0
Padi	44	28	2	5	7
Palawija	5	2	0	0	0
Hortikultura	2	1	0	1	0
Lainnya	1	1	0	0	0'

lvl_harga_jual <- c('1-150', '151-300',	'301-450', '451-600',	'> 600')

dat_harga <- 
  dm::read_string(
    s3, col_names = c('komoditas', lvl_harga_jual) 
  ) %>% 
  mutate(
    kab = rep(c('Bandung Barat', 'Purwakarta'), each = 4),
    .before = komoditas
  ) %>% 
  pivot_longer(-c(kab, komoditas), names_to = 'harga_jual', values_to = 'n') %>% 
  group_by(komoditas, kab) %>% 
  mutate(
    harga_jual = factor(harga_jual, levels = rev(lvl_harga_jual)),
    p = n/sum(n)
  )
dat_harga


lvl_komoditas <- c('Padi', 'Palawija', 'Hortikultura', 'Lainnya')

dat_harga %>% 
  mutate(
    col_text = if_else(harga_jual %in% lvl_harga_jual[1:2], 'w', 'b')
  ) %>% 
  ggplot(
  aes(
    x = p, y = factor(komoditas, rev(lvl_komoditas)),
    fill = harga_jual, 
  )
  ) +
  geom_bar(stat = 'identity', position = position_stack(), width = 0.85) +
  geom_text(
    data = ~ .x %>%
      dplyr::filter(
        p != 0
      ),
    aes(
      colour = col_text,
      label = scales::percent(p, accuracy = 0.1)
    ),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_colour_manual(values = c('black', 'white')) +
  # scale_fill_brewer(palette = 'Greens') +
  scale_fill_manual(values = rev(my_col[c(1, 2, 3, 4, 5)])) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = scales::percent,
    expand = expansion(mult = c(0.005, 0.03))
  ) +
  facet_grid(kab ~ ., scales = 'free_x', switch = 'y') +
  labs(
    x = NULL, y = NULL, 
    fill = NULL, 
    title = expression('Harga Jual Lahan berdasarkan Komoditasnya (ribu per' ~ m^2 ~ ')')
  ) +
  theme_minimal(base_line_size = 1) +
  guides(
    colour = 'none'
  ) +
  theme(
    legend.position = 'bottom',
    legend.justification = "center",
    strip.background = element_blank(),
    strip.placement = "outside", 
    strip.text = element_text(size = 11.5),
    axis.text.x = element_text(size = 10.5),
    panel.grid.major = element_line(colour = 'gray', size = 0)
  )


file.path(loc_hasil, '1.18 Harga Jual Lahan berdasarkan Komoditasnya.png')
ggsave(
  filename = file.path(
    loc_hasil,
    '1.18 Harga Jual Lahan berdasarkan Komoditasnya.png'
  ),
  width = 7.5,
  height = 4,
  units = "in",
  dpi = 300,
  scale = 1,
  bg = 'white'
)

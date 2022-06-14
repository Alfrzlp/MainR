pak::pak('GGally')

library(GGally)
ggpairs(dat_jatim[, -1])


dat_jatim$nmkab
dat_jatim <- dat_jatim %>% 
  extract(col = nmkab, into = c('status', 'nmkab'), regex = '(.*)\\s(.*)')


table(dat_jatim$status)





# -------------------------------------------------------------------------
dat_jatim_full <- 
  readxl::read_xlsx('D:/tugas/dataPA_jatim.xlsx')


ggplot(dat_jatim_full, aes(x = hls, y = pm20)) +
  geom_point() +
  scale_y_continuous(
    labels = function(x) scales::percent(x/100)
  ) +
  geom_smooth(method = 'lm') +
  labs(
    x = 'Angka Harapan Lama Sekolah',
    y = 'Persentase Penduduk Miskin'
  )


ggplot(dat_jatim_full, aes(x = tpt20, y = pm20)) +
  geom_point() +
  scale_y_continuous(
    labels = function(x) scales::percent(x/100)
  ) +
  geom_smooth(method = 'lm') +
  labs(
    x = 'Tingkat Pengangguran Terbuka (TPT)',
    y = 'Persentase Penduduk Miskin'
  )


ggplot(dat_jatim_full, aes(x = rk, y = pm20)) +
  geom_point() +
  scale_y_continuous(
    labels = function(x) scales::percent(x/100)
  ) +
  geom_smooth(method = 'lm') +
  labs(
    x = 'Rasio Ketergantungan',
    y = 'Persentase Penduduk Miskin'
  )



dat_jatim_full %>% 
  select(nmkab, tpt20, pm20, hls, rk) %>% 
  pivot_longer(-c(nmkab, pm20)) %>% 
  ggplot(aes(x = value, y = pm20)) +
  geom_point() +
  scale_y_continuous(
    labels = function(x) scales::percent(x/100)
  ) +
  geom_smooth(method = 'lm') +
  labs(
    x = NULL,
    y = 'Persentase Penduduk Miskin'
  ) + 
  facet_grid(~name, scales = 'free',
             labeller = as_labeller(
              c(
                'hls'='Angka Harapan Lama Sekolah',
                'rk'='Rasio Ketergantungan',
                'tpt20'='Tingkat Pengangguran Terbuka (TPT)'
              ) 
             )) +
  theme(
    strip.text = element_text(size = 11)
  )



ggsave(
  filename = 'E:/Visualisasi/tugas/gab2.png',
  dpi = 500,
  width = 10,
  height = 4,
  scale = 0.9
)



dat_jatim_full %>% 
  select(nmkab, tpt20, hls, rk) %>% 
  pivot_longer(-c(nmkab, tpt20)) %>% 
  ggplot(aes(x = value, y = tpt20)) +
  geom_point() +
  scale_y_continuous(
    labels = function(x) scales::percent(x/100)
  ) +
  geom_smooth(method = 'lm') +
  labs(
    x = NULL,
    y = 'Tingkat Pengangguran Terbuka (TPT)'
  ) + 
  facet_grid(~name, scales = 'free',
             labeller = as_labeller(
               c(
                 'hls'='Angka Harapan Lama Sekolah',
                 'rk'='Rasio Ketergantungan'
               ) 
             )) +
  theme(
    strip.text = element_text(size = 11)
  )


# -------------------------------------------------------------------------
ggplot(dat_jatim, aes(x = hls, y = tpt20)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    x = 'Angka Harapan Lama Sekolah',
    y = 'Tingkat Pengangguran Terbuka'
  ) +
  facet_grid(~status, scales = 'free_x')



ggplot(dat_jatim, aes(x = rk, y = tpt20)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    x = 'Rasio Ketergantungan',
    y = 'Tingkat Pengangguran Terbuka'
  ) +
  facet_grid(~status, scales = 'free_x')



# -------------------------------------------------------------------------
s <- 'SD	4,34%
SLTP	11,10%
SLTA	59,55%
DIPLOMA I	0,58%
DIPLOMA II	0,50%
DIPLOMA III	4,29%
DIPLOMA IV	4,42%
SARJANA (S1)	10,89%
MAGISTER (S2)	0,18%
Tidak diKetahui	4,15%'

library(dm)

dat <- read_pattern(s, 1, 2) %>% 
  separator_convert(v2) %>% 
  rename(p = v2)

dat %>% 
  ggplot(aes(x = p, y = factor(v1, levels = rev(v1)))) +
  geom_col(fill = 'steelblue') +
  geom_text(
    aes(label = paste(p, '%')),
    nudge_x = 5,
    size = 3.1,
    color = 'black',
    fontface = 1.5
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(NA, 0.15)),
    labels = function(x) scales::percent(x/100)
  ) +
  scale_y_discrete(
    labels = function(x) str_wrap(x, 15)
  ) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    panel.grid.major.x = element_line(colour = 'gray80')
  ) +
  labs(
    x = 'Persentase',
    y = 'Pendidikan Tertinggi',
    title = str_wrap('Pencari Kerja Terdaftar Menurut Pendidikan Tertinggi yang Ditamatkan', 50)
  ) 
  scale_fill_manual(
    values = c('#FDD0A2', "#FD8D3C", "#E6550D", "#A63603")
  )

  
  
  ggsave(
    filename = 'E:/Visualisasi/tugas/carikerja.png',
    bg = 'white',
    dpi = 500,
    width = 6,
    height = 5,
    scale = 0.9
  )

  
  

# -------------------------------------------------------------------------
s <- '15–19	3,16%
20–24	8,06%
25–29	9,71%
30–34	10,41%
35–39	11,23%
40–44	11,48%
45+	45,95%
'
  
  
dat2 <- read_pattern(s, 1, 2) %>% 
    separator_convert(v2) %>% 
    rename(p = v2)
  
dat2  

dat2 %>% 
    ggplot(aes(x = p, y = factor(v1, levels = rev(v1)))) +
    geom_col(fill = 'steelblue') +
    geom_text(
      aes(label = paste(p, '%')),
      nudge_x = 3,
      size = 3.1,
      color = 'black',
      fontface = 1.5
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(NA, 0.15)),
      labels = function(x) scales::percent(x/100)
    ) +
    scale_y_discrete(
      labels = function(x) str_wrap(x, 15)
    ) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      panel.grid.major.x = element_line(colour = 'gray80')
    ) +
    labs(
      x = 'Persentase',
      y = 'Kelompok Umur',
      title = str_wrap('Penduduk Berumur 15 Tahun Keatas yang Bekerja Selama Seminggu yang Lalu', 50)
    )
  
  

# -------------------------------------------------------------------------

dat %>% 
  mutate(
    kat_color = if_else(nmprov == 'INDONESIA', 1, 0)
  ) %>% 
  ggplot(aes(x = p02, y = reorder(nmprov, p02))) +
  geom_col(aes(fill = factor(kat_color))) +
  geom_text(
    aes(x = p02 + 1.5 * sign(p02), label = p02),
    size = 3
  ) +
  scale_y_discrete(
    labels = function(x) str_to_title(x)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    labels = function(x) scales::percent(x/100)
  ) +
  scale_fill_manual(
    values = c('steelblue', 'tomato')
  ) +
  theme_bw() +
  theme(
    legend.position = 'none'
  ) +
  labs(
    x = 'Persentase Penduduk Miskin',
    y = 'Provinsi'
  )



ggsave(
  filename = 'E:/Visualisasi/tugas/ppmindo.png',
  dpi = 500,
  width = 7.5,
  height = 8,
  scale = 0.8
)


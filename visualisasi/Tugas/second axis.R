dat <- structure(list(tahun = c(2021, 2020, 2019, 2018, 2017, 2016, 
                                2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006), 
                      jka = c(975, 928, 884, 845, 944, 932, 644, 741, 706, 707, 
                              682, 666, 612, 587, 567, 486), shu = c(19666, 19920, 16065.5, 
                                                                     14169, 11815, 4432, 23313.21, 27134, 22111, 17898, 18348, 
                                                                     18265, 15462, 18122, 13643, 48438), volume = c(268169, 285894, 
                                                                                                                    198287, 244299, 216042, 42692, 410781, 353778, 336555, 273962, 
                                                                                                                    272451, 271242, 224051, 247412, 217651, 233137)), row.names = c(NA, 
                                                                                                                                                                                    -16L), class = "data.frame")
dat %>% glimpse()


ggplot(dat, aes(x = tahun, y = jka)) +
  geom_col(fill = 'steelblue') +
  theme_bw() +
  labs(
    x = 'Tahun',
    y = 'Jumlah Koperasi Aktif',
    title = 'Jumlah Koperasi Aktif Tahun 2006-2021',
    subtitle = 'Provinsi Gorontalo'
  ) +
  theme(
    plot.title = element_text(color = 'black', face = 2),
    plot.subtitle = element_text(color = 'gray35')
  )

ggplot(dat, aes(x = tahun, y = shu)) +
  geom_col(fill = 'tan1') +
  theme_bw() +
  labs(
    x = 'Tahun',
    y = 'Sisa Hasil Usaha (Juta Rupiah)',
    title = 'Sisa Hasil Usaha Tahun 2006-2021',
    subtitle = 'Provinsi Gorontalo'
  ) +
  theme(
    plot.title = element_text(color = 'black', face = 2),
    plot.subtitle = element_text(color = 'gray35')
  )







# Second Axis -------------------------------------------------------------
dat %>% 
  mutate(volume = volume * 0.07) %>% 
  pivot_longer(-tahun, names_to = 'variabel') %>% 
  dplyr::filter(variabel != 'jka') %>% 
  ggplot(aes(x = tahun, y = value)) +
  geom_col(
    aes(fill = variabel),
    position = position_dodge()
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~./0.07, name="Volumen Usaha Koperasi")
  ) +
  scale_fill_manual(
    values = c("#E6550D", '#FDD0A2')
  ) +
  theme_bw() +
  labs(
    x = 'Tahun',
    y = 'Sisa Hasil Usaha (Juta Rupiah)',
    title = 'SHU dan Volume Usaha Koperasi Tahun 2006-2021',
    subtitle = 'Provinsi Gorontalo'
  ) +
  theme(
    plot.title = element_text(color = 'black', face = 2),
    plot.subtitle = element_text(color = 'gray35')
  )

lm(shu ~ volume, data = dat[-16,]) %>% summary()



ggplot(as.data.frame(scale(dat[-16,])), aes(x = volume, y = shu)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    y = 'Sisa Hasil Usaha (Juta Rupiah)',
    x = 'Volume Usaha Koperasi'
  )




# -------------------------------------------------------------------------
ggsave(
  filename = "E:/Visualisasi/tugas/hubungan.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 500,
  scale = 0.9,
  bg = 'white'
)

ump <- '11	ACEH	3,166,460.00	3,165,031.00	3,165,031.00
12	SUMATERA UTARA	2,522,609.94	2,499,423.06	2,499,423.06
13	SUMATERA BARAT	2,512,539.00	2,484,041.00	2,484,041.00
14	RIAU	2,938,564.01	2,888,564.01	2,888,564.01
15	JAMBI	2,649,034.24	2,630,162.13	2,630,162.13
16	SUMATERA SELATAN	3,144,446.00	3,144,446.00	3,043,111.00
17	BENGKULU	2,238,094.31	2,215,000.00	2,213,604.00
18	LAMPUNG	2,440,486.18	2,432,001.57	2,432,001.57
19	KEP. BANGKA BELITUNG	3,264,884.00	3,230,023.66	3,230,023.66
21	KEP. RIAU	3,050,172.00	3,005,460.00	3,005,460.00
31	DKI JAKARTA	4,453,935.54	4,416,186.55	4,276,349.91
32	JAWA BARAT	1,841,487.31	1,810,351.36	1,810,351.36
33	JAWA TENGAH	1,812,935.43	1,798,979.12	1,742,015.22
34	DI YOGYAKARTA	1,840,915.53	1,765,000.00	1,704,608.25
35	JAWA TIMUR	1,891,567.12	1,868,777.08	1,768,777.08
36	BANTEN	2,501,203.11	2,460,996.54	2,460,996.54
51	BALI	2,516,971.00	2,494,000.00	2,494,000.00
52	NUSA TENGGARA BARAT	2,207,212.00	2,183,883.00	2,183,883.00
53	NUSA TENGGARA TIMUR	1,975,000.00	1,950,000.00	1,950,000.00
61	KALIMANTAN BARAT	2,434,328.19	2,399,698.65	2,399,698.65
62	KALIMANTAN TENGAH	2,922,516.00	2,903,144.70	2,903,144.70
63	KALIMANTAN SELATAN	2,906,473.32	2,877,448.59	2,877,448.59
64	KALIMANTAN TIMUR	3,014,497.22	2,981,378.72	2,981,378.72
65	KALIMANTAN UTARA	3,016,738.00	3,000,804.00	3,000,804.00
71	SULAWESI UTARA	3,310,723.00	3,310,723.00	3,310,723.00
72	SULAWESI TENGAH	2,390,739.00	2,303,711.00	2,303,711.00
73	SULAWESI SELATAN	3,165,876.00	3,165,876.00	3,103,800.00
74	SULAWESI TENGGARA	2,710,595.92	2,552,014.52	2,552,014.52
75	GORONTALO	2,800,580.00	2,788,826.00	2,788,826.00
76  SULAWESI BARAT 2,678,863.10	2,678,863.10	2,678,863.10
81	MALUKU	2,619,312.83	2,604,961.00	2,604,961.00
82	MALUKU UTARA	2,862,231.00	2,721,530.00	2,721,530.00
91	PAPUA BARAT	3,200,000.00	3,134,600.00	3,134,600.00
94	PAPUA	3,561,932.00	3,516,700.00	3,516,700.00
'

inflasi <- '1
1
0
1
1
0
1
0
1
1
0
0
0
0
1
0
0
1
0
1
1
1
0
0
1
1
0
1
1
1
0
1
1
0
'

library(dm)
dat <- readxl::read_xlsx('D:/tugas/data mca.xlsx')
dat <- dat %>% 
  set_names(c('nmprov', colnames(dat)[-1])) %>% 
  janitor::clean_names()
dat



ump <- read_pattern(
  ump,
  pos_non_angka = 2:5,
  pos_angka = 1
) %>% 
  separator_convert(v3:v5, to_sep = '', to_numeric = T) %>% 
  select(nmprov = v2, ump21 = v4) %>% 
  as_tibble() %>% 
  mutate(
    nmprov = str_trim(nmprov)
  )

setdiff(dat$nmprov, ump$nmprov)
setdiff(ump$nmprov, dat$nmprov)

indo <- dat[35, ]
indo

dat <- dat %>% 
  right_join(ump, by = 'nmprov')  

dat <- dat %>% 
  mutate(
    pkb = str_replace_all(str_remove_all(pkb, '\\s'), ',', '\\.'),
    across(-1, ~ as.numeric(.x))
  ) 



glimpse(dat)




dat_final <- dat %>% 
  mutate(
    gr1 = if_else(gini_ratio1 > indo$gini_ratio1, 1, 0),
    gr2 = if_else(gini_ratio2 > indo$gini_ratio2, 1, 0),
    
    gk1 = if_else(gk_kota1 > indo$gk_kota1 & gk_desa1 > indo$gk_desa1, 1, 0),
    gk2 = if_else(gk_kota2 > indo$gk_kota2 & gk_desa2 > indo$gk_desa2, 1, 0),
    
    pkb = cut(pkb, breaks = quantile(pkb), include.lowest = TRUE),
    pkb = as.numeric(pkb),
    ipm = cut(ipm, breaks = quantile(ipm), include.lowest = TRUE),
    ipm = as.numeric(ipm),
    
    gk1 = if_else(nmprov == 'DKI JAKARTA', 1, 0),
    gk2 = if_else(nmprov == 'DKI JAKARTA', 1, 0),
    
    .after = nmprov,
    .keep = 'unused'
  ) %>% 
  as.data.frame()

dat_final$nmprov <- dat$nmprov
dat_final$inflasi <- str2vec(inflasi)

dat_final <- dat_final %>% 
  relocate(nmprov, ump21, .before = gr1)
  
dat_final <- dat_final %>% 
  select(-c(gr1, gk1)) %>% 
  rename(gk = gk2, gr = gr2)

dat_final <- dat_final %>% 
  mutate_at(vars(gr:inflasi), ~ as.factor(.x))

dat_final %>% 
  glimpse()

  
dat
dat_final2 <- dat_final %>% 
  filter(ump21 != max(ump21))



# Model -------------------------------------------------------------------
m1 <- aov(ump21 ~ gk + pkb + ipm + inflasi + gr, 
          data = dat_final)
summary(m1)


m1 <- aov(ump21 ~ gk + pkb + ipm + gr:pkb, 
          data = dat_final)
summary(m1)
step(m1)



m1 <- aov(ump21 ~ (gr + gk + pkb + ipm + inflasi)^2, data = dat_final)
m1 <- aov(ump21 ~ gk + gr:inflasi, data = dat_final)
summary(m1)






dat_final %>% 
  openxlsx::write.xlsx('D:/tugas/dataMCA.xlsx')

dat_final <- dat_final %>% 
  rename(gr = gr2, gk = gk2)



pkb <- dat %>% 
  select(pkb) %>% 
  mutate(
    p = cut(pkb, breaks = c(840358.6, 1112809.9, 1188903.8, 1412359.2, 2336429.5), include.lowest = TRUE),
    p = as.numeric(p)
  ) %>% 
  pull(p) 
  as.data.frame()



dat_final$pkb <- pkb



# -------------------------------------------------------------------------
dat_final <- readxl::read_xlsx('D:/tugas/dataMCA.xlsx')
dat_final <- dat_final %>% 
  mutate(
    gr = factor(gr, levels = 0:1),
    gk = factor(gr, levels = 0:1),
    pkb = factor(pkb, levels = 1:4),
    ipm = factor(ipm, levels = 1:4),
    inflasi = factor(inflasi, levels = 0:1),
  )

# viz ---------------------------------------------------------------------
# Inflasi
ggplot(dat_final, aes(y = ump21, x = inflasi, fill = inflasi)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = str_wrap(
      c('Dibawah Inflasi Nasional', 'Diatas Inflasi Nasional'),
      15
    )
  ) +
  scale_y_continuous(
    labels = function(x) scales::dollar(x/1000000, prefix = '', accuracy = 0.1, suffix = ' Juta')
  ) +
  labs(
    title = 'Upah Minimum Provinsi berdasarkan Inflasi',
    y = 'Upah Minimum Provinsi',
    x = 'Inflasi'  
  ) +
  guides(fill = guide_none()) +
  theme_bw()




# Garis Kemiskinan
ggplot(dat_final, aes(y = ump21, x = gk, fill = gk)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = str_wrap(
      c('Dibawah Garis Kemiskinan Nasional', 'Diatas Garis Kemiskinan Nasional'),
      20
    )
  ) +
  scale_y_continuous(
    labels = function(x) scales::dollar(x/1000000, prefix = '', accuracy = 0.1, suffix = ' Juta')
  ) +
  labs(
    title = str_wrap(
      'Upah Minimum Provinsi berdasarkan Garis Kemiskinan',
      35
    ),
    y = 'Upah Minimum Provinsi',
    x = NULL  
  ) +
  guides(fill = guide_none()) +
  theme_bw()



# Gini Ratio
ggplot(dat_final, aes(y = ump21, x = gr, fill = gr)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = str_wrap(
      c('Dibawah Gini Ratio Nasional', 'Diatas Gini Ratio Nasional'),
      20
    )
  ) +
  scale_y_continuous(
    labels = function(x) scales::dollar(x/1000000, prefix = '', accuracy = 0.1, suffix = ' Juta')
  ) +
  labs(
    title = str_wrap(
      'Upah Minimum Provinsi berdasarkan Gini Ratio ',
      50
    ),
    y = 'Upah Minimum Provinsi',
    x = NULL  
  ) +
  guides(fill = guide_none()) +
  theme_bw()


# ipm
ggplot(dat_final, aes(y = ump21, x = ipm, fill = ipm)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = 
      paste0('Kuartil ', 1:4)
  ) +
  scale_y_continuous(
    labels = function(x) scales::dollar(x/1000000, prefix = '', accuracy = 0.1, suffix = ' Juta')
  ) +
  labs(
    title = str_wrap(
      'Upah Minimum Provinsi berdasarkan Kategori IPM',
      50
    ),
    y = 'Upah Minimum Provinsi',
    x = NULL  
  ) +
  guides(fill = guide_none()) +
  theme_bw()


# pkb
ggplot(dat_final, aes(y = ump21, x = pkb_new, fill = pkb_new)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = 
      paste0('Kuartil ', 1:6)
  ) +
  scale_y_continuous(
    labels = function(x) scales::dollar(x/1000000, prefix = '', accuracy = 0.1, suffix = ' Juta')
  ) +
  labs(
    title = str_wrap(
      'Upah Minimum Provinsi berdasarkan Kuartil PKB',
      50
    ),
    y = 'Upah Minimum Provinsi',
    x = NULL  
  ) +
  guides(fill = guide_none()) +
  theme_bw()


ggsave(
  filename = "E:/Visualisasi/tugas/pkb.png",
  width = 5,
  height = 3.5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)
#

dat %>% dplyr::filter(gini_ratio2 == min(gini_ratio2))
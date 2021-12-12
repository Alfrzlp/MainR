
# Soal 2017/2018 ----------------------------------------------------------
s <- 
'Keanggotaan Kelompok Tani 1.241 .074
Akses Sarana Produksi Pertanian .669 .080
Penyuluhan Pertanian -.934 .073
Bantuan/Subsidi .625 .081
Jenis Kelamin KRT .975 .064
Jumlah ART .330 .011
Pendidikan KRT 1.617 .144
Umur KRT by Pendidikan KRT -.008 .002
Constant -5.937 .132'



data.frame(s = str_split(s, '\n')[[1]]) %>% 
  extract(s, into = c('var', 'b', 'se'),
          regex = '^(\\D+)\\s(-?\\d*?\\.\\d*|-?\\d+)\\s(-?\\d*?\\.\\d*|-?\\d+)$',
          convert = T) %>% 
  mutate(
    wald = (b/se)^2,
    pvalue = round(pchisq(wald, 1,  lower.tail = F), 3)
  )

exp(.215 - 0.933)/(exp(.215 - 0.933) + 1)

xpander = c(0.213, -0.247, 0.00904)
avanza = c(3.115, -0.684, 0.459)
ertiga = c(-1.895, -1.459, 0.284)
mobilio = 0

# predict
x = c(1, 1, 0)
xpander*x

z = c(sum(xpander*x), sum(avanza*x), sum(ertiga*x), sum(mobilio*x))
softmax <- function(z) exp(z)/sum(exp(z))
softmax(z)

# Kategori refernsi menjadi Avanza
# xpander
xpander - avanza
# avanza
avanza - avanza
# ertiga
ertiga - avanza
# mobilio
# ln(mobilio/avanza) = ln(1 / (avanza/mobilio))
# = ln(1) - ln(avanza/mobilio)
log(1) - avanza



# Soal 2019/2020 ----------------------------------------------------------

s <- 'Intercept 23.7380 2.1190 [1] [2]
Jeniskelamin -0.8770 0.2280 [3] [4]
Umur -0.7015609 0.0640 120.1630 [6]
Hobi -0.6540 0.2290 [7] [8]
Intercept 11.1340 1.5310 [9] [10]
Jeniskelamin -0.4250 0.1920 [11] [12]
Umur -0.3177459 0.0450 49.8580 [14]
Hobi 0.0570 0.1930 [15] [16]'


# Mencari estimasi
# wald = (estimasi/se)^2
# wald*se^2 = estimasi^2
# sqrt(wald*se^2) = estimasi
sqrt(120.163*0.064^2)
sqrt(49.858*0.045^2)

exp(-sqrt(120.163*0.064^2))
exp(sqrt(49.858*0.045^2))

read.table(textConnection(s), header = F) %>% 
  `colnames<-`(c('nama', 'estimasi', 'se', 'wald', 'or')) %>% 
  mutate_at(2:5, ~ as.numeric(.x)) %>% 
  mutate(
    wald = (estimasi/se)^2,
    or = exp(estimasi)
  ) %>% 
  as_tibble()

suv = c(23.7380, -0.8770, -0.7015609, -0.6540)
mpv = c(11.1340, -0.4250, -0.3177459, 0.0570)
cc = 0

# prediksi
x = c(1, 1, 30, 0)
# Nilai sum(x*suv) akan memberikan nilai beta + x*beta + .. dst

# output fungsi sofmax adalah 
# Nilai X*beta
c(sum(x*suv), sum(x*mpv), sum(x*cc))
# nilai peluang dari suv, mpv, dan city car
softmax(c(sum(x*suv), sum(x*mpv), sum(x*cc)))

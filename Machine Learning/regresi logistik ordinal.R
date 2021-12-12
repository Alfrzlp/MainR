# y = 1 Korban Luka Ringan
# y = 2 Korban Luka Berat
# y = 3 Korban Meninggal Dunia
# 
# 1 = 0-9 tahun
# 2 = 10-15 tahun
# 3 = 16-30 tahun
# 4 = 31-40 tahun
# 5 = 41-50 tahun
# 6 = 51 tahun ke atas
# 
# 1 = Tunggal
# 2 = Depan Depan
# 3 = Depan Belakang
# 4 = Depan Samping
# 5 = Samping Samping
# 6 = Beruntun
# 7 = Tabrak Manusia
# 8 = Tabrak Hewan
# 9 = Lain-lain
# 
# 1 = Laki-laki
# 2 = Perempuan
# 
# 1 = Sepeda Motor
# 2 = Kendaraan Penumpang
# 3 = Kendaraan Barang
# 4 = Kendaraan Bus
# 5 = Kendaraan Khusus
# 
# 1 = Pukul 00.00-06.00
# 2 = Pukul 06.00-12.00
# 3 = Pukul 12.00-18.00
# 4 = Pukul 18.00-00.00
# 
# 1 = Lengah
# 2 = Lelah
# 3 = Mengantuk
# 4 = Sakit
# 5 = Tidak tertib
# 6 = Tekanan Psikologis
# 7 = Pengaruh Obat
# 8 = Pengaruh Alkohol atau Minuman Keras
# 9 = Batas Kecepatan

parah <- readxl::read_xlsx('D:/_Datasets/keparahan_kecelakaan.xlsx') %>% 
  select(-no)
parah

library(MASS)
library(Hmisc)
parah <- parah %>% 
  mutate(keparahan_kecelakaan = factor(keparahan_kecelakaan, 1:3))

m <- polr(keparahan_kecelakaan ~ ., data = parah, Hess=TRUE)
summary(m)

(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

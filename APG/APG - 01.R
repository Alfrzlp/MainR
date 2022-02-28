s1 <- 'Aceh 19,5 16,8 - 22,6 39,6 40,9 464
Sumatera Utara 32,7 29,0 - 36,5 48,5 18,8 1.068
Sumatera Barat 38,7 34,4 - 43,2 48,1 13,2 419
Riau 34,4 29,7 - 39,4 51,3 14,3 528
Jambi 62,6 56,9 - 67,9 26,8 10,6 268
Sumatera Selatan 48,3 43,1 - 53,5 42,4 9,3 607
Bengkulu 62,6 56,0 - 68,7 28,1 9,3 137
Lampung 67,3 62,2 - 72,0 25,8 6,9 602
Bangka Belitung 75,2 66,9 - 81,9 16,7 8,1 103
Kepulauan Riau 71,2 59,8 - 80,5 23,3 5,4 185
DKI Jakarta 68,0 60,9 - 74,4 30,4 1,5 688
Jawa Barat 58,3 54,3 - 62,1 35,3 6,5 3.184
Jawa Tengah 75,0 72,1 - 77,8 22,3 2,7 2.003
DI Yogyakarta 83,7 76,0 - 89,3 16,3 0,0 228
Jawa Timur 69,2 66,2 - 72,0 26,3 4,6 2.293
Banten 47,0 40,8 - 53,3 37,4 15,6 875
Bali 92,1 86,8 - 95,3 7,4 0,5 237
Nusa Tenggara Barat 70,8 65,4 - 75,6 24,8 4,5 393
Nusa Tenggara Timur 51,6 47,3 - 55,8 40,5 7,9 493
Kalimantan Barat 48,0 42,8 - 53,3 35,4 16,6 388
Kalimantan Tengah 47,5 41,2 - 53,8 34,2 18,3 198
Kalimantan Selatan 68,7 63,4 - 73,6 24,3 6,9 338
Kalimantan Timur 73,6 66,8 - 79,5 19,9 6,4 302
Kalimantan Utara 73,3 64,8 - 80,4 22,2 4,5 63
Sulawesi Utara 56,9 50,2 - 63,4 37,1 6,0 154
Sulawesi Tengah 47,9 42,3 - 53,5 39,9 12,3 250
Sulawesi Selatan 60,8 56,4 - 65,1 31,7 7,4 660
Sulawesi Tenggara 45,6 39,9 - 51,5 43,0 11,4 223
Gorontalo 61,6 53,3 - 69,4 33,7 4,7 93
Sulawesi Barat 50,2 41,8 - 58,5 39,6 10,2 124
Maluku 33,1 27,4 - 39,4 49,5 17,4 165
Maluku Utara 38,1 31,8 - 44,8 43,8 18,1 106
Papua Barat 47,6 39,2 - 56,1 34,1 18,3 83
Papua 29,2 24,2 - 34,7 37,6 33,2 245'

dm::read_pattern(
  s1, pos_non_angka = c(1, 4),
  pos_angka = c(2, 3, 5:8)
) %>% 
  mutate_at(c(2, 3, 5:7), ~str_replace_all(.x, ',', '.')) %>% 
  mutate_at(8, ~str_replace_all(.x, '\\.', '')) %>% 
  type_convert() %>% 
  pull(v2) -> idl


s <- 'Aceh 53,9 50,7 26,9 24,9 22,0 25,9 37,9 464
Sumatera Utara 62,7 75,8 44,3 43,1 39,2 47,1 65,4 1.068
Sumatera Barat 76,2 81,2 53,7 51,4 45,4 50,5 63,9 419
Riau 74,8 79,6 40,7 39,3 36,7 45,8 68,6 528
Jambi 84,7 86,3 63,7 62,9 60,5 69,0 80,3 268
Sumatera Selatan 79,7 85,8 54,1 53,3 51,1 59,7 78,4 607
Bengkulu 81,1 88,1 68,2 68,4 67,6 72,6 85,1 137
Lampung 84,9 90,7 72,0 71,2 70,1 79,4 83,0 602
Bangka Belitung 88,9 87,7 73,5 72,4 72,0 80,4 81,9 103
Kepulauan Riau 92,7 89,6 79,7 76,0 74,9 77,9 80,0 185
DKI Jakarta 97,2 97,4 77,9 76,9 72,9 78,4 82,1 688
Jawa Barat 86,7 89,7 66,7 65,3 61,9 67,8 78,1 3.184
Jawa Tengah 94,3 94,8 80,3 79,2 77,1 82,6 86,6 2.003
DI Yogyakarta 97,8 99,2 91,0 91,0 91,0 86,9 96,9 228
Jawa Timur 92,1 92,4 75,2 73,7 72,0 76,8 82,8 2.293
Banten 76,6 79,7 58,4 54,8 52,6 57,0 63,8 875
Bali 99,5 98,0 90,8 90,8 90,6 96,0 94,4 237
Nusa Tenggara Barat 90,7 94,0 71,0 71,1 69,2 78,2 87,9 393
Nusa Tenggara Timur 78,6 86,8 49,7 48,5 46,8 65,5 82,5 493
Kalimantan Barat 71,2 77,6 68,2 66,5 61,9 60,3 68,1 388
Kalimantan Tengah 68,4 76,0 60,1 57,9 54,2 56,4 67,0 198
Kalimantan Selatan 86,8 88,7 73,2 70,9 68,6 76,1 81,0 338
Kalimantan Timur 87,1 92,2 75,0 72,8 71,6 80,9 84,1 302
Kalimantan Utara 90,3 93,9 78,2 77,2 75,2 84,8 85,4 63
Sulawesi Utara 81,2 91,9 64,0 62,7 61,8 71,3 84,1 154
Sulawesi Tengah 74,3 80,7 60,0 56,2 53,3 61,2 75,7 250
Sulawesi Selatan 86,5 86,9 67,2 65,8 63,7 69,8 79,8 660
Sulawesi Tenggara 68,1 84,5 54,6 53,6 49,8 59,8 80,2 223
Gorontalo 87,1 91,4 69,2 66,9 66,3 72,8 81,3 93
Sulawesi Barat 78,7 82,6 65,6 63,9 56,9 64,2 75,3 124
Maluku 52,1 75,8 46,3 45,4 42,8 51,9 68,9 165
Maluku Utara 68,9 75,8 54,6 52,0 48,8 50,2 66,0 106
Papua Barat 68,7 75,6 55,0 52,4 50,7 55,3 73,0 83
Papua 42,0 62,5 37,0 36,0 34,0 39,7 60,7 245'

# 12 - 23 Bulan 
dm::read_pattern(
  s, pos_non_angka = 1, pos_angka = 2:9
) %>% 
  mutate_at(2:8, ~str_replace_all(.x, ',', '.')) %>% 
  mutate_at(9, ~str_replace_all(.x, '\\.', '')) %>% 
  type_convert() %>% 
  rename(
    prov =  v1, 
    HB0 = v2,
    BCG = v3,
    DPTHBhib3 = v6,
    polio4 = v7,
    campak = v8
  ) %>% 
  select(c(prov, HB0, BCG, DPTHBhib3, polio4, campak)) %>% 
  mutate(
    idl
  ) %>% 
  openxlsx::write.xlsx('D:/RKD2018_imunisasi.xlsx')

readxl::read_excel('D:/RKD2018_imunisasi.xlsx') %>% 
  select_if(is.numeric) %>% 
  cor()


list(s, s) %>% 
  lapply(
    function (x) dm::read_pattern(
      x, pos_non_angka = 1, pos_angka = 2:9
    )
  ) %>% 
  do.call(rbind, .)



# -------------------------------------------------------------------------


library(readxl) 

# Data --------------------------------------------------------------------
imunisasi <- read_excel('D:/RKD2018_imunisasi.xlsx')
head(imunisasi)


# Matrix (n x p) ----------------------------------------------------------
A <- unname(as.matrix(imunisasi[, -1]))
A

# unname = menghapus nama kolom
# imunisai[, -1] membuang kolom prov


# vektor rata-rata --------------------------------------------------------
colMeans(A)


# matriks ragam-peragam ---------------------------------------------------
# yang berada pada diagonal merupakan varians,
# dan elemen matriks yang lain merupakan covarians
var(A)


# matriks korelasi --------------------------------------------------------
cor(A)

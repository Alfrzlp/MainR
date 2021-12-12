library(tidyverse)

# copy dari soal
input = "
No. Nama Belakang Usia Nilai
1 Jazira Azzahra 22 87
2 Azizah Muthmainah 17 90
3 Aqila Ghina_Rofa 18 72
4 Atiqah Khairunisa 19 55
5 Danis Ara 16 64
6 Hanin Hafidzoh 21 47
7 Tifa Fathiya_R 23 61
1 Afifa Qyla 25 86
2 Annisa Nur_Azizah 25 59
3 Fathimah Mutia 16 41
4 Fida Marsaoly_A 25 60
5 Hurin Tsaqiya_I 19 85
6 Naila Al_Fathonah 24 46
7 Nazhifah Ad_Dhuha 19 50
1 Azizah Fitrya_Achmad 21 82
2 Ahda Khairani 23 57
3 Lathifah Althafunnisa 18 35
4 Nadia IlmaNaila 19 86
5 Nisa R.A 18 71
6 Nadia RahmaFadjrin 16 78
7 Nasywa Ashilla 20 92
1 Alifah Nailatul_A 24 73
2 Hilma Qonita 16 79
3 Annura Latifa_Z 20 93
4 Ala Hadiya_Saadah 16 70
5 Aisyah Afifah_H 18 35
6 Haniyya Salma 22 73
7 Hafsah SYhidah 20 71
1 Dhia Syarafana 24 77
2 T. Khairunisa 22 46
3 Shinta Nurmala 23 68
4 Najwa Mutiara_Harun 25 54
5 Qonita Alya 24 90
6 Adnin _ 22 79
7 Softwatul _ 15 95
1 Aisyah Nasywa 19 42
2 Fathiya Azzahra 17 50
3 Indah Naila 24 45
4 Khadijah Shofiyyah 17 51
5 Nadya Qaulany 17 89
6 Frida Zasiyva 21 82
7 Shafiya Salsabila 19 50
"
df = read.table(textConnection(input), header = T)
df
df = df %>% 
  mutate(Nama = paste(Nama, Belakang),
         Nama = str_replace(Nama, "_", " "),
         kelas = rep(c("A", "B", "C"), each = 14),
         AC = rep(c("ya", "tidak"), each = 7, len = 42)) %>% 
  dplyr::select(-No., -Belakang)
df

# No 1 A
anova = aov(Nilai~kelas, data = df)
summary(anova)

# No 1 B
anova1 = aov(Nilai~kelas*AC, data = df)
summary(anova1)

# No 1 C
a_sebelum = "
No. Nama belakang  Nilai
1 Jazira Azzahra 78
2 Azizah Muthmainah 80
3 Aqila Ghina_Rofa 70
4 Atiqah Khairunisa 56
5 Danis Ara 60
6 Hanin Hafidzoh 48
7 Tifa Fathiya_R 62
1 Afifa Qyla 66
2 Annisa Nur_Azizah 49
3 Fathimah Mutia 41
4 Fida Marsaoly_A 60
5 Hurin Tsaqiya_I 75
6 Naila Al_Fathonah 66
7 Nazhifah Ad_Dhuha 40
"
a_sebelum = read.table(textConnection(a_sebelum), header = T) %>% 
  mutate(Nama = paste(Nama, belakang)) %>% 
  select(-No., -belakang)

a_sebelum

a_sesudah = df %>% filter(kelas == "A") %>% 
  select(Nama, Nilai)

# kita dapatkan 2 data frame
a_sebelum
a_sesudah

t.test(a_sesudah$Nilai, a_sebelum$Nilai, paired = T, alternative = "greater")




# No 2----------------------------------------------------------
input2 = '
tk_pendapatan Sangat_Tidak_Puas sedikit_Puas Puas Puas_Sekali
Rendah 1 3 11 2
Sedang 2 3 17 3
Tinggi 1 1 8 5
Sangat_Tinggi 0 2 4 5
Rendah 1 1 5 1
Sedang 1 3 13 1
Tinggi 1 2 12 3
Sangat_Tinggi 0 1 9 11
'
data_abc = read.table(textConnection(input2), header = T)
data_abc

data_abc = data_abc %>% 
  mutate(jk = rep(c("perempuan", "laki-laki"), each = 4),
         .before = tk_pendapatan)
data_abc


# No 2 A
library(janitor)

data_abc %>% group_by(jk) %>% 
  summarise_if(is.numeric, sum) %>% 
  adorn_totals("col")

hasil = chisq.test(c(65, 68))
prop.test(65, n = 133)




# No 2 B
data2b = data_abc %>% 
  group_by(tk_pendapatan) %>% 
  summarise_if(is.numeric, sum) %>% 
  select(-tk_pendapatan)

data2b = as.matrix(data2b) %>% as.table()

hasil = chisq.test(data2b)
hasil$expected

colSums(data2b[1:2,])



# No 2 C
data_abc %>% group_by(jk) %>% 
  summarise_if(is.numeric, sum) %>% 
  adorn_totals("row")

prop.test(x = data[1, 2:5] , n = data[3, 2:5], conf.level = 0.95)



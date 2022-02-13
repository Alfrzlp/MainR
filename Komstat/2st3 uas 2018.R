# %>% penggunaan

head(df)
df %>% head()

rep(1, each = 5)
1 %>% rep(each = 5)

rep(2, each = 10)
10 %>% rep(2, each = .)


str <- "No Hair Eye Sex Count N_UTS N_UAS N_TUGAS
1 Black Brown Male 32 61.19 67.06 82.6
2 Brown Brown Male 53 61.51 58.08 79.94
3 Red Brown Male 10 67.58 64.44 89.91
4 Blond Brown Male 3 64.51 72.12 87.39
5 Black Blue Male 11 68.62 63.61 83.89
6 Brown Blue Male 50 68.22 62.05 86.71
7 Red Blue Male 10 62.47 57.89 77.51
8 Blond Blue Male 30 60.76 65.91 88.09
9 Black Hazel Male 10 62.35 65.39 85.54
10 Brown Hazel Male 25 66.25 65.97 83.95
11 Red Hazel Male 7 63.05 58.76 81.75
12 Blond Hazel Male 5 63.04 59.88 82.37
13 Black Green Male 3 60.56 66.52 85.23
14 Brown Green Male 15 66.25 67.41 81.93
15 Red Green Male 7 70.04 64.07 84.7
16 Blond Green Female 8 67.45 71.68 82.71
17 Black Brown Female 36 65.95 64.66 88.14
18 Brown Brown Female 66 64.84 77.45 87.23
19 Red Brown Female 16 71.48 62.22 84.74
20 Blond Brown Female 4 64.61 60.18 87.73
21 Black Blue Female 9 67.65 67.71 80.85
22 Brown Blue Female 34 60.4 50.68 81.69
23 Red Blue Female 7 61.90 68.36 80.71
24 Blond Blue Female 64 62.71 75.09 82.65
25 Black Hazel Female 5 64.55 59.42 81.24
26 Brown Hazel Female 29 71.80 71.53 83.01
27 Red Hazel Female 7 63.23 64.03 88.61
28 Blond Hazel Female 5 63.23 65.71 85.96
29 Black Green Female 2 67.42 73.36 84.77
30 Brown Green Female 14 65.48 66.61 87.38
31 Red Green Female 7 64.27 66.53 86.76
32 Blond Green Female 8 64.62 56.01 86.95
"

df <- read.table(textConnection(str), header = T)
df

library(dplyr)
df <- df %>%
  mutate(n_total = 0.3 * N_UTS + 0.2 * N_TUGAS + 0.5 * N_UAS)
df

# no 1A-----------------------------
# jenis kelamin, warna rambut, dan warna mata
# dengan iteraksi pakai *
# tanpa interaksi pakai +

anova <- aov(n_total ~ Sex * Hair * Eye, data = df)
summary(anova)


# No 2 ------------------------
# Sementara peneliti lain berhipotesis bahwa
# proporsi mahasiswa berambut pirang dan bermata
# biru berbeda antara laki-laki dan perempuan. Lakukan uji yang sesuai untuk menjawab
# pertanyaan tersebut.

# p1 = mahasiswa lk rambut pirang mata biru
# p2 = mahasiswa perempuan rambut pirang mata biru

# proporsi mahasiswa rambut pirang mata biru
df %>%
  group_by(Sex) %>%
  filter(Hair == "Blond", Eye == "Blue") %>%
  summarise(x = sum(Count))

# Total Mahasiswa berdasarkan jenis kelamin
df %>%
  group_by(Sex) %>%
  summarise(n = sum(Count))

prop.test(x = c(64, 30), n = c(321, 271))




# No 3 -----------------------------
# Dapatkan penduga Bootstrap dan Jackknife dari
# rata-rata Nilai Total mahasiswa berdasarkan jenis
# kelaminnya


# nilai total dari laki-laki
laki <- df %>%
  filter(Sex == "Male") %>%
  select(n_total) %>%
  pull() # pull untuk jadikan vektor
laki

# nilai total dari perempuan
wanita <- df %>%
  filter(Sex == "Female") %>%
  select(n_total) %>%
  pull() # pull untuk jadikan vektor
wanita

# prinsipnya
boot <- c()
boot
boot <- c(boot, 3)
boot


# bootsrtap
# ambil sampel sebanyak n dari niali tsb scr wr

# laki ---------------------------
boot <- c()
for (i in 1:1000) {
  x <- sample(laki, replace = T)
  boot <- c(boot, mean(x))
}
boot
# Hasilnya laki-laki
mean(boot)


# perempuan --------------------
boot <- c()
for (i in 1:1000) {
  x <- sample(wanita, replace = T)
  boot <- c(boot, mean(x))
}
boot
# Hasilnya perempuan
mean(boot)



# jackknife-------------------------

# hitung rata-rata n_total tanpa unsur ke i,
# dimana i dari 1 sampai n
# jumlah semua rata2 lalu dibagi n

jack <- c()
for (i in 1:length(laki)) {
  x <- mean(laki[-i])
  jack <- c(jack, x)
}
jack

# hasilnya
sum(jack) / length(laki)

# pakai library
library(bootstrap)
jackknife(laki, mean)
jackknife(wanita, mean)

# jack se
var <- sum((laki - sum(jack) / length(laki))^2) / (length(jack) - 1)
sqrt(var) / sqrt(length(laki))

sd(laki) / sqrt(length(jack) - 1)
sd(laki)
# jack bias
# rata2 n total laki - nilai estimasi dengan jackknife


# No 4 -----------------------------------
# p1 = proporsi tidak selamat penumpang
# p2 = proporsi tidak selamat awak
# tapi masih gatau bener apa nggak
titanic <- data.frame(Titanic)
titanic

# total penumpang
titanic %>%
  group_by(Class) %>%
  dplyr::summarise(n = sum(Freq))

# tidak survive
titanic %>%
  group_by(Class) %>%
  filter(Survived == "No") %>%
  dplyr::summarise(n = sum(Freq))

# tidak survive
c(122 + 167 + 528, 673)

# total
c(325 + 285 + 706, 885)

prop.test(x = c(122 + 167 + 528, 673), n = c(325 + 285 + 706, 885))



# No 5-------------------------------------
library(vcd)
library(vcdExtra)
# masih bingung pakai data Titanic atau Titanicp
titanic <- data.frame(Titanic)
titanic

Titanicp
dim(Titanicp)

# buat kolom berat badan dari distribusi normal
# dibulat kan ke 2 digit
Titanicp <- Titanicp %>%
  mutate(
    bb = rnorm(n = 1309, mean = 34, sd = 5),
    bb = round(bb, 2)
  )

Titanicp

# perbedaan rata-rata berat badan penumpang termasuk
# awak kapal jika dilihat dari kelas,
# usia, dan jenis kelamin.

anova5 <- aov(bb ~ pclass * age * sex, data = Titanicp)
summary(anova5)


anova <- aov(bb ~ pclass, Titanicp)
summary(anova)

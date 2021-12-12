library(tidyverse)

ChickWeight %>% 
  mutate(kategori = case_when(weight >= 150~"berat",
                              weight >70 & weight <150~"sedang",
                              weight <= 70~"ringan")) %>% 
  group_by(kategori) %>% 
  count()


# mean, standar deviasi, IQR, Q1, Q2, Q3, dan jumlah observasi
mean(chickwts$weight)
sd(chickwts$weight)
quantile(chickwts$weight, 0.75)
length(chickwts$weight)



# panjang sepal
# jika rata-rata panjang sepal dari spesies tersebut merupakan nilai tertinggi
# dari rata-rata panjang sepal spesies lainnya 
library(janitor)
iris = iris %>% clean_names()

iris %>% group_by(species) %>% 
  summarise(mean = mean(sepal_length))
# terpilih virginica
iris %>% filter(species == "virginica") %>% 
  dplyr::select(sepal_length) %>% pull() %>% 
  boxplot()


iris %>% group_by(species) %>% 
  summarise(mean = mean(petal_width))
# terpilih versicolor
iris %>% filter(species == "versicolor") %>% 
  dplyr::select(petal_width) %>% pull() %>% 
  boxplot()


str = "Provinsi Persentase Provinsi Persentase
Aceh 57.41 NTB 56.35
Sumatera_Utara 64.65 NTT 32.05
Sumatera_Barat 51.42 KalimantanBarat 53.52
Riau 62.94 KalimantanTengah 47.90
Jambi 54.78 KalimantanSelatan 46.73
SumateraSelatan 52.24 KalimantanTimur 65.55
Bengkulu 41.73 KalimantanUtara 60.76
Lampung 52.00 SulawesiUtara 64.61
Kep.BangkaBelitung 26.16 SulawesiTengah 56.65
Kep.Riau 44.09 SulawesiSelatan 60.93
DKIJakarta 34.25 SulawesiTenggara 59.82
JawaBarat 49.29 Gorontalo 62.26
JawaTengah 64.69 SulawesiBarat 47.23
DIYogyakarta 81.61 Maluku 51.75
JawaTimur 65.61 MalukuUtara 59.03
Banten 56.92 PapuaBarat 52.22
Bali 77.24 Papua 26.19"

df = read.table(textConnection(str), header = T)
df = df %>% dplyr::select(Provinsi,  Persentase) %>% 
  filter(Provinsi != "Bali")
df
# H0 : rata2 presentase >= 60
# H1 : rata2 presentase < 60
t.test(df$Persentase, alternative = "less", mu = 60)



str = 
"Merk pH
A 6.7
B 7
C 6.6
D 7.5
E 8
F 6.8
G 7.2
H 6.8
"
df2 = read.table(textConnection(str), header = T)
df2
# Dengan hipotesis alternatif pH lebih besar dari 7 
# dan tingkat signifikansi 1% dan 3%
t.test(df2$pH, alternative = "g", mu = 7)
# pvalue > alpha maka gagal tolak Ho


str = "MURID Sebelum Setelah
1 48 64
2 63 50
3 82 92
4 59 78
5 78 84
6 68 67
7 65 70
8 74 70
9 45 50
10 99 110
"
df3 = read.table(textConnection(str), header = T)
df3
t.test(df3$Setelah, df3$Sebelum, paired = T, conf.level = 0.9)



x = c(613, 151, 81, 13199 ,9049, 3145, 209, 30, 35,790, 892, 339)
shapiro.test(x)


str = "No Sebelum Sesudah
1 11.6 10.9
2 7.9 11.5
3 10.1 12.7
4 12.4 10.3
5 9.8 14.9
6 11.3 12.2
7 12.5 11.0
8 11.0 10.8
9 7.9 13.7
10 10.6 14.9
11 12.7 12.0
12 10.4 10.7
13 14 7.8
14 10.3 9.0
15 11.6 10.1
16 13.9 12.0
17 11.0 8.9
18 15.4 9.8
19 6.9 9.9
20 7.1 10.4"
df4 = read.table(textConnection(str), header = T)
df4
t.test(df4$Sesudah, df4$Sebelum, paired = F)


str = "Mahasiswai Waktu Mahasiswai Waktu Mahasiswai Waktu
1 2 11 0.2 21 1.5
2 3 12 2.3 22 0.5
3 0.3 13 1.5 23 2.5
4 3.3 14 4 24 5
5 1.3 15 5.9 25 1
6 0.4 16 1.8 26 6
7 0.2 17 4.7 27 5.6
8 6 18 0.7 28 6
9 5.5 19 4.5 29 1.2
10 6.5 20 0.3 30 0.2"
df5 = read.table(textConnection(str), header = T)
df5
waktu = c(df5$Waktu, df5$Waktu.1, df5$Waktu.2)
chisq.test(waktu)


prop.test(c(150, 162), c(250, 300))



str = "A 56 78 93 88 89 68 75 71 69 59
B 114 132 120 95 135 98 127 130 115 104
C 80 76 90 102 72 75 81 70 85 71
"
df6 = read.table(textConnection(str))
df6
df6 = df6 %>% tidyr::pivot_longer(2:11)
df6
anova = aov(value~V1, df6)
library(agricolae)
duncan = duncan.test(anova, "V1")
duncan


str = "HalmaheraBarat 1531.06 1447.05 1375.07 1310.01 1246.03
HalmaheraTengah 1522.08 1428.05 1335.02 1262.00 1134.04
HalmaheraSelatan 4860.02 4338.08 3758.06 3238.06 3064.02
HalmaheraUtara 3770.05 3659.02 3571.09 3350.02 3220.05
HalmaheraTimur 2301.08 2179.08 2018.02 1883.02 1784.09"
df7 = read.table(textConnection(str))
df7 = df7 %>% pivot_longer(2:6)
df7
anova2 = aov(value~V1, df7)
summary(anova2)
TukeyHSD(anova2)


str = "SAMPEL Metode1 Metode2 Metode3 Metode4
Sampel1 4 8 7 6
Sampel2 6 12 3 5
Sampel3 4 NA NA 5"
df8 = read.table(textConnection(str), header = T)
df8
df8 = df8 %>% pivot_longer(2:5) %>% 
  drop_na()
df8
anova3 = aov(value~name, df8)
summary(anova3)

titanic = as.data.frame(Titanic)
titanic


# jumlah semua penumpang
sum(titanic$Freq)

titanic %>% filter(Class == "1st", Survived == "No") %>% 
  summarise(sum(Freq))
122/2201

titanic %>% filter(Class == "Crew") %>% 
  summarise(sum(Freq))
885/2201
prop.test(885, 2201, p = 0.4)



titanic %>% filter(Sex == "Female") %>% 
  summarise(sum(Freq))
titanic %>% filter(Survived == "No", Sex == "Female") %>% 
  summarise(sum(Freq))
126/470


titanic %>% filter(Sex == "Male") %>% 
  summarise(sum(Freq))
titanic %>% filter(Survived == "No", Sex == "Male") %>% 
  summarise(sum(Freq))
1364/1731


titanic %>% filter(Survived == "No") %>% 
  summarise(sum(Freq))
titanic %>% filter(Survived == "No", Class == "Crew") %>% 
  summarise(sum(Freq))
673/1490


titanic %>% filter(Survived == "Yes") %>% 
  summarise(sum(Freq))
titanic %>% group_by(Class) %>% 
  filter(Survived == "Yes") %>% 
  summarise(sum(Freq)/771)

titanic %>% filter(Survived == "Yes") %>% 
  summarise(sum(Freq))
titanic %>% filter(Survived == "Yes", Sex == "Male", Class == "3rd")
75/2201

titanic %>% filter(Survived == "Yes") %>% 
  summarise(sum(Freq))
titanic %>% filter(Survived == "Yes", Class == "1st") %>% 
  summarise(sum(Freq))
prop.test(203, 711, p = 0.25)

# Penumpang anak-anak dengan tiket kelas III 
# proporsi antara yang selamat dan yang
# meninggal adalah sama
titanic %>% filter(Survived == "No") %>% 
  summarise(sum(Freq))
titanic %>% filter(Survived == "No", Age == "Child", Class == "3rd") %>% 
  summarise(sum(Freq))
27/711  # anak hidup
52/1490 # anak mati


# kurang dari 30 tahun 100 100
# 30 tahun atau lebih 40 160
qf(0.05, 2, 9, lower.tail = F)

summary(1:10)

colSums(is.na(storms))
hist(c(1:10, 1:5))

df3
cor(df3$MURID, df3$Sebelum)


# membuat matriks 20x20 diagonalnya 10
diag(10, 20)

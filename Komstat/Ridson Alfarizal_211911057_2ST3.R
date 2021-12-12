pendidikan_Rendah<- c(12,33,18,13,15,2,23,17,9,11,5,7)
Buta_Huruf<-c(16,25,29,4,23,7,3,27,14,14,9,6)
Kemiskinan<-c(26,19,7,30,22,10,8,29,28,12,4,21)

df = data.frame(pendidikan_Rendah, Buta_Huruf, Kemiskinan)
head(df)

#1.
#Asosiasi ketiga variable
cor(df)
#Visualisasi
library(corrplot)
corrplot(cor(df), "shade")

#2.
#Regresi
plot(pendidikan_Rendah, Kemiskinan)
abline(lm(pendidikan_Rendah~Kemiskinan))
lm(pendidikan_Rendah~Kemiskinan)

#Regresi
model_reg1 = lm(pendidikan_Rendah~Kemiskinan, data=df)
summary(model_reg1)

#Homoskedastisitas
library(car)
ncvTest(model_reg1)
library(lmtest)
bptest(model_reg1)

#Normalitas
qqPlot(model_reg1, main="QQ Plot")

#1C
#Membuat variabel baru yaitu Kategori miskin
library(dplyr)
df <- df %>% 
  mutate(Kategori_miskin = if_else(Kemiskinan >= median(Kemiskinan),
                                       "Miskin", "Sangat Miskin"))
head(df)

library(ggplot2)
ggplot(df, aes(x=pendidikan_Rendah, y=Buta_Huruf, col=Kategori_miskin)) +
  geom_point(size=5) + xlab("Presentase Penduduk Pendidikan Rendah") +
  ylab("Presentase Penduduk Buta Huruf")


perempuan <- c(180, 120,	180, 360,	240,
                120, 180,	120, 240, 170,	
                150,	120,	180,	180,	150,	
                200,	150,	180,	150,	180,	
                120,	60,	120,	180,	180,	
                90,	240,	180,	115,	120)	

laki <- c(90,120,	30,	90,	200,
          90,	45,	30,	120,	75,
          150,	120,	60,	240,	300,
          240,	60,	120,	60,	30,
          30,	230,	120,	95,	150,
          0,	200,	120,	120,	180)

data = data.frame(c(perempuan, laki), rep(c("wanita", "laki"), each=30))

colnames(data) <- c("waktu_belajar", "jenis_kelamin")
head(data)


#1. peluang Mahasiswa laki-laki yang waktu belajarnya lebih dari median data
data %>% filter(jenis_kelamin == "laki",
                         waktu_belajar > median(waktu_belajar)) %>% nrow()/60

#2. peluang Mahasiswa perempuan yang waktu belajarnya <= 120 menit
data %>% filter(jenis_kelamin == "wanita", waktu_belajar <= 120) %>% nrow()/60

#================
#penjelasan %>%
data %>% nrow()
#sama dengan
nrow(data)
#================

#3B
#Histogram
ggplot(data) + 
  geom_histogram(aes(waktu_belajar, fill=jenis_kelamin)) + 
  facet_grid(.~jenis_kelamin) +
  ggtitle("Lamanya Belajar Mahasiswa/i Universitas A") +
  labs(fill="Jenis Kelamin" + ylab("Banyaknya Mahasiswa/i") 

#boxplot
ggplot(data) + 
  geom_boxplot(aes(y=waktu_belajar, fill=jenis_kelamin)) +
  ylab("Lamanya belajar (Menit)") + 
  ggtitle("Lamanya Belajar Mahasiswa Universitas A") +
  theme(axis.text.x = element_blank())

#grafik kurang 1

#ini jawaban revisi
#jawaban aslinya ngawur banget





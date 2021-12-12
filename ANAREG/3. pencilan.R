#Analisis Residual - Mendeteksi Pencilan (Outlier) dan Amatan Berpengaruh (Influential Case)

#Load library yg diperlukan
library(readxl)
library(olsrr)

#Persiapkan data
data1 <- read_xlsx("pencilan.xlsx")
View(data1)

#Pembentukan model regresi
model1 <- lm(Y ~ ., data = data1)
summary(model1)

#Identifikasi
ols_plot_resid_stud(model1)

ols_plot_resid_lev(model1)

ols_plot_dfbetas(model1)

ols_plot_dffits(model1)

ols_plot_cooksd_chart(model1)

#Catatan
#1. Untuk Nilai Mahalanobis, tidak terdaftar dalam library olsrr, happy searching :)
#2. Untuk mendapatkan nilai dari setiap kriteria, tambahkan print_plot = FALSE
#Sebagai contoh:
ols_plot_cooksd_chart(model1, print_plot = FALSE) #berlaku untuk keempat syntax yg lain
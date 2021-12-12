#Metode Seleksi Model Terbaik

#Load library yg diperlukan
library(readxl)
library(olsrr)                          #package model regresi


#Persiapkan data
data1 <- read_xlsx("seleksi model.xlsx")
View(data1)

#Pembentukan model regresi
model1 <- lm(y ~ ., data = df)
summary(model1)

#All Possible Regression (Best Subset)
ols_step_best_subset(model1)
View(ols_step_best_subset(model1))      #Tampilan yang lebih ramah di mata

#Forward Selection
ols_step_forward_p(model1)              #Tabel yang ditampilkan adalah variabel yang DIMASUKKAN dalam model

#Backward Selection
ols_step_backward_p(model1)             #Tabel yang ditampilkan adalah variabel yang DIBUANG dalam model

#Stepwise Regression
ols_step_both_p(model1)                 #Tabel yang ditampilkan adalah variabel yang DIMASUKKAN/DIBUANG (sesuai keterangan) dalam model

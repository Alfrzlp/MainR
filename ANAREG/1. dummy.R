#Regresi dengan Variabel Bebas Kategorik

#Load library yg diperlukan
library(readxl)                   #library read data excel
library(dplyr)                    #library data manipulation

#Persiapkan data
data1 <- read_xlsx("dummy.xlsx")  #read data
View(data1)                       #tampilkan data

#Recode dummy variable
data1$dummyX2 <- recode(data1$jabatan, "1" = 1, "2" = 0, "3" = 0) #recode dummy X2
data1$dummyX3 <- recode(data1$jabatan, "1" = 0, "2" = 1, "3" = 0) #recode dummy X3
View(data1)

#Pembentukan model regresi
model1 <- lm(gaji ~ usia + dummyX2 + dummyX3, data = data1)       #pembentukan model
summary(model1)                                                   #summary model

#Model dengan interaksi
model2 <- lm(gaji ~ usia + dummyX2 + dummyX3 + usia*dummyX2 + usia*dummyX3 + dummyX2*dummyX3, data = data1)
summary(model2)
#Mendeteksi Heteroskedastisitas - Uji Breusch Pagan

#Load library yg diperlukan
library(readxl)
library(olsrr)

#Persiapkan data
data1 <- read_xlsx("heteroskedastis.xlsx")
View(data1)

#Pembentukan model regresi
model1 <- lm(salary ~ ., data = data1)
summary(model1)

#Plot residual vs fit
ols_plot_resid_fit(model1)

#Uji Breusch Pagan
ols_test_breusch_pagan(model1, rhs = T)

#Untuk Tindakan Perbaikannya, lebih baik gunakan SPSS
#Sekalian semuanya pke SPSS
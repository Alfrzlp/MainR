# Mendeteksi dan Mengatasi Autokorelasi - Uji Durbin Watson + Cochrane Orcutt

# Load library yg diperlukan
library(readxl)
library(orcutt) # library Cochrane Orcutt Procedure
library(lmtest) # library uji regresi

# Persiapkan data
data1 <- read_xlsx("autokorelasi.xlsx")
View(data1)

# Pembentukan model regresi
model1 <- lm(Y ~ X, data = data1)
summary(model1)

# Uji Durbin Watson
dwtest(model1)

# Procedure Cochrane Orcutt
cochrane.orcutt(model1)

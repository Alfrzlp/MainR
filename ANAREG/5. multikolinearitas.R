# Mendeteksi dan Mengatasi Multikolinearitas - VIF + Ridge Regression

# Load library yg diperlukan
library(readxl)
library(olsrr)
library(lmridge) # library ridge regression

# Persiapkan data
data1 <- read_xlsx("multikolinearitas.xlsx")
View(data1)

# Matriks korelasi antarvariabel bebas
cor(data1[, 1:3])

# Pembentukan model regresi
model1 <- lm(Y ~ ., data = data1)
summary(model1)

# VIF
ols_vif_tol(model)

# Ridge regression
rmod <- lmridge(Y ~ ., data = data1, K = seq(0, 1, 0.001)) # model ridge
plot(rmod) # ridge trace plot
vif(rmod) # vif model ridge
rmod # nilai koef ridge
rmod2 <- lmridge(Y ~ ., data = data1, K = 0.063) # model ridge dengan c terpilih
summary(rmod2) # summary model ridge

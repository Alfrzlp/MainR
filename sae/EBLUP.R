library(sae)
library(tidyverse)


# Data --------------------------------------------------------------------
dat <- foreign::read.spss(
  "D:/_bacaan/SAE/Tahap 1/5. Data Latihan/Data_Indikator_SDGS_latihan_SAE_kabkota.sav",
  to.data.frame = T
)
glimpse(dat)



# Direct Estimation -------------------------------------------------------
y9 <- dat$Y9
summary(y9)
hist(y9)
shapiro.test(y9)
var_y9 <- dat$VarY9

#Auxiliary variable
x1 <- dat$jml_sma
x2 <- dat$jml_pt


#Fitting model
hasil_y9 = mseFH(y9 ~ x1 + x2, vardir = var_y9)

#ekstrak eblup
y9_eblup <- hasil_y9$est$eblup

#ekstrak model
model_9 <- hasil_y9$est$fit$estcoef

#gamma
s_u <- hasil_y9$est$fit$refvar

gama <- s_u/(s_u + var_y9)
summary(gama)
plot(gama)

#MSE
RMSE_direct <- sqrt(var_y9)
RMSE_eblup <- sqrt(hasil_y9$mse)

summary(RMSE_direct)
summary(RMSE_eblup)

#Plot y_direct vs y_eblup
plot(y9, main="APK Kab/kota",type="l")
lines(y9_eblup,type="l",col=2)

plot(y9[1:100], main="APK Kab/kota",type="l")
lines(y9_eblup[1:100],type="l",col=2)

#Plot RMSE direct vs RMSE eblup
plot(RMSE_direct[1:100], main="RMSE APK Kab/kota",type="l")
lines(RMSE_eblup[1:100],type="l",col=2)

plot(RMSE_direct[400:500], main="RMSE APK Kab/kota",type="l")
lines(RMSE_eblup[400:500],type="l",col=2)

#RSE direct 
RSE_direct=RMSE_direct/y9
RSE_eblup=RMSE_eblup/y9_eblup

summary(RSE_direct)
summary(RSE_eblup)

plot(RSE_direct[1:100], main="RSE APK Kab/kota",type="l")
lines(RSE_eblup[1:100],type="l",col=2)

plot(RSE_direct[400:500], main="RSE APK Kab/kota",type="l")
lines(RSE_eblup[400:500],type="l",col=2)






# STEPWISE REGRESSION -----------------------------------------------------
y9=data$Y9

x1=data$jml_sma
x2=data$jml_pt
x3=data$Pdesa_sklh
x4=data$Pdesa_sktm
x5=data$Pdesa_angkum
x6=data$Pdesa_listrik
x7=data$Rfas_kes
x8=data$Rfas_sklh
x9=data$Cfas_swalayan
x10=data$Rfas_bank

#model Regresi

reg=lm(y9~x1+x2+x3+x4+x5+x6+x7+x8+x10)
summary(reg)

#eliminasi variabel
reg1=step(reg)
summary(reg1)

reg=lm(y9~x1+x2+x3+x4+x5+x10)
summary(reg)

#EBLUP

hasil_y9_1=mseFH(y9~x1+x2+x3+x4+x5+x10,vardir = var_y9)

#ekstraksi y_eblup
y_eblup_1=hasil_y9_1$est$eblup

#Model eblup
model_y9_1=hasil_y9_1$est$fit$estcoef

#Gama
s_u_1=hasil_y9_1$est$fit$refvar

gama=s_u/(s_u+var_y9)
gama1=s_u_1/(s_u_1+var_y9)

summary(gama)
summary(gama1)

#RMSE
RMSE_eblup_1=sqrt(hasil_y9_1$mse)

summary(RMSE_eblup)
summary(RMSE_eblup_1)

Ef=RMSE_eblup_1/RMSE_direct*100
summary(Ef)


#Kasus NA

data1=na.omit(data)
dim(data)
dim(data1)

#Kasus seleksi varian=0
library(dplyr)
data1=data %>% filter(VarY1!=0)
dim(data)
dim(data1)

summary(data1$VarY1)



set.seed(1)
sample(1:150, 5)

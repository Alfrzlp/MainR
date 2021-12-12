berat_katak <- c(10, 20, 23, 32, 35)
panjang_katak <- c(5, 7, 10, 12, 15)

model_reg = lm(berat_katak~panjang_katak)
summary(model_reg)

#Homoskedastisitas
library(car)
ncvTest(model_reg)
library(lmtest)
bptest(model_reg)

#Normalitas
qqPlot(model_reg, main="QQ Plot")

panjang_cakar = c(20.9, 58.3, 35.5)
ukuran = c(4.5, 6.3, 5.5)
populasi = c("England", " Scotland", "Wales")

naga = data.frame(panjang_cakar, ukuran, populasi)

#grafik bar
library(ggplot2)
ggplot(naga, aes(x=populasi, y=panjang_cakar, fill=populasi)) +
  geom_col()


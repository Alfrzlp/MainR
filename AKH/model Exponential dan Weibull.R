library(dplyr)
library(survival)


# Kebanyakan model parametrik adalah model AFT sehingga tidak memerlukan asumsi 
# proporsional hazard. Exponensial dan Weibull, merupakan model parametrik yang 
# juga memenuhi asumsi AFT sekaligus asumsi PH


# Data --------------------------------------------------------------------
leuk <- read.csv('D:/__SEMESTER 6/AKH/leuk.csv', stringsAsFactors = T)
glimpse(leuk)

levels(leuk$treat)
# "_0treatment" "_1placebo" 

# R secara default akan menjadikan kat reference 
# adalah level yang terakhir

leuk <- leuk %>% 
  mutate(treat = fct_rev(treat))

levels(leuk$treat)
# "_1placebo"   "_0treatment"




# Model Exponential -------------------------------------------------------
# model exponential merupakan model weibull dengan p = 1

hasil_exp <- survreg(Surv(waktu, status) ~ treat=='_0treatment', data = leuk, dist = 'exp')
hasil_exp <- survreg(Surv(waktu, status) ~ treat, data = leuk, dist = 'exp')
summary(hasil_exp)
# jika kat referensi dibalik maka koef X
# -value, std tetap, -z, p tetap
# sedangakn koef intercept beda semua


# output di atas merupakan koefisien regresi dari model AFT
# jika ingin model PH maka beta1 = p * a1
-1 * res_exp$estimate


# Uji Goodness of Fit
# uji kesesuaian model menggunkan uji chi-square menunjukkan 
# bahwa model tersebut signifikan dengan nilai chi-square
# 16.49 (p-value = 0.00)


res_exp <- broom::tidy(hasil_exp)
res_exp %>% 
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )






# estimasi faktor accelerated ---------------------------------------------
fa <- exp(res_exp$estimate[-1])
fa
# selang kepercayaan accelerated faktor
exp(c(
  res_exp$estimate[-1] - 1.96 * res_exp$std.error[-1],
  res_exp$estimate[-1] + 1.96 * res_exp$std.error[-1]
))



# bukti jika nilai rasio waktu survive antar 2 group adalah sama
# dievaluasi pada kuantil S(t) berapapun
tibble(
  # S(t) = q
  q = seq(0.25, 0.75, by = 0.25)
) %>% 
  mutate(
    t_treat0 = -log(q) * exp(res_exp$estimate[1] + res_exp$estimate[2] * 0),
    t_treat1 = -log(q) * exp(res_exp$estimate[1] + res_exp$estimate[2] * 1),
    gamma = t_treat1 / t_treat0
  )

# Dari tabel di atas, jika kita evaluasi pada nilai q berapapun,
# menghasilkan nilai faktor accelerated yang sama yaitu 4.62. 
# Ini berarti bahwa variabel TRT memenuhi asumsi AFT  

# treatment efektif menunda waktu kekambuhan leukimia dengan membentangkan 
# waktu survive menjadi 4.62 kali lebih panjang dari mereka yang tidak 
# mendapatkan treatment





# estimasi HR -------------------------------------------------------------
1 / fa
exp(-res_exp$estimate[-1])

# selang kepercayaan 
exp(c(
  -res_exp$estimate[-1] - 1.96 * res_exp$std.error[-1],
  -res_exp$estimate[-1] + 1.96 * res_exp$std.error[-1]
))

# Dari estimasi interval HR yang signifikan di bawah 1, kita dapat
# menyimpulkan bahwa hazard pada treatment group lebih rendah dibanding 
# hazard pada kontrol group, artinya treatment yang dilakukan signifikan 
# menurunkan resiko kematian pasien. 

# _0treatment / _1treatment




# jika gamma > 1 maka HR < 1 artinya treatment memberi efek positif
# jika gamma < 1 maka HR > 1 artinya treatment tidak bermakna



# Model Weibull -----------------------------------------------------------
# dua sifat penting model weibull
# 1. Jika berlaku asumsi PH maka berlaku asumsi AFT dan sebaliknya
# 2. ln(-ln(S(t))) merupakan fungsi linier dari ln(t) 

# hasil plot ln(-ln(S(t))) dengan ln(t) 
# - Garis lurus berarti Weibull model cocok digunakan
# - Garis paralel berarti asumsi PH (yang berarti juga) asumsi AFT dipenuhi.

hasil_wb <- survreg(Surv(waktu, status) ~ treat=='_0treatment', data = leuk, dist = 'wei')
summary(hasil_wb)

# nilai p = Scale = 0.732
p <- hasil_wb$scale


res_wb <- broom::tidy(hasil_wb)

# estimasi faktor accelerated ---------------------------------------------
fa <- exp(res_wb$estimate[2])
fa

# selang kepercayaan accelerated faktor
exp(c(
  res_wb$estimate[2] - 1.96 * res_wb$std.error[2],
  res_wb$estimate[2] + 1.96 * res_wb$std.error[2]
))


# Dengan demikian dapat disimpulkan bahwa treatment mampu menaikkan survival 
# time 3.55 kali lebih lama di banding control group. Perhatikan bahwa ketika kita 
# gunakan model eksponensial, didapat gamma = 4.62,
# namun exponensial didapat dari asumsi bahwa p=1, padahal 
# nilai p yang sebenarnya adalah 0.732 sehingga kita lebih 
# percaya dengan hasil analisis menggunakan model Weibull. 



# Estimasi HR -------------------------------------------------------------
# HR = exp(beta1) = exp(-p*a1)
exp(-p * res_wb$estimate[2])

# maka hazard dari treatment group adalah 0.4 kali 
# lebih kecil dibanding group kontrol

exp(c(
  -p * res_wb$estimate[2] - 1.96 * res_wb$std.error[2],
  -p * res_wb$estimate[2] + 1.96 * res_wb$std.error[2]
))




# Perbandingan Model ------------------------------------------------------
# H0 : model exponensial lebih baik
# H1 : model weibull lebih baik
anova(hasil_exp, hasil_wb)


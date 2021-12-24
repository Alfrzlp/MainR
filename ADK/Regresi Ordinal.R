mental_impairment <- rep(c('well', 'Mild', 'Moderate', 'Impaired'), c(12, 12, 7, 9))
ses <- c(1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0)
life_events <- c(1, 9, 4, 3, 2, 0, 1, 3, 3, 7, 1, 2, 5, 6, 3, 1, 8, 2, 5, 5, 9, 3, 3, 1, 0, 4, 3, 9, 6, 4, 3, 8, 2, 7, 5, 4, 4, 8, 8, 9) 

df_mi <- 
  data.frame(mi = mental_impairment, ses, life_events) %>% 
  mutate(
    mi = factor(mi, levels = rev(unique(mi))),
    ses = factor(ses, levels = c(1, 0))
  )

df_mi$mi


# Model ------------------------------------------------------------------
library(MASS)

m <- polr(mi ~ ses + life_events, df_mi, Hess = T, model = T, method="logistic")
summary(m)

# Misal a b c dimana c (kat referensi) maka ada 2 model
# ln[ P(Y <= a)/P(Y > a) ] 
# ln[ P(Y <= b)/P(Y > b) ] 


# library(ordinal)
# clm(mi ~ ses + life_events, data = df_mi, link = "logit", threshold = 'flexible')

# koefisien ---------------------------------------------------------------
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = round(p, 3)))

# odds ratio and ci
(ci <- confint(m))
# confint.default(m) # CIs assuming normality
exp(cbind(odds_ratio = coef(m), ci))



# Uji serentak ------------------------------------------------------------
lmtest::lrtest(m)
anova(m, polr(kat_stress ~ 1, df_hasil, Hess = T))



# uji Goodnes of fit ------------------------------------------------------
# Ho : Model cocok dengan data
library(generalhoslem)

lipsitz.test(m)
logitgof(df_mi$mi, fitted(m), ord = TRUE)

chisq.test(m$model$mi, predict(m))

# Pseudo R2 ---------------------------------------------------------------
# Tidak bisa menilai keakuratan model. 
# hanya untuk perbandingan saja biasanya
DescTools::PseudoR2(m, which = c('CoxSnell', 'Nagelkerke', 'McFadden')) %>% 
  round(4)



# Test of Parallel Lines --------------------------------------------------
# untuk proporional odds model, untuk menguji apakah
# koefisien slope sama untuk setiap kategori variabel respon

# Ho : Garis regresi pararel (Model menghasilkan koefisien regresi (slope) yang sama)
brant::brant(m) 

# Gagal tolak Ho semua :  
# semua variabel memenuhi asumsi proportional odds

# Omnibus Gagal tolak : artinya semua variabel memenuhi asumsi proportional odds
# jika gagal tolak maka minimal terdapat 1 variabel yang tidak memnuhi



# Interpretasi ------------------------------------------------------------

# Contoh ln[ P(Y <= 1)/P(Y > 1) ] 

# 1 = sangat puas, 2 = puas, 3 = tidak puas
# jenis pekerjaan (1 = pelajar/mahasiswa, 2 = pengusaha/wiraswasta, 3 =
#                    pegawai/karyawan, 4 = tidak bekerja).

# Pelanggan yang bekerja sebagai pelajar/mahasiswa memiliki kecenderungan sebesar 
# exp(???1,091) = 0,3359 kali dibandingkan pelanggan yang tidak bekerja untuk tingkat
# kepuasan yang sangat puas (dibandingkan dengan minimum puas)


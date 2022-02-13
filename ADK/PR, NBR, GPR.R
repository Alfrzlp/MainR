dataku <-
  readxl::read_xlsx("D:/__SEMESTER 5/Metode Penelitian/data.xlsx") %>%
  # Ganti nama kolom lebih simpel
  `colnames<-`(c("kab", "jk", "tk", "rb", "kp")) %>%
  type_convert() %>%
  drop_na()

head(dataku)

# Multikolinearitas -------------------------------------------------------
car::vif(lm(jk ~ tk + rb + kp, data = dataku))



# Poisson Regression ------------------------------------------------------
# Poisson Model
pm <- glm(jk ~ tk + rb + kp, data = dataku, family = "poisson")
summary(pm)

lmtest::lrtest(pm)
anova(glm(jk ~ 1, data = dataku, family = "poisson"), pm, test = "LRT")


# Cek equidispersi --------------------------------------------------------
# jika deviance(pm)/df.residual(pm) > 1 maka overdispersi
deviance(pm) / df.residual(pm)

# Alternatif uji lain
# H1 : rasio dispersi > 1 (mengalami overdispersi)
AER::dispersiontest(pm)


# Negative Binomial Regression --------------------------------------------
library(MASS)

nbm <- glm.nb(jk ~ rb + kp + tk, data = dataku)
summary(nbm)

# Ho : Poisson Lebih baik
# H1 : Binomial Negatif Lebih baik
lrtest(pm, nbm)


# Generalized Poisson Regression ------------------------------------------
library(VGAM)

gpm <- vglm(jk ~ tk + rb + kp, genpoisson2, data = dataku, trace = TRUE, model = T)
summary(gpm)

pchisq(-2 * -225.5378, 61, lower.tail = F)

# Uji simultan ------------------------------------------------------------
library(lmtest)

# lmtest::lrtest Artinya fungsi lrtest dari package lmtest
lmtest::lrtest(pm)
lmtest::lrtest(nbm)

lrtest_vglm(gpm)


# Perbandingan Antar Model ------------------------------------------------
data.frame(
  model = c("Regresi Poisson", "Negative Binomial", "Generalized Poisson Regression"),
  aic = c(AIC(pm), AIC(nbm), AIC(gpm)),
  bic = c(BIC(pm), BIC(nbm), BIC(gpm))
)


# Interpretasi ------------------------------------------------------------
# exp(Bj) -----------------------------------------------------------------
# saat Xj berubah satu satuan maka nilai frekuensi harapan akan
# berubah sebesar exp(Bj) dg asumsi variabel bebas yg lain konstan



# [exp(Bj) - 1]*100% ------------------------------------------------------
# perubahan (%) dari frekuensi harapan utk setiap kenaikan 1 unit
# variabel Xj adalah [exp(Bj) - 1]*100%

# Jika var dummy maka dia rasio frekuensi harapan kategori tertentu
# terhadap kategori referensi.

# Frekuensi Harapan laki2 untuk banyaknya kursus matematika
# sebelumnya adalah lebih tinggi [exp(Bj) - 1]*100% dari pada wanita

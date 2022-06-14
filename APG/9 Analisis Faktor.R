
# analisis faktor untuk mendapatkan kejelasan dari 
# variabel laten. Variabel laten nantinya bisa digunakan
# untuk analisis lainnya
# 
# variabel laten adalah variabel yang tidak bisa langsung diukur dari 
# lapangan (contoh: ketaatan, kebahagiaan)

# Data --------------------------------------------------------------------
data(Investment, package = "sandwich")
dat <- as.data.frame(Investment[,1:6])
dat


# Uji Asumsi --------------------------------------------------------------
# Normalitas
MVN::mvn(dat, mvnTest = 'hz')

# Uji Bartlett
uji_Bartlett <- function(x) {
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x))
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1 - n + (2 * p + 5) / 6) * log(det(cor(x)))
  df <- p * (p - 1) / 2
  p.value <- pchisq(chisq, df, lower.tail = FALSE)
  names(chisq) <- "Khi-squared"
  names(df) <- "df"
  return(structure(list(
    statistic = chisq, parameter = df, p.value = p.value,
    method = method, data.name = data.name
  ), class = "htest"))
}

uji_Bartlett(dat)
# menguji apakah ada korelasi yang cukup signifikan 
# diantara variabel yang diamati

# Uji Kaiser-Meyer-Oklin
psych::KMO(dat)
# Total Nilai KMO = 0.72 artinya data cukup untuk analisis faktor
# nilai MSA mendakti 1 artinya variabel tsb semakin bisa diperdiksi oleh 
# variabel lain dengan kesalahan kecil
# Nilai MSA kurang dari 0.5 maka variabel harus keluar namun bertahap



# Factor Analysis ---------------------------------------------------------
R <- cov(dat)

# menentukan n factor
eig <- eigen(R)
ap <- nFactors::parallel(subject = 20, var = 6, rep = 100, cent = 0.05)
nfactor <- nFactors::nScree(eig$values, ap$eigen$qevpea)
nFactors::plotnScree(nfactor)
# dari hasil diatas, aturan kaiser jumlah faktor ditentukan
# berdasarkan  nilai eigen >1 . maka n = 1 komponen



# -------------------------------------------------------------------------
investment_fac <- factanal(dat, factors = 5, covmat = R)
investment_fac

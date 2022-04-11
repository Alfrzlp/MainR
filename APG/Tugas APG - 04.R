library(tidyverse)
library(readxl) 

# Data --------------------------------------------------------------------
imunisasi <- read_xlsx('D:/RKD2018_imunisasi.xlsx', sheet = 1)
head(imunisasi)


# anades ------------------------------------------------------------------
summary(imunisasi)

library(GGally)
ggpairs(imunisasi, columns = 2:6)


# Matrix (n x p) ----------------------------------------------------------
X <- as.matrix(imunisasi[, -1])
X

c2 <- mahalanobis(X, center = colMeans(X), cov = cov(X))

# cara 3 
imunisasi$c2 <- c2
imunisasi %>% 
  ggplot(aes(sample = c2)) +
  stat_qq() +
  stat_qq_line(col = 'steelblue') +
  labs(
    title = 'Chi-Square Plot',
    y = expression(paste("Mahalanobis Distance ( ", c^{2}, ")")),
    x = 'Chi-square Quantile'
  ) +
  theme_bw()



# p = 5
nilai_chisq <- qchisq(0.5, 5, lower.tail = F)
nilai_chisq

# Proporsi c2 yang lebih dari nilai chisquare tabel
table(c2 > nilai_chisq) / nrow(X)
# karena proporsi yang lebih dari nilai chi-square tabel 44.12 % < 50%
# maka gagal tolak Ho. data berdistribusi normal multivariat




# Menggunakan Uji Royston -------------------------------------------------

# sebelum transformasi
library(MVN)
mvn(
  X, 
  mvnTest = 'royston',
  univariateTest = 'SW',
  multivariatePlot = 'qq'
)


# transformasi ------------------------------------------------------------
library(MASS)

# mencari lambda terbaik untuk variabel BCG dan campak
bc <- boxcox(lm(imunisasi$BCG ~ 1))
(lambda <- bc$x[which.max(bc$y)])

bc <- boxcox(lm(imunisasi$campak ~ 1))
(lambda <- bc$x[which.max(bc$y)])



# transformasi kuadrat untuk BCG dan Campak
# sudah normal
mvn(
  X, 
  transform = 'square',
  mvnTest = 'royston',
  univariateTest = 'SW',
  multivariatePlot = 'qq'
)


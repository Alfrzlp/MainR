library(readxl) 

# Data --------------------------------------------------------------------
imunisasi <- read_excel('D:/RKD2018_imunisasi.xlsx')
head(imunisasi)


# Matrix (n x p) ----------------------------------------------------------
A <- unname(as.matrix(imunisasi[, -1]))
A

# unname = menghapus nama kolom
# imunisai[, -1] membuang kolom prov


# vektor rata-rata --------------------------------------------------------
colMeans(A)


# matriks ragam-peragam ---------------------------------------------------
# yang berada pada diagonal merupakan varians,
# dan elemen matriks yang lain merupakan covarians
var(A)


# matriks korelasi --------------------------------------------------------
cor(A)

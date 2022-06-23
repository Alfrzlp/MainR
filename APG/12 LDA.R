library(MASS)
pak::pak('kernlab')


# Data --------------------------------------------------------------------
iris <- iris %>%
  dplyr::filter(Species != "versicolor") %>%
  mutate(Species = factor(Species))

head(iris)


# Uji Asumsi --------------------------------------------------------------
# Uji Normalitas masing2 kelompok
MVN::mvn(iris[, -5])

# Uji kesamaan varians
# ragam tidak sama pakai alat lain
# Ho : Matriks Covariance sama
heplots::boxM(iris[, -5], iris$Species)

# Uji perbedaan vector rata2
rrcov::Wilks.test(iris[, -5], grouping = factor(iris$Species))
# signifikan -> terdapat perbedaan (yang dicari)
# bisa pakai manova


# Model -------------------------------------------------------------------
m1 <- lda(Species ~ ., data = iris)
m1

# Fungsi Diskriminan
print_eq(m1$scaling)


# confussion matrix
pred <- predict(m1, iris)$class
cm <- table(actual = iris$Species, prediction = pred)
cm

# akurasi
sum(diag(cm)) / sum(cm)

# Nilai Apparent Error Rate (APER)
1 - sum(diag(cm)) / sum(cm)

# Model diskriminan yang terbentuk ini mempunyai ketepatan pengklasifikasian
# sebesar 100%. Sehingga model diatas dapat digunakan untuk
# mengklasifikasikan jenis bunga iris





# Pendakatan Fisher -------------------------------------------------------
iris <- sample_n(iris, 100, replace = F)

sentosa <- iris %>% 
  dplyr::filter(Species == 'setosa') %>% 
  dplyr::select(-Species)

virginica <- iris %>% 
  dplyr::filter(Species == 'virginica') %>% 
  dplyr::select(-Species)

n1 <- nrow(sentosa)
n2 <- nrow(virginica)

xbar1 <- matrix(colMeans(sentosa))
xbar2 <- matrix(colMeans(virginica))
S1 <- var(sentosa)
S2 <- var(virginica)

spool <- ((n1 - 1) * S1 + (n2 - 1) * S2) / (n1 + n2 - 2)
spool

a <- solve(spool) %*% (xbar1 - xbar2)
a
print_eq(a)


# normalisasi
a / sqrt(t(a) %*% a)[1]

# mid point (leih dari 3 kat tidak bisa pakai mid point)
h <- as.numeric(t(a) %*% (xbar1 + xbar2) / 2)
h

pred <- as.matrix(iris[, -5]) %*% a
pred <- ifelse(as.vector(pred) > h, 'setosa', 'virginica')
pred

# confussion matrix
table(actual = iris$Species, prediction = pred)











# -------------------------------------------------------------------------
print_eq <- function(mat, comp = 1:ncol(mat)) {
  for (i in comp) {
    cat(stringr::str_glue('y{i} = {paste0(round(mat[,i], 3), "*X", 1:length(mat[,i]), collapse = " + ")}'), "\n")
  }
}

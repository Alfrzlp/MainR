# 3.10, 3.14, 3.19, 3.6
library(MASS)

X <- matrix(c(9, 1, 5, 3, 1, 2), nrow = 3, byrow = T)
X <- matrix(c(3, 4, 6, -2, 3, 1), nrow = 3, byrow = T)
X

library(ggrepel)
par(mar = c(0, 0, 0, 0))

data.frame(X) %>%
  ggplot() +
  geom_point(
    aes(x = X1, y = X2),
    inherit.aes = F
  ) +
  geom_point(
    aes(x = mean(X1), y = mean(X2)),
    inherit.aes = F,
    col = "red", shape = 1, size = 3.5
  ) +
  geom_text_repel(
    aes(x = mean(X1), y = mean(X2)),
    label = "mean"
  ) +
  theme_minimal()

library(plotly)
plot_ly(
  x = X[1, ], y = X[2, ], z = X[3, ],
  type = "scatter3d",
  mode = "lines+markers"
)




d1 <- X[, 1] - mean(X[, 1])
d2 <- X[, 2] - mean(X[, 2])
d1
d2
len(d1)
len(d2)

# Matrix sample covariance
angle(d1, d2)

n <- length(d1)
# Sn
Sn <- matrix(c(
  len(d1)^2 / n, sum(d1 * d2) / n,
  sum(d1 * d2) / n, len(d2)^2 / n
), 2, byrow = T)
Sn %>% fractions()
# Matrix korelasi (R)

R <- `diag<-`(Sn / (sqrt(Sn[1, 1] * Sn[2, 2])), 1)
R
R %>% fractions()






# 3.3 ---------------------------------------------------------------------
y1 <- c(1, 4, 4)
x1 <- 3 * c(1, 1, 1)

y1 - x1
x1 + (y1 - x1)


# 3.5 ---------------------------------------------------------------------
# generalized sample variance |S|
(X <- matrix(c(9, 1, 5, 3, 1, 2), nrow = 3, byrow = T))
cov(X)
det(cov(X))

(X <- matrix(c(3, 4, 6, -2, 3, 1), nrow = 3, byrow = T))
cov(X)
det(cov(X))


X - matrix(colMeans(X), nrow = nrow(X), ncol = ncol(X), byrow = T)
colMeans(X)


# 3.6 ---------------------------------------------------------------------
(X <- matrix(c(-1, 3, -2, 2, 4, 2, 5, 2, 3), nrow = 3, byrow = T))

# a. matrix deviasi (residual), X-1xbar apakah full rank
colMeans(X)
d <- X - matrix(colMeans(X), nrow = nrow(X), ncol = ncol(X), byrow = T)
d

# full rank max 3 untuk matrix 3x3
# jika ada kombinasi 1 maka rank = 2
# tidak ada kombinasi maka full rank


# b. Hitung S dan generalized sample variance |S|
cov(X)
det(cov(X))
# 0


# secara geometry volume yang terbentuk dari vector1 deviasinya
# adalah 0. d3 merupakan kombinasi liniear dari d1 dan d2. sehingga
# apabila diilustrasikan d3 berada pada bidang datar d1 dan d2
# sehingga tidak ada tinggi hanya ada alas maka volume 0


library(plotly)
d %>% 
  t() %>% 
  rbind(
    matrix(0, ncol = 3, nrow = 3)
  ) %>% 
  `colnames<-`(c('x', 'y', 'z')) %>% 
  as.data.frame() %>% 
  mutate(
    group = rep(c('d1', 'd2', 'd3'), 2)
  ) %>% 
  plot_ly(
    x = ~x, y = ~y, z = ~z
  ) %>% 
  add_lines(color = ~group)



plot_ly() %>% 
  add_surface(
    x = d[1, ], y = d[2, ], z = ~ d[3, ]
  )

# c. Total sample variance
# Sigma Sii
sum(diag(cov(X)))


# 3.7 ---------------------------------------------------------------------
S1 <- matrix(c(5, 4, 4, 5), 2)
S2 <- matrix(c(5, -4, -4, 5), 2)
S3 <- diag(3, nrow = 2)



# 3.8 ---------------------------------------------------------------------
S1 <- diag(1, nrow = 3)
S2 <- `diag<-`(matrix(-1 / 2, nrow = 3, ncol = 3), 1)

# a
sum(diag(S1))
sum(diag(S2))

# b. generalized sample variance
det(S1)
det(S2)


# 3.9 ---------------------------------------------------------------------
X <- matrix(c(
  12, 17, 29,
  18, 20, 38,
  14, 16, 30,
  20, 18, 38,
  16, 19, 35
),
nrow = 5,
byrow = T
)
X

# a. mean corrected matrix (deviation matrix)
Xc <- deviation_matrix(X)

# c1 + c2 = c3
# nilai a' = [1, 1, -1] menggambarkan Xa = 0
# maka matrix X linear dependent
a <- c(1, 1, -1)

# b. Sample covariance matrix S
S <- cov(Xc)
det(S) # 0 maka volume 0

S %*% matrix(a)

# c
all.equal(X[, 1] + X[, 2], X[, 3])



# 3.10 --------------------------------------------------------------------
X <- matrix(c(
  3, 1, 0,
  6, 4, 6,
  4, 2, 2,
  7, 0, 3,
  5, 3, 4
),
nrow = 5,
byrow = T
)
X

(Xc <- deviation_matrix(X))
# c1 + c2 = c3
# maka Xc linear dependent
a <- matrix(c(1, 1, -1), ncol = 1)
Xc %*% a


# b. Sample covariance matrix S
cov(Xc)
det(cov(Xc))

# c.
X
R(X)
# banyak baris/kolom yang bebas linier
# full rank = semua baris / kolom bebas linear

library(matlib)
showEqn(X, rep(0, nrow(X)))
hasil <- Solve(X, rep(0, nrow(X)), fractions = TRUE)
hasil
# karena x1 = x2 = x3 = 0, maka X linearly independent



# 3.11 --------------------------------------------------------------------
S <- matrix(c(252.04, -68.43, -68.43, 123.67), 2)
S


V12 <- diag(sqrt(diag(S)))
V12
# R (atau rho, matriks korelasi)
# R = (V^1/2)^-1 sigma (V^1/2)^-1
(R <- solve(V12) %*% S %*% solve(V12))

# check
V12 %*% R %*% V12


# 3.12 --------------------------------------------------------------------

# S = (V^1/2) R (V^1/2)
# |S| = |(V^1/2) R (V^1/2)|
# |S| = |(V^1/2)| |R| |(V^1/2)|
# det matrix diagonal = kali semua elemen diagonal
# Maka |(V^1/2)| = sqrt(S11)*sqrt(S22) ... sqrt(Spp)
# |S| = |(V^1/2)| |R| |(V^1/2)|
# |S| = (S11*S22 ... Spp)|R|


# 3.13 --------------------------------------------------------------------

# S = d'd/(n-1)
# (n-1)S = d'd
# dimana d matrix deviasi
x - xbar / Sqrt(Sii)


# 3.14 --------------------------------------------------------------------
X <- matrix(c(9, 1, 5, 3, 1, 2), nrow = 3, byrow = T)
# b'X = 2x1 + 3x2
# c'X = -x1 + 2x2
b <- matrix(c(2, 3))
c <- matrix(c(-1, 2))

# a --- (first principle)

X[, 1] * 2 + X[, 2] * 3
# sample mean
mean(X[, 1] * 2 + X[, 2] * 3)
# sample varians
var(X[, 1] * 2 + X[, 2] * 3)


-X[, 1] + X[, 2] * 2
# sample mean
mean(-X[, 1] + X[, 2] * 2)
# sample varians
var(-X[, 1] + X[, 2] * 2)

# sample covariance of b'X and c'X
cov(
  X[, 1] * 2 + X[, 2] * 3,
  -X[, 1] + X[, 2] * 2
)
# (21-16)*(-7+1) + (19-16)*(1+1) ... /(2)


# b --
# sample mean dan varians
(xbar <- matrix(colMeans(X)))
(S <- cov(X))

# sample mean dari b'X dan c'X
t(b) %*% xbar
t(c) %*% xbar
# sample variance
t(b) %*% S %*% b
t(c) %*% S %*% c
# sample covariance
t(c) %*% S %*% b
t(b) %*% S %*% c


# 3.15 --------------------------------------------------------------------
X <- matrix(c(1, 6, 8, 4, 2, 3, 3, 6, 3), 3)
X

# b'X = x1 + x2 + x3
# c'X = x1 + 2x2 - 3x2
b <- matrix(1, nrow = 3)
c <- matrix(c(1, 2, -3))



# a --- (first principle)

X[, 1] + X[, 2] + X[, 3]
# sample mean
mean(X[, 1] + X[, 2] + X[, 3])
# sample varians
var(X[, 1] + X[, 2] + X[, 3])


X[, 1] + 2*X[, 2] - 3*X[, 3]
# sample mean
mean(X[, 1] + 2*X[, 2] - 3*X[, 3])
# sample varians
var(X[, 1] + 2*X[, 2] - 3*X[, 3])

# sample covariance of b'X and c'X
cov(
  X[, 1] + X[, 2] + X[, 3],
  X[, 1] + 2*X[, 2] - 3*X[, 3]
)


# b --
# sample mean dan varians
(xbar <- matrix(colMeans(X)))
(S <- cov(X))

# sample mean dari b'X dan c'X
t(b) %*% xbar
t(c) %*% xbar
# sample variance
t(b) %*% S %*% b
t(c) %*% S %*% c
# sample covariance
t(c) %*% S %*% b
t(b) %*% S %*% c


# 3.16 --------------------------------------------------------------------
# pakai 
# var(V) = E(VV') - (E(V))^2
# Sigma = E(VV') - (miu)^2
# Sigma + miu miu' = E(VV')

# 3.17 --------------------------------------------------------------------


# 3.18 --------------------------------------------------------------------
xbar <- matrix(c(0.766, 0.508, 0.438, 0.161))
S <- matrix(c(0.856, 0.635, 0.173, 0.096,
              0.635, 0.568, 0.127, 0.067, 
              0.173, 0.128, 0.171, 0.039,
              0.096, 0.067, 0.039, 0.043),
            4)
S
# a. sample mean, sample variance of total
# karena total maka isinya x1 + x2 + x3 + x4
a <- matrix(1, nrow = nrow(xbar))
t(a) %*% xbar
t(a) %*% S %*% a


# a. sample mean, sample variance dari x1 - x2
b <- matrix(c(1, -1, 0, 0))
t(b) %*% xbar
t(b) %*% S %*% b

# sample covariance
t(b) %*% S %*% a



# 3.19 --------------------------------------------------------------------
# buktikan |S| = (s11 s22 s33)|R|

S <- S[1:3, 1:3]
V12 <- diag(sqrt(diag(S)))
V12
(R <- solve(V12) %*% S %*% solve(V12))


(shasil <- prod(diag(S)) * det(R))
det(S)

all.equal(hasil, det(S))
# terbukti


# 3.20 --------------------------------------------------------------------
dm::read_img(bahasa = 'eng', to_df = T, quote = F, row.names = F)

s <- '12.5	13.7
14.5	16.5
8	17.4
9	11
19.5	23.6
8	13.2
9	32.1
7	12.3
7	11.8
9	24.4
6.5	18.2
10.5	22
10	32.5
4.5	18.7
7	15.8
8.5	15.6
6.5	12
8	12.8
3.5	26.1
8	14.5
17.5	42.3
10.5	17.5
12	21.8
6	10.4
13	25.6
'

dat <- dm::read_string(s, col_names = c('x1', 'x2'))
head(dat)
X <- unname(as.matrix(dat))
X

# a.
(xbar <- matrix(colMeans(X)))
(S <- var(X))
# sample mean and sample variance dari x1 - x2
a <- matrix(c(-1, 1))
t(a) %*% xbar
t(a) %*% S %*% a


# b.
dat$x2 - dat$x1
mean(dat$x2 - dat$x1)
var(dat$x2 - dat$x1)


# -------------------------------------------------------------------------
styler::style_file("APG/APG - 03.R")

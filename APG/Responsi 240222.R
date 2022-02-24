S <- matrix(c(25, -2, 4, -2, 4, 1, 4, 1, 9), nrow = 3, byrow = T)
S

# Matriks Korelasinya
# R = V^-1/2 * S * V^-1/2
v12 = diag(sqrt(diag(S)))
(R <- solve(v12) %*% S %*% solve(v12))
MASS::fractions(R)



# buktikan bahwa S = sigma lambda ei ei'
lambda <- eigen(S)$values
eigenvector <- eigen(S)$vector
eigenvector <- lapply(1:ncol(eigenvector), function(i) as.matrix(eigenvector[,i], ncol = 1))


hasil <- 0
for (i in 1:length(lambda)) {
  hasil <- hasil + lambda[i] * (eigenvector[[i]] %*% t(eigenvector[[i]]))
}

hasil
S

all.equal(hasil, S)
# terbukti


# buktikan bahwa S^1/2 = sigma sqrt(lambda) ei ei'
hasil2 <- 0
for (i in 1:length(lambda)) {
  hasil2 <- hasil2 + sqrt(lambda[i]) * (eigenvector[[i]] %*% t(eigenvector[[i]]))
}
hasil2
expm::sqrtm(S)

all.equal(hasil2, expm::sqrtm(S))
# terbukti
R <- matrix(c(
  1, 0.4, 0.5, 0.6,
  0.4, 1, 0.3, 0.4,
  0.5, 0.3, 1, 0.2,
  0.6, 0.4, 0.2, 1
), byrow = T, nrow = 4)
R

R11 <- R[1:2, 1:2]
R22 <- R[3:4, 3:4]

R12 <- R[1:2, 3:4]
R21 <- t(R12)

eigR11 <- eigen(R11)
eigR22 <- eigen(R22)

# R ^ -1/2
R11_I_sqrt <- degree_sigma(eigR11, -1 / 2)
R11_I_sqrt
R22_I_sqrt <- degree_sigma(eigR22, -1 / 2)
R22_I_sqrt


M <- R11_I_sqrt %*% R12 %*% solve(R22) %*% R21 %*% R11_I_sqrt
M

rho2 <- eigen(M)$values
rho2
rho <- sqrt(rho2)
rho


eig <- eigen(M)$vector

# U1
a1 <- R11_I_sqrt %*% eig[, 1]
a1
# U1 <- a1[1] * X1_1 +




degree_sigma <- function(eig, degree) {
  hasil <- matrix(0, nrow = nrow(eig$vectors), ncol = ncol(eig$vectors))
  for (i in 1:length(eig$values)) {
    hasil <- hasil + eig$values[i]^degree * eig$vectors[, i] %*% t(eig$vectors[, i])
  }
  hasil
}


eig <- eigR22
1 / sqrt(eig$values[1]) * eig$vectors[, 1] %*% t(eig$vectors[, 1]) + 1 / sqrt(eig$values[2]) * eig$vectors[, 2] %*% t(eig$vectors[, 2])

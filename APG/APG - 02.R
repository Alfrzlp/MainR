# Utils -------------------------------------------------------------------
deg2rad <- function(deg) return((pi * deg) / 180)
rad2deg <- function(rad) return((180 * rad) / pi)
len <- function(x) sqrt(sum(x^2))
angle <- function(x, y, show = T) {
  cos_theta <- sum(x * y) / (len(x) * len(y))
  theta <- rad2deg(acos(cos_theta))
  
  if (show) {
    cat("Cos(theta)  :", cos_theta,"\nTheta       :", theta, "\n")
  }
  return(invisible(theta))
}
proj <- function(x, y) {
  # bayangan vektor x ke vektor y
  # proyeksi  y ke x
  proj_vec <- (sum(x * y) / sum(x * x)) * x
  cat("Panjang Proyeksi  :", len(x) * cos(angle(x, y, F)), "\n")
  cat("Vektor Proyeksi   :[", paste(MASS::fractions(proj_vec)), "]\n")
  return(invisible(proj_vec))
}

# hanya untuk matrix simetris
spectral_decomposition <- function(A){
  lambda <- eigen(A)$values
  x1 <- c(1, -(A[1,1] - lambda[1])/A[1,2])
  x2 <- c(1, -(A[1,1] - lambda[2])/A[1,2])
  
  # normalisasi
  e <- cbind(x1/len(x1), x2/len(x2))
  
  A1 <- (lambda[1] * e[,1] %*% t(e[,1]))
  A2 <- (lambda[2] * e[,2] %*% t(e[,2]))
  
  to_str <- function(vec, digits = 4) paste0('[',paste0(round(vec, digits), collapse = ', '), ']')
  return(list(
    result = data.frame(
      lambda, 
      eigen_vector = c(to_str(x1), to_str(x2)),
      length = c(len(x1), len(x2)),
      eigen_vector_norm = c(to_str(e[,1]), to_str(e[,2]))
    ),
    A1 = A1,
    A2 = A2
  ))
}




# 2.1 ---------------------------------------------------------------------
x <- c(5, 1, 3)
y <- c(-1, 3, 1)

len(x)
len(y)
angle(x, y)
proj(x, y)


# 2.5 ---------------------------------------------------------------------
A <- matrix(c(5, 12, -12, 5)/13, nrow = 2, byrow = T)
A

t(A) %*% A
# jika hasilnya identitas maka orthogonal

# 2.6 ---------------------------------------------------------------------
A <- matrix(c(9, -2, -2, 6), nrow = 2, byrow = T)
A
A == t(A)

# jika simetris dan nilai eigen positif
det(m)
matrixcalc::is.positive.definite(A)


# 2.7 ---------------------------------------------------------------------
A <- matrix(c(9, -2, -2, 6), nrow = 2, byrow = T)

spectral_decomposition(A)
spectral_decomposition(solve(A))


# 2.8 ---------------------------------------------------------------------
(A <- matrix(c(1, 2, 2, -2), nrow = 2, byrow = T))
spectral_decomposition(A)


# 2.9 ---------------------------------------------------------------------
solve(A)
spectral_decomposition(solve(A))



# 2.21 --------------------------------------------------------------------
A <- matrix(c(1, 1, 2, -2, 2, 2), nrow = 3, byrow = T)
t(A) %*% A

eigen(t(A) %*% A)
eigen(A %*% t(A))


# 2.24 --------------------------------------------------------------------
A <- diag(c(4, 9, 1))
A

solve(A)
eigen(A)
eigen(solve(A))


# 2.25 --------------------------------------------------------------------
S <- matrix(c(25, -2, 4, -2, 4, 1, 4, 1, 9), nrow = 3, byrow = T)
S

# V^1/2
V12 <- diag(sqrt(diag(S)))
V12
# R (atau rho, matriks korelasi)
# R = (V^1/2)^-1 sigma (V^1/2)^-1
(R <- solve(V12) %*% A %*% solve(V12))

# check
V12 %*% R %*% V12


# 2.32 --------------------------------------------------------------------
miu_x <- c(2, 4, -1, 3, 0)
S_x <- matrix(c(4, -1, 1/2, -1/2, 0,
                -1, 3, 1, -1, 0,
                1/2, 1, 6, 1, -1, 
                -1/2, -1, 1, 4, 0, 
                0, 0, -1, 0, 2),
                nrow = 5)
A <- matrix(c(1, -1, 1, 1), nrow = 2)
B <- matrix(c(1, 1, 1, 1, 1, -2), nrow = 2)

miu_x
S_x
A
B

a <- 2
S_x[1:a, 1:a]
S_x[(a + 1):nrow(S_x), (a + 1):ncol(S_x)]



# 2.33 --------------------------------------------------------------------



# 2.41 --------------------------------------------------------------------
S_x <- diag(3, nrow = 4)
miu_x <- c(3, 2, -1, 0)
A <- matrix(c(1, -1, 0, 0, 1, 1, -2, 0, 1, 1, 1, -3), nrow = 3, byrow = T)

# E(AX) = A E(X)
A %*% matrix(miu_x, ncol = 1)

# Cov(AX) = A Cov(X) A'
A %*% S_x %*% t(A)

# tidak ada



# 2.42 --------------------------------------------------------------------
S_x <- `diag<-`(matrix(1, nrow = 4, ncol = 4), 3)
miu_x <- c(3, 2, -1, 0)
A <- matrix(c(1, -1, 0, 0, 1, 1, -2, 0, 1, 1, 1, -3), nrow = 3, byrow = T)

# E(AX) = A E(X)
A %*% matrix(miu_x, ncol = 1)

# Cov(AX) = A Cov(X) A'
A %*% S_x %*% t(A)

# tidak ada













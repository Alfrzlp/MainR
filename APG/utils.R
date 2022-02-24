library(tidyverse)
# hal 130

m <- matrix(c(2.2, 0.4, 0.4, 2.8), nrow = 2, byrow = T)
# dimana vector eigen telah dinormalisasi -> x/sqrt(x'x)
library(GGally)
ggpairs(nutrient[, -1],
        title = "Correlation Matrix",
        progress = FALSE)
# Fun -------------------------------------------------------------------------

# Catatan
# The trigonometric functions in R use radians, not degrees
# e.g. 360 degrees is 2pi radians.
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

# bayangan vektor x ke vektor y
proj <- function(x, y) {
  # proyeksi  y ke x
  proj_vec <- (sum(x * y) / sum(x * x)) * x
  cat("Panjang Proyeksi  :", len(x) * cos(angle(x, y, F)), "\n")
  cat("Vektor Proyeksi   :[", paste(MASS::fractions(proj_vec)), "]\n")
  return(invisible(proj_vec))
}

spectral_decomposition <- function(A){
  r <- eigen(A)
  lambda <- r$values
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

# -------------------------------------------------------------------------
x <- c(5, 1, 3)
y <- c(-1, 3, 1)

len(x)
len(y)
angle(x, y)
proj(x, y)



# Check ortogonal ---------------------------------------------------------
m <- matrix(c(5, 12, -12, 5)/13, nrow = 2, byrow = T)
m

t(m) %*% m
# jika hasilnya identitas maka orthogonal


# Check simetris ----------------------------------------------------------
m <- matrix(c(9, -2, -2, 6), nrow = 2, byrow = T)
m
m == t(m)


# Definit positif ---------------------------------------------------------
# jika simetris dan nilai eigen positif
det(m)
matrixcalc::is.positive.definite(m)

# cara lain
# x'Ax jika semua koef x > 0 maka def positif


c(m[1,1], det(m)) > 0
mean(m == t(m)) == 1



# Eigen -------------------------------------------------------------------
(r <- eigen(A))

(x1 <- A - diag(r$values[1], nrow = nrow(A)))
(x2 <- A - diag(r$values[2], nrow = nrow(A)))



(x1 <- c(1, -x1[1,1]/x1[1,2]))
x1/len(x1)
(A %*% matrix(x1)) == (r$values[1] * matrix(x1))


(x2 <- c(1, -x2[1,1]/x2[1,2]))
x2/len(x2)
(A %*% matrix(x2)) == (r$values[2] * matrix(x2))

# normalisasi
e <- cbind(x1/len(x1), x2/len(x2))
e

# Spectral decomposition --------------------------------------------------
# Sigma lamda * e * e'

lambda <- r$values
# e <- r$vectors

(A1 <- (lambda[1] * e[,1] %*% t(e[,1])))
(A2 <- (lambda[2] * e[,2] %*% t(e[,2])))
A1 + A2

all.equal(A, A1 + A2)

sum(A^2)
sum(sum(A1^2), sum(A2^2))



spectral_decomposition(A)

# Invers ------------------------------------------------------------------
solve(m)
eigen(solve(m))

# hubungan
1/lambda
# vektor eigen sama
eigen(m)



# -------------------------------------------------------------------------
S <- matrix(c(25, -2, 4, -2, 4, 1, 4, 1, 9), nrow = 3, byrow = T)
S

# V^1/2
V12 <- diag(sqrt(diag(S)))
# matriks korelasi
# rho = (V^1/2)^-1 sigma (V^1/2)^-1
(P <- solve(V12) %*% S %*% solve(V12))

# check
V12 %*% P %*% V12


# -------------------------------------------------------------------------
miu_x <- c(2, 4, -1, 3, 0)
sigma_x <- matrix(c(4, -1, 1/2, -1/2, 0,
                    -1, 3, 1, -1, 0,
                    1/2, 1, 6, 1, -1, 
                    -1/2, -1, 1, 4, 0, 
                    0, 0, -1, 0, 2),
                  nrow = 5)
A <- matrix(c(1, -1, 1, 1), nrow = 2)
B <- matrix(c(1, 1, 1, 1, 1, -2), nrow = 2)

miu_x
sigma_x
A
B

a <- 2
sigma_x[1:a, 1:a]
sigma_x[(a + 1):nrow(sigma_x), (a + 1):ncol(sigma_x)]



















library(rgl)

dat <- data.frame(rbind(x, y)) %>% 
  `colnames<-`(c('x', 'y', 'z')) %>% 
  rownames_to_column('id')

mycolors <- c('royalblue1', 'darkcyan', 'oldlace')
dat$color <- mycolors[ as.numeric(dat$id) ]
dat
head(data)

# Plot
plot3d( 
  x = dat$x, y = dat$y, z = dat$z, 
  col = 'orange', 
  type = 's', 
  radius = 0.3,
  xlab = "X", ylab = "Y", zlab = "Z", box = F)

library(plot3D)
scatter3D(x = dat$x, y = dat$y, z = dat$z,
          col = 'blue', pch = 19, bty = "b2",
          phi = 0, theta = 90,
          ticktype = "detailed")

par(mar = c(0, 0, 0, 0))

library(tidyverse)

m <- matrix(c(2.2, 0.4, 0.4, 2.8), nrow = 2, byrow = T)
(e <- m %>% eigen())


# dimana vector eigen telah dinormalisasi -> x/sqrt(x'x)
e$vectors

x <- m
diag(x) <- diag(x) - e$values
x <- x[, 1]
x / sqrt(crossprod(x))


m %*% matrix(c(0.5, 1), nrow = 2)
3 * matrix(c(0.5, 1), nrow = 2)




angle <- function(x, y, show = T) {
  cos_theta <- sum(x * y) / (sqrt(len(x)) * sqrt(len(y)))
  theta <- rad2deg(acos(cos_theta))

  if (show) {
    cat("Cos(theta)  :", cos_theta, "\n")
    cat("Theta       :", theta, "\n")
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

# Catatan
# The trigonometric functions in R use radians, not degrees
# e.g. 360 degrees is 2pi radians.

deg2rad <- function(deg) {
  return((pi * deg) / 180)
}

rad2deg <- function(rad) {
  return((180 * rad) / pi)
}

len <- function(x) sum(x^2)


# -------------------------------------------------------------------------
x <- c(5, 1, 3)
y <- c(-1, 3, 1)

len(x)
len(y)
angle(x, y)
proj(x, y)


MASS::fractions(x / 35)

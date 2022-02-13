# inner product
u <- seq(1, 5)
v <- seq(6, 10)
u %*% v

# panjang vektor
sqrt(sum(u * u))

# matriks--------------
a <- matrix(1:9, 3)
b <- matrix(10:18, 3)
c <- matrix(1:6, nrow = 3)

a + 1
a + b
a %*% c

# obe______________________________________________________________________
scale_row <- function(m, r, k) {
  m[r, ] <- m[r, ] * k
  return(m)
}

#---------------------------------------------------------------------
eliminasi_gauss <- function(a, b) {
  replace_row <- function(m, r1, r2, k) {
    m[r1, ] <- m[r1, ] + m[r2, ] * k
    return(m)
  }

  swap_row <- function(m, r1, r2) {
    temp <- m[r2, ]
    m[r2, ] <- m[r1, ]
    m[r1, ] <- temp
    return(m)
  }

  m <- matrix(c(a, b), nrow = nrow(a))
  b <- nrow(m)
  colx <- 1
  i <- 1

  for (j in 1:b) {
    if (m[j, j] == 0 && j + 1 <= b) {
      m <- swap_row(m, j, j + 1)
    } else if (m[j, j] == 0 && j + 1 > b) {
      return(m)
    }
  }

  while (b != 1) {
    for (i in 1:(b - 1)) {
      m <- replace_row(m, i + colx, colx, -m[i + colx, colx] / m[colx, colx])
      print(m)
    }
    b <- b - 1
    colx <- colx + 1
  }
  return(m)
}
#-----------------------------------------------------------------------
m <- matrix(c(1, 2, 3, -1, 1, 1, -1, 2, 0, -1, -1, 3, 3, 1, 2, 1), nrow = 4)
n <- matrix(c(4, 1, -3, 4))

eliminasi_gauss(m, n)

x <- matrix(c(1, 6, 6, 1, -4, 0, -1, 0, 2), nrow = 3)
y <- matrix(c(0, 24, 10))

eliminasi_gauss(x, y)

swap <- function(m) {
  b <- nrow(m)

  for (i in 1:b) {
    if (m[i, i] == 0) {
      pos <- i
      repeat{
        pos <- pos + 1
        if (m[pos, i] != 0 && pos <= b) {
          m <- swap_row(m, i, pos)
        } else if (pos > b) {
          break
        }
      }
    }
  }
  return(m)
}

x <- matrix(c(0, 6, 0, 0, 0, 9, 5, 0, 0, 3, 0, 2, 3, 4, 0, 1), nrow = 4)
swap(x)

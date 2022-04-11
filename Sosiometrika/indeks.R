
x <- matrix(c(
  NA, 2, 1, 0,
  1, NA, 2, 0,
  0, 2, NA, 1,
  1, 2, 0, NA
),
nrow = 4, byrow = T
)
colnames(x) <- letters[1:4]
rownames(x) <- letters[1:4]
x




N <- nrow(x)
Si <- colSums(x, na.rm = T)
Ci <- colSums(x > 0, na.rm = T)
Ri <- colSums(x == -1, na.rm = T)
Mij <- sum(((x > 0) == T) & (t(x > 0) == T)) / 2

data.frame(
  individu = colnames(x),
  # Intensitas Hubungan
  Int = Si / Ci,

  # Indeks Pemilihan
  IP = Ci / (N - 1),
  # Indeks Penolakan
  IK = Ri / (N - 1),
  # Indeks Pemilihan Penolakan
  IPP = (Ci + Ri) / (N - 1)
)



# Indeks Kohensi Kelompok (keeratan hubungan kelompok)
x <- matrix(c(
  0, 1, 0, 0, 1, 0,
  0, 0, 0, 1, 0, 0,
  0, 1, 0, 0, 0, 0,
  1, 1, 0, 0, 1, 0,
  1, 0, 0, 0, 0, 1,
  1, 0, 0, 0, 1, 0
), nrow = 6, byrow = T)
x


socio_index <- function(x, fraction = T, nama = NULL) {
  x <- as.matrix(x)
  N <- nrow(x)
  Si <- colSums(
    ifelse(x > 0, x, 0),
    na.rm = T
  )
  Ci <- colSums(x > 0, na.rm = T)
  Ri <- colSums(ifelse(x < 0, -1, 0), na.rm = T)
  Mij <- sum(((x > 0) == T) & (t(x > 0) == T)) / 2
  ifelse(is.null(nama), nama <- LETTERS[1:N], nama <- nama)
  
  dt <- data.frame(
    individu = nama,
    Si, Ci, Ri,
    # Intensitas Hubungan
    Int = Si / Ci,

    # Indeks Pemilihan
    IP = Ci / (N - 1),
    # Indeks Penolakan
    IK = abs(Ri) / (N - 1),
    # Indeks Pemilihan Penolakan
    IPP = (Ci + Ri) / (N - 1)
  )
  rownames(dt) <- NULL
  
  if (fraction) dt <- mutate_if(dt, is.numeric, ~ as.character(MASS::fractions(.x)))

  # Indeks Kohesi Kelompok
  Ic <- Mij / (N * (N - 1) / 2)
  # Indeks Kepadatan Kelompok
  ID1 <- (sum(x > 0) - Mij) / (N * (N - 1) / 2)
  ID2 <- sum(x > 0) / (N * (N - 1))

  return(
    list(
      dt,
      "Indeks Kohesi Kelompok (Ic)" = Ic,
      "Indeks Kepadatan Kelompok (Type I)" = ID1,
      "Indeks Kepadatan Kelompok (Type II)" = ID2
    )
  )
}

socio_index(x)

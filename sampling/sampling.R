systematic_sampling <- function(data, n, metode = "l", kosong = 0) {
  k <- ceiling(length(data) / n)
  N <- length(data)
  gugus_sample <- data.frame()
  start <- 1

  if (metode == "c") {
    ngs <- length(data)
  } else {
    ngs <- k
  }
  #----------------------------------------------------------------
  # membuat df all posible sample
  for (i in start:ngs) {
    pos <- i
    for (j in 1:n) {
      if ((pos > N) & (metode == "l")) {
        gugus_sample[j, i] <- kosong
      } else if ((pos <= N) & (metode == "l")) {
        gugus_sample[j, i] <- as.numeric(data[(pos)])
      } else if ((pos > N) & (metode == "c")) {
        pos <- pos - N
        gugus_sample[j, i] <- as.numeric(data[(pos)])
      } else {
        gugus_sample[j, i] <- as.numeric(data[(pos)])
      }
      pos <- pos + k
    }
  }
  #----------------------------------------------------------------
  # mendektesi kolom yang ganda, menyimpan indeks kolom tsb ke vector exe
  exe <- c()
  for (i in 2:ncol(gugus_sample)) {
    for (j in 1:(i - 1)) {
      if (setequal(gugus_sample[, i], gugus_sample[, j])) {
        exe <- unique(append(exe, i))
      } else {
        next()
      }
    }
  }
  #----------------------------------------------------------------
  # mencoba menghapus kolom yang ganda, Jika error maka skip
  # bisa juga warning, finnaly
  tryCatch(
    {
      gugus_sample <- gugus_sample[, -exe]
    },
    error = function(a) gugus_sample <- gugus_sample
  )

  #----------------------------------------------------------------
  # perhitungan dari data gugus_sample
  Ybar <- sum(colMeans(gugus_sample)) / k
  varybar <- sum((colMeans(gugus_sample) - Ybar)^2) / k
  S2 <- sum((gugus_sample - Ybar)^2) / (N - 1)

  swsy2 <- 0
  for (i in 1:k) {
    swsy2 <- swsy2 + sum((gugus_sample[, i] - colMeans(gugus_sample)[i])^2)
  }
  swsy2 <- swsy2 / (k * (n - 1))
  p <- varybar * n * N / ((n - 1) * S2 * (N - 1)) - 1 / (n - 1)

  print(paste("Y bar      :", Ybar))
  print(paste("V(ybar sy) :", varybar))
  print(paste("S^2        :", S2))
  print(paste("Swsy^2     :", swsy2))
  print(paste("p          :", p))

  return(gugus_sample)
}
#-------------------------------------------------------------------------------
systematic_sampling(1:96, 12, "l")
d <- c(
  24,
  36,
  48,
  60,
  72,
  84,
  84,
  96,
  108,
  120,
  132,
  144
)
systematic_sampling(d, 4, "l")
#-------------------------------------------------------------------------------
kerangka_sample <- c(1:9)
systematic_sampling(kerangka_sample, 3)

systematic_sampling(1:4, 2, "c")

systematic_sampling(1:5, 2, "l")
systematic_sampling(1:5, 2, "c")
systematic_sampling(1:6, 3, "c")

pop <- rep(1:5, 3)
systematic_sampling(pop, 3)
#--------------------------------------------------------------------------
data <- c(6, 2, 7, 4, 4, 5, 6, 4, 2, 5, 3, 2, 6, 12, 3, 7, 3, 0)
data1 <- c(62.5, 14.49, 73.6, 259.99, 45, 20.3, 46.9, 147.5, 105, 75, 20.35, 56)
data1 <- data.frame(data1)
n <- nrow(data1)
N <- 40

# ========================================
psm <- function(sampel, N, pasangan = 1) {
  n <- length(sampel)
  vybar <- 0
  if (n %% 2 == 0) {
    for (i in 1:(n / 2)) {
      isi <- (sampel[2 * i] - sampel[2 * i - 1])^2
      vybar <- sum(vybar, isi)
    }
    vybar <- vybar * (1 - (n / N)) / n^2
  } else {
    akhir <- ceiling(n / 2)
    sampel[n + 1] <- sampel[pasangan]
    for (i in 1:akhir) {
      isi <- (sampel[2 * i] - sampel[2 * i - 1])^2
      vybar <- sum(vybar, isi)
    }
    vybar <- vybar * (1 - (n / N)) / (n * ((n + 1)))
  }
  print(paste("v(ybar) :", vybar))
}
# =====================================================
sdm <- function(sampel, N) {
  n <- length(sampel)
  vybar <- 0
  for (i in 1:(n - 1)) {
    isi <- (sampel[i + 1] - sampel[i])^2
    vybar <- sum(vybar, isi)
  }
  vybar <- vybar * (1 - (n / N)) / (2 * n * ((n - 1)))
  print(paste("v(ybar) :", vybar))
}



data <- c(
  0, 0.9, 0, 0, 0.3, 0.1, 0.5, 3, 2.8, 2.7, 2.8, 2.6, 2.3, 3.5, 2.4,
  3.8, 4.1, 4.9, 6, 5.4, 2.3, 2.9, 2.1, 6.3, 8.2, 5.4, 6.5, 6.6, 4.1
)
data1 <- c(6, 2, 7, 4, 4, 5, 6, 4, 2, 5, 3, 2, 6, 12, 3, 7, 3, 0)
psm(data, 290, pasangan = 28)
psm(data1, 162)

sdm(data, 290)
sdm(data1, 162)
sdm(c(36, 72, 96, 132), 12)

p <- c(1.5, 4.0, 4.4, 4.4, 4.7, 4.4, 4.8, 5.3, 5.9, 5.7, 6.2, 7.2)
length(p)
mean(p)
sdm(p, 96)

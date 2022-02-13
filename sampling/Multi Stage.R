str <- "3.88 4.53 3.8 3.98 4.44
4.54 3.67 3.83 2.62 4.20
4.90 4.78 4.34 4.28 3.81
4.62 4.50 4.14 3.86 4.44
3.95 4.65 4.57 4.59 4.08
3.75 4.30 3.82 4.51 4.33"

data <- get_allCluster(str)

# multistage cluster sampling
Mo <- 1470
N <- 98
Mi <- 1470 / 98

df <- lapply(data, function(x) {
  data.frame(
    mi = length(x),
    ybari = mean(x),
    swi2 = var(x)
  )
}) %>%
  bind_rows() %>%
  mutate(Mi, .before = mi)
df

n <- nrow(df)
f1 <- n / N
f2 <- df$mi / Mi


# cara 1
ybarn <- mean(df$ybari)
sb2 <- var(df$ybari)
vybarn <- (1 - f1) * sb2 / n + sum((1 - f2) * df$swi2 / df$mi) / (n * N)

# cara 2
Mbar <- mean(Mi)
ybarn <- sum(df$Mi * df$ybari) / sum(df$Mi)
sb2 <- sum(Mi^2 * (df$ybari - ybarn)^2) / (Mbar^2 * (n - 1))
vybarn <- (1 - f1) * sb2 / n + sum(Mi^2 * (1 - f2) * df$swi2 / df$mi) / (n * N * Mbar^2)

# cara 3
Mbar <- Mo / N
x <- Mi * df$ybari / Mbar
ybarn <- mean(x)
sb2 <- var(x)


vybarn <- (1 - f1) * sb2 / n + sum(Mi^2 * (1 - f2) * df$swi2 / df$mi) / (n * N * Mbar^2)

ybarn
sb2
vybarn



# Two stage ================================================================
TwoStage <- function(data, N, Mi, cara = 2, Mo = NULL) {
  df <- lapply(data, function(x) {
    data.frame(
      mi = length(x),
      ybari = mean(x),
      swi2 = var(x)
    )
  }) %>%
    bind_rows() %>%
    mutate(Mi, .before = mi)

  n <- nrow(df)
  f1 <- n / N
  f2 <- df$mi / Mi
  Mbar <- mean(Mi) # deafultnya

  if (cara == 1) {
    ybarn <- mean(df$ybari)
    sb2 <- var(df$ybari)
  } else if (cara == 2) {
    ybarn <- sum(Mi * df$ybari) / sum(df$Mi)
    sb2 <- sum(Mi^2 * (df$ybari - ybarn)^2) / (Mbar^2 * (n - 1))
  } else if (cara == 3) {
    Mbar <- Mo / N
    x <- Mi * df$ybari / Mbar
    ybarn <- mean(x)
    sb2 <- var(x)
  }

  vybarn <- (1 - f1) * sb2 / n + sum(Mi^2 * (1 - f2) * df$swi2 / df$mi) / (n * N * Mbar^2)
  seybarn <- sqrt(vybarn)
  rseybarn <- seybarn * 100 / ybarn

  return(list(
    df,
    rata2 = data.frame(Estimasi = c(ybarn, vybarn, seybarn, rseybarn)) %>%
      `rownames<-`(c("ybar n", "v(ybar n)", "se(ybar n)", "rse(ybar n)")),
    Total = data.frame(Estimasi = c(ybarn * N * Mbar, vybarn * (N * Mbar)^2, sqrt(vybarn) * N * Mbar, rseybarn)) %>%
      `rownames<-`(c("ycap n", "v(ycap n)", "se(ycap n)", "rse(ycap n)")),
    Metode = paste("Two Stage Cluster Sampling cara", cara)
  ))
}

# Stratified Two stage (srs wor srs wor) ===================================
nh <- c(2, 4, 3)
daftar <- nh
phi <- Mhi / rep(Xh, nh)
Nh <- c(40, 60, 50)
Nh <- rep(Nh, nh)
nh <- rep(nh, nh)
Mhi <- df$V1
mhi <- df$V2

Xh <- c(2500, 2500, 3000)
yij <- df$V3

# srs srs
sum(Nh * Mhi * yij / (mhi * nh))

# pps srs
sum(Mhi * yij / (mhi * phi * nh))
x <- Mhi * yij / (mhi * phi * nh)
weight <- Mhi / (mhi * phi * nh)

# total setiap strata
Ycaph <- split(x, rep(1:length(daftar), daftar)) %>%
  sapply(sum)

# 191712,8
Ycaphi <- yij * weight * nh
(Ycaphi - rep(Ycaph, daftar))^2 / (nh * (nh - 1))

split((1 - nh / Nh) * (Ycaphi - rep(Ycaph, daftar))^2 / (nh * (nh - 1)), rep(1:length(daftar), daftar)) %>%
  sapply(sum)

vYcap <- sum(x)
vYcap
sqrt(vYcap) * 100 / Ycap




stratified2 <- function(Nh, nh, Mhi, mhi, yij, phi, metode) {
  Nh <- rep(Nh, nh)
  nh <- rep(nh, nh)

  df <- data.frame(Nh, nh, Mhi, mhi, yij)

  metode <- match.arg(metode, c("srs srs", "pps srs"))
  if (metode == "srs srs") {
    weight <- Nh * Mhi / (mhi * nh)
  } else if (metode == "pps srs") {
    weight <- Mhi / (mhi * phi * nh)
    df$phi <- phi
  }

  ycap <- sum(yij * weight)
  df$weight <- weight
  return(list(
    df, ycap
  ))
}
# contoh ====================================================
str <- "50 10 2
70 15 6
60 12 3
90 20 3
50 8 0
70 12 3
60 10 2
100 16 4
80 8 1
"
df <- read.table(textConnection(str), header = F)
nh <- c(2, 4, 3)
phi <- Mhi / rep(Xh, nh)
Nh <- c(40, 60, 50)
Mhi <- df$V1
mhi <- df$V2
Xh <- c(2500, 2500, 3000)
yij <- df$V3 # sudah total

stratified2(Nh, nh, Mhi, mhi, yij, Mhi / rep(Xh, nh), "pps srs")



# PPS wr-SRS wr ============================================================
n <- length(Xi)
Xi <- c(10, 9, 5, 6, 4, 7)
X <- 127
pi <- Xi / X

Mi <- c(48, 45, 23, 25, 20, 36)
mi <- c(5, 4, 3, 5, 7, 8)
weight <- Mi / (n * pi * mi)
weight

# data semua Ycap
x <- (sapply(data, sum) * weight * n)
# Ycap semua
ycap <- (sapply(data, sum) * weight) %>% sum()
# v(Ycap)
vycap <- var(sapply(data, sum) * weight * n) / n


# Three stage ============================================================
Mi <- c(64, 40, 72, 50, 8)
mi <- c(3, 2, 3, 2, 2)
Mi <- rep(Mi, mi)
mi <- rep(mi, mi)
Lij <- c(30, 32, 36, 34, 40, 50, 40, 60, 30, 36, 40, 50)
lij <- c(6, 5, 4, 3, 5, 5, 4, 6, 3, 2, 6, 5)
N <- 30
n <- 5

str <- "100 90 60 75 150 50
90 80 75 70 100
70 90 60 200
70 100 65
80 60 150 90 70
60 80 100 90 50
120 90 75 70
90 200 80 70 100 50
80 60 70
70 120
90 60 95 100 90 75
70 150 100 60 70"

data <- get_allCluster(str)
data
weight <- N * Mi * Lij / (mi * lij * n)
# Ycap
Ycap <- sum(sapply(data, sum) * weight)
Ycap

f1 <- n / N
f2 <- mi / Mi
f3 <- lij / Lij
suij2 <- sapply(data, var)

sum((sapply(data, sum) * N * Mi * Lij / (mi * lij * n) - Ycap)^2) / (n * (n - 1))

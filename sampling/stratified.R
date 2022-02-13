str <- "1 Desa A 1200 1.5 0.5
2 Desa B 800 1.25 0.4
3 Desa C 600 1.2 0.32
4 Desa D 400 0.8 0.18"

df <- str.to.df(str, dim = c(4, 6))
df <- df[, -1:-2]
colnames(df) <- c("Desa", "Nh", "ybarh", "sh")
df

# =============================================================================================
# input data frame harus memiliki variabel
# Nh ybarh sh (jika optimum harus ada ch)
# Nama juga boleh (dianjurkan)
alokasi.stratified <- function(data1, alokasi, alpha = 0.05, daksen, total_biaya = 0, biaya_tetap = 0, minim_biaya = T) {
  if (class(data1) == "list") {
    data <- data1[[1]]
  } else {
    data <- data1
  }

  data <- data %>%
    mutate(
      "Wh.ybarh" = Nh * ybarh / sum(Nh),
      "Nh.Sh^2" = Nh * `Sh^2`,
      "Nh^2.Sh^2" = Nh^2 * `Sh^2`
    )

  L <- nrow(data)
  z <- qnorm(alpha / 2, lower.tail = F)
  N <- sum(data$Nh)
  ybarst <- sum(data$Wh.ybarh)
  D <- daksen * ybarst / z # 1-ci = d'

  # n dibulatkan keatas
  # nh dibulatkan ke bilangan terdekat

  if (alokasi == "sama") {
    n <- (L * sum(data$`Nh^2.Sh^2`)) / (N^2 * D^2 + sum(data$`Nh.Sh^2`))
    n <- ceiling(n)
    data <- data %>% mutate(nh = n / L)
  } else if (alokasi == "sebanding") {
    n <- (N * sum(data$`Nh.Sh^2`)) / (N^2 * D^2 + sum(data$`Nh.Sh^2`))
    n <- ceiling(n)
    data <- data %>% mutate(nh = Nh * n / N)
  } else if (alokasi == "neyman") {
    data <- data %>% mutate("Nh.Sh" = Nh * sqrt(`Sh^2`), .before = `Nh.Sh^2`)
    n <- (sum(data$`Nh.Sh`)^2) / (N^2 * D^2 + sum(data$`Nh.Sh^2`))
    n <- ceiling(n)
    data <- data %>% mutate(nh = Nh * sqrt(`Sh^2`) * n / sum(data$`Nh.Sh`))
  } else if (alokasi == "optimum") { # ada perubahan rumus
    data <- data %>% mutate(
      "Nh.Sh.sqrt(ch)" = Nh * sqrt(`Sh^2`) * sqrt(ch),
      "Nh.Sh/sqrt(ch)" = Nh * sqrt(`Sh^2`) / sqrt(ch)
    )
    if (minim_biaya == T) { # biaya kecil dengan varians tertentu
      print("Meminimumkan biaya, dengan varians tertentu")
      n <- (sum(data$`Nh.Sh.sqrt(ch)`) * sum(data$`Nh.Sh/sqrt(ch)`)) / (N^2 * D^2 + sum(data$`Nh.Sh^2`))
    } else { # biaya besar tapi varians kecil
      print("Meminimumkan Varians, dengan biaya tertentu")
      n <- (total_biaya - biaya_tetap) * sum(data$`Nh.Sh/sqrt(ch)`) / sum(data$`Nh.Sh.sqrt(ch)`)
    }
    n <- ceiling(n)
    data <- data %>% mutate(nh = `Nh.Sh.sqrt(ch)` * n / sum(data$`Nh.Sh/sqrt(ch)`))
  }
  print(paste("n  :", n))
  data <- data %>%
    select(1:4, nh)

  if (class(data1) == "list") {
    return(list(Parameter = data, "Strata~variabel" = data1[[2]]))
  } else {
    return(data)
  }
}
# =============================================================================================

alokasi.stratified(df, alokasi = "sama", 0.95)
alokasi.stratified(df, alokasi = "sebanding", 0.95) %>%
  mutate(v = (1 - nh / Nh) * sh^2 * (Nh / 3000)^2 / nh) %>%
  summarise(sum(v))
alokasi.stratified(df, alokasi = "neyman", 0.95)

df$ch <- c(40000, 22500, 22500, 62500)
df

sampel.stratified(df, alokasi = "optimum", 0.95)
sampel.stratified(df,
  alokasi = "optimum", 0.95, total_biaya = 20000000,
  biaya_tetap = 4000000, minim_biaya = F
)

# =========================================================================================
strata <- list(
  laki = c(7, 10, 3, 6, 14, 5),
  perempuan = c(14, 18, 21, 10, 16)
)
strata2 <- list(
  laki = c(1, 0, 1, 1, 0, 1),
  perempuan = c(0, 1, 0, 0, 1)
)
Nh <- c(19, 17)


olah <- function(lis, Nh, N, ps = "wor") {
  if (ps == "wor") {
    data$fh <- 1 - (data$nh / data$Nh)
  } else {
    fh <- 0
  }

  data <- data %>% mutate(
    "v(ybarh)" = (1 - fh) * varh / nh,
    "se(ybarh)" = sqrt(`v(ybarh)`),
    "rse(ybarh)" = `se(ybarh)` * 100 / ybarh
  )

  ybarst <- sum(data$ybar * data$Wh)
  seyst <- sum(data$Wh^2 * data$`v(ybarh)`) %>% sqrt()

  print(data)
  print(paste0("ybarst      :", ybarst))
  print(paste0("v(ybarst)   :", sum(data$Wh^2 * data$`v(ybarh)`)))
  print(paste0("se(ybarst)  :", seyst))
  print(paste0("rse(ybarst) :", seyst * 100 / ybarst))
}

# terdapat perbedaan di fh

olah(strata, Nh, 36, "wor")
olah(strata2, Nh, 36, "wor")

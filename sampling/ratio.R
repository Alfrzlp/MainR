str <- "9	5	7	8	9	4	4	6	3	5
80	42	59	66	84	36	33	50	25	41"
df <- read.table(textConnection(str)) %>%
  t() %>%
  as.data.frame() %>%
  remove_rownames() %>%
  rename(x = V1, y = V2)

cor(df)
(150 - 10) * var(df$y) * (1 - 0.992^2) / (150 * 10)


(var(df$y) * 9 / 10)
mean(df$y)
lm(y ~ x, df)


library(tidyverse)


str <-
  c(
    "4,63; 2,90; 1,45; 3,38; \n5,17; 3,35	290,6; 179,5; 92,3; 212,4; 318,7; 221,4",
    "8,66; 6,61; 7,35; 9,87; 6,42; 8,60; 9,70; 6,75	565,1; 436,2; 490,6; 648,1; 450,5; 560,7; 635,5; 446,0",
    "11,60; 15,29; 17,94; 14,29; 18,00; 13,20; 11,50; 14,70; 12,01; 17,96; 23,15	810,4; 927; 1374; 995,8; 1499; 750; 861,3; 847; 879,7; 1420,5; 2115"
  )




str_replace_all(str[1], ";", " ")

a <- data.frame(
  x = c(4.63, 2.9, 1.45, 3.38, 5.17, 3.35),
  y = c(290.6, 179.5, 92.3, 212.4, 318.7, 221.4)
)
b <- data.frame(
  x = c(8.66, 6.61, 7.35, 9.87, 6.42, 8.6, 9.7, 6.75),
  y = c(565.1, 436.2, 490.6, 648.1, 450.5, 560.7, 635.5, 446)
)
c <- data.frame(
  x = c(11.6, 15.29, 17.94, 14.29, 18, 13.2, 11.5, 14.7, 12.01, 17.96, 23.15),
  y = c(810.4, 927, 1374, 995.8, 1499, 750, 861.3, 847, 879.7, 1420.5, 2115)
)



str2vec <- function(str, pemisah, format_koma = T) {
  # jika angka sudah berformat spt 2.1 maka skip
  if (format_koma) str <- gsub(",", ".", str)
  str <- gsub(paste0(pemisah, "|\t|\n"), " ", str)
  str <- str_split(str, pattern = " ")[[1]]
  # ambil yang ada angka dan ubah jadi numeric
  str <- as.numeric(str[str_detect(str, "[:alnum:]")])
  return(str)
}






# untuk stratified ratio
ratioEst <- function(data, Nh, Xh, metode = "separate") {
  df <- sapply(data, function(a) {
    hasil <- c(
      mean(a$x), mean(a$y),
      var(a$x), var(a$y),
      cor(a$x, a$y),
      mean(a$y) / mean(a$x),
      nrow(a)
    )
    names(hasil) <- c("mean_x", "mean_y", "var_x", "var_y", "cor", "Rcap", "nh")
    return(hasil)
  })

  # cek ada kesamaan string atau tidak
  if (grepl(metode, "combined")) {
    Rcapc <- sum(Nh * df["mean_y", ]) / sum(Nh * df["mean_x", ])
    df["Rcap", ] <- Rcapc
    # jika gaada yang mirip sama sekali
  } else if (grepl(metode, "combined") + grepl(metode, "separate") == 0) {
    warning("Metode tidak diketahui. default adalah separate")
  }

  data <- rbind(df, Nh, Xh)
  ycap <- 0
  vycap <- 0

  for (i in 1:ncol(data)) {
    Nh <- data["Nh", i]
    nh <- data["nh", i]
    var_x <- data["var_x", i]
    var_y <- data["var_y", i]
    cor <- data["cor", i]
    Rcap <- data["Rcap", i]

    vycap <- vycap + Nh * (Nh - nh) * (var_y + Rcap^2 * var_x - 2 * Rcap * cor * sqrt(var_x * var_y)) / nh
    ycap <- ycap + Rcap * data["Xh", i]
  }

  estycap <- c(ycap, vycap, sqrt(vycap), sqrt(vycap) * 100 / ycap)
  names(estycap) <- c("ycap", "v(ycap)", "se(ycap)", "rse(ycap)")

  N <- sum(data["Nh", ])
  estybar <- c(ycap / N, vycap / N^2, sqrt(vycap / N^2), sqrt(vycap / N^2) * 100 / (ycap / N))
  names(estybar) <- c("ybar", "v(ybar)", "se(ybar)", "rse(ybar)")

  i <- which.max(c(
    grepl(metode, "separate"),
    grepl(metode, "combined"),
    grepl(metode, "regresi"),
    grepl(metode, "difference")
  ))
  return(list(
    data = data,
    estycap = estycap,
    estybar = estybar,
    metode = c("separate", "combined", "regresi", "difference")[i]
  ))
}


Nh <- c(985, 1196, 1020)
Xh <- c(3253, 9115, 15270)

ratioEst(list(a, b, c), Nh, Xh, metode = "sep")
ratioEst(list(a, b, c), Nh, Xh, metode = "combined")







ratioEst2 <- function(data, Nh, Xh, metode = "separate") {
  df <- sapply(data, function(a) {
    hasil <- c(
      mean(a$x), mean(a$y),
      var(a$x), var(a$y),
      cor(a$x, a$y),
      mean(a$y) / mean(a$x),
      nrow(a)
    )
    names(hasil) <- c("mean_x", "mean_y", "var_x", "var_y", "cor", "Rcap", "nh")
    return(hasil)
  })

  # cek ada kesamaan string atau tidak
  if (grepl(metode, "combined")) {
    Rcapc <- sum(Nh * df["mean_y", ]) / sum(Nh * df["mean_x", ])
    df["Rcap", ] <- Rcapc
    # jika gaada yang mirip sama sekali
  } else if (grepl(metode, "combined") + grepl(metode, "separate") == 0) {
    warning("Metode tidak diketahui. default adalah separate")
  }

  data <- rbind(df, Nh, Xh)
  ycap <- 0
  vycap <- 0

  for (i in 1:ncol(data)) {
    Nh <- data["Nh", i]
    nh <- data["nh", i]
    var_x <- data["var_x", i]
    var_y <- data["var_y", i]
    cor <- data["cor", i]
    Rcap <- 0.6565

    vycap <- vycap + Nh * (Nh - nh) * (var_y + Rcap^2 * var_x - 2 * Rcap * cor * sqrt(var_x * var_y)) / nh
    ycap <- ycap + Rcap * data["Xh", i]
  }

  estycap <- c(ycap, vycap, sqrt(vycap), sqrt(vycap) * 100 / ycap)
  names(estycap) <- c("ycap", "v(ycap)", "se(ycap)", "rse(ycap)")

  N <- sum(data["Nh", ])
  estybar <- c(ycap / N, vycap / N^2, sqrt(vycap / N^2), sqrt(vycap / N^2) * 100 / (ycap / N))
  names(estybar) <- c("ybar", "v(ybar)", "se(ybar)", "rse(ybar)")

  i <- which.max(c(
    grepl(metode, "separate"),
    grepl(metode, "combined"),
    grepl(metode, "regresi"),
    grepl(metode, "difference")
  ))
  return(list(
    data = data,
    estycap = estycap,
    estybar = estybar,
    metode = c("separate", "combined", "regresi", "difference")[i]
  ))
}

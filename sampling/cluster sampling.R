library(tidyverse)
# fungsi untuk menghitung estimasi cluster sampling
# input -> list

str <- "161	179	134	180	152	158	191	169	140	179		
149	182	158	190	143	171	185	170	150	174	165	180
152	180	177	163	148	169	150	194	181	160	149	
129	165	132	153	180	143	179	191				
160	185	153	147	169	139	165	176	170		"

str <- "561	159	364	960	252	658	361	869	250	459	258	160	743	661	361	360	449	965	866	470
749	462	258	160	743	661	585	270	965	374	165	550	232	853	560					
252	760	457	363	648	959	250	464	361	360	449									
129	565	232	853	560	743	259	361	869	250	459	258	160	743	661	661	585	270		
760	385	153	457	569	259	965	866	470	661	585	270	965							"

str <- c(
  "561	159	364	960	252	658	361	869	250	459	258	160	743	
  749	462	258	160	743	661	585	270	965	374	165	550	232	853",

  "252	760	457	363	648	959	250	464	361	360	449			
  129	565	232	853	560	743	259	361	869	250	459	258	160	743
  760	385	153	457	569	259	965	866	470	661	585	270	965	
  252	658	361	869	250	459	258	160	743	661	",

  "743	661	585	270	965	374	165	550	232	853	464	361	360	
  648	959	250	464	361	360	449	160	743	661				
  560	743	259	361	869	250	459	258	160	743	385	153	"
)


str2vec <- function(str, adakoma = T, pemisah = "\\s") {
  str <- gsub(",", ".", str)
  str <- gsub(paste0(pemisah, "|\t|\n"), " ", str)
  str <- strsplit(str, split = " ")[[1]]
  # ambil yang ada angka dan ubah jadi numeric
  vec <- as.numeric(str[str_detect(str, "[:alnum:]")])
  return(vec)
}

get_allCluster <- function(vec_str, ...) {
  all_cluster <- strsplit(vec_str, split = "\n")[[1]]
  all_cluster <- sapply(all_cluster, str2vec, USE.NAMES = F, ...)
  return(all_cluster)
}

get_allCluster(str[2])
get_allCluster(str)



estCS <- function(all_cluster, N, cara = 2, Mbar = NA) {
  df <- sapply(all_cluster, function(a) {
    hasil <- c(mean(a), length(a), var(a))
    names(hasil) <- c("ybar i", "Mi", "s^2")
    return(hasil)
  })

  n <- ncol(df)
  if (cara != 3) Mbar <- mean(df[2, ])

  if (cara == 1) {
    ybarn <- mean(df[1, ])
    sb2 <- var(df[1, ])
  } else if (cara == 2) {
    ybarn <- sum(df[2, ] * df[1, ]) / sum(df[2, ])
    sb2 <- sum(df["Mi", ]^2 * (df["ybar i", ] - ybarn)^2) / (Mbar^2 * (n - 1))
  } else if (cara == 3) {
    ybarn <- mean(df[2, ] * df[1, ] / Mbar)
    sb2 <- var(df[2, ] * df[1, ] / Mbar)
  }
  vybarn <- (N - n) * sb2 / (N * n)
  df1 <- data.frame(
    nilai = c(ybarn, vybarn, sqrt(vybarn), sqrt(vybarn) * 100 / ybarn),
    row.names = c("ybar n", "v(ybar n)", "se(ybar n)", "rse(ybar n)")
  )

  ycapn <- N * Mbar * ybarn
  vycapn <- (N * Mbar)^2 * vybarn
  df2 <- data.frame(
    nilai = c(ycapn, vycapn, sqrt(vycapn), sqrt(vycapn) * 100 / ycapn),
    row.names = c("ycap n", "v(ycap n)", "se(ycap n)", "rse(ycap n)")
  )
  return(list(
    "Perhitungan tiap cluster" = df,
    "sb^2" = sb2, "Mbar" = Mbar,
    "estimasi rata-rata" = df1,
    "estimasi Total" = df2
  ))
}

stratified_cluster <- function(data, Nh, Mo = NA, ...) {
  if (class(data) == "character") {
    cluster <- lapply(data, get_allCluster)
    data <- tibble(strata = 1:length(cluster), cluster, Nh)
  }
  data$Nh <- Nh
  df <- list()


  if (length(Mo) != length(Nh)) {
    for (i in 1:nrow(data)) {
      df[[i]] <- estCS(data$cluster[[i]], N = data$Nh[i], ...)
    }
    data$Mbar <- sapply(df, function(a) a$Mbar)
  } else if (length(Mo) == length(Nh)) {
    data$Mbar <- Mo / Nh # anggap aja Mo/N
    for (i in 1:nrow(data)) {
      df[[i]] <- estCS(data$cluster[[i]],
        N = data$Nh[i], cara = 3, Mbar = data$Mbar[i]
      )
    }
  }

  data$Wh <- data$Nh * data$Mbar / sum(data$Nh * data$Mbar)
  data$sb2 <- sapply(df, function(a) a$`sb^2`)
  data$ybarn <- sapply(df, function(a) a$`estimasi rata-rata`[1, ])
  data$vybarn <- sapply(df, function(a) a$`estimasi rata-rata`[2, ])
  data$perhitungan <- df
  data

  hasil <- data %>%
    as.data.frame() %>%
    select(-cluster) %>%
    summarise(
      ybarst = sum(Wh * ybarn),
      vybarst = sum(Wh^2 * vybarn),
      se = sqrt(vybarst),
      rse = se * 100 / ybarst
    ) %>%
    `colnames<-`(c("ybar st", "v(ybar st)", "se(ybar st)", "rse(ybar st)"))

  hasil2 <- hasil * c(sum(data$Nh * data$Mbar), sum(data$Nh * data$Mbar)^2, sum(data$Nh * data$Mbar), 1) %>%
    `names<-`(c("ycap st", "v(ycap st)", "se(ycap st)", "rse(ycap st)"))

  return(list(
    data,
    "Estimasi Rata-rata" = hasil,
    "Estimsasi Total" = hasil2
  ))
}



estCS(all_cluster, 100, cara = 1, Mbar = 9.8)
estCS(all_cluster, 65)

stratified_cluster(str, Nh = c(40, 60, 50))
stratified_cluster(str, Nh = c(40, 60, 50), cara = 1)













# PPS cluster
str <- "5,6	1,5	3,6	9,6	2,5	6,5	3,6	8,6						
7,4	4,6	2,5	1,6	7,4	6,6	5,8	2,7	9,6	3,7				
2,5	7,6	4,5	3,6	6,4	9,5	2,5	4,6	3,6	3,6	4,4	2,5	1,6	7,4
1,2	5,6	2,3	8,5	5,6	7,4	2,5	3,6	8,6	2,5				
7,6	3,8	1,5	4,5	5,6	2,5	9,6	8,6	4,7					
9,6	2,5	6,5	3,6	3,6	8,6	2   "


estPPS_CS <- function(all_cluster, N, X, xi = NULL, Mo, size = "X") {
  n <- length(all_cluster)
  Mi <- sapply(all_cluster, length)

  if (tolower(size) == "x") {
    pi <- xi / X
    size <- "Xi/X"
  } else if (tolower(size) == "m") {
    pi <- Mi / Mo
    size <- "Mi/Mo"
  } else {
    return(NULL)
  }

  ybari <- sapply(all_cluster, mean)
  ybari

  zbari <- Mi * ybari / (Mo * pi)
  zbarn <- mean(zbari)
  vzbarn <- var(zbari) / n
  # zcapn = zbarn*N*Mbar
  # karena Mbar = Mo/N, maka zcapn = zbarn*Mo
  zcapn <- zbarn * Mo
  vzcapn <- vzbarn * Mo^2

  hasil <- list(
    data <- as.table(cbind(ybari, zbari, Mi)),
    est_Mean = c(zbarn, vzbarn, sqrt(vzbarn), sqrt(vzbarn) * 100 / zbarn) %>%
      `names<-`(c("zbar n", "v(zbar n)", "se(zbar n)", "rse(zbar n)")),
    est_Total = c(zcapn, vzcapn, sqrt(vzcapn), sqrt(vzcapn) * 100 / zcapn) %>%
      `names<-`(c("zcap n", "v(zcap n)", "se(zcap n)", "rse(zcap n)")),
    size = size
  )
  return(hasil)
}


estPPS_CS(get_allCluster(str),
  N = 40, X = 4000,
  xi = c(60, 80, 120, 100, 110, 90), Mo = 360, size = "X"
)


str <- "0,5 1,2 3,1 0,8 4,2 2,5
1,3 0,6 1,8 3,4 3,8 2,3 4,4 1,7
3,6 4,1 0,9 2,6"
get_allCluster(str)

str2 <- "1,1 2,9 3,6 5,2 8,4 4,2 1,9 2,4 3,7 4,6 4,9 6,5
1,3 1,9 2,6 7,4 4,4 5,8 6,3 6,6 5,4 4,9 6,9 8,6 4,8 2,7 2,4 5,2"
get_allCluster(str)

7232
9108

estPPS_CS(get_allCluster(str), N = 64, X = 7232, Mo = 448, size = "M")
estPPS_CS(get_allCluster(str2), N = 46, X = 9108, Mo = 690, size = "M")

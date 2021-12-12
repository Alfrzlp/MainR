#Algoritma untuk mendaptkan sample data scr systematic
sample_systematic <- function(data, n, sort_by=NA, metode="circular"){
  N = nrow(data)
  k = round(N/n, 0)
  
  if(tolower(metode) == "liniear"){
    AR = round(runif(1, 1, k))
    index = c(AR)
    for(i in 1:(n-1)){
      if(AR+k*i <= N) isi = AR+k*i
      else isi = NA
      index = append(index, isi)
    }
    
  }else if(tolower(metode) =="circular"){
    AR = round(runif(1, 1, N))
    index = c(AR)
    for(i in 1:(n-1)) index = append(index, (AR+k*i)%%N )
  }else print("Metode salah")
  
  index = replace(index, index == 0, N)
  
  data %>% 
    #sort_by bukan string tapi nama kolom
    arrange({{sort_by}}) %>% 
    rowid_to_column("id") %>% 
    filter(id %in% index) %>% 
    select(-id) %>% 
    return()
}

#====================================================================
#Algoritma untuk mendaptkan sample data scr pps kumulatif
sample_pps = function(data, n, xi, inc_prob = F, ...){
  variabel = deparse(substitute(xi))
  sampel = sample(1:sum(data[,variabel]), n, ...)
  
  data = data %>% mutate(fk = cumsum({{xi}}))
  data$bawah = c(1, data$fk[-nrow(data)]+1)
  print(data)
  
  sam = data.frame()
  for(i in 1:length(sampel)){
    sam = rbind(sam,
                filter(data, bawah <= sampel[i] & fk >= sampel[i])
    )
  }
  
  
  print(sampel)
  if(inc_prob == T){
    X = data %>% summarise(sum({{xi}})) %>% pull()
    sam$pi = sam[,variabel]/X
    sam = sam %>% mutate(inc_prob = 1-(1-pi)^length(sampel))
    
    return(sam)
  }else return(sam)
}

sample_pps(iris, 5, Sepal.Width)

# algoritma HansenHurwitz untuk pps wr-----------------------------------------
HansenHurwitz = function(yi, pi){
  hasil = TeachingSampling::HH(yi, pi) %>% as.data.frame()
  s2 = var(yi/pi)
  # maka v(Y cap) = s^2/n
  var = s2/length(yi)
  
  hasil = hasil %>% 
    add_row(y = var, .before = 2) %>% 
    add_row(y = s2)
  
  rownames(hasil) <- c("Y cap", "v(Y cap)", "se(Y cap)", "rse(Y cap)", "s^2")
  return(round(hasil, 3))
}

# Algoritma desraj -> pps wor--------------------------------------------------
library(fpest)
# yi , pi(peluang terpilihnya unit ke i) -> Xi/X
# estimasi total, varians, nilai Zi
desraj(c(60, 50), c(50, 44)/270)


# Algoritma HT -> pps wor-----------------------------------------------------
HT2 = function (y, Pik) {
  y <- cbind(1, y)
  y <- as.data.frame(y)
  names(y)[1] <- "N"
  Total <- matrix(NA, nrow = 4, ncol = dim(y)[2])
  rownames(Total) = c("Y cap", "v(Y cap)", 
                      "se(Y cap)", "rse(Y cap)")
  colnames(Total) <- names(y)
  n <- length(Pik)
  for (k in 1:dim(y)[2]) {
    ty <- sum(y[, k]/Pik)
    ck <- (1 - Pik) * (n/(n - 1))
    P1 <- sum(ck * y[, k]/Pik)
    P2 <- sum(ck)
    ystar <- Pik * P1/P2
    P3 <- ck/(Pik^2)
    if (sum(Pik) == n) {
      Vty <- 0
    }
    else {
      Vty <- sum(P3 * ((y[, k] - ystar)^2))
    }
    CVe <- 100 * sqrt(Vty)/ty
    N <- sum(1/Pik)
    VMAS <- (N^2) * (1 - (n/N)) * var(y[, k])/(n)
    DEFF <- Vty/VMAS
    Total[, k] <- c(ty, Vty, sqrt(Vty), CVe)
  }
  Total = round(Total, 3)
  return(Total[,2] %>% as.data.frame())
}
# y   inclusion prob (phi)
HT2(c(60, 50), c(0.336, 0.299))

# stratified ================================================================
#Algoritma untuk mendaptkan sample data scr stratified
sample_stratified = function(data, strata, variabel, n_sampel, metode = "srswor",...){
  nstrata = unique(data[,deparse(substitute(strata))])
  df = data.frame()
  jumlah = n_sampel
  
  for(i in 1:length(nstrata)){
    df = rbind(df, 
               if(metode == "systematic"){
                 data %>% 
                   filter({{strata}} == nstrata[i]) %>% 
                   sample_systematic(jumlah[i], ...) 
               }else if(metode == "srswor"){
                 data %>% 
                   filter({{strata}} == nstrata[i]) %>% 
                   sample_n(jumlah[i])
               }else if(metode == "srswr"){
                 data %>% 
                   filter({{strata}} == nstrata[i]) %>% 
                   sample_n(jumlah[i], replace = T)
               }else return("metode salah")
    )
  }
  
  statistik = df %>% group_by({{strata}}) %>% 
    summarise(nh = n(),
              "ybarh (sampel)" = mean({{variabel}}),
              "sh^2" = var({{variabel}})) %>% 
    as.data.frame()
  
  return(list(data_sampel = df, statistik = statistik))
}

#input data frame harus memiliki variabel-------------------------------------
# Nh ybarh Sh (jika optimum harus ada ch)
# Nama juga boleh (dianjurkan)
alokasi.stratified <- function(data1, alokasi, alpha=0.05, daksen=NA, d=NA, total_biaya=0, biaya_tetap=0, minim_biaya=T){
  if(class(data1) == "list"){
    data = data1[[1]]
  }else data = data1
  
  data = data %>% 
    mutate("Wh.ybarh" = Nh*ybarh/sum(Nh),
           "Nh.Sh^2" = Nh*`Sh^2`,
           "Nh^2.Sh^2" = Nh^2*`Sh^2`)
  
  L = nrow(data)
  z = qnorm(alpha/2, lower.tail = F)
  N = sum(data$Nh)
  ybarst = sum(data$Wh.ybarh) 
  
  if(is.na(d))  D = daksen*ybarst/z           #1-ci = d'
  else if(is.na(daksen)) D = d/z 
  
  cat("D  :", D)
  #n dibulatkan keatas
  #nh dibulatkan ke bilangan terdekat 
  
  if(alokasi == "sama"){
    n = (L*sum(data$`Nh^2.Sh^2`))/(N^2*D^2 + sum(data$`Nh.Sh^2`)) 
    n = ceiling(n)
    data = data %>% mutate(nh = n/L)
  }else if(alokasi == "sebanding"){
    n = (N*sum(data$`Nh.Sh^2`))/(N^2*D^2 + sum(data$`Nh.Sh^2`)) 
    n = ceiling(n)
    data = data %>% mutate(nh = Nh*n/N)
  }else if(alokasi == "neyman"){
    data = data %>% mutate("Nh.Sh" = Nh*sqrt(`Sh^2`), .before = `Nh.Sh^2`)
    n = (sum(data$`Nh.Sh`)^2)/(N^2*D^2 + sum(data$`Nh.Sh^2`)) 
    n = ceiling(n)
    data = data %>% mutate(nh = Nh*sqrt(`Sh^2`)*n/sum(data$`Nh.Sh`))
  }else if(alokasi == "optimum"){ #ada perubahan rumus
    data = data %>% mutate("Nh.Sh.sqrt(ch)" = Nh*sqrt(`Sh^2`)*sqrt(ch),
                           "Nh.Sh/sqrt(ch)" = Nh*sqrt(`Sh^2`)/sqrt(ch))
    if(minim_biaya == T){ #biaya kecil dengan varians tertentu
      print("Meminimumkan biaya, dengan varians tertentu")
      n = (sum(data$`Nh.Sh.sqrt(ch)`)*sum(data$`Nh.Sh/sqrt(ch)`))/(N^2*D^2 + sum(data$`Nh.Sh^2`)) 
    }else{ #biaya besar tapi varians kecil
      print("Meminimumkan Varians, dengan biaya tertentu")
      n = (total_biaya-biaya_tetap)*sum(data$`Nh.Sh/sqrt(ch)`)/sum(data$`Nh.Sh.sqrt(ch)`)
    }
    n = ceiling(n)
    data = data %>% mutate(nh = `Nh.Sh.sqrt(ch)`*n/sum(data$`Nh.Sh/sqrt(ch)`))
  }
  cat("\nn  :",n, "\n")
  # data = data %>% 
  #   select(1:4, nh)
  
  if(class(data1) == "list"){
    return(list(Parameter = data, "Strata~variabel"= data1[[2]]))
  }else return(data)
  
}


# perhitungan estimasi--------------------------------------------------------
# input df, dengan kolom Nh,   nh, sh, ybarh
estimasi_stratified <- function(data, sample = "wor"){
  N = sum(data$Nh)
  if(sample == "wor"){
    data$vybarst = (1-data$nh/data$Nh)*data$sh^2/data$nh
  }else data$vybarst = data$sh^2/data$nh
  
  data$Wh = data$Nh/N
  
  N = sum(data$Nh)
  ybarst = sum(data$Nh*data$ybarh)/N
  vybarst = sum(data$Wh^2*data$vybarst)
  
  cat("ybarst        :", ybarst)
  cat("\nv(ybarst)     :", vybarst)
  cat("\nse(ybarst)    :", sqrt(vybarst))
  cat("\nrse(ybarst)   :", sqrt(vybarst)*100/ybarst)
  cat("\n", ybarst-1.96*sqrt(vybarst))
  cat("\n", ybarst+1.96*sqrt(vybarst))
  
  cat("\n\nycapst        :", N*ybarst)
  cat("\nv(ycapst)     :", N^2*vybarst)
  cat("\nse(ycapst)    :", sqrt(N^2*vybarst))
  cat("\nrse(ycapst)   :", sqrt(N^2*vybarst)*100/(N*ybarst), "\n")
  
  return(data)
}

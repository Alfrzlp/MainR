library(tidyverse)

#membaca data dengan library xlsx
data = readxl::read_xlsx("D:/Datasets/Contoh Data Sensus_1 2020.xlsx")

#melihat struktur data
glimpse(data)

#bebrerapa kesalahan
data %>% 
  count(jenis_kelamin)
data %>% 
  count(pakai_kacamata)

#masih banyak type data yang tidak tepat
#misal tinggi badan - chr ====  harusnya dbl

#nama probvinsi ada besar ada kecil
#sumatra utara 	 -> sumatera utara
#di. yogyakarta	 -> di yogyakarta

#ubah jenis kelamin
#     1 = laki-laki
#     0 = perempuan
#ubah pakai kacamata
#     1 = iya
#     0 = tidak

data = data %>% 
  type_convert() %>% #ubah type data
  janitor::clean_names()  %>% #ubah nama kolom menjadi yg sesuai (*tdk boleh ada spasi)
  mutate(asal_provinsi = tolower(asal_provinsi), #jadikan huruf kecil nama provinsi agar mudah diproses
         asal_provinsi = if_else(asal_provinsi == "sumatra utara", "sumatera utara", asal_provinsi),  
         asal_provinsi = str_remove_all(asal_provinsi, "[:punct:]"), #hapus titik di  di. yogyakarta
         asal_provinsi = str_to_title(asal_provinsi), #ubah ke huruf besar seperti judul, co : Jawa Timur
         jenis_kelamin = if_else(jenis_kelamin=="Laki-laki", "Laki-Laki", jenis_kelamin),
         jenis_kelamin = if_else(jenis_kelamin=="Pria", "Laki-Laki", jenis_kelamin),
         jenis_kelamin = if_else(jenis_kelamin=="Wanita", "Perempuan", jenis_kelamin),
         jenis_kelamin = if_else(jenis_kelamin=="Laki-Laki", 1, 0), #ubah jenis kelamin
         pakai_kacamata = str_remove_all(pakai_kacamata, "[:punct:]"),
         pakai_kacamata = str_to_lower(pakai_kacamata),
         pakai_kacamata = if_else(pakai_kacamata == "ya", "iya", pakai_kacamata),
         pakai_kacamata = if_else(pakai_kacamata == "iya", 1, 0)
         ) %>%  #ubah pakai kacamata
  as.data.frame() #dijadikan data frame 

#Data siap diolah 
View(data)
#====================================================================
varp <- function(x) var(x)*(length(x)-1)/length(x)

library(xlsx)
write.xlsx("D:/tugas mpc.xlsx", sheetName = "parameter", append = T)

write.xlsx(df, "D:/tugas mpc.xlsx", sheetName = "sampel", append = T)

write.xlsx("D:/tugas mpc.xlsx", sheetName = "estimasi2", append = T)

#====================================================================
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
stratified = function(data, strata, variabel){
  varp <- function(x) var(x)*(length(x)-1)/length(x)
  
  data = data %>% group_by({{strata}}) %>% 
    summarise(Nh = n(),
              "ybarh" = mean({{variabel}}),
              "Sh^2" = varp({{variabel}})) %>% 
    as.data.frame()
  #print.data.frame(data)
  
  # variabel = deparse(substitute(variabel))
  # form = as.formula(paste0(colnames(data)[1],"~",variabel))
  return(data)
}
#====================================================================
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
#====================================================================
sample_pps = function(data, n, xi, inc_prob = F, ...){
  sampel = sample(1:sum({{xi}}), n, ...)
  
  data = data %>% mutate(fk = cumsum({{xi}}))
  data$bawah = c(1, data$fk[-nrow(data)]+1)
  
  sam = data.frame()
  for(i in 1:length(sampel)){
    sam = rbind(sam,
                filter(data, bawah <= sampel[i] & fk >= sampel[i])
    )
  }
  print(sampel)
  if(inc_prob == T){
    X = data %>% summarise(sum({{xi}})) %>% pull()
    variabel = deparse(substitute(xi))
    sam$pi = sam[,variabel]/X
    sam = sam %>% mutate(inc_prob = 1-(1-pi)^length(sampel))
    
    return(select(sam, -fk, -bawah))
  }else return(select(sam, -fk, -bawah))
}

E.piPS2 = function (y, Pik) {
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
#====================================================================
options(dplyr.summarise.inform = F)  

stratified(data, kelas, pakai_kacamata) %>% 
  alokasi.stratified("sebanding", daksen = 0.1)


hasil = sample_stratified(
  data, kelas, pakai_kacamata,
  c(27, 27, 26, 27, 26), metode = "srswor")
hasil[[2]]


write.xlsx(hasil[[1]], "D:/tugas mpc.xlsx", sheetName = "sampel", append = T)
write.xlsx(hasil[[2]], "D:/tugas mpc.xlsx", sheetName = "estimasi", append = T)





n_sampel = function(data, d , alpha, type="wor"){
  N = length(data)
  z = qnorm(alpha/2, lower.tail = F)
  #n = (N*z^2*varp(data))/(N*d^2 + z^2*varp(data))
  n0 = z^2*varp(data)/d^2
  
  if(type == "wor") return(n0/(1+n0/N))
  else return(n0)
}

attach(data)
n_sampel(jenis_kelamin, 0.1*mean(jenis_kelamin), 0.05)
n_sampel(berat_badan, 0.1*mean(berat_badan), 0.05)
n_sampel(tinggi_badan, 0.1*mean(tinggi_badan), 0.05)
n_sampel(pakai_kacamata, 0.1*mean(pakai_kacamata), 0.05)
n_sampel(jumlah_anggota_keluarga, 0.1*mean(jumlah_anggota_keluarga), 0.05)

#======================================================================
library(TeachingSampling)

sample_pps(data, 13, berat_badan, inc_prob = T)
# WOR
# HT
x = c(5, 2, 1)
y = c(60 ,14, 1)
pi = x/100
1-(1-pi)^4  # inclusion prob

# y   inclusion prob (phi)
E.piPS2(y, 1-(1-pi)^4)  
E.piPS2(c(60, 50), c(0.336, 0.299))



library(fpest)
# yi , pi(peluang terpilihnya unit ke i) -> Xi/X
# estimasi total, varians, nilai Zi
desraj(c(60, 50), c(50, 44)/270)


#=======================================================================
# WR
luas1 = c(25.1, 21.8, 35.0, 33.4, 22.3, 21.6, 23.5, 18.9, 24.3, 16.2, 20.4, 12.8)
panen1 = c(58.6, 42.7, 84.2, 78.3, 53.6, 40.9, 48.7, 38.5, 56.2, 32.2, 40.5, 25.7)
luas2 = c(3.9, 7.6, 5.6, 10.2, 4.8, 12.1, 6.5, 8.6)
panen2 = c(10.3, 16.8, 14.2, 22.6, 13.1, 25.8, 15.9, 19.2)

# HH
# y  pi = Xi/X                 s^2
HH(panen1, luas1/(0.6*2019));  var(panen1/(luas1/(0.6*2019)))

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

HansenHurwitz(yi = panen1, pi = luas1/(0.6*2019))
HansenHurwitz(yi = panen2, pi = luas2/(0.4*2019))



str = "
1 52 91
2 24 78 
3 36 63 
4 44 88 "

s1 = read.table(textConnection(str))
HansenHurwitz(s1$V3, s1$V2/400)


# systematic
n_sampel(berat_badan, 0.1*mean(berat_badan), 0.05)
sys = sample_systematic(data, 13, sort_by = berat_badan)
sys
mean(sys$berat_badan)
psm(sys$berat_badan, 177)
sdm(sys$berat_badan, 177)





















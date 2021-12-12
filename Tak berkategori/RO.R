# ============================
library(lpSolve)
library(lpSolveAPI)

# Set coefficients of the objective function
f.obj <- c(2, 3)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negative constraint; it is automatically assumed
f.con <- matrix(c(1/2, 1/4,
                  1, 3,
                  1, 1), nrow = 3, byrow = TRUE)

# Set unequality signs
f.dir <- c("<=",
           ">=",
           "=")

# Set right hand side coefficients
f.rhs <- c(4,
           20,
           10)

# Final value (z)
hasil = lp("min", f.obj, f.con, f.dir, f.rhs)
hasil$constraints
hasil$solution
# hasil dari solusi
hasil$objval





# contoh =====================================================
# Set coefficients of the objective function
f.obj <- c(40, 50, 0, 0)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negative constraint; it is automatically assumed
f.con <- matrix(c(1, 2, 1, 0,
                  4, 3, 0, 0), nrow = 2, byrow = TRUE)

# Set unequality signs
f.dir <- c("=",
           "=")

# Set right hand side coefficients
f.rhs <- c(40,
           120)

# Final value (z)
hasil = lp("max", f.obj, f.con, f.dir, f.rhs)
# tablo 
hasil$constraints
hasil$solution
# hasil dari solusi
hasil$objval






# responsi
x = matrix(c(6, 6, 6, 4, 4, 2, 4, 5, 5, 6, 7, 8), 3, 4,byrow = T)
x
row.signs <- rep ("=", 3)
cap = c(310, 100, 290)
col.signs <- rep ("=", 4)
requ = c(150, 130, 120, 300)

hasil = lp.transport(x, "max", row.signs, cap, col.signs, requ)
hasil
hasil$solution


hasil = lp.transport(8-x, "min", row.signs, cap, col.signs, requ)
hasil
hasil$solution



# soal di ppt
x = matrix(c(20, 5, 8, 15, 20, 10, 25, 10, 19), 3, 3, byrow = T)
x
row.signs <- rep ("=", 3)
kanan = c(90, 60, 50)
col.signs <- rep ("=", 3)
bawah = c(50, 110, 40)
hasil = lp.transport(x, "min", row.signs, kanan, col.signs, bawah)

for(i in hasil) print(i)

hasil$solution
hasil


# cash flow ===============================================
str <- '6000 0,1 14000 0,1
7000 0,2 12000 0,15
8000 0,4 10000 0,5
9000 0,2 8000 0,15
10000 0,1 6000 0,1'

df = read.table(textConnection(str)) %>% 
    mutate_all(~str_replace_all(.x, ',', '.')) %>% 
    type_convert() 

df[,1:2] %>% `colnames<-`(c('ncf', 'p')) %>% 
  mutate(ENCF = ncf*p,
         v = (ncf - sum(ENCF))^2*p) %>% 
  summarise('E(NCF)'= sum(ENCF), 'v(NCF)' = sum(v),
            cv = sqrt(sum(v))/sum(ENCF))

df[,3:4] %>% `colnames<-`(c('ncf', 'p')) %>% 
  mutate(ENCF = ncf*p,
         v = (ncf - sum(ENCF))^2*p) %>% 
  summarise('E(NCF)'= sum(ENCF), 'v(NCF)' = sum(v),
            cv = sqrt(sum(v))/sum(ENCF))



# asignment problem ==============================================================
matriks <- matrix(c(1, 4, 6, 3,
                9, 7, 10, 9,
                4, 5, 11, 7,
                8, 7, 8, 5), 4, byrow = T)

hasil = lp.assign(matriks)
hasil$solution



matriks <- matrix(c(52.4, 48.3, 55.6, 49.5,
                    55.4, 58.2, 59.1, 57.3,
                    62.7, 62.5, 60.9, 63.2,
                    47.7, 49.1, 53.5, 52.1), 4, byrow = T)

hasil = lp.assign(matriks)
hasil$solution


matriks <- matrix(c(30, 37, 40, 28, 40,
                    40, 24, 27, 21, 36,
                    40, 32, 33, 30, 35,
                    25, 38, 40, 36, 36,
                    29, 62, 41, 34, 39), 5, byrow = T)
matriks
hasil = lp.assign(matriks, "max")
hasil$solution




# pm
library(ProjectManagement)
prec1and2<-matrix(0,nrow=10,ncol=10)
prec1and2[1,7]<-1; prec1and2[2,4]<-1; prec1and2[2,8]<-1; prec1and2[3,5]<-1;
prec1and2[3,6]<-1; prec1and2[4,7]<-1; prec1and2[5,9]<-1; prec1and2[6,10]<-1;
prec1and2[8,9]<-1; prec1and2[4,5]<-2
prec3and4<-matrix(0,nrow=10,ncol=10)
prec3and4[8,6]<-3; prec3and4[9,7]<-4
dag.plot(prec1and2)


duration<-c(2,1.5,1,1.5,2,2.5,3,4,2,5)
schedule.pert(duration,prec1and2,prec3and4)


m <- matrix(c(1, -30, -20, -60, 0, 0, 0, 0,
              0, 1, 0, 2, 1, 0, 0, 80,
              0, 2, 3, 1, 0, 1, 0, 100,
              0, 1, 2, 6, 0, 0, 1, 190), nrow = 4, ncol = 8, byrow = T)
while (T) {
  k = which.min(m[1,])
  b = which.min(abs(m[-1,ncol(m)]/m[-1,k])) + 1
  cat('indeks kunci baris :', b, "kolom :", k, '\n')
  m[b,] <- m[b,]/m[b,k]
  
  for (i in 1:nrow(m)) {
    if(i != b){
      m[i,] <- m[i,] - m[i,k]*m[b,]
    }
  }
  print(m)
  if(all(m[1,] >= 0) == T) break
}



# moving average =======================================
str <- 'Januari	450
Februari	440
Maret	460
April	410
Mei	380
Juni	400
Juli	370
Agustus	360
September	410
Oktober	450
November	470
Desember	490'

str = '1 Januari 108.216
2 Februari 99.155
3 Maret 90.700
4 April 123.586
5 Mei 91.761
6 Juni 110.646
7 Juli 135.077
8 Agustus 61.545
9 September 60.153
10 Oktober 75.651
11 November 77.060
12 Desember 78.267'

df = read.table(textConnection(str), header = F)
df = read.table(textConnection(str), header = F,
                col.names = c('bulan', 'p'))


SE <- function(x, alpha) {
  hasil = NULL
  for (i in 1:(length(x)-1)) hasil = c(hasil, alpha*x[i+1] + (1-alpha)*x[i])
  return(c(NA, hasil))
}

SMA(x, 5)
SE(x, 0.9)

SMA(df$p, 3)
SE(df$p, 0.8)



df %>% 
  mutate('3' = c(NA, SMA(p, 3)[-12]),
         '5' = c(NA, SMA(p, 5)[-12]),
         '0.2'= SE(p, 0.2),
         '0.8'= SE(p, 0.8)) %>% 
  mutate_at(3:6, ~(.x-p)^2) %>% 
  janitor::adorn_totals('row')









# pay off
x = c(20, 40, 60, 80)
y = c(10, 30, 50, 70)
harga = c(70, 67, 65, 64)

expand.grid(x, y) %>% 
  `colnames<-`(c('t', 'k')) %>% 
  rowwise() %>% 
  mutate(terjual = min(t, k)) %>% 
  ungroup() %>% 
  mutate(harga  = rep(harga, 4),
         sisa = if_else(t > k, t-k, 0),
         lebih = if_else(t < k, k-t, 0),
         untung = terjual*100 + sisa*45 - t*harga - lebih*5) %>% 
  group_by(t) %>% 
  mutate(sesal = max(untung)-untung) %>% 
  dplyr::filter(sesal == max(sesal))


# =====================================================
# permintaan
keadaan = 25:28 
# yang dipunya
tindakan = 25:28 
untung1 = 2
rugi1 = 8


expand.grid(keadaan, tindakan) %>% 
  `colnames<-`(c('keadaan', 'tindakan')) %>% 
  rowwise() %>% 
  mutate(terjual = min(keadaan, tindakan),
         sisa = max(keadaan, tindakan)-terjual) %>% 
  ungroup() %>% 
  mutate(untung = terjual*untung1 - sisa*rugi1) %>% 
  
  pull(untung) %>% 
  matrix(ncol = 4)



# =====================================================
# yang dipunya
tindakan = c(20, 40, 60, 80)
# permintaan
keadaan = c(10, 30, 50, 70)
# harga kulak
harga = c(70, 67, 65, 64)


harga1 = 100
harga1_diluar = 45
rugi = 5

expand.grid(tindakan, keadaan) %>% 
  `colnames<-`(c('tindakan', 'keadaan')) %>% 
  rowwise() %>% 
  mutate(terjual = min(keadaan, tindakan),
         sisa_barang = if_else(tindakan > keadaan, tindakan-keadaan, 0),
         pembeli_tdk_dpt = keadaan-terjual) %>% 
  ungroup() %>% 
  mutate(harga = rep(harga, length(harga)),
         untung = terjual*harga1 + sisa_barang*harga1_diluar - pembeli_tdk_dpt*rugi - tindakan*harga) %>% 
  
  pull(untung) %>% 
  matrix(ncol = 4, byrow = T)






# keputusan dibawah resiko
hbeli = c(100000, 140000)
boperasional = c(0.14, 0.06)

data.frame(permintaan = c(800000, 600000, 400000),
           p = c(0.2, 0.5, 0.3)) %>% 
  mutate(m1 = hbeli[1] + boperasional[1]*permintaan,
         m2 = hbeli[2] + boperasional[2]*permintaan) %>% 
  summarise(sum(p*m1), sum(p*m2))
           


hbeli = c(500, 600, 800)
boperasional = c(6, 4, 3)
permintaan = c(600, 400, 300)
p = c(0.3, 0.4, 0.3)

data.frame(permintaan,
           p) %>% 
  mutate(m1 = hbeli[1] + boperasional[1]*permintaan,
         m2 = hbeli[2] + boperasional[2]*permintaan,
         m3 = hbeli[3] + boperasional[3]*permintaan) %>% 
  summarise(sum(p*m1), sum(p*m2), sum(p*m3))













# model M/M/S =================================
miu = 40
lambda = 30
s = 2


mms <- function(lambda, miu, s, nn = NULL){
  rho = lambda/miu/s
  i = 0:(s-1)
  po = 1/(sum((lambda/miu)^i/factorial(i)) + (lambda/miu)^s/(factorial(s)*(1-rho)))
  lq = po*(lambda/miu)^s*rho/(factorial(s)*(1-rho)^2)
  l = lq + lambda/miu
  wq = lq/lambda
  w = wq + 1/miu
  df <- data.frame(nama = c("rho", "Po", "L", "Lq", "W", "Wq"),
               value = c(rho, po, l, lq, w, wq))
  
  if(!is.null(nn)){
    for (n in 1:nn) {
      if(n > s) pn = (lambda/miu)^n*rho/(factorial(s)*s^(n-s))
      else pn = (lambda/miu)^n*po/factorial(n)
      
      df <- df %>% 
        add_row(nama = paste0("p", n),
                value = pn)    
    }
  }
  return(df)
}

mms(50, miu = 40, 2, n = 3)
mms(8.6, 4, 3, 4)




# pay off =============

x = matrix(c(100, 5, 5,
             9, 81, 0,
             6, 2, 62), byrow = T, ncol = 3)

t = x %>% rowSums()
x = x/t
x = x %>% 
  round(2)

phi = matrix(c(110, 90, 70), nrow = 1)
phi = (phi/sum(phi)) %>% round(2)
phi2%*%(x)

phi2 = (phi%*%(x)) %>% 
  round(2)

(phi2%*%(x)) %>% 
  {(.)*270}

m = t(x)
m

solve(m)%*%matrix(1, nrow = ncol(m), byrow = T)



str <- '0,796 0,091 0,045 0,068
0,133 0,767 0,017 0,083
0 0,109 0,891 0
0,040 0,060 0,040 0,860'

read.table(textConnection(str), header = F) %>% 
  mutate_all(~str_replace_all(.x, ",", ".")) %>% 
  type_convert() %>% 
  solve() %>% 
  {(.)%*%matrix(1, nrow = ncol(m))}

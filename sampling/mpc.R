passenger <- read.csv("D:/Datasets/passenger_estonia.csv", sep =";")
N = nrow(passenger)
n = 200
#=============================================================
#nilai k
k = round(N/n, 0)
k

#AR1 (liniear)
AR = runif(n=1, min=0, max=k)
AR = round(AR, 0)
AR

AR = round(runif(1, 1, 6))

#=============================================================
#Algoritma untuk mendaptkan samepl data scr systematic
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
#=============================================================
index_sampel = systematic(passenger$PassengerId, 200, AR, "liniear")
index_sampel

#ambil subset data sampel
library(dplyr)
sampel = passenger %>% filter(PassengerId %in% index_sampel)
sampel

#Estimasi Total WOR
asal_negara = sampel %>% group_by(Country) %>% count() %>% 
  rename(jumlah_penumpang = n) %>% 
  mutate(proporsi = jumlah_penumpang/n, Total = N*proporsi,
         varians_totaL = N^2*(N-n)*proporsi*(1-proporsi)/(N*(n-1)),
         se = sqrt(varians_totaL),
         rse = se*100/proporsi,
         batas_bawah = Total-qnorm(0.975)*se,
         batas_atas = Total+qnorm(0.975)*se) %>% 
  rename(n = jumlah_penumpang, "v(Ycap)"=varians_totaL)

asal_negara

#Estimasi rata-rata
umur = sampel %>% group_by(Gender) %>% 
  count(xbar = mean(Age), "s^2" = var(Age), 
        varians_rata2 = (N-n)*var(Age)/(N*n),
        se = sqrt(varians_rata2) ,
        rse = se*100/mean(Age),
        batas_bawah = mean(Age)-qnorm(0.975)*se,
        batas_atas = mean(Age)+qnorm(0.975)*se)

umur

#urutkan ====================================================================
passenger = passenger %>% arrange(Gender, Age)
passenger

index_sampel = systematic(passenger$PassengerId, 200, AR, "liniear")
index_sampel

sampel = passenger %>% filter(PassengerId %in% index_sampel)
sampel

#Estimasi Total (proporsi)
asal_negara = sampel %>% group_by(Country) %>% count() %>% 
  rename(jumlah_penumpang = n) %>% 
  mutate(proporsi = jumlah_penumpang/n, Total = N*proporsi,
         varians_totaL = N^2*(N-n)*proporsi*(1-proporsi)/(N*(n-1)),
         se = sqrt(varians_totaL),
         rse = se*100/proporsi,
         batas_bawah = Total-qnorm(0.975)*se,
         batas_atas = Total+qnorm(0.975)*se) %>% 
  rename(n = jumlah_penumpang)

asal_negara

#===========================================================================

umur
pengunjung1 = c(68, 74, 59, 83, 134, 150, 90, 88, 120, 93, 89, 80)
pengunjung = data.frame(pengunjung1)
pengeluaran1 =c(545500
,1265900
,1789765
,320987
,786876
,1345897
,897095
,2098786
,245357
,1111324
,1876909
,2765989
,1000909
,897009
,489009)
pengeluaran = data.frame(pengeluaran)
#estimasi rata-rata
N = 100
n = 15
f = ((N-n)/N)
f = 1
pengeluaran %>% 
  count("s^2" = var(pengeluaran), 
        rata2 = mean(pengeluaran),
        varians_rata = f*var(pengeluaran)/n,
        se = sqrt(varians_rata) ,
        rse = se*100/rata2,
        batas_bawah = rata2- qnorm(0.975)*se,
        batas_atas = rata2+ qnorm(0.975)*se)

(100*0.04^2*var(pengeluaran1))/( 100*0.02^2 + 0.04^2*var(pengeluaran1))
#estimasi total 
N = 45
n = 12
f = ((N-n)/N)
pengeluaran %>% 
  count("s^2" = var(pengeluaran), 
        total = N*mean(pengeluaran),
        varians_total = f*N^2*var(pengeluaran)/n,
        se = sqrt(varians_total) ,
        rse = se*100/total,
        batas_bawah = total-qnorm(0.975)*se,
        batas_atas = total+qnorm(0.975)*se)
      
data1 = c(2,4,5,1,3,5,3,6,1,4,5,6,4,3,2)
data1 = data.frame(data1)
n = nrow(data1)
N = 100
f = ((N-n)/N)
#estimasi proporsi
data1 %>% transmute(kelas = cut(data1, c(0, 3, 5,Inf), c(1,2,3))) %>% 
  group_by(kelas) %>%  count() %>% rename(jumlah = n) %>% 
  mutate(p = jumlah/nrow(data1), varians_rata = f*p*(1-p)/(n-1),
         se = sqrt(varians_rata), rse = se*100/p, 
         batas_bawah = p-qnorm(0.975)*se, batas_atas = p+qnorm(0.975)*se)

f = ((N-n)/N)
#estimasi jumlah
data1 %>% transmute(kelas = cut(data1, c(0,100, Inf), c(1,0))) %>% 
  group_by(kelas) %>%  count() %>% rename(jumlah = n) %>% 
  mutate(p = jumlah/nrow(data1), varians_rata = f*p*(1-p)/(n-1),
         se = sqrt(varians_rata), rse = se*100/p, 
         batas_bawah = p-qnorm(0.975)*se, batas_atas = p+qnorm(0.975)*se)

#==========================================================
data1 = c(24, 35, 42, 31, 29, 50, 27, 52, 46, 39)
data1 = data.frame(data1)
N = 50
n = nrow(data1)
f = ((N-n)/N)
data1 %>% transmute(kelas = cut(data1, c(0, 30, Inf), c(1,0))) %>% 
  group_by(kelas) %>%  count() %>% rename(jumlah = n) %>% 
  mutate(p = jumlah/nrow(data1), varians_rata = f*(p*(1-p))/(n-1),
         se = sqrt(varians_rata), rse = se*100/p, 
         batas_bawah = p-qnorm(0.975)*se, batas_atas = p+qnorm(0.975)*se)
#==========================================================
library(survey)
passenger   

passenger$sampling.weight = N/n
passenger

#desain sampling wr
passenger = passenger %>% mutate(jk = if_else(Gender == "Female", 0, 1))
passenger

desing.srs.wr <- svydesign(ids=~1, weights=~sampling.weight, data=passenger)

#estimasi rata" pengeluaran
est1.wr <- svymean(~Age, desing.srs.wr)
est1.wr

#estimasi proporsi
est2.wr <- svymean(~jk, desing.srs.wr)
est2.wr
confint(est2.wr)

#estimasi total laki-laki karena nilainya 1
est3.wr <- svytotal(~jk, desing.srs.wr)
est3.wr
confint(est3.wr)

#estimasi rata" umur menurut jenis kelamin
est4.wr <- svyby(~Age, ~jk, desing.srs.wr, svymean)
est4.wr
confint(est4.wr)

desing.srs.wor <- svydesign(ids=~1, weights=~sampling.weight, data=passenger)
est2.wr <- svytotal(~Country, desing.srs.wor)
est2.wr
confint(est2.wr)

#========================================================
data = c(6,2,7,4,4,5,6,4,2,5,3,2,6,12,3,7,3,0)

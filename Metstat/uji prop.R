#Latihan Syntax:
#Uji Proporsi

#Uji Proporsi 1 dan 2 sample
## Generate data
heads <- rbinom(1, size = 100, prob = .5)
heads
## H0: p == 0.3
## H1: p != 0.3
prop.test(heads, 100, p=0.3)

## H0: p == 0.5
## H1: p != 0.5
prop.test(heads, 100, p=0.6)
prop.test(heads, 100, correct = FALSE)

## H0: p == 0.08
## H1: p != 0.08
prop.test(x=25, n=500, p= 0.08, correct= F)

## Kasus: ada 2 kota  (A dan B) dgn jumlah penduduk  masing2 200 dan 500,
## dilakukan jajak pendapat, dan didapat pada kota A dan yang setuju sebayak 120 dan 240 orang.
## H0: p1 == p2 Proporsi penduduk yang setuju antara 2 kota adalah sama
## H1: p1 != p2 Proporsi penduduk yang setuju antara 2 kota berbeda
setuju <- c( 120, 240 )
penduduk <- c( 200, 500 )
prop.test(setuju, penduduk)

#Uji Proporsi 1 dan 2 sample
## H0: The null hypothesis is that the four populations from which
## the patients were drawn have the same true proportion of smokers.
## A: The alternative is that this proportion is different in at
## least one of the populations.
smokers <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )
prop.test(smokers, patients)

#Chi-squared Test of Independence
## Chi-squared Test of Independence ##
#Dari data hsb2 ingin dilakukan uji apakah ada asosiasi antara type sekolah dengan social economic status.
hsb = read.csv("D:/Datasets/hsb2-2.csv")

tab <- table(hsb$schtyp, hsb$ses)
tab
chisq.test(tab)

## test for distribution #
x <- rnorm(50,10,4)
y <- runif(100, 0, 0.5)
qqnorm(x, pch= 16); qqline(x,col = 2, lwd =4)
qqnorm(y, pch= 16); qqline(x,col=2, lwd=4)

plot(x)
plot(y,ylim = c(0,1))
plot(density(x))
plot(density(y))

#Do x and y come from the same distribution
ks.test(x,y)
shapiro.test(x)
shapiro.test(y)
ks.test(x, "pnorm",5,4)  #cek apakah sama dengan distribusi pnorm
ks.test(y, "punif",0,0.5)
ks.test(x, "pnorm", 10, 4)

library(stringr)
library(dplyr)
#fungsi mengubah string menjadi df
#=====================================================================
str.to.df <- function(str, pemisah=" ", dim, koma=T){
  if(koma) str = str_replace_all(str, ",", ".")
  
  str = str_replace_all(str, "\n", ",")
  str = str_replace_all(str, pemisah, ",")
  str = str_split(str, ",", simplify = T)
  str = data.frame(matrix(str, nrow = dim[1], ncol=dim[2], byrow = T))
  
  for(i in 1:ncol(str)){
    tryCatch({str[,i] = as.numeric(str[,i])}, 
             warning = function(a) str[,i] = str[,i])
  }
  return(str)
}
#=====================================================================
#copy data dari soal
str = "100 434 79
151 0 0
71 10 0
3776 7335 2088
5070 2726 1253
2902 224 19
85 73 51
0 30 0
35 0 0
270 326 194
266 502 164
339 0 0"

df = str.to.df(str, dim = c(12, 3))
#atau bisa ketik sendiri datanya jika tidak pakai fungsi diatas
colnames(df) = c("40%_terbawah", "40%_Menengah", "20%_Teratas")
df


df = df %>% mutate(status_kemiskinan = rep(c("Tidak miskin", "miskin"), each=6),
              status_pekerjaan = rep(c("Bukan Petani", "Petani", "Bukan Petani", "Petani"), each=3),
              sumber_pendapatan_utama = rep(c("Bukan Pertanian", "Pertanian", "Pendapatan Lainnya"), 4),
              .before = "40%_terbawah")
df



## H0: P1 == P2
## H1: P1 != P2

petani = c(
  df %>% filter(status_pekerjaan == "Petani", status_kemiskinan == "miskin") %>% 
    summarise_if(is.numeric, sum) %>% sum(),
  df %>% filter(status_pekerjaan == "Petani", status_kemiskinan == "Tidak miskin") %>% 
    summarise_if(is.numeric, sum) %>% sum()
)

total = c(
  df %>% filter(status_kemiskinan == "miskin") %>% 
    summarise_if(is.numeric, sum) %>% sum(),
  df %>% filter(status_kemiskinan == "Tidak miskin") %>% 
    summarise_if(is.numeric, sum) %>% sum()
)
#        miskin   tidak miskin
petani
total

prop.test(petani, total)



## H0: P1 == P2 == P3
## H1: Pi != Pj       i!=j   i,j = 1,2,3

petani = c(
  df %>% filter(status_pekerjaan == "Petani") %>% 
    summarise(`40%_terbawah`) %>% sum(),
  df %>% filter(status_pekerjaan == "Petani") %>% 
    summarise(`40%_Menengah`) %>% sum(),
  df %>% filter(status_pekerjaan == "Petani") %>% 
    summarise(`20%_Teratas`) %>% sum()
)

total = c(
  df %>% summarise(`40%_terbawah`) %>% sum(),
  df %>% summarise(`40%_Menengah`) %>% sum(),
  df %>% summarise(`20%_Teratas`) %>% sum()
)

#       40% terbawah    40% menengah      20% tertatas
petani
total

prop.test(petani, total)


## H0: P1 == P2 == P3
## H1: Pi != Pj       i!=j   i,j = 1,2,3

petani = c(
  df %>% filter(status_pekerjaan == "Petani", sumber_pendapatan_utama == "Bukan Pertanian") %>% 
    summarise_if(is.numeric, sum) %>% sum(),
  df %>% filter(status_pekerjaan == "Petani", sumber_pendapatan_utama != "Bukan Pertanian") %>% 
    summarise_if(is.numeric, sum) %>% sum()
)

total = c(
  df %>% filter(sumber_pendapatan_utama == "Bukan Pertanian") %>% 
    summarise_if(is.numeric, sum) %>% sum(),
  df %>% filter(sumber_pendapatan_utama != "Bukan Pertanian") %>% 
    summarise_if(is.numeric, sum) %>% sum()
)

#         bukan pertanian       pertanian + pendapatan lainnya 
petani
total

prop.test(petani, total)


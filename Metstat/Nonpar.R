library(tidyverse)

# binom test ===========================================
binom.test(x = 14, n = 20, p = 1/2, conf.level = 0.95)
binom.test(x = 2, n = 18, p = 1/2, conf.level = 0.95)
binom.test(x = 42, n = 64, p = 1/2, conf.level = 0.95)

# untuk binom test di X bukan yang terkecil tapi
# yang ada di H0 : A = B
# maka X itu A
binom.test(6, 20)
binom.test(24, 30, alternative = "g")

pnorm(-3.10)




# chi-square 1 populasi ================================
c(13, 33, 14, 7, 36, 17) %>% 
  chisq.test()

# ada yang Ei < 5
c(3, 5, 6, 3) %>% 
  chisq.test()

c(3, 5, 6 + 3)
c(6, 6, 3 + 2)





# kolmogorv 1 sampel
ks.test(c(0, 1, 0, 5, 4), "punif", 1, 5)

x = c(1, 2, 4, 3)
cumsum(x)




# run test
snpar::runs.test(c(1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 2, 2))

library(snpar)
str = "M M S M S S M S M M S M S S S S M M S S S M S M S M S M M M S S S M S S M M M S"

x = str_split(str, " ")[[1]] %>% 
  factor(labels = 1:2) %>% as.numeric()
x
table(x)
runs.test(x)


# mc nemar test =========================================================
# untuk A + D < 10
# n = A + D
# x = terkecil antara A dan D
binom.test(x = 3, n = 9, alternative = "less") # 1 arah
binom.test(x = 3, n = 9) # 2 arah


x = matrix(c(10, 40, 85, 85), nrow = 2, byrow = T,
           dimnames = list("sebelum" = c("Membeli", "Tidak Membeli"),
                           "sesudah" = c("Tidak Membeli", "Membeli"))
           )
# untuk A + D >= 10
mcnemar.test(x)


# uji Tanda ==============================================================
# untuk n <= 25
# x = terkecil antara tanda - dan +
# n = n(+) + n(-)
binom.test(3, 14, alternative = "less") # 1 arah

# untuk n > 25
N = 85
x = 59

if(x >  N/2){
  z <- ((x - 0.5) - N/2)/(sqrt(N)/2)
}else{
  z <- ((x + 0.5) - N/2)/(sqrt(N)/2)
}

pnorm(z, lower.tail = F) # 1 sisi
# Tolah Ho : pvalue <= alpha


str = "1	2	3	4	5	6	7	8	9	10
5	2	3	5	3	3	4	4	4	3
4	1	4	4	1	3	2	2	4	4
11	12	13	14	15	16	17	18	19	20
2	2	4	4	1	3	3	4	1	5
2	1	3	2	3	2	1	2	2	3
21	22	23	24	25	26	27	28	29	30
3	2	2	1	4	5	4	3	2	2
2	2	1	2	3	3	3	2	3	1"
dt = read.table(textConnection(str))
dt = cbind(dt[1:3,], dt[4:6,], dt[7:9,])
dt = dt %>% 
  t() %>% 
  as.data.frame() %>% 
  remove_rownames() %>% 
  select(-1) %>% 
  rename(lama = 1, baru = 2)

library(BSDA)
z = dt$lama-dt$baru
SIGN.test(z, md = 0, alternative = "g")

str = "A 4 2 2 +
B 4 3 1 +
C 5 3 2 +
D 5 3 2 +
E 3 3 0 0
F 2 3 -1 -
G 5 3 2 +
H 3 3 0 0
I 1 2 -1 -
J 5 3 2 +
K 5 2 3 +
L 5 2 3 +
M 4 5 -1 -
N 5 2 3 +
O 5 5 0 0
P 5 3 2 +
Q 5 1 4 +"
dt = read.table(textConnection(str)) 
# H1 ibu lebih kuat
SIGN.test(dt$V2, dt$V3, alternative = "g")


qchisq(0.05*2, 1, lower.tail = F)
qnorm(0.05/2)
2*pnorm(2.5495, lower.tail = F)



str = "58	78	84	90	97	70	90	86	82	  68	  93	 70	 94	 70	 100	 67	 68	 75
59	90	70	74	83	90	76	88	84	  80	 68	 82	 100	 92	 100	 84	 98	 80"
d = read.table(textConnection(str)) 
d = cbind(d[1,], d[2,]) %>% 
  t() %>% as.data.frame() %>% 
  pull

length(unique(d))
table(d)

data.frame(d) %>% 
  group_by(d) %>% 
  summarise(n = n()) %>% 
  mutate(fk = cumsum(n), 
         sx = fk/36,
         sx = round(sx, 4),
         fx = pnorm((d-80)/10),
         fx = round(fx, 4),
         dd = abs(fx-sx)) %>% 
  as.data.frame() %>% 
  filter(dd == max(dd))

ks.test(d, "pnorm", 80, 10)


# wilcoxon =======================================
str = "1	4.2	4	10	4.9	4.9	19	4.5	4.1
2	4.7	4.1	11	6.3	6	20	5.1	5
3	6.6	6.2	12	5.3	4.9	21	5.4	5.7
4	7	6.9	13	6.9	6.5	22	6.3	6.3
5	6.9	6.2	14	5.6	5.5	23	5.4	5.7
6	4.5	4.2	15	4.1	4.7	24	4.6	4.1
7	5.7	5.7	16	5.5	5.1	25	5.3	5
8	6.3	5.8	17	5.7	5.2	26	5.1	5.1
9	7.4	6.9	18	6.1	6.4	 	 	 "
df = read.table(textConnection(str)) %>% as.matrix()
df = rbind(
  df[,1:3], df[,4:6], df[,7:9]
) %>% as.data.frame() %>% 
  drop_na()

# biasa radial
colnames(df) <- c("m", "b", "r")

df
df %>% 
  mutate(d = r-b,
         rank = abs(d)) %>% 
  arrange(rank) %>% 
  mutate(peringkat = c(0,0,0,0,1:22)) %>% 
  group_by(rank) %>% 
  mutate(p = mean(peringkat)) %>% 
  arrange(m) %>% 
  as.data.frame() 

wilcox.test(df$r,df$b, paired = T)


str = '83 91 94 89 96 91 92 90 92 85 91 90 81 83 84 83 88 90 84 85'
read.table(textConnection(str), header = F) %>% 
  t() %>% as.data.frame() %>% pull() %>% 
  median()



# fisher
mat = matrix(c(5,8,7,2), 2,2, byrow = T)
mat
fisher.test(mat)
mat = matrix(c(5,9,7,2), 2,2, byrow = T)
mat
hasil = fisher.test(mat)
hasil
fisher.test(mat, alternative = "l")

mat = matrix(c(4,3,1,4), 2,2, byrow = T)
mat
fisher.test(mat)

vec = c(12, 2, 9, 0) %>% factorial()
a = factorial(14)*factorial(9)*factorial(12)*factorial(11)
a/(factorial(23)*vec[1]*vec[2]*vec[3]*vec[4])


mat = matrix(c(5,9,1,2), 2,2, byrow = T)

for(i in 1:2){
  mat = mat - matrix(c(1,-1,-1,1), 2,2, byrow = T)
  print(mat)
  vec = as.vector(mat) %>% factorial()
  a = factorial(colSums(mat)[1])*factorial(colSums(mat)[2])*factorial(rowSums(mat)[1])*factorial(rowSums(mat)[2])
  p = a/(factorial(sum(mat))*vec[1]*vec[2]*vec[3]*vec[4])
  print(p)
}

# mann whitney ===================================================
# nilai W = U
wilcox.test(nilai~bimbingan, data = df, alternative = "g")


desa = c(5, 7, 10, 11, 9, 7, 6, 6, 6, 5, 8, 9, 10, 10, 9, 9, 8, 8, 7, 6)
kota = c(8, 8, 8, 10, 8, 7, 7, 7, 7, 5, 8, 9, 11, 10, 9, 10, 10, 11, 8, 10)
wilcox.test(desa, kota)
pnorm(-1.63)


data.frame(nilai = c(desa, kota),
           lokasi = rep(c("desa", "kota"), each = 20)) %>% 
  mutate(rangking = rank(nilai)) %>% 
  group_by(lokasi) %>% 
  summarise(sum(rangking)) %>% 
  as.data.frame()



data.frame(nilai = c(desa, kota),
           lokasi = rep(c("desa", "kota"), each = 20)) %>% 
  group_by(nilai) %>% 
  summarise(t = n(), tt = t^3-t) %>% 
  summarise(sum(tt))



# soal no 3
sk = str2vec("10	6	8	10	12	13	11	9	5	11	10	6	5	4	5	10")
se = str2vec("13	17	14	12	10	9	15	16	11	8	9	7	15	7	14	17	18	20	15	16	16")

data.frame(lbelajar = c(sk, se),
           mahasiswa = rep(c("sk", "se"), c(16, 21))) %>% 
  mutate(rangking = rank(lbelajar)) %>% 
  group_by(lbelajar) %>%
  summarise(t = n(), tt = t^3-t) %>% 
  summarise(sum(tt))

wilcox.test(sk, se, paired = F, alternative = "l")
(57.5 - 21*16/2)/sqrt(21*16*(21+16+1)/12)

pnorm(-3.3971, lower.tail = F)
pnorm(-3.3876)

# no 1
a = c(6, 7, 7, 6, 8, 10, 9, 9, 8, 10)
b = c(5, 10, 9, 9, 8, 7, 7, 7, 8, 6)
wilcox.test(a, b)

data.frame(nilai = c(a, b), metode = rep(letters[1:2], each = 10)) %>% 
  mutate(rangking = rank(nilai)) %>% 
  group_by(metode) %>% 
  summarise(sum(rangking))


df$rank <- 0
df$rank[df$d != 0] <- rank(df$d[df$d != 0])
df


# fisher test ========================================================
olah <- function(mat){
  print(mat)
  vec = as.vector(mat) %>% factorial()
  a = factorial(colSums(mat)[1])*factorial(colSums(mat)[2])*factorial(rowSums(mat)[1])*factorial(rowSums(mat)[2])
  p = a/(factorial(sum(mat))*vec[1]*vec[2]*vec[3]*vec[4])
  cat(p, "\n")
  return(p)
}

fisher.test2 <- function(m){
  mat = m
  pvalue = c(olah(mat))
  # ifelse(which.min(m) %in% c(2, 3), j <- -1, j <-  1)
  if(which.min(m) %in% c(2, 3)){
    j <- -1
    a = min(m[c(1,4)])
  }else{
    j <- 1
    a = min(m[c(2,3)])
  }
  # kurangi dulu
  for(i in 1:min(m)){
    mat = mat - j*matrix(c(1,-1,-1,1), 2, 2, byrow = T)
    pvalue = c(pvalue, olah(mat))
  }
  mat = m
  for(i in 1:a){
    mat = mat + j*matrix(c(1,-1,-1,1), 2, 2, byrow = T)
    pvalue = c(pvalue, olah(mat))
  }
  return(sum(pvalue[pvalue <= pvalue[1]]))
}


b = matrix(c(3, 2, 4, 11), 2, byrow = T)
fisher.test(mat)
fisher.test2(mat)

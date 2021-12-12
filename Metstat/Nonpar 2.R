n_dupc <- function(x){
  n = length(x)-dplyr::n_distinct(x)
  if(n == 0) return(n)
  else return(n+1)
}

# uji q cohcran ====================================
str <- 'A 0 1 1 1 
B 1 0 0 1 
C 0 1 0 0
D 0 0 0 0 
E 0 0 0 1 
F 0 0 1 1 
G 0 1 0 0 
H 0 0 0 0 
I 1 0 0 1 
J 1 1 1 1 
K 0 0 1 1 
L 0 0 0 0 
M 0 0 0 0 
N 1 1 0 1 
O 0 0 0 1
P 0 0 1 1
Q 1 1 0 1
R 0 0 0 1
S 0 0 1 0 
T 0 0 1 1'

x= read.table(textConnection(str)) %>% 
  type_convert() %>% 
  select(-V1) %>% 
  as.matrix()

x %>% as.data.frame() %>% 
  janitor::adorn_totals('col', name = 'Li') %>% 
  mutate(Li2 = Li^2) %>% 
  janitor::adorn_totals('row', name = 'Gi') 

library(nonpar)
cochrans.q(x)
cochrans.q(x[,-1])

k = ncol(x)
(k-1)*(k*sum(colSums(x)^2)-sum(colSums(x))^2)/(k*sum(rowSums(x))-sum(rowSums(x)^2))

# Uji friedman =====================================
str <- '7.1	4.1	4.1	5.1
6.2	3.4	4.4	6.5
8.3	4.2	4.2	7.3
6.5	5.2	1.3	5.4
7.6	4.1	2.4	6.8
5.6	6.3	4.3	5.6
7.9	6.7	3.1	6.7
7.4	7.4	8.2	8.4
5.4	5.4	2.4	5.4
8.1	7.3	4.3	4.3'

x = read.table(textConnection(str)) %>% as.matrix()
x = df %>% as.matrix()
friedman.test(x)

rank = NULL
for (i in 1:nrow(x)) rank = rbind(rank, rank(-x[i,]))
rank
rank %>% colSums()
k = ncol(x)
n = nrow(x)

12*sum(colSums(rank)^2)/(n*k*(k+1)) - 3*n*(k+1)


x %>% as.data.frame() %>% 
  rowwise() %>% 
  # jangan lupa ganti mulai-sampai kolom berapa
  mutate(t = ncol(x)-length(unique(c_across(everything()))),
         t = if_else(t==0,0,t+1)) %>% 
  ungroup() %>% 
  summarise(sum(t), sum(t^3))

13.23/(1-(75-15)/(n*k*(k^2-1)))

# perbandingan 
k = ncol(x)
n = nrow(x)
alpha = 0.01
nilai = qnorm(1-alpha/(k*(k-1)))*sqrt(n*k*(k+1)/6)
nilai  

# jumlah rank
nama <- colnames(x)
nama <- expand.grid(nama[1:(k/2)], nama[(k/2 + 1):k]) %>% 
  transmute(nama = paste0(Var1, '-', Var2)) %>% 
  pull

r = colSums(rank)
r
expand.grid(r[1:(k/2)], r[(k/2 + 1):k]) %>% 
  mutate(nama, '|Ri-Rj|'= abs(Var1-Var2),
         keputusan = ifelse(`|Ri-Rj|`>=nilai, 'Berbeda', 'tidak'))


# uji kruskal wallis ==============================
str <- 'Produksi	Pemasaran	Keuangan
56	100	42
39	87	38
48	51	89
38	95	75
73	68	35
50	42	61
62	100	NA
NA	89	NA'
  
str = "3.1	3.8	4.0
2.6	4.1	5.5
2.9	2.9	5.0
NA 3.4	4.8
NA 4.2	NA"

df <- read.table(textConnection(str), header = F) 
df <- df %>% 
  pivot_longer(everything()) %>% 
  drop_na() %>% 
  arrange(name) 
df
kruskal.test(value~name, df)


df %>% 
  mutate(rank = rank(value)) %>% 
  group_by(name) %>% 
  # arrange(name) %>%
  # as.data.frame()
  summarise(r = sum(rank), n = n(), rbar = mean(rank)) %>% 
  as.data.frame() %>% 
  ungroup() -> rank

rank
rank %>%   
  summarise(h = 12*sum(r^2/n)/(sum(n)*(sum(n)+1))-3*(sum(n)+1))

# hitung t
df %>% 
  group_by(value) %>%
  summarise(t = n_dupc(value), n = n(),
            `sigma t^3 - t`=t^3 - t) %>% 
  as.data.frame() %>% 
  summarise_all(sum) %>% 
  mutate(fk = 1-`sigma t^3 - t`/(n^3 - n))



2*21.702/4
qchisq(0.05, 3-1, lower.tail = F)


# perbandingan ganda
k = nrow(rank)
N = sum(rank$n)
ni = 3
nj = 5
qnorm(1-0.05/(k*(k-1)))*sqrt(N*(N+1)*(1/ni + 1/nj)/12)

rank
nama <- combn(1:nrow(rank), 2, FUN = function(s) paste(s[1],"-",s[2]))
n <- combn(rank$n, 2) %>% t() %>% as.data.frame() %>% rename(ni = V1, nj = V2)
combn(rank$rbar, 2) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(nama, .before = V1) %>% 
  mutate(ni = n$ni, nj = n$nj,
         abs(V1-V2),
         nilai = qnorm(1-0.05/(k*(k-1)))*sqrt(N*(N+1)*(1/ni + 1/nj)/12),
         status = if_else(abs(V1-V2) >= nilai, 'Tolak Ho', 'Gagal'))






# uji perluasan median ===================================
str <- 'Gol_I	0	4	2	4	4	1	3	3	2	2	1	4	6
Gol_II	4	2	2	4	6	1	3	4	2	3	4	NA NA 	
Gol_III	5	3	4	5	3	2	3	3	3	2	1	2	NA
Gol_IV	5	3	4	6	8	5	6	4	3	3	4	4	7'

read.table(textConnection(str), header = F) %>% 
  pivot_longer(2:14) %>% 
  select(-name) %>% 
  drop_na() -> x

# MG
mg = median(x$value)

x %>% 
  mutate(pos = if_else(value > mg, 'A', 'B'),
         .before = V1) %>% 
  select(-value) %>% 
  table() 
  chisq.test()

library(agricolae)
Median.test(x$value, x$V1)



# koefisien korelasi 


# koefisien korelasi kontingensi ======================
library(rcompanion)
str <- '37	12	5
11	80	5
6	40	27
5	10	33'

str = '19 17
16 20
5 3
'
df <- read.table(textConnection(str), header = F)
hasil <- chisq.test(df)
hasil

# koef kontigensi
sqrt(hasil$statistic/(hasil$statistic + sum(hasil$observed))) %>% 
  `names<-`(NULL)

# cramer v
df %>% 
  pivot_longer(everything()) %>%   
  {cramerV((.)$name, (.)$value)}


# spearman ============================================
str <- '9	10
2	6
5	3
11	14
10	4
4	9
3	1
14	7
1	2
8	12
15	13
7	8
12	11
6	5
13	15'

df <- read.table(textConnection(str), header = F) 
df %>% 
  mutate(di = V1-V2,
         di^2) 
  summarise_all(sum)
  
cor.test(df$V1, df$V2, method = "spearman")


# kendal tau ==========================================
str <- 
'2 2 2 4 5 6 7 8 9 10 11 12
10 11 9 12 6 4.5 8 4.5 7 2 1 3'

df <- read.table(textConnection(str), header = F) %>% 
  t() %>% as.data.frame() %>% 
  arrange(V1) %>% 
  remove_rownames()
df



kendal_tau <- function(df, acuan){
  if(acuan == 1) df <- df %>% `colnames<-`(c('V1', 'V2'))
  else if(acuan == 2) df <- df %>% `colnames<-`(c('V2', 'V1'))
  
  df <- df %>% 
    arrange(V1)
  
  value = c()
  for (i in 1:nrow(df)) {
    x = c(rep(NA, i-1), df$V2[i:nrow(df)])
    #x[x == x[i]] <- 0
    x = ifelse(x > x[i], 'C',
               ifelse(x == x[i], '0', 'd'))
    
    if(df$V1[i] == df$V1[2 + (i-1)%%(nrow(df)-1)]){
      x[i+1] = 0
    }

    value = c(value, x)
  }
  
  #2 + (19-1)%%(nrow(df)-1)
  
  matrix(value, nrow = nrow(df), byrow = T) %>% 
    `diag<-`(df$V2) %>% 
    as_tibble() %>% 
    rowwise() %>% 
    mutate(C = sum(c_across(everything())=='C', na.rm = T),
           D = sum(c_across(-C)=='d', na.rm = T)) %>% 
    ungroup() %>% 
    janitor::adorn_totals('row')
}


kendal_tau(df, 1)



kendal_tau(df, acuan = 2)
cor.test(df$V2, df$V3, method = 'ken', exact = F)
cor.test(df$tes, df$rating, method = 'ken', exact = F)

# hitung t
df %>% 
  group_by(V2) %>%
  summarise(t = n_dupc(V2), n = n(),
            t^2 - t) %>% 
  as.data.frame() %>% 
  summarise_all(sum)





# Kendal W ==========================================
# kategori sebagai kolom
str <- '1	6	3	2	5	4
1	5	6	4	2	3
6	3	2	5	4	1'

df <- read.table(textConnection(str), header = F) 
df

t(df)

# input sampel sebagai kolom
library(irr)
kendall(t(df), correct = T)
kendall(df, correct = T)

# cara manual , sampel sebagai baris
df <- df %>% 
  mutate_all(~rank(.x)) %>% 
  t()
# sebagai kolom
df <- df %>% 
  t() %>% as.data.frame() %>% 
  mutate_all(~rank(.x)) %>% 
  t()

colSums(df) # RI
colMeans(df) 

sum(df)/ncol(df) # dibagi n
# s
s = (colSums(df) - sum(df)/ncol(df))^2 %>% sum
s
# w
n = ncol(df)
(12*sum(colMeans(df)^2) - 3*n*(n+1)^2)/(n*(n^2-1))



# w jika ada yang sama
value_t <- function(x){
  table(x) %>% 
    as.data.frame() %>% 
    dplyr::filter(Freq > 1) %>% 
    summarise(sum(Freq^3 - Freq)) %>% 
    pull %>% 
    return
}

df %>% t() %>% 
  as.data.frame() %>% 
  summarise_all(value_t)
  sum -> TT
TT

chi <- (12*sum(colMeans(df)^2) - 3*n*(n+1)^2)/(n*(n^2-1) - TT/nrow(df)) 
chi

# chisqr
nrow(df)*(n-1)*chi


qchisq(0.05, n-1, lower.tail = F)

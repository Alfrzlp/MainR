library(dm)

# 2014/2015 No 1 ------------------------------------------------------------
s1 <- 'Branded 6 3 7 4 5 2 4 4
Local 9 7 9 8 8 5 7 6'

s2 <- 'Branded 4 2 5 3 3 1 2 3
Local 6 5 7 6 7 3 5 4'

x1 <- read_string(s1)[-1] %>% t()
x2 <- read_string(s2)[-1] %>% t()

x1
x2

S1 <- var(x1)
S2 <- var(x2)

# jika simetris dan nilai eigen positif
all.equal(var(x1), t(var(x1)))

eigen(var(x1))
x <- matrix(c(-100, -100), nrow = nrow(S1))
t(x) %*% S1 %*% x 

matrixcalc::is.positive.definite(var(x1))

# b
det(S1)
det(S2)

# c
p <- ncol(x1)
alpha <- 0.05
n1 <- nrow(x1)
n2 <- nrow(x2)
Spooled <- ((n1 - 1)*S1 + (n2 - 1)*S2)/(n1 + n2 - 2)
solve(Spooled)

xbar_dif <- matrix(colMeans(x1)-colMeans(x2))
T2 <- n1*n2/(n1 + n2)*t(xbar_dif) %*% solve(Spooled) %*% xbar_dif
T2
c2 <- (n1 + n2 - 2)*p*qf(alpha, p, n1 + n2 - p - 1, lower.tail = F)/(n1 + n2 - p - 1)
c2

# ci
data.frame(
  lower = xbar_dif - sqrt(c2 * diag(Spooled) * (1/n1 + 1/n2)),
  upper = xbar_dif + sqrt(c2 * diag(Spooled) * (1/n1 + 1/n2))
)

#

# package
result <- Hotelling::hotelling.test(x1, x2)
result



# 2014/2015 No 2 ----------------------------------------------------------
s <- '11 24.8 12.1 45.2 9.6 39
9.6 37.1 12.2 39.4 10.7 50.6
13.7 37.4 12.6 40.8 9.6 40.9
9.6 33.3 11.2 30.1 11.8 28.5
13.6 40.4 13.1 31.9 8.8 44.3
11.6 40.9 13 35.6 8.4 51.6
13.3 44.3 8 43 9.6 41.3
15.2 44.5 14 63.3 9.1 54
7.8 41.2 11.4 48.4 11.6 53.7
11.1 41.1 11.8 31.5 10.2 40.6
NA NA NA NA 6.4 40.3
NA NA NA NA 13.1 34.8'

colname <- c('bb', 'p')
dat <- read_string(s) %>% 
    {rbind(
      (.)[1:2] %>% `colnames<-`(colname),
      (.)[3:4] %>% `colnames<-`(colname), 
      (.)[5:6] %>% `colnames<-`(colname) 
    )} %>% 
  mutate(
    merek = rep(c('m', 'h', 't'), each = 12)
  ) %>% 
  drop_na()
dat    
    

m <- manova(cbind(bb, p) ~ merek, dat)
summary(m)


# 2015/2016 No 1 ----------------------------------------------------------
xbar1 <- matrix(c(2.287, 12.600, 0.347, 10.587))
xbar2 <- matrix(c(2.40, 7.16, 0.52, 10.594))
S1 <- matrix(c(0.287,
               0.859,
               -0.087,
               0.892,
               1,
               3.839,
               -0.332,
               3.571,
               1,
               1,
               0.069,
               -0.511,
               1,
               1,
               1,
               13.689), ncol = 4)
S2 <- matrix(c( 
              0.418,
              -1.295,
              0.187,
              -0.492,
              1,
              10.351,
              -0.690,
              -1.813,
              1,
              1,
              0.097,
              -0.209,
              1,
              1,
              1,
              5.614), ncol = 4)
S1
S2

S1[upper.tri(S1)] <- t(S1)[upper.tri(S1)]
S2[upper.tri(S2)] <- t(S2)[upper.tri(S2)]
S1 == t(S1)
S2 == t(S2)

S1 <- S1[-4, -4]
S2 <- S2[-4, -4]

p <- 3
alpha <- 0.05
n1 <- 20
n2 <- 20
Spooled <- ((n1 - 1)*S1 + (n2 - 1)*S2)/(n1 + n2 - 2)
solve(Spooled)

xbar_dif <- matrix(colMeans(x1) - colMeans(x2))
xbar_dif <- (xbar1 -  xbar2)[-4]
T2 <- n1*n2/(n1 + n2)*t(xbar_dif) %*% solve(Spooled) %*% xbar_dif
T2
c2 <- (n1 + n2 - 2)*p*qf(alpha, p, n1 + n2 - p - 1, lower.tail = F)/(n1 + n2 - p - 1)
c2

# ci
data.frame(
  lower = xbar_dif - sqrt(c2 * diag(Spooled) * (1/n1 + 1/n2)),
  upper = xbar_dif + sqrt(c2 * diag(Spooled) * (1/n1 + 1/n2))
)
# karena x2 saja yang selang tidak melewati 0 maka
# x2 adalah karakteristik finansial yang paling dapat 
# menunjukkan perbedaan


# 2015/2016 No 2 ----------------------------------------------------------

s <- 'Aceh 44,92 46,57 46,12 Barat
Sumatera Utara 47,47 47,62 48,56 Barat
Sumatera Barat 48,02 49,37 49,71 Barat
Riau 36,83 36,89 35,56 Barat
Jambi 40,86 41,07 41,85 Barat
Sumatera Selatan 42,53 43,13 42,81 Barat
Bengkulu 38,68 39,28 40,29 Barat
Lampung 47,54 48,45 48,32 Barat
Bangka Belitung 27,21 28,71 28,01 Barat
Kepulauan Riau 31,46 31,6 34,63 Barat
DKI Jakarta 55,4 55,23 58,22 Barat
Jawa Barat 57,6 59,22 58,74 Barat
Jawa Tengah 56,13 54,47 57,7 Barat
DI Yogyakarta 56,02 55,89 61,88 Barat
Jawa Timur 59,29 54,89 61,74 Barat
Banten 50,39 49,11 51,45 Barat
Kalimantan Barat 57,11 30,9 58,09 Tengah
Kalimantan Tengah 47,41 28,49 49,69 Tengah
Bali 31,8 56,25 34,91 Tengah
Nusa Tenggara Barat 31,37 49,45 30,39 Tengah
Nusa Tenggara Timur 26,27 30,3 30,01 Tengah
Kalimantan Selatan 39,1 41,67 42,05 Tengah
Kalimantan Timur 39,25 39,41 39,42 Tengah
Sulawesi Utara 48,76 48,83 48,46 Tengah
Sulawesi Tengah 45,88 46,96 44,71 Tengah
Sulawesi Selatan 49,44 50,74 50,98 Tengah
Sulawesi Tenggara 42,19 41,34 41,47 Tengah
Gorontalo 55,2 51,87 48,01 Tengah
Sulawesi Barat 47,8 47,9 49,21 Tengah
Maluku 41,08 41,21 41,13 Timur
Maluku Utara 34,47 36,3 36,91 Timur
Papua Barat 36,19 35,38 39,03 Timur
Papua 38,45 39,45 37,16 Timur'

dat <- read_pattern(
  s, pos_non_angka = c(1, 5),
  pos_angka = c(2:4)
) %>% 
  `colnames<-`(c('prov', paste0('t', 2010:2012), 'wilayah')) %>% 
  mutate_at(
    2:4,
    ~ str_replace_all(.x, ',', '\\.')
  ) %>% 
  type_convert()

dat


# 2018/2019 No 1 ----------------------------------------------------------
xbar <- matrix(c(5.359273, 6.953631))
lambda <- c(14.267678, 6.390188)
e1 <- matrix(c(-0.1045187, 0.9945229))
e2 <- matrix(c(-0.9945229, -0.1045187))
e <- cbind(e1, e2)

# genetalized variance
S <- (e1 %*% t(e1))*lambda[1] + (e2 %*% t(e2))*lambda[2]
det(S)
# volume
sqrt((30 - 1)^2 * det(S))

c <- sqrt(qchisq(0.05, 2, lower.tail = F))
# Major (nilai eigen yg besar)
e1
xbar + c * sqrt(lambda[1]) * e[,1]
xbar - c * sqrt(lambda[1]) * e[,1]

# Minor 
xbar + c * sqrt(lambda[2]) * e[,2]
xbar - c * sqrt(lambda[2]) * e[,2]


MVQuickGraphs::confidenceEllipse(
  X.mean = xbar,
  eig = eigen(S),
  n = 30, p = 2,
  alpha = 0.05
)

major1 <- xbar + c * sqrt(lambda[1]) * e[,1]
major2 <- xbar - c * sqrt(lambda[1]) * e[,1]
minor1 <- xbar + c * sqrt(lambda[2]) * e[,2]
minor2 <- xbar - c * sqrt(lambda[2]) * e[,2]

point <- 
  cbind(major1, major2, minor1, minor2) %>% 
  t() %>% 
  as.data.frame() %>% 
  `colnames<-`(c('x', 'y'))



len <- function(x, y = c(0, 0)) sqrt(sum((x - y)^2))
major <- xbar + c * sqrt(lambda[1]) * e[,1]
minor <- xbar + c * sqrt(lambda[2]) * e[,2]
a <- len(xbar, major)
b <- len(xbar, minor)
angle <- acos(sum(xbar * minor) / (len(xbar) * len(minor)))
angle

# a jarak x dan b jarak y
ggplot() +
  ggforce::geom_ellipse(
    aes(x0 = xbar[1], y0 = xbar[2], a = a, b = b, angle = angle*2)
  ) +
  geom_point(
    data = point,
    aes(x = x, y = y)
  ) +
  geom_segment(
    aes(
      x = point[c(1, 3), 1], y = point[c(1, 3), 2], 
      xend = point[c(2, 4), 1], yend = point[c(2, 4), 2]
    ), 
  )


# 2018/2019 No 2 ----------------------------------------------------------
n1 <- 50
n2 <- 55
xbar1 <- matrix(c(3, 9, 5))
xbar2 <- matrix(c(4, 10, 4))
S1 <- matrix(c(3, 1.8, 1.5,
               1.8, 6, 2.1,
               1.5, 2.1, 4
               ), byrow = T, ncol = 3)
S2 <- matrix(c(2.5, 1.0, 1.2,
               1.0, 5.0, 2.0,
               1.2, 2.0, 3.5
              ), byrow = T, ncol = 3)

p <- ncol(S1)
alpha <- 0.05
Spooled <- ((n1 - 1)*S1 + (n2 - 1)*S2)/(n1 + n2 - 2)
Spooled
solve(Spooled)

xbar_dif <- matrix(xbar1 - xbar2)
T2 <- n1*n2/(n1 + n2)*t(xbar_dif) %*% solve(Spooled) %*% xbar_dif
T2
c2 <- (n1 + n2 - 2)*p*qf(alpha, p, n1 + n2 - p - 1, lower.tail = F)/(n1 + n2 - p - 1)
c2

# ci
data.frame(
  lower = xbar_dif - sqrt(c2 * diag(Spooled) * (1/n1 + 1/n2)),
  upper = xbar_dif + sqrt(c2 * diag(Spooled) * (1/n1 + 1/n2))
)


# Kedua variabel lainnya iatu pengeluaran/kapita non makanan dan
# luas lantai, tak bisa disimpulkan 
# siapa yang lebih besar antara pesisir atau pegunungan. 



# 2018/2019 No 3 ----------------------------------------------------------

s <- '1 Pola 1 3,20 3,10
2 Pola 1 3,40 3,40
3 Pola 1 3,40 3,30
4 Pola 1 3,30 3,50
5 Pola 2 3,60 3,70
6 Pola 2 3,50 3,40
7 Pola 2 3,70 3,60
8 Pola 2 3,80 3,70
9 Pola 3 2,80 2,90
10 Pola 3 3,10 3,00
11 Pola 3 3,00 3,10
12 Pola 3 3,00 2,90
'

dat <- read_string(s) %>% 
  dplyr::select(-(V1:V2)) %>% 
  `colnames<-`(c('pola', 'ip5', 'ip6')) %>% 
  mutate(
    across(ip5:ip6, ~ str_replace_all(.x, ',', '\\.'))
  ) %>% 
  type_convert()


# 2019/2020 No 1 ----------------------------------------------------------

# offline online
xbar <- matrix(c(4.64, 9.965))
miu <- matrix(c(4, 10))
lambda <- c(5.100851, 1.406176)
e <- matrix(c(-0.6314539, -0.7754134, 0.7754134, -0.6314539), ncol = 2)
alpha <- 0.1
n <- 20
p <- nrow(xbar)

S <- lapply(1:length(lambda), function(x) lambda[x] * e[,x] %*% t(e[,x])) %>% 
  Reduce("+", .)

S


T2 <- n * t(xbar - miu) %*% solve(S) %*% (xbar - miu)
T2
c2 <- (n - 1) * p * qf(alpha, p, n - p, lower.tail = F) / (n - p)
c2

s.ci(xbar, S, n, alpha = 0.1)

(n - 1) * p * f_tabel / (n - p)

MVQuickGraphs::confidenceEllipse(
  X.mean = xbar,
  eig = eigen(S),
  n = 20, p = 2,
  alpha = 0.1
)


c <- sqrt(qchisq(0.05, 2, lower.tail = F))
c <- sqrt(c2)
# Major (nilai eigen yg besar)
xbar + c * sqrt(lambda[1]) * e[,1]
xbar - c * sqrt(lambda[1]) * e[,1]

# Minor 
xbar + c * sqrt(lambda[2]) * e[,2]
xbar - c * sqrt(lambda[2]) * e[,2]


# 2019/2020 No 2 ----------------------------------------------------------

s <- '1 0.750 0.727 0.588
0.750 1 0.629 0.05
0.727 0.629 1 0.684
0.588 0.05 0.684 1'

R <- dm::read_string(s) %>% 
  as.matrix()
# diagonal sqrt(S)
V12 <- diag(c(0.251, 0.149, 0.254, 0.141))
S <- V12 %*% R %*% V12
xbar <- matrix(c(3.23, 3.22, 3.12, 3.11))
C <- matrix(c(1, 1, -1, -1,
              1, -1, 1, -1,
              1, -1, -1, 1), byrow = T, nrow = 3)
C
# Tidal terdapat interaksi antara pemberian uang tunjangan belajar
# dengan pemberian fasilitas asrama dalam memengaruhi IPK
repeated_md(xbar, S, C, n = 100)


# 2019/2020 No 3 ----------------------------------------------------------

R1 <- '1.00 -0.65 0.34
-0.65 1.00 0.42
0.34 0.42 1.00'

R1 <- read_string(R1) %>% as.matrix()
R2 <- read_string('1.00 -0.65 0.34
                  -0.65 1.00 0.42
                  0.34 0.42 1.00') %>% as.matrix()
R3 <- read_string('1.00 -0.59 0.43
-0.59 1.00 0.46
0.43 0.46 1.00') %>% as.matrix()

R4 <- read_string('1.00 -0.55 0.42
-0.55 1.00 0.45
0.42 0.45 1.00') %>% as.matrix()

R5 <- read_string('1.00 -0.66 0.38
-0.66 1.00 0.41
0.38 0.41 1.00') %>% as.matrix()

s <- '1. Pertanian 21650 39 93640 1250 14 12740
2. Pertambangan 20130 43 89560 1190 11 13100
3. Industri dan manufaktur 22170 41 95560 1310 12 12100
4. Jasa 16250 35 96640 1210 13 12640
5. Perdagangan 19860 44 92620 1336 15 1229'


dat <- dm::read_pattern(
  s, pos_angka = c(1, 3:8),
  pos_non_angka = 2
)

n <- c(15, 17, 18, 20, 20)

dat[,3:5]

dat[,6:8] %>% 
  split(1:nrow(.)) %>% 
  lapply(function(x) diag(x) %*% R %*% diag(x))


# 2019/2020 No 4 ----------------------------------------------------------

s <- '1 71 0.17 0.00 0.45 31 73 0.14 0.06 0.20
2 71 0.49 0.02 1.25 32 73 0.09 0.03 0.13
3 71 0.25 0.01 0.50 33 73 0.27 0.04 0.50
4 71 0.05 0.01 0.00 34 73 0.69 0.02 2.00
5 71 0.04 0.00 0.00 35 73 0.07 0.02 0.10
6 71 0.06 0.01 0.09 36 73 0.20 0.01 0.50
7 71 0.17 0.01 0.40 37 74 0.35 0.06 0.56
8 71 0.12 0.00 0.17 38 74 0.49 0.07 1.25
9 71 0.11 0.03 0.14 39 74 0.71 0.08 2.00
10 71 0.10 0.01 0.14 40 74 1.03 0.27 3.50
11 71 0.13 0.01 0.17 41 74 0.06 0.02 0.04
12 71 0.37 0.02 0.64 42 74 0.74 0.00 2.13
13 71 0.24 0.01 0.05 43 74 1.67 0.02 6.00
14 71 0.23 0.00 0.05 44 74 0.56 0.01 1.50
15 71 0.29 0.01 0.05 45 74 0.13 0.01 0.20
16 71 0.10 0.01 0.14 46 74 0.40 0.00 0.75
17 72 0.60 0.24 1.57 47 75 0.08 0.01 0.10
18 72 0.15 0.08 0.33 48 75 3.89 0.07 6.00
19 72 0.47 0.06 1.00 49 75 0.31 0.05 0.56
20 72 0.55 0.18 1.44 50 75 0.61 0.04 2.00
21 72 0.27 0.07 0.50 51 75 0.41 0.03 0.75
22 72 0.18 0.11 0.50 52 75 0.18 0.09 0.50
23 72 0.04 0.08 0.00 53 75 0.09 5.00 0.14
24 72 0.05 0.24 0.00 54 75 0.38 0.02 0.67
25 73 0.13 0.03 0.17 55 75 0.55 0.02 1.33
26 73 0.88 0.02 2.75 56 75 0.47 0.04 1.00
27 73 0.16 0.05 0.39 57 75 1.42 0.01 4.33
28 73 0.11 0.02 0.17 58 75 0.37 0.00 0.57
29 73 0.79 0.08 2.45 59 75 0.44 0.00 1.00
30 73 0.43 0.03 1.00 NA NA NA NA NA
'


colname <- c('no', 'kode', 'x1', 'x2', 'x3')

dat <- read.table(textConnection(s), header = F) %>% 
  {rbind(
    (.)[1:length(colname)] %>% `colnames<-`(colname),
    (.)[1:length(colname) + length(colname)] %>% `colnames<-`(colname) 
  )} %>% 
  drop_na()

formula1 <- cbind(x1, x2, x3) ~ kode
formula1


dat <- dat %>% 
  select(all.vars(formula1)) %>% 
  drop_na()

tret <- all.vars(rlang::f_rhs(formula1))

x <- dat %>% 
  dplyr::select(-tret) %>% 
  as.matrix()

xbar_l <- dat %>% 
  group_by_at(tret) %>% 
  arrange_at(tret) %>% 
  mutate_at(vars(-tret), mean, na.rm = T) %>% 
  ungroup() %>% 
  dplyr::select(-tret) %>% 
  as.matrix()

xbar <- dat %>% 
  mutate_at(vars(-tret), mean, na.rm = T) %>% 
  dplyr::select(-tret) %>% 
  as.matrix()


alpha <- 0.05
p <- ncol(x)
g <- length(unique(pull(dat[tret])))
n <- nrow(x)
tau <- xbar_l - xbar 
e <- x - xbar_l

SSt <- B <-  t(tau) %*% tau
SSres <- W <- t(e) %*% e
lambda <- det(W) /  det(B + W)

# P=2 g>=2
Fhitung <- ((n - g - 1) / (g - 1)) * ((1 - sqrt(lambda))/sqrt(lambda))
Ftabel <- qf(alpha, 2*(g - 1), 2*(n - g - 1), lower.tail = F)

# p>=1 g=3
Fhitung <- ((n - p - 2) / p) * ((1 - sqrt(lambda))/sqrt(lambda))
Ftabel <- qf(alpha, 2*p, 2*(n - p - 2), lower.tail = F)

# kasus selain itu dan n besar pendekatan 
-(n - 1 - (p + g) / 2) * log(lambda)
qchisq(alpha, p*(g - 1), lower.tail = F)


Fhitung
Ftabel

ifelse(Fhitung > Ftabel, 'Tolak Ho', 'Gagal Tolak Ho')

m <- manova(formula1, dat)
summary(m, test = 'Wilks')
summary.aov(m)



# 2020/2021 No 1 ----------------------------------------------------------

s <- '1 10,23 32,29 69,96
2 3,41 10,31 78,62
3 24,67 0,00 80,60
4 1,85 10,84 74,76
5 2,09 19,64 77,77
6 2,46 16,93 84,06
7 1,10 23,88 80,55
8 2,82 7,72 86,46
9 5,88 12,07 68,51
10 2,07 23,17 82,73
11 3,98 17,78 80,03
12 2,15 33,05 86,78
13 1,53 11,46 75,85
'

dat <- dm::read_string(s) %>% 
  mutate_all(~str_replace_all(.x, ',', '.')) %>% 
  type_convert() %>% 
  dplyr::select(-1)

xbar <- matrix(colMeans(dat))
miu <- matrix(c(3.56, 19.75, 75.67))
S <- var(dat)
alpha <- 0.05
n <- nrow(dat)
p <- nrow(xbar)
n
p

T2 <- n * t(xbar - miu) %*% solve(S) %*% (xbar - miu)
T2

c2 <- (n - 1) * p * qf(alpha, p, n - p, lower.tail = F) / (n - p)
c2


# 2020/2021 No 2 ----------------------------------------------------------

s1 <- 'A 14,11 2,86 71,52 A 16,80 4,53 71,67
B 10,08 3,47 70,29 B 14,30 3,8 70,52
C 13,90 3,19 70,65 C 15,40 3,31 70,93
D 14,99 5,36 72,9 D 14,00 6,11 73,12
E 13,17 4,66 72,62 E 11,90 4,66 72,80
F 9,29 3,60 71,16 F 12,30 4,05 71,40
G 7,39 4,92 70,49 G 10,40 5,65 70,76
H 12,36 4,03 71,9 H 12,80 5,27 72,14
I 11,23 3,76 71,88 I 13,60 3,99 72,11
J 14,00 3,10 71,39 J 9,80 3,46 71,56
K 11,30 3,08 74,41 K 12,00 3,2 74,56'




colname <- c('prov', 'kgizi', 'p_kes', 'ahh')

dat <- read.table(textConnection(s1), header = F) %>% 
  {rbind(
    (.)[1:length(colname)] %>% `colnames<-`(colname),
    (.)[1:length(colname) + length(colname)] %>% `colnames<-`(colname) 
  )} %>% 
  mutate(periode = rep(1:2, each = 11)) %>% 
  mutate_at(2:4, ~str_replace_all(.x, ',', '.')) %>% 
  type_convert()

dat

x1 <- dat[1:11, -c(1, 5)]
x2 <- dat[12:22, -c(1, 5)]

x1
x2


paired_test(x2, x1)


# 2020/2021 No 3 ----------------------------------------------------------

s2 <- 'MK1 75.7 82.8 67.5 72.3
MK2 77 83.7 67.3 74.9
MK3 71.3 77.1 70.1 76
MK4 69.3 78.9 75.9 79.8
MK5 73.6 84.1 72.1 80.2
MK6 73.2 81.6 66.2 69.2
MK7 74 82.3 69.8 73
MK8 77.6 86 72.2 76.3
MK9 73 80.2 62.7 73.8
MK10 77.7 90.2 74.2 78.9
MK11 74 82.8 64.2 76.3
MK12 73 78.7 72.3 74.5
MK13 77.6 85.6 70.1 72.8
MK14 75 82 67.3 75.6
MK15 82.3 88.1 69.2 71.5
MK16 70.3 75.9 70.4 75.6
MK17 71.9 83.4 73.2 76.3
MK18 78.1 86.5 73.2 77.7
MK19 78.3 86.6 67.9 75
MK20 73.4 84.4 71.9 80.2'

dat <- read_string(s2) 
x1 <- dat[,2:3]
x2 <- dat[,4:5]
x1
x2

p <- ncol(x1)
alpha <- 0.05
S1 <- var(x1)
S2 <- var(x2)
n1 <- nrow(x1)
n2 <- nrow(x2)
Spooled <- ((n1 - 1)*S1 + (n2 - 1)*S2)/(n1 + n2 - 2)
solve(Spooled)

xbar_dif <- matrix(colMeans(x1) - colMeans(x2))
T2 <- n1*n2/(n1 + n2)*t(xbar_dif) %*% solve(Spooled) %*% xbar_dif
T2
c2 <- (n1 + n2 - 2)*p*qf(alpha, p, n1 + n2 - p - 1, lower.tail = F)/(n1 + n2 - p - 1)
c2

# ci
data.frame(
  lower = xbar_dif - sqrt(c2 * diag(Spooled) * (1/n1 + 1/n2)),
  upper = xbar_dif + sqrt(c2 * diag(Spooled) * (1/n1 + 1/n2))
)

#

# package
result <- Hotelling::hotelling.test(x1, x2)
result


# 2020/2021 No 4 ----------------------------------------------------------

s <- '1 60 79 74 66 68 76
2 42 80 74 74 83 74
3 56 89 84 69 89 81
4 49 80 86 79 NA NA
5 69 84 NA NA NA NA'

dat <- read_string(s)
x1 <- dat[,2:3] %>% drop_na()
x2 <- dat[,2:3 + 2] %>% drop_na()
x3 <- dat[,2:3 + 4] %>% drop_na()

x1
x2
x3

det(var(x1))
det(var(x2))
det(var(x3))



dat <- lapply(
  list(x1, x2, x3), 
  function(x) `colnames<-`(x, c('apg', 'anareg'))
) %>% 
  bind_rows() %>% 
  mutate(
    kelas = rep(letters[1:3], c(nrow(x1), nrow(x2), nrow(x3)))
  )
  # plyr::rbind.fill()



formula1 <- cbind(apg, anareg) ~ kelas
formula1


dat <- dat %>% 
  select(all.vars(formula1)) %>% 
  drop_na()

tret <- all.vars(rlang::f_rhs(formula1))

x <- dat %>% 
  dplyr::select(-tret) %>% 
  as.matrix()

xbar_l <- dat %>% 
  group_by_at(tret) %>% 
  arrange_at(tret) %>% 
  mutate_at(vars(-tret), mean, na.rm = T) %>% 
  ungroup() %>% 
  dplyr::select(-tret) %>% 
  as.matrix()

xbar <- dat %>% 
  mutate_at(vars(-tret), mean, na.rm = T) %>% 
  dplyr::select(-tret) %>% 
  as.matrix()


alpha <- 0.05
p <- ncol(x)
g <- length(unique(pull(dat[tret])))
n <- nrow(x)
tau <- xbar_l - xbar 
e <- x - xbar_l

SSt <- B <-  t(tau) %*% tau
SSres <- W <- t(e) %*% e
lambda <- det(W) /  det(B + W)

# P=2 g>=2
Fhitung <- ((n - g - 1) / (g - 1)) * ((1 - sqrt(lambda))/sqrt(lambda))
Ftabel <- qf(alpha, 2*(g - 1), 2*(n - g - 1), lower.tail = F)

# p>=1 g=3
Fhitung <- ((n - p - 2) / p) * ((1 - sqrt(lambda))/sqrt(lambda))
Ftabel <- qf(alpha, 2*p, 2*(n - p - 2), lower.tail = F)

Fhitung
Ftabel


# karena tolak Ho
ttabel <- qt(alpha/(p*g*(g - 1)), n -  g, lower.tail = F)
index_i = c(1:2, 1)
index_j = c(2, 3, 3)
ni <- table(pull(dat[tret])) %>% as.vector()
# bonferoni





kat <- 1
data.frame(
  i = index_i, 
  j = index_j,
  n1 = ni[index_i],
  n2 = ni[index_j],
  xbar1 = unique(xbar_l)[index_i, kat],
  xbar2 = unique(xbar_l)[index_j, kat]
) %>% 
  mutate(
    xbar_diff = xbar1 - xbar2,
    lower = xbar_diff - ttabel * sqrt(W[kat, kat] * (1/n1 + 1/n2)/(n - g)),
    upper = xbar_diff + ttabel * sqrt(W[kat, kat] * (1/n1 + 1/n2)/(n - g))
  )



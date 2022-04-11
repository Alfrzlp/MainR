library(dm)

# No 1 --------------------------------------------------------------------
n <- 12
p <- 3

s <- '13.957 13.719
10.540 10.432
17.063 16.893'

miu <- (read_string(s)$V1 - read_string(s)$V2)*1000
miu
xbar <- matrix(c(317.5, 211.25, 368.9167))
R <- matrix(c(1,  0.1778,  -0.6976,
              0.1778, 1, 0.1046,
              -0.6976, 0.1046, 1), ncol = 3, byrow = T)
R
V12 <- diag(sqrt(c(6947.545, 13119.659, 10451.356)))
V12

S <- V12 %*% R %*% V12
S


T2 <- n * t(xbar - miu) %*% solve(S) %*% (xbar - miu)
T2

c2 <- (n - 1) * p * qf(0.05, p, n - p, lower.tail = F) / (n - p)
c2


s.ci(xbar, S, n = n)

# No 2 --------------------------------------------------------------------

n <- 20
p <- 3
alpha <- 0.05
dbar <- matrix(c(-44.05, 98.35, 41.65))
s1 <- '1748,89 218,91 718,67
218,91 321,92 345,23
718,67 345,23 1216,77'

Sd <- read_string(s1) %>%
  mutate_all(~str_replace_all(.x, ',', '.')) %>% 
  type_convert() %>% 
  as.matrix()


T2 <- n * t(dbar) %*% solve(Sd) %*% dbar
T2
ftabel <- qf(alpha, p, n - p, lower.tail = F)
c2 <- (n - 1) * p * ftabel / (n - p)
c2
ttabel <- qt(1 - alpha / (2 * p), n - 1)


ci <- data.frame(
  lower = dbar - sqrt(c2 * diag(Sd) / n),
  upper = dbar + sqrt(c2 * diag(Sd) / n),
  bon.lower = dbar - ttabel * sqrt(diag(Sd) / n),
  bon.upper = dbar + ttabel * sqrt(diag(Sd) / n)
)
ci


# No 3 --------------------------------------------------------------------
p <- 3
alpha <- 0.05

s3 <- 'Metstat 1 0.82 0.06 0 0 0
Statmat 0.82 1 -0.02 0 0 0
MPC 0.06 -0.02 1 0 0 0
Metstat 0 0 0 1 0.74 0.43
Statmat 0 0 0 0.74 1 0.24
MPC 0 0 0 0.43 0.24 1'

Rl <- read_string(s3)[1:3, 2:4] %>% as.matrix()
Rd <- read_string(s3)[4:6, 5:7] %>% as.matrix()
S1 <- diag(c(0.176, 0.170,0.247)) %*% Rl %*% diag(c(0.176, 0.170,0.247)) 
S2 <- diag(c(0.157, 0.157, 0.213)) %*% Rd %*% diag(c(0.157, 0.157, 0.213)) 


n1 <- 30
n2 <- 20
Spooled <- ((n1 - 1)*S1 + (n2 - 1)*S2)/(n1 + n2 - 2)
solve(Spooled)

xbar_dif <- matrix(c(80.05 - 81.05, 85.06 - 83.03, 89.99 - 89.96 ))
T2 <- n1*n2/(n1 + n2)*t(xbar_dif) %*% solve(Spooled) %*% xbar_dif
T2
c2 <- (n1 + n2 - 2)*p*qf(alpha, p, n1 + n2 - p - 1, lower.tail = F)/(n1 + n2 - p - 1)
c2

# ci
data.frame(
  lower = xbar_dif - sqrt(c2 * diag(Spooled) * (1/n1 + 1/n2)),
  upper = xbar_dif + sqrt(c2 * diag(Spooled) * (1/n1 + 1/n2))
)


# No 4 --------------------------------------------------------------------

s <- 'XA 60.81 30.04 29.34 44.13 14.03 23.68
Xb 30.04 78.45 53.04 14.03 46.09 24.26
Xc 29.34 53.04 60.56 23.68 24.26 31.89'
s1 <- 'xa 58.44 22.89 39.17 82.55 63.42 41.18
Xb 22.89 78.35 54.81 63.42 109.80 81.91
Xc 39.17 54.81 66.44 41.18 81.91 85.53'
S1 <- read_string(s)[5:7]
S2 <- read_string(s1)[2:4]
S3 <- read_string(s1)[5:7]

xbar <- matrix(c(73.93, 89.67, 86.94))
xbar1 <- matrix(c(76.04, 90.81, 85.91))
xbar2 <- matrix(c(71.64, 91.88, 89.72) )
xbar3 <- matrix(c(73.91, 86.22, 85.28))
xbar_l <- rbind(t(xbar1), t(xbar2), t(xbar3))

W <- (12 - 1) * S1 + (11 - 1) * S2 + (11 - 1) * S3
W <- as.matrix(W)

B <- 12 * (xbar1-xbar) %*% t(xbar1-xbar) + 11 * (xbar2-xbar) %*% t(xbar2-xbar) + 11 * (xbar3-xbar) %*% t(xbar3-xbar)
B <- as.matrix(B)

p <- 3
g <- 3

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


ttabel <- qt(alpha/(p*g*(g - 1)), n -  g, lower.tail = F)
index_i = c(1:2, 1)
index_j = c(3, 3, 2)
ni <- c(12, 11, 11)
# bonferoni
kat <- 3
data.frame(
  i = index_i, j = index_j,
  n1 = c(12, 11, 12),
  n2 = c(11, 11, 11),
  xbar1 = xbar_l[index_i,  kat],
  xbar2 = xbar_l[index_j, kat]
) %>% 
  mutate(
    xbar_diff = xbar1 - xbar2,
    lower = xbar_diff - ttabel * sqrt(W[kat, kat] * (1/n1 + 1/n2)/(n - g)),
    upper = xbar_diff + ttabel * sqrt(W[kat, kat] * (1/n1 + 1/n2)/(n - g))
  )
-=
  
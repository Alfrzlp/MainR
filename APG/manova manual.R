
# Independent -------------------------------------------------------------
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


# Univariate Anova --------------------------------------------------------
x1 <- c(9, 6, 9)
x2 <- c(0, 2)
x3 <- c(3, 1, 2)

n1 <- length(x1)
n2 <- length(x2)
n3 <- length(x3)
g <- 3

n <- n1 + n2 + n3
x <- c(x1, x2, x3)
xbar <- rep(mean(x), n)
xbar1 <- matrix(mean(x1), nrow = n1)
xbar2 <- matrix(mean(x2), nrow = n2)
xbar3 <- matrix(mean(x3), nrow = n3)
xbar_l <- rbind(xbar1, xbar2, xbar3)

alpha <- 0.01
tau <- xbar_l - xbar 
e <- x - xbar_l

SSt <- t(tau) %*% tau
SSres <- t(e) %*% e

Fhitung <- (SSt/(g - 1))/(SSres/(n - g))
Ftabel <- qf(alpha, g - 1, n - g, lower.tail = F)

Fhitung
Ftabel


# Multivariate Anova ------------------------------------------------------
x1 <- matrix(c(9, 6, 9,
               3, 2, 7), ncol = 2)
x1
x2 <- matrix(c(0, 2,
               4, 0), ncol = 2)
x2
x3 <- matrix(c(3, 1, 2, 
               8, 9, 7), ncol = 2)
x3


x <- rbind(x1, x2, x3)
xbar <- matrix(colMeans(x), ncol = ncol(x), nrow = nrow(x), byrow = T)
xbar1 <- matrix(colMeans(x1), ncol = ncol(x), nrow = nrow(x1), byrow = T)
xbar2 <- matrix(colMeans(x2), ncol = ncol(x), nrow = nrow(x2), byrow = T)
xbar3 <- matrix(colMeans(x3), ncol = ncol(x), nrow = nrow(x3), byrow = T)
xbar_l <- rbind(xbar1, xbar2, xbar3)

x



alpha <- 0.01
tau <- xbar_l - xbar 
e <- x - xbar_l

SSt <- B <-  t(tau) %*% tau
SSres <- W <- t(e) %*% e
lambda <- det(W) /  det(B + W)

# P=2 g>=2
Fhitung <- ((n - g - 1) / (g - 1)) * ((1 - sqrt(lambda))/sqrt(lambda))
Ftabel <- qf(alpha, 2*(g - 1), 2*(n - g - 1), lower.tail = F)

Fhitung
Ftabel




manova(x1, x2, x3)

x <- x %>% 
  as.data.frame() %>% 
  mutate(
    tret = rep(c('x1', 'x2', 'x3'), c(3, 2, 3))
  ) 
x
manova(cbind(V1, V2) ~ tret, x) %>% 
  summary()




formula1 <- cbind(bb, p) ~ merek
formula1

rlang::f_lhs(formula1)
rlang::f_rhs(formula1)

formula1[[1]] <- quote(cor)
formula1
eval(formula1, dat)

dat[as.character(rlang::f_lhs(formula1))[-1]]




# Manual complete ---------------------------------------------------------
formula1 <- cbind(ip5, ip6) ~ pola
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

# Jika tolak Ho uji satu2
m <- manova(formula1, dat)
summary.aov(m)


ttabel <- qt(alpha/(p*g*(g - 1)), n -  g, lower.tail = F)
index_i = c(1:2, 1)
index_j = c(3, 3, 2)
ni <- table(pull(dat[tret]))
# bonferoni
data.frame(
  i = index_i, j = index_j,
  n1 = ni[as.character(index_i)][[1]],
  n2 = ni[as.character(index_j)][[1]],
  xbar1 = unique(xbar_l)[index_i, 1],
  xbar2 = unique(xbar_l)[index_j,1]
) %>% 
  mutate(
    xbar_diff = xbar1 - xbar2,
    lower = xbar_diff - ttabel * sqrt(W[1, 1] * (1/n1 + 1/n2)/(n - g)),
    upper = xbar_diff + ttabel * sqrt(W[1, 1] * (1/n1 + 1/n2)/(n - g))
  )


data.frame(
  i = index_i, j = index_j,
  n1 = ni[as.character(index_i)][[1]],
  n2 = ni[as.character(index_j)][[1]],
  xbar1 = unique(xbar_l)[index_i, 2],
  xbar2 = unique(xbar_l)[index_j, 2]
) %>% 
  mutate(
    xbar_diff = xbar1 - xbar2,
    lower = xbar_diff - ttabel * sqrt(W[2, 2] * (1/n1 + 1/n2)/(n - g)),
    upper = xbar_diff + ttabel * sqrt(W[2, 2] * (1/n1 + 1/n2)/(n - g))
  )

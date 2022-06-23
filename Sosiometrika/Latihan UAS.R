library(dm)

# 20/21 No 2 --------------------------------------------------------------
s <- '1 30 70 12 15
2 32 78 14 20
3 45 56 10 23
4 24 45 10 30
5 46 68 12 22
6 32 67 14 18
7 33 54 12 25
8 35 50 16 15
9 20 45 12 16
10 41 70 16 20'

dat <- read_string(s) %>% 
  select(-1) %>% 
  setNames(paste0('x', 1:4)) %>% 
  scale() %>% 
  as.data.frame()

dat

m1 <- lm(x4 ~ x1 + x2 + x3, data = dat)
m2 <- lm(x3 ~ x1 + x2, data = dat)

m1 %>% coef() %>% round(3)
m2 %>% coef() %>% round(3)

m1 %>% summary()
m2 %>% summary()


R <- cor(dat)
R

R[1:2, 1:2]
R[3, 1:2]

solve(R[1:2, 1:2]) %*% R[3, 1:2]


R[1:3, 1:3]
R[4, 1:3]

solve(R[1:3, 1:3]) %*% R[4, 1:3]


# 19/20 No 1 --------------------------------------------------------------

s <- '1 30 70 6 15
2 32 78 7 20
3 45 56 5 23
4 24 45 5 30
5 46 68 6 22
6 32 67 7 18
7 33 54 6 25
8 35 50 8 15
9 20 45 6 16
10 41 70 8 20'

dat <- dm::read_string(s) %>% 
  select(-1) %>% 
  setNames(paste0('x', 1:4)) %>% 
  scale() %>% 
  as.data.frame()

dat

m1 <- lm(x4 ~ x1 + x2 + x3, data = dat)
m2 <- lm(x3 ~ x1 + x2, data = dat)

m1 %>% coef() %>% round(3)
m2 %>% coef() %>% round(3)

m1 %>% summary()
m2 %>% summary()


R <- cor(dat)
R

solve(R[1:2, 1:2]) %*% R[3, 1:2]
solve(R[1:3, 1:3]) %*% R[4, 1:3]

# 19/20 No 3 --------------------------------------------------------------
s <- '8 2 2 1
2 1 1 2
3 1 2 2
5 2 2 2
2 1 1 2
4 2 1 2
2 1 1 1
6 2 2 2
5 2 1 1
7 2 2 1
3 1 1 1
2 1 1 2
6 2 2 1
4 2 1 1
3 2 1 2'

dat <- read_string(s, col_names = c('y', paste0('x', 1:3)))

mod <- aov(y ~ ., data = dat) 
mod %>% summary()


r <- yardstick::tidy(mod)

nr <- nrow(r)
SSmod <- sum(r$sumsq[-nr])
SStot <- sum(r$sumsq)
dfMod <- sum(r$df[-nr])
MSmod <- SSmod / dfMod
MSres <- r$meansq[nr]

r %>% 
  mutate(term = str_remove_all(term, 'factor|\\)|\\(')) %>% 
  bind_rows(
    data.frame(
      term = c('Model', 'Total', '(Combined)'),
      df = c(dfMod, sum(r$df), dfMod),
      sumsq = c(SSmod, SStot, SSmod)
    ) %>% 
      mutate(
        meansq = sumsq / df,
        statistic = ifelse(term == 'Model', MSmod / MSres, NA),
        p.value = ifelse(term == 'Model', pf(statistic, dfMod, r$df[nr], lower.tail = F), NA)
      )
  ) %>% 
  slice(
    c(nr + 3, 1:(nr - 1), nr + 1, nr, nr + 2)
  )
# pvalue model adalah nilai uji simultan


# eta
p <- ncol(dat) - 1
sqrt(r$sumsq[1:p] / SStot)
# Untuk nilai eta, memberikan informasi keeratan hubungan 
# variabel kepadatan penduduk  terhadap IPM sebelum dikontrol 
# oleh variabel bebas lain

# R2 adj
R2 <- SSmod / SStot
R2
sqrt(R2)


dat %>% 
  pivot_longer(-y, names_to = 'variabel') %>% 
  group_by(variabel, value) %>% 
  summarise(
    n = n(),
    unadjusted = mean(y)
  ) %>% 
  mutate(
    deviasi = unadjusted - mean(dat$y)
  )

# deviasi
# provinsi yang mencapai kepadatan penduduk pada 
# kategori pertama menurunkan angka IPM sebesar 1,67509%

7/3
16/3

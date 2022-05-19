library(dplyr)

dat <- 
  foreign::read.spss(
    'D:/__SEMESTER 6/Sosiometrika/Bahan/tugas 12.sav',
    to.data.frame = T
  ) %>% 
  janitor::clean_names()

head(dat)
glimpse(dat)

model <- aov(pendapatan ~ ., dat)
model
summary(model)


glm(jenis_kelamin  ~ ., dat[-5], family = 'binomial')


option(pillar.signif = 4)
r <- yardstick::tidy(model)

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
   c(1:(nr - 1), nr + 1, nr, nr + 2)
  )






(sum(r$sumsq[1:4]) / 5) / r$meansq[5]



TotalSS <- sum(r$sumsq[1:nr])
E <- sum(r$sumsq[1:(nr - 1)])
C <- sum(r$df[-nr] + 1)
P <- ncol(dat) - 1
N <- nrow(dat)

# uji simultan
(E / (C - P))/((TotalSS - E)/(N + P - C - 1))
MSmod / MSres

pf(MSmod / MSres, dfMod, df.residual(model), lower.tail = F)






# eta
p <- ncol(dat) - 1
sqrt(r$sumsq[1:p] / SStot)


# R2 adj
R2 <- SSmod / SStot
R2

sqrt(SSmod / SStot)
# Hubungan produktivitas dan motivasi terhadap prestasi sebesar 47,1


dat %>% 
  pivot_longer(-pendapatan, names_to = 'variabel') %>% 
  group_by(variabel, value) %>% 
  summarise(
    n = n(),
    unadjusted = mean(pendapatan)
  ) %>% 
  group_by(variabel) %>% 
  mutate(
    n_kat = n(),
    # adj = 1 - (1 - unadjusted^2) * (n - 1) / (n + P - n_kat - 1),
    adj = unadjusted * (1 - 0.20756232),
    adj = sqrt(adj)
  ) %>% 
  as.data.frame()
  mutate(
    deviasi = unadjusted - mean(dat$pendapatan)
  )

2346283 - SStot -  (1/233) * sd()


dat %>% 
  pivot_longer(-pendapatan, names_to = 'variabel') %>% 
  group_by(variabel) %>% 
  summarise(sd(pendapatan))




(N - 1)/(N + P - C - 1)
2340548.45/ 2346283.26
1571509.1 / 1562229.86
2084315.17 / 2121693.35
1852483.69 / 1658852.46




2340548.45 + 1571509.1
2346283.26 + 1562229.86



45.8090 + 5.53978

51.34878 - 45.6406 





# Manual ------------------------------------------------------------------
s <- '1 1 1 3
2 1 1 2
3 3 2 6
4 1 1 1
5 1 1 5
6 1 2 8
7 2 2 0
8 2 1 9
9 2 1 13
10 2 1 6
11 3 1 8
12 2 1 1
13 1 1 9
14 3 1 7
15 2 2 0
16 1 2 0
17 3 1 6
18 1 2 6
19 1 2 2
20 2 1 9
21 3 1 4
22 2 2 5
23 3 2 3
24 2 2 1
25 3 2 3
26 2 2 3
27 3 2 5
28 3 1 7
29 3 2 3
30 1 2 3'

dat <- dm::read_string(s) %>% 
  select(-1) %>% 
  setNames(c('motivasi', 'produktivitas', 'prestasi')) %>% 
  type_convert()  

head(dat)
glimpse(dat)

aov(prestasi ~  produktivitas / motivasi, data = dat) %>% 
  summary()
aov(prestasi ~  motivasi / produktivitas, data = dat) %>% 
  summary()


sum((as.matrix(dat[, 1:2]) - mean(as.matrix(dat[, 1:2])))^2)

Ybar <- mean(dat$prestasi)
# T
SStot <- sum((dat$prestasi - Ybar)^2)
var(dat$prestasi) * (nrow(dat) - 1)

# U1
U1 <- dat %>% 
  group_by(produktivitas) %>% 
  summarise(n = n(), U = n*(mean(prestasi) -  Ybar)^2) %>% 
  pull(U) %>% 
  sum()

# U2
U2 <- dat %>% 
  group_by(motivasi) %>% 
  summarise(n = n(), U = n*(mean(prestasi) -  Ybar)^2) %>% 
  pull(U) %>% 
  sum()

E <- Tsum - U1 - U2
E
C <- sum(c(length(unique(dat$motivasi)), length(unique(dat$produktivitas))))
P <- ncol(dat) - 1
N <- nrow(dat)

(E / (C - P))/((Tsum - E)/(N + P - C - 1))

SSrow <- U1
SScol <- U2
SScombined <- SSmodel <- SSrow + SScol
SSres <- SStot - SSmodel



data.frame(
  x = c('Main Effect', '', '', 'Model', 'Residual', 'Total'),
  y = c('(Combined)', colnames(dat)[-3], '', '', ''),
  SumSq = c(SScombined, SScol, SSrow, SSmodel, SSres, SStot),
  df = c()
)

deparse(substitute())



model <- aov(prestasi ~ factor(motivasi) + factor(produktivitas) , dat) 




dat %>% 
  arrange(produktivitas) %>% 
  pivot_wider(names_from = motivasi, values_from = prestasi)



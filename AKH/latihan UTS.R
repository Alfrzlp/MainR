
# 2017/2018 ---------------------------------------------------------------
# No 2 --------------------------------------------------------------------
slta <- c(3, 1, 2, 1, 1, 4, 1, 1, 5, 2, 3, 3, 1, 2)
slta <- rep(rep(c("smk", "sma"), length = length(slta)), slta)
wt <- c(4, 7, 8, 29, 29, 31, 40, 65, 69, 78, 79, 106, 107, 129, 130, 140, 142, 149, 158, 160, 161, 162, 187, 188, 197, 204, 208, 221, 228, 231)
status <- c(1, 1, 2, 1, 1, 3, 2, 1, 1, 1, 3, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1)
status <- rep(rep(c("menganggur", "bekerja"), length = length(status)), status)

x <- data.frame(slta, waktu = wt, status)
head(x)

# 0 sensor
x <- x %>%
  mutate(
    status = if_else(status == "menganggur", 0, 1)
  )

library(survival)
library(survminer)

km <- survfit(Surv(waktu, status) ~ slta, data = x)
summary(km)
# mf = n event
# nf = n.risk



obj <- Surv(waktu, status) ~ slta
# logrank
survdiff(obj, data = x)
# breslow generalized wilcoxon
PHInfiniteEstimates::gehan.wilcoxon.test(obj, data = x)
# tarone ware
coin::logrank_test(
  obj,
  data = x %>%
    mutate(
      slta = factor(slta, labels = c(0, 1), levels = c("sma", "smk"))
    ),
  type = "Tarone-Ware"
)

# Ho : Tidak ada perbedaan kurva km
# H1 : Minimal terdapat 1 kurva km yang berbeda


# No 3 --------------------------------------------------------------------
# Hazard ratio untuk CHR
# dimana CH1 = 1 (menyatakan ada riwayat chronic desease)
exp(0.8051)
# seseorang yang memiliki riwayat chronic desease) memiliki hazard lebih
# besar dari pada seseorang yg tidak memiliki riwayat chronic desease








# 2018/2019 ---------------------------------------------------------------
s <- "1 Pria 2 Membeli
13 Pria 7 Membeli
2 Pria 2 Tidak 
14 Pria 5 Tidak
3 Wanita 6 Membeli
15 Wanita 9 Membeli
4 Pria 20 Tidak 
16 Pria 25 Tidak
5 Pria 11 Membeli 
17 Pria 15 Membeli 
6 Wanita 2 Tidak 
18 Wanita 5 Tidak
7 Pria 5 Membeli 
19 Pria 10 Membeli 
8 Pria 4 Tidak 
20 Pria 8 Tidak 
9 Wanita 1 Membeli 
21 Wanita 5 Membeli 
10 Pria 13 Membeli
22 Pria 16 Membeli
11 Wanita 3 Membeli
23 Wanita 8 Membeli
12 Wanita 7 Tidak 
24 Wanita 11 Tidak"

x <- dm::read_string(s) %>%
  `colnames<-`(c("id", "jk", "waktu", "status")) %>%
  arrange(id) %>%
  mutate(
    status = if_else(status == "Membeli", 1, 0)
  )
head(x)


library(survival)
library(survminer)

obj <- Surv(waktu, status) ~ jk
km <- survfit(obj, data = x)
summary(km)

dat <- data.frame(
  tf = km$time,
  nf = km$n.risk,
  mf = km$n.event,
  qf = km$n.censor,
  Sf = round(km$surv, 3),
  group = rep(gsub("^\\D+=", "", names(km$strata)), km$strata)
) %>%
  group_split(group)

dat[[1]]
dat[[2]]

result <- dat %>%
  {
    merge(
      (.)[[1]], (.)[[2]],
      by = "tf", all = T,
      suffixes = c(1, 2)
    )
  } %>%
  dplyr::select(-starts_with("group")) %>%
  mutate_at(
    vars(starts_with(c("mf", "qf"))),
    ~ if_else(is.na(.x), 0, .x)
  ) %>%
  tidyr::fill(
    c(nf1, nf2, Sf1, Sf2),
    .direction = "up"
  ) %>%
  mutate_at(
    vars(starts_with("nf")),
    ~ if_else(is.na(.x), 0, .x)
  ) %>%
  mutate(
    e1 = nf1 * (mf1 + mf2) / (nf1 + nf2),
    e2 = nf2 * (mf1 + mf2) / (nf1 + nf2),
    d1 = mf1 - e1,
    d2 = mf2 - e2,
    var = nf1 * nf2 * (mf1 + mf2) * ((nf1 + nf2) - (mf1 + mf2)) / ((nf1 + nf2)^2 * ((nf1 + nf2) - 1))
  )

sum(result$d1)
sum(result$d2)
sum(result$var, na.rm = T)

# Statistik Uji
sum(result$d1, na.rm = T)^2 / sum(result$var, na.rm = T)
qchisq(0.05, 1, lower.tail = F)

survdiff(obj, data = x)


dat[[1]]
dat[[2]]



lrt <- function(lk1, lk2, df1, df2){
  lr <- -2 * lk1 - (-2 * lk2)
  cat('LR :', lr, ' p-value  :', pchisq(lr, df1 - df2, lower.tail = F), '\n')
}

lrt(-218.8256, -219.0131, 4, 3)



# no 3 --------------------------------------------------------------------
s <- '1 2 1 1
2 4 0 1
3 5 0 0
4 7 1 0
5 9 0 1
6 10 1 1'

x <- dm::read_string(s, col_names = c('id', 'time', 'status', 'jk'))

cox <- coxph(Surv(time, status) ~ jk, data = x)
summary(cox)

exp(coef(cox))
exp(confint(cox))


# No 2 --------------------------------------------------------------------
waktu <- c(6, 6, 7, 7, 8, 9, 10, 10, 11, 13, 16, 17, 19, 20, 22, 23, 25, 30, 36, 36, 36)
status <- c(1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1,
            0, 0, 0, 1, 1, 0, 0, 0, 0, 0)
jk <- c(2, 3, 2, 1, 3, 2, 1, 3, 2, 2)
jk <- rep(rep(c("P", "L"), length = length(jk)), jk)


x <- data.frame(waktu, status, jk)

obj <- Surv(waktu, status) ~ jk
km <- survfit(obj, data = x)
summary(km)

# S(t = 23)
# Pada saat bulan ke-23, peluang laki-laki lulusan S1 program studi 
# statistika untuk tidak mendapat pekerjaan tetap adalah 0.4848

# S(t = 16)
# ada saat bulan ke-16, peluang perempuan lulusan S1 program
# studi statistika untuk tidak mendapat pekerjaan tetap adalah 0.4114.

survdiff(obj, x)

dat <- data.frame(
  tf = km$time,
  nf = km$n.risk,
  mf = km$n.event,
  qf = km$n.censor,
  Sf = round(km$surv, 3),
  group = rep(gsub("^\\D+=", "", names(km$strata)), km$strata)
) %>%
  group_split(group)

dat[[1]]
dat[[2]]

result <- dat %>%
  {
    merge(
      (.)[[1]], (.)[[2]],
      by = "tf", all = T,
      suffixes = c(1, 2)
    )
  } %>%
  dplyr::select(-starts_with("group")) %>%
  mutate_at(
    vars(starts_with(c("mf", "qf"))),
    ~ if_else(is.na(.x), 0, .x)
  ) %>%
  tidyr::fill(
    c(nf1, nf2, Sf1, Sf2),
    .direction = "up"
  ) %>%
  mutate_at(
    vars(starts_with("nf")),
    ~ if_else(is.na(.x), 0, .x)
  ) %>%
  mutate(
    e1 = nf1 * (mf1 + mf2) / (nf1 + nf2),
    e2 = nf2 * (mf1 + mf2) / (nf1 + nf2),
    d1 = mf1 - e1,
    d2 = mf2 - e2,
    var = nf1 * nf2 * (mf1 + mf2) * ((nf1 + nf2) - (mf1 + mf2)) / ((nf1 + nf2)^2 * ((nf1 + nf2) - 1))
  )

result

sum(result$d1*sum(result$nf1 + result$nf2), na.rm = T)^2 /( sum((result$nf1 + result$nf2)^2) *sum(result$var, na.rm = T))
qchisq(0.05, 1, lower.tail = F)

exp(-2.2)
exp(0.0073)
exp(-0.535)

exp(-1.644)
exp(0.0073)
cox


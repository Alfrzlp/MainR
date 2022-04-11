tf <- c(3:10, 13, 15:19, 2:5, 7:13)
nf <- c(20, 18, 16, 14, 13, 12, 11, 10, 8, 6, 4, 3, 2, 1, 20, 17, 16, 12, 10, 7, 5:1)
mf <- c(1, 1, 1, 0, 1, 0, 0, 1, 2, 2, 1, 1, 1, 0, 2, 1, 3, 2, 2, 1, 0, 1, 1, 0, 1)
qf <- c(1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0 ,1, 1, 1, 0, 0, 1, 0)

dat <- data.frame(tf, nf, mf, qf)

datx <- dat[1:14,] %>% 
  mutate(
    Sf = (nf - mf)/nf,
    Sf = accumulate(Sf, function(prev, new) prev*new)
  )
daty <- dat[15:25,] %>% 
  mutate(
    Sf = (nf - mf)/nf,
    Sf = accumulate(Sf, function(prev, new) prev*new)
  )
datx
daty

result <- merge(
      datx, daty,
      by = "tf", all = T,
      suffixes = c(1, 2)
    ) %>%
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
sum(result$nf1^2)
sum(result$d1*result$nf1, na.rm = T)^2
sum(result$var, na.rm = T)
sum(result$d1*result$nf1, na.rm = T)^2 / (sum(result$nf1^2)*sum(result$var, na.rm = T))

# 8.635393

dat <- dat %>% 
  mutate(group = rep(1:2, c(14, 11))) %>% 
  pivot_longer(3:4, names_to = 'status') %>% 
  mutate(
    status = ifelse(status == 'mf', 1, 0)
  ) %>% 
  dplyr::select(
    waktu = tf, 
    status, group
  )
  
dat
library(PHInfiniteEstimates)
gehan.wilcoxon.test(Surv(waktu, status) ~ group, data = dat)

sum(result$d2, na.rm = T)^2 / sum(result$var, na.rm = T)
survdiff(Surv(waktu, status) ~ group, data = dat)

s <- '0	21	0	0
1	21	2	0
2	19	2	0
3	17	1	0
4	16	2	0
5	14	2	0
8	12	4	0
11	8	2	0
12	6	2	0
15	4	1	0
17	3	1	0
22	2	1	0
23	1	1	0'


library(purrr)

dm::read_string(s) %>% 
  `colnames<-`(c('tf', 'nf', 'mf', 'qf')) %>% 
  mutate(
    S = (nf - mf)/nf,
    S = accumulate(S, function(prev, new) prev*new)
  )



# Kaplan - Meier ----------------------------------------------------------
library(survival)
library(survminer)

leuk <- read.csv('D:/__SEMESTER 6/AKH/leuk.csv')
km <- survfit(Surv(waktu, status) ~ treat, data = leuk)

summary(km)
plot(km)

ggsurvplot(km, data = leuk)


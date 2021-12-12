library(tidyverse)
library(DescTools)
library(fmsb)
library(vcdExtra)

df <- 
  data.frame(
    ecg = c(1, 1, 1, 1, 2, 2, 2, 2), 
    bmi = c(1, 1, 2, 2, 1, 1, 2, 2), 
    smoke = c(1, 2, 1, 2, 1, 2, 1, 2), 
    n = c(47, 10, 8, 6, 25, 15, 35, 30)
  ) %>% 
  slice(rep(1:n(), n)) %>% 
  select(-n) %>% 
  mutate_all(~ .x - 1)

sample_n(df, 5)

# x y z
(tab <- df %>% 
  xtabs( ~ ecg + bmi + smoke, .))

ExpFreq(tab)

chisq.test(tab[,,1])
chisq.test(tab[,,2])
chisq.test(tab[,,1] + tab[,,2])

BreslowDayTest(tab)
mantelhaen.test(tab)

# deault 1
fit <- glm((smoke==0) ~ bmi + ecg, family = poisson, df)

(fm <- loglin(tab, list(1, 2, c(1, 2)), print = T))




# kway
Kway(Freq ~ smoke + bmi + ecg, data = dat) %>% 
  LRstats()
Kway(Freq ~ smoke + bmi + ecg, data = dat) %>% 
  anova(test = 'LRT')



dat <- as.data.frame(tab) %>% 
  mutate_at(-4, ~ relevel(.x, ref = 1)) 

(fit <- MASS::loglm(Freq ~ smoke*bmi*ecg, data = tab))
step(fit, trace = T, test = 'Chisq')  

# manual
fit <- MASS::loglm(Freq ~ smoke*bmi*ecg, data = tab)
drop1(fit, test = 'Chisq')

fit <- MASS::loglm(Freq ~ smoke + bmi + ecg + smoke:bmi + 
                  smoke:ecg + bmi:ecg, data = tab)
drop1(fit, test = 'Chisq')

fit <- MASS::loglm(Freq ~ smoke + bmi + ecg + 
                     smoke:ecg + bmi:ecg, data = tab)
drop1(update(fit, ~., -smoke*bmi), test = 'Chisq')




fit <- glm(Freq ~ smoke*bmi*ecg, data = tab, family = poisson)
drop1(fit,  test = 'Chisq')
# . artinya apa yang sebelumnya ada di bagian formula ini
drop1(update(fit, ~. -smoke:bmi:ecg), test = 'Chisq')
drop1(update(fit, ~. -smoke:bmi:ecg -smoke:bmi), test = 'Chisq')


backward <- function(model, ...){
  h <- drop1(fit,  test = 'Chisq', ...)
  print(h)
  nama <- c('~.')
  while(sum(h$`Pr(>Chi)` > 0.05, na.rm = T) != 0) {
    nama <- c(nama, rownames(h)[which.max(h$`Pr(>Chi)`)])
    new_formula <- as.formula(paste0(nama, collapse = ' -'))
    h <- drop1(update(fit, new_formula), test = 'Chisq', ...)
    print(h)  
  }
}

fit <- glm(Freq ~ smoke*bmi*ecg, data = tab, family = poisson)
backward(fit)

nama <- '~.'


#h <- drop1(do.call('update', list(fit, new_formula)), test = 'Chisq', ...)
















m = c('baik', 'buruk')
a = c('rendah', 'tinggi')
d = c('rendah', 'tinggi')

n = c(103, 32, 87, 42, 59, 78, 109, 205)
tab <- tabkon(a, d, m, nilai = n)

dat <- 
  as.data.frame(tab) %>% 
  mutate_at(1:2, ~ relevel(.x, ref = 'tinggi')) %>% 
  mutate_at(3, ~ relevel(.x, ref = 'buruk'))

(fit <- glm(Freq ~ m + a + d + a:d + m:a + d:m,
            data = dat, family = poisson()))

MASS::loglm(Freq ~ m + a + d + a:d + m:a + d:m,
      data = dat, family = poisson)

fit$fitted.values
coef(fit) %>% sum()

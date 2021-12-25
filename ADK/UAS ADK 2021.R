library(tidyverse)
library(lmtest)


dat <- foreign::read.spss('D:/__SEMESTER 5/Data-UASADK01-2021.sav',
                to.data.frame = T)
head(dat)
str(dat)

dat <- dat %>% 
  mutate_at(-2, ~ fct_rev(.x))


# Model -------------------------------------------------------------------
m <- glm(Y~., data = dat, family = binomial())
summary(m)

exp(coef(m))

# Pseudo R2 ---------------------------------------------------------------
DescTools::PseudoR2(m, c('CoxSnell', 'Nagelkerke', 'McFadden'))


# Hosmer and Lemeshow test ------------------------------------------------
# Ho : Model fit dengan data
ResourceSelection::hoslem.test(m$y, m$fitted.values)



# No C --------------------------------------------------------------------
m <- glm((Y=='Miskin')~., data = dat, family = binomial())
summary(m)
x <- sum(coef(m)[-2]*c(1, 1, 0, 0, 0, 1, 1))

# Minimal Umur
(log(1) - x)/coef(m)[2]


# Bukti
newdata <- 
  data.frame(
    X1 = 60.41392,
    X2 = 'Minimal SMA',
    X3 = 'Perempuan',
    X4 = 'Tidak Bekerja',
    X5 = 'Maksimum 4',
    X6 = 'Menabung'
  )
newdata

predict(m, newdata, type = 'resp')

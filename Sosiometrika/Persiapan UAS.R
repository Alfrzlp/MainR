s <- '73 63 53 63 61 65
82 63 72 64 65 70
80 58 70 70 59 56
80 77 66 66 82 79
75 77 71 71 90 89
79 89 73 73 71 81'

library(dm)

dat <- read_string(s) %>% 
  mutate(jk = rep(c('l', 'p'), each = 3)) %>% 
  pivot_longer(-jk) %>% 
  mutate(
    metode = case_when(
      name %in% c('V1', 'V2') ~ 'selidik',
      name %in% c('V3', 'V4') ~ 'demo',
      name %in% c('V5', 'V6') ~ 'ide'
    ),
    .keep = 'unused'
  )

dat %>% 
  group_by(jk) %>% 
  summarise(mean(value))

dat %>% 
  group_by(metode) %>% 
  summarise(mean(value))

mean(dat$value)



mod <- aov(value ~ jk + metode, data = dat) 
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
    c(1:(nr - 1), nr + 1, nr, nr + 2)
  )



# eta
p <- ncol(dat) - 1
sqrt(r$sumsq[1:p] / SStot)


# R2 adj
R2 <- SSmod / SStot
R2





# -------------------------------------------------------------------------
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

dat <- read_string(s, col_names = c('y', 'jk', 'kelas', 'jurusan'))

mod <- aov(y ~ jk + kelas + jurusan,
           data = dat %>% mutate_at(vars(-y), ~factor(.x))) 
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
        # statistic = ifelse(term == 'Model', MSmod / MSres, NA),
        statistic = ifelse(term %in% c('Model', '(Combined)'), meansq / MSres, NA),
        p.value = ifelse(term %in% c('Model', '(Combined)'), pf(statistic, df, r$df[nr], lower.tail = F), NA)
      )
  ) %>% 
  slice(
    nr + 3, c(1:(nr - 1), nr + 1, nr, nr + 2)
  )


# eta
p <- ncol(dat) - 1
sqrt(r$sumsq[1:p] / SStot)


# R2 adj
R2 <- SSmod / SStot
R2
sqrt(R2)



# Analisis Jalur ----------------------------------------------------------
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

library(dm)
dat <- read_string(s) %>% 
  select(-V1) %>% 
  set_names(c('x1', 'x2', 'x3', 'x4')) %>% 
  scale() %>% 
  as.data.frame()

cor(dat)

# x3 ~ x1 + x2
# x4 ~ x1 + x2 + x3

m1 <- lm(x3 ~ x1 + x2, data = dat)
m1 %>% coef() %>% round(3)

m2 <- lm(x4 ~ x1 + x2 + x3, data = dat)
m2 %>% coef() %>% round(3)


get_hasil <- function(res){
  res <- broom::tidy(res)
  t(res[, 2]) %>% 
    as.data.frame() %>% 
    setNames(res$term)
}

resm1 <- get_hasil(m1)
resm2 <- get_hasil(m2)


resm1$x1 * resm2$x3
resm1$x2 * resm2$x3

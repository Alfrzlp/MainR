library(nnet)

dat <- foreign::read.spss('D:/_Datasets/Multinomial logit.sav',
                          use.value.labels = T, to.data.frame = T) %>% 
       janitor::clean_names()
dat

# relevel data ------------------------------------------------------------
dat <- 
  dat %>% 
  mutate_at(1:2, ~ fct_relevel(.x, ~ tail(.x, 1))) %>% 
  mutate(pi = fct_rev(pi))


m <- multinom(pi ~ putih + laki, dat, weights = frekuensi, model = T)
summary(m)



# koef --------------------------------------------------------------------
options(pillar.sigfig = 3)

tibble(
  coef = coef(m)[1,],
  std = result$standard.errors[1,]
) %>% 
  mutate(
    z = coef/std,
    wald = z^2,
    df = 1,
    sig = pchisq(wald, df, lower.tail = F)
  ) %>% 
  cbind(
    exp(cbind(exp_beta = coef(m)[1,], ci[, , 1]))
  ) %>% 
  round(3)



summary_koef <- function(model){
  output <- list()
  koef <- coef(m)
  result <- summary(m)
  ci <- confint(m)
  
  for(i in 1:length(row.names(koef))){
    output[[row.names(koef)[i]]] <- 
    tibble(
      coef = koef[i,],
      std = result$standard.errors[i,]
    ) %>% 
      mutate(
        z = coef/std,
        wald = z^2,
        df = 1, # masih belum tau dari mana df
        sig = pchisq(wald, df, lower.tail = F),
        exp_beta = exp(coef)
      ) %>% 
      cbind(
        exp(ci[, , i])
      ) %>% 
      round(3)
  }
  return(output)
}
summary_koef(m)


# len dibagi maka kurang
coef(m)[1,] - coef(m)[2,]



# Menghitung peluang setiap kategori

# P(A) = exp(beta_a*Xa) / (exp(beta_a*Xa) + exp(beta_b*Xb) + exp(c))
# P(B) = exp(beta_b*Xb) / (exp(beta_a*Xa) + exp(beta_b*Xb) + exp(c))
# Dimana exp(c) = 1 (kategori refrence)

exp(-0.597) / (exp(-0.597) + exp(-0.575) + 1)
exp(-0.575) / (exp(-0.597) + exp(-0.575) + 1)
1/(1 + exp(-0.575) + exp(-0.597))

# Atau pakai softmax
x = c(-0.597, -0.575, 0)
exp(x)/sum(exp(x))


# Membalik kategori referensi ---------------------------------------------
# ln(a/b) = ln((a/ref) / (b/ref))

# mobilio
# ln(mobilio/avanza) = ln(1 / (avanza/mobilio))
# maka = ln(1) - ln(avanza/mobilio)

# uji serentak ------------------------------------------------------------
m0 <- multinom(pi ~ 1, dat, weights = frekuensi)
lmtest::lrtest(m0, m)


# uji Goodnes of fit ------------------------------------------------------
# (Perlu di fix kan)
# Ho : Model cocok dengan data
library(generalhoslem)

raw_data <- 
  dat %>% 
  slice(rep(1:n(), frekuensi)) %>% 
  dplyr::select(-frekuensi)

m_raw <- multinom(pi ~ ., raw_data)



logitgof(raw_data$pi, fitted(m_raw))
chisq.test(raw_data$pi, predict(m_raw))

# Pseudo R2 ---------------------------------------------------------------
# Tidak bisa menilai keakuratan model. 
# hanya untuk perbandingan saja biasanya
DescTools::PseudoR2(m, which = c('CoxSnell', 'Nagelkerke', 'McFadden')) %>% 
  round(4)


library(tidyverse)

tab <- rbind(
  c(0.2, 0.35, 0.45) * 500,
  c(76, 167, 257)
)
tab

# Nilai expectasi
(hasil <- chisq.test(tab))
hasil$expected


# No 2 --------------------------------------------------------------------

(mat <- matrix(c(
  25, 10, 15, 0, 15, 15, 6, 6, 6, 9, 12,
  12, 1, 6, 17, 10
), 4))
# Nilai expectasi
(h2 <- chisq.test(mat))
h2$expected

library(vcdExtra)
c_gamma <- GKgamma(mat)
# nilai C dan D
c_gamma$C
c_gamma$D
c_gamma


# No 3 --------------------------------------------------------------------

tab3 <- matrix(c(
  76, 55, 24, 40, 31, 45, 13, 14, 25, 45,
  46, 8, 30, 18, 33, 24, 24, 49
), nrow = 3)
tab3

marginal_tab <-
  rbind(
    colSums(tab3)[1:3],
    colSums(tab3)[4:6]
  )
marginal_tab

(ra3 <- chisq.test(marginal_tab))
ra3$expected

# baby boomer
bb_tab <-
  rbind(tab3[1, 1:3], tab3[1, 4:6])
bb_tab

(rbb <- chisq.test(bb_tab))
rbb$expected

# Gen X
gx_tab <-
  rbind(tab3[2, 1:3], tab3[2, 4:6])
gx_tab

(rgx <- chisq.test(gx_tab))
rgx$expected


# Gen Milenial
gm_tab <-
  rbind(tab3[3, 1:3], tab3[3, 4:6])
gm_tab

(rgm <- chisq.test(gm_tab))
rgm$expected





df <-
  as.data.frame(tab3) %>%
  mutate(
    usia = c("bb", "Gen X", "Gen Milenial"),
    .before = V1
  ) %>%
  `colnames<-`(c(
    "U", "ikut_tinggi", "ikut_sedang", "ikut_rendah",
    "tdkikut_tinggi", "tdkikut_sedang", "tdkikut_rendah"
  )) %>%
  pivot_longer(2:7, values_to = "Freq") %>%
  separate(name, into = c("K", "P"), sep = "_") %>%
  mutate(
    U = fct_relevel(U, c("Gen Milenial", "Gen X", "bb")),
    K = fct_relevel(K, c("tdkikut", "ikut")),
    P = fct_relevel(P, c("rendah", "sedang", "tinggi"))
  )

# CMH test
library(DescTools)
xtabs(Freq ~ K + P + U, df) %>%
  mantelhaen.test()

df

# Model
(fit1 <- glm(Freq ~ U + K + P + U:P,
  data = df, family = poisson
))

(fit2 <- glm(Freq ~ U + K + P + K:P + U:P,
  data = df, family = poisson
))

# Nilai Deviance Residual adalah Nilai LR test
summary(fit1)
summary(fit2)

hasil <- data.frame(
  obs = df$Freq,
  "K_UP" = fit1$fitted.values,
  "KP_UP" = fit2$fitted.values
)

hasil

MASS::loglm(Freq ~ U + K + P + K:P,
  data = df, family = poisson
)
MASS::loglm(Freq ~ U + K + P + U:P + K:P,
  data = df, family = poisson
)



glm(Freq ~ U + K + P, data = df, family = poisson)

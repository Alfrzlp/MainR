hsb <- read.csv("D:/Datasets/hsb2-2.csv")
head(hsb)
bartlett.test(science ~ c(female + schtyp), data = hsb)

library(tidyverse)
metode <- tibble(
  metode1 = c(21, 27, 29, 23, 25),
  metode2 = c(17, 25, 20, 15, 23),
  metode3 = c(31, 28, 22, 30, 24)
)
metode
metode <- metode %>% pivot_longer(1:3)

aov(value ~ name, data = metode)

tablet <- tibble(
  a = c(5, 4, 8, 6, 3),
  b = c(9, 7, 8, 6, 9),
  c = c(3, 5, 2, 3, 7),
  d = c(2, 3, 4, 1, 4),
  e = c(7, 6, 9, 4, 7)
)
tablet
tablet <- tablet %>% pivot_longer(everything())
tablet

aov(value ~ name, data = tablet)


mobil <- tibble(
  jenis_mobil = c(rep("a", 4), rep("b", 6), rep("c", 5)),
  cacat = c(4, 7, 6, 6, 5, 1, 3, 5, 3, 4, 8, 6, 8, 9, 5)
)
mobil
hasil <- aov(cacat ~ jenis_mobil, data = mobil)



# duncan test-----------------------------------------
lemak <- c(5, 4, 8, 6, 4, 6, 9, 7, 8, 6, 9, 6, 3, 6, 2, 4, 7, 8, 2, 3, 4, 3, 4, 5, 7, 6, 9, 5, 7, 5)

data <- data.frame(lemak, nama = rep(LETTERS[1:5], each = 6))
data

anova <- aov(lemak ~ nama, data)
anova %>% summary()

out <- agricolae::duncan.test(anova, "nama")
out

# rp dan Rp
out$duncan

# hasil
out$groups

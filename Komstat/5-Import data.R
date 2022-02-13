library(dplyr)
loc <- "D:/R/gapminder.tsv"
data <- read.csv(loc, header = TRUE, sep = "\t")
View(data)

data <- data %>% filter(year == 1952)
View(data)
miu <- mean(data$gdpPercap)
sd(data$lifeExp)
# 70.72848

library(combinat)
all_psample <- combn(data$lifeExp, 140)
View(all_psample)

#------------------------------------------------------
ttest <- function(dt, ci) {
  ci <- ci + (1 - ci) / 2
  xbar <- mean(dt)
  s <- sd(dt)
  e <- qt(ci, length(dt) - 1) * s / sqrt(length(dt))
  up <- xbar + e
  dw <- xbar - e

  if ((miu > dw) & (miu < up)) {
    print(paste0(dw, "< miu <", up))
    return(1)
  } else {
    print(paste0(dw, "< miu <", up))
    return(0)
  }
}
#------------------------------------------------------
hasil <- 0
for (i in 1:ncol(all_psample)) {
  hasil <- hasil + ttest(all_psample[, i], 0.90)
  print(hasil)
}

for (i in 1:100) {
  smpl <- sample_n(data, 50)
  hasil <- hasil + ttest(smpl$gdpPercap, 0.8)
  print(hasil)
}
